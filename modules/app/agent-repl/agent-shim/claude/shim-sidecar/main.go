// Command shim-claude-sidecar is the agent-shim file-plane reader (design §7): a
// singleton, launchd-managed process that discovers the Claude harness's on-disk
// artifacts (session transcripts, agent sidechains, workflow journals, /tmp task
// spools), tails them with cursored truncation-aware reads, converts records into
// agent-shim protocol events, infers LOST terminal transitions per the staleness
// policy, and writes everything to the shim-store with atomic cursor advancement.
//
// Flags (the launchd plists reference these):
//
//	--store-socket   shim-store UDS path        (…/sock/store.sock)
//	--config-roots   comma-separated config roots (~/.claude,~/.claude-chesscom)
//	--spool-root     /tmp task-spool root       (/tmp; resolves claude-<uid>/… itself)
//	--log            append-only log file (also to stderr)
package main

import (
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"os/signal"
	"path/filepath"
	"strings"
	"syscall"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-claude-sidecar/internal/discover"
	"agentrepl/shim-claude-sidecar/internal/handler"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/stale"
	"agentrepl/shim-claude-sidecar/internal/storeclient"
	"agentrepl/shim-claude-sidecar/internal/tail"
	"golang.org/x/sys/unix"
)

func main() {
	base := defaultCacheDir()
	storeSocket := flag.String("store-socket", filepath.Join(base, "sock", "store.sock"), "shim-store UDS path")
	configRoots := flag.String("config-roots", "~/.claude,~/.claude-chesscom", "comma-separated config roots")
	spoolRoot := flag.String("spool-root", "/tmp", "task-spool root (resolves claude-<uid>/ itself)")
	logPath := flag.String("log", filepath.Join(base, "log", "shim-claude-sidecar.log"), "log file path (also to stderr)")
	flag.Parse()

	if err := run(*storeSocket, parseRoots(*configRoots), *spoolRoot, *logPath); err != nil {
		reportFatal(err, os.Stderr)
		os.Exit(1)
	}
}

// reportFatal writes only bootstrap failures because all post-bootstrap errors
// have already reached the canonical logger and its stderr sink.
func reportFatal(err error, stderr io.Writer) {
	if isBootstrapError(err) {
		payload, encodeErr := json.Marshal(map[string]any{
			"timestamp": time.Now().Local().Format(logging.TimestampLayout),
			"runtime":   "sidecar", "pid": os.Getpid(), "level": "error", "verbosity": "normal",
			"operation": "sidecar.bootstrap", "message": "sidecar bootstrap failed",
			"context": map[string]any{"error": err.Error()},
		})
		if encodeErr != nil {
			panic(fmt.Sprintf("shim-claude-sidecar bootstrap log encode failed: %v", encodeErr))
		}
		if _, writeErr := stderr.Write(append(payload, '\n')); writeErr != nil {
			panic(fmt.Sprintf("shim-claude-sidecar bootstrap log write failed: %v", writeErr))
		}
	}
}

func run(storeSocket string, roots []string, spoolRoot, logPath string) error {
	logf, closeLog, err := openLogger(storeSocket, logPath)
	if err != nil {
		return err
	}
	defer closeLog()

	sigc := make(chan os.Signal, 1)
	signal.Notify(sigc, syscall.SIGINT, syscall.SIGTERM)
	return runWithLogger(storeSocket, roots, spoolRoot, logf, sigc)
}

// openLogger creates the sidecar's only persistent diagnostic sink. Failures
// here are bootstrap failures because no canonical logger can exist yet.
func openLogger(storeSocket, logPath string) (*logging.Bound, func(), error) {
	if err := os.MkdirAll(filepath.Dir(logPath), 0o755); err != nil {
		return nil, nil, bootstrapError{fmt.Errorf("creating log dir: %w", err)}
	}
	lf, err := os.OpenFile(logPath, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o644)
	if err != nil {
		return nil, nil, bootstrapError{fmt.Errorf("opening log %q: %w", logPath, err)}
	}
	logf := logging.New(os.Stderr, lf).With(logging.Context{Component: "sidecar", StoreSocket: storeSocket})
	return logf, func() { _ = lf.Close() }, nil
}

// runWithLogger owns process-level sidecar failures after canonical logging is
// available. Lower layers retain ownership of errors they log themselves.
func runWithLogger(storeSocket string, roots []string, spoolRoot string, logf *logging.Bound, stop <-chan os.Signal) error {
	sc := newSidecar(storeSocket, roots, spoolRoot, logf)
	logf.With(logging.Context{Operation: "start"}).Log("roots=%v spool=%s", roots, spoolRoot)
	return runLogged(logf, func() error { return sc.Run(stop) })
}

func runLogged(logf *logging.Bound, execute func() error) error {
	if err := execute(); err != nil {
		logf.With(logging.Context{Operation: "run"}).Log("sidecar stopped with error: %v", err)
		return err
	}
	return nil
}

// bootstrapError marks the only failures that may be reported before the
// canonical logger exists.
type bootstrapError struct{ err error }

func (e bootstrapError) Error() string { return e.err.Error() }
func (e bootstrapError) Unwrap() error { return e.err }

func isBootstrapError(err error) bool {
	var target bootstrapError
	return errors.As(err, &target)
}

// ---------------------------------------------------------------------------
// Sidecar orchestration
// ---------------------------------------------------------------------------

type watched struct {
	target    discover.Target
	sessionID string
	tailer    *tail.Tailer
}

// UnownedSpoolWindow is how long a spool may sit unattributed before it counts
// as an anomaly rather than a race. The launch line naming an owner is written
// when the task starts, so a hold normally clears within a rescan tick; one
// that outlives this window means the mapping is genuinely missing.
const UnownedSpoolWindow = 60 * time.Second

// heldReport is the last hold summary written to the log, so an unchanged
// backlog stays silent.
//
// REPORTED IN AGGREGATE, deliberately. /tmp accumulates spools from every
// project and every past session — well over a thousand here — and their launch
// lines sit far behind every cursor, so they are permanently unattributable.
// A line per spool meant thousands per restart, which buries the one thing
// worth seeing: that the backlog CHANGED.
type heldReport struct {
	held  int
	stale int
}

type sidecar struct {
	store   *storeclient.Client
	disc    *discover.Discoverer
	tracker *stale.Tracker
	roots   []string
	log     *logging.Bound

	watchers map[string]*watched // by path

	// owners maps a task id to the session that LAUNCHED it — the only session
	// identifier in the system, sourced from the transcript that announced the
	// task. A /tmp spool has no identity of its own (see internal/discover), so
	// this is what attributes one. Seeded per connection from the store's
	// authoritative open tasks and extended as transcripts are tailed.
	owners map[string]string // task id -> session id
	// held records spools discovered before their owner was known, so an
	// attribution that never arrives is visible rather than silent. A held
	// spool is NOT tailed: reading it would mean inventing an owner.
	held map[string]int64 // path -> first-seen ms
	// lastHeldReport is the last summary written, so an unchanged backlog is
	// not re-reported on every rescan.
	lastHeldReport heldReport

	// Store-link state machine (link.go). `cursors` is CONNECTION-SCOPED: it is
	// recovered as the first act of every established connection and dropped the
	// moment the link is lost, so a tailer can never be built from a stale — or
	// absent — recovery.
	link         linkState
	cursors      map[string]*corev1.CursorState // by path; nil unless recovered
	dialT        *time.Timer
	backoff      time.Duration
	dialFailures int
	downSince    time.Time
	bootSwept    bool
	diagnostics  diagnosticOutbox
}

func newSidecar(storeSocket string, roots []string, spoolRoot string, log *logging.Bound) *sidecar {
	s := &sidecar{
		store:    storeclient.New(storeSocket, log.With(logging.Context{Component: "storeclient"})),
		disc:     discover.New(roots, spoolRoot, log.With(logging.Context{Component: "discover"})),
		tracker:  stale.New(stale.Options{}, log.With(logging.Context{Component: "stale"})),
		roots:    roots,
		log:      log,
		watchers: map[string]*watched{},
		owners:   map[string]string{},
		held:     map[string]int64{},
		// A fresh sidecar is simply a sidecar whose link is not up yet, with its
		// first dial due immediately. That is all "boot" means here.
		link:      linkDown,
		downSince: time.Now(),
		dialT:     time.NewTimer(0),
	}
	// Delivery only appends to an in-memory outbox. The event loop owns all
	// store I/O, so a log created while processing a store operation cannot
	// recursively write to the store.
	log.SetDiagnosticSink(s.diagnostics.enqueue)
	return s
}

// Run drives the store-link state machine and, while that link is up, the
// poll/rescan/sweep/heartbeat loop, until a termination signal.
func (s *sidecar) Run(stop <-chan os.Signal) error {
	pollT := time.NewTicker(time.Second)
	sweepT := time.NewTicker(30 * time.Second)
	beatT := time.NewTicker(15 * time.Second)
	rescanT := time.NewTicker(10 * time.Second)
	defer pollT.Stop()
	defer sweepT.Stop()
	defer beatT.Stop()
	defer rescanT.Stop()
	defer s.dialT.Stop()

	for {
		select {
		case <-stop:
			s.log.With(logging.Context{Operation: "shutdown"}).Log("received signal")
			return s.store.Close()
		case <-s.dialT.C:
			s.dial()
		case <-rescanT.C:
			s.whenUp(s.rescan)
		case <-pollT.C:
			s.whenUp(func() {
				s.pollAll()
				s.flushDiagnostics()
			})
		case <-sweepT.C:
			s.whenUp(s.sweep)
		case <-beatT.C:
			s.whenUp(s.heartbeat)
		}
	}
}

// sweep emits the LOST inferences that crossed their thresholds.
func (s *sidecar) sweep() {
	s.emit(s.tracker.Sweep(time.Now().UnixMilli()))
}

// heartbeat uses a correlated store health probe.  The sidecar only reads
// files while its store link is healthy, so a socket that merely exists must
// never keep ingestion running.
func (s *sidecar) heartbeat() {
	requestID := fmt.Sprintf("sidecar-health-%d", time.Now().UnixNano())
	if err := s.store.Health(requestID); err != nil {
		s.log.With(logging.Context{Operation: "health", RequestID: requestID}).Log("health check failed: %v", err)
		s.noteStoreErr("health", err)
	}
}

// bootSweep LOSTs persisted open tasks that predate the machine boot. The
// tracker's set was restored from authoritative store lifecycle state before
// this runs; discovered artifacts are deliberately not evidence of liveness.
func (s *sidecar) bootSweep() {
	boot := bootTimeMillis()
	if boot == 0 {
		s.log.With(logging.Context{Operation: "boot-sweep"}).Log("boot time unavailable")
		return
	}
	now := time.Now().UnixMilli()
	if ev := s.tracker.BootSweep(boot, now); len(ev) > 0 {
		s.log.With(logging.Context{Operation: "boot-sweep"}).Log("%d pre-boot task(s) inferred LOST", len(ev))
		s.emit(ev)
	}
}

// rescan discovers targets and creates a tailer for each new one, seeded from
// the cursor this connection's store handed us.
//
// This is the ONLY place a tailer is ever built, which is why it asserts the
// link: a tailer built without a recovered cursor map starts at offset 0, and
// that silent cold start is the bug the whole state machine exists to prevent.
// A file the connected store genuinely holds no cursor for still starts at 0 —
// that case is honest, and it is the backfill path.
func (s *sidecar) rescan() {
	s.requireLinkUp("rescan")
	now := time.Now().UnixMilli()
	for _, tgt := range s.disc.Scan() {
		if _, ok := s.watchers[tgt.Path]; ok {
			continue
		}
		session, ok := s.resolveOwner(tgt, now)
		if !ok {
			// Unattributed: held, never guessed. Tailing it would mean either
			// inventing a session or reviving the /tmp path id this change
			// exists to delete.
			continue
		}
		ctx := &tail.Context{
			SessionID: session,
			Path:      tgt.Path,
			Kind:      tgt.Kind,
			TaskID:    tgt.TaskID,
			SpoolDir:  tgt.SpoolDir,
			RunID:     tgt.RunID,
		}
		bound := s.log.With(logging.Context{Component: "tail", Path: tgt.Path, Session: session, Task: tgt.TaskID})
		tr := tail.New(tgt.Path, tgt.Codec(), s.newHandler(tgt.Kind, bound), ctx, bound)
		if c := s.cursors[tgt.Path]; c != nil {
			tr.Restore(c)
		}
		s.watchers[tgt.Path] = &watched{target: tgt, sessionID: session, tailer: tr}
	}
	s.reportHeld(now)
}

// resolveOwner returns the session a target's events belong to.
//
// A config-root path names its own session (the transcript IS the session's
// record), so it answers immediately. A /tmp spool does not: its path states
// only where the bytes live, so its owner is looked up by task id against the
// launch the transcript announced. An unresolved spool is HELD — reported and
// left untailed — because the alternatives are inventing a session or reading
// the path's runtime id, and that id being mistaken for an identity is the bug
// this whole change removes.
func (s *sidecar) resolveOwner(tgt discover.Target, nowMs int64) (string, bool) {
	if tgt.SessionID != "" {
		return tgt.SessionID, true
	}
	session := s.owners[tgt.TaskID]
	if session == "" {
		s.holdUnowned(tgt, nowMs)
		return "", false
	}
	if firstSeenMs, ok := s.held[tgt.Path]; ok {
		delete(s.held, tgt.Path)
		// Per-spool, because a hold that CLEARS is rare and is the interesting
		// half: it says attribution arrived, and how long it took.
		s.log.With(logging.Context{Operation: "resolve-spool-owner", Path: tgt.Path, Session: session, Task: tgt.TaskID}).Log(
			"owner resolved after %dms held", nowMs-firstSeenMs)
	}
	return session, true
}

// holdUnowned records a spool whose owning session is not known yet. Silent by
// itself — reportHeld does the talking, in aggregate, once the pass is done.
func (s *sidecar) holdUnowned(tgt discover.Target, nowMs int64) {
	if _, ok := s.held[tgt.Path]; !ok {
		s.held[tgt.Path] = nowMs
	}
}

// reportHeld summarizes the unattributed backlog after a rescan pass, and only
// when it CHANGED. /tmp holds spools from every past session and project, whose
// launch lines sit behind every cursor and will never be re-read, so the steady
// state is a large permanent backlog: reporting it per spool (or on every pass)
// drowns the log instead of informing it. What matters is the delta — a NEW
// spool nothing can attribute — and that moves the counts.
func (s *sidecar) reportHeld(nowMs int64) {
	report := heldReport{held: len(s.held)}
	for _, firstSeenMs := range s.held {
		if nowMs-firstSeenMs >= UnownedSpoolWindow.Milliseconds() {
			report.stale++
		}
	}
	if report == s.lastHeldReport {
		return
	}
	s.lastHeldReport = report
	if report.held == 0 {
		s.log.With(logging.Context{Operation: "report-held-spools"}).Log("no unattributed spools remain")
		return
	}
	s.log.With(logging.Context{Operation: "report-held-spools", Level: "warn"}).Log(
		"%d held awaiting attribution (%d past %s) — not tailed rather than filed under a guessed session",
		report.held, report.stale, UnownedSpoolWindow)
}

// seedOwners populates the owner index from the store's authoritative open
// tasks, returning how many mappings it established. This is what makes the
// index survive a restart: the launch line naming an owner may sit far behind
// the resumed cursor and never be re-read.
func (s *sidecar) seedOwners(states []*corev1.OpenTaskState) int {
	n := 0
	for _, state := range states {
		ev := state.GetStarted()
		if ts := ev.GetTaskStarted(); ts != nil && s.noteOwner(ts.GetTaskId(), ev.GetSessionId()) {
			n++
		}
	}
	return n
}

// noteOwner records that TASKID was launched by SESSION, reporting (never
// silently resolving) a task that claims two different launching sessions.
// That is precisely the corruption this change eliminates upstream, so if one
// ever appears again it must be loud rather than absorbed. The first mapping
// wins, so the attribution cannot flap between rescans.
func (s *sidecar) noteOwner(taskID, session string) bool {
	if taskID == "" || session == "" {
		return false
	}
	if prior, ok := s.owners[taskID]; ok {
		if prior != session {
			s.log.With(logging.Context{Operation: "record-spool-owner", Session: prior, Task: taskID, Level: "error"}).Log(
				"spool: CONFLICTING owner for task=%s — already attributed to session %s, now also claimed by %s; KEEPING %s",
				taskID, prior, session, prior)
		}
		return false
	}
	s.owners[taskID] = session
	return true
}

// pollAll polls every watched file once, writing any batch to the store and
// committing the cursor only on a durable ack. It is reachable only with the
// link up, so a file is never read without somewhere to put what it says.
func (s *sidecar) pollAll() {
	s.requireLinkUp("pollAll")
	now := time.Now().UnixMilli()
	for path, w := range s.watchers {
		res, err := w.tailer.Poll()
		if err != nil {
			if os.IsNotExist(err) {
				// Vanished file: start the grace clock; stop watching it.
				if w.target.TaskID != "" {
					s.tracker.MarkVanished(w.sessionID, w.target.TaskID, now)
				}
				delete(s.watchers, path)
				s.log.With(logging.Context{Operation: "vanished", Path: path, Session: w.sessionID, Task: w.target.TaskID}).Log("tail vanished, grace clock started")
				continue
			}
			s.log.With(logging.Context{Operation: "poll", Path: path, Session: w.sessionID, Task: w.target.TaskID, Level: "error"}).Log("tail poll failed: %v", err)
			continue
		}
		if !res.Changed {
			continue
		}
		writeStart := time.Now()
		if err := s.writeBatch(res); err != nil {
			// Honest sad path: do NOT commit; the batch replays and dedup absorbs.
			s.log.With(logging.Context{Operation: "store-write", Path: path, Session: w.sessionID, Task: w.target.TaskID, Level: "error", SinkEmergency: true}).Log("cursor not advanced after %d events: %v", len(res.Events), err)
			if s.link != linkUp {
				// The write did not merely fail, it revealed a dead link. Abandon
				// the pass rather than reading the remaining files with nowhere
				// to put what they say.
				return
			}
			continue
		}
		storeWriteMs := time.Since(writeStart).Milliseconds()
		w.tailer.Commit(res)
		if w.target.TaskID != "" {
			s.tracker.Activity(w.sessionID, w.target.TaskID, now)
		}
		s.applyLifecycle(res.Events, now)
		// The happy path is the one that carries the user's prompt echo to the
		// GUI, so it gets a line too: steady state is silent, a pickup is not.
		s.log.With(logging.Context{Operation: "tail-pickup", Path: path, Session: w.sessionID, Task: w.target.TaskID}).Log(
			"picked up %d event(s) kind=%s store_write_ms=%d",
			len(res.Events), kindLabel(w.target.Kind), storeWriteMs)
	}
}

// writeBatch sends one tailer batch (events + cursor advance) as a StoreWrite.
func (s *sidecar) writeBatch(res tail.PollResult) error {
	diagnostics := s.diagnostics.snapshot()
	events := make([]*corev1.Event, 0, len(res.Events)+len(diagnostics))
	events = append(events, res.Events...)
	events = append(events, diagnostics...)
	batch := &corev1.EventBatch{Events: events, CursorAdvance: res.Next}
	if err := s.storeWrite("tailer batch", batch); err != nil {
		return err
	}
	s.diagnostics.acknowledge(len(diagnostics))
	return nil
}

// flushDiagnostics sends records not naturally piggy-backed on a tail batch.
// Failed writes retain the exact event objects for retry and store dedup.
func (s *sidecar) flushDiagnostics() {
	diagnostic, err := s.diagnostics.flush(func(event *corev1.Event) error {
		return s.storeWrite("diagnostic outbox", &corev1.EventBatch{Events: []*corev1.Event{event}})
	})
	if err != nil {
		s.log.With(logging.Context{
			Operation:     "diagnostic-flush",
			Level:         "error",
			Session:       diagnostic.GetSessionId(),
			RequestID:     diagnostic.GetRequestId(),
			Path:          diagnostic.GetFilePlaneDiagnostic().GetSourcePath(),
			SinkEmergency: true,
		}).Log("diagnostic outbox write failed: %v", err)
	}
}

func (s *sidecar) newHandler(kind tail.Kind, log *logging.Bound) tail.Handler {
	handlerLog := log.With(logging.Context{Component: "handler"})
	switch kind {
	case tail.KindSessionTranscript:
		return handler.NewSessionTranscriptHandler(handlerLog)
	case tail.KindAgentTranscript:
		return handler.NewAgentTranscriptHandler(handlerLog)
	case tail.KindWorkflowJournal:
		return handler.NewWorkflowJournalHandler(handlerLog)
	case tail.KindShellSpool:
		return handler.NewShellOutputHandler(handlerLog)
	default:
		panic(fmt.Sprintf("sidecar: unsupported tail kind %d", kind))
	}
}

// applyLifecycle updates the stale tracker from a batch's lifecycle events: a
// TaskStarted opens a task, a real TaskEnded closes it (so it is never LOST-swept).
//
// A TaskStarted also ATTRIBUTES the task: it is emitted by the handler for the
// transcript that announced the launch, so it carries the launching session.
// That is what later lets a bare /tmp spool be tailed without the path ever
// being read as an identity.
func (s *sidecar) applyLifecycle(events []*corev1.Event, nowMs int64) {
	for _, e := range events {
		if ts := e.GetTaskStarted(); ts != nil {
			s.noteOwner(ts.GetTaskId(), e.GetSessionId())
			s.tracker.Open(ts.GetTaskId(), taskKindToTail(ts.GetKind()), e.GetSessionId(), ts.GetOutputPath(), nowMs, nowMs)
		}
		if te := e.GetTaskEnded(); te != nil && te.GetStatus() != corev1.TerminalStatus_TERMINAL_STATUS_LOST {
			s.tracker.Close(e.GetSessionId(), te.GetTaskId())
		}
	}
}

// emit writes a set of synthetic/lifecycle events (e.g. LOST sweeps) to the store
// as a single cursor-less batch.
func (s *sidecar) emit(events []*corev1.Event) {
	if len(events) == 0 {
		return
	}
	if err := s.storeWrite("synthetic events", &corev1.EventBatch{Events: events}); err != nil {
		seenSessions := map[string]bool{}
		for _, event := range events {
			if event.GetSessionId() != "" {
				seenSessions[event.GetSessionId()] = true
			}
		}
		if len(seenSessions) == 0 {
			s.log.With(logging.Context{Operation: "store-write", Level: "error", SinkEmergency: true}).Log("synthetic event write failed for %d events: %v", len(events), err)
			return
		}
		for session := range seenSessions {
			s.log.With(logging.Context{Operation: "store-write", Level: "error", Session: session, SinkEmergency: true}).Log("synthetic event write failed for %d events: %v", len(events), err)
		}
	}
}

// ---------------------------------------------------------------------------
// Pure helpers (tested)
// ---------------------------------------------------------------------------

// taskKindToTail maps a core TaskKind back to the tail.Kind the stale tracker
// keys its per-kind silence windows on.
func taskKindToTail(k corev1.TaskKind) tail.Kind {
	switch k {
	case corev1.TaskKind_TASK_KIND_SHELL:
		return tail.KindShellSpool
	case corev1.TaskKind_TASK_KIND_WORKFLOW:
		return tail.KindWorkflowJournal
	default:
		return tail.KindAgentTranscript
	}
}

// kindLabel renders a watched file's kind for the log.
func kindLabel(k tail.Kind) string {
	switch k {
	case tail.KindSessionTranscript:
		return "session"
	case tail.KindAgentTranscript:
		return "agent"
	case tail.KindWorkflowJournal:
		return "workflow"
	case tail.KindShellSpool:
		return "shell"
	default:
		return fmt.Sprintf("kind(%d)", int(k))
	}
}

// parseRoots splits a comma-separated root list and expands a leading ~.
func parseRoots(csv string) []string {
	var out []string
	for _, r := range strings.Split(csv, ",") {
		r = strings.TrimSpace(r)
		if r == "" {
			continue
		}
		out = append(out, expandHome(r))
	}
	return out
}

func expandHome(p string) string {
	if p == "~" || strings.HasPrefix(p, "~/") {
		if home, err := os.UserHomeDir(); err == nil {
			return filepath.Join(home, strings.TrimPrefix(strings.TrimPrefix(p, "~"), "/"))
		}
	}
	return p
}

// indexCursorsByPath keys recovered cursors by their file path for tailer restore.
func indexCursorsByPath(cs []*corev1.CursorState) map[string]*corev1.CursorState {
	m := make(map[string]*corev1.CursorState, len(cs))
	for _, c := range cs {
		if c.GetPath() != "" {
			m[c.GetPath()] = c
		}
	}
	return m
}

// bootTimeMillis returns the machine boot time in unix millis, or 0 if
// unavailable (darwin/BSD kern.boottime).
func bootTimeMillis() int64 {
	tv, err := unix.SysctlTimeval("kern.boottime")
	if err != nil {
		return 0
	}
	return int64(tv.Sec)*1000 + int64(tv.Usec)/1000
}

func defaultCacheDir() string {
	if d := os.Getenv("XDG_CACHE_HOME"); d != "" {
		return filepath.Join(d, "agent-repl")
	}
	home, err := os.UserHomeDir()
	if err != nil {
		home = os.TempDir()
	}
	return filepath.Join(home, ".cache", "agent-repl")
}
