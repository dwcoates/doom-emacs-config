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

	sharedlogging "agentrepl/logging"
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
			"timestamp": sharedlogging.Timestamp(time.Now()),
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

func run(storeSocket string, roots []string, spoolRoot, logPath string) (err error) {
	logf, closeLog, err := openLogger(storeSocket, logPath)
	if err != nil {
		return err
	}
	defer closeLog()
	defer logProcessExit(logf, &err)

	sigc := make(chan os.Signal, 1)
	signal.Notify(sigc, syscall.SIGINT, syscall.SIGTERM)
	return runWithLogger(storeSocket, roots, spoolRoot, logf, sigc)
}

// logProcessExit is the sidecar's one deferred exit trace: whatever caused
// runWithLogger to return — a clean signal-driven shutdown, a runtime
// failure, or a panic — is the last record this process writes, so a
// truncated log still names why the process is gone.
//
// It re-panics after logging rather than recovering: a panic here is an
// invariant violation, and this trace exists to narrate the crash, not to
// turn it into a normal exit.
func logProcessExit(logf *logging.Bound, err *error) {
	if r := recover(); r != nil {
		logf.With(logging.Context{Operation: "exit", Level: "error"}).Log("sidecar exiting: panic: %v", r)
		panic(r)
	}
	if *err != nil {
		logf.With(logging.Context{Operation: "exit", Level: "error"}).Log("sidecar exiting: %v", *err)
		return
	}
	logf.With(logging.Context{Operation: "exit"}).Log("sidecar exiting cleanly")
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
		logf.With(logging.Context{Operation: "run", Level: "error"}).Log("sidecar stopped with error: %v", err)
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

const (
	// HeldDiagnosticSampleLimit bounds every spool-level diagnostic dimension.
	HeldDiagnosticSampleLimit = 8
	// ActiveUnresolvedSpoolThreshold makes any active attribution gap a
	// readiness failure while terminal historical spools remain informational.
	ActiveUnresolvedSpoolThreshold = 0
)

type sidecar struct {
	store     *storeclient.Client
	disc      *discover.Discoverer
	tracker   *stale.Tracker
	roots     []string
	spoolRoot string
	log       *logging.Bound

	watchers map[string]*watched // by path

	// owners maps a task id to the session that LAUNCHED it — the only session
	// identifier in the system, sourced from the transcript that announced the
	// task. A /tmp spool has no identity of its own (see internal/discover), so
	// this is what attributes one. Seeded per connection from the store's
	// authoritative open tasks and extended as transcripts are tailed.
	owners             map[string]string      // task id -> session id
	ownerSource        map[string]OwnerSource // task id -> authoritative source
	ownerTaskOutput    map[string]string      // task id -> exact output path when provided
	ownerByOutput      map[string]ownerRecord // normalized output path -> owner
	ownerPathConflicts map[string]bool        // normalized output path -> conflicting claims observed
	ownerConflicts     map[string]bool        // task id -> conflicting session claim observed
	openTasks          map[string]bool        // task id -> present in authoritative open-task state
	// held owns explicit active and terminal classifications for every spool
	// without an authoritative owner. A held or terminal spool is never tailed.
	held *HeldLifecycle

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
		store:              storeclient.New(storeSocket, log.With(logging.Context{Component: "storeclient"})),
		disc:               discover.New(roots, spoolRoot, log.With(logging.Context{Component: "discover"})),
		tracker:            stale.New(stale.Options{}, log.With(logging.Context{Component: "stale"})),
		roots:              roots,
		spoolRoot:          spoolRoot,
		log:                log,
		watchers:           map[string]*watched{},
		owners:             map[string]string{},
		ownerSource:        map[string]OwnerSource{},
		ownerTaskOutput:    map[string]string{},
		ownerByOutput:      map[string]ownerRecord{},
		ownerPathConflicts: map[string]bool{},
		ownerConflicts:     map[string]bool{},
		openTasks:          map[string]bool{},
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
	s.held = NewHeldLifecycle(HeldDiagnosticSampleLimit, s.reportHeldLifecycle)
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
		case sig := <-stop:
			// storeclient.Client.Close (storeclient/client.go) owns the
			// close-requested/closed-or-failed narration for the one teardown
			// step this shutdown has: the shim-store connection.
			s.log.With(logging.Context{Operation: "shutdown"}).Log("received signal=%s; beginning sidecar shutdown", sig)
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
		// The probe that FAILS here is what tears the link down and halts every
		// tail, so it is the cause of an ingestion outage, not a poll result.
		s.log.With(logging.Context{Operation: "health", RequestID: requestID, Level: "error"}).Log("health check failed: %v", err)
		s.noteStoreErr("health", err)
	}
}

// bootSweep LOSTs persisted open tasks that predate the machine boot. The
// tracker's set was restored from authoritative store lifecycle state before
// this runs; discovered artifacts are deliberately not evidence of liveness.
func (s *sidecar) bootSweep() {
	boot := bootTimeMillis()
	if boot == 0 {
		// Without a boot time the pre-boot sweep never runs, so tasks killed by
		// the reboot stay "running" in the GUI forever.
		s.log.With(logging.Context{Operation: "boot-sweep", Level: "warn"}).Log("boot time unavailable; pre-boot open tasks are not swept and stay running")
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
	now := time.Now()
	for _, tgt := range s.disc.Scan() {
		if _, ok := s.watchers[tgt.Path]; ok {
			continue
		}
		session, ok := s.resolveTargetOwner(tgt, now)
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
	s.held.Readiness(ActiveUnresolvedSpoolThreshold, now)
}

// resolveTargetOwner returns the session a target's events belong to.
//
// A config-root path names its own session (the transcript IS the session's
// record), so it answers immediately. A /tmp spool does not: its path states
// only where the bytes live, so its owner is looked up by task id against the
// launch the transcript announced. An unresolved spool is HELD — reported and
// left untailed — because the alternatives are inventing a session or reading
// the path's runtime id, and that id being mistaken for an identity is the bug
// this whole change removes.
func (s *sidecar) resolveTargetOwner(tgt discover.Target, now time.Time) (string, bool) {
	resolution := s.resolveOwnerResult(tgt)
	if tgt.SessionID != "" {
		if !resolution.Resolved() {
			panic(fmt.Sprintf("sidecar: config target %q did not resolve its path-owned session", tgt.Path))
		}
		return resolution.SessionID, true
	}
	info, err := os.Stat(tgt.Path)
	if err != nil {
		ctx := logging.Context{Operation: "classify-spool-lifecycle", Path: tgt.Path, Task: tgt.TaskID, Level: "error"}
		if os.IsNotExist(err) {
			ctx.Level = "debug"
		}
		s.log.With(ctx).Log("spool stat failed before lifecycle classification: %v", err)
		return "", false
	}
	path := normalizeOwnerOutputPath(tgt.Path)
	decision, err := s.held.Observe(
		HeldTarget{Path: path, Root: normalizeOwnerOutputPath(s.spoolRoot), TaskID: tgt.TaskID, ModTime: info.ModTime()},
		resolution,
		HeldEvidence{ModTime: info.ModTime(), ActiveTaskKnown: true, ActiveTask: s.taskOpen(tgt.TaskID)},
		now,
	)
	if err != nil {
		// HeldLifecycle owns the canonical error record with the hashed path and
		// evidence. The scan aborts this target without creating a tailer.
		return "", false
	}
	if decision.State != HeldStateResolved {
		return "", false
	}
	return decision.SessionID, true
}

// reportHeldLifecycle routes held-state instrumentation through the sidecar's
// canonical logger. Per-spool observations are verbose; bounded aggregate
// reports and rare owner resolutions remain normal records.
func (s *sidecar) reportHeldLifecycle(record HeldLogRecord) {
	message := fmt.Sprintf("state=%s reason=%s path_hash=%s root=%s age_bucket=%s %s",
		record.State, record.Reason, record.PathHash, record.Root, record.AgeBucket, record.Message)
	bound := s.log.With(logging.Context{Operation: record.Operation, Task: record.TaskID, Level: record.Level})
	if record.Verbose {
		bound.LogVerbose("%s", message)
		return
	}
	bound.Log("%s", message)
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
				// Any appended bytes past the committed offset went with the file.
				s.log.With(logging.Context{Operation: "vanished", Path: path, Session: w.sessionID, Task: w.target.TaskID, Level: "warn"}).Log("tail vanished, grace clock started; uncommitted appended bytes are unrecoverable")
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
			s.markTaskOpen(ts.GetTaskId(), OwnerSourceLiveLaunch)
			s.noteTaskOwner(ts.GetTaskId(), e.GetSessionId(), ts.GetOutputPath(), OwnerSourceLiveLaunch)
			s.tracker.Open(ts.GetTaskId(), taskKindToTail(ts.GetKind()), e.GetSessionId(), ts.GetOutputPath(), nowMs, nowMs)
		}
		if te := e.GetTaskEnded(); te != nil && te.GetStatus() != corev1.TerminalStatus_TERMINAL_STATUS_LOST {
			s.markTaskClosed(te.GetTaskId())
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
