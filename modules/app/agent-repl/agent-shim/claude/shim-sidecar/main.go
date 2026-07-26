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
	"flag"
	"fmt"
	"io"
	"os"
	"os/signal"
	"path/filepath"
	"strings"
	"sync"
	"syscall"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-claude-sidecar/internal/discover"
	"agentrepl/shim-claude-sidecar/internal/handler"
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
		fmt.Fprintln(os.Stderr, "shim-claude-sidecar:", err)
		os.Exit(1)
	}
}

func run(storeSocket string, roots []string, spoolRoot, logPath string) error {
	if err := os.MkdirAll(filepath.Dir(logPath), 0o755); err != nil {
		return fmt.Errorf("creating log dir: %w", err)
	}
	lf, err := os.OpenFile(logPath, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o644)
	if err != nil {
		return fmt.Errorf("opening log %q: %w", logPath, err)
	}
	defer lf.Close()
	logf := newLogger(io.MultiWriter(os.Stderr, lf))

	sc := newSidecar(storeSocket, roots, spoolRoot, logf)
	logf("starting: store=%s roots=%v spool=%s", storeSocket, roots, spoolRoot)

	sigc := make(chan os.Signal, 1)
	signal.Notify(sigc, syscall.SIGINT, syscall.SIGTERM)
	return sc.Run(sigc)
}

// ---------------------------------------------------------------------------
// Sidecar orchestration
// ---------------------------------------------------------------------------

type watched struct {
	target discover.Target
	tailer *tail.Tailer
}

type sidecar struct {
	store   *storeclient.Client
	disc    *discover.Discoverer
	tracker *stale.Tracker
	roots   []string
	log     handler.Logf

	handlers map[tail.Kind]tail.Handler
	watchers map[string]*watched // by path

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
}

func newSidecar(storeSocket string, roots []string, spoolRoot string, log handler.Logf) *sidecar {
	return &sidecar{
		store:   storeclient.New(storeSocket, log),
		disc:    discover.New(roots, spoolRoot, log),
		tracker: stale.New(stale.Options{}, log),
		roots:   roots,
		log:     log,
		handlers: map[tail.Kind]tail.Handler{
			tail.KindSessionTranscript: handler.NewSessionTranscriptHandler(log),
			tail.KindAgentTranscript:   handler.NewAgentTranscriptHandler(log),
			tail.KindWorkflowJournal:   handler.NewWorkflowJournalHandler(log),
			tail.KindShellSpool:        handler.NewShellOutputHandler(log),
		},
		watchers: map[string]*watched{},
		// A fresh sidecar is simply a sidecar whose link is not up yet, with its
		// first dial due immediately. That is all "boot" means here.
		link:      linkDown,
		downSince: time.Now(),
		dialT:     time.NewTimer(0),
	}
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
			s.log("received signal; shutting down")
			return s.store.Close()
		case <-s.dialT.C:
			s.dial()
		case <-rescanT.C:
			s.whenUp(s.rescan)
		case <-pollT.C:
			s.whenUp(s.pollAll)
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

// heartbeat pings the store, and treats a dead connection as a lost link rather
// than a log line.
func (s *sidecar) heartbeat() {
	if err := s.store.Heartbeat(); err != nil {
		s.log("heartbeat failed: %v", err)
		s.noteStoreErr("heartbeat", err)
	}
}

// bootSweep LOSTs discovered task files whose mtime predates the machine boot.
// Runs once per process, on the first established connection — the sweep is
// about MACHINE boot, but its LOST events need a store to land in.
func (s *sidecar) bootSweep() {
	boot := bootTimeMillis()
	if boot == 0 {
		s.log("boot time unavailable; skipping boot sweep")
		return
	}
	now := time.Now().UnixMilli()
	for _, tgt := range s.disc.Scan() {
		if tgt.TaskID == "" {
			continue
		}
		if fi, err := os.Stat(tgt.Path); err == nil {
			s.tracker.Open(tgt.TaskID, tgt.Kind, tgt.SessionID, tgt.SpoolDir, fi.ModTime().UnixMilli(), now)
		}
	}
	if ev := s.tracker.BootSweep(boot, now); len(ev) > 0 {
		s.log("boot sweep: %d pre-boot task(s) → LOST", len(ev))
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
	for _, tgt := range s.disc.Scan() {
		if _, ok := s.watchers[tgt.Path]; ok {
			continue
		}
		ctx := &tail.Context{
			SessionID: tgt.SessionID,
			Path:      tgt.Path,
			Kind:      tgt.Kind,
			TaskID:    tgt.TaskID,
			SpoolDir:  tgt.SpoolDir,
			RunID:     tgt.RunID,
		}
		tr := tail.New(tgt.Path, tgt.Codec(), s.handlers[tgt.Kind], ctx, s.log)
		if c := s.cursors[tgt.Path]; c != nil {
			tr.Restore(c)
		}
		s.watchers[tgt.Path] = &watched{target: tgt, tailer: tr}
		if tgt.TaskID != "" {
			if fi, err := os.Stat(tgt.Path); err == nil {
				s.tracker.Open(tgt.TaskID, tgt.Kind, tgt.SessionID, tgt.SpoolDir, fi.ModTime().UnixMilli(), time.Now().UnixMilli())
			}
		}
	}
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
					s.tracker.MarkVanished(w.target.TaskID, now)
				}
				delete(s.watchers, path)
				s.log("tail: %s vanished; grace clock started", path)
				continue
			}
			s.log("tail: poll %s failed: %v", path, err)
			continue
		}
		if !res.Changed {
			continue
		}
		if err := s.writeBatch(res); err != nil {
			// Honest sad path: do NOT commit; the batch replays and dedup absorbs.
			s.log("store write failed for %s (cursor NOT advanced, %d events dropped this cycle): %v",
				path, len(res.Events), err)
			if s.link != linkUp {
				// The write did not merely fail, it revealed a dead link. Abandon
				// the pass rather than reading the remaining files with nowhere
				// to put what they say.
				return
			}
			continue
		}
		w.tailer.Commit(res)
		if w.target.TaskID != "" {
			s.tracker.Activity(w.target.TaskID, now)
		}
		s.applyLifecycle(res.Events, now)
	}
}

// writeBatch sends one tailer batch (events + cursor advance) as a StoreWrite.
func (s *sidecar) writeBatch(res tail.PollResult) error {
	batch := &corev1.EventBatch{Events: res.Events, CursorAdvance: res.Next}
	return s.storeWrite("tailer batch", batch)
}

// applyLifecycle updates the stale tracker from a batch's lifecycle events: a
// TaskStarted opens a task, a real TaskEnded closes it (so it is never LOST-swept).
func (s *sidecar) applyLifecycle(events []*corev1.Event, nowMs int64) {
	for _, e := range events {
		if ts := e.GetTaskStarted(); ts != nil {
			s.tracker.Open(ts.GetTaskId(), taskKindToTail(ts.GetKind()), e.GetSessionId(), ts.GetOutputPath(), nowMs, nowMs)
		}
		if te := e.GetTaskEnded(); te != nil && te.GetStatus() != corev1.TerminalStatus_TERMINAL_STATUS_LOST {
			s.tracker.Close(te.GetTaskId())
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
		s.log("store write failed for %d synthetic event(s): %v", len(events), err)
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

func newLogger(w io.Writer) handler.Logf {
	var mu sync.Mutex
	return func(format string, args ...any) {
		ts := time.Now().Format("15:04:05.000")
		msg := fmt.Sprintf(format, args...)
		mu.Lock()
		fmt.Fprintf(w, "%s [shim-claude-sidecar] %s\n", ts, msg)
		mu.Unlock()
	}
}
