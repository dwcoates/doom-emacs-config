package dlog

import (
	"fmt"
	"os"
	"path/filepath"
	"sync"
	"time"
)

// scanInterval is how often the periodic cap scan runs. The daemon's own
// writes are capped synchronously; this scan exists for the bytes the shim
// writes straight to the same inode through inherited fd 3, which the daemon
// never sees go by.
const scanInterval = 30 * time.Second

// surfaces is the Surfaces implementation: the run log, the one terminal
// mirror, and the per-workspace sink map that is this runtime's memory of
// which target each canonical link names.
type surfaces struct {
	runLog  *runLog
	mirror  *mirror
	verbose bool
	pid     int
	now     func() time.Time

	mu         sync.Mutex
	workspaces map[string]*workspaceSinks
	closed     bool

	scanEvery time.Duration
	stop      chan struct{}
	stopOnce  sync.Once
	scanDone  chan struct{}
}

// workspaceSinks are one workspace's durable sinks, opened lazily: a
// workspace that never spawns a shim never grows a shim.log.
type workspaceSinks struct {
	dir   string
	id    string
	sinks map[string]*sink
}

// OpenSurfaces opens the daemon's log surfaces under the state root's logs
// directory. runLog is the restart-scoped run log path; verbose gates the
// terminal mirror for verbose records.
//
// The run log's open failure is returned here and is a BOOT FATAL for the
// caller: a daemon that cannot write its own narrative cannot report what it
// then does wrong.
func OpenSurfaces(runLog string, verbose bool) (Surfaces, error) {
	return openSurfaces(runLog, verbose, os.Stderr)
}

// openSurfaces is OpenSurfaces with the terminal injected, which is how the
// mirror's decoupling is tested.
func openSurfaces(runLogPath string, verbose bool, terminal interface{ Write([]byte) (int, error) }) (*surfaces, error) {
	rl, err := openRunLog(runLogPath, RunLogBackups)
	if err != nil {
		return nil, fmt.Errorf("open the daemon run log (boot fatal): %w", err)
	}
	s := &surfaces{
		runLog:     rl,
		mirror:     newMirror(terminal, mirrorDepth),
		verbose:    verbose,
		pid:        os.Getpid(),
		now:        time.Now,
		workspaces: make(map[string]*workspaceSinks),
		scanEvery:  scanInterval,
		stop:       make(chan struct{}),
		scanDone:   make(chan struct{}),
	}
	go s.scanLoop()
	return s, nil
}

// Global is the service logger, backed by the restart-scoped run log. The
// state root layout names no second global file, so the run log IS the global
// sink. Only events with no conceptual workspace may use it.
func (s *surfaces) Global() Logger {
	return &logger{s: s, dest: s.runLog, runtime: RuntimeDaemon}
}

// Workspace resolves the logger whose durable sink is the canonical
// <dir>/.claude/emacs/daemon.log. It fails rather than falling back to the
// global sink.
func (s *surfaces) Workspace(dir string) (Logger, error) {
	ws, sk, err := s.resolve(dir, "daemon")
	if err != nil {
		return nil, err
	}
	return &logger{
		s:       s,
		dest:    sk,
		runtime: RuntimeDaemon,
		base:    Context{KeyWorkspaceDir: ws.dir, KeyWorkspaceID: ws.id},
	}, nil
}

// ShimSink borrows the already-open shim log sink for one workspace, to be
// passed as the spawned shim's fd 3. The handle is non-closeable.
func (s *surfaces) ShimSink(dir string) (Borrowed, error) {
	ws, sk, err := s.resolve(dir, "shim")
	if err != nil {
		return nil, err
	}
	if poison := sk.poisoned(); poison != nil {
		return nil, poison
	}
	warnLog, err := s.Workspace(ws.dir)
	if err != nil {
		return nil, err
	}
	return &borrowed{f: sk.file(), log: warnLog, name: "shim.log"}, nil
}

// ClientLog persists a console-less client's diagnostic record into that
// client's sink inside the owning workspace. The record keeps the client's
// own runtime and its own identity; the daemon only converts the foreign
// timestamp into the local zone every runtime's records are compared in.
func (s *surfaces) ClientLog(dir string, rec ClientRecord) error {
	name, runtime, err := clientSink(rec.ClientKind)
	if err != nil {
		return err
	}
	if !validLevel(rec.Level) {
		return fmt.Errorf("client record level %q is not one of debug, info, warn, error", rec.Level)
	}
	if rec.Operation == "" {
		return fmt.Errorf("client record operation is empty")
	}
	ws, sk, err := s.resolve(dir, name)
	if err != nil {
		return err
	}
	at, stamped, err := clientInstant(rec.Timestamp, s.now)
	if err != nil {
		return err
	}
	ctx := merge(rec.Context, Context{
		KeyWorkspaceDir: ws.dir,
		KeyWorkspaceID:  ws.id,
	})
	if stamped {
		// The client sent no instant; say whose clock the timestamp is,
		// rather than let it read as the client's own.
		ctx["timestamp_source"] = "daemon_arrival"
	}
	// pid is deliberately 0 (omitted): a forwarded record carries the sending
	// runtime's identity, and the daemon's pid would misattribute it.
	out := newRecord(at, runtime, rec.Level, rec.Operation, rec.Message, ctx, 0)
	line := out.marshal()
	if err := sk.write(line); err != nil {
		return err
	}
	if out.Verbosity != VerbosityVerbose || s.verbose {
		s.mirror.enqueue(line)
	}
	return nil
}

// Evict closes one workspace's sinks when the workspace closes. The canonical
// links and their targets stay on disk: a reader keeps resolving them, and a
// later runtime makes its own target rather than trusting this one.
func (s *surfaces) Evict(dir string) error {
	clean, err := cleanDir(dir)
	if err != nil {
		return err
	}
	s.mu.Lock()
	ws, ok := s.workspaces[clean]
	delete(s.workspaces, clean)
	s.mu.Unlock()
	if !ok {
		return nil
	}
	return closeWorkspace(ws)
}

// Close flushes and closes every sink the daemon opened.
func (s *surfaces) Close() error {
	s.mu.Lock()
	if s.closed {
		s.mu.Unlock()
		return nil
	}
	s.closed = true
	workspaces := make([]*workspaceSinks, 0, len(s.workspaces))
	for _, ws := range s.workspaces {
		workspaces = append(workspaces, ws)
	}
	s.workspaces = make(map[string]*workspaceSinks)
	s.mu.Unlock()

	s.stopOnce.Do(func() { close(s.stop) })
	<-s.scanDone

	var firstErr error
	for _, ws := range workspaces {
		if err := closeWorkspace(ws); err != nil && firstErr == nil {
			firstErr = err
		}
	}
	if err := s.runLog.close(); err != nil && firstErr == nil {
		firstErr = err
	}
	s.mirror.close()
	return firstErr
}

// resolve finds (or opens) one named sink of one workspace.
func (s *surfaces) resolve(dir, name string) (*workspaceSinks, *sink, error) {
	clean, err := cleanDir(dir)
	if err != nil {
		return nil, nil, err
	}
	info, err := os.Stat(clean)
	if err != nil {
		return nil, nil, fmt.Errorf("resolve workspace %q for its log sink: %w", clean, err)
	}
	if !info.IsDir() {
		return nil, nil, fmt.Errorf("resolve workspace %q for its log sink: not a directory", clean)
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.closed {
		return nil, nil, fmt.Errorf("log surfaces are closed")
	}
	ws, ok := s.workspaces[clean]
	if !ok {
		id, err := LogWorkspaceID(clean)
		if err != nil {
			return nil, nil, err
		}
		ws = &workspaceSinks{dir: clean, id: id, sinks: make(map[string]*sink, len(SinkNames))}
		s.workspaces[clean] = ws
	}
	if sk, ok := ws.sinks[name]; ok {
		return ws, sk, nil
	}
	sk, err := openSink(ws.dir, ws.id, name)
	if err != nil {
		return nil, nil, fmt.Errorf("open %s.log for workspace %s: %w", name, ws.id, err)
	}
	ws.sinks[name] = sk
	return ws, sk, nil
}

// scanLoop runs the periodic cap scan until Close.
func (s *surfaces) scanLoop() {
	defer close(s.scanDone)
	t := time.NewTicker(s.scanEvery)
	defer t.Stop()
	for {
		select {
		case <-t.C:
			s.scanOnce()
		case <-s.stop:
			return
		}
	}
}

// scanOnce enforces the cap on every open sink from the file's real size,
// which is the only way the shim's fd-3 writes are ever counted.
func (s *surfaces) scanOnce() {
	type entry struct {
		ws   *workspaceSinks
		name string
		sk   *sink
	}
	s.mu.Lock()
	entries := make([]entry, 0, len(s.workspaces)*len(SinkNames))
	for _, ws := range s.workspaces {
		for name, sk := range ws.sinks {
			entries = append(entries, entry{ws: ws, name: name, sk: sk})
		}
	}
	s.mu.Unlock()
	for _, e := range entries {
		if err := e.sk.scan(); err != nil {
			s.reportSinkFailure(e.ws, e.name, err)
		}
	}
}

// reportSinkFailure records a poisoned sink as a workspace-attributed error.
// It goes to that workspace's daemon.log, never to the global sink; if the
// daemon.log is itself the poisoned sink, the emergency output is the one
// permitted exception, because the canonical sink cannot record its own
// failure.
func (s *surfaces) reportSinkFailure(ws *workspaceSinks, name string, cause error) {
	rec := newRecord(s.now(), RuntimeDaemon, LevelError,
		"daemon.dlog.sink_poisoned",
		"a workspace log sink failed cap maintenance and stopped accepting records",
		Context{
			KeyWorkspaceDir: ws.dir,
			KeyWorkspaceID:  ws.id,
			"sink":          name + ".log",
			"cause":         cause.Error(),
		}, s.pid)
	line := rec.marshal()
	s.mirror.enqueue(line)

	s.mu.Lock()
	daemonSink := ws.sinks["daemon"]
	s.mu.Unlock()
	if daemonSink == nil {
		emergency(cause, line)
		return
	}
	if err := daemonSink.write(line); err != nil {
		emergency(err, line)
	}
}

// closeWorkspace closes every sink of one workspace.
func closeWorkspace(ws *workspaceSinks) error {
	var firstErr error
	for _, sk := range ws.sinks {
		if err := sk.close(); err != nil && firstErr == nil {
			firstErr = err
		}
	}
	return firstErr
}

// cleanDir is the one spelling of a workspace directory used as a map key and
// as the workspace_id input, so a record and a lock file agree.
func cleanDir(dir string) (string, error) {
	if dir == "" {
		return "", fmt.Errorf("workspace directory is empty: a workspace-owned record has no sink to resolve")
	}
	abs, err := filepath.Abs(dir)
	if err != nil {
		return "", fmt.Errorf("resolve workspace directory %q: %w", dir, err)
	}
	return filepath.Clean(abs), nil
}

// clientSink maps a client kind onto the sink it owns and the runtime its
// records are written under. emacs is absent deliberately: Emacs owns
// emacs.log and the daemon never writes it.
func clientSink(kind string) (name, runtime string, err error) {
	switch kind {
	case RuntimeWebapp:
		return "webapp", RuntimeWebapp, nil
	case RuntimeSidecar:
		return "sidecar", RuntimeSidecar, nil
	default:
		return "", "", fmt.Errorf(
			"client kind %q has no daemon-owned sink: the daemon persists only webapp and sidecar records (Emacs owns emacs.log, the shim writes shim.log itself)",
			kind)
	}
}

// validLevel reports whether a client-supplied level is one of the four.
func validLevel(level string) bool {
	switch level {
	case LevelDebug, LevelInfo, LevelWarn, LevelError:
		return true
	default:
		return false
	}
}

// clientInstant parses a forwarded record's timestamp. A foreign runtime may
// send a UTC instant or any other offset; it is parsed as ordinary RFC 3339
// and rendered in the local zone, so a webapp line interleaves with the daemon
// lines around it. An unparseable timestamp is an error, never a silently
// substituted one.
func clientInstant(raw string, now func() time.Time) (at time.Time, stamped bool, err error) {
	if raw == "" {
		return now(), true, nil
	}
	parsed, perr := time.Parse(time.RFC3339, raw)
	if perr != nil {
		return time.Time{}, false, fmt.Errorf("parse client record timestamp %q as RFC 3339: %w", raw, perr)
	}
	return parsed.Local(), false, nil
}
