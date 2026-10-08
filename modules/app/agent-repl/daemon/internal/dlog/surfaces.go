package dlog

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"sync"
	"sync/atomic"
	"time"

	"agentrepl/logging"
	"claude-repld/internal/dirpath"
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
	runLog *runLog
	// logsDir is the state root's logs directory — the run log's own — and is
	// where every workspace sink's daemon-owned target is minted.
	logsDir string
	mirror  *mirror
	// window is the process's live level: a level other than info reverts to
	// info by itself when its window ends (see admits).
	window *logging.Window
	pid    int
	now    func() time.Time

	mu sync.Mutex
	// lookup resolves a workspace directory to its daemon-minted
	// ids.WorkspaceID. It is bound once, after the state client is open, and
	// every workspace record's workspace_id and every minted sink name comes
	// from it. Unbound, a workspace sink cannot be resolved at all: the
	// surfaces REFUSE rather than attribute a record to a path-derived
	// stand-in.
	lookup     WorkspaceIDLookup
	workspaces map[string]*workspaceSinks
	// targets remembers each workspace sink's daemon-owned file for this
	// runtime's whole lifetime, so an EVICTED sink that is re-opened keeps
	// appending to the file the canonical link already names rather than
	// minting a second one and orphaning everything written before.
	targets map[string]string
	closed  bool

	// dropWarnOnce guards the ONE stderr warning that says workspace records
	// arriving after Close are being dropped.
	dropWarnOnce sync.Once
	// centralFallbacks are the workspace directories already reported as
	// unable to host a durable sink of their own. The report is made ONCE per
	// directory: the condition is a property of the workspace, not of the
	// record that met it, and a per-record report is the error flood this set
	// exists to prevent.
	centralFallbacks map[string]struct{}
	// clientCentral are the workspace directories whose FORWARDED CLIENT
	// RECORDS already went to the central sink because the directory is gone.
	// Its own set, not centralFallbacks: that report is DEBUG narration of
	// the daemon's own loggers, and a set shared with it would let a DEBUG
	// note swallow the one INFO this condition is owed. An entry is dropped
	// when the directory resolves again (a restored worktree), so a second
	// disappearance is stated afresh.
	clientCentral map[string]struct{}
	// detached are the workspace directories the daemon is REMOVING (or has
	// removed), by clean path: DetachDir adds one before the removal starts
	// and AttachDir drops it once a worktree is created there again. A sink
	// of a detached directory creates nothing inside it -- see DetachDir.
	detached map[string]struct{}
	// keys maps each cleaned SPELLING of a workspace directory to the sink key
	// it resolves to: dirpath.Canonical's answer, the on-disk spelling with
	// its case. Two spellings of one directory used to be two entries sharing
	// ONE canonical link on disk, so each one's records were appended to the
	// target the other had installed (realtest 7, 2026-09-24,
	// attribution-conflict). The canonicalization reads the directory tree, so
	// it is paid once per spelling and remembered here.
	keys map[string]string
	// canonical is dirpath.Canonical, injectable so a test can model a
	// case-folding volume.
	canonical func(string) (string, error)

	// tee is the bound record tee, read on every workspace Warn and Error
	// without taking mu: a record can be emitted by a caller that the tee's
	// own consumer is waiting on, so the read must never contend.
	tee atomic.Pointer[teeBox]

	scanEvery time.Duration
	stop      chan struct{}
	stopOnce  sync.Once
	scanDone  chan struct{}
	shimRolls chan ShimRollRequest
}

// workspaceSinks are one workspace's durable sinks, opened lazily: a
// workspace that never spawns a shim never grows a shim.log.
type workspaceSinks struct {
	dir string
	// id is the daemon-minted ids.WorkspaceID, the record's workspace_id and
	// the name of every target minted for this workspace.
	id string
	// dirHash is md5hex(dir)[:8], the kernel lock file's derivation, recorded
	// as ordinary evidence under workspace_dir_hash.
	dirHash string
	sinks   map[string]*sink
	// detached says the directory is being removed, so every sink this entry
	// opens from here on writes its target with no canonical link.
	detached bool
}

// OpenSurfaces opens the daemon's log surfaces under the state root's logs
// directory. AGENT_REPL_LOG_LEVEL governs both persistence and the terminal
// mirror; a level other than info holds only inside the window
// AGENT_REPL_LOG_LEVEL_UNTIL names, and an invalid setting is a boot failure.
// A level that was asked for and not honored, or honored until a window's
// end, is stated at info in the run log.
//
// The run log's open failure is returned here and is a BOOT FATAL for the
// caller: a daemon that cannot write its own narrative cannot report what it
// then does wrong.
func OpenSurfaces(runLog string) (Surfaces, error) {
	sel, err := logging.SelectLevel(os.Getenv(LevelEnvironment), os.Getenv(logging.UntilEnvironment), time.Now())
	if err != nil {
		return nil, err
	}
	s, err := openSurfacesWindow(runLog, logging.NewWindow(sel, time.Now), os.Stderr)
	if err != nil {
		return nil, err
	}
	if message, ok := sel.Note(); ok {
		s.Global().Info(levelWindowOperation, message, Context(sel.Context()))
	}
	return s, nil
}

// openSurfaces is OpenSurfaces at one fixed level with the terminal injected,
// which is how the level filter and the mirror's decoupling are tested.
func openSurfaces(runLogPath, configuredLevel string, terminal interface{ Write([]byte) (int, error) }) (*surfaces, error) {
	level, err := parseLevel(configuredLevel)
	if err != nil {
		return nil, err
	}
	return openSurfacesWindow(runLogPath, logging.FixedWindow(level), terminal)
}

// openSurfacesWindow opens the surfaces on a live level window.
func openSurfacesWindow(runLogPath string, window *logging.Window, terminal interface{ Write([]byte) (int, error) }) (*surfaces, error) {
	if window == nil {
		panic("dlog: nil level window")
	}
	rl, err := openRunLog(runLogPath, RunLogBackups)
	if err != nil {
		return nil, fmt.Errorf("open the daemon run log (boot fatal): %w", err)
	}
	s := &surfaces{
		runLog:     rl,
		logsDir:    filepath.Dir(runLogPath),
		mirror:     newMirror(terminal, mirrorDepth),
		window:     window,
		pid:        os.Getpid(),
		now:        time.Now,
		workspaces: make(map[string]*workspaceSinks),
		targets:    make(map[string]string),
		detached:   make(map[string]struct{}),
		keys:       make(map[string]string),
		canonical:  dirpath.Canonical,
		scanEvery:  scanInterval,
		stop:       make(chan struct{}),
		scanDone:   make(chan struct{}),
		shimRolls:  make(chan ShimRollRequest, 128),
	}
	go s.scanLoop()
	return s, nil
}

// BindWorkspaceIDs installs the minted-workspace-id lookup. The daemon calls
// it once the state client is open; until then only Global records are
// possible, because Workspace, ShimSink and ClientLog all need the id.
func (s *surfaces) BindWorkspaceIDs(lookup WorkspaceIDLookup) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.lookup = lookup
}

// Global is the service logger, backed by the size-rotated run log. The
// state root layout names no second global file, so the run log IS the global
// sink. Only events with no conceptual workspace may use it.
func (s *surfaces) Global() Logger {
	return &logger{s: s, dest: s.runLog, runtime: RuntimeDaemon}
}

// errWorkspaceDirGone marks a resolve whose workspace directory does not
// exist. It is a sentinel, and not a test for fs.ErrNotExist on whatever
// resolve returned, because a sink open failing with ENOENT (a logs directory
// that went away) is a broken sink, not a vanished workspace: only the stat of
// the workspace directory itself may say "gone".
var errWorkspaceDirGone = errors.New("the workspace directory is gone")

// errSurfacesClosed is resolve's refusal once Close has run. It is a sentinel
// because Workspace has to tell "this daemon is shutting down" apart from
// every other resolution failure.
var errSurfacesClosed = errors.New("log surfaces are closed")

// droppedSink is the destination a workspace logger gets when the surfaces are
// ALREADY CLOSED: every record is discarded, and the first one says so on
// stderr. LOGGING MAY NEVER FAIL AN RPC -- a late request that arrives while
// the daemon tears down still has to reach its handler and get the handler's
// own typed answer, so a closed sink costs the record, never the answer.
type droppedSink struct{ s *surfaces }

func (d droppedSink) write(line []byte) error {
	d.s.dropWarnOnce.Do(func() {
		fmt.Fprintf(os.Stderr,
			"agent-repl daemon: LOG SURFACES CLOSED: workspace records arriving after Close are dropped\nfirst dropped record: %s",
			line)
	})
	return nil
}

// workspaceDest routes one workspace's records to that workspace's sink BY
// DIRECTORY, resolving the sink at write time instead of pinning the *sink the
// logger was built from.
//
// THE PIN WAS THE DEFECT. Long-lived components -- the topbar's publisher, the
// shim client supervisor, the stop path -- hold a workspace logger for as long
// as they run, and Evict releases the sink they were built on. Every record
// they wrote afterwards met a released handle, and eviction poisoned it, so
// ordinary INFO records about the workspace's own teardown came back as
// sink_failure errors. Resolving here instead lets openSink's remembered
// target do what it was written for: the sink re-opens and appends to the same
// file the canonical link already names.
type workspaceDest struct {
	s *surfaces
	// dir is already cleanDir'd, and is the surfaces' own map key.
	dir  string
	name string
}

// write resolves the sink and appends. The two ways resolution can fail are
// both ordinary and neither is an error record:
//
//   - the surfaces are CLOSED, and the record is dropped with the one stderr
//     warning droppedSink documents, because the run log is closed too;
//   - the workspace DIRECTORY is gone -- a nuke, or a merge that removed the
//     worktree -- which is exactly the condition WorkspaceOrCentral names: the
//     record lands in the central sink and the condition is reported once per
//     directory at DEBUG.
func (d workspaceDest) write(line []byte) error {
	_, sk, err := d.s.resolve(d.dir, d.name)
	if err == nil {
		return sk.write(line)
	}
	if errors.Is(err, errSurfacesClosed) {
		return droppedSink{s: d.s}.write(line)
	}
	d.s.noteCentralFallback(d.dir, err)
	return d.s.runLog.write(line)
}

// Workspace resolves the logger whose durable sink is the canonical
// <dir>/.claude/emacs/daemon.log. It fails rather than falling back to the
// global sink -- EXCEPT once the surfaces are closed, where it hands back a
// dropping logger so a request racing the daemon's shutdown is answered by its
// handler instead of failing inside logging.
func (s *surfaces) Workspace(dir string) (Logger, error) {
	ws, _, err := s.resolve(dir, "daemon")
	if errors.Is(err, errSurfacesClosed) {
		clean, cerr := cleanDir(dir)
		if cerr != nil {
			return nil, cerr
		}
		hash, herr := WorkspaceDirHash(clean)
		if herr != nil {
			return nil, herr
		}
		return &logger{
			s:       s,
			dest:    droppedSink{s: s},
			runtime: RuntimeDaemon,
			base:    Context{KeyWorkspaceDir: clean, KeyWorkspaceDirHash: hash},
		}, nil
	}
	if err != nil {
		return nil, err
	}
	// The sink resolve just opened is NOT captured: the destination re-resolves
	// by directory on every record, so a logger that outlives an eviction goes
	// on writing to the workspace's own log. Resolving here still refuses a
	// directory that cannot host a sink, which is the contract this surface's
	// callers depend on.
	return &logger{
		s:           s,
		dest:        workspaceDest{s: s, dir: ws.dir, name: "daemon"},
		runtime:     RuntimeDaemon,
		base:        Context{KeyWorkspaceDir: ws.dir, KeyWorkspaceID: ws.id, KeyWorkspaceDirHash: ws.dirHash},
		workspaceID: ws.id,
	}, nil
}

// teeBox holds the bound record tee so it can be swapped atomically.
type teeBox struct{ tee RecordTee }

// BindRecordTee installs the record tee every workspace logger hands its Warn
// and Error records to. A nil tee unbinds it.
func (s *surfaces) BindRecordTee(tee RecordTee) {
	s.tee.Store(&teeBox{tee: tee})
}

// WorkspaceOrCentral answers the logger for a workspace's records and is
// TOTAL: it always returns a logger.
//
// Workspace REFUSES when the directory cannot host a sink, and that refusal
// stays: a caller that must not proceed without the workspace's own durable
// sink still gets told. This surface is for the callers that merely RENDER,
// SWEEP or BOUND a workspace — work whose whole point is to keep running over
// every workspace the registry holds. For them a workspace whose directory is
// a scratch path, has been deleted, or does not exist yet is an ORDINARY
// outcome: the record goes to the central sink with `unroutable_workspace`
// naming the workspace it is about, and the condition is reported ONCE per
// directory at DEBUG rather than as an error beside every record.
func (s *surfaces) WorkspaceOrCentral(dir string) Logger {
	log, err := s.Workspace(dir)
	if err == nil {
		return log
	}
	s.noteCentralFallback(dir, err)
	return s.Global().With(Context{
		KeyWorkspaceDir:        dir,
		KeyUnroutableWorkspace: dir,
	})
}

// noteCentralFallback reports one workspace's move to the central sink, the
// first time that workspace moves there. The lock is released before the
// record is written so the write cannot re-enter the surfaces' own mutex.
func (s *surfaces) noteCentralFallback(dir string, cause error) {
	key := dir
	if clean, err := cleanDir(dir); err == nil {
		key = clean
	}
	s.mu.Lock()
	if s.centralFallbacks == nil {
		s.centralFallbacks = make(map[string]struct{})
	}
	_, reported := s.centralFallbacks[key]
	s.centralFallbacks[key] = struct{}{}
	s.mu.Unlock()
	if reported {
		return
	}
	s.Global().Debug("daemon.dlog.central_fallback",
		"the workspace cannot host a durable sink; its records go to the central sink",
		Context{
			KeyWorkspaceDir:        dir,
			KeyUnroutableWorkspace: dir,
			"cause":                cause.Error(),
		})
}

// ShimSink borrows the already-open shim log sink for one workspace, to be
// passed as the spawned shim's fd 3. The handle is non-closeable.
func (s *surfaces) ShimSink(dir string) (Borrowed, error) {
	ws, sk, err := s.resolve(dir, "shim")
	if err != nil {
		return nil, err
	}
	rolled, err := sk.rotateShim()
	if err != nil {
		return nil, err
	}
	warnLog, err := s.Workspace(ws.dir)
	if err != nil {
		return nil, err
	}
	if rolled {
		warnLog.Info("daemon.dlog.shim_rotated", "rotated shim.log while rolling the workspace's shim", Context{
			"target":     sk.target,
			"generation": sk.target + ".1",
		})
	}
	f, err := sk.borrowFile()
	if err != nil {
		return nil, err
	}
	return &borrowed{f: f, log: warnLog, name: "shim.log"}, nil
}

// ShimRollRequests exposes the hard-ceiling requests emitted by the cap scan.
func (s *surfaces) ShimRollRequests() <-chan ShimRollRequest { return s.shimRolls }

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
	if !s.admits(rec.Level) {
		return nil
	}
	at, stamped, err := clientInstant(rec.Timestamp, s.now)
	if err != nil {
		return err
	}
	// A WORKSPACE WHOSE DIRECTORY IS GONE STILL OWNS ITS RECORDS. A client
	// (the sidecar tailing a transcript, a webapp tab left open) can go on
	// reporting about a workspace whose worktree was deleted underneath it;
	// the record has no per-workspace sink to land in, and refusing it failed
	// the rpc at ERROR for every record while the client retried and wrote it
	// to its own global sink at WARN. It is not a fault of the rpc: the
	// record lands in the CENTRAL sink, still naming its workspace, and the
	// condition is stated once per workspace. Every other resolve failure
	// stays a failure.
	var dest interface{ write([]byte) error }
	var who workspaceIdentity
	central := false
	ws, sk, err := s.resolve(dir, name)
	switch {
	case err == nil:
		dest, who = sk, workspaceIdentity{dir: ws.dir, id: ws.id, dirHash: ws.dirHash}
	case errors.Is(err, errWorkspaceDirGone):
		gone, gerr := s.goneWorkspace(dir)
		if gerr != nil {
			return gerr
		}
		s.noteClientCentral(gone, rec.ClientKind, err)
		dest, who, central = s.runLog, gone, true
	default:
		return err
	}
	fields := Context{
		KeyWorkspaceDir:     who.dir,
		KeyWorkspaceID:      who.id,
		KeyWorkspaceDirHash: who.dirHash,
	}
	if central {
		fields[KeyUnroutableWorkspace] = who.dir
	}
	ctx := merge(rec.Context, fields)
	if stamped {
		// The client sent no instant; say whose clock the timestamp is,
		// rather than let it read as the client's own.
		ctx["timestamp_source"] = "daemon_arrival"
	}
	// pid is deliberately 0 (omitted): a forwarded record carries the sending
	// runtime's identity, and the daemon's pid would misattribute it.
	verbosity := VerbosityNormal
	if rec.Verbose {
		verbosity = VerbosityVerbose
	}
	out := newRecordWithVerbosity(at, runtime, rec.Level, verbosity, rec.Operation, rec.Message, ctx, 0)
	line := out.marshal()
	if err := dest.write(line); err != nil {
		return err
	}
	s.mirror.enqueue(line)
	return nil
}

// goneWorkspace answers the identity a record about a workspace whose
// directory is gone carries: its sink key, its minted id and its directory
// hash, exactly as a resolvable one's records carry them. NOTHING IS OPENED
// AND NOTHING IS REMEMBERED: the entry is not added to the sink map, so the
// directory is stat-ed afresh on the next record and a restored worktree gets
// its own sinks back. A workspace the registry cannot name is still a refusal.
func (s *surfaces) goneWorkspace(dir string) (workspaceIdentity, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	clean, err := s.sinkKeyLocked(dir)
	if err != nil {
		return workspaceIdentity{}, err
	}
	return s.identityLocked(clean)
}

// noteClientCentral states, ONCE per workspace directory, that its forwarded
// client records go to the central sink because the directory is gone. It is
// INFO: a workspace whose worktree was removed is an ordinary state of the
// registry, not a fault, and the per-record report is the flood this exists
// to prevent. The lock is released before the record is written so the write
// cannot re-enter the surfaces' own mutex.
func (s *surfaces) noteClientCentral(ws workspaceIdentity, clientKind string, cause error) {
	s.mu.Lock()
	if s.clientCentral == nil {
		s.clientCentral = make(map[string]struct{})
	}
	_, reported := s.clientCentral[ws.dir]
	s.clientCentral[ws.dir] = struct{}{}
	s.mu.Unlock()
	if reported {
		return
	}
	s.Global().Info("daemon.dlog.client_central_fallback",
		"the workspace's directory is gone; its forwarded client records go to the central sink",
		Context{
			KeyWorkspaceDir:        ws.dir,
			KeyWorkspaceID:         ws.id,
			KeyUnroutableWorkspace: ws.dir,
			"client_kind":          clientKind,
			"cause":                cause.Error(),
		})
}

// Evict closes one workspace's sinks when the workspace closes. The canonical
// links and their targets stay on disk: a reader keeps resolving them, and a
// later runtime makes its own target rather than trusting this one.
func (s *surfaces) Evict(dir string) error {
	s.mu.Lock()
	clean, err := s.sinkKeyLocked(dir)
	if err != nil {
		s.mu.Unlock()
		return err
	}
	ws, ok := s.workspaces[clean]
	delete(s.workspaces, clean)
	s.mu.Unlock()
	if !ok {
		return nil
	}
	return closeWorkspace(ws)
}

// DetachDir tells the surfaces that the daemon is about to REMOVE a workspace
// directory. From the moment it returns, no sink of that directory creates,
// re-points or reads anything inside it: the sinks already open forget their
// canonical links, and every sink opened later writes its daemon-owned target
// under logsDir with no link at all. Records keep flowing to the same targets.
//
// IT EXISTS BECAUSE A SINK OPEN RE-CREATED A REMOVED WORKTREE. A merge's
// teardown ran `git worktree remove --force`, a late sidecar record for the
// merged workspace opened its first `sidecar.log` one instant later, and the
// open's MkdirAll of `<worktree>/.claude/emacs` brought the directory back:
// the teardown's own check then found the worktree "still present after
// removal" and recorded two ERRORs (TestHandoverTransfersAtFreeness,
// 2026-09-23, 7ms apart). The mark is taken under the same mutex every sink
// open holds, so a link is either created before the removal starts — and is
// removed with the directory — or never.
func (s *surfaces) DetachDir(dir string) error {
	s.mu.Lock()
	clean, err := s.sinkKeyLocked(dir)
	if err != nil {
		s.mu.Unlock()
		return err
	}
	s.detached[clean] = struct{}{}
	ws, open := s.workspaces[clean]
	sinks := 0
	if open {
		ws.detached = true
		for _, sk := range ws.sinks {
			sk.detach()
			sinks++
		}
	}
	s.mu.Unlock()
	s.Global().Debug("daemon.dlog.dir_detached",
		"a workspace directory the daemon is removing was detached from its log sinks; nothing is created inside it from here on",
		Context{KeyWorkspaceDir: clean, "open_sinks": sinks})
	return nil
}

// AttachDir lifts a DetachDir once a worktree has been created at the same
// path again, so the new workspace's sinks carry their canonical links. An
// entry left from the removed workspace is dropped with its descriptors: the
// directory now belongs to a workspace that must resolve afresh.
func (s *surfaces) AttachDir(dir string) error {
	s.mu.Lock()
	clean, err := s.sinkKeyLocked(dir)
	if err != nil {
		s.mu.Unlock()
		return err
	}
	_, was := s.detached[clean]
	delete(s.detached, clean)
	var stale *workspaceSinks
	if ws, ok := s.workspaces[clean]; ok && ws.detached {
		stale = ws
		delete(s.workspaces, clean)
	}
	s.mu.Unlock()
	if !was {
		return nil
	}
	s.Global().Debug("daemon.dlog.dir_attached",
		"a worktree was created at a detached workspace directory; its sinks link into it again",
		Context{KeyWorkspaceDir: clean, "dropped_entry": stale != nil})
	if stale != nil {
		return closeWorkspace(stale)
	}
	return nil
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
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.closed {
		return nil, nil, errSurfacesClosed
	}
	clean, err := s.sinkKeyLocked(dir)
	if err != nil {
		return nil, nil, err
	}
	ws, ok := s.workspaces[clean]
	_, detached := s.detached[clean]
	if !ok && detached {
		// A DETACHED DIRECTORY IS NOT STAT-ED: the daemon is removing it, so
		// its absence is the expected answer and not a reason to refuse. Its
		// records still reach the workspace's own targets under logsDir.
		entry, err := s.newWorkspaceEntryLocked(clean, true)
		if err != nil {
			return nil, nil, err
		}
		ws, ok = entry, true
	}
	if !ok {
		// THE DIRECTORY IS STAT-ED ONCE, ON THE FIRST RESOLVE, AND NEVER
		// AGAIN. A workspace already carrying open sinks keeps them: the
		// records live in logsDir and the workspace holds only a symlink to
		// them, so a directory that goes away underneath an open sink costs
		// nothing. The daemon REMOVES that directory itself when a merge
		// lands, and re-stat-ing here made every later per-workspace rpc on
		// the merged workspace -- the footer and the feed its own roster
		// still lists under recently_merged -- fail with "no such file or
		// directory" and record an ERROR for a teardown the daemon ordered.
		info, err := os.Stat(clean)
		if errors.Is(err, fs.ErrNotExist) {
			return nil, nil, fmt.Errorf("resolve workspace %q for its log sink: %w: %w", clean, errWorkspaceDirGone, err)
		}
		if err != nil {
			return nil, nil, fmt.Errorf("resolve workspace %q for its log sink: %w", clean, err)
		}
		if !info.IsDir() {
			return nil, nil, fmt.Errorf("resolve workspace %q for its log sink: not a directory", clean)
		}
		entry, err := s.newWorkspaceEntryLocked(clean, false)
		if err != nil {
			return nil, nil, err
		}
		ws = entry
	}
	if sk, ok := ws.sinks[name]; ok {
		return ws, sk, nil
	}
	if !ws.detached {
		// A NEW SINK OF A LINKED WORKSPACE IS A MkdirAll INSIDE ITS DIRECTORY,
		// so it is never opened once that directory is gone: the open would
		// RESURRECT the deleted worktree as a bare `.claude/emacs` tree, and a
		// later OpenWorkspace would then find a directory, skip restoring the
		// worktree, and spawn a shim in a directory that is no checkout. Sinks
		// already open are untouched (their records live in logsDir), which
		// is why this is checked only here and not on every resolve.
		if _, err := os.Stat(ws.dir); errors.Is(err, fs.ErrNotExist) {
			return nil, nil, fmt.Errorf("resolve workspace %q for its %s log sink: %w: %w", ws.dir, name, errWorkspaceDirGone, err)
		}
	}
	key := ws.id + "/" + name
	remembered := s.targets[key] != ""
	sk, err := openSink(s.logsDir, ws.dir, ws.id, name, s.targets[key], !ws.detached)
	if err != nil {
		return nil, nil, fmt.Errorf("open %s.log for workspace %s: %w", name, ws.id, err)
	}
	s.targets[key] = sk.target
	ws.sinks[name] = sk
	s.reportSinkOpened(ws, name, sk, remembered)
	return ws, sk, nil
}

// newWorkspaceEntryLocked mints and records one workspace's sink entry: its
// minted id, its directory hash, and whether its directory is detached. It is
// the ONE place an entry is built, for a detached directory and a live one alike.
func (s *surfaces) newWorkspaceEntryLocked(clean string, detached bool) (*workspaceSinks, error) {
	who, err := s.identityLocked(clean)
	if err != nil {
		return nil, err
	}
	id, hash := who.id, who.dirHash
	ws := &workspaceSinks{dir: clean, id: id, dirHash: hash, sinks: make(map[string]*sink, len(SinkNames)), detached: detached}
	s.workspaces[clean] = ws
	delete(s.clientCentral, clean)
	return ws, nil
}

// workspaceIdentity is what every record about a workspace carries: its sink
// key, its daemon-minted id and its directory hash.
type workspaceIdentity struct {
	dir, id, dirHash string
}

// identityLocked resolves one sink key's identity. It is the ONE derivation, for
// an entry the surfaces keep and for a gone directory they keep nothing for.
func (s *surfaces) identityLocked(clean string) (workspaceIdentity, error) {
	id, err := s.mintedIDLocked(clean)
	if err != nil {
		return workspaceIdentity{}, err
	}
	hash, err := WorkspaceDirHash(clean)
	if err != nil {
		return workspaceIdentity{}, err
	}
	return workspaceIdentity{dir: clean, id: id, dirHash: hash}, nil
}

// mintedIDLocked resolves a workspace directory to its daemon-minted
// ids.WorkspaceID. There is NO fallback: an unbound lookup and a lookup that
// cannot name the workspace are both refusals, recorded by the caller that
// asked for the sink, because a record attributed to a path-derived
// stand-in would split one workspace into two in every reader.
func (s *surfaces) mintedIDLocked(dir string) (string, error) {
	if s.lookup == nil {
		return "", fmt.Errorf(
			"resolve the minted workspace id for %q: no workspace id lookup is bound to the log surfaces", dir)
	}
	id, err := s.lookup(dir)
	if err != nil {
		return "", fmt.Errorf("resolve the minted workspace id for %q: %w", dir, err)
	}
	if id == "" {
		return "", fmt.Errorf("resolve the minted workspace id for %q: the lookup named no workspace", dir)
	}
	return id, nil
}

// reportSinkOpened records the id scheme every sink name and every record of
// this workspace carries, so a reader that meets an older md5-named target
// beside a newer minted-id one can tell from the log which is which.
func (s *surfaces) reportSinkOpened(ws *workspaceSinks, name string, sk *sink, remembered bool) {
	origin := "minted_target"
	switch {
	case remembered:
		origin = "runtime_memory"
	case !sk.mintedTarget:
		origin = "standing_canonical_link"
	}
	s.Global().Info("daemon.dlog.sink_opened", "opened a workspace log sink", Context{
		KeyWorkspaceDir:     ws.dir,
		KeyWorkspaceID:      ws.id,
		KeyWorkspaceDirHash: ws.dirHash,
		"sink":              name + ".log",
		"target":            sk.target,
		"target_origin":     origin,
		"id_scheme":         "daemon_minted_workspace_id",
	})
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
		result, err := e.sk.scan()
		if err != nil {
			s.reportSinkFailure(e.ws, e.name, err)
			continue
		}
		if result.marked {
			s.reportShimMarked(e.ws, e.sk, result.size)
		}
		if result.hardFirst {
			log, err := s.reportShimHardCeiling(e.ws, e.sk, result.size)
			if err != nil {
				e.sk.retryHardReport()
				continue
			}
			s.enqueueShimRoll(ShimRollRequest{
				Dir:       e.ws.dir,
				LogID:     e.ws.id,
				SizeBytes: result.size,
				HardBytes: hardCap(e.sk.cap),
				Log:       log,
			})
		}
	}
}

func (s *surfaces) reportShimMarked(ws *workspaceSinks, sk *sink, size int64) {
	log, err := s.Workspace(ws.dir)
	if err != nil {
		s.emergency(RuntimeDaemon, err, nil)
		return
	}
	log.Info("daemon.dlog.shim_rotation_marked", "marked shim.log to rotate with the workspace's next shim process", Context{
		"target": sk.target, "size_bytes": size, "cap_bytes": sk.cap,
	})
}

func (s *surfaces) reportShimHardCeiling(ws *workspaceSinks, sk *sink, size int64) (Logger, error) {
	log, err := s.Workspace(ws.dir)
	if err != nil {
		s.emergency(RuntimeDaemon, err, nil)
		return nil, fmt.Errorf("bind the hard-ceiling record to workspace %q: %w", ws.dir, err)
	}
	log.Error("daemon.dlog.shim_hard_ceiling", "shim.log reached its hard ceiling; forcing a shim roll at the next free turn boundary", Context{
		"target": sk.target, "size_bytes": size, "cap_bytes": sk.cap, "hard_ceiling_bytes": hardCap(sk.cap),
	})
	return log, nil
}

// enqueueShimRoll never drops a hard-ceiling request. Close unblocks a scan
// whose consumer has already stopped, so shutdown cannot hang behind logging.
func (s *surfaces) enqueueShimRoll(req ShimRollRequest) {
	select {
	case s.shimRolls <- req:
	case <-s.stop:
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
			KeyWorkspaceDir:     ws.dir,
			KeyWorkspaceID:      ws.id,
			KeyWorkspaceDirHash: ws.dirHash,
			"sink":              name + ".log",
			"cause":             cause.Error(),
		}, s.pid)
	line := rec.marshal()
	s.mirror.enqueue(line)

	s.mu.Lock()
	daemonSink := ws.sinks["daemon"]
	s.mu.Unlock()
	if daemonSink == nil {
		s.emergency(RuntimeDaemon, cause, line)
		return
	}
	if err := daemonSink.write(line); err != nil {
		s.emergency(RuntimeDaemon, err, line)
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

// sinkKeyLocked answers the sink key of one spelling of a workspace
// directory: the on-disk spelling dirpath.Canonical reads, remembered per
// spelling. A spelling it cannot canonicalize is refused, never keyed as
// given: keyed as given, it is exactly the second entry over one directory
// this key exists to make unrepresentable.
func (s *surfaces) sinkKeyLocked(dir string) (string, error) {
	clean, err := cleanDir(dir)
	if err != nil {
		return "", err
	}
	if key, ok := s.keys[clean]; ok {
		return key, nil
	}
	key, err := s.canonical(clean)
	if err != nil {
		return "", fmt.Errorf("resolve the sink key of workspace directory %q: %w", clean, err)
	}
	s.keys[clean] = key
	return key, nil
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
