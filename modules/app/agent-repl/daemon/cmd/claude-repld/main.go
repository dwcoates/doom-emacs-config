// Command claude-repld is the Go daemon between the Emacs/webapp
// frontend clients and per-session TS shim subprocesses. Wire formats:
// modules/app/agent-repl/proto/agentshim/ for the protobuf planes, and
// internal/protocol for the pre-cutover stdio/WebSocket NDJSON planes.
package main

import (
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"os/signal"
	"path/filepath"
	"strings"
	"sync"
	"sync/atomic"
	"syscall"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/frontend"
	"claude-repld/internal/keepalive"
	"claude-repld/internal/login"
	"claude-repld/internal/pprofsurface"
	"claude-repld/internal/progress"
	"claude-repld/internal/registry"
	"claude-repld/internal/replog"
	"claude-repld/internal/server"
	"claude-repld/internal/sessioncontroller"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/shim"
	"claude-repld/internal/shimlisten"
	"claude-repld/internal/ssm"
	"claude-repld/internal/statedb"
	"claude-repld/internal/stateroot"
	"claude-repld/internal/storehistory"
	"claude-repld/internal/workspace/geometry"
	"claude-repld/internal/workspace/merge"
)

// shimProtocolVersion is the agent-shim wire protocol version the daemon's
// per-session shimclient negotiates in DaemonHello; it must equal the shim's.
const shimProtocolVersion = "1"

const daemonVersion = "0.1.0"

// shutdownRequest is one commanded teardown: the stop-shims decision the
// requester made, and the cause it made it for. They ride together so the
// single shutdown goroutine cannot pair one requester's decision with another's
// attribution.
type shutdownRequest struct {
	stopShims bool
	cause     sessioncontroller.StopCause
}

// daemonReadiness is the single daemon-global readiness truth used by both
// GET /healthz and the frontend DaemonHealth command.  It becomes true only
// after all boot dependencies and the frontend UDS listener are live.
type daemonReadiness struct{ ready atomic.Bool }

func (r *daemonReadiness) DaemonHealth() (bool, string) {
	if r.ready.Load() {
		return true, ""
	}
	return false, "daemon initialization is incomplete"
}

func healthzHandler(r *daemonReadiness, logf func(string, ...any)) http.Handler {
	if r == nil || logf == nil {
		panic("claude-repld: health handler requires readiness and logger")
	}
	return http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		healthy, reason := r.DaemonHealth()
		if !healthy {
			logf("claude-repld: /healthz unhealthy reason=%q", reason)
			http.Error(w, reason, http.StatusServiceUnavailable)
			return
		}
		w.WriteHeader(http.StatusNoContent)
	})
}

// webappHandler builds the handler mounted at "/" for the -webapp SPA
// directory. It returns nil when dir is empty (no webapp is served).
//
// When dir is set but has no index.html — the misconfiguration that
// otherwise surfaces as Go's opaque "404 page not found", e.g. because
// the worktree the daemon was launched from has since been deleted, or
// the webapp was never built there — it logs a prominent one-time
// warning via logf and serves a clear diagnostic (HTTP 503) at every
// path instead. index.html presence is re-checked per request, so the
// daemon self-corrects the moment the assets appear (a webapp rebuild)
// without needing a restart.
func webappHandler(dir string, logf func(format string, args ...any)) http.Handler {
	if dir == "" {
		return nil
	}
	index := filepath.Join(dir, "index.html")
	if _, err := os.Stat(index); err != nil {
		logf("claude-repld: WARNING: -webapp %q has no index.html (%v); serving a diagnostic at / instead of the SPA — rebuild the webapp (bin/build-frontend.sh webapp) or restart the daemon (M-x agent-repl-frontend-daemon-restart)", dir, err)
	}
	fs := http.FileServer(http.Dir(dir))
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if _, err := os.Stat(index); err != nil {
			http.Error(w, webappMissingMessage(dir), http.StatusServiceUnavailable)
			return
		}
		// THE HTML ENTRY POINT IS NEVER CACHED, and the hashed assets it names
		// are cached freely. Vite fingerprints every bundle, so an asset URL
		// addresses exactly one build and can be held forever; index.html is the
		// only thing that says WHICH build, and it is served from a fixed path.
		//
		// Without this the response carries no cache directive at all, and a
		// client applies heuristic freshness from Last-Modified — so a webview
		// keeps running a bundle from a previous build, and keeps running it
		// after that file has been deleted, because it is answering out of its
		// own cache rather than asking. A deploy is then invisible for as long
		// as the heuristic holds.
		if isWebappDocument(r.URL.Path) {
			w.Header().Set("Cache-Control", "no-store, must-revalidate")
		}
		fs.ServeHTTP(w, r)
	})
}

// isWebappDocument reports whether path addresses the SPA's HTML entry point
// rather than one of its fingerprinted assets.
//
// The entry point is reachable both as "/" and as an explicit "/index.html",
// and an SPA route that is not a file resolves to it too — so anything that is
// not under the build's hashed asset tree is treated as the document.
func isWebappDocument(path string) bool {
	return !strings.HasPrefix(path, "/assets/")
}

// webappMissingMessage is the diagnostic body served at "/" when the
// -webapp directory has no index.html, replacing the bare
// "404 page not found" with a message that names the cause and the fix.
func webappMissingMessage(dir string) string {
	return fmt.Sprintf(`claude-repld: webapp assets not found

The daemon's -webapp directory has no index.html:
  %s

This usually means the worktree the daemon was launched from was removed,
or the webapp was never built there.

Fix: rebuild the webapp (bin/build-frontend.sh webapp), or restart the
daemon from a checkout whose webapp/dist exists
(M-x agent-repl-frontend-daemon-restart).
`, dir)
}

// launchedBinaryMTime returns the Unix mtime (seconds) of the executable
// this process was launched from, or 0 when it cannot be resolved.
//
// Captured ONCE at boot, never per request: `go build -o` replaces the
// on-disk binary in place, so stat-ing os.Executable() after a rebuild
// would report the NEW binary's mtime and mask the very staleness the
// value exists to expose. The daemon serves this boot-time snapshot on
// GET /sessions so Emacs can compare it against the current on-disk
// binary and bounce only when the build has moved ahead of this process.
// A resolution or stat failure is logged and reported as 0 (staleness
// never asserted) rather than aborting boot over a diagnostic field.
func launchedBinaryMTime(logger *dlog.Logger) int64 {
	exe, err := os.Executable()
	if err != nil {
		logger.With("operation", "stat-daemon-binary").Log("claude-repld: cannot resolve own executable for staleness reporting: %v", err)
		return 0
	}
	info, err := os.Stat(exe)
	if err != nil {
		logger.With("operation", "stat-daemon-binary", "executable", exe).Log("claude-repld: cannot stat own executable: %v", err)
		return 0
	}
	return info.ModTime().Unix()
}

// bootFatal is the sole bootstrap emergency stderr path. The durable sink does
// not exist yet, so recording this failure anywhere else is impossible.
func bootFatal(format string, args ...any) {
	line := bootFatalLine(fmt.Sprintf(format, args...))
	for len(line) > 0 {
		n, err := os.Stderr.Write(line)
		if err != nil || n <= 0 || n > len(line) {
			break
		}
		line = line[n:]
	}
	os.Exit(1)
}

func bootFatalLine(message string) []byte {
	line, err := json.Marshal(dlog.Record{
		Timestamp: dlog.NewStamp(time.Now()), Runtime: dlog.RuntimeDaemon, PID: os.Getpid(),
		Level: dlog.LevelError, Verbosity: dlog.Normal,
		Operation: "daemon.bootstrap.fatal", Message: message, Context: map[string]any{},
	})
	if err != nil {
		panic(fmt.Sprintf("claude-repld: encode bootstrap fatal JSON: %v", err))
	}
	return append(line, '\n')
}

func daemonFatal(logger *dlog.Logger, format string, args ...any) {
	logger.Log(format, args...)
	logger.With("operation", "exit").LogError("claude-repld exiting: fatal error above")
	os.Exit(1)
}

// logDaemonProcessExit is the daemon's deferred exit trace for the graceful
// shutdown path (a SIGTERM/SIGINT or a shutdown FrontendCommand): whatever
// causes main to return — completed teardown, or a recovered panic — is the
// last record this process writes, so a truncated log still names why the
// daemon is gone. daemonFatal (above) carries the equivalent trace for every
// non-graceful exit path, since those terminate via os.Exit before this
// defer would ever run.
//
// It re-panics after logging rather than recovering: a panic here is an
// invariant violation, and this trace exists to narrate the crash, not to
// turn it into a normal exit.
func logDaemonProcessExit(logger *dlog.Logger) {
	if r := recover(); r != nil {
		logger.With("operation", "exit").LogError("claude-repld exiting: panic: %v", r)
		panic(r)
	}
	logger.With("operation", "exit").Log("claude-repld exiting cleanly")
}

// udsShimLogger keeps daemon-owned errors in daemon.log while direct shim JSON
// records remain single-writer records in the inherited shim.log descriptor.
type udsShimLogger struct {
	workspace dlog.Workspace
	daemon    *dlog.Logger
	terminal  io.Writer
	sessionID string
}

func (l *udsShimLogger) emit(level dlog.Level, verbosity dlog.Verbosity, message string) {
	event := dlog.Event{Runtime: dlog.RuntimeDaemon, Level: level, Operation: "shim.stderr", Message: message, Context: map[string]any{"session_id": l.sessionID}, AgentReplSessionID: l.sessionID}
	var err error
	if verbosity == dlog.Verbose {
		err = l.daemon.EmitWorkspaceVerbose(l.workspace, event)
	} else {
		err = l.daemon.EmitWorkspaceNormal(l.workspace, event)
	}
	if err != nil {
		fmt.Fprintf(l.terminal, "claude-repld: workspace shim diagnostic persistence failed: %v\n", err)
	}
}

func (l *udsShimLogger) Log(format string, args ...any) {
	l.emit(dlog.LevelError, dlog.Normal, fmt.Sprintf(format, args...))
}
func (l *udsShimLogger) LogVerbose(format string, args ...any) {
	l.emit(dlog.LevelInfo, dlog.Verbose, fmt.Sprintf(format, args...))
}

// LogLifecycle keeps the stderr pump's own bring-up and teardown at INFO. Every
// healthy spawn writes one of each, and routing them through Log stamped a
// level=error record on a workspace that had nothing wrong with it — the one
// thing a level is for is telling those two cases apart. The verbosity stays
// NORMAL: this is the context a spawn post-mortem reads first.
func (l *udsShimLogger) LogLifecycle(format string, args ...any) {
	l.emit(dlog.LevelInfo, dlog.Normal, fmt.Sprintf(format, args...))
}
func (l *udsShimLogger) MirrorShimRecord(line string) { fmt.Fprintln(l.terminal, line) }
func main() {
	// One-off subcommands are dispatched BEFORE any daemon bootstrap: a
	// `create-workspace` run must not open the daemon's run log, its listeners,
	// or its session fleet. It exits from here and never reaches the code below.
	runSubcommandOrExit(context.Background())

	ready := &daemonReadiness{}

	// Disk log, wired before daemon logging begins: every dlog record
	// lands in both stderr (the *claude-repld* buffer Emacs captures)
	// and a per-run file under the state root, so daemon history
	// survives the buffer and stays readable without a live editor.
	// An unresolvable root or unopenable file is fatal — a daemon whose
	// evidence trail cannot exist is exactly the failure this log is
	// for. bootFatal is intentionally the only emergency stderr path before
	// that sink exists.
	logRoot, err := stateroot.Root()
	if err != nil {
		bootFatal("claude-repld: %v", err)
	}
	logFile, logWarnings, err := replog.Open(logRoot)
	if err != nil {
		bootFatal("claude-repld: %v", err)
	}
	// CappedWriter, not the bare file: a long-lived run that logs
	// unexpectedly heavily would otherwise grow claude-repld.log without
	// bound between restarts.
	cappedLog := replog.NewCappedWriter(logRoot, logFile, replog.CapBytes)
	defer cappedLog.Close()
	// THE TERMINAL MIRROR IS DECOUPLED FROM THE DAEMON'S CRITICAL PATH.
	//
	// In production this process is a child of Emacs and stderr is a pty Emacs
	// drains from its process filter. While Emacs is busy — its whole startup,
	// which is also when the daemon logs hardest — that pty stops being read,
	// its kernel buffer fills, and a synchronous terminal write blocks for as
	// long as Emacs stays busy. It used to block holding the durable sink's
	// mutex, which put every other emitter behind it, and the daemon logs on
	// the critical path of every frontend command: one boot's roster publish
	// spent 6957ms of its 6957ms ack inside a single log call for exactly that
	// reason. TerminalSink queues the mirror line and returns, so the durable
	// sink — which is authoritative and fast — is never held hostage by
	// whoever owns the terminal. Nothing is dropped and no failure is
	// swallowed; see dlog/terminal.go.
	//
	// ONE sink is shared by every logger so the mirror keeps a single FIFO
	// order across the daemon log, the workspace logs and the forwarded runtime
	// logs, and its Close is registered first so it flushes LAST.
	logTerminal := dlog.NewTerminalSink(os.Stderr, dlog.DefaultTerminalBufferBytes)
	defer func() {
		if err := logTerminal.Close(); err != nil {
			// A mirror failure is latched and already reported to whichever
			// emitter met it during the run; this is the backstop for one that
			// happened with no emitter left to tell. It is recorded through a
			// logger writing to the still-open durable sink and to the real
			// stderr, because the queued mirror is exactly what just failed.
			dlog.New(cappedLog, os.Stderr, true).
				With("operation", "daemon.logging.terminal-mirror-failure").
				LogError("claude-repld: terminal log mirror failed: %v", err)
		}
	}()
	daemonLog := dlog.New(cappedLog, logTerminal, os.Getenv("AGENT_REPL_LOG_VERBOSE") != "")
	defer logDaemonProcessExit(daemonLog)
	targets := dlog.NewTargetManager()
	defer func() {
		if err := targets.Close(); err != nil {
			daemonLog.With("operation", "close-workspace-log-targets").Log("claude-repld: close workspace log targets: %v", err)
		}
	}()
	stopWorkspaceLogMaintenance := startWorkspaceLogMaintenance(
		targets, daemonLog, logTerminal, os.Getenv("AGENT_REPL_LOG_VERBOSE") != "",
	)
	defer stopWorkspaceLogMaintenance()
	legacyLog := dlog.Legacy(daemonLog)
	// The leveled companions of legacyLog. A subsystem injected with plain
	// callbacks takes these for records that accompany a regression the user
	// can see, so a degraded turn, a refused command or a lost accounting row
	// is not indexed at info beside routine progress.
	legacyWarn := dlog.LegacyWarn(daemonLog)
	legacyError := dlog.LegacyError(daemonLog)
	daemonLog.With("operation", "boot", "pid", os.Getpid(), "log_path", cappedLog.Name()).Log("claude-repld: booted")
	for _, w := range logWarnings {
		daemonLog.With("operation", "boot").Log("claude-repld: %s", w)
	}

	binaryMTime := launchedBinaryMTime(daemonLog)

	var (
		addr         = flag.String("addr", "127.0.0.1:8787", "listen address")
		nodeBin      = flag.String("node", "node", "node binary used to run the shim")
		claudeBin    = flag.String("claude-bin", "", "path to the claude CLI the SDK drives (empty = the SDK's bundled native Claude Code binary)")
		shimScript   = flag.String("shim", "", "path to the shim entrypoint (agent-shim/claude/shim/dist/main.js)")
		fake         = flag.Bool("fake", false, "force --fake (offline scripted SDK) on every session")
		idleTimeout  = flag.Duration("idle-timeout", time.Hour, "hibernate a session (SIGTERM its UDS shim; keep the record rehydratable) after this long with nothing happening to its workspace; 0 disables. Measured from the newest row on the workspace's state log, so a finished turn starts the clock rather than arming an immediate sweep. A hibernated session costs a full shim bring-up on the next act, so the window is generous rather than tight")
		webappDir    = flag.String("webapp", "", "optional directory of webapp static files to serve at /")
		widgetAssets = flag.String("widget-assets", envStr("AGENT_REPL_WIDGET_ASSETS", ""), "optional directory of embeddable-widget assets (e.g. a chess-widget dist) to serve at /widget-assets/; empty = capability off")
		accountsFlag = flag.String("accounts", "", "canonical account roster as comma-separated label=config-dir pairs (empty dir = the CLI's default root), e.g. \"personal=,work=/home/u/.claude-chesscom\"; empty = account routes disabled")
		pprofAddr    = flag.String("pprof", envStr(pprofsurface.EnvAddr, ""), "OPT-IN Go profiling surface: a unix socket path, or an explicitly loopback host:port (127.0.0.1:6060). Empty = OFF, which is the default; there is no always-on listener. The resolved surface is named in the daemon.pprof.enabled record at startup")
	)
	flag.Parse()

	accounts, err := parseAccounts(*accountsFlag)
	if err != nil {
		daemonLog.With("operation", "parse-accounts").Log("claude-repld: -accounts: %v", err)
		os.Exit(2)
	}

	// The profiling surface is opened BEFORE the daemon's dependencies, so a
	// boot that wedges in state-store or geometry work is still profilable —
	// which is precisely the window the command-path stalls were observed in.
	pprofSurface, err := openPprofSurface(*pprofAddr, daemonLog)
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: %v", err)
	}
	defer func() {
		if err := pprofSurface.Close(); err != nil {
			daemonLog.With("operation", "daemon.pprof.close").LogError("claude-repld: close pprof surface: %v", err)
		}
	}()

	if *shimScript == "" {
		daemonLog.With("operation", "validate-config").Log("claude-repld: --shim is required (path to agent-shim/claude/shim/dist/main.js)")
		os.Exit(2)
	}

	// THE STATE STORE. One SQLite database carries both the session
	// registry's identity tables and the SSM's state log, opened ONCE here and
	// shared by both owners: that is what lets a cursor, a replay floor and a
	// vendor identity move in a single transaction instead of relying on
	// write ordering across two files.
	statePath, err := registry.DefaultDBPath()
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: %v", err)
	}
	stateStore, err := statedb.Open(statePath)
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: open state store: %v", err)
	}
	defer func() {
		if err := stateStore.Close(); err != nil {
			daemonLog.With("operation", "close-state-store").Log("claude-repld: close state store: %v", err)
		}
	}()

	// The durable half of every prompt receipt, in the same store the state log
	// and the merge lease ledger live in. A daemon that cannot install it cannot
	// promise a submitted prompt survives its own death, and starting anyway
	// would make that promise silently false.
	promptReceipts, err := statedb.NewPromptReceipts(stateStore)
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: open prompt receipt store: %v", err)
	}
	// The keep-alive window ledger. FATAL on failure for the prompt receipts'
	// reason inverted: without it the daemon cannot tell its own cache pings
	// from the user's prompts, and would render machine-generated turns as
	// conversation.
	keepAliveWindows, err := statedb.NewKeepAliveWindows(stateStore)
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: open keep-alive window store: %v", err)
	}
	turnAccountings, err := statedb.NewTurnAccountings(stateStore)
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: open turn accounting store: %v", err)
	}
	// EVERY KEEP-ALIVE WINDOW STILL OPEN HERE IS AN ORPHAN. No session
	// controller exists yet, so no ping can be in flight, so an unclosed row can
	// only be one a previous daemon died in the middle of — and an open window
	// has no upper bound, meaning that row is withholding its workspace's entire
	// conversation from every rendering until somebody closes it.
	//
	// It runs after the turn accounting store because the repair is stamped from
	// that store's durable turn ends, never from now: now is when the daemon
	// restarted, and closing there would extend the exclusion over every real
	// turn the outage spanned.
	//
	// FATAL on failure, for the ledger's own reason: a daemon that cannot
	// reconcile the ledger cannot tell its pings from the user's conversation,
	// and starting anyway would silently blackout whatever it failed to repair.
	reconciled, err := keepAliveWindows.ReconcileOpenWindows(turnAccountings)
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: reconcile open keep-alive windows: %v", err)
	}
	if reconciled > 0 {
		daemonLog.With("operation", "reconcile-keep-alive-windows").Log(
			"claude-repld: closed %d keep-alive window(s) left open by a previous daemon; each was withholding every later conversation item on its workspace", reconciled)
	}
	tokenUtilizations, err := statedb.NewTokenUtilizations(stateStore)
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: open token utilization store: %v", err)
	}

	// The scheduled-shutdown drain lease and the prompts it parks. A daemon
	// that cannot install these cannot promise that a bounce it scheduled
	// survives a crash, nor that a prompt it parked behind that bounce is
	// delivered afterwards — and starting anyway would make both promises
	// silently false, which is exactly the shape of loss they exist to end.
	shutdownSchedules, err := statedb.NewShutdownSchedules(stateStore)
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: open shutdown schedule store: %v", err)
	}

	// Persistent session registry: the in-memory session map dies with
	// the process, so this write-through record store is what lets a
	// restarted daemon keep resolving the s_<hex> ids its frontends
	// still hold. A daemon that cannot even resolve WHERE the registry
	// lives must fail loudly at startup. Checkpoint repair and terminal
	// compaction are required durability work: any load/migration/save failure
	// aborts startup rather than serving from fabricated empty state.
	//
	// FIRST BOOT AFTER THIS MIGRATION performs a one-time import of the
	// pre-SQLite JSON registry (and its checkpoint sidecar) into the tables,
	// logged loudly. The file is left on disk as inert history; nothing reads
	// it again, and NOTHING WRITES IT — every boot re-asserts a `.RETIRED`
	// deprecation record beside it naming this store as the successor
	// authority, so its frozen mtime cannot be read as a broken writer.
	legacyRegistryPath, err := registry.LegacyJSONPath()
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: %v", err)
	}
	sessionRegistry := registry.OpenWith(registry.Options{
		DB:             stateStore,
		StorePath:      statePath,
		LegacyJSONPath: legacyRegistryPath,
		Logf:           legacyLog,
	})
	if err := sessionRegistry.Prepare(); err != nil {
		daemonFatal(daemonLog, "claude-repld: registry prepare: %v", err)
	}
	// Every superseded session left standing by a previous daemon is closed
	// here, BEFORE any frontend can connect and be handed a snapshot
	// (supersederesolve.go). Both parties to a supersede died with the daemon
	// that performed it, so its card is history rather than an open failure;
	// held open, it re-presented on every boot forever. Within THIS lifetime a
	// supersede is only ever resolved by its successor reaching operational.
	server.ReconcileSupersededDeaths(sessionRegistry, nil, legacyLog)

	// Interactive Claude login, on a pty the daemon owns and the webapp
	// renders. Nothing here parses the terminal: the login is a full-screen
	// TUI gated behind stateful prompts before it ever reaches OAuth, so a
	// human reads it and the daemon only carries it.
	//
	// The login is a direct CLI invocation rather than an SDK-driven one, so
	// it needs a real executable: -claude-bin when it names one, else the
	// `claude` on PATH (which is what the vterm login always ran).
	loginBin := *claudeBin
	if loginBin == "" {
		loginBin = "claude"
	}
	logins := login.NewManager(login.Config{
		Start:  login.SpawnVendor([]string{loginBin, "/login"}),
		Logger: daemonLog,
	})
	defer logins.CloseAll()

	// A single graceful-shutdown path serves BOTH SIGTERM and the shutdown
	// FrontendCommand: Emacs uses the latter to bounce a stale daemon it
	// adopted from another Emacs (no local process handle to signal).
	//
	// The request carries ONE decision — whether to stop the session shims on
	// the way out — so it rides a buffered channel rather than a bare close.
	// SIGTERM cannot express a mode at all, and it takes the default: PRESERVE.
	// That is the honest reading of an unqualified "stop this daemon", and it
	// is what makes an OS-level bounce as cheap as a commanded one.
	// The request carries the CAUSE alongside the decision, because the two
	// requesters are different events: an ordinary shutdown command, and the
	// execution phase of a scheduled drain a deploy is waiting on. Both stop
	// the same shims; only the cause says on whose behalf.
	shutdownReq := make(chan shutdownRequest, 1)
	var shutdownOnce sync.Once
	requestShutdown := func(stopShims bool, cause sessioncontroller.StopCause) {
		shutdownOnce.Do(func() {
			shutdownReq <- shutdownRequest{stopShims: stopShims, cause: cause}
			close(shutdownReq)
		})
	}

	// The SSM (resolved per-workspace state) is opened here and shared by BOTH
	// the frontend snapshot/merge push loop AND the per-session controller's
	// lifecycle-event application; one owner (main) closes it once.
	ssmMgr, err := ssm.Open(ssm.Options{
		DB:       stateStore,
		Resolver: server.NewRegistryResolver(sessionRegistry),
		Logf:     legacyLog,
		Warnf:    legacyWarn,
		Errorf:   legacyError,
	})
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: open SSM: %v", err)
	}
	defer ssmMgr.Close()

	// The progress-footer resolver (F1) is the SSM's sibling and is owned here
	// for the same reason: both the frontend push loop and the per-session
	// controller feed it, so one owner closes it once.
	// THE KEEP-ALIVE POLICY IS RESOLVED AT BOOT AND FATAL ON A BAD KNOB. An
	// operator who set AGENT_REPL_KEEPALIVE_CACHE_TTL_MS=0 meant something by
	// it, and quietly running the shipped hour while they believe the feature
	// is off is the failure a loud refusal exists to prevent.
	//
	// Resolved HERE, ahead of every consumer, because three of them take a knob
	// from it — the progress resolver's cost threshold, the session
	// controller's policy, and the sweeper's — and a config read per consumer
	// would be three chances to disagree about the same environment.
	keepAliveConfig, err := keepalive.FromEnv()
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: %v", err)
	}
	legacyLog("claude-repld: cache keep-alive policy ttl=%s leeway=%s idle_cutoff=%s uncached_cost_alert=%d tokens",
		keepAliveConfig.CacheTTL, keepAliveConfig.Leeway, keepAliveConfig.IdleCutoff, keepAliveConfig.UncachedCostAlertTokens)
	progressMgr := progress.New(progress.Options{
		Logf:                legacyLog,
		UncachedAlertTokens: keepAliveConfig.UncachedCostAlertTokens,
	})
	defer progressMgr.Close()

	// Shims dial US. One listening socket serves every session; each shim
	// announces itself with its ShimHello and the listener routes the
	// connection to the session controller that owns that session. Started BEFORE any
	// session is brought up so a shim surviving a previous daemon has
	// somewhere to reconnect to the instant this one boots.
	shimSocketPath, err := shimlisten.DefaultSocketPath()
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: resolve shim socket path: %v", err)
	}
	shimListener := shimlisten.New(legacyLog)
	if err := shimListener.Listen(shimSocketPath); err != nil {
		daemonFatal(daemonLog, "claude-repld: listen for shims: %v", err)
	}
	defer shimListener.Close()
	// The shim takes its session lock here; it must exist before any spawn.
	if err := sessionlock.EnsureDir(); err != nil {
		daemonFatal(daemonLog, "claude-repld: create session lock dir: %v", err)
	}

	// The per-session shim-controller consumes each session's UDS shim stream and
	// renders it onto the frontend surface + SSM. Its push target (the
	// frontend.Server) does not exist until WireAgentShim returns, so it pushes
	// through a late-bound forwarder whose target is set below.
	forwarder := &server.PushForwarder{Logf: legacyLog}
	// The controller spawns a fresh UDS-mode shim when no live one is listening. The
	// exec stays here (main owns node/shim paths); production omits
	// --store-socket so the shim defaults to the launchd singleton store. The
	// returned stop func SIGTERMs the shim on hibernation.
	// The spawn watch is what makes a shim that dies between exec and its first
	// frame observable: it holds the child's exit status and stderr tail and
	// hands them to the bring-up that is waiting for the connection, which
	// would otherwise sit out its whole deadline with nothing to report.
	shimSpawnWatch := server.NewShimSpawnWatch(shimListener.Connected, legacyLog)
	// ONE SPAWN PROCEDURE, shared with the e2e harnesses (server/udsspawn.go).
	// What remains here is only what is genuinely this deployment's: the extra
	// argv a configured CLI binary adds, the workspace-scoped loggers, and the
	// spawn watch a bring-up waits on.
	var extraShimArgv []string
	if *claudeBin != "" {
		extraShimArgv = append(extraShimArgv, "--claude-bin", *claudeBin)
	}
	// spawnEvent persists one spawn-lifecycle fact in the workspace's own
	// daemon log. EmitWorkspaceNormal owns its JSON emergency path when durable
	// persistence fails; a second plaintext stderr line would duplicate the
	// error and violate the all-JSON logging contract.
	spawnEvent := func(level dlog.Level, workspace dlog.Workspace, sessionID, message string, context map[string]any) {
		workspaceLog, err := targets.OpenWorkspaceLogger(workspace, logTerminal, os.Getenv("AGENT_REPL_LOG_VERBOSE") != "")
		if err != nil {
			panic(fmt.Sprintf("claude-repld: open daemon workspace logger for %q: %v", workspace.Directory, err))
		}
		if err := workspaceLog.EmitWorkspaceNormal(workspace, dlog.Event{
			Runtime: dlog.RuntimeDaemon, Level: level, Operation: "shim.spawn",
			Message: message, Context: context, AgentReplSessionID: sessionID,
		}); err != nil {
			panic(err)
		}
	}
	udsSpawn, err := server.NewUDSSpawner(server.UDSSpawnConfig{
		Targets:    targets,
		Node:       *nodeBin,
		Script:     *shimScript,
		ShimSocket: shimSocketPath,
		ForceFake:  *fake,
		ExtraArgv:  extraShimArgv,
		ExtraEnv:   func(opts server.CreateOpts) []string { return server.ShimEnv(opts, *addr) },
		Logger: func(workspace dlog.Workspace, sessionID string) shim.Logger {
			workspaceLog, logErr := targets.OpenWorkspaceLogger(workspace, logTerminal, os.Getenv("AGENT_REPL_LOG_VERBOSE") != "")
			if logErr != nil {
				panic(fmt.Sprintf("claude-repld: open daemon workspace logger for %q: %v", workspace.Directory, logErr))
			}
			return &udsShimLogger{workspace: workspace, daemon: workspaceLog, terminal: logTerminal, sessionID: sessionID}
		},
		Event: spawnEvent,
		Spawned: func(s server.SpawnedShim) {
			shimSpawnWatch.Spawned(s.SessionID, s.Proc.StderrTail)
		},
		Exited: func(s server.SpawnedShim, werr error) (string, map[string]any) {
			// The watch is what unblocks a bring-up still waiting for this
			// shim's connection, so it is notified before the record is
			// written; a shim that died before it ever connected is named as
			// such rather than reported as an ordinary exit.
			failure := shimSpawnWatch.Exited(s.SessionID, werr)
			if failure == nil {
				return "", nil
			}
			return "UDS shim exited BEFORE it ever connected to the daemon", map[string]any{"error": failure.Error()}
		},
	})
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: build the UDS shim spawner: %v", err)
	}
	// Held by pointer so its SessionView re-push can be late-bound below: the
	// Server it pushes through does not exist yet (same shape as forwarder).
	modelCatalogs := server.NewSessionModelCatalogs()
	// The transcript backup plane rides the registrar because the registrar is
	// the first thing in the daemon to hear both boundaries a copy is worth
	// taking at — a turn ending and a vendor uuid rotating — and it already
	// holds the registry that turns a session id into a transcript path.
	registrar := &server.RegistryRegistrar{
		Reg:           sessionRegistry,
		Logf:          legacyLog,
		ModelCatalogs: modelCatalogs,
		Backups:       &server.TranscriptBackups{Reg: sessionRegistry, Logf: legacyLog},
	}
	// One registry adapter serves both durable seq marks: last_seen_seq (the
	// shimclient replay high-water) and newest_clear_or_compact_seq (the
	// frontend replay floor).
	seqStore := server.NewRegistrySeqStore(sessionRegistry, legacyLog)
	fileDiagnostics, err := server.NewTargetFileDiagnosticPersister(targets, logTerminal, os.Getenv("AGENT_REPL_LOG_VERBOSE") != "")
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: build sidecar diagnostic persister: %v", err)
	}
	// The below-floor history re-pull needs no wiring of its own: it rides the
	// session's existing shim connection as a ReplayRequest, so the store stays
	// behind the agent-shim facade (sessioncontroller/repull.go).
	//
	// A resync for a workspace with NO live session controller has no such
	// connection to ride, and bringing one up to answer a read would charge a
	// frontend's mount a vendor process. It reads the store directly instead
	// (sessioncontroller/durablereplay.go), keyed by the VENDOR session uuid the
	// store files that conversation's seq space under.
	storeSocketPath, err := storehistory.DefaultSocketPath()
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: resolve store socket path: %v", err)
	}
	durableHistory := &storehistory.Reader{
		Socket: storeSocketPath,
		Vendor: func(sessionID string) (string, bool) {
			rec, ok := sessionRegistry.Get(sessionID)
			if !ok {
				return "", false
			}
			return rec.ClaudeSessionID, rec.ClaudeSessionID != ""
		},
		Logf: legacyLog,
	}
	// THE DAEMON'S ONE CLOCK. The idle sweeper decides a hibernation against
	// the server's clock and the session controller's transition re-validates
	// that decision's idleness against its own; if those are two clocks they
	// are two authorities for one policy, and the gate refuses decisions the
	// sweeper legitimately took. Both fields below are fed from this variable
	// so the decision and its gate are the same authority by construction.
	nowFn := time.Now
	nowMsFn := func() int64 { return nowFn().UnixMilli() }
	shimSpawner := server.NewShimSpawner(sessionRegistry, shimListener.Connected, shimListener.Evict, udsSpawn, legacyLog)
	// The respawn path must reach the create path's verdict on the resume gate
	// for the very same session, and -fake is what that verdict turns on.
	shimSpawner.ForceFake(*fake)
	controller, err := sessioncontroller.New(sessioncontroller.Config{
		Push:              forwarder,
		SSM:               ssmMgr,
		Progress:          progressMgr,
		Spawner:           shimSpawner,
		Source:            &server.ShimConnSource{Listener: shimListener, Deaths: shimSpawnWatch},
		FileDiagnostics:   fileDiagnostics,
		Locator:           &server.SessionLocator{Reg: sessionRegistry},
		SeqStore:          seqStore,
		ClearCompactStore: seqStore,
		DurableHistory:    durableHistory,
		PromptReceipts:    promptReceipts,
		TurnAccountings:   turnAccountings,
		HistoricalUsage:   tokenUtilizations,
		ShutdownHolds:     shutdownSchedules,
		PermissionModes:   server.NewRegistryModeStore(sessionRegistry),
		Registrar:         registrar,
		ModelCatalogs:     registrar,
		Hibernations:      registrar,
		VendorSessions:    registrar,
		// THE SWEEPER'S OWN STAMPING RULE, handed to the controller so the
		// staleness check taken at prompt acceptance and at bring-up measures a
		// pre-keep-alive session exactly as the sweep does rather than through
		// a second copy of the rule.
		LegacyTurnEnds: server.LegacyTurnEndStamps{
			Reg:      sessionRegistry,
			Activity: ssmMgr,
			Logf:     legacyLog,
		},
		KeepAliveWindows: server.KeepAliveWindowStore{Windows: keepAliveWindows},
		KeepAlive:        keepAliveConfig,
		VendorSessionOf: func(sessionID string) (string, bool) {
			rec, ok := sessionRegistry.Get(sessionID)
			if !ok || rec.ClaudeSessionID == "" {
				return "", false
			}
			return rec.ClaudeSessionID, true
		},
		DaemonVersion:   daemonVersion,
		ProtocolVersion: shimProtocolVersion,
		Logf:            legacyLog,
		Warnf:           legacyWarn,
		Errorf:          legacyError,
		// One authority with the server's idle sweeper (see nowFn above).
		Now: nowMsFn,
		// The prompt queue's classifier (E4). A queued prompt is judged by a
		// cheap headless run under the SESSION's own account, so the
		// classification cannot land on a different account's quota or config.
		// THE STALE-SHIM REFRESH's other half: the build identity of the bundle
		// this daemon would spawn today, read fresh on every comparison so a
		// deploy that lands WHILE the daemon runs is seen without a restart.
		ShimBuildSHA: shimBuildSHA(*shimScript),
		Classifier:   sessioncontroller.NewCLIClassifier("", legacyLog),
		SessionConfigDir: func(sessionID string) string {
			rec, ok := sessionRegistry.Get(sessionID)
			if !ok {
				return ""
			}
			return rec.ConfigDir
		},
	})
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: build session controller: %v", err)
	}
	defer controller.Close()

	// THE merge-geometry map: which branch, which worktree, and which parent
	// worktree each workspace merges with. It lives in the shared state store
	// beside the registry and the SSM because it is durable daemon-owned
	// identity of exactly their kind, and it must survive the daemon bounce a
	// self-merge causes. Emacs sends a bare merge request keyed by workspace and
	// this map answers it; a daemon that cannot open the map cannot merge, so
	// failing to open it aborts startup rather than serving guessed targets.
	geometryStore, err := geometry.Open(stateStore, legacyLog)
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: open workspace merge geometry: %v", err)
	}

	// ONE-TIME REPAIR for workspaces that predate the daemon owning the map.
	// They have no record, and without one every merge of one would be refused
	// forever, so their geometry is derived from git at boot: the worktree's
	// checked-out branch, and the repository's main worktree as the target.
	//
	// This is a repair of missing state, not a fallback for the recording path:
	// it never touches a workspace that already has a record, and a workspace
	// whose branch or worktree git cannot answer for (a detached HEAD, a
	// deleted worktree) deliberately keeps NO record so its merge is refused
	// with an explanation rather than run against a guess.
	geometryDeriver, err := geometry.NewDeriver(legacyLog)
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: build workspace geometry deriver: %v", err)
	}
	geometryBackfiller, err := geometry.NewBackfiller(geometry.BackfillConfig{
		Store:   geometryStore,
		Deriver: geometryDeriver,
		Lister:  registryGeometryLister{Reg: sessionRegistry},
		Logf:    legacyLog,
	})
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: build workspace geometry backfiller: %v", err)
	}
	geometryReport, err := geometryBackfiller.Run(context.Background())
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: backfill workspace merge geometry: %v", err)
	}
	daemonLog.With("operation", "geometry-backfill").Log(
		"claude-repld: workspace merge geometry backfill recorded=%d already=%d underivable=%d",
		geometryReport.Recorded, geometryReport.AlreadyRecorded, geometryReport.Underivable)

	// The merge queue's durable substrate and the shim exclusivity lease.
	// Both are constructed HERE, not inside WireAgentShim, because the SSM's
	// merge.Lease binds the SAME queue instance the coordinator drains: two
	// queues over one directory would put two merge subsystems behind one
	// manager's leases. The queue directory lives under the state root so a
	// queued merge survives the daemon bounce a self-merge of the daemon
	// causes.
	mergeQueueDir, err := stateroot.Root()
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: resolve merge queue dir: %v", err)
	}
	mergeQueue, err := merge.NewFileQueue(filepath.Join(mergeQueueDir, "merge-queue"), legacyLog)
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: open merge queue: %v", err)
	}
	mergeLease, err := ssm.NewMergeLease(ssm.MergeLeaseConfig{
		Manager:     ssmMgr,
		Queue:       mergeQueue,
		Interrupter: controller,
	})
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: build merge lease: %v", err)
	}

	// Agent-shim frontend.v1 surface (design §9.1, §14.2): the SSM-backed
	// snapshot + merge.Driver + frontend Server. Always on post-cutover — it is
	// the daemon's consumption plane, not an optional add.
	// sessionCommands is the late-bound daemon-core surface (session
	// create/delete + DaemonView) the frontend command handler and snapshot
	// provider need. Its *Server target does not exist until server.New below,
	// so bind it after — the same late-bind shape as forwarder.
	sessionCommands := &server.SessionCommandBinding{Logf: legacyLog}
	// Workspace creation owns worktrees, waiting shims, durable job state, and
	// retained host actions.  Build its bridge before WireAgentShim so frontend
	// commands never see an unbound creation capability, but do not drain the
	// inbox until the server-side SessionCommandBinding has its real target.
	workspaceCreateCtx, cancelWorkspaceCreate := context.WithCancel(context.Background())
	defer cancelWorkspaceCreate()
	workspaceAssembly, err := NewWorkspaceCreateAssembly(WorkspaceCreateAssemblyConfig{
		StateRoot:      logRoot,
		Commands:       sessionCommands,
		Registry:       sessionRegistry,
		Geometry:       daemonGeometryRecorder{Store: geometryStore, Logf: legacyLog},
		Health:         sessionControllerHealthProbe{Controller: controller, Logf: legacyLog},
		InitialPrompts: controller,
		Logf:           legacyLog,
		Errorf:         dlog.LegacyError(daemonLog),
		InboxInterval:  time.Second,
	})
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: initialize workspace creation: %v", err)
	}
	workspaceBridge, err := NewWorkspaceCreationBridge(workspaceCreateCtx, workspaceAssembly.Manager, workspaceAssembly.Store)
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: initialize workspace creation bridge: %v", err)
	}
	if err := workspaceAssembly.Forwarder.SetTargets(workspaceBridge, workspaceBridge, workspaceBridge, workspaceBridge); err != nil {
		daemonFatal(daemonLog, "claude-repld: bind workspace creation host forwarder: %v", err)
	}
	clientLogs, err := server.NewTargetClientLogWriter(
		targets,
		&server.RegistryClientLogIdentityResolver{Reg: sessionRegistry},
		logTerminal,
		os.Getenv("AGENT_REPL_LOG_VERBOSE") != "",
	)
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: build client log writer: %v", err)
	}
	// THE COMMAND LATENCY KNOB IS RESOLVED AT BOOT AND FATAL ON A BAD VALUE,
	// for keepalive's reason: an operator who set the threshold meant something
	// by it, and running the shipped two seconds while they believe they
	// changed it is exactly the silence this telemetry exists to end.
	ackWarn, err := frontend.AckWarnFromEnv()
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: %v", err)
	}
	commandLatency, err := server.NewTargetCommandLatencyRecorder(
		targets, daemonLog, logTerminal, os.Getenv("AGENT_REPL_LOG_VERBOSE") != "",
	)
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: build command latency recorder: %v", err)
	}
	daemonLog.With("operation", "command-latency-policy", "ack_warn_ms", ackWarn.Milliseconds(),
		"ack_deadline_ms", frontend.CommandAckDeadline.Milliseconds()).
		Log("claude-repld: frontend command latency telemetry armed")
	// NEVER-BLUE (workspaceopen.go): bind each registered workspace to its
	// on-disk transcript at boot, so a restart already knows every resume
	// target before a frontend can connect, and ensure eagerly on open.
	opener := &server.WorkspaceOpener{
		Reg:     sessionRegistry,
		Ensurer: controller,
		// The SAME creation entry point the createSession command reaches, so
		// an open that starts a workspace's first session runs every check an
		// explicit create runs — the ungated-consent refusal above all.
		Creator:    sessionCommands,
		ConfigDirs: knownConfigDirs(accounts),
		Logf:       legacyLog,
	}
	opener.BindAll()
	agentShim, err := server.WireAgentShim(server.AgentShimConfig{
		SSM:      ssmMgr,
		Progress: progressMgr,
		// Prompts is BOTH the frontend's submit path and merge.Coordinator's
		// conflict-resolution path (the controller implements
		// merge.ConflictResolver), so the session a merge drives is necessarily
		// the session the user prompts.
		Prompts:      controller,
		Turns:        controller,
		Health:       controller,
		Restarts:     controller,
		Hibernations: controller,
		DaemonHealth: ready,
		Lifecycle:    opener,
		// The registry's own record of a deliberate deletion, exposed so a merge
		// can tell a hibernated session (rehydrate) from a deleted one (refuse).
		SessionDeaths: server.RegistrySessionDeaths{Reg: sessionRegistry},
		Sessions:      server.RegistrySessions{Reg: sessionRegistry, Controller: controller, ModelCatalogs: modelCatalogs, TokenUsage: tokenUtilizations, Logf: legacyLog},
		// The two per-session facts the resolved-view publisher reads: the
		// owning session's durable record and its published model menu. Both
		// are the SAME instances every other reader here takes, so the topbar
		// and the session view can never name different models.
		SessionRecords:    sessionRegistry,
		ModelCatalogs:     modelCatalogs,
		Inits:             controller,
		Catalogs:          controller,
		AsyncBubbles:      controller,
		Queues:            controller,
		SessionCommands:   sessionCommands,
		Resyncer:          controller,
		WorkspaceCreation: workspaceBridge,
		RequestShutdown:   requestShutdown,
		// The scheduled-shutdown drain lease: its durable record and the fleet
		// it derives its holds from. The controller satisfies DrainHoldSource,
		// and the scheduler binds itself to it at construction.
		ShutdownSchedules: shutdownSchedules,
		DrainHolds:        controller,
		// The durable evidence a RESTORED lease seeds itself from. It is the
		// SAME registry and the SAME two probes the boot sweeper classifies
		// with, below, so the sweeper's verdict about a surviving shim and the
		// lease's verdict about it cannot be two different verdicts.
		DrainEvidence: server.RegistryDrainEvidence{
			Reg:       sessionRegistry,
			Connected: shimListener.Connected,
			Held:      sessionlock.Held,
		},
		ClientLogs:       clientLogs,
		CommandLatency:   commandLatency,
		AckWarnThreshold: ackWarn,
		// A closed workspace gives its log descriptors back (dlog.EvictWorkspace),
		// so a long-lived daemon does not accumulate one set per workspace it has
		// ever touched.
		LogTargets: targets,
		// The daemon resolves a workspace's conversation for itself. Frontends
		// send an intent (continue / fresh / explicit), never a remembered
		// vendor uuid — see server.ConversationResolver.
		Resumes:       &server.ConversationResolver{Reg: sessionRegistry, Logf: legacyLog},
		MergeLease:    mergeLease,
		MergeQueue:    mergeQueue,
		MergeGeometry: geometryStore,
		Logf:          legacyLog,
		Warnf:         legacyWarn,
		LogVerbosef:   daemonLog.LogVerbose,
	})
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: frontend surface: %v", err)
	}
	// Bind the session controller's push target now that the frontend server exists.
	forwarder.SetTarget(agentShim.Server)
	// A schedule this daemon's PREDECESSOR took and did not finish draining is
	// re-taken here, before any client connects: the deploy that asked for the
	// bounce is still waiting for it, and coming back idle would strand it
	// while the next prompt started a turn under a lease it still believes
	// stands. Restoring is done exactly once, at boot.
	if agentShim.ShutdownScheduler != nil {
		if rerr := agentShim.ShutdownScheduler.Restore(); rerr != nil {
			daemonFatal(daemonLog, "claude-repld: restore the scheduled-shutdown drain lease: %v", rerr)
		}
	}
	// The prompts that lease PARKED are materialized here, immediately after it
	// and before the frontend serves its first snapshot. Their sessions have not
	// wired yet — after a bounce that stopped the shims, some may not wire for a
	// long time — and until this ran they were invisible to every client and
	// unreachable by cancel for the whole of that window, which is the promise
	// the lease made to their submitters going unkept.
	if materialized, merr := controller.MaterializeShutdownHolds(); merr != nil {
		daemonFatal(daemonLog, "claude-repld: materialize the drain-lease parked-prompt ledger: %v", merr)
	} else if materialized > 0 {
		daemonLog.With("operation", "materialize-drain-holds").Log(
			"claude-repld: materialized %d prompt(s) parked by a previous daemon's scheduled bounce", materialized)
	}
	defer func() {
		if cerr := agentShim.Close(); cerr != nil {
			daemonLog.With("operation", "close-frontend-surface").Log("claude-repld: frontend surface close: %v", cerr)
		}
	}()

	srv := server.New(server.Config{
		Logf: legacyLog,
		// One authority with the session controller's hibernation gate.
		Now:             nowFn,
		DaemonVersion:   daemonVersion,
		BinaryMTime:     binaryMTime,
		ForceFake:       *fake,
		Registry:        sessionRegistry,
		ModelCatalogs:   modelCatalogs,
		TokenUsage:      tokenUtilizations,
		Logins:          logins,
		Accounts:        accounts,
		IdleTimeout:     *idleTimeout,
		KeepAlive:       keepAliveConfig,
		WidgetAssetsDir: *widgetAssets,
		DaemonAddr:      *addr,
		Controller:      controller,
		// THE FRONTEND SURFACE WHOLE: the same state machine, the same frame
		// fan-out and the SAME resolved-view publisher the SSM's state
		// subscription drives, so the breakdown the SessionView push resolves
		// and the topbar the state push resolves are retained together and
		// snapshot together. Handing over one of the three and forgetting
		// another is not expressible.
		AgentShim: agentShim,
	})
	// Bind the session-command surface now that the *Server exists (createSession
	// /deleteSession UDS commands and the snapshot DaemonView delegate to it).
	sessionCommands.SetTarget(srv)
	// Only now can a creation job invoke the daemon's real session path.  The
	// inbox stopping is a DEGRADED FEATURE, never a dead daemon: a single
	// failed creation used to hibernate every live session in the editor.  Job
	// failures are contained inside the inbox loop (durable, logged, surfaced
	// to the host); only a structural failure ends Run, and even that leaves
	// the daemon serving its sessions with one loud, unmissable log line.
	// The merge verb in a workspace command file is routed DAEMON-SIDE, through
	// the same command path a frontend merge takes. It used to be handed to
	// Emacs as a host action, resolved there by workspace name, and sent back as
	// a merge command; that round trip is gone. Bind before the inbox starts so
	// no claimed file can find the route unwired.
	workspaceAssembly.Merges.SetTarget(agentShim.MergeDispatch)
	// EXECUTION IS OFF THE ROUTER. The inbox goroutine only claims, persists,
	// and routes; these two workers own the creation state machine and the host
	// action publication respectively. A wedged job can stall creation, but it
	// can no longer stop ingestion — which is exactly what it used to do, one
	// goroutine having owned both.
	go func() {
		if err := workspaceAssembly.Manager.RunCreationWorker(workspaceCreateCtx); err != nil && workspaceCreateCtx.Err() == nil {
			daemonLog.With("operation", "workspace-creation-worker").
				LogError("claude-repld: workspace creation worker stopped: %v", err)
		}
	}()
	go func() {
		if err := workspaceAssembly.Manager.RunHostActionWorker(workspaceCreateCtx); err != nil && workspaceCreateCtx.Err() == nil {
			daemonLog.With("operation", "workspace-host-action-worker").
				LogError("claude-repld: workspace host-action worker stopped: %v", err)
		}
	}()
	go func() {
		if inboxErr := workspaceAssembly.Inbox.Run(workspaceCreateCtx); inboxErr != nil && workspaceCreateCtx.Err() == nil {
			daemonLog.With("operation", "workspace-creation-inbox").
				LogError("claude-repld: workspace creation inbox stopped: %v", inboxErr)
		}
	}()
	// Same late bind for the registrar's SessionView re-push, so a backfill
	// transition reaches a CONNECTED frontend rather than waiting for the next
	// unrelated push (F2).
	registrar.PushView = srv.RepushSessionView

	mux := http.NewServeMux()
	mux.Handle("/healthz", healthzHandler(ready, legacyLog))
	// Mount the API at every prefix its routes live under. Driven off
	// server.APIPrefixes rather than a hand-kept list here: anything not
	// mounted falls through to the SPA at "/" and is answered by the file
	// server, so a missing prefix reads as a 404 from the frontend rather than
	// as a routing bug.
	api := srv.Handler()
	for _, prefix := range server.APIPrefixes {
		mux.Handle(prefix, api)
	}
	if h := webappHandler(*webappDir, legacyLog); h != nil {
		mux.Handle("/", h)
	}
	// Widget assets are served in place from wherever they were built
	// (e.g. an explanation-engine checkout's dist), never copied into
	// this repo: the mount existing is what the webapp's capability
	// probe detects.
	if *widgetAssets != "" {
		mux.Handle("/widget-assets/", http.StripPrefix("/widget-assets/", http.FileServer(http.Dir(*widgetAssets))))
	}
	// Unfiltered frontend.v1 consumers: the /frontend WS endpoint and the Emacs
	// UDS listener, both serving every workspace's frames. (The webapp's scoped
	// view rides GET /workspace-stream?workspace=<dir> or GET
	// /sessions/{id}/stream, scope-filtered by the server handler; both mount
	// off server.APIPrefixes above.)
	mux.HandleFunc("/frontend", agentShim.Server.ServeWS)
	sockPath, perr := frontend.DefaultSocketPath()
	if perr != nil {
		daemonFatal(daemonLog, "claude-repld: frontend socket path: %v", perr)
	}
	frontendListener, err := frontend.ListenUDS(sockPath)
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: frontend UDS listen %s: %v", sockPath, err)
	}
	go func() {
		daemonLog.With("operation", "serve-frontend-uds", "socket", sockPath).Log("claude-repld: frontend UDS listening")
		if serveErr := agentShim.Server.Serve(frontendListener); serveErr != nil {
			daemonLog.With("operation", "serve-frontend-uds", "socket", sockPath).Log("claude-repld: frontend UDS serve ended: %v", serveErr)
		}
	}()

	httpServer := &http.Server{Addr: *addr, Handler: mux}
	httpListener, err := net.Listen("tcp", *addr)
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: HTTP listen %s: %v", *addr, err)
	}

	sigCh := make(chan os.Signal, 1)
	signal.Notify(sigCh, os.Interrupt, syscall.SIGTERM)
	go func() {
		req := shutdownRequest{cause: sessioncontroller.StopCauseDaemonShutdown()}
		select {
		case sig := <-sigCh:
			daemonLog.With("operation", "shutdown", "signal", sig).Log("claude-repld: signal received, shutting down with shims preserved")
		case req = <-shutdownReq:
			daemonLog.With("operation", "shutdown", "source", "frontend", "stop_shims", req.stopShims, "cause", req.cause.String()).Log("claude-repld: shutdown command received")
		}
		ready.ready.Store(false)
		daemonLog.With("operation", "shutdown-stop-workspace-creation").Log("claude-repld: shutdown step: stopping workspace creation workers")
		cancelWorkspaceCreate()
		daemonLog.With("operation", "shutdown-all-sessions").Log("claude-repld: shutdown step: stopping session work (idle sweeper drain, shim stop decisions)")
		srv.ShutdownAll(req.stopShims, req.cause)
		// Login terminals are children of THIS process, so a daemon that
		// exits without killing them strands an orphaned claude TUI on a pty
		// nobody is reading.
		daemonLog.With("operation", "shutdown-close-logins").Log("claude-repld: shutdown step: closing login terminals")
		logins.CloseAll()
		// Belt-and-suspenders: the registry is write-through crash-safe
		// (SIGKILL loses nothing), so this flush is an optimization that
		// re-asserts the on-disk state after the drain, never the
		// mechanism durability depends on.
		daemonLog.With("operation", "shutdown-flush-registry").Log("claude-repld: shutdown step: flushing session registry")
		if err := sessionRegistry.Flush(); err != nil {
			daemonLog.With("operation", "shutdown-flush-registry").LogError("claude-repld: registry flush on shutdown: %v", err)
		} else {
			daemonLog.With("operation", "shutdown-flush-registry").Log("claude-repld: session registry flushed")
		}
		daemonLog.With("operation", "shutdown-close-http").Log("claude-repld: shutdown step: closing HTTP server")
		if err := httpServer.Close(); err != nil {
			daemonLog.With("operation", "shutdown-close-http").LogError("claude-repld: http close: %v", err)
		} else {
			daemonLog.With("operation", "shutdown-close-http").Log("claude-repld: HTTP server closed")
		}
	}()

	// Both listeners are bound, the frontend has subscribed to the durable
	// workspace bridge, and the inbox has a live daemon session target.  Only
	// this completed state may report readiness.
	ready.ready.Store(true)

	// BOOT RECONCILIATION, strictly AFTER readiness (bootsweep.go). Shims that
	// outlived the previous daemon are already redialling this process's
	// listener; without this nothing ever claims them, and a restart leaves
	// every surviving session unclaimed and every workspace blue until some
	// later act happens to bring it up. It is reconciliation, not a boot
	// dependency, so it must never sit in front of /healthz.
	sweepCtx, cancelSweep := context.WithCancel(context.Background())
	defer cancelSweep()
	go (&server.BootSweeper{
		Reg:       sessionRegistry,
		Connected: shimListener.Connected,
		Held:      sessionlock.Held,
		Ensurer:   controller,
		Logf:      legacyLog,
	}).Run(sweepCtx)
	daemonLog.With("operation", "serve-http", "version", daemonVersion, "address", *addr,
		"shim", *shimScript, "workspace_create_inbox", workspaceAssembly.Inbox.Dir).
		Log("claude-repld listening with healthz ready")
	if err := httpServer.Serve(httpListener); err != nil && err != http.ErrServerClosed {
		daemonFatal(daemonLog, "claude-repld: %v", err)
	}
}

// openPprofSurface binds the opt-in profiling surface and records the decision
// either way.
//
// BOTH OUTCOMES ARE LOGGED. "Off" is the shipped state and must be
// distinguishable from "on but nobody can find the address", and an enabled
// surface is only usable if its record names the exact socket or port a
// `go tool pprof` invocation should target. A configured surface that cannot
// bind is a hard error: an operator who asked for profiles and silently got
// none is the failure this whole addition exists to end.
func openPprofSurface(addr string, logger *dlog.Logger) (*pprofsurface.Surface, error) {
	surface, err := pprofsurface.Open(addr)
	if err != nil {
		return nil, err
	}
	if surface == nil {
		if err := logger.EmitVerbose(dlog.GlobalScope(), dlog.Event{
			Runtime: dlog.RuntimeDaemon, Level: dlog.LevelDebug, Operation: "daemon.pprof.disabled",
			Message: "Go profiling surface is off",
			Context: map[string]any{"env": pprofsurface.EnvAddr, "flag": "-pprof"},
		}); err != nil {
			return nil, fmt.Errorf("claude-repld: record pprof surface state: %w", err)
		}
		return nil, nil
	}
	if err := logger.EmitNormal(dlog.GlobalScope(), dlog.Event{
		Runtime: dlog.RuntimeDaemon, Level: dlog.LevelWarn, Operation: "daemon.pprof.enabled",
		Message: "Go profiling surface is LISTENING; it exposes goroutine stacks, the command line and heap contents",
		Context: map[string]any{
			"network": surface.Network(), "address": surface.Address(),
			"url": surface.URL(), "env": pprofsurface.EnvAddr,
		},
	}); err != nil {
		surface.Close()
		return nil, fmt.Errorf("claude-repld: record pprof surface state: %w", err)
	}
	go func() {
		if err := surface.Serve(); err != nil && !errors.Is(err, http.ErrServerClosed) {
			logger.With("operation", "daemon.pprof.serve").LogError("claude-repld: pprof surface serve ended: %v", err)
		}
	}()
	return surface, nil
}

func startWorkspaceLogMaintenance(targets *dlog.TargetManager, logger *dlog.Logger, terminal io.Writer, verbose bool) func() {
	return startWorkspaceLogMaintenanceAtInterval(
		targets, logger, terminal, verbose, dlog.WorkspaceRuntimeCapInterval, nil,
	)
}

// onTick, when non-nil, is invoked after EVERY completed maintenance pass. It
// is the injectable completion boundary a test synchronizes on: the pass runs
// on its own goroutine, and a test that waited by sampling the target's size
// was measuring the clock rather than rendezvousing with the work.
func startWorkspaceLogMaintenanceAtInterval(targets *dlog.TargetManager, logger *dlog.Logger, terminal io.Writer, verbose bool, interval time.Duration, onTick func()) func() {
	if interval <= 0 {
		panic("claude-repld: workspace log maintenance interval must be positive")
	}
	if logger == nil {
		panic("claude-repld: workspace log maintenance needs a daemon logger for its active-target gauge")
	}
	stop := make(chan struct{})
	done := make(chan struct{})
	go func() {
		defer close(done)
		ticker := time.NewTicker(interval)
		defer ticker.Stop()
		for {
			select {
			case <-ticker.C:
				maintainWorkspaceLogTargets(targets, logger, terminal, verbose)
				if onTick != nil {
					onTick()
				}
			case <-stop:
				return
			}
		}
	}()
	var once sync.Once
	return func() {
		once.Do(func() {
			close(stop)
			<-done
		})
	}
}

func maintainWorkspaceLogTargets(targets *dlog.TargetManager, logger *dlog.Logger, terminal io.Writer, verbose bool) {
	for _, failure := range targets.MaintainSizeCaps() {
		// Reporting first tries the affected workspace daemon logger, then
		// canonical JSON emergency output. An error means durable workspace
		// persistence failed, so continuing would silently lose the canonical
		// per-workspace record even when emergency output succeeded.
		if err := targets.ReportTargetCapError(failure, terminal, verbose); err != nil {
			panic(fmt.Sprintf("claude-repld: workspace log maintenance reporting failed: %v", err))
		}
	}
	// THE GAUGE. Targets are opened per workspace runtime and released when the
	// workspace closes, so this number is the observable form of that binding:
	// a count that only ever climbs across a long-lived daemon is the leak the
	// eviction path exists to prevent, and it is unfalsifiable without a
	// periodic reading.
	logger.With("operation", "daemon.logging.workspace-target-gauge",
		"active_targets", targets.ActiveTargets()).
		LogVerbose("claude-repld: workspace log targets held")
}

// parseAccounts decodes the -accounts flag: comma-separated label=dir
// pairs, where an empty dir names the CLI's own default root. An empty
// flag is the capability being unconfigured (nil roster), not an error;
// a malformed pair or duplicate label IS one — a half-parsed roster
// would silently offer the menu with entries missing.
func parseAccounts(raw string) ([]server.Account, error) {
	if raw == "" {
		return nil, nil
	}
	var accounts []server.Account
	seen := map[string]bool{}
	for _, pair := range strings.Split(raw, ",") {
		label, dir, ok := strings.Cut(pair, "=")
		if !ok || label == "" {
			return nil, fmt.Errorf("malformed pair %q (want label=config-dir)", pair)
		}
		if seen[label] {
			return nil, fmt.Errorf("duplicate label %q", label)
		}
		seen[label] = true
		accounts = append(accounts, server.Account{Label: label, ConfigDir: dir})
	}
	return accounts, nil
}

func closeOrLog(logger *dlog.Logger, c io.Closer, what string) {
	if err := c.Close(); err != nil {
		logger.With("operation", "close", "resource", what).Log("claude-repld: close failed: %v", err)
	}
}

// envStr returns the environment variable name, or def when it is unset
// or empty. Used to source a flag's default from the environment.
func envStr(name, def string) string {
	if v := os.Getenv(name); v != "" {
		return v
	}
	return def
}

// shimBuildSHA returns a reader for the build identity of the shim bundle at
// shimScript: the `.built-sha` stamp bin/build-frontend.sh writes beside it
// from the very value it injected into the bundle.
//
// It is read PER CALL rather than captured at boot, because a deploy can land
// while this daemon runs — that is the ordinary case, since build-frontend
// rebuilds before the bounce — and a boot-time snapshot would compare every
// surviving shim against the bundle that was current when the daemon started.
//
// An unreadable or absent stamp reports "", which the session controller reads as UNKNOWN
// and never as a mismatch: a checkout with no stamp must not bounce every shim
// it meets.
func shimBuildSHA(shimScript string) func() string {
	stamp := filepath.Join(filepath.Dir(shimScript), ".built-sha")
	return func() string {
		b, err := os.ReadFile(stamp)
		if err != nil {
			return ""
		}
		return strings.TrimSpace(string(b))
	}
}
