// Command claude-repld is the Go daemon between the Emacs/webapp
// frontend clients and per-session TS shim subprocesses. Wire formats:
// modules/app/agent-repl/proto/agentshim/ for the protobuf planes, and
// internal/protocol for the pre-cutover stdio/WebSocket NDJSON planes.
package main

import (
	"bufio"
	"context"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"
	"strings"
	"sync"
	"sync/atomic"
	"syscall"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/frontend"
	"claude-repld/internal/login"
	"claude-repld/internal/progress"
	"claude-repld/internal/registry"
	"claude-repld/internal/remediation"
	"claude-repld/internal/replog"
	"claude-repld/internal/server"
	"claude-repld/internal/sessioncontroller"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/shim"
	"claude-repld/internal/shimlisten"
	"claude-repld/internal/ssm"
	"claude-repld/internal/statedb"
	"claude-repld/internal/stateroot"
)

// shimProtocolVersion is the agent-shim wire protocol version the daemon's
// per-session shimclient negotiates in DaemonHello; it must equal the shim's.
const shimProtocolVersion = "1"

const daemonVersion = "0.1.0"

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
		fs.ServeHTTP(w, r)
	})
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
		Timestamp: time.Now().UTC(), Runtime: dlog.RuntimeDaemon, PID: os.Getpid(),
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
	os.Exit(1)
}

// canonicalShimCreateOpts makes the child process and shim protocol use the
// exact canonical path whose MD5 identifies the workspace log targets.
func canonicalShimCreateOpts(opts server.CreateOpts) (dlog.Workspace, server.CreateOpts, error) {
	workspace, err := dlog.WorkspaceFromDirectory(opts.CWD)
	if err != nil {
		return dlog.Workspace{}, server.CreateOpts{}, err
	}
	canonical := opts
	canonical.CWD = workspace.Directory
	return workspace, canonical, nil
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
func (l *udsShimLogger) MirrorShimRecord(line string) { fmt.Fprintln(l.terminal, line) }
func main() {
	bootedAt := time.Now()

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
	daemonLog := dlog.New(cappedLog, os.Stderr, os.Getenv("AGENT_REPL_LOG_VERBOSE") != "")
	targets := dlog.NewTargetManager()
	defer func() {
		if err := targets.Close(); err != nil {
			daemonLog.With("operation", "close-workspace-log-targets").Log("claude-repld: close workspace log targets: %v", err)
		}
	}()
	stopWorkspaceLogMaintenance := startWorkspaceLogMaintenance(
		targets, os.Stderr, os.Getenv("AGENT_REPL_LOG_VERBOSE") != "",
	)
	defer stopWorkspaceLogMaintenance()
	legacyLog := dlog.Legacy(daemonLog)
	daemonLog.With("operation", "boot", "pid", os.Getpid(), "log_path", cappedLog.Name()).Log("claude-repld: booted")
	for _, w := range logWarnings {
		daemonLog.With("operation", "boot").Log("claude-repld: %s", w)
	}

	binaryMTime := launchedBinaryMTime(daemonLog)

	var (
		addr           = flag.String("addr", "127.0.0.1:8787", "listen address")
		nodeBin        = flag.String("node", "node", "node binary used to run the shim")
		claudeBin      = flag.String("claude-bin", "", "path to the claude CLI the SDK drives (empty = the SDK's bundled native Claude Code binary)")
		shimScript     = flag.String("shim", "", "path to the shim entrypoint (agent-shim/claude/shim/dist/main.js)")
		fake           = flag.Bool("fake", false, "force --fake (offline scripted SDK) on every session")
		idleTimeout    = flag.Duration("idle-timeout", time.Hour, "hibernate a session (SIGTERM its UDS shim; keep the record rehydratable) after this long with nothing happening to its workspace; 0 disables. Measured from the newest row on the workspace's state log, so a finished turn starts the clock rather than arming an immediate sweep. A hibernated session costs a full shim bring-up on the next act, so the window is generous rather than tight")
		webappDir      = flag.String("webapp", "", "optional directory of webapp static files to serve at /")
		widgetAssets   = flag.String("widget-assets", envStr("AGENT_REPL_WIDGET_ASSETS", ""), "optional directory of embeddable-widget assets (e.g. a chess-widget dist) to serve at /widget-assets/; empty = capability off")
		remediationDir = flag.String("remediation-dir", "", "checkout the \"session gone\" analyst diagnoses and opens a resilience workspace against (empty = remediation disabled)")
		remediationPM  = flag.String("remediation-permission-mode", "", "--permission-mode for the \"session gone\" analyst (empty = the CLI default, under which every headless tool call is auto-denied)")
		//nolint:lll // the consent's whole job is to state what it consents to.
		remediationUngated = flag.Bool("allow-ungated-remediation", false, "consent to running the \"session gone\" analyst with NO permission gate; required when -remediation-permission-mode is ungated (bypassPermissions), because that analyst then approves its own tool calls against -remediation-dir unattended. Without it such a config REFUSES to boot rather than running ungated by default")
		accountsFlag       = flag.String("accounts", "", "canonical account roster as comma-separated label=config-dir pairs (empty dir = the CLI's default root), e.g. \"personal=,work=/home/u/.claude-chesscom\"; empty = account routes disabled")
	)
	flag.Parse()

	accounts, err := parseAccounts(*accountsFlag)
	if err != nil {
		daemonLog.With("operation", "parse-accounts").Log("claude-repld: -accounts: %v", err)
		os.Exit(2)
	}

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
	// it again.
	legacyRegistryPath, err := registry.LegacyJSONPath()
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: %v", err)
	}
	sessionRegistry := registry.OpenWith(registry.Options{
		DB:             stateStore,
		LegacyJSONPath: legacyRegistryPath,
		Logf:           legacyLog,
	})
	if err := sessionRegistry.Prepare(); err != nil {
		daemonFatal(daemonLog, "claude-repld: registry prepare: %v", err)
	}

	// "session gone" remediation: the frontend can only report the loss,
	// so the daemon owns the analyst that diagnoses it and opens the
	// resilience workspace. Disabled when no checkout is nominated.
	var remediator server.Remediator
	if *remediationDir != "" {
		runner, err := remediation.New(remediation.Config{
			Bin:            *claudeBin,
			Dir:            *remediationDir,
			PermissionMode: *remediationPM,
			AllowUngated:   *remediationUngated,
			Start: func(argv []string, dir string) error {
				return startAnalyst(daemonLog.With("operation", "remediation-analyst", "cwd", dir), argv, dir)
			},
			Logf: legacyLog,
		}, bootedAt)
		if err != nil {
			daemonFatal(daemonLog, "claude-repld: %v", err)
		}
		remediator = runner
	}

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
	shutdownReq := make(chan bool, 1)
	var shutdownOnce sync.Once
	requestShutdown := func(stopShims bool) {
		shutdownOnce.Do(func() { shutdownReq <- stopShims; close(shutdownReq) })
	}

	// The SSM (resolved per-workspace state) is opened here and shared by BOTH
	// the frontend snapshot/merge push loop AND the per-session controller's
	// lifecycle-event application; one owner (main) closes it once.
	ssmMgr, err := ssm.Open(ssm.Options{
		DB:       stateStore,
		Resolver: server.NewRegistryResolver(sessionRegistry),
		Logf:     legacyLog,
	})
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: open SSM: %v", err)
	}
	defer ssmMgr.Close()

	// The progress-footer resolver (F1) is the SSM's sibling and is owned here
	// for the same reason: both the frontend push loop and the per-session
	// controller feed it, so one owner closes it once.
	progressMgr := progress.New(progress.Options{Logf: legacyLog})
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
	udsSpawn := func(sessionID string, opts server.CreateOpts) (func() error, error) {
		workspace, canonicalOpts, workspaceErr := canonicalShimCreateOpts(opts)
		if workspaceErr != nil {
			return nil, fmt.Errorf("resolve UDS shim workspace for %q: %w", opts.CWD, workspaceErr)
		}
		workspaceLog, targetErr := targets.OpenWorkspaceLogger(workspace, os.Stderr, os.Getenv("AGENT_REPL_LOG_VERBOSE") != "")
		if targetErr != nil {
			return nil, fmt.Errorf("open daemon workspace logger for %q: %w", workspace.Directory, targetErr)
		}
		shimTarget, targetErr := targets.OpenWorkspaceRuntime(workspace, dlog.RuntimeShim)
		if targetErr != nil {
			return nil, fmt.Errorf("open shim log target for workspace %q: %w", workspace.Directory, targetErr)
		}
		argv := append(server.ShimUDSArgv(*nodeBin, *shimScript, sessionID, *fake, canonicalOpts, shimSocketPath), "--log-fd", "3")
		if *claudeBin != "" {
			argv = append(argv, "--claude-bin", *claudeBin)
		}
		spawnEvent := func(level dlog.Level, message string, context map[string]any) {
			// EmitWorkspaceNormal owns its JSON emergency path when durable
			// persistence fails. A second plaintext stderr line would duplicate
			// the error and violate the all-JSON logging contract.
			if err := workspaceLog.EmitWorkspaceNormal(workspace, dlog.Event{Runtime: dlog.RuntimeDaemon, Level: level, Operation: "shim.spawn", Message: message, Context: context, AgentReplSessionID: sessionID}); err != nil {
				panic(err)
			}
		}
		spawnEvent(dlog.LevelInfo, "spawning UDS shim", map[string]any{"cwd": workspace.Directory, "model": opts.Model, "config_dir": opts.ConfigDir, "daemon_socket": shimSocketPath})
		proc, spawnErr := shim.Spawn(shim.Options{
			Argv:       argv,
			Dir:        workspace.Directory,
			ExtraEnv:   server.ShimEnv(opts, *addr),
			ExtraFiles: []*os.File{shimTarget},
			Logger:     &udsShimLogger{workspace: workspace, daemon: workspaceLog, terminal: os.Stderr, sessionID: sessionID},
		})
		if spawnErr != nil {
			spawnEvent(dlog.LevelError, "UDS shim spawn failed", map[string]any{"error": spawnErr.Error(), "cwd": workspace.Directory})
			return nil, spawnErr
		}
		spawnEvent(dlog.LevelInfo, "UDS shim spawned", map[string]any{"argv": strings.Join(argv, " "), "cwd": workspace.Directory})
		// In UDS mode the shim streams over its socket, not stdout, so its
		// stdout event channel stays empty — drain it so the stdout pump never
		// blocks, and reap the process when it exits.
		go func() {
			for range proc.Events() { //nolint:revive
			}
		}()
		go func() {
			if werr := proc.Wait(); werr != nil {
				spawnEvent(dlog.LevelError, "UDS shim exited", map[string]any{"error": werr.Error(), "cwd": workspace.Directory})
			}
		}()
		return func() error { return proc.Terminate() }, nil
	}
	// Held by pointer so its SessionView re-push can be late-bound below: the
	// Server it pushes through does not exist yet (same shape as forwarder).
	modelCatalogs := server.NewSessionModelCatalogs()
	registrar := &server.RegistryRegistrar{Reg: sessionRegistry, Logf: legacyLog, ModelCatalogs: modelCatalogs}
	// One registry adapter serves both durable seq marks: last_seen_seq (the
	// shimclient replay high-water) and newest_clear_or_compact_seq (the
	// frontend replay floor).
	seqStore := server.NewRegistrySeqStore(sessionRegistry, legacyLog)
	fileDiagnostics, err := server.NewTargetFileDiagnosticPersister(targets, os.Stderr, os.Getenv("AGENT_REPL_LOG_VERBOSE") != "")
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: build sidecar diagnostic persister: %v", err)
	}
	// The below-floor history re-pull needs no wiring of its own: it rides the
	// session's existing shim connection as a ReplayRequest, so the store stays
	// behind the agent-shim facade (sessioncontroller/repull.go).
	controller, err := sessioncontroller.New(sessioncontroller.Config{
		Push:              forwarder,
		SSM:               ssmMgr,
		Progress:          progressMgr,
		Spawner:           server.NewShimSpawner(sessionRegistry, shimListener.Connected, udsSpawn, legacyLog),
		Source:            &server.ShimConnSource{Listener: shimListener},
		FileDiagnostics:   fileDiagnostics,
		Locator:           &server.SessionLocator{Reg: sessionRegistry},
		SeqStore:          seqStore,
		ClearCompactStore: seqStore,
		PermissionModes:   server.NewRegistryModeStore(sessionRegistry),
		Registrar:         registrar,
		ModelCatalogs:     registrar,
		DaemonVersion:     daemonVersion,
		ProtocolVersion:   shimProtocolVersion,
		Logf:              legacyLog,
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

	// Agent-shim frontend.v1 surface (design §9.1, §14.2): the SSM-backed
	// snapshot + merge Engine + frontend Server. Always on post-cutover — it is
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
		Health:         sessionControllerHealthProbe{Controller: controller, Logf: legacyLog},
		InitialPrompts: controller,
		Logf:           legacyLog,
		InboxInterval:  time.Second,
	})
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: initialize workspace creation: %v", err)
	}
	workspaceBridge, err := NewWorkspaceCreationBridge(workspaceCreateCtx, workspaceAssembly.Manager, workspaceAssembly.Store)
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: initialize workspace creation bridge: %v", err)
	}
	if err := workspaceAssembly.Forwarder.SetTargets(workspaceBridge, workspaceBridge); err != nil {
		daemonFatal(daemonLog, "claude-repld: bind workspace creation host forwarder: %v", err)
	}
	clientLogs, err := server.NewTargetClientLogWriter(
		targets,
		&server.RegistryClientLogIdentityResolver{Reg: sessionRegistry},
		os.Stderr,
		os.Getenv("AGENT_REPL_LOG_VERBOSE") != "",
	)
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: build client log writer: %v", err)
	}
	// NEVER-BLUE (workspaceopen.go): bind each registered workspace to its
	// on-disk transcript at boot, so a restart already knows every resume
	// target before a frontend can connect, and ensure eagerly on open.
	opener := &server.WorkspaceOpener{
		Reg:        sessionRegistry,
		Ensurer:    controller,
		ConfigDirs: knownConfigDirs(accounts),
		Logf:       legacyLog,
	}
	opener.BindAll()
	agentShim, err := server.WireAgentShim(server.AgentShimConfig{
		SSM:               ssmMgr,
		Progress:          progressMgr,
		Prompts:           controller,
		Turns:             controller,
		Health:            controller,
		Restarts:          controller,
		DaemonHealth:      ready,
		Lifecycle:         opener,
		Sessions:          registrySessions{reg: sessionRegistry, controller: controller, modelCatalogs: modelCatalogs, logf: legacyLog},
		Inits:             controller,
		Catalogs:          controller,
		Queues:            controller,
		SessionCommands:   sessionCommands,
		Resyncer:          controller,
		WorkspaceCreation: workspaceBridge,
		RequestShutdown:   requestShutdown,
		ClientLogs:        clientLogs,
		// The daemon resolves a workspace's conversation for itself. Frontends
		// send an intent (continue / fresh / explicit), never a remembered
		// vendor uuid — see server.ConversationResolver.
		Resumes: &server.ConversationResolver{Reg: sessionRegistry, Logf: legacyLog},
		Logf:    legacyLog,
	})
	if err != nil {
		daemonFatal(daemonLog, "claude-repld: frontend surface: %v", err)
	}
	// Bind the session controller's push target now that the frontend server exists.
	forwarder.SetTarget(agentShim.Server)
	defer func() {
		if cerr := agentShim.Close(); cerr != nil {
			daemonLog.With("operation", "close-frontend-surface").Log("claude-repld: frontend surface close: %v", cerr)
		}
	}()

	srv := server.New(server.Config{
		Logf:            legacyLog,
		DaemonVersion:   daemonVersion,
		BinaryMTime:     binaryMTime,
		ForceFake:       *fake,
		Remediator:      remediator,
		Registry:        sessionRegistry,
		ModelCatalogs:   modelCatalogs,
		Logins:          logins,
		Accounts:        accounts,
		IdleTimeout:     *idleTimeout,
		WidgetAssetsDir: *widgetAssets,
		DaemonAddr:      *addr,
		Controller:      controller,
		SSM:             ssmMgr,
		Frontend:        agentShim.Server,
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
	go func() {
		if inboxErr := workspaceAssembly.Inbox.Run(workspaceCreateCtx); inboxErr != nil && workspaceCreateCtx.Err() == nil {
			daemonLog.With("operation", "workspace-creation-inbox", "level", "error").
				Log("claude-repld: workspace creation inbox stopped: %v", inboxErr)
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
	// UDS listener, both serving every workspace's frames. (The webapp's
	// per-session view rides GET /sessions/{id}/stream, scope-filtered by the
	// server handler.)
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
		stopShims := false
		select {
		case sig := <-sigCh:
			daemonLog.With("operation", "shutdown", "signal", sig).Log("claude-repld: signal received, shutting down with shims preserved")
		case stopShims = <-shutdownReq:
			daemonLog.With("operation", "shutdown", "source", "frontend", "stop_shims", stopShims).Log("claude-repld: shutdown command received")
		}
		ready.ready.Store(false)
		cancelWorkspaceCreate()
		srv.ShutdownAll(stopShims)
		// Login terminals are children of THIS process, so a daemon that
		// exits without killing them strands an orphaned claude TUI on a pty
		// nobody is reading.
		logins.CloseAll()
		// Belt-and-suspenders: the registry is write-through crash-safe
		// (SIGKILL loses nothing), so this flush is an optimization that
		// re-asserts the on-disk state after the drain, never the
		// mechanism durability depends on.
		if err := sessionRegistry.Flush(); err != nil {
			daemonLog.With("operation", "shutdown-flush-registry").Log("claude-repld: registry flush on shutdown: %v", err)
		}
		if err := httpServer.Close(); err != nil {
			daemonLog.With("operation", "shutdown-close-http").Log("claude-repld: http close: %v", err)
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

func startWorkspaceLogMaintenance(targets *dlog.TargetManager, terminal io.Writer, verbose bool) func() {
	return startWorkspaceLogMaintenanceAtInterval(
		targets, terminal, verbose, dlog.WorkspaceRuntimeCapInterval,
	)
}

func startWorkspaceLogMaintenanceAtInterval(targets *dlog.TargetManager, terminal io.Writer, verbose bool, interval time.Duration) func() {
	if interval <= 0 {
		panic("claude-repld: workspace log maintenance interval must be positive")
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
				maintainWorkspaceLogTargets(targets, terminal, verbose)
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

func maintainWorkspaceLogTargets(targets *dlog.TargetManager, terminal io.Writer, verbose bool) {
	for _, failure := range targets.MaintainSizeCaps() {
		// Reporting first tries the affected workspace daemon logger, then
		// canonical JSON emergency output. An error means durable workspace
		// persistence failed, so continuing would silently lose the canonical
		// per-workspace record even when emergency output succeeded.
		if err := targets.ReportTargetCapError(failure, terminal, verbose); err != nil {
			panic(fmt.Sprintf("claude-repld: workspace log maintenance reporting failed: %v", err))
		}
	}
}

// startAnalyst launches the headless remediation analyst and returns as
// soon as it is running: the caller is an HTTP handler, and the analyst
// itself runs for as long as diagnosing a lost session takes. Its output
// is pumped into the daemon log so the plan it devises is visible in the
// same place the failure was.
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

func startAnalyst(logger *dlog.Logger, argv []string, dir string) error {
	// An os.Pipe (rather than Cmd.StdoutPipe) hands the child a raw fd
	// for both streams, so the reader below is independent of Wait's
	// pipe bookkeeping and the two cannot race.
	pr, pw, err := os.Pipe()
	if err != nil {
		return fmt.Errorf("analyst pipe: %w", err)
	}
	cmd := exec.Command(argv[0], argv[1:]...)
	cmd.Dir = dir
	cmd.Stdout = pw
	cmd.Stderr = pw
	if err := cmd.Start(); err != nil {
		closeOrLog(logger, pr, "analyst pipe read end")
		closeOrLog(logger, pw, "analyst pipe write end")
		return fmt.Errorf("start %s: %w", argv[0], err)
	}
	// The child owns its dup of the write end now; dropping ours is what
	// lets the reader see EOF when the analyst exits.
	closeOrLog(logger, pw, "analyst pipe write end")
	go pumpAnalystOutput(logger, pr)
	go func() {
		if err := cmd.Wait(); err != nil {
			logger.Log("claude-repld: remediation analyst exited: %v", err)
			return
		}
		logger.Log("claude-repld: remediation analyst finished")
	}()
	return nil
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

// pumpAnalystOutput mirrors the analyst's output into the daemon log.
func pumpAnalystOutput(logger *dlog.Logger, out io.Reader) {
	scanner := bufio.NewScanner(out)
	scanner.Buffer(make([]byte, 0, 64*1024), 1024*1024)
	for scanner.Scan() {
		if line := strings.TrimSpace(scanner.Text()); line != "" {
			logger.LogVerbose("claude-repld: remediation: %s", line)
		}
	}
	if err := scanner.Err(); err != nil {
		logger.Log("claude-repld: remediation output: %v", err)
	}
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
