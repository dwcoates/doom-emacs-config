// Command claude-repld is the Go daemon between the Emacs/webapp
// WebSocket clients (Layer 2) and per-session TS shim subprocesses
// (Layer 1). Wire formats: modules/app/agent-repl/shared/protocol.md.
package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"syscall"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/frontend"
	"claude-repld/internal/login"
	"claude-repld/internal/registry"
	"claude-repld/internal/remediation"
	"claude-repld/internal/replog"
	"claude-repld/internal/sentinel"
	"claude-repld/internal/server"
	"claude-repld/internal/sessiondrv"
	"claude-repld/internal/shim"
	"claude-repld/internal/ssm"
	"claude-repld/internal/stateroot"
)

// shimProtocolVersion is the agent-shim wire protocol version the daemon's
// per-session shimclient negotiates in DaemonHello; it must equal the shim's.
const shimProtocolVersion = "1"

const daemonVersion = "0.1.0"

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
func launchedBinaryMTime() int64 {
	exe, err := os.Executable()
	if err != nil {
		log.Printf("claude-repld: cannot resolve own executable for staleness reporting: %v", err)
		return 0
	}
	info, err := os.Stat(exe)
	if err != nil {
		log.Printf("claude-repld: cannot stat own executable %q for staleness reporting: %v", exe, err)
		return 0
	}
	return info.ModTime().Unix()
}

func main() {
	bootedAt := time.Now()

	// Disk log, wired before anything can log: every log.Printf line
	// lands in both stderr (the *claude-repld* buffer Emacs captures)
	// and a per-run file under the state root, so daemon history
	// survives the buffer and stays readable without a live editor.
	// An unresolvable root or unopenable file is fatal — a daemon whose
	// evidence trail cannot exist is exactly the failure this log is
	// for, and Fatalf still reaches stderr.
	logRoot, err := stateroot.Root()
	if err != nil {
		log.Fatalf("claude-repld: %v", err)
	}
	logFile, logWarnings, err := replog.Open(logRoot)
	if err != nil {
		log.Fatalf("claude-repld: %v", err)
	}
	// CappedWriter, not the bare file: a long-lived run that logs
	// unexpectedly heavily would otherwise grow claude-repld.log without
	// bound between restarts.
	cappedLog := replog.NewCappedWriter(logRoot, logFile, replog.CapBytes)
	defer cappedLog.Close()
	log.SetOutput(io.MultiWriter(os.Stderr, cappedLog))
	log.Printf("claude-repld: booted (pid %d); logging to %s", os.Getpid(), cappedLog.Name())
	for _, w := range logWarnings {
		log.Printf("claude-repld: %s", w)
	}

	binaryMTime := launchedBinaryMTime()

	var (
		addr           = flag.String("addr", "127.0.0.1:8787", "listen address")
		nodeBin        = flag.String("node", "node", "node binary used to run the shim")
		claudeBin      = flag.String("claude-bin", "", "path to the claude CLI the SDK drives (empty = SDK-bundled cli.js)")
		shimScript     = flag.String("shim", "", "path to the shim entrypoint (agent-shim/claude-shim/dist/main.js)")
		fake           = flag.Bool("fake", false, "force --fake (offline scripted SDK) on every session")
		idleTimeout    = flag.Duration("idle-timeout", 10*time.Minute, "hibernate a session (SIGTERM its UDS shim; keep the record rehydratable) after this long without a real act; 0 disables")
		webappDir      = flag.String("webapp", "", "optional directory of webapp static files to serve at /")
		widgetAssets   = flag.String("widget-assets", envStr("AGENT_REPL_WIDGET_ASSETS", ""), "optional directory of embeddable-widget assets (e.g. a chess-widget dist) to serve at /widget-assets/; empty = capability off")
		remediationDir = flag.String("remediation-dir", "", "checkout the \"session gone\" analyst diagnoses and opens a resilience workspace against (empty = remediation disabled)")
		remediationPM  = flag.String("remediation-permission-mode", "", "--permission-mode for the \"session gone\" analyst (empty = the CLI default, under which every headless tool call is auto-denied)")
		accountsFlag   = flag.String("accounts", "", "canonical account roster as comma-separated label=config-dir pairs (empty dir = the CLI's default root), e.g. \"personal=,work=/home/u/.claude-chesscom\"; empty = account routes disabled")
	)
	flag.Parse()

	accounts, err := parseAccounts(*accountsFlag)
	if err != nil {
		log.Printf("claude-repld: -accounts: %v", err)
		os.Exit(2)
	}

	if *shimScript == "" {
		log.Printf("claude-repld: --shim is required (path to agent-shim/claude-shim/dist/main.js)")
		os.Exit(2)
	}

	// Agent-state sentinel side channel (daemon -> Emacs), resolved from
	// the inherited AGENT_REPL_STATE_DIR exactly like the hook scripts.
	sentinelWriter, err := sentinel.NewWriter(log.Printf)
	if err != nil {
		log.Fatalf("claude-repld: %v", err)
	}
	defer sentinelWriter.Close()

	// Persistent session registry: the in-memory session map dies with
	// the process, so this write-through record store is what lets a
	// restarted daemon keep resolving the s_<hex> ids its frontends
	// still hold. A daemon that cannot even resolve WHERE the registry
	// lives must fail loudly at startup; a registry that fails to LOAD
	// starts empty and logs (inside Open), never refuses to boot.
	registryPath, err := registry.DefaultPath()
	if err != nil {
		log.Fatalf("claude-repld: %v", err)
	}
	sessionRegistry := registry.Open(registryPath, log.Printf)

	// "session gone" remediation: the frontend can only report the loss,
	// so the daemon owns the analyst that diagnoses it and opens the
	// resilience workspace. Disabled when no checkout is nominated.
	var remediator server.Remediator
	if *remediationDir != "" {
		runner, err := remediation.New(remediation.Config{
			Bin:            *claudeBin,
			Dir:            *remediationDir,
			PermissionMode: *remediationPM,
			Start:          startAnalyst,
			Logf:           log.Printf,
		}, bootedAt)
		if err != nil {
			log.Fatalf("claude-repld: %v", err)
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
		Start: login.Spawn([]string{loginBin, "/login"}),
		Logf:  log.Printf,
	})
	defer logins.CloseAll()

	// A single graceful-shutdown path serves BOTH SIGTERM and the
	// POST /shutdown route: Emacs uses the latter to bounce a stale daemon
	// it adopted from another Emacs (no local process handle to signal). The
	// closure only closes the channel, so it is safe to hand to the server
	// before the http.Server exists; sync.Once guards concurrent requests.
	shutdownReq := make(chan struct{})
	var shutdownOnce sync.Once
	requestShutdown := func() { shutdownOnce.Do(func() { close(shutdownReq) }) }

	// The SSM (resolved per-workspace state) is opened here and shared by BOTH
	// the frontend snapshot/merge push loop AND the per-session driver's
	// lifecycle-event application; one owner (main) closes it once.
	ssmMgr, err := ssm.Open(ssm.Options{
		Resolver: server.NewRegistryResolver(sessionRegistry),
		Logf:     log.Printf,
	})
	if err != nil {
		log.Fatalf("claude-repld: open SSM: %v", err)
	}
	defer ssmMgr.Close()

	// The per-session shim-driver consumes each session's UDS shim stream and
	// renders it onto the frontend surface + SSM. Its push target (the
	// frontend.Server) does not exist until WireAgentShim returns, so it pushes
	// through a late-bound forwarder whose target is set below.
	forwarder := &server.PushForwarder{Logf: log.Printf}
	// The driver spawns a fresh UDS-mode shim when no live one is listening. The
	// exec stays here (main owns node/shim paths); production omits
	// --store-socket so the shim defaults to the launchd singleton store. The
	// returned stop func SIGTERMs the shim on hibernation.
	udsSpawn := func(sessionID string, opts server.CreateOpts, socketPath string) (func() error, error) {
		argv := server.ShimUDSArgv(*nodeBin, *shimScript, sessionID, *fake, opts, socketPath)
		if *claudeBin != "" {
			argv = append(argv, "--claude-bin", *claudeBin)
		}
		done := dlog.Call(log.Printf, "uds shim spawn",
			"session", sessionID, "cwd", opts.CWD, "model", opts.Model,
			"config_dir", opts.ConfigDir, "socket", socketPath)
		proc, spawnErr := shim.Spawn(shim.Options{
			Argv:     argv,
			Dir:      opts.CWD,
			ExtraEnv: server.ShimEnv(opts, *addr),
			Logf:     dlog.Tag(log.Printf, "session", sessionID, "cwd", opts.CWD),
		})
		if spawnErr != nil {
			done("", spawnErr)
			return nil, spawnErr
		}
		done(strings.Join(argv, " "), nil)
		// In UDS mode the shim streams over its socket, not stdout, so its
		// stdout event channel stays empty — drain it so the stdout pump never
		// blocks, and reap the process when it exits.
		go func() {
			for range proc.Events() { //nolint:revive
			}
		}()
		go func() {
			if werr := proc.Wait(); werr != nil {
				log.Printf("claude-repld: UDS shim for session %s exited: %v", sessionID, werr)
			}
		}()
		return func() error { return proc.Terminate() }, nil
	}
	driver, err := sessiondrv.New(sessiondrv.Config{
		Push:            forwarder,
		SSM:             ssmMgr,
		Spawner:         server.NewShimSpawner(sessionRegistry, nil, udsSpawn, log.Printf),
		Locator:         &server.SessionLocator{Reg: sessionRegistry},
		SeqStore:        server.NewRegistrySeqStore(sessionRegistry, log.Printf),
		Registrar:       server.RegistryRegistrar{Reg: sessionRegistry, Logf: log.Printf},
		DaemonVersion:   daemonVersion,
		ProtocolVersion: shimProtocolVersion,
		Logf:            log.Printf,
	})
	if err != nil {
		log.Fatalf("claude-repld: build session driver: %v", err)
	}
	defer driver.Close()

	// Agent-shim frontend.v1 surface (design §9.1, §14.2): the SSM-backed
	// snapshot + merge Engine + frontend Server. Always on post-cutover — it is
	// the daemon's consumption plane, not an optional add.
	// sessionCommands is the late-bound daemon-core surface (session
	// create/delete + DaemonView) the frontend command handler and snapshot
	// provider need. Its *Server target does not exist until server.New below,
	// so bind it after — the same late-bind shape as forwarder.
	sessionCommands := &server.SessionCommandBinding{Logf: log.Printf}
	agentShim, err := server.WireAgentShim(server.AgentShimConfig{
		SSM:             ssmMgr,
		Prompts:         driver,
		MergeDirs:       pendingMergeDirs{},
		Lifecycle:       pendingLifecycle{},
		Sessions:        registrySessions{reg: sessionRegistry, driver: driver},
		Inits:           driver,
		SessionCommands: sessionCommands,
		Resyncer:        driver,
		RequestShutdown: requestShutdown,
		Logf:            log.Printf,
	})
	if err != nil {
		log.Fatalf("claude-repld: frontend surface: %v", err)
	}
	// Bind the driver's push target now that the frontend server exists.
	forwarder.SetTarget(agentShim.Server)
	defer func() {
		if cerr := agentShim.Close(); cerr != nil {
			log.Printf("claude-repld: frontend surface close: %v", cerr)
		}
	}()

	srv := server.New(server.Config{
		DaemonVersion:   daemonVersion,
		BinaryMTime:     binaryMTime,
		RequestShutdown: requestShutdown,
		ForceFake:       *fake,
		Sentinel:        sentinelWriter,
		Remediator:      remediator,
		Registry:        sessionRegistry,
		Logins:          logins,
		Accounts:        accounts,
		IdleTimeout:     *idleTimeout,
		WidgetAssetsDir: *widgetAssets,
		DaemonAddr:      *addr,
		Driver:          driver,
		SSM:             ssmMgr,
		Frontend:        agentShim.Server,
	})
	// Bind the session-command surface now that the *Server exists (createSession
	// /deleteSession UDS commands and the snapshot DaemonView delegate to it).
	sessionCommands.SetTarget(srv)

	mux := http.NewServeMux()
	mux.Handle("/sessions", srv.Handler())
	mux.Handle("/sessions/", srv.Handler())
	mux.Handle("/remediation", srv.Handler())
	mux.Handle("/accounts", srv.Handler())
	mux.Handle("/capabilities", srv.Handler())
	mux.Handle("/workspaces/", srv.Handler())
	mux.Handle("/shutdown", srv.Handler())
	if h := webappHandler(*webappDir, log.Printf); h != nil {
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
		log.Fatalf("claude-repld: frontend socket path: %v", perr)
	}
	go func() {
		log.Printf("claude-repld: frontend UDS listening on %s", sockPath)
		if serveErr := agentShim.Server.ServeUDS(sockPath); serveErr != nil {
			log.Printf("claude-repld: frontend UDS serve ended: %v", serveErr)
		}
	}()

	httpServer := &http.Server{Addr: *addr, Handler: mux}

	sigCh := make(chan os.Signal, 1)
	signal.Notify(sigCh, os.Interrupt, syscall.SIGTERM)
	go func() {
		select {
		case sig := <-sigCh:
			log.Printf("claude-repld: %v received, shutting down sessions", sig)
		case <-shutdownReq:
			log.Printf("claude-repld: POST /shutdown received, shutting down sessions")
		}
		srv.ShutdownAll()
		// Login terminals are children of THIS process, so a daemon that
		// exits without killing them strands an orphaned claude TUI on a pty
		// nobody is reading.
		logins.CloseAll()
		// Belt-and-suspenders: the registry is write-through crash-safe
		// (SIGKILL loses nothing), so this flush is an optimization that
		// re-asserts the on-disk state after the drain, never the
		// mechanism durability depends on.
		if err := sessionRegistry.Flush(); err != nil {
			log.Printf("claude-repld: registry flush on shutdown: %v", err)
		}
		if err := httpServer.Close(); err != nil {
			log.Printf("claude-repld: http close: %v", err)
		}
	}()

	log.Printf("claude-repld %s listening on %s (shim: %s)", daemonVersion, *addr, *shimScript)
	if err := httpServer.ListenAndServe(); err != nil && err != http.ErrServerClosed {
		log.Fatalf("claude-repld: %v", err)
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

func startAnalyst(argv []string, dir string) error {
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
		closeOrLog(pr, "analyst pipe read end")
		closeOrLog(pw, "analyst pipe write end")
		return fmt.Errorf("start %s: %w", argv[0], err)
	}
	// The child owns its dup of the write end now; dropping ours is what
	// lets the reader see EOF when the analyst exits.
	closeOrLog(pw, "analyst pipe write end")
	go pumpAnalystOutput(pr)
	go func() {
		if err := cmd.Wait(); err != nil {
			log.Printf("claude-repld: remediation analyst exited: %v", err)
			return
		}
		log.Printf("claude-repld: remediation analyst finished")
	}()
	return nil
}

func closeOrLog(c io.Closer, what string) {
	if err := c.Close(); err != nil {
		log.Printf("claude-repld: close %s: %v", what, err)
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

// envBool parses name as a bool default (via strconv), falling back to def
// when the variable is unset or unparseable. An unparseable value is
// surfaced loudly rather than silently treated as false.
func envBool(name string, def bool) bool {
	v := os.Getenv(name)
	if v == "" {
		return def
	}
	b, err := strconv.ParseBool(v)
	if err != nil {
		log.Printf("claude-repld: %s=%q is not a bool (%v); using default %t", name, v, err, def)
		return def
	}
	return b
}

// pumpAnalystOutput mirrors the analyst's output into the daemon log.
func pumpAnalystOutput(out io.Reader) {
	scanner := bufio.NewScanner(out)
	scanner.Buffer(make([]byte, 0, 64*1024), 1024*1024)
	for scanner.Scan() {
		if line := strings.TrimSpace(scanner.Text()); line != "" {
			log.Printf("claude-repld: remediation: %s", line)
		}
	}
	if err := scanner.Err(); err != nil {
		log.Printf("claude-repld: remediation output: %v", err)
	}
}
