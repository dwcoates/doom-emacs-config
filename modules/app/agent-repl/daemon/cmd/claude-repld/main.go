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
	"strings"
	"syscall"
	"time"

	"claude-repld/internal/login"
	"claude-repld/internal/registry"
	"claude-repld/internal/remediation"
	"claude-repld/internal/sentinel"
	"claude-repld/internal/server"
	"claude-repld/internal/session"
	"claude-repld/internal/shim"
)

const daemonVersion = "0.1.0"

func main() {
	bootedAt := time.Now()

	var (
		addr           = flag.String("addr", "127.0.0.1:8787", "listen address")
		nodeBin        = flag.String("node", "node", "node binary used to run the shim")
		claudeBin      = flag.String("claude-bin", "", "path to the claude CLI the SDK drives (empty = SDK-bundled cli.js)")
		shimScript     = flag.String("shim", "", "path to the shim entrypoint (shim/dist/main.js)")
		fake           = flag.Bool("fake", false, "force --fake (offline scripted SDK) on every session")
		retention      = flag.Int("retention", 4096, "per-session frame retention window for replay")
		webappDir      = flag.String("webapp", "", "optional directory of webapp static files to serve at /")
		remediationDir = flag.String("remediation-dir", "", "checkout the \"session gone\" analyst diagnoses and opens a resilience workspace against (empty = remediation disabled)")
		remediationPM  = flag.String("remediation-permission-mode", "", "--permission-mode for the \"session gone\" analyst (empty = the CLI default, under which every headless tool call is auto-denied)")
	)
	flag.Parse()

	if *shimScript == "" {
		fmt.Fprintln(os.Stderr, "claude-repld: --shim is required (path to shim/dist/main.js)")
		os.Exit(2)
	}

	spawn := func(sessionID string, opts server.CreateOpts) (session.ShimHandle, error) {
		argv := server.ShimArgv(*nodeBin, *shimScript, sessionID, *fake, opts)
		if *claudeBin != "" {
			argv = append(argv, "--claude-bin", *claudeBin)
		}
		proc, err := shim.Spawn(shim.Options{
			Argv: argv,
			Dir:  opts.CWD,
			// The SDK's claude subprocess inherits this overlay: the
			// ownership marker (whose hook scripts stamp sentinel line 3,
			// so Emacs accepts session-id updates from this CLI) and the
			// session's CLAUDE_CONFIG_DIR (which account it runs as).
			ExtraEnv: server.ShimEnv(opts),
		})
		if err != nil {
			return nil, err
		}
		go func() {
			if err := proc.Wait(); err != nil {
				log.Printf("claude-repld: shim for session %s exited: %v", sessionID, err)
			}
		}()
		return proc, nil
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

	srv := server.New(server.Config{
		DaemonVersion: daemonVersion,
		Retention:     *retention,
		ForceFake:     *fake,
		Spawn:         spawn,
		Sentinel:      sentinelWriter,
		Remediator:    remediator,
		Registry:      sessionRegistry,
		Logins:        logins,
	})

	mux := http.NewServeMux()
	mux.Handle("/sessions", srv.Handler())
	mux.Handle("/sessions/", srv.Handler())
	mux.Handle("/remediation", srv.Handler())
	if *webappDir != "" {
		mux.Handle("/", http.FileServer(http.Dir(*webappDir)))
	}

	httpServer := &http.Server{Addr: *addr, Handler: mux}

	sigCh := make(chan os.Signal, 1)
	signal.Notify(sigCh, os.Interrupt, syscall.SIGTERM)
	go func() {
		sig := <-sigCh
		log.Printf("claude-repld: %v received, shutting down sessions", sig)
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
