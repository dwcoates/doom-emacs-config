// Command claude-repld is the Go daemon between the Emacs/webapp
// WebSocket clients (Layer 2) and per-session TS shim subprocesses
// (Layer 1). Wire formats: modules/app/agent-repl/shared/protocol.md.
package main

import (
	"flag"
	"fmt"
	"log"
	"net/http"
	"os"
	"os/signal"
	"syscall"

	"claude-repld/internal/server"
	"claude-repld/internal/session"
	"claude-repld/internal/shim"
)

const daemonVersion = "0.1.0"

func main() {
	var (
		addr       = flag.String("addr", "127.0.0.1:8787", "listen address")
		nodeBin    = flag.String("node", "node", "node binary used to run the shim")
		claudeBin  = flag.String("claude-bin", "", "path to the claude CLI the SDK drives (empty = SDK-bundled cli.js)")
		shimScript = flag.String("shim", "", "path to the shim entrypoint (shim/dist/main.js)")
		fake       = flag.Bool("fake", false, "force --fake (offline scripted SDK) on every session")
		retention  = flag.Int("retention", 4096, "per-session frame retention window for replay")
		webappDir  = flag.String("webapp", "", "optional directory of webapp static files to serve at /")
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
		proc, err := shim.Spawn(shim.Options{Argv: argv, Dir: opts.CWD})
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

	srv := server.New(server.Config{
		DaemonVersion: daemonVersion,
		Retention:     *retention,
		Spawn:         spawn,
	})

	mux := http.NewServeMux()
	mux.Handle("/sessions", srv.Handler())
	mux.Handle("/sessions/", srv.Handler())
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
		if err := httpServer.Close(); err != nil {
			log.Printf("claude-repld: http close: %v", err)
		}
	}()

	log.Printf("claude-repld %s listening on %s (shim: %s)", daemonVersion, *addr, *shimScript)
	if err := httpServer.ListenAndServe(); err != nil && err != http.ErrServerClosed {
		log.Fatalf("claude-repld: %v", err)
	}
}
