// Command agent-repl-tests is the test service: a singleton process the
// platform's service manager keeps running, like the store and the sidecar,
// that owns every merge gate's run and serves testservice.v1.Tests to the
// agent-repl daemon over a unix domain socket with Connect. A daemon deploy
// never cuts a gate: the gate's run lives here, and the new daemon watches it
// by its id.
//
// Flags (every path is injectable, so a test can point it at a temp dir):
//
//	--socket  UDS path to serve on  (env AGENT_REPL_TESTS_SOCKET, else ~/.cache/agent-repl/sock/tests.sock)
//	--runs    run registry root     (~/.cache/agent-repl/tests/runs)
//	--log     size-capped rotating log file (~/.cache/agent-repl/log/tests.log)
package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"net/http"
	"os"
	"os/signal"
	"path/filepath"
	"syscall"
	"time"

	"agentrepl/logging/buildreport"
	"agentrepl/logging/recordlog"
	"agentrepl/logging/singletonsocket"
	"agentrepl/testservice/internal/runs"
	"agentrepl/testservice/internal/server"
)

// Runtime is the name every record of the test service carries.
const Runtime = "tests"

// shutdownGrace bounds the orderly drain of in-flight requests; a watch is
// one, and ends with the drain while its run goes on.
const shutdownGrace = 5 * time.Second

// probeTimeout bounds the dial that decides whether an existing socket has a
// live test service behind it.
const probeTimeout = 2 * time.Second

func main() {
	home, err := os.UserHomeDir()
	if err != nil {
		fmt.Fprintf(os.Stderr, "agent-repl-tests: resolving the home directory failed: %v\n", err)
		os.Exit(1)
	}
	cache := cacheDir(home)
	socketPath := flag.String("socket", envOr(server.EnvSocket, filepath.Join(cache, "sock", "tests.sock")), "UDS path to serve testservice.v1.Tests on")
	runsRoot := flag.String("runs", filepath.Join(cache, "tests", "runs"), "run registry root")
	logPath := flag.String("log", filepath.Join(cache, "log", "tests.log"), "log file path (size-capped, rotated)")
	flag.Parse()
	if err := run(*socketPath, *runsRoot, *logPath); err != nil {
		os.Exit(1)
	}
}

// run opens the log, reports the build, opens the registry and serves until
// a termination signal. Every failure after the log opens is logged before it
// returns; a failure to open the log itself goes to stderr.
func run(socketPath, runsRoot, logPath string) (err error) {
	log, closeLog, err := recordlog.Open(Runtime, logPath, os.Stderr, os.Getenv, time.Now)
	if err != nil {
		fmt.Fprintf(os.Stderr, "agent-repl-tests: opening the log failed: %v\n", err)
		return err
	}
	defer func() { _ = closeLog() }()
	log = log.With(recordlog.Context{"socket": socketPath, "runs": runsRoot})
	defer func() {
		if err != nil {
			log.Error("tests.exit", "the test service is exiting on a failure", recordlog.Context{"cause": err.Error()})
		} else {
			log.Info("tests.exit", "the test service is exiting cleanly", nil)
		}
	}()
	buildreport.Announce(buildreport.ServiceTests, "tests.buildreport", log, os.Getenv)

	sigc := make(chan os.Signal, 1)
	signal.Notify(sigc, syscall.SIGINT, syscall.SIGTERM)
	defer signal.Stop(sigc)

	reg, err := runs.Open(runsRoot, log, time.Now)
	if err != nil {
		return err
	}
	ln, err := singletonsocket.ListenRecorded(socketPath, probeTimeout, log)
	if err != nil {
		return err
	}
	srv := server.New(reg, log)
	errc := make(chan error, 1)
	go func() { errc <- srv.Serve(ln) }()
	select {
	case sig := <-sigc:
		log.Info("tests.shutdown", "received a termination signal", recordlog.Context{"signal": sig.String()})
		ctx, cancel := context.WithTimeout(context.Background(), shutdownGrace)
		defer cancel()
		if err := srv.Shutdown(ctx); err != nil {
			log.Error("tests.shutdown", "draining in-flight requests failed", recordlog.Context{"cause": err.Error()})
			return err
		}
		if serveErr := <-errc; serveErr != nil && !errors.Is(serveErr, http.ErrServerClosed) {
			return serveErr
		}
		return nil
	case serveErr := <-errc:
		if serveErr != nil && !errors.Is(serveErr, http.ErrServerClosed) {
			log.Error("tests.serve", "serving ended on a failure", recordlog.Context{"cause": serveErr.Error()})
			return serveErr
		}
		return nil
	}
}

func cacheDir(home string) string {
	if d := os.Getenv("XDG_CACHE_HOME"); d != "" {
		return filepath.Join(d, "agent-repl")
	}
	return filepath.Join(home, ".cache", "agent-repl")
}

func envOr(name, def string) string {
	if v := os.Getenv(name); v != "" {
		return v
	}
	return def
}
