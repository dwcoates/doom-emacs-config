// Command shim-store is the agent-shim event store (design §6): a singleton,
// launchd-managed UDS service that owns the SQLite event database, assigns each
// session's gapless seq, dedups stream-plane/file-plane overlap, and serves
// replay-then-live-tail subscriptions.
//
// Flags (all paths default under the agent-repl cache dir but are always
// injectable, which is what lets tests point every path at a temp dir):
//
//	-socket  UDS path the store listens on          (…/sock/store.sock)
//	-db      SQLite database path                    (…/store/events.db)
//	-log     append-only log file (also to stderr)   (…/log/shim-store.log)
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
	"syscall"
	"time"

	"agentrepl/shim-store/internal/db"
	"agentrepl/shim-store/internal/healthcheck"
	"agentrepl/shim-store/internal/logging"
	"agentrepl/shim-store/internal/server"

	sharedlogging "agentrepl/logging"
)

func main() {
	base := defaultCacheDir()
	socketPath := flag.String("socket", filepath.Join(base, "sock", "store.sock"), "UDS path to listen on")
	dbPath := flag.String("db", filepath.Join(base, "store", "events.db"), "SQLite database path")
	logPath := flag.String("log", filepath.Join(base, "log", "shim-store.log"), "log file path (also mirrored to stderr)")
	healthCheck := flag.Bool("health-check", false, "send one correlated HealthCheck and emit its JSON result")
	healthRequestID := flag.String("health-request-id", "", "required correlation ID for -health-check")
	healthTimeout := flag.Duration("health-timeout", 0, "required deadline for -health-check")
	flag.Parse()
	if *healthCheck {
		os.Exit(runHealthCheck(*socketPath, *logPath, *healthRequestID, *healthTimeout, os.Stdout, os.Stderr))
	}

	if err := run(*socketPath, *dbPath, *logPath); err != nil {
		reportFatal(err, os.Stderr)
		os.Exit(1)
	}
}

// runHealthCheck owns the CLI's one-shot health mode.  Its stdout is exactly
// one Result JSON object; store diagnostics continue through the canonical log
// and stderr sink rather than contaminating the machine-readable response.
func runHealthCheck(socketPath, logPath, requestID string, timeout time.Duration, stdout, stderr io.Writer) int {
	log, closeLog, err := openHealthLogger(socketPath, logPath, stderr)
	if err != nil {
		reportFatal(err, stderr)
		writeHealthResult(stdout, healthcheck.Result{
			RequestID:    requestID,
			LatencyMS:    0,
			Component:    "",
			Healthy:      false,
			FailureClass: healthcheck.FailureClientFailure,
			Reason:       fmt.Sprintf("health logger bootstrap failed: %v", err),
		})
		return healthcheck.ExitClientFailure
	}
	defer closeLog()

	result, exitCode := healthcheck.Probe(healthcheck.Config{
		SocketPath: socketPath,
		RequestID:  requestID,
		Timeout:    timeout,
	}, log)
	if err := writeHealthResult(stdout, result); err != nil {
		log.Log(logging.Fields{Component: "store", Socket: socketPath, RequestID: requestID, Operation: "health-check-output", Level: "error"}, "health JSON output failed: %v", err)
		return healthcheck.ExitWriteFailure
	}
	return exitCode
}

func writeHealthResult(stdout io.Writer, result healthcheck.Result) error {
	return json.NewEncoder(stdout).Encode(result)
}

// openHealthLogger creates the same durable store diagnostic sink used by the
// service without touching the event database.  A one-shot probe is a store
// operation, so failures must be traceable in the canonical store log.
func openHealthLogger(socketPath, logPath string, stderr io.Writer) (*logging.Logger, func(), error) {
	if err := os.MkdirAll(filepath.Dir(logPath), 0o755); err != nil {
		return nil, nil, bootstrapError{fmt.Errorf("creating dir for %q: %w", logPath, err)}
	}
	lf, err := os.OpenFile(logPath, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o644)
	if err != nil {
		return nil, nil, bootstrapError{fmt.Errorf("opening log %q: %w", logPath, err)}
	}
	log := logging.New(lf, stderr, os.Getenv("AGENT_REPL_LOG_VERBOSE") != "")
	log = log.With(logging.Fields{Component: "store", Socket: socketPath})
	return log, func() { _ = lf.Close() }, nil
}

// reportFatal writes only bootstrap failures because all post-bootstrap errors
// have already reached the canonical logger and its stderr sink.
func reportFatal(err error, stderr io.Writer) {
	if isBootstrapError(err) {
		payload, encodeErr := json.Marshal(map[string]any{
			"timestamp": sharedlogging.Timestamp(time.Now()),
			"runtime":   "store", "pid": os.Getpid(), "level": "error", "verbosity": "normal",
			"operation": "store.bootstrap", "message": "shim-store bootstrap failed",
			"context": map[string]any{"error": err.Error()},
		})
		if encodeErr != nil {
			panic(fmt.Sprintf("shim-store bootstrap log encode failed: %v", encodeErr))
		}
		if _, writeErr := stderr.Write(append(payload, '\n')); writeErr != nil {
			panic(fmt.Sprintf("shim-store bootstrap log write failed: %v", writeErr))
		}
	}
}

// run wires up logging, the database, and the server, then blocks until a
// termination signal or a fatal serve error. Factored out of main so its wiring
// is exercised with temp paths in tests.
func run(socketPath, dbPath, logPath string) (err error) {
	log, closeLog, err := openLogger(socketPath, dbPath, logPath)
	if err != nil {
		return err
	}
	defer closeLog()
	defer logProcessExit(log, &err)
	return runWithLogger(socketPath, dbPath, log)
}

// logProcessExit is shim-store's one deferred exit trace: whatever caused
// runWithLogger to return — a clean signal-driven shutdown, a runtime
// failure, or a panic — is the last record this process writes, so a
// truncated log still names why the process is gone.
//
// It re-panics after logging rather than recovering: a panic here is an
// invariant violation (e.g. server.New's nil-dependency guard), and this
// trace exists to narrate the crash, not to turn it into a normal exit.
func logProcessExit(log *logging.Logger, err *error) {
	if r := recover(); r != nil {
		log.Log(logging.Fields{Operation: "exit", Level: "error"}, "shim-store exiting: panic: %v", r)
		panic(r)
	}
	if *err != nil {
		log.Log(logging.Fields{Operation: "exit", Level: "error"}, "shim-store exiting: %v", *err)
		return
	}
	log.Log(logging.Fields{Operation: "exit"}, "shim-store exiting cleanly")
}

// openLogger creates shim-store's only persistent diagnostic sink. Directory
// and file-open failures occur before that sink exists and are bootstrap-only.
func openLogger(socketPath, dbPath, logPath string) (*logging.Logger, func(), error) {
	for _, p := range []string{socketPath, dbPath, logPath} {
		if err := os.MkdirAll(filepath.Dir(p), 0o755); err != nil {
			return nil, nil, bootstrapError{fmt.Errorf("creating dir for %q: %w", p, err)}
		}
	}

	lf, err := os.OpenFile(logPath, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o644)
	if err != nil {
		return nil, nil, bootstrapError{fmt.Errorf("opening log %q: %w", logPath, err)}
	}
	log := logging.New(lf, os.Stderr, os.Getenv("AGENT_REPL_LOG_VERBOSE") != "")
	log = log.With(logging.Fields{Component: "store", DatabasePath: dbPath, Socket: socketPath})
	return log, func() { _ = lf.Close() }, nil
}

// runWithLogger owns errors that reach the process orchestration after logging
// is available. db.Open and server.Listen retain their lower-layer ownership.
func runWithLogger(socketPath, dbPath string, log *logging.Logger) error {
	database, err := db.Open(dbPath, log.With(logging.Fields{Component: "db", Table: "event"}))
	if err != nil {
		return err
	}
	defer database.Close()

	ln, err := server.Listen(socketPath, log.With(logging.Fields{Component: "server"}))
	if err != nil {
		return err
	}
	srv := server.New(database, log.With(logging.Fields{Component: "server"}), 0)

	sigc := make(chan os.Signal, 1)
	signal.Notify(sigc, syscall.SIGINT, syscall.SIGTERM)

	errc := make(chan error, 1)
	go func() { errc <- srv.Serve(ln) }()
	log.Log(logging.Fields{Operation: "serve"}, "listening")

	select {
	case sig := <-sigc:
		log.Log(logging.Fields{Operation: "shutdown"}, "received signal=%s", sig)
		return runLogged(log, "shutdown", srv.Close)
	case err := <-errc:
		return runLogged(log, "serve", func() error { return err })
	}
}

func runLogged(log *logging.Logger, operation string, execute func() error) error {
	if err := execute(); err != nil {
		log.Log(logging.Fields{Operation: operation}, "runtime operation failed: %v", err)
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

// defaultCacheDir resolves the agent-repl cache base (~/.cache/agent-repl,
// honoring XDG_CACHE_HOME).
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
