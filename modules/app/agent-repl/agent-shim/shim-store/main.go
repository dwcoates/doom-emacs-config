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
	"agentrepl/shim-store/internal/logging"
	"agentrepl/shim-store/internal/server"
)

func main() {
	base := defaultCacheDir()
	socketPath := flag.String("socket", filepath.Join(base, "sock", "store.sock"), "UDS path to listen on")
	dbPath := flag.String("db", filepath.Join(base, "store", "events.db"), "SQLite database path")
	logPath := flag.String("log", filepath.Join(base, "log", "shim-store.log"), "log file path (also mirrored to stderr)")
	flag.Parse()

	if err := run(*socketPath, *dbPath, *logPath); err != nil {
		reportFatal(err, os.Stderr)
		os.Exit(1)
	}
}

// reportFatal writes only bootstrap failures because all post-bootstrap errors
// have already reached the canonical logger and its stderr sink.
func reportFatal(err error, stderr io.Writer) {
	if isBootstrapError(err) {
		payload, encodeErr := json.Marshal(map[string]any{
			"timestamp": time.Now().Local().Format(logging.TimestampLayout),
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
func run(socketPath, dbPath, logPath string) error {
	log, closeLog, err := openLogger(socketPath, dbPath, logPath)
	if err != nil {
		return err
	}
	defer closeLog()
	return runWithLogger(socketPath, dbPath, log)
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
