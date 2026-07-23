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
	"flag"
	"fmt"
	"io"
	"os"
	"os/signal"
	"path/filepath"
	"sync"
	"syscall"
	"time"

	"agentrepl/shim-store/internal/db"
	"agentrepl/shim-store/internal/server"
)

func main() {
	base := defaultCacheDir()
	socketPath := flag.String("socket", filepath.Join(base, "sock", "store.sock"), "UDS path to listen on")
	dbPath := flag.String("db", filepath.Join(base, "store", "events.db"), "SQLite database path")
	logPath := flag.String("log", filepath.Join(base, "log", "shim-store.log"), "log file path (also mirrored to stderr)")
	flag.Parse()

	if err := run(*socketPath, *dbPath, *logPath); err != nil {
		fmt.Fprintln(os.Stderr, "shim-store:", err)
		os.Exit(1)
	}
}

// run wires up logging, the database, and the server, then blocks until a
// termination signal or a fatal serve error. Factored out of main so its wiring
// is exercised with temp paths in tests.
func run(socketPath, dbPath, logPath string) error {
	for _, p := range []string{socketPath, dbPath, logPath} {
		if err := os.MkdirAll(filepath.Dir(p), 0o755); err != nil {
			return fmt.Errorf("creating dir for %q: %w", p, err)
		}
	}

	lf, err := os.OpenFile(logPath, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o644)
	if err != nil {
		return fmt.Errorf("opening log %q: %w", logPath, err)
	}
	defer lf.Close()
	logf := newLogger(io.MultiWriter(os.Stderr, lf))

	database, err := db.Open(dbPath, logf)
	if err != nil {
		return err
	}
	defer database.Close()

	ln, err := server.Listen(socketPath)
	if err != nil {
		return err
	}
	srv := server.New(database, logf, 0)

	sigc := make(chan os.Signal, 1)
	signal.Notify(sigc, syscall.SIGINT, syscall.SIGTERM)

	errc := make(chan error, 1)
	go func() { errc <- srv.Serve(ln) }()
	logf("listening: socket=%s db=%s", socketPath, dbPath)

	select {
	case sig := <-sigc:
		logf("received %s; shutting down", sig)
		return srv.Close()
	case err := <-errc:
		return err
	}
}

// newLogger returns a §12 loud-logging sink: one line per call, prefixed with
// a millisecond wall-clock time and the component tag.
func newLogger(w io.Writer) server.Logf {
	var mu sync.Mutex
	return func(format string, args ...any) {
		ts := time.Now().Format("15:04:05.000")
		msg := fmt.Sprintf(format, args...)
		mu.Lock()
		fmt.Fprintf(w, "%s [shim-store] %s\n", ts, msg)
		mu.Unlock()
	}
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
