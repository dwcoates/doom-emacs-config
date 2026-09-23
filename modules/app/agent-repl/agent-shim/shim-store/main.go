// Command shim-store is the agent-repl record store: a singleton,
// launchd-managed service that owns the SQLite database and serves
// store.v1.ShimStore over a unix domain socket with Connect.
//
// Flags (every path is injectable, which is what lets a test point the whole
// stack at a temp dir):
//
//	--socket        UDS path to serve on   (env AGENT_REPL_STORE_SOCKET, else …/sock/store.sock)
//	--db            SQLite database path                          (…/store/events.db)
//	--log           size-capped rotating log file (…/log/shim-store.log)
//	--pprof         OPT-IN local-only profiling surface           (env AGENT_REPL_STORE_PPROF_ADDR)
//	--watch-buffer  per-subscriber watch frame buffer             (8192)
//
// THERE IS NO HEALTH MODE. The old -health-check/-health-request-id/
// -health-timeout trio died with the dial protocol: the store has no health
// verb by design, and agent-shim-doctor probes the Connect endpoints directly.
package main

import (
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/signal"
	"path/filepath"
	"syscall"
	"time"

	"agentrepl/shim-store/internal/db"
	"agentrepl/shim-store/internal/logging"
	"agentrepl/shim-store/internal/pprofsurface"
	"agentrepl/shim-store/internal/server"

	sharedlogging "agentrepl/logging"
)

// shutdownGrace bounds the orderly drain: standing watches are ended first, so
// what remains is in-flight unary work.
const shutdownGrace = 5 * time.Second

// pprofBootFailureGrace bounds how long a FAILED boot holds an ENABLED
// profiling surface open.
//
// A surface that dies with the failure it was opened to explain never served
// its purpose: the boot order puts it ahead of the database precisely so a boot
// that cannot get past the database is diagnosable, and a store that exits in
// the same millisecond is not. So a failed boot keeps the surface serving until
// it has answered a request, or until this grace expires — never longer, and
// never at all unless an operator asked for the surface by name.
const pprofBootFailureGrace = 5 * time.Second

func main() {
	base := defaultCacheDir()
	socketPath := flag.String("socket", socketDefault(base), "UDS path to serve store.v1.ShimStore on; defaults to $"+server.EnvSocket+" when set")
	dbPath := flag.String("db", filepath.Join(base, "store", "events.db"), "SQLite database path")
	logPath := flag.String("log", filepath.Join(base, "log", "shim-store.log"), "log file path (size-capped, rotated into N generations)")
	pprofAddr := flag.String("pprof", envStr(pprofsurface.EnvAddr, ""), "OPT-IN Go profiling surface: a unix socket path, or an explicitly loopback host:port (127.0.0.1:6061). Empty = OFF, which is the default; there is no always-on listener. The resolved surface is named in the store.pprof.enabled record at startup")
	watchBuffer := flag.Int("watch-buffer", server.DefaultWatchBuffer, "per-subscriber WatchAgentSession frame buffer; a subscriber that overflows it is ended and must re-open with known_through")
	flag.Parse()

	if err := run(*socketPath, *dbPath, *logPath, *pprofAddr, *watchBuffer); err != nil {
		reportFatal(err, os.Stderr)
		os.Exit(1)
	}
}

// reportFatal writes only bootstrap failures because all post-bootstrap errors
// have already reached the canonical logger and its stderr sink.
func reportFatal(err error, stderr io.Writer) {
	if !isBootstrapError(err) {
		return
	}
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

// run wires up logging, the profiling surface, the database and the server,
// then blocks until a termination signal or a fatal serve error. Factored out
// of main so its wiring is exercised with temp paths in tests.
func run(socketPath, dbPath, logPath, pprofAddr string, watchBuffer int) (err error) {
	log, closeLog, err := openLogger(socketPath, dbPath, logPath)
	if err != nil {
		return err
	}
	defer closeLog()
	defer logProcessExit(log, &err)
	reportBuild(log, defaultBuildReportDeps())
	return runWithLogger(socketPath, dbPath, pprofAddr, watchBuffer, log)
}

// runWithLogger owns errors that reach the process orchestration after logging
// is available. db.Open and server.Listen retain their lower-layer ownership.
func runWithLogger(socketPath, dbPath, pprofAddr string, watchBuffer int, log *logging.Logger) (err error) {
	// OPENED BEFORE THE DATABASE, so a store wedged recreating its schema or
	// on a cold-cache first read of a large database is still profilable.
	pprofSurface, err := openPprofSurface(pprofAddr, log)
	if err != nil {
		return err
	}
	defer func() {
		if closeErr := pprofSurface.Close(); closeErr != nil {
			log.Log(logging.Fields{Operation: "store.pprof.close", Level: "error"}, "closing pprof surface failed: %v", closeErr)
		}
	}()
	// Registered AFTER the close above, so it runs BEFORE it: the surface is
	// still listening while a failed boot is held open to be profiled.
	defer func() {
		if err != nil {
			holdPprofForDiagnosis(pprofSurface, log)
		}
	}()

	// THE SIGNAL HANDLER IS INSTALLED BEFORE ANYTHING IS BINDABLE OR OPENABLE.
	//
	// Installed after Listen, there was a window in which the socket already
	// ACCEPTED — the kernel queues connections from the listen(2) call onward,
	// so a supervisor or a test that waits for the socket sees a ready store —
	// while SIGTERM still had its default disposition and killed the process
	// outright. The listener never closed, so the socket file survived, and the
	// successor met a corpse it had to reclaim. Notify is cheap and the channel
	// is buffered, so a signal arriving during db.Open or Listen simply waits
	// there and is answered by the select below the moment serving starts.
	sigc := make(chan os.Signal, 1)
	signal.Notify(sigc, syscall.SIGINT, syscall.SIGTERM)
	defer signal.Stop(sigc)

	database, err := db.Open(dbPath, log.With(logging.Fields{Component: "db"}))
	if err != nil {
		return err
	}
	defer joinClose(&err, database.Close)

	// THE LEDGER SWEEP RUNS FOR AS LONG AS THE STORE SERVES, AND STOPS BEFORE
	// THE DATABASE CLOSES. Its defer is registered AFTER database.Close's, so
	// it runs first: a sweep batch still in flight against a closed handle
	// would be a storage error on a perfectly orderly shutdown. It writes
	// through the same serialized write slot every producer's batch does, one
	// bounded batch at a time, so it can delay a producer by one batch and
	// never by a whole sweep.
	sweepCtx, stopSweep := context.WithCancel(context.Background())
	sweepDone := make(chan struct{})
	go func() {
		defer close(sweepDone)
		database.SweepWriteLedger(sweepCtx, db.DefaultLedgerSweepInterval)
	}()
	defer func() {
		stopSweep()
		<-sweepDone
	}()

	ln, err := server.Listen(socketPath, log.With(logging.Fields{Component: "server"}))
	if err != nil {
		return err
	}
	srv := server.New(database, log.With(logging.Fields{Component: "server"}), watchBuffer)

	errc := make(chan error, 1)
	go func() { errc <- srv.Serve(ln) }()

	select {
	case sig := <-sigc:
		log.Log(logging.Fields{Operation: "store.shutdown"}, "received signal=%s", sig)
		ctx, cancel := context.WithTimeout(context.Background(), shutdownGrace)
		defer cancel()
		if shutdownErr := srv.Shutdown(ctx); shutdownErr != nil {
			return shutdownErr
		}
		// Serve returns http.ErrServerClosed once the drain completes; that is
		// the orderly outcome and is not an error.
		if serveErr := <-errc; serveErr != nil && !errors.Is(serveErr, http.ErrServerClosed) {
			return serveErr
		}
		return nil
	case serveErr := <-errc:
		if serveErr != nil && !errors.Is(serveErr, http.ErrServerClosed) {
			return serveErr
		}
		return nil
	}
}

// joinClose runs a deferred close and joins its failure onto the run's result,
// so a store whose database failed to close never reports a clean exit. The
// closer owns the failure's record (db.Close logs its own error with the pool
// it was closing); this only carries the error to logProcessExit and the exit
// status.
func joinClose(err *error, close func() error) {
	if closeErr := close(); closeErr != nil {
		*err = errors.Join(*err, closeErr)
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
// none is the failure this addition exists to end.
func openPprofSurface(addr string, log *logging.Logger) (*pprofsurface.Surface, error) {
	surface, err := pprofsurface.Open(addr)
	if err != nil {
		return nil, err
	}
	if surface == nil {
		log.LogVerbose(logging.Fields{Operation: "store.pprof.disabled", Level: "debug"},
			"Go profiling surface is off env=%s flag=--pprof", pprofsurface.EnvAddr)
		return nil, nil
	}
	log.Log(logging.Fields{Operation: "store.pprof.enabled", Level: "warn", Socket: surface.Address()},
		"Go profiling surface is LISTENING; it exposes goroutine stacks, the command line and heap contents network=%s address=%s url=%s env=%s",
		surface.Network(), surface.Address(), surface.URL(), pprofsurface.EnvAddr)
	go func() {
		if serveErr := surface.Serve(); serveErr != nil && !errors.Is(serveErr, http.ErrServerClosed) {
			log.Log(logging.Fields{Operation: "store.pprof.serve", Level: "error"}, "pprof surface serve ended: %v", serveErr)
		}
	}()
	return surface, nil
}

// holdPprofForDiagnosis keeps an enabled profiling surface serving after the
// boot failed, until it has answered a request or the grace expires.
//
// IT IS A NO-OP WHEN THE SURFACE IS OFF, which is the shipped state: an
// ordinary failed boot still exits at once. Only an operator who asked for
// profiles by name pays this wait, and it is what makes the surface's
// before-the-database boot order mean anything.
func holdPprofForDiagnosis(surface *pprofsurface.Surface, log *logging.Logger) {
	if surface == nil {
		log.LogVerbose(logging.Fields{Operation: "store.pprof.hold", Level: "debug"},
			"the boot failed with no profiling surface to hold open")
		return
	}
	log.Log(logging.Fields{Operation: "store.pprof.hold", Level: "warn", Socket: surface.Address()},
		"the boot failed; holding the profiling surface open so it can be profiled url=%s grace_ms=%d", surface.URL(), pprofBootFailureGrace.Milliseconds())
	select {
	case <-surface.Served():
		log.Log(logging.Fields{Operation: "store.pprof.hold", Level: "warn", Socket: surface.Address()},
			"the failed boot was profiled; exiting")
	case <-time.After(pprofBootFailureGrace):
		log.Log(logging.Fields{Operation: "store.pprof.hold", Level: "warn", Socket: surface.Address()},
			"nobody profiled the failed boot within the grace; exiting grace_ms=%d", pprofBootFailureGrace.Milliseconds())
	}
}

// socketDefault is the --socket flag's default: $AGENT_REPL_STORE_SOCKET when
// it is set, else the cache-dir path.
//
// AN EXPLICIT FLAG STILL BEATS THE ENVIRONMENT, because this only ever supplies
// flag.String's default value. The variable exists so a test harness can point
// every participant at a private store without editing a command line.
func socketDefault(base string) string {
	return envStr(server.EnvSocket, filepath.Join(base, "sock", "store.sock"))
}

// envStr returns the environment variable's value, or def when it is unset or
// empty. It is what makes $AGENT_REPL_STORE_SOCKET the DEFAULT of --socket: an
// explicit flag still beats it, because flag.String only uses this as its
// default value.
func envStr(name, def string) string {
	if v := os.Getenv(name); v != "" {
		return v
	}
	return def
}

// logProcessExit is shim-store's one deferred exit trace: whatever caused
// runWithLogger to return — a clean signal-driven shutdown, a runtime failure,
// or a panic — is the last record this process writes, so a truncated log still
// names why the process is gone.
//
// It re-panics after logging rather than recovering: a panic here is an
// invariant violation (server.New's nil-dependency guard, for instance), and
// this trace exists to narrate the crash, not to turn it into a normal exit.
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
//
// IT CREATES ONLY ITS OWN DIRECTORY. Pre-creating the database's and the
// socket's directories here would move their failures ahead of the profiling
// surface, which is the one thing the boot order exists to prevent: an
// unopenable --db has to fail at db.Open, with pprof already serving. Each of
// those paths is created by the layer that owns it (db.OpenWithOptions,
// server.Listen).
func openLogger(socketPath, dbPath, logPath string) (*logging.Logger, func(), error) {
	level, err := sharedlogging.ParseLevel(os.Getenv("AGENT_REPL_LOG_LEVEL"))
	if err != nil {
		return nil, nil, bootstrapError{err}
	}
	if err := os.MkdirAll(filepath.Dir(logPath), 0o755); err != nil {
		return nil, nil, bootstrapError{fmt.Errorf("creating dir for %q: %w", logPath, err)}
	}
	lf, err := sharedlogging.OpenRotating(logPath, sharedlogging.DefaultCapBytes, sharedlogging.DefaultBackups)
	if err != nil {
		return nil, nil, bootstrapError{fmt.Errorf("opening log %q: %w", logPath, err)}
	}
	log := logging.NewDurableOnlyAtLevel(lf, os.Stderr, level)
	log = log.With(logging.Fields{Component: "store", DatabasePath: dbPath, Socket: socketPath})
	return log, func() { _ = lf.Close() }, nil
}

// bootstrapError marks the only failures that may be reported before the
// canonical logger exists.
type bootstrapError struct{ err error }

func (e bootstrapError) Error() string { return e.err.Error() }

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
