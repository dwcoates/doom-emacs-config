// Command shim-claude-sidecar is the agent-shim FILE-PLANE READER: a singleton,
// launchd-managed process that discovers what the vendor's agent binary writes
// to disk (session transcripts, subagent transcripts, workflow journals and
// their per-agent transcripts, task spools), tails them with cursored,
// truncation-aware reads, converts each record into conversation.v1 vocabulary,
// and writes it to the store as store.v1 StoreEntry batches with the reader
// position riding the same transaction.
//
// It is a COPIER. It has no view of liveness, no session semantics and no
// contact with the daemon; the only thing it ever concludes on its own is that
// it STOPPED SEEING a detached run (see internal/stale).
//
// Flags (the launchd plists reference these):
//
//	--store-socket     store UDS path (default $AGENT_REPL_STORE_SOCKET, else …/sock/store.sock)
//	--config-roots     comma-separated config roots (~/.claude,~/.claude-chesscom)
//	--spool-root       task-spool root (/tmp; resolves claude-<uid>/… itself)
//	--log              append-only log file (also to stderr)
//	--poll-interval    how often each watched file is polled (1s)
//	--rescan-interval  how often discovery runs (30s)
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
	"strings"
	"syscall"
	"time"

	"agentrepl/shim-claude-sidecar/internal/logging"

	sharedlogging "agentrepl/logging"
)

// Defaults for the two loop intervals. Polling is frequent because it is what
// carries a user's prompt echo to the GUI; rescanning is not, because a new
// file appearing is rare and fsnotify is the latency path.
const (
	DefaultPollInterval   = time.Second
	DefaultRescanInterval = 30 * time.Second
)

// StoreSocketEnv names the store's socket for both the store and the sidecar.
// An explicit --store-socket beats it; it is what lets a test store live
// somewhere private without either side hard-coding the path.
const StoreSocketEnv = "AGENT_REPL_STORE_SOCKET"

func main() {
	base := defaultCacheDir()
	storeSocket := flag.String("store-socket", defaultStoreSocket(base), "store UDS path (default $"+StoreSocketEnv+")")
	configRoots := flag.String("config-roots", "~/.claude,~/.claude-chesscom", "comma-separated config roots")
	spoolRoot := flag.String("spool-root", "/tmp", "task-spool root (resolves claude-<uid>/ itself)")
	logPath := flag.String("log", filepath.Join(base, "log", "shim-claude-sidecar.log"), "log file path (also to stderr)")
	pollInterval := flag.Duration("poll-interval", DefaultPollInterval, "how often each watched file is polled")
	rescanInterval := flag.Duration("rescan-interval", DefaultRescanInterval, "how often discovery runs")
	flag.Parse()

	options := Options{
		StoreSocket:    *storeSocket,
		ConfigRoots:    parseRoots(*configRoots),
		SpoolRoot:      *spoolRoot,
		PollInterval:   *pollInterval,
		RescanInterval: *rescanInterval,
	}
	if err := run(options, *logPath); err != nil {
		reportFatal(err, os.Stderr)
		os.Exit(1)
	}
}

// Options is the sidecar's whole configuration, after flag and env resolution.
type Options struct {
	StoreSocket    string
	ConfigRoots    []string
	SpoolRoot      string
	PollInterval   time.Duration
	RescanInterval time.Duration
}

// defaultStoreSocket resolves the store socket's default: the shared env var
// when it is set, otherwise the cache-dir path both services agree on.
func defaultStoreSocket(base string) string {
	if socket := os.Getenv(StoreSocketEnv); socket != "" {
		return socket
	}
	return filepath.Join(base, "sock", "store.sock")
}

// reportFatal writes only bootstrap failures, because every post-bootstrap
// error has already reached the canonical logger and its stderr sink.
func reportFatal(err error, stderr io.Writer) {
	if !isBootstrapError(err) {
		return
	}
	payload, encodeErr := json.Marshal(map[string]any{
		"timestamp": sharedlogging.Timestamp(time.Now()),
		"runtime":   "sidecar", "pid": os.Getpid(), "level": "error", "verbosity": "normal",
		"operation": "sidecar.bootstrap", "message": "sidecar bootstrap failed",
		"context": map[string]any{"error": err.Error()},
	})
	if encodeErr != nil {
		panic(fmt.Sprintf("shim-claude-sidecar bootstrap log encode failed: %v", encodeErr))
	}
	if _, writeErr := stderr.Write(append(payload, '\n')); writeErr != nil {
		panic(fmt.Sprintf("shim-claude-sidecar bootstrap log write failed: %v", writeErr))
	}
}

func run(options Options, logPath string) (err error) {
	logf, closeLog, err := openLogger(options.StoreSocket, logPath)
	if err != nil {
		return err
	}
	defer closeLog()
	defer logProcessExit(logf, &err)

	signals := make(chan os.Signal, 1)
	signal.Notify(signals, syscall.SIGINT, syscall.SIGTERM)
	return runWithLogger(options, logf, signals)
}

// logProcessExit is the sidecar's one deferred exit trace: whatever caused the
// run to return — a clean signal-driven shutdown, a runtime failure, or a panic
// — is the last record this process writes, so a truncated log still names why
// the process is gone.
//
// It re-panics after logging rather than recovering: a panic here is an
// invariant violation, and this trace narrates the crash rather than turning it
// into a normal exit.
func logProcessExit(logf *logging.Bound, err *error) {
	if r := recover(); r != nil {
		logf.With(logging.Context{Operation: "exit", Level: "error"}).Log("sidecar exiting: panic: %v", r)
		panic(r)
	}
	if *err != nil {
		logf.With(logging.Context{Operation: "exit", Level: "error"}).Log("sidecar exiting: %v", *err)
		return
	}
	logf.With(logging.Context{Operation: "exit"}).Log("sidecar exiting cleanly")
}

// openLogger creates the sidecar's only persistent diagnostic sink. Failures
// here are bootstrap failures, because no canonical logger can exist yet.
func openLogger(storeSocket, logPath string) (*logging.Bound, func(), error) {
	if err := os.MkdirAll(filepath.Dir(logPath), 0o755); err != nil {
		return nil, nil, bootstrapError{fmt.Errorf("creating log dir: %w", err)}
	}
	file, err := os.OpenFile(logPath, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o644)
	if err != nil {
		return nil, nil, bootstrapError{fmt.Errorf("opening log %q: %w", logPath, err)}
	}
	logf := logging.New(os.Stderr, file).With(logging.Context{Component: "sidecar", StoreSocket: storeSocket})
	return logf, func() { _ = file.Close() }, nil
}

// runWithLogger owns process-level failures once canonical logging exists.
// Lower layers keep ownership of the errors they log themselves.
func runWithLogger(options Options, logf *logging.Bound, stop <-chan os.Signal) error {
	sc := newSidecar(options, logf)
	logf.With(logging.Context{Operation: "start"}).Log(
		"sidecar starting config_roots=%v spool_root=%s poll_interval=%s rescan_interval=%s",
		options.ConfigRoots, options.SpoolRoot, options.PollInterval, options.RescanInterval)
	if err := sc.Run(stop); err != nil {
		logf.With(logging.Context{Operation: "run", Level: "error"}).Log("sidecar stopped with error: %v", err)
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

// parseRoots splits a comma-separated root list and expands a leading ~.
func parseRoots(csv string) []string {
	var out []string
	for _, root := range strings.Split(csv, ",") {
		root = strings.TrimSpace(root)
		if root == "" {
			continue
		}
		out = append(out, expandHome(root))
	}
	return out
}

func expandHome(path string) string {
	if path == "~" || strings.HasPrefix(path, "~/") {
		if home, err := os.UserHomeDir(); err == nil {
			return filepath.Join(home, strings.TrimPrefix(strings.TrimPrefix(path, "~"), "/"))
		}
	}
	return path
}

func defaultCacheDir() string {
	if dir := os.Getenv("XDG_CACHE_HOME"); dir != "" {
		return filepath.Join(dir, "agent-repl")
	}
	home, err := os.UserHomeDir()
	if err != nil {
		home = os.TempDir()
	}
	return filepath.Join(home, ".cache", "agent-repl")
}
