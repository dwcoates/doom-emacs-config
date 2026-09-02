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
//
// The LOST policy's windows (internal/stale) are configurable too, so the
// integration suite can exercise a conclusion in milliseconds instead of
// waiting out a production window. Each takes Go duration syntax, each has an
// env var standing in for it, and an explicit flag beats the env:
//
//	--stale-grace             $AGENT_REPL_STALE_GRACE             (30s)
//	--stale-shell-silence     $AGENT_REPL_STALE_SHELL_SILENCE     (30m)
//	--stale-agent-silence     $AGENT_REPL_STALE_AGENT_SILENCE     (60m)
//	--stale-workflow-silence  $AGENT_REPL_STALE_WORKFLOW_SILENCE  (60m)
//	--unowned-spool-window    $AGENT_REPL_UNOWNED_SPOOL_WINDOW    (60s)
//
// UNSET KEEPS THE PACKAGE DEFAULT; A MALFORMED VALUE IS REFUSED. A window that
// does not parse, or that is negative, is a bootstrap error: the process states
// it once and exits non-zero rather than starting with a default the operator
// did not ask for, because a silently-defaulted window is a policy nobody chose.
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
	"agentrepl/shim-claude-sidecar/internal/stale"

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

// The env vars that stand in for the LOST policy's four window flags. An
// explicit flag beats the env, exactly as --store-socket does.
const (
	StaleGraceEnv           = "AGENT_REPL_STALE_GRACE"
	StaleShellSilenceEnv    = "AGENT_REPL_STALE_SHELL_SILENCE"
	StaleAgentSilenceEnv    = "AGENT_REPL_STALE_AGENT_SILENCE"
	StaleWorkflowSilenceEnv = "AGENT_REPL_STALE_WORKFLOW_SILENCE"

	// UnownedSpoolWindowEnv names how long an unclaimed spool is held before
	// its bytes are ingested as residue. It sits with the LOST windows because
	// it is the other wall-clock wait the file plane makes a caller sit through.
	UnownedSpoolWindowEnv = "AGENT_REPL_UNOWNED_SPOOL_WINDOW"
)

func main() {
	base := defaultCacheDir()
	storeSocket := flag.String("store-socket", defaultStoreSocket(base), "store UDS path (default $"+StoreSocketEnv+")")
	configRoots := flag.String("config-roots", "~/.claude,~/.claude-chesscom", "comma-separated config roots")
	spoolRoot := flag.String("spool-root", "/tmp", "task-spool root (resolves claude-<uid>/ itself)")
	logPath := flag.String("log", filepath.Join(base, "log", "shim-claude-sidecar.log"), "log file path (also to stderr)")
	pollInterval := flag.Duration("poll-interval", DefaultPollInterval, "how often each watched file is polled")
	rescanInterval := flag.Duration("rescan-interval", DefaultRescanInterval, "how often discovery runs")
	// The LOST windows are STRINGS rather than flag.Duration values because
	// flag.Duration cannot tell "the operator passed nothing" from "the operator
	// passed 0s", and it answers a malformed value by printing usage and exiting
	// 2 — neither of which can be told apart from a default this process chose.
	staleGrace := flag.String("stale-grace", "",
		"LOST grace window after a run's file vanishes (Go duration; default $"+StaleGraceEnv+", else 30s)")
	staleShellSilence := flag.String("stale-shell-silence", "",
		"how long a shell spool may stop growing before it is LOST (Go duration; default $"+StaleShellSilenceEnv+", else 30m)")
	staleAgentSilence := flag.String("stale-agent-silence", "",
		"how long an agent transcript spool may stop growing before it is LOST (Go duration; default $"+StaleAgentSilenceEnv+", else 60m)")
	staleWorkflowSilence := flag.String("stale-workflow-silence", "",
		"how long a workflow journal may stop growing before it is LOST (Go duration; default $"+StaleWorkflowSilenceEnv+", else 60m)")
	unownedSpoolWindow := flag.String("unowned-spool-window", "",
		"how long an unclaimed spool is held before its bytes are ingested as residue (Go duration; default $"+UnownedSpoolWindowEnv+", else 60s)")
	flag.Parse()

	unowned, err := durationSource{flagName: "unowned-spool-window", envName: UnownedSpoolWindowEnv, raw: *unownedSpoolWindow}.resolve()
	if err != nil {
		reportFatal(err, os.Stderr)
		os.Exit(1)
	}

	staleOptions, err := resolveStaleOptions(
		durationSource{flagName: "stale-grace", envName: StaleGraceEnv, raw: *staleGrace},
		durationSource{flagName: "stale-shell-silence", envName: StaleShellSilenceEnv, raw: *staleShellSilence},
		durationSource{flagName: "stale-agent-silence", envName: StaleAgentSilenceEnv, raw: *staleAgentSilence},
		durationSource{flagName: "stale-workflow-silence", envName: StaleWorkflowSilenceEnv, raw: *staleWorkflowSilence},
	)
	if err != nil {
		reportFatal(err, os.Stderr)
		os.Exit(1)
	}

	options := Options{
		StoreSocket:        *storeSocket,
		ConfigRoots:        parseRoots(*configRoots),
		SpoolRoot:          *spoolRoot,
		PollInterval:       *pollInterval,
		RescanInterval:     *rescanInterval,
		Stale:              staleOptions,
		UnownedSpoolWindow: unowned,
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
	// Stale carries the LOST policy's windows. A zero field keeps
	// internal/stale's own default, which is the only meaning "unset" has here.
	Stale stale.Options
	// UnownedSpoolWindow is how long an unclaimed spool is held before its bytes
	// are ingested as residue. Zero keeps held.go's UnownedSpoolWindow.
	UnownedSpoolWindow time.Duration
}

// durationSource is one duration option's two spellings: the flag value the
// operator passed (empty when they passed none) and the env var that stands in
// for it.
type durationSource struct {
	flagName string
	envName  string
	raw      string
}

// resolve answers the option's effective value: the flag when it was passed,
// else the env, else zero — which is how the caller says "keep the package
// default". A value that is present but unusable is REFUSED rather than
// defaulted, because a window nobody chose is a policy nobody chose.
//
// It cannot log: it runs before the canonical logger exists, so the bootstrap
// error it returns IS its record (reportFatal writes it exactly once).
func (d durationSource) resolve() (time.Duration, error) {
	value, origin := strings.TrimSpace(d.raw), "--"+d.flagName
	if value == "" {
		value, origin = strings.TrimSpace(os.Getenv(d.envName)), d.envName
	}
	if value == "" {
		return 0, nil
	}
	parsed, err := time.ParseDuration(value)
	if err != nil {
		return 0, bootstrapError{fmt.Errorf("%s: %q is not a Go duration: %w", origin, value, err)}
	}
	if parsed < 0 {
		return 0, bootstrapError{fmt.Errorf("%s: %q is negative, and a negative window concludes every run LOST at once", origin, value)}
	}
	return parsed, nil
}

// resolveStaleOptions resolves the four LOST windows in flag order. The FIRST
// unusable value stops bootstrap: starting with three of four windows the
// operator asked for is worse than not starting.
func resolveStaleOptions(grace, shellSilence, agentSilence, workflowSilence durationSource) (stale.Options, error) {
	var out stale.Options
	for _, field := range []struct {
		source durationSource
		into   *time.Duration
	}{
		{grace, &out.Grace},
		{shellSilence, &out.ShellSilence},
		{agentSilence, &out.AgentSilence},
		{workflowSilence, &out.WorkflowSilence},
	} {
		resolved, err := field.source.resolve()
		if err != nil {
			return stale.Options{}, err
		}
		*field.into = resolved
	}
	return out, nil
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
		"sidecar starting config_roots=%v spool_root=%s poll_interval=%s rescan_interval=%s lost_windows=%+v",
		options.ConfigRoots, options.SpoolRoot, options.PollInterval, options.RescanInterval, sc.tracker.Windows())
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
