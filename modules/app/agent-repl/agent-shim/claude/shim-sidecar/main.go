// Command shim-claude-sidecar is the agent-shim FILE-PLANE READER: a singleton,
// launchd-managed process that discovers what the vendor's agent binary writes
// to disk (session transcripts, subagent transcripts, workflow journals and
// their per-agent transcripts, task spools), tails them with cursored,
// truncation-aware reads, converts each record into conversation.v1 vocabulary,
// and writes it to the store as store.v1 StoreEntry batches with the reader
// position riding the same transaction. File-scoped diagnostics are forwarded
// to the daemon's ClientLog RPC for persistence in workspace sidecar.log.
//
// It is a COPIER. It has no view of liveness and no session semantics. Its
// daemon interactions are the read-only workspace-roster lookup needed to
// obtain a daemon-minted ref and the ClientLog forwarding that uses it; the
// only thing it concludes on its own is that it STOPPED SEEING a detached run
// (see internal/stale).
//
// Flags (the launchd plists reference these):
//
//	--store-socket     store UDS path (default $AGENT_REPL_STORE_SOCKET, else …/sock/store.sock)
//	--state-dir        agent-repl state root (default $AGENT_REPL_STATE_DIR, else ~/.claude-emacs)
//	--config-roots     comma-separated config roots (~/.claude,~/.claude-chesscom)
//	--spool-root       task-spool root (/tmp; resolves claude-<uid>/… itself)
//	--log              size-capped rotating log file
//	--poll-interval    how often each watched file is polled, and how often the
//	                   directory-change probe looks for NEW files (1s)
//	--rescan-interval  how often the FULL discovery enumeration runs (30s)
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
//	--recover-backoff-min     $AGENT_REPL_RECOVER_BACKOFF_MIN     (250ms)
//	--recover-backoff-max     $AGENT_REPL_RECOVER_BACKOFF_MAX     (10s)
//
// The last two are the store-recovery ladder's floor and ceiling. They were
// the only windows in this process with no override, which left the outage
// subjects — which run a real sidecar, so the cycle's injected clock does not
// reach them — waiting out real rungs of production's ladder.
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

	"agentrepl/shim-claude-sidecar/internal/daemonclient"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/stale"

	sharedlogging "agentrepl/logging"
)

// Defaults for the two loop intervals. Polling is frequent because it is what
// carries a user's prompt echo to the GUI; rescanning is not, because a new
// file appearing is rare and there is no fsnotify path to catch it sooner.
const (
	DefaultPollInterval   = time.Second
	DefaultRescanInterval = 30 * time.Second
)

// StoreSocketEnv names the store's socket for both the store and the sidecar.
// An explicit --store-socket beats it; it is what lets a test store live
// somewhere private without either side hard-coding the path.
const StoreSocketEnv = "AGENT_REPL_STORE_SOCKET"

// StateDirEnv names the agent-repl state root — the same variable the daemon
// resolves and exports into every shim it spawns. The sidecar reads it because
// the shim's identity records live under it: `shim/<workspace-key>/agent-id.json`
// and `shim/<workspace-key>/vendor-id/<vendor-session-id>.json` are the ONLY
// place the link between a rotated vendor session id and the conversation's
// original one exists — the vendor's own transcripts carry no lineage.
// An explicit --state-dir beats it, exactly as --store-socket beats its env.
const StateDirEnv = "AGENT_REPL_STATE_DIR"

// DefaultStateDirName is the state root's location under the home directory
// when neither the flag nor the environment names one. It is the daemon's
// stateroot.DefaultDirName, restated here because this module cannot import the
// daemon; the two are documented on each other and move together or not at all.
const DefaultStateDirName = ".claude-emacs"

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

	// RecoverBackoffMinEnv and RecoverBackoffMaxEnv name the store-recovery
	// ladder's floor and ceiling — the delay before the first retry of a
	// suspended cycle, and the ceiling the doubling holds forever.
	RecoverBackoffMinEnv = "AGENT_REPL_RECOVER_BACKOFF_MIN"
	RecoverBackoffMaxEnv = "AGENT_REPL_RECOVER_BACKOFF_MAX"
)

func main() {
	base := defaultCacheDir()
	storeSocket := flag.String("store-socket", defaultStoreSocket(base), "store UDS path (default $"+StoreSocketEnv+")")
	stateDir := flag.String("state-dir", "",
		"agent-repl state root holding the shim's identity records (default $"+StateDirEnv+", else ~/"+DefaultStateDirName+")")
	configRoots := flag.String("config-roots", "~/.claude,~/.claude-chesscom", "comma-separated config roots")
	spoolRoot := flag.String("spool-root", "/tmp", "task-spool root (resolves claude-<uid>/ itself)")
	logPath := flag.String("log", filepath.Join(base, "log", "shim-claude-sidecar.log"), "log file path (size-capped, rotated into N generations)")
	pollInterval := flag.Duration("poll-interval", DefaultPollInterval, "how often each watched file is polled, and how often the directory-change probe looks for new files")
	rescanInterval := flag.Duration("rescan-interval", DefaultRescanInterval, "how often the full discovery enumeration runs")
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
	recoverBackoffMinFlag := flag.String("recover-backoff-min", "",
		"delay before the first retry of a suspended cycle (Go duration; default $"+RecoverBackoffMinEnv+", else 250ms)")
	recoverBackoffMaxFlag := flag.String("recover-backoff-max", "",
		"ceiling the store-recovery ladder's doubling holds forever (Go duration; default $"+RecoverBackoffMaxEnv+", else 10s)")
	flag.Parse()

	// ONE RESOLUTION, ONE REFUSAL. Every window is resolved by a single tested
	// function and every way of getting one wrong leaves the process through
	// the same two lines. Three separate call-and-check pairs here would put
	// three copies of the refusal inside main, which is the one function in
	// this file no test can enter.
	w, err := resolveWindows(
		durationSource{flagName: "unowned-spool-window", envName: UnownedSpoolWindowEnv, raw: *unownedSpoolWindow},
		durationSource{flagName: "stale-grace", envName: StaleGraceEnv, raw: *staleGrace},
		durationSource{flagName: "stale-shell-silence", envName: StaleShellSilenceEnv, raw: *staleShellSilence},
		durationSource{flagName: "stale-agent-silence", envName: StaleAgentSilenceEnv, raw: *staleAgentSilence},
		durationSource{flagName: "stale-workflow-silence", envName: StaleWorkflowSilenceEnv, raw: *staleWorkflowSilence},
		durationSource{flagName: "recover-backoff-min", envName: RecoverBackoffMinEnv, raw: *recoverBackoffMinFlag},
		durationSource{flagName: "recover-backoff-max", envName: RecoverBackoffMaxEnv, raw: *recoverBackoffMaxFlag},
	)
	if err != nil {
		reportFatal(err, os.Stderr)
		os.Exit(1)
	}

	options := Options{
		StoreSocket:        *storeSocket,
		StateDir:           resolveStateDir(*stateDir),
		ConfigRoots:        parseRoots(*configRoots),
		SpoolRoot:          *spoolRoot,
		PollInterval:       *pollInterval,
		RescanInterval:     *rescanInterval,
		Stale:              w.Stale,
		UnownedSpoolWindow: w.UnownedSpool,
		RecoverBackoffMin:  w.RecoverBackoffMin,
		RecoverBackoffMax:  w.RecoverBackoffMax,
	}
	if err := run(options, *logPath); err != nil {
		reportFatal(err, os.Stderr)
		os.Exit(1)
	}
}

// Options is the sidecar's whole configuration, after flag and env resolution.
type Options struct {
	StoreSocket string
	// StateDir is the agent-repl state root that holds the shim's identity
	// records and the daemon's current address. It is required before the
	// process opens its durable log because file-scoped diagnostics cannot be
	// persisted without it.
	StateDir       string
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
	// RecoverBackoffMin and RecoverBackoffMax are the store-recovery ladder's
	// floor and ceiling. Zero keeps cycle.go's recoverBackoffMin /
	// recoverBackoffMax.
	RecoverBackoffMin time.Duration
	RecoverBackoffMax time.Duration
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

// windows is every duration option this process takes, resolved.
type windows struct {
	Stale             stale.Options
	UnownedSpool      time.Duration
	RecoverBackoffMin time.Duration
	RecoverBackoffMax time.Duration
}

// resolveWindows resolves all seven duration options and answers the FIRST
// refusal, applying nothing when there is one: a bootstrap that was refused
// configures no window at all, rather than half of them.
func resolveWindows(unowned, grace, shellSilence, agentSilence, workflowSilence, backoffMin, backoffMax durationSource) (windows, error) {
	resolvedUnowned, err := unowned.resolve()
	if err != nil {
		return windows{}, err
	}
	staleOptions, err := resolveStaleOptions(grace, shellSilence, agentSilence, workflowSilence)
	if err != nil {
		return windows{}, err
	}
	min, max, err := resolveBackoffOptions(backoffMin, backoffMax)
	if err != nil {
		return windows{}, err
	}
	return windows{
		Stale:             staleOptions,
		UnownedSpool:      resolvedUnowned,
		RecoverBackoffMin: min,
		RecoverBackoffMax: max,
	}, nil
}

// resolveBackoffOptions answers the store-recovery ladder's floor and ceiling.
//
// Zero means "keep the package default", exactly as it does for every other
// window. A CEILING BELOW THE FLOOR IS REFUSED rather than quietly clamped: it
// describes a ladder that cannot climb, which is not a policy anyone chose, and
// a process that started with it would retry forever at a delay the operator
// never asked for. Both spellings are named in the refusal, because the two
// values only conflict together and the operator may have supplied one of them
// through the environment.
func resolveBackoffOptions(min, max durationSource) (time.Duration, time.Duration, error) {
	resolvedMin, err := min.resolve()
	if err != nil {
		return 0, 0, err
	}
	resolvedMax, err := max.resolve()
	if err != nil {
		return 0, 0, err
	}
	effectiveMin, effectiveMax := resolvedMin, resolvedMax
	if effectiveMin == 0 {
		effectiveMin = recoverBackoffMin
	}
	if effectiveMax == 0 {
		effectiveMax = recoverBackoffMax
	}
	if effectiveMax < effectiveMin {
		return 0, 0, bootstrapError{fmt.Errorf(
			"--%s/%s resolves to %s and --%s/%s to %s: a recovery ladder whose ceiling is below its floor cannot climb",
			min.flagName, min.envName, effectiveMin, max.flagName, max.envName, effectiveMax)}
	}
	return resolvedMin, resolvedMax, nil
}

// resolveStateDir answers the state root: the flag when the operator passed one,
// else $AGENT_REPL_STATE_DIR, else $HOME/.claude-emacs — the same precedence the
// daemon's stateroot.Root applies, so both processes resolve one root.
//
// A HOME THAT CANNOT BE RESOLVED answers empty. openLogger refuses that state
// before opening the durable sink because file-scoped diagnostics now require
// daemon.addr beneath the same root; running without it would silently discard
// every workspace diagnostic.
func resolveStateDir(flagValue string) string {
	if dir := strings.TrimSpace(flagValue); dir != "" {
		return expandHome(dir)
	}
	if dir := strings.TrimSpace(os.Getenv(StateDirEnv)); dir != "" {
		return expandHome(dir)
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return ""
	}
	return filepath.Join(home, DefaultStateDirName)
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
	logf, closeLog, err := openLogger(options.StoreSocket, options.StateDir, logPath)
	if err != nil {
		return err
	}
	defer closeLog()
	defer logProcessExit(logf, &err)
	reportBuild(logf, defaultBuildReportDeps())

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
//
// THE SINK IS BOUNDED AND IT IS THE ONLY COPY. Both halves of that matter, and
// both were missing: the log was an ordinary append-only file, and every
// record was ALSO mirrored to stderr, which under launchd is a second
// append-only file the process does not own. This is a long-lived service
// watching thousands of transcripts, so the two copies grew without limit —
// 6.2 GB of stderr beside a 666 MB `--log` on the owner's machine. The durable
// sink now rolls at a byte cap with a fixed number of generations, and the
// terminal keeps only the sink-emergency record it is the last channel for.
func openLogger(storeSocket, stateDir, logPath string) (*logging.Bound, func(), error) {
	return openLoggerTo(os.Stderr, storeSocket, stateDir, logPath)
}

// openLoggerTo is openLogger with the launcher's terminal named explicitly, so
// a test can prove that a normal run writes NOTHING there. The terminal under
// launchd is an append-only file nobody rolls, and "the durable sink is the only
// copy" is a property worth a test rather than a comment.
func openLoggerTo(terminal io.Writer, storeSocket, stateDir, logPath string) (*logging.Bound, func(), error) {
	level, err := sharedlogging.ParseLevel(os.Getenv("AGENT_REPL_LOG_LEVEL"))
	if err != nil {
		return nil, nil, bootstrapError{err}
	}
	if strings.TrimSpace(stateDir) == "" {
		return nil, nil, bootstrapError{fmt.Errorf("agent-repl state directory is empty; daemon.addr cannot be resolved")}
	}
	file, err := sharedlogging.OpenRotating(logPath, sharedlogging.DefaultCapBytes, sharedlogging.DefaultBackups)
	if err != nil {
		return nil, nil, bootstrapError{fmt.Errorf("opening log %q: %w", logPath, err)}
	}
	forwarder := daemonclient.New(stateDir)
	logf := logging.NewForwardingDurableOnlyAtLevel(terminal, file, level, forwarder).
		With(logging.Context{Component: "sidecar", StoreSocket: storeSocket})
	forwarder.SetRefReplacedObserver(refReplacedObserver(logf))
	return logf, func() {
		// BOUNDED, BECAUSE LAUNCHD IS WAITING. The drain dials the daemon once
		// per queued record; with the daemon gone and a boot's backlog queued,
		// an unbounded wait held one stop past three minutes.
		logf.CloseWithin(logging.DefaultShutdownDrain)
		_ = file.Close()
	}, nil
}

// refReplacedObserver states, ONCE PER REPLACEMENT, that the daemon roster
// moved a directory from one minted workspace ref to another — a workspace
// forgotten and the same directory registered again. It is the ORDINARY shape
// of a person closing a workspace and re-creating it over the same path, so it
// is an `info`, never a warning; the warning is reserved for the directory the
// roster genuinely no longer holds, which keeps its own refusal path.
//
// THE RECORD IS GLOBAL ON PURPOSE. A record carrying `workspace_dir` is a
// file-scoped diagnostic and is QUEUED FOR FORWARDING (see Logger.write), and
// this observer runs inside the forwarder — including inside the bounded drain
// at Close, where enqueuing another forward is a panic. So the directory and
// the two ids ride the message, which is the one place they cannot re-enter
// the channel that produced them.
func refReplacedObserver(logf *logging.Bound) func(dir, oldID, newID string) {
	return func(dir, oldID, newID string) {
		logf.With(logging.Context{Operation: "workspace-ref-replaced"}).Log(
			"the daemon roster now registers %s as workspace %s; the cached ref %s it replaced is no longer attributable",
			dir, newID, oldID)
	}
}

// runWithLogger owns process-level failures once canonical logging exists.
// Lower layers keep ownership of the errors they log themselves.
func runWithLogger(options Options, logf *logging.Bound, stop <-chan os.Signal) error {
	// THE CATCH-UP WINDOW OPENS BEFORE ANY FILE IS READ and closes when the
	// first full poll pass has drained the corpus that was already on disk. The
	// operations named here are the ones the boot walk restates wholesale.
	logf.BeginCatchup(catchupOperations...)
	sc := newSidecar(options, logf)
	// THE ROOTS ARE READ BACK OFF THE DISCOVERER, NEVER OFF THE FLAGS. Every
	// root is symlink-resolved when the discoverer is built, and every `path`
	// a later record carries is spelled that way — so naming the flags here
	// would print `/tmp` above thousands of records under `/private/tmp` and
	// break the one join this record exists to make.
	logf.With(logging.Context{Operation: "start"}).Log(
		"sidecar starting config_roots=%v spool_root=%s state_dir=%s poll_interval=%s rescan_interval=%s lost_windows=%+v",
		sc.disc.ConfigRoots(), sc.disc.SpoolRoot(), options.StateDir, options.PollInterval, options.RescanInterval, sc.tracker.Windows())
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
