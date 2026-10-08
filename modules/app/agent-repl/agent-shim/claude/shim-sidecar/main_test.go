package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/stale"
)

func TestDefaultStoreSocketPrefersTheSharedEnv(t *testing.T) {
	// Arrange: the env var is how a private test store is reached without
	// either side hard-coding a path.
	t.Setenv(StoreSocketEnv, "/private/tmp/ar-test.sock")

	// Act.
	got := defaultStoreSocket("/cache/agent-repl")

	// Assert.
	if got != "/private/tmp/ar-test.sock" {
		t.Fatalf("default store socket = %q, want the env value", got)
	}
}

func TestDefaultStoreSocketFallsBackToTheCacheDir(t *testing.T) {
	// Arrange.
	t.Setenv(StoreSocketEnv, "")

	// Act.
	got := defaultStoreSocket("/cache/agent-repl")

	// Assert.
	if got != filepath.Join("/cache/agent-repl", "sock", "store.sock") {
		t.Fatalf("default store socket = %q, want the cache-dir path", got)
	}
}

func TestAnExplicitSocketBeatsTheEnv(t *testing.T) {
	// Arrange: the flag's default is the env value, so an explicitly parsed
	// flag is what beats it. This asserts the resolution order the ruling names.
	t.Setenv(StoreSocketEnv, "/private/tmp/ar-env.sock")
	options := Options{StoreSocket: "/private/tmp/ar-flag.sock"}

	// Act.
	got := options.StoreSocket

	// Assert.
	if got != "/private/tmp/ar-flag.sock" {
		t.Fatalf("store socket = %q, want the explicit flag to win", got)
	}
}

func TestPollAndRescanDefaults(t *testing.T) {
	tests := []struct {
		name string
		got  time.Duration
		want time.Duration
	}{
		{name: "poll", got: DefaultPollInterval, want: time.Second},
		{name: "rescan", got: DefaultRescanInterval, want: 30 * time.Second},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act, Assert.
			if tc.got != tc.want {
				t.Fatalf("%s interval = %s, want %s", tc.name, tc.got, tc.want)
			}
		})
	}
}

func TestParseRootsSplitsBothConfigRoots(t *testing.T) {
	// Arrange: the second account's transcripts are invisible without its root.
	t.Setenv("HOME", "/home/tester")

	// Act.
	got := parseRoots("~/.claude,~/.claude-chesscom")

	// Assert.
	want := []string{"/home/tester/.claude", "/home/tester/.claude-chesscom"}
	if len(got) != 2 || got[0] != want[0] || got[1] != want[1] {
		t.Fatalf("parseRoots = %v, want %v", got, want)
	}
}

func TestParseRootsTrimsWhitespace(t *testing.T) {
	// Arrange, Act.
	got := parseRoots(" /a , /b ")

	// Assert.
	if len(got) != 2 || got[0] != "/a" || got[1] != "/b" {
		t.Fatalf("parseRoots = %v, want the roots trimmed", got)
	}
}

func TestParseRootsDropsEmptyEntries(t *testing.T) {
	// Arrange, Act.
	got := parseRoots("/a,,/b,")

	// Assert.
	if len(got) != 2 {
		t.Fatalf("parseRoots = %v, want the empty entries dropped", got)
	}
}

func TestParseRootsOnAnEmptyList(t *testing.T) {
	// Arrange, Act.
	got := parseRoots("")

	// Assert.
	if len(got) != 0 {
		t.Fatalf("parseRoots = %v, want no roots", got)
	}
}

func TestExpandHomeLeavesAnAbsolutePathAlone(t *testing.T) {
	// Arrange, Act.
	got := expandHome("/absolute/path")

	// Assert.
	if got != "/absolute/path" {
		t.Fatalf("expandHome = %q, want the path unchanged", got)
	}
}

func TestExpandHomeExpandsABareTilde(t *testing.T) {
	// Arrange.
	t.Setenv("HOME", "/home/tester")

	// Act.
	got := expandHome("~")

	// Assert.
	if got != "/home/tester" {
		t.Fatalf("expandHome = %q, want the home dir", got)
	}
}

func TestExpandHomeDoesNotExpandAMidPathTilde(t *testing.T) {
	// Arrange.
	t.Setenv("HOME", "/home/tester")

	// Act.
	got := expandHome("/opt/~/claude")

	// Assert.
	if got != "/opt/~/claude" {
		t.Fatalf("expandHome = %q, want the path unchanged", got)
	}
}

func TestDefaultCacheDirHonorsXDG(t *testing.T) {
	// Arrange.
	t.Setenv("XDG_CACHE_HOME", "/xdg/cache")

	// Act.
	got := defaultCacheDir()

	// Assert.
	if got != filepath.Join("/xdg/cache", "agent-repl") {
		t.Fatalf("cache dir = %q, want the XDG path", got)
	}
}

func TestDefaultCacheDirFallsBackToHome(t *testing.T) {
	// Arrange.
	t.Setenv("XDG_CACHE_HOME", "")
	t.Setenv("HOME", "/home/tester")

	// Act.
	got := defaultCacheDir()

	// Assert.
	if got != filepath.Join("/home/tester", ".cache", "agent-repl") {
		t.Fatalf("cache dir = %q, want the home cache path", got)
	}
}

func TestBootstrapFailureIsReportedToStderr(t *testing.T) {
	// Arrange: no canonical logger can exist yet, so this is the one path that
	// may write diagnostics itself.
	stderr := &bytes.Buffer{}

	// Act.
	reportFatal(bootstrapError{errors.New("opening log: permission denied")}, stderr)

	// Assert.
	var record map[string]any
	if err := json.Unmarshal(stderr.Bytes(), &record); err != nil {
		t.Fatalf("decoding %q: %v", stderr.String(), err)
	}
	if record["operation"] != "sidecar.bootstrap" {
		t.Fatalf("record = %v, want the bootstrap operation", record)
	}
}

func TestAPostBootstrapFailureIsNotReReported(t *testing.T) {
	// Arrange: every post-bootstrap error has already reached the logger.
	stderr := &bytes.Buffer{}

	// Act.
	reportFatal(errors.New("cursor recovery failed"), stderr)

	// Assert.
	if stderr.Len() != 0 {
		t.Fatalf("stderr = %q, want the error left to its owning layer", stderr.String())
	}
}

func TestOpenLoggerCreatesItsDirectory(t *testing.T) {
	// Arrange.
	t.Setenv("AGENT_REPL_LOG_LEVEL", "info")
	path := filepath.Join(t.TempDir(), "nested", "sidecar.log")

	// Act.
	logf, closeLog, err := openLogger("/private/tmp/store.sock", t.TempDir(), path)
	if err != nil {
		t.Fatalf("openLogger: %v", err)
	}
	defer closeLog()
	logf.With(logging.Context{Operation: "test"}).Log("hello")

	// Assert.
	raw, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("reading %s: %v", path, err)
	}
	if !strings.Contains(string(raw), "hello") {
		t.Fatalf("log = %q, want the record persisted", raw)
	}
}

func TestOpenLoggerFailureIsABootstrapError(t *testing.T) {
	// Arrange: a log path under a regular file cannot be created.
	t.Setenv("AGENT_REPL_LOG_LEVEL", "info")
	base := t.TempDir()
	blocker := filepath.Join(base, "blocker")
	if err := os.WriteFile(blocker, []byte("x"), 0o644); err != nil {
		t.Fatalf("writing %s: %v", blocker, err)
	}

	// Act.
	_, _, err := openLogger("/private/tmp/store.sock", t.TempDir(), filepath.Join(blocker, "sidecar.log"))

	// Assert.
	if !isBootstrapError(err) {
		t.Fatalf("openLogger error = %v, want a bootstrap error", err)
	}
}

func TestOpenLoggerRejectsAnInvalidLevelBeforeCreatingItsDirectory(t *testing.T) {
	// Arrange.
	t.Setenv("AGENT_REPL_LOG_LEVEL", "verbose")
	path := filepath.Join(t.TempDir(), "nested", "sidecar.log")

	// Act.
	_, _, err := openLogger("/private/tmp/store.sock", t.TempDir(), path)

	// Assert.
	if !isBootstrapError(err) {
		t.Fatalf("openLogger error = %v, want a bootstrap error", err)
	}
	if _, statErr := os.Stat(filepath.Dir(path)); !os.IsNotExist(statErr) {
		t.Fatalf("log directory stat = %v, want no state created", statErr)
	}
}

func TestOpenLoggerRejectsAnEmptyStateDirectoryBeforeCreatingItsLog(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "nested", "sidecar.log")

	// Act.
	_, _, err := openLogger("/private/tmp/store.sock", "", path)

	// Assert.
	if !isBootstrapError(err) {
		t.Fatalf("openLogger error = %v, want bootstrap error", err)
	}
	if !strings.Contains(err.Error(), "state directory is empty") {
		t.Fatalf("openLogger error = %v, want the missing state directory named", err)
	}
	if _, statErr := os.Stat(filepath.Dir(path)); !os.IsNotExist(statErr) {
		t.Fatalf("log directory exists after state-dir refusal (stat error %v), want no mutation", statErr)
	}
}

// ---------------------------------------------------------------------------
// The LOST policy's windows: flag, env, refusal.
// ---------------------------------------------------------------------------

func TestAStaleWindowFallsBackToItsEnvVar(t *testing.T) {
	// Arrange: the operator set only the env var, which is how the integration
	// suite shrinks a production window without editing a launchd plist.
	t.Setenv(StaleShellSilenceEnv, "250ms")
	source := durationSource{flagName: "stale-shell-silence", envName: StaleShellSilenceEnv, raw: ""}

	// Act.
	got, err := source.resolve()

	// Assert.
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if got != 250*time.Millisecond {
		t.Fatalf("shell silence = %s, want the env value 250ms", got)
	}
}

func TestAnExplicitStaleWindowBeatsItsEnvVar(t *testing.T) {
	// Arrange.
	t.Setenv(StaleGraceEnv, "9s")
	source := durationSource{flagName: "stale-grace", envName: StaleGraceEnv, raw: "40ms"}

	// Act.
	got, err := source.resolve()

	// Assert.
	if got != 40*time.Millisecond || err != nil {
		t.Fatalf("grace = %s (err %v), want the flag value 40ms to beat the env", got, err)
	}
}

func TestAnUnsetStaleWindowResolvesToZeroSoThePackageDefaultStands(t *testing.T) {
	// Arrange: neither spelling is present.
	t.Setenv(StaleAgentSilenceEnv, "")
	source := durationSource{flagName: "stale-agent-silence", envName: StaleAgentSilenceEnv, raw: ""}

	// Act.
	got, err := source.resolve()

	// Assert: zero is the ONLY meaning of "unset" here — internal/stale fills it.
	if got != 0 || err != nil {
		t.Fatalf("agent silence = %s (err %v), want 0 so internal/stale's default stands", got, err)
	}
}

func TestAMalformedStaleWindowIsABootstrapErrorRatherThanADefault(t *testing.T) {
	// Arrange: a value the operator meant, spelled wrongly.
	source := durationSource{flagName: "stale-workflow-silence", envName: StaleWorkflowSilenceEnv, raw: "30 minutes"}

	// Act.
	_, err := source.resolve()

	// Assert.
	if err == nil {
		t.Fatalf("a malformed window was accepted; it must refuse rather than start on a window nobody chose")
	}
	if !isBootstrapError(err) {
		t.Fatalf("refusal %v is not a bootstrap error, so the process would not state it once and exit non-zero", err)
	}
}

func TestANegativeStaleWindowIsRefused(t *testing.T) {
	// Arrange: a negative silence window concludes every tracked run LOST at once.
	source := durationSource{flagName: "stale-grace", envName: StaleGraceEnv, raw: "-1s"}

	// Act.
	_, err := source.resolve()

	// Assert.
	if !isBootstrapError(err) {
		t.Fatalf("a negative window resolved to %v, want a bootstrap refusal", err)
	}
}

func TestTheFirstUnusableWindowStopsBootstrap(t *testing.T) {
	// Arrange: the third of four windows is unusable.
	good := durationSource{flagName: "stale-grace", envName: StaleGraceEnv, raw: "10ms"}
	bad := durationSource{flagName: "stale-agent-silence", envName: StaleAgentSilenceEnv, raw: "nonsense"}

	// Act.
	got, err := resolveStaleOptions(good, good, bad, good)

	// Assert: nothing partially applied.
	if !isBootstrapError(err) {
		t.Fatalf("resolveStaleOptions returned %v, want a bootstrap refusal", err)
	}
	if got != (stale.Options{}) {
		t.Fatalf("resolveStaleOptions returned %+v alongside a refusal; a refused bootstrap configures nothing", got)
	}
}

func TestEveryResolvedWindowLandsOnItsOwnField(t *testing.T) {
	// Arrange: four distinct values, so a crossed wire is visible.
	source := func(name, env, raw string) durationSource {
		return durationSource{flagName: name, envName: env, raw: raw}
	}

	// Act.
	got, err := resolveStaleOptions(
		source("stale-grace", StaleGraceEnv, "1ms"),
		source("stale-shell-silence", StaleShellSilenceEnv, "2ms"),
		source("stale-agent-silence", StaleAgentSilenceEnv, "3ms"),
		source("stale-workflow-silence", StaleWorkflowSilenceEnv, "4ms"),
	)

	// Assert.
	if err != nil {
		t.Fatalf("resolveStaleOptions: %v", err)
	}
	want := stale.Options{
		Grace:           time.Millisecond,
		ShellSilence:    2 * time.Millisecond,
		AgentSilence:    3 * time.Millisecond,
		WorkflowSilence: 4 * time.Millisecond,
	}
	if got != want {
		t.Fatalf("resolved windows = %+v, want %+v", got, want)
	}
}

// ---------------------------------------------------------------------------
// The store-recovery ladder's two rungs.
// ---------------------------------------------------------------------------

// backoffSources spells the ladder's two options the way main does.
func backoffSources(min, max string) (durationSource, durationSource) {
	return durationSource{flagName: "recover-backoff-min", envName: RecoverBackoffMinEnv, raw: min},
		durationSource{flagName: "recover-backoff-max", envName: RecoverBackoffMaxEnv, raw: max}
}

func TestAnUnsetRecoveryLadderResolvesToZeroSoThePackageDefaultStands(t *testing.T) {
	// Arrange.
	min, max := backoffSources("", "")

	// Act.
	gotMin, gotMax, err := resolveBackoffOptions(min, max)

	// Assert: zero is how "unset" reaches Options; cycle.go fills the default.
	if err != nil {
		t.Fatalf("resolveBackoffOptions: %v", err)
	}
	if gotMin != 0 || gotMax != 0 {
		t.Fatalf("resolveBackoffOptions with nothing set = (%s, %s), want (0s, 0s)", gotMin, gotMax)
	}
}

func TestEachRecoveryRungLandsOnItsOwnField(t *testing.T) {
	// Arrange: two distinct values, so a crossed wire is visible.
	min, max := backoffSources("5ms", "40ms")

	// Act.
	gotMin, gotMax, err := resolveBackoffOptions(min, max)

	// Assert.
	if err != nil {
		t.Fatalf("resolveBackoffOptions: %v", err)
	}
	if gotMin != 5*time.Millisecond || gotMax != 40*time.Millisecond {
		t.Fatalf("resolveBackoffOptions = (%s, %s), want (5ms, 40ms)", gotMin, gotMax)
	}
}

func TestAMalformedRecoveryFloorStopsBootstrap(t *testing.T) {
	// Arrange.
	min, max := backoffSources("nonsense", "40ms")

	// Act.
	gotMin, gotMax, err := resolveBackoffOptions(min, max)

	// Assert: nothing partially applied.
	if !isBootstrapError(err) {
		t.Fatalf("a malformed recovery floor resolved to %v, want a bootstrap refusal", err)
	}
	if gotMin != 0 || gotMax != 0 {
		t.Fatalf("a refused bootstrap returned (%s, %s); it must configure nothing", gotMin, gotMax)
	}
}

func TestAMalformedRecoveryCeilingStopsBootstrap(t *testing.T) {
	// Arrange.
	min, max := backoffSources("5ms", "nonsense")

	// Act.
	_, _, err := resolveBackoffOptions(min, max)

	// Assert.
	if !isBootstrapError(err) {
		t.Fatalf("a malformed recovery ceiling resolved to %v, want a bootstrap refusal", err)
	}
}

func TestARecoveryCeilingBelowItsFloorStopsBootstrap(t *testing.T) {
	// Arrange: a ladder that cannot climb.
	min, max := backoffSources("40ms", "5ms")

	// Act.
	_, _, err := resolveBackoffOptions(min, max)

	// Assert.
	if !isBootstrapError(err) {
		t.Fatalf("a ceiling below its floor resolved to %v, want a bootstrap refusal", err)
	}
}

// TestARecoveryFloorAboveTheDEFAULTCeilingStopsBootstrap asserts the conflict is
// judged against the EFFECTIVE ladder, not only against two values the operator
// happened to spell together: a floor of a minute with the ceiling left unset
// is still a ladder whose ceiling is below its floor.
func TestARecoveryFloorAboveTheDefaultCeilingStopsBootstrap(t *testing.T) {
	// Arrange.
	min, max := backoffSources("60s", "")

	// Act.
	_, _, err := resolveBackoffOptions(min, max)

	// Assert.
	if !isBootstrapError(err) {
		t.Fatalf("a floor above the default ceiling resolved to %v, want a bootstrap refusal", err)
	}
}

// ---------------------------------------------------------------------------
// Resolving every window at once.
// ---------------------------------------------------------------------------

// allWindows spells the seven sources the way main does, from raw values.
func allWindows(unowned, grace, shell, agent, workflow, min, max string) (durationSource, durationSource, durationSource, durationSource, durationSource, durationSource, durationSource) {
	backoffMin, backoffMax := backoffSources(min, max)
	return durationSource{flagName: "unowned-spool-window", envName: UnownedSpoolWindowEnv, raw: unowned},
		durationSource{flagName: "stale-grace", envName: StaleGraceEnv, raw: grace},
		durationSource{flagName: "stale-shell-silence", envName: StaleShellSilenceEnv, raw: shell},
		durationSource{flagName: "stale-agent-silence", envName: StaleAgentSilenceEnv, raw: agent},
		durationSource{flagName: "stale-workflow-silence", envName: StaleWorkflowSilenceEnv, raw: workflow},
		backoffMin, backoffMax
}

func TestEveryResolvedWindowLandsOnItsOwnFieldOfTheWholeSet(t *testing.T) {
	// Arrange: seven distinct values, so a crossed wire is visible.
	a, b, c, d, e, f, g := allWindows("1ms", "2ms", "3ms", "4ms", "5ms", "6ms", "7ms")

	// Act.
	got, err := resolveWindows(a, b, c, d, e, f, g)

	// Assert.
	if err != nil {
		t.Fatalf("resolveWindows: %v", err)
	}
	want := windows{
		Stale: stale.Options{
			Grace:           2 * time.Millisecond,
			ShellSilence:    3 * time.Millisecond,
			AgentSilence:    4 * time.Millisecond,
			WorkflowSilence: 5 * time.Millisecond,
		},
		UnownedSpool:      time.Millisecond,
		RecoverBackoffMin: 6 * time.Millisecond,
		RecoverBackoffMax: 7 * time.Millisecond,
	}
	if got != want {
		t.Fatalf("resolveWindows = %+v, want %+v", got, want)
	}
}

func TestAnUnusableUnownedSpoolWindowStopsTheWholeResolution(t *testing.T) {
	// Arrange.
	a, b, c, d, e, f, g := allWindows("nonsense", "2ms", "3ms", "4ms", "5ms", "6ms", "7ms")

	// Act.
	got, err := resolveWindows(a, b, c, d, e, f, g)

	// Assert: nothing partially applied.
	if !isBootstrapError(err) {
		t.Fatalf("resolveWindows returned %v, want a bootstrap refusal", err)
	}
	if got != (windows{}) {
		t.Fatalf("resolveWindows returned %+v alongside a refusal; a refused bootstrap configures nothing", got)
	}
}

func TestAnUnusableStaleWindowStopsTheWholeResolution(t *testing.T) {
	// Arrange.
	a, b, c, d, e, f, g := allWindows("1ms", "2ms", "nonsense", "4ms", "5ms", "6ms", "7ms")

	// Act.
	got, err := resolveWindows(a, b, c, d, e, f, g)

	// Assert.
	if !isBootstrapError(err) {
		t.Fatalf("resolveWindows returned %v, want a bootstrap refusal", err)
	}
	if got != (windows{}) {
		t.Fatalf("resolveWindows returned %+v alongside a refusal; a refused bootstrap configures nothing", got)
	}
}

func TestAnUnusableRecoveryLadderStopsTheWholeResolution(t *testing.T) {
	// Arrange: a ceiling below its floor.
	a, b, c, d, e, f, g := allWindows("1ms", "2ms", "3ms", "4ms", "5ms", "40ms", "6ms")

	// Act.
	got, err := resolveWindows(a, b, c, d, e, f, g)

	// Assert.
	if !isBootstrapError(err) {
		t.Fatalf("resolveWindows returned %v, want a bootstrap refusal", err)
	}
	if got != (windows{}) {
		t.Fatalf("resolveWindows returned %+v alongside a refusal; a refused bootstrap configures nothing", got)
	}
}

func TestAnUnsetWholeSetResolvesToZerosSoEveryPackageDefaultStands(t *testing.T) {
	// Arrange.
	a, b, c, d, e, f, g := allWindows("", "", "", "", "", "", "")

	// Act.
	got, err := resolveWindows(a, b, c, d, e, f, g)

	// Assert.
	if err != nil {
		t.Fatalf("resolveWindows: %v", err)
	}
	if got != (windows{}) {
		t.Fatalf("resolveWindows with nothing set = %+v, want every field zero", got)
	}
}

// TestResolveStateDirPrecedence pins the three spellings of the state root, in
// the order the daemon's own stateroot.Root applies them. The root is where the
// shim's identity records live, so a process that resolved a DIFFERENT one from
// the daemon would silently book every rotated transcript under the wrong id.
func TestResolveStateDirPrecedence(t *testing.T) {
	home := t.TempDir()
	tests := []struct {
		name string
		flag string
		env  string
		want string
	}{
		{
			name: "an explicit flag beats the environment",
			flag: "/state/from-flag", env: "/state/from-env", want: "/state/from-flag",
		},
		{
			name: "the environment answers when no flag was passed",
			flag: "", env: "/state/from-env", want: "/state/from-env",
		},
		{
			name: "neither one leaves the home-directory default",
			flag: "", env: "", want: filepath.Join(home, DefaultStateDirName),
		},
		{
			name: "a whitespace-only flag is no flag at all",
			flag: "   ", env: "/state/from-env", want: "/state/from-env",
		},
		{
			name: "a leading tilde is expanded, exactly as a config root's is",
			flag: "~/state-here", env: "", want: filepath.Join(home, "state-here"),
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			t.Setenv("HOME", home)
			t.Setenv(StateDirEnv, tc.env)

			// Act.
			got := resolveStateDir(tc.flag)

			// Assert.
			if got != tc.want {
				t.Errorf("resolveStateDir(%q) with $%s=%q = %q, want %q", tc.flag, StateDirEnv, tc.env, got, tc.want)
			}
		})
	}
}

// THE DURABLE SINK IS THE ONLY COPY. Under launchd the terminal is
// `StandardErrorPath` — a plain append-only file the service does not own and
// cannot roll — and mirroring the record stream there grew a 6.28 GB second
// copy beside a rotated 64 MB log until the disk filled and the process
// panicked. The production constructor must write NOTHING there.
func TestOpenLoggerWritesNothingToTheTerminal(t *testing.T) {
	// Arrange.
	base := t.TempDir()
	terminal := &bytes.Buffer{}
	logPath := filepath.Join(base, "sidecar.log")
	logf, closeLog, err := openLoggerTo(terminal, filepath.Join(base, "store.sock"), base, logPath)
	if err != nil {
		t.Fatalf("openLoggerTo: %v", err)
	}
	defer closeLog()

	// Act.
	logf.With(logging.Context{Operation: "start"}).Log("sidecar starting")

	// Assert.
	if terminal.Len() != 0 {
		t.Fatalf("an ordinary record reached the launcher's terminal: %q", terminal.String())
	}
	durable, err := os.ReadFile(logPath)
	if err != nil || len(durable) == 0 {
		t.Fatalf("the record did not reach the durable sink (%d bytes, err=%v)", len(durable), err)
	}
}

// readDurable reads the records one test's logger wrote to its durable sink.
func readDurable(t *testing.T, logPath string) []logRecord {
	t.Helper()
	raw, err := os.ReadFile(logPath)
	if err != nil {
		t.Fatalf("read the durable sink: %v", err)
	}
	return parseLogLines(t, strings.Split(string(raw), "\n"))
}

// A DIRECTORY RE-REGISTERED UNDER A NEW WORKSPACE REF IS AN ORDINARY EVENT, and
// the two ids are the only thing that joins the records written before it to
// the records written after. It is stated once, at info — never at warn, which
// is reserved for the directory the roster genuinely no longer holds.
func TestRefReplacedIsStatedOnceAtInfo(t *testing.T) {
	// Arrange.
	base := t.TempDir()
	logPath := filepath.Join(base, "sidecar.log")
	logf, closeLog, err := openLoggerTo(&bytes.Buffer{}, filepath.Join(base, "store.sock"), base, logPath)
	if err != nil {
		t.Fatalf("openLoggerTo: %v", err)
	}

	// Act.
	refReplacedObserver(logf)("/work/repo", "af24557b1ddd4b9c", "54578ede3d834dea")
	closeLog()

	// Assert.
	record := requireOnceIn(t, readDurable(t, logPath), "workspace-ref-replaced", "info")
	for _, want := range []string{"/work/repo", "af24557b1ddd4b9c", "54578ede3d834dea"} {
		if !strings.Contains(record.Message, want) {
			t.Fatalf("the replacement record does not name %q: %q", want, record.Message)
		}
	}
}

// THE REPLACEMENT RECORD IS GLOBAL, NOT FILE-SCOPED. A record carrying
// `workspace_dir` is QUEUED FOR FORWARDING, and this one is written from inside
// the forwarder — including inside the bounded drain at Close, where enqueuing
// another forward panics the process.
func TestRefReplacedIsGlobalSoItIsNeverQueuedForForwarding(t *testing.T) {
	// Arrange.
	base := t.TempDir()
	logPath := filepath.Join(base, "sidecar.log")
	logf, closeLog, err := openLoggerTo(&bytes.Buffer{}, filepath.Join(base, "store.sock"), base, logPath)
	if err != nil {
		t.Fatalf("openLoggerTo: %v", err)
	}

	// Act.
	refReplacedObserver(logf)("/work/repo", "af24557b1ddd4b9c", "54578ede3d834dea")
	closeLog()

	// Assert.
	raw, err := os.ReadFile(logPath)
	if err != nil {
		t.Fatalf("read the durable sink: %v", err)
	}
	if strings.Contains(string(raw), `"workspace_dir"`) {
		t.Fatalf("the replacement record is file-scoped and would be forwarded: %s", raw)
	}
}

func TestOpenLoggerStartsALeftoverDebugLevelAtInfo(t *testing.T) {
	// Arrange. A debug level with no window is a leftover, never a setting.
	t.Setenv("AGENT_REPL_LOG_LEVEL", "debug")
	t.Setenv("AGENT_REPL_LOG_LEVEL_UNTIL", "")
	base := t.TempDir()
	logPath := filepath.Join(base, "sidecar.log")
	logf, closeLog, err := openLoggerTo(&bytes.Buffer{}, filepath.Join(base, "store.sock"), base, logPath)
	if err != nil {
		t.Fatalf("openLoggerTo: %v", err)
	}

	// Act.
	logf.With(logging.Context{Operation: "tail"}).LogVerbose("dropped")
	closeLog()

	// Assert.
	raw, err := os.ReadFile(logPath)
	if err != nil {
		t.Fatalf("read the log: %v", err)
	}
	if strings.Contains(string(raw), `"operation":"tail"`) {
		t.Fatalf("log = %q, want the debug record dropped", raw)
	}
	if !strings.Contains(string(raw), `"outcome":"no_expiry"`) {
		t.Fatalf("log = %q, want the ignored level noted", raw)
	}
}

func TestOpenLoggerRejectsAMalformedLevelWindow(t *testing.T) {
	// Arrange.
	t.Setenv("AGENT_REPL_LOG_LEVEL", "debug")
	t.Setenv("AGENT_REPL_LOG_LEVEL_UNTIL", "soon")
	base := t.TempDir()

	// Act.
	_, _, err := openLoggerTo(&bytes.Buffer{}, filepath.Join(base, "store.sock"), base, filepath.Join(base, "sidecar.log"))

	// Assert.
	var bootstrap bootstrapError
	if !errors.As(err, &bootstrap) {
		t.Fatalf("error %T = %v, want a bootstrap error", err, err)
	}
}
