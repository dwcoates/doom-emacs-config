package main

import (
	"errors"
	"flag"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	"claude-repld/internal/daemonaddr"
	"claude-repld/internal/merge"
)

// TestTheStoreSocketPrecedence pins the store socket's three-way precedence:
// the flag beats the environment, which beats the default under the home
// directory. The socket ALWAYS rides argv to the shim, so whichever wins here
// is what every session is told.
func TestTheStoreSocketPrecedence(t *testing.T) {
	// Arrange.
	home := func() (string, error) { return "/home/tester", nil }
	tests := []struct {
		name      string
		flagValue string
		envValue  string
		want      string
	}{
		{name: "the flag beats the environment", flagValue: "/run/flag.sock", envValue: "/run/env.sock", want: "/run/flag.sock"},
		{name: "the environment beats the default", flagValue: "", envValue: "/run/env.sock", want: "/run/env.sock"},
		{name: "the default is under the home directory", want: "/home/tester/.cache/agent-repl/sock/store.sock"},
		{name: "a tilde override is expanded", envValue: "~/s/store.sock", want: "/home/tester/s/store.sock"},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Act.
			got, err := resolveStoreSocket(test.flagValue, test.envValue, home)

			// Assert.
			if err != nil || got != test.want {
				t.Fatalf("resolveStoreSocket(%q, %q) = (%q, %v), want %q", test.flagValue, test.envValue, got, err, test.want)
			}
		})
	}
}

// TestTheStoreSocketRefusals pins that the socket is never guessed: a relative
// one is refused, and so is a default whose home directory cannot be named,
// which used to fall back to the bare relative default.
func TestTheStoreSocketRefusals(t *testing.T) {
	tests := []struct {
		name      string
		flagValue string
		home      func() (string, error)
		wantErr   string
	}{
		{name: "a relative flag is refused", flagValue: "run/store.sock", home: func() (string, error) { return "/home/tester", nil }, wantErr: "not an absolute path"},
		{name: "an unnamed home refuses the default", home: func() (string, error) { return "", errors.New("$HOME is not defined") }, wantErr: "$HOME is not defined"},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Act.
			got, err := resolveStoreSocket(test.flagValue, "", test.home)

			// Assert.
			if err == nil || !strings.Contains(err.Error(), test.wantErr) {
				t.Fatalf("resolveStoreSocket() = (%q, %v), want an error containing %q", got, err, test.wantErr)
			}
		})
	}
}

// TestAnAbsoluteStoreSocketNeverAsksForHome pins that the home directory is
// asked for only when the winning socket needs it.
func TestAnAbsoluteStoreSocketNeverAsksForHome(t *testing.T) {
	// Arrange.
	asked := false
	home := func() (string, error) { asked = true; return "", errors.New("unreachable") }

	// Act.
	got, err := resolveStoreSocket("/run/flag.sock", "", home)

	// Assert.
	if err != nil || got != "/run/flag.sock" || asked {
		t.Fatalf("resolveStoreSocket() = (%q, %v), asked for home = %v, want the flag and no home lookup", got, err, asked)
	}
}

// TestNoArgvIsLegal pins the launch contract: Emacs starts the binary with NO
// argv and the state travels in the environment, so an empty command line
// parses.
func TestNoArgvIsLegal(t *testing.T) {
	// Arrange.
	t.Setenv("AGENT_REPL_STORE_SOCKET", "/run/env.sock")

	// Act.
	opts, err := parseFlags("claude-repld", nil)

	// Assert.
	if err != nil {
		t.Fatalf("parseFlags with no argv: %v", err)
	}
	if opts.joining != "" {
		t.Fatalf("opts.joining = %q, want an incumbent", opts.joining)
	}
}

// TestTheFlagSetSpellsEveryBindingName pins the AGENTS.md flag table: a
// renamed flag is a launch that silently loses an override, because Go's flag
// package rejects the old spelling rather than ignoring it.
func TestTheFlagSetSpellsEveryBindingName(t *testing.T) {
	// Arrange.
	argv := []string{
		"--state-dir", "/state",
		"--fake",
		"--joining", "127.0.0.1:41111",
		"--store-socket", "/run/store.sock",
		"--shim-main", "/checkout/main.js",
		"--node", "/usr/bin/node",
		"--webapp-dist", "/checkout/dist",
		"--prompts-dir", "/checkout/prompts",
		"--default-config-dir", "/home/tester/.claude",
		"--multi-repo-config-dir", "/home/tester/.claude-multi",
		"--idle-cutoff", "45m",
		"--pprof", "/tmp/pprof.sock",
		"--self-repo", "/checkout",
		"--feed-tail-retention", "16",
		"--footer-momentary-dwell", "120ms",
	}

	// Act.
	opts, err := parseFlags("claude-repld", argv)

	// Assert.
	if err != nil {
		t.Fatalf("parseFlags: %v", err)
	}
	switch {
	case opts.stateDir != "/state":
		t.Fatalf("opts.stateDir = %q", opts.stateDir)
	case !opts.fake:
		t.Fatal("opts.fake = false, want the flag honored")
	case opts.joining != "127.0.0.1:41111":
		t.Fatalf("opts.joining = %q", opts.joining)
	case opts.storeSocket != "/run/store.sock":
		t.Fatalf("opts.storeSocket = %q", opts.storeSocket)
	case opts.shim != "/checkout/main.js":
		t.Fatalf("opts.shim = %q", opts.shim)
	case opts.webapp != "/checkout/dist":
		t.Fatalf("opts.webapp = %q", opts.webapp)
	case opts.promptsDir != "/checkout/prompts":
		t.Fatalf("opts.promptsDir = %q", opts.promptsDir)
	case opts.defaultConfigDir != "/home/tester/.claude":
		t.Fatalf("opts.defaultConfigDir = %q", opts.defaultConfigDir)
	case opts.multiRepoConfigDir != "/home/tester/.claude-multi":
		t.Fatalf("opts.multiRepoConfigDir = %q", opts.multiRepoConfigDir)
	case opts.idleCutoff != 45*time.Minute:
		t.Fatalf("opts.idleCutoff = %v", opts.idleCutoff)
	case opts.pprof != "/tmp/pprof.sock":
		t.Fatalf("opts.pprof = %q", opts.pprof)
	case opts.selfRepo != "/checkout":
		t.Fatalf("opts.selfRepo = %q", opts.selfRepo)
	case opts.feedTailRetention != 16:
		t.Fatalf("opts.feedTailRetention = %d", opts.feedTailRetention)
	}
}

func TestTheRolloutFlagsAreParsed(t *testing.T) {
	tests := []struct {
		name          string
		argv          []string
		wantReplacing bool
		wantLayout    bool
	}{
		{name: "the replacing flag", argv: []string{"--replacing"}, wantReplacing: true},
		{name: "the layout question", argv: []string{"-layout-version"}, wantLayout: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			opts, err := parseFlags("claude-repld", tt.argv)

			// Assert.
			if err != nil {
				t.Fatalf("parseFlags: %v", err)
			}
			if opts.replacing != tt.wantReplacing || opts.layoutVersion != tt.wantLayout {
				t.Fatalf("replacing %v layoutVersion %v, want %v %v", opts.replacing, opts.layoutVersion, tt.wantReplacing, tt.wantLayout)
			}
		})
	}
}

// TestAReplacementIsNeverAlsoJoining pins that the two roles are exclusive: a
// replacement boots as the incumbent once its predecessor is gone, a
// successor joins a predecessor that is still serving.
func TestAReplacementIsNeverAlsoJoining(t *testing.T) {
	// Act.
	_, err := parseFlags("claude-repld", []string{"--replacing", "--joining", "127.0.0.1:1"})

	// Assert.
	if err == nil {
		t.Fatal("parseFlags accepted a daemon that both replaces and joins")
	}
}

// sampleFlagValue is a set, non-default value for flag f, by its value type.
func sampleFlagValue(t *testing.T, f *flag.Flag) string {
	t.Helper()
	if b, ok := f.Value.(interface{ IsBoolFlag() bool }); ok && b.IsBoolFlag() {
		return "true"
	}
	switch f.DefValue {
	case "0":
		return "7"
	case "0s":
		return "1m0s"
	}
	return "/sample/" + f.Name
}

func TestParseFlagsInheritsOnlyTheConfigurationFlagsThatWereSet(t *testing.T) {
	tests := []struct {
		name string
		argv []string
		want []string
	}{
		{name: "nothing set inherits nothing", want: []string{}},
		{name: "a set bool is inherited and an unset default is not", argv: []string{"--fake"}, want: []string{"--fake=true"}},
		{name: "a separated value is inherited as parsed", argv: []string{"--default-config-dir", "/roots/default"}, want: []string{"--default-config-dir=/roots/default"}},
		{name: "a replacement's role is not inherited", argv: []string{"--replacing", "--default-config-dir", "/roots/default"}, want: []string{"--default-config-dir=/roots/default"}},
		{name: "a successor's role is not inherited", argv: []string{"--joining", "127.0.0.1:9", "--node", "/bin/node"}, want: []string{"--node=/bin/node"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			opts, err := parseFlags("claude-repld", tt.argv)

			// Assert
			if err != nil {
				t.Fatalf("parseFlags: %v", err)
			}
			if !slices.Equal(opts.inherited, tt.want) {
				t.Fatalf("inherited = %v, want %v", opts.inherited, tt.want)
			}
		})
	}
}

// TestEveryConfigurationFlagRoundTripsThroughItsInheritedArgv pins that a
// spawned daemon parses the inherited argv back to the same configuration,
// for EVERY configuration flag, so a new flag of a new value type cannot be
// inherited in a spelling its own parse would read differently.
func TestEveryConfigurationFlagRoundTripsThroughItsInheritedArgv(t *testing.T) {
	var scratch options
	config, _ := newFlagSets("claude-repld", &scratch)
	config.VisitAll(func(f *flag.Flag) {
		t.Run(f.Name, func(t *testing.T) {
			// Arrange
			argv := []string{"--" + f.Name + "=" + sampleFlagValue(t, f)}
			first, err := parseFlags("claude-repld", argv)
			if err != nil {
				t.Fatalf("parseFlags(%v): %v", argv, err)
			}

			// Act
			second, err := parseFlags("claude-repld", first.inherited)

			// Assert
			if err != nil {
				t.Fatalf("parseFlags(%v): %v", first.inherited, err)
			}
			if len(first.inherited) != 1 || !slices.Equal(second.inherited, first.inherited) {
				t.Fatalf("inherited %v re-parsed to %v, want one flag that round-trips", first.inherited, second.inherited)
			}
		})
	})
}

// TestNoBootFlagIsEverInherited pins the split itself: set on its own, every
// boot flag leaves the inherited argv empty.
func TestNoBootFlagIsEverInherited(t *testing.T) {
	var scratch options
	_, boot := newFlagSets("claude-repld", &scratch)
	boot.VisitAll(func(f *flag.Flag) {
		t.Run(f.Name, func(t *testing.T) {
			// Arrange
			argv := []string{"--" + f.Name + "=" + sampleFlagValue(t, f)}

			// Act
			opts, err := parseFlags("claude-repld", argv)

			// Assert
			if err != nil {
				t.Fatalf("parseFlags(%v): %v", argv, err)
			}
			if len(opts.inherited) != 0 {
				t.Fatalf("inherited = %v, want no flag from boot flag %s", opts.inherited, f.Name)
			}
		})
	})
}

// TestTheGraphNamesEveryUnwiredCollaborator pins that a graph which cannot be
// built says WHICH collaborator has no landed source, rather than failing with
// a bare refusal a reader has to go hunting behind. The list is empty today —
// every collaborator has a producer — and the test stays so that adding one
// keeps the refusal legible.
func TestTheGraphNamesEveryUnwiredCollaborator(t *testing.T) {
	// Arrange.
	if len(unwired) == 0 {
		t.Skip("the graph is fully wired")
	}

	// Act.
	_, err := buildGraph(t.Context(), process{})

	// Assert.
	if err == nil {
		t.Fatal("buildGraph returned no error while collaborators are unwired")
	}
	for _, u := range unwired {
		if !strings.Contains(err.Error(), u) {
			t.Fatalf("the refusal does not name %q: %v", u, err)
		}
	}
}

// TestTheFeedTailRetentionEnvironmentKnobBeatsTheFlag pins the precedence a
// test relies on: a suite sets the environment and must not also have to know
// how the daemon under it was launched.
func TestTheFeedTailRetentionEnvironmentKnobBeatsTheFlag(t *testing.T) {
	// Arrange, Act.
	got, err := resolveFeedTailRetention(4096, "3")

	// Assert.
	if err != nil {
		t.Fatalf("resolveFeedTailRetention: %v", err)
	}
	if got != 3 {
		t.Fatalf("retention = %d, want the environment's 3", got)
	}
}

// TestTheFeedTailRetentionKnobRefusesANonNumber covers the loud failure: a knob
// that silently did nothing would make the suite it was set for lie.
func TestTheFeedTailRetentionKnobRefusesANonNumber(t *testing.T) {
	// Arrange, Act.
	_, err := resolveFeedTailRetention(0, "lots")

	// Assert.
	if err == nil {
		t.Fatal("resolveFeedTailRetention with a non-numeric knob = nil, want a refusal")
	}
}

// TestTheFeedTailRetentionKnobRefusesANonPositiveNumber covers the other
// malformed shape: zero rows retains nothing and is not what any caller means.
func TestTheFeedTailRetentionKnobRefusesANonPositiveNumber(t *testing.T) {
	// Arrange, Act.
	_, err := resolveFeedTailRetention(0, "0")

	// Assert.
	if err == nil {
		t.Fatal("resolveFeedTailRetention with a zero knob = nil, want a refusal")
	}
}

// TestTheFeedTailRetentionDefaultsToTheResolversOwn covers the ordinary boot:
// neither the flag nor the knob is set, and zero means the resolver decides.
func TestTheFeedTailRetentionDefaultsToTheResolversOwn(t *testing.T) {
	// Arrange, Act.
	got, err := resolveFeedTailRetention(0, "")

	// Assert.
	if err != nil {
		t.Fatalf("resolveFeedTailRetention: %v", err)
	}
	if got != 0 {
		t.Fatalf("retention = %d, want zero so the resolver's own default stands", got)
	}
}

// TestTheFooterMomentaryDwellFlagIsParsed covers the flag itself: the dwell is
// a product window, so a caller that wants a different one states it on the
// command line exactly as -feed-tail-retention is stated.
func TestTheFooterMomentaryDwellFlagIsParsed(t *testing.T) {
	// Arrange, Act.
	opts, err := parseFlags("claude-repld", []string{"--footer-momentary-dwell", "120ms"})

	// Assert.
	if err != nil {
		t.Fatalf("parseFlags: %v", err)
	}
	if opts.footerMomentaryDwell != 120*time.Millisecond {
		t.Fatalf("opts.footerMomentaryDwell = %s, want the flag's 120ms", opts.footerMomentaryDwell)
	}
}

// TestTheFooterMomentaryDwellEnvironmentKnobBeatsTheFlag pins the precedence a
// test relies on: a suite sets the environment and must not also have to know
// how the daemon under it was launched.
func TestTheFooterMomentaryDwellEnvironmentKnobBeatsTheFlag(t *testing.T) {
	// Arrange, Act.
	got, err := resolveFooterMomentaryDwell(1500*time.Millisecond, "40ms")

	// Assert.
	if err != nil {
		t.Fatalf("resolveFooterMomentaryDwell: %v", err)
	}
	if got != 40*time.Millisecond {
		t.Fatalf("dwell = %s, want the environment's 40ms", got)
	}
}

// TestTheFooterMomentaryDwellKnobRefusesANonDuration covers the loud failure: a
// knob that silently did nothing would make the suite it was set for lie about
// how long the daemon actually held the status.
func TestTheFooterMomentaryDwellKnobRefusesANonDuration(t *testing.T) {
	// Arrange, Act.
	_, err := resolveFooterMomentaryDwell(0, "a while")

	// Assert.
	if err == nil {
		t.Fatal("resolveFooterMomentaryDwell with a non-duration knob = nil, want a refusal")
	}
}

// TestTheFooterMomentaryDwellKnobRefusesANonPositiveDuration covers the other
// malformed shape: a zero dwell retires the status in the same instant it is
// published, which is not a window any caller means to ask for.
func TestTheFooterMomentaryDwellKnobRefusesANonPositiveDuration(t *testing.T) {
	// Arrange, Act.
	_, err := resolveFooterMomentaryDwell(0, "0s")

	// Assert.
	if err == nil {
		t.Fatal("resolveFooterMomentaryDwell with a zero knob = nil, want a refusal")
	}
}

// TestTheFooterMomentaryDwellRefusesANegativeFlag covers the flag's own
// malformed shape, which the environment's parse never reaches.
func TestTheFooterMomentaryDwellRefusesANegativeFlag(t *testing.T) {
	// Arrange, Act.
	_, err := resolveFooterMomentaryDwell(-time.Second, "")

	// Assert.
	if err == nil {
		t.Fatal("resolveFooterMomentaryDwell with a negative flag = nil, want a refusal")
	}
}

// TestTheFooterMomentaryDwellDefaultsToTheResolversOwn covers the ordinary
// boot: neither the flag nor the knob is set, and zero means the footer
// resolver's DefaultMomentaryDwell decides.
func TestTheFooterMomentaryDwellDefaultsToTheResolversOwn(t *testing.T) {
	// Arrange, Act.
	got, err := resolveFooterMomentaryDwell(0, "")

	// Assert.
	if err != nil {
		t.Fatalf("resolveFooterMomentaryDwell: %v", err)
	}
	if got != 0 {
		t.Fatalf("dwell = %s, want zero so the resolver's own default stands", got)
	}
}

// TestTheClaimWaitOutlastsTheMergeDrain is the drift guard on
// daemonaddr.ClaimWaitBound. This is the one package that may see both the
// claim bound and the term that dominates a healthy shutdown, and the bound is
// only meaningful while it outlasts that term: a replacement that gives up
// before the outgoing daemon's merge drain has even finished is the
// 2026-09-12 restart that destroyed the daemon.
func TestTheClaimWaitOutlastsTheMergeDrain(t *testing.T) {
	// Arrange, Act.
	got := daemonaddr.ClaimWaitBound

	// Assert.
	if got <= merge.TerminalDrainBound {
		t.Fatalf("ClaimWaitBound = %s, want more than the merge drain bound %s", got, merge.TerminalDrainBound)
	}
}

// TestProductionHooksWaitForAHeldClaim pins that the production boot -- and
// only the production boot -- pays the claim bound. A hooks value that left it
// zero would boot with the pre-2026-09-12 exit-on-first-refusal behavior.
func TestProductionHooksWaitForAHeldClaim(t *testing.T) {
	// Arrange, Act.
	got := productionHooks().ClaimWait

	// Assert.
	if got != daemonaddr.ClaimWaitBound {
		t.Fatalf("productionHooks().ClaimWait = %s, want %s", got, daemonaddr.ClaimWaitBound)
	}
}

// TestProbeBootClaimReportsAFreeClaim pins the elisp departure wait's answer
// for a state root nobody holds.
func TestProbeBootClaimReportsAFreeClaim(t *testing.T) {
	// Arrange.
	root := shortRoot(t)
	if err := os.MkdirAll(root, 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}

	// Act.
	err := probeBootClaim(options{stateDir: root})

	// Assert.
	if err != nil {
		t.Fatalf("probeBootClaim on a free claim = %v, want nil", err)
	}
}

// TestProbeBootClaimReportsAHeldClaim pins the other answer: while a daemon
// holds the claim it has NOT departed, whatever daemon.addr says.
func TestProbeBootClaimReportsAHeldClaim(t *testing.T) {
	// Arrange.
	root := shortRoot(t)
	if err := os.MkdirAll(root, 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}
	incumbent, err := daemonaddr.Bind(filepath.Join(root, "daemon.addr"), 0)
	if err != nil {
		t.Fatalf("the incumbent could not bind: %v", err)
	}
	defer incumbent.Close()

	// Act.
	got := probeBootClaim(options{stateDir: root})

	// Assert.
	if !errors.Is(got, daemonaddr.ErrClaimed) {
		t.Fatalf("probeBootClaim = %v, want it to wrap %v", got, daemonaddr.ErrClaimed)
	}
}

// TestProbeBootClaimIsOffByDefault pins that no ordinary launch ever probes:
// the daemon Emacs spawns must still be a daemon.
func TestProbeBootClaimIsOffByDefault(t *testing.T) {
	// Arrange, Act.
	opts, err := parseFlags("claude-repld", nil)

	// Assert.
	if err != nil {
		t.Fatalf("parseFlags: %v", err)
	}
	if opts.probeBootClaim {
		t.Fatalf("probeBootClaim = true with no argv, want false")
	}
}

// TestProbeBootClaimFlagIsParsed pins the flag Emacs's departure wait spells.
func TestProbeBootClaimFlagIsParsed(t *testing.T) {
	// Arrange, Act.
	opts, err := parseFlags("claude-repld", []string{"-probe-boot-claim"})

	// Assert.
	if err != nil {
		t.Fatalf("parseFlags: %v", err)
	}
	if !opts.probeBootClaim {
		t.Fatalf("probeBootClaim = false with -probe-boot-claim, want true")
	}
}
