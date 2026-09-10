package main

import (
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// TestTheStoreSocketPrecedence pins the store socket's three-way precedence:
// the flag beats the environment, which beats the default under the home
// directory. The socket ALWAYS rides argv to the shim, so whichever wins here
// is what every session is told.
func TestTheStoreSocketPrecedence(t *testing.T) {
	// Arrange.
	tests := []struct {
		name      string
		flagValue string
		envValue  string
		want      string
	}{
		{name: "the flag beats the environment", flagValue: "/run/flag.sock", envValue: "/run/env.sock", want: "/run/flag.sock"},
		{name: "the environment beats the default", flagValue: "", envValue: "/run/env.sock", want: "/run/env.sock"},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Act.
			got := resolveStoreSocket(test.flagValue, test.envValue)

			// Assert.
			if got != test.want {
				t.Fatalf("resolveStoreSocket(%q, %q) = %q, want %q", test.flagValue, test.envValue, got, test.want)
			}
		})
	}
}

// TestTheStoreSocketFallsBackToTheHomeDefault pins the last step of the same
// precedence: with neither a flag nor an environment value the daemon names the
// store's default socket rather than passing nothing.
func TestTheStoreSocketFallsBackToTheHomeDefault(t *testing.T) {
	// Arrange.
	t.Setenv("HOME", "/home/tester")

	// Act.
	got := resolveStoreSocket("", "")

	// Assert.
	if want := filepath.Join("/home/tester", defaultStoreSocket); got != want {
		t.Fatalf("resolveStoreSocket(\"\", \"\") = %q, want %q", got, want)
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
