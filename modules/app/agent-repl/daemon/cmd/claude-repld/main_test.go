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
