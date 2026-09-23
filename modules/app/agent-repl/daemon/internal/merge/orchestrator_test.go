package merge

import (
	"strings"
	"testing"
)

// TestNewRefusesAnIncompleteDependencySet covers the construction guard:
// missing collaborators are found at construction rather than mid-merge, where
// the failure would land on a half-applied merge.
func TestNewRefusesAnIncompleteDependencySet(t *testing.T) {
	tests := []struct {
		name string
		drop func(*Deps)
		want string
	}{
		{name: "no store", drop: func(d *Deps) { d.DB = nil }, want: "DB"},
		{name: "no git", drop: func(d *Deps) { d.Git = nil }, want: "Git"},
		{name: "no queue", drop: func(d *Deps) { d.Queue = nil }, want: "Queue"},
		{name: "no brief loader", drop: func(d *Deps) { d.Briefs = nil }, want: "Briefs"},
		{name: "no painter", drop: func(d *Deps) { d.Painter = nil }, want: "Painter"},
		{name: "no test runner", drop: func(d *Deps) { d.TestRunner = nil }, want: "TestRunner"},
		{name: "no session stop", drop: func(d *Deps) { d.StopSession = nil }, want: "StopSession"},
		{name: "no freeness", drop: func(d *Deps) { d.Freeness = nil }, want: "Freeness"},
		{name: "no rollout trigger", drop: func(d *Deps) { d.Rollout = nil }, want: "Rollout"},
		{name: "no state root", drop: func(d *Deps) { d.StateDir = "" }, want: "StateDir"},
		{name: "no self repository", drop: func(d *Deps) { d.SelfRepoDir = "" }, want: "SelfRepoDir"},
		{name: "no test command", drop: func(d *Deps) { d.TestCommand = nil }, want: "TestCommand"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a complete set with one collaborator removed.
			h := newHarness(t)
			deps := h.deps()
			tc.drop(&deps)

			// Act.
			_, err := New(deps)

			// Assert.
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("New answered %v, want it to name the missing %s", err, tc.want)
			}
		})
	}
}

// TestNewAcceptsACompleteDependencySet covers the other side of the guard.
func TestNewAcceptsACompleteDependencySet(t *testing.T) {
	// Arrange: every collaborator present.
	h := newHarness(t)

	// Act.
	o, err := New(h.deps())

	// Assert.
	if err != nil || o == nil {
		t.Fatalf("New failed on a complete set: %v", err)
	}
}

// TestFactsAreAbsentBeforeAnyMerge covers the roster's question for an ordinary
// workspace: it has no merge, which is not the same as a merge in some state.
func TestFactsAreAbsentBeforeAnyMerge(t *testing.T) {
	// Arrange: a workspace that never merged.
	h := newHarness(t)

	// Act.
	_, has := h.o.Facts(theWorkspace)

	// Assert.
	if has {
		t.Fatal("a workspace with no merge reported merge facts")
	}
}

// TestTestCommandPrefersTheScriptOverride covers the gate's test knob, whose
// existence is what lets the landed-range-to-deploy path be asserted end to end
// without running the repository's real suite.
func TestTestCommandPrefersTheScriptOverride(t *testing.T) {
	// Arrange: the override set.
	t.Setenv("AGENT_REPL_TEST_ALL_SCRIPT", "/fixtures/test-all.sh")

	// Act.
	got := TestCommandFor("/checkout")

	// Assert.
	if len(got) != 2 || got[0] != "bash" || got[1] != "/fixtures/test-all.sh" {
		t.Fatalf("TestCommandFor answered %v, want bash and the override", got)
	}
}

// TestTestCommandFallsBackToTheRepositoryEntrypoint covers the ordinary case.
func TestTestCommandFallsBackToTheRepositoryEntrypoint(t *testing.T) {
	// Arrange: no override.
	t.Setenv("AGENT_REPL_TEST_ALL_SCRIPT", "")

	// Act.
	got := TestCommandFor("/checkout")

	// Assert.
	want := "/checkout/modules/app/agent-repl/bin/test-all.sh"
	if len(got) != 2 || got[1] != want {
		t.Fatalf("TestCommandFor answered %v, want %q", got, want)
	}
}
