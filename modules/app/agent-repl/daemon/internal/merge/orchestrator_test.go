package merge

import (
	"strings"
	"testing"
	"time"

	"agent-repld/internal/resolve/footer"
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
		{name: "no banners", drop: func(d *Deps) { d.Banners = nil }, want: "Banners"},
		{name: "no brief loader", drop: func(d *Deps) { d.Briefs = nil }, want: "Briefs"},
		{name: "no painter", drop: func(d *Deps) { d.Painter = nil }, want: "Painter"},
		{name: "no test runner", drop: func(d *Deps) { d.TestRunner = nil }, want: "TestRunner"},
		{name: "no session stop", drop: func(d *Deps) { d.StopSession = nil }, want: "StopSession"},
		{name: "no freeness", drop: func(d *Deps) { d.Freeness = nil }, want: "Freeness"},
		{name: "no rollout trigger", drop: func(d *Deps) { d.Rollout = nil }, want: "Rollout"},
		{name: "no state root", drop: func(d *Deps) { d.StateDir = "" }, want: "StateDir"},
		{name: "no hook runner", drop: func(d *Deps) { d.HookRunner = nil }, want: "HookRunner"},
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
	got := TestCommandFor("/checkout", "bin/suites.sh")

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
	got := TestCommandFor("/checkout", "bin/suites.sh")

	// Assert: the config's script, under the tree being tested.
	want := "/checkout/bin/suites.sh"
	if len(got) != 2 || got[1] != want {
		t.Fatalf("TestCommandFor answered %v, want %q", got, want)
	}
}

func TestAMergeInLineOrInFlightWithNoQueueInstantIsRecorded(t *testing.T) {
	tests := []struct {
		name  string
		facts MergeFacts
	}{
		{name: "queued", facts: MergeFacts{State: StateQueued, Step: footer.StepEnqueued}},
		{name: "merging", facts: MergeFacts{State: StateMerging, Step: footer.StepTesting}},
		{name: "merging from the epoch", facts: MergeFacts{State: StateMerging, QueuedAt: time.UnixMilli(0), Step: footer.StepTesting}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			h.o.publish(h.repoKey(), theWorkspace, tc.facts)

			// Assert: recorded with what diagnoses it, and still published.
			record, ok := h.recordFor("error", "daemon.merge.publish")
			if !ok {
				t.Fatal("no daemon.merge.publish error was recorded")
			}
			if record.Context["state"] != tc.facts.State || record.Context["step"] != string(tc.facts.Step) || record.Context["invariant_violation"] == nil {
				t.Fatalf("record context = %v, want the state, the step and the violated invariant", record.Context)
			}
			if got := h.footer.last(); got.State != tc.facts.State {
				t.Fatalf("footer facts = %+v, want the published %q", got, tc.facts.State)
			}
		})
	}
}

func TestAMergeWithItsQueueInstantOrNoneStandingIsNotRecorded(t *testing.T) {
	tests := []struct {
		name  string
		facts MergeFacts
	}{
		{name: "queued with its instant", facts: MergeFacts{State: StateQueued, QueuedAt: time.UnixMilli(1_000), Step: footer.StepEnqueued}},
		{name: "merging with its instant", facts: MergeFacts{State: StateMerging, QueuedAt: time.UnixMilli(1_000), Step: footer.StepTesting}},
		{name: "failed", facts: MergeFacts{State: StateFailed, FailedArea: footer.FailedTests}},
		{name: "merged", facts: MergeFacts{State: StateMerged}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			h.o.publish(h.repoKey(), theWorkspace, tc.facts)

			// Assert.
			if record, ok := h.recordFor("error", "daemon.merge.publish"); ok {
				t.Fatalf("recorded %v for a merge that keeps the invariant", record)
			}
		})
	}
}
