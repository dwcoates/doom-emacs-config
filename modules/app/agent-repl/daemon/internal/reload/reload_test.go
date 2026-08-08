package reload

import (
	"context"
	"fmt"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"testing"

	"claude-repld/internal/workspace/merge"
)

// fakeLauncher records what it was asked to deploy and never spawns anything.
type fakeLauncher struct {
	mu    sync.Mutex
	plans []Plan
	err   error
}

func (f *fakeLauncher) Launch(_ context.Context, plan Plan) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.plans = append(f.plans, plan)
	return f.err
}

func (f *fakeLauncher) calls() []Plan {
	f.mu.Lock()
	defer f.mu.Unlock()
	return slices.Clone(f.plans)
}

func newTestTrigger(t *testing.T, self SelfRepo, launcher Launcher) *Trigger {
	t.Helper()
	trigger, err := New(Config{Self: self, Launcher: launcher, Logf: t.Logf})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	return trigger
}

// selfMergeRequest is the merge.Request a completed merge into dir looks like.
func selfMergeRequest(dir, branch string) merge.Request {
	return merge.Request{
		Workspace:    filepath.Join(dir, "..", "ws-"+branch),
		Name:         branch,
		SourceBranch: branch,
		SourceDir:    filepath.Join(dir, "..", "ws-"+branch),
		TargetDir:    dir,
	}
}

func TestAfterMergedLaunchesARedeployForItsOwnCheckout(t *testing.T) {
	// Arrange — a merge that landed daemon and elisp changes in the daemon's
	// own checkout.
	dir := mergedFixture(t, "feature", map[string]string{
		"modules/app/agent-repl/daemon/internal/reload/reload.go": "package reload\n",
		"modules/app/agent-repl/lisp/panels.el":                        ";; panels\n",
	})
	launcher := &fakeLauncher{}
	trigger := newTestTrigger(t, fixtureSelf(t, dir), launcher)

	// Act.
	err := trigger.AfterMerged(context.Background(), selfMergeRequest(dir, "feature"))

	// Assert.
	if err != nil {
		t.Fatalf("AfterMerged: %v", err)
	}
	calls := launcher.calls()
	if len(calls) != 1 {
		t.Fatalf("launcher calls = %d, want 1", len(calls))
	}
	if !slices.Equal(calls[0].Components, []Component{ComponentDaemon, ComponentElisp}) {
		t.Fatalf("launched components = %v, want [daemon elisp]", calls[0].Components)
	}
	if !strings.Contains(calls[0].Range, "..") {
		t.Fatalf("launched range = %q, want a base..head spec", calls[0].Range)
	}
}

// The overwhelmingly common case: a merge of some entirely different
// repository. It must not come anywhere near the deploy script.
func TestAfterMergedNeverTouchesTheScriptForAnotherRepository(t *testing.T) {
	// Arrange — the merge target is a repository unrelated to the daemon's own.
	other := mergedFixture(t, "feature", map[string]string{
		"modules/app/agent-repl/daemon/internal/reload/reload.go": "package reload\n",
	})
	mine := newFixtureRepo(t)
	launcher := &fakeLauncher{}
	trigger := newTestTrigger(t, fixtureSelf(t, mine), launcher)

	// Act.
	err := trigger.AfterMerged(context.Background(), selfMergeRequest(other, "feature"))

	// Assert.
	if err != nil {
		t.Fatalf("AfterMerged: %v", err)
	}
	if calls := launcher.calls(); len(calls) != 0 {
		t.Fatalf("launcher calls = %v, want none for another repository's merge", calls)
	}
}

// A child workspace merging into its PARENT workspace's worktree is the same
// repository but not the code this daemon was built from, so rebuilding the
// daemon's own checkout would deploy a tree without the merged commits.
func TestAfterMergedSkipsASiblingWorktreeOfItsOwnRepository(t *testing.T) {
	// Arrange.
	main := newFixtureRepo(t)
	parent := filepath.Join(t.TempDir(), "parent")
	gitFixture(t, main, "worktree", "add", "-b", "parent", parent, "main")
	launcher := &fakeLauncher{}
	trigger := newTestTrigger(t, fixtureSelf(t, main), launcher)

	// Act.
	err := trigger.AfterMerged(context.Background(), selfMergeRequest(parent, "parent"))

	// Assert.
	if err != nil {
		t.Fatalf("AfterMerged: %v", err)
	}
	if calls := launcher.calls(); len(calls) != 0 {
		t.Fatalf("launcher calls = %v, want none for a sibling worktree", calls)
	}
}

func TestAfterMergedSkipsAMergeThatTouchedNothingTheStackExecutes(t *testing.T) {
	// Arrange — a self-merge whose commits landed outside the stack.
	dir := mergedFixture(t, "feature", map[string]string{
		"modules/lang/personal-org/config.el": ";; unrelated\n",
	})
	launcher := &fakeLauncher{}
	trigger := newTestTrigger(t, fixtureSelf(t, dir), launcher)

	// Act.
	err := trigger.AfterMerged(context.Background(), selfMergeRequest(dir, "feature"))

	// Assert.
	if err != nil {
		t.Fatalf("AfterMerged: %v", err)
	}
	if calls := launcher.calls(); len(calls) != 0 {
		t.Fatalf("launcher calls = %v, want none for an out-of-stack merge", calls)
	}
}

func TestAfterMergedSkipsAMergeThatAddedNoCommits(t *testing.T) {
	// Arrange — the branch was never picked onto main, which is what an
	// already-incorporated merge leaves behind.
	dir := newFixtureRepo(t)
	commitOnBranch(t, dir, "feature", "unpicked", map[string]string{
		"modules/app/agent-repl/lisp/panels.el": ";; unpicked\n",
	})
	launcher := &fakeLauncher{}
	trigger := newTestTrigger(t, fixtureSelf(t, dir), launcher)

	// Act.
	err := trigger.AfterMerged(context.Background(), selfMergeRequest(dir, "feature"))

	// Assert.
	if err != nil {
		t.Fatalf("AfterMerged: %v", err)
	}
	if calls := launcher.calls(); len(calls) != 0 {
		t.Fatalf("launcher calls = %v, want none when nothing landed", calls)
	}
}

// Belt-and-braces against a reload loop: a redeploy already launched in this
// daemon's lifetime is the live one, and a second must not stack on it.
func TestAfterMergedLatchesOutASecondRedeploy(t *testing.T) {
	// Arrange — one self-merge, delivered twice.
	dir := mergedFixture(t, "feature", map[string]string{
		"modules/app/agent-repl/daemon/internal/reload/reload.go": "package reload\n",
	})
	launcher := &fakeLauncher{}
	trigger := newTestTrigger(t, fixtureSelf(t, dir), launcher)
	req := selfMergeRequest(dir, "feature")
	if err := trigger.AfterMerged(context.Background(), req); err != nil {
		t.Fatalf("first AfterMerged: %v", err)
	}

	// Act.
	err := trigger.AfterMerged(context.Background(), req)

	// Assert.
	if err != nil {
		t.Fatalf("second AfterMerged: %v", err)
	}
	if calls := launcher.calls(); len(calls) != 1 {
		t.Fatalf("launcher calls = %d, want the second redeploy latched out", len(calls))
	}
}

// A launch that never happened must not hold the latch against the next merge.
func TestAfterMergedReleasesTheLatchWhenTheLaunchFailed(t *testing.T) {
	// Arrange.
	dir := mergedFixture(t, "feature", map[string]string{
		"modules/app/agent-repl/daemon/internal/reload/reload.go": "package reload\n",
	})
	launcher := &fakeLauncher{err: fmt.Errorf("spawn refused")}
	trigger := newTestTrigger(t, fixtureSelf(t, dir), launcher)
	req := selfMergeRequest(dir, "feature")
	if err := trigger.AfterMerged(context.Background(), req); err == nil {
		t.Fatalf("first AfterMerged() error = nil, want the launch failure surfaced")
	}

	// Act.
	err := trigger.AfterMerged(context.Background(), req)

	// Assert.
	if err == nil {
		t.Fatalf("second AfterMerged() error = nil, want the launch attempted again")
	}
	if calls := launcher.calls(); len(calls) != 2 {
		t.Fatalf("launcher calls = %d, want the failed launch to have released the latch", len(calls))
	}
}

func TestAfterMergedSurfacesALaunchFailure(t *testing.T) {
	// Arrange.
	dir := mergedFixture(t, "feature", map[string]string{
		"modules/app/agent-repl/daemon/internal/reload/reload.go": "package reload\n",
	})
	trigger := newTestTrigger(t, fixtureSelf(t, dir), &fakeLauncher{err: fmt.Errorf("spawn refused")})

	// Act.
	err := trigger.AfterMerged(context.Background(), selfMergeRequest(dir, "feature"))

	// Assert.
	if err == nil {
		t.Fatalf("AfterMerged() error = nil, want the launch failure surfaced")
	}
	if !strings.Contains(err.Error(), "spawn refused") {
		t.Fatalf("AfterMerged() error = %v, want the launcher's cause preserved", err)
	}
}

func TestAfterMergedSurfacesAnUnidentifiableTarget(t *testing.T) {
	// Arrange — a target directory that is no repository at all.
	mine := newFixtureRepo(t)
	launcher := &fakeLauncher{}
	trigger := newTestTrigger(t, fixtureSelf(t, mine), launcher)

	// Act.
	err := trigger.AfterMerged(context.Background(), selfMergeRequest(t.TempDir(), "feature"))

	// Assert.
	if err == nil {
		t.Fatalf("AfterMerged() error = nil, want the identity failure surfaced")
	}
	if calls := launcher.calls(); len(calls) != 0 {
		t.Fatalf("launcher calls = %v, want none when the target could not be identified", calls)
	}
}

func TestNewRefusesAMissingDependency(t *testing.T) {
	self := SelfRepo{Root: "/repo", CommonDir: "/repo/.git"}
	tests := []struct {
		name string
		cfg  Config
	}{
		{"no self root", Config{Self: SelfRepo{CommonDir: "/repo/.git"}, Launcher: &fakeLauncher{}, Logf: t.Logf}},
		{"no self common dir", Config{Self: SelfRepo{Root: "/repo"}, Launcher: &fakeLauncher{}, Logf: t.Logf}},
		{"no launcher", Config{Self: self, Logf: t.Logf}},
		{"no logf", Config{Self: self, Launcher: &fakeLauncher{}}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			trigger, err := New(tc.cfg)

			// Assert.
			if err == nil {
				t.Fatalf("New(%+v) error = nil, want a refusal", tc.cfg)
			}
			if trigger != nil {
				t.Fatalf("New returned %v alongside its refusal", trigger)
			}
		})
	}
}
