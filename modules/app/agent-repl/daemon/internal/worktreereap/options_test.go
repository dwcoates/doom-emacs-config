package worktreereap

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/gitclient"
	"agent-repld/internal/recordlock"
	"agent-repld/internal/wsm"
)

// sweepWith runs one sweep shaped by OPTS, failing on an error.
func (w *world) sweepWith(opts Options) Report {
	w.t.Helper()
	report, err := w.reaper().SweepWith(context.Background(), opts)
	if err != nil {
		w.t.Fatalf("SweepWith(%+v): %v", opts, err)
	}
	return report
}

// unlanded is merge-tree's answer for a tree whose changes have not landed.
var unlanded = &gitclient.MergeTreeOutcome{Tree: "7000000000000000000000000000000000000099"}

// actingWorld is a repository holding one tree of every kind a sweep acts on:
// a landed tree, an expired unlanded tree and a stale registration.
func actingWorld(t *testing.T) (*world, map[string]string) {
	t.Helper()
	w := newWorld(t)
	repo := w.addRepo("repo")
	dirs := map[string]string{
		"landed":  w.addTree(repo, tree{name: "landed"}),
		"expired": w.addTree(repo, tree{name: "expired", outcome: unlanded, born: now.Add(-30 * 24 * time.Hour)}),
		"stale":   w.addTree(repo, tree{name: "stale", prunable: true, missing: true}),
	}
	w.activeEvery(repo, now.Add(-30*24*time.Hour), now, time.Hour)
	return w, dirs
}

// acted reports every git call that changes anything.
func acted(calls []string) []string {
	var out []string
	for _, c := range calls {
		for _, verb := range []string{"remove ", "force_remove ", "preserve ", "delete_branch ", "prune "} {
			if strings.HasPrefix(c, verb) {
				out = append(out, c)
			}
		}
	}
	return out
}

func TestADryRunChangesNothing(t *testing.T) {
	// Arrange.
	w, _ := actingWorld(t)

	// Act.
	w.sweepWith(Options{DryRun: true})

	// Assert.
	if got := acted(w.git.called()); len(got) != 0 {
		t.Fatalf("a dry run made the git calls %v, want none that change anything", got)
	}
}

func TestADryRunAnswersWhatASweepWouldDo(t *testing.T) {
	// Arrange.
	dry, _ := actingWorld(t)
	real, _ := actingWorld(t)

	// Act.
	would := dry.sweepWith(Options{DryRun: true})
	did := real.sweep()

	// Assert.
	if !would.DryRun || did.DryRun {
		t.Fatalf("dry_run = %v and %v, want true for the dry run only", would.DryRun, did.DryRun)
	}
	trim := func(dirs []string, root string) []string {
		out := make([]string, 0, len(dirs))
		for _, d := range dirs {
			out = append(out, strings.TrimPrefix(d, root))
		}
		return out
	}
	if !slices.Equal(trim(would.Removed, dry.root), trim(did.Removed, real.root)) ||
		!slices.Equal(trim(would.Expired, dry.root), trim(did.Expired, real.root)) ||
		would.Pruned != did.Pruned {
		t.Fatalf("dry run %+v, sweep %+v: want the same removals, expiries and prunes", would, did)
	}
}

// forcedTree is one closed or open workspace's tree, recently active, landed
// or not, in a fresh world.
func forcedTree(t *testing.T, closed, landed, registered bool) (*world, string) {
	t.Helper()
	w := newWorld(t)
	repo := w.addRepo("repo")
	spec := tree{name: "subject", touched: now.Add(-time.Hour), born: now.Add(-time.Hour), committed: now.Add(-time.Hour)}
	if !landed {
		spec.outcome = unlanded
	}
	dir := w.addTree(repo, spec)
	if registered {
		w.register(repo, dir, wsm.Workspace{Closed: closed})
	}
	return w, dir
}

func TestAForcedSweepTakesAClosedWorkspacesTreeWithoutWaiting(t *testing.T) {
	tests := []struct {
		name   string
		landed bool
		taken  func(Report) []string
		call   string
	}{
		{name: "a landed tree is removed", landed: true, taken: func(r Report) []string { return r.Removed }, call: "remove "},
		{name: "an unlanded tree is preserved and removed", taken: func(r Report) []string { return r.Forced }, call: "preserve "},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: active an hour ago, so no window has run out.
			w, dir := forcedTree(t, true, tt.landed, true)

			// Act.
			report := w.sweepWith(Options{Force: true})

			// Assert.
			if !slices.Equal(tt.taken(report), []string{dir}) {
				t.Fatalf("report %+v, want %s taken", report, dir)
			}
			if !slices.ContainsFunc(w.git.called(), func(c string) bool { return strings.HasPrefix(c, tt.call+dir) }) {
				t.Fatalf("git calls %v, want %s%s", w.git.called(), tt.call, dir)
			}
		})
	}
}

func TestAnUnforcedSweepWaitsOutAClosedWorkspacesWindows(t *testing.T) {
	// Arrange.
	w, dir := forcedTree(t, true, false, true)

	// Act.
	report := w.sweepWith(Options{})

	// Assert.
	if len(report.Removed)+len(report.Expired)+len(report.Forced) != 0 || w.keptFor(dir) != keepActive {
		t.Fatalf("report %+v kept %q, want the tree kept as recently active", report, w.keptFor(dir))
	}
}

func TestAForcedSweepLeavesEveryOtherTreeAlone(t *testing.T) {
	tests := []struct {
		name       string
		closed     bool
		registered bool
		wantKept   string
	}{
		{name: "an open workspace's tree", registered: true, wantKept: keepOpenWorkspace},
		{name: "a tree no workspace names", wantKept: keepActive},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			w, dir := forcedTree(t, tt.closed, false, tt.registered)

			// Act.
			report := w.sweepWith(Options{Force: true})

			// Assert.
			if len(report.Removed)+len(report.Expired)+len(report.Forced) != 0 || w.keptFor(dir) != tt.wantKept {
				t.Fatalf("report %+v kept %q, want the tree kept as %s", report, w.keptFor(dir), tt.wantKept)
			}
		})
	}
}

func TestAForcedDryRunTakesNothing(t *testing.T) {
	// Arrange.
	w, dir := forcedTree(t, true, false, true)

	// Act.
	report := w.sweepWith(Options{Force: true, DryRun: true})

	// Assert.
	if !slices.Equal(report.Forced, []string{dir}) || len(acted(w.git.called())) != 0 {
		t.Fatalf("report %+v, git %v, want the tree listed and nothing done", report, w.git.called())
	}
}

func TestSweepWithAnswersHeldElsewhereWhileAnotherProcessSweeps(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	w.addRepo("repo")
	held, ok, err := recordlock.Hold(w.lockPath)
	if err != nil || !ok {
		t.Fatalf("taking the lock = (%v, %v)", ok, err)
	}
	t.Cleanup(func() { held.Release() })

	// Act.
	_, err = w.reaper().SweepWith(context.Background(), Options{})

	// Assert.
	if !errors.Is(err, ErrHeldElsewhere) {
		t.Fatalf("SweepWith = %v, want ErrHeldElsewhere", err)
	}
}

// closedOnlyWorld is a repository holding a closed workspace's expired tree
// beside everything a full sweep would also act on: a landed tree no
// workspace names, a stale registration and a landed workspace's branch.
func closedOnlyWorld(t *testing.T) (*world, string, string) {
	t.Helper()
	w, closed, _ := closedTree(t, 4*24*time.Hour, tree{})
	repo := w.registry.repos[0].Dir
	landed := w.addTree(repo, tree{name: "landed"})
	w.addTree(repo, tree{name: "stale", prunable: true, missing: true})
	return w, closed, landed
}

func TestAClosedOnlySweepTakesOnlyClosedWorkspacesTrees(t *testing.T) {
	// Arrange.
	w, closed, landed := closedOnlyWorld(t)

	// Act.
	report := w.sweepWith(Options{ClosedOnly: true})

	// Assert.
	if !slices.Equal(report.Expired, []string{closed}) || len(report.Removed) != 0 || report.Worktrees != 1 {
		t.Fatalf("report %+v, want only the closed workspace's tree judged and expired", report)
	}
	if w.git.saw("remove "+landed) || w.git.saw("admin_dir "+landed) {
		t.Fatalf("git calls %v, want nothing asked about the tree no closed workspace names", w.git.called())
	}
}

func TestAClosedOnlySweepSkipsARepositoryWithNoIdleClosedWorkspace(t *testing.T) {
	tests := []struct {
		name     string
		register func(w *world, repo, dir string)
	}{
		{"no workspace at all", func(*world, string, string) {}},
		{"only an open workspace", func(w *world, repo, dir string) { w.register(repo, dir, wsm.Workspace{}) }},
		{"only a closed workspace touched inside the idle window", func(w *world, repo, dir string) {
			closure := wsm.Closure{At: now.Add(-time.Hour), By: wsm.CloseByUser}
			w.register(repo, dir, wsm.Workspace{Closed: true, Closure: &closure})
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			w := newWorld(t)
			repo := w.addRepo("repo")
			dir := w.addTree(repo, tree{name: "tree"})
			tt.register(w, repo, dir)

			// Act.
			report := w.sweepWith(Options{ClosedOnly: true})

			// Assert.
			if len(w.git.called()) != 0 || report.Repositories != 0 {
				t.Fatalf("git calls %v report %+v, want git asked nothing about the repository", w.git.called(), report)
			}
		})
	}
}

func TestAClosedOnlySweepHasItsOwnRecords(t *testing.T) {
	// Arrange.
	w, _, _ := closedOnlyWorld(t)

	// Act.
	w.sweepWith(Options{ClosedOnly: true})

	// Assert: a reader counting the schedule's sweeps counts none here.
	var finished bool
	for _, r := range w.records("info", opSweep) {
		finished = finished || r.Message == "the closed-workspace sweep finished"
	}
	if !finished || sweepsFinished(w) != 0 {
		t.Fatalf("sweep records %v, want the closed-workspace sweep's own summary and no full sweep's", w.records("info", opSweep))
	}
}
