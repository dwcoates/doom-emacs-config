package reload

import (
	"context"
	"slices"
	"strings"
	"testing"
)

func TestLandedRangeCoversTheJustPickedCommits(t *testing.T) {
	// Arrange — one branch's single commit picked onto main.
	dir := mergedFixture(t, "feature", map[string]string{
		"modules/app/agent-repl/daemon/internal/reload/reload.go": "package reload\n",
	})

	// Act.
	got, err := landedRange(context.Background(), dir, "feature")

	// Assert.
	if err != nil {
		t.Fatalf("landedRange: %v", err)
	}
	if got.Count != 1 {
		t.Fatalf("landedRange Count = %d, want 1", got.Count)
	}
	head := gitFixture(t, dir, "rev-parse", "HEAD")
	base := gitFixture(t, dir, "rev-parse", "HEAD~1")
	if got.Spec != base+".."+head {
		t.Fatalf("landedRange Spec = %s, want %s..%s", got.Spec, base, head)
	}
}

func TestLandedRangeSpansEveryCommitOfAMultiCommitMerge(t *testing.T) {
	// Arrange — two commits on the branch, both picked.
	dir := newFixtureRepo(t)
	gitFixture(t, dir, "checkout", "-b", "feature", "main")
	writeFixtureFile(t, dir, "modules/app/agent-repl/lisp/status.el", ";; one\n")
	gitFixture(t, dir, "add", "-A")
	gitFixture(t, dir, "commit", "-m", "first")
	writeFixtureFile(t, dir, "modules/app/agent-repl/lisp/panels.el", ";; two\n")
	gitFixture(t, dir, "add", "-A")
	gitFixture(t, dir, "commit", "-m", "second")
	gitFixture(t, dir, "checkout", "main")
	cherryPickBranch(t, dir, "feature")

	// Act.
	got, err := landedRange(context.Background(), dir, "feature")

	// Assert.
	if err != nil {
		t.Fatalf("landedRange: %v", err)
	}
	if got.Count != 2 {
		t.Fatalf("landedRange Count = %d, want 2", got.Count)
	}
}

// The target's history is FULL of cherry-pick annotations, one set per previous
// merge. The walk must stop at the first commit that is not a pick of THIS
// source branch.
func TestLandedRangeStopsAtAnEarlierMergesPicks(t *testing.T) {
	// Arrange — an earlier branch merged first, then a second one.
	dir := newFixtureRepo(t)
	commitOnBranch(t, dir, "earlier", "earlier work", map[string]string{
		"modules/app/agent-repl/lisp/status.el": ";; earlier\n",
	})
	cherryPickBranch(t, dir, "earlier")
	commitOnBranch(t, dir, "later", "later work", map[string]string{
		"modules/app/agent-repl/lisp/panels.el": ";; later\n",
	})
	cherryPickBranch(t, dir, "later")

	// Act.
	got, err := landedRange(context.Background(), dir, "later")

	// Assert.
	if err != nil {
		t.Fatalf("landedRange: %v", err)
	}
	if got.Count != 1 {
		t.Fatalf("landedRange Count = %d, want only the later branch's single commit", got.Count)
	}
}

// A merge whose commits were already incorporated adds nothing, so there is
// nothing to redeploy.
func TestLandedRangeReportsNothingWhenNoCommitLanded(t *testing.T) {
	// Arrange — a branch that exists but was never picked onto main.
	dir := newFixtureRepo(t)
	commitOnBranch(t, dir, "feature", "unpicked work", map[string]string{
		"modules/app/agent-repl/lisp/status.el": ";; unpicked\n",
	})

	// Act.
	got, err := landedRange(context.Background(), dir, "feature")

	// Assert.
	if err != nil {
		t.Fatalf("landedRange: %v", err)
	}
	if got.Count != 0 {
		t.Fatalf("landedRange Count = %d, want 0", got.Count)
	}
	if got.Spec != "" {
		t.Fatalf("landedRange Spec = %q, want empty", got.Spec)
	}
}

func TestLandedRangeRefusesAnEmptySourceBranch(t *testing.T) {
	// Arrange.
	dir := newFixtureRepo(t)

	// Act.
	_, err := landedRange(context.Background(), dir, "")

	// Assert.
	if err == nil {
		t.Fatalf("landedRange() error = nil, want a refusal")
	}
}

func TestLandedRangeSurfacesAnUnreadableTarget(t *testing.T) {
	// Arrange — a directory that is no repository.
	dir := t.TempDir()

	// Act.
	_, err := landedRange(context.Background(), dir, "feature")

	// Assert.
	if err == nil {
		t.Fatalf("landedRange() error = nil, want the git failure surfaced")
	}
	if !strings.Contains(err.Error(), "read target history") {
		t.Fatalf("landedRange() error = %v, want it to name the failed step", err)
	}
}

func TestChangedPathsListsTheRangesFiles(t *testing.T) {
	// Arrange.
	dir := mergedFixture(t, "feature", map[string]string{
		"modules/app/agent-repl/lisp/panels.el":            ";; panels\n",
		"modules/app/agent-repl/daemon/internal/x.go": "package x\n",
	})
	land, err := landedRange(context.Background(), dir, "feature")
	if err != nil {
		t.Fatalf("landedRange: %v", err)
	}

	// Act.
	got, err := changedPaths(context.Background(), dir, land.Spec)

	// Assert.
	if err != nil {
		t.Fatalf("changedPaths: %v", err)
	}
	want := []string{
		"modules/app/agent-repl/daemon/internal/x.go",
		"modules/app/agent-repl/lisp/panels.el",
	}
	slices.Sort(got)
	if !slices.Equal(got, want) {
		t.Fatalf("changedPaths = %v, want %v", got, want)
	}
}

func TestParseLogRecordsRefusesARecordWithoutItsFieldSeparator(t *testing.T) {
	// Arrange — output whose field separator was lost.
	out := "deadbeef no separator here\x1e"

	// Act.
	_, _, err := parseLogRecords(out)

	// Assert.
	if err == nil {
		t.Fatalf("parseLogRecords() error = nil, want a refusal")
	}
}
