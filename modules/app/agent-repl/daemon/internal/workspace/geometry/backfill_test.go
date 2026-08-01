package geometry

import (
	"context"
	"path/filepath"
	"strings"
	"testing"
)

type fakeLister struct{ workspaces []string }

func (f fakeLister) GeometryBackfillCandidates() []string { return f.workspaces }

func newBackfiller(t *testing.T, store *Store, logs *[]string, workspaces ...string) *Backfiller {
	t.Helper()
	logf := func(format string, args ...any) { *logs = append(*logs, format) }
	deriver, err := NewDeriver(logf)
	if err != nil {
		t.Fatal(err)
	}
	b, err := NewBackfiller(BackfillConfig{Store: store, Deriver: deriver, Lister: fakeLister{workspaces: workspaces}, Logf: logf})
	if err != nil {
		t.Fatal(err)
	}
	return b
}

func TestBackfillRecordsAPreCutoverWorkspace(t *testing.T) {
	// Arrange — a linked worktree the daemon never recorded geometry for.
	store, _, _ := openStore(t)
	main := newRepo(t)
	linked := filepath.Join(filepath.Dir(main), "legacy")
	runGit(t, main, "worktree", "add", "-b", "DWC/legacy", linked)
	var logs []string
	b := newBackfiller(t, store, &logs, linked)

	// Act.
	report, err := b.Run(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if report.Recorded != 1 || report.Underivable != 0 || report.AlreadyRecorded != 0 {
		t.Fatalf("report = %#v", report)
	}
	got, found, err := store.Lookup(context.Background(), linked)
	if err != nil || !found {
		t.Fatalf("Lookup found=%t err=%v", found, err)
	}
	if got.SourceBranch != "DWC/legacy" || got.Origin != OriginBackfilled {
		t.Fatalf("record = %#v", got)
	}
}

func TestBackfillLeavesAnAlreadyRecordedWorkspaceUntouched(t *testing.T) {
	// Arrange — the create path already recorded this workspace.
	store, _, _ := openStore(t)
	main := newRepo(t)
	linked := filepath.Join(filepath.Dir(main), "recorded")
	runGit(t, main, "worktree", "add", "-b", "DWC/recorded", linked)
	recorded := Record{Workspace: linked, SourceBranch: "DWC/recorded", SourceDir: linked, TargetDir: "/some/parent/worktree", Origin: OriginCreated}
	if err := store.Record(context.Background(), recorded); err != nil {
		t.Fatal(err)
	}
	var logs []string
	b := newBackfiller(t, store, &logs, linked)

	// Act.
	report, err := b.Run(context.Background())

	// Assert — the derived answer never displaces the observed one.
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if report.AlreadyRecorded != 1 || report.Recorded != 0 {
		t.Fatalf("report = %#v", report)
	}
	got, _, err := store.Lookup(context.Background(), linked)
	if err != nil {
		t.Fatal(err)
	}
	if got.TargetDir != "/some/parent/worktree" || got.Origin != OriginCreated {
		t.Fatalf("record = %#v, want the untouched observed record", got)
	}
}

func TestBackfillContainsAnUnderivableWorkspaceAndKeepsGoing(t *testing.T) {
	// Arrange — one detached worktree between two derivable ones.
	store, _, _ := openStore(t)
	main := newRepo(t)
	first := filepath.Join(filepath.Dir(main), "first")
	detached := filepath.Join(filepath.Dir(main), "detached")
	last := filepath.Join(filepath.Dir(main), "last")
	runGit(t, main, "worktree", "add", "-b", "DWC/first", first)
	runGit(t, main, "worktree", "add", "--detach", detached)
	runGit(t, main, "worktree", "add", "-b", "DWC/last", last)
	var logs []string
	b := newBackfiller(t, store, &logs, first, detached, last)

	// Act.
	report, err := b.Run(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if report.Recorded != 2 || report.Underivable != 1 {
		t.Fatalf("report = %#v", report)
	}
	if _, found, err := store.Lookup(context.Background(), detached); err != nil || found {
		t.Fatalf("the detached worktree got a record: found=%t err=%v", found, err)
	}
	if !containsFormat(logs, "geometry: backfill UNDERIVABLE") {
		t.Fatalf("the underivable workspace was not logged: %#v", logs)
	}
}

func TestBackfillCountsAnEmptyWorkspaceKeyAsUnderivable(t *testing.T) {
	// Arrange.
	store, _, _ := openStore(t)
	var logs []string
	b := newBackfiller(t, store, &logs, "")

	// Act.
	report, err := b.Run(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if report.Underivable != 1 || report.Recorded != 0 {
		t.Fatalf("report = %#v", report)
	}
}

func TestBackfillAbortsOnAStoreFailure(t *testing.T) {
	// Arrange — a closed database is a structural failure, not a per-workspace
	// one, so the pass must abort rather than march on recording nothing.
	store, db, _ := openStore(t)
	main := newRepo(t)
	linked := filepath.Join(filepath.Dir(main), "structural")
	runGit(t, main, "worktree", "add", "-b", "DWC/structural", linked)
	var logs []string
	b := newBackfiller(t, store, &logs, linked)
	if err := db.Close(); err != nil {
		t.Fatal(err)
	}

	// Act.
	_, err := b.Run(context.Background())

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "backfill lookup") {
		t.Fatalf("Run error = %v, want a structural lookup failure", err)
	}
}

func TestNewBackfillerRefusesMissingCollaborators(t *testing.T) {
	store, _, _ := openStore(t)
	deriver, err := NewDeriver(func(string, ...any) {})
	if err != nil {
		t.Fatal(err)
	}
	logf := func(string, ...any) {}
	tests := []struct {
		name string
		cfg  BackfillConfig
		want string
	}{
		{"no store", BackfillConfig{Deriver: deriver, Lister: fakeLister{}, Logf: logf}, "Store"},
		{"no deriver", BackfillConfig{Store: store, Lister: fakeLister{}, Logf: logf}, "Deriver"},
		{"no lister", BackfillConfig{Store: store, Deriver: deriver, Logf: logf}, "WorkspaceLister"},
		{"no logger", BackfillConfig{Store: store, Deriver: deriver, Lister: fakeLister{}}, "Logf"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act.
			_, err := NewBackfiller(tc.cfg)

			// Assert.
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("NewBackfiller error = %v, want one containing %q", err, tc.want)
			}
		})
	}
}
