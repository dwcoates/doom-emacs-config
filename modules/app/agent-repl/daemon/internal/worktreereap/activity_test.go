package worktreereap

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"claude-repld/internal/gitclient"
	"claude-repld/internal/wsm"
)

// activityOf reads one worktree's last activity out of the world.
func activityOf(t *testing.T, w *world, repo, dir string, record *wsm.Workspace) (activity, error) {
	t.Helper()
	var wt gitclient.Worktree
	for _, candidate := range w.git.worktrees[repo] {
		if candidate.Dir == dir {
			wt = candidate
		}
	}
	return lastActivity(context.Background(), w.git, repo, wt, record)
}

func TestTheNewestSignalIsTheLastActivity(t *testing.T) {
	recent := now.Add(-time.Hour)
	cases := []struct {
		name   string
		touch  string
		record func(*wsm.Workspace)
		commit bool
		want   string
	}{
		{name: "admin HEAD", touch: "HEAD", want: "admin:HEAD"},
		{name: "admin index", touch: "index", want: "admin:index"},
		{name: "admin reflog", touch: filepath.Join("logs", "HEAD"), want: "admin:" + filepath.Join("logs", "HEAD")},
		{name: "head committed", commit: true, want: "head_committed"},
		{name: "workspace created", record: func(ws *wsm.Workspace) { ws.CreatedAt = recent }, want: "workspace_created"},
		{name: "workspace activity", record: func(ws *wsm.Workspace) { ws.LastActivityAt = &recent }, want: "workspace_activity"},
		{name: "workspace selected", record: func(ws *wsm.Workspace) { ws.LastSelectedAt = &recent }, want: "workspace_selected"},
		{name: "workspace merged", record: func(ws *wsm.Workspace) { ws.MergedAt = &recent }, want: "workspace_merged"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: every signal is old except the one under test.
			w := newWorld(t)
			repo := w.addRepo("repo")
			spec := tree{name: "wt"}
			if tc.commit {
				spec.committed = recent
			}
			dir := w.addTree(repo, spec)
			if tc.touch != "" {
				w.writeAged(filepath.Join(w.git.admin[dir], tc.touch), recent)
			}
			record := &wsm.Workspace{CreatedAt: longAgo}
			if tc.record != nil {
				tc.record(record)
			}

			// Act.
			got, err := activityOf(t, w, repo, dir, record)

			// Assert.
			if err != nil || !got.At.Equal(recent) || got.Signal != tc.want {
				t.Fatalf("lastActivity = (%+v, %v), want %s at %v", got, err, tc.want, recent)
			}
		})
	}
}

func TestAnUnregisteredWorktreeIsJudgedByGitAlone(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "wt"})

	// Act.
	got, err := activityOf(t, w, repo, dir, nil)

	// Assert.
	if err != nil || !got.At.Equal(longAgo) {
		t.Fatalf("lastActivity = (%+v, %v), want the git signals' %v", got, err, longAgo)
	}
}

func TestAMissingOptionalAdminFileIsNoSignal(t *testing.T) {
	// Arrange: a fresh tree has no reflog, and reflogs can be turned off.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "wt"})
	if err := os.RemoveAll(filepath.Join(w.git.admin[dir], "logs")); err != nil {
		t.Fatalf("removing the reflog: %v", err)
	}

	// Act.
	_, err := activityOf(t, w, repo, dir, nil)

	// Assert.
	if err != nil {
		t.Fatalf("lastActivity = %v, want no error for a missing reflog", err)
	}
}

func TestAMissingAdminHEADIsAnError(t *testing.T) {
	// Arrange: git always writes HEAD, so its absence means the activity
	// cannot be told.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "wt"})
	if err := os.Remove(filepath.Join(w.git.admin[dir], "HEAD")); err != nil {
		t.Fatalf("removing HEAD: %v", err)
	}

	// Act.
	_, err := activityOf(t, w, repo, dir, nil)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "missing") {
		t.Fatalf("lastActivity = %v, want the missing HEAD refused", err)
	}
}

func TestAnUnreadableAdminFileIsAnError(t *testing.T) {
	// Arrange: the admin dir is a regular file, so every stat under it fails
	// with something other than "not there".
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "wt"})
	notADir := filepath.Join(w.root, "not-a-dir")
	w.writeAged(notADir, longAgo)
	w.git.admin[dir] = notADir

	// Act.
	_, err := activityOf(t, w, repo, dir, nil)

	// Assert.
	if err == nil || strings.Contains(err.Error(), "missing") {
		t.Fatalf("lastActivity = %v, want a read failure rather than absence", err)
	}
}

func TestACommitterTimeFailureIsAnError(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "wt"})
	delete(w.git.committed, w.headOf(repo, dir))

	// Act.
	_, err := activityOf(t, w, repo, dir, nil)

	// Assert.
	if err == nil {
		t.Fatal("lastActivity = nil error, want the committer-time failure")
	}
}
