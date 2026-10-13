package wsm

import (
	"context"
	"testing"
)

func TestTheMigrationStampsEveryHiddenRowWithAHideInstant(t *testing.T) {
	// Arrange: a layout-42 file holding one hidden workspace and one shown.
	path := fixtureAt(t, 42)
	execRaw(t, path, `INSERT INTO workspaces (id, repo_id, dir, name, branch, parent_branch, closed, attention, is_current, created_at, hidden)
		VALUES ('ws-hidden', 'repo-1', '/tmp/ws-hidden', 'hidden', 'hidden', 'master', 0, 0, 0, 1, 1)`)
	execRaw(t, path, `INSERT INTO workspaces (id, repo_id, dir, name, branch, parent_branch, closed, attention, is_current, created_at, hidden)
		VALUES ('ws-shown', 'repo-1', '/tmp/ws-shown', 'shown', 'shown', 'master', 0, 0, 0, 1, 0)`)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	handle.Close()

	// Assert: hidden and its instant stand together, and nothing is dwelt.
	if n := rawScalar[int](t, path, `SELECT count(*) FROM workspaces WHERE id = 'ws-hidden' AND hidden_at IS NOT NULL`); n != 1 {
		t.Fatal("the hidden row was not stamped with a hide instant")
	}
	if n := rawScalar[int](t, path, `SELECT count(*) FROM workspaces WHERE id = 'ws-shown' AND hidden_at IS NULL`); n != 1 {
		t.Fatal("a shown row was stamped with a hide instant")
	}
	if n := rawScalar[int](t, path, `SELECT count(*) FROM workspaces WHERE dwelt_at IS NOT NULL`); n != 0 {
		t.Fatalf("%d rows carry a dwell no user made", n)
	}
}
