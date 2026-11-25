package wsm

import (
	"context"
	"database/sql"
	"testing"
)

// backlogFixture is a file at the layout before the backlog's columns, holding
// one workspace backlogged with a root, one shown, and a view whose backlog
// section is folded and sized to four lines.
func backlogFixture(t *testing.T) string {
	t.Helper()
	path := fixtureAt(t, stepNamed(t, "workspaces_backlogged")-1)
	execRaw(t, path, `INSERT INTO workspaces (id, repo_id, dir, name, branch, parent_branch, closed, attention, is_current, created_at, hidden, hidden_at, hidden_with)
		VALUES ('ws-backlogged', 'repo-1', '/tmp/ws-backlogged', 'backlogged', 'backlogged', 'master', 0, 0, 0, 1, 1, 7, 'ws-root')`)
	execRaw(t, path, `INSERT INTO workspaces (id, repo_id, dir, name, branch, parent_branch, closed, attention, is_current, created_at)
		VALUES ('ws-shown', 'repo-1', '/tmp/ws-shown', 'shown', 'shown', 'master', 0, 0, 0, 1)`)
	execRaw(t, path, `INSERT OR IGNORE INTO sidebar_view (id, merged_folded) VALUES (1, 0)`)
	execRaw(t, path, `UPDATE sidebar_view SET hidden_folded = 1, hidden_lines = 4 WHERE id = 1`)
	return path
}

func TestTheBacklogMigrationCarriesEveryHiddenValue(t *testing.T) {
	tests := []struct {
		name  string
		query string
		want  string
	}{
		{name: "a backlogged row stays backlogged", query: `SELECT backlogged FROM workspaces WHERE id = 'ws-backlogged'`, want: "1"},
		{name: "its instant is kept", query: `SELECT backlogged_at FROM workspaces WHERE id = 'ws-backlogged'`, want: "7"},
		{name: "its root is kept", query: `SELECT backlogged_with FROM workspaces WHERE id = 'ws-backlogged'`, want: "ws-root"},
		{name: "a shown row stays shown", query: `SELECT backlogged FROM workspaces WHERE id = 'ws-shown'`, want: "0"},
		{name: "a shown row has no instant", query: `SELECT coalesce(backlogged_at, 'null') FROM workspaces WHERE id = 'ws-shown'`, want: "null"},
		{name: "the section's fold is kept", query: `SELECT backlog_section_folded FROM sidebar_view WHERE id = 1`, want: "1"},
		{name: "the section's height is kept", query: `SELECT backlog_section_lines FROM sidebar_view WHERE id = 1`, want: "4"},
		{name: "the index follows the column", query: `SELECT count(*) FROM sqlite_master WHERE type = 'index' AND name = 'workspaces_by_backlogged_with'`, want: "1"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := backlogFixture(t)

			// Act
			handle, err := Open(context.Background(), path, WithUnsyncedWrites())
			if err != nil {
				t.Fatalf("Open: %v", err)
			}
			handle.Close()

			// Assert
			if got := rawScalar[string](t, path, tt.query); got != tt.want {
				t.Fatalf("%s = %q, want %q", tt.query, got, tt.want)
			}
		})
	}
}

func TestTheRetiredHiddenColumnsAreGone(t *testing.T) {
	tests := []struct {
		name, table, column string
	}{
		{name: "workspaces.hidden", table: "workspaces", column: "hidden"},
		{name: "workspaces.hidden_at", table: "workspaces", column: "hidden_at"},
		{name: "workspaces.hidden_with", table: "workspaces", column: "hidden_with"},
		{name: "sidebar_view.hidden_folded", table: "sidebar_view", column: "hidden_folded"},
		{name: "sidebar_view.hidden_lines", table: "sidebar_view", column: "hidden_lines"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := backlogFixture(t)

			// Act
			handle, err := Open(context.Background(), path, WithUnsyncedWrites())
			if err != nil {
				t.Fatalf("Open: %v", err)
			}
			handle.Close()

			// Assert
			if got := rawScalar[int](t, path, `SELECT count(*) FROM pragma_table_info(?) WHERE name = ?`, tt.table, tt.column); got != 0 {
				t.Fatalf("%s.%s exists %d times after the contract, want 0", tt.table, tt.column, got)
			}
		})
	}
}

func TestTheRetiredHiddenShapesLeaveNoIndexOrTrigger(t *testing.T) {
	// Arrange
	path := backlogFixture(t)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	handle.Close()

	// Assert
	if got := rawScalar[int](t, path, `SELECT count(*) FROM sqlite_master WHERE (type = 'index' AND name = 'workspaces_by_hidden_with') OR (type = 'trigger' AND (name LIKE '%hidden%' OR name LIKE '%backlog%'))`); got != 0 {
		t.Fatalf("%d hidden index or mirroring triggers outlived the contract, want 0", got)
	}
}

// THE TRIGGERS KEEP A HANDOVER'S TWO BUILDS ON ONE BACKLOG: between layout
// 210 and its contract, the incumbent writes the hidden columns and this
// build the backlog's, and each write reaches the other pair.
func TestTheBacklogMirrorsKeepBothPairsEqualWhileBothBuildsServe(t *testing.T) {
	tests := []struct {
		name  string
		write string
		read  string
		want  string
	}{
		{name: "a hide the incumbent writes is backlogged",
			write: `UPDATE workspaces SET hidden = 1, hidden_at = 9, hidden_with = 'ws-root' WHERE id = 'ws-shown'`,
			read:  `SELECT backlogged || ',' || backlogged_at || ',' || backlogged_with FROM workspaces WHERE id = 'ws-shown'`, want: "1,9,ws-root"},
		{name: "a show the incumbent writes leaves the backlog",
			write: `UPDATE workspaces SET hidden = 0, hidden_at = NULL, hidden_with = NULL WHERE id = 'ws-backlogged'`,
			read:  `SELECT backlogged || ',' || coalesce(backlogged_at, 'null') || ',' || coalesce(backlogged_with, 'null') FROM workspaces WHERE id = 'ws-backlogged'`, want: "0,null,null"},
		{name: "a backlog this build writes is hidden for the incumbent",
			write: `UPDATE workspaces SET backlogged = 1, backlogged_at = 9, backlogged_with = NULL WHERE id = 'ws-shown'`,
			read:  `SELECT hidden || ',' || hidden_at || ',' || coalesce(hidden_with, 'null') FROM workspaces WHERE id = 'ws-shown'`, want: "1,9,null"},
		{name: "a row the incumbent inserts hidden is backlogged",
			write: `INSERT INTO workspaces (id, repo_id, dir, name, branch, parent_branch, closed, attention, is_current, created_at, hidden, hidden_at)
				VALUES ('ws-new', 'repo-1', '/tmp/ws-new', 'new', 'new', 'master', 0, 0, 0, 1, 1, 3)`,
			read: `SELECT backlogged || ',' || backlogged_at FROM workspaces WHERE id = 'ws-new'`, want: "1,3"},
		{name: "a row this build inserts backlogged is hidden for the incumbent",
			write: `INSERT INTO workspaces (id, repo_id, dir, name, branch, parent_branch, closed, attention, is_current, created_at, backlogged, backlogged_at)
				VALUES ('ws-new', 'repo-1', '/tmp/ws-new', 'new', 'new', 'master', 0, 0, 0, 1, 1, 3)`,
			read: `SELECT hidden || ',' || hidden_at FROM workspaces WHERE id = 'ws-new'`, want: "1,3"},
		{name: "a fold the incumbent writes folds the backlog",
			write: `UPDATE sidebar_view SET hidden_folded = 0, hidden_lines = 2 WHERE id = 1`,
			read:  `SELECT backlog_section_folded || ',' || backlog_section_lines FROM sidebar_view WHERE id = 1`, want: "0,2"},
		{name: "a fold this build writes folds the incumbent's section",
			write: `UPDATE sidebar_view SET backlog_section_folded = 0, backlog_section_lines = NULL WHERE id = 1`,
			read:  `SELECT hidden_folded || ',' || coalesce(hidden_lines, 'null') FROM sidebar_view WHERE id = 1`, want: "0,null"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the file as a joining successor leaves it, the additive
			// step applied and its contract still owed.
			path := backlogFixture(t)
			for _, m := range migrations {
				if m.To == stepNamed(t, "workspaces_backlogged") {
					execRaw(t, path, m.DDL)
				}
			}

			// Act
			execRaw(t, path, tt.write)

			// Assert
			if got := rawScalar[sql.NullString](t, path, tt.read); got.String != tt.want {
				t.Fatalf("%s = %q, want %q", tt.read, got.String, tt.want)
			}
		})
	}
}
