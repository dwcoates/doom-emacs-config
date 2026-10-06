package wsm

import (
	"context"
	"database/sql"
	"errors"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/dlog"
)

// withCanonicalDir replaces the open's canonicalizer, so a test models a
// case-folding volume whatever volume the test runs on.
func withCanonicalDir(canonical func(string) (string, error)) Option {
	return func(s *store) { s.canonicalDir = canonical }
}

// foldingVolume answers every spelling of kept under case folding as kept,
// and every other directory as itself: the one directory of the fixture that
// a case-insensitive volume stores under a single spelling.
func foldingVolume(kept string) func(string) (string, error) {
	return func(dir string) (string, error) {
		if strings.EqualFold(dir, kept) {
			return kept, nil
		}
		return dir, nil
	}
}

// spellingFixture is a database holding one workspace, ws-kept, registered
// at its directory's on-disk spelling, plus whatever rows arrange adds through
// a raw handle. It answers the database path and that on-disk spelling.
func spellingFixture(t *testing.T, arrange func(db *sql.DB, repo string, kept string)) (string, string) {
	t.Helper()
	path := filepath.Join(t.TempDir(), "wsm.db")
	kept := "/Volume/Users/me/ChessCom/iterm-1"
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	if err := handle.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	withRawDB(t, path, func(db *sql.DB) {
		exec(t, db, `INSERT INTO repositories (id, dir, name, default_branch) VALUES ('repo-1', '/Volume/Users/me/ChessCom', 'ChessCom', 'master')`)
		arrange(db, "repo-1", kept)
	})
	return path, kept
}

// insertWorkspace writes one raw workspace row.
func insertWorkspace(t *testing.T, db *sql.DB, id, repo, dir string, closed bool, createdAt int) {
	t.Helper()
	exec(t, db, `INSERT INTO workspaces (id, repo_id, dir, name, branch, parent_branch, closed, attention, is_current, created_at)
		VALUES (?, ?, ?, 'iterm-1', '', 'master', ?, 0, 0, ?)`, id, repo, dir, closed, createdAt)
}

func exec(t *testing.T, db *sql.DB, query string, args ...any) {
	t.Helper()
	if _, err := db.Exec(query, args...); err != nil {
		t.Fatalf("exec %q: %v", query, err)
	}
}

// openSpelled opens the fixture under the folding volume.
func openSpelled(t *testing.T, path, kept string) (*store, *dlog.TestLogger) {
	t.Helper()
	log := dlog.NewTestLogger()
	handle, err := Open(context.Background(), path, WithUnsyncedWrites(), WithLogger(log), withCanonicalDir(foldingVolume(kept)))
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { handle.Close() })
	return handle.(*store), log
}

const lowered = "/Volume/Users/me/chesscom/iterm-1"

func TestOpenRespellsAClosedSoleRowToItsOnDiskSpelling(t *testing.T) {
	// Arrange
	path, kept := spellingFixture(t, func(db *sql.DB, repo, _ string) {
		insertWorkspace(t, db, "ws-lower", repo, lowered, true, 1)
	})

	// Act
	s, _ := openSpelled(t, path, kept)

	// Assert
	if got := scalar[string](t, s, `SELECT dir FROM workspaces WHERE id = 'ws-lower'`); got != kept {
		t.Fatalf("dir = %q, want %q", got, kept)
	}
}

func TestOpenRecordsARespellAtInfo(t *testing.T) {
	// Arrange
	path, kept := spellingFixture(t, func(db *sql.DB, repo, _ string) {
		insertWorkspace(t, db, "ws-lower", repo, lowered, true, 1)
	})

	// Act
	_, log := openSpelled(t, path, kept)

	// Assert
	if _, ok := findRecord(log, "info", "respelled a closed workspace to its directory's on-disk spelling"); !ok {
		t.Fatalf("no respell record; records = %+v", log.Records())
	}
}

func TestOpenRetiresAClosedCaseDuplicateThatHoldsNothing(t *testing.T) {
	// Arrange
	path, kept := spellingFixture(t, func(db *sql.DB, repo, kept string) {
		insertWorkspace(t, db, "ws-lower", repo, lowered, true, 1)
		insertWorkspace(t, db, "ws-kept", repo, kept, true, 2)
	})

	// Act
	s, _ := openSpelled(t, path, kept)

	// Assert
	if got := scalar[string](t, s, `SELECT group_concat(id) FROM workspaces`); got != "ws-kept" {
		t.Fatalf("workspaces = %q, want only ws-kept", got)
	}
}

func TestOpenRecordsARetirementAtInfoNamingTheKeptWorkspace(t *testing.T) {
	// Arrange
	path, kept := spellingFixture(t, func(db *sql.DB, repo, kept string) {
		insertWorkspace(t, db, "ws-lower", repo, lowered, true, 1)
		insertWorkspace(t, db, "ws-kept", repo, kept, true, 2)
	})

	// Act
	_, log := openSpelled(t, path, kept)

	// Assert
	record, ok := findRecord(log, "info", "retired a closed workspace registered under another spelling of a registered directory; it held nothing")
	if !ok {
		t.Fatalf("no retirement record; records = %+v", log.Records())
	}
	if record.Context["kept_workspace"] != "ws-kept" {
		t.Fatalf("kept_workspace = %v, want ws-kept", record.Context["kept_workspace"])
	}
}

func TestOpenKeepsACaseDuplicateThatHoldsState(t *testing.T) {
	tests := []struct {
		name  string
		held  string
		query string
	}{
		{name: "a turn", held: "turns",
			query: `INSERT INTO turns (id, workspace_id, text, origin, displaced, started_at) VALUES ('t-1', 'ws-lower', 'hi', 'user', 0, 1)`},
		{name: "a prompt still held", held: "held_prompts",
			query: `INSERT INTO held_prompts (turn_id, workspace_id, said, origin, accepted, queued_at) VALUES ('h-1', 'ws-lower', x'00', 'user', 0, 1)`},
		{name: "a live session", held: "live_session",
			query: `INSERT INTO sessions (workspace_id, host_session_id, vendor_session_id, config_dir, model, permission_mode, started_at, last_engagement_at) VALUES ('ws-lower', 'h', 'v', '/c', 'opus', 'auto', 1, 1)`},
		{name: "a lease", held: "leases",
			query: `INSERT INTO leases (id, workspace_id, holder, policy, acquired_at) VALUES ('l-1', 'ws-lower', 1, 1, 1)`},
		{name: "a ported prompt", held: "ported_prompts",
			query: `INSERT INTO ported_prompts (workspace_id, turn_id, ordinal, text, origin, started_at) VALUES ('ws-lower', 't-1', 0, 'hi', 'user', 1)`},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			path, kept := spellingFixture(t, func(db *sql.DB, repo, kept string) {
				insertWorkspace(t, db, "ws-lower", repo, lowered, true, 1)
				insertWorkspace(t, db, "ws-kept", repo, kept, true, 2)
				exec(t, db, tc.query)
			})

			// Act
			s, log := openSpelled(t, path, kept)

			// Assert
			if got := scalar[int](t, s, `SELECT count(*) FROM workspaces WHERE id = 'ws-lower'`); got != 1 {
				t.Fatalf("ws-lower rows = %d, want it kept", got)
			}
			record, ok := findRecord(log, "error", "workspaces are registered under a spelling that is not their directory's on-disk one")
			if !ok {
				t.Fatalf("no violation record; records = %+v", log.Records())
			}
			if rows, _ := record.Context["rows"].(string); !strings.Contains(rows, tc.held) {
				t.Fatalf("rows = %q, want it to name %s", rows, tc.held)
			}
		})
	}
}

func TestOpenRetiresACaseDuplicateWhosePromptsAreAllTombstoned(t *testing.T) {
	// Arrange: the live b58a6f10703e493f's shape — its one prompt was dropped
	// and its session killed.
	path, kept := spellingFixture(t, func(db *sql.DB, repo, kept string) {
		insertWorkspace(t, db, "ws-lower", repo, lowered, true, 1)
		insertWorkspace(t, db, "ws-kept", repo, kept, true, 2)
		exec(t, db, `INSERT INTO held_prompts (turn_id, workspace_id, said, origin, accepted, tombstone_kind, tombstone_at, queued_at) VALUES ('h-1', 'ws-lower', x'00', 'user', 0, 'dropped', 2, 1)`)
		exec(t, db, `INSERT INTO sessions (workspace_id, host_session_id, vendor_session_id, config_dir, model, permission_mode, started_at, last_engagement_at, terminal_kind, terminal_detail, terminal_at) VALUES ('ws-lower', 'h', 'v', '/c', 'opus', 'auto', 1, 1, 'killed', 'KillWorkspace', 2)`)
	})

	// Act
	s, _ := openSpelled(t, path, kept)

	// Assert
	if got := scalar[int](t, s, `SELECT count(*) FROM workspaces WHERE id = 'ws-lower'`); got != 0 {
		t.Fatalf("ws-lower rows = %d, want it retired", got)
	}
}

func TestOpenKeepsAnOpenRowUnderAnotherSpelling(t *testing.T) {
	// Arrange
	path, kept := spellingFixture(t, func(db *sql.DB, repo, _ string) {
		insertWorkspace(t, db, "ws-lower", repo, lowered, false, 1)
	})

	// Act
	s, log := openSpelled(t, path, kept)

	// Assert
	if got := scalar[string](t, s, `SELECT dir FROM workspaces WHERE id = 'ws-lower'`); got != lowered {
		t.Fatalf("dir = %q, want the open row left at %q", got, lowered)
	}
	if _, ok := findRecord(log, "error", "workspaces are registered under a spelling that is not their directory's on-disk one"); !ok {
		t.Fatalf("no violation record; records = %+v", log.Records())
	}
}

func TestOpenReportsARowWhoseDirectoryCannotBeCanonicalized(t *testing.T) {
	// Arrange
	path, _ := spellingFixture(t, func(db *sql.DB, repo, _ string) {
		insertWorkspace(t, db, "ws-lower", repo, lowered, true, 1)
	})
	log := dlog.NewTestLogger()
	unreadable := func(string) (string, error) { return "", errors.New("permission denied") }

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites(), WithLogger(log), withCanonicalDir(unreadable))
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	defer handle.Close()

	// Assert
	record, ok := findRecord(log, "error", "workspaces are registered under a spelling that is not their directory's on-disk one")
	if !ok {
		t.Fatalf("no violation record; records = %+v", log.Records())
	}
	if rows, _ := record.Context["rows"].(string); !strings.Contains(rows, "cannot canonicalize") {
		t.Fatalf("rows = %q, want the canonicalization failure named", rows)
	}
}

func TestOpenRecordsNothingAboveDebugWhenEveryRowIsCanonical(t *testing.T) {
	// Arrange
	path, kept := spellingFixture(t, func(db *sql.DB, repo, kept string) {
		insertWorkspace(t, db, "ws-kept", repo, kept, false, 1)
	})

	// Act
	_, log := openSpelled(t, path, kept)

	// Assert
	for _, record := range log.Records() {
		if record.Level != "debug" && strings.Contains(record.Message, "spelling") {
			t.Fatalf("recorded %s %q for a canonical registry", record.Level, record.Message)
		}
	}
}
