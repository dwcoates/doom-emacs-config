package wsm

import (
	"context"
	"database/sql"
	"path/filepath"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
)

// testStore opens a fresh IN-MEMORY store (OpenInMemory), with a capturing
// logger, and closes it at the end of the test. A test whose subject is the
// FILE (reopening it, a second handle on it, migrating it, its read-only mode)
// uses fileStore instead.
func testStore(t *testing.T) (*store, *dlog.TestLogger) {
	t.Helper()
	log := dlog.NewTestLogger()
	handle, err := OpenInMemory(context.Background(), WithLogger(log))
	if err != nil {
		t.Fatalf("OpenInMemory: %v", err)
	}
	t.Cleanup(func() { handle.Close() })
	s, ok := handle.(*store)
	if !ok {
		t.Fatalf("Open returned %T, want *store", handle)
	}
	return s, log
}

// fileStore opens a fresh store on a real FILE in the test's own temp dir, for
// a test whose subject is the file itself. Its writes skip SQLite's forced
// flushes (WithUnsyncedWrites): the file is thrown away with the test.
func fileStore(t *testing.T) (*store, *dlog.TestLogger) {
	t.Helper()
	log := dlog.NewTestLogger()
	handle, err := Open(context.Background(), filepath.Join(t.TempDir(), "wsm.db"), WithLogger(log), WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { handle.Close() })
	s, ok := handle.(*store)
	if !ok {
		t.Fatalf("Open returned %T, want *store", handle)
	}
	return s, log
}

// testWorkspace registers one workspace in a fresh temp dir, the arrangement
// almost every table's test needs before it can write a dependent row.
func testWorkspace(t *testing.T, s *store) Workspace {
	t.Helper()
	return testWorkspaceNamed(t, s, "sample")
}

// testWorkspaceNamed registers a SECOND workspace beside the first, for the
// tables whose subject is that one workspace's rows are not another's.
func testWorkspaceNamed(t *testing.T, s *store, name string) Workspace {
	t.Helper()
	dir := t.TempDir()
	ws, created, err := s.RegisterWorkspace(context.Background(), dir, RegisterFacts{
		Name: name, Branch: "feature", ParentBranch: "master", RepoDir: dir,
	})
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}
	if !created {
		t.Fatalf("RegisterWorkspace reported an existing record for a fresh store")
	}
	return ws
}

// corrupt runs one raw statement against the store, so a test can write the
// exact malformed row a decoder must refuse.
func corrupt(t *testing.T, s *store, query string, args ...any) {
	t.Helper()
	if _, err := s.db().ExecContext(context.Background(), query, args...); err != nil {
		t.Fatalf("corrupt %q: %v", query, err)
	}
}

// scalar reads one value back with raw SQL, so a test can assert what the row
// actually holds rather than what a getter reports.
func scalar[T any](t *testing.T, s *store, query string, args ...any) T {
	t.Helper()
	var out T
	if err := s.db().QueryRowContext(context.Background(), query, args...).Scan(&out); err != nil {
		t.Fatalf("scalar %q: %v", query, err)
	}
	return out
}

// said builds a submission carrying one text block, the minimum a held prompt
// must round-trip.
func said(text string) *conversationv1.UserSaid {
	return &conversationv1.UserSaid{
		Content: &conversationv1.UserContent{
			Blocks: []*conversationv1.UserContentBlock{{
				Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}},
			}},
		},
	}
}

// firstText returns the first text block of a submission, for asserting a
// round trip without comparing whole proto messages.
func firstText(s *conversationv1.UserSaid) string {
	if s == nil || s.GetContent() == nil || len(s.GetContent().GetBlocks()) == 0 {
		return ""
	}
	return s.GetContent().GetBlocks()[0].GetText().GetText()
}

// instant is a fixed, non-zero instant: tests assert stored times exactly, so
// none of them reads the wall clock.
var instant = time.Date(2026, 8, 29, 12, 0, 0, 0, time.UTC)

// nullable reports whether a column is NULL for one row.
func isNull(t *testing.T, s *store, query string, args ...any) bool {
	t.Helper()
	var out sql.NullString
	if err := s.db().QueryRowContext(context.Background(), query, args...).Scan(&out); err != nil {
		t.Fatalf("isNull %q: %v", query, err)
	}
	return !out.Valid
}
