package workspace

import (
	"context"
	"errors"
	"go/ast"
	"go/token"
	"io/fs"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/sourcescan"
	"agent-repld/internal/wsm"
)

// testMissingDirOp is the operation the tests' settlements are recorded under.
const testMissingDirOp = "daemon.test.missing_dir"

// errGone is the stat error a settlement is handed.
var errGone = &fs.PathError{Op: "stat", Path: "/gone", Err: fs.ErrNotExist}

func TestSettleMissingDirectory(t *testing.T) {
	merged := time.Unix(1, 0)
	tests := []struct {
		name         string
		arrange      func(f *fixture, ws wsm.Workspace)
		wantRestored bool
		wantMessage  string
		wantRestErr  bool
	}{
		{
			name:         "the branch survives: recreated, left open",
			arrange:      func(f *fixture, ws wsm.Workspace) { withBranch(f, ws) },
			wantRestored: true,
			wantMessage:  "the workspace directory was gone; its worktree was recreated from its branch",
		},
		{
			name:        "the branch is gone: closed by the daemon",
			arrange:     func(*fixture, wsm.Workspace) {},
			wantMessage: "the workspace directory is gone; the workspace is closed",
		},
		{
			name: "the workspace was merged: closed, never recreated",
			arrange: func(f *fixture, ws wsm.Workspace) {
				withBranch(f, ws)
				ws.MergedAt = &merged
				f.db.with(ws)
			},
			wantMessage: "the workspace directory is gone; the workspace is closed",
		},
		{
			name: "git cannot add the worktree: closed, carrying the restore's error",
			arrange: func(f *fixture, ws wsm.Workspace) {
				withBranch(f, ws)
				f.git.restoreErr = errors.New("git exploded")
			},
			wantMessage: "the workspace directory is gone; the workspace is closed",
			wantRestErr: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			ws := goneWorkspace(t, f)
			tt.arrange(f, ws)
			ws = f.db.workspaces["w1"]

			// Act.
			restored, err := f.verbs.SettleMissingDirectory(context.Background(), testMissingDirOp, ws, errGone)

			// Assert.
			if err != nil || restored != tt.wantRestored {
				t.Fatalf("SettleMissingDirectory = (%v, %v), want (%v, nil)", restored, err, tt.wantRestored)
			}
			closures := f.db.closures["w1"]
			if tt.wantRestored && len(closures) != 0 {
				t.Fatalf("closures = %+v, want none for a recreated workspace", closures)
			}
			if !tt.wantRestored && (len(closures) != 1 || closures[0].By != wsm.CloseByDaemon) {
				t.Fatalf("closures = %+v, want one close by the daemon", closures)
			}
			record, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: testMissingDirOp, Message: tt.wantMessage})
			if !ok {
				t.Fatalf("no INFO record %q under %s: %+v", tt.wantMessage, testMissingDirOp, f.log.logger.Records())
			}
			if record.Context[dlog.KeyWorkspaceID] != "w1" || record.Context["error"] != errGone.Error() {
				t.Fatalf("record = %+v, want the workspace and the stat error", record)
			}
			if _, carried := record.Context["restore_error"]; carried != tt.wantRestErr {
				t.Fatalf("restore_error carried = %v, want %v: %+v", carried, tt.wantRestErr, record.Context)
			}
			if _, warned := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelWarn, Operation: testMissingDirOp}); warned {
				t.Fatalf("the ruled settlement was recorded at WARN: %+v", f.log.logger.Records())
			}
		})
	}
}

func TestSettleMissingDirectorySurfacesAFailedClose(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := goneWorkspace(t, f)
	f.db.setClosedErr = errors.New("database is locked")

	// Act.
	restored, err := f.verbs.SettleMissingDirectory(context.Background(), testMissingDirOp, ws, errGone)

	// Assert.
	if restored || err == nil || !strings.Contains(err.Error(), "database is locked") {
		t.Fatalf("SettleMissingDirectory = (%v, %v), want the close's failure", restored, err)
	}
	record, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: testMissingDirOp})
	if !ok || record.Context["cause"] != "database is locked" || record.Context[dlog.KeyWorkspaceID] != "w1" {
		t.Fatalf("ERROR record = (%+v, %v), want the close's cause and the workspace", record, ok)
	}
	if _, closedInfo := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: testMissingDirOp}); closedInfo {
		t.Fatalf("a close that was not written was stated at INFO: %+v", f.log.logger.Records())
	}
}

func TestPublishRegistrySettlesAMissingDirectory(t *testing.T) {
	tests := []struct {
		name       string
		branch     bool
		wantClosed bool
	}{
		{name: "a surviving branch recreates the worktree and keeps the row open", branch: true, wantClosed: false},
		{name: "no branch closes the row", branch: false, wantClosed: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			ws := goneWorkspace(t, f)
			if tt.branch {
				withBranch(f, ws)
			}

			// Act.
			if err := f.verbs.PublishRegistry(context.Background()); err != nil {
				t.Fatalf("PublishRegistry: %v", err)
			}

			// Assert.
			if got := f.db.closedFlags["w1"]; got != tt.wantClosed {
				t.Fatalf("closed = %v, want %v", got, tt.wantClosed)
			}
			if restored := len(f.git.restored) == 1; restored != tt.branch {
				t.Fatalf("restored = %v, want a restore %v", f.git.restored, tt.branch)
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opRegister}); !ok {
				t.Fatalf("the settlement was not recorded at INFO under %s: %+v", opRegister, f.log.logger.Records())
			}
		})
	}
}

// TestAMissingDirectoryIsClosedOnlyBySettleMissingDirectory keeps every
// missing-directory close in the daemon on the one settlement: a caller
// closing such a row itself skips the ruled recreation, which is how a running
// daemon closed workspaces whose branches survived (2026-10-24).
func TestAMissingDirectoryIsClosedOnlyBySettleMissingDirectory(t *testing.T) {
	// Arrange.
	const closeRecord = "a workspace whose directory is gone could not be closed"
	root := sourcescan.DaemonRoot(t)

	// Act.
	var spelledIn []string
	sourcescan.EachDaemonSource(t, root, nil, func(fset *token.FileSet, file *ast.File) {
		ast.Inspect(file, func(n ast.Node) bool {
			if lit, ok := n.(*ast.BasicLit); ok && lit.Kind == token.STRING && strings.Contains(lit.Value, closeRecord) {
				rel, _ := filepath.Rel(root, fset.Position(lit.Pos()).Filename)
				spelledIn = append(spelledIn, rel)
			}
			return true
		})
	})

	// Assert.
	want := []string{filepath.Join("internal", "workspace", "missingdir.go")}
	if !slices.Equal(spelledIn, want) {
		t.Fatalf("the missing-directory close is spelled in %v, want only %v", spelledIn, want)
	}
}
