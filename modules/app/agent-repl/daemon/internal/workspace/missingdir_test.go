package workspace

import (
	"context"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
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

// TestSettleMissingDirectoryLeavesAWorkspaceThatOwesANuke pins the guard: a
// workspace whose tree a stopped nuke set aside is neither recreated from the
// branch the nuke has yet to delete nor closed; it is left for the nuke.
func TestSettleMissingDirectoryLeavesAWorkspaceThatOwesANuke(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := goneWorkspace(t, f)
	withBranch(f, ws)
	owed := time.Unix(5, 0)
	ws = f.db.workspaces["w1"]
	ws.NukeOwedAt = &owed
	f.db.with(ws)

	// Act.
	restored, err := f.verbs.SettleMissingDirectory(context.Background(), testMissingDirOp, ws, errGone)

	// Assert.
	if err != nil || restored {
		t.Fatalf("SettleMissingDirectory = (%v, %v), want (false, nil)", restored, err)
	}
	if closures := f.db.closures["w1"]; len(closures) != 0 {
		t.Fatalf("closures = %+v, want none for a workspace that owes a nuke", closures)
	}
	for _, call := range f.git.gitCalls {
		if strings.HasPrefix(call, "restore ") {
			t.Fatalf("git calls = %v, want no restore", f.git.gitCalls)
		}
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: testMissingDirOp, Message: "the workspace directory is gone because a nuke is under way; it is left for the nuke to finish"}); !ok {
		t.Fatalf("records = %+v, want the owed nuke recorded", f.log.logger.Records())
	}
}

// TestSettleMissingDirectoryActsOnTheRowReadAfterTheStat pins the re-read: a
// caller's row read before a landing stamped and closed it never sends a
// restore into the removal still holding the branch (deploy
// 2026-11-17T00:29:50).
func TestSettleMissingDirectoryActsOnTheRowReadAfterTheStat(t *testing.T) {
	merged := time.Unix(1, 0)
	tests := []struct {
		name   string
		landed func(ws wsm.Workspace) wsm.Workspace
	}{
		{name: "merged and closed meanwhile", landed: func(ws wsm.Workspace) wsm.Workspace {
			ws.MergedAt, ws.Closed = &merged, true
			return ws
		}},
		{name: "closed meanwhile", landed: func(ws wsm.Workspace) wsm.Workspace {
			ws.Closed = true
			return ws
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			ws := goneWorkspace(t, f)
			withBranch(f, ws)
			stale := f.db.workspaces["w1"]
			f.db.with(tt.landed(stale))

			// Act.
			restored, err := f.verbs.SettleMissingDirectory(context.Background(), testMissingDirOp, stale, errGone)

			// Assert.
			if err != nil || restored {
				t.Fatalf("SettleMissingDirectory = (%v, %v), want (false, nil)", restored, err)
			}
			if closures := f.db.closures["w1"]; len(closures) != 0 {
				t.Fatalf("closures = %+v, want none for a row already closed", closures)
			}
			for _, call := range f.git.gitCalls {
				if strings.HasPrefix(call, "restore ") {
					t.Fatalf("git calls = %v, want no restore", f.git.gitCalls)
				}
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: testMissingDirOp, Message: "the workspace directory is gone and the workspace was closed meanwhile; it is left closed"}); !ok {
				t.Fatalf("records = %+v, want the closed row recorded", f.log.logger.Records())
			}
		})
	}
}

func TestSettleMissingDirectorySurfacesAFailedReRead(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := goneWorkspace(t, f)
	withBranch(f, ws)
	f.db.workspaceErr = errors.New("registry down")

	// Act.
	restored, err := f.verbs.SettleMissingDirectory(context.Background(), testMissingDirOp, ws, errGone)

	// Assert.
	if restored || err == nil || !strings.Contains(err.Error(), "registry down") {
		t.Fatalf("SettleMissingDirectory = (%v, %v), want the read's failure", restored, err)
	}
	record, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: testMissingDirOp})
	if !ok || record.Context["cause"] != "registry down" || record.Context[dlog.KeyWorkspaceID] != "w1" {
		t.Fatalf("ERROR record = (%+v, %v), want the read's cause and the workspace", record, ok)
	}
	if closures := f.db.closures["w1"]; len(closures) != 0 {
		t.Fatalf("closures = %+v, want none after a failed read", closures)
	}
	for _, call := range f.git.gitCalls {
		if strings.HasPrefix(call, "restore ") {
			t.Fatalf("git calls = %v, want no restore", f.git.gitCalls)
		}
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

func TestSettleMissingDirectoryHoldsTheWorkspacesDirectoryGateAcrossTheRestore(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := goneWorkspace(t, f)
	withBranch(f, ws)
	gate := f.verbs.(*verbs).dirGates.For("w1")
	heldDuringRestore := false
	f.git.duringRestore = func() { heldDuringRestore = !gate.TryLock() }

	// Act.
	restored, err := f.verbs.SettleMissingDirectory(context.Background(), testMissingDirOp, ws, errGone)

	// Assert.
	if err != nil || !restored {
		t.Fatalf("SettleMissingDirectory = (%v, %v), want (true, nil)", restored, err)
	}
	if !heldDuringRestore {
		t.Fatal("the directory gate was free while the settle restored the worktree")
	}
	if !gate.TryLock() {
		t.Fatal("the directory gate stayed held after the settle answered")
	}
}

func TestSettleMissingDirectoryBindsADirectoryRestoredMeanwhile(t *testing.T) {
	// Arrange: an open holding the gate first brought the directory back.
	f := newFixture(t)
	ws := goneWorkspace(t, f)
	withBranch(f, ws)
	if err := os.MkdirAll(ws.Dir, 0o755); err != nil {
		t.Fatalf("make the restored directory: %v", err)
	}

	// Act.
	restored, err := f.verbs.SettleMissingDirectory(context.Background(), testMissingDirOp, ws, errGone)

	// Assert.
	if err != nil || !restored {
		t.Fatalf("SettleMissingDirectory = (%v, %v), want (true, nil)", restored, err)
	}
	if len(f.git.restored) != 0 {
		t.Fatalf("restored = %+v, want no restore into a directory that is there", f.git.restored)
	}
	if got := f.footer.dirs["w1"]; got != ws.Dir {
		t.Fatalf("footer bound dir = %q, want %q", got, ws.Dir)
	}
	if closures := f.db.closures["w1"]; len(closures) != 0 {
		t.Fatalf("closures = %+v, want none", closures)
	}
	want := "the workspace directory was gone and was restored meanwhile; its views are bound"
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: testMissingDirOp, Message: want}); !ok {
		t.Fatalf("no INFO record %q: %+v", want, f.log.logger.Records())
	}
}

func TestSettleMissingDirectorySurfacesAFailedBindOfADirectoryRestoredMeanwhile(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := goneWorkspace(t, f)
	if err := os.MkdirAll(ws.Dir, 0o755); err != nil {
		t.Fatalf("make the restored directory: %v", err)
	}
	f.footer.setDirErr = errors.New("bind exploded")

	// Act.
	restored, err := f.verbs.SettleMissingDirectory(context.Background(), testMissingDirOp, ws, errGone)

	// Assert.
	if restored || err == nil || !strings.Contains(err.Error(), "bind exploded") {
		t.Fatalf("SettleMissingDirectory = (%v, %v), want (false, the bind's error)", restored, err)
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opRegister, Message: "a resolver could not be bound to the workspace's directory"}); !ok {
		t.Fatalf("no ERROR record of the failed bind: %+v", f.log.logger.Records())
	}
	if closures := f.db.closures["w1"]; len(closures) != 0 {
		t.Fatalf("closures = %+v, want none", closures)
	}
}

func TestSettleMissingDirectorySurfacesAFailedNamingOfADirectoryRestoredMeanwhile(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := goneWorkspace(t, f)
	if err := os.MkdirAll(ws.Dir, 0o755); err != nil {
		t.Fatalf("make the restored directory: %v", err)
	}
	f.db.sessionErr = errors.New("session exploded")

	// Act.
	restored, err := f.verbs.SettleMissingDirectory(context.Background(), testMissingDirOp, ws, errGone)

	// Assert.
	if restored || err == nil || !strings.Contains(err.Error(), "session exploded") {
		t.Fatalf("SettleMissingDirectory = (%v, %v), want (false, the naming's error)", restored, err)
	}
	record, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: testMissingDirOp, Message: "could not publish the naming of the workspace restored meanwhile"})
	if !ok {
		t.Fatalf("no ERROR record of the failed naming: %+v", f.log.logger.Records())
	}
	if record.Context[dlog.KeyWorkspaceID] != "w1" || !strings.Contains(record.Context["cause"].(string), "session exploded") {
		t.Fatalf("record = %+v, want the workspace and the cause", record)
	}
	if closures := f.db.closures["w1"]; len(closures) != 0 {
		t.Fatalf("closures = %+v, want none", closures)
	}
}

func TestSettleMissingDirectorySurfacesAStatThatCannotTell(t *testing.T) {
	// Arrange: the directory's parent cannot be searched, so the stat under
	// the gate answers neither "there" nor "gone".
	f := newFixture(t)
	ws := goneWorkspace(t, f)
	withBranch(f, ws)
	parent := filepath.Dir(ws.Dir)
	if err := os.Chmod(parent, 0); err != nil {
		t.Fatalf("lock the parent: %v", err)
	}
	t.Cleanup(func() { _ = os.Chmod(parent, 0o755) })

	// Act.
	restored, err := f.verbs.SettleMissingDirectory(context.Background(), testMissingDirOp, ws, errGone)

	// Assert.
	if restored || err == nil {
		t.Fatalf("SettleMissingDirectory = (%v, %v), want (false, the stat's error)", restored, err)
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: testMissingDirOp, Message: "could not tell again whether the workspace directory is gone; it is left as it is"}); !ok {
		t.Fatalf("no ERROR record of the stat: %+v", f.log.logger.Records())
	}
	if len(f.git.restored) != 0 {
		t.Fatalf("restored = %+v, want no restore", f.git.restored)
	}
	if closures := f.db.closures["w1"]; len(closures) != 0 {
		t.Fatalf("closures = %+v, want none", closures)
	}
}
