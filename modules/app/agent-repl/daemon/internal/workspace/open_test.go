package workspace

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/gitclient"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/rollout"
	"agent-repld/internal/sessionwatcher"
	"agent-repld/internal/wsm"
)

func TestOpenStartsTheSession(t *testing.T) {
	// Arrange: mounting a parked workspace IS an implicit revival.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Open(context.Background(), "w1", nil); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	if len(f.fleet.started) != 1 {
		t.Fatalf("sessions started = %d, want exactly one", len(f.fleet.started))
	}
}

func TestOpenIsIdempotentWhenTheSessionIsAlreadyLive(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.fleet.live["w1"] = true

	// Act.
	if err := f.verbs.Open(context.Background(), "w1", nil); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	if len(f.fleet.started) != 0 {
		t.Fatalf("sessions started = %d, want none for an already-live session", len(f.fleet.started))
	}
}

func TestOpenClearsTheClosedFlag(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir())
	ws.Closed = true
	f.db.with(ws)

	// Act.
	if err := f.verbs.Open(context.Background(), "w1", nil); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	if closed, ok := f.db.closedFlags["w1"]; !ok || closed {
		t.Fatalf("closed flag = (%v, %v), want it cleared", closed, ok)
	}
}

func TestOpenOfAClosedWorkspaceClaimsServingInTheReopen(t *testing.T) {
	tests := []struct {
		name string
		live bool
	}{
		// THE LIVE CASE IS THE ONE THE CLAIM EXISTS FOR: a close leaves the
		// session alone, so the open brings nothing up and only the reopen
		// can claim the row back (2026-10-25).
		{name: "its session still live", live: true},
		{name: "its session down", live: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			ws := f.workspace("w1", t.TempDir())
			ws.Closed = true
			f.db.with(ws)
			f.fleet.live["w1"] = tt.live

			// Act.
			if err := f.verbs.Open(context.Background(), "w1", nil); err != nil {
				t.Fatalf("Open: %v", err)
			}

			// Assert.
			want := []servingClaim{{ws: "w1", instance: fixtureInstance}}
			if !reflect.DeepEqual(f.db.reopenClaims, want) {
				t.Fatalf("reopen claims = %+v, want %+v", f.db.reopenClaims, want)
			}
		})
	}
}

func TestOpenTakesAOneShotBack(t *testing.T) {
	tests := []struct {
		name   string
		closed bool
		want   bool
	}{
		{name: "reopening a closed one-shot clears its mark", closed: true, want: false},
		{name: "opening an open one-shot keeps its mark", closed: false, want: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			ws := f.workspace("w1", t.TempDir())
			ws.Closed = tt.closed
			ws.OneShot = true
			f.db.with(ws)
			f.db.jobs["w1"] = wsm.CreationJob{Workspace: "w1", OneShot: true}
			oneShotBriefs(f)

			// Act
			err := f.verbs.Open(context.Background(), "w1", nil)

			// Assert
			if err != nil || f.db.jobs["w1"].OneShot != tt.want {
				t.Fatalf("Open = %v, one-shot %v; want %v", err, f.db.jobs["w1"].OneShot, tt.want)
			}
		})
	}
}

func TestOpenSurfacesAOneShotThatCannotBeTakenBack(t *testing.T) {
	// Arrange
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir())
	ws.Closed, ws.OneShot = true, true
	f.db.with(ws)
	f.db.jobs["w1"] = wsm.CreationJob{Workspace: "w1", OneShot: true}
	f.db.setOneShotErr = errors.New("db gone")

	// Act
	err := f.verbs.Open(context.Background(), "w1", nil)

	// Assert
	if err == nil {
		t.Fatal("Open succeeded, want the failed take-back surfaced")
	}
}

func TestOpenRetiresAStandingCloseRefusal(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Open(context.Background(), "w1", nil); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	if blocked, ok := f.footer.closing["w1"]; !ok || blocked != nil {
		t.Fatalf("footer close refusal = %v, want it cleared", blocked)
	}
}

func TestOpenRunsTheBuildStalenessCheck(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Open(context.Background(), "w1", nil); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	if len(f.rollout.relaunches) != 1 || f.rollout.relaunches[0].Reason != rollout.ReasonBuildStale {
		t.Fatalf("relaunches = %+v, want one build-staleness bounce", f.rollout.relaunches)
	}
}

func TestOpenSurvivesAFailedBuildStalenessCheck(t *testing.T) {
	// Arrange: the session is up and usable on the older build, so a bounce
	// that will not run is a warning rather than a failed mount.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.rollout.checkErr = errors.New("the installed shim build is unreadable")

	// Act.
	err := f.verbs.Open(context.Background(), "w1", nil)

	// Assert.
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
}

func TestOpenSurfacesABringUpFailure(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.fleet.startErr = errors.New("the shim exited during bring-up")

	// Act.
	err := f.verbs.Open(context.Background(), "w1", nil)

	// Assert.
	if err == nil {
		t.Fatal("Open() = nil error, want the bring-up failure surfaced")
	}
}

func TestOpenRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	err := f.verbs.Open(context.Background(), "nope", nil)

	// Assert.
	asRefusal(t, err, ArmUnknownWorkspace)
}

// goneWorkspace arranges a workspace whose directory is gone, in a repository
// that still exists, and answers its record. Whether its branch survives is
// the caller's to arrange.
func goneWorkspace(t *testing.T, f *fixture) wsm.Workspace {
	t.Helper()
	return f.workspace("w1", filepath.Join(t.TempDir(), "gone"))
}

// withBranch makes the repository hold ws's branch, so a restore can find it.
func withBranch(f *fixture, ws wsm.Workspace) {
	f.git.existingBranches = map[string]bool{ws.Branch: true}
}

func TestOpenRestoresAGoneWorktreeFromItsBranch(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := goneWorkspace(t, f)
	withBranch(f, ws)

	// Act.
	err := f.verbs.Open(context.Background(), "w1", nil)

	// Assert.
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	want := restoredWorktree{RepoDir: filepath.Dir(ws.Dir), WorktreeDir: ws.Dir, Branch: ws.Branch}
	if len(f.git.restored) != 1 || f.git.restored[0] != want {
		t.Fatalf("restored = %+v, want exactly %+v", f.git.restored, want)
	}
}

func TestOpenStartsTheSessionInARestoredWorktree(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := goneWorkspace(t, f)
	withBranch(f, ws)

	// Act.
	if err := f.verbs.Open(context.Background(), "w1", nil); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	if len(f.fleet.started) != 1 {
		t.Fatalf("sessions started = %d, want the restored workspace's session", len(f.fleet.started))
	}
}

func TestOpenClearsTheClosedFlagOfARestoredWorkspace(t *testing.T) {
	// Arrange: boot closed the row when it found the directory gone.
	f := newFixture(t)
	ws := goneWorkspace(t, f)
	ws.Closed = true
	f.db.with(ws)
	withBranch(f, ws)

	// Act.
	if err := f.verbs.Open(context.Background(), "w1", nil); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	if closed, ok := f.db.closedFlags["w1"]; !ok || closed {
		t.Fatalf("closed flag = (%v, %v), want it cleared", closed, ok)
	}
}

func TestOpenRetiresTheStaleRegistrationBeforeRestoring(t *testing.T) {
	// Arrange: the directory was deleted with rm, so git still registers it
	// and a bare `worktree add` would refuse.
	f := newFixture(t)
	ws := goneWorkspace(t, f)
	withBranch(f, ws)
	f.git.worktrees = []gitclient.Worktree{{Dir: ws.Dir, Branch: ws.Branch, Prunable: true}}

	// Act.
	err := f.verbs.Open(context.Background(), "w1", nil)

	// Assert.
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	repo := filepath.Dir(ws.Dir)
	want := []string{"list " + repo, "unregister " + ws.Dir, "restore " + ws.Dir}
	if strings.Join(f.git.gitCalls, "|") != strings.Join(want, "|") {
		t.Fatalf("git calls = %v, want %v", f.git.gitCalls, want)
	}
}

func TestOpenLeavesEveryOtherRegistrationAlone(t *testing.T) {
	// Arrange: another missing worktree is registered too; it is not this
	// workspace's to retire.
	f := newFixture(t)
	ws := goneWorkspace(t, f)
	withBranch(f, ws)
	other := gitclient.Worktree{Dir: filepath.Join(t.TempDir(), "someone-elses"), Branch: "theirs", Prunable: true}
	f.git.worktrees = []gitclient.Worktree{other, {Dir: ws.Dir, Branch: ws.Branch, Prunable: true}}

	// Act.
	if err := f.verbs.Open(context.Background(), "w1", nil); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	if len(f.git.worktrees) != 1 || f.git.worktrees[0].Dir != other.Dir {
		t.Fatalf("registrations left = %+v, want only %s", f.git.worktrees, other.Dir)
	}
}

func TestOpenRetiresNoRegistrationWhenGitHoldsNone(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := goneWorkspace(t, f)
	withBranch(f, ws)

	// Act.
	if err := f.verbs.Open(context.Background(), "w1", nil); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	for _, call := range f.git.gitCalls {
		if strings.HasPrefix(call, "unregister ") {
			t.Fatalf("git calls = %v, want no unregister when nothing is registered", f.git.gitCalls)
		}
	}
}

func TestOpenRefusesToForceALockedMissingWorktree(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := goneWorkspace(t, f)
	withBranch(f, ws)
	f.git.worktrees = []gitclient.Worktree{{Dir: ws.Dir, Branch: ws.Branch, Locked: true, LockedReason: "on a usb disk"}}

	// Act.
	err := f.verbs.Open(context.Background(), "w1", nil)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "locked") {
		t.Fatalf("Open = %v, want a failure naming the lock", err)
	}
	if !hasRecord(f, dlog.LevelError, opOpen) || len(f.git.restored) != 0 {
		t.Fatalf("restored = %v, want nothing restored and the lock recorded at ERROR", f.git.restored)
	}
}

func TestOpenRecordsTheRestoreAtInfoWithItsDirectoryAndBranch(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := goneWorkspace(t, f)
	withBranch(f, ws)

	// Act.
	if err := f.verbs.Open(context.Background(), "w1", nil); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	for _, r := range f.log.logger.Records() {
		if r.Level == dlog.LevelInfo && r.Operation == opOpen && strings.Contains(r.Message, "restored its worktree") {
			if r.Context["dir"] != ws.Dir || r.Context["branch"] != ws.Branch {
				t.Fatalf("restore record context = %v, want dir %q and branch %q", r.Context, ws.Dir, ws.Branch)
			}
			return
		}
	}
	t.Fatalf("no INFO record of the restore; records = %+v", f.log.logger.Records())
}

func TestOpenReportsTheRestoreStageBetweenTheCheckAndTheBringUp(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := goneWorkspace(t, f)
	withBranch(f, ws)
	progress := &recordingOpenProgress{}

	// Act.
	if err := f.verbs.Open(context.Background(), "w1", progress); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	want := []OpenStage{OpenStageCheckingWorktree, OpenStageRestoringWorktree, OpenStageStartingSession, OpenStageCheckingBuild}
	if len(progress.stages) != len(want) {
		t.Fatalf("stages = %v, want %v", progress.stages, want)
	}
	for i := range want {
		if progress.stages[i] != want[i] {
			t.Fatalf("stages = %v, want %v", progress.stages, want)
		}
	}
}

func TestOpenBindsTheRestoredWorkspacesViews(t *testing.T) {
	// Arrange: boot binds the views only of a workspace whose directory
	// exists, so a restored one has none until the open binds them.
	f := newFixture(t)
	ws := goneWorkspace(t, f)
	withBranch(f, ws)

	// Act.
	if err := f.verbs.Open(context.Background(), "w1", nil); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	if got := f.footer.dirs["w1"]; got != ws.Dir {
		t.Fatalf("footer bound dir = %q, want the restored %q", got, ws.Dir)
	}
}

func TestOpenBindsNoViewsForAnUnrestorableWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	goneWorkspace(t, f)

	// Act.
	_ = f.verbs.Open(context.Background(), "w1", nil)

	// Assert.
	if got, bound := f.footer.dirs["w1"]; bound {
		t.Fatalf("footer bound dir = %q, want nothing bound for a directory that is not there", got)
	}
}

func TestOpenTouchesNoGitWhenTheDirectoryIsPresent(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Open(context.Background(), "w1", nil); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	if len(f.git.gitCalls) != 0 {
		t.Fatalf("git calls = %v, want none for a workspace whose directory is there", f.git.gitCalls)
	}
}

// TestOpenRefusesAGoneWorkspaceWithNothingToRestoreFrom pins the refusal arm
// for every way there is nothing left to restore a gone directory from.
func TestOpenRefusesAGoneWorkspaceWithNothingToRestoreFrom(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, f *fixture, ws wsm.Workspace)
		says    string
	}{
		{
			name:    "the branch no longer exists",
			arrange: func(*testing.T, *fixture, wsm.Workspace) {},
			says:    "no longer exists",
		},
		{
			name: "no branch was recorded",
			arrange: func(_ *testing.T, f *fixture, ws wsm.Workspace) {
				ws.Branch = ""
				f.db.with(ws)
			},
			says: "no branch was recorded",
		},
		{
			name: "the repository is gone too",
			arrange: func(t *testing.T, f *fixture, ws wsm.Workspace) {
				withBranch(f, ws)
				f.db.repositories = []wsm.Repository{{ID: "repo-1", Dir: filepath.Join(t.TempDir(), "gone-repo")}}
			},
			says: "so is its repository",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			ws := goneWorkspace(t, f)
			tc.arrange(t, f, ws)

			// Act.
			err := f.verbs.Open(context.Background(), "w1", nil)

			// Assert.
			refusal := asRefusal(t, err, ArmWorktreeUnrestorable)
			if !strings.Contains(refusal.Reason, ws.Dir) || !strings.Contains(refusal.Reason, tc.says) {
				t.Fatalf("refusal reason = %q, want it to name %q and say %q", refusal.Reason, ws.Dir, tc.says)
			}
		})
	}
}

func TestOpenCarriesTheGoneDirectoryAndBranchOnItsRefusal(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := goneWorkspace(t, f)

	// Act.
	err := f.verbs.Open(context.Background(), "w1", nil)

	// Assert.
	refusal := asRefusal(t, err, ArmWorktreeUnrestorable)
	if refusal.Fields["dir"] != ws.Dir || refusal.Fields["branch"] != ws.Branch {
		t.Fatalf("refusal fields = %v, want dir %q and branch %q", refusal.Fields, ws.Dir, ws.Branch)
	}
}

func TestOpenStartsNoSessionWhenThereIsNothingToRestore(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	goneWorkspace(t, f)

	// Act.
	_ = f.verbs.Open(context.Background(), "w1", nil)

	// Assert: a shim with no working tree is never spawned.
	if len(f.fleet.started) != 0 || len(f.git.restored) != 0 {
		t.Fatalf("sessions started = %d, restored = %v, want neither", len(f.fleet.started), f.git.restored)
	}
}

func TestOpenRecordsAnUnrestorableWorkspaceAsNoFault(t *testing.T) {
	// Arrange: a user picking a row that cannot be restored is an answer.
	f := newFixture(t)
	goneWorkspace(t, f)

	// Act.
	_ = f.verbs.Open(context.Background(), "w1", nil)

	// Assert.
	for _, r := range f.log.logger.Records() {
		if r.Level == dlog.LevelWarn || r.Level == dlog.LevelError {
			t.Fatalf("an unrestorable workspace produced a fault record: %+v", r)
		}
	}
}

// TestOpenFailsLoudlyWhenTheRestoreCannotRun pins that every git the restore
// depends on failing is an ERROR and a returned failure, never a refusal and
// never a bring-up in a directory that is not there.
func TestOpenFailsLoudlyWhenTheRestoreCannotRun(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fixture)
	}{
		{name: "the branch probe fails", arrange: func(f *fixture) { f.git.branchExistsErr = errors.New("git exploded") }},
		{name: "the worktree listing fails", arrange: func(f *fixture) { f.git.listErr = errors.New("git exploded") }},
		{name: "retiring the stale registration fails", arrange: func(f *fixture) {
			f.git.worktrees = []gitclient.Worktree{{Dir: f.db.workspaces["w1"].Dir, Prunable: true}}
			f.git.unregisterErr = errors.New("git exploded")
		}},
		{name: "the worktree add fails", arrange: func(f *fixture) { f.git.restoreErr = errors.New("git exploded") }},
		{name: "the repository is not registered", arrange: func(f *fixture) { f.db.repositories = nil }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			ws := goneWorkspace(t, f)
			withBranch(f, ws)
			tc.arrange(f)

			// Act.
			err := f.verbs.Open(context.Background(), "w1", nil)

			// Assert.
			if err == nil {
				t.Fatal("Open succeeded, want the restore's failure")
			}
			if _, refused := AsRefusal(err); refused {
				t.Fatalf("Open = refusal %v, want a failure: git that could not act is not an answer", err)
			}
			if !hasRecord(f, dlog.LevelError, opOpen) {
				t.Fatal("the restore's failure was not recorded at ERROR")
			}
			if len(f.fleet.started) != 0 {
				t.Fatalf("sessions started = %d, want none after a failed restore", len(f.fleet.started))
			}
		})
	}
}

// TestOpenProceedsWhenTheDirectoryStatCannotTell pins boot's own discipline: a
// stat that does not say "not exist" is never read as gone.
func TestOpenProceedsWhenTheDirectoryStatCannotTell(t *testing.T) {
	// Arrange: a path whose PARENT is a regular file, so the stat fails with
	// ENOTDIR rather than ENOENT.
	f := newFixture(t)
	blocker := filepath.Join(t.TempDir(), "file")
	if err := os.WriteFile(blocker, nil, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	f.workspace("w1", filepath.Join(blocker, "under"))

	// Act.
	err := f.verbs.Open(context.Background(), "w1", nil)

	// Assert.
	if err != nil {
		t.Fatalf("Open() = %v, want the open to proceed on an undecidable stat", err)
	}
}

func TestCloseBlockerReportsATurnInFlight(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	turn := wsm.TurnID("t1")
	f.running.Turn = &turn

	// Act.
	blocked, err := f.verbs.(*verbs).closeBlocker(context.Background(), "w1")

	// Assert.
	if err != nil {
		t.Fatalf("closeBlocker: %v", err)
	}
	if blocked == nil || blocked.Reason != "turn_in_flight" {
		t.Fatalf("blocker = %+v, want turn_in_flight", blocked)
	}
}

func TestCloseBlockerReportsNothingWhenQuiet(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	blocked, err := f.verbs.(*verbs).closeBlocker(context.Background(), "w1")

	// Assert.
	if err != nil || blocked != nil {
		t.Fatalf("closeBlocker() = (%+v, %v), want quiet", blocked, err)
	}
}

func TestMergeIsPendingOnlyWhileAMergeStillOwesWork(t *testing.T) {
	tests := []struct {
		state string
		want  bool
	}{
		{state: "none", want: false},
		{state: "enqueuing", want: true},
		{state: "queued", want: true},
		{state: "merging", want: true},
		{state: "conflict", want: true},
		{state: "failed", want: false},
		{state: "merged", want: false},
	}
	for _, tt := range tests {
		t.Run(tt.state, func(t *testing.T) {
			// Arrange in the table. Act.
			got := mergeIsPending(tt.state)
			// Assert.
			if got != tt.want {
				t.Fatalf("mergeIsPending(%q) = %v, want %v", tt.state, got, tt.want)
			}
		})
	}
}

// TestCloseBlockerCountsTheLiveWorkItems is landing 7's evidence half: the
// blocker carries the live-work COUNT, not only the sentence.
func TestCloseBlockerCountsTheLiveWorkItems(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.running.LiveWork = sessionwatcher.LiveWorkSet{
		Agents: []*conversationv1.AgentId{{Value: "agent-1"}},
		Shells: []*conversationv1.DetachedWorkId{{Value: "w-1"}, {Value: "w-2"}},
	}

	// Act.
	blocked, err := f.verbs.(*verbs).closeBlocker(context.Background(), "w1")

	// Assert.
	if err != nil {
		t.Fatalf("closeBlocker: %v", err)
	}
	if blocked == nil || blocked.LiveWork != 3 {
		t.Fatalf("blocker = %+v, want live_work 3", blocked)
	}
}

// TestCloseBlockerCountsTheHeldPromptsBehindALeadingBlocker is the whole-picture
// half: every blocker is computed, so evidence for one that is NOT the leading
// reason still rides the refusal.
func TestCloseBlockerCountsTheHeldPromptsBehindALeadingBlocker(t *testing.T) {
	// Arrange: a turn in flight leads, with a held prompt behind it.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	turn := wsm.TurnID("t1")
	f.running.Turn = &turn
	f.db.held["w1"] = []wsm.HeldPrompt{{Workspace: "w1", Turn: "t2"}}

	// Act.
	blocked, err := f.verbs.(*verbs).closeBlocker(context.Background(), "w1")

	// Assert.
	if err != nil {
		t.Fatalf("closeBlocker: %v", err)
	}
	if blocked == nil || blocked.Reason != "turn_in_flight" || blocked.HeldPrompts != 1 {
		t.Fatalf("blocker = %+v, want turn_in_flight leading with held_prompts 1", blocked)
	}
}

// TestCloseBlockerReportsAQueuedMergeBehindALeadingBlocker is the same for the
// merge blocker.
func TestCloseBlockerReportsAQueuedMergeBehindALeadingBlocker(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	turn := wsm.TurnID("t1")
	f.running.Turn = &turn
	f.merge.facts["w1"] = footer.MergeFacts{State: "queued"}

	// Act.
	blocked, err := f.verbs.(*verbs).closeBlocker(context.Background(), "w1")

	// Assert.
	if err != nil {
		t.Fatalf("closeBlocker: %v", err)
	}
	if blocked == nil || !blocked.MergeQueued {
		t.Fatalf("blocker = %+v, want merge_queued evidence", blocked)
	}
}

func TestOpenRevivesAndUnparksAHibernatedWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.hibernate("w1")

	// Act.
	if err := f.verbs.Open(context.Background(), "w1", nil); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	if len(f.fleet.started) != 1 || f.fleet.started[0] != "w1" {
		t.Fatalf("started = %v, want [w1]", f.fleet.started)
	}
	if len(f.topbarParked) != 1 || f.topbarParked[0] {
		t.Fatalf("topbar parked = %v, want the park lifted", f.topbarParked)
	}
}

func TestOpenLiftsNoParkFromAWorkspaceThatWasNotAsleep(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Open(context.Background(), "w1", nil); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	if len(f.topbarParked) != 0 {
		t.Fatalf("topbar parked = %v, want untouched", f.topbarParked)
	}
}

// recordingOpenProgress collects the stages an Open reported, in order. It is
// the whole observation surface of the stage contract: the ORDER is part of it,
// because a client renders the stages as a ladder.
type recordingOpenProgress struct {
	stages []OpenStage
}

func (r *recordingOpenProgress) Stage(stage OpenStage) { r.stages = append(r.stages, stage) }

func TestOpenReportsItsStagesInOrder(t *testing.T) {
	tests := []struct {
		name  string
		setup func(t *testing.T, f *fixture)
		want  []OpenStage
	}{
		{
			name:  "a plain open reports only the unconditional stages",
			setup: func(*testing.T, *fixture) {},
			want: []OpenStage{
				OpenStageCheckingWorktree,
				OpenStageStartingSession,
				OpenStageCheckingBuild,
			},
		},
		{
			name:  "a hibernated workspace also reports the revival",
			setup: func(_ *testing.T, f *fixture) { f.hibernate("w1") },
			want: []OpenStage{
				OpenStageCheckingWorktree,
				OpenStageStartingSession,
				OpenStageReviving,
				OpenStageCheckingBuild,
			},
		},
		{
			name: "a closed workspace also reports the flag being cleared",
			setup: func(_ *testing.T, f *fixture) {
				ws := f.db.workspaces["w1"]
				ws.Closed = true
				f.db.with(ws)
			},
			want: []OpenStage{
				OpenStageCheckingWorktree,
				OpenStageStartingSession,
				OpenStageClearingClosed,
				OpenStageCheckingBuild,
			},
		},
		{
			name:  "an already-live session skips the bring-up stage",
			setup: func(_ *testing.T, f *fixture) { f.fleet.live["w1"] = true },
			want: []OpenStage{
				OpenStageCheckingWorktree,
				OpenStageCheckingBuild,
			},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			test.setup(t, f)
			progress := &recordingOpenProgress{}

			// Act.
			if err := f.verbs.Open(context.Background(), "w1", progress); err != nil {
				t.Fatalf("Open: %v", err)
			}

			// Assert.
			if len(progress.stages) != len(test.want) {
				t.Fatalf("stages = %v, want %v", progress.stages, test.want)
			}
			for i, stage := range test.want {
				if progress.stages[i] != stage {
					t.Fatalf("stages = %v, want %v", progress.stages, test.want)
				}
			}
		})
	}
}

func TestOpenReportsTheWorktreeCheckBeforeRefusingAMissingDirectory(t *testing.T) {
	// Arrange: the refusal comes FROM the stage the ladder is standing on, so
	// a client's last line names the step that actually failed.
	f := newFixture(t)
	dir := filepath.Join(t.TempDir(), "gone")
	f.workspace("w1", dir)
	progress := &recordingOpenProgress{}

	// Act.
	err := f.verbs.Open(context.Background(), "w1", progress)

	// Assert.
	if err == nil {
		t.Fatalf("Open: want a refusal for a directory that is gone")
	}
	if len(progress.stages) != 1 || progress.stages[0] != OpenStageCheckingWorktree {
		t.Fatalf("stages = %v, want only the worktree check", progress.stages)
	}
}

func TestOpenReportsNoStageAfterAFailedBringUp(t *testing.T) {
	// Arrange: a ladder must not advance past the stage that failed.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.fleet.startErr = errors.New("the shim would not spawn")
	progress := &recordingOpenProgress{}

	// Act.
	err := f.verbs.Open(context.Background(), "w1", progress)

	// Assert.
	if err == nil {
		t.Fatalf("Open: want the bring-up failure")
	}
	last := progress.stages[len(progress.stages)-1]
	if last != OpenStageStartingSession {
		t.Fatalf("stages = %v, want the last to be the bring-up", progress.stages)
	}
}

// openWithAKilledRecord arranges the production shape the live-shim invariant
// was written for: the daemon's fleet holds a live shim for the workspace, and
// the durable session record still reads KILLED from a KillWorkspace nothing
// ever retired.
func openWithAKilledRecord(t *testing.T, terminal wsm.SessionTerminal) *fixture {
	t.Helper()
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.fleet.live["w1"] = true
	f.db.sessions["w1"] = wsm.Session{Workspace: "w1", HostSessionID: "host-1", VendorSessionID: "vendor-1"}
	if err := f.db.SetSessionTerminal(context.Background(), "w1", terminal); err != nil {
		t.Fatalf("SetSessionTerminal: %v", err)
	}
	return f
}

func TestOpenRetiresAStaleKilledSessionRecord(t *testing.T) {
	// Arrange: a live session is why this verb starts nothing, and it was
	// also why the record was never revisited — so the open answered in
	// milliseconds and left the roster receding a workspace whose shim was
	// serving.
	f := openWithAKilledRecord(t, wsm.SessionTerminal{Kind: "killed", Detail: "KillWorkspace"})

	// Act.
	if err := f.verbs.Open(context.Background(), "w1", nil); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	if terminal := f.db.sessions["w1"].Terminal; terminal != nil {
		t.Fatalf("terminal = %+v after the open, want it retired", terminal)
	}
}

func TestOpenRepublishesARosterRowThatDoesNotRecede(t *testing.T) {
	// Arrange: the roster RECEDES a killed session's row and Emacs gives a tab
	// only to a row that is not receded, so the record the republish carries
	// is what decides whether the user gets a tab at all
	// (internal/resolve/sidebar's `recedes`).
	f := openWithAKilledRecord(t, wsm.SessionTerminal{Kind: "killed", Detail: "KillWorkspace"})

	// Act.
	if err := f.verbs.Open(context.Background(), "w1", nil); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	registries := f.sidebar.snapshotRegistries()
	if len(registries) != 1 {
		t.Fatalf("roster republishes = %d, want exactly one", len(registries))
	}
	sessions := registries[0].Sessions
	if len(sessions) != 1 {
		t.Fatalf("registry sessions = %+v, want the workspace's own record", sessions)
	}
	if sessions[0].Terminal != nil {
		t.Fatalf("republished terminal = %+v, want a row that does not recede", sessions[0].Terminal)
	}
}

func TestOpenDoesNotResurrectADeletedSession(t *testing.T) {
	// Arrange: a deleted session's cause of death is final; the open leaves it
	// standing rather than reconciling it away.
	f := openWithAKilledRecord(t, wsm.SessionTerminal{Kind: "deleted", Detail: "forget"})

	// Act.
	if err := f.verbs.Open(context.Background(), "w1", nil); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	terminal := f.db.sessions["w1"].Terminal
	if terminal == nil || terminal.Kind != "deleted" {
		t.Fatalf("terminal = %+v after the open, want the deletion kept", terminal)
	}
}

func TestOpenSurfacesAFailedTerminalRetirement(t *testing.T) {
	// Arrange.
	f := openWithAKilledRecord(t, wsm.SessionTerminal{Kind: "killed", Detail: "KillWorkspace"})
	f.db.clearTerminalErr = errors.New("the store is unreadable")

	// Act.
	err := f.verbs.Open(context.Background(), "w1", nil)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "the store is unreadable") {
		t.Fatalf("Open = %v, want the failed retirement surfaced", err)
	}
}

// TestRestoreMissingWorktreeAnswersTheBootsThreeOutcomes pins the boot's view
// of a restore: restored, nothing to restore from (never an error), and a git
// that could not act (an error).
func TestRestoreMissingWorktreeAnswersTheBootsThreeOutcomes(t *testing.T) {
	merged := time.Unix(1, 0)
	tests := []struct {
		name         string
		arrange      func(f *fixture, ws wsm.Workspace)
		wantRestored bool
		wantErr      bool
	}{
		{name: "the branch survives", arrange: func(f *fixture, ws wsm.Workspace) { withBranch(f, ws) }, wantRestored: true},
		{name: "the branch is gone", arrange: func(*fixture, wsm.Workspace) {}},
		{name: "the workspace was merged", arrange: func(f *fixture, ws wsm.Workspace) {
			withBranch(f, ws)
			ws.MergedAt = &merged
			f.db.with(ws)
		}},
		{name: "git cannot add the worktree", arrange: func(f *fixture, ws wsm.Workspace) {
			withBranch(f, ws)
			f.git.restoreErr = errors.New("git exploded")
		}, wantErr: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			ws := goneWorkspace(t, f)
			tc.arrange(f, ws)
			ws = f.db.workspaces["w1"]

			// Act.
			restored, err := f.verbs.(*verbs).RestoreMissingWorktree(context.Background(), testMissingDirOp, ws)

			// Assert.
			if restored != tc.wantRestored || (err != nil) != tc.wantErr {
				t.Fatalf("RestoreMissingWorktree = (%v, %v), want restored %v and error %v", restored, err, tc.wantRestored, tc.wantErr)
			}
		})
	}
}

func TestRestoreMissingWorktreeNeverRecreatesAMergedWorkspace(t *testing.T) {
	// Arrange: the merge queue stamps merged_at BEFORE it closes the row and
	// removes the tree, so a crash can leave an open, merged row whose tree
	// is gone on purpose. Its branch still exists.
	f := newFixture(t)
	ws := goneWorkspace(t, f)
	withBranch(f, ws)
	merged := time.Unix(1, 0)
	ws.MergedAt = &merged
	f.db.with(ws)

	// Act.
	_, _ = f.verbs.(*verbs).RestoreMissingWorktree(context.Background(), testMissingDirOp, ws)

	// Assert.
	if len(f.git.restored) != 0 || len(f.git.gitCalls) != 0 {
		t.Fatalf("restored = %v, git calls = %v, want no git at all for a merged workspace", f.git.restored, f.git.gitCalls)
	}
}

// mergedWorkspace registers w1 as the merge queue leaves a landed requester:
// merged, closed, its worktree removed, its branch and its parent branch kept.
func mergedWorkspace(t *testing.T, f *fixture) wsm.Workspace {
	t.Helper()
	ws := goneWorkspace(t, f)
	merged := time.Unix(1, 0)
	ws.MergedAt = &merged
	ws.Closed = true
	ws.ParentBranch = "master"
	f.db.with(ws)
	f.git.existingBranches = map[string]bool{ws.Branch: true, ws.ParentBranch: true}
	return ws
}

func TestOpenRestoresAMergedWorkspaceFromItsBranch(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := mergedWorkspace(t, f)

	// Act.
	err := f.verbs.Open(context.Background(), "w1", nil)

	// Assert.
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	want := restoredWorktree{RepoDir: filepath.Dir(ws.Dir), WorktreeDir: ws.Dir, Branch: ws.Branch}
	if len(f.git.restored) != 1 || f.git.restored[0] != want {
		t.Fatalf("restored = %+v, want exactly %+v", f.git.restored, want)
	}
}

func TestOpenStartsTheSessionOfAReopenedMergedWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	mergedWorkspace(t, f)

	// Act.
	if err := f.verbs.Open(context.Background(), "w1", nil); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	if len(f.fleet.started) != 1 {
		t.Fatalf("sessions started = %d, want the reopened merged workspace's session", len(f.fleet.started))
	}
}

func TestOpenAdvancesAReopenedMergedWorkspaceToItsParentsTip(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := mergedWorkspace(t, f)

	// Act.
	if err := f.verbs.Open(context.Background(), "w1", nil); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	want := ws.Dir + "->" + ws.ParentBranch
	if len(f.git.fastForwarded) != 1 || f.git.fastForwarded[0] != want {
		t.Fatalf("fast-forwards = %v, want exactly %q", f.git.fastForwarded, want)
	}
}

func TestOpenAdvancesAMergedWorkspaceOnlyFromInsideItsParent(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := mergedWorkspace(t, f)

	// Act.
	if err := f.verbs.Open(context.Background(), "w1", nil); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	want := "HEAD.." + ws.ParentBranch
	if len(f.git.ancestorChecks) != 1 || f.git.ancestorChecks[0] != want {
		t.Fatalf("ancestor checks = %v, want exactly %q", f.git.ancestorChecks, want)
	}
}

// TestOpenLeavesAMergedWorkspaceTheAdvanceCannotCarryAtItsTip pins every way a
// reopened merged workspace stays where its branch stands: no fast-forward,
// the open succeeds, and an INFO record says why.
func TestOpenLeavesAMergedWorkspaceTheAdvanceCannotCarryAtItsTip(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fixture, ws wsm.Workspace)
		why     string
	}{
		{name: "no parent branch was recorded", arrange: func(f *fixture, ws wsm.Workspace) {
			ws.ParentBranch = ""
			f.db.with(ws)
		}, why: "no parent branch was recorded"},
		{name: "the parent branch is gone", arrange: func(f *fixture, ws wsm.Workspace) {
			delete(f.git.existingBranches, ws.ParentBranch)
		}, why: "the parent branch no longer exists"},
		{name: "the worktree is dirty", arrange: func(f *fixture, _ wsm.Workspace) {
			f.git.dirty = true
		}, why: "the worktree holds uncommitted changes"},
		{name: "the branch moved on after landing", arrange: func(f *fixture, _ wsm.Workspace) {
			f.git.offPrimary = map[string]bool{"HEAD": true}
		}, why: "the branch holds commits its parent does not"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			ws := mergedWorkspace(t, f)
			tc.arrange(f, ws)

			// Act.
			err := f.verbs.Open(context.Background(), "w1", nil)

			// Assert.
			if err != nil {
				t.Fatalf("Open: %v", err)
			}
			if len(f.git.fastForwarded) != 0 {
				t.Fatalf("fast-forwards = %v, want none", f.git.fastForwarded)
			}
			for _, r := range f.log.logger.Records() {
				if r.Level == dlog.LevelInfo && r.Operation == opOpen && r.Context["why"] == tc.why {
					return
				}
			}
			t.Fatalf("no INFO record saying %q; records = %+v", tc.why, f.log.logger.Records())
		})
	}
}

// TestOpenFailsLoudlyWhenTheMergedAdvanceCannotRun pins that a git the advance
// depends on failing is an ERROR naming the step and a returned failure, and
// that no session comes up in a tree whose state the open could not settle.
func TestOpenFailsLoudlyWhenTheMergedAdvanceCannotRun(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fixture)
		step    string
	}{
		{name: "the parent probe fails", arrange: func(f *fixture) {
			f.git.branchExistsErr = errors.New("git exploded")
		}, step: "probe the parent branch"},
		{name: "the cleanliness read fails", arrange: func(f *fixture) {
			f.git.isCleanErr = errors.New("git exploded")
		}, step: "read whether the worktree is clean"},
		{name: "the ancestry read fails", arrange: func(f *fixture) {
			f.git.ancestorErr = errors.New("git exploded")
		}, step: "read whether the branch is inside its parent"},
		{name: "the fast-forward fails", arrange: func(f *fixture) {
			f.git.fastForwardErr = errors.New("git exploded")
		}, step: "fast-forward"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the worktree is present, so the advance is the first git.
			f := newFixture(t)
			ws := f.workspace("w1", t.TempDir())
			merged := time.Unix(1, 0)
			ws.MergedAt, ws.Closed, ws.ParentBranch = &merged, true, "master"
			f.db.with(ws)
			f.git.existingBranches = map[string]bool{ws.Branch: true, ws.ParentBranch: true}
			tc.arrange(f)

			// Act.
			err := f.verbs.Open(context.Background(), "w1", nil)

			// Assert.
			if err == nil || !strings.Contains(err.Error(), tc.step) || !strings.Contains(err.Error(), "git exploded") {
				t.Fatalf("Open = %v, want a failure naming %q and its cause", err, tc.step)
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opOpen}); !ok {
				t.Fatalf("no ERROR record of the failed advance; records = %+v", f.log.logger.Records())
			}
			if len(f.fleet.started) != 0 || len(f.db.reopened) != 0 {
				t.Fatalf("started = %d, reopened = %v, want nothing after a failed advance", len(f.fleet.started), f.db.reopened)
			}
		})
	}
}

func TestOpenReopensAMergedWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	mergedWorkspace(t, f)

	// Act.
	if err := f.verbs.Open(context.Background(), "w1", nil); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	if len(f.db.reopened) != 1 || f.db.reopened[0] != "w1" {
		t.Fatalf("reopened = %v, want w1 reopened once", f.db.reopened)
	}
}

func TestOpenReopensAMergedRowACrashLeftOpen(t *testing.T) {
	// Arrange: the queue stamps merged_at before it closes the row.
	f := newFixture(t)
	ws := mergedWorkspace(t, f)
	ws.Closed = false
	f.db.with(ws)

	// Act.
	if err := f.verbs.Open(context.Background(), "w1", nil); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	if len(f.db.reopened) != 1 {
		t.Fatalf("reopened = %v, want the open merged row reopened", f.db.reopened)
	}
}

func TestOpenRetiresTheLandedMergeOfAReopenedMergedWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	mergedWorkspace(t, f)

	// Act.
	if err := f.verbs.Open(context.Background(), "w1", nil); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	if len(f.merge.retired) != 1 || f.merge.retired[0] != "w1" {
		t.Fatalf("retired merges = %v, want w1's", f.merge.retired)
	}
}

func TestOpenRecordsTheReopenedMergeAtInfo(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := mergedWorkspace(t, f)

	// Act.
	if err := f.verbs.Open(context.Background(), "w1", nil); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	for _, r := range f.log.logger.Records() {
		if r.Level == dlog.LevelInfo && r.Operation == opOpen && strings.Contains(r.Message, "reopened a merged workspace") {
			if r.Context["branch"] != ws.Branch {
				t.Fatalf("reopen record context = %v, want branch %q", r.Context, ws.Branch)
			}
			return
		}
	}
	t.Fatalf("no INFO record of the reopened merge; records = %+v", f.log.logger.Records())
}

func TestOpenTouchesNoMergeOfAWorkspaceThatIsNotMerged(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir())
	ws.Closed, ws.ParentBranch = true, "master"
	f.db.with(ws)

	// Act.
	if err := f.verbs.Open(context.Background(), "w1", nil); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	if len(f.merge.retired) != 0 || len(f.git.fastForwarded) != 0 || len(f.git.ancestorChecks) != 0 {
		t.Fatalf("retired = %v, fast-forwards = %v, ancestor checks = %v, want none for an unmerged workspace",
			f.merge.retired, f.git.fastForwarded, f.git.ancestorChecks)
	}
}

func TestRestoreMissingWorktreeRecordsTheMergedWorkspaceItLeavesAtInfo(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := mergedWorkspace(t, f)

	// Act.
	_, _ = f.verbs.(*verbs).RestoreMissingWorktree(context.Background(), testMissingDirOp, ws)

	// Assert.
	for _, r := range f.log.logger.Records() {
		if r.Level == dlog.LevelInfo && strings.Contains(r.Message, "the daemon does not restore it") {
			return
		}
	}
	t.Fatalf("no INFO record of the merged workspace the daemon left; records = %+v", f.log.logger.Records())
}

func TestOpenRefusesAClosedChildOfAParentThatIsNotOpenBeforeStartingAnything(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("parent", t.TempDir())
	ws := f.workspace("w1", t.TempDir())
	ws.Closed = true
	parent := ids.WorkspaceID("parent")
	ws.Parent = &parent
	f.db.with(ws)
	f.db.openParentErr = errParentNotOpen

	// Act.
	err := f.verbs.Open(context.Background(), "w1", nil)

	// Assert.
	requireFamilyAnswer(t, f, err, errParentNotOpen, opOpen)
	if len(f.fleet.startCalls) != 0 || len(f.db.reopened) != 0 {
		t.Fatalf("starts = %v, reopens = %v; want neither for a refused open", f.fleet.startCalls, f.db.reopened)
	}
	if len(f.db.openParentAsked) != 1 || f.db.openParentAsked[0] != parent {
		t.Fatalf("parents asked = %v, want [parent]", f.db.openParentAsked)
	}
}

func TestOpenAsksNoParentForAnOpenWorkspace(t *testing.T) {
	// Arrange -- an open legacy child is no re-open, whatever its parent.
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir())
	parent := ids.WorkspaceID("parent")
	ws.Parent = &parent
	f.db.with(ws)
	f.db.openParentErr = errParentNotOpen

	// Act.
	err := f.verbs.Open(context.Background(), "w1", nil)

	// Assert.
	if err != nil || len(f.db.openParentAsked) != 0 {
		t.Fatalf("Open = %v asking %v, want success asking nothing", err, f.db.openParentAsked)
	}
}

func TestOpenAnswersAReopenTheFamilyRulesRefuse(t *testing.T) {
	// Arrange -- the parent closed between the read and the re-open.
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir())
	ws.Closed = true
	f.db.with(ws)
	f.db.setClosedErr = errParentNotOpen

	// Act.
	err := f.verbs.Open(context.Background(), "w1", nil)

	// Assert.
	requireFamilyAnswer(t, f, err, errParentNotOpen, opOpen)
}

func TestOpenFailsWhenTheParentCannotBeRead(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir())
	ws.Closed = true
	parent := ids.WorkspaceID("parent")
	ws.Parent = &parent
	f.db.with(ws)
	f.db.openParentErr = errors.New("disk I/O error")

	// Act.
	err := f.verbs.Open(context.Background(), "w1", nil)

	// Assert.
	if err == nil || !loggedAtError(f, opOpen) {
		t.Fatalf("Open = %v, want a failure recorded at ERROR", err)
	}
	if len(f.fleet.startCalls) != 0 {
		t.Fatalf("starts = %v, want none", f.fleet.startCalls)
	}
}
