package workspace

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/rollout"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/wsm"
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

// TestOpenRefusesAWorkspaceWhoseDirectoryIsGone pins the decision that a
// re-open of a workspace whose worktree no longer exists is a NAMED refusal
// rather than an internal error: boot already closes such a row, and there is
// nothing to open.
func TestOpenRefusesAWorkspaceWhoseDirectoryIsGone(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", filepath.Join(t.TempDir(), "gone"))

	// Act.
	err := f.verbs.Open(context.Background(), "w1", nil)

	// Assert.
	asRefusal(t, err, ArmSpawnFailed)
}

func TestOpenNamesTheMissingDirectoryInItsRefusal(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := f.workspace("w1", filepath.Join(t.TempDir(), "gone"))

	// Act.
	err := f.verbs.Open(context.Background(), "w1", nil)

	// Assert: the refusal's evidence says WHICH directory is gone.
	refusal := asRefusal(t, err, ArmSpawnFailed)
	if !strings.Contains(refusal.Reason, ws.Dir) {
		t.Fatalf("refusal reason = %q, want it to name %q", refusal.Reason, ws.Dir)
	}
}

func TestOpenStartsNoSessionWhenTheDirectoryIsGone(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", filepath.Join(t.TempDir(), "gone"))

	// Act.
	_ = f.verbs.Open(context.Background(), "w1", nil)

	// Assert: a shim with no working tree is never spawned.
	if len(f.fleet.started) != 0 {
		t.Fatalf("sessions started = %d, want none for a missing directory", len(f.fleet.started))
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
