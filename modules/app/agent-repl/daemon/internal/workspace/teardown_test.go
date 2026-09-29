package workspace

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"testing"

	"claude-repld/internal/wsm"
)

func TestKillForcesTheSessionDeath(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Kill(context.Background(), "w1"); err != nil {
		t.Fatalf("Kill: %v", err)
	}

	// Assert.
	if len(f.shim.killedSession) != 1 || !f.shim.killedSession[0] {
		t.Fatalf("KillSession forces = %v, want exactly one forced kill", f.shim.killedSession)
	}
}

func TestKillStopsTheShimEvenWhenTheSessionWillNotAnswer(t *testing.T) {
	// Arrange: a shim that refuses the forced kill is not a reason to leave the
	// workspace alive.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.shim.killSessionErr = errors.New("the query refused to end")

	// Act.
	if err := f.verbs.Kill(context.Background(), "w1"); err != nil {
		t.Fatalf("Kill: %v", err)
	}

	// Assert.
	if len(f.fleet.stopped) != 1 || !f.fleet.stopped[0].Force {
		t.Fatalf("stopped sessions = %+v, want one forced stop", f.fleet.stopped)
	}
}

func TestKillClosesTheOrphanedTurns(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Kill(context.Background(), "w1"); err != nil {
		t.Fatalf("Kill: %v", err)
	}

	// Assert: the terminal is REHYDRATABLE, so the record says how it died.
	terminal, ok := f.db.terminals["w1"]
	if !ok || terminal.Kind != "killed" {
		t.Fatalf("session terminal = %+v, want a killed terminal", terminal)
	}
}

func TestKillMarksTheRosterRowClosed(t *testing.T) {
	// Arrange: Emacs derives its tab set from closed = true.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Kill(context.Background(), "w1"); err != nil {
		t.Fatalf("Kill: %v", err)
	}

	// Assert.
	if !f.db.closedFlags["w1"] {
		t.Fatal("Kill() did not mark the roster row closed")
	}
}

func TestKillLeavesTheWorkspaceRecordInPlace(t *testing.T) {
	// Arrange: Kill destroys no data.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Kill(context.Background(), "w1"); err != nil {
		t.Fatalf("Kill: %v", err)
	}

	// Assert.
	if len(f.db.forgotten) != 0 {
		t.Fatalf("forgotten workspaces = %v, want none", f.db.forgotten)
	}
}

func TestKillWithNoLiveSessionStillStopsTheShim(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.hasSession = false

	// Act.
	if err := f.verbs.Kill(context.Background(), "w1"); err != nil {
		t.Fatalf("Kill: %v", err)
	}

	// Assert.
	if len(f.shim.killedSession) != 0 {
		t.Fatalf("KillSession calls = %v, want none without a live session", f.shim.killedSession)
	}
}

// TestKillWithoutASessionRowSucceedsAndTearsDown covers the freshly-opened
// workspace whose session bring-up never ran: it has a roster row but no
// session, so recording a terminal answers wsm.ErrNotFound. Killing it must
// still succeed and tear the workspace down -- there is no session to
// terminate.
func TestKillWithoutASessionRowSucceedsAndTearsDown(t *testing.T) {
	// Arrange: no session row, so the terminal write answers the wsm not-found
	// sentinel exactly as the store does.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.setTerminalErr = fmt.Errorf("wsm: session for workspace %s: %w", "w1", wsm.ErrNotFound)

	// Act.
	if err := f.verbs.Kill(context.Background(), "w1"); err != nil {
		t.Fatalf("Kill: %v", err)
	}

	// Assert: the teardown still ran -- the roster row is closed, no terminal
	// was recorded for a session that never existed, and the record survives.
	if !f.db.closedFlags["w1"] {
		t.Fatal("Kill() did not mark the roster row closed for a session-less workspace")
	}
	if _, ok := f.db.terminals["w1"]; ok {
		t.Fatalf("terminal recorded = %+v, want none for a session-less workspace", f.db.terminals["w1"])
	}
	if len(f.db.forgotten) != 0 {
		t.Fatalf("forgotten workspaces = %v, want none", f.db.forgotten)
	}
}

// TestKillWithASessionRecordsTheTerminal is the regression lock: a workspace
// that HAS a session still records its killed terminal exactly as before.
func TestKillWithASessionRecordsTheTerminal(t *testing.T) {
	// Arrange: the default fixture has a session and no terminal-write failure.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Kill(context.Background(), "w1"); err != nil {
		t.Fatalf("Kill: %v", err)
	}

	// Assert.
	terminal, ok := f.db.terminals["w1"]
	if !ok || terminal.Kind != "killed" {
		t.Fatalf("session terminal = %+v, want a killed terminal recorded", terminal)
	}
}

// TestKillFailsWhenTheTerminalWriteFailsForAnExistingSession locks the error
// handling: only the not-found sentinel is benign. A real failure recording a
// terminal for a session that DOES exist still fails the kill.
func TestKillFailsWhenTheTerminalWriteFailsForAnExistingSession(t *testing.T) {
	// Arrange: a non-not-found failure from the terminal write.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.setTerminalErr = errFake

	// Act.
	err := f.verbs.Kill(context.Background(), "w1")

	// Assert.
	if err == nil {
		t.Fatal("Kill() = nil error, want the terminal-write failure surfaced")
	}
	if !strings.Contains(err.Error(), "record the terminal") {
		t.Fatalf("Kill() error = %v, want it to name the terminal record failure", err)
	}
}

func TestNukeDestroysTheWorktreeAndTheBranch(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Nuke(context.Background(), "w1"); err != nil {
		t.Fatalf("Nuke: %v", err)
	}

	// Assert.
	if len(f.git.nuked) != 1 {
		t.Fatalf("nuked worktrees = %+v, want exactly one", f.git.nuked)
	}
	if f.git.nuked[0].WorktreeDir != ws.Dir || f.git.nuked[0].Branch != ws.Branch {
		t.Fatalf("nuked %+v, want the workspace's own dir and branch", f.git.nuked[0])
	}
}

func TestNukeLeavesTheRoster(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Nuke(context.Background(), "w1"); err != nil {
		t.Fatalf("Nuke: %v", err)
	}

	// Assert.
	if len(f.db.forgotten) != 1 || f.db.forgotten[0] != "w1" {
		t.Fatalf("forgotten workspaces = %v, want w1", f.db.forgotten)
	}
}

func TestNukeKillsALiveSessionFirst(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.fleet.live["w1"] = true

	// Act.
	if err := f.verbs.Nuke(context.Background(), "w1"); err != nil {
		t.Fatalf("Nuke: %v", err)
	}

	// Assert.
	if len(f.shim.killedSession) != 1 {
		t.Fatalf("KillSession calls = %v, want one before the nuke", f.shim.killedSession)
	}
}

func TestNukeForgetsNothingWhenTheDestructionFails(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.git.nukeErr = errors.New("the worktree is locked")

	// Act.
	err := f.verbs.Nuke(context.Background(), "w1")

	// Assert.
	if err == nil {
		t.Fatal("Nuke() = nil error, want the git failure surfaced")
	}
	if len(f.db.forgotten) != 0 {
		t.Fatalf("forgotten workspaces = %v, want none after a failed destruction", f.db.forgotten)
	}
}

func TestNukeRefusesAnUnregisteredRepository(t *testing.T) {
	// Arrange: the repository the worktree belongs to is not in the registry.
	f := newFixture(t)
	f.db.with(f.workspace("w1", t.TempDir()))
	f.db.repositories = nil

	// Act.
	err := f.verbs.Nuke(context.Background(), "w1")

	// Assert.
	if err == nil {
		t.Fatal("Nuke() = nil error, want the unregistered repository surfaced")
	}
}

// TestNukeAnswersGitFailedWithGitsOwnAccount covers the arm the contract spells
// for a destruction git would not perform: the caller learns what could not be
// destroyed, not merely that something went wrong.
func TestNukeAnswersGitFailedWithGitsOwnAccount(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.git.nukeErr = errors.New("fatal: unable to remove worktree")

	// Act.
	err := f.verbs.Nuke(context.Background(), "w1")

	// Assert.
	var refusal *Refusal
	if !errors.As(err, &refusal) {
		t.Fatalf("Nuke() = %v, want a *Refusal naming git_failed", err)
	}
	if refusal.Arm != ArmGitFailed {
		t.Fatalf("refusal arm = %q, want %q", refusal.Arm, ArmGitFailed)
	}
	if !strings.Contains(refusal.Reason, "unable to remove worktree") {
		t.Fatalf("refusal reason = %q, want git's own account", refusal.Reason)
	}
}

// TestKillAbandonsTheWorkspacesWaitingMerge covers the one door a queued
// merge's workspace can leave through: Close refuses while a merge is queued,
// but Kill never blocks, so the merge has to be told the workspace is gone.
func TestKillAbandonsTheWorkspacesWaitingMerge(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Kill(context.Background(), "w1"); err != nil {
		t.Fatalf("Kill: %v", err)
	}

	// Assert.
	if len(f.merge.closed) != 1 || f.merge.closed[0] != "w1" {
		t.Fatalf("merges told of the close = %v, want exactly the killed workspace", f.merge.closed)
	}
}

// TestNukeAbandonsTheWorkspacesWaitingMerge is the same for the nuke, which
// destroys the very worktree the merge would have run against.
func TestNukeAbandonsTheWorkspacesWaitingMerge(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Nuke(context.Background(), "w1"); err != nil {
		t.Fatalf("Nuke: %v", err)
	}

	// Assert.
	if len(f.merge.closed) != 1 || f.merge.closed[0] != "w1" {
		t.Fatalf("merges told of the nuke = %v, want exactly the nuked workspace", f.merge.closed)
	}
}

// TestKillArmsTheStandDownLatchBeforeTheForcedKill covers the order the
// unconditional process stop depends on: the stop after a kill that did not
// answer is a teardown this daemon ordered, and the latch is what says so to
// every side that later sees the shim go.
func TestKillArmsTheStandDownLatchBeforeTheForcedKill(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Kill(context.Background(), "w1"); err != nil {
		t.Fatalf("Kill: %v", err)
	}

	// Assert.
	if !f.shim.standDownBeforeKill {
		t.Fatal("the stand-down latch was not armed before the forced KillSession")
	}
}

// TestKillRecordsTheUnansweredSessionKillByWhoOrderedIt covers the record: an
// unanswered kill inside a stand-down this daemon ordered is the escalation
// working and is INFO, while one outside such a stand-down -- a DETACHED
// client, whose process is the successor daemon's -- stays a WARN.
func TestKillRecordsTheUnansweredSessionKillByWhoOrderedIt(t *testing.T) {
	tests := []struct {
		name      string
		refused   bool
		wantLevel string
	}{
		{name: "the daemon ordered this stand-down", refused: false, wantLevel: "info"},
		{name: "the kill is outside a stand-down this daemon ordered", refused: true, wantLevel: "warn"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.shim.standDownRefused = tt.refused
			f.shim.killSessionErr = errors.New("the shim never answered")

			// Act.
			if err := f.verbs.Kill(context.Background(), "w1"); err != nil {
				t.Fatalf("Kill: %v", err)
			}

			// Assert.
			for _, record := range f.log.logger.Records() {
				if record.Message != "the forced KillSession did not answer" {
					continue
				}
				if record.Level != tt.wantLevel {
					t.Fatalf("the unanswered kill was recorded at %q, want %q", record.Level, tt.wantLevel)
				}
				return
			}
			t.Fatalf("records = %+v, want the unanswered kill recorded at %s", f.log.logger.Records(), tt.wantLevel)
		})
	}
}

// ---- the fast half: the workspace is closed before its teardown runs ----

// beginVerbs are the two verbs with a fast half, run against one fixture.
var beginVerbs = []struct {
	name  string
	begin func(f *fixture) (Teardown, error)
}{
	{name: "kill", begin: func(f *fixture) (Teardown, error) { return f.verbs.BeginKill(context.Background(), "w1") }},
	{name: "nuke", begin: func(f *fixture) (Teardown, error) { return f.verbs.BeginNuke(context.Background(), "w1") }},
}

func TestBeginMarksTheWorkspaceClosedBeforeItsTeardownRuns(t *testing.T) {
	for _, tt := range beginVerbs {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.fleet.live["w1"] = true

			// Act.
			teardown, err := tt.begin(f)

			// Assert.
			if err != nil || teardown == nil {
				t.Fatalf("begin = (%v, %v), want a teardown", teardown != nil, err)
			}
			if !f.db.closedFlags["w1"] {
				t.Fatal("the workspace is not closed after the fast half")
			}
			if len(f.sidebar.registries) != 1 {
				t.Fatalf("roster republishes = %d, want the close published at once", len(f.sidebar.registries))
			}
			if len(f.shim.killedSession) != 0 || len(f.fleet.stopped) != 0 || len(f.git.nuked) != 0 {
				t.Fatalf("the fast half tore something down: kills=%v stops=%v nuked=%v",
					f.shim.killedSession, f.fleet.stopped, f.git.nuked)
			}
			if len(f.merge.closed) != 1 {
				t.Fatalf("merges dropped = %v, want the queued merge dropped", f.merge.closed)
			}
		})
	}
}

func TestTheTeardownKillsTheSessionTheFastHalfLeftAlive(t *testing.T) {
	for _, tt := range beginVerbs {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.fleet.live["w1"] = true
			teardown, err := tt.begin(f)
			if err != nil {
				t.Fatalf("begin: %v", err)
			}

			// Act.
			if err := teardown(context.Background()); err != nil {
				t.Fatalf("teardown: %v", err)
			}

			// Assert.
			if len(f.shim.killedSession) != 1 {
				t.Fatalf("KillSession calls = %v, want the session killed by the teardown", f.shim.killedSession)
			}
		})
	}
}

func TestBeginRefusesAWorkspaceThatIsNotThisDaemonsAndClosesNothing(t *testing.T) {
	for _, tt := range beginVerbs {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.owner.standing = StandingTransferringAway

			// Act.
			teardown, err := tt.begin(f)

			// Assert.
			if err == nil || teardown != nil {
				t.Fatalf("begin = (%v, %v), want a refusal and no teardown", teardown != nil, err)
			}
			if _, set := f.db.closedFlags["w1"]; set {
				t.Fatal("a refused verb marked the workspace closed")
			}
			if len(f.merge.closed) != 0 {
				t.Fatalf("merges dropped = %v, want none for a refused verb", f.merge.closed)
			}
		})
	}
}

func TestBeginFailsLoudlyWhenTheCloseCannotBeRecorded(t *testing.T) {
	for _, tt := range beginVerbs {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.db.setClosedErr = errFake

			// Act.
			teardown, err := tt.begin(f)

			// Assert.
			if err == nil || teardown != nil {
				t.Fatalf("begin = (%v, %v), want the failure and no teardown", teardown != nil, err)
			}
			if !strings.Contains(err.Error(), "record closed") {
				t.Fatalf("error = %v, want it to name the close record", err)
			}
			awaitRecord(t, f, "error", "daemon.workspace."+tt.name)
			if len(f.sidebar.registries) != 0 {
				t.Fatalf("roster republishes = %d, want none for an unrecorded close", len(f.sidebar.registries))
			}
		})
	}
}
