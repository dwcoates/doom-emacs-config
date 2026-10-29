package workspace

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/wsm"
)

// standingWorkspace is a fixture whose workspace w1, created, is owned by
// ws-owner.
func standingWorkspace(t *testing.T) *fixture {
	t.Helper()
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir())
	owner := ids.WorkspaceID("ws-owner")
	ws.Parent = &owner
	f.db.with(ws)
	f.db.jobs["w1"] = wsm.CreationJob{Workspace: "w1"}
	oneShotBriefs(f)
	return f
}

func idOf(id ids.WorkspaceID) *ids.WorkspaceID { return &id }

func TestSetOneShotMarksAndAnswersTheDirective(t *testing.T) {
	tests := []struct {
		name  string
		asker *ids.WorkspaceID
	}{
		{name: "the user", asker: nil},
		{name: "the workspace itself", asker: idOf("w1")},
		{name: "its owner", asker: idOf("ws-owner")},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := standingWorkspace(t)

			// Act
			directive, err := f.verbs.SetOneShot(context.Background(), "w1", tt.asker, true)

			// Assert
			if err != nil || directive != completionDirectiveLead+"DIRECTIVE" {
				t.Fatalf("SetOneShot = %q, %v; want the completion directive", directive, err)
			}
			if !f.db.jobs["w1"].OneShot {
				t.Error("the creation job is not marked a one-shot")
			}
		})
	}
}

func TestSetOneShotUnmarksWithNoDirective(t *testing.T) {
	// Arrange
	f := standingWorkspace(t)
	job := f.db.jobs["w1"]
	job.OneShot = true
	f.db.jobs["w1"] = job

	// Act
	directive, err := f.verbs.SetOneShot(context.Background(), "w1", nil, false)

	// Assert
	if err != nil || directive != "" || f.db.jobs["w1"].OneShot {
		t.Fatalf("SetOneShot = %q, %v, one-shot %v; want unmarked with no directive", directive, err, f.db.jobs["w1"].OneShot)
	}
}

func TestSetOneShotRefuses(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fixture)
		asker   *ids.WorkspaceID
		arm     string
	}{
		{name: "a closed workspace", arrange: func(f *fixture) {
			ws := f.db.workspaces["w1"]
			ws.Closed = true
			f.db.with(ws)
		}, arm: ArmClosed},
		{name: "a stranger asking", arrange: func(*fixture) {}, asker: idOf("ws-stranger"), arm: ArmRequesterMayNotSet},
		{name: "a child its owner lands", arrange: func(f *fixture) {
			ws := f.db.workspaces["w1"]
			ws.OwnerLands = true
			f.db.with(ws)
		}, arm: ArmOwnerLands},
		{name: "a registered workspace", arrange: func(f *fixture) { delete(f.db.jobs, "w1") }, arm: ArmNoCreationJob},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := standingWorkspace(t)
			tt.arrange(f)

			// Act
			_, err := f.verbs.SetOneShot(context.Background(), "w1", tt.asker, true)

			// Assert
			asRefusal(t, err, tt.arm)
			if f.db.jobs["w1"].OneShot {
				t.Error("a refused mark marked the workspace")
			}
		})
	}
}

func TestSetOneShotFailsHardWhenTheMarkCannotBeWritten(t *testing.T) {
	// Arrange
	f := standingWorkspace(t)
	f.db.setOneShotErr = errors.New("db gone")

	// Act
	_, err := f.verbs.SetOneShot(context.Background(), "w1", nil, true)

	// Assert
	if _, refused := AsRefusal(err); err == nil || refused {
		t.Fatalf("SetOneShot = %v, want a hard failure", err)
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opSetOneShot,
		Message: "could not record the workspace's one-shot standing"}); !ok {
		t.Errorf("no error record: %+v", f.log.logger.Records())
	}
}

func TestSetOneShotFailsHardWhenTheDaemonsDirectiveCannotBeRead(t *testing.T) {
	// Arrange: the daemon's own corpus has no completion directive.
	f := standingWorkspace(t)
	delete(f.briefs, BriefOneShotCompletionDirective)

	// Act
	_, err := f.verbs.SetOneShot(context.Background(), "w1", nil, true)

	// Assert
	if _, refused := AsRefusal(err); err == nil || refused {
		t.Fatalf("SetOneShot = %v, want a hard failure", err)
	}
	if f.db.jobs["w1"].OneShot {
		t.Error("a failed mark marked the workspace")
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opSetOneShot,
		Message: "could not read the daemon's one-shot completion directive"}); !ok {
		t.Errorf("no error record: %+v", f.log.logger.Records())
	}
}

// markedOneShot is standingWorkspace with w1 marked a one-shot.
func markedOneShot(t *testing.T) *fixture {
	t.Helper()
	f := standingWorkspace(t)
	if err := f.db.SetOneShot(context.Background(), "w1", true); err != nil {
		t.Fatalf("mark: %v", err)
	}
	return f
}

func TestTakeBackOneShotClearsTheMarkAndRecordsWhatClearedIt(t *testing.T) {
	for _, by := range []OneShotTakeBack{TakenBackByUserPrompt, TakenBackByReopen} {
		t.Run(string(by), func(t *testing.T) {
			// Arrange
			f := markedOneShot(t)

			// Act
			_, err := f.verbs.TakeBackOneShot(context.Background(), "w1", by)

			// Assert
			if err != nil || f.db.jobs["w1"].OneShot {
				t.Fatalf("TakeBackOneShot = %v, one-shot %v; want the mark cleared", err, f.db.jobs["w1"].OneShot)
			}
			record, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "info", Operation: opTakeBackOneShot,
				Message: "the user took the one-shot back; its mark is cleared"})
			if !ok || record.Context["cleared_by"] != string(by) || record.Context["workspace"] != "w1" {
				t.Errorf("info record = %+v, want one naming w1 and %s", record, by)
			}
		})
	}
}

func TestTakeBackOneShotRepublishesTheRoster(t *testing.T) {
	// Arrange
	f := markedOneShot(t)
	before := len(f.sidebar.registries)

	// Act
	_, err := f.verbs.TakeBackOneShot(context.Background(), "w1", TakenBackByUserPrompt)

	// Assert
	if err != nil || len(f.sidebar.registries) == before {
		t.Fatalf("TakeBackOneShot = %v; the roster was not republished", err)
	}
}

func TestTakeBackOneShotLeavesAnUnmarkedWorkspaceAlone(t *testing.T) {
	// Arrange
	f := standingWorkspace(t)
	f.db.setOneShotErr = errors.New("no write was expected")

	// Act
	_, err := f.verbs.TakeBackOneShot(context.Background(), "w1", TakenBackByUserPrompt)

	// Assert
	if err != nil {
		t.Fatalf("TakeBackOneShot = %v, want nothing written", err)
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Operation: opTakeBackOneShot}); ok {
		t.Error("an unmarked workspace recorded a take-back")
	}
}

func TestTakeBackOneShotLeavesAClosedWorkspacesMark(t *testing.T) {
	// Arrange -- a finished one-shot keeps its mark until it is reopened.
	f := markedOneShot(t)
	ws := f.db.workspaces["w1"]
	ws.Closed = true
	f.db.workspaces["w1"] = ws

	// Act
	_, err := f.verbs.TakeBackOneShot(context.Background(), "w1", TakenBackByUserPrompt)

	// Assert
	if err != nil || !f.db.jobs["w1"].OneShot {
		t.Fatalf("TakeBackOneShot = %v, one-shot %v; want the closed workspace's mark kept", err, f.db.jobs["w1"].OneShot)
	}
}

func TestTakeBackOneShotFailsHardWhenTheMarkCannotBeCleared(t *testing.T) {
	// Arrange
	f := markedOneShot(t)
	f.db.setOneShotErr = errors.New("db gone")

	// Act
	_, err := f.verbs.TakeBackOneShot(context.Background(), "w1", TakenBackByReopen)

	// Assert
	if _, refused := AsRefusal(err); err == nil || refused {
		t.Fatalf("TakeBackOneShot = %v, want a hard failure", err)
	}
	record, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opTakeBackOneShot,
		Message: "could not clear the one-shot mark of a workspace the user took back"})
	if !ok || record.Context["cleared_by"] != string(TakenBackByReopen) {
		t.Errorf("error record = %+v, want one naming the reopen", record)
	}
}

func TestSetCorrespondenceMarksAndClears(t *testing.T) {
	// Arrange
	f := standingWorkspace(t)
	f.db.with(wsm.Workspace{ID: "w2", Name: "planner"})

	// Act
	markErr := f.verbs.SetCorrespondence(context.Background(), "w1", idOf("w1"), idOf("w2"))
	marked := f.db.workspaces["w1"].Awaiting
	drawnMarked := f.footer.awaiting["w1"]
	clearErr := f.verbs.SetCorrespondence(context.Background(), "w1", nil, nil)

	// Assert
	if markErr != nil || clearErr != nil {
		t.Fatalf("SetCorrespondence = %v, %v", markErr, clearErr)
	}
	if marked == nil || *marked != "w2" || drawnMarked != "planner" {
		t.Errorf("marked %v drawn %q, want w2 drawn as planner", marked, drawnMarked)
	}
	if f.db.workspaces["w1"].Awaiting != nil || f.footer.awaiting["w1"] != "" {
		t.Errorf("after clearing: %v, drawn %q; want nothing", f.db.workspaces["w1"].Awaiting, f.footer.awaiting["w1"])
	}
}

func TestSetCorrespondenceRefuses(t *testing.T) {
	tests := []struct {
		name    string
		asker   *ids.WorkspaceID
		awaited *ids.WorkspaceID
		arm     string
	}{
		{name: "its owner asking", asker: idOf("ws-owner"), awaited: idOf("w2"), arm: ArmRequesterMayNotSet},
		{name: "awaiting nobody registered", awaited: idOf("ws-none"), arm: ArmUnknownAwaited},
		{name: "awaiting itself", awaited: idOf("w1"), arm: ArmUnknownAwaited},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := standingWorkspace(t)
			f.db.with(wsm.Workspace{ID: "w2", Name: "planner"})

			// Act
			err := f.verbs.SetCorrespondence(context.Background(), "w1", tt.asker, tt.awaited)

			// Assert
			asRefusal(t, err, tt.arm)
			if f.db.workspaces["w1"].Awaiting != nil {
				t.Error("a refused mark marked the workspace")
			}
		})
	}
}

func TestRegisteringAWorkspaceThatAwaitsDrawsItInTheBackground(t *testing.T) {
	// Arrange
	f := standingWorkspace(t)
	f.db.with(wsm.Workspace{ID: "w2", Name: "planner"})
	v := f.verbs.(*verbs)
	record := f.db.workspaces["w1"]
	record.Awaiting = idOf("w2")

	// Act
	err := v.restoreAwaiting(context.Background(), dlog.NewTestLogger(), record)

	// Assert
	if err != nil || f.footer.awaiting["w1"] != "planner" {
		t.Fatalf("restoreAwaiting = %v, drawn %q; want planner", err, f.footer.awaiting["w1"])
	}
}

// rootOneShot is a fixture whose top-level workspace w1, created, is marked a
// one-shot in a repository with a subsection, and the placement brief is
// arranged.
func rootOneShot(t *testing.T) *fixture {
	t.Helper()
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir())
	f.db.jobs["w1"] = wsm.CreationJob{Workspace: "w1"}
	oneShotBriefs(f)
	placementBrief(f)
	f.db.subsections = []wsm.Subsection{{ID: "sub-1", Repo: ws.Repo, Name: "later", Place: 1}}
	if err := f.db.SetOneShot(context.Background(), "w1", true); err != nil {
		t.Fatalf("mark: %v", err)
	}
	return f
}

// placementSubmissions are the placement briefs the fixture's queue accepted.
func placementSubmissions(f *fixture) []promptqueue.Submission {
	var out []promptqueue.Submission
	for _, sub := range f.queue.submissions {
		if sub.Origin == conversationv1.PromptOrigin_PROMPT_ORIGIN_SUBSECTION_PLACEMENT {
			out = append(out, sub)
		}
	}
	return out
}

// takeBackAndSubmit takes w1 back for BY and queues the placement it owes,
// as the take-back's callers do.
func takeBackAndSubmit(f *fixture, by OneShotTakeBack) error {
	placement, err := f.verbs.TakeBackOneShot(context.Background(), "w1", by)
	if err != nil {
		return err
	}
	return f.verbs.SubmitPlacement(context.Background(), placement)
}

func TestTakeBackOneShotHandsThePlacementBackUnqueued(t *testing.T) {
	// Arrange
	f := rootOneShot(t)

	// Act
	placement, err := f.verbs.TakeBackOneShot(context.Background(), "w1", TakenBackByUserPrompt)

	// Assert
	if err != nil || !placement.Owed() {
		t.Fatalf("TakeBackOneShot = %+v, %v; want an owed placement", placement, err)
	}
	if subs := placementSubmissions(f); len(subs) != 0 {
		t.Fatalf("placement briefs = %+v, want none queued before the caller asks", subs)
	}
}

func TestSubmitPlacementQueuesNothingForANoneOwed(t *testing.T) {
	// Arrange
	f := rootOneShot(t)
	f.queue.submitErr = errors.New("no submission was expected")

	// Act
	err := f.verbs.SubmitPlacement(context.Background(), PendingPlacement{})

	// Assert
	if err != nil {
		t.Fatalf("SubmitPlacement = %v, want nothing done", err)
	}
}

func TestLeavingTheOneShotStateAsksTheWorkspaceToFileItself(t *testing.T) {
	tests := []struct {
		name  string
		leave func(f *fixture) error
	}{
		{name: "its agent unmarks it", leave: func(f *fixture) error {
			_, err := f.verbs.SetOneShot(context.Background(), "w1", idOf("w1"), false)
			return err
		}},
		{name: "the user's prompt takes it back", leave: func(f *fixture) error {
			return takeBackAndSubmit(f, TakenBackByUserPrompt)
		}},
		{name: "a reopen takes it back", leave: func(f *fixture) error {
			return takeBackAndSubmit(f, TakenBackByReopen)
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := rootOneShot(t)

			// Act
			err := tt.leave(f)

			// Assert
			if err != nil {
				t.Fatalf("leave: %v", err)
			}
			subs := placementSubmissions(f)
			if len(subs) != 1 {
				t.Fatalf("placement briefs = %d, want one", len(subs))
			}
			if subs[0].WS != "w1" || subs[0].Delivery != wsm.DeliveryDeferred || subs[0].Said.GetContent().GetBlocks()[0].GetText().GetText() != "PLACE" {
				t.Fatalf("placement brief = %+v, want a deferred PLACE to w1", subs[0])
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opOneShotExitPlacement}); !ok {
				t.Error("the placement brief was not recorded")
			}
		})
	}
}

func TestStayingOrBecomingAOneShotAsksNoPlacement(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T) *fixture
		oneShot bool
	}{
		{name: "marking a workspace", arrange: func(t *testing.T) *fixture {
			f := rootOneShot(t)
			if err := f.db.SetOneShot(context.Background(), "w1", false); err != nil {
				t.Fatalf("unmark: %v", err)
			}
			return f
		}, oneShot: true},
		{name: "unmarking a workspace that is none", arrange: func(t *testing.T) *fixture {
			f := rootOneShot(t)
			if err := f.db.SetOneShot(context.Background(), "w1", false); err != nil {
				t.Fatalf("unmark: %v", err)
			}
			return f
		}},
		{name: "a child leaving its one-shot state", arrange: func(t *testing.T) *fixture {
			f := rootOneShot(t)
			ws := f.db.workspaces["w1"]
			ws.Parent = idOf("ws-owner")
			f.db.with(ws)
			return f
		}},
		{name: "a repository with nothing to file into", arrange: func(t *testing.T) *fixture {
			f := rootOneShot(t)
			f.db.subsections = nil
			return f
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := tt.arrange(t)

			// Act
			_, err := f.verbs.SetOneShot(context.Background(), "w1", nil, tt.oneShot)

			// Assert
			if err != nil {
				t.Fatalf("SetOneShot: %v", err)
			}
			if subs := placementSubmissions(f); len(subs) != 0 {
				t.Fatalf("placement briefs = %+v, want none", subs)
			}
		})
	}
}

func TestLeavingTheOneShotStateFailsLoudlyWhenThePlacementCannotBeAsked(t *testing.T) {
	tests := []struct {
		name     string
		arrange  func(f *fixture)
		wantMark bool
		message  string
	}{
		{name: "the brief is missing", arrange: func(f *fixture) { delete(f.briefs, BriefSubsectionPlacement) },
			wantMark: true, message: "could not read a create note's brief"},
		{name: "the arrangement cannot be read", arrange: func(f *fixture) { f.db.readArrangementErr = errors.New("the store is gone") },
			wantMark: true, message: "could not read the arrangement a new workspace could file itself into"},
		{name: "the queue fails the brief", arrange: func(f *fixture) { f.queue.submitErr = errors.New("the queue is gone") },
			message: "the workspace's queue did not accept its placement brief"},
		{name: "the queue refuses the brief", arrange: func(f *fixture) { f.queue.submitRefusedArm = "merge_in_progress" },
			message: "the workspace's queue refused its placement brief"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := rootOneShot(t)
			tt.arrange(f)

			// Act
			_, err := f.verbs.SetOneShot(context.Background(), "w1", idOf("w1"), false)

			// Assert
			if err == nil {
				t.Fatal("SetOneShot succeeded without asking for the placement")
			}
			if got := f.db.jobs["w1"].OneShot; got != tt.wantMark {
				t.Fatalf("one-shot = %v, want %v", got, tt.wantMark)
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Message: tt.message}); !ok {
				t.Fatalf("no error record %q: %+v", tt.message, f.log.logger.Records())
			}
		})
	}
}

func TestTakeBackOneShotKeepsTheMarkForThePromptAfterAStop(t *testing.T) {
	// Arrange
	f := markedOneShot(t)
	runningTurn(f, 0)
	if _, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{Turn: true}, false); err != nil {
		t.Fatalf("Interrupt: %v", err)
	}

	// Act
	_, err := f.verbs.TakeBackOneShot(context.Background(), "w1", TakenBackByUserPrompt)

	// Assert
	if err != nil || !f.db.jobs["w1"].OneShot {
		t.Fatalf("TakeBackOneShot = %v, one-shot %v; want the mark kept", err, f.db.jobs["w1"].OneShot)
	}
}

func TestTakeBackOneShotTakesBackOnTheSecondPromptAfterAStop(t *testing.T) {
	// Arrange
	f := markedOneShot(t)
	runningTurn(f, 0)
	if _, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{Turn: true}, false); err != nil {
		t.Fatalf("Interrupt: %v", err)
	}
	if _, err := f.verbs.TakeBackOneShot(context.Background(), "w1", TakenBackByUserPrompt); err != nil {
		t.Fatalf("first TakeBackOneShot: %v", err)
	}

	// Act
	_, err := f.verbs.TakeBackOneShot(context.Background(), "w1", TakenBackByUserPrompt)

	// Assert
	if err != nil || f.db.jobs["w1"].OneShot {
		t.Fatalf("TakeBackOneShot = %v, one-shot %v; want the mark cleared", err, f.db.jobs["w1"].OneShot)
	}
}

func TestTakeBackOneShotByReopenIgnoresAStop(t *testing.T) {
	// Arrange
	f := markedOneShot(t)
	runningTurn(f, 0)
	if _, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{Turn: true}, false); err != nil {
		t.Fatalf("Interrupt: %v", err)
	}

	// Act
	_, err := f.verbs.TakeBackOneShot(context.Background(), "w1", TakenBackByReopen)

	// Assert
	if err != nil || f.db.jobs["w1"].OneShot {
		t.Fatalf("TakeBackOneShot = %v, one-shot %v; want the mark cleared", err, f.db.jobs["w1"].OneShot)
	}
}
