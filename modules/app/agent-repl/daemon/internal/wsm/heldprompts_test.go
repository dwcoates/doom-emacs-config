package wsm

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/feedid"
)

func TestPutHeldPromptRoundTripsTheWholeSubmission(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := NewTurnID()

	// Act
	if err := s.PutHeldPrompt(context.Background(), HeldPrompt{
		Workspace: ws.ID, Turn: turn, Said: said("what the user typed"), Origin: "webapp", QueuedAt: instant,
	}); err != nil {
		t.Fatalf("PutHeldPrompt: %v", err)
	}
	got, err := s.HeldPrompts(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("HeldPrompts: %v", err)
	}
	if len(got) != 1 {
		t.Fatalf("loaded %d holds, want 1", len(got))
	}
	if firstText(got[0].Said) != "what the user typed" {
		t.Fatalf("said = %q, want the submission verbatim", firstText(got[0].Said))
	}
	if got[0].Origin != "webapp" || !got[0].QueuedAt.Equal(instant) {
		t.Fatalf("hold = %+v, want the recorded facts", got[0])
	}
}

func TestPutHeldPromptPreservesAnAttachedImage(t *testing.T) {
	// Arrange — a text-only column would drop this block, which is why the
	// record is the whole serialized submission.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	submission := said("look at this")
	submission.Content.Blocks = append(submission.Content.Blocks, &conversationv1.UserContentBlock{
		Block: &conversationv1.UserContentBlock_Image{Image: &conversationv1.ImageBlock{}},
	})

	// Act
	if err := s.PutHeldPrompt(context.Background(), HeldPrompt{
		Workspace: ws.ID, Turn: NewTurnID(), Said: submission, Origin: "webapp", QueuedAt: instant,
	}); err != nil {
		t.Fatalf("PutHeldPrompt: %v", err)
	}
	got, err := s.HeldPrompts(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("HeldPrompts: %v", err)
	}
	if n := len(got[0].Said.GetContent().GetBlocks()); n != 2 {
		t.Fatalf("restored %d blocks, want 2", n)
	}
}

func TestPutHeldPromptRefusesAMissingSubmission(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.PutHeldPrompt(context.Background(), HeldPrompt{Workspace: ws.ID, Turn: NewTurnID(), Origin: "webapp", QueuedAt: instant})

	// Assert
	if err == nil {
		t.Fatalf("PutHeldPrompt with no submission succeeded")
	}
	if !loggedOperation(log, "daemon.wsm.put_held_prompt", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
}

func TestPutHeldPromptRoundTripsTheComposerTarget(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	agent := &conversationv1.AgentId{Value: "agent-7"}
	target := feedid.Ref{
		WS:   ws.ID,
		Feed: feedid.Feed{Agent: agent},
		Row:  feedid.RowKey{Kind: feedid.KindActivity, ID: "unit-1", Sub: "agent-7"},
	}

	// Act
	if err := s.PutHeldPrompt(context.Background(), HeldPrompt{
		Workspace: ws.ID, Turn: NewTurnID(), Said: said("addressed"), Origin: "bubble", Target: &target, QueuedAt: instant,
	}); err != nil {
		t.Fatalf("PutHeldPrompt: %v", err)
	}
	got, err := s.HeldPrompts(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("HeldPrompts: %v", err)
	}
	restored := got[0].Target
	if restored == nil || restored.Feed.Agent.GetValue() != "agent-7" || restored.Row.ID != "unit-1" || restored.Row.Sub != "agent-7" {
		t.Fatalf("target = %+v, want the addressed row", restored)
	}
}

func TestPutHeldPromptRequiresAScheduleForAShutdownHold(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	hold := HoldShutdown

	// Act
	err := s.PutHeldPrompt(context.Background(), HeldPrompt{
		Workspace: ws.ID, Turn: NewTurnID(), Said: said("x"), Origin: "webapp", Hold: &hold, QueuedAt: instant,
	})

	// Assert
	if err == nil {
		t.Fatalf("a shutdown hold with no schedule id succeeded")
	}
	if !loggedOperation(log, "daemon.wsm.put_held_prompt", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
}

func TestPutHeldPromptRefusesAScheduleWithNoHold(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.PutHeldPrompt(context.Background(), HeldPrompt{
		Workspace: ws.ID, Turn: NewTurnID(), Said: said("x"), Origin: "webapp", ScheduleID: "drain-1", QueuedAt: instant,
	})

	// Assert
	if err == nil {
		t.Fatalf("a schedule id with no hold kind succeeded")
	}
}

func TestPutHeldPromptRoundTripsAShutdownHold(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	hold := HoldShutdown

	// Act
	if err := s.PutHeldPrompt(context.Background(), HeldPrompt{
		Workspace: ws.ID, Turn: NewTurnID(), Said: said("x"), Origin: "webapp",
		Hold: &hold, ScheduleID: "drain-1", QueuedAt: instant,
	}); err != nil {
		t.Fatalf("PutHeldPrompt: %v", err)
	}
	got, err := s.HeldPrompts(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("HeldPrompts: %v", err)
	}
	if got[0].Hold == nil || *got[0].Hold != HoldShutdown || got[0].ScheduleID != "drain-1" {
		t.Fatalf("hold = %v / schedule = %q, want a shutdown hold on drain-1", got[0].Hold, got[0].ScheduleID)
	}
}

func TestPutHeldPromptRefusesAnUndeclaredHoldKind(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	hold := HoldKind(99)

	// Act
	err := s.PutHeldPrompt(context.Background(), HeldPrompt{
		Workspace: ws.ID, Turn: NewTurnID(), Said: said("x"), Origin: "webapp", Hold: &hold, QueuedAt: instant,
	})

	// Assert
	if err == nil {
		t.Fatalf("PutHeldPrompt with an undeclared hold kind succeeded")
	}
}

func TestPutHeldPromptRefusesAnAcceptOnAnUnofferedVerdict(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.PutHeldPrompt(context.Background(), HeldPrompt{
		Workspace: ws.ID, Turn: NewTurnID(), Said: said("x"), Origin: "webapp",
		Classification: &Classification{Arm: ArmInterject, Reason: "urgent", At: instant},
		Accepted:       true, QueuedAt: instant,
	})

	// Assert
	if !errors.Is(err, ErrAcceptNotOffered) {
		t.Fatalf("PutHeldPrompt = %v, want ErrAcceptNotOffered", err)
	}
}

func TestUpdateHeldPromptClassificationRoundTripsTheVerdict(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := standingHold(t, s, ws.ID)

	// Act
	if err := s.UpdateHeldPromptClassification(context.Background(), turn, Classification{
		Arm: ArmUninterruptibleTurn, Reason: "the running turn is a /compact", Command: conversationv1.SessionCommand_SESSION_COMMAND_COMPACT, At: instant,
	}); err != nil {
		t.Fatalf("UpdateHeldPromptClassification: %v", err)
	}
	got, err := s.HeldPrompts(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("HeldPrompts: %v", err)
	}
	c := got[0].Classification
	if c == nil || c.Arm != ArmUninterruptibleTurn || c.Command != conversationv1.SessionCommand_SESSION_COMMAND_COMPACT {
		t.Fatalf("classification = %+v, want the uninterruptible verdict with its command", c)
	}
	if c.Reason != "the running turn is a /compact" || !c.At.Equal(instant) {
		t.Fatalf("classification = %+v, want the recorded evidence", c)
	}
}

func TestUpdateHeldPromptClassificationRefusesAnUndeclaredArm(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	turn := standingHold(t, s, ws.ID)

	// Act
	err := s.UpdateHeldPromptClassification(context.Background(), turn, Classification{Arm: ClassificationArm(99), At: instant})

	// Assert
	if err == nil {
		t.Fatalf("UpdateHeldPromptClassification with an undeclared arm succeeded")
	}
	if !loggedOperation(log, "daemon.wsm.update_held_prompt_classification", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
}

func TestUpdateHeldPromptClassificationClearsAStaleAcceptance(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := standingHold(t, s, ws.ID)
	if err := s.UpdateHeldPromptClassification(context.Background(), turn, Classification{Arm: ArmHoldForTurnEnd, At: instant}); err != nil {
		t.Fatalf("UpdateHeldPromptClassification: %v", err)
	}
	if err := s.SetHeldPromptAccepted(context.Background(), turn); err != nil {
		t.Fatalf("SetHeldPromptAccepted: %v", err)
	}

	// Act
	if err := s.UpdateHeldPromptClassification(context.Background(), turn, Classification{Arm: ArmInterject, At: instant}); err != nil {
		t.Fatalf("UpdateHeldPromptClassification: %v", err)
	}

	// Assert
	got, err := s.HeldPrompts(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("HeldPrompts: %v", err)
	}
	if got[0].Accepted {
		t.Fatalf("accepted survived a verdict that offers no acceptance")
	}
}

func TestSetHeldPromptAcceptedFlipsTheOfferedVerdict(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := standingHold(t, s, ws.ID)
	if err := s.UpdateHeldPromptClassification(context.Background(), turn, Classification{Arm: ArmHoldForTurnEnd, At: instant}); err != nil {
		t.Fatalf("UpdateHeldPromptClassification: %v", err)
	}

	// Act
	if err := s.SetHeldPromptAccepted(context.Background(), turn); err != nil {
		t.Fatalf("SetHeldPromptAccepted: %v", err)
	}

	// Assert
	got, err := s.HeldPrompts(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("HeldPrompts: %v", err)
	}
	if !got[0].Accepted {
		t.Fatalf("accepted = false after SetHeldPromptAccepted")
	}
}

func TestSetHeldPromptAcceptedRefusesAnUnofferedVerdict(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := standingHold(t, s, ws.ID)
	if err := s.UpdateHeldPromptClassification(context.Background(), turn, Classification{Arm: ArmInterject, At: instant}); err != nil {
		t.Fatalf("UpdateHeldPromptClassification: %v", err)
	}

	// Act
	err := s.SetHeldPromptAccepted(context.Background(), turn)

	// Assert
	if !errors.Is(err, ErrAcceptNotOffered) {
		t.Fatalf("SetHeldPromptAccepted = %v, want ErrAcceptNotOffered", err)
	}
}

func TestSetHeldPromptAcceptedRefusesAnUnjudgedPrompt(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := standingHold(t, s, ws.ID)

	// Act
	err := s.SetHeldPromptAccepted(context.Background(), turn)

	// Assert
	if !errors.Is(err, ErrAcceptNotOffered) {
		t.Fatalf("SetHeldPromptAccepted = %v, want ErrAcceptNotOffered", err)
	}
}

func TestUpdateHeldPromptHoldClearsTheCondition(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := standingHold(t, s, ws.ID)
	starting := HoldSessionStarting
	if err := s.UpdateHeldPromptHold(context.Background(), turn, &starting, ""); err != nil {
		t.Fatalf("UpdateHeldPromptHold: %v", err)
	}

	// Act
	if err := s.UpdateHeldPromptHold(context.Background(), turn, nil, ""); err != nil {
		t.Fatalf("UpdateHeldPromptHold(nil): %v", err)
	}

	// Assert
	got, err := s.HeldPrompts(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("HeldPrompts: %v", err)
	}
	if got[0].Hold != nil {
		t.Fatalf("hold = %v after clearing, want nil", *got[0].Hold)
	}
}

func TestUpdateHeldPromptHoldRequiresAScheduleForAShutdownHold(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	turn := standingHold(t, s, ws.ID)
	hold := HoldShutdown

	// Act
	err := s.UpdateHeldPromptHold(context.Background(), turn, &hold, "")

	// Assert
	if err == nil {
		t.Fatalf("a shutdown hold with no schedule id succeeded")
	}
	if !loggedOperation(log, "daemon.wsm.update_held_prompt_hold", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
}

func TestUpdateHeldPromptHoldRefusesAnUnknownPrompt(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	hold := HoldBuildRefresh

	// Act
	err := s.UpdateHeldPromptHold(context.Background(), TurnID("absent"), &hold, "")

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("UpdateHeldPromptHold = %v, want ErrNotFound", err)
	}
}

func TestTombstoneHeldPromptRetiresTheHold(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := standingHold(t, s, ws.ID)

	// Act
	if err := s.TombstoneHeldPrompt(context.Background(), turn, Tombstone{Kind: "delivered", At: instant}); err != nil {
		t.Fatalf("TombstoneHeldPrompt: %v", err)
	}

	// Assert
	got, err := s.HeldPrompts(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("HeldPrompts: %v", err)
	}
	if len(got) != 0 {
		t.Fatalf("a retired hold is still standing: %+v", got)
	}
}

func TestTombstoneHeldPromptPreventsResurrection(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	turn := standingHold(t, s, ws.ID)
	if err := s.TombstoneHeldPrompt(context.Background(), turn, Tombstone{Kind: "dropped", At: instant}); err != nil {
		t.Fatalf("TombstoneHeldPrompt: %v", err)
	}

	// Act
	err := s.PutHeldPrompt(context.Background(), HeldPrompt{
		Workspace: ws.ID, Turn: turn, Said: said("back from the dead"), Origin: "webapp", QueuedAt: instant,
	})

	// Assert
	if !errors.Is(err, ErrTombstoned) {
		t.Fatalf("PutHeldPrompt on a retired hold = %v, want ErrTombstoned", err)
	}
	if !loggedOperation(log, "daemon.wsm.put_held_prompt", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
}

func TestTombstoneHeldPromptRefusesARetiredHold(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := standingHold(t, s, ws.ID)
	if err := s.TombstoneHeldPrompt(context.Background(), turn, Tombstone{Kind: "delivered", At: instant}); err != nil {
		t.Fatalf("TombstoneHeldPrompt: %v", err)
	}

	// Act
	err := s.TombstoneHeldPrompt(context.Background(), turn, Tombstone{Kind: "dropped", At: instant})

	// Assert
	if !errors.Is(err, ErrTombstoned) {
		t.Fatalf("TombstoneHeldPrompt = %v, want ErrTombstoned", err)
	}
}

func TestUpdateHeldPromptClassificationRefusesARetiredHold(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := standingHold(t, s, ws.ID)
	if err := s.TombstoneHeldPrompt(context.Background(), turn, Tombstone{Kind: "delivered", At: instant}); err != nil {
		t.Fatalf("TombstoneHeldPrompt: %v", err)
	}

	// Act
	err := s.UpdateHeldPromptClassification(context.Background(), turn, Classification{Arm: ArmInterject, At: instant})

	// Assert
	if !errors.Is(err, ErrTombstoned) {
		t.Fatalf("UpdateHeldPromptClassification = %v, want ErrTombstoned", err)
	}
}

func TestAllHeldPromptsLoadsEveryWorkspacesHolds(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	first := testWorkspace(t, s)
	second := testWorkspace(t, s)
	standingHold(t, s, first.ID)
	standingHold(t, s, second.ID)

	// Act
	got, err := s.AllHeldPrompts(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("AllHeldPrompts: %v", err)
	}
	if len(got) != 2 {
		t.Fatalf("loaded %d holds, want 2", len(got))
	}
}

func TestAllHeldPromptsLoadsNothingWhenOneRowIsCorrupt(t *testing.T) {
	// Arrange — the all-or-nothing hold restore: one bad row must lose no other
	// user's typing silently, so the whole read fails.
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	standingHold(t, s, ws.ID)
	broken := standingHold(t, s, ws.ID)
	corrupt(t, s, `UPDATE held_prompts SET hold_kind = 99 WHERE turn_id = ?`, broken)

	// Act
	got, err := s.AllHeldPrompts(context.Background())

	// Assert
	var refusal *DecodeError
	if !errors.As(err, &refusal) || refusal.Table != "held_prompts" || refusal.Field != "hold_kind" {
		t.Fatalf("AllHeldPrompts = %v, want a *DecodeError naming held_prompts.hold_kind", err)
	}
	if got != nil {
		t.Fatalf("loaded %d holds alongside the refusal, want none", len(got))
	}
	if !loggedOperation(log, "daemon.wsm.all_held_prompts", "error") {
		t.Fatalf("the decode failure was not logged at error: %v", log.Records())
	}
}

func TestHeldPromptsFailsWholeOnAnUnparseableSubmission(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	broken := standingHold(t, s, ws.ID)
	corrupt(t, s, `UPDATE held_prompts SET said = ? WHERE turn_id = ?`, []byte{0xff, 0xff, 0xff}, broken)

	// Act
	got, err := s.HeldPrompts(context.Background(), ws.ID)

	// Assert
	var refusal *DecodeError
	if !errors.As(err, &refusal) || refusal.Table != "held_prompts" || refusal.Field != "said" {
		t.Fatalf("HeldPrompts = %v, want a *DecodeError naming held_prompts.said", err)
	}
	if got != nil {
		t.Fatalf("loaded %d holds alongside the refusal, want none", len(got))
	}
}

func TestHeldPromptsFailsWholeOnAHalfWrittenClassification(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	broken := standingHold(t, s, ws.ID)
	corrupt(t, s, `UPDATE held_prompts SET classification_arm = 1 WHERE turn_id = ?`, broken)

	// Act
	_, err := s.HeldPrompts(context.Background(), ws.ID)

	// Assert
	var refusal *DecodeError
	if !errors.As(err, &refusal) || refusal.Field != "classification" {
		t.Fatalf("HeldPrompts = %v, want a *DecodeError naming held_prompts.classification", err)
	}
}

func TestHeldPromptsFailsWholeOnAnUndeclaredClassificationArm(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	broken := standingHold(t, s, ws.ID)
	if err := s.UpdateHeldPromptClassification(context.Background(), broken, Classification{Arm: ArmInterject, At: instant}); err != nil {
		t.Fatalf("UpdateHeldPromptClassification: %v", err)
	}
	corrupt(t, s, `UPDATE held_prompts SET classification_arm = 99 WHERE turn_id = ?`, broken)

	// Act
	_, err := s.HeldPrompts(context.Background(), ws.ID)

	// Assert
	var refusal *DecodeError
	if !errors.As(err, &refusal) || refusal.Field != "classification_arm" {
		t.Fatalf("HeldPrompts = %v, want a *DecodeError naming held_prompts.classification_arm", err)
	}
}

func TestHeldPromptsFailsWholeOnAnUnknownSessionCommand(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	broken := standingHold(t, s, ws.ID)
	if err := s.UpdateHeldPromptClassification(context.Background(), broken, Classification{Arm: ArmUninterruptibleTurn, At: instant}); err != nil {
		t.Fatalf("UpdateHeldPromptClassification: %v", err)
	}
	corrupt(t, s, `UPDATE held_prompts SET classification_command = 9999 WHERE turn_id = ?`, broken)

	// Act
	_, err := s.HeldPrompts(context.Background(), ws.ID)

	// Assert
	var refusal *DecodeError
	if !errors.As(err, &refusal) || refusal.Field != "classification_command" {
		t.Fatalf("HeldPrompts = %v, want a *DecodeError naming held_prompts.classification_command", err)
	}
}

func TestHeldPromptsFailsWholeOnAnAcceptedRowWithNoOffer(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	broken := standingHold(t, s, ws.ID)
	corrupt(t, s, `UPDATE held_prompts SET accepted = 1 WHERE turn_id = ?`, broken)

	// Act
	_, err := s.HeldPrompts(context.Background(), ws.ID)

	// Assert
	var refusal *DecodeError
	if !errors.As(err, &refusal) || refusal.Field != "accepted" {
		t.Fatalf("HeldPrompts = %v, want a *DecodeError naming held_prompts.accepted", err)
	}
}

func TestHeldPromptsFailsWholeOnAShutdownHoldWithNoSchedule(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	broken := standingHold(t, s, ws.ID)
	corrupt(t, s, `UPDATE held_prompts SET hold_kind = ?, hold_schedule_id = '' WHERE turn_id = ?`, int(HoldShutdown), broken)

	// Act
	_, err := s.HeldPrompts(context.Background(), ws.ID)

	// Assert
	var refusal *DecodeError
	if !errors.As(err, &refusal) || refusal.Field != "hold_schedule_id" {
		t.Fatalf("HeldPrompts = %v, want a *DecodeError naming held_prompts.hold_schedule_id", err)
	}
}

func TestHeldPromptsFailsWholeOnAnUnknownTargetRowKind(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	broken := standingHold(t, s, ws.ID)
	corrupt(t, s, `UPDATE held_prompts SET target = ? WHERE turn_id = ?`,
		`{"ws":"x","feed":{"root":true},"row":{"kind":"invented","id":"1"}}`, broken)

	// Act
	_, err := s.HeldPrompts(context.Background(), ws.ID)

	// Assert
	var refusal *DecodeError
	if !errors.As(err, &refusal) || refusal.Field != "row_kind" {
		t.Fatalf("HeldPrompts = %v, want a *DecodeError naming the row kind", err)
	}
}

func TestForgetDeletesAWorkspacesHolds(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	standingHold(t, s, ws.ID)

	// Act
	if _, err := s.Forget(context.Background(), ws.ID); err != nil {
		t.Fatalf("Forget: %v", err)
	}

	// Assert
	if n := scalar[int](t, s, `SELECT count(*) FROM held_prompts WHERE workspace_id = ?`, ws.ID); n != 0 {
		t.Fatalf("%d holds survived the nuke, want none", n)
	}
}

// standingHold records one unjudged, unretired hold and returns its turn.
func standingHold(t *testing.T, s *store, ws WorkspaceID) TurnID {
	t.Helper()
	turn := NewTurnID()
	if err := s.PutHeldPrompt(context.Background(), HeldPrompt{
		Workspace: ws, Turn: turn, Said: said("held"), Origin: "webapp", QueuedAt: instant,
	}); err != nil {
		t.Fatalf("PutHeldPrompt: %v", err)
	}
	return turn
}

func TestReplaceHeldPromptSaidReplacesTheContent(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := standingHold(t, s, ws.ID)

	// Act
	if err := s.ReplaceHeldPromptSaid(context.Background(), turn, said("the edited words")); err != nil {
		t.Fatalf("ReplaceHeldPromptSaid: %v", err)
	}

	// Assert
	got, err := s.HeldPrompts(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("HeldPrompts: %v", err)
	}
	if firstText(got[0].Said) != "the edited words" {
		t.Fatalf("said = %q, want the replacement", firstText(got[0].Said))
	}
}

func TestReplaceHeldPromptSaidKeepsTheQueuePosition(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := standingHold(t, s, ws.ID)

	// Act
	if err := s.ReplaceHeldPromptSaid(context.Background(), turn, said("the edited words")); err != nil {
		t.Fatalf("ReplaceHeldPromptSaid: %v", err)
	}

	// Assert
	got, err := s.HeldPrompts(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("HeldPrompts: %v", err)
	}
	if !got[0].QueuedAt.Equal(instant) {
		t.Fatalf("queued_at = %v, want the original %v", got[0].QueuedAt, instant)
	}
}

func TestReplaceHeldPromptSaidDiscardsTheVerdict(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := standingHold(t, s, ws.ID)
	if err := s.UpdateHeldPromptClassification(context.Background(), turn, Classification{Arm: ArmHoldForTurnEnd, At: instant}); err != nil {
		t.Fatalf("UpdateHeldPromptClassification: %v", err)
	}
	if err := s.SetHeldPromptAccepted(context.Background(), turn); err != nil {
		t.Fatalf("SetHeldPromptAccepted: %v", err)
	}

	// Act
	if err := s.ReplaceHeldPromptSaid(context.Background(), turn, said("the edited words")); err != nil {
		t.Fatalf("ReplaceHeldPromptSaid: %v", err)
	}

	// Assert
	got, err := s.HeldPrompts(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("HeldPrompts: %v", err)
	}
	if got[0].Classification != nil || got[0].Accepted {
		t.Fatalf("hold = %+v, want the verdict and the acceptance discarded", got[0])
	}
}

func TestReplaceHeldPromptSaidRefusesARetiredHold(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	turn := standingHold(t, s, ws.ID)
	if err := s.TombstoneHeldPrompt(context.Background(), turn, Tombstone{Kind: "delivered", At: instant}); err != nil {
		t.Fatalf("TombstoneHeldPrompt: %v", err)
	}

	// Act
	err := s.ReplaceHeldPromptSaid(context.Background(), turn, said("too late"))

	// Assert
	if !errors.Is(err, ErrTombstoned) {
		t.Fatalf("ReplaceHeldPromptSaid = %v, want ErrTombstoned", err)
	}
	if !loggedOperation(log, "daemon.wsm.replace_held_prompt_said", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
}

func TestReplaceHeldPromptSaidRefusesAMissingSubmission(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	turn := standingHold(t, s, ws.ID)

	// Act
	err := s.ReplaceHeldPromptSaid(context.Background(), turn, nil)

	// Assert
	if err == nil {
		t.Fatalf("ReplaceHeldPromptSaid(nil) succeeded, want a refusal")
	}
	if !loggedOperation(log, "daemon.wsm.replace_held_prompt_said", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
}

func TestHeldPromptByTurnFindsAStandingHold(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := standingHold(t, s, ws.ID)

	// Act
	got, ok, err := s.HeldPromptByTurn(context.Background(), turn)

	// Assert
	if err != nil || !ok {
		t.Fatalf("HeldPromptByTurn = (%v, %v), want the hold", ok, err)
	}
	if got.Turn != turn || got.Tombstone != nil {
		t.Fatalf("hold = %+v, want the standing hold", got)
	}
}

func TestHeldPromptByTurnFindsARetiredHold(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := standingHold(t, s, ws.ID)
	if err := s.TombstoneHeldPrompt(context.Background(), turn, Tombstone{Kind: "dropped", At: instant}); err != nil {
		t.Fatalf("TombstoneHeldPrompt: %v", err)
	}

	// Act
	got, ok, err := s.HeldPromptByTurn(context.Background(), turn)

	// Assert
	if err != nil || !ok {
		t.Fatalf("HeldPromptByTurn = (%v, %v), want the retired hold", ok, err)
	}
	if got.Tombstone == nil || got.Tombstone.Kind != "dropped" {
		t.Fatalf("tombstone = %+v, want the dropped retirement", got.Tombstone)
	}
}

func TestHeldPromptByTurnReportsAnUnknownTurn(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	testWorkspace(t, s)

	// Act
	_, ok, err := s.HeldPromptByTurn(context.Background(), NewTurnID())

	// Assert
	if err != nil || ok {
		t.Fatalf("HeldPromptByTurn = (%v, %v), want not found and no error", ok, err)
	}
}

func TestPutHeldPromptRoundTripsTheDelivery(t *testing.T) {
	tests := []struct {
		name     string
		delivery Delivery
	}{
		{name: "an ordinary prompt", delivery: DeliveryOrdinary},
		{name: "a deferred prompt", delivery: DeliveryDeferred},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			if err := s.PutHeldPrompt(context.Background(), HeldPrompt{
				Workspace: ws.ID, Turn: NewTurnID(), Said: said("x"), Origin: "webapp", QueuedAt: instant, Delivery: tc.delivery,
			}); err != nil {
				t.Fatalf("PutHeldPrompt: %v", err)
			}
			got, err := s.HeldPrompts(context.Background(), ws.ID)

			// Assert
			if err != nil || len(got) != 1 || got[0].Delivery != tc.delivery {
				t.Fatalf("HeldPrompts = (%+v, %v), want one hold delivered %s", got, err, tc.delivery)
			}
		})
	}
}

func TestPutHeldPromptRefusesAnUndeclaredDelivery(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.PutHeldPrompt(context.Background(), HeldPrompt{
		Workspace: ws.ID, Turn: NewTurnID(), Said: said("x"), Origin: "webapp", QueuedAt: instant, Delivery: Delivery(99),
	})

	// Assert
	if err == nil {
		t.Fatalf("PutHeldPrompt with an undeclared delivery succeeded")
	}
}

func TestAHeldPromptWithAnUnknownDeliveryIsNeverReadAsOrdinary(t *testing.T) {
	// Arrange — a deferred prompt misread as ordinary would be classified and
	// could interject, so the whole read fails instead.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	broken := standingHold(t, s, ws.ID)
	corrupt(t, s, `UPDATE held_prompts SET delivery = 99 WHERE turn_id = ?`, broken)

	// Act
	_, err := s.AllHeldPrompts(context.Background())

	// Assert
	var refusal *DecodeError
	if !errors.As(err, &refusal) || refusal.Field != "delivery" {
		t.Fatalf("AllHeldPrompts = %v, want a *DecodeError naming held_prompts.delivery", err)
	}
}
