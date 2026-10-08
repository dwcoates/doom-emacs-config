package server

import (
	"context"
	"strings"
	"testing"
	"time"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/replyquote"
	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/workspace"
	"claude-repld/internal/wsm"
)

// keepFilesPlan builds a PlanRollback request that keeps files.
func keepFilesPlan() *agentreplv1.PlanRollbackRequest {
	return &agentreplv1.PlanRollbackRequest{
		Workspace: ref(),
		Files:     &agentreplv1.PlanRollbackRequest_KeepFiles{KeepFiles: &agentreplv1.PlanRollbackKeepFiles{}},
	}
}

// restoreFilesPlan builds a PlanRollback request that restores files.
func restoreFilesPlan() *agentreplv1.PlanRollbackRequest {
	return &agentreplv1.PlanRollbackRequest{
		Workspace: ref(),
		Files:     &agentreplv1.PlanRollbackRequest_RestoreFiles{RestoreFiles: &agentreplv1.PlanRollbackRestoreFiles{}},
	}
}

// seedPlannable arranges the feed so PlanRollback resolves one reachable
// prompt, naming turn "t1", that the test can plan and roll back.
func seedPlannable(h *harness) {
	h.Feed.prompts = feedIDs("p1")
	h.Feed.rollbackTargetOK = true
	h.Feed.rollbackTarget = feed.RollbackTarget{Turns: []ids.TurnID{"t1"}, Said: said("rolled back prompt")}
}

// rollBackRequest builds a RollBack request naming TOKEN.
func rollBackRequest(token string) *agentreplv1.RollBackRequest {
	return &agentreplv1.RollBackRequest{Workspace: ref(), Token: &agentreplv1.RollbackToken{Value: token}}
}

// planToken plans REQ, fails the test unless it planned, and answers the
// minted token.
func planToken(t *testing.T, h *harness, req *agentreplv1.PlanRollbackRequest) string {
	t.Helper()
	resp, err := h.Client.PlanRollback(context.Background(), connect.NewRequest(req))
	if err != nil {
		t.Fatalf("PlanRollback: %v", err)
	}
	plan := resp.Msg.GetSuccess().GetPlan()
	if plan == nil {
		t.Fatalf("PlanRollback did not plan: %v", resp.Msg)
	}
	return plan.GetToken().GetValue()
}

// multiBlockSaid builds a UserSaid whose content carries one text block per
// TEXTS entry, in order.
func multiBlockSaid(texts ...string) *conversationv1.UserSaid {
	blocks := make([]*conversationv1.UserContentBlock, len(texts))
	for i, text := range texts {
		blocks[i] = &conversationv1.UserContentBlock{
			Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}},
		}
	}
	return &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: blocks}}
}

// TestPlanRollbackRequiresFiles pins that an unset `files` oneof is refused
// AT ONCE, before any resolution.
func TestPlanRollbackRequiresFiles(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	req := &agentreplv1.PlanRollbackRequest{Workspace: ref()}

	// Act.
	_, err := h.Client.PlanRollback(context.Background(), connect.NewRequest(req))

	// Assert.
	if connectCode(t, err) != connect.CodeInvalidArgument {
		t.Fatalf("code = %v, want InvalidArgument", connectCode(t, err))
	}
}

// TestPlanRollbackRequiresAValidWorkspace pins that the ref refusals mirror
// every other per-workspace rpc: an unregistered id answers unknown_workspace.
func TestPlanRollbackRequiresAValidWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	req := keepFilesPlan()
	req.Workspace = &workspacev1.WorkspaceRef{Id: "ws-nope"}

	// Act.
	resp, err := h.Client.PlanRollback(context.Background(), connect.NewRequest(req))

	// Assert.
	if err != nil {
		t.Fatalf("PlanRollback: %v", err)
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("result = %v, want unknown_workspace", resp.Msg.GetResult())
	}
}

// TestPlanRollbackNothingToRollBack pins that a feed with no reachable prompt
// and nothing selected answers nothing_to_roll_back rather than minting a
// token.
func TestPlanRollbackNothingToRollBack(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.PlanRollback(context.Background(), connect.NewRequest(keepFilesPlan()))

	// Assert.
	if err != nil {
		t.Fatalf("PlanRollback: %v", err)
	}
	if resp.Msg.GetSuccess().GetNothingToRollBack() == nil {
		t.Fatalf("outcome = %v, want nothing_to_roll_back", resp.Msg.GetSuccess().GetOutcome())
	}
}

// TestPlanRollbackChoosesTheLatestPromptWhenNoneSelected pins that with no
// selection standing, the plan targets the newest reachable prompt.
func TestPlanRollbackChoosesTheLatestPromptWhenNoneSelected(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.prompts = feedIDs("p1", "p2")
	h.Feed.rollbackTargetOK = true
	h.Feed.rollbackTarget = feed.RollbackTarget{Turns: []ids.TurnID{"t1"}, Said: said("x")}

	// Act.
	resp, err := h.Client.PlanRollback(context.Background(), connect.NewRequest(keepFilesPlan()))

	// Assert.
	if err != nil {
		t.Fatalf("PlanRollback: %v", err)
	}
	plan := resp.Msg.GetSuccess().GetPlan()
	if plan.GetTarget().GetLatest() == nil {
		t.Fatalf("target chosen = %v, want latest", plan.GetTarget().GetChosen())
	}
	if n := len(h.Feed.rollbackTargetRows); n == 0 || h.Feed.rollbackTargetRows[n-1].GetValue() != "p2" {
		t.Fatalf("RollbackTarget asked about %v, want the latest p2", h.Feed.rollbackTargetRows)
	}
}

// TestPlanRollbackChoosesTheSelectedPrompt pins that a held prompt selection
// overrides the latest: the plan targets the SELECTED row, not p2.
func TestPlanRollbackChoosesTheSelectedPrompt(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.prompts = feedIDs("p1", "p2")
	h.Feed.rollbackTargetOK = true
	h.Feed.rollbackTarget = feed.RollbackTarget{Turns: []ids.TurnID{"t1"}, Said: said("x")}
	// Seed the newest (p2), then step OLDER onto p1: the selection held is p1.
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(promptStep(newer))); err != nil {
		t.Fatalf("seed the newest prompt: %v", err)
	}
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(promptStep(older))); err != nil {
		t.Fatalf("step onto p1: %v", err)
	}

	// Act.
	resp, err := h.Client.PlanRollback(context.Background(), connect.NewRequest(keepFilesPlan()))

	// Assert.
	if err != nil {
		t.Fatalf("PlanRollback: %v", err)
	}
	plan := resp.Msg.GetSuccess().GetPlan()
	if plan.GetTarget().GetSelected() == nil {
		t.Fatalf("target chosen = %v, want selected", plan.GetTarget().GetChosen())
	}
	if n := len(h.Feed.rollbackTargetRows); n == 0 || h.Feed.rollbackTargetRows[n-1].GetValue() != "p1" {
		t.Fatalf("RollbackTarget asked about %v, want the selected p1", h.Feed.rollbackTargetRows)
	}
}

// TestPlanRollbackEffectsInterruptWhenATurnIsOpen pins that an open turn
// surfaces the `interrupt` field on the plan's view.
func TestPlanRollbackEffectsInterruptWhenATurnIsOpen(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	seedPlannable(h)
	h.DB.openTurns = []wsm.Turn{{ID: "t1"}}

	// Act.
	resp, err := h.Client.PlanRollback(context.Background(), connect.NewRequest(keepFilesPlan()))

	// Assert.
	if err != nil {
		t.Fatalf("PlanRollback: %v", err)
	}
	if resp.Msg.GetSuccess().GetPlan().GetInterrupt() == nil {
		t.Fatalf("plan = %v, want an interrupt since a turn is open", resp.Msg.GetSuccess().GetPlan())
	}
}

// TestPlanRollbackEffectsDropQueuedFromHeldSince pins that the held prompts
// HeldSince answers are reflected as the plan's drop_queued count.
func TestPlanRollbackEffectsDropQueuedFromHeldSince(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	seedPlannable(h)
	h.Queue.heldSince = []ids.TurnID{"h1", "h2"}

	// Act.
	resp, err := h.Client.PlanRollback(context.Background(), connect.NewRequest(keepFilesPlan()))

	// Assert.
	if err != nil {
		t.Fatalf("PlanRollback: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetPlan().GetDropQueued().GetPrompts(); got != 2 {
		t.Fatalf("drop_queued.prompts = %d, want 2", got)
	}
}

// TestPlanRollbackEffectsCancelDetachedOnlyWithRestoreAndWhenPositive pins
// that detached work is counted ONLY when files are restored, and only
// surfaces on the view when the count is positive.
func TestPlanRollbackEffectsCancelDetachedOnlyWithRestoreAndWhenPositive(t *testing.T) {
	tests := []struct {
		name     string
		restore  bool
		detached int
	}{
		{name: "files kept, nothing detached", restore: false, detached: 0},
		{name: "files kept: detached is never even computed", restore: false, detached: 5},
		{name: "files restored, nothing detached", restore: true, detached: 0},
		{name: "files restored, some detached", restore: true, detached: 3},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			seedPlannable(h)
			h.Feed.liveDetached = tc.detached
			req := keepFilesPlan()
			if tc.restore {
				req = restoreFilesPlan()
			}

			// Act.
			resp, err := h.Client.PlanRollback(context.Background(), connect.NewRequest(req))

			// Assert.
			if err != nil {
				t.Fatalf("PlanRollback: %v", err)
			}
			files := resp.Msg.GetSuccess().GetPlan().GetFiles()
			switch {
			case !tc.restore:
				if files.GetKept() == nil {
					t.Fatalf("files = %v, want kept", files)
				}
				if h.Feed.liveDetachedCalls != 0 {
					t.Fatalf("LiveDetachedIn was called %d times, want 0 when files are kept", h.Feed.liveDetachedCalls)
				}
			case tc.detached == 0:
				if files.GetRestored() == nil || files.GetRestored().GetCancelDetached() != nil {
					t.Fatalf("files = %v, want restored with no cancel_detached", files)
				}
			default:
				if got := files.GetRestored().GetCancelDetached().GetItems(); int(got) != tc.detached {
					t.Fatalf("cancel_detached.items = %d, want %d", got, tc.detached)
				}
			}
		})
	}
}

// TestPlanRollbackATurnTheDBNeverRecordedGetsNoDropQueued pins that a prompt
// the DB never recorded (wsm.ErrNotFound) is given the far-future `since`
// sentinel, and the plan carries no drop_queued, rather than failing.
func TestPlanRollbackATurnTheDBNeverRecordedGetsNoDropQueued(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	seedPlannable(h)
	h.DB.turnStartedErr = wsm.ErrNotFound

	// Act.
	resp, err := h.Client.PlanRollback(context.Background(), connect.NewRequest(keepFilesPlan()))

	// Assert.
	if err != nil {
		t.Fatalf("PlanRollback: %v", err)
	}
	if resp.Msg.GetSuccess().GetPlan().GetDropQueued() != nil {
		t.Fatalf("plan = %v, want no drop_queued", resp.Msg.GetSuccess().GetPlan())
	}
	if n := len(h.Queue.heldSinceCalls); n == 0 || !h.Queue.heldSinceCalls[n-1].Equal(time.Unix(1<<62, 0)) {
		t.Fatalf("HeldSince was asked with %v, want the far-future sentinel", h.Queue.heldSinceCalls)
	}
}

// TestRollBackRequiresAToken pins that an empty token is refused AT ONCE.
func TestRollBackRequiresAToken(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.RollBack(context.Background(), connect.NewRequest(rollBackRequest("")))

	// Assert.
	if connectCode(t, err) != connect.CodeInvalidArgument {
		t.Fatalf("code = %v, want InvalidArgument", connectCode(t, err))
	}
}

// TestRollBackUnknownTokenIsStale pins that a token this daemon never minted
// is refused as plan_stale.
func TestRollBackUnknownTokenIsStale(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.RollBack(context.Background(), connect.NewRequest(rollBackRequest("never-minted")))

	// Assert.
	if err != nil {
		t.Fatalf("RollBack: %v", err)
	}
	if resp.Msg.GetError().GetPlanStale() == nil {
		t.Fatalf("result = %v, want plan_stale", resp.Msg.GetResult())
	}
}

// TestRollBackTokenReusedIsStaleTheSecondTime pins that a token is spent by
// its first use: a second RollBack with the same token is refused as
// plan_stale even though the first succeeded.
func TestRollBackTokenReusedIsStaleTheSecondTime(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	seedPlannable(h)
	token := planToken(t, h, keepFilesPlan())

	// Act.
	first, err := h.Client.RollBack(context.Background(), connect.NewRequest(rollBackRequest(token)))
	if err != nil {
		t.Fatalf("RollBack (first): %v", err)
	}
	second, err := h.Client.RollBack(context.Background(), connect.NewRequest(rollBackRequest(token)))

	// Assert.
	if first.Msg.GetSuccess() == nil {
		t.Fatalf("first RollBack = %v, want success", first.Msg.GetResult())
	}
	if err != nil {
		t.Fatalf("RollBack (second): %v", err)
	}
	if second.Msg.GetError().GetPlanStale() == nil {
		t.Fatalf("second result = %v, want plan_stale", second.Msg.GetResult())
	}
}

// TestRollBackEffectsChangedSincePlanningIsStale pins that re-planning before
// performing catches a conversation that changed since PlanRollback: the
// workspace now has an open turn, flipping `running`, so the confirmed plan no
// longer describes what would happen and RollBack never reaches the verb.
func TestRollBackEffectsChangedSincePlanningIsStale(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	seedPlannable(h)
	token := planToken(t, h, keepFilesPlan())
	h.DB.openTurns = []wsm.Turn{{ID: "t1"}}

	// Act.
	resp, err := h.Client.RollBack(context.Background(), connect.NewRequest(rollBackRequest(token)))

	// Assert.
	if err != nil {
		t.Fatalf("RollBack: %v", err)
	}
	if resp.Msg.GetError().GetPlanStale() == nil {
		t.Fatalf("result = %v, want plan_stale", resp.Msg.GetResult())
	}
	if len(h.Verbs.rollBackReq) != 0 {
		t.Fatalf("Verbs.RollBack was called %d times, want 0", len(h.Verbs.rollBackReq))
	}
}

// TestRollBackHoldsChangedDuringPerformIsStale pins that the verb's
// ErrHoldsChanged (the queue changed under the confirmed rollback) answers
// plan_stale rather than an internal failure.
func TestRollBackHoldsChangedDuringPerformIsStale(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	seedPlannable(h)
	token := planToken(t, h, keepFilesPlan())
	h.Verbs.rollBackErr = promptqueue.ErrHoldsChanged

	// Act.
	resp, err := h.Client.RollBack(context.Background(), connect.NewRequest(rollBackRequest(token)))

	// Assert.
	if err != nil {
		t.Fatalf("RollBack: %v", err)
	}
	if resp.Msg.GetError().GetPlanStale() == nil {
		t.Fatalf("result = %v, want plan_stale", resp.Msg.GetResult())
	}
}

// TestRollBackShimRefusalArmsMapToMatchingRollBackErrorArms pins that a
// *workspace.ShimRefusal from the verb maps onto the RollBackError arm of the
// same name, and that the two arms carrying vendor words (vendor_refused,
// files_not_restorable) carry them.
func TestRollBackShimRefusalArmsMapToMatchingRollBackErrorArms(t *testing.T) {
	tests := []struct {
		name          string
		arm           string
		carriesVendor bool
	}{
		{name: "vendor refused", arm: workspace.ArmShimVendorRefused, carriesVendor: true},
		{name: "files not restorable", arm: workspace.ArmShimFilesNotRestorable, carriesVendor: true},
		{name: "no session", arm: workspace.ArmShimNoSession, carriesVendor: false},
		{name: "first prompt", arm: workspace.ArmShimFirstPrompt, carriesVendor: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			seedPlannable(h)
			token := planToken(t, h, keepFilesPlan())
			h.Verbs.rollBackErr = &workspace.ShimRefusal{Arm: tc.arm, Detail: "the vendor said no"}

			// Act.
			resp, err := h.Client.RollBack(context.Background(), connect.NewRequest(rollBackRequest(token)))

			// Assert.
			if err != nil {
				t.Fatalf("RollBack: %v", err)
			}
			cause := resp.Msg.GetError()
			switch tc.arm {
			case workspace.ArmShimVendorRefused:
				if cause.GetVendorRefused() == nil {
					t.Fatalf("result = %v, want vendor_refused", resp.Msg.GetResult())
				}
			case workspace.ArmShimFilesNotRestorable:
				if cause.GetFilesNotRestorable() == nil {
					t.Fatalf("result = %v, want files_not_restorable", resp.Msg.GetResult())
				}
			case workspace.ArmShimNoSession:
				if cause.GetNoSession() == nil {
					t.Fatalf("result = %v, want no_session", resp.Msg.GetResult())
				}
			case workspace.ArmShimFirstPrompt:
				if cause.GetFirstPrompt() == nil {
					t.Fatalf("result = %v, want first_prompt", resp.Msg.GetResult())
				}
			}
			if tc.carriesVendor {
				var got string
				switch tc.arm {
				case workspace.ArmShimVendorRefused:
					got = cause.GetVendorRefused().GetVendorMessage()
				case workspace.ArmShimFilesNotRestorable:
					got = cause.GetFilesNotRestorable().GetVendorMessage()
				}
				if got != "the vendor said no" {
					t.Fatalf("vendor_message = %q, want %q", got, "the vendor said no")
				}
			}
		})
	}
}

// TestRollBackSuccessReturnsThePromptAndFilesRestoredOnlyWhenRestoring pins
// that a success carries the rolled-back prompt always, and `files_restored`
// exactly when the plan restored files.
func TestRollBackSuccessReturnsThePromptAndFilesRestoredOnlyWhenRestoring(t *testing.T) {
	tests := []struct {
		name          string
		restore       bool
		filesRestored int
	}{
		{name: "files kept", restore: false, filesRestored: 0},
		{name: "files restored", restore: true, filesRestored: 3},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Feed.prompts = feedIDs("p1")
			h.Feed.rollbackTargetOK = true
			h.Feed.rollbackTarget = feed.RollbackTarget{Turns: []ids.TurnID{"t1"}, Said: said("roll me back")}
			h.Verbs.rollBackResult = workspace.RollbackResult{FilesRestored: tc.filesRestored}
			req := keepFilesPlan()
			if tc.restore {
				req = restoreFilesPlan()
			}
			token := planToken(t, h, req)

			// Act.
			resp, err := h.Client.RollBack(context.Background(), connect.NewRequest(rollBackRequest(token)))

			// Assert.
			if err != nil {
				t.Fatalf("RollBack: %v", err)
			}
			success := resp.Msg.GetSuccess()
			if success == nil {
				t.Fatalf("result = %v, want success", resp.Msg.GetResult())
			}
			if got := promptText(success.GetPrompt()); got != "roll me back" {
				t.Fatalf("prompt = %q, want %q", got, "roll me back")
			}
			if tc.restore {
				if success.GetFilesRestored() == nil || int(success.GetFilesRestored().GetFiles()) != tc.filesRestored {
					t.Fatalf("files_restored = %v, want %d files", success.GetFilesRestored(), tc.filesRestored)
				}
			} else if success.GetFilesRestored() != nil {
				t.Fatalf("files_restored = %v, want unset when files were kept", success.GetFilesRestored())
			}
		})
	}
}

// TestRollBackSuccessWithASelectedPromptEndsTheSelection pins that a
// successful rollback of a SELECTED prompt ends that selection and pushes
// return_to_tail, exactly as SelectFeedRow's own CLEAR does: the row rolled
// back to is gone, so nothing can still be selected.
// TestRollBackSuccessHandsBackAQuotedPromptsWordsAlone pins that a rolled-back
// reply returns to the composer as the person's words, its quote left out.
func TestRollBackSuccessHandsBackAQuotedPromptsWordsAlone(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.prompts = feedIDs("p1")
	h.Feed.rollbackTargetOK = true
	h.Feed.rollbackTarget = feed.RollbackTarget{Turns: []ids.TurnID{"t1"}, Said: replyquote.Quote(said("roll me back"), "Paris.", false)}
	token := planToken(t, h, keepFilesPlan())

	// Act.
	resp, err := h.Client.RollBack(context.Background(), connect.NewRequest(rollBackRequest(token)))

	// Assert.
	if err != nil {
		t.Fatalf("RollBack: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetPrompt(); !proto.Equal(got, said("roll me back")) {
		t.Fatalf("prompt = %v, want the words without the quote", got)
	}
}

func TestRollBackSuccessWithASelectedPromptEndsTheSelection(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.prompts = feedIDs("p1", "p2")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := openRootWatch(t, h, ctx)
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(promptStep(newer))); err != nil {
		t.Fatalf("seed the newest prompt: %v", err)
	}
	receiveSelection(t, stream)
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(promptStep(older))); err != nil {
		t.Fatalf("step onto p1: %v", err)
	}
	receiveSelection(t, stream)
	h.Feed.rollbackTargetOK = true
	h.Feed.rollbackTarget = feed.RollbackTarget{Turns: []ids.TurnID{"t1"}, Said: said("x")}
	h.Verbs.rollBackResult = workspace.RollbackResult{}
	token := planToken(t, h, keepFilesPlan())

	// Act.
	resp, err := h.Client.RollBack(context.Background(), connect.NewRequest(rollBackRequest(token)))

	// Assert.
	if err != nil {
		t.Fatalf("RollBack: %v", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("result = %v, want success", resp.Msg.GetResult())
	}
	sel := receiveSelection(t, stream)
	if sel.GetNone().GetReturnToTail() == nil {
		t.Fatalf("selection = %v, want none{return_to_tail} after the rolled-back selection ended", sel)
	}
}

// TestExcerptOf pins excerptOf's shape: every text block joined with a single
// space, internal whitespace collapsed, and truncation at exactly 60 RUNES
// (not bytes) with an ellipsis appended past that bound.
func TestExcerptOf(t *testing.T) {
	sixty := strings.Repeat("a", 60)
	sixtyOne := strings.Repeat("a", 61)
	multibyte := strings.Repeat("日", 65)
	tests := []struct {
		name string
		said *conversationv1.UserSaid
		want string
	}{
		{name: "multiple blocks join with one space", said: multiBlockSaid("Hello", "world"), want: "Hello world"},
		{name: "internal whitespace collapses to single spaces",
			said: multiBlockSaid("Hello\n\n  world   foo"), want: "Hello world foo"},
		{name: "exactly sixty runes is unchanged", said: multiBlockSaid(sixty), want: sixty},
		{name: "sixty-one runes truncates with an ellipsis", said: multiBlockSaid(sixtyOne), want: sixty + "…"},
		{name: "a multibyte prompt truncates by runes, not bytes",
			said: multiBlockSaid(multibyte), want: strings.Repeat("日", 60) + "…"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := excerptOf(tc.said)

			// Assert.
			if got != tc.want {
				t.Fatalf("excerptOf(...) = %q, want %q", got, tc.want)
			}
		})
	}
}
