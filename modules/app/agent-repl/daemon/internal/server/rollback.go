package server

import (
	"context"
	"errors"
	"slices"
	"strings"
	"time"
	"unicode/utf8"

	"connectrpc.com/connect"
	"github.com/google/uuid"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/replyquote"
	"claude-repld/internal/workspace"
	"claude-repld/internal/wsm"
)

const opRollback = "daemon.server.rollback"

// rollbackExcerptRunes is how much of the prompt the confirmation names.
const rollbackExcerptRunes = 60

// rollbackPlan is what a planned rollback would do, as the confirmation
// stated it. The token names one; RollBack performs it only while planning
// again would state the same thing.
type rollbackPlan struct {
	ws       ids.WorkspaceID
	row      *frontendv1.FeedId
	selected bool
	restore  bool
	turns    []ids.TurnID
	since    time.Time
	dropHeld []ids.TurnID
	running  bool
	detached int
	excerpt  string
	said     *conversationv1.UserSaid
}

// sameEffects reports whether two plans would do the same thing.
func (p rollbackPlan) sameEffects(other rollbackPlan) bool {
	return p.ws == other.ws && p.row.GetValue() == other.row.GetValue() && p.restore == other.restore &&
		slices.Equal(p.turns, other.turns) && slices.Equal(p.dropHeld, other.dropHeld) &&
		p.running == other.running && p.detached == other.detached
}

// PlanRollback says what rolling back would do, for the user to confirm, and
// mints the token that names it. See endpoint_plan_rollback.proto.
func (s *server) PlanRollback(
	ctx context.Context,
	req *connect.Request[agentreplv1.PlanRollbackRequest],
) (*connect.Response[agentreplv1.PlanRollbackResponse], error) {
	const rpc = "PlanRollback"
	if req.Msg.GetFiles() == nil {
		return nil, invalid("files", "a rollback states whether files are restored")
	}
	if err := validateWorkspaceRef("workspace", req.Msg.GetWorkspace()); err != nil {
		return nil, err
	}
	resp := &agentreplv1.PlanRollbackResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	restore := req.Msg.GetRestoreFiles() != nil
	plan, ok, err := s.planRollback(ctx, subject.Record.ID, restore, nil)
	if err != nil {
		return nil, fail(subject.Log, rpc, err)
	}
	if !ok {
		subject.Log.Info(opRollback, "a rollback was asked for with no prompt to roll back to", dlog.Context{"restore_files": restore})
		resp.Result = &agentreplv1.PlanRollbackResponse_Success{Success: &agentreplv1.PlanRollbackSuccess{
			Outcome: &agentreplv1.PlanRollbackSuccess_NothingToRollBack{NothingToRollBack: &agentreplv1.PlanRollbackNothingToRollBack{}}}}
		return connect.NewResponse(resp), nil
	}
	token := uuid.New().String()
	s.mu.Lock()
	s.rollbackPlans[token] = plan
	s.mu.Unlock()
	subject.Log.Info(opRollback, "planned a rollback for the user to confirm", plan.context())
	resp.Result = &agentreplv1.PlanRollbackResponse_Success{Success: &agentreplv1.PlanRollbackSuccess{
		Outcome: &agentreplv1.PlanRollbackSuccess_Plan{Plan: plan.view(token)}}}
	return connect.NewResponse(resp), nil
}

// RollBack performs a confirmed plan. See endpoint_roll_back.proto.
func (s *server) RollBack(
	ctx context.Context,
	req *connect.Request[agentreplv1.RollBackRequest],
) (*connect.Response[agentreplv1.RollBackResponse], error) {
	const rpc = "RollBack"
	if req.Msg.GetToken().GetValue() == "" {
		return nil, invalid("token", "a rollback names the plan the user confirmed")
	}
	if err := validateWorkspaceRef("workspace", req.Msg.GetWorkspace()); err != nil {
		return nil, err
	}
	resp := &agentreplv1.RollBackResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	ws := subject.Record.ID
	// A TOKEN IS SPENT BY ITS FIRST USE, performed or refused: a confirmed plan
	// is one rollback.
	s.mu.Lock()
	planned, known := s.rollbackPlans[req.Msg.GetToken().GetValue()]
	delete(s.rollbackPlans, req.Msg.GetToken().GetValue())
	s.mu.Unlock()
	stale := func(why string) (*connect.Response[agentreplv1.RollBackResponse], error) {
		return answer(resp, s.refuse(subject.Log, rpc, resp, s.fill(refusal{Arm: "plan_stale", Reason: why, Info: true})))
	}
	if !known || planned.ws != ws {
		return stale("the plan is not one this daemon made for this workspace")
	}
	now, ok, err := s.planRollback(ctx, ws, planned.restore, planned.row)
	if err != nil {
		return nil, fail(subject.Log, rpc, err)
	}
	if !ok || !now.sameEffects(planned) {
		return stale("the conversation changed since the rollback was planned")
	}
	result, err := s.deps.Verbs.RollBack(ctx, ws, workspace.RollbackRequest{
		Turns: planned.turns, Since: planned.since, DropHeld: planned.dropHeld, RestoreFiles: planned.restore,
	})
	if err != nil {
		if errors.Is(err, promptqueue.ErrHoldsChanged) {
			return stale("the held prompts changed since the rollback was planned")
		}
		if refusal, ok := workspace.AsShimRefusal(err); ok {
			return answer(resp, s.refuse(subject.Log, rpc, resp, rollbackRefusal(s, refusal)))
		}
		if r, ok := s.asRefusal(err); ok {
			return answer(resp, s.refuse(subject.Log, rpc, resp, r))
		}
		return nil, fail(subject.Log, rpc, err)
	}
	// THE ROLLED-BACK PROMPT'S SELECTION ENDS WITH IT: its row is gone.
	if planned.selected {
		s.endSelection(subject.Log, ws, planned.row, returnToTail(), "rolled_back")
	}
	// THE COMPOSER HOLDS THE PERSON'S WORDS AGAIN, never a reply's quote: a
	// quote is the daemon's, made from a selection, and a resend replies to
	// whatever is selected when it is sent.
	success := &agentreplv1.RollBackSuccess{Prompt: replyquote.Words(planned.said)}
	if planned.restore {
		success.FilesRestored = &agentreplv1.RollBackFilesRestored{Files: uint32(result.FilesRestored)}
	}
	subject.Log.Info(opRollback, "the conversation was rolled back", planned.context())
	resp.Result = &agentreplv1.RollBackResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// rollbackRefusal maps the shim's refusal onto RollBackError's arm.
func rollbackRefusal(s *server, shim *workspace.ShimRefusal) refusal {
	r := refusal{Arm: shim.Arm, Reason: shim.Detail}
	switch shim.Arm {
	case workspace.ArmShimVendorRefused, workspace.ArmShimFilesNotRestorable:
		r.Fields = map[string]any{"vendor_message": shim.Detail}
	}
	return s.fill(r)
}

// planRollback works out what rolling back would do. ROW names the prompt
// (a plan being checked again); nil resolves it: the selected prompt, else
// the latest. ok is false when no prompt can be rolled back to.
func (s *server) planRollback(ctx context.Context, ws ids.WorkspaceID, restore bool, row *frontendv1.FeedId) (rollbackPlan, bool, error) {
	plan := rollbackPlan{ws: ws, restore: restore, row: row}
	if row == nil {
		if selected, kind, held := s.currentSelection(ws); held && kind == selectionPrompt {
			plan.row, plan.selected = selected, true
		} else if prompts := s.deps.Feed.RollbackPrompts(ws); len(prompts) > 0 {
			plan.row = prompts[len(prompts)-1]
		} else {
			return rollbackPlan{}, false, nil
		}
	}
	target, ok := s.deps.Feed.RollbackTarget(ws, plan.row)
	if !ok {
		return rollbackPlan{}, false, nil
	}
	plan.turns, plan.said = target.Turns, target.Said
	plan.excerpt = excerptOf(target.Said)
	since, err := s.deps.DB.TurnStartedAt(ctx, ws, target.Turns[0])
	switch {
	case errors.Is(err, wsm.ErrNotFound):
		// A PROMPT THE WORKSPACE NEVER RECORDED (one the vendor recorded first,
		// adopted from its transcript) has no queue position: no held prompt
		// was queued behind it, so none is dropped.
		since = time.Unix(1<<62, 0)
	case err != nil:
		return rollbackPlan{}, false, err
	}
	plan.since = since
	if plan.dropHeld, err = s.deps.Queue.HeldSince(ctx, ws, since); err != nil {
		return rollbackPlan{}, false, err
	}
	open, err := s.deps.DB.OpenTurns(ctx, ws)
	if err != nil {
		return rollbackPlan{}, false, err
	}
	plan.running = len(open) > 0
	if restore {
		plan.detached = s.deps.Feed.LiveDetachedIn(ws, target.Turns)
	}
	return plan, true, nil
}

// view is the plan as the confirmation draws it.
func (p rollbackPlan) view(token string) *agentreplv1.RollbackPlan {
	view := &agentreplv1.RollbackPlan{
		Token:  &agentreplv1.RollbackToken{Value: token},
		Target: &agentreplv1.RollbackPlanTarget{Excerpt: p.excerpt, PromptsDropped: uint32(len(p.turns))},
		Files:  &agentreplv1.RollbackPlanFiles{},
	}
	if p.selected {
		view.Target.Chosen = &agentreplv1.RollbackPlanTarget_Selected{Selected: &agentreplv1.RollbackPlanTargetSelected{}}
	} else {
		view.Target.Chosen = &agentreplv1.RollbackPlanTarget_Latest{Latest: &agentreplv1.RollbackPlanTargetLatest{}}
	}
	if p.restore {
		restored := &agentreplv1.RollbackPlanFilesRestored{}
		if p.detached > 0 {
			restored.CancelDetached = &agentreplv1.RollbackPlanCancelDetached{Items: uint32(p.detached)}
		}
		view.Files.Files = &agentreplv1.RollbackPlanFiles_Restored{Restored: restored}
	} else {
		view.Files.Files = &agentreplv1.RollbackPlanFiles_Kept{Kept: &agentreplv1.RollbackPlanFilesKept{}}
	}
	if p.running {
		view.Interrupt = &agentreplv1.RollbackPlanInterrupt{}
	}
	if len(p.dropHeld) > 0 {
		view.DropQueued = &agentreplv1.RollbackPlanDropQueued{Prompts: uint32(len(p.dropHeld))}
	}
	return view
}

func (p rollbackPlan) context() dlog.Context {
	return dlog.Context{
		"row": p.row.GetValue(), "selected": p.selected, "restore_files": p.restore,
		"turns": len(p.turns), "drop_held": len(p.dropHeld), "running": p.running, "detached": p.detached,
	}
}

// excerptOf is the prompt's opening words, for the confirmation to name it.
func excerptOf(said *conversationv1.UserSaid) string {
	var text strings.Builder
	for _, block := range said.GetContent().GetBlocks() {
		if t := block.GetText().GetText(); t != "" {
			if text.Len() > 0 {
				text.WriteByte(' ')
			}
			text.WriteString(t)
		}
	}
	words := strings.Join(strings.Fields(text.String()), " ")
	if utf8.RuneCountInString(words) <= rollbackExcerptRunes {
		return words
	}
	return string([]rune(words)[:rollbackExcerptRunes]) + "…"
}
