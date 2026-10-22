package server

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"strings"
	"time"
	"unicode/utf8"

	"connectrpc.com/connect"
	"github.com/google/uuid"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/prose"
	"agent-repld/internal/replyquote"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
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
	// named marks a plan for the prompt the request named
	// (PlanRollbackRequest.prompt).
	named   bool
	restore bool
	// followed reports agent output drawn after the prompt
	// (feed.Resolver.PromptFollowed).
	followed bool
	// requireBare marks a plan minted without a confirmation because nothing
	// followed its prompt (the stop key's second press): it stays valid only
	// while nothing does.
	requireBare bool
	turns       []ids.TurnID
	since       time.Time
	dropHeld    []ids.TurnID
	running     bool
	detached    int
	excerpt     string
	said        *conversationv1.UserSaid
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
	named := req.Msg.GetPrompt()
	plan, ok, err := s.planRollback(ctx, subject.Record.ID, restore, named)
	if err != nil {
		return nil, fail(subject.Log, rpc, err)
	}
	if !ok && named != nil {
		// A NAMED PROMPT IS NEVER QUIETLY REPLACED by another: the reader chose
		// that one, so a prompt rolled back or cut off since is refused.
		return answer(resp, s.refuse(subject.Log, rpc, resp, s.fill(refusal{
			Arm:    "prompt_not_reachable",
			Reason: "the named prompt is not one a rollback can reach",
			Fields: map[string]any{"prompt": named},
			Info:   true,
		})))
	}
	plan.named = named != nil
	if !ok {
		subject.Log.Info(opRollback, "a rollback was asked for with no prompt to roll back to", dlog.Context{"restore_files": restore})
		resp.Result = &agentreplv1.PlanRollbackResponse_Success{Success: &agentreplv1.PlanRollbackSuccess{
			Outcome: &agentreplv1.PlanRollbackSuccess_NothingToRollBack{NothingToRollBack: &agentreplv1.PlanRollbackNothingToRollBack{}}}}
		return connect.NewResponse(resp), nil
	}
	token := s.mintRollbackToken(plan)
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
	if err := validateRollBackResend(req.Msg.Resend); err != nil {
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
	if planned.requireBare && now.followed {
		return stale("agent output landed after the prompt since the rollback was offered without a confirmation")
	}
	// THE RESEND IS COMPOSED, AND VALIDATED AS SUBMITPROMPT VALIDATES IT,
	// BEFORE ANYTHING IS ROLLED BACK: a resend that cannot be sent refuses
	// the whole call while the conversation is still whole.
	var resend *agentreplv1.SubmitPromptRequest
	if r := req.Msg.Resend; r != nil {
		said, ok := resendSaid(r, replyquote.Words(planned.said))
		if !ok {
			return answer(resp, s.refuse(subject.Log, rpc, resp, refusal{
				Arm:    "entry_not_found",
				Reason: fmt.Sprintf("no entry of the rolled-back prompt answers %q", r.GetEntry().GetLabel()),
				Fields: map[string]any{"label": r.GetEntry().GetLabel()},
				Info:   true,
			}))
		}
		resend = &agentreplv1.SubmitPromptRequest{
			Workspace: req.Msg.GetWorkspace(), Said: said, IdempotencyKey: r.GetIdempotencyKey(), Origin: r.GetOrigin(),
		}
		if err := validateSubmitPromptRequest(resend); err != nil {
			return nil, err
		}
	}
	result, err := s.deps.Verbs.RollBack(ctx, ws, workspace.RollbackRequest{
		Turns: planned.turns, Since: planned.since, DropHeld: planned.dropHeld, RestoreFiles: planned.restore,
	})
	if err != nil {
		if r, ok := s.rollBackRefusalOf(err); ok {
			return answer(resp, s.refuse(subject.Log, rpc, resp, r))
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
	if resend != nil {
		sent, err := s.SubmitPrompt(ctx, connect.NewRequest(resend))
		if err != nil {
			subject.Log.Error(opRollback, "the conversation was rolled back, but its resend could not be submitted",
				dlog.Context{"cause": err.Error(), "idempotency_key": resend.GetIdempotencyKey()})
			return nil, err
		}
		success.Resend = sent.Msg
		subject.Log.Info(opRollback, "the rollback's resend was submitted",
			dlog.Context{"idempotency_key": resend.GetIdempotencyKey(), "accepted": sent.Msg.GetSuccess() != nil})
	}
	resp.Result = &agentreplv1.RollBackResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// resendSaid composes what a rollback's resend R sends: its whole prompt, or
// WORDS (the rolled-back prompt as the person said it) with the entry R names
// saying R's text instead. ok is false when WORDS holds no entry answering
// the label an entry edit names.
func resendSaid(r *agentreplv1.RollBackResend, words *conversationv1.UserSaid) (*conversationv1.UserSaid, bool) {
	if whole := r.GetWhole(); whole != nil {
		return whole.GetSaid(), true
	}
	edit := r.GetEntry()
	said := proto.Clone(words).(*conversationv1.UserSaid)
	for _, block := range said.GetContent().GetBlocks() {
		text := block.GetText()
		if text == nil {
			continue
		}
		if replaced, ok := prose.ReplaceEntry(text.GetText(), edit.GetLabel(), edit.GetText()); ok {
			text.Text = replaced
			return said, true
		}
	}
	return nil, false
}

// validateRollBackResend requires a resend, when one is carried, to name its
// key, its origin and its edit, and an entry edit to name its label.
func validateRollBackResend(r *agentreplv1.RollBackResend) *connect.Error {
	if r == nil {
		return nil
	}
	if r.GetIdempotencyKey() == "" {
		return invalid("resend.idempotency_key", "a resend's idempotency key is required")
	}
	if r.GetOrigin() == conversationv1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED {
		return invalid("resend.origin", "a resend's origin is required")
	}
	switch edit := r.GetEdit().(type) {
	case *agentreplv1.RollBackResend_Whole:
		return validateUserSaid("resend.whole.said", edit.Whole.GetSaid())
	case *agentreplv1.RollBackResend_Entry:
		if edit.Entry.GetLabel() == "" {
			return invalid("resend.entry.label", "an entry edit names the entry's label")
		}
		return nil
	}
	return invalid("resend.edit", "a resend's edit is required")
}

// mintRollbackToken files PLAN under a fresh token and answers the token: the
// one way a plan becomes one RollBack can perform.
func (s *server) mintRollbackToken(plan rollbackPlan) string {
	token := uuid.New().String()
	s.mu.Lock()
	s.rollbackPlans[token] = plan
	s.mu.Unlock()
	return token
}

// planLatestPrompt plans a files-keeping rollback to just before WS's latest
// reachable prompt: what the stop key rolls back. ok is false, logged under
// SITUATION, when no prompt is reachable or the latest cannot be planned.
func (s *server) planLatestPrompt(ctx context.Context, ws ids.WorkspaceID, log dlog.Logger, situation string) (rollbackPlan, bool, error) {
	prompts := s.deps.Feed.RollbackPrompts(ws)
	if len(prompts) == 0 {
		log.Info(opRollback, situation+" and no prompt to roll back", nil)
		return rollbackPlan{}, false, nil
	}
	latest := prompts[len(prompts)-1]
	plan, ok, err := s.planRollback(ctx, ws, false, latest)
	if err != nil {
		return rollbackPlan{}, false, err
	}
	if !ok {
		log.Info(opRollback, situation+" and the latest prompt could not be planned", dlog.Context{"row": latest.GetValue()})
		return rollbackPlan{}, false, nil
	}
	return plan, true, nil
}

// unansweredRollback plans the rollback a turn stop that asked for one
// performs (agentrepl.v1.InterruptTurn.roll_back_unanswered): the latest
// prompt's, when a turn is open and nothing the agent produced follows the
// prompt. ok is false, and the stop only stops, otherwise.
func (s *server) unansweredRollback(ctx context.Context, ws ids.WorkspaceID, log dlog.Logger) (rollbackPlan, bool, error) {
	plan, ok, err := s.planLatestPrompt(ctx, ws, log, "the stop key found a turn to stop")
	if err != nil || !ok {
		return rollbackPlan{}, false, err
	}
	if !plan.running || plan.followed {
		log.Info(opRollback, "the stop key's prompt is answered or no turn is open, so the stop rolls nothing back", plan.context())
		return rollbackPlan{}, false, nil
	}
	log.Info(opRollback, "nothing follows the stop key's prompt, so the stop rolls it back", plan.context())
	return plan, true, nil
}

// rollBackErrorOf answers the RollBackError a refused rollback ERR is
// relayed as. ERR is always a refusal (workspace.InterruptOutcome
// .RollbackRefused), so a failure here is a defect, answered as one.
func (s *server) rollBackErrorOf(log dlog.Logger, rpc string, err error) (*agentreplv1.RollBackError, error) {
	r, ok := s.rollBackRefusalOf(err)
	if !ok {
		return nil, fail(log, rpc, fmt.Errorf("a refused rollback carried a failure, not a refusal: %w", err))
	}
	holder := &agentreplv1.RollBackResponse{}
	if !setResponseError(holder, r.Arm, r.Fields) {
		return nil, UnlandedArm(log, rpc, r.Arm, r.Reason, r.NotFound)
	}
	return holder.GetError(), nil
}

// interruptRollbackOffer answers what the stop key's second press offers for
// WS: nil when no prompt can be rolled back; `immediate`, with a token minted
// for the latest prompt, when nothing the agent produced follows it; `choose`
// when agent output does. See endpoint_interrupt.proto.
func (s *server) interruptRollbackOffer(ctx context.Context, ws ids.WorkspaceID, log dlog.Logger) (*agentreplv1.InterruptNothingRunningRollback, error) {
	plan, ok, err := s.planLatestPrompt(ctx, ws, log, "the stop key found nothing running")
	if err != nil || !ok {
		return nil, err
	}
	if plan.followed {
		log.Info(opRollback, "the stop key found nothing running; agent output follows the latest prompt, so the reader chooses", plan.context())
		return &agentreplv1.InterruptNothingRunningRollback{Offer: &agentreplv1.InterruptNothingRunningRollback_Choose{
			Choose: &agentreplv1.InterruptNothingRunningRollbackChoose{}}}, nil
	}
	plan.requireBare = true
	token := s.mintRollbackToken(plan)
	log.Info(opRollback, "the stop key found nothing running and nothing after the latest prompt; it is offered for rollback at once", plan.context())
	return &agentreplv1.InterruptNothingRunningRollback{Offer: &agentreplv1.InterruptNothingRunningRollback_Immediate{
		Immediate: &agentreplv1.InterruptNothingRunningRollbackImmediate{Token: &agentreplv1.RollbackToken{Value: token}}}}, nil
}

// rollBackRefusalOf answers the RollBackError arm a rollback the verbs
// refused is answered with: the queue's changed holds are a stale plan, and
// the shim's refusal names its own arm. false when ERR is a failure rather
// than a refusal.
func (s *server) rollBackRefusalOf(err error) (refusal, bool) {
	if !workspace.IsRollbackRefusal(err) {
		return refusal{}, false
	}
	if errors.Is(err, promptqueue.ErrHoldsChanged) {
		return s.fill(refusal{Arm: "plan_stale", Reason: "the held prompts changed since the rollback was planned", Info: true}), true
	}
	shim, ok := workspace.AsShimRefusal(err)
	if !ok {
		panic(fmt.Sprintf("server: a rollback refusal is neither changed holds nor the shim's: %v", err))
	}
	return rollbackRefusal(s, shim), true
}

// rollbackRefusal maps the shim's refusal onto RollBackError's arm.
func rollbackRefusal(s *server, shim *workspace.ShimRefusal) refusal {
	r := refusal{Arm: shim.Arm, Reason: shim.Detail}
	switch shim.Arm {
	case workspace.ArmShimFilesNotRestorable:
		r.Fields = map[string]any{"vendor_message": shim.Detail}
	}
	return s.fill(r)
}

// planRollback works out what rolling back would do. ROW names the prompt
// (a named prompt, or a plan being checked again); nil resolves it: the
// selected prompt, else the latest. ok is false when no prompt can be rolled back to.
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
	first, err := s.deps.DB.Turn(ctx, ws, target.Turns[0])
	since := first.StartedAt
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
	plan.followed = s.deps.Feed.PromptFollowed(ws, plan.row)
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
	switch {
	case p.named:
		view.Target.Chosen = &agentreplv1.RollbackPlanTarget_Named{Named: &agentreplv1.RollbackPlanTargetNamed{}}
	case p.selected:
		view.Target.Chosen = &agentreplv1.RollbackPlanTarget_Selected{Selected: &agentreplv1.RollbackPlanTargetSelected{}}
	default:
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
		"row": p.row.GetValue(), "selected": p.selected, "named": p.named, "restore_files": p.restore,
		"turns": len(p.turns), "drop_held": len(p.dropHeld), "running": p.running, "detached": p.detached,
		"followed": p.followed, "require_bare": p.requireBare,
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
