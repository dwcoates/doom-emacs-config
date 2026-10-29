package workspace

import (
	"context"
	"errors"
	"fmt"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/health"
	"agent-repld/internal/ids"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/resolve/topbar"
	"agent-repld/internal/wsm"
)

// AnswerPermission delivers a permission card's verdict to the agent that
// asked.
//
// The answer is ECHOED against what the daemon SERVED: the ask must still be
// standing, and allow_standing is legal ONLY when the ask actually OFFERED a
// standing grant — granting a standing permission the vendor never offered
// would write a rule the user was never shown.
func (v *verbs) AnswerPermission(ctx context.Context, ws ids.WorkspaceID, answer *conversationv1.AgentAnswer) error {
	_, log, err := v.owned(ctx, "AnswerPermission", ws)
	if err != nil {
		return err
	}

	decision := answer.GetPermissionDecision()
	if decision == nil {
		return refuse(log, "AnswerPermission", ArmUnservedAnswer,
			"the answer carries no permission decision", false)
	}
	ask := decision.GetAsk()
	if ask.GetValue() == "" {
		return refuse(log, "AnswerPermission", ArmUnservedAnswer,
			"the permission decision names no ask", true)
	}

	served, ok := v.deps.Cards.Permission(ws, ask)
	if !ok {
		return refuse(log, "AnswerPermission", ArmUnservedAnswer,
			fmt.Sprintf("no permission ask %q is standing", ask.GetValue()), true)
	}

	if standing := decision.GetAllowed().GetStanding(); standing != nil {
		if served.StandingFor == nil {
			return refuse(log, "AnswerPermission", ArmNoStandingOffer,
				fmt.Sprintf("permission ask %q offered no standing grant", ask.GetValue()), false)
		}
		// The grant DELIVERED is the one the ask offered, never the one the
		// client echoed back: a client that rewrote the changes would be
		// writing rules of its own.
		standing.Standing = served.StandingFor
	}

	shim, live := v.deps.Shim(ws)
	if !live {
		return refuse(log, "AnswerPermission", ArmNoSession,
			fmt.Sprintf("workspace %q has no live session to answer through", ws), false)
	}
	if err := shim.Answer(ctx, served.Agent, answer); err != nil {
		if refusal, ok := AsShimRefusal(err); ok {
			// The shim's own arm reaches the caller: `not_deliverable` (the SDK
			// has no route to that agent) is a different answer from
			// `no_open_ask` (the card is stale), and the user acts differently
			// on each. Neither is hidden.
			return refuse(log, "AnswerPermission", refusal.Arm, refusal.Detail, false)
		}
		log.Error(opAnswerPerm, "the permission answer was not accepted", dlog.Context{
			"ask": ask.GetValue(), "cause": err.Error(),
		})
		return fmt.Errorf("answer permission %q: %w", ask.GetValue(), err)
	}
	log.Info(opAnswerPerm, "delivered the permission verdict", dlog.Context{
		"ask": ask.GetValue(), "agent": served.Agent.GetValue(),
	})
	return nil
}

// AnswerQuestion delivers a question card's answer, echoing the SERVED batch.
//
// Two echo rules are enforced here and nowhere else:
//
//   - every answered question's text, and every chosen label, must be one the
//     batch actually served — an unserved value means the client is answering a
//     stale card;
//   - a SINGLE-SELECT question takes exactly one choice, so a multi-pick on one
//     is refused rather than silently truncated.
func (v *verbs) AnswerQuestion(ctx context.Context, ws ids.WorkspaceID, answer *conversationv1.AgentAnswer) error {
	_, log, err := v.owned(ctx, "AnswerQuestion", ws)
	if err != nil {
		return err
	}

	given := answer.GetQuestionAnswer()
	if given == nil {
		return refuse(log, "AnswerQuestion", ArmUnservedAnswer, "the answer carries no question answer", false)
	}
	ask := given.GetAsk()
	if ask.GetValue() == "" {
		return refuse(log, "AnswerQuestion", ArmAskNotStanding, "the question answer names no ask", true)
	}
	served, ok := v.deps.Cards.Question(ws, ask)
	if !ok {
		return refuse(log, "AnswerQuestion", ArmAskNotStanding,
			fmt.Sprintf("no question batch %q is standing", ask.GetValue()), true)
	}
	if err := validateQuestionEcho(log, served.Batch, given.GetAnswers()); err != nil {
		return err
	}

	shim, live := v.deps.Shim(ws)
	if !live {
		return refuse(log, "AnswerQuestion", ArmNoSession,
			fmt.Sprintf("workspace %q has no live session to answer through", ws), false)
	}
	if err := shim.Answer(ctx, served.Agent, answer); err != nil {
		if refusal, ok := AsShimRefusal(err); ok {
			return refuse(log, "AnswerQuestion", refusal.Arm, refusal.Detail, false)
		}
		log.Error(opAnswerQuestion, "the question answer was not accepted", dlog.Context{
			"ask": ask.GetValue(), "cause": err.Error(),
		})
		return fmt.Errorf("answer question %q: %w", ask.GetValue(), err)
	}
	log.Info(opAnswerQuestion, "delivered the question answer", dlog.Context{
		"ask": ask.GetValue(), "agent": served.Agent.GetValue(), "answers": len(given.GetAnswers().GetAnswers()),
	})
	return nil
}

// validateQuestionEcho checks each selection against the served batch.
func validateQuestionEcho(log dlog.Logger, batch *conversationv1.AgentQuestionBatch, answers *conversationv1.AgentQuestionAnswers) error {
	asked := map[string]*conversationv1.AgentQuestionAsked{}
	for _, q := range batch.GetQuestions() {
		asked[q.GetQuestion().GetText()] = q
	}
	for _, selection := range answers.GetAnswers() {
		text := selection.GetQuestion().GetText()
		question, ok := asked[text]
		if !ok {
			log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "!ok"})
			return refuse(log, "AnswerQuestion", ArmUnservedAnswer,
				fmt.Sprintf("question %q was never served in this batch", text), false)
		}
		single := question.GetSingleSelect() != nil
		if single && len(selection.GetChosen()) > 1 {
			log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "single && len(selection.GetChosen()) > 1"})
			return refuse(log, "AnswerQuestion", ArmMultiPickOnSingleSelect,
				fmt.Sprintf("question %q is single-select but %d choices were made", text, len(selection.GetChosen())), false)
		}
		labels := map[string]bool{}
		for _, option := range question.GetSingleSelect().GetOptions() {
			labels[option.GetLabel().GetLabel()] = true
		}
		for _, option := range question.GetMultiSelect().GetOptions() {
			labels[option.GetLabel().GetLabel()] = true
		}
		for _, chosen := range selection.GetChosen() {
			if !labels[chosen.GetLabel().GetLabel()] {
				log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "!labels[chosen.GetLabel().GetLabel()]"})
				return refuse(log, "AnswerQuestion", ArmUnservedAnswer,
					fmt.Sprintf("question %q never offered the choice %q", text, chosen.GetLabel().GetLabel()), false)
			}
		}
	}
	return nil
}

// AnswerColdGate takes the user's choice on a standing cold gate, retires the
// gate AT ONCE, and starts the remediation it chose.
//
// THE GATE DISAPPEARS THE MOMENT THE DAEMON HAS THE CHOICE (owner ruling,
// 2026-09-29). The answer is validated against the SERVED MENU -- a compaction
// naming a model or a scope the menu did not offer is refused rather than sent
// -- then the gate is spent and its row retired from the feed, and the verb
// answers. The re-open it chose runs AFTER the answer, off this goroutine
// (Sessions.ResumeColdDetached): a compaction can run for a minute, and for that
// minute the gate stood as an inert card and then turned into a trace that a
// compaction's own context cut could hide.
//
// THE ACT IS THE FOOTER'S FROM THE CLICK TO THE OUTCOME (owner ruling,
// 2026-09-14). The request line is published before anything is dialed and
// refined by every phase the shim relays, and it is cleared on every way out.
// A re-open that FAILS opens the `cold_gate_reopen_failed` fault and stands the
// gate again from the facts it was first raised with: the session is still
// parked, so the choice is the user's again.
//
// THE PROMPTS HELD AT THE GATE ARE SETTLED BEFORE THE SESSION COMES BACK
// (owner ruling, 2026-10-13): clear discards them, pay and compact keep them
// for the user, and only compact-and-resume sends them
// (promptqueue.Queue.ColdGateAnswered). The re-open starts only after the
// queue has settled them, so the session-up release it raises cannot send a
// prompt the answer kept or discarded. A queue that cannot settle them
// refuses the answer, and the gate stands again.
//
// AN ANSWERED GATE MARKS ITS CACHE COLD first (cachelapse.Keeper.ColdGateAnswered),
// so the re-opened session owes no keep-alive until its next turn ends.
//
// AN ANSWERED GATE SHOWS ITS WORKSPACE (owner ruling, 2026-10-17). A hidden
// workspace's answer moves it out of the hidden section before the re-open
// starts, so the session the answer brings up is one the daemon runs and
// keeps warm, never one the cache keeper stands down on arrival. A show that
// cannot be recorded refuses the answer, and the gate stands again.
func (v *verbs) AnswerColdGate(ctx context.Context, ws ids.WorkspaceID, answer *agentreplv1.AnswerColdGateRequest) error {
	record, log, err := v.owned(ctx, "AnswerColdGate", ws)
	if err != nil {
		return err
	}
	if answer.GetChoice() == nil {
		return refuse(log, "AnswerColdGate", ArmUnservedRemediation, "the answer names no choice", false)
	}
	served, ok := v.deps.Cards.ColdGate(ws)
	if !ok {
		return refuse(log, "AnswerColdGate", ArmNoColdGate,
			fmt.Sprintf("no cold gate is standing on workspace %q", ws), false)
	}
	remediation, err := coldRemediation(log, served, answer)
	if err != nil {
		return err
	}
	choice := coldChoiceName(answer)
	// THE GATE IS SPENT, in one step with the check that it still stands: of
	// two answers racing for one gate, the second finds nothing to spend.
	if !v.deps.Cards.TakeColdGate(ws, served.VendorSessionID) {
		return refuse(log, "AnswerColdGate", ArmNoColdGate,
			fmt.Sprintf("the cold gate on workspace %q was already answered", ws), false)
	}
	// THE CACHE IS MARKED COLD BEFORE ANYTHING THE ANSWER CANNOT UNDO (owner
	// ruling, 2026-10-29: passing the gate by any answer makes no LLM request
	// of its own). Unmarked, the re-open's session would owe a keep-alive at
	// once, timed from the turn before the gate, so a mark that cannot be
	// written refuses the answer while its held prompts are still unsettled.
	if err := v.deps.CacheLapses.ColdGateAnswered(ctx, ws); err != nil {
		// The keeper recorded the failure at ERROR as its owner.
		v.reraiseRefusedColdGate(log, ws, served.VendorSessionID, choice, "the cache could not be marked cold", err)
		return fmt.Errorf("mark the cache of the workspace whose cold gate was answered, %q, cold: %w", ws, err)
	}
	if record.InHiddenSection() {
		if err := v.recordHidden(ctx, log, ws, false); err != nil {
			// recordHidden recorded the failure at ERROR as its owner.
			v.reraiseRefusedColdGate(log, ws, served.VendorSessionID, choice, "the workspace could not be shown", err)
			return fmt.Errorf("show the workspace whose cold gate was answered, %q: %w", ws, err)
		}
		log.Info(opColdGate, "the answered gate's workspace was hidden; it is shown again", dlog.Context{"choice": choice})
	}
	if err := v.deps.Queue.ColdGateAnswered(ctx, ws, coldGateFate(answer)); err != nil {
		// The queue recorded the failure at ERROR as its owner; what is the
		// verb's own is that the gate stands again, unspent.
		v.reraiseRefusedColdGate(log, ws, served.VendorSessionID, choice, "the prompts held at the cold gate could not be settled", err)
		return fmt.Errorf("settle the prompts held at the cold gate of %q: %w", ws, err)
	}
	v.deps.Feed.RetireRow(ws, rootFeed(), coldGateRowID(ws, served.VendorSessionID))

	v.spendingColdGate(ws, choice, footer.CompactionRequestLine(choice, served.Detail))
	log.Info(opColdGate, "answering the cold gate: "+choice, dlog.Context{"choice": choice})

	// THE RE-OPEN IS A SESSION BRING-UP, not a bare shim call, so it goes
	// through the fleet's one start path: the remediated resume must leave the
	// workspace with its session facts recorded, its session watcher installed
	// and its host view live, exactly as a cold start does.
	v.deps.Sessions.ResumeColdDetached(ws, ColdResume{
		VendorSessionID: served.VendorSessionID,
		Remediation:     remediation,
		// EVERY PHASE THE SHIM RELAYS BECOMES THIS ACT'S LINE. The footer
		// words the phase itself, the summarizer's spend included, exactly as
		// it words the same phase arriving on the session stream.
		OnPhase: func(progress *conversationv1.SessionCompactionProgress) {
			v.deps.Footer.SetColdGateAnswer(ws, &footer.ColdGateAnswer{Choice: choice, Progress: progress})
		},
	}, func(runCtx context.Context, err error) {
		v.coldGateSettled(runCtx, log, ws, served.VendorSessionID, choice, err)
	})
	return nil
}

// reraiseRefusedColdGate stands a spent gate again when its answer is
// refused before the re-open starts. The step that failed recorded its own
// failure at ERROR; WHY names it for this record.
func (v *verbs) reraiseRefusedColdGate(log dlog.Logger, ws ids.WorkspaceID, vendorSessionID, choice, why string, cause error) {
	v.deps.ColdGateAnswers.CountColdGateAnswer(ws, choice, GateAnswerRefused)
	stood := v.deps.Cards.ReraiseColdGate(ws, vendorSessionID)
	log.Info(opColdGate, why+"; the answer is refused", dlog.Context{
		"choice": choice, "gate_stands_again": stood, "cause": cause.Error(),
	})
	if !stood {
		log.Error(opColdGate, "the refused answer's gate could not stand again: its cold facts are gone",
			dlog.Context{"vendor_session_id": vendorSessionID})
	}
}

// coldGateSettled is an answered gate's outcome, once its re-open settles.
//
// On SUCCESS the session is up, so the footer's and the strip's parked state
// retire and the session's own facts take them back. The run's own context
// ending is this daemon standing down, not a fault. Any other failure -- a
// typed refusal (already recorded where it was raised), a bring-up that broke,
// or a start that outran its own bound -- opens the `cold_gate_reopen_failed`
// fault, which is the footer's line for it, and stands the gate again so the
// user can choose again.
//
// THE STAND-DOWN IS READ OFF THE RUN'S CONTEXT, NEVER OFF THE ERROR. A start
// that outran its bound fails with context.DeadlineExceeded too, and on
// 2026-10-20 that deadline was read as the daemon standing down: no fault, no
// gate, and a workspace left neither parked nor up, with nothing to retry it.
func (v *verbs) coldGateSettled(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, vendorSessionID, choice string, err error) {
	switch {
	case err == nil:
		// THE GATE RETIRES BEFORE THE ANSWER'S LINE DOES. With the answer's
		// line gone first, the footer would draw the still-standing gate for
		// one push: a gate the user already answered, shown again.
		v.deps.Cards.EndColdGate(ws, vendorSessionID)
		v.deps.Footer.SetColdGate(ws, footer.ColdGate{Standing: false})
		v.deps.Footer.SetColdGateAnswer(ws, nil)
		// AND THE STRIP STOPS SAYING IT TOO. The re-open starts a session, so
		// the topbar's own session facts arrive on its heels and the full view
		// returns; retiring the gate here is what lets them.
		v.deps.Topbar.SetColdGate(ws, topbar.ColdGate{Standing: false})
		v.deps.ColdGateAnswers.CountColdGateAnswer(ws, choice, GateAnswerResumed)
		log.Info(opColdGate, "answered the cold gate", dlog.Context{"choice": choice})
	case ctx.Err() != nil:
		v.deps.ColdGateAnswers.CountColdGateAnswer(ws, choice, GateAnswerAbandoned)
		v.deps.Footer.SetColdGateAnswer(ws, nil)
		log.Info(opColdGate, "the answered cold gate's re-open was abandoned as the daemon stood down",
			dlog.Context{"choice": choice, "cause": err.Error()})
	default:
		v.deps.ColdGateAnswers.CountColdGateAnswer(ws, choice, GateAnswerFailed)
		v.deps.Footer.SetColdGateAnswer(ws, nil)
		var refusal *Refusal
		if !errors.As(err, &refusal) {
			log.Error(opColdGate, "the re-open with the remediation failed", dlog.Context{"cause": err.Error()})
		}
		v.noteColdGateReopenFailed(ctx, log, ws, err)
		if !v.deps.Cards.ReraiseColdGate(ws, vendorSessionID) {
			log.Error(opColdGate, "the failed re-open's gate could not stand again: its cold facts are gone",
				dlog.Context{"choice": choice})
		}
	}
}

// spendingColdGate publishes one line of the act a gate's answer is spending.
func (v *verbs) spendingColdGate(ws ids.WorkspaceID, choice, line string) {
	v.deps.Footer.SetColdGateAnswer(ws, &footer.ColdGateAnswer{Choice: choice, Request: line})
}

// coldRemediation translates the answered gate into the shim's remediation,
// validating a compaction against the served menu. Both compact answers carry
// the same remediation: what tells them apart is the held prompts' fate
// (coldGateFate), never the shim's work.
func coldRemediation(log dlog.Logger, served ServedColdGate, answer *agentreplv1.AnswerColdGateRequest) (*conversationv1.SessionColdRemediation, error) {
	switch choice := answer.GetChoice().(type) {
	case *agentreplv1.AnswerColdGateRequest_Pay:
		log.Debug("daemon.workspace.transition_decision", "selected a workspace transition branch", dlog.Context{"function": "workspace", "branch": "pay"})
		return payRemediation(), nil
	case *agentreplv1.AnswerColdGateRequest_Clear:
		log.Debug("daemon.workspace.transition_decision", "selected a workspace transition branch", dlog.Context{"function": "workspace", "branch": "clear"})
		return &conversationv1.SessionColdRemediation{
			Remediation: &conversationv1.SessionColdRemediation_Clear{Clear: &conversationv1.SessionColdClear{}},
		}, nil
	case *agentreplv1.AnswerColdGateRequest_Compact:
		log.Debug("daemon.workspace.transition_decision", "selected a workspace transition branch", dlog.Context{"function": "workspace", "branch": "compact"})
		return compactRemediation(log, served, choice.Compact)
	case *agentreplv1.AnswerColdGateRequest_CompactAndResume:
		log.Debug("daemon.workspace.transition_decision", "selected a workspace transition branch", dlog.Context{"function": "workspace", "branch": "compact_and_resume"})
		return compactRemediation(log, served, choice.CompactAndResume)
	default:
		log.Debug("daemon.workspace.transition_decision", "selected a workspace transition branch", dlog.Context{"function": "workspace", "branch": "default"})
		return nil, refuse(log, "AnswerColdGate", ArmUnservedRemediation, "the answer names no choice", false)
	}
}

// compactRemediation is the compaction a compact answer asks for, refused
// unless the gate served a compact menu offering its model and scope.
func compactRemediation(log dlog.Logger, served ServedColdGate, compact *agentreplv1.AnswerColdGateCompact) (*conversationv1.SessionColdRemediation, error) {
	if served.Compact == nil {
		return nil, refuse(log, "AnswerColdGate", ArmUnservedRemediation,
			"the cold gate offered no compaction for this account", false)
	}
	model, scope := compact.GetModel(), compact.GetScope()
	if !servedModel(served.Compact.Models, model) {
		log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "!servedModel(served.Compact.Models, model)"})
		return nil, refuse(log, "AnswerColdGate", ArmUnservedRemediation,
			fmt.Sprintf("the compact menu never offered the model %q", model.GetName()), false)
	}
	if !servedScope(served.Compact.Scopes, scope) {
		log.Debug("daemon.workspace.flow_decision", "selected a workspace flow branch", dlog.Context{"function": "workspace", "condition": "!servedScope(served.Compact.Scopes, scope)"})
		return nil, refuse(log, "AnswerColdGate", ArmUnservedRemediation,
			fmt.Sprintf("the compact menu never offered the scope %s", scope), false)
	}
	return &conversationv1.SessionColdRemediation{
		Remediation: &conversationv1.SessionColdRemediation_Compact{
			Compact: &conversationv1.SessionColdCompact{Model: model, Scope: scope},
		},
	}, nil
}

// coldGateFate is what the answer does to the prompts held at the gate: only
// compact-and-resume sends them. The caller has validated the choice
// (coldRemediation), so an answer naming none is an invariant violation the
// queue refuses loudly.
func coldGateFate(answer *agentreplv1.AnswerColdGateRequest) promptqueue.GateFate {
	switch answer.GetChoice().(type) {
	case *agentreplv1.AnswerColdGateRequest_Clear:
		return promptqueue.GateDiscard
	case *agentreplv1.AnswerColdGateRequest_Pay, *agentreplv1.AnswerColdGateRequest_Compact:
		return promptqueue.GateKeep
	case *agentreplv1.AnswerColdGateRequest_CompactAndResume:
		return promptqueue.GateSend
	default:
		return 0
	}
}

func servedModel(models []*conversationv1.AgentModel, want *conversationv1.AgentModel) bool {
	for _, m := range models {
		if m.GetName() == want.GetName() {
			return true
		}
	}
	return false
}

func servedScope(scopes []conversationv1.SessionCompactScope, want conversationv1.SessionCompactScope) bool {
	for _, s := range scopes {
		if s == want {
			return true
		}
	}
	return false
}

func coldChoiceName(answer *agentreplv1.AnswerColdGateRequest) string {
	switch answer.GetChoice().(type) {
	case *agentreplv1.AnswerColdGateRequest_Pay:
		return footer.ChoicePay
	case *agentreplv1.AnswerColdGateRequest_Clear:
		return footer.ChoiceClear
	case *agentreplv1.AnswerColdGateRequest_Compact:
		return footer.ChoiceCompact
	case *agentreplv1.AnswerColdGateRequest_CompactAndResume:
		return footer.ChoiceCompactAndResume
	default:
		return "none"
	}
}

// noteColdGateReopenFailed records the failed re-open as the workspace's OWN
// fault, which is what puts its line on the footer.
//
// GROUNDED (docs/FOOTER-TOPOLOGY-AUDIT.md section 4): ResumeCold never called
// noteStartFailed, so unlike an ordinary bring-up death this raised no fault,
// no start-failed line and no dead link. The gate stayed standing, the strip
// kept saying the session was parked, and the user's answer did nothing —
// twice in one afternoon, with four daemon ERROR records and nothing drawn.
func (v *verbs) noteColdGateReopenFailed(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, cause error) {
	if ctx.Err() != nil {
		// A CANCELLED CONTEXT IS A DAEMON STANDING DOWN, not a fault to file.
		return
	}
	workspace := ws
	if _, err := v.deps.Health.OpenFault(ctx, wsm.Fault{
		Workspace: &workspace,
		Kind:      health.KindColdGateReopenFailed,
		Detail:    "the cold gate's answer re-opened the session and it did not come back",
		Evidence:  map[string]string{"cause": cause.Error()},
	}); err != nil {
		log.Error(opColdGate, "could not record the failed cold-gate re-open",
			dlog.Context{"cause": err.Error()})
	}
}
