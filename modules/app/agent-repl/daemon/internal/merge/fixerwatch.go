package merge

import (
	"context"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/inflight"
	"agent-repld/internal/wsm"
)

// fixerStoppedSummary is what the user is told when the mediator's fix turn
// ends with no finish under way.
const fixerStoppedSummary = "the deploy mediator's turn ended without landing a fix for the problems it dispatched; the queue waits for you"

// watchFixer settles check M of queue Q, dispatched to the mediator, by
// where the mediator's fix turn stands: the check is escalated to the user
// when that turn ended with no finish under way (owner ruling, 2026-11-17),
// since a mediator that stops without fixing would otherwise hold the queue
// forever (2026-11-17 16:20, held 45 minutes). It NEVER WAITS: a turn still
// under way ends with an edge that runs the next pass (FixerTurnEnded), and
// a turn that ended while no daemon watched has its close in the turn
// store, which the boot's pass reads. THE TURN IS THE CHECK'S OWN RECORD
// (wsm.Mediation.FixerTurn), the latest brief the daemon gave the mediator
// for its fix; one held or never submitted is brought on (bringTurnOn). A
// finish under way owns the queue from there (runRemediationFinish), and
// its failed gate briefs the mediator again (briefFixer).
func (o *orchestrator) watchFixer(ctx context.Context, q checkedQueue, m wsm.Mediation, fields dlog.Context) {
	finish, err := o.finishUnderWay(ctx, q.key)
	if err != nil {
		o.roundFailed(ctx, q, m, "could not read whether the deploy mediator's finish is under way", err, fields)
		return
	}
	if finish != nil {
		o.deps.Log.Global().Info(opMediation, "the mediator's finish is under way; the queue stays held for its deploy", withField(fields, "finishing", string(finish.Workspace)))
		return
	}
	mediator, found, err := o.deps.DB.DeployMediatorOf(ctx, q.key)
	if err != nil {
		o.roundFailed(ctx, q, m, "could not read the deploy mediator the fix is dispatched to", err, fields)
		return
	}
	// A DISPATCH IS ONLY EVER RECORDED BY THE MEDIATOR, with its turn
	// (wsm.DispatchMediation refuses one without), so neither is ever
	// missing from a dispatched check.
	if !found {
		panic(fmt.Sprintf("merge: the check of %s owed at %v is dispatched with no deploy mediator recorded", q.key, m.OwedAt))
	}
	turn := m.FixerTurn
	if turn.ID == "" {
		panic(fmt.Sprintf("merge: the check of %s owed at %v is dispatched with no fix turn recorded", q.key, m.OwedAt))
	}
	fields = withField(withField(fields, "mediator", string(mediator.Workspace)), "turn", string(turn.ID))
	place, closed, err := o.bringTurnOn(ctx, mediator.Workspace, fields, turn.ID, turn.Brief, conversationv1.PromptOrigin(turn.Origin))
	if err != nil {
		o.roundFailed(ctx, q, m, "could not tell where the deploy mediator's fix turn stands", err, fields)
		return
	}
	if place == turnUnderWay {
		o.deps.Log.Global().Info(opMediation, "the mediator's fix turn is under way; its end runs the check's next round", fields)
		return
	}
	// A FIX TURN THE USER INTERRUPTED IS THE USER'S (owner ruling,
	// 2026-11-18): nothing is escalated into the session they took over; the
	// queue stays held for the fix they drive the mediator to.
	if closed == wsm.CloseKilled {
		o.deps.Log.Global().Info(opMediation, "the user interrupted the mediator's fix turn; the fix is left to the user's turns with the mediator, and nothing is escalated", fields)
		return
	}
	if err := o.escalateForMediator(ctx, q.key, fixerStoppedSummary, fields); err != nil {
		o.roundFailed(ctx, q, m, "the deploy mediator stopped without a fix, and its check could not be read to escalate it", err, fields)
	}
}

// FixerTurnEnded implements Orchestrator: a turn of WS ended, and when WS is
// a deploy mediator whose check is dispatched, a pass settles that check
// (watchFixer). Any other workspace's turn is no check's.
func (o *orchestrator) FixerTurnEnded(ctx context.Context, ws wsm.WorkspaceID) {
	fields := dlog.Context{"workspace": string(ws)}
	repo, isMediator, err := o.deps.DB.MediatorRepoOf(ctx, ws)
	if err != nil {
		o.mediationFailed(ctx, fields, "could not read whether the workspace whose turn ended is a deploy mediator; its check is settled by the next pass", err)
		return
	}
	if !isMediator {
		return
	}
	m, err := o.deps.DB.MediationOf(ctx, repo)
	if err != nil {
		o.mediationFailed(ctx, withField(fields, "repo", string(repo)), "could not read the mediator's check after its turn ended; it is settled by the next pass", err)
		return
	}
	// A FINISH SENT BACK TO FIX FOR NO DISPATCHED CHECK has no fixer watch
	// (briefFixer): this turn's end settles it when the mediator stopped.
	if m == nil || !m.Dispatched {
		o.settleStoppedFix(ctx, repo, ws, withField(fields, "repo", string(repo)))
		return
	}
	o.deps.Log.Global().Info(opMediation, "the mediator's turn ended with its fix dispatched; its check is settled", withField(fields, "repo", string(repo)))
	o.mediateAgain(ctx)
}

// briefFixer gives the mediator WS of queue REPO a later brief TEXT (a
// failed gate's), recorded on the check before it is submitted when the
// check is dispatched, so the check waits on it (watchFixer), and runs a
// pass to wait on it. A brief while no fix is dispatched (an escalation's
// own finish) is submitted and not watched: the user holds that check.
func (o *orchestrator) briefFixer(ctx context.Context, repo wsm.RepoKey, ws wsm.WorkspaceID, text string, origin conversationv1.PromptOrigin, fields dlog.Context) error {
	turn := wsm.FixerTurn{ID: wsm.NewTurnID(), Brief: text, Origin: int32(origin)}
	watched := false
	m, err := o.deps.DB.MediationOf(ctx, repo)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the check the mediator's brief is for; it is not briefed", withField(fields, "error", err.Error()))
		return err
	}
	if m != nil && m.Dispatched {
		if watched, err = o.deps.DB.RecordFixerTurn(ctx, repo, m.OwedAt, turn); err != nil {
			o.deps.Log.Global().Error(opMediation, "the mediator's brief could not be recorded on its check; it is not briefed", withField(fields, "error", err.Error()))
			return err
		}
	}
	if watched {
		// THE PASS RUNS EITHER WAY: a brief that did not reach the queue is
		// submitted again by the round that waits on it (reattachTurn).
		defer o.mediateAgain(ctx)
	}
	if _, err := o.submitCheckPrompt(ctx, ws, turn.ID, text, origin); err != nil {
		o.deps.Log.Global().Error(opMediation, "the mediator's brief could not be submitted", withField(withField(fields, "error", err.Error()), "watched", watched))
		return err
	}
	return nil
}

// mediateAgain runs a mediation pass, as an edge that needs a round would.
func (o *orchestrator) mediateAgain(ctx context.Context) {
	if !o.async {
		o.Mediate(ctx)
		return
	}
	inflight.Go(func() { o.Mediate(ctx) })
}
