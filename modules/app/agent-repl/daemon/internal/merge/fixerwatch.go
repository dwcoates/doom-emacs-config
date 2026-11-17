package merge

import (
	"context"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/inflight"
	"agent-repld/internal/wsm"
)

// fixerStoppedSummary is what the user is told when a dispatched fixer's turn
// ends with no fix of its own under way.
const fixerStoppedSummary = "the deploy fixer's turn ended without landing a fix for the problems it was dispatched; the queue waits for you"

// watchFixer settles check M of queue Q, dispatched to a fixer, by where
// the fixer's turn stands: the check is escalated to the user when that turn
// ended with no finish of the fixer's own under way (owner ruling,
// 2026-11-17), since a fixer that stops without fixing would otherwise hold
// the queue forever (2026-11-17 16:20, held 45 minutes). It NEVER WAITS: a
// turn still under way ends with an edge that runs the next pass
// (FixerTurnEnded), and a turn that ended while no daemon watched has its
// close in the turn store, which the boot's pass reads. THE TURN IS THE
// CHECK'S OWN RECORD (wsm.Mediation.FixerTurn), the latest brief the daemon
// gave the fixer; one held or never submitted is brought on (bringTurnOn). A
// dispatch an earlier build recorded names no turn: its fixer's open turns
// stand for it. A finish under way owns the queue from there
// (runRemediationFinish), and its failed gate briefs the fixer again
// (briefFixer).
func (o *orchestrator) watchFixer(ctx context.Context, q checkedQueue, m wsm.Mediation, fields dlog.Context) {
	fixer := m.Remediation
	fields = withField(fields, "remediation", string(fixer))
	finish, err := o.finishUnderWay(ctx, q.key)
	if err != nil {
		o.mediationFailed(ctx, fields, "could not read whether the fixer's finish is under way; the queue stays held for the fixer", err)
		return
	}
	if finish != nil && finish.Workspace == fixer {
		o.deps.Log.Global().Info(opMediation, "the fixer's finish is under way; the queue stays held for its deploy", fields)
		return
	}
	turn := m.FixerTurn
	if turn.ID == "" {
		open, err := o.deps.DB.OpenTurns(ctx, fixer)
		if err != nil {
			o.mediationFailed(ctx, fields, "could not read the fixer's open turns; the queue stays held for the fixer", err)
			return
		}
		if len(open) > 0 {
			o.deps.Log.Global().Info(opMediation, "the dispatch names no fixer turn and the fixer's turns are under way; their end runs the check's next round", withField(fields, "open_turns", len(open)))
			return
		}
	} else {
		fields = withField(fields, "turn", string(turn.ID))
		place, _, err := o.bringTurnOn(ctx, fixer, fields, turn.ID, turn.Brief, conversationv1.PromptOrigin(turn.Origin))
		if err != nil {
			o.mediationFailed(ctx, fields, "could not tell where the fixer's turn stands; the queue stays held for the fixer until the next round", err)
			return
		}
		if place == turnUnderWay {
			o.deps.Log.Global().Info(opMediation, "the fixer's turn is under way; its end runs the check's next round", fields)
			return
		}
	}
	if err := o.escalateForIntake(ctx, q.key, fixer, fixerStoppedSummary, fields); err != nil {
		o.mediationFailed(ctx, fields, "the fixer stopped without a fix but its check could not be escalated; the queue stays held", err)
	}
}

// FixerTurnEnded implements Orchestrator: a turn of WS ended, and when WS is
// the fixer an owed check is dispatched to, a pass settles that check
// (watchFixer). Any other workspace's turn is no check's.
func (o *orchestrator) FixerTurnEnded(ctx context.Context, ws wsm.WorkspaceID) {
	role, err := o.deps.DB.MediationRoleOf(ctx, ws)
	if err != nil {
		o.mediationFailed(ctx, dlog.Context{"workspace": string(ws)}, "could not read whether the workspace whose turn ended is a dispatched fixer; its check is settled by the next pass", err)
		return
	}
	if role == nil || !role.Remediation || !role.Mediation.Dispatched {
		return
	}
	o.deps.Log.Global().Info(opMediation, "the dispatched fixer's turn ended; its check is settled", dlog.Context{"workspace": string(ws), "repo": string(role.Repo)})
	o.mediateAgain(ctx)
}

// briefFixer gives the fixer WS of queue REPO's dispatched check a later
// brief TEXT (a failed gate's), recorded on the check before it is submitted
// so the check waits on it (watchFixer), and runs a pass to wait on it. A
// fixer no dispatch is out to (an escalation's own finish) is briefed and
// not watched: the user holds that check.
func (o *orchestrator) briefFixer(ctx context.Context, repo wsm.RepoKey, ws wsm.WorkspaceID, text string, origin conversationv1.PromptOrigin, fields dlog.Context) error {
	turn := wsm.FixerTurn{ID: wsm.NewTurnID(), Brief: text, Origin: int32(origin)}
	watched := false
	m, err := o.deps.DB.MediationOf(ctx, repo)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the check the fixer's brief is for; it is not briefed", withField(fields, "error", err.Error()))
		return err
	}
	if m != nil && m.Dispatched && m.Remediation == ws {
		if watched, err = o.deps.DB.RecordFixerTurn(ctx, repo, m.OwedAt, ws, turn); err != nil {
			o.deps.Log.Global().Error(opMediation, "the fixer's brief could not be recorded on its check; it is not briefed", withField(fields, "error", err.Error()))
			return err
		}
	}
	if watched {
		// THE PASS RUNS EITHER WAY: a brief that did not reach the queue is
		// submitted again by the round that waits on it (reattachTurn).
		defer o.mediateAgain(ctx)
	}
	if _, err := o.submitCheckPrompt(ctx, ws, turn.ID, text, origin); err != nil {
		o.deps.Log.Global().Error(opMediation, "the fixer's brief could not be submitted", withField(withField(fields, "error", err.Error()), "watched", watched))
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
