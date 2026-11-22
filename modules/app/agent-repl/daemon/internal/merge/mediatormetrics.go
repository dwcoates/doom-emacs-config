package merge

import (
	"context"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/wsm"
)

// THE DEPLOY MEDIATOR'S METRICS (owner request, 2026-11-20). A RUN of the
// mediator starts at a deploy's hot reload and ends at its approval, or at
// the landing of the finish that settled an escalation or a remediation
// (CountMediatorRun). Each turn the mediator ran is metered by the tokens it
// spent, labeled by the phase of the run the turn ended in
// (MediatorTurnSpent), and each compaction of its conversation by how long
// it took (compactMediator), or, after a remediation, by how long owing it to
// the session's next start took (compactMediatorAfterRemediation).

// The phases of a mediator's run a turn's tokens are labeled by.
const (
	// MediatorPhaseRollout: a deploy owes a check the mediator is not told
	// of yet; its hot reload is still rolling out.
	MediatorPhaseRollout = "rollout"
	// MediatorPhaseAssessing: the mediator is told of the deploy and judges
	// it.
	MediatorPhaseAssessing = "assessing"
	// MediatorPhaseFixing: the mediator fixes the problems it dispatched,
	// through its finish's gate and landing.
	MediatorPhaseFixing = "fixing"
	// MediatorPhaseEscalation: the check is put before the user, and the
	// mediator settles it with them, through its finish's gate and landing.
	MediatorPhaseEscalation = "escalation"
	// MediatorPhaseIdle: no run is under way.
	MediatorPhaseIdle = "idle"
)

// The kinds of token a mediator's turn is metered by.
const (
	// MediatorTokensFreshInput is the input the prompt cache did not serve,
	// cache writes included.
	MediatorTokensFreshInput = "fresh_input"
	// MediatorTokensCacheRead is the input the prompt cache served.
	MediatorTokensCacheRead = "cache_read"
	// MediatorTokensOutput is every generated token, thinking included.
	MediatorTokensOutput = "output"
)

// MediatorTokenKinds are every kind a turn records, each once per turn.
var MediatorTokenKinds = []string{MediatorTokensFreshInput, MediatorTokensCacheRead, MediatorTokensOutput}

// The ways a mediator's run ends.
const (
	// MediatorRunApproved: the mediator approved the deploy.
	MediatorRunApproved = "approved"
	// MediatorRunFinishedEscalation: the finish that settled an escalation
	// landed.
	MediatorRunFinishedEscalation = "finished_escalation"
	// MediatorRunFinishedRemediation: the finish of a fix the mediator
	// dispatched landed.
	MediatorRunFinishedRemediation = "finished_remediation"
)

// The ways a compaction of the mediator ends.
const (
	// MediatorCompactionCompacted: its conversation was compacted.
	MediatorCompactionCompacted = "compacted"
	// MediatorCompactionSkipped: it had no conversation to compact, or a turn
	// ran, so its next start compacts it.
	MediatorCompactionSkipped = "skipped"
	// MediatorCompactionFailed: the compaction, or owing it, failed.
	MediatorCompactionFailed = "failed"
	// MediatorCompactionOwed: the compaction was owed to the session's next
	// start and the session ended, after a remediation; no compaction ran,
	// and the duration is the owing's.
	MediatorCompactionOwed = "owed"
)

// mediatorCompactionOutcome is how a compaction that answered COMPACTED and
// ERR ended.
func mediatorCompactionOutcome(compacted bool, err error) string {
	switch {
	case err != nil:
		return MediatorCompactionFailed
	case compacted:
		return MediatorCompactionCompacted
	}
	return MediatorCompactionSkipped
}

// mediatorOwedCompactionOutcome is how owing a compaction that answered OWED
// and ERR ended: owed with the session ended, skipped (no conversation, or a
// turn ran, so its next start compacts it), or failed.
func mediatorOwedCompactionOutcome(owed bool, err error) string {
	switch {
	case err != nil:
		return MediatorCompactionFailed
	case owed:
		return MediatorCompactionOwed
	}
	return MediatorCompactionSkipped
}

// spentOf is SPEND's tokens of KIND, one of MediatorTokenKinds.
func spentOf(spend footer.TurnSpent, kind string) uint64 {
	switch kind {
	case MediatorTokensFreshInput:
		return spend.FreshInput
	case MediatorTokensCacheRead:
		return spend.CacheRead
	case MediatorTokensOutput:
		return spend.Output
	}
	panic("merge: a mediator token kind with no figure: " + kind)
}

// runEnd is the end of a queue's last mediator run in this process: when it
// ended and the phase it ended in, which labels the turn that ended it.
type runEnd struct {
	at    time.Time
	phase string
}

// endMediatorRun records that the mediator's run of queue REPO ended as
// OUTCOME, in PHASE: it is counted, and the turn that ended it is labeled by
// PHASE when that turn ends after the run did (mediatorPhase).
func (o *orchestrator) endMediatorRun(repo wsm.RepoKey, phase, outcome string) {
	o.deps.Meter.CountMediatorRun(outcome)
	o.mu.Lock()
	defer o.mu.Unlock()
	if o.runEnds == nil {
		o.runEnds = map[string]runEnd{}
	}
	o.runEnds[canonicalKey(repo)] = runEnd{at: o.deps.Now(), phase: phase}
}

// escalatedPhase is the phase a run ends in: the escalation when the check
// was escalated, otherwise UNESCALATED.
func escalatedPhase(escalated bool, unescalated string) string {
	if escalated {
		return MediatorPhaseEscalation
	}
	return unescalated
}

// finishPhase is the phase of the run finish F serves: the escalation's, or
// the fix's.
func finishPhase(f wsm.RemediationFinish) string {
	return escalatedPhase(f.ServesEscalation, MediatorPhaseFixing)
}

// finishRunEnd is how the landing of finish F ends its run.
func finishRunEnd(f wsm.RemediationFinish) string {
	if f.ServesEscalation {
		return MediatorRunFinishedEscalation
	}
	return MediatorRunFinishedRemediation
}

// MediatorTurnSpent implements Orchestrator: meters the tokens a turn of WS
// spent, by the phase of its run the turn ended in, when WS is a deploy
// mediator. Any other workspace's turn is no mediator's. A phase that cannot
// be read is recorded at ERROR, and the turn goes unmetered.
func (o *orchestrator) MediatorTurnSpent(ctx context.Context, ws wsm.WorkspaceID, spend footer.TurnSpent) {
	fields := dlog.Context{"workspace": string(ws), "turn": string(spend.Turn)}
	repo, isMediator, err := o.deps.DB.MediatorRepoOf(ctx, ws)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read whether the workspace whose turn ended is a deploy mediator; its turn's tokens are not metered", withField(fields, "error", err.Error()))
		return
	}
	if !isMediator {
		return
	}
	fields["repo"] = string(repo)
	phase, err := o.mediatorPhase(ctx, repo, spend.StartedAt)
	if err != nil {
		o.deps.Log.Global().Error(opMediation, "could not read the phase the deploy mediator's turn ended in; its tokens are not metered", withField(fields, "error", err.Error()))
		return
	}
	for _, kind := range MediatorTokenKinds {
		o.deps.Meter.ObserveMediatorTurnTokens(phase, kind, spentOf(spend, kind))
	}
	o.deps.Log.Global().Info(opMediation, "metered the deploy mediator's turn", withField(withField(withField(withField(fields,
		"phase", phase), "fresh_input", spend.FreshInput), "cache_read", spend.CacheRead), "output", spend.Output))
}

// mediatorPhase is the phase of the run of queue REPO a mediator's turn that
// started at STARTED ended in: a finish under way is its own escalation's or
// fix's; an owed check stands where its mediator took it; with neither, the
// turn ended the run when the run ended after it started, and is idle
// otherwise.
func (o *orchestrator) mediatorPhase(ctx context.Context, repo wsm.RepoKey, started time.Time) (string, error) {
	finish, err := o.finishUnderWay(ctx, repo)
	if err != nil {
		return "", err
	}
	if finish != nil {
		return finishPhase(*finish), nil
	}
	m, err := o.deps.DB.MediationOf(ctx, repo)
	if err != nil {
		return "", err
	}
	switch {
	case m == nil:
	case m.Dispatched:
		return MediatorPhaseFixing, nil
	case m.Escalated():
		return MediatorPhaseEscalation, nil
	case m.Prompted():
		return MediatorPhaseAssessing, nil
	default:
		return MediatorPhaseRollout, nil
	}
	o.mu.Lock()
	end, ended := o.runEnds[canonicalKey(repo)]
	o.mu.Unlock()
	if ended && !end.at.Before(started) {
		return end.phase, nil
	}
	return MediatorPhaseIdle, nil
}
