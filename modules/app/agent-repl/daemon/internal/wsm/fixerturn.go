package wsm

import (
	"context"
	"errors"
	"time"

	"agent-repld/internal/dlog"
)

// mediationFixerTurnDDL is the layout-166 addition: the latest brief the
// daemon gave the fixer a check's fix is dispatched to (Mediation.FixerTurn),
// so the check waits on that turn and is escalated when it ends with no fix
// under way (owner ruling, 2026-11-17), across restarts. IT IS ADDITIVE:
// defaulted columns the previous build never names.
const mediationFixerTurnDDL = `
ALTER TABLE merge_queue_repos ADD COLUMN mediation_fixer_turn TEXT NOT NULL DEFAULT '';
ALTER TABLE merge_queue_repos ADD COLUMN mediation_fixer_brief TEXT NOT NULL DEFAULT '';
ALTER TABLE merge_queue_repos ADD COLUMN mediation_fixer_origin INTEGER NOT NULL DEFAULT 0;
`

// FixerTurn is one brief the daemon gave a dispatched fixer: the turn it
// was submitted as, its text, and its prompt origin (the contract's
// conversation.v1.PromptOrigin value), so a turn a restart cut before it
// reached the queue is submitted again as it was.
type FixerTurn struct {
	ID     TurnID
	Brief  string
	Origin int32
}

// valid refuses a brief with no turn or no text.
func (t FixerTurn) valid() error {
	if t.ID == "" || t.Brief == "" {
		return errors.New("wsm: a fixer's brief names its turn and its text")
	}
	return nil
}

// RecordFixerTurn records TURN as the latest brief the daemon gave the
// mediator while the mediation REPO owes at OWED is dispatched. It answers
// false when that mediation is no longer the one owed, or is not
// dispatched.
func (s *store) RecordFixerTurn(ctx context.Context, repo RepoKey, owed time.Time, turn FixerTurn) (bool, error) {
	const op = "daemon.wsm.record_fixer_turn"
	if err := turn.valid(); err != nil {
		s.log.Error(op, "refused a fixer's brief that is incomplete", withError(dlog.Context{"repo": string(repo)}, err))
		return false, err
	}
	return s.mediationWrite(ctx, op, repo, owed,
		`UPDATE merge_queue_repos SET mediation_fixer_turn = ?, mediation_fixer_brief = ?, mediation_fixer_origin = ?
		 WHERE mediation_dispatched = 1 AND repo_key = ? AND mediation_owed_at = ?`,
		string(turn.ID), turn.Brief, turn.Origin)
}
