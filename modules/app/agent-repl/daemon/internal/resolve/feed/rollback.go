package feed

import (
	"context"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
)

// ROLLED-BACK TURNS (agentrepl.v1.RollBack): turns the user rolled the
// conversation back past. THE FEED NEVER DRAWS A ROW OF ONE, and the refusal
// lives at the one upsert door every row passes (upsertOne), so it holds for a
// live frame that was already in flight, for a history page replayed after a
// restart, and for the vendor transcript's abandoned branch the store keeps
// ingesting — the store has no delete, so the feed's gate is what hides them.
// The set is recorded durably and loaded when a process first builds the
// workspace's feed state, exactly as recorded rows are (recorded.go).

// RolledBackTurnStore is the durable record of rolled-back turns (wsm.DB).
type RolledBackTurnStore interface {
	RecordRolledBackTurns(ctx context.Context, id ids.WorkspaceID, turns []ids.TurnID) error
	RolledBackTurns(ctx context.Context, id ids.WorkspaceID) ([]ids.TurnID, error)
}

const (
	opRollbackApply   = "daemon.feed.rollback_apply"
	opRollbackRestore = "daemon.feed.rollback_restore"
)

// RollbackTarget is what a rollback to just before one prompt drops.
type RollbackTarget struct {
	// Turns is the prompt's turn and every later turn a rollback can reach,
	// oldest first.
	Turns []ids.TurnID
	// Said is the prompt as the user said it.
	Said *conversationv1.UserSaid
}

// RollbackTarget answers what rolling back to just before the prompt ROW
// drops; false when ROW is not a prompt a rollback can reach (RollbackPrompts).
func (r *resolver) RollbackTarget(ws ids.WorkspaceID, row *frontendv1.FeedId) (RollbackTarget, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s, ok := r.workspaces[ws]
	if !ok {
		return RollbackTarget{}, false
	}
	prompts := r.rollbackPrompts(s)
	for i, prompt := range prompts {
		if prompt.row.GetValue() != row.GetValue() {
			continue
		}
		target := RollbackTarget{Said: s.promptSaid[prompt.turn]}
		for _, later := range prompts[i:] {
			target.Turns = append(target.Turns, later.turn)
		}
		return target, true
	}
	return RollbackTarget{}, false
}

// rollbackPrompt is one prompt a rollback can reach.
type rollbackPrompt struct {
	row  *frontendv1.FeedId
	turn ids.TurnID
}

// rollbackPrompts answers the prompts a rollback can reach, oldest first: the
// root feed's prompt rows that OPEN a turn of the workspace's own current
// conversation, after its newest context cut (a clear or a compaction), whose
// prompt the shim has confirmed as said. A fork's inherited past is not the
// workspace's own; a prompt folded into a running turn opens no turn of its
// own, so rolling back to it would cut a turn in half; a prompt only the queue
// has mirrored is not yet known to the vendor. None is reachable.
func (r *resolver) rollbackPrompts(s *wsState) []rollbackPrompt {
	root := feedid.Feed{Root: true}
	f, ok := s.feeds[r.feedKey(s.id, root)]
	if !ok {
		return nil
	}
	var out []rollbackPrompt
	for _, id := range f.order[boundIndex(f, f.order)+1:] {
		row := f.rows[id]
		if row.GetUserPrompt() == nil || f.rank[id].plane.inheritedPast() {
			continue
		}
		turn := ids.TurnID(row.GetTurn().GetValue())
		if turn == "" || s.promptSaid[turn] == nil ||
			id != r.rowID(s.id, root, feedid.RowKey{Kind: feedid.KindPrompt, ID: string(turn)}).GetValue() {
			continue
		}
		out = append(out, rollbackPrompt{row: row.GetId(), turn: turn})
	}
	return out
}

// RollbackPrompts answers the prompt rows a rollback can reach, oldest first.
func (r *resolver) RollbackPrompts(ws ids.WorkspaceID) []*frontendv1.FeedId {
	r.mu.Lock()
	defer r.mu.Unlock()
	s, ok := r.workspaces[ws]
	if !ok {
		return nil
	}
	prompts := r.rollbackPrompts(s)
	out := make([]*frontendv1.FeedId, 0, len(prompts))
	for _, prompt := range prompts {
		out = append(out, prompt.row)
	}
	return out
}

// RollBackTurns removes TURNS from the feed for good: every row of one is
// retired from every feed, the feed refuses to draw any row of one from now
// on, and the turns are recorded so a later process refuses them too. The
// removal always happens; an error says only that recording failed (logged at
// ERROR and raised on the topbar here), so a daemon restart would draw the
// turns again.
func (r *resolver) RollBackTurns(ws ids.WorkspaceID, turns []ids.TurnID) error {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	for _, turn := range turns {
		s.rolledBack[turn] = true
		delete(s.promptSaid, turn)
	}
	retired := 0
	for key, f := range s.feeds {
		for _, id := range append([]string(nil), f.order...) {
			if s.rolledBack[ids.TurnID(f.rows[id].GetTurn().GetValue())] && r.retire(s, s.feedAddrs[key], id) {
				retired++
			}
		}
	}
	s.finalAnswers = keepDrawn(s, s.finalAnswers)
	log := r.logger(ws)
	fields := dlog.Context{"turns": len(turns), "first": firstTurn(turns), "retired": retired}
	if r.deps.RolledBack != nil {
		if err := r.deps.RolledBack.RecordRolledBackTurns(context.Background(), ws, turns); err != nil {
			fields["error"] = err.Error()
			log.Error(opRollbackApply,
				"rolled-back turns were removed from the feed but could not be recorded; a daemon restart will draw them again", fields)
			r.raiseWarning(s, opRollbackApply,
				fmt.Sprintf("the rolled-back prompts could not be saved and will reappear after a daemon restart: %v", err))
			return err
		}
	}
	log.Info(opRollbackApply, "rolled-back turns were removed from the feed and recorded", fields)
	return nil
}

// keepDrawn answers the final answers still drawn on some feed.
func keepDrawn(s *wsState, answers []*frontendv1.FeedId) []*frontendv1.FeedId {
	kept := answers[:0]
	for _, id := range answers {
		drawn := false
		for _, f := range s.feeds {
			if _, ok := f.rows[id.GetValue()]; ok {
				drawn = true
				break
			}
		}
		if drawn {
			kept = append(kept, id)
			continue
		}
		delete(s.answerMarkdown, id.GetValue())
		delete(s.finalAnswerSeen, id.GetValue())
	}
	return kept
}

func firstTurn(turns []ids.TurnID) string {
	if len(turns) == 0 {
		return ""
	}
	return string(turns[0])
}

// rolledBackRow reports whether ROW belongs to a rolled-back turn, which the
// upsert door refuses to draw.
func (r *resolver) rolledBackRow(s *wsState, f *feedState, row *frontendv1.FeedRow) bool {
	turn := row.GetTurn().GetValue()
	if turn == "" || !s.rolledBack[ids.TurnID(turn)] {
		return false
	}
	r.logger(s.id).Debug("daemon.feed.rolled_back_row_refused",
		"a row of a rolled-back turn was not drawn",
		dlog.Context{"feed": f.key, "row": row.GetId().GetValue(), "turn": turn})
	return true
}

// restoreRolledBack loads the workspace's rolled-back turns when a process
// first builds its feed state, before any row is drawn.
func (r *resolver) restoreRolledBack(s *wsState) {
	if r.deps.RolledBack == nil {
		return
	}
	turns, err := r.deps.RolledBack.RolledBackTurns(context.Background(), s.id)
	if err != nil {
		r.logger(s.id).Error(opRollbackRestore,
			"the workspace's rolled-back turns could not be loaded; their rows may be drawn again",
			dlog.Context{"error": err.Error()})
		r.raiseWarning(s, opRollbackRestore, "rolled-back prompts could not be loaded and may be drawn again")
		return
	}
	for _, turn := range turns {
		s.rolledBack[turn] = true
	}
	if len(turns) > 0 {
		r.logger(s.id).Info(opRollbackRestore, "the workspace's rolled-back turns were loaded; their rows are not drawn",
			dlog.Context{"turns": len(turns)})
	}
}

// LiveDetachedIn answers how many detached subagents and shells drawn in
// TURNS the feed still draws as live: the work a files-restoring rollback to
// the first of TURNS stops. A monitor has no detached head (its feed entry is
// its call's card), so monitors are not counted, though the rollback stops
// them too.
func (r *resolver) LiveDetachedIn(ws ids.WorkspaceID, turns []ids.TurnID) int {
	r.mu.Lock()
	defer r.mu.Unlock()
	s, ok := r.workspaces[ws]
	if !ok {
		return 0
	}
	dropped := make(map[string]bool, len(turns))
	for _, turn := range turns {
		dropped[string(turn)] = true
	}
	live := 0
	for _, f := range s.feeds {
		for _, row := range f.rows {
			if !dropped[row.GetTurn().GetValue()] {
				continue
			}
			// A detached shell's state is drawn on its HEAD (`shell_head`); its
			// `detached_shell` row is the spool body and carries no state.
			if row.GetDetachedSubagent().GetSubagent().GetLive() != nil ||
				row.GetShellHead().GetLive() != nil {
				live++
			}
		}
	}
	return live
}
