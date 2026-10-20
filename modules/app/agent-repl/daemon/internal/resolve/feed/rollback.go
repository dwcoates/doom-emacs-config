package feed

import (
	"context"
	"fmt"
	"slices"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
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
	opRollbackHold    = "daemon.feed.rollback_hold"
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
	for _, id := range f.order[cutIndex(f, f.order)+1:] {
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

// RollbackChoice is one prompt a rollback can reach, with what tells it apart
// from the others in a reader's choice.
type RollbackChoice struct {
	// Row is the prompt's root-feed row.
	Row *frontendv1.FeedId
	// Turn is the turn the prompt opened.
	Turn ids.TurnID
	// Said is the prompt as the user said it.
	Said *conversationv1.UserSaid
	// ContextTokens is how full the main agent's context was just before the
	// prompt (context.go); nil when no reading stands between the newest
	// context cut and the prompt.
	ContextTokens *uint64
}

// RollbackChoices answers the prompts a rollback can reach, oldest first, each
// with the context figure that stood before it.
func (r *resolver) RollbackChoices(ws ids.WorkspaceID) []RollbackChoice {
	r.mu.Lock()
	defer r.mu.Unlock()
	s, ok := r.workspaces[ws]
	if !ok {
		return nil
	}
	f, ok := s.feeds[r.feedKey(s.id, feedid.Feed{Root: true})]
	if !ok {
		return nil
	}
	cut := ""
	if i := cutIndex(f, f.order); i >= 0 {
		cut = f.rank[f.order[i]].key
	}
	prompts := r.rollbackPrompts(s)
	out := make([]RollbackChoice, 0, len(prompts))
	for _, prompt := range prompts {
		choice := RollbackChoice{Row: prompt.row, Turn: prompt.turn, Said: s.promptSaid[prompt.turn]}
		if tokens, ok := contextBefore(s, cut, f.rank[prompt.row.GetValue()].key); ok {
			choice.ContextTokens = &tokens
		}
		out = append(out, choice)
	}
	return out
}

// PromptFollowed reports whether agent output is drawn after the prompt ROW
// in the root feed: a response, thinking or tool card (an activity row), a
// detached subagent or shell bubble, a permission or a question card. A
// turn's end marker, a divider, a badge, a peer message and a command panel
// are not the agent answering the prompt, and a pinned row stands at the
// feed's bottom whatever it belongs to, so none of those counts. False when
// ROW is not on the root feed.
func (r *resolver) PromptFollowed(ws ids.WorkspaceID, row *frontendv1.FeedId) bool {
	r.mu.Lock()
	defer r.mu.Unlock()
	s, ok := r.workspaces[ws]
	if !ok {
		return false
	}
	f, ok := s.feeds[r.feedKey(s.id, feedid.Feed{Root: true})]
	if !ok {
		return false
	}
	at := slices.Index(f.order, row.GetValue())
	if at < 0 {
		return false
	}
	for _, id := range f.order[at+1:] {
		if !isPinKey(f.rank[id].key) && answersPrompt(f.rows[id]) {
			return true
		}
	}
	return false
}

// answersPrompt reports whether ROW is agent output a reader sees as the
// agent answering: a response bubble or a purple-titled card.
func answersPrompt(row *frontendv1.FeedRow) bool {
	switch row.GetRow().(type) {
	case *frontendv1.FeedRow_Activity, *frontendv1.FeedRow_DetachedSubagent, *frontendv1.FeedRow_ShellHead,
		*frontendv1.FeedRow_Permission, *frontendv1.FeedRow_Question:
		return true
	}
	return false
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
	return r.rollBackTurnsLocked(r.state(ws), ws, turns)
}

// rollBackTurnsLocked is RollBackTurns under the resolver's lock.
func (r *resolver) rollBackTurnsLocked(s *wsState, ws ids.WorkspaceID, turns []ids.TurnID) error {
	for _, turn := range turns {
		s.rolledBack[turn] = true
		delete(s.rollingBack, turn)
		delete(s.promptSaid, turn)
	}
	// The rows withheld while the rollback ran belong to turns now gone.
	s.withheld = slices.DeleteFunc(s.withheld, func(w withheldRow) bool { return s.rolledBack[w.turn] })
	retired := 0
	r.eachStoredRow(s, func(addr feedid.Feed, _ *feedState, id string, row *frontendv1.FeedRow) {
		if s.rolledBack[ids.TurnID(row.GetTurn().GetValue())] && r.retireRolledBack(s, addr, id) {
			retired++
		}
	})
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

// withheldRow is a row of a turn being rolled back, kept undrawn until the
// rollback settles (HoldTurns).
type withheldRow struct {
	turn    ids.TurnID
	at      placement
	row     *frontendv1.FeedRow
	durable bool
}

// HoldTurns withholds every row of TURNS that arrives from now on, because a
// rollback is cutting them away. The shim interrupts a running turn before it
// cuts, and the turn's interrupted ending reaches the feed before the cut is
// answered; held, it is never drawn only to vanish when RollBackTurns lands.
// Every hold ends in exactly one of RollBackTurns (the rows are dropped) or
// ReleaseTurns (the rows are drawn).
func (r *resolver) HoldTurns(ws ids.WorkspaceID, turns []ids.TurnID) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	for _, turn := range turns {
		s.rollingBack[turn] = true
	}
	r.logger(ws).Debug(opRollbackHold, "the rows of the turns being rolled back are withheld until the rollback settles",
		dlog.Context{"turns": len(turns), "first": firstTurn(turns)})
}

// ReleaseTurns ends the hold on TURNS for a rollback that did not happen: the
// rows withheld meanwhile are drawn, in the order they arrived.
func (r *resolver) ReleaseTurns(ws ids.WorkspaceID, turns []ids.TurnID) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.releaseTurnsLocked(r.state(ws), ws, turns)
}

// releaseTurnsLocked is ReleaseTurns under the resolver's lock.
func (r *resolver) releaseTurnsLocked(s *wsState, ws ids.WorkspaceID, turns []ids.TurnID) {
	for _, turn := range turns {
		delete(s.rollingBack, turn)
	}
	var replay []withheldRow
	s.withheld = slices.DeleteFunc(s.withheld, func(w withheldRow) bool {
		if s.rollingBack[w.turn] {
			return false
		}
		replay = append(replay, w)
		return true
	})
	for _, w := range replay {
		r.upsert(s, w.at, w.row, w.durable)
	}
	r.logger(ws).Info(opRollbackHold, "the rollback did not happen; the rows withheld while it ran are drawn",
		dlog.Context{"turns": len(turns), "first": firstTurn(turns), "drawn": len(replay)})
}

// WithholdDrawnTurns takes the rows already drawn for TURNS, which a rollback
// holds (HoldTurns), off the feed until the rollback settles.
//
// THE SHIM ANSWERS A ROLLBACK ONCE ITS CUT IS MADE and restarts the vendor
// behind the answer (conversation.v1.SessionRollbackSettled), so the prompt
// the user took back would stay drawn for the whole vendor boot, some 1.7s
// (2026-10-16). Each such row is snapshotted with its place in the order and
// retired; the settle retires it for good (RollBackTurns) or draws it again
// where it stood (ReleaseTurns). A row that cannot be snapshotted stays drawn.
func (r *resolver) WithholdDrawnTurns(ws ids.WorkspaceID, turns []ids.TurnID) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	cut := map[ids.TurnID]bool{}
	for _, turn := range turns {
		cut[turn] = s.rollingBack[turn]
	}
	withheld := 0
	r.eachStoredRow(s, func(addr feedid.Feed, f *feedState, id string, row *frontendv1.FeedRow) {
		turn := ids.TurnID(row.GetTurn().GetValue())
		if !cut[turn] {
			return
		}
		snapshot, ok := r.snapshotRow(s, row, unclonable{
			operation: opRollbackHold,
			message:   "a drawn row of a turn being rolled back could not be snapshotted; it stays drawn until the rollback settles",
			context:   dlog.Context{"feed": f.key, "row": id, "turn": string(turn)},
		})
		if !ok {
			return
		}
		rank := f.rank[id]
		durable := !f.nonDurable[id]
		if !r.retireRolledBack(s, addr, id) {
			return
		}
		s.withheld = append(s.withheld, withheldRow{turn: turn, at: placement{feed: addr, inherit: &rank}, row: snapshot, durable: durable})
		withheld++
	})
	r.logger(ws).Info(opRollbackHold, "the cut was made; the rolled-back turns' drawn rows are withheld until the vendor's restart settles",
		dlog.Context{"turns": len(turns), "first": firstTurn(turns), "withheld": withheld})
}

// settleRollbackLocked takes how a rollback's restart settled
// (conversation.v1.SessionRollbackSettled): the turns this feed holds for it
// are removed for good when the vendor resumed at the cut, and drawn again
// where they stood, with a warning raised, when it did not. Turns it holds
// none of (a failed try's rewind by the prompt queue, a settle restated on a
// re-opened watch) change nothing.
func (r *resolver) settleRollbackLocked(s *wsState, ws ids.WorkspaceID, settled *conversationv1.SessionRollbackSettled) {
	var held []ids.TurnID
	for _, turn := range settled.GetDroppedTurns() {
		if s.rollingBack[ids.TurnID(turn.GetValue())] {
			held = append(held, ids.TurnID(turn.GetValue()))
		}
	}
	log := r.logger(ws)
	fields := dlog.Context{"dropped": len(settled.GetDroppedTurns()), "held": len(held), "first": firstTurn(held)}
	if len(held) == 0 {
		fields["outcome"] = rollbackOutcomeName(settled)
		log.Info(opRollbackApply, "a rollback settled for turns this feed holds none of; nothing changes", fields)
		return
	}
	switch outcome := settled.GetOutcome().(type) {
	case *conversationv1.SessionRollbackSettled_ResumedAtCut:
		_ = r.rollBackTurnsLocked(s, ws, held)
	case *conversationv1.SessionRollbackSettled_Refused:
		r.releaseTurnsLocked(s, ws, held)
		fields["vendor_message"] = outcome.Refused.GetVendorMessage()
		log.Warn(opRollbackApply, "the vendor refused the rollback's cut; its prompts are drawn again and stay in the conversation", fields)
		r.raiseWarning(s, opRollbackApply,
			fmt.Sprintf("the rollback did not happen: the vendor refused it, so its prompt is still in the conversation (%s)", outcome.Refused.GetVendorMessage()))
	case *conversationv1.SessionRollbackSettled_Failed:
		r.releaseTurnsLocked(s, ws, held)
		fields["detail"] = outcome.Failed.GetDetail()
		log.Error(opRollbackApply, "the rollback's restart failed; its prompts are drawn again and stay in the conversation", fields)
		r.raiseWarning(s, opRollbackApply,
			fmt.Sprintf("the rollback did not happen: the vendor could not restart at the cut, so its prompt is still in the conversation (%s)", outcome.Failed.GetDetail()))
	default:
		// A SETTLE WITH NO OUTCOME IS THE SHIM'S MALFORMED MESSAGE: whether the
		// cut stands is unknown, so the rows are drawn again, as for a cut that
		// did not happen, and the breach is recorded.
		r.releaseTurnsLocked(s, ws, held)
		log.Error(opRollbackApply, "a rollback settled with no outcome; its prompts are drawn again", fields)
		r.raiseWarning(s, opRollbackApply, "a rollback's outcome was not stated; its prompt is shown again")
	}
}

// rollbackOutcomeName names SETTLED's outcome arm for a log record.
func rollbackOutcomeName(settled *conversationv1.SessionRollbackSettled) string {
	switch settled.GetOutcome().(type) {
	case *conversationv1.SessionRollbackSettled_ResumedAtCut:
		return "resumed_at_cut"
	case *conversationv1.SessionRollbackSettled_Refused:
		return "refused"
	case *conversationv1.SessionRollbackSettled_Failed:
		return "failed"
	}
	return "unset"
}

// withheldRollingBack reports whether ROW belongs to a turn being rolled back,
// keeping a snapshot of it to draw should the rollback be refused.
func (r *resolver) withheldRollingBack(s *wsState, f *feedState, at placement, row *frontendv1.FeedRow, durable bool) bool {
	turn := ids.TurnID(row.GetTurn().GetValue())
	if turn == "" || !s.rollingBack[turn] {
		return false
	}
	snapshot, ok := r.snapshotRow(s, row, unclonable{
		operation: opRollbackHold,
		message:   "a row of a turn being rolled back could not be snapshotted; it is not drawn even if the rollback is refused",
		context:   dlog.Context{"feed": f.key, "row": row.GetId().GetValue(), "turn": string(turn)},
	})
	if !ok {
		return true
	}
	s.withheld = append(s.withheld, withheldRow{turn: turn, at: at, row: snapshot, durable: durable})
	r.logger(s.id).Debug(opRollbackHold, "a row of a turn being rolled back was withheld",
		dlog.Context{"feed": f.key, "row": row.GetId().GetValue(), "turn": string(turn)})
	return true
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
