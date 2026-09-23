package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/figures"
	"claude-repld/internal/sessionwatcher"
)

// THE SUBAGENT BUBBLE — sync or detached, ONE component. The bubble IS a feed:
// its rows never ride this row. This is the COLLAPSED HEAD, carried on the
// parent feed so every bubble paints from the parent's one connection; the
// child's own connection exists only while expanded.

// drawSubagent draws a spawn's bubble head. detached selects the placement
// wrapper — sync-vs-detached is PLACEMENT, never a second drawing.
//
// A FRAME THAT NAMES NO CREATED AGENT, BEFORE ANY FRAME HAS, DRAWS NOTHING YET.
// See subagentState.held: the row's identity carries the created agent. The
// start always states it, and a success MAY (AgentSubagentSuccess.created_agent_id),
// which is what lets a settled-only delivery — a replayed history, a transcript
// read with no live producer watching — draw an addressable row with no start
// ever arriving.
func (r *resolver) drawSubagent(s *wsState, at placement, act *conversationv1.AgentActivity, spawn *conversationv1.AgentSubagent, detached bool) (*frontendv1.FeedRow, error) {
	unitID := act.GetActivityId().GetValue()
	state, ok := s.subagents[unitID]
	if !ok {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "!ok"})
		state = &subagentState{}
		s.subagents[unitID] = state
	}
	// THE MARK IS CLAIMED UNCONDITIONALLY, never behind the flag: a detachment
	// announced before this unit drew is exactly the case the flag cannot
	// carry, and leaving the mark standing would report the unit as one
	// nothing ever drew.
	announcedWork, announcedDetached := s.claimDetached(unitID)
	if detached || announcedDetached {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "detached || announcedDetached"})
		state.detached = true
	}
	switch {
	case announcedDetached:
		state.work = announcedWork
	case detached:
		// BORN DETACHED: the announcement's own handle is this unit's id (the
		// created arm is drawn under an activity keyed by the work).
		state.work = unitID
	}
	if state.bubble == nil {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "state.bubble == nil"})
		state.bubble = &frontendv1.FeedSubagent{}
	}

	_, isStart := spawn.GetResult().(*conversationv1.AgentSubagent_Start)
	// A START IS TAKEN AS NAMING THE AGENT WHATEVER IT CARRIES: created_agent_id
	// is not optional there, so a start with an empty one is a producer fault
	// that still retires the hold rather than joining it — a start held against
	// itself would wait for a frame that has already arrived.
	namesAgent := isStart || namedCreatedAgent(spawn).GetValue() != ""
	if !namesAgent && state.created.GetValue() == "" {
		if !subagentArmDraws(spawn) {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "!subagentArmDraws(spawn)"})
			return nil, errNotARow
		}
		// THE PLACEMENT IS RECORDED WITH THE HOLD: the frames were carried on
		// this feed, and the retirement that draws them has no frame of its
		// own to place them from.
		state.feed = at
		state.held = append(state.held, spawn)
		r.logger(s.id).Debug("daemon.feed.subagent_held",
			"a spawn's frame arrived before any frame named the created agent; it is held until one does",
			dlog.Context{"unit": unitID, "held": len(state.held)})
		return nil, nil
	}

	// THE NAMING FRAME RETIRES THE HOLD, and the frames are folded in the order
	// THE RUN happened rather than the order they arrived: a start is the run's
	// first frame however late it lands, so it folds before what was held; any
	// other naming frame is later than everything held, so it folds after. Fold
	// a settled terminal before a held update and the update would draw the
	// finished bubble live again.
	held := state.held
	state.held = nil
	if !isStart {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "!isStart"})
		r.foldHeldSubagentFrames(s, unitID, state, held, "when a later frame named the created agent")
		held = nil
	}
	if err := r.foldSubagentFrame(s, unitID, state, spawn); err != nil {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "err := r.foldSubagentFrame(s, unitID, state, spawn); err != nil"})
		// THE HOLD IS PUT BACK rather than lost with the frame that failed:
		// this frame drew nothing, so nothing has named the created agent yet
		// and what was waiting is still waiting.
		state.held = held
		return nil, err
	}
	r.foldHeldSubagentFrames(s, unitID, state, held, "once its start landed")

	return r.composeSubagent(s, at, unitID, state, commissionOf(spawn)), nil
}

// namedCreatedAgent answers the created agent this frame states, if its arm
// states one at all. The start always does; a success does when its producer
// knew the id, which is what makes a settled-only delivery addressable.
func namedCreatedAgent(spawn *conversationv1.AgentSubagent) *conversationv1.AgentId {
	switch frame := spawn.GetResult().(type) {
	case *conversationv1.AgentSubagent_Start:
		return frame.Start.GetCreatedAgentId()
	case *conversationv1.AgentSubagent_Success:
		return frame.Success.GetCreatedAgentId()
	}
	return nil
}

// foldHeldSubagentFrames folds frames released from the hold, reporting any
// that cannot be folded rather than dropping them silently.
func (r *resolver) foldHeldSubagentFrames(s *wsState, unitID string, state *subagentState, held []*conversationv1.AgentSubagent, occasion string) {
	for _, frame := range held {
		if err := r.foldSubagentFrame(s, unitID, state, frame); err != nil {
			r.logger(s.id).Error("daemon.feed.subagent_held_frame_undrawable",
				"a held spawn frame could not be folded "+occasion,
				dlog.Context{"unit": unitID, "cause": err.Error()})
		}
	}
}

// subagentArmDraws reports whether a spawn frame carries an arm this family
// draws. An unset or unknown arm is never held: a hold exists to be folded,
// and a frame nothing can fold would sit until the turn's terminal reported it.
func subagentArmDraws(spawn *conversationv1.AgentSubagent) bool {
	switch spawn.GetResult().(type) {
	case *conversationv1.AgentSubagent_Start,
		*conversationv1.AgentSubagent_Update,
		*conversationv1.AgentSubagent_Success,
		*conversationv1.AgentSubagent_Failure:
		return true
	}
	return false
}

// foldSubagentFrame folds ONE frame onto the bubble the state carries. It is
// the whole of a frame's effect on the head, so a held frame folded later is
// folded exactly as a live one would have been.
func (r *resolver) foldSubagentFrame(s *wsState, unitID string, state *subagentState, spawn *conversationv1.AgentSubagent) error {
	bubble := state.bubble
	switch frame := spawn.GetResult().(type) {
	case *conversationv1.AgentSubagent_Start:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "foldSubagentFrame", "branch": "case *conversationv1.AgentSubagent_Start"})
		state.created = frame.Start.GetCreatedAgentId()
		applyPrompt(bubble, frame.Start.GetPrompt())
		// The ORIGINAL instant: a start is re-announced on the work's own
		// stream when the spawn detaches, and it repeats the same instant, so
		// the clock never resets when work moves.
		bubble.Runtime = &frontendv1.FeedSubagentRuntime{StartedAtMs: frame.Start.GetStartedAt().GetAtMs()}
		// A START AFTER A TERMINAL DOES NOT REOPEN. The same re-announcement
		// the instant above is guarded against also replays the start of work
		// that has ALREADY SETTLED, and taking it as live would un-settle a
		// finished bubble that nothing will ever settle again.
		if _, settled := bubble.GetState().(*frontendv1.FeedSubagent_Settled); !settled {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "_, settled := bubble.GetState().(*frontendv1.FeedSubagent_Settled); !settled"})
			bubble.State = &frontendv1.FeedSubagent_Live{Live: &frontendv1.FeedSubagentLive{}}
		}
	case *conversationv1.AgentSubagent_Update:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "foldSubagentFrame", "branch": "case *conversationv1.AgentSubagent_Update"})
		applyPrompt(bubble, frame.Update.GetPrompt())
		progress := frame.Update.GetProgress()
		if progress.GetTotalTokens() > 0 {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "progress.GetTotalTokens() > 0"})
			bubble.Tokens = &frontendv1.FeedSubagentTokens{Text: figures.Tokens(progress.GetTotalTokens()) + " tok"}
		}
		bubble.State = &frontendv1.FeedSubagent_Live{Live: &frontendv1.FeedSubagentLive{
			LastProgress: &frontendv1.FeedSubagentLastProgress{AtMs: r.deps.Now().UnixMilli()},
		}}
	case *conversationv1.AgentSubagent_Success:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "foldSubagentFrame", "branch": "case *conversationv1.AgentSubagent_Success"})
		// A SETTLED FRAME MAY NAME THE CREATED AGENT, and when it does this is
		// the only place the row's identity can come from — no start is coming
		// on a settled-only delivery. It never OVERWRITES with nothing: a
		// producer that did not know the id leaves it unset, and whatever the
		// start already told us stands.
		if created := frame.Success.GetCreatedAgentId(); created.GetValue() != "" {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "created := frame.Success.GetCreatedAgentId(); created.GetValue() != \"\""})
			state.created = created
		}
		applyPrompt(bubble, frame.Success.GetPrompt())
		applyTotals(bubble, frame.Success.GetTotals())
		// THE SETTLED SPAN, kept so a settled-only replay (no start frame) can
		// reconstruct the clock's start as end − duration. See subagentStart.
		state.durationMs = frame.Success.GetTotals().GetDurationMs()
		bubble.State = &frontendv1.FeedSubagent_Settled{Settled: &frontendv1.FeedSubagentSettled{
			EndedAtMs: frame.Success.GetSettledAt().GetAtMs(),
			Outcome:   &frontendv1.FeedSubagentSettled_Succeeded{Succeeded: &frontendv1.FeedSubagentSucceeded{}},
		}}
	case *conversationv1.AgentSubagent_Failure:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "foldSubagentFrame", "branch": "case *conversationv1.AgentSubagent_Failure"})
		settled := &frontendv1.FeedSubagentSettled{EndedAtMs: failureSettledMs(frame.Failure.GetError())}
		subagentFailureOutcome(r.logger(s.id), unitID, frame.Failure)(settled)
		bubble.State = &frontendv1.FeedSubagent_Settled{Settled: settled}
	default:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "foldSubagentFrame", "branch": "default"})
		return errNotARow
	}
	return nil
}

// composeSubagent renders the bubble the state now holds into its row, mints
// the sub-feed the row addresses, and draws the commission on it.
func (r *resolver) composeSubagent(s *wsState, at placement, unitID string, state *subagentState, commission *conversationv1.AgentSubagentPrompt) *frontendv1.FeedRow {
	bubble := state.bubble
	// THE CLOCK NEVER COUNTS FROM THE EPOCH. An authoritative start wins; a
	// settled-only replay reconstructs its start from the run's duration; a
	// live bubble with no start yet counts from a first-observed instant. Only
	// a zero here (the old fallback) drew the ~492762h clock.
	start := r.subagentStart(state)
	if bubble.Runtime == nil {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "bubble.Runtime == nil"})
		bubble.Runtime = &frontendv1.FeedSubagentRuntime{}
	}
	bubble.Runtime.StartedAtMs = start
	if bubble.Label == nil {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "bubble.Label == nil"})
		bubble.Label = &frontendv1.FeedSubagentLabel{Text: "Agent"}
	}

	stampDetachedWork(state)
	id := r.rowID(s.id, at.feed, feedid.RowKey{
		Kind: feedid.KindActivity, ID: unitID, Sub: state.created.GetValue(),
	})
	r.announceEntry(s, unitID, state.row, id)
	state.row = id
	state.feed = at

	// The bubble's own FeedId IS the sub-feed's address; recording it is what
	// makes an expand's OpenFeed resolve and a page's crumbs draw.
	if state.created.GetValue() != "" {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "state.created.GetValue() != \"\""})
		r.mintSubFeed(s, id, at.feed, feedid.Feed{Agent: state.created}, bubbleLabel(bubble))
		r.drawCommission(s, at, unitID, state, commission)
	}

	row := &frontendv1.FeedRow{Id: id}
	if state.detached {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "state.detached"})
		row.Row = &frontendv1.FeedRow_DetachedSubagent{DetachedSubagent: &frontendv1.FeedDetachedSubagent{
			Subagent: bubble,
		}}
	} else {
		row.Row = &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{
			Unit: &frontendv1.FeedTurnActivity_Subagent{Subagent: bubble},
		}}
	}
	return row
}

// stampDetachedWork puts the run's detached-work id on its head, and only on a
// detached one: a synchronous spawn is the turn's own progress and names none.
// A detached bubble whose handle the resolver never learned draws none rather
// than a guessed one.
func stampDetachedWork(state *subagentState) {
	if !state.detached || state.work == "" {
		state.bubble.WorkId = nil
		return
	}
	state.bubble.WorkId = &frontendv1.FeedDetachedWorkId{Text: state.work}
}

// announceEntry tells Deps.EntryPlaced where UNIT's entry is drawn, when that
// is news: the first draw, or a FeedId that changed. A redraw at the same
// address announces nothing, so the receiver is not woken on every frame.
func (r *resolver) announceEntry(s *wsState, unit string, previous, current *frontendv1.FeedId) {
	if r.deps.EntryPlaced == nil || unit == "" {
		return
	}
	if previous.GetValue() == current.GetValue() {
		return
	}
	r.logger(s.id).Debug("daemon.feed.entry_placed",
		"a detached-work-capable entry was placed; its address was handed to the footer",
		dlog.Context{"unit": unit, "row": current.GetValue(), "previous": previous.GetValue()})
	r.deps.EntryPlaced(s.id, unit, current)
}

// retireHeldSpawns draws every spawn whose frames are still waiting for one
// that names the created agent, and empties the hold.
//
// AN IDENTITY THAT NEVER CAME IS A PRODUCER FAULT, not a reason to lose the
// spawn. A settled-only delivery is no longer such a fault — the success arm
// can name the agent itself — so what reaches here is a spawn where neither a
// start nor a naming terminal ever arrived:
// the bubble is drawn from what did arrive, and it is warned, because its row
// carries no created agent and so addresses no sub-feed. Nothing is left held
// afterwards — a hold that outlived the delivery it was waiting on would sit
// in this workspace's state for the rest of the daemon's life.
func (r *resolver) retireHeldSpawns(s *wsState, occasion string) {
	log := r.logger(s.id)
	for unitID, state := range s.subagents {
		if len(state.held) == 0 {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "len(state.held) == 0"})
			continue
		}
		held := state.held
		state.held = nil
		log.Warn("daemon.feed.subagent_without_start",
			"a spawn's frames arrived with none of them naming the created agent; its bubble is drawn but addresses no sub-feed",
			dlog.Context{"unit": unitID, "frames": len(held), "occasion": occasion})
		var commission *conversationv1.AgentSubagentPrompt
		for _, frame := range held {
			if err := r.foldSubagentFrame(s, unitID, state, frame); err != nil {
				log.Error("daemon.feed.subagent_held_frame_undrawable",
					"a held spawn frame could not be folded at its retirement",
					dlog.Context{"unit": unitID, "cause": err.Error()})
				continue
			}
			if p := commissionOf(frame); p != nil {
				r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "p := commissionOf(frame); p != nil"})
				commission = p
			}
		}
		row := r.composeSubagent(s, state.feed, unitID, state, commission)
		r.stampTurn(s, row, nil)
		r.upsert(s, state.feed, row, true)
	}
}

// applyPrompt folds the commission's label and description onto the head. The
// description is UNSET when the spawn carried none: the head then draws the
// label alone, never a synthesized description.
func applyPrompt(bubble *frontendv1.FeedSubagent, prompt *conversationv1.AgentSubagentPrompt) {
	if prompt == nil {
		return
	}
	if t := prompt.GetSubagentType(); t != "" {
		bubble.Label = &frontendv1.FeedSubagentLabel{Text: t}
	}
	if prompt.Description != nil && prompt.GetDescription() != "" {
		bubble.Description = &frontendv1.FeedSubagentDescription{Text: prompt.GetDescription()}
	}
}

// commissionOf answers the commission carried on whichever arm this frame is.
// EVERY frame of a spawn restates it (agent_activity.proto: "Carried on every
// frame of the spawn, so each frame stands alone"), so the body redraws from
// the frame in hand rather than from a remembered one.
func commissionOf(spawn *conversationv1.AgentSubagent) *conversationv1.AgentSubagentPrompt {
	switch frame := spawn.GetResult().(type) {
	case *conversationv1.AgentSubagent_Start:
		return frame.Start.GetPrompt()
	case *conversationv1.AgentSubagent_Update:
		return frame.Update.GetPrompt()
	case *conversationv1.AgentSubagent_Success:
		return frame.Success.GetPrompt()
	}
	return nil
}

// drawCommission draws THE INSTRUCTION the subagent was given, on the
// subagent's OWN feed.
//
// WHERE THE PROTO PUTS IT. AgentSubagentPrompt.text is "the full instruction
// the subagent was given. Drawn only where there is room for it — A BUBBLE'S
// BODY, NOT ITS HEAD" (conversation/v1/agent_activity.proto), and a bubble's
// body IS its sub-feed (frontend/v1/feed.proto: "THE BUBBLE IS A FEED: its
// rows are never carried here"). So the commission is a row on the created
// agent's feed, drawn with the ONE kind the contract has for what an agent
// addressed to another agent — FeedAgentPrompt, on the recipient's end, whose
// address line is "from <sender>".
//
// The SENDER'S END IS THE BUBBLE ITSELF, which is why no second row is drawn
// on the caller's feed: the head already carries the label and the
// description, and the contract reserves the head for exactly those.
func (r *resolver) drawCommission(s *wsState, at placement, unitID string, state *subagentState, prompt *conversationv1.AgentSubagentPrompt) {
	if prompt.GetText() == "" {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "prompt.GetText() == \"\""})
		// A COMMISSION WITH NO INSTRUCTION DRAWS NOTHING rather than an empty
		// bubble body: the field is the whole row, and a blank one would say
		// the caller asked for nothing.
		return
	}
	sub := feedid.Feed{Agent: state.created}
	row := &frontendv1.FeedRow{
		Id: r.rowID(s.id, sub, feedid.RowKey{
			Kind: feedid.KindPrompt, ID: unitID, Sub: "commission",
		}),
		Row: &frontendv1.FeedRow_AgentPrompt{AgentPrompt: &frontendv1.FeedAgentPrompt{
			Address: &frontendv1.FeedAgentPromptAddress{
				Text: "from " + feedLabel(s, r.feedKey(s.id, at.feed)),
			},
			Body: &frontendv1.FeedAgentPromptBody{Blocks: []*frontendv1.FeedAgentPromptBlock{{
				Block: &frontendv1.FeedAgentPromptBlock_Text{
					Text: &frontendv1.FeedTextBlock{Text: prompt.GetText()},
				},
			}}},
		}},
	}
	r.stampTurn(s, row, nil)
	r.logger(s.id).Debug("daemon.feed.subagent_commission",
		"a spawn's commission was drawn on the subagent's own feed",
		dlog.Context{"unit": unitID, "agent": state.created.GetValue()})
	r.upsert(s, placement{feed: sub}, row, true)
}

// applyTotals folds a settled run's token sum onto the head. The two usage
// arms are the spawn path's honesty: a sync run states the full breakdown, an
// async one at most a total.
func applyTotals(bubble *frontendv1.FeedSubagent, totals *conversationv1.AgentSubagentTotals) {
	if totals == nil {
		return
	}
	switch usage := totals.GetUsage().(type) {
	case *conversationv1.AgentSubagentTotals_Full:
		// THE RUN'S TOTAL, every token it consumed — cache reads included. The
		// head draws ONE figure standing for the whole run, the same quantity
		// the live path shows (AgentSubagentProgress.total_tokens, the vendor's
		// running grand total) and the async path shows (total_only.total_tokens),
		// so the number does not change basis when a live bubble settles. The
		// cache-read bucket is CHEAP but it is still tokens the run consumed:
		// dropping it (as this once did) understated the total by the cached
		// context a subagent reads, which for a Claude Code run is most of it.
		// This is deliberately NOT the footer's "expensive sum" — that cell
		// answers "what did this turn cost", a different question with its own
		// component breakdown; this answers "how big was this run".
		full := usage.Full
		hits := full.GetInputHits()
		misses := full.GetInputMisses()
		sum := hits.GetRead() + misses.GetWritten() + misses.GetUnwritten() + full.GetOutputTokens()
		bubble.Tokens = &frontendv1.FeedSubagentTokens{Text: figures.Tokens(sum) + " tok"}
	case *conversationv1.AgentSubagentTotals_TotalOnly:
		if usage.TotalOnly.TotalTokens == nil {
			return
		}
		bubble.Tokens = &frontendv1.FeedSubagentTokens{
			Text: figures.Tokens(usage.TotalOnly.GetTotalTokens()) + " tok",
		}
	}
}

// subagentFailureOutcome picks the settled treatment. A person's stop is NOT a
// fault, and work we merely stopped being able to see is LOST rather than
// failed — the word carries the distinction so it never draws as a plain
// failure.
func subagentFailureOutcome(log dlog.Logger, unitID string, failure *conversationv1.AgentSubagentFailure) subagentOutcome {
	if cause := lostCauseOfSubagent(failure); cause != lostNone {
		return func(settled *frontendv1.FeedSubagentSettled) {
			lost := &frontendv1.FeedSubagentLost{}
			if !applySubagentLostHow(lost, cause) {
				log.Warn("daemon.feed.subagent_lost_unlanded_arm",
					"a spawn was lost in a way this build does not draw; the bubble carries no cause",
					dlog.Context{"unit": unitID, "cause": cause.String()})
			}
			settled.Outcome = &frontendv1.FeedSubagentSettled_Lost{Lost: lost}
		}
	}
	if _, stopped := failure.GetCause().(*conversationv1.AgentSubagentFailure_StoppedByUser); stopped {
		return func(settled *frontendv1.FeedSubagentSettled) {
			settled.Outcome = &frontendv1.FeedSubagentSettled_Cancelled{Cancelled: &frontendv1.FeedSubagentCancelled{}}
		}
	}
	return func(settled *frontendv1.FeedSubagentSettled) {
		settled.Outcome = &frontendv1.FeedSubagentSettled_Failed{Failed: &frontendv1.FeedSubagentFailed{}}
	}
}

// subagentOutcome sets a settled bubble's outcome arm. A setter rather than the
// generated oneof interface, whose method is unexported and unimplementable
// from here.
type subagentOutcome func(*frontendv1.FeedSubagentSettled)

// bubbleLabel composes the crumb a page inside the bubble draws: the
// commission's description when there is one, the type otherwise.
func bubbleLabel(bubble *frontendv1.FeedSubagent) string {
	if d := bubble.GetDescription().GetText(); d != "" {
		return d
	}
	return bubble.GetLabel().GetText()
}

// ---- DETACHED WORK: the announcement, and the shell's own bubble ----

// drawDetachedWork draws work that left the stream. It is announced HERE and
// nowhere else, so this is where a bubble first becomes a detached one.
func (r *resolver) drawDetachedWork(s *wsState, agent *conversationv1.AgentId, work *conversationv1.AgentDetachedWork) {
	log := r.logger(s.id)
	at := r.place(s, agent)
	workID := work.GetWork().GetValue()

	switch origin := work.GetOrigin().(type) {
	case *conversationv1.AgentDetachedWork_Detached:
		// ONE IDENTITY SPANS THE MOVE: the element already on screen continues
		// as a detached one rather than being replaced by a second drawing.
		unitID := origin.Detached.GetDetachedFromId().GetValue()
		if state, ok := s.subagents[unitID]; ok {
			state.detached = true
			state.work = workID
			r.republishSubagent(s, unitID, state)
			log.Debug("daemon.feed.detached_subagent",
				"a subagent bubble moved to its detached placement",
				dlog.Context{"unit": unitID, "work": workID})
			return
		}
		if r.detachForegroundShell(s, at, unitID, workID) {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "r.detachForegroundShell(s, at, unitID, workID)"})
			return
		}
		// A UNIT WHOSE KIND DRAWS NOTHING is not a unit that has yet to draw.
		// A monitor is footer-only and always detached, so its announcement
		// has no row to continue and never will; holding it would report the
		// footer's own bookkeeping as a producer fault at the turn's
		// terminal.
		if s.undrawable(unitID) {
			log.Debug("daemon.feed.detachment_draws_nothing",
				"a detachment named a unit whose kind draws no feed row; the footer carries the work",
				dlog.Context{"unit": unitID, "work": workID})
			return
		}
		// THE UNIT MAY SIMPLY NOT HAVE DRAWN YET. The announcement is held
		// against its identity so the unit lands through the detached
		// placement when it does draw; a mark still standing when the turn
		// ends is what earns the warning, in drawTerminal.
		s.markDetached(unitID, workID)
		log.Debug("daemon.feed.detachment_held",
			"a detachment named a unit this resolver has not drawn yet; it is held until the unit draws",
			dlog.Context{"unit": unitID, "work": workID})
	case *conversationv1.AgentDetachedWork_Created:
		switch created := origin.Created.GetWorkCreated().GetWork().(type) {
		case *conversationv1.DetachableWork_Subagent:
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawDetachedWork", "branch": "case *conversationv1.DetachableWork_Subagent"})
			act := &conversationv1.AgentActivity{
				ActivityId: &conversationv1.AgentActivityId{Value: workID},
				Item:       &conversationv1.AgentActivity_Subagent{Subagent: created.Subagent},
			}
			row, err := r.drawSubagent(s, at, act, created.Subagent, true)
			if err != nil {
				r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "err != nil"})
				return
			}
			if row == nil {
				r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "row == nil"})
				// HELD: the announcement carried a frame that is not the
				// spawn's start, so nothing names the created agent yet.
				return
			}
			r.stampTurn(s, row, nil)
			r.upsert(s, at, row, true)
		case *conversationv1.DetachableWork_Bash:
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawDetachedWork", "branch": "case *conversationv1.DetachableWork_Bash"})
			sh := s.shell(workID)
			sh.feed = at
			r.drawDetachedShell(s, work.GetWork(), created.Bash)
		default:
			// A workflow is kicked this wave, and a monitor is FOOTER-ONLY:
			// neither has a feed row.
			log.Debug("daemon.feed.detached_draws_nothing",
				"a detached-work kind draws no feed row", dlog.Context{"work": workID})
		}
	}
}

// detachForegroundShell turns an already-drawn foreground shell into its
// canonical detached bubble, answering whether there was one to turn.
//
// RETIRE THEN REDRAW. The foreground call drew a running tool card
// (KindActivity); its work has now moved to the background, where the shell
// bubble is its head. The head's kind (KindShellHead) differs from the card's,
// so the card cannot simply change arm under one identity the way a subagent
// bubble does — it is RETIRED, and the KindShellHead bubble is drawn in its
// place. The unit is marked moved so every later frame of it (the vendor's
// launch receipt, the other plane's replay, the next turn's live-work
// reconciliation) draws nothing rather than a second, stale card beside the
// bubble.
func (r *resolver) detachForegroundShell(s *wsState, at placement, unitID, workID string) bool {
	u, ok := s.units[unitID]
	if !ok || u.input == "" {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "!ok || u.input == \"\""})
		return false
	}
	cardID := r.rowID(s.id, at.feed, feedid.RowKey{Kind: feedid.KindActivity, ID: unitID})
	retired := r.retire(s, at.feed, cardID.GetValue())
	u.moved = true
	u.movedTo = workID
	sh := s.shell(workID)
	sh.command = u.input
	sh.startedAtMs = u.startedAtMs
	sh.feed = at
	// THE CALL MAY HAVE ENDED ALREADY. Its result and its move are separate
	// records, and a result drawn first is still the work's ending: the head
	// is drawn settled from it rather than live.
	r.publishShell(s, workID, sh, shellEnding(r.logger(s.id), workID, u.ending))
	r.logger(s.id).Debug("daemon.feed.detached_shell",
		"a foreground shell's running card was retired and redrawn as its detached shell bubble",
		dlog.Context{"unit": unitID, "work": workID, "card_retired": retired, "ended": u.ending != nil})
	return true
}

// applyHeldDetachment completes a detachment that was announced BEFORE the
// unit it named had drawn. A subagent claims its own mark while composing its
// bubble, because the mark decides which wrapper the bubble rides; a shell has
// no such choice, so its held detachment is applied once its foreground row
// exists.
func (r *resolver) applyHeldDetachment(s *wsState, at placement, unitID string) {
	work, held := s.claimDetached(unitID)
	if !held {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "!held"})
		return
	}
	if r.detachForegroundShell(s, at, unitID, work) {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "r.detachForegroundShell(s, at, unitID, work)"})
		return
	}
	// NOT DRAWABLE AS A SHELL AND NOT A SPAWN: the mark goes back, so the
	// turn's terminal still reports a detachment that never found its unit.
	s.markDetached(unitID, work)
}

// retireDetachment drops a detachment held against a unit whose kind draws no
// feed row. The mark exists so a row can ride the detached wrapper when it
// draws; a unit that will never draw one has nothing to hand it to, and a mark
// left standing is reported at the turn's terminal as a producer fault.
func (r *resolver) retireDetachment(s *wsState, unitID string) {
	work, held := s.claimDetached(unitID)
	if !held {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "!held"})
		return
	}
	r.logger(s.id).Debug("daemon.feed.detachment_retired",
		"a held detachment named a unit whose kind draws no feed row; the mark is retired",
		dlog.Context{"unit": unitID, "work": work})
}

// republishSubagent re-pushes a bubble whose placement wrapper changed.
func (r *resolver) republishSubagent(s *wsState, unitID string, state *subagentState) {
	stampDetachedWork(state)
	row := &frontendv1.FeedRow{Id: state.row}
	if state.detached {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "state.detached"})
		row.Row = &frontendv1.FeedRow_DetachedSubagent{DetachedSubagent: &frontendv1.FeedDetachedSubagent{
			Subagent: state.bubble,
		}}
	} else {
		row.Row = &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{
			Unit: &frontendv1.FeedTurnActivity_Subagent{Subagent: state.bubble},
		}}
	}
	r.stampTurn(s, row, nil)
	r.upsert(s, state.feed, row, true)
}

// drawDetachedShell draws one detached shell's bubble: the command head and
// the spool's TAIL, capped by the daemon and replaced whole on every push.
func (r *resolver) drawDetachedShell(s *wsState, work *conversationv1.DetachedWorkId, bash *conversationv1.AgentBash) {
	log := r.logger(s.id)
	workID := work.GetValue()
	sh := s.shell(workID)
	if sh.feed.feed == (feedid.Feed{}) {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "sh.feed.feed == (feedid.Feed{})"})
		sh.feed = placement{feed: feedid.Feed{Root: true}}
	}

	var settled *frontendv1.FeedShellSettled
	switch frame := bash.GetResult().(type) {
	case *conversationv1.AgentBash_Start:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawDetachedShell", "branch": "case *conversationv1.AgentBash_Start"})
		sh.stateCommand(frame.Start.GetCommand().GetLine())
		if sh.startedAtMs == 0 {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "sh.startedAtMs == 0"})
			sh.startedAtMs = frame.Start.GetStartedAt().GetAtMs()
		}
	case *conversationv1.AgentBash_Update:
		// The offset is a GAP DETECTOR: bytes must arrive contiguously, and a
		// frame that does not continue the spool is REFUSED rather than
		// concatenated across a hole.
		//
		// A RE-DELIVERY IS NOT A HOLE. Two producers write this run's frames
		// under one upsert key — the shim from the live stream, the sidecar
		// from the spool file — so the consumer legitimately sees bytes it has
		// already accumulated a second time. Those are dropped as a replay
		// once they are shown to AGREE with what is held; a frame that starts
		// past the spool's end, or that restates already-held bytes
		// DIFFERENTLY, is real loss and stays an error.
		from, out := frame.Update.GetFromOffset(), frame.Update.GetNewOutput()
		if from > sh.nextOffset {
			log.Error("daemon.feed.spool_gap",
				"a detached shell's output frame did not continue the spool; the frame was refused",
				dlog.Context{"work": workID, "expected_offset": sh.nextOffset, "got_offset": from})
			return
		}
		if from < sh.nextOffset {
			overlap := sh.nextOffset - from
			if overlap > uint64(len(out)) {
				r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "overlap > uint64(len(out))"})
				overlap = uint64(len(out))
			}
			if sh.spool[from:from+overlap] != out[:overlap] {
				log.Error("daemon.feed.spool_gap",
					"a detached shell's output frame restated held bytes differently; the frame was refused",
					dlog.Context{"work": workID, "expected_offset": sh.nextOffset, "got_offset": from})
				return
			}
			log.Debug("daemon.feed.spool_replay",
				"a detached shell's output frame re-delivered bytes the spool already holds",
				dlog.Context{"work": workID, "expected_offset": sh.nextOffset, "got_offset": from, "replayed": overlap})
			out = out[overlap:]
		}
		sh.spool += out
		sh.nextOffset += uint64(len(out))
		// SPOOL GROWTH IS THE BEAT, and the daemon stamps it on the append it
		// just observed. FeedShellLive says so in the contract: the drawn
		// instant is "the last output the daemon observed", so this ONE
		// observer at this ONE point is the whole of it.
		sh.lastProgressMs = r.deps.Now().UnixMilli()
	case *conversationv1.AgentBash_Progress:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawDetachedShell", "branch": "case *conversationv1.AgentBash_Progress"})
		// A BEAT MOVES NOTHING HERE, deliberately.
		//
		// This used to overwrite lastProgressMs with
		// AgentToolCallProgress.last_progress_at_ms, which is not a value that
		// belongs in this field. That instant is the PRODUCER's — when the shim
		// or the sidecar observed the vendor report the call alive — while the
		// appends above are stamped by the DAEMON on receipt. Two observers
		// stamping at two points in the pipeline are not one timeline, so
		// last-write-wins between them made the drawn instant jump, and jump
		// BACKWARDS whenever a beat carrying an older producer instant arrived
		// after an append the daemon had already stamped. The client ticks
		// "quiet for N" off that instant, so the row's age ran backwards.
		//
		// The contract settles which of the two the field means: FeedShellLive
		// is "spool growth IS the beat — the daemon stamps it on each append it
		// observes", and AgentBash.progress is explicitly "NOT an update — no
		// growth is reported". A frame that reports no growth therefore has
		// nothing to say about the last growth, and the beat is still what
		// re-pushes the row.
		//
		// This is NOT the tool-call rule: FeedToolCallLastProgress is "when the
		// vendor last reported this call alive", so toolcall.go rightly carries
		// the producer's instant through. Two fields, two meanings, one
		// observer each.
	case *conversationv1.AgentBash_Success:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawDetachedShell", "branch": "case *conversationv1.AgentBash_Success"})
		sh.stateCommand(frame.Success.GetCommand().GetLine())
		settled = shellEnding(log, workID, bash)
	case *conversationv1.AgentBash_Failure:
		settled = shellEnding(log, workID, bash)
	}

	r.publishShell(s, workID, sh, settled)
}

// settleShellsLeftLive settles, as LOST, every drawn detached shell that the
// live set held at its previous publication and no longer holds, unless the
// shell has already settled.
//
// A RUN THAT LEFT THE LIVE SET HAS ENDED FOR EVERY READER. The live set is the
// open watch set: a shell leaves it at its own terminal (which settled the
// head before the set was republished, so nothing is left to do here), or
// because nothing will report it again -- its stream could not be re-opened,
// the link was severed, the session's query died. In those cases no terminal
// is coming, and a head left running drew an orange dot and a stop button
// forever over work nobody could see any more. "We stopped being able to see
// it" is exactly FeedShellLost; no staleness ruling stated WHY, so the arm
// carries no cause.
//
// ONLY A SHELL THE SET HELD. A shell the feed drew but the watcher never held
// (its first watch open was refused while the store had no rows for it yet)
// has not left anything: a repeated announcement is its re-open.
func (r *resolver) settleShellsLeftLive(s *wsState, live sessionwatcher.LiveWorkSet) {
	now := make(map[string]struct{}, len(live.Shells))
	for _, work := range live.Shells {
		now[work.GetValue()] = struct{}{}
	}
	for workID := range s.liveShells {
		if _, still := now[workID]; still {
			continue
		}
		delete(s.liveShells, workID)
		sh, drawn := s.shells[workID]
		if !drawn || sh.row == nil {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "settleShellsLeftLive", "condition": "!drawn || sh.row == nil"})
			continue
		}
		if sh.settled != nil {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "settleShellsLeftLive", "condition": "sh.settled != nil"})
			continue
		}
		r.publishShell(s, workID, sh, &frontendv1.FeedShellSettled{
			EndedAtMs: r.deps.Now().UnixMilli(),
			Outcome:   &frontendv1.FeedShellSettled_Lost{Lost: &frontendv1.FeedShellLost{}},
		})
		r.logger(s.id).Info("daemon.feed.detached_shell_left_live",
			"a detached shell left the live set with no terminal; its head is settled lost",
			dlog.Context{"work": workID})
	}
	for workID := range now {
		s.liveShells[workID] = struct{}{}
	}
}

// shellStart answers the instant a run's clock counts from. The authoritative
// start wins whenever one has landed; until then the run's first-observed
// instant stands in, stamped ONCE here off the daemon clock, so the clock never
// counts from the epoch while a producer's re-announced `start` is still in
// flight (see shellState.firstObservedMs).
func (r *resolver) shellStart(sh *shellState) int64 {
	if sh.startedAtMs != 0 {
		return sh.startedAtMs
	}
	if sh.firstObservedMs == 0 {
		sh.firstObservedMs = r.deps.Now().UnixMilli()
	}
	return sh.firstObservedMs
}

// subagentStart answers the instant a subagent bubble's clock counts from,
// never the epoch. It mirrors shellStart's intent, with one difference the
// subagent needs: a subagent bubble is often delivered SETTLED-ONLY (a replayed
// history carries no start), and a settled clock draws end − start, so a
// first-observed instant stamped at replay time would read end − now and, since
// end is in the past, clamp to "0s" rather than the run's real span.
//
// The order is therefore: an authoritative start (from a start frame) wins;
// else a settled run reconstructs its start from the totals' duration
// (end − duration), so the settled clock shows the true elapsed; else a LIVE
// run with no start yet counts from the first-observed instant, stamped ONCE
// off the daemon clock (see subagentState.firstObservedMs). formatElapsed
// clamps a negative span to "0s", so a settled run with neither a start nor a
// duration draws "0s" rather than an absurd age.
func (r *resolver) subagentStart(state *subagentState) int64 {
	bubble := state.bubble
	if ms := bubble.GetRuntime().GetStartedAtMs(); ms != 0 {
		return ms
	}
	if settled, ok := bubble.GetState().(*frontendv1.FeedSubagent_Settled); ok {
		end := settled.Settled.GetEndedAtMs()
		if end != 0 && state.durationMs != 0 && end >= int64(state.durationMs) {
			return end - int64(state.durationMs)
		}
	}
	if state.firstObservedMs == 0 {
		state.firstObservedMs = r.deps.Now().UnixMilli()
	}
	return state.firstObservedMs
}

// spoolCap is how much of a spool's tail the daemon carries. The body is a
// SNAPSHOT replaced whole on every push, so a cap here is what keeps watching
// a long command from costing more than running it.
const spoolCap = 16 * 1024

// shellSubFeed is the sub-feed a detached shell's spool BODY rides — the feed
// the head's FeedId resolves to, keyed by the run's own work id.
func shellSubFeed(workID string) feedid.Feed {
	id := feedid.ShellID(workID)
	return feedid.Feed{Shell: &id}
}

// publishShell renders and upserts a detached shell's CANONICAL BUBBLE: a
// spool-less HEAD on the parent feed (command + clock + stop), and — once there
// is output — a spool-only BODY row on the shell's own sub-feed. This mirrors
// the subagent bubble: the head is carried on the parent's one connection, and
// the head's FeedId IS the sub-feed's address, so an expand's OpenFeed resolves
// and the spool streams LAZILY — a collapsed bubble tails nothing, because only
// a reader that opened the sub-feed subscribes to it.
func (r *resolver) publishShell(s *wsState, workID string, sh *shellState, settled *frontendv1.FeedShellSettled) {
	// A SETTLED RUN STAYS SETTLED. The ending is remembered on the run rather
	// than read off the frame in hand, because every push after the terminal —
	// a replayed announcement, the other plane's spool replay, a beat — carries
	// no ending at all and would otherwise draw the finished run live again.
	if settled != nil {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "settled != nil"})
		sh.settled = settled
	}

	head := &frontendv1.FeedShell{
		Command: &frontendv1.FeedShellCommand{Text: sh.command},
		Runtime: &frontendv1.FeedShellRuntime{StartedAtMs: r.shellStart(sh)},
		// A shell bubble exists only for detached work, so every head names it.
		WorkId: &frontendv1.FeedDetachedWorkId{Text: workID},
	}
	if sh.settled != nil {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "sh.settled != nil"})
		head.State = &frontendv1.FeedShell_Settled{Settled: sh.settled}
	} else {
		live := &frontendv1.FeedShellLive{}
		if sh.lastProgressMs > 0 {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "sh.lastProgressMs > 0"})
			live.LastProgress = &frontendv1.FeedShellLastProgress{AtMs: sh.lastProgressMs}
		}
		head.State = &frontendv1.FeedShell_Live{Live: live}
	}

	// THE HEAD, on the parent feed. NO SPOOL: the command, clock and stop live
	// here; the spool is the body, on the sub-feed.
	headID := r.rowID(s.id, sh.feed.feed, feedid.RowKey{Kind: feedid.KindShellHead, ID: workID})
	r.announceEntry(s, workID, sh.row, headID)
	sh.row = headID
	headRow := &frontendv1.FeedRow{
		Id:  headID,
		Row: &frontendv1.FeedRow_ShellHead{ShellHead: head},
	}
	r.stampTurn(s, headRow, nil)
	r.upsert(s, sh.feed, headRow, true)

	// The head's own FeedId IS the sub-feed's address; recording it is what
	// makes an expand's OpenFeed resolve and the body's crumbs draw. Minted
	// after the head is upserted so the parent feed is known.
	sub := shellSubFeed(workID)
	r.mintSubFeed(s, headID, sh.feed.feed, sub, sh.command)

	// THE BODY: the spool tail, on the shell's own sub-feed, present from the
	// first output. Snapshot semantics — capped and replaced whole on every
	// push. Nothing is pushed to a tail that has not opened this sub-feed.
	if sh.spool == "" {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "sh.spool == \"\""})
		r.logger(s.id).Debug("daemon.feed.detached_shell_row",
			"a detached shell's head was upserted with no spool body yet",
			dlog.Context{"work": workID, "settled": sh.settled != nil})
		return
	}
	tail, omittedLines := capSpool(sh.spool)
	spool := &frontendv1.FeedShellSpool{Text: tail}
	if omittedLines > 0 {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "omittedLines > 0"})
		spool.Omitted = &frontendv1.FeedShellOmitted{Text: formatEarlierLines(omittedLines)}
	}
	// THE BODY IS SPOOL-ONLY. The command, clock and state live on the head;
	// carrying them here too would duplicate them, so the body FeedShell sets
	// only its spool (feed.proto: FeedDetachedShell is the spool BODY row).
	body := &frontendv1.FeedShell{Spool: spool}
	bodyID := r.rowID(s.id, sub, feedid.RowKey{Kind: feedid.KindDetachedShell, ID: workID})
	bodyRow := &frontendv1.FeedRow{
		Id:  bodyID,
		Row: &frontendv1.FeedRow_DetachedShell{DetachedShell: &frontendv1.FeedDetachedShell{Shell: body}},
	}
	r.stampTurn(s, bodyRow, nil)
	r.upsert(s, placement{feed: sub}, bodyRow, true)
	r.logger(s.id).Debug("daemon.feed.detached_shell_row",
		"a detached shell's head and spool body were upserted",
		dlog.Context{"work": workID, "spool_bytes": len(sh.spool), "settled": sh.settled != nil})
}

// capSpool keeps the spool's TAIL and reports how many earlier lines it drops.
func capSpool(spool string) (string, uint64) {
	if len(spool) <= spoolCap {
		return spool, 0
	}
	dropped := spool[:len(spool)-spoolCap]
	tail := spool[len(spool)-spoolCap:]
	// Cut on a line boundary so the tail never begins mid-line.
	for i := 0; i < len(tail); i++ {
		if tail[i] == '\n' {
			dropped = spool[:len(spool)-spoolCap+i+1]
			tail = tail[i+1:]
			break
		}
	}
	return tail, countLines(dropped)
}

// shellEnding renders how a shell ended from its terminal frame, and nil for
// a frame that is not one (or for no frame at all). ONE rendering for every
// source of a shell's ending -- its own run stream, or the call's own result
// when the call's work had moved -- so the two can never draw it differently.
func shellEnding(log dlog.Logger, workID string, bash *conversationv1.AgentBash) *frontendv1.FeedShellSettled {
	switch frame := bash.GetResult().(type) {
	case *conversationv1.AgentBash_Success:
		return shellSettled(log, workID, frame.Success)
	case *conversationv1.AgentBash_Failure:
		return &frontendv1.FeedShellSettled{
			EndedAtMs: failureSettledMs(frame.Failure.GetError()),
			Outcome:   &frontendv1.FeedShellSettled_Cancelled{Cancelled: &frontendv1.FeedShellCancelled{}},
		}
	}
	return nil
}

// shellSettled renders a settled shell. A non-zero exit still COMPLETED —
// "failure" is the reader's judgment of the code, never an arm.
func shellSettled(log dlog.Logger, workID string, success *conversationv1.AgentBashSuccess) *frontendv1.FeedShellSettled {
	settled := &frontendv1.FeedShellSettled{EndedAtMs: success.GetSettledAt().GetAtMs()}
	switch outcome := success.GetOutcome().(type) {
	case *conversationv1.AgentBashSuccess_Completed:
		if exited, ok := outcome.Completed.GetTermination().GetHow().(*conversationv1.AgentBashTermination_Exited); ok {
			settled.Exit = &frontendv1.FeedShellExit{Code: exited.Exited.GetCode()}
		}
		settled.Outcome = &frontendv1.FeedShellSettled_Completed{Completed: &frontendv1.FeedShellCompleted{}}
	case *conversationv1.AgentBashSuccess_Interrupted:
		if cause := lostCauseOfBash(outcome.Interrupted); cause != lostNone {
			lost := &frontendv1.FeedShellLost{}
			if !applyShellLostHow(lost, cause) {
				log.Warn("daemon.feed.shell_lost_unlanded_arm",
					"a shell was lost in a way this build does not draw; the bubble carries no cause",
					dlog.Context{"work": workID, "cause": cause.String()})
			}
			settled.Outcome = &frontendv1.FeedShellSettled_Lost{Lost: lost}
			break
		}
		settled.Outcome = &frontendv1.FeedShellSettled_Cancelled{Cancelled: &frontendv1.FeedShellCancelled{}}
	default:
		settled.Outcome = &frontendv1.FeedShellSettled_Completed{Completed: &frontendv1.FeedShellCompleted{}}
	}
	return settled
}
