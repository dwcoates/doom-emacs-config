package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/figures"
)

// THE SUBAGENT BUBBLE — sync or detached, ONE component. The bubble IS a feed:
// its rows never ride this row. This is the COLLAPSED HEAD, carried on the
// parent feed so every bubble paints from the parent's one connection; the
// child's own connection exists only while expanded.

// drawSubagent draws a spawn's bubble head. detached selects the placement
// wrapper — sync-vs-detached is PLACEMENT, never a second drawing.
func (r *resolver) drawSubagent(s *wsState, at placement, act *conversationv1.AgentActivity, spawn *conversationv1.AgentSubagent, detached bool) (*frontendv1.FeedRow, error) {
	unitID := act.GetActivityId().GetValue()
	state, ok := s.subagents[unitID]
	if !ok {
		state = &subagentState{}
		s.subagents[unitID] = state
	}
	// THE MARK IS CLAIMED UNCONDITIONALLY, never behind the flag: a detachment
	// announced before this unit drew is exactly the case the flag cannot
	// carry, and leaving the mark standing would report the unit as one
	// nothing ever drew.
	_, announcedDetached := s.claimDetached(unitID)
	if detached || announcedDetached {
		state.detached = true
	}
	bubble := state.bubble
	if bubble == nil {
		bubble = &frontendv1.FeedSubagent{}
		state.bubble = bubble
	}

	switch frame := spawn.GetResult().(type) {
	case *conversationv1.AgentSubagent_Start:
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
			bubble.State = &frontendv1.FeedSubagent_Live{Live: &frontendv1.FeedSubagentLive{}}
		}
	case *conversationv1.AgentSubagent_Update:
		applyPrompt(bubble, frame.Update.GetPrompt())
		progress := frame.Update.GetProgress()
		if progress.GetTotalTokens() > 0 {
			bubble.Tokens = &frontendv1.FeedSubagentTokens{Text: figures.Tokens(progress.GetTotalTokens()) + " tok"}
		}
		bubble.State = &frontendv1.FeedSubagent_Live{Live: &frontendv1.FeedSubagentLive{
			LastProgress: &frontendv1.FeedSubagentLastProgress{AtMs: r.deps.Now().UnixMilli()},
		}}
	case *conversationv1.AgentSubagent_Success:
		applyPrompt(bubble, frame.Success.GetPrompt())
		applyTotals(bubble, frame.Success.GetTotals())
		bubble.State = &frontendv1.FeedSubagent_Settled{Settled: &frontendv1.FeedSubagentSettled{
			EndedAtMs: frame.Success.GetSettledAt().GetAtMs(),
			Outcome:   &frontendv1.FeedSubagentSettled_Succeeded{Succeeded: &frontendv1.FeedSubagentSucceeded{}},
		}}
	case *conversationv1.AgentSubagent_Failure:
		settled := &frontendv1.FeedSubagentSettled{EndedAtMs: failureSettledMs(frame.Failure.GetError())}
		subagentFailureOutcome(r.logger(s.id), unitID, frame.Failure)(settled)
		bubble.State = &frontendv1.FeedSubagent_Settled{Settled: settled}
	default:
		return nil, errNotARow
	}

	if bubble.Runtime == nil {
		bubble.Runtime = &frontendv1.FeedSubagentRuntime{StartedAtMs: 0}
	}
	if bubble.Label == nil {
		bubble.Label = &frontendv1.FeedSubagentLabel{Text: "Agent"}
	}

	id := r.rowID(s.id, at.feed, feedid.RowKey{
		Kind: feedid.KindActivity, ID: unitID, Sub: state.created.GetValue(),
	})
	state.row = id
	state.feed = at

	// The bubble's own FeedId IS the sub-feed's address; recording it is what
	// makes an expand's OpenFeed resolve and a page's crumbs draw.
	if state.created.GetValue() != "" {
		r.mintSubFeed(s, id, feedid.Feed{Agent: state.created}, bubbleLabel(bubble))
		r.drawCommission(s, at, unitID, state, commissionOf(spawn))
	}

	row := &frontendv1.FeedRow{Id: id}
	if state.detached {
		row.Row = &frontendv1.FeedRow_DetachedSubagent{DetachedSubagent: &frontendv1.FeedDetachedSubagent{
			Subagent: bubble,
		}}
	} else {
		row.Row = &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{
			Unit: &frontendv1.FeedTurnActivity_Subagent{Subagent: bubble},
		}}
	}
	return row, nil
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
		misses := usage.Full.GetInputMisses()
		sum := misses.GetWritten() + misses.GetUnwritten() + usage.Full.GetOutputTokens()
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
			r.republishSubagent(s, unitID, state)
			log.Debug("daemon.feed.detached_subagent",
				"a subagent bubble moved to its detached placement",
				dlog.Context{"unit": unitID, "work": workID})
			return
		}
		if r.detachForegroundShell(s, at, unitID, workID) {
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
			act := &conversationv1.AgentActivity{
				ActivityId: &conversationv1.AgentActivityId{Value: workID},
				Item:       &conversationv1.AgentActivity_Subagent{Subagent: created.Subagent},
			}
			row, err := r.drawSubagent(s, at, act, created.Subagent, true)
			if err != nil {
				return
			}
			r.stampTurn(s, row, nil)
			r.upsert(s, at, row, true)
		case *conversationv1.DetachableWork_Bash:
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
// detached bubble, answering whether there was one to turn.
func (r *resolver) detachForegroundShell(s *wsState, at placement, unitID, workID string) bool {
	u, ok := s.units[unitID]
	if !ok || u.input == "" {
		return false
	}
	sh := s.shell(workID)
	sh.command = u.input
	sh.startedAtMs = u.startedAtMs
	sh.feed = at
	r.publishShell(s, workID, sh, nil)
	r.logger(s.id).Debug("daemon.feed.detached_shell",
		"a foreground shell became a detached shell bubble",
		dlog.Context{"unit": unitID, "work": workID})
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
		return
	}
	if r.detachForegroundShell(s, at, unitID, work) {
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
		return
	}
	r.logger(s.id).Debug("daemon.feed.detachment_retired",
		"a held detachment named a unit whose kind draws no feed row; the mark is retired",
		dlog.Context{"unit": unitID, "work": work})
}

// republishSubagent re-pushes a bubble whose placement wrapper changed.
func (r *resolver) republishSubagent(s *wsState, unitID string, state *subagentState) {
	row := &frontendv1.FeedRow{Id: state.row}
	if state.detached {
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
		sh.feed = placement{feed: feedid.Feed{Root: true}}
	}

	var settled *frontendv1.FeedShellSettled
	switch frame := bash.GetResult().(type) {
	case *conversationv1.AgentBash_Start:
		sh.stateCommand(frame.Start.GetCommand().GetLine())
		if sh.startedAtMs == 0 {
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
		sh.lastProgressMs = r.deps.Now().UnixMilli()
	case *conversationv1.AgentBash_Progress:
		sh.lastProgressMs = frame.Progress.GetLastProgressAtMs()
	case *conversationv1.AgentBash_Success:
		sh.stateCommand(frame.Success.GetCommand().GetLine())
		settled = shellSettled(log, workID, frame.Success)
	case *conversationv1.AgentBash_Failure:
		settled = &frontendv1.FeedShellSettled{
			EndedAtMs: failureSettledMs(frame.Failure.GetError()),
			Outcome:   &frontendv1.FeedShellSettled_Cancelled{Cancelled: &frontendv1.FeedShellCancelled{}},
		}
	}

	r.publishShell(s, workID, sh, settled)
	log.Debug("daemon.feed.detached_shell_row",
		"a detached shell's bubble was upserted",
		dlog.Context{"work": workID, "spool_bytes": len(sh.spool), "settled": settled != nil})
}

// spoolCap is how much of a spool's tail the daemon carries. The body is a
// SNAPSHOT replaced whole on every push, so a cap here is what keeps watching
// a long command from costing more than running it.
const spoolCap = 16 * 1024

// publishShell renders and upserts a shell bubble.
func (r *resolver) publishShell(s *wsState, workID string, sh *shellState, settled *frontendv1.FeedShellSettled) {
	shell := &frontendv1.FeedShell{
		Command: &frontendv1.FeedShellCommand{Text: sh.command},
		Runtime: &frontendv1.FeedShellRuntime{StartedAtMs: sh.startedAtMs},
	}
	if sh.spool != "" {
		tail, omittedLines := capSpool(sh.spool)
		spool := &frontendv1.FeedShellSpool{Text: tail}
		if omittedLines > 0 {
			spool.Omitted = &frontendv1.FeedShellOmitted{Text: formatEarlierLines(omittedLines)}
		}
		shell.Spool = spool
	}
	if settled != nil {
		shell.State = &frontendv1.FeedShell_Settled{Settled: settled}
	} else {
		live := &frontendv1.FeedShellLive{}
		if sh.lastProgressMs > 0 {
			live.LastProgress = &frontendv1.FeedShellLastProgress{AtMs: sh.lastProgressMs}
		}
		shell.State = &frontendv1.FeedShell_Live{Live: live}
	}

	id := r.rowID(s.id, sh.feed.feed, feedid.RowKey{Kind: feedid.KindDetachedShell, ID: workID})
	sh.row = id
	row := &frontendv1.FeedRow{
		Id:  id,
		Row: &frontendv1.FeedRow_DetachedShell{DetachedShell: &frontendv1.FeedDetachedShell{Shell: shell}},
	}
	r.stampTurn(s, row, nil)
	r.upsert(s, sh.feed, row, true)
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
