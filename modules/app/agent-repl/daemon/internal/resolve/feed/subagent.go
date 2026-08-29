package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
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
	if detached {
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
		bubble.State = &frontendv1.FeedSubagent_Live{Live: &frontendv1.FeedSubagentLive{}}
	case *conversationv1.AgentSubagent_Update:
		applyPrompt(bubble, frame.Update.GetPrompt())
		progress := frame.Update.GetProgress()
		if progress.GetTotalTokens() > 0 {
			bubble.Tokens = &frontendv1.FeedSubagentTokens{Text: formatTokens(progress.GetTotalTokens()) + " tok"}
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
		subagentFailureOutcome(frame.Failure)(settled)
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
		bubble.Tokens = &frontendv1.FeedSubagentTokens{Text: formatTokens(sum) + " tok"}
	case *conversationv1.AgentSubagentTotals_TotalOnly:
		if usage.TotalOnly.TotalTokens == nil {
			return
		}
		bubble.Tokens = &frontendv1.FeedSubagentTokens{
			Text: formatTokens(usage.TotalOnly.GetTotalTokens()) + " tok",
		}
	}
}

// subagentFailureOutcome picks the settled treatment. A person's stop is NOT a
// fault, and work we merely stopped being able to see is LOST rather than
// failed — the word carries the distinction so it never draws as a plain
// failure.
func subagentFailureOutcome(failure *conversationv1.AgentSubagentFailure) subagentOutcome {
	if lostCauseOfSubagent(failure) != lostNone {
		return func(settled *frontendv1.FeedSubagentSettled) {
			settled.Outcome = &frontendv1.FeedSubagentSettled_Lost{Lost: &frontendv1.FeedSubagentLost{}}
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
		if u, ok := s.units[unitID]; ok && u.input != "" {
			sh := s.shell(workID)
			sh.command = trimCommandPrefix(u.input)
			sh.startedAtMs = u.startedAtMs
			sh.feed = at
			r.publishShell(s, workID, sh, nil)
			log.Debug("daemon.feed.detached_shell",
				"a foreground shell became a detached shell bubble",
				dlog.Context{"unit": unitID, "work": workID})
			return
		}
		log.Warn("daemon.feed.detached_unknown_unit",
			"work detached from a unit this resolver never drew",
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

// trimCommandPrefix removes the composed shell chrome from an input line so
// the shell bubble draws the command itself ("$" is the client's chrome).
func trimCommandPrefix(input string) string {
	if len(input) > 2 && input[0] == '$' && input[1] == ' ' {
		return input[2:]
	}
	return input
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
		sh.command = frame.Start.GetCommand().GetLine()
		if sh.startedAtMs == 0 {
			sh.startedAtMs = frame.Start.GetStartedAt().GetAtMs()
		}
	case *conversationv1.AgentBash_Update:
		// The offset is a GAP DETECTOR: it must equal what the consumer has
		// already accumulated. Anything else means bytes were lost, and the
		// frame is REFUSED rather than concatenated across a hole.
		if frame.Update.GetFromOffset() != sh.nextOffset {
			log.Error("daemon.feed.spool_gap",
				"a detached shell's output frame did not continue the spool; the frame was refused",
				dlog.Context{"work": workID, "expected_offset": sh.nextOffset, "got_offset": frame.Update.GetFromOffset()})
			return
		}
		sh.spool += frame.Update.GetNewOutput()
		sh.nextOffset += uint64(len(frame.Update.GetNewOutput()))
		sh.lastProgressMs = r.deps.Now().UnixMilli()
	case *conversationv1.AgentBash_Progress:
		sh.lastProgressMs = frame.Progress.GetLastProgressAtMs()
	case *conversationv1.AgentBash_Success:
		sh.command = frame.Success.GetCommand().GetLine()
		settled = shellSettled(frame.Success)
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
func shellSettled(success *conversationv1.AgentBashSuccess) *frontendv1.FeedShellSettled {
	settled := &frontendv1.FeedShellSettled{EndedAtMs: success.GetSettledAt().GetAtMs()}
	switch outcome := success.GetOutcome().(type) {
	case *conversationv1.AgentBashSuccess_Completed:
		if exited, ok := outcome.Completed.GetTermination().GetHow().(*conversationv1.AgentBashTermination_Exited); ok {
			settled.Exit = &frontendv1.FeedShellExit{Code: exited.Exited.GetCode()}
		}
		settled.Outcome = &frontendv1.FeedShellSettled_Completed{Completed: &frontendv1.FeedShellCompleted{}}
	case *conversationv1.AgentBashSuccess_Interrupted:
		if lostCauseOfBash(outcome.Interrupted) != lostNone {
			settled.Outcome = &frontendv1.FeedShellSettled_Lost{Lost: &frontendv1.FeedShellLost{}}
			break
		}
		settled.Outcome = &frontendv1.FeedShellSettled_Cancelled{Cancelled: &frontendv1.FeedShellCancelled{}}
	default:
		settled.Outcome = &frontendv1.FeedShellSettled_Completed{Completed: &frontendv1.FeedShellCompleted{}}
	}
	return settled
}
