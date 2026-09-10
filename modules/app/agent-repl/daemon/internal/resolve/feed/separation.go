package feed

import (
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/figures"
)

// ⑤ THE SEPARATION DIVIDER: the session changed shape or place here, and a
// reader scrolling back must see where. ONE row kind for every arm — context
// cuts and worktree moves alike — because one renderer subroutine draws them
// all and an arm selects only its accent and its label.
//
// A separation belongs to NO TURN and is deliberately left unstamped.

// drawContextCut draws the divider a context cut leaves.
//
// THE CUT'S IDENTITY IS THE STORE ENTRY IT ARRIVED ON, never a count of
// arrivals, and that is the whole of why `pointer` is here.
//
// A cut reaches this daemon MORE THAN ONCE by design. The shim's stream plane
// converts the vendor's `compact_boundary` live and the sidecar's file plane
// reads the same boundary out of the transcript, both writing the SAME store
// entry — and every write of an entry is delivered on the agent's tail, so the
// daemon sees the cut twice. A per-workspace counter minted a fresh row key on
// each of those, so ONE compaction drew TWO separation rows:
//
//	17:59:09.014 daemon.feed.separation kind=compacted row=…separation.context_cut:1
//	17:59:09.020 daemon.feed.separation kind=compacted row=…separation.context_cut:2
//
// with the second drawn from whichever plane's frame was less complete.
// `TestCompactionDirectedWithSummaryOverride` read the summary off the wrong
// one and found it empty, and the webapp layer's `!rotate` scenario took the
// duplicate for its own turn's row and read `compacted` where it wanted
// `cleared`.
//
// The store's `position` is never in its upsert's UPDATE clause — an upsert
// "supersedes a row's content whole and leaves its place in the book exactly
// where the first insert put it" — so the pointer is the same for every
// delivery of one entry and different for every distinct cut. Keyed on it, the
// second delivery UPSERTS the first's row, which is what every other family
// here already does.
//
// A DELIVERY WITH NO POINTER still draws, on the counter, and says so: a
// producer that states no position is a fault to see rather than a row to
// drop, and the duplicate it may leave is strictly better than a missing
// divider.
func (r *resolver) drawContextCut(s *wsState, agent *conversationv1.AgentId, cut *conversationv1.ContextCut, pointer *conversationv1.HistoryPointer) {
	log := r.logger(s.id)
	at := r.place(s, agent)
	key := pointer.GetValue()
	if key == "" {
		s.synthSeq++
		key = fmt.Sprintf("unpositioned:%d", s.synthSeq)
		log.Warn("daemon.feed.context_cut_unpositioned",
			"a context cut arrived with no store pointer, so its divider is keyed on an arrival counter and a second delivery of the same cut would draw a second row",
			dlog.Context{"agent": agent.GetValue()})
	}
	id := r.rowID(s.id, at.feed, feedid.RowKey{
		Kind: feedid.KindSeparation,
		ID:   "context_cut:" + key,
	})

	separation := &frontendv1.FeedSessionSeparation{}
	switch arm := cut.GetCut().(type) {
	case *conversationv1.ContextCut_Cleared:
		// The vendor's conversation-reset record carries NO token delta, so
		// none is drawn: an invented figure would be worse than none.
		separation.Label = &frontendv1.FeedSessionSeparationLabel{Text: "context cleared"}
		separation.Kind = &frontendv1.FeedSessionSeparation_Cleared{Cleared: &frontendv1.FeedContextCutCleared{}}
	case *conversationv1.ContextCut_Compacted:
		compacted := arm.Compacted
		separation.Label = &frontendv1.FeedSessionSeparationLabel{Text: compactionLabel(compacted)}
		separation.Kind = &frontendv1.FeedSessionSeparation_Compacted{Compacted: &frontendv1.FeedContextCutCompacted{
			Summary: &frontendv1.FeedContextCutSummary{Markdown: compacted.GetSummary().GetMarkdown()},
			// Folded by default: the cut is not a hole in the conversation,
			// but it is not the conversation either.
			Fold: &frontendv1.FeedContextCutFold{Folded: true},
		}}
		// BOTH SIDES ARE FORMATTED HERE. The client renders them verbatim and
		// does no arithmetic and no unit rounding of its own.
		if tokens := compacted.GetTokens(); tokens != nil {
			separation.Tokens = &frontendv1.FeedContextCutTokens{
				BeforeText: figures.Tokens(uint64(tokens.GetTokensBefore())),
				AfterText:  figures.Tokens(uint64(tokens.GetTokensAfter())),
			}
		}
	case *conversationv1.ContextCut_CompactionFailed:
		// NOTHING WAS CUT, and the divider says so IN THE SLOT the compacted
		// divider would have taken (landing 8): a compaction was offered and
		// did not happen, which is neither a compaction nor silence. `tokens`
		// stays UNSET — no size changed. It is still a WARNING, and it still
		// rides the turn's evidence, because the terminal is where a reader
		// looks when they ask what went wrong.
		reason := arm.CompactionFailed.GetError()
		r.addEvidence(s, turnEvidenceLine{text: "a compaction failed and nothing was cut: " + reason})
		log.Warn("daemon.feed.compaction_failed",
			"a compaction failed, so the divider says nothing was cut; it also rides the turn's evidence",
			dlog.Context{"agent": agent.GetValue(), "error": reason})
		separation.Label = &frontendv1.FeedSessionSeparationLabel{Text: "compaction failed"}
		separation.Kind = &frontendv1.FeedSessionSeparation_CompactionFailed{
			CompactionFailed: &frontendv1.FeedContextCutCompactionFailed{Error: reason},
		}
	default:
		log.Warn("daemon.feed.context_cut_unset",
			"a context cut arrived with no arm set; no divider was drawn",
			dlog.Context{"agent": agent.GetValue()})
		return
	}

	log.Debug("daemon.feed.separation",
		"a session separation divider was drawn",
		dlog.Context{"row": id.GetValue(), "kind": separationArm(separation)})
	r.upsert(s, at, &frontendv1.FeedRow{
		Id:  id,
		Row: &frontendv1.FeedRow_Separation{Separation: separation},
	}, true)
}

// compactionLabel words a compaction's divider. A MANUAL compaction is
// something the user did; an AUTOMATIC one is something that HAPPENED to them
// — their conversation was silently rewritten while they watched — and drawing
// the two identically is the most misleading thing this divider can do.
func compactionLabel(compacted *conversationv1.ContextCompacted) string {
	label := "context compacted"
	switch compacted.GetTrigger().(type) {
	case *conversationv1.ContextCompacted_Automatic:
		label = "context compacted automatically"
	case *conversationv1.ContextCompacted_Requested:
		label = "context compacted on request"
	}
	if ms := compacted.GetDurationMs(); ms > 0 {
		label = label + " · took " + formatDuration(int64(ms))
	}
	return label
}

// separationArm names a divider's arm for a log record.
func separationArm(separation *frontendv1.FeedSessionSeparation) string {
	switch separation.GetKind().(type) {
	case *frontendv1.FeedSessionSeparation_Cleared:
		return "cleared"
	case *frontendv1.FeedSessionSeparation_Compacted:
		return "compacted"
	case *frontendv1.FeedSessionSeparation_CompactionFailed:
		return "compaction_failed"
	case *frontendv1.FeedSessionSeparation_WorktreeEntered:
		return "worktree_entered"
	case *frontendv1.FeedSessionSeparation_WorktreeLeft:
		return "worktree_left"
	}
	return "unset"
}

// drawWorktree draws the divider a worktree move leaves. Each SETTLED act is
// its own divider — never a tool card, and never coalesced with its pair: the
// two moments can be far apart and everything between them happened inside the
// tree. A worktree call that FAILED is a tool failure, not a divider.
func (r *resolver) drawWorktree(s *wsState, at placement, act *conversationv1.AgentActivity, worktree *conversationv1.AgentWorktree) (*frontendv1.FeedRow, error) {
	unitID := act.GetActivityId().GetValue()
	success, ok := worktree.GetState().(*conversationv1.AgentWorktree_Success)
	if !ok {
		return nil, errNotARow
	}

	separation := &frontendv1.FeedSessionSeparation{}
	switch outcome := success.Success.GetAct().(type) {
	case *conversationv1.AgentWorktreeSuccess_Entered:
		entered := outcome.Entered
		separation.Label = &frontendv1.FeedSessionSeparationLabel{
			Text: "entered worktree " + entered.GetPath(),
		}
		payload := &frontendv1.FeedWorktreeEntered{
			Path: &frontendv1.FeedWorktreePath{Text: entered.GetPath()},
		}
		if entered.Branch != nil && entered.GetBranch() != "" {
			payload.Branch = &frontendv1.FeedWorktreeBranch{Text: entered.GetBranch()}
		}
		separation.Kind = &frontendv1.FeedSessionSeparation_WorktreeEntered{WorktreeEntered: payload}
	case *conversationv1.AgentWorktreeSuccess_Exited:
		exited := outcome.Exited
		separation.Label = &frontendv1.FeedSessionSeparationLabel{Text: "left the worktree"}
		separation.Kind = &frontendv1.FeedSessionSeparation_WorktreeLeft{WorktreeLeft: worktreeLeft(exited)}
	default:
		return nil, errNotARow
	}

	// UNSET on the worktree arms: they change no context.
	r.logger(s.id).Debug("daemon.feed.worktree_separation",
		"a worktree act was drawn as a separation divider",
		dlog.Context{"unit": unitID, "kind": separationArm(separation)})
	return &frontendv1.FeedRow{
		Id:  r.rowID(s.id, at.feed, feedid.RowKey{Kind: feedid.KindSeparation, ID: unitID}),
		Row: &frontendv1.FeedRow_Separation{Separation: separation},
	}, nil
}

// worktreeLeft renders what became of the tree. A KEPT tree is somewhere to
// go; a REMOVED one may have taken work with it, so the discard line is drawn
// loud when anything was discarded.
func worktreeLeft(exited *conversationv1.AgentWorktreeExited) *frontendv1.FeedWorktreeLeft {
	switch outcome := exited.GetOutcome().(type) {
	case *conversationv1.AgentWorktreeExited_Kept:
		return &frontendv1.FeedWorktreeLeft{Outcome: &frontendv1.FeedWorktreeLeft_Kept{
			Kept: &frontendv1.FeedWorktreeKept{Path: &frontendv1.FeedWorktreePath{Text: exited.GetPath()}},
		}}
	case *conversationv1.AgentWorktreeExited_Removed:
		removed := &frontendv1.FeedWorktreeRemoved{}
		if line := discardLine(outcome.Removed); line != "" {
			removed.Discarded = &frontendv1.FeedWorktreeDiscarded{Text: line}
		}
		return &frontendv1.FeedWorktreeLeft{Outcome: &frontendv1.FeedWorktreeLeft_Removed{Removed: removed}}
	}
	return &frontendv1.FeedWorktreeLeft{Outcome: &frontendv1.FeedWorktreeLeft_Kept{
		Kept: &frontendv1.FeedWorktreeKept{Path: &frontendv1.FeedWorktreePath{Text: exited.GetPath()}},
	}}
}

// discardLine composes what the removal took with it. UNSET figures are NOT
// zero — the vendor stating no figure is different from it stating none were
// discarded — so an unstated figure contributes nothing to the line.
func discardLine(removed *conversationv1.AgentWorktreeRemoved) string {
	var parts []string
	if removed.DiscardedFiles != nil && removed.GetDiscardedFiles() > 0 {
		parts = append(parts, fmt.Sprintf("%d files", removed.GetDiscardedFiles()))
	}
	if removed.DiscardedCommits != nil && removed.GetDiscardedCommits() > 0 {
		parts = append(parts, fmt.Sprintf("%d commits", removed.GetDiscardedCommits()))
	}
	if len(parts) == 0 {
		return ""
	}
	line := parts[0]
	for _, part := range parts[1:] {
		line = line + ", " + part
	}
	return line + " discarded"
}
