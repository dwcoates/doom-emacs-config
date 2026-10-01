package feed

import (
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/figures"
	"claude-repld/internal/ids"
)

// clearCutRowID is the divider identity for a /clear the daemon issued. It is
// keyed on the TURN — a fact the daemon holds at receipt, before the shim has
// rotated to any new session — so the optimistic red bar and the shim's later
// ContextCut resolve to ONE row. The `clear:` segment keeps it disjoint from
// the pointer-keyed identity every other cut takes.
func clearCutRowID(turn ids.TurnID) string {
	return "context_cut:clear:" + string(turn)
}

// clearTurnFor answers the /clear turn a cleared cut belongs to, if any.
//
// THE CUT'S OWN IDENTITY WINS OVER WHATEVER IS RUNNING NOW. A /clear's cut is
// delivered on BOTH planes at one store pointer, and the FILE plane's copy is
// forwarded LATE — after the identity rotation, often after the user has
// already sent the next prompt. By then the turn in flight is that NEXT turn,
// not the clear; keying the late cut on the turn in flight would draw a second
// divider below the new prompt and suppress that prompt's own terminal. So a
// cut already attributed keeps its turn (the pointer map), and only a FRESH cut
// — one no plane has attributed yet — takes the turn in flight, and then only
// when that turn is itself a directive (a cleared cut belongs to a
// /clear turn, never to an ordinary one). The directive is recognised from the
// prompt text on every path, so this holds on a fresh-resolver replay too.
func (r *resolver) clearTurnFor(s *wsState, pointer *conversationv1.HistoryPointer) (ids.TurnID, bool) {
	if p := pointer.GetValue(); p != "" {
		if turn, ok := s.clearedTurnByPointer[p]; ok {
			return turn, true
		}
	}
	if turn := s.evidenceTurn(); turn != nil && s.directiveTurns[*turn] {
		return *turn, true
	}
	return "", false
}

// OnClearReceived draws the cleared divider the MOMENT the daemon accepts a
// /clear, before it is dispatched to the shim. The red bar and the cleared feed
// are what a user should see instantly; the shim's ContextCut later confirms it
// in place with the "context cleared" subtext. Its label is empty until then:
// the bar without the subtext is exactly the not-yet-confirmed state.
func (r *resolver) OnClearReceived(ws ids.WorkspaceID, turn ids.TurnID) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	log := r.logger(ws)
	s.clearTurns[turn] = true
	// A /clear draws no user-prompt bubble: it is a directive, and its only
	// visible outcome is this divider.
	s.directiveTurns[turn] = true

	at := r.outputPlacement(s, &turn)
	id := r.rowID(ws, at.feed, feedid.RowKey{Kind: feedid.KindSeparation, ID: clearCutRowID(turn)})
	row := &frontendv1.FeedRow{
		Id: id,
		Row: &frontendv1.FeedRow_Separation{Separation: &frontendv1.FeedSessionSeparation{
			// NO SUBTEXT YET: the bar stands for a clear the shim has not
			// confirmed. drawContextCut fills the label in when the cut lands.
			Label: &frontendv1.FeedSessionSeparationLabel{Text: ""},
			Kind:  &frontendv1.FeedSessionSeparation_Cleared{Cleared: &frontendv1.FeedContextCutCleared{}},
		}},
	}
	log.Info("daemon.feed.clear_received",
		"a /clear was accepted; its cleared divider was drawn optimistically before the shim round-trip",
		dlog.Context{"turn": string(turn), "row": id.GetValue()})
	r.upsert(s, at, row, true)

	// The optimistic bar bounds delivery at once, which is what CLEARS the feed
	// for the reader; the same fact drawContextCut records when a cut confirms.
	if boundsDelivery(row) {
		f := r.feed(s, at.feed)
		withheld := boundIndex(f, durableOrder(f))
		log.Info("daemon.feed.delivery_bound_moved",
			"an optimistic /clear divider moved the feed's delivery bound: nothing above it is served or pushed from here on",
			dlog.Context{"feed": f.key, "row": id.GetValue(), "kind": "cleared", "withheld": withheld})
	}
}

// OnCompactReceived registers a /compact as a directive turn so it, like a
// /clear, draws no user-prompt bubble. It draws NO optimistic divider: a
// compaction's divider carries the summary the vendor produces, which does not
// exist until the shim compacts, so the bar appears when the shim confirms the
// cut rather than on receipt.
func (r *resolver) OnCompactReceived(ws ids.WorkspaceID, turn ids.TurnID) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	s.directiveTurns[turn] = true
	r.logger(ws).Info("daemon.feed.compact_received",
		"a /compact was accepted; it draws no prompt bubble and its divider follows the shim's compaction",
		dlog.Context{"turn": string(turn)})
}

// OnContextCutAborted undoes a directive turn the shim refused before any turn
// ran: it retires the optimistic /clear divider (a no-op for a /compact, which
// drew none) and forgets the turn, so no phantom bar is left and the feed
// recovers to exactly what it showed before.
func (r *resolver) OnContextCutAborted(ws ids.WorkspaceID, turn ids.TurnID) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	r.retireOptimisticClear(s, turn, "the context cut was refused before it reached the shim")
	delete(s.clearTurns, turn)
	delete(s.clearConfirmed, turn)
	delete(s.directiveTurns, turn)
}

// retireOptimisticClear removes the turn-keyed red bar a /clear drew. Called
// when a clear FAILS — refused before dispatch, or ended without ever cutting
// context — so the divider does not outlive the clear it promised.
func (r *resolver) retireOptimisticClear(s *wsState, turn ids.TurnID, why string) {
	at := r.outputPlacement(s, &turn)
	id := r.rowID(s.id, at.feed, feedid.RowKey{Kind: feedid.KindSeparation, ID: clearCutRowID(turn)})
	removed := r.retire(s, at.feed, id.GetValue())
	r.logger(s.id).Info("daemon.feed.clear_bar_retired",
		"an optimistic /clear divider was retired because the clear did not happen",
		dlog.Context{"turn": string(turn), "row": id.GetValue(), "found": removed, "why": why})
}

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
	at, ok := r.place(s, agent)
	if !ok {
		return
	}
	key := pointer.GetValue()
	if key == "" {
		s.synthSeq++
		key = fmt.Sprintf("unpositioned:%d", s.synthSeq)
		log.Warn("daemon.feed.context_cut_unpositioned",
			"a context cut arrived with no store pointer, so its divider is keyed on an arrival counter and a second delivery of the same cut would draw a second row",
			dlog.Context{"agent": agent.GetValue()})
	}
	// The pointer-keyed identity is the default: the entry's position is the
	// cut's identity, so the two producing planes' deliveries of one cut land on
	// one row. A CLEAR the daemon itself issued overrides this below, keying its
	// cut on the turn so the shim's confirmation collapses onto the optimistic
	// red bar the receipt path already drew.
	cutRowID := "context_cut:" + key

	var confirmedClearTurn ids.TurnID
	var confirmsClear bool

	separation := &frontendv1.FeedSessionSeparation{}
	switch arm := cut.GetCut().(type) {
	case *conversationv1.ContextCut_Cleared:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawContextCut", "branch": "case *conversationv1.ContextCut_Cleared"})
		// The vendor's conversation-reset record carries NO token delta, so
		// none is drawn: an invented figure would be worse than none.
		separation.Label = &frontendv1.FeedSessionSeparationLabel{Text: "context cleared"}
		separation.Kind = &frontendv1.FeedSessionSeparation_Cleared{Cleared: &frontendv1.FeedContextCutCleared{}}
		// THE SHIM'S CUT CONFIRMS THE OPTIMISTIC RED BAR. When this cleared cut
		// belongs to a turn the daemon opened as a /clear, it reuses that turn's
		// divider row so the bar drawn on receipt gains its "context cleared"
		// subtext in place rather than a second red bar appearing beside it.
		if turn, ok := r.clearTurnFor(s, pointer); ok {
			cutRowID = clearCutRowID(turn)
			confirmedClearTurn = turn
			confirmsClear = true
		}
	case *conversationv1.ContextCut_Compacted:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawContextCut", "branch": "case *conversationv1.ContextCut_Compacted"})
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
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "tokens := compacted.GetTokens(); tokens != nil"})
			separation.Tokens = &frontendv1.FeedContextCutTokens{
				BeforeText: figures.Tokens(uint64(tokens.GetTokensBefore())),
				AfterText:  figures.Tokens(uint64(tokens.GetTokensAfter())),
			}
		}
	case *conversationv1.ContextCut_CompactionFailed:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawContextCut", "branch": "case *conversationv1.ContextCut_CompactionFailed"})
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

	id := r.rowID(s.id, at.feed, feedid.RowKey{
		Kind: feedid.KindSeparation,
		ID:   cutRowID,
	})

	log.Debug("daemon.feed.separation",
		"a session separation divider was drawn",
		dlog.Context{"row": id.GetValue(), "kind": separationArm(separation)})
	row := &frontendv1.FeedRow{
		Id:  id,
		Row: &frontendv1.FeedRow_Separation{Separation: separation},
	}
	r.upsert(s, at, row, true)

	if confirmsClear {
		// The clear SUCCEEDED: mark it confirmed so its turn's terminal is
		// suppressed, and remember the pointer→turn mapping so the OTHER plane's
		// later delivery of the same cut — which can land after the terminal, with
		// no turn in flight — still resolves to this one row.
		s.clearConfirmed[confirmedClearTurn] = true
		if p := pointer.GetValue(); p != "" {
			s.clearedTurnByPointer[p] = confirmedClearTurn
		}
		log.Info("daemon.feed.clear_confirmed",
			"a /clear's context cut confirmed the optimistic divider; its terminal will draw no bubble",
			dlog.Context{"turn": string(confirmedClearTurn), "row": id.GetValue()})
	}

	// THE FEED NOW BEGINS HERE (see `deliverable`). The bound is read off the
	// order at every page, so there is nothing to store; what is recorded is
	// the MOMENT it moved, and how much of the feed a reader is about to stop
	// being served — the count a client's own truncation should match.
	if boundsDelivery(row) {
		f := r.feed(s, at.feed)
		withheld := boundIndex(f, durableOrder(f))
		log.Info("daemon.feed.delivery_bound_moved",
			"a context cut moved the feed's delivery bound: nothing above this divider is served or pushed from here on",
			dlog.Context{
				"feed": f.key, "row": id.GetValue(),
				"kind": separationArm(separation), "withheld": withheld,
			})
	}
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
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "!ok"})
		return nil, errNotARow
	}

	separation := &frontendv1.FeedSessionSeparation{}
	switch outcome := success.Success.GetAct().(type) {
	case *conversationv1.AgentWorktreeSuccess_Entered:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawWorktree", "branch": "case *conversationv1.AgentWorktreeSuccess_Entered"})
		entered := outcome.Entered
		separation.Label = &frontendv1.FeedSessionSeparationLabel{
			Text: "entered worktree " + entered.GetPath(),
		}
		payload := &frontendv1.FeedWorktreeEntered{
			Path: &frontendv1.FeedWorktreePath{Text: entered.GetPath()},
		}
		if entered.Branch != nil && entered.GetBranch() != "" {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "entered.Branch != nil && entered.GetBranch() != \"\""})
			payload.Branch = &frontendv1.FeedWorktreeBranch{Text: entered.GetBranch()}
		}
		separation.Kind = &frontendv1.FeedSessionSeparation_WorktreeEntered{WorktreeEntered: payload}
	case *conversationv1.AgentWorktreeSuccess_Exited:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawWorktree", "branch": "case *conversationv1.AgentWorktreeSuccess_Exited"})
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
		parts = append(parts, countOf(removed.GetDiscardedFiles(), "file"))
	}
	if removed.DiscardedCommits != nil && removed.GetDiscardedCommits() > 0 {
		parts = append(parts, countOf(removed.GetDiscardedCommits(), "commit"))
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

// countOf renders "1 file" / "3 files". A discard line is a loud one and it is
// read by a person, so a single file does not lose a commit's worth of trust to
// "1 files".
func countOf(n uint32, noun string) string {
	if n == 1 {
		return "1 " + noun
	}
	return fmt.Sprintf("%d %ss", n, noun)
}
