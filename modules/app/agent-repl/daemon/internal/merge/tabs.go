package merge

import (
	"fmt"
	"strconv"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// This file synthesizes the merge bubble: the head row on the root feed, and
// the tab rows on the bubble's own sub-feed.
//
// Tabs are APPEND-ONLY and ROUND-NUMBERED. A tab never reopens — a second pass
// is a SECOND TAB carrying the round in its label ("tests (2)") — so the row
// key carries the round and a re-push of the same round upserts that tab in
// place rather than growing a third.

// mergeFeed addresses one merge bubble's sub-feed.
func mergeFeed(lease ids.LeaseID) feedid.Feed {
	id := lease
	return feedid.Feed{Merge: &id}
}

// headRef addresses the merge bubble's HEAD row, which lives on the ROOT feed:
// the bubble is an activity unit of the turn the user is watching, and its
// content hangs off the sub-feed the head decodes to.
func headRef(ws ids.WorkspaceID, lease ids.LeaseID) feedid.Ref {
	return feedid.Ref{
		WS:   ws,
		Feed: feedid.Feed{Root: true},
		Row:  feedid.RowKey{Kind: feedid.KindMergeHead, ID: string(lease)},
	}
}

// tabRef addresses one tab row of a merge bubble's sub-feed. The round is part
// of the key, which is what makes a second round a second tab.
func tabRef(ws ids.WorkspaceID, lease ids.LeaseID, kind string, round int) feedid.Ref {
	return feedid.Ref{
		WS:   ws,
		Feed: mergeFeed(lease),
		Row:  feedid.RowKey{Kind: feedid.KindMergeTab, ID: kind, Sub: strconv.Itoa(round)},
	}
}

// tabLabel resolves a tab's drawn label. The client decorates rounds beyond the
// first and draws nothing for round one, so the round travels as a number
// rather than baked into the text.
func tabLabel(kind string, round int) *frontendv1.FeedMergeTabLabel {
	return &frontendv1.FeedMergeTabLabel{Text: tabWord(kind), Round: uint32(round)}
}

// tabWord is the drawn word for a tab kind.
func tabWord(kind string) string {
	switch kind {
	case TabQueue:
		return "queue"
	case TabPrePrompt:
		return "pre-prompt"
	case TabRebasing:
		return "rebasing"
	case TabConflicts:
		return "conflicts"
	case TabTests:
		return "tests"
	case TabFixes:
		return "fixes"
	case TabCommitting:
		return "committing"
	case TabUpdatingMain:
		return "updating main"
	case TabPostPrompt:
		return "post-prompt"
	}
	return kind
}

// tabState is one tab round's badge: LIVE, or SETTLED with when it ended and
// the daemon's one-line account of a failure. Either way it carries when the
// round BEGAN, which the client ticks a live tab's elapsed time from and
// subtracts from a settled tab's end.
//
// A tabState is minted only by the round it describes (tabRound.live,
// tabRound.settled) or, for the queue tab, from the queue entry's own times
// (queueTabState), so no badge can be built without its start.
type tabState struct {
	startedMS int64
	settled   bool
	endedMS   int64
	failure   string
}

// badge resolves the state into the shared leaf messages every kind's state
// oneof selects from: exactly one of the two is non-nil.
func (s tabState) badge() (*frontendv1.FeedMergeTabLive, *frontendv1.FeedMergeTabSettled) {
	if !s.settled {
		return &frontendv1.FeedMergeTabLive{StartedAtMs: s.startedMS}, nil
	}
	settled := &frontendv1.FeedMergeTabSettled{EndedAtMs: s.endedMS, StartedAtMs: s.startedMS}
	if s.failure != "" {
		settled.Outcome = &frontendv1.FeedMergeTabSettled_Failed{Failed: &frontendv1.FeedMergeTabFailed{Summary: s.failure}}
	} else {
		settled.Outcome = &frontendv1.FeedMergeTabSettled_Succeeded{Succeeded: &frontendv1.FeedMergeTabSucceeded{}}
	}
	return nil, settled
}

// tabRow wraps one tab's kind arm into the feed row that carries it.
func tabRow(ws ids.WorkspaceID, lease ids.LeaseID, kind string, round int, tab *frontendv1.FeedMergeTab) *frontendv1.FeedRow {
	tab.Label = tabLabel(kind, round)
	return &frontendv1.FeedRow{
		Id:  feedid.Encode(tabRef(ws, lease, kind, round)),
		Row: &frontendv1.FeedRow_MergeTab{MergeTab: tab},
	}
}

// headRow builds the bubble's collapsed head: the branch line, the clock, and
// THE ARM THAT IS THE STATE of the merge as a whole.
func headRow(ws ids.WorkspaceID, lease ids.LeaseID, label string, startedMS int64, result any) *frontendv1.FeedRow {
	merge := &frontendv1.FeedMerge{
		Head: &frontendv1.FeedMergeHead{
			Glyph:   &frontendv1.FeedMergeGlyph{Icon: "merge"},
			Label:   &frontendv1.FeedMergeLabel{Text: label},
			Runtime: &frontendv1.FeedMergeRuntime{StartedAtMs: startedMS},
			// The bubble opens folded: the footer already carries the merge's
			// live state, so the open body would only repeat it.
			Fold: &frontendv1.FeedMergeFold{Folded: true},
		},
	}
	switch r := result.(type) {
	case *frontendv1.FeedMergeSuccess:
		merge.Result = &frontendv1.FeedMerge_Success{Success: r}
	case *frontendv1.FeedMergeError:
		merge.Result = &frontendv1.FeedMerge_Error{Error: r}
	default:
		merge.Result = &frontendv1.FeedMerge_Update{Update: &frontendv1.FeedMergeUpdate{}}
	}
	return &frontendv1.FeedRow{
		Id: feedid.Encode(headRef(ws, lease)),
		Row: &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{
			Unit: &frontendv1.FeedTurnActivity_Merge{Merge: merge},
		}},
	}
}

// branchLabel resolves the head's branch line ("ABC/fix-flaky → master").
func branchLabel(source, target string) string {
	return fmt.Sprintf("%s → %s", source, target)
}

// queueSnapshot renders one repository's queue as this workspace sees it:
// who is ahead, this workspace, who is behind. The structure is what makes
// "you are here" a fact rather than something a client derives by comparing
// ids.
//
// standings are queueStandings' answer, one per entry in queue order: the
// FRONT entry carries its own run's ACTIVE TAB and when it began, so a waiting
// user sees the front's progress, and every entry behind it when it was
// queued. The front workspace itself is shown nothing about the queue: its
// bubble is busy merging.
func queueSnapshot(entries []wsm.MergeQueueEntry, self ids.WorkspaceID, names map[ids.WorkspaceID]string, dirs map[ids.WorkspaceID]string, standings []*frontendv1.FeedMergeQueueEntry) *frontendv1.FeedMergeQueue {
	snap := &frontendv1.FeedMergeQueue{}
	seenSelf := false
	for i, entry := range entries {
		row := &frontendv1.FeedMergeQueueEntry{
			Workspace: &frontendv1.FeedMergeQueueWorkspace{Ref: &workspacev1.WorkspaceRef{
				Id:  string(entry.Workspace),
				Dir: dirs[entry.Workspace],
			}},
			Label:  &frontendv1.FeedMergeQueueLabel{Text: names[entry.Workspace]},
			Status: standings[i].Status,
		}
		switch {
		case entry.Workspace == self:
			snap.Current = row
			seenSelf = true
		case seenSelf:
			snap.Behind = append(snap.Behind, row)
		default:
			snap.Ahead = append(snap.Ahead, row)
		}
	}
	return snap
}

// dequeueOffer composes the tray's merge-dequeue question. The daemon composes
// the sentence; the card draws it and offers the two answers, which are the
// answer request's arms rather than fields here.
func dequeueOffer(name string) *frontendv1.HeldOffer {
	return &frontendv1.HeldOffer{Offer: &frontendv1.HeldOffer_MergeDequeue{
		MergeDequeue: &frontendv1.HeldOfferMergeDequeue{
			Headline: &frontendv1.HeldOfferHeadline{Text: fmt.Sprintf(
				"interrupting %s — keep its merge's queue slot, or release it?", name)},
		},
	}}
}

// saidText composes the one canonical prompt form the daemon submits a brief
// as. A brief travels as a person's words would, because the queue's one path
// carries nothing else.
func saidText(text string) *conversationv1.UserSaid {
	return &conversationv1.UserSaid{Content: &conversationv1.UserContent{
		Blocks: []*conversationv1.UserContentBlock{{
			Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}},
		}},
	}}
}

// rebasingTab builds a rebasing round's tab: the replay's progress and its
// narration, replaced whole per push.
func rebasingTab(st tabState, replayed, total int, lines []string) *frontendv1.FeedMergeTab {
	inner := &frontendv1.FeedMergeTabRebasing{Progress: &frontendv1.FeedMergeRebaseProgress{Replayed: uint32(replayed), Total: uint32(total)}}
	for _, line := range lines {
		inner.Lines = append(inner.Lines, &frontendv1.FeedMergeRebaseLine{Text: line})
	}
	if live, settled := st.badge(); live != nil {
		inner.State = &frontendv1.FeedMergeTabRebasing_Live{Live: live}
	} else {
		inner.State = &frontendv1.FeedMergeTabRebasing_Settled{Settled: settled}
	}
	return &frontendv1.FeedMergeTab{Kind: &frontendv1.FeedMergeTab_Rebasing{Rebasing: inner}}
}

// conflictsTab builds the conflicts tab, whose content is the sub-feed rows
// parented to it: the requester's own session resolving the conflict.
func conflictsTab(st tabState) *frontendv1.FeedMergeTab {
	inner := &frontendv1.FeedMergeTabConflicts{}
	if live, settled := st.badge(); live != nil {
		inner.State = &frontendv1.FeedMergeTabConflicts_Live{Live: live}
	} else {
		inner.State = &frontendv1.FeedMergeTabConflicts_Settled{Settled: settled}
	}
	return &frontendv1.FeedMergeTab{Kind: &frontendv1.FeedMergeTab_Conflicts{Conflicts: inner}}
}

// testsTab builds a tests round's tab: the per-suite rows with their painted
// output, and the round's log link once the run has written it, replaced whole
// per push.
func testsTab(st tabState, suites []*frontendv1.FeedMergeTestSuite, log *frontendv1.FeedMergeTestLog) *frontendv1.FeedMergeTab {
	inner := &frontendv1.FeedMergeTabTests{Suites: suites, Log: log}
	if live, settled := st.badge(); live != nil {
		inner.State = &frontendv1.FeedMergeTabTests_Live{Live: live}
	} else {
		inner.State = &frontendv1.FeedMergeTabTests_Settled{Settled: settled}
	}
	return &frontendv1.FeedMergeTab{Kind: &frontendv1.FeedMergeTab_Tests{Tests: inner}}
}

// fixesTab builds a fixing attempt's tab, carrying the attempt and the ONE
// bound; its content is the sub-feed rows parented to it.
func fixesTab(st tabState, attempt int) *frontendv1.FeedMergeTab {
	inner := &frontendv1.FeedMergeTabFixes{Attempt: &frontendv1.FeedMergeFixAttempt{Attempt: uint32(attempt), MaxAttempts: MaxFixAttempts}}
	if live, settled := st.badge(); live != nil {
		inner.State = &frontendv1.FeedMergeTabFixes_Live{Live: live}
	} else {
		inner.State = &frontendv1.FeedMergeTabFixes_Settled{Settled: settled}
	}
	return &frontendv1.FeedMergeTab{Kind: &frontendv1.FeedMergeTab_Fixes{Fixes: inner}}
}

// committingTab builds the committing tab: the merge commit's subject.
func committingTab(st tabState, subject string) *frontendv1.FeedMergeTab {
	inner := &frontendv1.FeedMergeTabCommitting{Subject: &frontendv1.FeedMergeCommitSubject{Text: subject}}
	if live, settled := st.badge(); live != nil {
		inner.State = &frontendv1.FeedMergeTabCommitting_Live{Live: live}
	} else {
		inner.State = &frontendv1.FeedMergeTabCommitting_Settled{Settled: settled}
	}
	return &frontendv1.FeedMergeTab{Kind: &frontendv1.FeedMergeTab_Committing{Committing: inner}}
}

// updatingMainTab builds the updating-main tab: fetching until the upstream
// tip is known, then fast-forwarding to it.
func updatingMainTab(st tabState, commit string) *frontendv1.FeedMergeTab {
	inner := &frontendv1.FeedMergeTabUpdatingMain{Step: &frontendv1.FeedMergeUpdatingMainStep{
		Step: &frontendv1.FeedMergeUpdatingMainStep_Fetching{Fetching: &frontendv1.FeedMergeUpdatingMainFetching{}}}}
	if commit != "" {
		inner.Step = &frontendv1.FeedMergeUpdatingMainStep{Step: &frontendv1.FeedMergeUpdatingMainStep_FastForwarding{
			FastForwarding: &frontendv1.FeedMergeUpdatingMainFastForwarding{Commit: commit}}}
	}
	if live, settled := st.badge(); live != nil {
		inner.State = &frontendv1.FeedMergeTabUpdatingMain_Live{Live: live}
	} else {
		inner.State = &frontendv1.FeedMergeTabUpdatingMain_Settled{Settled: settled}
	}
	return &frontendv1.FeedMergeTab{Kind: &frontendv1.FeedMergeTab_UpdatingMain{UpdatingMain: inner}}
}
