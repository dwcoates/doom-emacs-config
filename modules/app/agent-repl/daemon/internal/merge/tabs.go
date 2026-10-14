package merge

import (
	"context"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// This file synthesizes the merge bubble: the head row on the root feed, and
// the tab rows on the bubble's own sub-feed.
//
// Top-level tabs are APPEND-ONLY and ROUND-NUMBERED. A tab never reopens — a
// second pass is a SECOND TAB carrying the round in its label ("committing
// (2)") — so the row key carries the round and a re-push of the same round
// upserts that tab in place rather than growing a third. The rebasing and the
// tests tab are the exception: ONE each, whose subtabs are keyed by what they
// are (nested.go).

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

// tabRef addresses one tab row of a merge bubble's sub-feed in WS's
// placement.
func tabRef(ws ids.WorkspaceID, lease ids.LeaseID, key tabKey) feedid.Ref {
	return feedid.Ref{
		WS:   ws,
		Feed: mergeFeed(lease),
		Row:  feedid.RowKey{Kind: feedid.KindMergeTab, ID: key.id, Sub: key.sub},
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
	case TabPick:
		// The ledger keeps the "pick" kind it always recorded; the tab is
		// drawn as the resolving turn it is (frontend.v1.FeedMergeTabResolving).
		return "resolving"
	case TabPrePrompt:
		return "pre-prompt"
	case TabRebasing:
		return "rebasing"
	case TabTests:
		return "tests"
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
	// waiting is a live agentic tab whose agent has an ask open: drawn
	// waiting on the user. Only conflict resolution and fixing read it.
	waiting bool
	// turnHolder names the merge holding the repository's test turn while a
	// test run, its round open, waits in line for it; empty otherwise. Only a
	// run's subtab reads it.
	turnHolder string
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

// waitingOnUser is the waiting badge of a live tab whose agent has an ask
// open, nil otherwise.
func (s tabState) waitingOnUser() *frontendv1.FeedMergeTabWaitingOnUser {
	if s.settled || !s.waiting {
		return nil
	}
	return &frontendv1.FeedMergeTabWaitingOnUser{StartedAtMs: s.startedMS}
}

// tabRow wraps one labelled tab into the feed row that carries it, parented
// to the tab it is drawn in when it is a subtab.
func tabRow(ws ids.WorkspaceID, lease ids.LeaseID, key tabKey, tab *frontendv1.FeedMergeTab) *frontendv1.FeedRow {
	row := &frontendv1.FeedRow{
		Id:  feedid.Encode(tabRef(ws, lease, key)),
		Row: &frontendv1.FeedRow_MergeTab{MergeTab: tab},
	}
	if key.parent != nil {
		row.Parent = &frontendv1.FeedRowParent{Row: feedid.Encode(tabRef(ws, lease, *key.parent))}
	}
	return row
}

// frontTabLabel is the label of the TOP-LEVEL tab a round is drawn under,
// which the queue shows for the merge at its front: a conflict resolution is
// drawn under the rebasing tab and a fixing attempt under the tests tab, so
// those rounds read as the tab the reader would find them in.
func frontTabLabel(round tabRound) *frontendv1.FeedMergeTabLabel {
	switch round.kind {
	case TabRebasing, TabConflicts:
		return tabLabel(TabRebasing, 1)
	case TabTests, TabFixes:
		return tabLabel(TabTests, 1)
	}
	return tabLabel(round.kind, round.n)
}

// headRow builds the bubble's collapsed head: the branch line, the clock, and
// THE ARM THAT IS THE STATE of the merge as a whole.
func headRow(ws ids.WorkspaceID, lease ids.LeaseID, label string, startedMS int64, result any) *frontendv1.FeedRow {
	merge := &frontendv1.FeedMerge{
		Head: &frontendv1.FeedMergeHead{
			Glyph:   &frontendv1.FeedMergeGlyph{Icon: "merge"},
			Label:   &frontendv1.FeedMergeLabel{Text: label},
			Runtime: &frontendv1.FeedMergeRuntime{StartedAtMs: startedMS},
			// NO FOLD: the feed resolver settles it on publication, the
			// reader's recorded fold or the daemon's default, at its one
			// deciding site (resolve/feed/mergefold.go).
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

// bubble is where one merging workspace's bubble is drawn, and how.
//
// THE BUBBLE IS DRAWN IN ONE FEED (frontend.v1.FeedMerge). A sync merge draws
// it in the merging workspace's own feed, PINNED while the merge is in flight.
// An ASYNC merge runs in a hidden merge workspace and draws its bubble in the
// REQUESTER's feed alone, PLACED rather than pinned: the requester's
// conversation moves on past it, and several async merges never stack at the
// feed's bottom. The merge workspace's own feed carries no bubble.
type bubble struct {
	// ws is the feed the bubble is drawn in.
	ws ids.WorkspaceID
	// mergeWS is the merge workspace an async merge runs in, nil for a sync
	// merge. The head's switch names it once it is visible.
	mergeWS *workspacev1.WorkspaceRef
}

// async reports whether the bubble is an async merge's.
func (b bubble) async() bool { return b.mergeWS != nil }

// mergeOrigin is what a merging workspace's bubble needs of its merge origin:
// the requester whose feed carries the bubble, and the merge workspace's own
// ref for the head's switch. Both are fixed at the merge workspace's
// registration.
type mergeOrigin struct {
	requester ids.WorkspaceID
	self      *workspacev1.WorkspaceRef
}

// originOf answers WS's merge origin, nil when WS is no merge workspace. The
// answer never changes once read, so it is cached.
//
// A MERGING WORKSPACE WHOSE RECORD CANNOT BE READ is recorded at ERROR and
// answered as no merge workspace, so its bubble is drawn in its own feed rather
// than nowhere.
func (o *orchestrator) originOf(ws ids.WorkspaceID) *mergeOrigin {
	o.mu.Lock()
	origin, cached := o.originsOf[ws]
	o.mu.Unlock()
	if cached {
		return origin
	}
	record, err := o.deps.DB.Workspace(context.Background(), ws)
	if err != nil {
		o.deps.Log.Global().Error("daemon.merge.placement", "could not read whether the merging workspace is a merge workspace; its bubble is drawn in its own feed",
			dlog.Context{"workspace": string(ws), "error": err.Error()})
		return nil
	}
	if record.MergeOf != nil {
		origin = &mergeOrigin{requester: record.MergeOf.Requester, self: &workspacev1.WorkspaceRef{Id: string(ws), Dir: record.Dir}}
	}
	o.mu.Lock()
	o.originsOf[ws] = origin
	o.mu.Unlock()
	return origin
}

// bubbleOf answers where WS's merge bubble is drawn.
func (o *orchestrator) bubbleOf(ws ids.WorkspaceID) bubble {
	if origin := o.originOf(ws); origin != nil {
		return bubble{ws: origin.requester, mergeWS: origin.self}
	}
	return bubble{ws: ws}
}

// requesterOf answers the requester of merge workspace WS, false when WS is no
// merge workspace.
func (o *orchestrator) requesterOf(ws ids.WorkspaceID) (ids.WorkspaceID, bool) {
	if origin := o.originOf(ws); origin != nil {
		return origin.requester, true
	}
	return "", false
}

// headSwitchLabel is the bubble head's switch text while the merge workspace
// exists.
const headSwitchLabel = "open merge workspace"

// mergeWorkspaceVisible reports whether merge workspace WS is drawn anywhere
// the user can switch to it: revealed, or landed (a landed merge workspace is
// an ordinary closed workspace). A record that cannot be read is recorded at
// ERROR and answered hidden, so the head offers no switch it cannot stand by.
func (o *orchestrator) mergeWorkspaceVisible(ws ids.WorkspaceID) bool {
	record, err := o.deps.DB.Workspace(context.Background(), ws)
	if err != nil {
		o.deps.Log.Global().Error("daemon.merge.placement", "could not read whether the merge workspace is visible; the bubble head offers no switch to it",
			dlog.Context{"workspace": string(ws), "error": err.Error()})
		return false
	}
	return !record.MergeHidden()
}

// publishHead publishes the bubble's head with RESULT, the arm in force.
//
// A SYNC HEAD IS PINNED TO THE ROOT FEED'S BOTTOM WHILE THE MERGE IS IN FLIGHT
// and settles where it stands when the merge resolves (owner ruling,
// 2026-10-10; frontend.v1 FeedRowOrder.pinned): a live head (no result) is
// published pinned, and the first publication carrying a result — landed,
// failed or abandoned — settles it, after which every later row lands below
// it. AN ASYNC HEAD IS NEVER PINNED (owner ruling, 2026-10-11): it is placed
// where it is first drawn. An async head carries the switch to the merge
// workspace once that workspace is visible.
//
// The arguments are kept, so a reveal can restate the head with its switch
// (restateHead).
func (o *orchestrator) publishHead(ws ids.WorkspaceID, lease ids.LeaseID, label string, startedMS int64, result any) {
	o.mu.Lock()
	o.heads[ws] = headArgs{lease: lease, label: label, startedMS: startedMS, result: result}
	o.mu.Unlock()
	at := o.bubbleOf(ws)
	row := headRow(at.ws, lease, label, startedMS, result)
	if at.async() && o.mergeWorkspaceVisible(ws) {
		row.GetActivity().GetMerge().Head.WorkspaceSwitch = &frontendv1.FeedMergeWorkspaceSwitch{
			Label: &frontendv1.FeedMergeWorkspaceSwitchLabel{Text: headSwitchLabel},
			Availability: &frontendv1.FeedMergeWorkspaceSwitch_Available{
				Available: &frontendv1.FeedMergeWorkspaceSwitchAvailable{Target: at.mergeWS},
			},
		}
	}
	if result == nil && !at.async() {
		o.mu.Lock()
		offered := o.offers[ws]
		o.mu.Unlock()
		if offered {
			row.GetActivity().GetMerge().QueueOffer = queueOffer()
		}
		o.deps.Feed.UpsertDurablePinned(at.ws, feedid.Feed{Root: true}, row)
		return
	}
	o.deps.Feed.UpsertDurable(at.ws, feedid.Feed{Root: true}, row)
}

// headArgs is the last head publishHead drew for one merging workspace.
type headArgs struct {
	lease     ids.LeaseID
	label     string
	startedMS int64
	result    any
}

// restateHead draws WS's merge head again as it was last drawn, so a change
// of the merge workspace's visibility reaches its switch. A workspace whose
// head was never drawn has nothing to restate.
func (o *orchestrator) restateHead(ws ids.WorkspaceID) {
	o.mu.Lock()
	head, drawn := o.heads[ws]
	o.mu.Unlock()
	if !drawn {
		return
	}
	o.publishHead(ws, head.lease, head.label, head.startedMS, head.result)
}

// publishRoundTab publishes one top-level tab round in the bubble's feed,
// labelled for its round.
func (o *orchestrator) publishRoundTab(ws ids.WorkspaceID, lease ids.LeaseID, kind string, round int, tab *frontendv1.FeedMergeTab) {
	tab.Label = tabLabel(kind, round)
	o.publishTab(ws, lease, roundTabKey(kind, round), tab)
}

// publishTab publishes one labelled tab row, under KEY, in the bubble's feed.
func (o *orchestrator) publishTab(ws ids.WorkspaceID, lease ids.LeaseID, key tabKey, tab *frontendv1.FeedMergeTab) {
	at := o.bubbleOf(ws)
	o.deps.Feed.UpsertDurable(at.ws, mergeFeed(lease), tabRow(at.ws, lease, key, tab))
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
