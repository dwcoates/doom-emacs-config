package merge

import (
	"context"
	"fmt"
	"sync"

	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// THE BUBBLE RENDERS THE MERGE'S VIEW (owner ruling, 2026-11-16). Whether a
// tab runs is never its publisher's to say: every tab a step publishes goes
// through renderTab with the bubble's hold, and the one republish
// (republishBubbles, which publishRosterQueue runs after every ledger write
// and every change of the queue's hold) redraws each bubble the moment its
// hold moves. A merge's ledger holds at most one live round
// (wsm.RecordTabInterval), so a merge whose view holds a stage held
// (viewOf) is held whole: every tab of its bubble that would run is drawn on
// hold, its subtabs' included, and the bubble and the roster cannot disagree.

// bubbleHold is whether one bubble's merge is held, and since when (0 when
// its queue's hold holds it, which names no instant of its own).
type bubbleHold struct {
	held   bool
	heldMS int64
}

// holdOf is the hold of the merge under LEASE whose queue QUEUEHELD says
// holds its merges, with HELDROUNDS the store's held live rounds
// (wsm.HeldMergeRounds): the same rule viewOf draws a live stage by.
func holdOf(lease ids.LeaseID, queueHeld bool, heldRounds map[wsm.LeaseID]int64) bubbleHold {
	if at, held := heldRounds[lease]; held {
		return bubbleHold{held: true, heldMS: at}
	}
	return bubbleHold{held: queueHeld}
}

// publishedBubble is one bubble's tabs as their steps last published them,
// and the hold they were last drawn under.
type publishedBubble struct {
	ws    ids.WorkspaceID
	hold  bubbleHold
	order []string
	tabs  map[string]publishedTab
}

// publishedTab is one tab row as its step published it.
type publishedTab struct {
	key tabKey
	tab *frontendv1.FeedMergeTab
}

// bubbles holds every bubble's published tabs, by ledger.
type bubbles struct {
	mu sync.Mutex
	by map[ids.LeaseID]*publishedBubble
}

// keep records TAB under KEY in LEASE's bubble, drawn in WS, and answers the
// hold the bubble is drawn under.
func (b *bubbles) keep(ws ids.WorkspaceID, lease ids.LeaseID, key tabKey, tab *frontendv1.FeedMergeTab) bubbleHold {
	b.mu.Lock()
	defer b.mu.Unlock()
	if b.by == nil {
		b.by = map[ids.LeaseID]*publishedBubble{}
	}
	pb, ok := b.by[lease]
	if !ok {
		pb = &publishedBubble{ws: ws, tabs: map[string]publishedTab{}}
		b.by[lease] = pb
	}
	id := key.id + "/" + key.sub
	if _, seen := pb.tabs[id]; !seen {
		pb.order = append(pb.order, id)
	}
	pb.tabs[id] = publishedTab{key: key, tab: tab}
	return pb.hold
}

// moved sets the hold of LEASE's bubble, drawn in WS, to HOLD, and answers
// whether it moved and its tabs to redraw. A bubble that published nothing
// yet records its hold too, so the footer reads it (holdOfLease).
func (b *bubbles) moved(ws ids.WorkspaceID, lease ids.LeaseID, hold bubbleHold) (bool, []publishedTab) {
	b.mu.Lock()
	defer b.mu.Unlock()
	if b.by == nil {
		b.by = map[ids.LeaseID]*publishedBubble{}
	}
	pb, ok := b.by[lease]
	if !ok {
		pb = &publishedBubble{ws: ws, tabs: map[string]publishedTab{}}
		b.by[lease] = pb
	}
	if pb.hold == hold {
		return false, nil
	}
	pb.hold = hold
	out := make([]publishedTab, 0, len(pb.order))
	for _, id := range pb.order {
		out = append(out, pb.tabs[id])
	}
	return true, out
}

// holdOfLease answers the hold LEASE's bubble was last drawn under.
func (b *bubbles) holdOfLease(lease ids.LeaseID) bubbleHold {
	b.mu.Lock()
	defer b.mu.Unlock()
	if pb, ok := b.by[lease]; ok {
		return pb.hold
	}
	return bubbleHold{}
}

// forget drops LEASE's bubble: its merge ended, or moved to another daemon.
func (b *bubbles) forget(lease ids.LeaseID) {
	b.mu.Lock()
	defer b.mu.Unlock()
	delete(b.by, lease)
}

// renderTab draws TAB under HOLD: a tab of a held bubble whose state would
// run (live, or waiting on the user) is drawn on hold, keeping when its work
// began; any other is drawn as published.
func renderTab(tab *frontendv1.FeedMergeTab, hold bubbleHold) *frontendv1.FeedMergeTab {
	if !hold.held {
		return tab
	}
	out := proto.Clone(tab).(*frontendv1.FeedMergeTab)
	kinds := out.ProtoReflect().Descriptor().Oneofs().ByName("kind")
	kindField := out.ProtoReflect().WhichOneof(kinds)
	if kindField == nil {
		panic("merge: a merge tab names no kind")
	}
	inner := out.ProtoReflect().Mutable(kindField).Message()
	states := inner.Descriptor().Oneofs().ByName("state")
	if states == nil {
		panic(fmt.Sprintf("merge: the %s tab has no state", kindField.Name()))
	}
	stateField := inner.WhichOneof(states)
	if stateField == nil || (stateField.Name() != "live" && stateField.Name() != "waiting_on_user") {
		return out
	}
	started := inner.Get(stateField).Message().Get(stateField.Message().Fields().ByName("started_at_ms")).Int()
	onHold := states.Fields().ByName("on_hold")
	if onHold == nil {
		panic(fmt.Sprintf("merge: the %s tab cannot be drawn on hold", kindField.Name()))
	}
	held := &frontendv1.FeedMergeTabHeld{StartedAtMs: started, HeldSinceMs: hold.heldMS}
	inner.Set(onHold, protoreflect.ValueOfMessage(held.ProtoReflect()))
	return out
}

// republishBubbles redraws the bubble of every merge in ENTRIES, REPO's
// queue, whose hold moved: QUEUEHELD is the queue's hold, and the store's
// held rounds each merge's own. publishRosterQueue runs it after every ledger
// write and every change of the queue's hold, so it is the ONE republish of
// the bubbles' hold.
func (o *orchestrator) republishBubbles(ctx context.Context, entries []wsm.MergeQueueEntry, queueHeld bool, heldRounds map[wsm.LeaseID]int64) {
	for _, entry := range entries {
		lease, ok := o.leaseOf(entry.Workspace)
		if !ok {
			continue
		}
		// A MERGE OUT OF LINE RUNS ITS REPAIR TURN whatever its queue does: only
		// its own suspension holds it.
		hold := holdOf(lease, queueHeld && entry.State != wsm.MergeRemediating, heldRounds)
		changed, tabs := o.bubbleTabs.moved(entry.Workspace, lease, hold)
		if !changed {
			continue
		}
		for _, t := range tabs {
			o.upsertTab(entry.Workspace, lease, t.key, renderTab(t.tab, hold))
		}
		// THE FOOTER'S STEP IS DRAWN FROM THE SAME HOLD: its facts are
		// published again under it (publish).
		o.mu.Lock()
		facts, has := o.facts[entry.Workspace]
		o.mu.Unlock()
		if has && (facts.State == StateMerging || facts.State == StateQueued) {
			o.publish(entry.Repo, entry.Workspace, facts)
		}
	}
}
