package merge

import (
	"context"

	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/reflect/protoreflect"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// A MERGE BUBBLE IS LIVE EXACTLY WHILE ITS MERGE IS IN A QUEUE (owner ruling,
// 2026-11-21). The stored queue is the one fact a bubble's life is read from:
// the head book knows every live bubble -- each one this daemon drew, and each
// one the durable store holds live from an earlier daemon (seedLiveBubbles)
// -- and every publication of a queue ends each live bubble whose ledger no
// stored queue entry names (endBubblesOffQueue). Every exit still draws its
// own terminal, which says how the merge ended and replaces this one; no
// exit's omission can leave a bubble live, and none survives a restart
// (2026-11-21: desktop-notifications-missing's bubble of 2026-11-14 stood
// live and pinned, its queue tab ticking, a week after its merge had left
// the queue, as did 36 others across the workspaces).

// offQueueSummary is the abandoned terminal's line for a bubble whose merge
// left its queue without its own terminal.
const offQueueSummary = "this merge is no longer in the merge queue"

// The operation this file's records carry.
const opLiveBubbles = "daemon.merge.live_bubbles"

// seedLiveBubbles adds to the head book every merge head the durable store
// holds LIVE in the feeds of SCOPE (every workspace when empty), so a bubble
// an earlier daemon drew is ended like one this daemon drew. A ledger this
// daemon already resolved stays resolved. A read that fails is an ERROR
// answered to the caller: the boot or the adoption fails loudly rather than
// leave live bubbles nothing knows of.
func (o *orchestrator) seedLiveBubbles(ctx context.Context, scope []ids.WorkspaceID) error {
	heads, err := o.storedLiveHeads(ctx, scope)
	if err != nil {
		o.deps.Log.Global().Error(opLiveBubbles, "could not read the live merge heads the store holds", dlog.WithCause(dlog.Context{"workspaces": len(scope)}, err))
		return err
	}
	o.heads.mu.Lock()
	defer o.heads.mu.Unlock()
	seeded := 0
	for ledger, feed := range heads {
		if o.heads.resolved[ledger] {
			continue
		}
		if _, known := o.heads.live[ledger]; !known {
			seeded++
		}
		o.heads.live[ledger] = feed
	}
	o.deps.Log.Global().Info(opLiveBubbles, "the head book knows every live merge bubble the store holds", dlog.Context{
		"workspaces": len(scope), "stored_live": len(heads), "seeded": seeded})
	return nil
}

// storedLiveHeads answers the ledger and feed workspace of every merge head
// the store holds live in the feeds of SCOPE.
func (o *orchestrator) storedLiveHeads(ctx context.Context, scope []ids.WorkspaceID) (map[ids.LeaseID]ids.WorkspaceID, error) {
	rows, err := o.deps.DB.MatchDurableFeedRows(ctx, scope, func(_ ids.WorkspaceID, id string) bool {
		_, head := mergeHeadLedger(id)
		return head
	})
	if err != nil {
		return nil, err
	}
	out := map[ids.LeaseID]ids.WorkspaceID{}
	for _, stored := range rows {
		row, err := decodeStored(stored)
		if err != nil {
			return nil, err
		}
		if !headRowIsLive(row) {
			continue
		}
		ledger, _ := mergeHeadLedger(stored.RowID)
		out[ledger] = stored.Workspace
	}
	return out, nil
}

// mergeHeadLedger answers the ledger of the merge head row ID names, false
// when ID is no merge head's.
func mergeHeadLedger(id string) (ids.LeaseID, bool) {
	ref, err := feedid.Decode(&frontendv1.FeedId{Value: id})
	if err != nil || !ref.Feed.Root || ref.Row.Kind != feedid.KindMergeHead {
		return "", false
	}
	return ids.LeaseID(ref.Row.ID), true
}

// headRowIsLive reports whether ROW is a merge head drawn in flight
// (frontend.v1.FeedMerge.update) rather than with an outcome.
func headRowIsLive(row *frontendv1.FeedRow) bool {
	return row.GetActivity().GetMerge().GetUpdate() != nil
}

// endBubblesOffQueue ends every live bubble whose ledger no stored queue
// entry names, WHY naming the publication that asked. It runs under the head
// book's lock, so no head publication interleaves with the read of the queue
// and the ends it decides. A read that fails is an ERROR answered to the
// caller, and nothing is ended.
func (o *orchestrator) endBubblesOffQueue(ctx context.Context, why string) error {
	o.heads.mu.Lock()
	defer o.heads.mu.Unlock()
	if len(o.heads.live) == 0 {
		return nil
	}
	queues, err := o.deps.DB.AllMergeQueues(ctx)
	if err != nil {
		o.deps.Log.Global().Error(opLiveBubbles, "could not read the merge queues; no live bubble is ended", dlog.WithCause(dlog.Context{"why": why}, err))
		return err
	}
	named := o.queuedLedgers(queues)
	for ledger, feed := range o.heads.live {
		if named[ledger] {
			continue
		}
		if err := o.endOffQueue(ctx, ledger, feed, why); err != nil {
			return err
		}
	}
	return nil
}

// queuedLedgers answers the ledger of every merge the stored QUEUES hold: the
// one its row carries, or, for a row an earlier build queued without one, the
// one this daemon minted for it (ledgerOfEntry).
func (o *orchestrator) queuedLedgers(queues map[wsm.RepoKey][]wsm.MergeQueueEntry) map[ids.LeaseID]bool {
	o.mu.Lock()
	defer o.mu.Unlock()
	named := map[ids.LeaseID]bool{}
	for _, entries := range queues {
		for _, entry := range entries {
			if entry.Ledger != "" {
				named[entry.Ledger] = true
			} else if minted, ok := o.ledgerOf[entry.Workspace]; ok {
				named[minted] = true
			}
		}
	}
	return named
}

// endOffQueue ends LEDGER's bubble in FEED's feed: its head on the abandoned
// terminal (offQueueSummary), settled where it stands, and every tab of it
// still running settled failed. The store's head is read again first: a head
// already settled there -- its exit's terminal drawn by another daemon -- is
// only forgotten. The caller holds the head book's lock.
func (o *orchestrator) endOffQueue(ctx context.Context, ledger ids.LeaseID, feed ids.WorkspaceID, why string) error {
	fields := dlog.Context{"ledger": string(ledger), "feed_workspace": string(feed), "why": why}
	rows, err := o.deps.DB.MatchDurableFeedRows(ctx, []ids.WorkspaceID{feed}, func(_ ids.WorkspaceID, id string) bool {
		return bubbleRowOf(id, ledger)
	})
	if err != nil {
		o.deps.Log.Global().Error(opLiveBubbles, "could not read a live bubble whose merge is in no queue; it is not ended", dlog.WithCause(fields, err))
		return err
	}
	endedMS := o.nowMS()
	var head *frontendv1.FeedRow
	var tabs []*frontendv1.FeedRow
	for _, stored := range rows {
		row, err := decodeStored(stored)
		if err != nil {
			o.deps.Log.Global().Error(opLiveBubbles, "a live bubble whose merge is in no queue has a row that cannot be read; it is not ended", dlog.WithCause(withField(fields, "row", stored.RowID), err))
			return err
		}
		if _, isHead := mergeHeadLedger(stored.RowID); isHead {
			head = row
			continue
		}
		if settleRunningStates(row.ProtoReflect(), endedMS, offQueueSummary) {
			tabs = append(tabs, row)
		}
	}
	delete(o.heads.live, ledger)
	o.bubbleTabs.forget(ledger)
	if head == nil || !headRowIsLive(head) {
		o.deps.Log.Global().Debug(opLiveBubbles, "a bubble whose merge is in no queue already stands settled in the store; it is forgotten", withField(fields, "stored", head != nil))
		return nil
	}
	o.heads.resolved[ledger] = true
	head.GetActivity().GetMerge().Result = &frontendv1.FeedMerge_Error{Error: &frontendv1.FeedMergeError{
		EndedAtMs: endedMS,
		Reason:    &frontendv1.FeedMergeError_Abandoned{Abandoned: &frontendv1.FeedMergeAbandoned{Summary: offQueueSummary}},
	}}
	head.GetActivity().GetMerge().QueueOffer = nil
	for _, tab := range tabs {
		o.deps.Feed.UpsertDurable(feed, mergeFeed(ledger), tab)
	}
	o.deps.Feed.UpsertDurable(feed, feedid.Feed{Root: true}, head)
	o.deps.Log.Global().Info(opLiveBubbles, "a live bubble's merge is in no queue; its bubble ends abandoned", withField(fields, "tabs_settled", len(tabs)))
	return nil
}

// bubbleRowOf reports whether row ID is LEDGER's bubble's: its head on the
// root feed, or a tab on its own sub-feed.
func bubbleRowOf(id string, ledger ids.LeaseID) bool {
	ref, err := feedid.Decode(&frontendv1.FeedId{Value: id})
	if err != nil {
		return false
	}
	if ref.Feed.Root {
		return ref.Row.Kind == feedid.KindMergeHead && ids.LeaseID(ref.Row.ID) == ledger
	}
	return ref.Feed.Merge != nil && *ref.Feed.Merge == ledger
}

// settleRunningStates settles, everywhere in MSG, every badge still running --
// live, waiting on the user, or on hold -- as failed with SUMMARY at ENDEDMS,
// keeping when its work began, and answers whether it settled any. A badge is
// a oneof that offers frontend.v1.FeedMergeTabSettled; every other is left
// as it stands.
func settleRunningStates(msg protoreflect.Message, endedMS int64, summary string) bool {
	changed := false
	oneofs := msg.Descriptor().Oneofs()
	for i := 0; i < oneofs.Len(); i++ {
		oneof := oneofs.Get(i)
		settled := settledField(oneof)
		set := msg.WhichOneof(oneof)
		if settled == nil || set == nil || !runningBadge(set) {
			continue
		}
		started := msg.Get(set).Message().Get(set.Message().Fields().ByName("started_at_ms")).Int()
		badge := &frontendv1.FeedMergeTabSettled{
			EndedAtMs: endedMS, StartedAtMs: started,
			Outcome: &frontendv1.FeedMergeTabSettled_Failed{Failed: &frontendv1.FeedMergeTabFailed{Summary: summary}},
		}
		msg.Set(settled, protoreflect.ValueOfMessage(badge.ProtoReflect()))
		changed = true
	}
	// NO MERGE ROW CARRIES A BADGE IN A LIST OR A MAP (frontend.v1 feed.proto),
	// so only the singular messages are walked.
	msg.Range(func(fd protoreflect.FieldDescriptor, v protoreflect.Value) bool {
		if fd.Message() != nil && fd.Cardinality() != protoreflect.Repeated {
			changed = settleRunningStates(v.Message(), endedMS, summary) || changed
		}
		return true
	})
	return changed
}

// settledField answers ONEOF's arm of type frontend.v1.FeedMergeTabSettled,
// nil when it offers none.
func settledField(oneof protoreflect.OneofDescriptor) protoreflect.FieldDescriptor {
	want := (&frontendv1.FeedMergeTabSettled{}).ProtoReflect().Descriptor().FullName()
	fields := oneof.Fields()
	for i := 0; i < fields.Len(); i++ {
		if f := fields.Get(i); f.Message() != nil && f.Message().FullName() == want {
			return f
		}
	}
	return nil
}

// runningBadge reports whether FIELD holds a badge of work still running.
func runningBadge(field protoreflect.FieldDescriptor) bool {
	if field.Message() == nil {
		return false
	}
	switch field.Message().FullName() {
	case (*frontendv1.FeedMergeTabLive)(nil).ProtoReflect().Descriptor().FullName(),
		(*frontendv1.FeedMergeTabWaitingOnUser)(nil).ProtoReflect().Descriptor().FullName(),
		(*frontendv1.FeedMergeTabHeld)(nil).ProtoReflect().Descriptor().FullName():
		return true
	}
	return false
}

// decodeStored decodes one durable row (wsm.DecodeDurableFeedRow).
func decodeStored(stored wsm.DurableFeedRow) (*frontendv1.FeedRow, error) {
	row, _, err := wsm.DecodeDurableFeedRow(stored.Row)
	return row, err
}
