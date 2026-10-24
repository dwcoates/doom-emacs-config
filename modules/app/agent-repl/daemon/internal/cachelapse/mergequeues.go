package cachelapse

import (
	"context"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// MergeQueueSink is told of every entry leaving a merge queue once it is
// recorded. The Keeper is the production one.
type MergeQueueSink interface {
	MergeLeftQueue(ws ids.WorkspaceID)
}

// ObserveMergeQueues wraps a state client so every removal from a merge queue
// (RemoveMergeQueueEntry, DropAdmittedMerge) it records is told to SINK,
// after the write.
// Nothing else about the client changes.
//
// EVERY END OF A MERGE REACHES THE KEEPER. A merge leaves its queue when it
// lands, fails, is taken off, or a restart or a fork failure ends it, and each
// of those writes the removal through this one client; told after the write,
// the keeper reads the queues without the entry. A write that failed recorded
// nothing, so the sink is not told.
func ObserveMergeQueues(db wsm.DB, sink MergeQueueSink) wsm.DB {
	return &mergeQueueObservedDB{DB: db, sink: sink}
}

type mergeQueueObservedDB struct {
	wsm.DB
	sink MergeQueueSink
}

// RemoveMergeQueueEntry records the removal, then tells the sink.
func (o *mergeQueueObservedDB) RemoveMergeQueueEntry(ctx context.Context, repo wsm.RepoKey, id wsm.WorkspaceID, cause string) error {
	if err := o.DB.RemoveMergeQueueEntry(ctx, repo, id, cause); err != nil {
		return err
	}
	o.sink.MergeLeftQueue(id)
	return nil
}

// DropAdmittedMerge records the drop, then tells the sink.
func (o *mergeQueueObservedDB) DropAdmittedMerge(ctx context.Context, repo wsm.RepoKey, id wsm.WorkspaceID, lease wsm.LeaseID, cause string) error {
	if err := o.DB.DropAdmittedMerge(ctx, repo, id, lease, cause); err != nil {
		return err
	}
	o.sink.MergeLeftQueue(id)
	return nil
}
