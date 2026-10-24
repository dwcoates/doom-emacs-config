package cachelapse

import (
	"context"
	"errors"
	"slices"
	"testing"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// dequeuingDB is the slice of a state client ObserveMergeQueues wraps: the
// two removals it observes.
type dequeuingDB struct {
	wsm.DB
	err   error
	wrote []string
}

func (d *dequeuingDB) RemoveMergeQueueEntry(_ context.Context, _ wsm.RepoKey, id wsm.WorkspaceID, _ string) error {
	d.wrote = append(d.wrote, "remove "+string(id))
	return d.err
}

func (d *dequeuingDB) DropAdmittedMerge(_ context.Context, _ wsm.RepoKey, id wsm.WorkspaceID, _ wsm.LeaseID, _ string) error {
	d.wrote = append(d.wrote, "drop "+string(id))
	return d.err
}

type queueEdges struct{ told []ids.WorkspaceID }

func (q *queueEdges) MergeLeftQueue(ws ids.WorkspaceID) { q.told = append(q.told, ws) }

func TestObserveMergeQueues(t *testing.T) {
	writes := []struct {
		name  string
		write func(wsm.DB) error
		wrote string
	}{
		{name: "a removal", write: func(db wsm.DB) error {
			return db.RemoveMergeQueueEntry(context.Background(), "/repo/.git", theWorkspace, "failed")
		}, wrote: "remove " + string(theWorkspace)},
		{name: "a dropped admitted merge", write: func(db wsm.DB) error {
			return db.DropAdmittedMerge(context.Background(), "/repo/.git", theWorkspace, "lease", "evicted")
		}, wrote: "drop " + string(theWorkspace)},
	}
	refused := errors.New("database is locked")
	outcomes := []struct {
		name string
		err  error
		told []ids.WorkspaceID
	}{
		{name: "recorded tells the sink", told: []ids.WorkspaceID{theWorkspace}},
		{name: "refused tells nothing", err: refused},
	}
	for _, w := range writes {
		for _, o := range outcomes {
			t.Run(w.name+" "+o.name, func(t *testing.T) {
				// Arrange
				inner := &dequeuingDB{err: o.err}
				sink := &queueEdges{}
				db := ObserveMergeQueues(inner, sink)

				// Act
				err := w.write(db)

				// Assert
				if !errors.Is(err, o.err) || (o.err == nil) != (err == nil) {
					t.Fatalf("write = %v, want %v", err, o.err)
				}
				if !slices.Equal(inner.wrote, []string{w.wrote}) {
					t.Fatalf("wrote %v, want [%s]", inner.wrote, w.wrote)
				}
				if !slices.Equal(sink.told, o.told) {
					t.Fatalf("told %v, want %v", sink.told, o.told)
				}
			})
		}
	}
}
