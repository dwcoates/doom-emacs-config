package cachelapse

import (
	"context"
	"errors"
	"slices"
	"testing"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// queueingDB is the slice of a state client ObserveMergeQueues wraps: the
// entry and the two removals it observes.
type queueingDB struct {
	wsm.DB
	err   error
	wrote []string
}

func (d *queueingDB) QueueMerge(_ context.Context, _ wsm.RepoKey, id wsm.WorkspaceID, _ wsm.LeaseID) (int, error) {
	d.wrote = append(d.wrote, "queue "+string(id))
	return 1, d.err
}

func (d *queueingDB) RemoveMergeQueueEntry(_ context.Context, _ wsm.RepoKey, id wsm.WorkspaceID, _ string) error {
	d.wrote = append(d.wrote, "remove "+string(id))
	return d.err
}

func (d *queueingDB) DropAdmittedMerge(_ context.Context, _ wsm.RepoKey, id wsm.WorkspaceID, _ wsm.LeaseID, _ string) error {
	d.wrote = append(d.wrote, "drop "+string(id))
	return d.err
}

type queueEdges struct{ told []string }

func (q *queueEdges) MergeEnteredQueue(ws ids.WorkspaceID) {
	q.told = append(q.told, "entered "+string(ws))
}
func (q *queueEdges) MergeLeftQueue(ws ids.WorkspaceID) { q.told = append(q.told, "left "+string(ws)) }

func TestObserveMergeQueues(t *testing.T) {
	writes := []struct {
		name  string
		write func(wsm.DB) error
		wrote string
		edge  string
	}{
		{name: "an entry", write: func(db wsm.DB) error {
			_, err := db.QueueMerge(context.Background(), "/repo/.git", theWorkspace, "lease")
			return err
		}, wrote: "queue " + string(theWorkspace), edge: "entered " + string(theWorkspace)},
		{name: "a removal", write: func(db wsm.DB) error {
			return db.RemoveMergeQueueEntry(context.Background(), "/repo/.git", theWorkspace, "failed")
		}, wrote: "remove " + string(theWorkspace), edge: "left " + string(theWorkspace)},
		{name: "a dropped admitted merge", write: func(db wsm.DB) error {
			return db.DropAdmittedMerge(context.Background(), "/repo/.git", theWorkspace, "lease", "evicted")
		}, wrote: "drop " + string(theWorkspace), edge: "left " + string(theWorkspace)},
	}
	refused := errors.New("database is locked")
	outcomes := []struct {
		name string
		err  error
		tell bool
	}{
		{name: "recorded tells the sink", tell: true},
		{name: "refused tells nothing", err: refused},
	}
	for _, w := range writes {
		for _, o := range outcomes {
			t.Run(w.name+" "+o.name, func(t *testing.T) {
				// Arrange
				inner := &queueingDB{err: o.err}
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
				var want []string
				if o.tell {
					want = []string{w.edge}
				}
				if !slices.Equal(sink.told, want) {
					t.Fatalf("told %v, want %v", sink.told, want)
				}
			})
		}
	}
}
