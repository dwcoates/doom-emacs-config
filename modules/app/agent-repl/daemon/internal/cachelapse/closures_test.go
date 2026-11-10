package cachelapse

import (
	"context"
	"errors"
	"slices"
	"testing"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// closingDB is the slice of a state client ObserveClosures wraps: the
// writes it observes.
type closingDB struct {
	wsm.DB
	err   error
	wrote []string
}

func (c *closingDB) MarkClosed(_ context.Context, id wsm.WorkspaceID, _ wsm.Closure) error {
	c.wrote = append(c.wrote, "close "+string(id))
	return c.err
}

func (c *closingDB) RemoveMergeQueueEntry(_ context.Context, _ wsm.RepoKey, id wsm.WorkspaceID, _ string, _ *wsm.Closure) error {
	c.wrote = append(c.wrote, "exit "+string(id))
	return c.err
}

func (c *closingDB) DropAdmittedMerge(_ context.Context, _ wsm.RepoKey, id wsm.WorkspaceID, _ wsm.LeaseID, _ string, _ *wsm.Closure) error {
	c.wrote = append(c.wrote, "drop "+string(id))
	return c.err
}

func (c *closingDB) Reopen(_ context.Context, id wsm.WorkspaceID, _ wsm.InstanceID) error {
	c.wrote = append(c.wrote, "reopen "+string(id))
	return c.err
}

type closureEdges struct{ told []ids.WorkspaceID }

func (c *closureEdges) ClosureChanged(ws ids.WorkspaceID) { c.told = append(c.told, ws) }

func TestObserveClosures(t *testing.T) {
	exitClose := &wsm.Closure{At: now, By: wsm.CloseByDaemon}
	writes := []struct {
		name  string
		write func(wsm.DB) error
		wrote string
		// silent is a write that changes no closure, so it tells nothing
		// even when recorded.
		silent bool
	}{
		{name: "a queue exit that closes", write: func(db wsm.DB) error {
			return db.RemoveMergeQueueEntry(context.Background(), "/repo/.git", theWorkspace, "evicted", exitClose)
		}, wrote: "exit " + string(theWorkspace)},
		{name: "an admitted drop that closes", write: func(db wsm.DB) error {
			return db.DropAdmittedMerge(context.Background(), "/repo/.git", theWorkspace, "lease", "evicted", exitClose)
		}, wrote: "drop " + string(theWorkspace)},
		{name: "a queue exit that closes nothing", write: func(db wsm.DB) error {
			return db.RemoveMergeQueueEntry(context.Background(), "/repo/.git", theWorkspace, "merged", nil)
		}, wrote: "exit " + string(theWorkspace), silent: true},
		{name: "an admitted drop that closes nothing", write: func(db wsm.DB) error {
			return db.DropAdmittedMerge(context.Background(), "/repo/.git", theWorkspace, "lease", "workspace_closed", nil)
		}, wrote: "drop " + string(theWorkspace), silent: true},
		{name: "a close", write: func(db wsm.DB) error {
			return db.MarkClosed(context.Background(), theWorkspace, wsm.Closure{At: now, By: wsm.CloseByUser})
		}, wrote: "close " + string(theWorkspace)},
		{name: "a reopen", write: func(db wsm.DB) error {
			return db.Reopen(context.Background(), theWorkspace, "instance-a")
		}, wrote: "reopen " + string(theWorkspace)},
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
				inner := &closingDB{err: o.err}
				sink := &closureEdges{}
				db := ObserveClosures(inner, sink)

				// Act
				err := w.write(db)

				// Assert
				if !errors.Is(err, o.err) || (o.err == nil) != (err == nil) {
					t.Fatalf("write = %v, want %v", err, o.err)
				}
				if !slices.Equal(inner.wrote, []string{w.wrote}) {
					t.Fatalf("wrote %v, want [%s]", inner.wrote, w.wrote)
				}
				want := o.told
				if w.silent {
					want = nil
				}
				if !slices.Equal(sink.told, want) {
					t.Fatalf("told %v, want %v", sink.told, want)
				}
			})
		}
	}
}
