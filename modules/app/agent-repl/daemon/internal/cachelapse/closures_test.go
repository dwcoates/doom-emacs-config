package cachelapse

import (
	"context"
	"errors"
	"slices"
	"testing"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// closingDB is the slice of a state client ObserveClosures wraps: the two
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

func (c *closingDB) Reopen(_ context.Context, id wsm.WorkspaceID, _ wsm.InstanceID) error {
	c.wrote = append(c.wrote, "reopen "+string(id))
	return c.err
}

type closureEdges struct{ told []ids.WorkspaceID }

func (c *closureEdges) ClosureChanged(ws ids.WorkspaceID) { c.told = append(c.told, ws) }

func TestObserveClosures(t *testing.T) {
	writes := []struct {
		name  string
		write func(wsm.DB) error
		wrote string
	}{
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
				if !slices.Equal(sink.told, o.told) {
					t.Fatalf("told %v, want %v", sink.told, o.told)
				}
			})
		}
	}
}
