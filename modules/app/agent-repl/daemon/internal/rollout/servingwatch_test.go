package rollout

import (
	"context"
	"errors"
	"sync"
	"testing"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// servingCountDB records every serving read the controller makes, and can
// fail the change watch.
type servingCountDB struct {
	wsm.DB
	mu       sync.Mutex
	batches  [][]wsm.WorkspaceID
	singles  int
	watchErr error
}

func (d *servingCountDB) ServingOf(ctx context.Context, workspaces []wsm.WorkspaceID) (map[wsm.WorkspaceID]*wsm.InstanceID, error) {
	d.mu.Lock()
	d.batches = append(d.batches, append([]wsm.WorkspaceID(nil), workspaces...))
	d.mu.Unlock()
	return d.DB.ServingOf(ctx, workspaces)
}

func (d *servingCountDB) Serving(ctx context.Context, ws wsm.WorkspaceID) (*wsm.InstanceID, error) {
	d.mu.Lock()
	d.singles++
	d.mu.Unlock()
	return d.DB.Serving(ctx, ws)
}

func (d *servingCountDB) Changes(ctx context.Context) (<-chan error, func()) {
	if d.watchErr == nil {
		return d.DB.Changes(ctx)
	}
	out := make(chan error, 1)
	out <- d.watchErr
	return out, func() {}
}

// recorded answers the serving reads so far: each batch, and how many
// single-row reads.
func (d *servingCountDB) recorded() ([][]wsm.WorkspaceID, int) {
	d.mu.Lock()
	defer d.mu.Unlock()
	return append([][]wsm.WorkspaceID(nil), d.batches...), d.singles
}

func newServingWatchHarness(t *testing.T, watchErr error) (*harness, *servingCountDB) {
	t.Helper()
	var db *servingCountDB
	h := newHarness(t, func(d *Deps) {
		db = &servingCountDB{DB: d.DB, watchErr: watchErr}
		d.DB = db
	})
	return h, db
}

// claimedBy registers a workspace the instance OWNER serves.
func claimedBy(t *testing.T, h *harness, owner ids.InstanceID) ids.WorkspaceID {
	t.Helper()
	ws, _ := h.workspace(t)
	if err := h.db.ClaimServing(context.Background(), ws, owner); err != nil {
		t.Fatalf("ClaimServing: %v", err)
	}
	return ws
}

// nextRead takes the waiter's next answer, failing the test when the test's
// own lifetime ends first.
func nextRead(t *testing.T, waiter *servingWaiter) servingRead {
	t.Helper()
	select {
	case read := <-waiter.reads:
		return read
	case <-t.Context().Done():
		t.Fatalf("no serving answer for %s", waiter.ws)
		return servingRead{}
	}
}

func TestServingWatchAnswersANewWaiterWithoutACommit(t *testing.T) {
	// Arrange
	h, _ := newServingWatchHarness(t, nil)
	ws := claimedBy(t, h, "daemon-incumbent")

	// Act
	waiter, stop := h.c.serving.watch(ws)
	defer stop()
	read := nextRead(t, waiter)

	// Assert
	if read.err != nil || read.owner == nil || *read.owner != "daemon-incumbent" {
		t.Fatalf("read = %+v, want the incumbent as owner", read)
	}
}

func TestServingWatchAnswersEveryWaiterFromOneReadOfEveryRow(t *testing.T) {
	// Arrange: three adoptions waiting on the same incumbent.
	h, db := newServingWatchHarness(t, nil)
	var waiters []*servingWaiter
	for range 3 {
		waiter, stop := h.c.serving.watch(claimedBy(t, h, "daemon-incumbent"))
		defer stop()
		waiters = append(waiters, waiter)
	}
	for _, waiter := range waiters {
		for read := nextRead(t, waiter); read.owner == nil; read = nextRead(t, waiter) {
		}
	}
	before, _ := db.recorded()

	// Act: the incumbent releases one row.
	released := waiters[1]
	if err := h.db.ReleaseServing(context.Background(), released.ws, "daemon-incumbent"); err != nil {
		t.Fatalf("ReleaseServing: %v", err)
	}
	for read := nextRead(t, released); read.owner != nil; read = nextRead(t, released) {
	}

	// Assert: every read since names every waiting row, and none reads one row.
	batches, singles := db.recorded()
	after := batches[len(before):]
	if len(after) == 0 {
		t.Fatalf("no serving read followed the release")
	}
	for _, batch := range after {
		if len(batch) != 3 {
			t.Fatalf("a serving read named %v, want all three waiting rows in one read", batch)
		}
	}
	if singles != 0 {
		t.Fatalf("single-row serving reads = %d, want none from the watch", singles)
	}
}

func TestServingWatchAnswersAnUnknownWorkspaceNotFound(t *testing.T) {
	// Arrange
	h, _ := newServingWatchHarness(t, nil)

	// Act
	waiter, stop := h.c.serving.watch("absent")
	defer stop()
	read := nextRead(t, waiter)

	// Assert
	if !errors.Is(read.err, wsm.ErrNotFound) || read.watchFailed {
		t.Fatalf("read = %+v, want a read failure wrapping ErrNotFound", read)
	}
}

func TestServingWatchAnswersAKnownWorkspaceBesideAnUnknownOne(t *testing.T) {
	// Arrange
	h, _ := newServingWatchHarness(t, nil)
	gone, stopGone := h.c.serving.watch("absent")
	defer stopGone()
	ws := claimedBy(t, h, "daemon-incumbent")

	// Act
	waiter, stop := h.c.serving.watch(ws)
	defer stop()
	nextRead(t, gone)
	read := nextRead(t, waiter)

	// Assert
	if read.err != nil || read.owner == nil || *read.owner != "daemon-incumbent" {
		t.Fatalf("read = %+v, want the incumbent, untouched by the unknown workspace", read)
	}
}

func TestServingWatchAnswersAWaiterRegisteredAfterTheLastLeft(t *testing.T) {
	// Arrange
	h, _ := newServingWatchHarness(t, nil)
	ws := claimedBy(t, h, "daemon-incumbent")
	first, stop := h.c.serving.watch(ws)
	nextRead(t, first)
	stop()

	// Act
	second, stopSecond := h.c.serving.watch(ws)
	defer stopSecond()
	read := nextRead(t, second)

	// Assert
	if read.err != nil || read.owner == nil {
		t.Fatalf("read = %+v, want the owner", read)
	}
}

func TestServingWatchMarksItselfStoppedWithNoWaiter(t *testing.T) {
	// Arrange
	h, _ := newServingWatchHarness(t, nil)
	w := h.c.serving
	w.running = true

	// Act
	got := w.registered()

	// Assert
	if got != nil || w.running {
		t.Fatalf("registered = %v, running = %v; want none and the loop marked stopped", got, w.running)
	}
}

func TestServingWatchHandsAFailedChangeWatchToItsWaiters(t *testing.T) {
	// Arrange
	watchErr := errors.New("kqueue refused the watch")
	h, _ := newServingWatchHarness(t, watchErr)
	ws := claimedBy(t, h, "daemon-incumbent")

	// Act
	waiter, stop := h.c.serving.watch(ws)
	defer stop()
	read := nextRead(t, waiter)
	for !read.watchFailed {
		read = nextRead(t, waiter)
	}

	// Assert
	if !errors.Is(read.err, watchErr) {
		t.Fatalf("read = %+v, want the change watch's failure", read)
	}
}

func TestServingWatchHandsTheEndOfTheLifetimeToItsWaiters(t *testing.T) {
	// Arrange
	h, _ := newServingWatchHarness(t, nil)
	ws := claimedBy(t, h, "daemon-incumbent")
	waiter, stop := h.c.serving.watch(ws)
	defer stop()
	nextRead(t, waiter)

	// Act
	h.endLifetime()
	read := nextRead(t, waiter)

	// Assert
	if !read.watchFailed || !errors.Is(read.err, context.Canceled) {
		t.Fatalf("read = %+v, want the ended lifetime as a watch failure", read)
	}
}
