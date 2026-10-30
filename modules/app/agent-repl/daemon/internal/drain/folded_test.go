package drain

import (
	"context"
	"sync"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// waitsClock is a Clock whose every After gets its OWN channel, so a test
// ends one wait of several and knows which.
type waitsClock struct {
	mu    sync.Mutex
	waits []chan time.Time
	asked chan time.Duration
}

func newWaitsClock() *waitsClock { return &waitsClock{asked: make(chan time.Duration, 16)} }

func (c *waitsClock) Now() time.Time { return instant }

func (c *waitsClock) After(d time.Duration) <-chan time.Time {
	ch := make(chan time.Time, 1)
	c.mu.Lock()
	c.waits = append(c.waits, ch)
	c.mu.Unlock()
	c.asked <- d
	return ch
}

// end ends wait N (0-based, in the order they were asked).
func (c *waitsClock) end(n int) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.waits[n] <- instant
}

// foldedHarness is a harness whose detached work runs on goroutines a test
// joins (settle), over a waitsClock.
type foldedHarness struct {
	*harness
	waits    *waitsClock
	detached sync.WaitGroup
	ctx      context.Context
	cancel   context.CancelFunc

	mu sync.Mutex
	// runs close as each detached run returns, in the order they started.
	runs []chan struct{}
}

func newFoldedHarness(t *testing.T) *foldedHarness {
	t.Helper()
	f := &foldedHarness{waits: newWaitsClock()}
	f.ctx, f.cancel = context.WithCancel(context.Background())
	f.harness = newHarness(t, func(d *Deps) {
		d.Clock = f.waits
		d.Detach = func(run func(context.Context)) {
			done := make(chan struct{})
			f.mu.Lock()
			f.runs = append(f.runs, done)
			f.mu.Unlock()
			f.detached.Add(1)
			go func() {
				defer f.detached.Done()
				defer close(done)
				run(f.ctx)
			}()
		}
	})
	t.Cleanup(func() {
		f.cancel()
		f.detached.Wait()
	})
	return f
}

// awaitWait blocks until the code under test asks for its next wait, and
// answers its length.
func (f *foldedHarness) awaitWait(t *testing.T) time.Duration {
	t.Helper()
	select {
	case d := <-f.waits.asked:
		return d
	case <-time.After(5 * time.Second):
		t.Fatal("no wait was asked for")
		return 0
	}
}

// settle ends wait N and joins every detached run it starts.
func (f *foldedHarness) settle(n int) {
	f.waits.end(n)
	f.detached.Wait()
}

func TestAFoldedWorkspaceWaitsTheFoldsDelay(t *testing.T) {
	// Arrange
	f := newFoldedHarness(t)
	ws := f.collapsed(t)

	// Act
	f.c.StandDownFolded(ws)
	waited := f.awaitWait(t)

	// Assert: nothing is stood down while the wait stands.
	if waited != FoldStandDownDelay {
		t.Fatalf("waited %v, want %v", waited, FoldStandDownDelay)
	}
	if killed := f.stand.Killed(); len(killed) != 0 {
		t.Fatalf("kills = %+v before the delay, want none", killed)
	}
}

func TestTheDelayIsFiveMinutes(t *testing.T) {
	// Assert: the owner's request, 2026-10-30.
	if FoldStandDownDelay != 5*time.Minute {
		t.Fatalf("FoldStandDownDelay = %v, want 5m", FoldStandDownDelay)
	}
}

func TestAFoldedWorkspaceIsStoodDownOnceTheDelayIsOver(t *testing.T) {
	// Arrange
	f := newFoldedHarness(t)
	ws := f.collapsed(t)
	f.c.StandDownFolded(ws)
	f.awaitWait(t)

	// Act
	f.settle(0)

	// Assert
	killed := f.stand.Killed()
	if len(killed) != 1 || killed[0].WS != ws || killed[0].Force {
		t.Fatalf("kills = %+v, want one graceful stand-down", killed)
	}
	if _, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Level: "info", Operation: opFolded, Message: "a folded workspace's delay is over; its stand-down is asked for"}); !ok {
		t.Fatal("no record of the delay's end")
	}
}

func TestASupersededWaitStandsNothingDown(t *testing.T) {
	// Arrange: folded, then folded again.
	f := newFoldedHarness(t)
	ws := f.collapsed(t)
	f.c.StandDownFolded(ws)
	f.awaitWait(t)
	f.c.StandDownFolded(ws)
	f.awaitWait(t)

	// Act: the first fold's wait ends, and its run returns.
	f.waits.end(0)
	f.mu.Lock()
	first := f.runs[0]
	f.mu.Unlock()
	<-first

	// Assert
	if killed := f.stand.Killed(); len(killed) != 0 {
		t.Fatalf("kills = %+v after a superseded wait, want none", killed)
	}
	if _, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Level: "debug", Operation: opFolded, Message: "a later collapse superseded this wait; its own wait stands the session down"}); !ok {
		t.Fatal("no record of the superseded wait")
	}
}

func TestTheLatestWaitStandsTheSessionDown(t *testing.T) {
	// Arrange: folded, then folded again.
	f := newFoldedHarness(t)
	ws := f.collapsed(t)
	f.c.StandDownFolded(ws)
	f.awaitWait(t)
	f.c.StandDownFolded(ws)
	f.awaitWait(t)

	// Act: the latest fold's wait ends, then the superseded one's.
	f.waits.end(1)
	f.settle(0)

	// Assert
	if killed := f.stand.Killed(); len(killed) != 1 || killed[0].WS != ws {
		t.Fatalf("kills = %+v, want the latest wait's stand-down", killed)
	}
}

func TestASectionExpandedDuringTheDelayKeepsItsSession(t *testing.T) {
	// Arrange
	f := newFoldedHarness(t)
	ws := f.collapsed(t)
	f.c.StandDownFolded(ws)
	f.awaitWait(t)
	record, err := f.db.Workspace(context.Background(), ws)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if err := f.db.SetRepositoryFolded(context.Background(), record.Repo, false); err != nil {
		t.Fatalf("SetRepositoryFolded: %v", err)
	}

	// Act
	f.settle(0)

	// Assert
	if killed := f.stand.Killed(); len(killed) != 0 {
		t.Fatalf("kills = %+v, want the expanded section's session kept", killed)
	}
}

func TestADaemonLeavingDuringTheDelayStandsNothingDown(t *testing.T) {
	// Arrange
	f := newFoldedHarness(t)
	ws := f.collapsed(t)
	f.c.StandDownFolded(ws)
	f.awaitWait(t)

	// Act
	f.cancel()
	f.detached.Wait()

	// Assert
	if killed := f.stand.Killed(); len(killed) != 0 {
		t.Fatalf("kills = %+v, want none once the daemon leaves", killed)
	}
	rec, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Level: "info", Operation: opFolded, Message: "the daemon is leaving; a folded workspace's stand-down is not taken"})
	if !ok || rec.Context["workspace"] != string(ws) {
		t.Fatalf("record = %+v, want the leave recorded with the workspace", rec)
	}
}

func TestACollapsedSubsectionsWorkspaceIsStoodDown(t *testing.T) {
	// Arrange: a workspace filed in a subsection the user collapsed.
	h := newHarness(t)
	ws := h.workspace(t, instant)
	h.inCollapsedSubsection(t, ws)

	// Act
	h.c.StandDownWithheld(ws)

	// Assert
	if killed := h.stand.Killed(); len(killed) != 1 || killed[0].WS != ws {
		t.Fatalf("kills = %+v, want the collapsed subsection's workspace stood down", killed)
	}
}

// inCollapsedSubsection files ws in a new subsection of its repository and
// collapses it.
func (h *harness) inCollapsedSubsection(t *testing.T, ws ids.WorkspaceID) {
	t.Helper()
	ctx := context.Background()
	record, err := h.db.Workspace(ctx, ws)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	sub, err := h.db.CreateSubsection(ctx, record.Repo, "folded")
	if err != nil {
		t.Fatalf("CreateSubsection: %v", err)
	}
	if err := h.db.PlaceWorkspaces(ctx, []ids.WorkspaceID{ws}, &sub.ID, wsm.AtEnd{}, wsm.ClearOneShotMarks); err != nil {
		t.Fatalf("PlaceWorkspaces: %v", err)
	}
	if err := h.db.SetSubsectionFolded(ctx, sub.ID, true); err != nil {
		t.Fatalf("SetSubsectionFolded: %v", err)
	}
}
