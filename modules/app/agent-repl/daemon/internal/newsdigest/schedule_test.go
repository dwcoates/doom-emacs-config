package newsdigest

import (
	"context"
	"errors"
	"testing"
	"time"

	"claude-repld/internal/flock"
)

// scheduled is one schedule run on its own goroutine.
type scheduled struct {
	cancel   context.CancelFunc
	finished chan struct{}
	err      error
}

// startSchedule runs d's schedule and stops it when the test ends.
func startSchedule(t *testing.T, d *Digester) *scheduled {
	t.Helper()
	ctx, cancel := context.WithCancel(context.Background())
	s := &scheduled{cancel: cancel, finished: make(chan struct{})}
	go func() {
		s.err = d.Run(ctx)
		close(s.finished)
	}()
	t.Cleanup(func() {
		cancel()
		<-s.finished
	})
	return s
}

// pastStartDelay starts the schedule and fires its start delay, answering
// the wait it asks for next.
func pastStartDelay(t *testing.T, w *world) time.Duration {
	t.Helper()
	startSchedule(t, w.digester())
	if got := <-w.clock.asked; got != DefaultStartDelay {
		t.Fatalf("the first wait = %v, want the start delay %v", got, DefaultStartDelay)
	}
	w.clock.fire <- now
	return <-w.clock.asked
}

func TestTheScheduleFirstWaitsTheStartDelay(t *testing.T) {
	// Arrange
	w := newWorld(t)

	// Act
	startSchedule(t, w.digester())

	// Assert
	if got := <-w.clock.asked; got != DefaultStartDelay {
		t.Fatalf("the first wait = %v, want %v", got, DefaultStartDelay)
	}
}

func TestAStoreNoRunWasRecordedInRunsOnceAtStart(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.fetcher.bodies[feedSource.URL] = atomWith(now, "a")
	w.fetcher.bodies[pageSource.URL] = "<p>x</p>"
	w.runner.text = `{"sections":[]}`

	// Act
	next := pastStartDelay(t, w)

	// Assert
	if runs := w.store.recorded(); len(runs) != 1 {
		t.Fatalf("runs = %d, want exactly one", len(runs))
	}
	if next != DefaultRecheck {
		t.Fatalf("the wait after the run = %v, want the recheck %v", next, DefaultRecheck)
	}
}

func TestAnOverdueRunHappensOnceNotOncePerMissedDay(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.store.state.LastRunEnd = now.Add(-72 * time.Hour)
	w.fetcher.bodies[feedSource.URL] = atomWith(now, "a")
	w.fetcher.bodies[pageSource.URL] = "<p>x</p>"
	w.runner.text = `{"sections":[]}`
	pastStartDelay(t, w)

	// Act
	w.clock.set(now.Add(DefaultRecheck))
	w.clock.fire <- now
	<-w.clock.asked

	// Assert
	if runs := w.store.recorded(); len(runs) != 1 {
		t.Fatalf("runs = %d, want one run for the three missed days", len(runs))
	}
}

func TestARunNotYetDueWaitsUntilItIs(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.store.state.LastRunEnd = now.Add(-DefaultEvery + 5*time.Minute)

	// Act
	next := pastStartDelay(t, w)

	// Assert
	if next != 5*time.Minute || len(w.store.recorded()) != 0 {
		t.Fatalf("wait = %v with %d runs, want 5m and no run", next, len(w.store.recorded()))
	}
}

func TestARunFarFromDueLooksAgainAfterTheRecheck(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.store.state.LastRunEnd = now.Add(-time.Hour)

	// Act
	next := pastStartDelay(t, w)

	// Assert
	if next != DefaultRecheck {
		t.Fatalf("wait = %v, want the recheck %v", next, DefaultRecheck)
	}
}

func TestADaemonThatDoesNotServeStartsNoRun(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.serves = false

	// Act
	next := pastStartDelay(t, w)

	// Assert
	if next != DefaultRecheck || len(w.store.recorded()) != 0 {
		t.Fatalf("wait = %v with %d runs, want the recheck and no run", next, len(w.store.recorded()))
	}
	if len(records(w.log, "info", opSchedule)) != 1 {
		t.Fatalf("records = %v, want the gate's one INFO", w.log.Records())
	}
}

func TestARunAnotherDaemonHoldsIsLookedAtAgainAfterTheRecheck(t *testing.T) {
	// Arrange
	w := newWorld(t)
	held, ok, err := flock.TryExclusive(w.lock)
	if err != nil || !ok {
		t.Fatalf("taking the lock: %v %v", ok, err)
	}
	t.Cleanup(func() { _ = held.Release() })

	// Act
	next := pastStartDelay(t, w)

	// Assert
	if next != DefaultRecheck || len(records(w.log, "error", opSchedule)) != 0 {
		t.Fatalf("wait = %v, records %v, want the recheck and no ERROR", next, w.log.Records())
	}
}

func TestARunAnotherDaemonJustFinishedIsNotRepeatedAndItsDigestIsDrawn(t *testing.T) {
	// Arrange: the schedule reads a due cadence; by the run's own read under
	// the lock, another daemon has run and left a digest standing.
	w := newWorld(t)
	overlay := encodedOverlay(t, "peer", 1)
	w.store.afterRead = func(s *fakeStore, n int) {
		if n == 1 {
			s.state.LastRunEnd = now
			s.state.LatestID = "peer"
			s.state.Standing = overlay
		}
	}
	d := w.digester()
	startSchedule(t, d)
	<-w.clock.asked

	// Act
	w.clock.fire <- now
	next := <-w.clock.asked

	// Assert
	if len(w.store.recorded()) != 0 {
		t.Fatal("a run another daemon just finished was repeated")
	}
	if got := latestStanding(t, d).GetShown().GetId().GetValue(); got != "peer" {
		t.Fatalf("standing = %q, want the peer's digest drawn", got)
	}
	if next != DefaultRecheck {
		t.Fatalf("wait = %v, want the recheck", next)
	}
}

func TestAnUnreadableStoreIsRecordedAndLookedAtAgainAfterTheRecheck(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.store.stateErr = errScripted

	// Act
	next := pastStartDelay(t, w)

	// Assert
	if next != DefaultRecheck || len(records(w.log, "error", opSchedule)) != 1 {
		t.Fatalf("wait = %v, records %v, want the recheck and one ERROR", next, w.log.Records())
	}
}

func TestAFailedRecordIsNotRetriedInABurst(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.withNewFeedEntry()
	w.runner.text = answerJSON
	w.store.recordErr = errScripted

	// Act
	next := pastStartDelay(t, w)

	// Assert
	if next != DefaultRecheck || len(records(w.log, "error", opSchedule)) != 1 {
		t.Fatalf("wait = %v, records %v, want the recheck and one ERROR", next, w.log.Records())
	}
}

func TestTheScheduleEndsWithItsContext(t *testing.T) {
	// Arrange
	w := newWorld(t)
	s := startSchedule(t, w.digester())
	<-w.clock.asked

	// Act
	s.cancel()
	<-s.finished

	// Assert
	if !errors.Is(s.err, context.Canceled) {
		t.Fatalf("Run = %v, want context.Canceled", s.err)
	}
}
