package worktreereap

import (
	"context"
	"errors"
	"testing"
)

// running is one schedule run on its own goroutine.
type running struct {
	cancel   context.CancelFunc
	finished chan struct{}
	err      error
}

// startRun runs the schedule and stops it when the test ends.
func startRun(t *testing.T, r *Reaper) *running {
	t.Helper()
	ctx, cancel := context.WithCancel(context.Background())
	run := &running{cancel: cancel, finished: make(chan struct{})}
	go func() {
		run.err = r.Run(ctx)
		close(run.finished)
	}()
	t.Cleanup(func() {
		cancel()
		<-run.finished
	})
	return run
}

// sweepsFinished counts the sweep summaries recorded.
func sweepsFinished(w *world) int {
	n := 0
	for _, r := range w.records("info", opSweep) {
		if r.Message == "the sweep finished" {
			n++
		}
	}
	return n
}

func TestTheFirstSweepWaitsTheStartDelay(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	r := w.reaper()

	// Act.
	startRun(t, r)

	// Assert.
	if got := <-w.clock.Asked; got != DefaultStartDelay {
		t.Fatalf("the first wait = %v, want the start delay %v", got, DefaultStartDelay)
	}
}

func TestTheStartDelaysExpirySweeps(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.addTree(repo, tree{name: "landed"})
	startRun(t, w.reaper())
	<-w.clock.Asked

	// Act.
	w.clock.Fire <- now
	<-w.clock.Asked // the next wait is asked for once the sweep is over

	// Assert.
	if !w.git.saw("remove " + dir) {
		t.Fatalf("git calls = %v, want the start sweep to have removed %s", w.git.called(), dir)
	}
}

func TestEverySweepAfterTheFirstWaitsTheCadence(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	startRun(t, w.reaper())
	<-w.clock.Asked

	// Act.
	w.clock.Fire <- now
	second := <-w.clock.Asked
	w.clock.Fire <- now
	third := <-w.clock.Asked

	// Assert.
	if second != DefaultEvery || third != DefaultEvery {
		t.Fatalf("the later waits = %v, %v, want the daily cadence %v", second, third, DefaultEvery)
	}
	if got := sweepsFinished(w); got != 2 {
		t.Fatalf("sweeps finished = %d, want 2", got)
	}
}

func TestAFailedSweepDoesNotEndTheSchedule(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	w.registry.reposErr = errScripted
	startRun(t, w.reaper())
	<-w.clock.Asked

	// Act.
	w.clock.Fire <- now
	next := <-w.clock.Asked

	// Assert.
	if next != DefaultEvery {
		t.Fatalf("the wait after a failed sweep = %v, want the cadence", next)
	}
}

func TestTheScheduleEndsWithItsContext(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	run := startRun(t, w.reaper())
	<-w.clock.Asked

	// Act.
	run.cancel()
	<-run.finished

	// Assert.
	if !errors.Is(run.err, context.Canceled) {
		t.Fatalf("Run = %v, want context.Canceled", run.err)
	}
}
