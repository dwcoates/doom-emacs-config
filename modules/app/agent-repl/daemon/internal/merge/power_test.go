package merge

import (
	"context"
	"errors"
	"slices"
	"testing"
	"time"

	"agent-repld/internal/cpugov"
	"agent-repld/internal/dlog"
)

func TestOnBatteryOnlyTheHeadIsWorkedOn(t *testing.T) {
	// Arrange: the host is on battery; A holds in its gate with B behind it.
	c := newChain(t)
	c.h.power(cpugov.PowerBattery)
	headGate, releaseHead := c.runner.hold(c.dirs[theWorkspace])
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	done := admitAsync(c.h, context.Background())

	// Act.
	<-headGate
	joined := c.h.o.inChain(c.h.repoKey(), "ws-b")
	close(releaseHead)
	<-done
	c.drain(t)

	// Assert.
	if joined {
		t.Fatal("the merge behind the head was worked on while the host ran on battery")
	}
	if got := c.git.landedOwn(); len(got) != 2 || !slices.Equal(got[0], []string{"a1"}) || !slices.Equal(got[1], []string{"b1"}) {
		t.Fatalf("landings brought %v, want [a1] then [b1]", got)
	}
}

func TestPluggingInStartsTheMergeBehindTheHead(t *testing.T) {
	// Arrange: on battery, A holds in its gate with B waiting behind it.
	c := newChain(t)
	c.h.power(cpugov.PowerBattery)
	headGate, releaseHead := c.runner.hold(c.dirs[theWorkspace])
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	done := admitAsync(c.h, context.Background())
	<-headGate

	// Act.
	c.h.power(cpugov.PowerAC)

	// Assert: B joins the race and waits for the test turn A holds.
	c.awaitLined(t, "ws-b")
	close(releaseHead)
	<-done
	c.drain(t)
}

func TestUnpluggingSuspendsTheMergeBehindTheHeadUntilItHeadsTheQueue(t *testing.T) {
	// Arrange: on AC power, A holds in its gate and B races it from behind,
	// waiting for the test turn A holds.
	c := newChain(t)
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	releaseHead, done := c.headHoldsTheTurn(t, "ws-b")

	// Act.
	c.h.power(cpugov.PowerBattery)

	// Assert: B is suspended and taken back, to resume once it heads the
	// queue; A lands, then B.
	c.h.o.mu.Lock()
	_, resumes := c.h.o.resumes["ws-b"]
	c.h.o.mu.Unlock()
	if c.h.o.inChain(c.h.repoKey(), "ws-b") || !resumes {
		t.Fatalf("in chain=%v taken back=%v, want B suspended and taken back", c.h.o.inChain(c.h.repoKey(), "ws-b"), resumes)
	}
	if !c.recorded("info", "daemon.merge.power", "a merge beyond the battery window is suspended") {
		t.Fatal("the suspension was not recorded")
	}
	close(releaseHead)
	<-done
	c.drain(t)
	if got := c.git.landedOwn(); len(got) != 2 || !slices.Equal(got[0], []string{"a1"}) || !slices.Equal(got[1], []string{"b1"}) {
		t.Fatalf("landings brought %v, want [a1] then [b1]", got)
	}
}

func TestATakeBackThatFailsOnUnpluggingIsRecorded(t *testing.T) {
	// Arrange: on AC power, A holds in its gate and B waits behind it; the
	// store cannot answer the take-back's read.
	c := newChain(t)
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	releaseHead, done := c.headHoldsTheTurn(t, "ws-b")
	c.h.db.mu.Lock()
	c.h.db.allQueuesErr = errors.New("the store is closed")
	c.h.db.mu.Unlock()

	// Act.
	c.h.power(cpugov.PowerBattery)

	// Assert.
	rec, found := dlog.FindRecord(c.h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.merge.power",
		Message: "could not take back a merge suspended beyond the window"})
	if !found || rec.Context["workspace"] != "ws-b" || rec.Context["error"] == nil {
		t.Fatalf("error record = %v (found %v), want the workspace and the cause", rec.Context, found)
	}
	c.h.db.mu.Lock()
	c.h.db.allQueuesErr = nil
	c.h.db.mu.Unlock()
	if err := c.h.o.AdoptWorkspace(context.Background(), "ws-b"); err != nil {
		t.Fatalf("taking B back: %v", err)
	}
	close(releaseHead)
	<-done
	c.drain(t)
}

func TestASuspensionThatFailsOnUnpluggingIsRecorded(t *testing.T) {
	// Arrange: on AC power, master moved, so B rebases behind A; B is parked
	// in a rebase that will not let go, past the suspension's bounds.
	c := newChain(t)
	c.h.o.drainBound, c.h.o.gitBound = time.Millisecond, time.Millisecond
	c.git.commitOn("master", "m1")
	headGate, releaseHead := c.runner.hold(c.dirs[theWorkspace])
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	behindRebase, releaseBehind := c.git.holdRebaseStubbornly(c.dirs["ws-b"])
	done := admitAsync(c.h, context.Background())
	<-headGate
	<-behindRebase

	// Act.
	c.h.power(cpugov.PowerBattery)

	// Assert.
	rec, found := dlog.FindRecord(c.h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.merge.power",
		Message: "could not suspend a merge beyond the window"})
	if !found || rec.Context["workspace"] != "ws-b" || rec.Context["error"] == nil {
		t.Fatalf("error record = %v (found %v), want the workspace and the cause", rec.Context, found)
	}
	close(releaseBehind)
	if err := c.h.o.AdoptWorkspace(context.Background(), "ws-b"); err != nil {
		t.Fatalf("taking B back: %v", err)
	}
	close(releaseHead)
	<-done
	c.drain(t)
}

func TestDisablingRacingSuspendsTheMergeBehindTheHeadOnACPower(t *testing.T) {
	// Arrange: on AC power, A holds in its gate and B races it from behind,
	// waiting for the test turn A holds.
	c := newChain(t)
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	releaseHead, done := c.headHoldsTheTurn(t, "ws-b")

	// Act.
	err := c.h.o.DisableRacing(context.Background())

	// Assert: B is suspended for the operator's switch, not the battery, and
	// taken back; A lands, then B.
	if err != nil {
		t.Fatalf("DisableRacing: %v", err)
	}
	c.h.o.mu.Lock()
	_, resumes := c.h.o.resumes["ws-b"]
	c.h.o.mu.Unlock()
	if c.h.o.inChain(c.h.repoKey(), "ws-b") || !resumes {
		t.Fatalf("in chain=%v taken back=%v, want B suspended and taken back", c.h.o.inChain(c.h.repoKey(), "ws-b"), resumes)
	}
	if !c.recorded("info", "daemon.merge.power", "racing being disabled") {
		t.Fatal("the suspension was not recorded as the racing switch's")
	}
	if got := c.h.governor.Window(); got != cpugov.BatteryWindow {
		t.Fatalf("Window = %d after DisableRacing, want the head alone", got)
	}
	close(releaseHead)
	<-done
	c.drain(t)
	if got := c.git.landedOwn(); len(got) != 2 || !slices.Equal(got[0], []string{"a1"}) || !slices.Equal(got[1], []string{"b1"}) {
		t.Fatalf("landings brought %v, want [a1] then [b1]", got)
	}
}

func TestWithRacingDisabledOnlyTheHeadIsWorkedOnOnACPower(t *testing.T) {
	// Arrange: on AC power with racing disabled, A holds in its gate with B
	// behind it.
	c := newChain(t)
	if err := c.h.o.DisableRacing(context.Background()); err != nil {
		t.Fatalf("DisableRacing: %v", err)
	}
	headGate, releaseHead := c.runner.hold(c.dirs[theWorkspace])
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	done := admitAsync(c.h, context.Background())

	// Act.
	<-headGate
	joined := c.h.o.inChain(c.h.repoKey(), "ws-b")
	close(releaseHead)
	<-done
	c.drain(t)

	// Assert.
	if joined {
		t.Fatal("the merge behind the head raced it while racing was disabled")
	}
	if got := c.git.landedOwn(); len(got) != 2 || !slices.Equal(got[0], []string{"a1"}) || !slices.Equal(got[1], []string{"b1"}) {
		t.Fatalf("landings brought %v, want [a1] then [b1]", got)
	}
}

func TestEnablingRacingStartsTheMergeBehindTheHeadOnACPower(t *testing.T) {
	// Arrange: on AC power with racing disabled, A holds in its gate with B
	// waiting behind it.
	c := newChain(t)
	if err := c.h.o.DisableRacing(context.Background()); err != nil {
		t.Fatalf("DisableRacing: %v", err)
	}
	headGate, releaseHead := c.runner.hold(c.dirs[theWorkspace])
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	done := admitAsync(c.h, context.Background())
	<-headGate

	// Act.
	err := c.h.o.EnableRacing(context.Background())

	// Assert: B joins the race and waits for the test turn A holds.
	if err != nil {
		t.Fatalf("EnableRacing: %v", err)
	}
	c.awaitLined(t, "ws-b")
	close(releaseHead)
	<-done
	c.drain(t)
}

func TestEnablingRacingOnBatteryStillWorksTheHeadAlone(t *testing.T) {
	// Arrange: on battery with racing disabled, A holds in its gate with B
	// waiting behind it.
	c := newChain(t)
	c.h.power(cpugov.PowerBattery)
	if err := c.h.o.DisableRacing(context.Background()); err != nil {
		t.Fatalf("DisableRacing: %v", err)
	}
	headGate, releaseHead := c.runner.hold(c.dirs[theWorkspace])
	enqueue(t, c.h)
	c.queue(t, "ws-b", "b1")
	done := admitAsync(c.h, context.Background())
	<-headGate

	// Act.
	err := c.h.o.EnableRacing(context.Background())
	joined := c.h.o.inChain(c.h.repoKey(), "ws-b")

	// Assert.
	if err != nil {
		t.Fatalf("EnableRacing: %v", err)
	}
	if joined {
		t.Fatal("the merge behind the head raced it on battery")
	}
	close(releaseHead)
	<-done
	c.drain(t)
}
