package main

import (
	"context"
	"errors"
	"testing"
	"time"

	"agent-repld/internal/creategate"
	"agent-repld/internal/dlog"
)

// controlledAfter is a cadence the test fires: asked reports each arm, and
// fire delivers the armed tick.
type controlledAfter struct {
	asked chan time.Duration
	tick  chan time.Time
}

func newControlledAfter() *controlledAfter {
	return &controlledAfter{asked: make(chan time.Duration, 8), tick: make(chan time.Time, 1)}
}

func (c *controlledAfter) after(d time.Duration) <-chan time.Time {
	c.asked <- d
	return c.tick
}

func TestTheExitClosesTheCreateGateNamingNoSuccessor(t *testing.T) {
	// Arrange.
	gate := creategate.New()
	log := dlog.NewTestLogger()

	// Act.
	awaitCreatesAtExit(gate, time.Minute, newControlledAfter().after, log)()

	// Assert.
	_, _, err := gate.Admit(context.Background())
	var closed *creategate.ClosedError
	if !errors.As(err, &closed) || closed.Successor != "" {
		t.Fatalf("Admit after the exit's wait = %v, want refused naming no successor", err)
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Message: "closed the create gate with no create in flight"}); !ok {
		t.Fatalf("records = %+v, want the close recorded", log.Records())
	}
}

func TestTheExitWaitsForTheCreatesInFlight(t *testing.T) {
	// Arrange: one create in flight.
	gate := creategate.New()
	_, release, _ := gate.Admit(context.Background())
	cadence := newControlledAfter()
	log := dlog.NewTestLogger()
	done := make(chan struct{})
	go func() {
		awaitCreatesAtExit(gate, time.Minute, cadence.after, log)()
		close(done)
	}()
	<-cadence.asked

	// Act: the cadence fires once, then the create finishes.
	cadence.tick <- time.Time{}
	<-cadence.asked
	release()
	<-done

	// Assert.
	records := log.Records()
	if _, ok := dlog.FindRecord(records, dlog.RecordQuery{Level: "warn", Message: "the exit is still waiting for creates in flight to finish"}); !ok {
		t.Fatalf("records = %+v, want the holdout named on the cadence", records)
	}
	if _, ok := dlog.FindRecord(records, dlog.RecordQuery{Level: "info", Message: "every create in flight finished; the exit proceeds"}); !ok {
		t.Fatalf("records = %+v, want the wait's end recorded", records)
	}
}
