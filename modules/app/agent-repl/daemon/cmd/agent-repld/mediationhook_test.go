package main

import (
	"context"
	"sync"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/merge"
)

// mediationOrchestrator records the mediation calls it is given.
type mediationOrchestrator struct {
	merge.Orchestrator
	mu       sync.Mutex
	owed     []time.Time
	mediated chan struct{}
}

func (o *mediationOrchestrator) OweMediations(_ context.Context, at time.Time) error {
	o.mu.Lock()
	defer o.mu.Unlock()
	o.owed = append(o.owed, at)
	return nil
}

func (o *mediationOrchestrator) Mediate(context.Context) { close(o.mediated) }

func TestAMediationHookCarriesBothEdgesOnceBound(t *testing.T) {
	// Arrange
	at := time.Date(2026, 11, 13, 21, 0, 0, 0, time.UTC)
	o := &mediationOrchestrator{mediated: make(chan struct{})}
	h := &mediationHook{ctx: context.Background(), now: func() time.Time { return at }, log: dlog.NewTestLogger()}
	h.bind(o)

	// Act
	h.began()
	h.ended()

	// Assert
	<-o.mediated
	o.mu.Lock()
	defer o.mu.Unlock()
	if len(o.owed) != 1 || !o.owed[0].Equal(at) {
		t.Fatalf("owed %v, want the deploy's start owed once", o.owed)
	}
}

func TestAnEdgeBeforeTheBindingIsRecordedAtError(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	h := &mediationHook{ctx: context.Background(), now: time.Now, log: log}

	// Act
	h.began()

	// Assert
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: opMediationHook}); !ok {
		t.Fatalf("no ERROR record of the unbound edge: %+v", log.Records())
	}
}

func TestSettleThenMediate(t *testing.T) {
	// Arrange
	o := &mediationOrchestrator{mediated: make(chan struct{})}
	h := &mediationHook{ctx: context.Background(), now: time.Now, log: dlog.NewTestLogger()}
	h.bind(o)
	settled := false

	// Act
	settleThenMediate(func(context.Context) { settled = true }, h)(context.Background())

	// Assert
	<-o.mediated
	if !settled {
		t.Fatal("the boot's settle did not run before the mediation")
	}
	if settleThenMediate(nil, h) != nil {
		t.Fatal("a successor, which settles nothing here, was given a settle")
	}
}
