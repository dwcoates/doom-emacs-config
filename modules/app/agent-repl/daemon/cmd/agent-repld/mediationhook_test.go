package main

import (
	"context"
	"slices"
	"sync"
	"testing"
	"time"

	"agent-repld/internal/deployprogress"
	"agent-repld/internal/dlog"
	"agent-repld/internal/merge"
	"agent-repld/internal/wsm"
)

// mediationOrchestrator records the mediation calls it is given.
type mediationOrchestrator struct {
	merge.Orchestrator
	mu       sync.Mutex
	owed     []time.Time
	mediated chan struct{}
	// calls is every release and round, in order.
	calls []string
}

func (o *mediationOrchestrator) ReleaseUnchangedDeploy(_ context.Context, at time.Time) error {
	o.mu.Lock()
	defer o.mu.Unlock()
	o.calls = append(o.calls, "release "+at.UTC().Format(time.RFC3339))
	return nil
}

func (o *mediationOrchestrator) NoteDeployEnding(_ context.Context, at time.Time, ending wsm.DeployEnding) error {
	o.mu.Lock()
	defer o.mu.Unlock()
	o.calls = append(o.calls, "ending "+string(ending)+" "+at.UTC().Format(time.RFC3339))
	return nil
}

func (o *mediationOrchestrator) OweMediations(_ context.Context, at time.Time) error {
	o.mu.Lock()
	defer o.mu.Unlock()
	o.owed = append(o.owed, at)
	return nil
}

func (o *mediationOrchestrator) Mediate(context.Context) {
	o.mu.Lock()
	o.calls = append(o.calls, "mediate")
	o.mu.Unlock()
	close(o.mediated)
}

func TestAMediationHookCarriesBothEdgesOnceBound(t *testing.T) {
	// Arrange
	at := time.Date(2026, 11, 13, 21, 0, 0, 0, time.UTC)
	o := &mediationOrchestrator{mediated: make(chan struct{})}
	h := &mediationHook{ctx: context.Background(), now: func() time.Time { return at }, log: dlog.NewTestLogger()}
	h.bind(o)

	// Act
	h.began()
	h.ended(deployprogress.EndedFinished)

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

func TestADeployThatChangedNothingReleasesItsChecksBeforeTheRounds(t *testing.T) {
	at := time.Date(2026, 11, 13, 21, 0, 0, 0, time.UTC)
	tests := []struct {
		name      string
		began     bool
		unchanged bool
		want      []string
		wantError bool
	}{
		{name: "an unchanged deploy that began here", began: true, unchanged: true, want: []string{"ending unchanged 2026-11-13T21:00:00Z", "release 2026-11-13T21:00:00Z", "mediate"}},
		{name: "a deploy that changed something", began: true, want: []string{"ending finished 2026-11-13T21:00:00Z", "mediate"}},
		{name: "an unchanged deploy that never began here", unchanged: true, want: []string{"mediate"}, wantError: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			o := &mediationOrchestrator{mediated: make(chan struct{})}
			log := dlog.NewTestLogger()
			h := &mediationHook{ctx: context.Background(), now: func() time.Time { return at }, log: log}
			h.bind(o)
			if tt.began {
				h.began()
			}

			// Act
			ending := deployprogress.EndedFinished
			if tt.unchanged {
				ending = deployprogress.EndedUnchanged
			}
			h.ended(ending)

			// Assert
			<-o.mediated
			o.mu.Lock()
			calls := slices.Clone(o.calls)
			o.mu.Unlock()
			if !slices.Equal(calls, tt.want) {
				t.Fatalf("calls %v, want %v", calls, tt.want)
			}
			if _, logged := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: opMediationHook}); logged != tt.wantError {
				t.Fatalf("records %+v, want an ERROR %v", log.Records(), tt.wantError)
			}
		})
	}
}

func TestABootsRoundsReleaseNothing(t *testing.T) {
	// Arrange
	o := &mediationOrchestrator{mediated: make(chan struct{})}
	h := &mediationHook{ctx: context.Background(), now: time.Now, log: dlog.NewTestLogger()}
	h.bind(o)

	// Act
	h.rounds()

	// Assert
	<-o.mediated
	o.mu.Lock()
	defer o.mu.Unlock()
	if !slices.Equal(o.calls, []string{"mediate"}) {
		t.Fatalf("calls %v, want the rounds alone", o.calls)
	}
}

func TestADeployThatStoppedShortRecordsItsEndingBeforeTheRounds(t *testing.T) {
	// Arrange
	at := time.Date(2026, 11, 13, 21, 0, 0, 0, time.UTC)
	o := &mediationOrchestrator{mediated: make(chan struct{})}
	h := &mediationHook{ctx: context.Background(), now: func() time.Time { return at }, log: dlog.NewTestLogger()}
	h.bind(o)
	h.began()

	// Act
	h.ended(deployprogress.EndedStoppedShort)

	// Assert
	<-o.mediated
	o.mu.Lock()
	defer o.mu.Unlock()
	if want := []string{"ending stopped_short 2026-11-13T21:00:00Z", "mediate"}; !slices.Equal(o.calls, want) {
		t.Fatalf("calls %v, want %v", o.calls, want)
	}
}

func TestWsmEndingNamesEveryEnding(t *testing.T) {
	tests := []struct {
		ending deployprogress.Ending
		want   wsm.DeployEnding
	}{
		{deployprogress.EndedFinished, wsm.DeployFinished},
		{deployprogress.EndedUnchanged, wsm.DeployUnchanged},
		{deployprogress.EndedStoppedShort, wsm.DeployStoppedShort},
	}
	for _, tt := range tests {
		t.Run(string(tt.want), func(t *testing.T) {
			// Act / Assert
			if got := wsmEnding(tt.ending); got != tt.want {
				t.Fatalf("wsmEnding(%d) = %q, want %q", tt.ending, got, tt.want)
			}
		})
	}
}
