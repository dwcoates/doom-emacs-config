package main

import (
	"context"
	"errors"
	"reflect"
	"slices"
	"sync"
	"testing"
	"time"

	"agent-repld/internal/deployprogress"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/resolve/footer"
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
	// oweErr fails every OweMediations.
	oweErr error
	// redeploys is how many checks were owed at each DeployUncheckedLandings.
	redeploys []int
}

func (o *mediationOrchestrator) DeployUncheckedLandings(context.Context) error {
	o.mu.Lock()
	defer o.mu.Unlock()
	o.redeploys = append(o.redeploys, len(o.owed))
	return nil
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

func (o *mediationOrchestrator) NoteTakenOverDeployEnding(_ context.Context, ending wsm.DeployEnding) error {
	o.mu.Lock()
	defer o.mu.Unlock()
	o.calls = append(o.calls, "taken-over ending "+string(ending))
	return nil
}

func (o *mediationOrchestrator) OweMediations(_ context.Context, at time.Time) error {
	o.mu.Lock()
	defer o.mu.Unlock()
	if o.oweErr != nil {
		return o.oweErr
	}
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
	settleThenMediate(func(context.Context) { settled = true }, h, &closesOwed{})(context.Background())

	// Assert
	<-o.mediated
	if !settled {
		t.Fatal("the boot's settle did not run before the mediation")
	}
	if len(o.owed) != 0 {
		t.Fatalf("owed %v; a boot whose predecessor withdrew owes no check", o.owed)
	}
	if settleThenMediate(nil, h, &closesOwed{}) != nil {
		t.Fatal("a successor, which settles nothing here, was given a settle")
	}
}

// closesOwed records each judgment of every standing close request owed.
type closesOwed struct{ owed int }

func (c *closesOwed) ReconsiderAll() { c.owed++ }

func TestAFreshBootJudgesEveryStandingCloseRequestOnceItSettled(t *testing.T) {
	// Arrange
	o := &mediationOrchestrator{mediated: make(chan struct{})}
	h := &mediationHook{ctx: context.Background(), now: time.Now, log: dlog.NewTestLogger()}
	h.bind(o)
	closes := &closesOwed{}
	settledFirst := false

	// Act
	settleThenMediate(func(context.Context) { settledFirst = closes.owed == 0 }, h, closes)(context.Background())

	// Assert
	<-o.mediated
	if !settledFirst || closes.owed != 1 {
		t.Fatalf("settled before the judgment = %v, judgments owed = %d; want the settle first and one judgment", settledFirst, closes.owed)
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

func TestADeploysEndingIsRecordedByWhereItBegan(t *testing.T) {
	at := time.Date(2026, 11, 13, 21, 0, 0, 0, time.UTC)
	tests := []struct {
		name   string
		began  bool
		ending deployprogress.Ending
		want   []string
	}{
		{name: "a deploy that began here is named by its start", began: true, ending: deployprogress.EndedFinished, want: []string{"ending finished 2026-11-13T21:00:00Z", "mediate"}},
		{name: "a finished deploy taken over is recorded as taken over", ending: deployprogress.EndedFinished, want: []string{"taken-over ending finished", "mediate"}},
		{name: "a deploy taken over that stopped short is recorded as taken over", ending: deployprogress.EndedStoppedShort, want: []string{"taken-over ending stopped_short", "mediate"}},
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
			h.ended(tt.ending)

			// Assert
			<-o.mediated
			o.mu.Lock()
			calls := slices.Clone(o.calls)
			o.mu.Unlock()
			if !slices.Equal(calls, tt.want) {
				t.Fatalf("calls %v, want %v", calls, tt.want)
			}
			if _, logged := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: opMediationHook}); logged {
				t.Fatalf("records %+v, want no ERROR", log.Records())
			}
		})
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

// fixerTurns is a mediationOrchestrator that records the turn ends it is told.
type fixerTurns struct {
	mediationOrchestrator
	ended chan ids.WorkspaceID
}

func (o *fixerTurns) FixerTurnEnded(_ context.Context, ws ids.WorkspaceID) { o.ended <- ws }

func TestATurnsEndReachesTheBoundOrchestrator(t *testing.T) {
	// Arrange
	o := &fixerTurns{ended: make(chan ids.WorkspaceID, 1)}
	h := &mediationHook{ctx: context.Background(), now: time.Now, log: dlog.NewTestLogger()}
	h.bind(o)

	// Act
	h.turnEnded("ws-fixer")

	// Assert
	if got := <-o.ended; got != "ws-fixer" {
		t.Fatalf("told %q, want ws-fixer's turn end", got)
	}
}

func TestATurnsEndBeforeTheBindingIsNoError(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	h := &mediationHook{ctx: context.Background(), now: time.Now, log: log}

	// Act
	h.turnEnded("ws-fixer")

	// Assert: the queue's first pass reads every turn's close.
	if records := log.Records(); len(records) != 0 {
		t.Fatalf("records = %+v, want none", records)
	}
}

// spentTurns is a mediationOrchestrator that records the turn spends it is
// told.
type spentTurns struct {
	mediationOrchestrator
	spent chan footer.TurnSpent
}

func (o *spentTurns) MediatorTurnSpent(_ context.Context, _ ids.WorkspaceID, spent footer.TurnSpent) {
	o.spent <- spent
}

func TestATurnsSpendReachesTheBoundOrchestrator(t *testing.T) {
	// Arrange
	o := &spentTurns{spent: make(chan footer.TurnSpent, 1)}
	h := &mediationHook{ctx: context.Background(), now: time.Now, log: dlog.NewTestLogger()}
	h.bind(o)
	want := footer.TurnSpent{Turn: "turn-1", FreshInput: 10, CacheRead: 20, Output: 30}

	// Act
	h.turnSpent("ws-fixer", want)

	// Assert
	if got := <-o.spent; got != want {
		t.Fatalf("told %+v, want %+v", got, want)
	}
}

func TestATurnsSpendBeforeTheBindingIsRecordedUnmetered(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	h := &mediationHook{ctx: context.Background(), now: time.Now, log: log}

	// Act
	h.turnSpent("ws-fixer", footer.TurnSpent{Turn: "turn-1"})

	// Assert
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: opMediationHook,
		Message: "a turn ended before the merge queue was built; its spend is not metered"}); !ok {
		t.Fatalf("records = %+v, want the unmetered turn recorded", log.Records())
	}
}

func TestTheHooksOrchestratorIsUnboundUntilBound(t *testing.T) {
	// Arrange
	h := &mediationHook{ctx: context.Background(), now: time.Now, log: dlog.NewTestLogger()}
	o := &mediationOrchestrator{}
	before := h.orchestrator()

	// Act
	h.bind(o)

	// Assert
	if before != nil || h.orchestrator() != o {
		t.Fatalf("orchestrator = %v before and %v after the binding, want nil then the bound one", before, h.orchestrator())
	}
}

// A BOOT AFTER A CRASH OWES THE CHECK OF IT: the death the boot reaped is
// owed as a check whose ending is the crash when its serving settles, inside
// the settle (the rollout's SettleBoot tells ServingSettled), before the
// boot's rounds run it.
func TestABootAfterACrashOwesTheCheckOfItBeforeItsRounds(t *testing.T) {
	// Arrange
	at := time.Date(2026, 11, 21, 20, 22, 30, 0, time.UTC)
	o := &mediationOrchestrator{mediated: make(chan struct{})}
	h := &mediationHook{ctx: context.Background(), now: func() time.Time { return at }, log: dlog.NewTestLogger()}
	h.bind(o)
	h.deathReaped()

	// Act
	settleThenMediate(func(context.Context) { h.servingSettled() }, h, &closesOwed{})(context.Background())

	// Assert
	<-o.mediated
	o.mu.Lock()
	defer o.mu.Unlock()
	want := []string{"ending crashed 2026-11-21T20:22:30Z", "mediate"}
	if len(o.owed) != 1 || !o.owed[0].Equal(at) || !reflect.DeepEqual(o.calls, want) {
		t.Fatalf("owed %v, calls %v; want the crash owed at the boot and its ending recorded before the round", o.owed, o.calls)
	}
}

func TestACrashsCheckThatCannotBeOwedRecordsNoEnding(t *testing.T) {
	// Arrange
	o := &mediationOrchestrator{mediated: make(chan struct{}), oweErr: errors.New("database is locked")}
	h := &mediationHook{ctx: context.Background(), now: time.Now, log: dlog.NewTestLogger()}
	h.bind(o)

	// Act
	h.crashed()

	// Assert
	o.mu.Lock()
	defer o.mu.Unlock()
	if len(o.calls) != 0 {
		t.Fatalf("calls %v; want no ending for a check never owed", o.calls)
	}
}

func TestACrashBeforeTheBindingIsRecordedAtError(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	h := &mediationHook{ctx: context.Background(), now: time.Now, log: log}

	// Act
	h.crashed()

	// Assert
	if rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: opMediationHook}); !ok || rec.Context["edge"] != "crashed" {
		t.Fatalf("no ERROR record of the unbound crash: %+v", log.Records())
	}
}

// A DEATH REAPED BEFORE THE SERVING SETTLED IS NOT OWED YET: a joining
// successor's state handle is read-only until its takeover, so the check
// waits for the settle.
func TestADeathReapedBeforeTheSettleIsOwedOnlyAtIt(t *testing.T) {
	// Arrange
	o := &mediationOrchestrator{mediated: make(chan struct{})}
	h := &mediationHook{ctx: context.Background(), now: time.Now, log: dlog.NewTestLogger()}
	h.bind(o)

	// Act
	h.deathReaped()
	o.mu.Lock()
	before := len(o.owed)
	o.mu.Unlock()
	owed := h.servingSettled()

	// Assert
	o.mu.Lock()
	defer o.mu.Unlock()
	if before != 0 || !owed || len(o.owed) != 1 {
		t.Fatalf("owed %d before the settle and %d after (settle answered %v), want none, then one", before, len(o.owed), owed)
	}
}

func TestASettleWithNoDeathReapedOwesNothing(t *testing.T) {
	// Arrange
	o := &mediationOrchestrator{mediated: make(chan struct{})}
	h := &mediationHook{ctx: context.Background(), now: time.Now, log: dlog.NewTestLogger()}
	h.bind(o)

	// Act
	owed := h.servingSettled()

	// Assert
	o.mu.Lock()
	defer o.mu.Unlock()
	if owed || len(o.owed) != 0 || len(o.calls) != 0 {
		t.Fatalf("settle answered %v, owed %v, calls %v; want nothing owed", owed, o.owed, o.calls)
	}
}

// A DEATH REAPED WHILE THIS DAEMON SERVES (an elder that died while this
// daemon awaited it) is owed at once, its ending recorded, and its round run.
func TestADeathReapedAfterTheSettleIsOwedAtOnceWithItsRound(t *testing.T) {
	// Arrange
	at := time.Date(2026, 11, 21, 21, 0, 0, 0, time.UTC)
	o := &mediationOrchestrator{mediated: make(chan struct{})}
	h := &mediationHook{ctx: context.Background(), now: func() time.Time { return at }, log: dlog.NewTestLogger()}
	h.bind(o)
	h.servingSettled()

	// Act
	h.deathReaped()

	// Assert
	<-o.mediated
	o.mu.Lock()
	defer o.mu.Unlock()
	want := []string{"ending crashed 2026-11-21T21:00:00Z", "mediate"}
	if len(o.owed) != 1 || !o.owed[0].Equal(at) || !reflect.DeepEqual(o.calls, want) {
		t.Fatalf("owed %v, calls %v; want the death owed at once and its round run", o.owed, o.calls)
	}
}

func TestTheSettleDeploysTheUncheckedLandingsAfterOwingADeathsCheck(t *testing.T) {
	tests := []struct {
		name          string
		deathReaped   bool
		wantRedeploys []int
	}{
		{name: "a settle with no death asks once", wantRedeploys: []int{0}},
		{name: "a settle owing a death asks once the death's check is owed", deathReaped: true, wantRedeploys: []int{1}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			o := &mediationOrchestrator{mediated: make(chan struct{})}
			h := &mediationHook{ctx: context.Background(), now: time.Now, log: dlog.NewTestLogger()}
			h.bind(o)
			if tt.deathReaped {
				h.deathReaped()
			}

			// Act
			h.servingSettled()

			// Assert
			o.mu.Lock()
			defer o.mu.Unlock()
			if !reflect.DeepEqual(o.redeploys, tt.wantRedeploys) {
				t.Fatalf("redeploys asked with %v checks owed, want %v", o.redeploys, tt.wantRedeploys)
			}
		})
	}
}

func TestASettleBeforeTheBindingIsRecordedAtError(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	h := &mediationHook{ctx: context.Background(), now: time.Now, log: log}

	// Act
	h.servingSettled()

	// Assert
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: opMediationHook,
		Match: func(r dlog.Record) bool { return r.Context["edge"] == "serving_settled" }}); !ok {
		t.Fatalf("no ERROR record of the unbound settle: %+v", log.Records())
	}
}
