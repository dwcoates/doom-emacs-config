package merge

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// pauseProduction pauses the production harness's repository, failing the
// test on error.
func pauseProduction(t *testing.T, h *harness) ProductionPaused {
	t.Helper()
	paused, err := h.o.PauseProduction(context.Background(), theScope)
	if err != nil {
		t.Fatalf("PauseProduction: %v", err)
	}
	return paused
}

// productionPaused answers whether the harness's repository stands paused.
func productionPaused(t *testing.T, h *harness) bool {
	t.Helper()
	pause, err := h.db.checks.ProductionPauseOf(context.Background(), mediationKey(h))
	if err != nil {
		t.Fatal(err)
	}
	return pause.Paused
}

func TestPausingProductionStopsTheMediatorAtOnce(t *testing.T) {
	// Arrange: the mediator was told of a due span and runs a turn.
	h := production(t)
	toldProduction(t, h)
	h.interruptRunning = true
	h.queue.supersedeRetired = true

	// Act
	paused := pauseProduction(t, h)

	// Assert
	t.Run("the told span is dropped and counted", func(t *testing.T) {
		if paused.Dropped != 1 {
			t.Fatalf("Dropped = %d, want 1", paused.Dropped)
		}
		if c, err := h.db.ProductionCheckOf(context.Background(), mediationKey(h)); err != nil || c != nil {
			t.Fatalf("ProductionCheckOf = (%+v, %v), want none", c, err)
		}
	})
	t.Run("the told span's brief is retired from the prompt queue", func(t *testing.T) {
		if !slices.Equal(h.queue.superseded, []ids.TurnID{"turn-told"}) {
			t.Fatalf("superseded = %v, want the told turn", h.queue.superseded)
		}
	})
	t.Run("the mediator's running turn is interrupted and named", func(t *testing.T) {
		if !slices.Equal(h.interrupted, []ids.WorkspaceID{"ws-fixer"}) || paused.Interrupted != "ws-fixer" {
			t.Fatalf("interrupted %v, answered %q; want ws-fixer", h.interrupted, paused.Interrupted)
		}
	})
	t.Run("the pause is recorded with what it stopped", func(t *testing.T) {
		rec, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opPauseProduction,
			Message: "paused the repository's production checks: its undecided spans are dropped and its mediator is told none"})
		if !ok || rec.Context["dropped_spans"] != 1 || rec.Context["interrupted"] != true || rec.Context["mediator"] != "ws-fixer" {
			t.Fatalf("records = %v, want the pause at INFO with its dropped span and interrupt", h.logs.Records())
		}
	})
	t.Run("the repository stands paused", func(t *testing.T) {
		if !productionPaused(t, h) {
			t.Fatal("the repository is not paused")
		}
	})
	t.Run("the dropped span is counted as left unreviewed by the pause", func(t *testing.T) {
		if !slices.Contains(h.meter.got, "production_check:paused:0") {
			t.Fatalf("metrics = %v, want the dropped span counted paused", h.meter.got)
		}
	})
}

func TestALandingDuringAPauseOwesNoSpanAndCountsItPaused(t *testing.T) {
	// Arrange
	h := production(t)
	pauseProduction(t, h)

	// Act
	deployEnds(t, h, wsm.DeployFinished)

	// Assert
	if c, err := h.db.ProductionCheckOf(context.Background(), mediationKey(h)); err != nil || c != nil {
		t.Fatalf("ProductionCheckOf = (%+v, %v), want none while paused", c, err)
	}
	if !slices.Contains(h.meter.got, "production_check:paused:0") {
		t.Fatalf("metrics = %v, want the skipped span counted paused", h.meter.got)
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opProduction,
		Message: "the landing closed a span while the production checks are paused; it is never reviewed"}); !ok {
		t.Fatalf("records = %v, want the skipped span recorded", h.logs.Records())
	}
}

func TestPausingProductionWithNoMediatorYetInterruptsNothing(t *testing.T) {
	// Arrange: a due span, and no production mediator made.
	h := production(t)
	deployEnds(t, h, wsm.DeployFinished)

	// Act
	paused := pauseProduction(t, h)

	// Assert
	if paused.Dropped != 1 || paused.Interrupted != "" || len(h.interrupted) != 0 || len(h.queue.superseded) != 0 {
		t.Fatalf("paused %+v, interrupted %v, superseded %v; want the span dropped and nothing interrupted", paused, h.interrupted, h.queue.superseded)
	}
}

func TestAPausedRepositoryTellsItsMediatorNoSpan(t *testing.T) {
	// Arrange: a span falls due, then the checks are paused.
	h := production(t)
	deployEnds(t, h, wsm.DeployFinished)
	pauseProduction(t, h)

	// Act
	h.o.Mediate(context.Background())

	// Assert
	if told := productionTellings(h); len(told) != 0 {
		t.Fatalf("production tellings = %+v, want none while paused", told)
	}
}

func TestAProductionPauseOrResumeOutOfTurnIsRefused(t *testing.T) {
	cases := []struct {
		name    string
		harness func(t *testing.T) *harness
		arrange func(t *testing.T, h *harness)
		act     func(h *harness) error
		arm     string
		// wantPaused is the repository's standing after the refusal.
		wantPaused bool
	}{
		{
			name: "a pause of paused checks", harness: production,
			arrange: func(t *testing.T, h *harness) { pauseProduction(t, h) },
			act: func(h *harness) error {
				_, err := h.o.PauseProduction(context.Background(), theScope)
				return err
			},
			arm: ArmAlreadyPaused, wantPaused: true,
		},
		{
			name: "a resume of checks that are not paused", harness: production,
			arrange: func(*testing.T, *harness) {},
			act:     func(h *harness) error { return h.o.ResumeProduction(context.Background(), theScope) },
			arm:     ArmNotPaused,
		},
		{
			name: "a pause of a repository that checks no production", harness: checked,
			arrange: func(*testing.T, *harness) {},
			act: func(h *harness) error {
				_, err := h.o.PauseProduction(context.Background(), theScope)
				return err
			},
			arm: ArmProductionChecksOff,
		},
		{
			name: "a resume of a repository that checks no production", harness: checked,
			arrange: func(*testing.T, *harness) {},
			act:     func(h *harness) error { return h.o.ResumeProduction(context.Background(), theScope) },
			arm:     ArmProductionChecksOff,
		},
		{
			name: "a pause of a repository the registry does not hold", harness: production,
			arrange: func(*testing.T, *harness) {},
			act: func(h *harness) error {
				_, err := h.o.PauseProduction(context.Background(), RepositoryScope{ID: "repo-unknown"})
				return err
			},
			arm: ArmUnknownRepository,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := tc.harness(t)
			tc.arrange(t, h)

			// Act
			err := tc.act(h)

			// Assert
			if refused, ok := Refused(err); !ok || refused.Arm != tc.arm {
				t.Fatalf("err = %v, want the %s refusal", err, tc.arm)
			}
			if productionPaused(t, h) != tc.wantPaused {
				t.Fatalf("paused = %v, want %v", !tc.wantPaused, tc.wantPaused)
			}
		})
	}
}

func TestResumingProductionLeavesThePauseAndIsRecorded(t *testing.T) {
	// Arrange
	h := production(t)
	pauseProduction(t, h)

	// Act
	err := h.o.ResumeProduction(context.Background(), theScope)

	// Assert
	if err != nil || productionPaused(t, h) {
		t.Fatalf("ResumeProduction = %v, paused %v; want resumed", err, productionPaused(t, h))
	}
	if _, ok := h.recordFor(dlog.LevelInfo, opResumeProduction); !ok {
		t.Fatalf("records = %v, want the resume at INFO", h.logs.Records())
	}
}

func TestAProductionPauseThatFailsSurfacesTheFailure(t *testing.T) {
	cause := errors.New("disk I/O error")
	cases := []struct {
		name    string
		arrange func(h *harness)
		act     func(h *harness) error
		op      string
		// wantPaused is whether the pause stands despite the failure.
		wantPaused bool
	}{
		{
			name:    "a pause the store cannot write pauses nothing",
			arrange: func(h *harness) { h.db.productionPauseErr = cause },
			act: func(h *harness) error {
				_, err := h.o.PauseProduction(context.Background(), theScope)
				return err
			},
			op: opPauseProduction,
		},
		{
			name:    "a resume the store cannot write leaves the checks paused",
			arrange: func(h *harness) { h.db.productionPauseErr = cause },
			act:     func(h *harness) error { return h.o.ResumeProduction(context.Background(), theScope) },
			op:      opResumeProduction,
		},
		{
			name:    "an interrupt that fails leaves the pause standing",
			arrange: func(h *harness) { h.interruptErr = cause },
			act: func(h *harness) error {
				_, err := h.o.PauseProduction(context.Background(), theScope)
				return err
			},
			op: opPauseProduction, wantPaused: true,
		},
		{
			name:    "a held brief that cannot be retired leaves the pause standing",
			arrange: func(h *harness) { h.queue.supersedeErr = cause },
			act: func(h *harness) error {
				_, err := h.o.PauseProduction(context.Background(), theScope)
				return err
			},
			op: opPauseProduction, wantPaused: true,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := production(t)
			toldProduction(t, h)
			if tc.op == opResumeProduction {
				pauseProduction(t, h)
			}
			tc.arrange(h)

			// Act
			err := tc.act(h)

			// Assert
			if !errors.Is(err, cause) {
				t.Fatalf("err = %v, want the cause surfaced", err)
			}
			rec, ok := h.recordFor(dlog.LevelError, tc.op)
			if !ok || !strings.Contains(fmt.Sprint(rec.Context), cause.Error()) || rec.Context["repo"] != string(mediationKey(h)) {
				t.Fatalf("records = %v, want an ERROR under %s carrying the repository and the cause", h.logs.Records(), tc.op)
			}
			h.db.productionPauseErr = nil
			if want := tc.wantPaused || tc.op == opResumeProduction; productionPaused(t, h) != want {
				t.Fatalf("paused = %v, want %v", !want, want)
			}
		})
	}
}

func TestAPausedMediatorDecidesAndFoldsNothing(t *testing.T) {
	cases := []struct {
		name string
		act  func(h *harness) error
	}{
		{name: "a decision is refused", act: func(h *harness) error {
			_, err := h.o.DecideProduction(context.Background(), "ws-fixer", twoChildren())
			return err
		}},
		{name: "a fold is refused", act: func(h *harness) error {
			return h.o.FoldProduction(context.Background(), "ws-fixer", "child-a", "child-b")
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := production(t)
			toldProduction(t, h)
			pauseProduction(t, h)

			// Act
			err := tc.act(h)

			// Assert
			if refused, ok := Refused(err); !ok || refused.Arm != ArmProductionPaused || len(h.children.made) != 0 {
				t.Fatalf("err = %v, made %v; want the paused refusal and nothing made", err, h.children.made)
			}
		})
	}
}

func TestAPauseReadThatFailsRefusesTheMediatorsAction(t *testing.T) {
	// Arrange
	h := production(t)
	toldProduction(t, h)
	cause := errors.New("disk I/O error")
	h.db.productionPauseErr = cause

	// Act
	_, err := h.o.DecideProduction(context.Background(), "ws-fixer", twoChildren())

	// Assert
	if !errors.Is(err, cause) || len(h.children.made) != 0 {
		t.Fatalf("err = %v, made %v; want the cause and nothing made", err, h.children.made)
	}
	if rec, ok := h.recordFor(dlog.LevelError, opProduction); !ok || rec.Context["action"] != "decide" {
		t.Fatalf("records = %v, want the failed read at ERROR naming the action", h.logs.Records())
	}
}

func TestAProductionTurnEndingUnderAPauseSaysItsSpanWasDropped(t *testing.T) {
	// Arrange: the told span's turn ends after a pause dropped it.
	h := production(t)
	toldProduction(t, h)
	q := productionQueueOf(t, h)
	told, err := h.db.ProductionCheckOf(context.Background(), mediationKey(h))
	if err != nil || told == nil {
		t.Fatalf("ProductionCheckOf = (%+v, %v)", told, err)
	}
	pauseProduction(t, h)

	// Act
	h.o.productionTurnEnded(context.Background(), q, "ws-fixer", *told, wsm.CloseKilled, dlog.Context{})

	// Assert
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opProduction,
		Message: "the production mediator's turn ended under a pause; its span was dropped, never reviewed"}); !ok {
		t.Fatalf("records = %v, want the turn's end recorded as under a pause", h.logs.Records())
	}
}

func TestTheProductionStandingSaysWhatTheChipDraws(t *testing.T) {
	cases := []struct {
		name    string
		harness func(t *testing.T) *harness
		arrange func(t *testing.T, h *harness)
		want    ProductionStanding
	}{
		{name: "a repository that checks no production is off", harness: checked, arrange: func(*testing.T, *harness) {}},
		{name: "checks with no mediator yet run", harness: production, arrange: func(*testing.T, *harness) {}, want: ProductionStanding{On: true}},
		{name: "paused checks name their mediator", harness: production, arrange: func(t *testing.T, h *harness) {
			toldProduction(t, h)
			pauseProduction(t, h)
		}, want: ProductionStanding{On: true, Paused: true, Mediator: "ws-fixer"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := tc.harness(t)
			tc.arrange(t, h)

			// Act
			got, err := h.o.ProductionStanding(context.Background(), theScope)

			// Assert
			if err != nil || got != tc.want {
				t.Fatalf("ProductionStanding = (%+v, %v), want %+v", got, err, tc.want)
			}
		})
	}
}

func TestAProductionStandingThatCannotBeReadIsAnError(t *testing.T) {
	// Arrange
	h := production(t)
	cause := errors.New("disk I/O error")
	h.db.productionPauseErr = cause

	// Act
	_, err := h.o.ProductionStanding(context.Background(), theScope)

	// Assert
	if !errors.Is(err, cause) {
		t.Fatalf("err = %v, want the cause", err)
	}
	if rec, ok := h.recordFor(dlog.LevelError, "daemon.merge.production_standing"); !ok || rec.Context["repo"] != string(mediationKey(h)) {
		t.Fatalf("records = %v, want the failed read at ERROR naming the repository", h.logs.Records())
	}
}
