package merge

import (
	"math/rand/v2"
	"testing"

	"agent-repld/internal/wsm"
)

func TestViewOfDrawsALiveRoundByWhatHoldsIt(t *testing.T) {
	held := at(15)
	tests := []struct {
		name      string
		round     wsm.TabInterval
		queueHeld bool
		want      stageState
		wantHeld  int64
	}{
		{name: "a live round of a running queue runs", round: wsm.TabInterval{Round: 1, Kind: TabTests, StartedAt: at(10)}, want: stageLive},
		{name: "a live round its merge is suspended at is held since then", round: wsm.TabInterval{Round: 1, Kind: TabTests, StartedAt: at(10), HeldSince: &held}, want: stageHeld, wantHeld: 15},
		{name: "a live round of a held queue is held", round: wsm.TabInterval{Round: 1, Kind: TabTests, StartedAt: at(10)}, queueHeld: true, want: stageHeld},
		{name: "a settled round of a held queue stays settled", round: wsm.TabInterval{Round: 1, Kind: TabTests, StartedAt: at(10), EndedAt: endedAt(12), Outcome: "succeeded"}, queueHeld: true, want: stageSucceeded},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			view, err := viewOf([]wsm.TabInterval{tt.round}, tt.queueHeld)

			// Assert
			if err != nil || len(view.stages) != 1 || view.stages[0].state != tt.want || view.stages[0].heldMS != tt.wantHeld {
				t.Fatalf("viewOf = (%+v, %v), want one stage %v held at %d", view, err, tt.want, tt.wantHeld)
			}
		})
	}
}

func TestViewOfRefusesWhatTheLedgerHasNoBusinessHolding(t *testing.T) {
	tests := []struct {
		name  string
		round wsm.TabInterval
	}{
		{name: "an unknown kind", round: wsm.TabInterval{Round: 1, Kind: "napping", StartedAt: at(10)}},
		{name: "an unknown outcome", round: wsm.TabInterval{Round: 1, Kind: TabTests, StartedAt: at(10), EndedAt: endedAt(11), Outcome: "shrugged"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, err := viewOf([]wsm.TabInterval{tt.round}, false)

			// Assert
			if err == nil {
				t.Fatal("viewOf accepted a round the ledger has no business holding")
			}
		})
	}
}

// generatedLedger is a ledger of random rounds, each kind and outcome one the
// ledger may hold, at most one of them live (wsm.RecordTabInterval keeps one
// live round per ledger), the live one held or not.
func generatedLedger(rng *rand.Rand) []wsm.TabInterval {
	kinds := []string{TabQueue, TabRebasing, TabConflicts, TabTests, TabFixes, TabCommitting, TabDeploy}
	outcomes := []string{"succeeded", "failed", wsm.TabOutcomeRestarted, "abandoned"}
	n := rng.IntN(8)
	rounds := make([]wsm.TabInterval, 0, n)
	start := int64(1_000)
	for i := range n {
		start += int64(rng.IntN(100) + 1)
		r := wsm.TabInterval{Round: rng.IntN(3) + 1, Kind: kinds[rng.IntN(len(kinds))], StartedAt: at(start)}
		if i < n-1 || rng.IntN(2) == 0 {
			ended := at(start + int64(rng.IntN(50)))
			r.EndedAt, r.Outcome = &ended, outcomes[rng.IntN(len(outcomes))]
		} else if rng.IntN(2) == 0 {
			held := at(start + 5)
			r.HeldSince = &held
		}
		rounds = append(rounds, r)
	}
	return rounds
}

// THE VIEW IS ONE STATE (owner requirement, 2026-11-16): under a held queue
// no generated ledger renders a stage live, and the roster's rendering of the
// view draws exactly the states the view holds.
func TestNoGeneratedLedgerRendersALiveRoundUnderAHeldQueue(t *testing.T) {
	rng := rand.New(rand.NewPCG(1, 2))
	for i := range 2000 {
		// Arrange
		rounds := generatedLedger(rng)
		queueHeld := rng.IntN(2) == 0

		// Act
		view, err := viewOf(rounds, queueHeld)
		if err != nil {
			t.Fatalf("case %d: viewOf(%+v) = %v", i, rounds, err)
		}
		drawn := rosterStages(view)

		// Assert
		if queueHeld && view.live() {
			t.Fatalf("case %d: a held queue's view %+v has a live stage", i, view)
		}
		for j, st := range view.stages {
			if got, want := shapeOf(drawn[j]).state, stateName(st.state); got != want {
				t.Fatalf("case %d: the roster drew stage %d %s, the view holds it %s", i, j, got, want)
			}
		}
	}
}

// stateName is a view state as shapeOf names a roster stage's arm.
func stateName(s stageState) string {
	switch s {
	case stageLive:
		return "live"
	case stageHeld:
		return "held"
	case stageSucceeded:
		return "succeeded"
	case stageFailed:
		return "failed"
	}
	return "unset"
}
