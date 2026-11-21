package workspace

import (
	"context"
	"testing"

	"agent-repld/internal/ids"
)

func TestColdGateRaiseAnsweredAutomatically(t *testing.T) {
	tests := []struct {
		name  string
		raise ColdGateRaise
		want  bool
	}{
		{name: "a gate a shim judged just now is answered", raise: RaiseJudged, want: true},
		{name: "a standing gate raised again on this daemon is answered", raise: RaiseStanding, want: true},
		{name: "a gate an answer just failed on is never answered again", raise: RaiseAfterFailedAnswer, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := tt.raise.AnsweredAutomatically()

			// Assert.
			if got != tt.want {
				t.Fatalf("%v.AnsweredAutomatically() = %v, want %v", tt.raise, got, tt.want)
			}
		})
	}
}

func TestColdGateRaiseJudged(t *testing.T) {
	tests := []struct {
		raise ColdGateRaise
		want  bool
	}{
		{raise: RaiseJudged, want: true},
		{raise: RaiseStanding, want: false},
		{raise: RaiseAfterFailedAnswer, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.raise.String(), func(t *testing.T) {
			// Act, Assert.
			if got := tt.raise.Judged(); got != tt.want {
				t.Fatalf("%v.Judged() = %v, want %v", tt.raise, got, tt.want)
			}
		})
	}
}

func TestAColdGateRaiseThatNamesNoRaisePanics(t *testing.T) {
	// Arrange.
	defer func() {
		if recover() == nil {
			t.Fatal("AnsweredAutomatically on an unnamed raise did not panic")
		}
	}()

	// Act.
	ColdGateRaise(0).AnsweredAutomatically()
}

// TestEachColdGateRaiseTellsItsCause pins that every path that stands a gate
// names why it stands, so a raise after a failed answer is never read as one
// an automatic answerer answers.
func TestEachColdGateRaiseTellsItsCause(t *testing.T) {
	tests := []struct {
		name  string
		raise func(f *fleetFixture, ws ids.WorkspaceID)
		want  []ColdGateRaise
	}{
		{name: "a gate a shim judged is judged", raise: func(f *fleetFixture, ws ids.WorkspaceID) {
			f.fleet.raiseColdGate(ws, "vendor-1", coldResponse().GetFailure().GetCold(), "/config", RaiseJudged)
		}, want: []ColdGateRaise{RaiseJudged}},
		{name: "a gate raised from its record at a start is standing", raise: func(f *fleetFixture, ws ids.WorkspaceID) {
			f.db.gates[ws] = recordedGate()
			if err := f.fleet.Start(context.Background(), ws); err != nil {
				t.Fatalf("Start: %v", err)
			}
		}, want: []ColdGateRaise{RaiseStanding}},
		{name: "a gate stood again after a failed answer says so", raise: func(f *fleetFixture, ws ids.WorkspaceID) {
			f.fleet.raiseColdGate(ws, "vendor-1", coldResponse().GetFailure().GetCold(), "/config", RaiseJudged)
			f.fleet.TakeColdGate(ws, "vendor-1")
			if !f.fleet.ReraiseColdGate(ws, "vendor-1") {
				t.Fatal("ReraiseColdGate = false, want the gate standing again")
			}
		}, want: []ColdGateRaise{RaiseJudged, RaiseAfterFailedAnswer}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := resumable(f)
			var told []ColdGateRaise
			f.fleet.deps.ColdGateRaised = func(_ ids.WorkspaceID, raise ColdGateRaise) { told = append(told, raise) }

			// Act.
			tt.raise(f, ws)

			// Assert.
			if len(told) != len(tt.want) {
				t.Fatalf("raises told = %v, want %v", told, tt.want)
			}
			for i := range told {
				if told[i] != tt.want[i] {
					t.Fatalf("raises told = %v, want %v", told, tt.want)
				}
			}
		})
	}
}
