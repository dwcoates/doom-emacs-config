package sidebar_test

import (
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/wsm"
)

// hiddenLapsing is the one workspace, hidden, its cache lapsing D after the
// epoch the resolver's clock stands at, gated when GATED is set.
func hiddenLapsing(d time.Duration, gated bool) wsm.Workspace {
	ws := hiddenWorkspace(string(theWS), "one")
	ws.CacheLapse = &wsm.CacheLapse{LapsesAt: epoch.Add(d)}
	if gated {
		tokens := uint64(120_000)
		ws.CacheLapse.GatedTokens = &tokens
	}
	return ws
}

// onlyHiddenRow is the one row the hidden section carries.
func onlyHiddenRow(t *testing.T, r sidebarResolver) *frontendv1.RosterRow {
	t.Helper()
	rows := latest(t, r).GetHidden().GetRows().GetRows()
	if len(rows) != 1 {
		t.Fatalf("the hidden section carried %d rows, want exactly 1", len(rows))
	}
	return rows[0]
}

func TestAHiddenRowCountsDownToItsCacheLapse(t *testing.T) {
	// Arrange, Act.
	r := arrange(t, hiddenLapsing(30*time.Minute, false))

	// Assert.
	hidden := onlyHiddenRow(t, r).GetWhen().GetHidden()
	if hidden.GetCache() == nil || hidden.GetCache().GetLapsesAtMs() != epoch.Add(30*time.Minute).UnixMilli() {
		t.Fatalf("when = %v, want the lapse 30 minutes on", hidden)
	}
}

func TestAHiddenRowWithNoKnownLapseCountsUpFromItsHide(t *testing.T) {
	// Arrange, Act.
	r := arrange(t, hiddenWorkspace(string(theWS), "one"))

	// Assert.
	hidden := onlyHiddenRow(t, r).GetWhen().GetHidden()
	if hidden.GetCache() != nil || hidden.GetAtMs() != epoch.UnixMilli() {
		t.Fatalf("when = %v, want the hide instant and no lapse", hidden)
	}
}

func TestAHiddenRowIsAColdGateOnlyOnceAGatedCacheHasLapsed(t *testing.T) {
	tests := []struct {
		name  string
		lapse time.Duration
		gated bool
		cold  bool
	}{
		{name: "a gated cache that has lapsed", lapse: -time.Minute, gated: true, cold: true},
		{name: "a gated cache lapsing at this instant", lapse: 0, gated: true, cold: true},
		{name: "a gated cache still warm", lapse: time.Minute, gated: true, cold: false},
		{name: "an ungated cache that has lapsed", lapse: -time.Minute, gated: false, cold: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange, Act.
			r := arrange(t, hiddenLapsing(tt.lapse, tt.gated))

			// Assert.
			if got := statusName(onlyHiddenRow(t, r)) == "cold_gate"; got != tt.cold {
				t.Fatalf("cold_gate = %v, want %v (status %q)", got, tt.cold, statusName(onlyHiddenRow(t, r)))
			}
		})
	}
}

func TestAnOpenInFlightOutranksTheHiddenColdGate(t *testing.T) {
	// Arrange.
	r := arrange(t, hiddenLapsing(-time.Minute, true))

	// Act.
	r.SetOpening(theWS, true)

	// Assert.
	if got := statusName(onlyHiddenRow(t, r)); got != "inactive" {
		t.Fatalf("status = %q, want inactive while the open is in flight", got)
	}
}
