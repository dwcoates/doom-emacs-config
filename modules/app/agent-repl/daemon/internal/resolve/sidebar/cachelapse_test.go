package sidebar_test

import (
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/wsm"
)

// backloggedLapsing is the one workspace, hidden, its cache lapsing D after the
// epoch the resolver's clock stands at, gated when GATED is set.
func backloggedLapsing(d time.Duration, gated bool) wsm.Workspace {
	ws := backloggedWorkspace(string(theWS), "one")
	ws.CacheLapse = &wsm.CacheLapse{LapsesAt: epoch.Add(d)}
	if gated {
		tokens := uint64(120_000)
		ws.CacheLapse.GatedTokens = &tokens
	}
	return ws
}

// onlyBackloggedRow is the one row the hidden section carries.
func onlyBackloggedRow(t *testing.T, r sidebarResolver) *frontendv1.RosterRow {
	t.Helper()
	rows := topBacklog(latest(t, r))
	if len(rows) != 1 {
		t.Fatalf("the hidden section carried %d rows, want exactly 1", len(rows))
	}
	return rows[0]
}

func TestABackloggedRowCountsDownToItsCacheLapse(t *testing.T) {
	// Arrange, Act.
	r := arrange(t, backloggedLapsing(30*time.Minute, false))

	// Assert.
	backlogged := onlyBackloggedRow(t, r).GetWhen().GetBacklogged()
	if backlogged.GetCache() == nil || backlogged.GetCache().GetLapsesAtMs() != epoch.Add(30*time.Minute).UnixMilli() {
		t.Fatalf("when = %v, want the lapse 30 minutes on", backlogged)
	}
}

func TestABackloggedRowWithNoKnownLapseCountsUpFromItsHide(t *testing.T) {
	// Arrange, Act.
	r := arrange(t, backloggedWorkspace(string(theWS), "one"))

	// Assert.
	backlogged := onlyBackloggedRow(t, r).GetWhen().GetBacklogged()
	if backlogged.GetCache() != nil || backlogged.GetAtMs() != epoch.UnixMilli() {
		t.Fatalf("when = %v, want the hide instant and no lapse", backlogged)
	}
}

func TestABackloggedRowIsAColdGateOnlyOnceAGatedCacheHasLapsed(t *testing.T) {
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
			r := arrange(t, backloggedLapsing(tt.lapse, tt.gated))

			// Assert.
			if got := statusName(onlyBackloggedRow(t, r)) == "cold_gate"; got != tt.cold {
				t.Fatalf("cold_gate = %v, want %v (status %q)", got, tt.cold, statusName(onlyBackloggedRow(t, r)))
			}
		})
	}
}

func TestAnOpenInFlightOutranksTheBackloggedColdGate(t *testing.T) {
	// Arrange.
	r := arrange(t, backloggedLapsing(-time.Minute, true))

	// Act.
	r.SetOpening(theWS, true)

	// Assert.
	if got := statusName(onlyBackloggedRow(t, r)); got != "inactive" {
		t.Fatalf("status = %q, want inactive while the open is in flight", got)
	}
}
