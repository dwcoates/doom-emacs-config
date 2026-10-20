package restarttiming_test

import (
	"testing"
	"time"

	"agent-repld/internal/restarttiming"
	"agent-repld/internal/restarttiming/restarttimingtest"
)

func TestAnExpectedReconnectIsTimedOnceFromItsRestart(t *testing.T) {
	// Arrange
	began := time.Date(2026, 10, 20, 12, 0, 0, 0, time.UTC)
	now := began.Add(3 * time.Second)
	var rec restarttimingtest.Recorder
	r := restarttiming.NewReconnects(&rec, func() time.Time { return now })
	r.Expect(restarttiming.ReconnectCold, began, restarttiming.ReconnectWeb, "w1", "w2")

	// Act
	r.Connected("w1", restarttiming.ReconnectWeb)
	r.Connected("w1", restarttiming.ReconnectWeb)

	// Assert: the second open of the same stream is no reconnect.
	got := rec.Of(restarttiming.WorkspaceReconnect)
	if len(got) != 1 || got[0].Workspace() != "w1" || got[0].Phase() != "web" || got[0].Cause() != "cold" || got[0].Seconds() != 3 {
		t.Fatalf("reconnects = %+v, want w1's web stream once, cold, 3s", got)
	}
}

func TestAConnectionNothingExpectedIsNotTimed(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(r *restarttiming.Reconnects, began time.Time)
	}{
		{"no restart expected anything", func(*restarttiming.Reconnects, time.Time) {}},
		{"another workspace was expected", func(r *restarttiming.Reconnects, began time.Time) {
			r.Expect(restarttiming.ReconnectCold, began, restarttiming.ReconnectWeb, "w2")
		}},
		{"another phase was expected", func(r *restarttiming.Reconnects, began time.Time) {
			r.Expect(restarttiming.ReconnectCold, began, restarttiming.ReconnectHost, "w1")
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			began := time.Date(2026, 10, 20, 12, 0, 0, 0, time.UTC)
			var rec restarttimingtest.Recorder
			r := restarttiming.NewReconnects(&rec, func() time.Time { return began.Add(time.Second) })
			tt.arrange(r, began)

			// Act
			r.Connected("w1", restarttiming.ReconnectWeb)

			// Assert
			if got := rec.Observations(); len(got) != 0 {
				t.Fatalf("observations = %+v, want none", got)
			}
		})
	}
}

func TestALaterExpectationReplacesAnEarlierOne(t *testing.T) {
	// Arrange
	boot := time.Date(2026, 10, 20, 12, 0, 0, 0, time.UTC)
	reload := boot.Add(time.Minute)
	var rec restarttimingtest.Recorder
	r := restarttiming.NewReconnects(&rec, func() time.Time { return reload.Add(2 * time.Second) })
	r.Expect(restarttiming.ReconnectCold, boot, restarttiming.ReconnectWeb, "w1")
	r.Expect(restarttiming.ReconnectWebappReload, reload, restarttiming.ReconnectWeb, "w1")

	// Act
	r.Connected("w1", restarttiming.ReconnectWeb)

	// Assert
	got := rec.Observations()
	if len(got) != 1 || got[0].Cause() != "webapp_reload" || got[0].Seconds() != 2 {
		t.Fatalf("reconnects = %+v, want the reload's, 2s", got)
	}
}

func TestAStartAfterNowIsStatedAsZero(t *testing.T) {
	// Arrange: a manifest stamped after this daemon's clock reads now.
	now := time.Date(2026, 10, 20, 12, 0, 0, 0, time.UTC)
	var rec restarttimingtest.Recorder
	r := restarttiming.NewReconnects(&rec, func() time.Time { return now })
	r.Expect(restarttiming.ReconnectSuccessor, now.Add(time.Second), restarttiming.ReconnectHost, "w1")

	// Act
	r.Connected("w1", restarttiming.ReconnectHost)

	// Assert
	if got := rec.Observations(); len(got) != 1 || got[0].Seconds() != 0 {
		t.Fatalf("reconnects = %+v, want one at zero", got)
	}
}
