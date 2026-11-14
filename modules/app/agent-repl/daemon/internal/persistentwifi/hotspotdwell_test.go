package persistentwifi

import (
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// onHotspotClosed is a harness standing joined, the lid closed into the
// quiet state with the mode on, the dwell watching the hotspot from the
// epoch.
func onHotspotClosed(t *testing.T, saved string) *harness {
	t.Helper()
	h := newHarness(t).standing(pmsetOn, summary(true, "")).withLid(lidClosed)
	h.c.quiet = true
	h.c.hotspot = hotspotDwell{on: true}
	h.r.on(argPreferred, answer{out: saved})
	return h
}

func TestTheHotspotIsLeftForAKnownNetworkOnlyOnceItStayedInSightAMinute(t *testing.T) {
	cases := []struct {
		name     string
		at       time.Duration
		wantJoin bool
	}{
		{name: "in sight for less than a minute", at: knownDwell - hotspotCheckEvery},
		{name: "in sight for a minute", at: knownDwell, wantJoin: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: on the hotspot; Home comes into sight at the first check.
			h := onHotspotClosed(t, preferred("Home", hotspot))
			h.sight(inSight(hotspot, "Home", hotspot))
			h.r.on(argJoinNamed("Home"), answer{})
			h.tickAt(0)

			// Act
			for d := hotspotCheckEvery; d <= tc.at; d += hotspotCheckEvery {
				h.tickAt(d)
			}

			// Assert
			if got := h.r.count(argJoinNamed("Home")) == 1; got != tc.wantJoin {
				t.Fatalf("Home joined = %t, want %t (calls %v)", got, tc.wantJoin, h.joinCalls())
			}
			if tc.wantJoin && h.c.hotspot.on {
				t.Fatal("the dwell still watches after the hotspot was left")
			}
		})
	}
}

func TestAKnownNetworkThatLeavesSightStartsItsMinuteOver(t *testing.T) {
	// Arrange: Home in sight, out of sight at one check, then back.
	h := onHotspotClosed(t, preferred("Home", hotspot))
	h.sight(inSight(hotspot, "Home"), inSight(hotspot, "Home"), inSight(hotspot), inSight(hotspot, "Home"))
	h.r.on(argJoinNamed("Home"), answer{})

	// Act: checks at 0, 15s, 30s (out of sight) and on to a minute past the
	// first sighting.
	for d := time.Duration(0); d <= knownDwell; d += hotspotCheckEvery {
		h.tickAt(d)
	}

	// Assert
	if n := h.r.count(argJoinNamed("Home")); n != 0 {
		t.Fatalf("Home joined %d times, want none: its minute started over when it left sight", n)
	}
}

func TestTheDwellNeverJoinsAnUnknownNetworkOrOneOutOfSight(t *testing.T) {
	// Arrange: Stranger in sight, Home known but never in sight.
	h := onHotspotClosed(t, preferred("Home", hotspot))
	h.sight(inSight(hotspot, "Stranger"))

	// Act
	for d := time.Duration(0); d <= 3*knownDwell; d += hotspotCheckEvery {
		h.tickAt(d)
	}

	// Assert
	if calls := h.joinCalls(); len(calls) != 0 {
		t.Fatalf("join calls = %v, want none", calls)
	}
}

func TestAScanOffTheHotspotEndsTheDwellAndItsScans(t *testing.T) {
	// Arrange: the machine left the hotspot by itself.
	h := onHotspotClosed(t, preferred("Home", hotspot))
	h.sight(inSight("Home", "Home"))

	// Act
	h.tickAt(0)
	h.tickAt(time.Minute)

	// Assert
	if h.c.hotspot.on || h.scanCount() != 1 {
		t.Fatalf("dwell = %+v after %d scans, want it ended after one", h.c.hotspot, h.scanCount())
	}
}

func TestAQuietStateThatBeginsJoinedLearnsWhetherItStandsOnTheHotspot(t *testing.T) {
	cases := []struct {
		name   string
		scan   wifiScan
		wantOn bool
	}{
		{name: "on the hotspot", scan: inSight(hotspot, hotspot), wantOn: true},
		{name: "on another network", scan: inSight("Home", "Home")},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t).standing(pmsetOn, summary(true, "")).withLid(lidClosed)
			h.c.quiet = true
			h.c.hotspot = hotspotDwell{unknown: true}
			h.sight(tc.scan)

			// Act
			h.tickAt(0)

			// Assert
			if h.c.hotspot.on != tc.wantOn || h.c.hotspot.unknown {
				t.Fatalf("dwell = %+v, want on %t and known", h.c.hotspot, tc.wantOn)
			}
		})
	}
}

func TestTheDwellChecksNothingWithTheLidOpenOrNoHotspotConfigured(t *testing.T) {
	cases := []struct {
		name    string
		harness func(t *testing.T) *harness
	}{
		{name: "the lid open", harness: func(t *testing.T) *harness {
			h := newHarness(t).standing(pmsetOn, summary(true, "")).withLid(lidOpen)
			h.c.hotspot = hotspotDwell{on: true}
			return h
		}},
		{name: "no hotspot configured", harness: func(t *testing.T) *harness {
			h := newHarnessFor(t, "").standing(pmsetOn, summary(true, "")).withLid(lidClosed)
			h.c.quiet = true
			h.c.hotspot = hotspotDwell{unknown: true}
			return h
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := tc.harness(t)

			// Act
			h.tickAt(0)
			h.tickAt(time.Minute)

			// Assert
			if n := h.scanCount(); n != 0 {
				t.Fatalf("scans = %d, want none", n)
			}
		})
	}
}

func TestFailoverRetryInWaitsForTheDwellsNextCheck(t *testing.T) {
	// Arrange: on the hotspot, its next check 15s out.
	h := onHotspotClosed(t, preferred(hotspot))
	h.c.last = &agentreplv1.PersistentWifiState{
		Mode: &agentreplv1.PersistentWifiState_On{On: &agentreplv1.PersistentWifiModeOn{}},
		Wifi: &agentreplv1.PersistentWifiState_Joined{Joined: &agentreplv1.PersistentWifiJoined{}},
	}
	h.c.hotspot.check = time.Unix(0, 0).Add(hotspotCheckEvery)

	// Act
	wait, ok := h.c.failoverRetryIn()

	// Assert
	if !ok || wait != hotspotCheckEvery {
		t.Fatalf("failoverRetryIn() = (%s, %t), want (%s, true)", wait, ok, hotspotCheckEvery)
	}
}

func TestTheQuietStatesEdgesResetTheDwell(t *testing.T) {
	// Arrange: a dwell an earlier quiet state left behind.
	h := newHarness(t).standing(pmsetOn, summary(true, "Home"))
	h.r.on(argDisplaySleep, answer{})
	h.r.on(argDisplayWake, answer{})
	h.c.hotspot = hotspotDwell{on: true, seen: map[string]time.Time{"Home": time.Unix(0, 0)}}

	// Act / Assert: closing learns afresh, opening forgets.
	h.hear(LidClosed)
	if d := h.c.hotspot; d.on || !d.unknown || len(d.seen) != 0 {
		t.Fatalf("dwell after the lid closed = %+v, want it learning whether it stands on the hotspot", d)
	}
	h.hear(LidOpen)
	if d := h.c.hotspot; d.on || d.unknown || len(d.seen) != 0 {
		t.Fatalf("dwell after the lid opened = %+v, want it cleared", d)
	}
}

func TestAScanThatNamesNothingEndsTheDwellForTheQuietState(t *testing.T) {
	// Arrange: the quiet state began joined; the helper is not authorized.
	h := newHarness(t).standing(pmsetOn, summary(true, "")).withLid(lidClosed)
	h.c.quiet = true
	h.c.hotspot = hotspotDwell{unknown: true}
	h.sight(wifiScan{Authorization: "not_determined"})

	// Act: the first check, then a long closed lid.
	for d := time.Duration(0); d <= 10*hotspotCheckEvery; d += hotspotCheckEvery {
		h.tickAt(d)
	}

	// Assert
	if n := h.scanCount(); n != 1 {
		t.Fatalf("scans = %d, want the one that found no names", n)
	}
	if errs := h.failoverRecords("error", "the visible Wi-Fi networks could not be read; only the hotspot can be joined"); len(errs) != 1 {
		t.Fatalf("error records = %d, want one", len(errs))
	}
}
