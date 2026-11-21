package cachelapse

import (
	"context"
	"slices"
	"testing"
	"time"

	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// longOutage is an outage long enough to pull beats forward.
const longOutage = OutagePrefetchMinimum + time.Minute

// dueIn is a visible workspace whose next beat is due IN from now.
func dueIn(in time.Duration) wsm.Workspace {
	return beatable(visible(), KeepaliveInterval-in)
}

// restoredHarness is a harness whose workspace's beat is armed IN from now.
func restoredHarness(t *testing.T, in time.Duration) *harness {
	t.Helper()
	h := newHarness(t, dueIn(in))
	h.shim.answer = sent()
	h.keeper.schedule(context.Background(), theWorkspace, "test")
	return h
}

func TestNetworkRestoredPullsBeatsForward(t *testing.T) {
	tests := []struct {
		name string
		// in is how far off the workspace's beat is when the network comes
		// back, and down how long it was down.
		in, down   time.Duration
		wantPulled bool
	}{
		{name: "a long outage pulls a beat due within the window", in: 15 * time.Minute, down: longOutage, wantPulled: true},
		{name: "a long outage pulls a beat due at the window's edge", in: OutagePrefetchWindow, down: longOutage, wantPulled: true},
		{name: "a long outage pulls a beat already overdue", in: -time.Minute, down: longOutage, wantPulled: true},
		{name: "a long outage leaves a beat due past the window", in: OutagePrefetchWindow + time.Minute, down: longOutage},
		{name: "an outage of exactly the minimum pulls nothing", in: 15 * time.Minute, down: OutagePrefetchMinimum},
		{name: "a short outage pulls nothing", in: 15 * time.Minute, down: time.Minute},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := restoredHarness(t, tc.in)
			scheduled := h.keeper.beats[theWorkspace]
			// Act
			h.keeper.NetworkRestored(tc.down)
			h.applyQueued()
			// Assert
			want := scheduled
			if tc.wantPulled {
				want = now
			}
			if got := h.keeper.beats[theWorkspace]; !got.Equal(want) {
				t.Fatalf("beat = %v, want %v", got, want)
			}
		})
	}
}

func TestAPulledBeatGoesOutAsAnOrdinaryBeat(t *testing.T) {
	// Arrange
	h := restoredHarness(t, 15*time.Minute)
	h.keeper.NetworkRestored(longOutage)
	h.applyQueued()
	// Act
	h.keeper.beatsDue(context.Background())
	// Assert
	rec, _ := h.db.Workspace(context.Background(), theWorkspace)
	if h.shim.beats != 1 {
		t.Fatalf("beats = %d, want 1", h.shim.beats)
	}
	if rec.LastKeepaliveAt == nil || !rec.LastKeepaliveAt.Equal(now) {
		t.Fatalf("LastKeepaliveAt = %v, want the pulled beat %v", rec.LastKeepaliveAt, now)
	}
	if got := h.keeper.beats[theWorkspace]; !got.Equal(now.Add(KeepaliveInterval)) {
		t.Fatalf("next beat = %v, want an interval after the pulled one", got)
	}
}

func TestASentPulledBeatIsNoLongerEarly(t *testing.T) {
	// Arrange
	h := restoredHarness(t, 15*time.Minute)
	h.keeper.NetworkRestored(longOutage)
	h.applyQueued()
	h.keeper.beatsDue(context.Background())
	// Act: the next edge re-times the beat.
	h.keeper.schedule(context.Background(), theWorkspace, "test")
	// Assert
	if _, early := h.keeper.early[theWorkspace]; early {
		t.Fatal("the pulled beat is still marked early after it went out")
	}
	if got := h.keeper.beats[theWorkspace]; !got.Equal(now.Add(KeepaliveInterval)) {
		t.Fatalf("beat = %v, want an interval after the pulled one", got)
	}
}

func TestAPulledBeatSurvivesARescheduleBeforeItGoesOut(t *testing.T) {
	// Arrange
	h := restoredHarness(t, 15*time.Minute)
	h.keeper.NetworkRestored(longOutage)
	h.applyQueued()
	// Act: a reconcile reads the row afresh before the beat goes out.
	h.keeper.schedule(context.Background(), theWorkspace, "a reconcile")
	// Assert
	if got := h.keeper.beats[theWorkspace]; !got.Equal(now) {
		t.Fatalf("beat = %v, want the pulled %v", got, now)
	}
}

func TestAParkedSessionsPulledBeatGoesOutOnceItIsRestored(t *testing.T) {
	// Arrange: a parked session, its beat pulled forward.
	h := newHarness(t, dueIn(15*time.Minute))
	h.shim.answer = sent()
	h.db.sessions[theWorkspace] = parkedSession(theWorkspace)
	h.serving = false
	h.keeper.schedule(context.Background(), theWorkspace, "test")
	h.keeper.NetworkRestored(longOutage)
	h.applyQueued()
	h.keeper.beatsDue(context.Background())
	if len(h.restoring) != 1 {
		t.Fatalf("restores asked = %d, want 1", len(h.restoring))
	}
	// Act: the session comes up.
	h.db.sessions[theWorkspace] = liveSession(theWorkspace)
	h.serving = true
	h.keeper.SessionUp(theWorkspace)
	h.applyQueued()
	h.keeper.beatsDue(context.Background())
	// Assert
	if h.shim.beats != 1 {
		t.Fatalf("beats = %d, want the pulled beat sent once the session came up", h.shim.beats)
	}
}

func TestAPulledBeatIsNotSentAtTheColdGate(t *testing.T) {
	// Arrange: the session parks at its cold gate after its beat was pulled.
	h := restoredHarness(t, 15*time.Minute)
	h.keeper.NetworkRestored(longOutage)
	h.applyQueued()
	h.gate = coldLapsed()
	// Act
	h.keeper.beatsDue(context.Background())
	// Assert
	if h.shim.beats != 0 {
		t.Fatalf("beats = %d, want none at the cold gate", h.shim.beats)
	}
	if _, early := h.keeper.early[theWorkspace]; early {
		t.Fatal("a beat owed no longer is still marked early")
	}
}

func TestAColdGatedWorkspaceIsNotPulled(t *testing.T) {
	// Arrange: the gate stands before the network comes back.
	h := newHarness(t, dueIn(15*time.Minute))
	h.gate = coldLapsed()
	h.keeper.schedule(context.Background(), theWorkspace, "test")
	// Act
	h.keeper.NetworkRestored(longOutage)
	h.applyQueued()
	h.keeper.beatsDue(context.Background())
	// Assert
	if _, early := h.keeper.early[theWorkspace]; early || h.shim.beats != 0 {
		t.Fatalf("early = %v, beats = %d, want a cold-gated workspace left alone", early, h.shim.beats)
	}
}

func TestAPulledBeatWaitingOnDetachedWorkStaysEarly(t *testing.T) {
	// Arrange
	h := restoredHarness(t, 15*time.Minute)
	h.shim.answer = skipped(&shimv1.SendKeepaliveSkipped{Reason: &shimv1.SendKeepaliveSkipped_DetachedWorkLive{DetachedWorkLive: &shimv1.SendKeepaliveSkippedDetachedWorkLive{LiveItems: 1}}})
	h.keeper.NetworkRestored(longOutage)
	h.applyQueued()
	h.keeper.beatsDue(context.Background())
	// Act: the detached work ends.
	h.shim.answer = sent()
	h.keeper.SessionFree(theWorkspace, true)
	h.applyQueued()
	h.keeper.beatsDue(context.Background())
	// Assert
	if h.shim.beats != 2 {
		t.Fatalf("beats = %d, want the pulled beat asked again once the work ended", h.shim.beats)
	}
}

func TestAPulledBeatThatFoundTheMainAgentBusyIsNoLongerEarly(t *testing.T) {
	// Arrange
	h := restoredHarness(t, 15*time.Minute)
	h.shim.answer = skipped(&shimv1.SendKeepaliveSkipped{Reason: &shimv1.SendKeepaliveSkipped_MainAgentBusy{MainAgentBusy: &shimv1.SendKeepaliveSkippedMainAgentBusy{}}})
	h.keeper.NetworkRestored(longOutage)
	h.applyQueued()
	// Act
	h.keeper.beatsDue(context.Background())
	// Assert
	if _, early := h.keeper.early[theWorkspace]; early {
		t.Fatal("a pulled beat the shim answered is still marked early")
	}
}

func TestNetworkRestoredRecordsItsDecision(t *testing.T) {
	tests := []struct {
		name       string
		down       time.Duration
		message    string
		wantPulled []string
	}{
		{name: "a long outage names the workspaces it pulled", down: longOutage,
			message: "the network was restored after a long outage; every keep-alive due soon is sent now", wantPulled: []string{string(theWorkspace)}},
		{name: "a short outage says it pulled nothing", down: time.Minute,
			message: "the network was restored after a short outage; no keep-alive is pulled forward"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := restoredHarness(t, 15*time.Minute)
			// Act
			h.keeper.NetworkRestored(tc.down)
			h.applyQueued()
			// Assert
			record, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "info", Operation: opOutage, Message: tc.message})
			if !ok || record.Context["down_for"] != tc.down.String() {
				t.Fatalf("record = %+v (found %v), want %q with down_for %v", record, ok, tc.message, tc.down)
			}
			if tc.wantPulled != nil {
				if pulled, _ := record.Context["pulled"].([]string); !slices.Equal(pulled, tc.wantPulled) {
					t.Fatalf("pulled = %v, want %v", record.Context["pulled"], tc.wantPulled)
				}
			}
		})
	}
}
