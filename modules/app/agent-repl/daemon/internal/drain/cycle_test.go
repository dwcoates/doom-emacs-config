package drain

import (
	"context"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/ids"
	"agent-repld/internal/shimlife"
	"agent-repld/internal/wsm"
)

// beatAfter is a keep-alive asked an hour after the harness's engagement
// instant, so the session's last real turn is before it.
var beatAfter = instant.Add(time.Hour)

// cyclePending reports whether a keep-alive cycle stand-down of ws is pending.
func (h *harness) cyclePending(ws ids.WorkspaceID) bool {
	h.c.mu.Lock()
	defer h.c.mu.Unlock()
	_, ok := h.c.cycles[ws]
	return ok
}

func TestAnIdleVisibleSessionIsStoodDownAfterItsKeepalive(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant)

	// Act
	h.c.CycleDown(ws, beatAfter)

	// Assert
	killed := h.stand.Killed()
	if len(killed) != 1 || killed[0].WS != ws || killed[0].Force {
		t.Fatalf("kills = %+v, want one graceful stand-down", killed)
	}
}

func TestACycleStandDownRecordsTheKeepaliveCycleTerminal(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant)

	// Act
	h.c.CycleDown(ws, beatAfter)

	// Assert
	if term := h.terminal(t, ws); term == nil || term.Kind != wsm.TerminalKeepaliveCycle {
		t.Fatalf("terminal = %+v, want %s", term, wsm.TerminalKeepaliveCycle)
	}
}

func TestACycleStandDownRecordsItsOwnReason(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant)

	// Act
	h.c.CycleDown(ws, beatAfter)

	// Assert
	if term := h.terminal(t, ws); term == nil || !strings.Contains(term.Detail, "between keep-alives") {
		t.Fatalf("terminal = %+v, want the keep-alive cycle's reason", term)
	}
}

func TestACycleStandDownParksTheViewsAsTheKeepaliveCycle(t *testing.T) {
	// Arrange
	var parked []wsm.Park
	h := newHarness(t, func(d *Deps) {
		d.SetParked = func(_ ids.WorkspaceID, park wsm.Park) { parked = append(parked, park) }
	})
	ws := h.workspace(t, instant)

	// Act
	h.c.CycleDown(ws, beatAfter)

	// Assert
	if len(parked) != 1 || parked[0] != wsm.ParkKeepaliveCycle {
		t.Fatalf("parks = %v, want one keep-alive cycle park", parked)
	}
}

func TestACycleStandDownSkips(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness) ids.WorkspaceID
	}{
		{name: "the workspace the user is looking at", arrange: func(t *testing.T, h *harness) ids.WorkspaceID {
			ws := h.workspace(t, instant)
			if err := h.db.SetCurrent(context.Background(), ws, time.Unix(1_700_000_000, 0)); err != nil {
				t.Fatalf("SetCurrent: %v", err)
			}
			return ws
		}},
		{name: "a hidden workspace, whose lapse is the cache keeper's", arrange: func(t *testing.T, h *harness) ids.WorkspaceID {
			return h.hidden(t)
		}},
		{name: "a closed workspace, whose lapse is the cache keeper's", arrange: func(t *testing.T, h *harness) ids.WorkspaceID {
			ws := h.workspace(t, instant)
			if err := h.db.MarkClosed(context.Background(), ws, wsm.Closure{At: instant, By: wsm.CloseByUser}); err != nil {
				t.Fatalf("MarkClosed: %v", err)
			}
			return ws
		}},
		{name: "a session a real turn engaged since the beat", arrange: func(t *testing.T, h *harness) ids.WorkspaceID {
			return h.workspace(t, beatAfter.Add(time.Minute))
		}},
		{name: "a workspace with no session", arrange: func(t *testing.T, h *harness) ids.WorkspaceID {
			return h.bare(t)
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws := tt.arrange(t, h)

			// Act
			h.c.CycleDown(ws, beatAfter)

			// Assert
			if killed := h.stand.Killed(); len(killed) != 0 {
				t.Fatalf("kills = %+v, want none", killed)
			}
			if h.cyclePending(ws) {
				t.Fatal("a skipped stand-down stayed pending")
			}
		})
	}
}

func TestABusySessionIsNotCycledDownUnderItsWork(t *testing.T) {
	// Arrange: a turn in flight or live detached work is not free.
	h := newHarness(t)
	ws := h.workspace(t, instant)
	h.freeness.SetFree(ws, false)

	// Act
	h.c.CycleDown(ws, beatAfter)

	// Assert
	if killed := h.stand.Killed(); len(killed) != 0 {
		t.Fatalf("kills = %+v, want none under work", killed)
	}
	if !h.cyclePending(ws) {
		t.Fatal("the deferred stand-down was not kept pending")
	}
}

func TestADeferredCycleStandDownIsTakenWhenTheSessionFallsFree(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant)
	h.freeness.SetFree(ws, false)
	h.c.CycleDown(ws, beatAfter)
	h.freeness.SetFree(ws, true)

	// Act
	h.c.RetryWithheld(ws)

	// Assert
	if killed := h.stand.Killed(); len(killed) != 1 {
		t.Fatalf("kills = %+v, want the stand-down taken on the free edge", killed)
	}
	if h.cyclePending(ws) {
		t.Fatal("a stand-down taken stayed pending")
	}
}

// cycleParked registers one workspace whose session the keep-alive cycle
// stood down, engaged at ENGAGED.
func (h *harness) cycleParked(t *testing.T, engaged time.Time) ids.WorkspaceID {
	t.Helper()
	ws := h.workspace(t, engaged)
	if err := h.db.SetSessionTerminal(context.Background(), ws, wsm.SessionTerminal{Kind: wsm.TerminalKeepaliveCycle, At: engaged}); err != nil {
		t.Fatalf("SetSessionTerminal: %v", err)
	}
	h.stand.standDown(ws)
	return ws
}

func TestTheSweepHibernatesACycleParkPastTheCutoff(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.cycleParked(t, instant.Add(-2*time.Hour))

	// Act
	hibernated, err := h.c.Sweep(context.Background(), instant)

	// Assert
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	if len(hibernated) != 1 || hibernated[0] != ws {
		t.Fatalf("hibernated = %v, want the parked session", hibernated)
	}
	if term := h.terminal(t, ws); term == nil || term.Kind != TerminalHibernated {
		t.Fatalf("terminal = %+v, want hibernated", term)
	}
}

func TestTheSweepParksTheViewsHibernatedForADeepenedCyclePark(t *testing.T) {
	// Arrange
	var parked []wsm.Park
	h := newHarness(t, func(d *Deps) {
		d.SetParked = func(_ ids.WorkspaceID, park wsm.Park) { parked = append(parked, park) }
	})
	h.cycleParked(t, instant.Add(-2*time.Hour))

	// Act
	if _, err := h.c.Sweep(context.Background(), instant); err != nil {
		t.Fatalf("Sweep: %v", err)
	}

	// Assert
	if len(parked) != 1 || parked[0] != wsm.ParkHibernated {
		t.Fatalf("parks = %v, want one hibernated park", parked)
	}
}

func TestTheSweepLeavesACycleParkInsideTheCutoff(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.cycleParked(t, instant.Add(-time.Minute))

	// Act
	if _, err := h.c.Sweep(context.Background(), instant); err != nil {
		t.Fatalf("Sweep: %v", err)
	}

	// Assert
	if term := h.terminal(t, ws); term == nil || term.Kind != wsm.TerminalKeepaliveCycle {
		t.Fatalf("terminal = %+v, want the keep-alive cycle's park kept", term)
	}
}

func TestTheSweepLeavesACycleParkThatIsComingBackUp(t *testing.T) {
	// Arrange: a restore of it is under way.
	h := newHarness(t)
	ws := h.cycleParked(t, instant.Add(-2*time.Hour))
	h.stand.startUnderWay(ws)

	// Act
	if _, err := h.c.Sweep(context.Background(), instant); err != nil {
		t.Fatalf("Sweep: %v", err)
	}

	// Assert
	if term := h.terminal(t, ws); term == nil || term.Kind != wsm.TerminalKeepaliveCycle {
		t.Fatalf("terminal = %+v, want the park left to the restore", term)
	}
}

func TestAVisitStandDownStandsTheSessionDownForItsOwnReason(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant)

	// Act
	h.c.VisitDown(ws, beatAfter)

	// Assert
	killed := h.stand.Killed()
	if len(killed) != 1 || killed[0].WS != ws || killed[0].Force || killed[0].Life != shimlife.StandDownSettingsVisit {
		t.Fatalf("kills = %+v, want one graceful settings_visit stand-down", killed)
	}
}

func TestAVisitStandDownRecordsItsOwnReason(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant)

	// Act
	h.c.VisitDown(ws, beatAfter)

	// Assert
	if term := h.terminal(t, ws); term == nil || !strings.Contains(term.Detail, "settings pick") {
		t.Fatalf("terminal = %+v, want the settings visit's reason", term)
	}
}

func TestAVisitStandDownSkipsASessionATurnEngagedSinceThePick(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, beatAfter.Add(time.Minute))

	// Act
	h.c.VisitDown(ws, beatAfter)

	// Assert
	if killed := h.stand.Killed(); len(killed) != 0 {
		t.Fatalf("kills = %+v, want none after a prompt ran since the pick", killed)
	}
}

// A WORKSPACE IN REMOTE CONTROL KEEPS ITS SHIM UP (owner ruling, 2026-10-31):
// neither a keep-alive's park nor a settings visit's stand-down is taken.
func TestARemoteControlWorkspaceIsNotStoodDown(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness) ids.WorkspaceID
		ask     func(c *controller, ws ids.WorkspaceID)
	}{
		{name: "a main worktree's default after a keep-alive", arrange: func(t *testing.T, h *harness) ids.WorkspaceID {
			return h.mainWorktree(t, instant)
		}, ask: func(c *controller, ws ids.WorkspaceID) { c.CycleDown(ws, beatAfter) }},
		{name: "a mode turned on after a keep-alive", arrange: func(t *testing.T, h *harness) ids.WorkspaceID {
			return h.remoteControlOn(t)
		}, ask: func(c *controller, ws ids.WorkspaceID) { c.CycleDown(ws, beatAfter) }},
		{name: "a mode turned on on the switch away from a settings pick", arrange: func(t *testing.T, h *harness) ids.WorkspaceID {
			return h.remoteControlOn(t)
		}, ask: func(c *controller, ws ids.WorkspaceID) { c.VisitDown(ws, beatAfter) }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws := tt.arrange(t, h)

			// Act
			tt.ask(h.c, ws)

			// Assert
			if killed := h.stand.Killed(); len(killed) != 0 {
				t.Fatalf("kills = %+v, want none for a workspace in remote control", killed)
			}
			if h.cyclePending(ws) {
				t.Fatal("a stand-down kept for remote control stayed pending")
			}
		})
	}
}

func TestARemoteControlSkipIsRecordedAtInfo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.remoteControlOn(t)

	// Act
	h.c.CycleDown(ws, beatAfter)

	// Assert
	for _, record := range records(h.log, opCycle) {
		if record.Level == "info" && strings.Contains(record.Message, "remote control is on") {
			return
		}
	}
	t.Fatalf("records = %+v, want the remote-control skip at info", records(h.log, opCycle))
}

func TestAModeTurnedOffSinceIsParkedAtTheNextKeepalive(t *testing.T) {
	// Arrange: the mode is read at the attempt, not when it was first on.
	h := newHarness(t)
	ws := h.mainWorktree(t, instant)
	if _, err := h.db.SetRemoteControlMode(context.Background(), ws, wsm.RemoteControlOff); err != nil {
		t.Fatalf("SetRemoteControlMode: %v", err)
	}

	// Act
	h.c.CycleDown(ws, beatAfter)

	// Assert
	if term := h.terminal(t, ws); term == nil || term.Kind != wsm.TerminalKeepaliveCycle {
		t.Fatalf("terminal = %+v, want the keep-alive cycle's park", term)
	}
}

// remoteControlOn registers a workspace whose mode was turned on explicitly.
func (h *harness) remoteControlOn(t *testing.T) ids.WorkspaceID {
	t.Helper()
	ws := h.workspace(t, instant)
	if _, err := h.db.SetRemoteControlMode(context.Background(), ws, wsm.RemoteControlOn); err != nil {
		t.Fatalf("SetRemoteControlMode: %v", err)
	}
	return ws
}
