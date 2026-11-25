package drain

import (
	"context"
	"fmt"
	"slices"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/sessionwhy"
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
	if term := h.terminal(t, ws); term == nil || term.Kind != wsm.TerminalParked {
		t.Fatalf("terminal = %+v, want %s", term, wsm.TerminalParked)
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

func TestACycleStandDownParksTheViews(t *testing.T) {
	// Arrange
	var parked []bool
	h := newHarness(t, func(d *Deps) {
		d.SetParked = func(_ ids.WorkspaceID, park bool) { parked = append(parked, park) }
	})
	ws := h.workspace(t, instant)

	// Act
	h.c.CycleDown(ws, beatAfter)

	// Assert
	if len(parked) != 1 || !parked[0] {
		t.Fatalf("parks = %v, want the one park raised once", parked)
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
		{name: "a backlogged workspace, whose lapse is the cache keeper's", arrange: func(t *testing.T, h *harness) ids.WorkspaceID {
			return h.backlogged(t)
		}},
		{name: "a closed workspace with no merge queued, whose lapse is the cache keeper's", arrange: func(t *testing.T, h *harness) ids.WorkspaceID {
			ws := h.workspace(t, instant)
			h.closedAt(t, ws)
			return ws
		}},
		{name: "a session a real turn ran on since the beat", arrange: func(t *testing.T, h *harness) ids.WorkspaceID {
			ws := h.workspace(t, instant)
			h.turnClosed(t, ws, beatAfter.Add(time.Minute))
			return ws
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
	if err := h.db.SetSessionTerminal(context.Background(), ws, wsm.SessionTerminal{Kind: wsm.TerminalParked, Detail: cycleReason.detail, At: engaged}); err != nil {
		t.Fatalf("SetSessionTerminal: %v", err)
	}
	h.stand.standDown(ws)
	return ws
}

func TestTheSweepLeavesAParkAsItStands(t *testing.T) {
	tests := []struct {
		name    string
		engaged time.Duration
		coming  bool
	}{
		{name: "a park inside the cutoff", engaged: -time.Minute},
		{name: "a park past the cutoff is no deeper a park", engaged: -2 * time.Hour},
		{name: "a park a restore is bringing back", engaged: -2 * time.Hour, coming: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var told []bool
			h := newHarness(t, func(d *Deps) {
				d.SetParked = func(_ ids.WorkspaceID, park bool) { told = append(told, park) }
			})
			ws := h.cycleParked(t, instant.Add(tt.engaged))
			if tt.coming {
				h.stand.startUnderWay(ws)
			}

			// Act
			parked, err := h.c.Sweep(context.Background(), instant)

			// Assert
			if err != nil {
				t.Fatalf("Sweep: %v", err)
			}
			if len(parked) != 0 {
				t.Errorf("parked = %v, want nothing parked again", parked)
			}
			if term := h.terminal(t, ws); term == nil || term.Kind != wsm.TerminalParked || term.Detail != cycleReason.detail {
				t.Errorf("terminal = %+v, want the keep-alive cycle's park kept with its own detail", term)
			}
			if len(told) != 0 {
				t.Errorf("SetParked calls = %v, want the views told nothing", told)
			}
		})
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

func TestAVisitStandDownSkipsASessionATurnRanOnSinceThePick(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant)
	h.turnClosed(t, ws, beatAfter.Add(time.Minute))

	// Act
	h.c.VisitDown(ws, beatAfter)

	// Assert
	if killed := h.stand.Killed(); len(killed) != 0 {
		t.Fatalf("kills = %+v, want none after a prompt ran since the pick", killed)
	}
}

// A BRING-UP IS NO WORK: a settings pick's revival records the session's
// start after the pick (2026-11-20, 25 visits kept up with no turn run), and
// the visit still ends, since a start never moves the idle clock.
func TestAVisitStandDownStandsDownASessionOnlyItsBringUpStarted(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant)
	h.restarted(t, ws, beatAfter.Add(2*time.Second))

	// Act
	h.c.VisitDown(ws, beatAfter)

	// Assert
	killed := h.stand.Killed()
	if len(killed) != 1 || killed[0].Life != shimlife.StandDownSettingsVisit {
		t.Fatalf("kills = %+v, want the visit's stand-down despite the bring-up", killed)
	}
}

// A TURN A BOUNCE ORPHANED IS NO WORK: it closes at the bounce, after the
// ask, and the idle park is taken all the same (invariant A, 2026-11-23).
func TestAnIdleParkIsTakenOverATurnABounceOrphanedSinceTheAsk(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant)
	h.turnCut(t, ws, beatAfter.Add(time.Minute), wsm.CloseKilled)

	// Act
	h.c.IdleDown(ws, beatAfter)

	// Assert
	killed := h.stand.Killed()
	if len(killed) != 1 {
		t.Fatalf("kills = %+v, want the idle park despite the orphaned turn's close", killed)
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
		{name: "a mode turned on when its cache went cold", arrange: func(t *testing.T, h *harness) ids.WorkspaceID {
			return h.remoteControlOn(t)
		}, ask: func(c *controller, ws ids.WorkspaceID) { c.ColdDown(ws, beatAfter, false) }},
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
	if term := h.terminal(t, ws); term == nil || term.Kind != wsm.TerminalParked {
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

// closedAt marks ws closed by the user at instant.
func (h *harness) closedAt(t *testing.T, ws ids.WorkspaceID) {
	t.Helper()
	if err := h.db.MarkClosed(context.Background(), ws, wsm.Closure{At: instant, By: wsm.CloseByUser}); err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}
}

// A CLOSED WORKSPACE WITH A MERGE QUEUED is parked as an open one is: the
// cache keeper's lapse never stands a queued workspace down.
func TestAClosedWorkspaceWithAMergeQueuedIsStoodDown(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant)
	h.closedAt(t, ws)
	queuedAt(t, h, ws, instant)

	// Act
	h.c.CycleDown(ws, beatAfter)

	// Assert
	if term := h.terminal(t, ws); term == nil || term.Kind != wsm.TerminalParked {
		t.Fatalf("terminal = %+v, want the queued closed workspace parked", term)
	}
}

func TestAClosedWorkspaceWhoseQueuesCannotBeReadIsNotStoodDown(t *testing.T) {
	// Arrange
	h := newHarness(t, func(d *Deps) { d.DB = sweepReadErrDB{DB: d.DB, read: "queues", err: errFake} })
	ws := h.workspace(t, instant)
	h.closedAt(t, ws)

	// Act
	h.c.CycleDown(ws, beatAfter)

	// Assert
	if killed := h.stand.Killed(); len(killed) != 0 {
		t.Fatalf("kills = %+v, want none when the queues cannot be read", killed)
	}
	if h.cyclePending(ws) {
		t.Fatal("a dropped stand-down stayed pending")
	}
	rec, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: opCycle,
		Message: "could not read whether the closed workspace has a merge queued; its stand-down is dropped"})
	if !ok {
		t.Fatalf("records = %+v, want the queue read's failure at error", records(h.log, opCycle))
	}
	if rec.Context["workspace"] != string(ws) || !strings.Contains(fmt.Sprint(rec.Context["cause"]), errFake.Error()) {
		t.Fatalf("record context = %v, want the workspace and the cause", rec.Context)
	}
}

func TestAColdStandDownParksTheSessionForItsOwnReason(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant)

	// Act
	h.c.ColdDown(ws, beatAfter, false)

	// Assert
	killed := h.stand.Killed()
	if len(killed) != 1 || killed[0].WS != ws || killed[0].Force || killed[0].Life != shimlife.StandDownCacheCold {
		t.Fatalf("kills = %+v, want one graceful cache_cold stand-down", killed)
	}
	if term := h.terminal(t, ws); term == nil || term.Kind != wsm.TerminalParked || term.Detail != coldReason.detail {
		t.Fatalf("terminal = %+v, want the one park with the cold cache's detail", term)
	}
}

func TestAColdGateStandDownParksTheSessionForItsOwnReason(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant)

	// Act
	h.c.ColdDown(ws, beatAfter, true)

	// Assert
	killed := h.stand.Killed()
	if len(killed) != 1 || killed[0].WS != ws || killed[0].Force || killed[0].Life != shimlife.StandDownColdGate {
		t.Fatalf("kills = %+v, want one graceful cold_gate stand-down", killed)
	}
	if term := h.terminal(t, ws); term == nil || term.Kind != wsm.TerminalParked || term.Detail != coldGateReason.detail {
		t.Fatalf("terminal = %+v, want the one park with the cold gate's detail", term)
	}
}

// A COLD GATE'S PARK IN REMOTE CONTROL IS TAKEN, AND BRIDGES THE GATE: a
// session spared at its gate left every answer refused.
func TestAColdGateStandDownInRemoteControlParksAndBridgesTheGate(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.remoteControlOn(t)

	// Act
	h.c.ColdDown(ws, beatAfter, true)

	// Assert
	if killed := h.stand.Killed(); len(killed) != 1 || killed[0].Life != shimlife.StandDownColdGate {
		t.Fatalf("kills = %+v, want the session in remote control parked behind its gate", killed)
	}
	if term := h.terminal(t, ws); term == nil || term.Kind != wsm.TerminalParked || term.Detail != coldGateReason.detail {
		t.Fatalf("terminal = %+v, want the one park with the cold gate's detail", term)
	}
	if !slices.Equal(h.bridged, []ids.WorkspaceID{ws}) {
		t.Fatalf("bridged = %v, want the gate bridged once after its park", h.bridged)
	}
	record := recordWith(t, h, opCold, "parked an idle session whose cold gate was raised")
	if record.Context["remote_control"] != true || record.Context["remote_control_policy"] != "bridges" {
		t.Fatalf("record = %+v, want the park naming remote control on and the bridging policy", record)
	}
}

func TestAColdGateStandDownOutOfRemoteControlBridgesNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant)

	// Act
	h.c.ColdDown(ws, beatAfter, true)

	// Assert
	if len(h.stand.Killed()) != 1 || len(h.bridged) != 0 {
		t.Fatalf("kills = %+v, bridged = %v; want the park alone", h.stand.Killed(), h.bridged)
	}
}

func TestAColdGateStandDownThatFailsBridgesNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.remoteControlOn(t)
	h.stand.failKill(ws, errFake)

	// Act
	h.c.ColdDown(ws, beatAfter, true)

	// Assert
	if len(h.bridged) != 0 {
		t.Fatalf("bridged = %v, want no bridge beside a session the park could not stand down", h.bridged)
	}
	if record := recordWith(t, h, opCold, "the graceful stand-down failed after the hibernate ack; deferring"); record.Level != "error" {
		t.Fatalf("record = %+v, want the failed stand-down at error", record)
	}
}

func TestAColdGateStandDownParksTheWorkspaceInView(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant)
	if err := h.db.SetCurrent(context.Background(), ws, time.Unix(1_700_000_000, 0)); err != nil {
		t.Fatalf("SetCurrent: %v", err)
	}

	// Act
	h.c.ColdDown(ws, beatAfter, true)

	// Assert
	if killed := h.stand.Killed(); len(killed) != 1 || killed[0].Life != shimlife.StandDownColdGate {
		t.Fatalf("kills = %+v, want the workspace in view parked behind its gate", killed)
	}
}

func TestAColdGateStandDownSkipsASessionARealTurnRanOn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant)
	h.turnClosed(t, ws, beatAfter.Add(time.Minute))

	// Act
	h.c.ColdDown(ws, beatAfter, true)

	// Assert
	if killed := h.stand.Killed(); len(killed) != 0 {
		t.Fatalf("kills = %+v, want none: a real turn ran since", killed)
	}
}

func TestAColdStandDownSkips(t *testing.T) {
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
		{name: "a session a real turn ran on since the cache went cold", arrange: func(t *testing.T, h *harness) ids.WorkspaceID {
			ws := h.workspace(t, instant)
			h.turnClosed(t, ws, beatAfter.Add(time.Minute))
			return ws
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws := tt.arrange(t, h)

			// Act
			h.c.ColdDown(ws, beatAfter, false)

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

func TestAColdStandDownOfABusySessionIsTakenWhenItFallsFree(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant)
	h.freeness.SetFree(ws, false)
	h.c.ColdDown(ws, beatAfter, false)
	if killed := h.stand.Killed(); len(killed) != 0 {
		t.Fatalf("kills = %+v, want none under work", killed)
	}
	h.freeness.SetFree(ws, true)

	// Act
	h.c.RetryWithheld(ws)

	// Assert
	killed := h.stand.Killed()
	if len(killed) != 1 || killed[0].Life != shimlife.StandDownCacheCold {
		t.Fatalf("kills = %+v, want the cold stand-down taken on the free edge", killed)
	}
	if h.cyclePending(ws) {
		t.Fatal("a stand-down taken stayed pending")
	}
}

func TestAColdStandDownParksAWorkspaceWithAMergeQueued(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant)
	queuedAt(t, h, ws, instant)

	// Act
	h.c.ColdDown(ws, beatAfter, false)

	// Assert
	if term := h.terminal(t, ws); term == nil || term.Kind != wsm.TerminalParked {
		t.Fatalf("terminal = %+v, want the queued workspace parked", term)
	}
}

// A COLD PARK AND A SETTINGS VISIT ARE TAKEN WHATEVER THE VISIBILITY: the
// cache keeper asks either only when no lapse owns the session.
func TestAColdParkOrAVisitStandsDownAClosedOrWithheldWorkspace(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness) ids.WorkspaceID
		ask     func(c *controller, ws ids.WorkspaceID)
		life    shimlife.StandDownReason
	}{
		{name: "a cold park of a backlogged workspace", arrange: func(t *testing.T, h *harness) ids.WorkspaceID { return h.backlogged(t) },
			ask: func(c *controller, ws ids.WorkspaceID) { c.ColdDown(ws, beatAfter, false) }, life: shimlife.StandDownCacheCold},
		{name: "a cold park of a closed workspace", arrange: func(t *testing.T, h *harness) ids.WorkspaceID {
			ws := h.workspace(t, instant)
			h.closedAt(t, ws)
			return ws
		}, ask: func(c *controller, ws ids.WorkspaceID) { c.ColdDown(ws, beatAfter, false) }, life: shimlife.StandDownCacheCold},
		{name: "a visit of a backlogged workspace", arrange: func(t *testing.T, h *harness) ids.WorkspaceID { return h.backlogged(t) },
			ask: func(c *controller, ws ids.WorkspaceID) { c.VisitDown(ws, beatAfter) }, life: shimlife.StandDownSettingsVisit},
		{name: "a visit of a closed workspace", arrange: func(t *testing.T, h *harness) ids.WorkspaceID {
			ws := h.workspace(t, instant)
			h.closedAt(t, ws)
			return ws
		}, ask: func(c *controller, ws ids.WorkspaceID) { c.VisitDown(ws, beatAfter) }, life: shimlife.StandDownSettingsVisit},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws := tt.arrange(t, h)

			// Act
			tt.ask(h.c, ws)

			// Assert
			killed := h.stand.Killed()
			if len(killed) != 1 || killed[0].WS != ws || killed[0].Life != tt.life {
				t.Fatalf("kills = %+v, want one %s stand-down", killed, tt.life)
			}
		})
	}
}
func TestCycleDownThenReportsHowTheStandDownSettled(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness) ids.WorkspaceID
		want    CycleOutcome
	}{
		{name: "an idle visible session is parked", arrange: func(t *testing.T, h *harness) ids.WorkspaceID {
			return h.workspace(t, instant)
		}, want: CycleOutcome{Down: true, Why: cycleReason.done.Template()}},
		{name: "the workspace in view is kept up", arrange: func(t *testing.T, h *harness) ids.WorkspaceID {
			ws := h.workspace(t, instant)
			if err := h.db.SetCurrent(context.Background(), ws, time.Unix(1_700_000_000, 0)); err != nil {
				t.Fatalf("SetCurrent: %v", err)
			}
			return ws
		}, want: CycleOutcome{Why: "the user is looking at the workspace; its session is kept up"}},
		{name: "a session a real turn ran on since is kept up", arrange: func(t *testing.T, h *harness) ids.WorkspaceID {
			ws := h.workspace(t, instant)
			h.turnClosed(t, ws, beatAfter.Add(time.Minute))
			return ws
		}, want: CycleOutcome{Why: "a real turn ran since; the session is kept up"}},
		{name: "a workspace with no session is already down", arrange: func(t *testing.T, h *harness) ids.WorkspaceID {
			ws := h.bare(t)
			if _, err := h.db.SetRemoteControlMode(context.Background(), ws, wsm.RemoteControlOff); err != nil {
				t.Fatalf("SetRemoteControlMode: %v", err)
			}
			return ws
		}, want: CycleOutcome{Down: true, Why: "the workspace has no live session; nothing to stand down"}},
		{name: "a workspace in remote control is kept up", arrange: func(t *testing.T, h *harness) ids.WorkspaceID {
			ws := h.workspace(t, instant)
			if _, err := h.db.SetRemoteControlMode(context.Background(), ws, wsm.RemoteControlOn); err != nil {
				t.Fatalf("SetRemoteControlMode: %v", err)
			}
			return ws
		}, want: CycleOutcome{Why: "remote control is on; the shim stays up for claude.ai"}},
		{name: "a hibernate directive that fails is a failure", arrange: func(t *testing.T, h *harness) ids.WorkspaceID {
			ws := h.workspace(t, instant)
			h.stand.hibernateErr[ws] = fmt.Errorf("the shim hung up")
			return ws
		}, want: CycleOutcome{Failed: true, Why: "the park failed; its record above says why"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws := tt.arrange(t, h)
			var got []CycleOutcome

			// Act
			h.c.CycleDownThen(ws, beatAfter, func(o CycleOutcome) { got = append(got, o) })

			// Assert
			if len(got) != 1 || got[0] != tt.want {
				t.Fatalf("reports = %+v, want one %+v", got, tt.want)
			}
		})
	}
}

func TestCycleDownThenWaitsWhileTheStandDownIsPending(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant)
	h.freeness.SetFree(ws, false)
	var got []CycleOutcome
	h.c.CycleDownThen(ws, beatAfter, func(o CycleOutcome) { got = append(got, o) })
	if len(got) != 0 {
		t.Fatalf("reports = %+v under work, want none yet", got)
	}
	h.freeness.SetFree(ws, true)

	// Act
	h.c.RetryWithheld(ws)

	// Assert
	if len(got) != 1 || !got[0].Down {
		t.Fatalf("reports = %+v, want the one park once the session fell free", got)
	}
}

// mergeQueuedDown asks for the park scoped to M.
func (h *harness) mergeQueuedDown(ws ids.WorkspaceID, m queuedMerge) {
	h.c.MergeQueuedDown(ws, m.repo, m.ledger)
}

func TestAMergeQueuedParkParksTheSessionForItsOwnReason(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant)
	m := queuedAt(t, h, ws, instant)

	// Act
	h.mergeQueuedDown(ws, m)

	// Assert
	killed := h.stand.Killed()
	if len(killed) != 1 || killed[0].WS != ws || killed[0].Force || killed[0].Life != shimlife.StandDownMergeQueued {
		t.Fatalf("kills = %+v, want one graceful merge_queued stand-down", killed)
	}
	if term := h.terminal(t, ws); term == nil || term.Kind != wsm.TerminalParked || term.Detail != mergeQueuedReason.detail {
		t.Fatalf("terminal = %+v, want the one park with the sync merge's detail", term)
	}
	rec, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "info", Operation: opMergeQueued, Message: mergeQueuedReason.done.Template()})
	if !ok {
		t.Fatalf("records = %+v, want the park recorded at info", records(h.log, opMergeQueued))
	}
	if rec.Context["workspace"] != string(ws) || rec.Context["repo"] != string(m.repo) || rec.Context["merge_entry"] != string(m.ledger) {
		t.Fatalf("record context = %v, want the workspace and the queue entry", rec.Context)
	}
	if h.cyclePending(ws) {
		t.Fatal("a park taken stayed pending")
	}
}

func TestAMergeQueuedParkIsTakenWhatever(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness, ws ids.WorkspaceID)
	}{
		{name: "the workspace the user is looking at", arrange: func(t *testing.T, h *harness, ws ids.WorkspaceID) {
			if err := h.db.SetCurrent(context.Background(), ws, time.Unix(1_700_000_000, 0)); err != nil {
				t.Fatalf("SetCurrent: %v", err)
			}
		}},
		{name: "a closed workspace", arrange: func(t *testing.T, h *harness, ws ids.WorkspaceID) { h.closedAt(t, ws) }},
		{name: "a real turn ran since the ask was stamped", arrange: func(t *testing.T, h *harness, ws ids.WorkspaceID) {
			h.turnClosed(t, ws, h.clock.Now().Add(time.Minute))
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws := h.workspace(t, instant)
			m := queuedAt(t, h, ws, instant)
			tt.arrange(t, h, ws)

			// Act
			h.mergeQueuedDown(ws, m)

			// Assert
			if killed := h.stand.Killed(); len(killed) != 1 || killed[0].Life != shimlife.StandDownMergeQueued {
				t.Fatalf("kills = %+v, want the merge_queued park taken", killed)
			}
		})
	}
}

func TestAMergeQueuedParkUnderATurnIsTakenWhenTheTurnEnds(t *testing.T) {
	// Arrange: the user's own finish puts the merge in line with the turn
	// still running; that turn's end is not engagement that keeps it up.
	h := newHarness(t)
	ws := h.workspace(t, instant)
	m := queuedAt(t, h, ws, instant)
	h.freeness.SetFree(ws, false)
	h.mergeQueuedDown(ws, m)
	if killed := h.stand.Killed(); len(killed) != 0 {
		t.Fatalf("kills = %+v, want none under the turn", killed)
	}
	if !h.cyclePending(ws) {
		t.Fatal("the park under the turn was not kept pending")
	}
	h.turnClosed(t, ws, h.clock.Now().Add(time.Minute))
	h.freeness.SetFree(ws, true)

	// Act
	h.c.RetryWithheld(ws)

	// Assert
	if killed := h.stand.Killed(); len(killed) != 1 || killed[0].Life != shimlife.StandDownMergeQueued {
		t.Fatalf("kills = %+v, want the park taken on the turn's free edge", killed)
	}
	if h.cyclePending(ws) {
		t.Fatal("a park taken stayed pending")
	}
}

func TestAMergeQueuedParkIsNotTaken(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness, ws ids.WorkspaceID, m *queuedMerge)
		message string
	}{
		{name: "in remote control", arrange: func(t *testing.T, h *harness, ws ids.WorkspaceID, _ *queuedMerge) {
			if _, err := h.db.SetRemoteControlMode(context.Background(), ws, wsm.RemoteControlOn); err != nil {
				t.Fatalf("SetRemoteControlMode: %v", err)
			}
		}, message: "remote control is on; the shim stays up for claude.ai"},
		{name: "once the merge was admitted", arrange: func(t *testing.T, h *harness, ws ids.WorkspaceID, m *queuedMerge) {
			if err := h.db.AdmitMerge(context.Background(), m.repo, ws); err != nil {
				t.Fatalf("AdmitMerge: %v", err)
			}
		}, message: "the merge no longer waits in line; the park is not taken"},
		{name: "once the merge left its queue", arrange: func(t *testing.T, h *harness, ws ids.WorkspaceID, m *queuedMerge) {
			if err := h.db.RemoveMergeQueueEntry(context.Background(), m.repo, ws, "user_dequeue"); err != nil {
				t.Fatalf("RemoveMergeQueueEntry: %v", err)
			}
		}, message: "the merge no longer waits in line; the park is not taken"},
		{name: "for an ask naming an entry the line no longer holds", arrange: func(t *testing.T, h *harness, ws ids.WorkspaceID, m *queuedMerge) {
			m.ledger = wsm.NewLeaseID()
		}, message: "the merge no longer waits in line; the park is not taken"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws := h.workspace(t, instant)
			m := queuedAt(t, h, ws, instant)
			tt.arrange(t, h, ws, &m)

			// Act
			h.mergeQueuedDown(ws, m)

			// Assert
			if killed := h.stand.Killed(); len(killed) != 0 {
				t.Fatalf("kills = %+v, want none", killed)
			}
			if h.cyclePending(ws) {
				t.Fatal("a park not taken stayed pending")
			}
			rec, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "info", Operation: opMergeQueued, Message: tt.message})
			if !ok {
				t.Fatalf("records = %+v, want %q at info", records(h.log, opMergeQueued), tt.message)
			}
			if rec.Context["workspace"] != string(ws) || rec.Context["merge_entry"] != string(m.ledger) {
				t.Fatalf("record context = %v, want the workspace and the queue entry", rec.Context)
			}
		})
	}
}

// TestAPendingMergeQueuedParkAndAnUnscopedAsk covers a sync merge's park
// pending under a busy session while another asker (the shim hold's idle
// park) asks too: neither ask is lost to the other, and the merge's park,
// which outranks it, is the one taken while its entry waits.
func TestAPendingMergeQueuedParkAndAnUnscopedAsk(t *testing.T) {
	tests := []struct {
		name string
		// ask puts both asks in, under the busy session, in their order.
		ask func(h *harness, ws ids.WorkspaceID, m queuedMerge)
		// leave takes the merge out of line before the session falls free.
		leave    bool
		wantLife shimlife.StandDownReason
		wantOp   string
		wantDone string
	}{
		{
			name: "an idle ask after the merge's park leaves the merge's park in force",
			ask: func(h *harness, ws ids.WorkspaceID, m queuedMerge) {
				h.mergeQueuedDown(ws, m)
				h.c.IdleDown(ws, beatAfter)
			},
			wantLife: shimlife.StandDownMergeQueued, wantOp: opMergeQueued, wantDone: mergeQueuedReason.done.Template(),
		},
		{
			name: "a merge's park after an idle ask takes its place",
			ask: func(h *harness, ws ids.WorkspaceID, m queuedMerge) {
				h.c.IdleDown(ws, beatAfter)
				h.mergeQueuedDown(ws, m)
			},
			wantLife: shimlife.StandDownMergeQueued, wantOp: opMergeQueued, wantDone: mergeQueuedReason.done.Template(),
		},
		{
			name: "the idle ask is taken once the merge left its line",
			ask: func(h *harness, ws ids.WorkspaceID, m queuedMerge) {
				h.mergeQueuedDown(ws, m)
				h.c.IdleDown(ws, beatAfter)
			},
			leave:    true,
			wantLife: shimlife.StandDownIdle, wantOp: opIdlePark, wantDone: idleParkReason.done.Template(),
		},
		{
			name: "an idle ask outranked before the merge's park is taken once the merge left its line",
			ask: func(h *harness, ws ids.WorkspaceID, m queuedMerge) {
				h.c.IdleDown(ws, beatAfter)
				h.mergeQueuedDown(ws, m)
			},
			leave:    true,
			wantLife: shimlife.StandDownIdle, wantOp: opIdlePark, wantDone: idleParkReason.done.Template(),
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws := h.workspace(t, instant)
			m := queuedAt(t, h, ws, instant)
			h.freeness.SetFree(ws, false)
			tt.ask(h, ws, m)
			if killed := h.stand.Killed(); len(killed) != 0 {
				t.Fatalf("kills = %+v, want none under the turn", killed)
			}
			if tt.leave {
				if err := h.db.RemoveMergeQueueEntry(context.Background(), m.repo, ws, "user_dequeue"); err != nil {
					t.Fatalf("RemoveMergeQueueEntry: %v", err)
				}
			}
			h.freeness.SetFree(ws, true)

			// Act
			h.c.RetryWithheld(ws)

			// Assert
			if killed := h.stand.Killed(); len(killed) != 1 || killed[0].Life != tt.wantLife {
				t.Fatalf("kills = %+v, want one %v park", killed, tt.wantLife)
			}
			if _, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "info", Operation: tt.wantOp, Message: tt.wantDone}); !ok {
				t.Fatalf("records = %+v, want %q under %s", records(h.log, tt.wantOp), tt.wantDone, tt.wantOp)
			}
			if h.cyclePending(ws) {
				t.Fatal("a park taken stayed pending")
			}
		})
	}
}

// mergeParkErrDB fails the one read a sync merge's park makes that READ
// names.
type mergeParkErrDB struct {
	wsm.DB
	read string
}

func (d mergeParkErrDB) Workspace(ctx context.Context, id wsm.WorkspaceID) (wsm.Workspace, error) {
	if d.read == "workspace" {
		return wsm.Workspace{}, errFake
	}
	return d.DB.Workspace(ctx, id)
}

func (d mergeParkErrDB) RemoteControl(ctx context.Context, id wsm.WorkspaceID) (wsm.RemoteControl, error) {
	if d.read == "remote" {
		return wsm.RemoteControl{}, errFake
	}
	return d.DB.RemoteControl(ctx, id)
}

func (d mergeParkErrDB) Session(ctx context.Context, id wsm.WorkspaceID) (wsm.Session, bool, error) {
	if d.read == "session" {
		return wsm.Session{}, false, errFake
	}
	return d.DB.Session(ctx, id)
}

func (d mergeParkErrDB) MergeQueue(ctx context.Context, repo wsm.RepoKey) ([]wsm.MergeQueueEntry, error) {
	if d.read == "queue" {
		return nil, errFake
	}
	return d.DB.MergeQueue(ctx, repo)
}

func TestAMergeQueuedParkThatFailsIsDroppedAndRecorded(t *testing.T) {
	tests := []struct {
		name    string
		read    string
		hibern  bool
		message string
	}{
		{name: "the workspace's record", read: "workspace", message: "could not read the workspace's record; its stand-down is dropped"},
		{name: "the remote-control mode", read: "remote", message: "could not read the workspace's remote-control mode; its stand-down is dropped"},
		{name: "the session", read: "session", message: "could not read the workspace's session; its stand-down is dropped"},
		{name: "the merge queue", read: "queue", message: "could not read whether the workspace's merge still waits in line; its park is dropped"},
		{name: "the hibernate directive", hibern: true, message: "the hibernate directive failed; deferring the park"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws := h.workspace(t, instant)
			m := queuedAt(t, h, ws, instant)
			h.c.deps.DB = mergeParkErrDB{DB: h.c.deps.DB, read: tt.read}
			if tt.hibern {
				h.stand.hibernateErr[ws] = errFake
			}

			// Act
			h.mergeQueuedDown(ws, m)

			// Assert
			if killed := h.stand.Killed(); len(killed) != 0 {
				t.Fatalf("kills = %+v, want none", killed)
			}
			if h.cyclePending(ws) {
				t.Fatal("a dropped park stayed pending")
			}
			if term := h.terminal(t, ws); term != nil {
				t.Fatalf("terminal = %+v, want none recorded", term)
			}
			rec, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: opMergeQueued, Message: tt.message})
			if !ok {
				t.Fatalf("records = %+v, want %q at error", records(h.log, opMergeQueued), tt.message)
			}
			if rec.Context["workspace"] != string(ws) || rec.Context["merge_entry"] != string(m.ledger) || rec.Context["repo"] != string(m.repo) ||
				!strings.Contains(fmt.Sprint(rec.Context["cause"]), errFake.Error()) {
				t.Fatalf("record context = %v, want the workspace, the queue entry and the cause", rec.Context)
			}
		})
	}
}

func TestAMergeQueuedParkLeavesTheOrdinaryParkARestoreReads(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant)
	m := queuedAt(t, h, ws, instant)

	// Act
	h.mergeQueuedDown(ws, m)

	// Assert: the keeper restores a parked session before each beat it owes
	// (wsm.Session.Parked), so the park must be the ordinary one.
	session, found, err := h.db.Session(context.Background(), ws)
	if err != nil || !found || !session.Parked() {
		t.Fatalf("session = %+v (found %v, err %v), want the ordinary park", session, found, err)
	}
}

func TestABusyCycleStandDownHoldsDeferredBusyUntilItIsTaken(t *testing.T) {
	// Arrange
	why := sessionwhy.NewTable()
	h := newHarness(t, func(d *Deps) { d.Why = why })
	ws := h.workspace(t, instant)
	h.freeness.SetFree(ws, false)

	// Act: held back busy, then taken once the session falls free.
	h.c.CycleDown(ws, beatAfter)
	held := why.Facts(ws).Hold
	h.freeness.SetFree(ws, true)
	h.c.RetryWithheld(ws)

	// Assert
	if held != sessionwhy.HoldDeferredBusy {
		t.Fatalf("hold while busy = %q, want %q", held, sessionwhy.HoldDeferredBusy)
	}
	if got := why.Facts(ws).Hold; got != "" {
		t.Fatalf("hold after the park = %q, want none", got)
	}
}

func TestAStandDownAskedBeforeABringUpForWorkIsNotTaken(t *testing.T) {
	tests := []struct {
		name      string
		bringUp   func(s *wsm.Session)
		wantKills int
	}{
		{name: "a session brought up for a turn not yet closed is kept up", bringUp: func(s *wsm.Session) { s.WorkBroughtUpAt = &beatAfter }},
		{name: "a session the user restarted moments ago is kept up", bringUp: func(s *wsm.Session) { up := beatAfter; s.UserBroughtUpAt = &up }},
		{name: "a session its bring-up no longer holds is parked", bringUp: func(*wsm.Session) {}, wantKills: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the gate's cold park was asked; the session then came up.
			h := newHarness(t)
			ws := h.workspace(t, instant)
			session, _, err := h.db.Session(context.Background(), ws)
			if err != nil {
				t.Fatalf("Session: %v", err)
			}
			tt.bringUp(&session)
			if err := h.db.PutSession(context.Background(), session); err != nil {
				t.Fatalf("PutSession: %v", err)
			}
			h.clock.Set(beatAfter.Add(time.Minute))

			// Act
			h.c.ColdDown(ws, beatAfter, true)

			// Assert
			if killed := h.stand.Killed(); len(killed) != tt.wantKills {
				t.Fatalf("kills = %+v, want %d", killed, tt.wantKills)
			}
		})
	}
}
