package drain

import (
	"context"
	"fmt"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
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
		{name: "a hidden workspace, whose lapse is the cache keeper's", arrange: func(t *testing.T, h *harness) ids.WorkspaceID {
			return h.hidden(t)
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

// A BRING-UP'S OWN ENGAGEMENT STAMP IS NO TURN: a settings pick's revival
// stamps the session's engagement after the pick (2026-11-20, 25 visits kept
// up with no turn run), and the visit still ends.
func TestAVisitStandDownStandsDownASessionOnlyItsBringUpEngaged(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, beatAfter.Add(2*time.Second))

	// Act
	h.c.VisitDown(ws, beatAfter)

	// Assert
	killed := h.stand.Killed()
	if len(killed) != 1 || killed[0].Life != shimlife.StandDownSettingsVisit {
		t.Fatalf("kills = %+v, want the visit's stand-down despite the bring-up's stamp", killed)
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
		}, ask: func(c *controller, ws ids.WorkspaceID) { c.ColdDown(ws, beatAfter) }},
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
	h.c.ColdDown(ws, beatAfter)

	// Assert
	killed := h.stand.Killed()
	if len(killed) != 1 || killed[0].WS != ws || killed[0].Force || killed[0].Life != shimlife.StandDownCacheCold {
		t.Fatalf("kills = %+v, want one graceful cache_cold stand-down", killed)
	}
	if term := h.terminal(t, ws); term == nil || term.Kind != wsm.TerminalParked || term.Detail != coldReason.detail {
		t.Fatalf("terminal = %+v, want the one park with the cold cache's detail", term)
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
			h.c.ColdDown(ws, beatAfter)

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
	h.c.ColdDown(ws, beatAfter)
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
	h.c.ColdDown(ws, beatAfter)

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
		{name: "a cold park of a hidden workspace", arrange: func(t *testing.T, h *harness) ids.WorkspaceID { return h.hidden(t) },
			ask: func(c *controller, ws ids.WorkspaceID) { c.ColdDown(ws, beatAfter) }, life: shimlife.StandDownCacheCold},
		{name: "a cold park of a closed workspace", arrange: func(t *testing.T, h *harness) ids.WorkspaceID {
			ws := h.workspace(t, instant)
			h.closedAt(t, ws)
			return ws
		}, ask: func(c *controller, ws ids.WorkspaceID) { c.ColdDown(ws, beatAfter) }, life: shimlife.StandDownCacheCold},
		{name: "a visit of a hidden workspace", arrange: func(t *testing.T, h *harness) ids.WorkspaceID { return h.hidden(t) },
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
