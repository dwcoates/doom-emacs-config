package cachelapse

import (
	"context"
	"errors"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// visitAt is when a settings pick started the harness workspace's shim.
var visitAt = now.Add(-time.Minute)

// warm is a visible workspace whose last turn ended ten minutes ago, so a
// beat is owed.
func warm() wsm.Workspace {
	rec := visible()
	ended := now.Add(-10 * time.Minute)
	rec.LastTurnEndedAt = &ended
	return rec
}

// knownCold is a visible workspace whose cache was marked cold at its last
// turn's end, so no beat is owed: a held keep-alive cadence's lapse.
func knownCold() wsm.Workspace {
	rec := warm()
	rec.CacheColdTurnAt = rec.LastTurnEndedAt
	return rec
}

// visited is REC with a settings visit begun at visitAt.
func visited(rec wsm.Workspace) wsm.Workspace {
	at := visitAt
	rec.SettingsVisitAt = &at
	return rec
}

// reconcile applies one reconcile edge to the harness workspace.
func (h *harness) reconcile(why string) {
	h.keeper.apply(context.Background(), edge{ws: theWorkspace, why: why})
}

// ownerRecords are the INFO records of the workspace's owner changing.
func (h *harness) ownerRecords() []dlog.Record {
	var out []dlog.Record
	for _, r := range h.log.Records() {
		if r.Operation == opOwner && r.Level == "info" {
			out = append(out, r)
		}
	}
	return out
}

func TestAHeldKeepaliveIdleSessionIsParkedWhenItsCacheIsCold(t *testing.T) {
	// Arrange
	h := newHarness(t, knownCold())

	// Act
	h.reconcile("a session came up")

	// Assert
	if len(h.coldDowns) != 1 || h.coldDowns[0].ws != theWorkspace || !h.coldDowns[0].beat.Equal(now) {
		t.Fatalf("cold parks = %+v, want one for the workspace at %v", h.coldDowns, now)
	}
	if len(h.cycled) != 0 || len(h.visitDowns) != 0 {
		t.Fatalf("cycle %+v, visit %+v: want only the cold park", h.cycled, h.visitDowns)
	}
	records := h.ownerRecords()
	if len(records) != 1 || records[0].Context["owner"] != ownerCold || records[0].Message != "the session's stand-down owner changed" {
		t.Fatalf("owner records = %+v, want one naming %s", records, ownerCold)
	}
}

func TestEachOwnerIsGivenByWhyNoBeatIsOwed(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		want    string
	}{
		{name: "a warm cache owes a beat", arrange: func(h *harness) { h.db.records[theWorkspace] = warm() }, want: ownerCycle},
		{name: "a cache with nothing warm to keep", arrange: func(h *harness) {}, want: ownerCold},
		{name: "a run at its break-even", arrange: func(h *harness) {
			rec := warm()
			rec.KeepaliveRunBeats = KeepaliveRunCap
			h.db.records[theWorkspace] = rec
		}, want: ownerCold},
		{name: "a hidden workspace whose lapse is armed", arrange: func(h *harness) {
			h.db.records[theWorkspace] = hidden()
			h.keeper.deadlines[theWorkspace] = lapseAt
		}, want: ownerLapse},
		{name: "a hidden workspace with no lapse armed", arrange: func(h *harness) { h.db.records[theWorkspace] = hidden() }, want: ownerCold},
		{name: "a closed workspace whose last beat is spent", arrange: func(h *harness) {
			rec := warm()
			rec.Closed = true
			rec.Closure = &wsm.Closure{At: now.Add(-time.Hour)}
			h.db.records[theWorkspace] = rec
		}, want: ownerCold},
		{name: "a session idle past the cutoff before its beat", arrange: func(h *harness) {
			h.db.records[theWorkspace] = warm()
			session := liveSession(theWorkspace)
			session.LastEngagementAt = now.Add(-6 * time.Hour)
			h.db.sessions[theWorkspace] = session
		}, want: ownerSweep},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, visible())
			tt.arrange(h)

			// Act
			h.reconcile("a session came up")

			// Assert
			if got := h.keeper.owners[theWorkspace]; got.owner != tt.want || got.up {
				t.Fatalf("owner = %+v, want %s", got, tt.want)
			}
			if parked := len(h.coldDowns) == 1; parked != (tt.want == ownerCold) {
				t.Fatalf("cold parks = %+v, want one only for the cold park", h.coldDowns)
			}
		})
	}
}

func TestAnOwnerThatStandsIsRecordedAtInfoOnce(t *testing.T) {
	// Arrange
	h := newHarness(t, knownCold())
	h.reconcile("a session came up")

	// Act
	h.reconcile("the session fell free")

	// Assert
	if records := h.ownerRecords(); len(records) != 1 {
		t.Fatalf("owner records = %+v, want the one change", records)
	}
	if len(h.coldDowns) != 2 {
		t.Fatalf("cold parks = %+v, want the park asked on each reconcile", h.coldDowns)
	}
}

func TestAnAdoptedSessionKeepsItsOwnerAcrossAHandover(t *testing.T) {
	tests := []struct {
		name   string
		rec    wsm.Workspace
		assert func(t *testing.T, h *harness)
	}{
		{name: "a cold cache", rec: knownCold(), assert: func(t *testing.T, h *harness) {
			if len(h.coldDowns) != 1 {
				t.Fatalf("cold parks = %+v, want the successor's park", h.coldDowns)
			}
		}},
		{name: "a settings visit", rec: visited(knownCold()), assert: func(t *testing.T, h *harness) {
			if len(h.visitDowns) != 1 || !h.visitDowns[0].beat.Equal(visitAt) {
				t.Fatalf("visit stand-downs = %+v, want the successor's at the pick", h.visitDowns)
			}
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the predecessor owned the session, and handed it over;
			// the successor shares only the store.
			before := newHarness(t, tt.rec)
			before.reconcile("a session came up")
			after := newHarness(t, tt.rec)
			after.db = before.db
			after.keeper.deps.DB = before.db
			want := before.keeper.owners[theWorkspace]

			// Act: the adoption's session coming up.
			after.keeper.SessionUp(theWorkspace)
			after.applyQueued()

			// Assert
			if got := after.keeper.owners[theWorkspace]; got != want {
				t.Fatalf("successor's owner = %+v, want the predecessor's %+v", got, want)
			}
			tt.assert(t, after)
		})
	}
}

func TestAnAPISettingsVisitIsStoodDownWithNoSwitchAway(t *testing.T) {
	// Arrange: the pick is recorded before its bring-up; nobody is looking
	// at the workspace.
	h := newHarness(t, warm())
	h.keeper.SettingsVisitStarted(theWorkspace, visitAt)

	// Act: the revival's session comes up.
	h.applyQueued()

	// Assert
	if len(h.visitDowns) == 0 || h.visitDowns[len(h.visitDowns)-1] != (cycleCall{ws: theWorkspace, beat: visitAt}) {
		t.Fatalf("visit stand-downs = %+v, want one since the pick", h.visitDowns)
	}
	if got := h.keeper.owners[theWorkspace].owner; got != ownerVisit {
		t.Fatalf("owner = %q, want %s", got, ownerVisit)
	}
	if !h.found(dlog.RecordQuery{Level: "info", Operation: opVisit, Message: "a settings pick started the workspace's shim; the visit owns its stand-down until it ends"}) {
		t.Fatal("the visit's start was not recorded at info")
	}
}

func TestASettingsVisitThatCannotBeRecordedIsAnError(t *testing.T) {
	// Arrange
	h := newHarness(t, warm())
	h.db.visitErr = errors.New("store unreachable")

	// Act
	h.keeper.SettingsVisitStarted(theWorkspace, visitAt)

	// Assert
	if !h.found(dlog.RecordQuery{Level: "error", Operation: opVisit}) {
		t.Fatal("the failed visit was not recorded at error")
	}
	if h.db.records[theWorkspace].SettingsVisitAt != nil || len(h.keeper.take()) != 0 {
		t.Fatal("a visit that could not be recorded left a mark or an edge")
	}
}

func TestARevivalThatOnlyDeliversHeldPromptsIsNoRealTurn(t *testing.T) {
	tests := []struct {
		name      string
		arrange   func(h *harness)
		wantVisit bool
	}{
		{name: "the bring-up stamped the session's engagement after the pick", arrange: func(h *harness) {
			session := liveSession(theWorkspace)
			session.LastEngagementAt = visitAt.Add(2 * time.Second)
			h.db.sessions[theWorkspace] = session
		}, wantVisit: true},
		{name: "a delivered prompt's turn closed after the pick", arrange: func(h *harness) {
			rec := h.db.records[theWorkspace]
			ended := visitAt.Add(30 * time.Second)
			rec.LastTurnEndedAt = &ended
			h.db.records[theWorkspace] = rec
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, visited(warm()))
			tt.arrange(h)

			// Act
			h.reconcile("a session came up")

			// Assert
			if asked := len(h.visitDowns) == 1; asked != tt.wantVisit {
				t.Fatalf("visit stand-downs = %+v, want asked %v", h.visitDowns, tt.wantVisit)
			}
			if ended := h.db.records[theWorkspace].SettingsVisitAt == nil; ended == tt.wantVisit {
				t.Fatalf("visit ended = %v, want it ended only after a real turn", ended)
			}
		})
	}
}

func TestARealTurnHandsTheVisitsSessionToTheCycle(t *testing.T) {
	// Arrange
	rec := visited(warm())
	ended := visitAt.Add(30 * time.Second)
	rec.LastTurnEndedAt = &ended
	h := newHarness(t, rec)

	// Act
	h.reconcile("a turn's close was recorded")

	// Assert
	if got := h.keeper.owners[theWorkspace].owner; got != ownerCycle {
		t.Fatalf("owner = %q, want %s", got, ownerCycle)
	}
	if !h.found(dlog.RecordQuery{Level: "info", Operation: opVisit, Message: "the settings visit ended"}) {
		t.Fatal("the visit's end was not recorded at info")
	}
}

func TestAVisitEndsOnlyOnceItsSessionIsStoodDown(t *testing.T) {
	tests := []struct {
		name     string
		parkedAt time.Time
		wantEnd  bool
	}{
		{name: "the park the pick revived", parkedAt: visitAt.Add(-time.Hour)},
		{name: "a park after the pick", parkedAt: visitAt.Add(time.Minute), wantEnd: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, visited(warm()))
			h.serving = false
			session := liveSession(theWorkspace)
			session.Terminal = &wsm.SessionTerminal{Kind: wsm.TerminalParked, At: tt.parkedAt}
			h.db.sessions[theWorkspace] = session

			// Act
			h.reconcile("the workspace was hidden, shown, selected or deselected")

			// Assert
			if ended := h.db.records[theWorkspace].SettingsVisitAt == nil; ended != tt.wantEnd {
				t.Fatalf("visit ended = %v, want %v", ended, tt.wantEnd)
			}
			if len(h.visitDowns) != 0 || len(h.coldDowns) != 0 {
				t.Fatalf("visit %+v, cold %+v: want nothing asked of a session no shim serves", h.visitDowns, h.coldDowns)
			}
		})
	}
}

func TestTheExemptionsLeaveTheSessionUp(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		want    string
	}{
		{name: "the workspace the user is looking at", arrange: func(h *harness) {
			rec := h.db.records[theWorkspace]
			rec.Current = true
			h.db.records[theWorkspace] = rec
		}, want: upInView},
		{name: "a workspace in remote control", arrange: func(h *harness) {
			h.db.remote = map[ids.WorkspaceID]bool{theWorkspace: true}
		}, want: upRemoteControl},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a cold cache and a standing visit, either of which
			// would stand an exempt-free session down.
			h := newHarness(t, visited(knownCold()))
			tt.arrange(h)

			// Act
			h.reconcile("a session came up")

			// Assert
			if len(h.coldDowns) != 0 || len(h.visitDowns) != 0 || len(h.cycled) != 0 {
				t.Fatalf("cold %+v, visit %+v, cycle %+v: want nothing asked", h.coldDowns, h.visitDowns, h.cycled)
			}
			records := h.ownerRecords()
			if len(records) != 1 || records[0].Context["left_up"] != tt.want {
				t.Fatalf("owner records = %+v, want the session left up for %s", records, tt.want)
			}
		})
	}
}

func TestAModeTurnedOffGivesTheSessionItsOwnerAgain(t *testing.T) {
	// Arrange
	h := newHarness(t, knownCold())
	h.db.remote = map[ids.WorkspaceID]bool{theWorkspace: true}
	h.reconcile("a session came up")
	h.db.remote[theWorkspace] = false

	// Act
	h.keeper.RemoteControlChanged(theWorkspace)
	h.applyQueued()

	// Assert
	if len(h.coldDowns) != 1 {
		t.Fatalf("cold parks = %+v, want the park once the mode is off", h.coldDowns)
	}
}

func TestARemoteControlModeThatCannotBeReadLeavesTheOwnerAsItWas(t *testing.T) {
	// Arrange
	h := newHarness(t, knownCold())
	h.db.remoteErr = errors.New("store unreachable")

	// Act
	h.reconcile("a session came up")

	// Assert
	if len(h.coldDowns) != 0 {
		t.Fatalf("cold parks = %+v, want none on an unread mode", h.coldDowns)
	}
	if _, owned := h.keeper.owners[theWorkspace]; owned {
		t.Fatal("an owner was recorded from an unread mode")
	}
	if !h.found(dlog.RecordQuery{Level: "error", Operation: opOwner}) {
		t.Fatal("the unread mode was not recorded at error")
	}
}

func TestASessionNoShimServesHasNoOwner(t *testing.T) {
	// Arrange
	h := newHarness(t, knownCold())
	h.reconcile("a session came up")
	h.serving = false

	// Act
	h.reconcile("the session parked")

	// Assert
	if _, owned := h.keeper.owners[theWorkspace]; owned {
		t.Fatal("a session no shim serves kept an owner")
	}
	if !h.found(dlog.RecordQuery{Level: "info", Operation: opOwner, Message: "the session is no longer live here; it needs no stand-down owner"}) {
		t.Fatal("the dropped owner was not recorded at info")
	}
	if len(h.coldDowns) != 1 {
		t.Fatalf("cold parks = %+v, want only the live session's", h.coldDowns)
	}
}
