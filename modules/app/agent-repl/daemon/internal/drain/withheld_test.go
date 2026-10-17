package drain

import (
	"context"
	"strings"
	"testing"
	"time"

	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// hidden registers one workspace with a live, free session, and hides it.
func (h *harness) hidden(t *testing.T) ids.WorkspaceID {
	t.Helper()
	ws := h.workspace(t, instant)
	if err := h.db.SetHidden(context.Background(), ws, true, time.Unix(1_700_000_000, 0)); err != nil {
		t.Fatalf("SetHidden: %v", err)
	}
	return ws
}

// terminal reads ws's session terminal, nil when it has none.
func (h *harness) terminal(t *testing.T, ws ids.WorkspaceID) *wsm.SessionTerminal {
	t.Helper()
	session, _, err := h.db.Session(context.Background(), ws)
	if err != nil {
		t.Fatalf("Session: %v", err)
	}
	return session.Terminal
}

// pending reports whether a stand-down of ws is still pending.
func (h *harness) pending(ws ids.WorkspaceID) bool {
	h.c.mu.Lock()
	defer h.c.mu.Unlock()
	_, ok := h.c.withheld[ws]
	return ok
}

func TestNewRefusesWithoutADetachedLifetime(t *testing.T) {
	// Arrange
	h := newHarness(t)
	deps := h.c.deps
	deps.Detach = nil

	// Act
	_, err := New(deps)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "detached") {
		t.Fatalf("New = %v, want the missing detached lifetime refused", err)
	}
}

func TestAHiddenFreeWorkspaceIsStoodDownAsTheSweepDoes(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.hidden(t)

	// Act
	h.c.StandDownWithheld(ws)

	// Assert
	killed := h.stand.Killed()
	if len(killed) != 1 || killed[0].WS != ws || killed[0].Force {
		t.Fatalf("kills = %+v, want one graceful stand-down", killed)
	}
	if term := h.terminal(t, ws); term == nil || term.Kind != TerminalHibernated {
		t.Fatalf("terminal = %+v, want hibernated", term)
	}
}

func TestAWithheldStandDownRecordsItsOwnReason(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.hidden(t)

	// Act
	h.c.StandDownWithheld(ws)

	// Assert
	if term := h.terminal(t, ws); term == nil || !strings.Contains(term.Detail, "hidden, collapsed or closed") {
		t.Fatalf("terminal = %+v, want the withheld reason", term)
	}
}

func TestAWithheldStandDownClearsTheShimPids(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.hidden(t)

	// Act
	h.c.StandDownWithheld(ws)

	// Assert
	session, _, err := h.db.Session(context.Background(), ws)
	if err != nil {
		t.Fatalf("Session: %v", err)
	}
	if session.ShimPID != nil {
		t.Fatalf("shim pid = %v, want cleared", *session.ShimPID)
	}
}

func TestACollapsedWorkspaceIsStoodDown(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant)
	record, err := h.db.Workspace(context.Background(), ws)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if err := h.db.SetRepositoryFolded(context.Background(), record.Repo, true); err != nil {
		t.Fatalf("SetRepositoryFolded: %v", err)
	}

	// Act
	h.c.StandDownWithheld(ws)

	// Assert
	if killed := h.stand.Killed(); len(killed) != 1 {
		t.Fatalf("kills = %+v, want the collapsed workspace stood down", killed)
	}
}

func TestAClosedWorkspaceIsStoodDown(t *testing.T) {
	// Arrange: a closed workspace is drawn in no tab, whatever its visibility.
	h := newHarness(t)
	ws := h.workspace(t, instant)
	if err := h.db.MarkClosed(context.Background(), ws, wsm.Closure{At: instant, By: wsm.CloseByUser}); err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}

	// Act
	h.c.StandDownWithheld(ws)

	// Assert
	if killed := h.stand.Killed(); len(killed) != 1 || killed[0].WS != ws || killed[0].Force {
		t.Fatalf("kills = %+v, want the closed workspace stood down gracefully", killed)
	}
}

func TestAWorkspaceShownAgainIsNotStoodDown(t *testing.T) {
	// Arrange: asked while hidden, shown again before the attempt reads it.
	h := newHarness(t)
	ws := h.workspace(t, instant)

	// Act
	h.c.StandDownWithheld(ws)

	// Assert
	if killed := h.stand.Killed(); len(killed) != 0 {
		t.Fatalf("kills = %+v, want none for a visible workspace", killed)
	}
	if h.pending(ws) {
		t.Fatal("a visible workspace's stand-down stayed pending")
	}
}

func TestABusyWorkspaceIsNotStoodDownUnderItsTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.hidden(t)
	h.freeness.SetFree(ws, false)

	// Act
	h.c.StandDownWithheld(ws)

	// Assert
	if killed := h.stand.Killed(); len(killed) != 0 {
		t.Fatalf("kills = %+v, want none under a turn", killed)
	}
	if !h.pending(ws) {
		t.Fatal("the deferred stand-down was not kept pending")
	}
}

func TestADeferredStandDownIsTakenOnTheTurnEndEdge(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.hidden(t)
	h.freeness.SetFree(ws, false)
	h.c.StandDownWithheld(ws)
	h.freeness.SetFree(ws, true)

	// Act
	h.c.RetryWithheld(ws)

	// Assert
	if killed := h.stand.Killed(); len(killed) != 1 {
		t.Fatalf("kills = %+v, want the stand-down taken on the edge", killed)
	}
	if h.pending(ws) {
		t.Fatal("a stand-down taken stayed pending")
	}
}

func TestADeferredStandDownOfAWorkspaceShownAgainIsDropped(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.hidden(t)
	h.freeness.SetFree(ws, false)
	h.c.StandDownWithheld(ws)
	if err := h.db.SetHidden(context.Background(), ws, false, time.Unix(1_700_000_000, 0)); err != nil {
		t.Fatalf("SetHidden: %v", err)
	}
	h.freeness.SetFree(ws, true)

	// Act
	h.c.RetryWithheld(ws)

	// Assert
	if killed := h.stand.Killed(); len(killed) != 0 {
		t.Fatalf("kills = %+v, want none for a workspace shown again", killed)
	}
	if h.pending(ws) {
		t.Fatal("a dropped stand-down stayed pending")
	}
}

func TestAnEdgeForAWorkspaceWithNothingPendingStandsNothingDown(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.hidden(t)

	// Act
	h.c.RetryWithheld(ws)

	// Assert
	if killed := h.stand.Killed(); len(killed) != 0 {
		t.Fatalf("kills = %+v, want none without a request", killed)
	}
}

func TestATurnInFlightRefusalKeepsTheStandDownPending(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.hidden(t)
	h.stand.answer[ws] = &shimv1.HibernateResponse{Result: &shimv1.HibernateResponse_Error{
		Error: &shimv1.HibernateError{Kind: &shimv1.HibernateError_TurnInFlight{TurnInFlight: &shimv1.HibernateTurnInFlight{}}}}}

	// Act
	h.c.StandDownWithheld(ws)

	// Assert
	if !h.pending(ws) {
		t.Fatal("a stand-down the shim deferred was not kept pending")
	}
}

func TestAStartUnderWayKeepsTheStandDownPending(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.hidden(t)
	h.stand.startUnderWay(ws)

	// Act
	h.c.StandDownWithheld(ws)

	// Assert
	if !h.pending(ws) || len(h.stand.Killed()) != 0 {
		t.Fatal("a session still starting was stood down or forgotten")
	}
}

func TestARevivalKeepsTheStandDownPending(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.hidden(t)
	h.reviving.Set(ws, true)

	// Act
	h.c.StandDownWithheld(ws)

	// Assert
	if !h.pending(ws) || len(h.stand.Killed()) != 0 {
		t.Fatal("a session a prompt is reviving was stood down or forgotten")
	}
}

func TestAWithheldWorkspaceWithNoSessionHasNothingToStandDown(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.bare(t)
	if err := h.db.SetHidden(context.Background(), ws, true, time.Unix(1_700_000_000, 0)); err != nil {
		t.Fatalf("SetHidden: %v", err)
	}

	// Act
	h.c.StandDownWithheld(ws)

	// Assert
	if h.pending(ws) || len(h.stand.Killed()) != 0 {
		t.Fatal("a workspace with no session was stood down or kept pending")
	}
}

func TestAWithheldWorkspaceWithNoShimHasNothingToStandDown(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.hidden(t)
	h.stand.standDown(ws)

	// Act
	h.c.StandDownWithheld(ws)

	// Assert
	if h.pending(ws) || len(h.stand.Killed()) != 0 {
		t.Fatal("a workspace with no shim was stood down or kept pending")
	}
}

func TestAFailedWithheldStandDownIsRecordedAndDropped(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.hidden(t)
	h.stand.failKill(ws, errFake)

	// Act
	h.c.StandDownWithheld(ws)

	// Assert
	errs := 0
	for _, r := range records(h.log, opWithheld) {
		if r.Level == dlog.LevelError {
			errs++
		}
	}
	if errs != 1 || h.pending(ws) {
		t.Fatalf("withheld error records = %d, pending = %v; want one ERROR and nothing pending", errs, h.pending(ws))
	}
}

func TestADeferredStandDownIsStatedAtInfo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.hidden(t)
	h.freeness.SetFree(ws, false)

	// Act
	h.c.StandDownWithheld(ws)

	// Assert
	for _, r := range records(h.log, opWithheld) {
		if r.Level == dlog.LevelInfo && strings.Contains(r.Message, "waits for the turn to end") {
			return
		}
	}
	t.Fatalf("records = %+v, want an INFO saying the stand-down waits", records(h.log, opWithheld))
}

func TestAnEdgeDuringARunningAttemptRunsOneMore(t *testing.T) {
	// Arrange: the attempt is parked until the test runs it.
	var parked []func(context.Context)
	h := newHarness(t, func(d *Deps) {
		d.Detach = func(run func(context.Context)) { parked = append(parked, run) }
	})
	ws := h.hidden(t)
	h.freeness.SetFree(ws, false)
	h.c.StandDownWithheld(ws)
	h.freeness.SetFree(ws, true)

	// Act: the edge arrives while the first attempt has not run yet.
	h.c.RetryWithheld(ws)
	if len(parked) != 1 {
		t.Fatalf("attempts parked = %d, want the edge folded into the running one", len(parked))
	}
	parked[0](context.Background())

	// Assert
	if killed := h.stand.Killed(); len(killed) != 1 {
		t.Fatalf("kills = %+v, want the stand-down taken", killed)
	}
}
