package rollout

import (
	"context"
	"errors"
	"slices"
	"testing"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/wsm"
)

// THE TAKEOVER RECOVERS WHAT NOTHING ADOPTED. A cold-started daemon's handover
// used to list none of its sessions, so its successor took over with their
// shims still running, locks held, served by no daemon (2026-09-24).

// orphan arranges a workspace whose shim a gone daemon left running: its lock
// is held, its serving row names a dead instance, and THIS daemon holds no
// client for it and was handed nothing for it.
func orphan(t *testing.T, h *harness) ids.WorkspaceID {
	t.Helper()
	ws, _ := h.workspace(t)
	if err := h.db.ClaimServing(context.Background(), ws, ids.InstanceID("a-dead-instance")); err != nil {
		t.Fatalf("ClaimServing: %v", err)
	}
	h.fleet.mu.Lock()
	delete(h.fleet.live, ws)
	h.fleet.mu.Unlock()
	return ws
}

// takeOver runs the takeover of a joining successor and waits for every
// adoption it started.
func takeOver(h *harness) {
	h.c.mu.Lock()
	h.c.joiningMode = true
	h.c.mu.Unlock()
	h.c.becomeIncumbent(nil)
	h.c.stragglerAdoptions.Wait()
}

func TestTheTakeoverAdoptsAShimNothingAdopted(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := orphan(t, h)

	// Act
	takeOver(h)

	// Assert
	if got := h.fleet.Adoptions(); !slices.Equal(got, []ids.WorkspaceID{ws}) {
		t.Fatalf("adoptions = %v, want the orphan adopted", got)
	}
}

func TestTheTakeoverClaimsTheRecoveredOrphansWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := orphan(t, h)

	// Act
	takeOver(h)

	// Assert
	owner, err := h.db.Serving(context.Background(), ws)
	if err != nil {
		t.Fatalf("Serving: %v", err)
	}
	if owner == nil || *owner != selfInstance {
		t.Fatalf("serving owner = %v, want this daemon so its next handover hands it over", owner)
	}
}

func TestTheTakeoverDialsNoShimForAFreeLock(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := orphan(t, h)
	record, err := h.db.Workspace(context.Background(), ws)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	h.mu.Lock()
	h.lockStates[record.Dir] = sessionlock.StateFree
	h.mu.Unlock()

	// Act
	takeOver(h)

	// Assert
	if got := h.fleet.Adoptions(); len(got) != 0 {
		t.Fatalf("adoptions = %v, want nothing dialed for a lock no shim holds", got)
	}
}

func TestTheTakeoverLeavesAClosedWorkspaceAlone(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := orphan(t, h)
	if err := h.db.SetClosed(context.Background(), ws, true); err != nil {
		t.Fatalf("SetClosed: %v", err)
	}

	// Act
	takeOver(h)

	// Assert
	if got := h.fleet.Adoptions(); len(got) != 0 {
		t.Fatalf("adoptions = %v, want a closed workspace left alone", got)
	}
}

func TestTheTakeoverDoesNotReadAnUnreadableLockAsHeld(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := orphan(t, h)
	record, err := h.db.Workspace(context.Background(), ws)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	h.mu.Lock()
	h.lockStates[record.Dir] = sessionlock.StateUnknown
	h.lockErr[record.Dir] = errors.New("arranged: the lock could not be read")
	h.mu.Unlock()

	// Act
	takeOver(h)

	// Assert
	if got := h.fleet.Adoptions(); len(got) != 0 {
		t.Fatalf("adoptions = %v, want nothing adopted on a lock that could not be read", got)
	}
	if warns := levelRecords(records(h.log, opOrphans), dlog.LevelWarn); len(warns) != 1 {
		t.Fatalf("orphan WARN records = %d, want exactly one naming the unreadable lock", len(warns))
	}
}

func TestAnOrphanThatCannotBeAdoptedIsLoudAndNotKilled(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := orphan(t, h)
	h.fleet.mu.Lock()
	h.fleet.adoptErr[ws] = errors.New("arranged: the dial was refused")
	h.fleet.mu.Unlock()

	// Act
	takeOver(h)

	// Assert
	if errs := levelRecords(records(h.log, opOrphans), dlog.LevelError); len(errs) != 1 {
		t.Fatalf("orphan ERROR records = %d, want exactly one", len(errs))
	}
	if steps := h.order.Taken(); slices.Contains(steps, "force_kill") || slices.Contains(steps, "kill_session") {
		t.Fatalf("steps = %v, want no stop of any kind attempted", steps)
	}
}

func TestTheTakeoverLeavesAHandedOverWorkspaceToItsRendezvous(t *testing.T) {
	// Arrange: the manifest named this workspace, so the rendezvous (or the
	// straggler adoption) is what brings it over, never the orphan sweep.
	h := newHarness(t)
	ws := orphan(t, h)
	h.c.mu.Lock()
	h.c.joining = map[ids.WorkspaceID]bool{ws: true}
	h.c.owned = map[ids.WorkspaceID]bool{ws: true}
	h.c.mu.Unlock()

	// Act
	takeOver(h)

	// Assert
	if got := records(h.log, opOrphans); len(levelRecords(got, dlog.LevelInfo)) != 0 {
		t.Fatalf("orphan INFO records = %+v, want the sweep to leave a handed-over workspace alone", got)
	}
}

func TestTheTakeoverClaimsAnOrphanOnTheJoiningReadOnlyHandle(t *testing.T) {
	// Arrange: a successor handed NOTHING never promoted its handle inside
	// an adoption, so the takeover is the first writer it has (live deploy
	// 2026-09-24 15:07, `wsm: handle is read-only` x3).
	h := newHarness(t)
	ws := orphan(t, h)
	h.joiningHandle(t)

	// Act
	takeOver(h)

	// Assert
	owner, err := h.db.Serving(context.Background(), ws)
	if err != nil {
		t.Fatalf("Serving: %v", err)
	}
	if owner == nil || *owner != selfInstance {
		t.Fatalf("serving owner = %q, want this daemon: the takeover promotes before it claims", ownerText(owner))
	}
}

// refusingPromote is a state client whose promotion fails.
type refusingPromote struct{ wsm.DB }

func (refusingPromote) Promote(context.Context) error {
	return errors.New("arranged: the writing handle could not be opened")
}

func TestATakeoverThatCannotPromoteRecoversNoOrphan(t *testing.T) {
	// Arrange
	h := newHarness(t)
	orphan(t, h)
	h.c.deps.DB = refusingPromote{DB: h.db}

	// Act
	takeOver(h)

	// Assert
	if got := h.fleet.Adoptions(); len(got) != 0 {
		t.Fatalf("adoptions = %v, want no shim adopted whose claim could not be written", got)
	}
	if errs := levelRecords(records(h.log, opAdopt), dlog.LevelError); len(errs) != 1 {
		t.Fatalf("takeover ERROR records = %d, want exactly one naming the refused promotion", len(errs))
	}
}

func TestTheTakeoverStartsTheSessionOfAFreeLock(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := orphan(t, h)
	record, err := h.db.Workspace(context.Background(), ws)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	h.mu.Lock()
	h.lockStates[record.Dir] = sessionlock.StateFree
	h.mu.Unlock()

	// Act
	takeOver(h)

	// Assert
	if got := h.Started(); !slices.Equal(got, []ids.WorkspaceID{ws}) {
		t.Fatalf("started = %v, want the session-less workspace started", got)
	}
}

func TestTheTakeoverStartsNoSessionForAHeldLock(t *testing.T) {
	// Arrange
	h := newHarness(t)
	orphan(t, h)

	// Act
	takeOver(h)

	// Assert
	if got := h.Started(); len(got) != 0 {
		t.Fatalf("started = %v, want none: the orphaned shim is adopted instead", got)
	}
}

func TestTheTakeoverStartsNoSessionForALiveClient(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)

	// Act
	takeOver(h)

	// Assert
	if got := h.Started(); len(got) != 0 {
		t.Fatalf("started = %v, want none for a workspace this daemon already serves", got)
	}
}

func TestTheTakeoverStartsNoSessionForAClosedWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := orphan(t, h)
	record, err := h.db.Workspace(context.Background(), ws)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	h.mu.Lock()
	h.lockStates[record.Dir] = sessionlock.StateFree
	h.mu.Unlock()
	if err := h.db.SetClosed(context.Background(), ws, true); err != nil {
		t.Fatalf("SetClosed: %v", err)
	}

	// Act
	takeOver(h)

	// Assert
	if got := h.Started(); len(got) != 0 {
		t.Fatalf("started = %v, want none for a closed workspace", got)
	}
}

func TestTheTakeoverLeavesAHandedOverWorkspaceToItsAdoption(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := orphan(t, h)
	record, err := h.db.Workspace(context.Background(), ws)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	h.mu.Lock()
	h.lockStates[record.Dir] = sessionlock.StateFree
	h.mu.Unlock()
	h.c.mu.Lock()
	if h.c.joining == nil {
		h.c.joining = map[ids.WorkspaceID]bool{}
	}
	h.c.joining[ws] = true
	if h.c.owned == nil {
		h.c.owned = map[ids.WorkspaceID]bool{}
	}
	h.c.owned[ws] = true
	h.c.mu.Unlock()

	// Act
	takeOver(h)

	// Assert
	if got := h.Started(); len(got) != 0 {
		t.Fatalf("started = %v, want none: the handover's own adoption decides a handed-over workspace", got)
	}
}
