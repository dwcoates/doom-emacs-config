package rollout

import (
	"context"
	"errors"
	"slices"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/sessionlock"
	"agent-repld/internal/wsm"
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
	h.c.enterJoiningLocked()
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

func TestTheTakeoverOfAFreeLock(t *testing.T) {
	tests := []struct {
		name      string
		parked    bool
		listening bool
		wantStood bool
	}{
		{name: "stands down the listening inert survivor of a shim that was not up", parked: true, listening: true, wantStood: true},
		{name: "holds nothing for a parked workspace no shim listens for", parked: true},
		{name: "stands down the listening inert survivor of a record that still reads running", listening: true, wantStood: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws := orphan(t, h)
			record, err := h.db.Workspace(context.Background(), ws)
			if err != nil {
				t.Fatalf("Workspace: %v", err)
			}
			if tt.parked {
				if err := h.db.SetSessionTerminal(context.Background(), ws, wsm.SessionTerminal{Kind: wsm.TerminalParked, At: time.Now()}); err != nil {
					t.Fatalf("SetSessionTerminal: %v", err)
				}
			}
			h.mu.Lock()
			h.lockStates[record.Dir] = sessionlock.StateFree
			h.mu.Unlock()
			h.fleet.mu.Lock()
			h.fleet.listening[ws] = tt.listening
			h.fleet.mu.Unlock()

			// Act
			takeOver(h)

			// Assert
			_, stood := h.fleet.InertAdoptionsAndStandDowns()
			if got := slices.Contains(stood, ws); got != tt.wantStood {
				t.Fatalf("stood down %v, want the survivor stood down: %v", stood, tt.wantStood)
			}
		})
	}
}

func TestTheTakeoverLeavesAClosedWorkspaceAlone(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := orphan(t, h)
	if err := h.db.MarkClosed(context.Background(), ws, wsm.Closure{At: time.Now(), By: wsm.CloseByUser}); err != nil {
		t.Fatalf("MarkClosed: %v", err)
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
	h := newFileHarness(t)
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
		t.Fatalf("serving owner = %q, want this daemon: the takeover promotes before it claims", ids.Optional(owner))
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

// TestTheTakeoverStartsNoSessionForAFreeLock pins the owner's ruling of
// 2026-11-23, which replaced the 2026-10-02 one: a free lock is no session
// running, so the takeover starts none, and a record that still reads
// running has its end recorded.
func TestTheTakeoverStartsNoSessionForAFreeLock(t *testing.T) {
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
	if got := h.Started(); len(got) != 0 {
		t.Fatalf("started = %v, want none for a free lock", got)
	}
	session, _, err := h.db.Session(context.Background(), ws)
	if err != nil {
		t.Fatalf("Session: %v", err)
	}
	if session.Terminal == nil || session.Terminal.Detail != staleDetail {
		t.Fatalf("terminal = %+v, want the stale record's end recorded", session.Terminal)
	}
}

func TestTheTakeoverStartsTheSessionOfAFreeLockAtAColdGate(t *testing.T) {
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
	h.c.deps.DB = coldGatedDB{DB: h.db, gated: []wsm.WorkspaceID{ws}}

	// Act
	takeOver(h)

	// Assert
	if got := h.Started(); !slices.Equal(got, []ids.WorkspaceID{ws}) {
		t.Fatalf("started = %v, want the workspace at its cold gate started", got)
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
	if err := h.db.MarkClosed(context.Background(), ws, wsm.Closure{At: time.Now(), By: wsm.CloseByUser}); err != nil {
		t.Fatalf("MarkClosed: %v", err)
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

// lockOf arranges WS's lock to read STATE to the takeover's probe.
func lockOf(t *testing.T, h *harness, ws ids.WorkspaceID, state sessionlock.State) {
	t.Helper()
	record, err := h.db.Workspace(context.Background(), ws)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	h.mu.Lock()
	defer h.mu.Unlock()
	h.lockStates[record.Dir] = state
}

// EACH ROW NOBODY HANDED OVER IS SETTLED BY THE ORPHAN PASS'S OWN DECISION
// FOR IT, once (sidebar carry.go): after its start for a row it starts, after
// its adoption for a shim it recovers, and at once for a row it starts
// nothing for.
func TestTheTakeoverSettlesEachRowItDecidesOnce(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(t *testing.T, h *harness) ids.WorkspaceID
		// started is whether the row's settle must follow its start.
		started bool
	}{
		{name: "a row at a cold gate after its start raised the gate", started: true, arrange: func(t *testing.T, h *harness) ids.WorkspaceID {
			ws := orphan(t, h)
			lockOf(t, h, ws, sessionlock.StateFree)
			h.c.deps.DB = coldGatedDB{DB: h.db, gated: []wsm.WorkspaceID{ws}}
			return ws
		}},
		{name: "a row whose shim was not up at once", arrange: func(t *testing.T, h *harness) ids.WorkspaceID {
			ws := orphan(t, h)
			lockOf(t, h, ws, sessionlock.StateFree)
			return ws
		}},
		{name: "a row whose orphaned shim is adopted", arrange: func(t *testing.T, h *harness) ids.WorkspaceID {
			return orphan(t, h)
		}},
		{name: "a row this daemon already serves", arrange: func(t *testing.T, h *harness) ids.WorkspaceID {
			ws, _ := h.workspace(t)
			return ws
		}},
		{name: "a closed row", arrange: func(t *testing.T, h *harness) ids.WorkspaceID {
			ws := orphan(t, h)
			if err := h.db.MarkClosed(context.Background(), ws, wsm.Closure{At: time.Now(), By: wsm.CloseByUser}); err != nil {
				t.Fatalf("MarkClosed: %v", err)
			}
			return ws
		}},
		{name: "a row whose lock could not be read", arrange: func(t *testing.T, h *harness) ids.WorkspaceID {
			ws := orphan(t, h)
			lockOf(t, h, ws, sessionlock.StateUnknown)
			return ws
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws := tc.arrange(t, h)

			// Act
			takeOver(h)

			// Assert
			if got := h.Settled(); !slices.Equal(got, []ids.WorkspaceID{ws}) {
				t.Fatalf("settled = %v, want the row settled once", got)
			}
			taken := h.order.Taken()
			if started, settled := indexOf(taken, "start_session"), indexOf(taken, "facts_settled"); tc.started && (started < 0 || settled < started) {
				t.Fatalf("steps = %v, want the row settled after its start", taken)
			}
		})
	}
}

// A row handed over is its adoption's to settle: the takeover's orphan pass
// settles nothing for it.
func TestTheTakeoverSettlesNoHandedOverRow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := orphan(t, h)
	lockOf(t, h, ws, sessionlock.StateFree)
	h.c.mu.Lock()
	h.c.joining = map[ids.WorkspaceID]bool{ws: true}
	h.c.owned = map[ids.WorkspaceID]bool{ws: true}
	h.c.mu.Unlock()

	// Act
	takeOver(h)

	// Assert
	if got := h.Settled(); len(got) != 0 {
		t.Fatalf("settled = %v, want nothing settled for a row its adoption owns", got)
	}
}

// WHILE AN OLDER DAEMON STILL RUNS, NO ROW NOBODY HANDED OVER IS SETTLED: the
// orphan pass that decides it waits for the older daemons, and the takeover's
// own settle settles no row (2026-11-25 12:32:56, three rows at a cold gate
// drawn `done` for seven seconds).
func TestATakeoverWithAnElderRunningSettlesNoRowUntilTheOrphanPassDecidesIt(t *testing.T) {
	// Arrange
	release := make(chan struct{})
	h := newHarness(t, withElder(release))
	ws := orphan(t, h)
	lockOf(t, h, ws, sessionlock.StateFree)
	h.c.deps.DB = coldGatedDB{DB: h.db, gated: []wsm.WorkspaceID{ws}}

	// Act: the takeover settles with the elder still running.
	takeOver(h)
	<-h.servingSettled

	// Assert
	if got := h.Settled(); len(got) != 0 {
		t.Fatalf("settled = %v, want no row settled while the elder runs", got)
	}

	// Act: the elder ends.
	close(release)
	<-h.c.eldersGoneSignal()

	// Assert
	if got := h.Settled(); !slices.Equal(got, []ids.WorkspaceID{ws}) {
		t.Fatalf("settled = %v, want the row settled once its start ended", got)
	}
}

// WHEN THE ORPHAN PASS WILL NOT RUN, every row nobody handed over is settled
// on the event that says so, and a handed-over row is still left to its
// adoption: nothing more will be established for the rest, and kept carried
// they would draw the incumbent's last arm for good.
func TestARowTheOrphanPassWillNotDecideIsSettledOnTheEventThatSaysSo(t *testing.T) {
	cases := []struct {
		name    string
		deps    func(d *Deps)
		arrange func(h *harness)
		record  string
	}{
		{
			name:    "a takeover that cannot promote its state handle",
			arrange: func(h *harness) { h.c.deps.DB = refusingPromote{DB: h.db} },
			record:  "the state handle could not be promoted to writing",
		},
		{
			name: "a takeover whose wait for the older daemons fails",
			deps: func(d *Deps) {
				d.LiveElders = func() ([]string, error) { return []string{"elder"}, nil }
				d.AwaitElders = func(context.Context, []string) error {
					return errors.New("arranged: the life locks could not be watched")
				}
			},
			arrange: func(*harness) {},
			record:  "the wait for the older daemons failed",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			settles := make(chan ids.WorkspaceID, 8)
			opts := []func(d *Deps){func(d *Deps) {
				record := d.FactsSettled
				d.FactsSettled = func(ws ids.WorkspaceID) {
					record(ws)
					settles <- ws
				}
			}}
			if tc.deps != nil {
				opts = append(opts, tc.deps)
			}
			h := newHarness(t, opts...)
			undecided := orphan(t, h)
			handed, _ := h.workspace(t)
			h.c.mu.Lock()
			h.c.joining = map[ids.WorkspaceID]bool{handed: true}
			h.c.owned = map[ids.WorkspaceID]bool{handed: true}
			h.c.mu.Unlock()
			tc.arrange(h)

			// Act
			takeOver(h)
			<-settles

			// Assert
			if got := h.Settled(); !slices.Equal(got, []ids.WorkspaceID{undecided}) {
				t.Fatalf("settled = %v, want only the row nobody handed over", got)
			}
			rec, found := dlog.FindRecord(records(h.log, opOrphans), dlog.RecordQuery{Message: "the orphan pass will not run; the rows it would have decided are drawn from this daemon's own facts"})
			if !found || rec.Level != dlog.LevelInfo || rec.Context["why"] != tc.record || rec.Context["workspaces"] != 1 {
				t.Fatalf("record = %+v (found %v), want one INFO record naming why, for one row", rec, found)
			}
		})
	}
}
