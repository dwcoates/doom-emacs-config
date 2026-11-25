package rollout

import (
	"context"
	"slices"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// sessionlessRecord reads a registered workspace's record for a direct call.
func sessionlessRecord(t *testing.T, h *harness, ws ids.WorkspaceID) wsm.Workspace {
	t.Helper()
	record, err := h.db.Workspace(context.Background(), ws)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	return record
}

func TestBringUpSessionlessStartsEachPendingWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)
	first, _ := h.workspace(t)
	second, _ := h.workspace(t)
	// THE ONE SESSION-LESS WORKSPACE A HANDOVER STARTS stands at a recorded
	// cold gate (owner ruling, 2026-11-23).
	h.c.deps.DB = coldGatedDB{DB: h.db, gated: []wsm.WorkspaceID{first, second}}
	pending := []wsm.Workspace{sessionlessRecord(t, h, first), sessionlessRecord(t, h, second)}

	// Act
	h.c.bringUpSessionless(pending, nil)

	// Assert
	got := h.Started()
	slices.Sort(got)
	want := []ids.WorkspaceID{first, second}
	slices.Sort(want)
	if !slices.Equal(got, want) {
		t.Fatalf("started = %v, want both pending workspaces", got)
	}
}

func TestBringUpSessionlessRaisesTheMarkerBeforeItReturns(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	// THE ONE SESSION-LESS WORKSPACE A HANDOVER STARTS stands at a recorded
	// cold gate (owner ruling, 2026-11-23).
	h.c.deps.DB = coldGatedDB{DB: h.db, gated: []wsm.WorkspaceID{ws}}
	h.mu.Lock()
	h.startErr[ws] = errFake
	h.mu.Unlock()

	// Act
	h.c.bringUpSessionless([]wsm.Workspace{sessionlessRecord(t, h, ws)}, nil)
	h.mu.Lock()
	first := append([]markerEdge(nil), h.marker...)
	h.mu.Unlock()

	// Assert
	if len(first) == 0 || first[0] != (markerEdge{ws: ws, up: true}) {
		t.Fatalf("marker edges on return = %v, want the raise made before the start ran", first)
	}
}

func TestBringUpSessionlessLowersTheMarkerOnceTheStartEnds(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	// THE ONE SESSION-LESS WORKSPACE A HANDOVER STARTS stands at a recorded
	// cold gate (owner ruling, 2026-11-23).
	h.c.deps.DB = coldGatedDB{DB: h.db, gated: []wsm.WorkspaceID{ws}}

	// Act
	h.c.bringUpSessionless([]wsm.Workspace{sessionlessRecord(t, h, ws)}, nil)

	// Assert
	want := []markerEdge{{ws: ws, up: true}, {ws: ws, up: false}}
	if got := h.Marker(); !slices.Equal(got, want) {
		t.Fatalf("marker edges = %v, want %v", got, want)
	}
}

func TestBringUpSessionlessDoesNothingForNoWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.c.bringUpSessionless(nil, nil)

	// Assert
	if got := h.Started(); len(got) != 0 {
		t.Fatalf("started = %v, want nothing", got)
	}
	if got := records(h.log, opBringUp); len(got) != 0 {
		t.Fatalf("bring-up records = %v, want none for an empty bring-up", got)
	}
}

func TestBringUpSessionlessRecordsAFailedStartAtError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	// THE ONE SESSION-LESS WORKSPACE A HANDOVER STARTS stands at a recorded
	// cold gate (owner ruling, 2026-11-23).
	h.c.deps.DB = coldGatedDB{DB: h.db, gated: []wsm.WorkspaceID{ws}}
	h.mu.Lock()
	h.startErr[ws] = errFake
	h.mu.Unlock()

	// Act
	h.c.bringUpSessionless([]wsm.Workspace{sessionlessRecord(t, h, ws)}, nil)
	h.Started()

	// Assert
	errs := levelRecords(records(h.log, opBringUp), dlog.LevelError)
	if len(errs) != 1 || errs[0].Context[dlog.KeyWorkspaceID] != string(ws) || errs[0].Context["error"] != errFake.Error() {
		t.Fatalf("bring-up error records = %+v, want one naming the workspace and the start's cause", errs)
	}
}

// TestBringUpSessionlessGoesThroughTheSharedBringUp pins that the takeover's
// starts are the boot's: the per-workspace record is bringup.Run's own, under
// this package's operation, so a hand-rolled start loop here fails.
func TestBringUpSessionlessGoesThroughTheSharedBringUp(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.c.deps.DB = coldGatedDB{DB: h.db, gated: []wsm.WorkspaceID{ws}}

	// Act
	h.c.bringUpSessionless([]wsm.Workspace{sessionlessRecord(t, h, ws)}, nil)
	h.Started()

	// Assert
	var shared bool
	for _, rec := range records(h.log, opBringUp) {
		if rec.Message == "an open workspace's session was started" {
			shared = true
		}
	}
	if !shared {
		t.Fatalf("bring-up records = %v, want bringup.Run's per-workspace record", records(h.log, opBringUp))
	}
}

// TestBringUpSessionlessLeavesARecordThatReadsRunningBackloggedOrNot pins the
// owner's ruling of 2026-11-23, which replaced the 2026-10-02 one: what
// reaches the session-less bring-up reads a free lock, so a record that still
// reads running is stale, whatever the workspace's visibility. It is not
// started, and its end is recorded.
func TestBringUpSessionlessLeavesARecordThatReadsRunningBackloggedOrNot(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(t *testing.T, h *harness, record wsm.Workspace)
	}{
		{name: "a visible workspace", arrange: func(*testing.T, *harness, wsm.Workspace) {}},
		{name: "a hidden workspace", arrange: func(t *testing.T, h *harness, record wsm.Workspace) {
			if err := h.db.SetBacklogged(context.Background(), record.ID, true, time.Unix(1_700_000_000, 0)); err != nil {
				t.Fatalf("SetBacklogged: %v", err)
			}
		}},
		{name: "a collapsed workspace", arrange: func(t *testing.T, h *harness, record wsm.Workspace) {
			if err := h.db.SetRepositoryFolded(context.Background(), record.Repo, true); err != nil {
				t.Fatalf("SetRepositoryFolded: %v", err)
			}
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws, _ := h.workspace(t)
			h.fleet.mu.Lock()
			delete(h.fleet.live, ws)
			h.fleet.mu.Unlock()
			record := sessionlessRecord(t, h, ws)
			tc.arrange(t, h, record)

			// Act
			left, ok := h.c.bringUpSessionless([]wsm.Workspace{record}, nil)

			// Assert
			if got := h.Started(); len(got) != 0 || !ok || len(left) != 1 {
				t.Fatalf("started = %v, left = %v, ok = %v; want nothing started and the workspace left", got, left, ok)
			}
			session, _, err := h.db.Session(context.Background(), ws)
			if err != nil {
				t.Fatalf("Session: %v", err)
			}
			if session.Terminal == nil || session.Terminal.Kind != wsm.TerminalParked || session.Terminal.Detail != staleDetail {
				t.Fatalf("terminal = %+v, want the stale record's end recorded", session.Terminal)
			}
		})
	}
}

func TestAStaleRecordWhoseEndCannotBeRecordedIsNotStartedAtError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	record := sessionlessRecord(t, h, ws)
	h.fleet.terminalErr = errFake

	// Act
	h.c.bringUpSessionless([]wsm.Workspace{record}, nil)

	// Assert
	if got := h.Started(); len(got) != 0 {
		t.Fatalf("started = %v, want none", got)
	}
	errs := levelRecords(records(h.log, opBringUp), dlog.LevelError)
	if len(errs) != 1 || errs[0].Context["workspace"] != string(ws) || errs[0].Context["cause"] != errFake.Error() {
		t.Fatalf("bring-up error records = %+v, want one naming the workspace and the cause", errs)
	}
}

func TestBringUpSessionlessNeverMarksAShimThatWasNotUp(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	if err := h.db.SetSessionTerminal(context.Background(), ws, wsm.SessionTerminal{Kind: wsm.TerminalParked, At: time.Unix(1_700_000_000, 0)}); err != nil {
		t.Fatalf("SetSessionTerminal: %v", err)
	}

	// Act
	h.c.bringUpSessionless([]wsm.Workspace{sessionlessRecord(t, h, ws)}, nil)

	// Assert
	if got := h.Marker(); len(got) != 0 {
		t.Fatalf("marker edges = %v, want none for a shim that was not up", got)
	}
}

func TestBringUpSessionlessStatesEachWithheldWorkspaceAtInfo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	if err := h.db.SetSessionTerminal(context.Background(), ws, wsm.SessionTerminal{Kind: wsm.TerminalParked, At: time.Unix(1_700_000_000, 0)}); err != nil {
		t.Fatalf("SetSessionTerminal: %v", err)
	}

	// Act
	h.c.bringUpSessionless([]wsm.Workspace{sessionlessRecord(t, h, ws)}, nil)

	// Assert
	infos := levelRecords(records(h.log, opBringUp), dlog.LevelInfo)
	if len(infos) != 1 || infos[0].Context["workspace"] != string(ws) || infos[0].Context["rule"] != string(takeoverNotUp) {
		t.Fatalf("bring-up info records = %+v, want one naming the workspace left as it is and its rule", infos)
	}
}

// TestTheTakeoverRecordsEachDecisionWithItsFacts pins the per-workspace
// record of a takeover's bring-up: the rule, the facts the records state of
// the shim, and the workspace's visibility and standing.
func TestTheTakeoverRecordsEachDecisionWithItsFacts(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(t *testing.T, h *harness, ws ids.WorkspaceID)
		rule    takeoverRule
		message string
		want    dlog.Context
	}{
		{
			name:    "a session record that still reads running",
			arrange: func(*testing.T, *harness, ids.WorkspaceID) {},
			rule:    takeoverRecordStale,
			message: "the session record reads running but no shim held the workspace's lock at the handover; the takeover records the session's end and does not start it",
			want:    dlog.Context{"was_up": true, "live": true, "cold_gate": false, "backlogged": false},
		},
		{
			name: "a parked session",
			arrange: func(t *testing.T, h *harness, ws ids.WorkspaceID) {
				if err := h.db.SetSessionTerminal(context.Background(), ws, wsm.SessionTerminal{Kind: wsm.TerminalParked, Detail: "keepalive_cycle", At: time.Unix(1_700_000_000, 0)}); err != nil {
					t.Fatalf("SetSessionTerminal: %v", err)
				}
			},
			rule:    takeoverNotUp,
			message: "the workspace's shim was not up when the last daemon stopped; the takeover does not start it, and a prompt brings it up",
			want:    dlog.Context{"was_up": false, "live": false, "terminal": wsm.TerminalParked, "terminal_detail": "keepalive_cycle"},
		},
		{
			name: "a hidden workspace whose record still reads running",
			arrange: func(t *testing.T, h *harness, ws ids.WorkspaceID) {
				if err := h.db.SetBacklogged(context.Background(), ws, true, time.Unix(1_700_000_000, 0)); err != nil {
					t.Fatalf("SetBacklogged: %v", err)
				}
			},
			rule:    takeoverRecordStale,
			message: "the session record reads running but no shim held the workspace's lock at the handover; the takeover records the session's end and does not start it",
			want:    dlog.Context{"was_up": true, "backlogged": true},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws, _ := h.workspace(t)
			tc.arrange(t, h, ws)
			record := sessionlessRecord(t, h, ws)

			// Act
			h.c.bringUpSessionless([]wsm.Workspace{record}, dlog.Context{"lock": "free"})
			h.Started()

			// Assert
			rec, ok := dlog.FindRecord(records(h.log, opBringUp), dlog.RecordQuery{Level: dlog.LevelInfo, Message: tc.message})
			if !ok {
				t.Fatalf("bring-up records = %v, want %q", records(h.log, opBringUp), tc.message)
			}
			want := dlog.Merge(tc.want, dlog.Context{
				"workspace": string(ws), "rule": string(tc.rule), "visibility": record.Visibility.String(), "lock": "free",
			})
			for key, value := range want {
				if rec.Context[key] != value {
					t.Fatalf("context[%q] = %v, want %v (record %v)", key, rec.Context[key], value, rec.Context)
				}
			}
		})
	}
}

func TestTheTakeoverRecordsTheStartOfAColdGate(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	gated := sessionlessRecord(t, h, ws)
	h.c.deps.DB = coldGatedDB{DB: h.db, gated: []wsm.WorkspaceID{ws}}

	// Act
	h.c.bringUpSessionless([]wsm.Workspace{gated}, nil)

	// Assert
	if got := h.Started(); len(got) != 1 || got[0] != ws {
		t.Fatalf("started = %v, want the gated workspace", got)
	}
	rec, ok := dlog.FindRecord(records(h.log, opBringUp), dlog.RecordQuery{Level: dlog.LevelInfo,
		Message: "the workspace stands at a recorded cold gate; the takeover starts it, which raises the gate"})
	if !ok || rec.Context["rule"] != string(takeoverColdGate) || rec.Context["cold_gate"] != true {
		t.Fatalf("bring-up records = %v, want the cold gate's decision", records(h.log, opBringUp))
	}
}

func TestATakeoverThatCannotReadTheSessionsStartsNoneAtError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	record := sessionlessRecord(t, h, ws)
	h.c.deps.DB = coldGatedDB{DB: h.db, sessionsErr: errFake}

	// Act
	left, ok := h.c.bringUpSessionless([]wsm.Workspace{record}, nil)

	// Assert
	if ok || left != nil {
		t.Fatalf("bringUpSessionless = %v, %v; want nothing decided", left, ok)
	}
	if got := h.Marker(); len(got) != 0 {
		t.Fatalf("marker edges = %v, want none", got)
	}
	errs := levelRecords(records(h.log, opBringUp), dlog.LevelError)
	if len(errs) != 1 || !slices.Contains(errs[0].Context["workspaces"].([]string), string(ws)) || errs[0].Context["cause"] == nil {
		t.Fatalf("bring-up error records = %+v, want one naming the workspaces and the cause", errs)
	}
}

// coldGatedDB is the harness's store with its recorded cold gates and its
// session read arranged.
type coldGatedDB struct {
	wsm.DB
	gated       []wsm.WorkspaceID
	sessionsErr error
}

func (d coldGatedDB) ColdGateWorkspaces(ctx context.Context) ([]wsm.WorkspaceID, error) {
	if d.gated == nil {
		return d.DB.ColdGateWorkspaces(ctx)
	}
	return d.gated, nil
}

func (d coldGatedDB) Sessions(ctx context.Context) ([]wsm.Session, error) {
	if d.sessionsErr != nil {
		return nil, d.sessionsErr
	}
	return d.DB.Sessions(ctx)
}

func TestBringUpSessionlessDoesNotStartAShimThatWasNotUp(t *testing.T) {
	cases := []struct {
		name string
		kind string
	}{
		{name: "parked between keep-alives", kind: wsm.TerminalParked},
		{name: "hibernated", kind: wsm.TerminalParked},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws, _ := h.workspace(t)
			record := sessionlessRecord(t, h, ws)
			if err := h.db.SetSessionTerminal(context.Background(), ws, wsm.SessionTerminal{Kind: tc.kind, At: time.Unix(1_700_000_000, 0)}); err != nil {
				t.Fatalf("SetSessionTerminal: %v", err)
			}

			// Act
			h.c.bringUpSessionless([]wsm.Workspace{record}, nil)

			// Assert
			if got := h.Started(); len(got) != 0 {
				t.Fatalf("started = %v, want nothing for a shim that was not up", got)
			}
		})
	}
}

// TestAStaleRecordOfAWorkspaceARevivalHoldsIsNotRepaired pins the repair's
// ordering: a session the fleet holds by the time the repair runs (a prompt
// drained during the adoption revived it) keeps its record running.
func TestAStaleRecordOfAWorkspaceARevivalHoldsIsNotRepaired(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	record := sessionlessRecord(t, h, ws)

	// Act
	h.c.bringUpSessionless([]wsm.Workspace{record}, nil)

	// Assert
	session, _, err := h.db.Session(context.Background(), ws)
	if err != nil {
		t.Fatalf("Session: %v", err)
	}
	if session.Terminal != nil {
		t.Fatalf("terminal = %+v, want the held session's record left running", session.Terminal)
	}
}
