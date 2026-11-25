package workspace

import (
	"context"
	"errors"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
	"agent-repld/internal/sessionwatcher"
	"agent-repld/internal/wsm"
)

// openingPointer is the main-watch pointer the retired watcher in these
// scenarios was served.
var openingPointer = &conversationv1.HistoryPointer{Value: "ptr-main-9"}

// TestStartWatcherOpeningFollowsTheReplayRule covers the owner's rule at the
// place the fleet decides it: a watcher REPLAYS the first page only when the
// workspace is opened in this process or a transcript was selected, and every
// other watcher resumes from the pointers of the watcher it replaces.
func TestStartWatcherOpeningFollowsTheReplayRule(t *testing.T) {
	tests := []struct {
		name string
		// before brings the workspace to the state the case is about; the
		// retired watcher it leaves behind was served openingPointer.
		before func(t *testing.T, f *fleetFixture, ws ids.WorkspaceID)
		// act starts the watcher under test.
		act         func(f *fleetFixture, ws ids.WorkspaceID) error
		wantOpening string
		wantMain    *conversationv1.HistoryPointer
	}{
		{
			name:        "a workspace's first open replays its first page",
			before:      func(*testing.T, *fleetFixture, ids.WorkspaceID) {},
			act:         func(f *fleetFixture, ws ids.WorkspaceID) error { return f.fleet.Start(context.Background(), ws) },
			wantOpening: "workspace_opened",
		},
		{
			name:        "a restart resumes from the retired watcher's pointers",
			before:      startedThenStopped,
			act:         func(f *fleetFixture, ws ids.WorkspaceID) error { return f.fleet.Start(context.Background(), ws) },
			wantOpening: "resumed",
			wantMain:    openingPointer,
		},
		{
			name:        "a transcript select replays the selected transcript's first page",
			before:      startedThenStopped,
			act:         func(f *fleetFixture, ws ids.WorkspaceID) error { return f.fleet.StartRebound(context.Background(), ws) },
			wantOpening: "transcript_selected",
		},
		{
			name: "a fresh conversation forgets the previous book's pointers",
			before: func(t *testing.T, f *fleetFixture, ws ids.WorkspaceID) {
				startedThenStopped(t, f, ws)
				delete(f.db.sessions, ws)
			},
			act:         func(f *fleetFixture, ws ids.WorkspaceID) error { return f.fleet.Start(context.Background(), ws) },
			wantOpening: "workspace_opened",
		},
		{
			name: "a relaunch's install resumes from the watcher it retires",
			before: func(t *testing.T, f *fleetFixture, ws ids.WorkspaceID) {
				t.Helper()
				if err := f.fleet.Start(context.Background(), ws); err != nil {
					t.Fatalf("Start: %v", err)
				}
				f.watcher.pointers = sessionwatcher.Pointers{Main: openingPointer}
			},
			act: func(f *fleetFixture, ws ids.WorkspaceID) error {
				return f.fleet.Install(context.Background(), ws, &fakeClient{response: startedResponse("vendor-1"), pid: 5151})
			},
			wantOpening: "resumed",
			wantMain:    openingPointer,
		},
		{
			name:        "a handover's adoption resumes from the incumbent's carried pointers",
			before:      handedOver,
			act:         func(f *fleetFixture, ws ids.WorkspaceID) error { return f.fleet.Start(context.Background(), ws) },
			wantOpening: "resumed",
			wantMain:    carriedPointer,
		},
		{
			name: "a fresh conversation forgets the carried pointers too",
			before: func(_ *testing.T, f *fleetFixture, ws ids.WorkspaceID) {
				f.fleet.ResumeWatchesFrom(ws, sessionwatcher.Pointers{Main: openingPointer})
				delete(f.db.sessions, ws)
			},
			act:         func(f *fleetFixture, ws ids.WorkspaceID) error { return f.fleet.Start(context.Background(), ws) },
			wantOpening: "workspace_opened",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			tt.before(t, f, ws.ID)
			before := len(f.openings)

			// Act.
			err := tt.act(f, ws.ID)

			// Assert.
			if err != nil {
				t.Fatalf("act: %v", err)
			}
			if len(f.openings) != before+1 {
				t.Fatalf("watchers started = %d, want exactly one", len(f.openings)-before)
			}
			got := f.openings[len(f.openings)-1]
			if got.String() != tt.wantOpening {
				t.Fatalf("opening = %q, want %q", got.String(), tt.wantOpening)
			}
			if main := got.From().Main; main.GetValue() != tt.wantMain.GetValue() {
				t.Fatalf("resumed main pointer = %q, want %q", main.GetValue(), tt.wantMain.GetValue())
			}
		})
	}
}

// TestStartWatcherLogsTheOpeningAtInfo covers the record that says, per
// bring-up, whether history was replayed: without it a replay is invisible in
// the logs, which is how every-turn replays went unnoticed.
func TestStartWatcherLogsTheOpeningAtInfo(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	for _, rec := range f.log.logger.Records() {
		if rec.Level == "info" && rec.Operation == opBringUp && rec.Context["opening"] == "workspace_opened" && rec.Context["replays_history"] == true {
			return
		}
	}
	t.Fatalf("no info %s record stated the opening; records: %v", opBringUp, f.log.logger.Records())
}

// TestFailedWatcherStartKeepsTheSelectionOwed covers the error path of a
// select: a watcher that never started has not replayed the selected
// transcript, so the next watcher still owes it the first page.
func TestFailedWatcherStartKeepsTheSelectionOwed(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	startedThenStopped(t, f, ws.ID)
	f.watchErr = errors.New("the watch fleet could not open")
	if err := f.fleet.StartRebound(context.Background(), ws.ID); err == nil {
		t.Fatal("StartRebound = nil error, want the watcher's failure surfaced")
	}
	f.watchErr = nil
	if err := f.fleet.Stop(context.Background(), ws.ID, true); err != nil {
		t.Fatalf("Stop: %v", err)
	}

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if got := f.openings[len(f.openings)-1].String(); got != "transcript_selected" {
		t.Fatalf("opening after a failed select = %q, want transcript_selected", got)
	}
}

// startedThenStopped brings a workspace up, has its watcher served
// openingPointer, and stops it: the retired watcher a restart replaces.
func startedThenStopped(t *testing.T, f *fleetFixture, ws ids.WorkspaceID) {
	t.Helper()
	if err := f.fleet.Start(context.Background(), ws); err != nil {
		t.Fatalf("Start: %v", err)
	}
	f.watcher.pointers = sessionwatcher.Pointers{Main: openingPointer}
	if err := f.fleet.Stop(context.Background(), ws, true); err != nil {
		t.Fatalf("Stop: %v", err)
	}
}

// carriedPointer is the main-watch pointer a handover's incumbent carried in
// these scenarios.
var carriedPointer = &conversationv1.HistoryPointer{Value: "ptr-carried-3"}

// handedOver brings ws to where a handover's successor stands: a resumable
// session this process holds no watcher of, and the incumbent's carried
// pointers.
func handedOver(t *testing.T, f *fleetFixture, ws ids.WorkspaceID) {
	t.Helper()
	startedThenStopped(t, f, ws)
	f.fleet.mu.Lock()
	delete(f.fleet.watched, ws)
	f.fleet.mu.Unlock()
	f.fleet.ResumeWatchesFrom(ws, sessionwatcher.Pointers{Main: carriedPointer})
}

func TestCarriedPointersAreSpentByTheWatcherThatStarts(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	handedOver(t, f, ws.ID)

	// Act
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("Start: %v", err)
	}
	f.fleet.mu.Lock()
	_, still := f.fleet.carried[ws.ID]
	f.fleet.mu.Unlock()
	if still {
		t.Fatalf("the carried pointers are still held after a watcher started from them")
	}
}

func TestWatchPointersAnswersTheLastWatchersPointers(t *testing.T) {
	tests := []struct {
		name        string
		before      func(t *testing.T, f *fleetFixture, ws ids.WorkspaceID)
		wantWatched bool
		wantMain    *conversationv1.HistoryPointer
	}{
		{
			name:   "a workspace this process never watched answers none",
			before: func(*testing.T, *fleetFixture, ids.WorkspaceID) {},
		},
		{
			name:        "a watched workspace answers its retired watcher's pointers",
			before:      startedThenStopped,
			wantWatched: true,
			wantMain:    openingPointer,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			tt.before(t, f, ws.ID)

			// Act
			got, watched := f.fleet.WatchPointers(ws.ID)

			// Assert
			if watched != tt.wantWatched || got.Main.GetValue() != tt.wantMain.GetValue() {
				t.Fatalf("WatchPointers = (%q, %v), want (%q, %v)", got.Main.GetValue(), watched, tt.wantMain.GetValue(), tt.wantWatched)
			}
		})
	}
}

// departedSink is the lifecycle sink a fleet's watcher wraps, recording each
// departure it is handed on.
type departedSink struct {
	sessionwatcher.LifecycleSink
	departures []sessionwatcher.Departure
}

func (s *departedSink) OnDeparted(_ ids.WorkspaceID, _ sessionwatcher.Watcher, departure sessionwatcher.Departure) {
	s.departures = append(s.departures, departure)
}

// TestAShimThatDiesRecordsItsSessionsEnd pins the ended-shim invariant for a
// death (terminalrecord.go): a shim whose watcher saw it go with nothing
// stopping it has its session's end recorded, and an ordered departure's end
// is left to the stop that ordered it.
func TestAShimThatDiesRecordsItsSessionsEnd(t *testing.T) {
	cases := []struct {
		name    string
		ordered bool
		want    bool
	}{
		{name: "a death", want: true},
		{name: "an ordered departure", ordered: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
				t.Fatalf("Start: %v", err)
			}
			inner := &departedSink{}
			sink := sessionNotingLifecycle{LifecycleSink: inner, fleet: f.fleet, client: f.client}

			// Act
			sink.OnDeparted(ws.ID, nil, sessionwatcher.Departure{Ordered: tc.ordered, Cause: sessionwatcher.DepartureLinkDead})
			if err := inflight.Default.AwaitIdleFor(context.Background(), []string{string(ws.ID)}); err != nil {
				t.Fatalf("AwaitIdleFor: %v", err)
			}

			// Assert
			if len(inner.departures) != 1 {
				t.Fatalf("departures handed on = %v, want the one", inner.departures)
			}
			terminal, recorded := f.db.terminals[ws.ID]
			if recorded != tc.want {
				t.Fatalf("terminal = %+v (recorded %v), want recorded: %v", terminal, recorded, tc.want)
			}
			if tc.want && (terminal.Kind != wsm.TerminalParked || !strings.HasPrefix(terminal.Detail, "shim_died: ")) {
				t.Fatalf("terminal = %+v, want a park naming the death", terminal)
			}
		})
	}
}

func TestADeathAfterANewerSessionTookTheWorkspaceRecordsNothing(t *testing.T) {
	// Arrange: the dead shim's client is no longer the one the fleet holds.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}
	dead := &fakeClient{pid: 9191}

	// Act
	f.fleet.recordDeath(ws.ID, dead)

	// Assert
	if terminal, recorded := f.db.terminals[ws.ID]; recorded {
		t.Fatalf("terminal = %+v, want none: a newer session holds the workspace", terminal)
	}
	if !recordedAt(f, dlog.LevelInfo, opBringUp, "a newer session holds the workspace, so the end of its last shim is not recorded") {
		t.Fatalf("records = %+v, want the skip at INFO", f.log.logger.Records())
	}
}

func TestReconcileLiveTerminalRetiresOnlyALiveWorkspacesTerminal(t *testing.T) {
	cases := []struct {
		name string
		live bool
	}{
		{name: "a live workspace", live: true},
		{name: "a workspace with no session"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			if tc.live {
				if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
					t.Fatalf("Start: %v", err)
				}
			}
			f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
			if err := f.db.SetSessionTerminal(context.Background(), ws.ID, wsm.SessionTerminal{Kind: "killed", At: fixedNow}); err != nil {
				t.Fatalf("SetSessionTerminal: %v", err)
			}

			// Act
			live, err := f.fleet.ReconcileLiveTerminal(context.Background(), f.log.logger, opOpen, ws.ID)

			// Assert
			if err != nil || live != tc.live {
				t.Fatalf("ReconcileLiveTerminal = %v, %v; want %v", live, err, tc.live)
			}
			if retired := f.db.sessions[ws.ID].Terminal == nil; retired != tc.live {
				t.Fatalf("terminal = %+v, want it retired only for a live workspace", f.db.sessions[ws.ID].Terminal)
			}
		})
	}
}
