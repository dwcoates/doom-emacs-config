package workspace

import (
	"context"
	"errors"
	"reflect"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/drain"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/wsm"
)

func TestWorkspaceFleetTransitionsRecordTheirBeforeAndAfter(t *testing.T) {
	tests := []struct {
		name   string
		state  string
		before any
		after  any
		act    func(*testing.T, *fleetFixture, ids.WorkspaceID)
	}{
		{
			name: "a replacement advances the shim generation", state: "shim_generation", before: 0, after: 1,
			act: func(_ *testing.T, f *fleetFixture, ws ids.WorkspaceID) { f.fleet.freshSocketPath(ws) },
		},
		{
			name: "a session enters the live fleet", state: "session_live", before: false, after: true,
			act: func(_ *testing.T, f *fleetFixture, ws ids.WorkspaceID) { f.fleet.remember(ws, &live{client: f.client}) },
		},
		{
			name: "an answered cold gate retires", state: "cold_gate_standing", before: true, after: false,
			act: func(_ *testing.T, f *fleetFixture, ws ids.WorkspaceID) {
				f.fleet.coldGates[ws] = ServedColdGate{}
				f.fleet.ClearColdGate(ws)
			},
		},
		{
			name: "a session leaves the live fleet", state: "session_live", before: true, after: false,
			act: func(t *testing.T, f *fleetFixture, ws ids.WorkspaceID) {
				f.fleet.remember(ws, &live{client: f.client})
				if err := f.fleet.Stop(context.Background(), ws, true); err != nil {
					t.Fatalf("Stop: %v", err)
				}
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("w1").ID
			beforeRecords := len(f.log.logger.Records())

			// Act.
			tt.act(t, f, ws)

			// Assert.
			for _, record := range f.log.logger.Records()[beforeRecords:] {
				if record.Level == "debug" && record.Operation == "daemon.workspace.state_transition" &&
					record.Context["workspace"] == string(ws) && record.Context["state"] == tt.state &&
					reflect.DeepEqual(record.Context["before"], tt.before) && reflect.DeepEqual(record.Context["after"], tt.after) {
					return
				}
			}
			t.Fatalf("records = %+v, want %s before=%v after=%v", f.log.logger.Records()[beforeRecords:], tt.state, tt.before, tt.after)
		})
	}
}

func TestFleetLifecycleEdgesRecordTheirCompletionAtInfo(t *testing.T) {
	tests := []struct {
		name      string
		operation string
		message   string
		act       func(*fleetFixture, ids.WorkspaceID) error
	}{
		{
			name: "an inert replacement finishes prelaunching", operation: opFleetRollout,
			message: "the replacement shim is prelaunched and inert",
			act: func(f *fleetFixture, ws ids.WorkspaceID) error {
				_, err := f.fleet.Prelaunch(context.Background(), ws)
				return err
			},
		},
		{
			name: "a replacement client finishes installing", operation: opFleetRollout,
			message: "installed the replacement shim client",
			act: func(f *fleetFixture, ws ids.WorkspaceID) error {
				return f.fleet.Install(context.Background(), ws, f.client)
			},
		},
		{
			name: "a live session finishes hibernating", operation: opFleetRollout,
			message: "hibernated the workspace session",
			act: func(f *fleetFixture, ws ids.WorkspaceID) error {
				f.fleet.remember(ws, &live{client: f.client})
				_, err := f.fleet.Hibernate(context.Background(), ws)
				return err
			},
		},
		{
			name: "a live session finishes stopping", operation: opBringUp,
			message: "stopped the workspace session",
			act: func(f *fleetFixture, ws ids.WorkspaceID) error {
				f.fleet.remember(ws, &live{client: f.client})
				return f.fleet.Stop(context.Background(), ws, true)
			},
		},
		{
			name: "a cold session finishes parking", operation: opBringUp,
			message: "the session is parked at its cold gate",
			act: func(f *fleetFixture, ws ids.WorkspaceID) error {
				f.db.sessions[ws] = wsm.Session{Workspace: ws, VendorSessionID: "vendor-1"}
				f.client.response = coldResponse()
				return f.fleet.Start(context.Background(), ws)
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("w1").ID
			before := len(f.log.logger.Records())

			// Act.
			if err := tt.act(f, ws); err != nil {
				t.Fatalf("lifecycle act: %v", err)
			}

			// Assert.
			for _, record := range f.log.logger.Records()[before:] {
				if record.Level == "info" && record.Operation == tt.operation && record.Message == tt.message &&
					record.Context["workspace"] == string(ws) {
					return
				}
			}
			t.Fatalf("records = %+v, want INFO %s %q for %s", f.log.logger.Records()[before:], tt.operation, tt.message, ws)
		})
	}
}

// TestRouteGuidanceRefusesAnUnspecifiedOrigin covers the origin the contract
// never delivers: an unlabeled prompt cannot be traced to the situation that
// caused it, so it is refused rather than sent.
func TestRouteGuidanceRefusesAnUnspecifiedOrigin(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)

	// Act
	_, err := f.fleet.RouteGuidance(context.Background(), "ws-1", nil,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED)

	// Assert
	if err == nil {
		t.Fatal("RouteGuidance accepted an unspecified origin, want a refusal")
	}
}

// TestRouteGuidanceRefusesAWorkspaceWithNoSession covers the delivery with
// nothing to deliver to.
func TestRouteGuidanceRefusesAWorkspaceWithNoSession(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)

	// Act
	_, err := f.fleet.RouteGuidance(context.Background(), "ws-1", nil,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR)

	// Assert
	if err == nil {
		t.Fatal("RouteGuidance delivered into a workspace with no live session")
	}
}

// TestFreeReportsAWorkspaceWithNoSessionFree covers the lease holder's wait on
// a hibernated workspace: there is nothing in flight to wait for.
func TestFreeReportsAWorkspaceWithNoSessionFree(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)

	// Act
	free := f.fleet.Free("ws-1")

	// Assert
	if !free {
		t.Fatal("Free reported a workspace with no session busy, want free")
	}
}

// TestAwaitFreeReturnsAtOnceForAWorkspaceWithNoSession covers the same fact on
// the blocking side: the wait must not hang forever on a session-less
// workspace.
func TestAwaitFreeReturnsAtOnceForAWorkspaceWithNoSession(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)

	// Act
	err := f.fleet.AwaitFree(context.Background(), "ws-1")

	// Assert
	if err != nil {
		t.Fatalf("AwaitFree on a session-less workspace = %v, want nil", err)
	}
}

// TestOccupyReportsAWorkspaceWithNoSession covers the merge on a workspace
// whose session is down: there is no process to occupy, and that is legal.
func TestOccupyReportsAWorkspaceWithNoSession(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)

	// Act
	_, ok, err := f.fleet.Occupy("ws-1", "merge")

	// Assert
	if err != nil {
		t.Fatalf("Occupy = error %v, want a clean false", err)
	}
	if ok {
		t.Fatal("Occupy took an occupancy on a workspace with no session")
	}
}

// TestCaptureDisplacedReportsNothingInFlight covers the ordinary merge: no
// turn was running, so nothing was displaced.
func TestCaptureDisplacedReportsNothingInFlight(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)

	// Act
	_, captured, err := f.fleet.CaptureDisplaced(context.Background(), "ws-1")

	// Assert
	if err != nil {
		t.Fatalf("CaptureDisplaced = error %v, want a clean false", err)
	}
	if captured {
		t.Fatal("CaptureDisplaced captured a turn on a workspace with no session")
	}
}

// TestCaptureDisplacedEndsOnlyTheTurn covers a merge admitted over a running
// user turn. The turn is marked displaced BEFORE it is ended, and it is ended
// UNFORCED: the user asked for a merge, never for the turn's detached work to
// stop. Every failure is recorded through the fleet's own logger or returned.
func TestCaptureDisplacedEndsOnlyTheTurn(t *testing.T) {
	const ws, turn = ids.WorkspaceID("ws-1"), ids.TurnID("turn-1")
	record := wsm.Turn{ID: turn, Workspace: ws, Text: "keep going"}
	tests := []struct {
		name         string
		openTurns    []wsm.Turn
		openTurnsErr error
		putTurnErr   error
		killTurnErr  error
		wantCaptured bool
		wantErr      string
		wantMarked   bool
		wantKills    int
		wantLevel    string
		wantMsg      string
		wantCause    any
	}{
		{
			name: "the turn is marked and ended unforced", openTurns: []wsm.Turn{record},
			wantCaptured: true, wantMarked: true, wantKills: 1,
			wantLevel: "debug", wantMsg: "captured the displaced turn",
		},
		{
			name: "a refused kill leaves the turn captured", openTurns: []wsm.Turn{record},
			killTurnErr:  errors.New("the shim would not answer"),
			wantCaptured: true, wantMarked: true, wantKills: 1,
			wantLevel: "warn", wantMsg: "the displaced turn could not be ended", wantCause: "the shim would not answer",
		},
		{
			name:         "an unreadable record captures and ends nothing",
			openTurnsErr: errors.New("the store is down"),
			wantErr:      "the store is down",
		},
		{
			name: "an unwritable mark ends nothing", openTurns: []wsm.Turn{record},
			putTurnErr: errors.New("the store is read-only"),
			wantErr:    "the store is read-only",
		},
		{
			name:      "a turn with no durable record is left running",
			wantLevel: "warn", wantMsg: "the in-flight turn has no open durable record to displace",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFleetFixture(t)
			f.db.openTurns = tt.openTurns
			f.db.openTurnsErr = tt.openTurnsErr
			f.db.putTurnErr = tt.putTurnErr
			f.client.killTurnErr = tt.killTurnErr
			inFlight := turn
			f.fleet.remember(ws, &live{client: f.client, watcher: &fakeWatcher{turn: &inFlight}})

			// Act
			got, captured, err := f.fleet.CaptureDisplaced(context.Background(), ws)

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("CaptureDisplaced = error %v, want one carrying %q", err, tt.wantErr)
				}
			} else if err != nil {
				t.Fatalf("CaptureDisplaced = error %v, want none", err)
			}
			if captured != tt.wantCaptured {
				t.Fatalf("captured = %v, want %v", captured, tt.wantCaptured)
			}
			if tt.wantCaptured && (got != Displaced{Turn: turn, Text: "keep going"}) {
				t.Fatalf("displaced = %+v, want the in-flight turn and its text", got)
			}
			marked := len(f.db.putTurns) == 1 && f.db.putTurns[0].ID == turn && f.db.putTurns[0].Displaced
			if marked != tt.wantMarked {
				t.Fatalf("durable marks = %+v, want marked=%v", f.db.putTurns, tt.wantMarked)
			}
			if len(f.client.killTurns) != tt.wantKills {
				t.Fatalf("KillTurn requests = %d, want %d", len(f.client.killTurns), tt.wantKills)
			}
			for _, req := range f.client.killTurns {
				if req.GetForce() {
					t.Fatal("the displaced turn's KillTurn was forced, want an unforced kill that spares its detached work")
				}
				if req.GetTurn().GetValue() != string(turn) {
					t.Fatalf("KillTurn turn = %q, want %q", req.GetTurn().GetValue(), turn)
				}
			}
			if tt.wantMsg == "" {
				return
			}
			for _, r := range f.log.logger.Records() {
				if r.Level == tt.wantLevel && r.Operation == opFleetRollout && r.Message == tt.wantMsg &&
					r.Context["turn"] == string(turn) && (tt.wantCause == nil || r.Context["cause"] == tt.wantCause) {
					return
				}
			}
			t.Fatalf("records = %+v, want %s %q", f.log.logger.Records(), tt.wantLevel, tt.wantMsg)
		})
	}
}

// TestRaiseColdGateRefusesWithoutColdFacts covers the gate with nothing to
// draw: the shim's own numbers are what the card shows.
func TestRaiseColdGateRefusesWithoutColdFacts(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)

	// Act
	err := f.fleet.RaiseColdGate(context.Background(), "ws-1", nil)

	// Assert
	if err == nil {
		t.Fatal("RaiseColdGate accepted a gate with no cold facts")
	}
}

// TestSessionBuildSHAReportsAnUnknownWorkspace covers the staleness check on a
// workspace whose session never started: nothing states a build.
func TestSessionBuildSHAReportsAnUnknownWorkspace(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)

	// Act
	_, known := f.fleet.SessionBuildSHA("ws-1")

	// Assert
	if known {
		t.Fatal("SessionBuildSHA answered a build for a workspace with no session")
	}
}

// TestHostSessionFactsAnswersTheDaemonsOwnSessionFacts covers the seam the
// host stream's HostSessionExisting arm is composed from.
func TestHostSessionFactsAnswersTheDaemonsOwnSessionFacts(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Act.
	facts, ok := f.fleet.HostSessionFacts(ws.ID)

	// Assert.
	if !ok {
		t.Fatalf("HostSessionFacts = (%+v, %v), want the operated session's facts", facts, ok)
	}
	if facts.SessionID == "" {
		t.Fatalf("facts = %+v, want a minted host session id", facts)
	}
	if facts.Generation == "" || facts.Generation == "0" {
		t.Fatalf("facts.Generation = %q, want the first generation", facts.Generation)
	}
	if !facts.ShimAttached {
		t.Fatal("facts report the shim detached while the watcher is connected")
	}
	if facts.BackfillKnown {
		t.Fatal("facts claim to know the backfill; the daemon holds no store client and can state none")
	}
	if recorded := f.db.sessions[ws.ID].HostSessionID; recorded != facts.SessionID {
		t.Fatalf("recorded host session id = %q, want the answered %q", recorded, facts.SessionID)
	}
}

// TestHostSessionFactsAnswersNoSessionForAWorkspaceWithNone covers the host
// stream's `none` arm: a registered workspace that never had a session.
func TestHostSessionFactsAnswersNoSessionForAWorkspaceWithNone(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)

	// Act.
	_, ok := f.fleet.HostSessionFacts("w1")

	// Assert.
	if ok {
		t.Fatal("HostSessionFacts reports a session this daemon does not operate")
	}
}

// TestResumeComesUpFreshWhenTheRecordedTranscriptIsMissing is the bounce of a
// session that pre-minted a vendor id and never took a turn: naming that id
// earned `unknown_session` from the shim, no client was installed, and every
// later prompt answered `no_session` forever.
func TestResumeComesUpFreshWhenTheRecordedTranscriptIsMissing(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-never-turned"}
	f.accounts.transcriptErr = errors.New("no such file")

	// Act.
	resumed, err := f.fleet.Resume(context.Background(), ws.ID, f.client)

	// Assert.
	if err != nil {
		t.Fatalf("Resume = %v, want a fresh session rather than a failure", err)
	}
	if resumed.Cold != nil {
		t.Fatalf("Resume = %+v, want no cold gate", resumed)
	}
	if got := f.client.requests[0].GetFresh(); got == nil {
		t.Fatalf("StartSession source = %+v, want the fresh arm", f.client.requests[0].GetSource())
	}
	if _, live := f.fleet.Client(ws.ID); !live {
		t.Fatal("the workspace has no installed client; every prompt would answer no_session")
	}
}

// TestResumeKeepsTheVendorIdWhenTheTranscriptIsFound is the classifier's other
// arm: a real conversation still really resumes.
func TestResumeKeepsTheVendorIdWhenTheTranscriptIsFound(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}

	// Act.
	if _, err := f.fleet.Resume(context.Background(), ws.ID, f.client); err != nil {
		t.Fatalf("Resume: %v", err)
	}

	// Assert.
	if got := f.client.requests[0].GetResume().GetVendorSessionId(); got != "vendor-1" {
		t.Fatalf("resumed vendor session id = %q, want the recorded conversation", got)
	}
	if got := f.db.sessions[ws.ID].VendorSessionID; got != "vendor-1" {
		t.Fatalf("recorded vendor session id = %q, want it unchanged by the resume", got)
	}
}

// TestResumePropagatesTheUnknownSessionArm covers a GENUINELY vanished
// transcript reaching the shim: the arm is named, never a generic sentence.
func TestResumePropagatesTheUnknownSessionArm(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
	f.client.response = &shimv1.StartSessionResponse{
		Result: &shimv1.StartSessionResponse_Failure{Failure: &shimv1.StartSessionFailure{
			Cause:  &shimv1.StartSessionFailure_UnknownSession{UnknownSession: &shimv1.StartSessionUnknownSession{}},
			Detail: "no transcript exists",
		}},
	}

	// Act.
	_, err := f.fleet.Resume(context.Background(), ws.ID, f.client)

	// Assert.
	asRefusal(t, err, ArmUnknownSession)
}

// TestResumeDoesNotRetryAHardVendorStartFailure is the RULING GUARD: a hard
// resume failure that is not unknown_session has no retry machinery, and the
// relaunch engine's fault is the remediation record.
func TestResumeDoesNotRetryAHardVendorStartFailure(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
	f.client.response = &shimv1.StartSessionResponse{
		Result: &shimv1.StartSessionResponse_Failure{Failure: &shimv1.StartSessionFailure{
			Cause:  &shimv1.StartSessionFailure_VendorStartFailed{VendorStartFailed: &shimv1.StartSessionVendorStartFailed{}},
			Detail: "the vendor binary is missing",
		}},
	}

	// Act.
	_, err := f.fleet.Resume(context.Background(), ws.ID, f.client)

	// Assert.
	if err == nil {
		t.Fatal("Resume() = nil error, want the vendor-start failure surfaced")
	}
	if len(f.client.requests) != 1 {
		t.Fatalf("StartSession calls = %d, want exactly one; there is no retry machinery", len(f.client.requests))
	}
}

// TestKillSessionTellsTheWatcherBeforeItEndsTheSession covers the ordering the
// stand-down rests on: the shim closes its standing streams as the session
// ends, so a watcher told afterwards would have already read the daemon's own
// act as a severed link and redialed a shim this call is about to stop.
func TestKillSessionTellsTheWatcherBeforeItEndsTheSession(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}
	*f.standDown = nil

	// Act.
	if err := f.fleet.KillSession(context.Background(), ws.ID, false); err != nil {
		t.Fatalf("KillSession: %v", err)
	}

	// Assert.
	got := *f.standDown
	want := []string{"watcher.SessionEnding", "shim.KillSession"}
	if len(got) != len(want) || got[0] != want[0] || got[1] != want[1] {
		t.Fatalf("stand-down order = %v, want %v", got, want)
	}
}

// TestClientAnswersALiveShimOnlyWhileItsProcessLives is the headless-run
// defect at its lowest seam: a shim SIGKILLed out from under the daemon leaves
// its row in the fleet, and answering that row as a live client sends a
// submitted prompt down the delivery path — StartTurn onto a socket nothing is
// listening on — instead of the revival path a workspace with no client takes.
func TestClientAnswersALiveShimOnlyWhileItsProcessLives(t *testing.T) {
	tests := []struct {
		name     string
		reaped   bool
		wantLive bool
	}{
		{name: "the shim is running", reaped: false, wantLive: true},
		{name: "the shim process is gone", reaped: true, wantLive: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
				t.Fatalf("Start: %v", err)
			}
			f.client.reaped = tt.reaped

			// Act.
			client, live := f.fleet.Client(ws.ID)

			// Assert.
			if live != tt.wantLive {
				t.Fatalf("Client() live = %v, want %v", live, tt.wantLive)
			}
			if live && client == nil {
				t.Fatal("Client() answered live with no client")
			}
			if !live && client != nil {
				t.Fatalf("Client() answered not live with client %v", client)
			}
		})
	}
}

// ---- a displaced watcher is closed, and a resume's watcher outlives its call ----

// TestResumeOpensItsWatchesOnAContextItsCallerCannotCancel is the defect the
// realtest sweep caught. `Resume` handed the watcher the RELAUNCH's own
// context, and a relaunch returns the moment the session is up: the freshly
// opened fleet was torn down by the daemon's own cancel within a millisecond,
// which the watcher then read back as `canceled: context canceled` on both
// standing streams -- two ERRORs, a `link_severed` WARN and its health fault --
// and the shim reported as two h2c CANCELs (2026-09-12T15:23:03.338).
func TestResumeOpensItsWatchesOnAContextItsCallerCannotCancel(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
	var opened context.Context
	f.fleet.watch = func(ctx context.Context, _ ids.WorkspaceID, _ shimclient.Client, _ sessionwatcher.Session, _ sessionwatcher.Sinks, _ dlog.Logger) (sessionwatcher.Watcher, error) {
		opened = ctx
		return &fakeWatcher{}, nil
	}
	ctx, cancel := context.WithCancel(context.Background())

	// Act.
	if _, err := f.fleet.Resume(ctx, ws.ID, f.client); err != nil {
		t.Fatalf("Resume: %v", err)
	}
	cancel()

	// Assert.
	if opened == nil {
		t.Fatal("the resume opened no watch fleet")
	}
	if err := opened.Err(); err != nil {
		t.Fatalf("the resumed fleet's context = %v, want it still live: a watcher outlives the call that opened it", err)
	}
}

// TestRememberClosesTheWatcherItDisplaces pins the leak the same bounce left
// behind. The fleet's map entry is a watcher's ONLY handle: overwriting it with
// a new session dropped the previous watcher with its streams still standing,
// and those streams then ended against a fleet nothing had told -- which is
// recorded as a severing.
func TestRememberClosesTheWatcherItDisplaces(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	displaced := &fakeWatcher{}
	f.fleet.remember(ws.ID, &live{client: f.client, watcher: displaced})

	// Act.
	f.fleet.remember(ws.ID, &live{client: f.client, watcher: &fakeWatcher{}})

	// Assert.
	if !displaced.closed {
		t.Fatal("the watcher a new session displaced was left open with its streams standing")
	}
}

// TestRememberLeavesAWatcherItDidNotDisplaceOpen is the guard against closing
// the very watcher being installed: a fleet that re-records the SAME watcher
// under a rotated client must not tear down the watches it just kept.
func TestRememberLeavesAWatcherItDidNotDisplaceOpen(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	kept := &fakeWatcher{}
	f.fleet.remember(ws.ID, &live{client: f.client, watcher: kept})

	// Act.
	f.fleet.remember(ws.ID, &live{client: f.client, watcher: kept})

	// Assert.
	if kept.closed {
		t.Fatal("a watcher carried across into the new session was closed")
	}
}

// TestRememberOverAWatcherlessSessionClosesNothing covers the cold-attach case:
// a session recorded with no watcher has nothing to displace, and reaching for
// one would be a nil dereference on every first bring-up.
func TestRememberOverAWatcherlessSessionClosesNothing(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.fleet.remember(ws.ID, &live{client: f.client})
	installed := &fakeWatcher{}

	// Act.
	f.fleet.remember(ws.ID, &live{client: f.client, watcher: installed})

	// Assert.
	if installed.closed {
		t.Fatal("the newly installed watcher was closed")
	}
}

// TestFleetServingAnswersWhatTheDirectiveCanReach covers the sweep's selection
// predicate: it must answer exactly what Hibernate's own client lookup
// answers, so a workspace the sweep selects is one the directive can land on.
func TestFleetServingAnswersWhatTheDirectiveCanReach(t *testing.T) {
	tests := []struct {
		name    string
		install func(*fleetFixture, ids.WorkspaceID)
		want    bool
	}{
		{
			name:    "a session this daemon never installed",
			install: func(*fleetFixture, ids.WorkspaceID) {},
			want:    false,
		},
		{
			name: "a session whose shim has been reaped",
			install: func(f *fleetFixture, ws ids.WorkspaceID) {
				f.client.reaped = true
				f.fleet.remember(ws, &live{client: f.client, sessionStarted: true})
			},
			want: false,
		},
		{
			name: "a live shim parked behind a cold gate, whose session never started",
			install: func(f *fleetFixture, ws ids.WorkspaceID) {
				f.fleet.remember(ws, &live{client: f.client})
			},
			want: false,
		},
		{
			name: "a session with a live shim",
			install: func(f *fleetFixture, ws ids.WorkspaceID) {
				f.fleet.remember(ws, &live{client: f.client, sessionStarted: true})
			},
			want: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("w1").ID
			tt.install(f, ws)

			// Act.
			serving := f.fleet.Serving(ws)

			// Assert.
			if serving != tt.want {
				t.Fatalf("Serving(%q) = %v, want %v", ws, serving, tt.want)
			}
		})
	}
}

// TestHibernateAnswersTheTypedNoLiveSessionState covers the sweep's other
// half: the directive's refusal on a workspace with no shim is a STATE the
// sweep matches on, not a message it reads.
func TestHibernateAnswersTheTypedNoLiveSessionState(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1").ID

	// Act.
	_, err := f.fleet.Hibernate(context.Background(), ws)

	// Assert.
	if !errors.Is(err, drain.ErrNoLiveSession) {
		t.Fatalf("Hibernate on a workspace with no session = %v, want drain.ErrNoLiveSession", err)
	}
}

// TestKillSessionSkipsTheDirectiveWhenNoSessionWasStarted is the other half of
// the stand-down ordering above: a shim that never started a session can only
// answer the directive `no_session`, and the daemon reported that answer as a
// kill that "did not answer" -- a WARN per teardown of every cold-gated
// workspace. The stop is what such a workspace was owed, and the directive is
// not sent at all.
func TestKillSessionSkipsTheDirectiveWhenNoSessionWasStarted(t *testing.T) {
	tests := []struct {
		name           string
		sessionStarted bool
		wantDirective  bool
	}{
		{
			name:           "a shim holding a started session is directed",
			sessionStarted: true,
			wantDirective:  true,
		},
		{
			name:           "a shim parked behind a cold gate is only stopped",
			sessionStarted: false,
			wantDirective:  false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			f.fleet.remember(ws.ID, &live{client: f.client, sessionStarted: tt.sessionStarted})
			*f.standDown = nil

			// Act.
			if err := f.fleet.KillSession(context.Background(), ws.ID, true); err != nil {
				t.Fatalf("KillSession: %v", err)
			}

			// Assert.
			var directed bool
			for _, step := range *f.standDown {
				if step == "shim.KillSession" {
					directed = true
				}
			}
			if directed != tt.wantDirective {
				t.Fatalf("shim.KillSession sent = %v, want %v (steps %v)",
					directed, tt.wantDirective, *f.standDown)
			}
		})
	}
}

// TestKillSessionArmsTheStandDownLatchBeforeTheAsk covers the ORDER the
// escalation depends on: the process stop after a kill that did not answer is
// a teardown this daemon ordered, and every side that later sees the departure
// -- the exit watcher, the redialer, the adopted-death witness -- reads the
// latch to tell it from a shim that died on its own.
func TestKillSessionArmsTheStandDownLatchBeforeTheAsk(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.fleet.remember(ws.ID, &live{client: f.client, sessionStarted: true})

	// Act.
	if err := f.fleet.KillSession(context.Background(), ws.ID, true); err != nil {
		t.Fatalf("KillSession: %v", err)
	}

	// Assert.
	if !f.client.standDownBeforeKill {
		t.Fatal("the stand-down latch was not armed before the KillSession ask")
	}
}

// TestKillSessionRecordsTheUnansweredKillByWhoOrderedIt covers the record the
// escalation writes. A forced stop under a stand-down THIS DAEMON ordered is
// the mechanism working and is recorded at INFO; a kill that does not answer
// outside one -- a DETACHED client, whose process belongs to the successor
// daemon -- stays a WARN.
func TestKillSessionRecordsTheUnansweredKillByWhoOrderedIt(t *testing.T) {
	tests := []struct {
		name      string
		refused   bool
		wantLevel string
	}{
		{name: "the daemon ordered this stand-down", refused: false, wantLevel: "info"},
		{name: "the kill is outside a stand-down this daemon ordered", refused: true, wantLevel: "warn"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			f.client.standDownRefused = tt.refused
			f.client.killSessionErr = errors.New("the shim never answered")
			f.fleet.remember(ws.ID, &live{client: f.client, sessionStarted: true})
			before := len(f.log.logger.Records())

			// Act.
			if err := f.fleet.KillSession(context.Background(), ws.ID, true); err != nil {
				t.Fatalf("KillSession: %v", err)
			}

			// Assert.
			for _, record := range f.log.logger.Records()[before:] {
				if record.Message != "the session kill did not answer; stopping the process anyway" {
					continue
				}
				if record.Level != tt.wantLevel {
					t.Fatalf("the unanswered kill was recorded at %q, want %q", record.Level, tt.wantLevel)
				}
				return
			}
			t.Fatalf("records = %+v, want the unanswered kill recorded at %s", f.log.logger.Records()[before:], tt.wantLevel)
		})
	}
}
