package workspace

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/drain"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/shimsocket"
	"claude-repld/internal/wsm"
	"fmt"
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
				f.fleet.coldGates[ws] = coldGate{served: ServedColdGate{VendorSessionID: "vendor-1"}}
				f.fleet.TakeColdGate(ws, "vendor-1")
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

func TestAwaitFactsAnswersAtOnceForAWorkspaceWithNoWatcher(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.fleet.remember(ws.ID, &live{client: f.client})

	// Act.
	err := f.fleet.AwaitFacts(context.Background(), ws.ID)

	// Assert.
	if err != nil {
		t.Fatalf("AwaitFacts with no watcher = %v, want nil: there is no conversation to announce", err)
	}
}

func TestAwaitFactsSurfacesTheWatchersRefusal(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.fleet.remember(ws.ID, &live{client: f.client, watcher: &fakeWatcher{factsErr: errFake}})

	// Act.
	err := f.fleet.AwaitFacts(context.Background(), ws.ID)

	// Assert.
	if !errors.Is(err, errFake) {
		t.Fatalf("AwaitFacts = %v, want the watcher's refusal surfaced", err)
	}
}

func TestColdGateStandingAnswersTheRaisedGatesFacts(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	cold := &conversationv1.SessionCold{ContextTokens: 123456}
	if err := f.fleet.RaiseColdGate(context.Background(), ws.ID, cold); err != nil {
		t.Fatalf("RaiseColdGate: %v", err)
	}

	// Act.
	got, standing := f.fleet.ColdGateStanding(ws.ID)

	// Assert.
	if !standing || got.GetContextTokens() != 123456 {
		t.Fatalf("ColdGateStanding = (%v, %v), want the raised gate's facts", got, standing)
	}
}

func TestColdGateStandingAnswersFalseWithNoGate(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")

	// Act.
	_, standing := f.fleet.ColdGateStanding(ws.ID)

	// Assert.
	if standing {
		t.Fatal("ColdGateStanding reports a gate nobody raised")
	}
}

func TestAdoptParkedHoldsTheShimWithNoWatcherAndRaisesTheGate(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1", HostSessionID: "host-1"}
	cold := &conversationv1.SessionCold{ContextTokens: 123456}

	// Act.
	if _, err := f.fleet.AdoptParked(context.Background(), ws.ID, cold); err != nil {
		t.Fatalf("AdoptParked: %v", err)
	}

	// Assert.
	f.fleet.mu.RLock()
	entry := f.fleet.sessions[ws.ID]
	f.fleet.mu.RUnlock()
	if entry == nil || entry.watcher != nil || entry.hostSessionID != "host-1" {
		t.Fatalf("session entry = %+v, want the parked client held with no watcher under its host identity", entry)
	}
	if gate, standing := f.fleet.ColdGate(ws.ID); !standing || gate.VendorSessionID != "vendor-1" {
		t.Fatalf("ColdGate = (%+v, %v), want the carried gate raised on the parked conversation", gate, standing)
	}
}

func TestAdoptParkedWithholdsTheCompactMenuForAWorkAccount(t *testing.T) {
	// Arrange: the parked session lives under the work account.
	f := newFleetFixture(t)
	f.accounts.multiRepoDir = "/work"
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1", HostSessionID: "host-1", ConfigDir: "/work"}

	// Act.
	if _, err := f.fleet.AdoptParked(context.Background(), ws.ID, &conversationv1.SessionCold{ContextTokens: 1}); err != nil {
		t.Fatalf("AdoptParked: %v", err)
	}

	// Assert.
	if gate, standing := f.fleet.ColdGate(ws.ID); !standing || gate.Compact != nil {
		t.Fatalf("ColdGate = (%+v, %v), want the gate raised with no compact menu", gate, standing)
	}
}

func TestRaiseColdGateWithholdsTheCompactMenuForAWorkAccount(t *testing.T) {
	// Arrange: the relaunched session is filed under the work account.
	f := newFleetFixture(t)
	f.accounts.multiRepoDir = "/work"
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1", ConfigDir: "/work"}

	// Act.
	if err := f.fleet.RaiseColdGate(context.Background(), ws.ID, &conversationv1.SessionCold{ContextTokens: 1}); err != nil {
		t.Fatalf("RaiseColdGate: %v", err)
	}

	// Assert.
	if gate, standing := f.fleet.ColdGate(ws.ID); !standing || gate.Compact != nil {
		t.Fatalf("ColdGate = (%+v, %v), want the gate raised with no compact menu", gate, standing)
	}
}

func TestAdoptParkedRefusesWithNoColdFacts(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")

	// Act.
	_, err := f.fleet.AdoptParked(context.Background(), ws.ID, nil)

	// Assert.
	if err == nil {
		t.Fatal("AdoptParked accepted a parked shim with no cold facts")
	}
	if len(f.supervisor.adopts) != 0 {
		t.Fatalf("adoptions = %v, want nothing dialed", f.supervisor.adopts)
	}
}

func TestAdoptParkedWhoseClaimIsRefusedLetsTheDialedShimGo(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.claimErr = errFake

	// Act.
	_, err := f.fleet.AdoptParked(context.Background(), ws.ID, &conversationv1.SessionCold{ContextTokens: 1})

	// Assert.
	if !errors.Is(err, errFake) {
		t.Fatalf("AdoptParked = %v, want the refused claim surfaced", err)
	}
	if f.client.detached != 1 || len(f.client.kills) != 0 {
		t.Fatalf("detaches = %d, kills = %d; want the link let go and the shim left running", f.client.detached, len(f.client.kills))
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

func TestPrelaunchStampsTheSpawnWithTheBundleItHolds(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)
	ws := f.workspace("w1").ID
	heldDuringSpawn := false
	f.supervisor.onSpawn = func() { heldDuringSpawn = f.bundle.holding() }

	// Act
	if _, err := f.fleet.Prelaunch(context.Background(), ws); err != nil {
		t.Fatalf("Prelaunch: %v", err)
	}

	// Assert
	if got := f.supervisor.spawns[0].ShimBuildSHA; got != "installed-build" {
		t.Fatalf("prelaunch build = %q, want the bundle's own", got)
	}
	if !heldDuringSpawn || f.bundle.holding() {
		t.Fatalf("held during the spawn = %v, still held after = %v; want held across it and released after", heldDuringSpawn, f.bundle.holding())
	}
}

func TestPrelaunchRefusesAnUnresolvableBundle(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)
	ws := f.workspace("w1").ID
	f.bundle.err = errors.New("main.js does not exist and SHIM_BUILD_SHA is unset")

	// Act
	_, err := f.fleet.Prelaunch(context.Background(), ws)

	// Assert
	if err == nil {
		t.Fatal("Prelaunch() = nil error, want the unresolvable bundle refused")
	}
	if f.supervisor.spawnAttempts != 0 {
		t.Fatalf("spawn attempts = %d, want none", f.supervisor.spawnAttempts)
	}
}

// A REFUSED SERVING CLAIM LEAVES NO HALF-ADOPTION. The live deploy of
// 2026-09-24 15:07 claimed after Install's map write on a read-only handle:
// the claim was refused and the fleet was left holding three clients with no
// watches that their callers believed were given up.

func TestAnInstallWhoseClaimIsRefusedHoldsNothing(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.claimErr = errFake

	// Act.
	err := f.fleet.Install(context.Background(), ws.ID, f.client)

	// Assert.
	if err == nil {
		t.Fatal("Install() = nil error, want the refused claim surfaced")
	}
	if _, held := f.fleet.Client(ws.ID); held {
		t.Fatal("the fleet holds the client of a refused install")
	}
}

func TestAnAdoptionWhoseClaimIsRefusedLetsTheDialedShimGo(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.claimErr = errFake

	// Act.
	_, err := f.fleet.Adopt(context.Background(), ws.ID)

	// Assert.
	if err == nil {
		t.Fatal("Adopt() = nil error, want the refused claim surfaced")
	}
	if f.client.detached != 1 || len(f.client.kills) != 0 {
		t.Fatalf("detaches = %d, kills = %d; want the link let go and the shim left running", f.client.detached, len(f.client.kills))
	}
}

func TestAdoptDialsTheNewestLiveSocketGeneration(t *testing.T) {
	// Arrange: a relaunch moved the running shim onto `<base>.n1.sock`.
	dir := t.TempDir()
	f := newFleetFixture(t)
	f.socketDir = dir
	ws := f.workspace("w1")
	base := filepath.Join(dir, "w1.sock")
	generation := strings.TrimSuffix(base, ".sock") + ".n1.sock"
	if err := os.WriteFile(generation, nil, 0o600); err != nil {
		t.Fatalf("writing the generation's socket path: %v", err)
	}
	f.socketState = shimsocket.StateAbsent
	f.socketStates = map[string]shimsocket.State{generation: shimsocket.StateLive}

	// Act.
	if _, err := f.fleet.Adopt(context.Background(), ws.ID); err != nil {
		t.Fatalf("Adopt: %v", err)
	}

	// Assert.
	if len(f.supervisor.adopts) != 1 || f.supervisor.adopts[0] != generation {
		t.Fatalf("adoptions = %+v, want exactly one of %q", f.supervisor.adopts, generation)
	}
}

// ---- the turns open at attach ----

// TestAnInstallHandsTheWatcherTheTurnsOpenAtAttach covers the adoption's
// snapshot: the workspace's open turn rows reach the watcher, which compares
// them with the shim's own turn_in_flight when its facts arrive.
func TestAnInstallHandsTheWatcherTheTurnsOpenAtAttach(t *testing.T) {
	// Arrange: an adopted conversation with two turn rows left open.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
	started := time.Date(2026, 9, 29, 12, 0, 0, 0, time.UTC)
	f.db.openTurns = []wsm.Turn{
		{ID: "turn-1", Workspace: ws.ID, StartedAt: started},
		{ID: "turn-2", Workspace: ws.ID, StartedAt: started.Add(time.Minute)},
	}

	// Act.
	if err := f.fleet.Install(context.Background(), ws.ID, f.client); err != nil {
		t.Fatalf("Install: %v", err)
	}

	// Assert.
	if len(f.openAtAttach) != 1 {
		t.Fatalf("watchers started = %d, want exactly one", len(f.openAtAttach))
	}
	want := []sessionwatcher.OpenTurn{
		{ID: "turn-1", StartedAt: started},
		{ID: "turn-2", StartedAt: started.Add(time.Minute)},
	}
	if got := f.openAtAttach[0]; len(got) != 2 || got[0] != want[0] || got[1] != want[1] {
		t.Fatalf("OpenAtAttach = %v, want %v: each open row with its own start", got, want)
	}
}

// TestAnInstallWhoseOpenTurnsCannotBeReadHoldsNothing covers the read's
// failure: it happens before the map write, so the fleet is left as it was
// and the caller still owns the client it passed.
func TestAnInstallWhoseOpenTurnsCannotBeReadHoldsNothing(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.openTurnsErr = errFake

	// Act.
	err := f.fleet.Install(context.Background(), ws.ID, f.client)

	// Assert.
	if !errors.Is(err, errFake) {
		t.Fatalf("Install() = %v, want the failed read surfaced", err)
	}
	if _, held := f.fleet.Client(ws.ID); held {
		t.Fatal("the fleet holds the client of an install whose open turns could not be read")
	}
}

// TestABringUpHandsTheWatcherNoTurnsOpenAtAttach pins the snapshot's scope to
// an ADOPTION. A bring-up's own StartSession answers for a session this daemon
// is starting, and a turn row already written for it (a created workspace's
// first prompt, recorded before its session exists) is not one that ended
// unobserved.
func TestABringUpHandsTheWatcherNoTurnsOpenAtAttach(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.openTurns = []wsm.Turn{{ID: "turn-1", Workspace: ws.ID}}

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if len(f.openAtAttach) != 1 || len(f.openAtAttach[0]) != 0 {
		t.Fatalf("OpenAtAttach per watcher start = %v, want one start carrying none", f.openAtAttach)
	}
}

func TestAnInstallOfADeadShimIsRefusedAndHoldsNothing(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.reaped = true

	// Act.
	err := f.fleet.Install(context.Background(), ws.ID, f.client)

	// Assert.
	if !errors.Is(err, ErrInstallDeadShim) {
		t.Fatalf("Install() = %v, want ErrInstallDeadShim", err)
	}
	f.fleet.mu.RLock()
	_, held := f.fleet.sessions[ws.ID]
	f.fleet.mu.RUnlock()
	if held {
		t.Fatal("the fleet holds a dead client")
	}
}

func TestHandOverClosesTheWatchesBeforeItDetaches(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	var order []string
	watcher := &fakeWatcher{onClose: func() { order = append(order, fmt.Sprintf("close(detached=%d)", f.client.detached)) }}
	f.fleet.remember(ws.ID, &live{client: f.client, watcher: watcher})

	// Act.
	handed, err := f.fleet.HandOver(ws.ID)

	// Assert.
	if err != nil || !handed {
		t.Fatalf("HandOver = (%v, %v), want (true, nil)", handed, err)
	}
	if !watcher.closed || f.client.detached != 1 {
		t.Fatalf("closed=%v detached=%d, want the watches closed and the shim detached once", watcher.closed, f.client.detached)
	}
	if len(order) != 1 || order[0] != "close(detached=0)" {
		t.Fatalf("close order = %v, want the watches closed before the detach", order)
	}
}

// TestAHandedOverShimIsNoLongerTheFleets pins that the detached client leaves
// the session map: a workspace taken back after its adoption window expired
// is re-dialed by Adopt, never answered with the closed link.
func TestAHandedOverShimIsNoLongerTheFleets(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.fleet.remember(ws.ID, &live{client: f.client, watcher: &fakeWatcher{}})

	// Act.
	if _, err := f.fleet.HandOver(ws.ID); err != nil {
		t.Fatalf("HandOver: %v", err)
	}

	// Assert.
	if client, held := f.fleet.Client(ws.ID); held {
		t.Fatalf("Client after the handover = %v, want no client: the detached link is not the fleet's", client)
	}
}

func TestHandOverAnswersFalseForAWorkspaceWithNoSession(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")

	// Act.
	handed, err := f.fleet.HandOver(ws.ID)

	// Assert.
	if err != nil || handed {
		t.Fatalf("HandOver = (%v, %v), want (false, nil)", handed, err)
	}
	if f.client.detached != 0 {
		t.Fatalf("detached = %d, want nothing detached", f.client.detached)
	}
}

func TestHandOverSurfacesAFailedCloseAndDetachesNothing(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.fleet.remember(ws.ID, &live{client: f.client, watcher: &fakeWatcher{closeErr: errors.New("the sinks would not join")}})

	// Act.
	_, err := f.fleet.HandOver(ws.ID)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "the sinks would not join") {
		t.Fatalf("HandOver error = %v, want the failed close surfaced", err)
	}
	if f.client.detached != 0 {
		t.Fatalf("detached = %d, want nothing detached after a failed close", f.client.detached)
	}
}

func TestAPrelaunchBesideAnAdoptedRelaunchedShimTakesTheNextGeneration(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	f := newFleetFixture(t)
	f.socketDir = dir
	ws := f.workspace("w1")
	held := filepath.Join(dir, "w1.n1.sock")
	if err := os.WriteFile(held, nil, 0o600); err != nil {
		t.Fatalf("writing the adopted shim's socket path: %v", err)
	}

	// Act.
	got := f.fleet.freshSocketPath(ws.ID)

	// Assert.
	if want := filepath.Join(dir, "w1.n2.sock"); got != want {
		t.Fatalf("freshSocketPath = %q, want %q past the adopted shim's generation", got, want)
	}
}

func TestRaiseCarriedColdGateRaisesTheGateOverTheAdoptedShim(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1", HostSessionID: "host-1"}
	if err := f.fleet.Install(context.Background(), ws.ID, f.client); err != nil {
		t.Fatalf("Install: %v", err)
	}

	// Act.
	err := f.fleet.RaiseCarriedColdGate(context.Background(), ws.ID, &conversationv1.SessionCold{ContextTokens: 123456})

	// Assert.
	if err != nil {
		t.Fatalf("RaiseCarriedColdGate: %v", err)
	}
	if gate, standing := f.fleet.ColdGate(ws.ID); !standing || gate.VendorSessionID != "vendor-1" {
		t.Fatalf("ColdGate = (%+v, %v), want the carried gate raised on the parked conversation", gate, standing)
	}
}

func TestRaiseCarriedColdGateRefusesWithNoColdFacts(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Install(context.Background(), ws.ID, f.client); err != nil {
		t.Fatalf("Install: %v", err)
	}

	// Act.
	err := f.fleet.RaiseCarriedColdGate(context.Background(), ws.ID, nil)

	// Assert.
	if err == nil {
		t.Fatal("RaiseCarriedColdGate accepted no cold facts")
	}
	if _, standing := f.fleet.ColdGate(ws.ID); standing {
		t.Fatal("a gate was raised with no cold facts")
	}
}

func TestRaiseCarriedColdGateRefusesAWorkspaceWithNoShim(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")

	// Act.
	err := f.fleet.RaiseCarriedColdGate(context.Background(), ws.ID, &conversationv1.SessionCold{ContextTokens: 1})

	// Assert.
	if err == nil {
		t.Fatal("RaiseCarriedColdGate raised a gate over no shim")
	}
	if _, standing := f.fleet.ColdGate(ws.ID); standing {
		t.Fatal("a gate was raised over no shim")
	}
}
