package workspace

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/wsm"
)

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
