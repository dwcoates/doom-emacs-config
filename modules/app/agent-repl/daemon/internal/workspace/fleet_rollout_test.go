package workspace

import (
	"context"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

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
	f.db.sessions["w1"] = wsm.Session{
		Workspace:       "w1",
		HostSessionID:   "host-1",
		VendorSessionID: "vendor-1",
		ConfigDir:       "/roots/default",
	}

	// Act.
	facts, ok, err := f.fleet.HostSessionFacts(context.Background(), "w1")

	// Assert.
	if err != nil || !ok {
		t.Fatalf("HostSessionFacts = (%+v, %v, %v), want the session's facts", facts, ok, err)
	}
	if facts.SessionID != "host-1" || facts.VendorSessionID != "vendor-1" || facts.ConfigDir != "/roots/default" {
		t.Fatalf("facts = %+v, want the recorded identities", facts)
	}
	if facts.ShimAttached {
		t.Fatal("facts report the shim attached with no live session, want false")
	}
	if facts.BackfillKnown {
		t.Fatal("facts claim to know the backfill; the daemon holds no store client and can state none")
	}
}

// TestHostSessionFactsAnswersNoSessionForAWorkspaceWithNone covers the host
// stream's `none` arm: a registered workspace that never had a session.
func TestHostSessionFactsAnswersNoSessionForAWorkspaceWithNone(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)

	// Act.
	_, ok, err := f.fleet.HostSessionFacts(context.Background(), "w1")

	// Assert.
	if err != nil {
		t.Fatalf("HostSessionFacts = error %v, want no session as an answer", err)
	}
	if ok {
		t.Fatal("HostSessionFacts reports a session for a workspace that never had one")
	}
}
