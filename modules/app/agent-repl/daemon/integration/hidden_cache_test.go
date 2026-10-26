//go:build integration

package integration

import (
	"agentrepl/protohelpers/rosterwalk"
	"testing"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/integration/harness"
)

// A HIDDEN WORKSPACE'S CACHE IS KEPT UNTIL IT LAPSES (owner request,
// 2026-10-13; docs/protobuf-design/hidden-cache-lapse.md): hiding holds the
// shim's keep-alives, the held shim projects its lapse, the hidden row counts
// down to it, and the session is stood down when it comes.

// hiddenRow finds a workspace's row in the hidden section.
func hiddenRow(roster *frontendv1.WorkspaceRoster, id string) *frontendv1.RosterRow {
	for _, r := range rosterwalk.HiddenRows(roster) {
		if r.GetWorkspace().GetWorkspace().GetId() == id {
			return r
		}
	}
	return nil
}

// cacheLapse is the projection a shim states.
func cacheLapse(at time.Time, gatedTokens uint64) *conversationv1.SessionUpdate {
	lapse := &conversationv1.SessionCacheLapse{LapsesAtMs: at.UnixMilli()}
	if gatedTokens > 0 {
		lapse.AfterLapse = &conversationv1.SessionCacheLapse_Gated{Gated: &conversationv1.SessionCacheLapseGated{ContextTokens: gatedTokens}}
	} else {
		lapse.AfterLapse = &conversationv1.SessionCacheLapse_Ungated{Ungated: &conversationv1.SessionCacheLapseUngated{}}
	}
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_CacheLapse{CacheLapse: lapse}}
}

// hiddenAndHeld opens a workspace and hides it. The daemon drives every
// keep-alive (2026-10-26), so a hidden workspace is simply sent none: there is
// no cadence to wait on, and the hide is recorded before its call answers.
func hiddenAndHeld(t *testing.T) *fixture {
	t.Helper()
	f := newOpened(t, harness.Opts{})
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCStartSession, &shimv1.StartSessionRequest{})
	hide(t, f.d, f.ws)
	return f
}

func TestHidingAWorkspaceStandsNothingDown(t *testing.T) {
	t.Parallel()
	// Arrange, Act
	f := hiddenAndHeld(t)

	// Assert
	if n := f.d.OpenedShim(f.ws).Count(harness.RPCHibernate); n != 0 {
		t.Fatalf("the hidden workspace was asked to hibernate %d times at the hide, want none", n)
	}
}

func TestAHiddenRowCountsDownToTheLapseItsShimProjected(t *testing.T) {
	t.Parallel()
	// Arrange
	f := hiddenAndHeld(t)
	roster := f.d.WatchRoster()
	lapsesAt := time.Now().Add(time.Hour)

	// Act
	f.d.OpenedShim(f.ws).PushSessionUpdate(cacheLapse(lapsesAt, 0))

	// Assert
	awaitRoster(t, f.d, roster, "the hidden row counting down to the lapse", func(r *frontendv1.WorkspaceRoster) bool {
		return hiddenRow(r, f.ws.GetId()).GetWhen().GetHidden().GetCache().GetLapsesAtMs() == lapsesAt.UnixMilli()
	})
}

func TestAHiddenWorkspaceIsStoodDownAndStruckThroughWhenItsGatedCacheLapses(t *testing.T) {
	t.Parallel()
	// Arrange
	f := hiddenAndHeld(t)
	f.d.ExpectWarnings(standDownWarnings...)
	roster := f.d.WatchRoster()

	// Act: the projection names a lapse already come.
	f.d.OpenedShim(f.ws).PushSessionUpdate(cacheLapse(time.Now().Add(-time.Minute), 120_000))

	// Assert: the idle sweep's stand-down, graceful, and the row struck through.
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCHibernate, &shimv1.HibernateRequest{})
	killed := &shimv1.KillSessionRequest{}
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCKillSession, killed)
	if killed.GetForce() {
		t.Fatal("the lapsed workspace's KillSession was forced, want a graceful stand-down")
	}
	awaitRoster(t, f.d, roster, "the hidden row struck through at its cold gate", func(r *frontendv1.WorkspaceRoster) bool {
		return hiddenRow(r, f.ws.GetId()).GetColdGate() != nil
	})
}

// sendHiddenKeepalives asks the daemon for SPC j R's keep-alives and answers
// its response, failing the test on a transport error.
func sendHiddenKeepalives(t *testing.T, d *harness.Daemon) *agentreplv1.SendHiddenKeepalivesResponse {
	t.Helper()
	resp, err := d.Client().SendHiddenKeepalives(d.Ctx(), connect.NewRequest(&agentreplv1.SendHiddenKeepalivesRequest{}))
	if err != nil {
		t.Fatalf("SendHiddenKeepalives = error %v", err)
	}
	return resp.Msg
}

// SPC j R: one keep-alive to every hidden workspace
// (docs/protobuf-design/hidden-keepalives-on-demand.md).
func TestSendingTheHiddenKeepalivesAsksEachHiddenShimForOne(t *testing.T) {
	t.Parallel()
	// Arrange
	f := hiddenAndHeld(t)

	// Act
	resp := sendHiddenKeepalives(t, f.d)

	// Assert: the shim was asked once, and answered sent.
	workspaces := resp.GetSuccess().GetWorkspaces()
	if len(workspaces) != 1 || workspaces[0].GetWorkspace().GetId() != f.ws.GetId() || workspaces[0].GetSent() == nil {
		t.Fatalf("SendHiddenKeepalives = %v, want the one hidden workspace sent", resp)
	}
	shim := f.d.OpenedShim(f.ws)
	if n := shim.Count(harness.RPCSendKeepalive); n != 1 {
		t.Fatalf("the shim was asked for %d keep-alives, want one", n)
	}
}

func TestTheKeepalivesAnswerMovesTheHiddenRowsCountdown(t *testing.T) {
	t.Parallel()
	// Arrange: a lapse projected, then the keep-alive sent.
	f := hiddenAndHeld(t)
	roster := f.d.WatchRoster()
	shim := f.d.OpenedShim(f.ws)
	shim.PushSessionUpdate(cacheLapse(time.Now().Add(10*time.Minute), 0))
	sendHiddenKeepalives(t, f.d)
	later := time.Now().Add(time.Hour)

	// Act: the held shim restates its lapse once the keep-alive's answer re-anchors.
	shim.PushSessionUpdate(cacheLapse(later, 0))

	// Assert
	awaitRoster(t, f.d, roster, "the hidden row counting down to the later lapse", func(r *frontendv1.WorkspaceRoster) bool {
		return hiddenRow(r, f.ws.GetId()).GetWhen().GetHidden().GetCache().GetLapsesAtMs() == later.UnixMilli()
	})
}

// A CLOSED WORKSPACE GETS ONE LAST KEEP-ALIVE ROUND, THEN THE COLD GATE (owner
// request, 2026-10-17): closing leaves the session up, the daemon sends the
// one beat still owed, and the session is stood down when the lapse the shim
// projects comes, so reopening it meets the cold gate.

// closedAndLastRound opens a workspace and closes it. The close is recorded
// before its call answers.
func closedAndLastRound(t *testing.T) *fixture {
	t.Helper()
	f := newOpened(t, harness.Opts{})
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCStartSession, &shimv1.StartSessionRequest{})
	if _, err := f.d.Client().CloseWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Requester: harness.User(), Workspace: f.ws})); err != nil {
		t.Fatalf("CloseWorkspace = error %v", err)
	}
	return f
}

func TestClosingAWorkspaceStandsNothingDown(t *testing.T) {
	t.Parallel()
	// Arrange, Act
	f := closedAndLastRound(t)

	// Assert
	if n := f.d.OpenedShim(f.ws).Count(harness.RPCHibernate); n != 0 {
		t.Fatalf("the closed workspace was asked to hibernate %d times at the close, want none", n)
	}
}

func TestAClosedWorkspaceIsStoodDownWhenItsCacheLapses(t *testing.T) {
	t.Parallel()
	// Arrange
	f := closedAndLastRound(t)
	f.d.ExpectWarnings(standDownWarnings...)

	// Act: the spent round's projection names a lapse already come.
	f.d.OpenedShim(f.ws).PushSessionUpdate(cacheLapse(time.Now().Add(-time.Minute), 120_000))

	// Assert: the idle sweep's stand-down, graceful.
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCHibernate, &shimv1.HibernateRequest{})
	killed := &shimv1.KillSessionRequest{}
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCKillSession, killed)
	if killed.GetForce() {
		t.Fatal("the lapsed closed workspace's KillSession was forced, want a graceful stand-down")
	}
}
