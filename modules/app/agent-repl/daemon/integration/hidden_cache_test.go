//go:build integration

package integration

import (
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
	for _, r := range roster.GetHidden().GetRows().GetRows() {
		if r.GetWorkspace().GetWorkspace().GetId() == id {
			return r
		}
	}
	return nil
}

// cacheLapse is the projection a held shim states.
func cacheLapse(at time.Time, gatedTokens uint64) *conversationv1.SessionUpdate {
	lapse := &conversationv1.SessionCacheLapse{LapsesAtMs: at.UnixMilli()}
	if gatedTokens > 0 {
		lapse.AfterLapse = &conversationv1.SessionCacheLapse_Gated{Gated: &conversationv1.SessionCacheLapseGated{ContextTokens: gatedTokens}}
	} else {
		lapse.AfterLapse = &conversationv1.SessionCacheLapse_Ungated{Ungated: &conversationv1.SessionCacheLapseUngated{}}
	}
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_CacheLapse{CacheLapse: lapse}}
}

// hiddenAndHeld opens a workspace, hides it, and waits for the daemon to hold
// its shim's keep-alives.
func hiddenAndHeld(t *testing.T) *fixture {
	t.Helper()
	f := newOpened(t, harness.Opts{})
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCStartSession, &shimv1.StartSessionRequest{})
	hide(t, f.d, f.ws)
	held := &shimv1.SetKeepaliveRequest{}
	f.d.AwaitShimLoggedRequestMatching(f.repo.Dir, harness.RPCSetKeepalive, "the keep-alives held", held, func() bool {
		return held.GetHold() != nil
	})
	return f
}

func TestHidingAWorkspaceHoldsItsKeepalivesAndStandsNothingDown(t *testing.T) {
	t.Parallel()
	// Arrange, Act
	f := hiddenAndHeld(t)

	// Assert: the hold is recorded, and nothing was stood down for it.
	f.d.AwaitLogRecord(f.d.RunLogPath(), "the keeper's hold", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.cachelapse.cadence" && r.Context["cadence"] == "hold"
	})
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

func TestShowingAHiddenWorkspaceRunsItsKeepalivesAgain(t *testing.T) {
	t.Parallel()
	// Arrange
	f := hiddenAndHeld(t)

	// Act
	show(t, f.d, f.ws)

	// Assert
	run := &shimv1.SetKeepaliveRequest{}
	f.d.AwaitShimLoggedRequestMatching(f.repo.Dir, harness.RPCSetKeepalive, "the keep-alives run again", run, func() bool {
		return run.GetRun() != nil
	})
}

// SPC j R: one keep-alive to every hidden workspace, each cadence left held
// (docs/protobuf-design/hidden-keepalives-on-demand.md).
func TestSendingTheHiddenKeepalivesAsksEachHeldShimForOneAndLeavesItHeld(t *testing.T) {
	t.Parallel()
	// Arrange
	f := hiddenAndHeld(t)

	// Act
	resp, err := f.d.Client().SendHiddenKeepalives(f.d.Ctx(), connect.NewRequest(&agentreplv1.SendHiddenKeepalivesRequest{}))

	// Assert: the shim was asked once, answered sent, and its cadence is still held.
	if err != nil {
		t.Fatalf("SendHiddenKeepalives = error %v", err)
	}
	workspaces := resp.Msg.GetSuccess().GetWorkspaces()
	if len(workspaces) != 1 || workspaces[0].GetWorkspace().GetId() != f.ws.GetId() || workspaces[0].GetSent() == nil {
		t.Fatalf("SendHiddenKeepalives = %v, want the one hidden workspace sent", resp.Msg)
	}
	shim := f.d.OpenedShim(f.ws)
	if n := shim.Count(harness.RPCSendKeepalive); n != 1 {
		t.Fatalf("the shim was asked for %d keep-alives, want one", n)
	}
	var last any
	for _, r := range f.d.RunLog() {
		if r.Operation == "daemon.cachelapse.cadence" {
			last = r.Context["cadence"]
		}
	}
	if last != "hold" {
		t.Fatalf("the shim's cadence was last set to %v, want it left held", last)
	}
}

func TestTheKeepalivesAnswerMovesTheHiddenRowsCountdown(t *testing.T) {
	t.Parallel()
	// Arrange: a lapse projected, then the keep-alive sent.
	f := hiddenAndHeld(t)
	roster := f.d.WatchRoster()
	shim := f.d.OpenedShim(f.ws)
	shim.PushSessionUpdate(cacheLapse(time.Now().Add(10*time.Minute), 0))
	if _, err := f.d.Client().SendHiddenKeepalives(f.d.Ctx(), connect.NewRequest(&agentreplv1.SendHiddenKeepalivesRequest{})); err != nil {
		t.Fatalf("SendHiddenKeepalives = error %v", err)
	}
	later := time.Now().Add(time.Hour)

	// Act: the held shim restates its lapse once the keep-alive's answer re-anchors.
	shim.PushSessionUpdate(cacheLapse(later, 0))

	// Assert
	awaitRoster(t, f.d, roster, "the hidden row counting down to the later lapse", func(r *frontendv1.WorkspaceRoster) bool {
		return hiddenRow(r, f.ws.GetId()).GetWhen().GetHidden().GetCache().GetLapsesAtMs() == later.UnixMilli()
	})
}
