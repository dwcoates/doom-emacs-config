//go:build integration

package integration

import (
	"agentrepl/protohelpers/rosterwalk"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/integration/harness"
)

// A BACKLOGGED WORKSPACE'S CACHE IS KEPT UNTIL IT LAPSES (owner request,
// 2026-10-13; docs/protobuf-design/backlogged-cache-lapse.md): hiding holds the
// shim's keep-alives, the held shim projects its lapse, the backlogged row counts
// down to it, and the session is stood down when it comes.

// backlogRow finds a workspace's row in the backlog.
func backlogRow(roster *frontendv1.RosterPush, id string) *frontendv1.RosterRow {
	for _, r := range rosterwalk.BacklogRows(roster.GetWorkspaces()) {
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

// backloggedAndHeld opens a workspace and hides it. The daemon drives every
// keep-alive (2026-10-26), so a backlogged workspace is simply sent none: there is
// no cadence to wait on, and the hide is recorded before its call answers.
func backloggedAndHeld(t *testing.T) *fixture {
	t.Helper()
	f := newInGroup(t, groupBackloggedKeepalives)
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCStartSession, &shimv1.StartSessionRequest{})
	hide(t, f.d, f.ws)
	return f
}

func TestABackloggedWorkspaceIsStoodDownAndStruckThroughWhenItsGatedCacheLapses(t *testing.T) {
	t.Parallel()
	// Arrange
	f := backloggedAndHeld(t)
	f.d.ExpectWarnings(standDownWarnings...)
	f.remoteControlOff()
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
	awaitRoster(t, f.d, roster, "the backlogged row struck through at its cold gate", func(r *frontendv1.RosterPush) bool {
		return backlogRow(r, f.ws.GetId()).GetColdGate() != nil
	})
}

// A CLOSED WORKSPACE GETS ONE LAST KEEP-ALIVE ROUND, THEN THE COLD GATE (owner
// request, 2026-10-17): closing leaves the session up, the daemon sends the
// one beat still owed, and the session is stood down when the lapse the shim
// projects comes, so reopening it meets the cold gate.
