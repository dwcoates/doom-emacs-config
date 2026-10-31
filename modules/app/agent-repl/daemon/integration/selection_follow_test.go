//go:build integration

package integration

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/integration/harness"

	"connectrpc.com/connect"
)

// TestASelectOnTheSuccessorReachesTheIncumbentsRosterMidHandover is the
// 2026-10-11 tab-bar freeze end to end: a handover kept open by a workspace
// the successor has not adopted yet leaves BOTH daemons serving a roster, and
// Emacs's roster stream still on the incumbent. A select of a workspace the
// successor already owns lands on the successor; the incumbent's roster must
// follow it through the state file both share, or the editor, which moves its
// frame only on its roster, never switches.
func TestASelectOnTheSuccessorReachesTheIncumbentsRosterMidHandover(t *testing.T) {
	t.Parallel()
	// Arrange: two open workspaces, so the incumbent outlives the first one's
	// adoption, and a roster watched on the incumbent as Emacs's is.
	selfRepo, d := drainSelfRepoDaemon(t, adoptAwaitsParticipantsEnv)
	d.ExpectWarnings("daemon.refusal.unlanded_arm.standing")
	held := drainOpenWorkspace(t, d)
	held.shim.ExpectStartSession()
	held.shim.ExpectWatchSession()
	moved := drainOpenWorkspace(t, d)
	moved.shim.ExpectStartSession()
	moved.shim.ExpectWatchSession()
	webs := map[string]*harness.Stream[*agentreplv1.WatchWebWorkspaceResponse]{}
	for _, f := range []*fixture{held, moved} {
		host := d.WatchHost(f.ws)
		webs[f.ws.GetId()] = d.WatchWeb(f.ws)
		harness.AwaitNext(t, d.Ctx(), host, "the fresh host push")
	}
	if _, err := d.Client().SelectWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: held.ws})); err != nil {
		t.Fatalf("SelectWorkspace(held) on the incumbent = error %v, want a success", err)
	}
	roster := d.WatchRoster()
	harness.AwaitView(t, d.Ctx(), roster, "the incumbent's roster naming the held workspace", func(r *frontendv1.WorkspaceRoster) bool {
		return r.GetCurrent().GetWorkspace().GetId() == held.ws.GetId()
	})
	daemonStream := d.WatchDaemonStream()
	drainTriggerDeploy(t, d, selfRepo, harness.DeployStaleDaemon)
	addr := harness.AwaitView(t, d.Ctx(), daemonStream, "shutdown_announced", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetShutdownAnnounced() != nil
	}).GetShutdownAnnounced().GetAddress()
	successor := drainDial(addr)
	harness.AwaitView(t, d.Ctx(), webs[moved.ws.GetId()], "transferred", func(r *agentreplv1.WatchWebWorkspaceResponse) bool {
		return r.GetTransferred() != nil
	})
	if o := adoptBothParticipants(d, successor, moved.ws); o.hostErr != nil || o.webErr != nil {
		t.Fatalf("adopting the moved workspace = host %v, web %v; want both to succeed", o.hostErr, o.webErr)
	}

	// Act: the user switches to the moved workspace, which the successor owns.
	resp, err := successor.SelectWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: moved.ws}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("SelectWorkspace(moved) on the successor = (%v, %v), want a success", resp, err)
	}

	// Assert: the incumbent's roster, still the editor's, names the switch.
	harness.AwaitView(t, d.Ctx(), roster, "the incumbent's roster naming the moved workspace", func(r *frontendv1.WorkspaceRoster) bool {
		return r.GetCurrent().GetWorkspace().GetId() == moved.ws.GetId()
	})

	// Cleanup: the held workspace transfers too, so the incumbent leaves in
	// order rather than at the test's end.
	harness.AwaitView(t, d.Ctx(), webs[held.ws.GetId()], "transferred", func(r *agentreplv1.WatchWebWorkspaceResponse) bool {
		return r.GetTransferred() != nil
	})
	if o := adoptBothParticipants(d, successor, held.ws); o.hostErr != nil || o.webErr != nil {
		t.Fatalf("adopting the held workspace = host %v, web %v; want both to succeed", o.hostErr, o.webErr)
	}
	if code := d.AwaitExit(); code != 0 {
		t.Fatalf("the incumbent's exit code = %d, want an orderly 0 after the last transfer", code)
	}
}
