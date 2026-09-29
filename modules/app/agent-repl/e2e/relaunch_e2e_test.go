// relaunch_e2e_test.go — the host's stop and the daemon that replaces it,
// measured against the WHOLE world.
//
// THE DEFECT (playbook 19, plan J58). A stop stands every session down, and
// Emacs then re-announces every workspace it holds to the daemon that
// replaces it. For a workspace whose panel was ALREADY mounted, that
// announcement is the only edge the fresh daemon gets — the mount happened
// against the daemon that died, so no OpenWorkspace follows it. The relaunched
// daemon registered the workspace, served its page and its WatchFeed, and
// answered ZERO rows: the pre-stop turn's two bubbles were gone and the footer
// read `ready` rather than `done`, for a conversation the store still held
// whole.
//
// This file covers it against the real thing — the real store, the real
// sidecar, two real daemons and the real Node shim — because the rehydration
// is exactly the seam a fake shim cannot vouch for: the rows the second daemon
// draws come out of the store's own book, served by a shim that resumed the
// conversation the first daemon recorded.
package e2e

import (
	"context"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"claude-repld/integration/harness"
)

// rlStopAndRelaunch performs the host's own stop and boots the daemon that
// replaces it: the SAME state root, kernel-lock dir, store socket and ACCOUNT
// ROOTS, which are exactly the facts a relaunch on one host keeps. A stop is
// not a crash — every session is stood down and every shim is reaped — so the
// successor adopts nothing and must revive from the durable record alone.
func rlStopAndRelaunch(t *testing.T, w *World) *harness.Daemon {
	t.Helper()
	// Standing a live session down on purpose is what this act IS; these are
	// its own trail.
	w.ExpectWarnings(
		"daemon.shimclient.exit", "daemon.shimclient.kill_session",
		"daemon.shimclient.kill", "daemon.shimclient.redial",
		"daemon.sessionwatcher.link_fault", "daemon.sessionwatcher.watch_session",
		"daemon.sessionwatcher.watch_agent", "daemon.sessionwatcher.reopen",
		"daemon.shimclient.watch_agent", "daemon.shimclient.watch_session",
		"daemon.workspace.kill", "daemon.workspace.bring_up",
		"daemon.health.open_fault", "daemon.health.session",
	)
	// ON ITS OWN BOUND, NOT ON WHAT THE RUN HAS LEFT: everything before this
	// has already spent out of w.Ctx(), so handing the remainder to the stop
	// would make it answer for time it never spent.
	stopCtx, cancelStop := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancelStop()
	resp, err := w.Client().UpdateShutdownSchedule(stopCtx, connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
		Action: &agentreplv1.UpdateShutdownScheduleRequest_Now{Now: &agentreplv1.UpdateShutdownScheduleNow{
			Reason: &agentreplv1.DrainReason{Kind: &agentreplv1.DrainReason_Operator{
				Operator: &agentreplv1.DrainReasonOperator{Note: "emacs"},
			}},
		}},
	}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("UpdateShutdownSchedule{now} = (%v, %v), want a success", resp, err)
	}
	w.AwaitExit()
	// THE OLD WORLD IS GONE BEFORE THE NEW ONE STARTS, and this is the
	// relaunch's own precondition rather than test hygiene: the stood-down
	// shim's `shim-lock` child holds the SESSION lock for the vendor
	// conversation, and a successor that resumes while it still stands is
	// answered `conversation_owned` — correctly, because a second process
	// really would be sharing one conversation. Emacs's own ensure is a human
	// interaction later; this is that same gap, taken as an event.
	if left := awaitWorldStraysGone(t, w, straySettleBound); len(left) > 0 {
		t.Fatalf("the host's stop left %d process(es) alive: %v — the relaunch cannot resume a conversation another process still holds", len(left), left)
	}

	// THE LOCK DIRECTORY AND THE LOCK BINARY ARE RESTATED by SuccessorOpts:
	// without the binary the revived shim's kernel claim dies `spawn ENOENT`
	// and the resume is refused `lock_holder_unavailable`: nobody holds the
	// conversation, the shim's own lock helper is missing.
	opts := w.SuccessorOpts(t)
	// THE ACCOUNT ROOTS ARE THE ONES THE CONVERSATION WAS FILED UNDER. A
	// resume is filed against the vendor transcript, and a successor that
	// minted its own roots would find none and come up FRESH — abandoning the
	// very conversation this test is about.
	opts.ExtraArgs = []string{
		"--default-config-dir", w.DefaultConfigDir,
		"--multi-repo-config-dir", w.MultiRepoConfigDir,
	}
	// The revival spawns a SECOND real shim process inside the registration
	// this test then asserts on, which is the same
	// two-process-lifecycles-on-one-budget shape the adoption chain has.
	opts.Timeout = AdoptionChainTimeout
	successor := harness.StartDaemon(t, opts)
	successor.ExpectWarnings("daemon.rollout.reconcile")
	return successor
}

// TestARelaunchedDaemonKeepsTheConversationItStoodDown is J58's assertion
// against the real quartet: after a stop and a relaunch, the announcement
// Emacs sends is what brings the conversation back, and the page the new
// daemon serves still draws the pre-stop turn.
func TestARelaunchedDaemonKeepsTheConversationItStoodDown(t *testing.T) {
	t.Parallel()
	// Arrange: a real turn run to its terminal, so the store durably holds
	// the conversation's own rows.
	w := NewWorld(t, WorldOpts{})
	w.ExpectWarnings("daemon.rollout.reconcile")
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "prose-streamed")

	// Act: the host's stop, a fresh daemon, and the announcement Emacs makes
	// when the link comes back.
	successor := rlStopAndRelaunch(t, w)
	again := harness.Register(t, successor, repo.Dir)
	if again.GetId() != ws.GetId() {
		t.Fatalf("RegisterWorkspace after the relaunch = %q, want the same workspace %q", again.GetId(), ws.GetId())
	}

	// Assert: the pre-stop turn is on the page the relaunched daemon serves.
	adAwaitReplayedFeedRow(t, successor, ws, "the pre-stop turn, rehydrated after the relaunch",
		endsTurn(turn))
	adAwaitReplayedFeedRow(t, successor, ws, "the pre-stop turn's prompt bubble, rehydrated after the relaunch",
		func(row *frontendv1.FeedRow) bool {
			return row.GetTurn().GetValue() == turn.GetValue() && row.GetUserPrompt() != nil
		})

	// Assert: and the footer reconciles to the terminal the turn recorded,
	// rather than reporting a conversation that has never run.
	rlAwaitFooterDone(t, successor, ws)
}

// rlAwaitFooterDone waits for the relaunched daemon's footer to read
// idle·done, holding both client hops open for the wait's life — the footer's
// connectivity truth is drawn from them (daemon.md invariant 11), and a footer
// stream opened alone reports a disconnected workspace forever.
func rlAwaitFooterDone(t *testing.T, d *harness.Daemon, ws *workspacev1.WorkspaceRef) {
	t.Helper()
	host := d.WatchHost(ws)
	defer host.Close()
	web := d.WatchWeb(ws)
	defer web.Close()
	stream := d.WatchFooter(ws)
	defer stream.Close()
	ctx, cancel := context.WithTimeout(d.Ctx(), DefaultTimeout)
	defer cancel()
	harness.AwaitView(t, ctx, stream, "idle.done after the relaunch", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle().GetDone() != nil
	})
}
