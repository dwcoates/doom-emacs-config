// rollout_e2e_test.go — RollOutBuild: how a DEPLOY reaches a running stack.
//
// The deploy chain builds with its bounce turned off and calls RollOutBuild;
// the daemon rolls the build out the way it rolls out a self-merge. The whole
// point is the difference from UpdateShutdownSchedule{now}: a rollout waits for
// each workspace's FREENESS and ENDS NO TURN. The scenarios here are that
// difference, against a real daemon, a real shim and the fake SDK:
//
//   - a free workspace is handed over at once, and the incumbent exits;
//   - a workspace MID-TURN is waited on — its turn keeps running through the
//     announcement — and is handed over only once the turn has ended;
//   - a rollout already in flight refuses a second one, naming the holdout;
//   - a SUCCESSOR takes the next rollout in turn — every daemon after the first
//     handover booted as a successor, so this is the ordinary case, not an edge.
//
// adoption_e2e_test.go covers the handover's own mechanics (the rendezvous,
// refusal ordering, the successor's adoption) under the self-merge trigger.
// This file covers the OTHER trigger and the invariant that justifies it.
package e2e

import (
	"context"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"
)

// roDaemonRebuilt is the request a deploy that rebuilt the daemon sends.
func roDaemonRebuilt() *connect.Request[agentreplv1.RollOutBuildRequest] {
	return connect.NewRequest(&agentreplv1.RollOutBuildRequest{Daemon: &agentreplv1.RollOutBuildDaemon{}})
}

// roAwaitAnnounced answers the handover's announcement off WatchDaemon.
func roAwaitAnnounced(t *testing.T, w *World, stream *harness.Stream[*agentreplv1.WatchDaemonResponse]) *agentreplv1.DaemonShutdownAnnounced {
	t.Helper()
	return harness.AwaitView(t, w.Ctx(), stream, "shutdown_announced", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetShutdownAnnounced() != nil
	}).GetShutdownAnnounced()
}

func TestRollOutBuildHandsOverAFreeWorkspace(t *testing.T) {
	t.Parallel()
	// Arrange: a HEADLESS workspace, which is free and has no rendezvous.
	_, w := adSelfRepoWorld(t)
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	daemonStream := w.WatchDaemonStream()
	defer daemonStream.Close()

	// Act
	resp, err := w.Client().RollOutBuild(w.Ctx(), roDaemonRebuilt())

	// Assert: accepted as a handover that waits on nobody.
	if err != nil {
		t.Fatalf("RollOutBuild: %v", err)
	}
	handover := resp.Msg.GetSuccess().GetHandover()
	if handover == nil {
		t.Fatalf("RollOutBuild = %v, want success.handover", resp.Msg)
	}
	if handover.GetBusy() != 0 {
		t.Fatalf("handover.busy = %d, want 0: nothing is running", handover.GetBusy())
	}

	// Assert: the announcement carries the successor, and the incumbent exits
	// in an orderly way once the workspace has moved.
	addr := roAwaitAnnounced(t, w, daemonStream).GetAddress()
	if addr == "" {
		t.Fatal("shutdown_announced.address is unset, want the successor's address")
	}
	if code := w.AwaitExit(); code != 0 {
		t.Fatalf("the incumbent's exit code = %d, want an orderly 0 after the handover", code)
	}
	adAwaitAddrFileChange(t, w.Daemon, addr)
	if _, err := adDial(addr).SelectWorkspace(w.Ctx(), connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: ws})); err != nil {
		t.Fatalf("SelectWorkspace on the successor = error %v, want the transferred workspace served", err)
	}
}

func TestRollOutBuildWaitsOutARunningTurnAndEndsNothing(t *testing.T) {
	t.Parallel()
	// Arrange: a workspace whose turn is parked on a foreground shell that
	// concludes on nothing but an interrupt — a turn that is RUNNING for as
	// long as this test lets it.
	_, w := adSelfRepoWorld(t)
	// The sidecar forwards its per-file diagnostics to the daemon it was
	// started against, and one can land after the workspace has moved. The
	// daemon refuses it correctly (transferring_away) — but ClientLogError has
	// no such arm yet, so the refusal leaves as a failed_precondition and is
	// recorded as UNLANDED. That gap is the refusal-arm work's to close
	// (docs/investigations/2026-09-21-doom-workspace-ux.md, item 8's class);
	// adoption_e2e_test.go's TestRefusalOrderingDuringHandover declares the
	// same record for the same reason.
	w.ExpectWarnings("daemon.refusal.unlanded_arm.standing", "daemon.refusal.unlanded_arm")
	ws := dbWorkspace(t, w)
	initial, feed := dbOpenRootFeed(t, w, ws)
	defer feed.Close()
	turn := SubmitPrompt(t, w, ws, "!bash-hold")
	held := func(row *frontendv1.FeedRow) bool {
		call := row.GetActivity().GetSimpleToolCall()
		return call.GetName().GetText() == "Bash" &&
			strings.Contains(call.GetInput().GetText(), "tail -f /var/log/system.log") &&
			call.GetRunning() != nil
	}
	sawHeld := false
	for _, row := range initial {
		sawHeld = sawHeld || held(row)
	}
	if !sawHeld {
		ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
		harness.AwaitView(t, ctx, feed, "the live foreground Bash unit the turn parks on", held)
		cancel()
	}
	daemonStream := w.WatchDaemonStream()
	defer daemonStream.Close()

	// Act: roll out a rebuilt daemon WHILE the turn runs.
	resp, err := w.Client().RollOutBuild(w.Ctx(), roDaemonRebuilt())

	// Assert: accepted, and it says the workspace is what it waits on.
	if err != nil {
		t.Fatalf("RollOutBuild: %v", err)
	}
	if busy := resp.Msg.GetSuccess().GetHandover().GetBusy(); busy != 1 {
		t.Fatalf("RollOutBuild = %v, want success.handover with busy = 1", resp.Msg)
	}
	roAwaitAnnounced(t, w, daemonStream)

	// Assert: THE HANDOVER IS PARKED ON THE RUNNING TURN. A second rollout is
	// refused naming exactly this workspace as the holdout, which can only be
	// true while the first has announced and not transferred it.
	again, err := w.Client().RollOutBuild(w.Ctx(), roDaemonRebuilt())
	if err != nil {
		t.Fatalf("second RollOutBuild: %v", err)
	}
	waiting := again.Msg.GetError().GetAlreadyRollingOut().GetWaitingOn()
	if len(waiting) != 1 || waiting[0] != ws.GetId() {
		t.Fatalf("second RollOutBuild = %v, want error.already_rolling_out waiting on %q", again.Msg, ws.GetId())
	}

	// Act: the turn ends the way its USER ends it — the rollout never did.
	stop, err := w.Client().Interrupt(w.Ctx(), connect.NewRequest(&agentreplv1.InterruptRequest{
		Workspace: ws,
		Target:    &agentreplv1.InterruptRequest_Turn{Turn: &agentreplv1.InterruptTurn{}},
	}))
	if err != nil {
		t.Fatalf("Interrupt(turn): %v", err)
	}

	// Assert: THE TURN WAS STILL THERE TO INTERRUPT. Had the rollout ended it,
	// the interrupt would have found nothing running.
	if stop.Msg.GetSuccess().GetInterruptedTurn() == nil {
		t.Fatalf("Interrupt(turn) = %v, want success.interrupted_turn: the rollout must have left the turn running", stop.Msg)
	}
	if ended := AwaitTurnEnded(t, w, ws, turn).GetTurnEnded(); ended.GetInterrupted() == nil {
		t.Fatalf("turn ended = %v, want the user's own interrupt as its only ending", ended)
	}

	// Assert: with the workspace free, the handover completes.
	if code := w.AwaitExit(); code != 0 {
		t.Fatalf("the incumbent's exit code = %d, want an orderly 0 once the turn had ended", code)
	}
}

func TestASuccessorTakesTheNextRollOut(t *testing.T) {
	t.Parallel()
	// Arrange: one completed handover, so the daemon now serving BOOTED AS A
	// SUCCESSOR — which is every daemon a second deploy ever talks to. Found
	// live on 2026-09-21: the first deploy after a handover was refused as
	// `joining` by a daemon that had finished joining minutes before.
	_, w := adSelfRepoWorld(t)
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	daemonStream := w.WatchDaemonStream()
	defer daemonStream.Close()
	if _, err := w.Client().RollOutBuild(w.Ctx(), roDaemonRebuilt()); err != nil {
		t.Fatalf("first RollOutBuild: %v", err)
	}
	first := roAwaitAnnounced(t, w, daemonStream).GetAddress()
	if code := w.AwaitExit(); code != 0 {
		t.Fatalf("the first incumbent's exit code = %d, want an orderly 0", code)
	}
	adAwaitAddrFileChange(t, w.Daemon, first)
	successor := adDial(first)

	// Act: the next deploy asks the successor to roll out in turn.
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	announcements, err := successor.WatchDaemon(ctx, connect.NewRequest(&agentreplv1.WatchDaemonRequest{Client: &agentreplv1.WatchDaemonRequest_Emacs{Emacs: &agentreplv1.WatchDaemonEmacs{ElispBuild: "elisp-test"}}}))
	if err != nil {
		t.Fatalf("WatchDaemon on the successor: %v", err)
	}
	defer announcements.Close()
	resp, err := successor.RollOutBuild(w.Ctx(), roDaemonRebuilt())

	// Assert: accepted as a handover, not refused as `joining`.
	if err != nil {
		t.Fatalf("RollOutBuild on the successor: %v", err)
	}
	if resp.Msg.GetSuccess().GetHandover() == nil {
		t.Fatalf("RollOutBuild on the successor = %v, want success.handover: it finished joining when it took the workspace", resp.Msg)
	}

	// Assert: it hands over to a THIRD daemon, which then serves the workspace.
	second := ""
	for second == "" && announcements.Receive() {
		second = announcements.Msg().GetShutdownAnnounced().GetAddress()
	}
	if second == "" {
		t.Fatalf("the successor never announced its own handover: %v", announcements.Err())
	}
	if second == first {
		t.Fatalf("the second handover announced %q, the successor's own address", second)
	}
	adAwaitAddrFileChange(t, w.Daemon, second)
	if _, err := adDial(second).SelectWorkspace(w.Ctx(), connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: ws})); err != nil {
		t.Fatalf("SelectWorkspace on the third daemon = error %v, want the workspace served", err)
	}
}
