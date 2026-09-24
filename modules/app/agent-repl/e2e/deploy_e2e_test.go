// deploy_e2e_test.go — Deploy: how a deploy reaches a running stack.
//
// The daemon owns deploys: it builds every component into staging, judges
// each running process's build against the fresh one by content hash, and
// puts into service what is out of date. A stale DAEMON is replaced by the
// blue-green handover, exactly as a self-merge landing's one deploy replaces
// it. The whole point is the difference from UpdateShutdownSchedule{now}: an
// unforced deploy waits for each workspace's FREENESS and ENDS NO TURN. The
// scenarios here are that difference, against a real daemon, a real shim,
// the real store and sidecar (whose own build reports the deploy reads, so it
// restarts neither) and the fake SDK, with the deploy's build staged as one
// whose daemon is not the running one (harness.DeployStaleDaemon):
//
//   - a free workspace is handed over at once, and the incumbent exits;
//   - a workspace MID-TURN is waited on — its turn keeps running through the
//     announcement — and is handed over only once the turn has ended;
//   - a handover already in flight refuses a second deploy, naming the
//     holdout;
//   - a SUCCESSOR takes the next deploy in turn — every daemon after the
//     first handover booted as a successor, so this is the ordinary case, not
//     an edge.
//
// adoption_e2e_test.go covers the handover's own mechanics (the rendezvous,
// refusal ordering, the successor's adoption) under the self-merge landing.
// This file covers the Deploy rpc and the invariant that justifies it.
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

// dpUnforced is an unforced deploy: it ends no turn.
func dpUnforced() *connect.Request[agentreplv1.DeployRequest] {
	return connect.NewRequest(&agentreplv1.DeployRequest{})
}

// dpStaleDaemonWorld is a world whose deploys find the daemon stale.
func dpStaleDaemonWorld(t *testing.T) *World {
	t.Helper()
	_, w := adSelfRepoWorld(t)
	w.StageDeployBuild(harness.DeployStaleDaemon)
	return w
}

// dpOutcome answers a successful deploy's decision for one component, or
// fails naming the whole answer.
func dpOutcome(t *testing.T, resp *connect.Response[agentreplv1.DeployResponse], component agentreplv1.DeployComponent) *agentreplv1.DeployComponentOutcome {
	t.Helper()
	for _, o := range resp.Msg.GetSuccess().GetComponents() {
		if o.GetComponent() == component {
			return o
		}
	}
	t.Fatalf("Deploy = %v, want success deciding %s", resp.Msg, component)
	return nil
}

// dpHandingOver answers a deploy's daemon decision, which must be the
// handover.
func dpHandingOver(t *testing.T, resp *connect.Response[agentreplv1.DeployResponse]) *agentreplv1.DeployHandingOver {
	t.Helper()
	daemon := dpOutcome(t, resp, agentreplv1.DeployComponent_DEPLOY_COMPONENT_DAEMON)
	if daemon.GetHandingOver() == nil {
		t.Fatalf("Deploy's daemon decision = %v, want handing_over", daemon)
	}
	return daemon.GetHandingOver()
}

// dpAwaitAnnounced answers the handover's announcement off WatchDaemon.
func dpAwaitAnnounced(t *testing.T, w *World, stream *harness.Stream[*agentreplv1.WatchDaemonResponse]) *agentreplv1.DaemonShutdownAnnounced {
	t.Helper()
	return harness.AwaitView(t, w.Ctx(), stream, "shutdown_announced", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetShutdownAnnounced() != nil
	}).GetShutdownAnnounced()
}

func TestADeployThatFindsTheDaemonStaleHandsOverAFreeWorkspace(t *testing.T) {
	t.Parallel()
	// Arrange: a HEADLESS workspace, which is free and has no rendezvous.
	w := dpStaleDaemonWorld(t)
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	daemonStream := w.WatchDaemonStream()
	defer daemonStream.Close()

	// Act
	resp, err := w.Client().Deploy(w.Ctx(), dpUnforced())

	// Assert: the daemon is handed over, waiting on nobody.
	if err != nil {
		t.Fatalf("Deploy: %v", err)
	}
	if handover := dpHandingOver(t, resp); handover.GetBusy() != 0 || handover.GetForced() {
		t.Fatalf("handing_over = %v, want busy = 0 and unforced: nothing is running", handover)
	}
	// Assert: the real services report the fresh build, so the deploy left
	// them alone.
	for _, service := range []agentreplv1.DeployComponent{
		agentreplv1.DeployComponent_DEPLOY_COMPONENT_STORE, agentreplv1.DeployComponent_DEPLOY_COMPONENT_SIDECAR,
	} {
		if o := dpOutcome(t, resp, service); o.GetUpToDate() == nil {
			t.Fatalf("%s = %v, want up to date: the world's real service runs the staged binary", service, o)
		}
	}
	if got := len(w.Launchctl.Invocations()); got != 0 {
		t.Fatalf("launchctl invocations = %d, want none: no service was out of date", got)
	}

	// Assert: the announcement carries the successor, and the incumbent exits
	// in an orderly way once the workspace has moved.
	addr := dpAwaitAnnounced(t, w, daemonStream).GetAddress()
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

func TestADeployOverARunningTurnWaitsItOutAndEndsNothing(t *testing.T) {
	t.Parallel()
	// Arrange: a workspace whose turn is parked on a foreground shell that
	// concludes on nothing but an interrupt — a turn that is RUNNING for as
	// long as this test lets it.
	w := dpStaleDaemonWorld(t)
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

	// Act: deploy a daemon build WHILE the turn runs.
	resp, err := w.Client().Deploy(w.Ctx(), dpUnforced())

	// Assert: the daemon is handed over, and it says the workspace is what
	// it waits on.
	if err != nil {
		t.Fatalf("Deploy: %v", err)
	}
	if handover := dpHandingOver(t, resp); handover.GetBusy() != 1 || handover.GetForced() {
		t.Fatalf("handing_over = %v, want busy = 1 and unforced", handover)
	}
	dpAwaitAnnounced(t, w, daemonStream)

	// Assert: THE HANDOVER IS PARKED ON THE RUNNING TURN. A second deploy is
	// refused naming exactly this workspace as the holdout, which can only be
	// true while the first has announced and not transferred it.
	again, err := w.Client().Deploy(w.Ctx(), dpUnforced())
	if err != nil {
		t.Fatalf("second Deploy: %v", err)
	}
	waiting := again.Msg.GetError().GetAlreadyRollingOut().GetWaitingOn()
	if len(waiting) != 1 || waiting[0] != ws.GetId() {
		t.Fatalf("second Deploy = %v, want error.already_rolling_out waiting on %q", again.Msg, ws.GetId())
	}

	// Act: the turn ends the way its USER ends it — the deploy never did.
	stop, err := w.Client().Interrupt(w.Ctx(), connect.NewRequest(&agentreplv1.InterruptRequest{
		Workspace: ws,
		Target:    &agentreplv1.InterruptRequest_Turn{Turn: &agentreplv1.InterruptTurn{}},
	}))
	if err != nil {
		t.Fatalf("Interrupt(turn): %v", err)
	}

	// Assert: THE TURN WAS STILL THERE TO INTERRUPT. Had the deploy ended it,
	// the interrupt would have found nothing running.
	if stop.Msg.GetSuccess().GetInterruptedTurn() == nil {
		t.Fatalf("Interrupt(turn) = %v, want success.interrupted_turn: the deploy must have left the turn running", stop.Msg)
	}
	if ended := AwaitTurnEnded(t, w, ws, turn).GetTurnEnded(); ended.GetInterrupted() == nil {
		t.Fatalf("turn ended = %v, want the user's own interrupt as its only ending", ended)
	}

	// Assert: with the workspace free, the handover completes.
	if code := w.AwaitExit(); code != 0 {
		t.Fatalf("the incumbent's exit code = %d, want an orderly 0 once the turn had ended", code)
	}
}

func TestASuccessorThatFinishedJoiningAcceptsADeploy(t *testing.T) {
	t.Parallel()
	// Arrange: one completed handover, so the daemon now serving BOOTED AS A
	// SUCCESSOR — which is every daemon a second deploy ever talks to. Found
	// live on 2026-09-21: the first deploy after a handover was refused as
	// `joining` by a daemon that had finished joining minutes before. The
	// staged build stays the stale-daemon one, so the successor — running the
	// same binary the incumbent did — finds itself stale in turn.
	w := dpStaleDaemonWorld(t)
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	daemonStream := w.WatchDaemonStream()
	defer daemonStream.Close()
	if resp, err := w.Client().Deploy(w.Ctx(), dpUnforced()); err != nil {
		t.Fatalf("first Deploy: %v", err)
	} else {
		dpHandingOver(t, resp)
	}
	first := dpAwaitAnnounced(t, w, daemonStream).GetAddress()
	if code := w.AwaitExit(); code != 0 {
		t.Fatalf("the first incumbent's exit code = %d, want an orderly 0", code)
	}
	adAwaitAddrFileChange(t, w.Daemon, first)
	successor := adDial(first)

	// Act: the next deploy asks the successor.
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	// The Emacs states the checkout's own elisp, so the deploy pushes it no
	// reload and the only thing it is shown is the handover.
	announcements, err := successor.WatchDaemon(ctx, connect.NewRequest(&agentreplv1.WatchDaemonRequest{Client: &agentreplv1.WatchDaemonRequest_Emacs{Emacs: &agentreplv1.WatchDaemonEmacs{ElispBuild: harness.PinnedElispBuild}}}))
	if err != nil {
		t.Fatalf("WatchDaemon on the successor: %v", err)
	}
	defer announcements.Close()
	resp, err := successor.Deploy(w.Ctx(), dpUnforced())

	// Assert: decided as a handover, not refused as `joining`.
	if err != nil {
		t.Fatalf("Deploy on the successor: %v", err)
	}
	if resp.Msg.GetError().GetJoining() != nil {
		t.Fatalf("Deploy on the successor = %v, want a handover: it finished joining when it took the workspace", resp.Msg)
	}
	dpHandingOver(t, resp)

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
