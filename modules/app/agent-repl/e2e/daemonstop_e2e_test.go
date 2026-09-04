// daemonstop_e2e_test.go — the host's stop, measured against the WHOLE world.
//
// THE DEFECT, MEASURED. Over one 24-scenario Emacs e2e run the teardown's
// reaper found 17 surviving `claude-repld` processes, 24 surviving shims and
// 48 surviving `shim-lock` holders, and the container's resident memory
// climbed from 0.5 GiB to 3.1 GiB across a full run — enough that a second
// concurrent sandbox OOM-killed the first. Two causes, one per layer:
//
//  1. the daemon exited without standing its shims down. Every shim is spawned
//     into a process group of ITS OWN so a bounce can hand it to an adopting
//     successor, and `UpdateShutdownSchedule{now}` has no successor — its
//     announcement carries no address — so each one was left holding the
//     workspace's two kernel claims with nothing to adopt it; and
//  2. the Emacs teardown fired the stop and killed Emacs in the same breath,
//     taking the curl child carrying the request with it (fixed in
//     emacs_test.go's `daemonStopForm`).
//
// This file covers (1) against the real thing: the real store, the real
// sidecar, a real daemon and the real Node shim with its real `shim-lock`
// children. It deliberately does NOT drive Emacs — the leak belongs to the
// daemon, and asserting it here isolates the subject from the editor.
package e2e

import (
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"connectrpc.com/connect"
)

// straySettleBound is how long the kernel is given to finish reaping a process
// group the daemon already SIGKILLed and already waited on
// (shimclient.client.Kill signals the group and then blocks on the reap), plus
// the daemon's own exit.
//
// MEASURED: with the stand-down in place the stray set reads empty on the
// first poll every time across this test's -count=10 run. Two seconds is a
// hundred times that; anything still standing after it is a leak, not a slow
// reap.
const straySettleBound = 2 * time.Second

// TestHostRequestedStopLeavesNoProcessBehind pins the whole-tree guarantee the
// proto's `now` states ("stop accepting work, flush in-flight writes, go"):
// after the host's stop, nothing this world started is still running except
// the store and the sidecar, which the test owns and stands down itself.
//
// The workspace is parked at a PERMISSION GATE, which is the state the leak
// was worst in and the one a freeness-waiting stop could never get out of: the
// real shim's `canUseTool` callback is standing on an ask nobody will answer,
// so the turn never ends on its own.
func TestHostRequestedStopLeavesNoProcessBehind(t *testing.T) {
	t.Parallel()
	// Arrange: a live session with a real shim, parked on an open ask.
	w, ws := pmNewPermissionWorld(t)
	// Standing a live session down on purpose is what the whole test is
	// about; these are that act's own trail.
	w.ExpectWarnings(
		"daemon.shimclient.exit", "daemon.shimclient.kill_session",
		"daemon.shimclient.kill", "daemon.shimclient.redial",
		"daemon.sessionwatcher.link_fault", "daemon.sessionwatcher.watch_session",
		"daemon.sessionwatcher.watch_agent", "daemon.sessionwatcher.reopen",
		"daemon.shimclient.watch_agent", "daemon.shimclient.watch_session",
		"daemon.workspace.kill", "daemon.workspace.bring_up",
		"daemon.health.open_fault", "daemon.health.session",
	)
	SubmitPrompt(t, w, ws, "!perm-hold")
	pmAwaitFeedRow(t, w, ws, "the open permission ask", func(r *frontendv1.FeedRow) bool {
		return r.GetPermission().GetOpen() != nil
	})
	before := w.StrayPIDs()
	if len(before) < 2 {
		t.Fatalf("only %d process(es) name this world's state root before the stop (%v); the daemon and at least one shim were expected, so this test could pass without reclaiming anything", len(before), before)
	}

	// Act: exactly what Emacs sends (lisp/daemon.el's
	// `agent-repl-frontend-daemon-stop`).
	resp, err := w.Client().UpdateShutdownSchedule(w.Ctx(), connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
		Action: &agentreplv1.UpdateShutdownScheduleRequest_Now{Now: &agentreplv1.UpdateShutdownScheduleNow{
			Reason: &agentreplv1.DrainReason{Kind: &agentreplv1.DrainReason_Operator{
				Operator: &agentreplv1.DrainReasonOperator{Note: "emacs"},
			}},
		}},
	}))

	// Assert
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("UpdateShutdownSchedule{now} = (%v, %v), want a success", resp, err)
	}
	w.AwaitExit()
	if left := awaitWorldStraysGone(t, w, straySettleBound); len(left) > 0 {
		t.Fatalf("the host's stop left %d process(es) alive: %v — a shim outliving its daemon keeps ~95 MiB resident and holds the workspace lock that refuses the next session", len(left), left)
	}
	// The store and the sidecar are the test's OWN and were spared from the
	// stray key, so their survival has to be stated separately: a stop that
	// reclaimed them too would be over-broad in exactly the way the exemption
	// exists to prevent.
	if w.Store.Exited() {
		t.Errorf("the host's stop took the store down with it; the store is the test's own process, not the daemon's tree")
	}
	if w.Sidecar.Exited() {
		t.Errorf("the host's stop took the sidecar down with it; the sidecar is the test's own process, not the daemon's tree")
	}
}

// awaitWorldStraysGone polls until nothing but the test's own spared processes
// names the world's state root, answering whatever is left at the bound.
func awaitWorldStraysGone(t *testing.T, w *World, bound time.Duration) []int {
	t.Helper()
	deadline := time.Now().Add(bound)
	ticker := time.NewTicker(10 * time.Millisecond)
	defer ticker.Stop()
	for {
		left := w.StrayPIDs()
		if len(left) == 0 || time.Now().After(deadline) {
			return left
		}
		<-ticker.C
	}
}
