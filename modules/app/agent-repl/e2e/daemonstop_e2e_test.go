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
//     taking the exchange carrying the request with it (fixed in
//     emacs_test.go's `daemonStopForm`).
//
// This file covers (1) against the real thing: the real store, the real
// sidecar, a real daemon and the real Node shim with its real `shim-lock`
// children. It deliberately does NOT drive Emacs — the leak belongs to the
// daemon, and asserting it here isolates the subject from the editor.
package e2e

import (
	"context"
	"path/filepath"
	"strings"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"

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
	// THE STOP IS THE DAEMON'S OWN ORDERED STAND-DOWN, so no system writes a
	// WARN or ERROR for it: the daemon's sweep runs with nothing declared, and
	// assertNoWarningInAnySystemLog reads the shim's, the store's and the
	// sidecar's logs too.
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
	//
	// ON ITS OWN BOUND, NOT ON WHAT THE RUN HAS LEFT. w.Ctx() is the WHOLE
	// RUN's budget, and everything above has already spent out of it: a real
	// daemon boot, a store, a sidecar, a real Node shim spawn and a turn driven
	// to an open permission ask. Handing that remainder to the stop makes this
	// call answer for time it did not spend -- the stop itself measures 9ms p50
	// and 15ms max across 104 runs of this shape, so a `deadline_exceeded` here
	// on a shared budget names the wrong step. DefaultTimeout is ~330x the
	// measured max, which is a failure bound for the stop and nothing else.
	stopCtx, cancelStop := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancelStop()
	resp, err := w.Client().UpdateShutdownSchedule(stopCtx, connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
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
	assertNoWarningInAnySystemLog(t, w)
}

// assertNoWarningInAnySystemLog fails the test on every WARN or worse in ANY
// log this world wrote: the daemon's run log and workspace sinks, the shim's,
// the sidecar's and the store's. The daemon's own sweep reads only the
// daemon's.
//
// It pins what a bounce relies on (2026-10-03): the stand-down
// `UpdateShutdownSchedule{now}` performs is the daemon's own, so it ends every
// shim against a store that is still up, every shim concludes its session
// cleanly, and the daemon reads every exit as one it ordered. A bounce that
// killed the shims and the store from outside instead put `shim died` in the
// daemon's log and `the store could not be reached` in every shim's.
func assertNoWarningInAnySystemLog(t *testing.T, w *World) {
	t.Helper()
	pattern := filepath.Join(w.StateDir, "logs", "*.log")
	paths, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob %s: %v", pattern, err)
	}
	if len(paths) == 0 {
		t.Fatalf("no log matches %s, so this assertion could pass reading nothing", pattern)
	}
	for _, path := range paths {
		for _, r := range harness.ReadLog(t, path) {
			switch r.Level {
			case "warn", "warning", "error", "fatal":
				t.Errorf("%s: %s %s: %s", filepath.Base(path), r.Level, r.Operation, r.Message)
			}
		}
	}
}

// stopAfterTurnBound is how long the host's stop may take on a session whose
// turn RAN AND ENDED, with its watches still standing — the shape every
// playbook finishes in.
//
// IT IS A FAILURE BOUND FOR ONE SPECIFIC STALL. The shim's teardown concludes
// every open `WatchAgent` tail through its book's head and waits for the tail
// to serve it, bounded by `WATCHER_CONCLUSION_BUDGET_MS` (1s). A tail that can
// never reach that head — the store streams an upsert of an old row at its
// ORIGINAL pointer, so the newest pointer a tail has served walks BACKWARD —
// spends the whole budget, and the daemon's stop is waiting inside it. That is
// what this measures: a stop anywhere near a second has ridden that budget.
//
// MEASURED across a -count=8 run against the real quartet: 7.4ms at the best,
// 9.7ms at the worst. 250ms is ~26x that worst case and a quarter of the budget
// a stalled tail would burn, so nothing healthy can reach it and nothing
// stalled can hide under it.
const stopAfterTurnBound = 250 * time.Millisecond

// TestAStopAfterACompletedTurnLeavesOnTheStop pins the cost of the ordinary
// stop against the WHOLE world: a real store, a real sidecar, a real daemon and
// the real Node shim, with a turn driven to its terminal and every watch of the
// session still standing.
//
// It is the e2e half of daemon/integration's test of the same name, which
// measures the same stop against the fake shim. Only this one exercises the
// real shim's teardown, which is where the tail conclusion lives.
func TestAStopAfterACompletedTurnLeavesOnTheStop(t *testing.T) {
	// This assertion is a latency measurement. Run it outside the suite's
	// parallel world burst so the interval stays attributable to the stop path.
	// Arrange: a live session whose turn has run and ended.
	w, ws := pmNewPermissionWorld(t)
	// THE STOP IS THE DAEMON'S OWN ORDERED STAND-DOWN, so no system writes a
	// WARN or ERROR for it: the daemon's sweep runs with nothing declared, and
	// assertNoWarningInAnySystemLog reads the shim's, the store's and the
	// sidecar's logs too.
	turn := SubmitPrompt(t, w, ws, "!prose-streamed")
	AwaitTurnEnded(t, w, ws, turn)

	// Act: exactly what Emacs sends, on its own bound for the reason the test
	// above states.
	stopCtx, cancelStop := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancelStop()
	started := time.Now()
	resp, err := w.Client().UpdateShutdownSchedule(stopCtx, connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
		Action: &agentreplv1.UpdateShutdownScheduleRequest_Now{Now: &agentreplv1.UpdateShutdownScheduleNow{
			Reason: &agentreplv1.DrainReason{Kind: &agentreplv1.DrainReason_Operator{
				Operator: &agentreplv1.DrainReasonOperator{Note: "emacs"},
			}},
		}},
	}))
	stop := time.Since(started)

	// Assert
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("UpdateShutdownSchedule{now} = (%v, %v), want a success", resp, err)
	}
	if stop > stopAfterTurnBound {
		t.Errorf("the stop took %v, over the %v bound — the shim's teardown rode a conclusion budget rather than ending its tails", stop, stopAfterTurnBound)
	}
	w.AwaitExit()
	t.Logf("the host's stop after a completed turn took %v", stop)
	assertNoWarningInAnySystemLog(t, w)
}

// TestACompletedTurnAndTeardownLeaveNoWatchTokenOutstanding is the store-side
// half of the same stop: the shim's reads must leave the store holding nothing.
//
// THE DEFECT, MEASURED. OpenAgentSession is unary and the store's service has
// no close, so a watch token minted for a page the caller then abandons can
// never be reclaimed — it lives for the store's whole process lifetime. The
// shim performs exactly two such reads per turn (the turn's opening page and
// the teardown's book head), and the singleton store's registry grew by two
// per turn for as long as the daemon ran. Both now open `page_only`, so the
// store mints nothing for them.
//
// The registry's size is stated in ONE place — the store's own shutdown
// record — so the assertion stops the store and reads it there, the same way
// the cold-bring-up tests read the store's refusals out of its real log.
func TestACompletedTurnAndTeardownLeaveNoWatchTokenOutstanding(t *testing.T) {
	t.Parallel()
	// Arrange: a live session whose turn has run and ended.
	w, ws := pmNewPermissionWorld(t)
	// Standing a live session down on purpose is what produces the teardown
	// this test is about; these are that act's own trail.
	w.ExpectWarnings(
		"daemon.shimclient.exit", "daemon.shimclient.kill_session",
		"daemon.shimclient.kill", "daemon.shimclient.redial",
		"daemon.sessionwatcher.link_fault", "daemon.sessionwatcher.watch_session",
		"daemon.sessionwatcher.watch_agent", "daemon.sessionwatcher.reopen",
		"daemon.shimclient.watch_agent", "daemon.shimclient.watch_session",
		"daemon.workspace.kill", "daemon.workspace.bring_up",
		"daemon.health.open_fault", "daemon.health.session",
	)
	turn := SubmitPrompt(t, w, ws, "!prose-streamed")
	AwaitTurnEnded(t, w, ws, turn)

	// The host's stop is what runs the teardown, and the teardown's book-head
	// read is the second of the two one-shot reads.
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

	// Act: the store counts its registry as it stands down.
	w.Store.Stop()

	// Assert.
	assertStoreHeldNoOutstandingTokens(t, w)
}

// assertStoreHeldNoOutstandingTokens reads the store's shutdown record — the
// one record that states the token registry's size — and requires it to be
// empty. The record's own vocabulary is what it matches on (the operation plus
// the `outstanding_tokens=` field), never the surrounding prose.
func assertStoreHeldNoOutstandingTokens(t *testing.T, w *World) {
	t.Helper()
	for _, rec := range harness.ReadLog(t, w.Store.LogPath) {
		if rec.Operation != "store.shutdown" || !strings.Contains(rec.Message, "outstanding_tokens=") {
			continue
		}
		if !strings.Contains(rec.Message, "outstanding_tokens=0") {
			t.Fatalf("the store stood down holding watch tokens nothing will ever spend: %s", rec.Message)
		}
		return
	}
	t.Fatalf("the store wrote no shutdown record stating its outstanding tokens; its log holds %d records", len(harness.ReadLog(t, w.Store.LogPath)))
}

// awaitWorldStraysGone polls until nothing but the test's own spared processes// awaitWorldStraysGone polls until nothing but the test's own spared processes
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
