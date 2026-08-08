// DRAIN-LEASE DURABILITY ACROSS A DAEMON BOUNCE, end to end.
//
// WHY DURABILITY IS PART OF THE CONTRACT AND NOT A NICETY. The whole point of
// the lease is to survive the bounce it is scheduling. A daemon that lost the
// schedule when it died mid-drain would come back believing nothing was
// pending — the exact silent-drop the frozen comment forbids ("a daemon that
// crashes mid-drain reboots knowing a schedule existed rather than silently
// dropping it") — and a daemon that lost the HELD PROMPTS would turn "your
// prompt is delayed" into "your prompt is gone", which is the failure the whole
// feature exists to prevent.
//
// WHY THIS FILE BOOTS TWO DAEMONS. A bounce is two daemons over one durable
// state directory, which is what shutdownWorld.boot is for. The shim processes
// are NOT torn down by a bounce: a shim outlives its daemon by design, redials
// the one well-known socket, and is parked by the next daemon's listener — so
// the surviving shim here is the production shape, not a harness shortcut.
package e2e

import (
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// snapshotQueue returns the QueueView a StateSnapshot carries for workspace, or
// nil. The connect snapshot is the right place to read this: a client joining
// after the bounce must see the held prompt WITHOUT waiting for an edge.
func snapshotQueue(snap *frontendv1.StateSnapshot, workspace string) *frontendv1.QueueView {
	for _, view := range snap.GetQueues() {
		if view.GetWorkspace() == workspace {
			return view
		}
	}
	return nil
}

// snapshotSessionID returns the session a StateSnapshot reports for workspace.
func snapshotSessionID(t *testing.T, snap *frontendv1.StateSnapshot, workspace string) string {
	t.Helper()
	for _, view := range snap.GetSessions() {
		if view.GetCwd() == workspace {
			return view.GetSessionId()
		}
	}
	t.Fatalf("the connect StateSnapshot reports no session for workspace %s", workspace)
	return ""
}

// TestE2EAScheduledShutdownSurvivesADaemonBounceMidDrain covers the CRASH
// MID-DRAIN: the daemon goes away with the lease held and prompts held under
// it, and the daemon that replaces it still knows both.
func TestE2EAScheduledShutdownSurvivesADaemonBounceMidDrain(t *testing.T) {
	// Arrange — a lease held by workspace A's turn, and a prompt held under it
	// in the idle workspace B.
	// Tempdirs before the world: cleanups run LIFO, so this tears the daemons
	// and their shims down before the directories are removed.
	cwdA, cwdB := t.TempDir(), t.TempDir()
	world := newShutdownWorld(t)
	first := world.boot(t)
	h := first.harness()
	_, connA, _, _ := liveSession(t, h, cwdA)
	_, connB, _, _ := liveSession(t, h, cwdB)
	holdTurnOpen(t, connA, cwdA, "r-hold-a", "sleep e2e-durable-drain")
	frontend := first.dialFrontend(t)
	draining := scheduleAndAwaitDraining(t, frontend, "r-sched", true, "deploy that changed the shim bundle")
	writeCmd(t, connB, `{"requestId":"r-b-prompt","submitPrompt":{"text":"held across the bounce","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)
	held := awaitHeldEntry(t, connB, cwdB, "a QueueEntry bearing shutdown_hold")

	// Act — the daemon dies with the drain outstanding, and a new one opens the
	// same durable state.
	first.bounce()
	second := world.boot(t)

	// Assert — the replacement daemon still holds the lease...
	snap := connectSnapshot(t, second)
	view := snap.GetShutdownSchedule()
	// Reported rather than fatal: the lease and what the lease was holding are
	// independent facts about the SAME post-bounce snapshot, and a bounce that
	// lost both should say so once rather than one failure at a time. The
	// getters below are nil-safe, so a missing arm reads as its zero value.
	if view.GetDraining() == nil {
		t.Errorf("after the bounce the shutdown_schedule is %T, want the draining arm: a schedule that existed must not be silently dropped by the crash it was scheduling", view.GetState())
	}
	if got, want := view.GetDraining().GetScheduleId(), draining.GetScheduleId(); got != want {
		t.Errorf("after the bounce schedule_id = %q, want the schedule the previous daemon minted, %q", got, want)
	}
	if got, want := view.GetDraining().GetStopShims(), true; got != want {
		t.Errorf("after the bounce stop_shims = %v, want %v: it is a property of WHAT was rebuilt and cannot be re-decided by a restart", got, want)
	}
	if got, want := view.GetDraining().GetCause(), "deploy that changed the shim bundle"; got != want {
		t.Errorf("after the bounce cause = %q, want %q", got, want)
	}

	// ... and still holds what the lease was holding.
	queue := snapshotQueue(snap, cwdB)
	if queue == nil {
		t.Errorf("after the bounce the snapshot carries no QueueView for %s at all: the held prompt is unaccounted for", cwdB)
	}
	// GetEntries is nil-safe, so the lookup below runs either way; the entry
	// assertions are then guarded so a missing view is reported once instead of
	// cascading into every fact that depends on it.
	var restored *frontendv1.QueueEntry
	for _, entry := range queue.GetEntries() {
		if entry.GetId() == held.GetId() {
			restored = entry
		}
	}
	if queue != nil && restored == nil {
		t.Errorf("after the bounce the queue for %s no longer carries the held entry %q: a held prompt is delayed, never lost", cwdB, held.GetId())
	}
	if restored != nil {
		if got, want := restored.GetShutdown().GetScheduleId(), draining.GetScheduleId(); got != want {
			t.Errorf("the restored entry names schedule_id %q, want %q: it must still be joined to the lease still holding it", got, want)
		}
	}
}

// TestE2EAnExecutedDrainLeavesNoLeaseOnTheNextBoot covers the other half of
// durability: the lease is persisted so a CRASH cannot drop it, which must not
// turn into a lease that outlives the bounce it already performed. A drain that
// executed is finished, and the daemon that comes up after it reports idle —
// the real value, not an absent field.
func TestE2EAnExecutedDrainLeavesNoLeaseOnTheNextBoot(t *testing.T) {
	// Arrange — a lease with exactly one hold.
	cwd := t.TempDir()
	world := newShutdownWorld(t)
	first := world.boot(t)
	_, conn, _, _ := liveSession(t, first.harness(), cwd)
	holdTurnOpen(t, conn, cwd, "r-hold", "sleep e2e-executed-drain")
	frontend := first.dialFrontend(t)
	scheduleAndAwaitDraining(t, frontend, "r-sched", false, "ordinary bounce")

	// Act — clear the hold, let the bounce run, then come back up.
	writeCmd(t, conn, `{"requestId":"r-interrupt","interrupt":{}}`)
	awaitExecutedShutdown(t, first)
	first.bounce()
	second := world.boot(t)

	// Assert
	view := connectSnapshot(t, second).GetShutdownSchedule()
	if view == nil {
		t.Fatal("the connect StateSnapshot carried no shutdown_schedule at all: idle is a real broadcast value, so an absent field is never a legal answer")
	}
	if view.GetIdle() == nil {
		t.Fatalf("after an EXECUTED drain the shutdown_schedule is %T, want the idle arm: the bounce it was holding for has already happened", view.GetState())
	}
}

// TestE2EAPromptHeldByAnExecutedDrainIsDeliveredAfterTheSwap covers the
// promise the whole lease rests on: a prompt the drain held is DELAYED, never
// lost. It survives the daemon swap the drain performed and is delivered by
// the daemon that comes up on the other side.
func TestE2EAPromptHeldByAnExecutedDrainIsDeliveredAfterTheSwap(t *testing.T) {
	// Arrange — A's turn is the only hold; B's prompt is held by the lease.
	cwdA, cwdB := t.TempDir(), t.TempDir()
	world := newShutdownWorld(t)
	first := world.boot(t)
	h := first.harness()
	_, connA, _, _ := liveSession(t, h, cwdA)
	_, connB, _, _ := liveSession(t, h, cwdB)
	holdTurnOpen(t, connA, cwdA, "r-hold-a", "sleep e2e-delivered-after-swap")
	frontend := first.dialFrontend(t)
	scheduleAndAwaitDraining(t, frontend, "r-sched", false, "ordinary bounce")
	const prompt = "delivered on the other side of the bounce"
	writeCmd(t, connB, `{"requestId":"r-b-prompt","submitPrompt":{"text":"`+prompt+`","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)
	awaitHeldEntry(t, connB, cwdB, "a QueueEntry bearing shutdown_hold")

	// Act — clear A's hold so the bounce runs, then bring the successor up.
	writeCmd(t, connA, `{"requestId":"r-interrupt","interrupt":{}}`)
	awaitExecutedShutdown(t, first)
	first.bounce()
	second := world.boot(t)

	// Assert — the successor delivers what the drain held.
	//
	// THE SCOPED CONNECTION IS DIALLED BEFORE THE SWEEP IS DRIVEN, and the
	// order is load-bearing rather than incidental. B's session is what has to
	// wire for the held prompt to be delivered, and wiring it is the boot
	// sweeper's re-check pass — so a subscriber dialled AFTER that pass can miss
	// the delivery it triggered. Subscribe first, then drive the event, then
	// wait for its consequence.
	snap := connectSnapshot(t, second)
	sessionB := snapshotSessionID(t, snap, cwdB)
	afterConn := second.harness().dial(t, sessionB)
	if firstFrame := readFrame(t, afterConn); firstFrame.GetSnapshot() == nil {
		t.Fatalf("first scoped frame = %T, want a StateSnapshot", firstFrame.GetFrame())
	}
	// B's shim outlived the bounce and is redialling; the re-check is the pass
	// that claims it. Fired on the OBSERVED redial rather than on test phase
	// order, so it cannot land before there is anything to claim.
	second.sweepRecheckWhenParked(t, sessionB)
	var terminal *frontendv1.TurnAccounting
	awaitAll(t, afterConn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the held prompt's own reply, from the daemon on the other side of the bounce": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, cwdB) {
				if strings.Contains(assistantText(item), echoOf(prompt)) {
					return true
				}
			}
			return false
		},
		// The accounting no longer rides the terminal item; the item's ARRIVAL
		// is the signal that the successor daemon finished the turn, and the
		// evidence itself is read from the durable ledger below.
		"the successor daemon's terminal result": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, cwdB) {
				if item.GetAgent().GetTurnResult() != nil {
					return true
				}
			}
			return false
		},
	})
	if terminal == nil || terminal.GetQueryInstanceId() == "" || terminal.GetRuntime() == nil || terminal.GetRuntime().GetVendorSessionId() == "" || terminal.GetUsageAtStart() == nil || terminal.GetUsageAtEnd() == nil || len(terminal.GetResponses()) == 0 {
		t.Fatalf("post-restart turn accounting omitted reconstructed query/runtime/usage evidence: %+v", terminal)
	}
	for _, problem := range terminal.GetInvalid().GetProblems() {
		if incomplete := problem.GetRuntimeIdentityIncomplete(); incomplete != nil {
			for _, path := range incomplete.GetMissingFieldPaths() {
				if path == "query_lifecycle["+terminal.GetQueryInstanceId()+"].runtime_observed" {
					t.Fatalf("post-restart terminal accounting lost its runtime snapshot: %+v", terminal)
				}
			}
		}
	}
}
