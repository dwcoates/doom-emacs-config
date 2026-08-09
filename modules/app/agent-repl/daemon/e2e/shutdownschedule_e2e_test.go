// THE SCHEDULED-SHUTDOWN DRAIN LEASE, end to end over the REAL processes.
//
// THE INCIDENT THIS EXISTS FOR. A daemon bounce taken immediately after a
// workspace merge killed every in-flight session mid-work. The requirement is
// therefore not "bounce more gently" but "do not bounce YET": the shutdown is
// SCHEDULED, it takes a lease the moment it is scheduled, the lease blocks new
// turns daemon-side at the prompt queue, and the bounce executes the instant
// the system has drained.
//
// WHAT THESE TESTS ARE PINNING. The lease is a DAEMON-SIDE fact, not a webapp
// affordance: prompts arrive from Emacs, the webapp, the merge machinery and
// the workspace inbox, so a hold enforced anywhere but the queue would be a
// hold with holes in it. Every assertion here is therefore made against the
// wire — ShutdownScheduleView broadcasts, StateSnapshot.shutdown_schedule,
// QueueEntry.shutdown_hold, CommandAcks — through a real shim, a real store, a
// real SSM and the real queue.
//
// WHAT IS DELIBERATELY NOT HERE.
//   - No timeout on a hold. A vendor turn that hangs forever holds the drain
//     forever, and that is the CORRECT fail-fast behavior: the alternative is a
//     bounce that kills exactly the mid-work session the feature exists to
//     protect. So there is no "the drain gives up eventually" test, because
//     there is no such behavior to cover.
//   - Durability across a bounce lives in shutdownscheduledurability_e2e_test.go,
//     which is the only file here that needs two daemons.
//
// Reuses shutdownscheduleharness_test.go's world/boot and e2e_test.go /
// interrupt_e2e_test.go / clearcompact_e2e_test.go helpers READ-ONLY.
package e2e

import (
	"fmt"
	"strings"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"github.com/gorilla/websocket"
)

// --- shared arrangement ------------------------------------------------------

// scheduleAndAwaitDraining sends a ScheduleShutdownCmd and waits for BOTH the
// ok CommandAck and the draining broadcast, in any order.
//
// Order-independence is the point: the ack is enqueued after the dispatch
// returns while the lease broadcast is pushed from inside it, so pinning an
// arrival order would assert an implementation detail rather than the contract.
func scheduleAndAwaitDraining(t *testing.T, conn *websocket.Conn, requestID string, stopShims bool, cause string) *frontendv1.ShutdownScheduleDraining {
	t.Helper()
	sendSchedule(t, conn, requestID, stopShims, cause)
	var (
		ack      *frontendv1.CommandAck
		draining *frontendv1.ShutdownScheduleDraining
	)
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the CommandAck for the schedule": func(frame *frontendv1.FrontendFrame) bool {
			if a := ackFor(frame, requestID); a != nil {
				ack = a
				return true
			}
			return false
		},
		"a ShutdownScheduleView on the DRAINING arm": func(frame *frontendv1.FrontendFrame) bool {
			view := scheduleView(frame)
			if view.GetDraining() == nil {
				return false
			}
			draining = view.GetDraining()
			return true
		},
	})
	if !ack.GetOk() {
		t.Fatalf("scheduleShutdown nacked: %s", ack.GetError())
	}
	return draining
}

// storeTaskStartedEvent is the stream-plane TaskStarted the shim's converter
// forwards when the vendor starts a subagent (core.proto Event arm 14, folded
// by the SSM into live_task_count).
//
// WHY IT IS INJECTED RATHER THAN PROVOKED. The `--fake` engine has no
// subagents: its only detached-work path emits a task-notification, which is a
// different arm entirely and moves no task lifecycle (interrupt_e2e_test.go's
// header traces this). The event is therefore written to the real store exactly
// as the converter writes it, so everything downstream of the producer — store
// ingest, fan-out, the shim's forward, the SSM fold, the drain's hold list — is
// exercised for real.
func storeTaskStartedEvent(vendorSessionID, taskID string) *corev1.Event {
	return &corev1.Event{
		SessionId:    vendorSessionID,
		Plane:        corev1.Plane_PLANE_STREAM,
		Class:        corev1.EventClass_EVENT_CLASS_PERSISTENT,
		ProducedAtMs: time.Now().UnixMilli(),
		DedupKey:     "task-started:" + taskID,
		Payload: &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{
			TaskId:      taskID,
			Kind:        corev1.TaskKind_TASK_KIND_AGENT,
			Description: "e2e drain-lease background task",
		}},
	}
}

// awaitLiveTasks waits until the workspace's resolved state reports at least
// one live task — the SSM's own statement that the injected lifecycle landed,
// which is what makes it sound to schedule afterwards.
func awaitLiveTasks(t *testing.T, conn *websocket.Conn, workspace string) {
	t.Helper()
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a WorkspaceState reporting a live task": func(frame *frontendv1.FrontendFrame) bool {
			return workspaceStateFor(frame, workspace).GetLiveTaskCount() >= 1
		},
	})
}

// leaseFixture is the arrangement every "a prompt arrives under the lease" test
// needs: TWO workspaces, because a lease-held prompt must be distinguishable
// from an ordinarily queued one. Workspace A holds a turn — that is the drain's
// hold — and workspace B is live and IDLE, so a prompt submitted there is
// blocked by nothing except the lease.
type leaseFixture struct {
	boot     *shutdownBoot
	frontend *websocket.Conn
	cwdA     string
	connA    *websocket.Conn
	cwdB     string
	connB    *websocket.Conn
	draining *frontendv1.ShutdownScheduleDraining
}

func newLeaseFixture(t *testing.T, cause string) *leaseFixture {
	t.Helper()
	// The workspace tempdirs are created BEFORE the world on purpose: cleanups
	// run LIFO, so this ordering tears the daemon (and its shim processes) down
	// before the directories are removed. The other order races a still-exiting
	// shim against RemoveAll, which fails the test from cleanup.
	cwdA, cwdB := t.TempDir(), t.TempDir()
	world := newShutdownWorld(t)
	boot := world.boot(t)
	h := boot.harness()
	_, connA, _, _ := liveSession(t, h, cwdA)
	_, connB, _, _ := liveSession(t, h, cwdB)
	holdTurnOpen(t, connA, cwdA, "r-hold-a", "sleep e2e-drain-lease")

	frontend := boot.dialFrontend(t)
	draining := scheduleAndAwaitDraining(t, frontend, "r-sched", false, cause)
	if holdFor(draining, cwdA) == nil {
		t.Fatalf("the drain is not holding on workspace %s, whose turn is in flight; holds=%s", cwdA, describeHolds(draining))
	}
	return &leaseFixture{boot: boot, frontend: frontend, cwdA: cwdA, connA: connA, cwdB: cwdB, connB: connB, draining: draining}
}

// --- (a) an in-flight turn is a hold -----------------------------------------

// TestE2EScheduleWithALiveTurnBroadcastsATurnHold covers the SCHEDULE ITSELF:
// scheduling while a turn is in flight takes the lease immediately, broadcasts
// it on the draining arm, and names the in-flight turn as the thing the bounce
// is waiting on.
func TestE2EScheduleWithALiveTurnBroadcastsATurnHold(t *testing.T) {
	// Arrange — one workspace with a turn genuinely in flight.
	cwd := t.TempDir()
	world := newShutdownWorld(t)
	boot := world.boot(t)
	id, conn, _, _ := liveSession(t, boot.harness(), cwd)
	holdTurnOpen(t, conn, cwd, "r-hold", "sleep e2e-schedule")
	frontend := boot.dialFrontend(t)
	const cause = "merge of feature-x rebuilt the daemon"

	// Act
	draining := scheduleAndAwaitDraining(t, frontend, "r-sched", false, cause)

	// Assert — the lease's identity and the terms it was taken on.
	if draining.GetScheduleId() == "" {
		t.Error("the draining lease carries no schedule_id: a cancel could then never name it, and a held entry could never be joined to it")
	}
	if draining.GetScheduledAtMs() == 0 {
		t.Error("the draining lease carries scheduled_at_ms=0: the elapsed-time display has nothing to anchor on")
	}
	if got := draining.GetCause(); got != cause {
		t.Errorf("draining cause = %q, want %q: the cause is carried verbatim for display", got, cause)
	}
	if draining.GetStopShims() {
		t.Error("draining stop_shims = true, want false: it is fixed at schedule time from the ScheduleShutdownCmd, which asked for shims to be preserved")
	}

	// ... and the hold itself.
	hold := holdFor(draining, cwd)
	if hold == nil {
		t.Fatalf("the drain lists no hold for workspace %s, whose turn is in flight; holds=%s", cwd, describeHolds(draining))
	}
	if got := hold.GetSessionId(); got != id {
		t.Errorf("hold session_id = %q, want %q: a hold must name the OWNING session so it cannot be attributed to a successor", got, id)
	}
	if hold.GetTurn() == nil {
		t.Fatalf("the hold on %s carries no turn, but its turn is in flight; hold=%v", cwd, hold)
	}
	if hold.GetTurn().GetTurnId() == "" {
		t.Error("the turn hold carries an empty turn_id: the webapp, the logs and the turn ledger cannot name the same turn")
	}
}

// TestE2EAMidDrainConnectSnapshotCarriesTheLease covers the RESYNC half of the
// broadcast contract: a client that connects mid-drain sees the lease in its
// StateSnapshot without waiting for an edge, which is what lets a reloaded
// webapp render the banner at once.
func TestE2EAMidDrainConnectSnapshotCarriesTheLease(t *testing.T) {
	// Arrange
	cwd := t.TempDir()
	world := newShutdownWorld(t)
	boot := world.boot(t)
	_, conn, _, _ := liveSession(t, boot.harness(), cwd)
	holdTurnOpen(t, conn, cwd, "r-hold", "sleep e2e-snapshot")
	frontend := boot.dialFrontend(t)
	draining := scheduleAndAwaitDraining(t, frontend, "r-sched", false, "manual restart")

	// Act — a client joining after the lease was taken.
	snap := connectSnapshot(t, boot)

	// Assert
	view := snap.GetShutdownSchedule()
	if view == nil {
		t.Fatal("the connect StateSnapshot carried no shutdown_schedule at all: exactly one arm is always set, so an absent field is never a legal answer")
	}
	// Reported rather than fatal: the arm, the schedule identity and the hold
	// are independent facts about the SAME snapshot, and the getters below are
	// nil-safe, so all three are worth reporting from one run.
	if view.GetDraining() == nil {
		t.Errorf("the connect StateSnapshot's shutdown_schedule is %T, want the draining arm: a lease is held", view.GetState())
	}
	if got, want := view.GetDraining().GetScheduleId(), draining.GetScheduleId(); got != want {
		t.Errorf("snapshot schedule_id = %q, want the live schedule %q", got, want)
	}
	if holdFor(view.GetDraining(), cwd) == nil {
		t.Errorf("the snapshot's drain lists no hold for %s; holds=%s", cwd, describeHolds(view.GetDraining()))
	}
}

// --- (b) live tasks are a hold, and both holds can co-occur -------------------

// TestE2EScheduleWithLiveTasksBroadcastsATasksHold covers the OTHER hold: an
// idle workspace with live background tasks blocks the drain just as an
// in-flight turn does. Bouncing under live tasks is the same lost work.
func TestE2EScheduleWithLiveTasksBroadcastsATasksHold(t *testing.T) {
	// Arrange — a live, TURN-IDLE session with one live task.
	cwd := t.TempDir()
	world := newShutdownWorld(t)
	boot := world.boot(t)
	id, conn, vendorID, store := liveSession(t, boot.harness(), cwd)
	store.write(storeTaskStartedEvent(vendorID, "e2e-drain-task-1"))
	awaitLiveTasks(t, conn, cwd)
	frontend := boot.dialFrontend(t)

	// Act
	draining := scheduleAndAwaitDraining(t, frontend, "r-sched", false, "live tasks")

	// Assert
	hold := holdFor(draining, cwd)
	if hold == nil {
		t.Fatalf("the drain lists no hold for %s, which has a live task; holds=%s", cwd, describeHolds(draining))
	}
	if got := hold.GetSessionId(); got != id {
		t.Errorf("hold session_id = %q, want %q", got, id)
	}
	// The tasks arm and the ABSENCE of a turn arm are independent facts about
	// the same hold, so a missing tasks arm is reported rather than fatal. The
	// count is chained off it, since a count read from a missing arm would only
	// restate the same failure.
	if hold.GetTasks() == nil {
		t.Errorf("the hold on %s carries no tasks, but a task is live; hold=%v", cwd, hold)
	} else if got := hold.GetTasks().GetCount(); got != 1 {
		t.Errorf("tasks hold count = %d, want 1: the count is the display-grade summary of the live tasks", got)
	}
	if hold.GetTurn() != nil {
		t.Errorf("the hold carries a turn (%v) although no turn is in flight: turn and tasks are separate facts, not one state", hold.GetTurn())
	}
}

// TestE2EATurnAndLiveTasksCoOccurAsSiblingHolds covers the reason ShutdownHold
// uses SIBLING optional messages rather than a oneof: a session can hold an
// in-flight turn AND live background tasks at the same time, and the drain is
// waiting on both.
func TestE2EATurnAndLiveTasksCoOccurAsSiblingHolds(t *testing.T) {
	// Arrange — one workspace, both conditions true at once.
	cwd := t.TempDir()
	world := newShutdownWorld(t)
	boot := world.boot(t)
	_, conn, vendorID, store := liveSession(t, boot.harness(), cwd)
	holdTurnOpen(t, conn, cwd, "r-hold", "sleep e2e-both-holds")
	store.write(storeTaskStartedEvent(vendorID, "e2e-drain-task-both"))
	awaitLiveTasks(t, conn, cwd)
	frontend := boot.dialFrontend(t)

	// Act
	draining := scheduleAndAwaitDraining(t, frontend, "r-sched", false, "turn and tasks")

	// Assert
	hold := holdFor(draining, cwd)
	if hold == nil {
		t.Fatalf("the drain lists no hold for %s at all; holds=%s", cwd, describeHolds(draining))
	}
	if hold.GetTurn() == nil || hold.GetTasks() == nil {
		t.Fatalf("the hold on %s reports turn=%v tasks=%v, want BOTH set: a turn in flight and live tasks are co-occurring facts the drain waits on together",
			cwd, hold.GetTurn(), hold.GetTasks())
	}
}

// --- (c) a prompt arriving under the lease is held, unclassified --------------

// TestE2EAPromptUnderTheLeaseIsHeldWithTheScheduleIdAndStartsNoTurn covers THE
// LEASE DOING ITS JOB: with the lease held by another workspace's turn, a
// prompt submitted into an entirely IDLE workspace does not start a turn. It is
// held, and the entry says which schedule is holding it.
//
// The negative half — no turn started — is enforced over the same read loop as
// the positive one, so it is a rejection rather than a wait.
//
// CONTRACT AMBIGUITY (reported, not resolved here): the frozen comments say the
// classifier NEVER runs on a lease-held entry, but do not say which
// QueueClassification such an entry carries. This test therefore asserts what
// the comments DO state — that no classifier ever touched it — via the states
// only a classifier produces (PENDING is "the classifier is running";
// INTERJECT/ERROR are its verdicts) and the rationale it would have written. It
// deliberately does not pin a positive enum value.
func TestE2EAPromptUnderTheLeaseIsHeldWithTheScheduleIdAndStartsNoTurn(t *testing.T) {
	// Arrange
	fx := newLeaseFixture(t, "prompt-under-lease")
	const prompt = "!tool sleep e2e-held-under-lease"

	// Act
	writeCmd(t, fx.connB, fmt.Sprintf(`{"requestId":"r-b-prompt","submitPrompt":{"text":%q,"promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`, prompt))

	// Assert
	var held *frontendv1.QueueEntry
	reject := func(frame *frontendv1.FrontendFrame) string {
		if workspaceStateFor(frame, fx.cwdB).GetTurnActive() {
			return fmt.Sprintf("workspace %s reported turn_active while the drain lease was held: no NEW turn may start anywhere", fx.cwdB)
		}
		for _, item := range deltaItems(frame, fx.cwdB) {
			if strings.Contains(assistantText(item), echoOf(prompt)) {
				return fmt.Sprintf("the prompt held by the lease was answered (%q): it was delivered instead of held", prompt)
			}
		}
		return ""
	}
	awaitAll(t, fx.connB, reject, map[string]func(*frontendv1.FrontendFrame) bool{
		"a QueueEntry bearing shutdown_hold": func(frame *frontendv1.FrontendFrame) bool {
			for _, entry := range queueViewFor(frame, fx.cwdB).GetEntries() {
				if entry.GetShutdown() != nil {
					held = entry
					return true
				}
			}
			return false
		},
	})
	if got, want := held.GetShutdown().GetScheduleId(), fx.draining.GetScheduleId(); got != want {
		t.Errorf("the held entry names schedule_id %q, want the live schedule %q: the bubble must join to the lease it explains", got, want)
	}
	if arm := held.GetClassification(); held.GetPending() != nil || held.GetInterject() != nil || held.GetError() != nil {
		t.Errorf("the lease-held entry's classification is %T: that state is only reachable by running the classifier, which never runs on a lease-held entry", arm)
	}
	if got := held.GetHoldForTurnEnd().GetRationale(); got != "" {
		t.Errorf("the lease-held entry carries a classifier rationale %q: the classifier never runs on such an entry", got)
	}
}

// --- (d) explicit force is the only classifier-free way through ---------------

// TestE2EForcingAHeldEntryDeliversItAndBecomesANewDrainHold covers the EXPLICIT
// override: force is the user saying "run it anyway, and yes, that delays the
// bounce". The proof that the delay is real is that the turn the force started
// shows up as a NEW hold on the same drain.
func TestE2EForcingAHeldEntryDeliversItAndBecomesANewDrainHold(t *testing.T) {
	// Arrange — a lease-held prompt in the idle workspace.
	fx := newLeaseFixture(t, "force-through")
	writeCmd(t, fx.connB, `{"requestId":"r-b-prompt","submitPrompt":{"text":"!tool sleep e2e-forced-through","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)
	held := awaitHeldEntry(t, fx.connB, fx.cwdB, "a QueueEntry bearing shutdown_hold")

	// Act
	writeCmd(t, fx.connB, fmt.Sprintf(`{"requestId":"r-force","queueForce":{"entryId":%q}}`, held.GetId()))

	// Assert — it was delivered and a turn genuinely started.
	var ack *frontendv1.CommandAck
	awaitAll(t, fx.connB, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the CommandAck for the force": func(frame *frontendv1.FrontendFrame) bool {
			if a := ackFor(frame, "r-force"); a != nil {
				ack = a
				return true
			}
			return false
		},
		"workspace B reporting a turn in flight": func(frame *frontendv1.FrontendFrame) bool {
			return workspaceStateFor(frame, fx.cwdB).GetTurnActive()
		},
	})
	if !ack.GetOk() {
		t.Fatalf("queueForce on a lease-held entry nacked: %s", ack.GetError())
	}

	// ... and that turn is now something the SAME drain is waiting on.
	deadline := time.Now().Add(frameTimeout)
	for time.Now().Before(deadline) {
		draining := scheduleView(readFrame(t, fx.frontend)).GetDraining()
		if draining == nil {
			continue
		}
		if got, want := draining.GetScheduleId(), fx.draining.GetScheduleId(); got != want {
			t.Fatalf("a draining broadcast arrived for schedule %q, want the live schedule %q: forcing a prompt must not mint a new schedule", got, want)
		}
		if hold := holdFor(draining, fx.cwdB); hold.GetTurn() != nil {
			return
		}
	}
	t.Fatalf("the turn started by forcing a held prompt never appeared as a hold on the drain: forcing further delays the bounce, so its turn must hold the lease like any other")
}

// --- (e) cancel drops a held entry -------------------------------------------

// TestE2ECancelingAHeldEntryDropsItForever covers the other explicit exit: a
// cancelled entry is never delivered — not while the lease is held, and not
// when the lease is released either.
//
// Releasing the lease afterwards is what makes the negative sound. Without it
// "never delivered" would only mean "not delivered yet"; with it, the entry has
// had its one opportunity and provably did not take it, and the sentinel prompt
// that DOES run is the terminator.
func TestE2ECancelingAHeldEntryDropsItForever(t *testing.T) {
	// Arrange
	fx := newLeaseFixture(t, "cancel-held-entry")
	const dropped = "dropped-under-the-lease"
	writeCmd(t, fx.connB, fmt.Sprintf(`{"requestId":"r-b-prompt","submitPrompt":{"text":%q,"promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`, dropped))
	held := awaitHeldEntry(t, fx.connB, fx.cwdB, "a QueueEntry bearing shutdown_hold")

	// Act — drop it, then release the lease so ordinary delivery resumes.
	writeCmd(t, fx.connB, fmt.Sprintf(`{"requestId":"r-qcancel","queueCancel":{"entryId":%q}}`, held.GetId()))
	var ack *frontendv1.CommandAck
	awaitAll(t, fx.connB, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the CommandAck for the queue cancel": func(frame *frontendv1.FrontendFrame) bool {
			if a := ackFor(frame, "r-qcancel"); a != nil {
				ack = a
				return true
			}
			return false
		},
		"a QueueView for B with the entry gone": func(frame *frontendv1.FrontendFrame) bool {
			view := queueViewFor(frame, fx.cwdB)
			if view == nil {
				return false
			}
			for _, entry := range view.GetEntries() {
				if entry.GetId() == held.GetId() {
					return false
				}
			}
			return true
		},
	})
	if !ack.GetOk() {
		t.Fatalf("queueCancel on a lease-held entry nacked: %s", ack.GetError())
	}
	sendCancelSchedule(t, fx.frontend, "r-cancel-sched", fx.draining.GetScheduleId())
	awaitIdleSchedule(t, fx.frontend, "the idle broadcast releasing the lease")

	// Assert — the sentinel runs; the dropped prompt never does.
	const sentinel = "sentinel-after-the-drop"
	writeCmd(t, fx.connB, fmt.Sprintf(`{"requestId":"r-b-sentinel","submitPrompt":{"text":%q,"promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`, sentinel))
	reject := func(frame *frontendv1.FrontendFrame) string {
		for _, item := range deltaItems(frame, fx.cwdB) {
			if strings.Contains(assistantText(item), echoOf(dropped)) {
				return fmt.Sprintf("the cancelled entry %q was delivered: a dropped entry is never delivered", dropped)
			}
		}
		return ""
	}
	awaitAll(t, fx.connB, reject, map[string]func(*frontendv1.FrontendFrame) bool{
		"the sentinel prompt's own reply": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, fx.cwdB) {
				if strings.Contains(assistantText(item), echoOf(sentinel)) {
					return true
				}
			}
			return false
		},
	})
}

// --- (f) the last hold clearing executes the bounce ---------------------------

// TestE2ETheLastHoldClearingExecutesTheShutdown covers THE TERMINAL STEP: the
// moment the drain has nothing left to wait on, the daemon executes the
// shutdown — with the stop_shims it was given at SCHEDULE time, because that
// flag is a property of WHAT was rebuilt and not of when the drain happened to
// finish.
//
// The two cases are the two real callers: an ordinary bounce PRESERVES shims
// (a survivor redials and is reattached for free), while a deploy that changed
// the shim bundle stops them so no survivor keeps running the old build.
func TestE2ETheLastHoldClearingExecutesTheShutdown(t *testing.T) {
	tests := []struct {
		name      string
		stopShims bool
	}{
		{name: "preserving shims", stopShims: false},
		{name: "stopping shims", stopShims: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange — one hold, and it is the only one.
			cwd := t.TempDir()
			world := newShutdownWorld(t)
			boot := world.boot(t)
			_, conn, _, _ := liveSession(t, boot.harness(), cwd)
			holdTurnOpen(t, conn, cwd, "r-hold", "sleep e2e-execute")
			frontend := boot.dialFrontend(t)
			draining := scheduleAndAwaitDraining(t, frontend, "r-sched", tc.stopShims, "deploy")
			if got := draining.GetStopShims(); got != tc.stopShims {
				t.Fatalf("draining stop_shims = %v, want %v: it is fixed at schedule time", got, tc.stopShims)
			}
			shimProc := boot.shimFor(t, cwd)

			// Act — clear the only hold by stopping the turn it is waiting on.
			writeCmd(t, conn, `{"requestId":"r-interrupt","interrupt":{}}`)

			// Assert — the bounce ran, on the terms it was scheduled with.
			if got := awaitExecutedShutdown(t, boot); got != tc.stopShims {
				t.Errorf("the executed shutdown used stop_shims=%v, want %v: the flag is fixed at schedule time, not re-decided at drain end", got, tc.stopShims)
			}
			// ... and the shims were treated accordingly. The executed hook
			// publishes only AFTER server.ShutdownAll returns, so both checks
			// below read a settled world rather than racing it.
			if tc.stopShims {
				select {
				case <-shimProc.exited:
				case <-time.After(frameTimeout):
					t.Errorf("the shim for %s is still running after a stop_shims shutdown: a deploy that changed the bundle must not leave a survivor on the old build", cwd)
				}
				return
			}
			select {
			case <-shimProc.exited:
				t.Errorf("the shim for %s exited on a shutdown that asked for shims to be PRESERVED: a survivor is what makes the next boot's reattach free", cwd)
			default:
			}
		})
	}
}

// --- (g) cancel releases the lease and everything it held ---------------------

// TestE2ECancelingTheScheduleReleasesHeldEntriesIntoOrdinaryDelivery covers the
// CANCEL: the lease goes away as a real idle broadcast (not an absent field),
// and every entry it was holding sheds its shutdown_hold and re-enters ordinary
// delivery rather than being stranded.
func TestE2ECancelingTheScheduleReleasesHeldEntriesIntoOrdinaryDelivery(t *testing.T) {
	// Arrange
	fx := newLeaseFixture(t, "cancel-the-schedule")
	const prompt = "held-then-released"
	writeCmd(t, fx.connB, fmt.Sprintf(`{"requestId":"r-b-prompt","submitPrompt":{"text":%q,"promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`, prompt))
	held := awaitHeldEntry(t, fx.connB, fx.cwdB, "a QueueEntry bearing shutdown_hold")

	// Act
	sendCancelSchedule(t, fx.frontend, "r-cancel", fx.draining.GetScheduleId())

	// Assert — the lease is representably gone.
	var ack *frontendv1.CommandAck
	awaitAll(t, fx.frontend, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the CommandAck for the cancel": func(frame *frontendv1.FrontendFrame) bool {
			if a := ackFor(frame, "r-cancel"); a != nil {
				ack = a
				return true
			}
			return false
		},
		"a ShutdownScheduleView on the IDLE arm": func(frame *frontendv1.FrontendFrame) bool {
			return scheduleView(frame).GetIdle() != nil
		},
	})
	if !ack.GetOk() {
		t.Fatalf("cancelScheduledShutdown with the live schedule_id nacked: %s", ack.GetError())
	}

	// ... and what it held is an ordinary prompt again: no shutdown_hold, and
	// delivered.
	awaitAll(t, fx.connB, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the released entry shedding its shutdown_hold": func(frame *frontendv1.FrontendFrame) bool {
			view := queueViewFor(frame, fx.cwdB)
			if view == nil {
				return false
			}
			for _, entry := range view.GetEntries() {
				if entry.GetId() == held.GetId() {
					return entry.GetShutdown() == nil
				}
			}
			// Gone from the queue entirely means it was delivered, which is
			// the same statement made more strongly.
			return true
		},
		"the released prompt's own reply": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, fx.cwdB) {
				if strings.Contains(assistantText(item), echoOf(prompt)) {
					return true
				}
			}
			return false
		},
	})
}

// --- (h) both mistakes are loud ----------------------------------------------

// TestE2EASecondScheduleIsALoudNack covers DOUBLE-SCHEDULE: two deploy flows
// must not be able to silently merge their intents, so a schedule taken while
// one is already live is refused rather than replacing it. The refusal is loud
// AND inert: the existing lease is untouched.
func TestE2EASecondScheduleIsALoudNack(t *testing.T) {
	// Arrange — a live lease.
	cwd := t.TempDir()
	world := newShutdownWorld(t)
	boot := world.boot(t)
	_, conn, _, _ := liveSession(t, boot.harness(), cwd)
	holdTurnOpen(t, conn, cwd, "r-hold", "sleep e2e-double")
	frontend := boot.dialFrontend(t)
	first := scheduleAndAwaitDraining(t, frontend, "r-sched-1", false, "the first deploy")

	// Act — a second flow schedules on top.
	sendSchedule(t, frontend, "r-sched-2", true, "the second deploy")

	// Assert
	var ack *frontendv1.CommandAck
	reject := func(frame *frontendv1.FrontendFrame) string {
		draining := scheduleView(frame).GetDraining()
		if draining != nil && draining.GetScheduleId() != first.GetScheduleId() {
			return fmt.Sprintf("a NEW schedule %q was broadcast while %q was live: a second schedule must be refused, never silently replace the first",
				draining.GetScheduleId(), first.GetScheduleId())
		}
		if scheduleView(frame).GetIdle() != nil {
			return "the lease went idle on a rejected second schedule: a refusal must leave the live lease untouched"
		}
		return ""
	}
	awaitAll(t, frontend, reject, map[string]func(*frontendv1.FrontendFrame) bool{
		"the CommandAck for the second schedule": func(frame *frontendv1.FrontendFrame) bool {
			if a := ackFor(frame, "r-sched-2"); a != nil {
				ack = a
				return true
			}
			return false
		},
	})
	if ack.GetOk() {
		t.Fatal("the second scheduleShutdown was acked ok: scheduling while a schedule already exists is a loud nack, never a silent replace")
	}
	if ack.GetError() == "" {
		t.Error("the rejected second schedule carries an empty error: a nack the user cannot read is a silent failure wearing a nack's clothes")
	}
}

// TestE2EACancelWithAStaleScheduleIdIsALoudNack covers the STALE CANCEL: a
// cancel aimed at a schedule that is no longer live must never kill the one
// that is. It is refused loudly and the live lease survives untouched.
func TestE2EACancelWithAStaleScheduleIdIsALoudNack(t *testing.T) {
	// Arrange
	cwd := t.TempDir()
	world := newShutdownWorld(t)
	boot := world.boot(t)
	_, conn, _, _ := liveSession(t, boot.harness(), cwd)
	holdTurnOpen(t, conn, cwd, "r-hold", "sleep e2e-stale-cancel")
	frontend := boot.dialFrontend(t)
	live := scheduleAndAwaitDraining(t, frontend, "r-sched", false, "the live schedule")

	// Act — a cancel from an older flow, naming a schedule that is not this one.
	sendCancelSchedule(t, frontend, "r-cancel-stale", "shutdown-schedule-that-is-not-live")

	// Assert
	var ack *frontendv1.CommandAck
	reject := func(frame *frontendv1.FrontendFrame) string {
		if scheduleView(frame).GetIdle() != nil {
			return "the lease went idle on a STALE cancel: a cancel aimed at an old schedule can never kill a newer one"
		}
		return ""
	}
	awaitAll(t, frontend, reject, map[string]func(*frontendv1.FrontendFrame) bool{
		"the CommandAck for the stale cancel": func(frame *frontendv1.FrontendFrame) bool {
			if a := ackFor(frame, "r-cancel-stale"); a != nil {
				ack = a
				return true
			}
			return false
		},
	})
	// Reported rather than fatal: whether the stale cancel was refused and
	// whether the live lease survived it are independent, and the survival check
	// is the more important of the two. The error-text check is chained, since a
	// nack's text only exists once there is a nack.
	if ack.GetOk() {
		t.Error("a cancelScheduledShutdown naming a schedule that is not live was acked ok: a stale id is a loud nack")
	} else if ack.GetError() == "" {
		t.Error("the rejected stale cancel carries an empty error")
	}

	// ... and the live lease is still exactly what it was.
	snap := connectSnapshot(t, boot)
	if got := snap.GetShutdownSchedule().GetDraining().GetScheduleId(); got != live.GetScheduleId() {
		t.Errorf("after a stale cancel the live schedule_id is %q, want %q: the refusal must leave the lease untouched", got, live.GetScheduleId())
	}
}
