// THE PARKED-PROMPT LEDGER ACROSS BOUNCES: what a successor daemon may and may
// not resurrect, and what a client may do to a prompt whose session has not
// wired to it.
//
// WHAT THIS FILE IS FOR. shutdownscheduledurability_e2e_test.go pins the
// PROMISE half of the drain lease: a parked prompt survives the bounce and is
// delivered on the other side. This file pins the three statements that promise
// is only safe with — every one of which is a claim about a DURABLE ROW
// outliving the memory it was mirrored in:
//
//  1. A prompt the user CANCELLED is gone for good. The cancel drops it from the
//     view at once, so nothing the user can see distinguishes "dropped from
//     memory" from "dropped from the ledger" — but the next boot reads the
//     LEDGER, and a row that outlived its cancel comes back as a prompt the user
//     already took back, delivered without their knowledge.
//
//  2. A prompt the user FORCED is delivered exactly once. Its row was created to
//     survive a bounce; once the force has delivered it, that same durability is
//     what would let a successor deliver it a second time.
//
//  3. A materialized entry — one whose session has not wired to this daemon — is
//     still a real entry: accept is a view-state control that needs no shim and
//     is honored, and force is the one control that genuinely cannot be served,
//     so its refusal is CLASSIFIED rather than falling through the classifier's
//     unclassified door.
//
// HOW UNWIREDNESS IS ARRANGED, and why it is arranged rather than waited for. A
// materialized entry exists only while no controller is live for its workspace,
// and "the boot sweeper has not claimed the shim yet" is a race, not a state. So
// the tests that need an unwired successor SCHEDULE THE DRAIN WITH stop_shims=true
// and let it execute: the shims are then gone by construction, nothing redials
// the successor, and the ledger materialization is the only thing standing
// behind the workspace's queue. That is also the production shape — a deploy
// that replaced the shim bundle is exactly the bounce that stops shims.
//
// Reuses shutdownscheduleharness_test.go's world/boot/frame accessors,
// shutdownschedule_e2e_test.go's scheduleAndAwaitDraining,
// shutdownscheduledurability_e2e_test.go's snapshot accessors, and
// e2e_test.go / interrupt_e2e_test.go / clearcompact_e2e_test.go's session
// helpers, all READ-ONLY.
package e2e

import (
	"fmt"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"github.com/gorilla/websocket"

	"claude-repld/internal/errclass"
)

// --- local accessors ---------------------------------------------------------

// entryInQueue returns the entry a QueueView carries under id, or nil. Both
// getters are nil-safe, so a nil view reads as "no such entry" rather than
// panicking a test that is asserting exactly that absence.
func entryInQueue(view *frontendv1.QueueView, id string) *frontendv1.QueueEntry {
	for _, entry := range view.GetEntries() {
		if entry.GetId() == id {
			return entry
		}
	}
	return nil
}

// describeQueue renders a QueueView for a failure message, so a test that found
// an entry it wanted gone says what the successor actually materialized.
func describeQueue(view *frontendv1.QueueView) string {
	if view == nil {
		return "(no QueueView for this workspace at all)"
	}
	if len(view.GetEntries()) == 0 {
		return "(an empty queue)"
	}
	parts := make([]string, 0, len(view.GetEntries()))
	for _, entry := range view.GetEntries() {
		parts = append(parts, fmt.Sprintf("{id=%s text=%q hold=%v accepted=%v}",
			entry.GetId(), entry.GetText(), entry.GetShutdownHold() != nil, entry.GetAccepted()))
	}
	return strings.Join(parts, " ")
}

// --- shared arrangements ------------------------------------------------------

// parkedUnderLease is the arrangement every test here starts from: workspace A
// holds a turn (the drain's hold), workspace B is idle, and a prompt submitted
// into B is parked by the lease.
//
// It is deliberately the SAME two-workspace shape newLeaseFixture uses, and for
// the same reason: a lease-held prompt must be distinguishable from an
// ordinarily queued one, which takes a workspace that is blocked by nothing
// except the lease.
type parkedUnderLease struct {
	world *shutdownWorld
	boot  *shutdownBoot
	cwdA  string
	connA *websocket.Conn
	cwdB  string
	connB *websocket.Conn
	held  *frontendv1.QueueEntry
}

// newParkedUnderLease schedules a drain on the given terms, parks `prompt` in
// the idle workspace, and returns once the parking is a fact on the wire.
func newParkedUnderLease(t *testing.T, stopShims bool, cause, prompt string) *parkedUnderLease {
	t.Helper()
	// The workspace tempdirs are created BEFORE the world: cleanups run LIFO, so
	// this tears the daemons and their shims down before the directories go.
	cwdA, cwdB := t.TempDir(), t.TempDir()
	world := newShutdownWorld(t)
	boot := world.boot(t)
	h := boot.harness()
	_, connA, _, _ := liveSession(t, h, cwdA)
	_, connB, _, _ := liveSession(t, h, cwdB)
	holdTurnOpen(t, connA, cwdA, "r-hold-a", "sleep e2e-parked-ledger")

	frontend := boot.dialFrontend(t)
	draining := scheduleAndAwaitDraining(t, frontend, "r-sched", stopShims, cause)
	if holdFor(draining, cwdA) == nil {
		t.Fatalf("the drain is not holding on workspace %s, whose turn is in flight; holds=%s", cwdA, describeHolds(draining))
	}
	writeCmd(t, connB, fmt.Sprintf(`{"requestId":"r-b-prompt","submitPrompt":{"text":%q,"promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`, prompt))
	held := awaitHeldEntry(t, connB, cwdB, "a QueueEntry bearing shutdown_hold")
	return &parkedUnderLease{
		world: world, boot: boot,
		cwdA: cwdA, connA: connA, cwdB: cwdB, connB: connB,
		held: held,
	}
}

// executeDrain clears the drain's last hold and returns once the daemon has
// EXECUTED the shutdown, so a caller may bounce into a successor knowing the
// teardown already happened rather than racing it.
func (p *parkedUnderLease) executeDrain(t *testing.T) {
	t.Helper()
	writeCmd(t, p.connA, `{"requestId":"r-interrupt","interrupt":{}}`)
	awaitExecutedShutdown(t, p.boot)
}

// materializedEntry is a successor daemon holding a parked prompt whose session
// is UNWIRED by construction: the drain that preceded it stopped every shim, so
// nothing is redialling this boot and the boot ledger is the only thing behind
// workspace B's queue.
type materializedEntry struct {
	conn    *websocket.Conn
	cwd     string
	session string
	entry   *frontendv1.QueueEntry
}

// newMaterializedEntry parks a prompt, lets a stop_shims drain execute, bounces,
// and returns the successor's materialized view of that prompt plus a scoped
// connection to send queue commands over.
func newMaterializedEntry(t *testing.T, cause, prompt string) *materializedEntry {
	t.Helper()
	parked := newParkedUnderLease(t, true /*stopShims*/, cause, prompt)
	parked.executeDrain(t)
	parked.boot.bounce()

	successor := parked.world.boot(t)
	snap := connectSnapshot(t, successor)
	session := snapshotSessionID(t, snap, parked.cwdB)
	queue := snapshotQueue(snap, parked.cwdB)
	entry := entryInQueue(queue, parked.held.GetId())
	// Fatal, not reported: every test built on this fixture asserts something
	// ABOUT the materialized entry, so its absence is a broken arrangement
	// rather than one more independent fact worth collecting.
	if entry == nil {
		t.Fatalf("the successor materialized no entry %q for workspace %s, so there is no unwired parked prompt to exercise; queue=%s",
			parked.held.GetId(), parked.cwdB, describeQueue(queue))
	}
	conn := successor.harness().dial(t, session)
	if first := readFrame(t, conn); first.GetSnapshot() == nil {
		t.Fatalf("first scoped frame = %T, want a StateSnapshot", first.GetFrame())
	}
	return &materializedEntry{conn: conn, cwd: parked.cwdB, session: session, entry: entry}
}

// --- (1) a cancelled prompt never resurrects ---------------------------------

// TestE2EACancelledParkedPromptNeverResurrectsOnALaterBoot covers the failure a
// cancel's ORDERING decides: the user took their prompt back, and a daemon that
// died between "gone from memory" and "gone from the ledger" must not hand the
// prompt to the next daemon to deliver behind their back.
//
// THREE BOOTS, AND EACH ONE IS LOAD-BEARING. The first parks the prompt and dies
// mid-drain, so the cancel lands on a prompt that reached its daemon through the
// DURABLE ROW rather than one the daemon had held in memory all along — from the
// boot ledger, or from the entries a wiring session adopted out of it, which are
// the same entry under the same id either way. The second serves the cancel and
// then dies too, which is the crash the drop has to have preceded. The third is
// the only place the question can be asked at all: it reads the ledger, and what
// it materializes is the ledger's own answer to "was this prompt cancelled".
//
// The delivery half is made sound the same way TestE2ECancelingAHeldEntryDropsItForever
// makes it sound: the lease is released and a sentinel prompt is run, so the
// cancelled prompt has had its one full opportunity and provably did not take it.
func TestE2EACancelledParkedPromptNeverResurrectsOnALaterBoot(t *testing.T) {
	// Arrange — a prompt parked under a lease, and a daemon that dies mid-drain.
	const cancelled = "cancelled-before-the-second-bounce"
	parked := newParkedUnderLease(t, true /*stopShims*/, "deploy that replaced the shim bundle", cancelled)
	entryID := parked.held.GetId()
	parked.boot.bounce()

	second := parked.world.boot(t)
	secondSnap := connectSnapshot(t, second)
	sessionB := snapshotSessionID(t, secondSnap, parked.cwdB)
	if entryInQueue(snapshotQueue(secondSnap, parked.cwdB), entryID) == nil {
		t.Fatalf("the first successor materialized no entry %q for %s, so there is nothing for this test to cancel; queue=%s",
			entryID, parked.cwdB, describeQueue(snapshotQueue(secondSnap, parked.cwdB)))
	}

	// Act — cancel it on the successor, then bounce again.
	secondConn := second.harness().dial(t, sessionB)
	if first := readFrame(t, secondConn); first.GetSnapshot() == nil {
		t.Fatalf("first scoped frame = %T, want a StateSnapshot", first.GetFrame())
	}
	writeCmd(t, secondConn, fmt.Sprintf(`{"requestId":"r-qcancel","queueCancel":{"entryId":%q}}`, entryID))
	var cancelAck *frontendv1.CommandAck
	awaitAll(t, secondConn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the CommandAck for the queue cancel": func(frame *frontendv1.FrontendFrame) bool {
			if a := ackFor(frame, "r-qcancel"); a != nil {
				cancelAck = a
				return true
			}
			return false
		},
	})
	if !cancelAck.GetOk() {
		t.Fatalf("queueCancel on the materialized entry nacked: %s — the rest of this test asks what a SUCCESSFUL cancel durably means", cancelAck.GetError())
	}
	second.bounce()
	third := parked.world.boot(t)

	// Assert — the ledger no longer knows about it.
	thirdSnap := connectSnapshot(t, third)
	thirdQueue := snapshotQueue(thirdSnap, parked.cwdB)
	// Reported rather than fatal: whether the row survived the cancel and
	// whether the prompt is ever DELIVERED are independent facts, and the
	// delivery half below is the one that names the user-visible harm.
	if entryInQueue(thirdQueue, entryID) != nil {
		t.Errorf("the third boot materialized the cancelled entry %q again: the cancel dropped it from memory but its durable row outlived the crash, so the ledger resurrected a prompt the user had already taken back; queue=%s",
			entryID, describeQueue(thirdQueue))
	}

	// ... and it is never delivered, even once the lease is released and the
	// session is back.
	//
	// THE SUBSCRIBER IS DIALLED BEFORE THE SWEEP IS DRIVEN. Wiring B is what the
	// re-check pass does, and a subscriber dialled after that pass can miss the
	// delivery it triggered — so subscribe, then drive the event, then wait.
	scheduleID := thirdSnap.GetShutdownSchedule().GetDraining().GetScheduleId()
	if scheduleID == "" {
		t.Fatalf("the third boot reports shutdown_schedule %T with no live drain, but workspace %s still holds an unfinished turn: the sentinel below can only prove anything while ordinary delivery is reachable",
			thirdSnap.GetShutdownSchedule().GetState(), parked.cwdA)
	}
	thirdFrontend := third.dialFrontend(t)
	sendCancelSchedule(t, thirdFrontend, "r-release", scheduleID)
	awaitIdleSchedule(t, thirdFrontend, "the idle broadcast releasing the lease")

	thirdConn := third.harness().dial(t, sessionB)
	if first := readFrame(t, thirdConn); first.GetSnapshot() == nil {
		t.Fatalf("first scoped frame = %T, want a StateSnapshot", first.GetFrame())
	}
	third.sweepRecheckWhenParked(t, sessionB)
	const sentinel = "sentinel-after-the-resurrection-window"
	writeCmd(t, thirdConn, fmt.Sprintf(`{"requestId":"r-b-sentinel","submitPrompt":{"text":%q,"promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`, sentinel))
	reject := func(frame *frontendv1.FrontendFrame) string {
		for _, item := range deltaItems(frame, parked.cwdB) {
			if strings.Contains(assistantText(item), echoOf(cancelled)) {
				return fmt.Sprintf("the cancelled prompt %q was answered by the third daemon: a prompt the user took back is dropped forever, not held in the ledger until a boot that has forgotten the cancel", cancelled)
			}
		}
		return ""
	}
	awaitAll(t, thirdConn, reject, map[string]func(*frontendv1.FrontendFrame) bool{
		"the sentinel prompt's own reply": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, parked.cwdB) {
				if strings.Contains(assistantText(item), echoOf(sentinel)) {
					return true
				}
			}
			return false
		},
	})
}

// --- (2) a forced prompt is never redelivered --------------------------------

// TestE2EAForcedParkedPromptIsNotMaterializedByTheSuccessor covers the mirror
// image of the cancel: force is the OTHER explicit exit, and it delivers. Its
// durable row exists so a bounce cannot lose the prompt; once the force has run
// it, that same row is what would let the next daemon run it a second time.
//
// MATERIALIZATION IS THE WHOLE QUESTION, because it is the only route a
// successor has to a prompt from a previous daemon: the boot ledger is what puts
// an entry back in a workspace's queue, and delivery-after-the-bounce is the
// wiring session collecting exactly those entries. A successor that materializes
// nothing for a forced prompt cannot redeliver it; one that materializes it will,
// the moment the session comes back.
func TestE2EAForcedParkedPromptIsNotMaterializedByTheSuccessor(t *testing.T) {
	// Arrange — a parked prompt, forced through, and its turn run to completion.
	const forced = "forced-before-the-bounce"
	parked := newParkedUnderLease(t, true /*stopShims*/, "deploy that replaced the shim bundle", forced)
	entryID := parked.held.GetId()
	writeCmd(t, parked.connB, fmt.Sprintf(`{"requestId":"r-force","queueForce":{"entryId":%q}}`, entryID))
	var forceAck *frontendv1.CommandAck
	awaitAll(t, parked.connB, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the CommandAck for the force": func(frame *frontendv1.FrontendFrame) bool {
			if a := ackFor(frame, "r-force"); a != nil {
				forceAck = a
				return true
			}
			return false
		},
		// The reply is the proof the force DELIVERED, which is what makes a
		// second delivery a duplicate rather than the first one.
		"the forced prompt's own reply": func(frame *frontendv1.FrontendFrame) bool {
			for _, item := range deltaItems(frame, parked.cwdB) {
				if strings.Contains(assistantText(item), echoOf(forced)) {
					return true
				}
			}
			return false
		},
	})
	if !forceAck.GetOk() {
		t.Fatalf("queueForce on the lease-held entry nacked: %s — this test asks what a DELIVERED force durably means", forceAck.GetError())
	}

	// Act — the drain the force delayed now executes, and a successor comes up.
	parked.executeDrain(t)
	parked.boot.bounce()
	successor := parked.world.boot(t)

	// Assert
	snap := connectSnapshot(t, successor)
	queue := snapshotQueue(snap, parked.cwdB)
	if got := entryInQueue(queue, entryID); got != nil {
		t.Errorf("the successor materialized the already-delivered entry %q (text %q): forcing a parked prompt delivered it, so its durable row must be gone before the force returns — a surviving row is a second delivery waiting for the session to wire; queue=%s",
			entryID, got.GetText(), describeQueue(queue))
	}
}

// --- (3) accept is honored on a materialized entry ---------------------------

// TestE2EAcceptOnAMaterializedEntryIsHonored covers the control that CAN be
// served without a shim. Accept is view state — the user saying "I have seen
// this hold" — so refusing it because the session has not wired makes the daemon
// unable to record a fact that has nothing to do with a shim, on exactly the
// entries whose whole point is that they outlived one.
func TestE2EAcceptOnAMaterializedEntryIsHonored(t *testing.T) {
	// Arrange
	fx := newMaterializedEntry(t, "deploy that replaced the shim bundle", "accepted-while-unwired")
	if fx.entry.GetAccepted() {
		t.Fatalf("the materialized entry %q arrived already accepted, so this test could not tell an honored accept from the arrangement", fx.entry.GetId())
	}

	// Act
	writeCmd(t, fx.conn, fmt.Sprintf(`{"requestId":"r-accept","queueAccept":{"entryId":%q}}`, fx.entry.GetId()))

	// Assert — the ack and the view are independent facts about the same accept:
	// an ok ack that changed no view is a control that acknowledges and achieves
	// nothing, which is exactly the failure the parked ledger exists to end.
	var (
		ack      *frontendv1.CommandAck
		accepted *frontendv1.QueueEntry
	)
	awaitAll(t, fx.conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the CommandAck for the accept": func(frame *frontendv1.FrontendFrame) bool {
			if a := ackFor(frame, "r-accept"); a != nil {
				ack = a
				return true
			}
			return false
		},
		"a QueueView for the workspace carrying the entry as accepted": func(frame *frontendv1.FrontendFrame) bool {
			entry := entryInQueue(queueViewFor(frame, fx.cwd), fx.entry.GetId())
			if entry == nil || !entry.GetAccepted() {
				return false
			}
			accepted = entry
			return true
		},
	})
	if !ack.GetOk() {
		t.Errorf("queueAccept on the materialized entry %q nacked: %s — accept records that the user saw the hold and needs no shim to do it, so an unwired session is no reason to refuse it",
			fx.entry.GetId(), ack.GetError())
	}
	if accepted == nil {
		t.Errorf("no QueueView ever showed entry %q as accepted on workspace %s", fx.entry.GetId(), fx.cwd)
	}
}

// --- (4) force on an unwired session is a CLASSIFIED refusal ------------------

// TestE2EForceOnAnUnwiredMaterializedEntryIsATypedNack covers the one control
// that genuinely cannot be served: a force is a delivery, a delivery needs a
// shim, and this daemon has none for that session. The refusal itself is
// correct — what this pins is that it is CLASSIFIED.
//
// WHY THE CLASS IS THE CONTRACT AND NOT A DETAIL. CommandAck.failure is the
// classified account both frontends render as a failure card; internal.unclassified
// is the classifier's loud fallthrough for text nobody taught it. A refusal that
// the daemon deliberately produces, whose cause it knows exactly, arriving as
// unclassified means the webapp shows the user a generic internal error for a
// perfectly ordinary "your session is not up yet" — and it means the failure
// vocabulary silently stopped covering a case it is supposed to name.
//
// The test asserts the class is a NAMED one rather than pinning which name,
// because the vocabulary's exact member is the daemon's to choose; what it may
// not be is the fallthrough.
func TestE2EForceOnAnUnwiredMaterializedEntryIsATypedNack(t *testing.T) {
	// Arrange
	fx := newMaterializedEntry(t, "deploy that replaced the shim bundle", "forced-while-unwired")

	// Act
	writeCmd(t, fx.conn, fmt.Sprintf(`{"requestId":"r-force","queueForce":{"entryId":%q}}`, fx.entry.GetId()))

	// Assert
	var ack *frontendv1.CommandAck
	awaitAll(t, fx.conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the CommandAck for the force": func(frame *frontendv1.FrontendFrame) bool {
			if a := ackFor(frame, "r-force"); a != nil {
				ack = a
				return true
			}
			return false
		},
	})
	// Reported rather than fatal throughout: the refusal, its classification and
	// its readability are independent facts about the same ack, and the getters
	// are nil-safe, so all of them are worth reporting from one run.
	if ack.GetOk() {
		t.Errorf("queueForce on the unwired entry %q was acked ok: a force is a delivery and this daemon has no shim for session %s to deliver to, so acking it claims work that did not happen",
			fx.entry.GetId(), fx.session)
	}
	if ack.GetFailure() == nil {
		t.Errorf("the refused force carries no classified failure at all (error=%q): CommandAck.failure is what both frontends render, so a refusal without one is a failure the webapp shows as nothing",
			ack.GetError())
	}
	if got := ack.GetFailure().GetErrorType(); got == string(errclass.TypeInternalUnclassified) {
		t.Errorf("the refused force is classified %s: this refusal is one the daemon deliberately produces and knows the cause of, so falling through to the unclassified door renders an ordinary 'the session is not up yet' as an internal error",
			got)
	} else if got == "" {
		t.Errorf("the refused force carries an empty error_type: an unnamed failure class cannot be rendered, logged or acted on differently from any other")
	}
}
