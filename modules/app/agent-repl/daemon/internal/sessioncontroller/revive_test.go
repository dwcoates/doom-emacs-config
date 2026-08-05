package sessioncontroller

import (
	"context"
	"errors"
	"runtime"
	"sync"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/registry"
)

// reviveRig is a hibernated, brought-up session ready for a revival.
func reviveRig(t *testing.T, cause string) (*Manager, *fakeApplier, *fakeHibernations) {
	t.Helper()
	m, applier, hib := newHibernationRig(t)
	hib.setAsleep("s1", registry.HibernationDetail{Cause: cause, SinceMs: 42})
	return m, applier, hib
}

// A mode is REQUIRED. The wire oneof makes "no decision" unrepresentable
// precisely so the daemon never invents a default, and inventing one here
// would spend the user's context budget on a choice they were being asked for.
func TestReviveSessionRefusesAnUnsetMode(t *testing.T) {
	// Arrange.
	m, _, _ := reviveRig(t, registry.HibernationCauseIdleCutoff)

	// Act.
	err := m.ReviveSession(context.Background(), "ws", ReviveModeUnset)

	// Assert.
	if err == nil {
		t.Fatal("ReviveSession with no mode = nil, want a refusal; the daemon has no default for the user's revival choice")
	}
}

// DIRECT clears the sleep, so the gate is gone and prompts flow.
func TestReviveSessionDirectRetiresTheGate(t *testing.T) {
	// Arrange.
	m, _, hib := reviveRig(t, registry.HibernationCauseCacheExpired)

	// Act.
	if err := m.ReviveSession(context.Background(), "ws", ReviveModeDirect); err != nil {
		t.Fatalf("ReviveSession direct: %v", err)
	}

	// Assert.
	if detail, asleep := hib.HibernationOf("s1"); asleep && detail.Cause != "" {
		t.Fatalf("hibernation detail = %+v after a direct revival, want the sleep retired", detail)
	}
}

func TestReviveForMergeClearsTheGateAndRefreshesTheIdleClock(t *testing.T) {
	// A merge command is an explicit direct-revival policy, unlike a user
	// revival command that must state a direct or compact-first choice.
	m, _, hib := reviveRig(t, registry.HibernationCauseIdleCutoff)
	if err := m.ReviveForMerge(context.Background(), "ws"); err != nil {
		t.Fatalf("ReviveForMerge: %v", err)
	}
	if detail, asleep := hib.HibernationOf("s1"); asleep && detail.Cause != "" {
		t.Fatalf("hibernation detail after merge revival = %+v, want cleared", detail)
	}
	if got := hib.lastTurnEnd("s1"); got <= 42 {
		t.Fatalf("last turn end after merge revival = %d, want a refreshed timestamp", got)
	}
}

func TestReviveForMergeRefusesAnInFlightUserRevival(t *testing.T) {
	m, _, _ := reviveRig(t, registry.HibernationCauseIdleCutoff)
	release, _, err := m.claimRevival("ws", "s1")
	if err != nil {
		t.Fatalf("claim user revival: %v", err)
	}
	defer release()
	err = m.ReviveForMerge(context.Background(), "ws")
	if !errors.Is(err, ErrRevivalInFlight) {
		t.Fatalf("ReviveForMerge during a user revival = %v, want ErrRevivalInFlight", err)
	}
}

// After a direct revival an ordinary prompt is no longer gated.
func TestReviveSessionDirectAdmitsPromptsAfterwards(t *testing.T) {
	// Arrange.
	m, _, _ := reviveRig(t, registry.HibernationCauseForced)
	if err := m.ReviveSession(context.Background(), "ws", ReviveModeDirect); err != nil {
		t.Fatalf("ReviveSession direct: %v", err)
	}

	// Act.
	err := m.SubmitPrompt(context.Background(), "ws", "req-1", "hello", "",
		corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)

	// Assert.
	if errors.Is(err, ErrHibernated) {
		t.Fatalf("SubmitPrompt after a direct revival = %v, want the gate gone", err)
	}
}

// THE ACK IS AT ACCEPTANCE. ReviveSession returns once the session is up and
// `/compact` is submitted, with the completion wait still pending — acking at
// completion left the user's ReviveSessionCmd unacked for as long as a
// whole-conversation compaction takes.
//
// AMENDED from a test that ran the revival on its own goroutine because the
// call could not return until the compaction was signalled; the direct call
// below IS the contract now.
func TestReviveSessionCompactFirstAcksOnceTheCompactionIsSubmitted(t *testing.T) {
	// Arrange.
	m, _, _ := reviveRig(t, registry.HibernationCauseIdleCutoff)

	// Act: the revival returns without anything ever signalling completion.
	err := m.ReviveSession(context.Background(), "ws", ReviveModeCompactFirst)

	// Assert.
	if err != nil {
		t.Fatalf("ReviveSession compact_first = %v, want the ack at acceptance", err)
	}
	if signal := compactionWaiter(m, "ws"); signal == nil {
		t.Fatal("the revival returned with no compaction waiter armed; the ack must mean the compaction is pending, not that it is over")
	}
}

// COMPACT-FIRST STAYS GATED UNTIL THE COMPACTION LANDS. Keeping the durable
// record hibernated is what keeps the gate standing, so "prompts are refused
// until compaction completes" is the same mechanism that refused them before
// the revival began rather than a second gate that could disagree with it.
//
// AMENDED for the acceptance ack: the gate is now inspected after the call has
// already returned, and the release is observed on the detached wait's own
// schedule rather than at the return.
func TestReviveSessionCompactFirstStaysGatedUntilCompactionLands(t *testing.T) {
	// Arrange.
	m, _, hib := reviveRig(t, registry.HibernationCauseIdleCutoff)
	if err := m.ReviveSession(context.Background(), "ws", ReviveModeCompactFirst); err != nil {
		t.Fatalf("ReviveSession compact_first: %v", err)
	}
	signal := awaitCompactionWaiter(t, m, "ws")

	// Assert the gate before the compaction is reported.
	if detail, asleep := hib.HibernationOf("s1"); !asleep || detail.Cause == "" {
		t.Fatalf("hibernation detail = %+v while the compaction is still running, want the session STILL gated", detail)
	}

	// Act.
	signal()

	// Assert.
	awaitGateReleased(t, hib, "s1")
}

// THE CLAIM IS HELD PAST THE ACK. The compaction is still pending, so a second
// ReviveSessionCmd must still be nacked: admitting it would arm a second waiter
// over the first one's, which is the strand armCompactionWait refuses.
func TestReviveSessionCompactFirstHoldsTheClaimWhileTheCompactionIsPending(t *testing.T) {
	// Arrange.
	m, _, _ := reviveRig(t, registry.HibernationCauseIdleCutoff)
	if err := m.ReviveSession(context.Background(), "ws", ReviveModeCompactFirst); err != nil {
		t.Fatalf("ReviveSession compact_first: %v", err)
	}
	awaitCompactionWaiter(t, m, "ws")

	// Act.
	err := m.ReviveSession(context.Background(), "ws", ReviveModeCompactFirst)

	// Assert.
	if !errors.Is(err, ErrRevivalInFlight) {
		t.Fatalf("a second revival while the compaction is pending = %v, want ErrRevivalInFlight", err)
	}
}

// THE CLAIM IS RELEASED WHEN THE COMPACTION LANDS, by the detached wait rather
// than by the returning call. A claim that outlived its compaction would make
// the workspace permanently un-revivable.
func TestReviveSessionCompactFirstReleasesTheClaimWhenTheCompactionLands(t *testing.T) {
	// Arrange.
	m, _, _ := reviveRig(t, registry.HibernationCauseIdleCutoff)
	if err := m.ReviveSession(context.Background(), "ws", ReviveModeCompactFirst); err != nil {
		t.Fatalf("ReviveSession compact_first: %v", err)
	}
	signal := awaitCompactionWaiter(t, m, "ws")

	// Act.
	signal()

	// Assert.
	awaitClaimFree(t, m, "ws")
}

// EACH REVIVAL'S COMPACTION SUBMITS UNDER A NAME OF ITS OWN. A request id is
// the submitted turn's identity on the wire, and the durable turn ledger
// refuses a second start under a name it already holds — so a session
// hibernated and compact-revived twice would otherwise submit its second
// compaction under the first one's name and be refused.
//
// AMENDED for the acceptance ack: each revival's return no longer means its
// compaction is over, so the second one waits for the first's detached wait to
// release the claim rather than for the first call to return.
func TestReviveSessionCompactFirstSubmitsUnderAFreshRequestIDEachTime(t *testing.T) {
	// Arrange.
	m, _, hib := reviveRig(t, registry.HibernationCauseIdleCutoff)

	// Act: two full compact-first revivals of the SAME session.
	for range 2 {
		hib.setAsleep("s1", registry.HibernationDetail{Cause: registry.HibernationCauseIdleCutoff, SinceMs: 42})
		if err := m.ReviveSession(context.Background(), "ws", ReviveModeCompactFirst); err != nil {
			t.Fatalf("ReviveSession compact_first: %v", err)
		}
		awaitCompactionWaiter(t, m, "ws")()
		awaitClaimFree(t, m, "ws")
	}

	// Assert.
	c := fakeClientFor(t, m, "ws")
	c.mu.Lock()
	defer c.mu.Unlock()
	if len(c.requestIDs) != 2 {
		t.Fatalf("submitted request ids = %v, want one per revival", c.requestIDs)
	}
	if c.requestIDs[0] == c.requestIDs[1] {
		t.Fatalf("both revivals submitted under %q; the second turn would claim the first's identity", c.requestIDs[0])
	}
}

// A COMPACTION THAT NEVER COMPLETES LEAVES THE SESSION GATED. The clear is the
// last step and is reached only on the completion signal, so there is no path
// in which a failed compaction ends with an ungated session.
//
// AMENDED from cancelling the command context, which no longer bounds the wait:
// the ack is at acceptance, so the caller's context is gone by the time the
// compaction is running and the daemon's own bound is what ends it.
func TestReviveSessionCompactFirstStaysGatedWhenTheCompactionTimesOut(t *testing.T) {
	// Arrange.
	m, _, hib := reviveRig(t, registry.HibernationCauseIdleCutoff)
	m.mu.Lock()
	m.reviveCompactBoundOverride = time.Millisecond
	m.mu.Unlock()

	// Act: never signal completion, so the bound is what ends the wait.
	if err := m.ReviveSession(context.Background(), "ws", ReviveModeCompactFirst); err != nil {
		t.Fatalf("ReviveSession compact_first: %v", err)
	}
	awaitClaimFree(t, m, "ws")

	// Assert.
	if detail, asleep := hib.HibernationOf("s1"); !asleep || detail.Cause == "" {
		t.Fatalf("hibernation detail = %+v after a compaction that timed out, want the session STILL gated", detail)
	}
}

// THE TIMED-OUT REVIVAL RELEASES ITS CLAIM, so the user can choose again. A
// claim held past its own bound would make the workspace permanently
// un-revivable, which is a worse failure than the double-submit it prevents.
func TestReviveSessionCompactFirstReleasesTheClaimWhenTheCompactionTimesOut(t *testing.T) {
	// Arrange.
	m, _, _ := reviveRig(t, registry.HibernationCauseIdleCutoff)
	m.mu.Lock()
	m.reviveCompactBoundOverride = time.Millisecond
	m.mu.Unlock()
	if err := m.ReviveSession(context.Background(), "ws", ReviveModeCompactFirst); err != nil {
		t.Fatalf("ReviveSession compact_first: %v", err)
	}

	// Act.
	awaitClaimFree(t, m, "ws")

	// Assert.
	release, _, err := m.claimRevival("ws", "s1")
	if err != nil {
		t.Fatalf("claimRevival after a timed-out compaction = %v, want the claim free", err)
	}
	release()
}

// THE TIMED-OUT REVIVAL RETIRES ITS WAITER, so the revival the user chooses
// next can arm its own. A waiter that outlived its bound would make the first
// compact-first revival the only one this controller could ever serve.
func TestReviveSessionCompactFirstDisarmsTheWaiterWhenTheCompactionTimesOut(t *testing.T) {
	// Arrange.
	m, _, _ := reviveRig(t, registry.HibernationCauseIdleCutoff)
	m.mu.Lock()
	m.reviveCompactBoundOverride = time.Millisecond
	m.mu.Unlock()
	if err := m.ReviveSession(context.Background(), "ws", ReviveModeCompactFirst); err != nil {
		t.Fatalf("ReviveSession compact_first: %v", err)
	}
	awaitClaimFree(t, m, "ws")

	// Act.
	m.mu.Lock()
	d := m.byWS["ws"]
	m.mu.Unlock()
	_, disarm, err := m.armCompactionWait(d)

	// Assert.
	if err != nil {
		t.Fatalf("armCompactionWait after a timed-out compaction = %v, want the slot free", err)
	}
	disarm()
}

// A SUBMIT THAT FAILED NEVER REACHES THE ACK. The ack means the compaction was
// submitted, so a shim that refused it is an error to the caller and the
// session stays gated with nothing left running.
func TestReviveSessionCompactFirstStaysGatedWhenTheSubmitFails(t *testing.T) {
	// Arrange.
	m, _, hib := reviveRig(t, registry.HibernationCauseIdleCutoff)
	c := fakeClientFor(t, m, "ws")
	c.mu.Lock()
	c.submitErrOnce = errors.New("shim refused the compaction")
	c.mu.Unlock()

	// Act.
	err := m.ReviveSession(context.Background(), "ws", ReviveModeCompactFirst)

	// Assert.
	if err == nil {
		t.Fatal("a refused compaction submit reported success; a compact-first revival must never ack a compaction that was never submitted")
	}
	if detail, asleep := hib.HibernationOf("s1"); !asleep || detail.Cause == "" {
		t.Fatalf("hibernation detail = %+v after a refused submit, want the session STILL gated", detail)
	}
}

// A REFUSED SUBMIT UNWINDS THE CLAIM. The handoff never happened, so nothing
// else will release it and the workspace would be permanently un-revivable.
func TestReviveSessionCompactFirstReleasesTheClaimWhenTheSubmitFails(t *testing.T) {
	// Arrange.
	m, _, _ := reviveRig(t, registry.HibernationCauseIdleCutoff)
	c := fakeClientFor(t, m, "ws")
	c.mu.Lock()
	c.submitErrOnce = errors.New("shim refused the compaction")
	c.mu.Unlock()
	_ = m.ReviveSession(context.Background(), "ws", ReviveModeCompactFirst)

	// Act.
	release, _, err := m.claimRevival("ws", "s1")

	// Assert.
	if err != nil {
		t.Fatalf("claimRevival after a refused submit = %v, want the claim free", err)
	}
	release()
}

// A MANAGER CLOSING AT THE HANDOFF IS A LOUD REFUSAL. Nothing would be left to
// observe the compaction, so the revival is reported as failed and the session
// stays gated rather than acked into a wait that will never run.
func TestReviveSessionCompactFirstRefusesTheHandoffWhenTheManagerIsClosing(t *testing.T) {
	// Arrange — the close lands between the submit and the handoff.
	m, _, _ := reviveRig(t, registry.HibernationCauseIdleCutoff)
	c := fakeClientFor(t, m, "ws")
	c.mu.Lock()
	c.onSubmit = func() {
		m.mu.Lock()
		m.closed = true
		m.mu.Unlock()
	}
	c.mu.Unlock()

	// Act.
	err := m.ReviveSession(context.Background(), "ws", ReviveModeCompactFirst)

	// Assert.
	if err == nil {
		t.Fatal("a revival handed off to a closing manager reported success; nothing would be left to observe the compaction")
	}
}

// THE DAEMON SHUTTING DOWN MID-COMPACTION LEAVES THE SESSION GATED. The
// detached wait ends on the root context, and its clear is reached only on the
// completion signal, so an interrupted compaction is a sleep the user can
// choose again rather than an ungated session nothing compacted.
func TestReviveSessionCompactFirstStaysGatedWhenTheDaemonShutsDownMidCompaction(t *testing.T) {
	// Arrange.
	m, _, hib := reviveRig(t, registry.HibernationCauseIdleCutoff)
	if err := m.ReviveSession(context.Background(), "ws", ReviveModeCompactFirst); err != nil {
		t.Fatalf("ReviveSession compact_first: %v", err)
	}
	awaitCompactionWaiter(t, m, "ws")

	// Act — Close cancels the root context and JOINS the detached wait.
	m.Close()

	// Assert.
	if detail, asleep := hib.HibernationOf("s1"); !asleep || detail.Cause == "" {
		t.Fatalf("hibernation detail = %+v after a shutdown mid-compaction, want the session STILL gated", detail)
	}
}

// A CLEAR THAT FAILED LEAVES THE GATE STANDING. The compaction landed, but the
// record still claims a sleep, and a gate the daemon believes it retired while
// the durable state disagrees is the one state the revival must never reach.
func TestReviveSessionCompactFirstStaysGatedWhenTheGateReleaseFails(t *testing.T) {
	// Arrange.
	m, _, hib := reviveRig(t, registry.HibernationCauseIdleCutoff)
	if err := m.ReviveSession(context.Background(), "ws", ReviveModeCompactFirst); err != nil {
		t.Fatalf("ReviveSession compact_first: %v", err)
	}
	signal := awaitCompactionWaiter(t, m, "ws")
	hib.mu.Lock()
	hib.writeErr = errors.New("registry write failed")
	hib.mu.Unlock()

	// Act.
	signal()
	awaitClaimFree(t, m, "ws")

	// Assert.
	if detail, asleep := hib.HibernationOf("s1"); !asleep || detail.Cause == "" {
		t.Fatalf("hibernation detail = %+v after a failed gate release, want the session STILL gated", detail)
	}
}

// A session that is NOT asleep is a no-op rather than an error: the user acted
// on a gate that is no longer standing.
func TestReviveSessionOnAnAwakeSessionIsANoOp(t *testing.T) {
	// Arrange.
	m, _, _ := newHibernationRig(t)

	// Act.
	err := m.ReviveSession(context.Background(), "ws", ReviveModeDirect)

	// Assert.
	if err != nil {
		t.Fatalf("ReviveSession on an awake session = %v, want a benign no-op", err)
	}
}

// ---------------------------------------------------------------------------
// Forced hibernation
// ---------------------------------------------------------------------------

// A HELD MERGE LEASE IS A LOUD NACK. The daemon never discards in-flight work
// to satisfy a hibernate; the user interrupts first.
func TestHibernateWorkspaceRefusedWhileTheMergeLeaseIsHeld(t *testing.T) {
	// Arrange.
	m, applier, hib := newHibernationRig(t)
	applier.mergeLeases = map[string]bool{"ws": true}

	// Act.
	err := m.HibernateWorkspace("ws")

	// Assert.
	if err == nil {
		t.Fatal("HibernateWorkspace under a held merge lease = nil, want a loud nack")
	}
	if n := hib.writeCount(); n != 0 {
		t.Fatalf("%d durable writes for a refused hibernate, want none", n)
	}
}

// The forced path records the FORCED cause, so the revival gate can say the
// user did this rather than attributing it to a timer.
func TestHibernateWorkspaceRecordsTheForcedCause(t *testing.T) {
	// Arrange.
	m, _, hib := newHibernationRig(t)

	// Act.
	if err := m.HibernateWorkspace("ws"); err != nil {
		t.Fatalf("HibernateWorkspace: %v", err)
	}

	// Assert.
	got, _ := hib.HibernationOf("s1")
	if got.Cause != registry.HibernationCauseForced {
		t.Fatalf("cause = %q, want %q", got.Cause, registry.HibernationCauseForced)
	}
}

// ---------------------------------------------------------------------------
// The revival claim
// ---------------------------------------------------------------------------

// THE EXCLUSIVITY CLAIM. Revival used to be a check-then-act with no claim at
// all: two ReviveSessionCmds both read a hibernated session, both brought it up
// and both submitted `/compact` under the SAME request id.
func TestReviveSessionRefusesAConcurrentRevival(t *testing.T) {
	// Arrange — the claim a revival already in flight would be holding.
	m, _, _ := reviveRig(t, registry.HibernationCauseIdleCutoff)
	release, _, err := m.claimRevival("ws", "s1")
	if err != nil {
		t.Fatalf("claimRevival: %v", err)
	}
	defer release()

	// Act.
	err = m.ReviveSession(context.Background(), "ws", ReviveModeDirect)

	// Assert.
	if !errors.Is(err, ErrRevivalInFlight) {
		t.Fatalf("ReviveSession while another is in flight = %v, want ErrRevivalInFlight", err)
	}
}

// THE REFUSED REVIVAL CHANGES NOTHING. The gate the second command was
// answering must still be standing, or the user would be left with an ungated
// session that no revival actually completed.
func TestReviveSessionRefusedByTheClaimLeavesTheGateStanding(t *testing.T) {
	// Arrange.
	m, _, hib := reviveRig(t, registry.HibernationCauseIdleCutoff)
	release, _, err := m.claimRevival("ws", "s1")
	if err != nil {
		t.Fatalf("claimRevival: %v", err)
	}
	defer release()

	// Act.
	_ = m.ReviveSession(context.Background(), "ws", ReviveModeDirect)

	// Assert.
	detail, _ := hib.HibernationOf("s1")
	if detail.Cause == "" {
		t.Fatal("a revival refused by the claim cleared the hibernation; the gate must still stand")
	}
}

// THE CLAIM IS RELEASED. An exclusivity claim that outlived its operation would
// make the workspace permanently un-revivable, which is a worse failure than
// the double-submit it prevents.
func TestReviveSessionReleasesTheClaim(t *testing.T) {
	// Arrange.
	m, _, _ := reviveRig(t, registry.HibernationCauseIdleCutoff)
	if err := m.ReviveSession(context.Background(), "ws", ReviveModeDirect); err != nil {
		t.Fatalf("ReviveSession: %v", err)
	}

	// Act.
	release, _, err := m.claimRevival("ws", "s1")

	// Assert.
	if err != nil {
		t.Fatalf("claimRevival after a completed revival = %v, want the claim free", err)
	}
	release()
}

// A SECOND ARM IS A LOUD REFUSAL, not an overwrite. The claim above makes this
// unreachable; it is kept as the fail-hard detection, because overwriting would
// strand the first revival on a channel nothing will ever close.
func TestArmCompactionWaitRefusesToOverwriteAnExistingWaiter(t *testing.T) {
	// Arrange.
	m, _, _ := reviveRig(t, registry.HibernationCauseIdleCutoff)
	m.mu.Lock()
	d := m.byWS["ws"]
	m.mu.Unlock()
	if _, _, err := m.armCompactionWait(d); err != nil {
		t.Fatalf("first armCompactionWait: %v", err)
	}

	// Act.
	_, _, err := m.armCompactionWait(d)

	// Assert.
	if err == nil {
		t.Fatal("a second armCompactionWait = nil, want a refusal; overwriting strands the revival that owns the first waiter")
	}
}

// THE DISARM RETIRES THE WAITER, so a later revival of the same session can arm
// its own. A refusal that never cleared would make the first compact-first
// revival the only one this controller could ever serve.
func TestArmCompactionWaitDisarmFreesTheSlot(t *testing.T) {
	// Arrange.
	m, _, _ := reviveRig(t, registry.HibernationCauseIdleCutoff)
	m.mu.Lock()
	d := m.byWS["ws"]
	m.mu.Unlock()
	_, disarm, err := m.armCompactionWait(d)
	if err != nil {
		t.Fatalf("first armCompactionWait: %v", err)
	}
	disarm()

	// Act.
	_, _, err = m.armCompactionWait(d)

	// Assert.
	if err != nil {
		t.Fatalf("armCompactionWait after a disarm = %v, want the slot free", err)
	}
}

// awaitCompactionWaiter blocks until the revival has armed its compaction wait
// and returns the func that fires the completion signal.
//
// It rendezvouses on the ARMED CALLBACK rather than on a clock: the callback is
// installed under the manager mutex before the compaction is submitted, so its
// presence is the exact moment the revival is waiting, and nothing else has to
// be timed.
func awaitCompactionWaiter(t *testing.T, m *Manager, workspace string) func() {
	t.Helper()
	for {
		if signal := compactionWaiter(m, workspace); signal != nil {
			return signal
		}
		runtime.Gosched()
	}
}

// compactionWaiter reports the armed completion callback, or nil when none is
// installed. It is the single read awaitCompactionWaiter spins on.
func compactionWaiter(m *Manager, workspace string) func() {
	m.mu.Lock()
	defer m.mu.Unlock()
	d, live := m.byWS[workspace]
	if !live || d.consumer == nil {
		return nil
	}
	return d.consumer.onContextCompacted
}

// awaitClaimFree blocks until the workspace's revival claim is released.
//
// It rendezvouses on the CLAIM ITSELF rather than on a clock: the detached
// completion wait releases it under the manager mutex as its last act before
// exiting, so its absence is the exact moment that wait is over.
func awaitClaimFree(t *testing.T, m *Manager, workspace string) {
	t.Helper()
	for {
		m.mu.Lock()
		held := m.reviving[workspace]
		m.mu.Unlock()
		if !held {
			return
		}
		runtime.Gosched()
	}
}

// awaitGateReleased blocks until the session's durable hibernation record stops
// standing the revival gate up.
func awaitGateReleased(t *testing.T, hib *fakeHibernations, sessionID string) {
	t.Helper()
	for {
		if detail, asleep := hib.HibernationOf(sessionID); !asleep || detail.Cause == "" {
			return
		}
		runtime.Gosched()
	}
}

// ---------------------------------------------------------------------------
// The revival PARK: prompts typed while a compact-first compaction is pending
// ---------------------------------------------------------------------------
//
// GATED MEANS DELAYED, NOT DROPPED. The gate's refusal is the right answer for
// a hibernated session nobody has made a revival decision about — that is the
// whole feature. It was the wrong answer for the window AFTER the user chose
// compact-first: the record stays hibernated on purpose while the compaction
// runs, so every prompt typed in that window was nacked outright with no queue
// entry made at all, and the two halves of the contract — do not RUN before the
// compaction lands, DO run after it — were served by a refusal that honored
// only the first.

// revivalParkRig is a session mid-compact-first-revival: brought up, still
// durably hibernated, its compaction armed and pending.
func revivalParkRig(t *testing.T) (*Manager, *fakeHibernations) {
	t.Helper()
	m, _, hib := reviveRig(t, registry.HibernationCauseIdleCutoff)
	if err := m.ReviveSession(context.Background(), "ws", ReviveModeCompactFirst); err != nil {
		t.Fatalf("ReviveSession compact_first: %v", err)
	}
	awaitCompactionWaiter(t, m, "ws")
	return m, hib
}

// revivalParkedEntries reports the workspace's queue entries.
func revivalParkedEntries(m *Manager, workspace string) []*queueEntry {
	m.mu.Lock()
	defer m.mu.Unlock()
	d, live := m.byWS[workspace]
	if !live {
		return nil
	}
	return append([]*queueEntry(nil), d.queue.entries...)
}

// parkDuringCompactionSubmit arranges for one prompt to be submitted from
// INSIDE the revival's own `/compact` submit, which is the deterministic
// rendezvous for "a prompt arrived while the compaction was pending": the hook
// runs on the reviving goroutine, with the claim held and the record still
// hibernated, before the bounded wait is started and before the submit can
// fail.
func parkDuringCompactionSubmit(t *testing.T, m *Manager, requestID, text string) {
	t.Helper()
	c := fakeClientFor(t, m, "ws")
	var once sync.Once
	c.mu.Lock()
	c.onSubmit = func() {
		once.Do(func() {
			if err := m.SubmitPrompt(context.Background(), "ws", requestID, text, "",
				corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT); err != nil {
				t.Errorf("SubmitPrompt during the compaction submit: %v", err)
				return
			}
			// The park is the PRECONDITION of every disposition test that uses
			// this hook, so it is verified here rather than assumed: a drop
			// assertion over a queue nothing was ever parked on would pass for
			// the wrong reason.
			if entries := revivalParkedEntries(m, "ws"); len(entries) != 1 {
				t.Errorf("%d entr(ies) parked during the compaction submit, want the one prompt", len(entries))
			}
		})
	}
	c.mu.Unlock()
}

// THE PROMPT IS ADMITTED, not refused. The user has already made the revival
// choice; refusing what they type while the daemon carries it out asks them to
// make it twice.
func TestPromptDuringAPendingCompactionIsAcceptedRatherThanNacked(t *testing.T) {
	// Arrange.
	m, _ := revivalParkRig(t)

	// Act.
	err := m.SubmitPrompt(context.Background(), "ws", "req-1", "typed-during-compaction", "",
		corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)

	// Assert.
	if err != nil {
		t.Fatalf("SubmitPrompt during a pending compaction = %v, want the prompt accepted and parked", err)
	}
}

// THE PROMPT BECOMES A PARKED QUEUE ENTRY, named to the session whose revival
// is holding it — the evidence that it was delayed rather than dropped.
func TestPromptDuringAPendingCompactionIsParkedOnTheQueue(t *testing.T) {
	// Arrange.
	m, _ := revivalParkRig(t)

	// Act.
	if err := m.SubmitPrompt(context.Background(), "ws", "req-1", "typed-during-compaction", "",
		corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert.
	entries := revivalParkedEntries(m, "ws")
	if len(entries) != 1 {
		t.Fatalf("%d queue entries, want the one parked prompt", len(entries))
	}
	if entries[0].revivalHoldSessionID != "s1" {
		t.Fatalf("revival hold = %q, want the revived session %q", entries[0].revivalHoldSessionID, "s1")
	}
}

// IT CARRIES THE HOLD STAMP for its whole parked life. PENDING would claim a
// classifier is running on it, and none ever will.
func TestARevivalParkedPromptIsStampedHold(t *testing.T) {
	// Arrange.
	m, _ := revivalParkRig(t)

	// Act.
	if err := m.SubmitPrompt(context.Background(), "ws", "req-1", "typed-during-compaction", "",
		corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert.
	entries := revivalParkedEntries(m, "ws")
	if len(entries) != 1 {
		t.Fatalf("%d queue entries, want the one parked prompt", len(entries))
	}
	if entries[0].classification != frontendv1.QueueClassification_QUEUE_CLASSIFICATION_HOLD {
		t.Fatalf("classification = %s, want HOLD", entries[0].classification)
	}
}

// THE CLASSIFIER NEVER RUNS ON IT. The turn in front of it is the revival's own
// `/compact`, and the only verdict that would change anything demands
// interrupting the very compaction the user chose to pay for.
func TestARevivalParkedPromptIsNeverClassified(t *testing.T) {
	// Arrange.
	m, _, _ := reviveRig(t, registry.HibernationCauseIdleCutoff)
	cls := &fakeClassifier{}
	m.cfg.Classifier = cls
	if err := m.ReviveSession(context.Background(), "ws", ReviveModeCompactFirst); err != nil {
		t.Fatalf("ReviveSession compact_first: %v", err)
	}
	awaitCompactionWaiter(t, m, "ws")

	// Act.
	if err := m.SubmitPrompt(context.Background(), "ws", "req-1", "typed-during-compaction", "",
		corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert.
	if reqs := cls.requests(); len(reqs) != 0 {
		t.Fatalf("%d classification(s) for a revival-parked prompt, want none: %+v", len(reqs), reqs)
	}
}

// IT DOES NOT RUN. The park is only worth having if the prompt genuinely stays
// behind the compaction; a parked prompt that still reached the shim would pay
// exactly the whole-conversation cost compact-first exists to pay once.
func TestARevivalParkedPromptDoesNotReachTheShim(t *testing.T) {
	// Arrange.
	m, _ := revivalParkRig(t)

	// Act.
	if err := m.SubmitPrompt(context.Background(), "ws", "req-1", "typed-during-compaction", "",
		corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert.
	for _, p := range fakeClientFor(t, m, "ws").promptTexts() {
		if p == "typed-during-compaction" {
			t.Fatal("a revival-parked prompt reached the shim while the compaction was still pending")
		}
	}
}

// THE RELEASE. The compaction lands, the gate comes down, and the prompt the
// user typed during it is delivered — the second half of delayed-never-dropped.
func TestALandedCompactionDeliversTheRevivalParkedPrompt(t *testing.T) {
	// Arrange.
	m, _ := revivalParkRig(t)
	signal := compactionWaiter(m, "ws")
	if err := m.SubmitPrompt(context.Background(), "ws", "req-1", "released-by-compaction", "",
		corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Act.
	signal()

	// Assert.
	c := fakeClientFor(t, m, "ws")
	waitFor(t, "the parked prompt to be delivered once the compaction landed", func() bool {
		for _, p := range c.promptTexts() {
			if p == "released-by-compaction" {
				return true
			}
		}
		return false
	})
}

// THE RELEASED ENTRY LEAVES THE QUEUE. A delivered prompt that stayed queued
// would render a chip for work the agent is already doing.
func TestALandedCompactionEmptiesTheRevivalParkedQueue(t *testing.T) {
	// Arrange.
	m, _ := revivalParkRig(t)
	signal := compactionWaiter(m, "ws")
	if err := m.SubmitPrompt(context.Background(), "ws", "req-1", "released-by-compaction", "",
		corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Act.
	signal()

	// Assert.
	waitFor(t, "the parked entry to leave the queue", func() bool {
		return len(revivalParkedEntries(m, "ws")) == 0
	})
}

// THE BOUND IS THE PARKED ENTRY'S EXPIRY. A compaction that never lands leaves
// the session asleep, so nothing can ever deliver what it parked — and an entry
// no path can select is a leak, not a delay. It is dropped, loudly.
func TestACompactionThatTimesOutDropsTheRevivalParkedPrompt(t *testing.T) {
	// Arrange.
	m, _, _ := reviveRig(t, registry.HibernationCauseIdleCutoff)
	m.mu.Lock()
	m.reviveCompactBoundOverride = time.Millisecond
	m.mu.Unlock()
	parkDuringCompactionSubmit(t, m, "req-1", "typed-during-compaction")

	// Act.
	if err := m.ReviveSession(context.Background(), "ws", ReviveModeCompactFirst); err != nil {
		t.Fatalf("ReviveSession compact_first: %v", err)
	}
	awaitClaimFree(t, m, "ws")

	// Assert.
	waitFor(t, "the parked entry to be dropped when the compaction bound expired", func() bool {
		return len(revivalParkedEntries(m, "ws")) == 0
	})
}

// THE EXPIRED PARK IS NEVER SUBMITTED. Dropping it is the honest outcome;
// delivering it on the way out would run the prompt against the uncompacted
// conversation the revival failed to compact.
func TestACompactionThatTimesOutNeverSubmitsTheParkedPrompt(t *testing.T) {
	// Arrange.
	m, _, _ := reviveRig(t, registry.HibernationCauseIdleCutoff)
	m.mu.Lock()
	m.reviveCompactBoundOverride = time.Millisecond
	m.mu.Unlock()
	parkDuringCompactionSubmit(t, m, "req-1", "typed-during-compaction")
	if err := m.ReviveSession(context.Background(), "ws", ReviveModeCompactFirst); err != nil {
		t.Fatalf("ReviveSession compact_first: %v", err)
	}
	awaitClaimFree(t, m, "ws")
	waitFor(t, "the parked entry to be dropped when the compaction bound expired", func() bool {
		return len(revivalParkedEntries(m, "ws")) == 0
	})

	// Act — the drop has happened; nothing further may deliver the prompt.
	texts := fakeClientFor(t, m, "ws").promptTexts()

	// Assert.
	for _, p := range texts {
		if p == "typed-during-compaction" {
			t.Fatal("a revival-parked prompt was submitted after its compaction timed out")
		}
	}
}

// A REVIVAL THAT NEVER REACHED ITS COMPACTION DISPOSES OF WHAT IT PARKED. The
// session is up by the time a submit can fail, so a prompt can already be
// parked behind a revival that is about to unwind — and that unwind is the last
// thing that knows a revival was ever in flight.
func TestARefusedCompactionSubmitDropsTheRevivalParkedPrompt(t *testing.T) {
	// Arrange.
	m, _, _ := reviveRig(t, registry.HibernationCauseIdleCutoff)
	parkDuringCompactionSubmit(t, m, "req-1", "typed-during-compaction")
	c := fakeClientFor(t, m, "ws")
	c.mu.Lock()
	c.submitErrOnce = errors.New("shim refused the compaction")
	c.mu.Unlock()

	// Act.
	if err := m.ReviveSession(context.Background(), "ws", ReviveModeCompactFirst); err == nil {
		t.Fatal("a refused compaction submit reported success")
	}

	// Assert.
	if entries := revivalParkedEntries(m, "ws"); len(entries) != 0 {
		t.Fatalf("%d entr(ies) still parked after the revival unwound, want them dropped rather than left behind a gate nothing will open", len(entries))
	}
}

// A CLEAR THAT FAILED LEAVES THE GATE STANDING, so what it parked is dropped
// too: the compaction landed but the record still claims a sleep, and every
// delivery path refuses a gated prompt.
func TestAFailedGateReleaseDropsTheRevivalParkedPrompt(t *testing.T) {
	// Arrange.
	m, hib := revivalParkRig(t)
	signal := compactionWaiter(m, "ws")
	if err := m.SubmitPrompt(context.Background(), "ws", "req-1", "typed-during-compaction", "",
		corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}
	hib.mu.Lock()
	hib.writeErr = errors.New("registry write failed")
	hib.mu.Unlock()

	// Act.
	signal()
	awaitClaimFree(t, m, "ws")

	// Assert.
	waitFor(t, "the parked entry to be dropped when the gate could not be released", func() bool {
		return len(revivalParkedEntries(m, "ws")) == 0
	})
}

// A FORCE IS REFUSED. The control's whole meaning is "run it now", and running
// it now would answer the prompt against the uncompacted conversation the user
// chose this mode to avoid paying for.
func TestForceIsRefusedForARevivalParkedPrompt(t *testing.T) {
	// Arrange.
	m, _ := revivalParkRig(t)
	if err := m.SubmitPrompt(context.Background(), "ws", "req-1", "typed-during-compaction", "",
		corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}
	entries := revivalParkedEntries(m, "ws")
	if len(entries) != 1 {
		t.Fatalf("%d queue entries, want the one parked prompt", len(entries))
	}

	// Act.
	err := m.ForceQueueEntry("ws", entries[0].id)

	// Assert.
	if !errors.Is(err, ErrHibernated) {
		t.Fatalf("ForceQueueEntry on a revival-parked prompt = %v, want the hibernation refusal", err)
	}
}

// A CANCEL IS HONORED. Nothing has to run to take a prompt back, and the
// refusal above is only defensible because the user keeps this way out of the
// wait it imposes.
func TestCancelIsHonoredForARevivalParkedPrompt(t *testing.T) {
	// Arrange.
	m, _ := revivalParkRig(t)
	if err := m.SubmitPrompt(context.Background(), "ws", "req-1", "typed-during-compaction", "",
		corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}
	entries := revivalParkedEntries(m, "ws")
	if len(entries) != 1 {
		t.Fatalf("%d queue entries, want the one parked prompt", len(entries))
	}

	// Act.
	if err := m.CancelQueueEntry("ws", entries[0].id); err != nil {
		t.Fatalf("CancelQueueEntry on a revival-parked prompt = %v, want the prompt taken back", err)
	}

	// Assert.
	if left := revivalParkedEntries(m, "ws"); len(left) != 0 {
		t.Fatalf("%d entr(ies) left after the cancel, want the prompt gone", len(left))
	}
}

// THE UNDECIDED SESSION'S NACK IS UNCHANGED, and it is the design. With no
// revival in flight there is nothing for a prompt to WAIT for, so admitting it
// to a queue would make the revival choice for the user by promising a delivery
// only they can authorize.
func TestPromptOnAHibernatedSessionWithNoRevivalInFlightIsStillNacked(t *testing.T) {
	// Arrange — hibernated, and no revival claimed.
	m, _, _ := reviveRig(t, registry.HibernationCauseIdleCutoff)

	// Act.
	err := m.SubmitPrompt(context.Background(), "ws", "req-1", "hello", "",
		corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)

	// Assert.
	if !errors.Is(err, ErrHibernated) {
		t.Fatalf("SubmitPrompt on an undecided hibernated session = %v, want ErrHibernated", err)
	}
}

// NOTHING IS PARKED FOR THE UNDECIDED SESSION EITHER. A nack that still left a
// queue entry behind would deliver the prompt at whatever boundary came next.
func TestPromptOnAHibernatedSessionWithNoRevivalInFlightParksNothing(t *testing.T) {
	// Arrange.
	m, _, _ := reviveRig(t, registry.HibernationCauseIdleCutoff)

	// Act.
	_ = m.SubmitPrompt(context.Background(), "ws", "req-1", "hello", "",
		corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)

	// Assert.
	if entries := revivalParkedEntries(m, "ws"); len(entries) != 0 {
		t.Fatalf("%d entr(ies) queued for a session whose prompt was nacked, want none", len(entries))
	}
}

// THE CLAIM ALONE DOES NOT PARK. A direct revival holds the very same claim and
// clears the gate FIRST, so a prompt arriving under it has nothing to wait for
// and must go straight through.
func TestARevivalClaimOverAnAwakeSessionParksNothing(t *testing.T) {
	// Arrange — the claim held over a session that is NOT hibernated.
	m, _, _ := newHibernationRig(t)
	release, _, err := m.claimRevival("ws", "s1")
	if err != nil {
		t.Fatalf("claimRevival: %v", err)
	}
	defer release()

	// Act.
	if err := m.SubmitPrompt(context.Background(), "ws", "req-1", "ordinary", "",
		corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert.
	if entries := revivalParkedEntries(m, "ws"); len(entries) != 0 {
		t.Fatalf("%d entr(ies) parked under a revival claim over an awake session, want the prompt forwarded", len(entries))
	}
}
