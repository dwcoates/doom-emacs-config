package sessioncontroller

import (
	"context"
	"errors"
	"runtime"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"

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
