package sessioncontroller

import (
	"context"
	"errors"
	"strings"
	"testing"

	"claude-repld/internal/keepalive"
	"claude-repld/internal/ssm"
)

// warmCompactRig is coldPingRig with a conversation big enough to be worth
// compacting already observed, which is the ordinary state of every session the
// warm-compaction arm reaches: a session that has run turns.
//
// The size is installed through the SAME reduction production uses rather than
// by assigning the field, so a test cannot pass against a figure the real
// response path would never have produced.
func warmCompactRig(t *testing.T, contextInputTokens int64) (*Manager, *logCapture) {
	t.Helper()
	m, _, _, _, capture := coldPingRig(t)
	if contextInputTokens > 0 {
		// The size rides one MAIN-AGENT response's cache read: the floor judges how
		// big the standing conversation is, and a standing conversation is the
		// prefix the cache is holding for the session's own agent.
		m.noteMainAgentContextSize(controllerFor(t, m), mainAgentUtilization("msg-prior", 0, 0, contextInputTokens))
	}
	return m, capture
}

// warmCompactAnchor is a durable last-turn-end distinct from the rig's own, so
// a test that asserts the anchor is asserting on the value it passed.
const warmCompactAnchor int64 = coldPingLastTurnEnd

// A SESSION BIG ENOUGH AND IDLE ENOUGH IS COMPACTED WHILE THE CACHE IS ALIVE.
// This is the whole feature: the whole-conversation read a compaction costs is
// served from the prompt cache here, and would be re-ingested at full price by
// the compact-first revival that used to be the only compaction the daemon ran.
func TestSubmitWarmCompactionSubmitsForAnIdleSessionAboveTheFloor(t *testing.T) {
	// Arrange.
	m, _ := warmCompactRig(t, keepalive.WarmCompactMinContextTokens)

	// Act.
	turnID, err := m.SubmitWarmCompaction(context.Background(), "ws", warmCompactAnchor)

	// Assert.
	if err != nil {
		t.Fatalf("SubmitWarmCompaction on an idle, above-floor session = %v, want a submitted compaction", err)
	}
	if turnID == "" {
		t.Fatal("SubmitWarmCompaction returned no turn id; the cold-read alarm has nothing to match a result against")
	}
}

// THE COMPACTION IS THE ONE THE REVIVAL ALREADY SUBMITS. Reusing `/compact`
// rather than inventing a prompt shape is what keeps the daemon's two
// compactions from drifting into two different things the vendor treats
// differently.
func TestSubmitWarmCompactionSubmitsTheCompactCommand(t *testing.T) {
	// Arrange.
	m, _ := warmCompactRig(t, keepalive.WarmCompactMinContextTokens)

	// Act.
	if _, err := m.SubmitWarmCompaction(context.Background(), "ws", warmCompactAnchor); err != nil {
		t.Fatalf("SubmitWarmCompaction: %v", err)
	}

	// Assert.
	c := fakeClientFor(t, m, "ws")
	c.mu.Lock()
	defer c.mu.Unlock()
	if len(c.prompts) != 1 || c.prompts[0] != compactCommandText {
		t.Fatalf("submitted prompts = %v, want exactly %q", c.prompts, compactCommandText)
	}
}

// A CONVERSATION BELOW THE FLOOR IS LEFT ALONE. Compacting it costs a
// full-history model call and buys back a context that was never the problem,
// while throwing away detail the user may still be working against.
func TestSubmitWarmCompactionDeclinesAConversationBelowTheFloor(t *testing.T) {
	// Arrange.
	m, capture := warmCompactRig(t, keepalive.WarmCompactMinContextTokens-1)

	// Act.
	_, err := m.SubmitWarmCompaction(context.Background(), "ws", warmCompactAnchor)

	// Assert.
	if !errors.Is(err, ErrWarmCompactNotEligible) {
		t.Fatalf("SubmitWarmCompaction below the floor = %v, want ErrWarmCompactNotEligible", err)
	}
	if !capture.contains("reason=context_below_floor") {
		t.Fatal("no record named the size floor as the reason; a decline with no stated reason is indistinguishable from the feature not running")
	}
}

// AN UNMEASURED CONVERSATION IS AN UNKNOWN, NOT A SMALL ONE. Every unknown
// answers none, the rule every other evaluator in the keep-alive policy
// follows: compacting a session this daemon has never seen a result for would
// be acting on absent evidence.
func TestSubmitWarmCompactionDeclinesASessionWhoseSizeIsUnknown(t *testing.T) {
	// Arrange: no result has ever been reduced for this session.
	m, capture := warmCompactRig(t, 0)

	// Act.
	_, err := m.SubmitWarmCompaction(context.Background(), "ws", warmCompactAnchor)

	// Assert.
	if !errors.Is(err, ErrWarmCompactNotEligible) {
		t.Fatalf("SubmitWarmCompaction with no observed size = %v, want ErrWarmCompactNotEligible", err)
	}
	if !capture.contains("reason=context_size_unknown") {
		t.Fatal("no record named the unknown size as the reason")
	}
}

// A LIVE TURN IS REAL WORK, and a compaction would rewrite the conversation out
// from under it.
func TestSubmitWarmCompactionDeclinesWhileATurnIsActive(t *testing.T) {
	// Arrange.
	m, capture := warmCompactRig(t, keepalive.WarmCompactMinContextTokens*10)
	m.mu.Lock()
	m.byWS["ws"].turn = turnRecord{phase: turnPhaseNamed, turnID: "user-turn"}
	m.mu.Unlock()

	// Act.
	_, err := m.SubmitWarmCompaction(context.Background(), "ws", warmCompactAnchor)

	// Assert.
	if !errors.Is(err, ErrWarmCompactNotEligible) {
		t.Fatalf("SubmitWarmCompaction during a live turn = %v, want ErrWarmCompactNotEligible", err)
	}
	if !capture.contains("reason=turn_active") {
		t.Fatal("no record named the live turn as the reason")
	}
}

// ONE ATTEMPT PER CACHE WINDOW. The arm is due across a whole span of elapsed
// idleness that the idle sweeper crosses many times, so without the anchor the
// same decision would be re-submitted on every tick.
func TestSubmitWarmCompactionDeclinesASecondAttemptAgainstTheSameAnchor(t *testing.T) {
	// Arrange: one attempt already made against this anchor, then its turn and
	// its claim both retired exactly as the turn-end boundary retires them — so
	// every earlier refusal is out of the way and the anchor is the only thing
	// that can decline the second attempt.
	m, capture := warmCompactRig(t, keepalive.WarmCompactMinContextTokens*10)
	turnID, err := m.SubmitWarmCompaction(context.Background(), "ws", warmCompactAnchor)
	if err != nil {
		t.Fatalf("first SubmitWarmCompaction: %v", err)
	}
	d := controllerFor(t, m)
	m.mu.Lock()
	m.releaseDaemonCompactionLocked(d, turnID)
	d.turn = turnRecord{}
	m.mu.Unlock()

	// Act.
	_, err = m.SubmitWarmCompaction(context.Background(), "ws", warmCompactAnchor)

	// Assert.
	if !errors.Is(err, ErrWarmCompactNotEligible) {
		t.Fatalf("a second attempt against the same anchor = %v, want ErrWarmCompactNotEligible", err)
	}
	if !capture.contains("reason=already_attempted_this_cache_window") {
		t.Fatal("no record named the anchor as the reason; the arm would re-submit on every sweep tick")
	}
}

// A FAILED SUBMIT LEAVES THE LIFECYCLE EXACTLY AS IT FOUND IT. The claim is
// released so no later turn's cost is misattributed to a compaction that never
// ran, the anchor STANDS so the failure is not re-attempted every tick, and
// nothing about the keep-alive or hibernation path is touched.
func TestFailedWarmCompactionReleasesItsClaimAndKeepsItsAnchor(t *testing.T) {
	// Arrange: a client that refuses the submit.
	m, capture := warmCompactRig(t, keepalive.WarmCompactMinContextTokens*10)
	c := fakeClientFor(t, m, "ws")
	c.mu.Lock()
	c.submitErrOnce = errors.New("the shim is gone")
	c.mu.Unlock()

	// Act.
	_, err := m.SubmitWarmCompaction(context.Background(), "ws", warmCompactAnchor)

	// Assert.
	if err == nil {
		t.Fatal("a refused submit returned nil; a failed warm compaction must be surfaced, never swallowed")
	}
	d := controllerFor(t, m)
	m.mu.Lock()
	claim, anchor := d.daemonCompaction, d.warmCompactAnchorMs
	m.mu.Unlock()
	if claim != nil {
		t.Fatalf("the compaction claim survived a failed submit as %+v; the next turn's cost would be attributed to a compaction that never ran", claim)
	}
	if anchor != warmCompactAnchor {
		t.Fatalf("the anchor after a failed submit = %d, want %d; a cleared anchor re-attempts the same failure on every sweep tick", anchor, warmCompactAnchor)
	}
	if !capture.contains("warm compaction SUBMIT FAILED") {
		t.Fatal("the failure was not recorded")
	}
}

// A SESSION WITH NO LIVE CONTROLLER IS NOT ELIGIBLE, and says so as an ordinary
// decline rather than a fault: the overwhelmingly common reason a sweep tick
// finds nothing to compact.
func TestSubmitWarmCompactionDeclinesAWorkspaceWithNoLiveController(t *testing.T) {
	// Arrange.
	m, _ := warmCompactRig(t, keepalive.WarmCompactMinContextTokens*10)

	// Act.
	_, err := m.SubmitWarmCompaction(context.Background(), "other-ws", warmCompactAnchor)

	// Assert.
	if !errors.Is(err, ErrWarmCompactNotEligible) {
		t.Fatalf("SubmitWarmCompaction for an unknown workspace = %v, want ErrWarmCompactNotEligible", err)
	}
}

// ---- The compaction gate --------------------------------------------------
//
// The anchor above makes the warm compaction exactly-once per CACHE WINDOW,
// which says nothing about a compaction some other path already ran. The gate
// is the fact every path closes, and these are the warm arm's two readings of
// it.

// A CONVERSATION THAT HAS ALREADY BEEN COMPACTED IS NOT COMPACTED AGAIN. This
// is the sequence the feature exists for: something else compacted this
// conversation, nothing has been said to it since, and compacting it now would
// read the whole history to produce a worse summary of the same material.
func TestSubmitWarmCompactionDeclinesAnAlreadyCompactedConversation(t *testing.T) {
	// Arrange.
	m, capture := warmCompactRig(t, keepalive.WarmCompactMinContextTokens*10)
	applierFor(t, m).setCompactionGate("ws", ssm.CompactionGate{CompactedAtMs: 200, PromptAtMs: 100})

	// Act.
	_, err := m.SubmitWarmCompaction(context.Background(), "ws", warmCompactAnchor)

	// Assert.
	if !errors.Is(err, ErrWarmCompactNotEligible) {
		t.Fatalf("SubmitWarmCompaction against an already-compacted conversation = %v, want ErrWarmCompactNotEligible", err)
	}
	if !capture.contains("reason=already_compacted:last_compacted_at_ms=200 last_cleared_at_ms=0 last_prompt_at_ms=100") {
		t.Fatal("no record named the gate as the reason, with the two timestamps the verdict was taken from")
	}
	c := fakeClientFor(t, m, "ws")
	c.mu.Lock()
	defer c.mu.Unlock()
	if len(c.prompts) != 0 {
		t.Fatalf("submitted prompts = %v, want nothing submitted for a conversation already compacted", c.prompts)
	}
}

// A CONVERSATION THAT WAS CLEARED IS NOT COMPACTED EITHER. A `/clear` discards
// the conversation, so a compaction submitted after one reads whatever the
// clear left behind to summarize material nobody has added to yet.
func TestSubmitWarmCompactionDeclinesAClearedConversation(t *testing.T) {
	// Arrange.
	m, capture := warmCompactRig(t, keepalive.WarmCompactMinContextTokens*10)
	applierFor(t, m).setCompactionGate("ws", ssm.CompactionGate{ClearedAtMs: 200, PromptAtMs: 100})

	// Act.
	_, err := m.SubmitWarmCompaction(context.Background(), "ws", warmCompactAnchor)

	// Assert.
	if !errors.Is(err, ErrWarmCompactNotEligible) {
		t.Fatalf("SubmitWarmCompaction against a cleared conversation = %v, want ErrWarmCompactNotEligible", err)
	}
	if !capture.contains("reason=already_cleared:last_compacted_at_ms=0 last_cleared_at_ms=200 last_prompt_at_ms=100") {
		t.Fatal("no record named the CLEAR as the reason; reporting it as a compaction sends a reader looking for one that never happened")
	}
	c := fakeClientFor(t, m, "ws")
	c.mu.Lock()
	defer c.mu.Unlock()
	if len(c.prompts) != 0 {
		t.Fatalf("submitted prompts = %v, want nothing submitted for a cleared conversation", c.prompts)
	}
}

// A PROMPT SINCE THE CLEAR IS NEW MATERIAL, exactly as one since a compaction
// is: the conversation has something in it again.
func TestSubmitWarmCompactionProceedsWhenAPromptFollowedTheClear(t *testing.T) {
	// Arrange.
	m, _ := warmCompactRig(t, keepalive.WarmCompactMinContextTokens*10)
	applierFor(t, m).setCompactionGate("ws", ssm.CompactionGate{ClearedAtMs: 200, PromptAtMs: 300})

	// Act.
	turnID, err := m.SubmitWarmCompaction(context.Background(), "ws", warmCompactAnchor)

	// Assert.
	if err != nil {
		t.Fatalf("SubmitWarmCompaction after a prompt following the clear = %v, want a submitted compaction", err)
	}
	if turnID == "" {
		t.Fatal("SubmitWarmCompaction returned no turn id")
	}
}

// A PROMPT SINCE THE COMPACTION IS NEW MATERIAL, and the conversation is
// compactable again. Without this the gate would close permanently on a
// session's first compaction.
func TestSubmitWarmCompactionProceedsWhenAPromptFollowedTheCompaction(t *testing.T) {
	// Arrange.
	m, _ := warmCompactRig(t, keepalive.WarmCompactMinContextTokens*10)
	applierFor(t, m).setCompactionGate("ws", ssm.CompactionGate{CompactedAtMs: 200, PromptAtMs: 300})

	// Act.
	turnID, err := m.SubmitWarmCompaction(context.Background(), "ws", warmCompactAnchor)

	// Assert.
	if err != nil {
		t.Fatalf("SubmitWarmCompaction after a prompt = %v, want a submitted compaction", err)
	}
	if turnID == "" {
		t.Fatal("SubmitWarmCompaction returned no turn id")
	}
}

// AN UNREADABLE GATE DECLINES, LOUDLY. The daemon cannot tell whether it is
// about to compact the same conversation twice, and guessing in its own favor
// is exactly how the duplicate gets submitted anyway.
func TestSubmitWarmCompactionDeclinesOnAnUnreadableGate(t *testing.T) {
	// Arrange.
	m, capture := warmCompactRig(t, keepalive.WarmCompactMinContextTokens*10)
	applier := applierFor(t, m)
	applier.reconcMutex.Lock()
	applier.compactionGateErr = errors.New("the state store is gone")
	applier.reconcMutex.Unlock()

	// Act.
	_, err := m.SubmitWarmCompaction(context.Background(), "ws", warmCompactAnchor)

	// Assert.
	if !errors.Is(err, ErrWarmCompactNotEligible) {
		t.Fatalf("SubmitWarmCompaction on an unreadable gate = %v, want ErrWarmCompactNotEligible", err)
	}
	if !strings.Contains(err.Error(), "the state store is gone") {
		t.Fatalf("err = %v, want the read failure carried through rather than swallowed", err)
	}
	if !capture.contains("warm compaction DECLINED ON AN UNREADABLE GATE") {
		t.Fatal("the unreadable gate was not reported as the fault it is")
	}
}
