package sessioncontroller

import (
	"context"
	"errors"
	"runtime"
	"testing"

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

// COMPACT-FIRST STAYS GATED UNTIL THE COMPACTION LANDS. Keeping the durable
// record hibernated is what keeps the gate standing, so "prompts are refused
// until compaction completes" is the same mechanism that refused them before
// the revival began rather than a second gate that could disagree with it.
func TestReviveSessionCompactFirstStaysGatedUntilCompactionLands(t *testing.T) {
	// Arrange.
	m, _, hib := reviveRig(t, registry.HibernationCauseIdleCutoff)
	done := make(chan error, 1)

	// Act: run the revival, then prove the gate still stands before the
	// compaction is reported.
	go func() { done <- m.ReviveSession(context.Background(), "ws", ReviveModeCompactFirst) }()
	signal := awaitCompactionWaiter(t, m, "ws")

	// Assert.
	if detail, asleep := hib.HibernationOf("s1"); !asleep || detail.Cause == "" {
		t.Fatalf("hibernation detail = %+v while the compaction is still running, want the session STILL gated", detail)
	}
	signal()
	if err := <-done; err != nil {
		t.Fatalf("ReviveSession compact_first: %v", err)
	}
	if detail, asleep := hib.HibernationOf("s1"); asleep && detail.Cause != "" {
		t.Fatalf("hibernation detail = %+v after the compaction landed, want the gate released", detail)
	}
}

// A COMPACTION THAT NEVER COMPLETES LEAVES THE SESSION GATED. The clear is the
// last step and is reached only on the completion signal, so there is no path
// in which a failed compaction ends with an ungated session.
func TestReviveSessionCompactFirstStaysGatedWhenCompactionNeverCompletes(t *testing.T) {
	// Arrange.
	m, _, hib := reviveRig(t, registry.HibernationCauseIdleCutoff)
	ctx, cancel := context.WithCancel(context.Background())

	// Act: abandon the wait, standing in for a compaction that failed.
	done := make(chan error, 1)
	go func() { done <- m.ReviveSession(ctx, "ws", ReviveModeCompactFirst) }()
	awaitCompactionWaiter(t, m, "ws")
	cancel()
	err := <-done

	// Assert.
	if err == nil {
		t.Fatal("an abandoned compaction reported success; a compact-first revival must never limp into accepting prompts")
	}
	if detail, asleep := hib.HibernationOf("s1"); !asleep || detail.Cause == "" {
		t.Fatalf("hibernation detail = %+v after a failed compaction, want the session STILL gated", detail)
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
		m.mu.Lock()
		d, live := m.byWS[workspace]
		var signal func()
		if live && d.consumer != nil {
			signal = d.consumer.onContextCompacted
		}
		m.mu.Unlock()
		if signal != nil {
			return signal
		}
		runtime.Gosched()
	}
}
