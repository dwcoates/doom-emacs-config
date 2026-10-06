package db

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

// ---- hook fixtures ----

// failedHook is a hook firing that failed: the outcome a card is drawn for.
func failedHook() *conversationv1.AgentHook {
	return &conversationv1.AgentHook{Result: &conversationv1.AgentHook_NonBlockingError{
		NonBlockingError: &conversationv1.AgentHookNonBlockingError{Command: "SessionStart:startup", ExitCode: 1},
	}}
}

// succeededHook is a hook firing that succeeded: it draws nothing.
func succeededHook() *conversationv1.AgentHook {
	return &conversationv1.AgentHook{Result: &conversationv1.AgentHook_Succeeded{
		Succeeded: &conversationv1.AgentHookSucceeded{Command: "SessionStart:resume"},
	}}
}

// hookEntry is a stream-plane hook line in agent-1's book.
func hookEntry(writeID, hookID string, hook *conversationv1.AgentHook) *storev1.StoreEntry {
	return stampedTurn(pageEntry(writeID, "activity:"+hookID, "agent-1", frameItem(activityFrame("agent-1", hookID, hook))), "turn-1")
}

// legacyHookLine stores a succeeded hook line as a pre-rule shim wrote it.
func legacyHookLine(t *testing.T, d *DB, hookID string) {
	t.Helper()
	writeOK(t, d, hookEntry("w-"+hookID, hookID, succeededHook()))
}

// startedHook is a hook's start: it draws nothing.
func startedHook() *conversationv1.AgentHook {
	return &conversationv1.AgentHook{Result: &conversationv1.AgentHook_Start{Start: &conversationv1.AgentHookStart{HookName: "SessionStart:startup"}}}
}

func sweepHooks(t *testing.T, d *DB) HookSweepResult {
	t.Helper()
	result, err := d.SweepHookLines(ctx())
	if err != nil {
		t.Fatalf("SweepHookLines: %v", err)
	}
	return result
}

// ---- the sweep of hook records stored before the rule ----

func TestTheHookSweepDropsAStoredHookPageLine(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	legacyHookLine(t, d, "h1")

	// Act
	sweepHooks(t, d)

	// Assert
	if got := kindOf(t, d, "activity:h1"); got != kindHookDropped {
		t.Fatalf("kind = %q, want %q", got, kindHookDropped)
	}
}

func TestTheHookSweepKeepsNoneOfTheRecord(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	legacyHookLine(t, d, "h1")

	// Act
	sweepHooks(t, d)

	// Assert
	if stored := storedEntry(t, d, "activity:h1"); stored.GetAgentUpdate() != nil {
		t.Fatalf("stored agent_update = %v, want none", stored.GetAgentUpdate())
	}
}

func TestTheHookSweepLeavesAnotherActivity(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "activity:r1", "agent-1", frameItem(activityFrame("agent-1", "r1", prose()))))

	// Act
	sweepHooks(t, d)

	// Assert
	if got := kindOf(t, d, "activity:r1"); got != kindPageLine {
		t.Fatalf("kind = %q, want %q", got, kindPageLine)
	}
}

func TestTheHookSweepBumpsNoWriteSeq(t *testing.T) {
	// Arrange: a dropped row drew nothing, so no watcher is told.
	d, _ := newStore(t)
	legacyHookLine(t, d, "h1")
	before := scalar[int64](t, d, `SELECT write_seq FROM entry WHERE upsert_key = ?`, "activity:h1")

	// Act
	sweepHooks(t, d)

	// Assert
	if after := scalar[int64](t, d, `SELECT write_seq FROM entry WHERE upsert_key = ?`, "activity:h1"); after != before {
		t.Fatalf("write_seq = %d, want %d", after, before)
	}
}

func TestTheHookSweepTakesTheRowOffItsPage(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	legacyHookLine(t, d, "h1")

	// Act
	sweepHooks(t, d)

	// Assert
	opened, err := d.OpenPage(ctx(), "agent-1", Repaint())
	if err != nil {
		t.Fatalf("OpenPage: %v", err)
	}
	if n := len(opened.Page.GetLines()); n != 0 {
		t.Fatalf("page lines = %d, want 0", n)
	}
}

func TestTheHookSweepFindsNothingTheSecondTime(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	legacyHookLine(t, d, "h1")
	sweepHooks(t, d)

	// Act
	result := sweepHooks(t, d)

	// Assert
	if result.Dropped != 0 {
		t.Fatalf("dropped = %d, want 0", result.Dropped)
	}
}

func TestTheHookSweepReadsPastAFullBatch(t *testing.T) {
	// Arrange: one more stream activity than a batch holds, the hook last.
	d, _ := newStore(t)
	for i := 0; i < hookSweepBatch; i++ {
		id := "r" + string(rune('a'+i%26)) + string(rune('a'+i/26%26)) + string(rune('a'+i/676))
		writeOK(t, d, pageEntry("w-"+id, "activity:"+id, "agent-1", frameItem(activityFrame("agent-1", id, prose()))))
	}
	legacyHookLine(t, d, "h1")

	// Act
	result := sweepHooks(t, d)

	// Assert
	if result.Dropped != 1 {
		t.Fatalf("dropped = %d, want 1", result.Dropped)
	}
}

func TestTheHookSweepLogsWhatItDroppedAtInfo(t *testing.T) {
	// Arrange
	d, s := newStore(t)
	legacyHookLine(t, d, "h1")

	// Act
	sweepHooks(t, d)

	// Assert
	s.assertLogged(t, "info", "hook sweep dropped 1 stored hook records")
}

func TestTheHookSweepDropsAStartWhoseOutcomeNeverCame(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, hookEntry("w1", "h1", startedHook()))

	// Act
	sweepHooks(t, d)

	// Assert
	if got := kindOf(t, d, "activity:h1"); got != kindHookDropped {
		t.Fatalf("kind = %q, want %q", got, kindHookDropped)
	}
}

func TestTheHookSweepKeepsAFailedHooksCard(t *testing.T) {
	// Arrange: a failed firing draws a card, and stays a page line until the
	// live-only carrier is settled.
	d, _ := newStore(t)
	writeOK(t, d, hookEntry("w1", "h1", failedHook()))

	// Act
	sweepHooks(t, d)

	// Assert
	if got := kindOf(t, d, "activity:h1"); got != kindPageLine {
		t.Fatalf("kind = %q, want %q", got, kindPageLine)
	}
}

func TestTheHookSweepKeepsADroppedRowsTurnStamp(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	legacyHookLine(t, d, "h1")

	// Act
	sweepHooks(t, d)

	// Assert
	if got := storedEntry(t, d, "activity:h1").GetTurn().GetValue(); got != "turn-1" {
		t.Fatalf("stored turn = %q, want turn-1", got)
	}
}

func TestADroppedHookRowsPointerStaysAValidKnownThrough(t *testing.T) {
	// Arrange: a reader's mark is the hook line it was served before the sweep.
	d, _ := newStore(t)
	legacyHookLine(t, d, "h1")
	opened, err := d.OpenPage(ctx(), "agent-1", Repaint())
	if err != nil {
		t.Fatalf("OpenPage: %v", err)
	}
	pointer := opened.Page.GetLines()[0].GetAt()
	sweepHooks(t, d)

	// Act
	_, err = d.OpenPage(ctx(), "agent-1", CatchUp(pointer))

	// Assert
	if err != nil {
		t.Fatalf("re-open from the dropped row's pointer: %v, want an ordinary page", err)
	}
}

func TestAFailedOutcomeTakesBackItsDroppedStart(t *testing.T) {
	// Arrange: a pre-rule shim wrote the start, the sweep dropped it, and the
	// firing failed after.
	d, _ := newStore(t)
	writeOK(t, d, hookEntry("w1", "h1", startedHook()))
	sweepHooks(t, d)

	// Act
	writeOK(t, d, hookEntry("w2", "h1", failedHook()))

	// Assert
	if got := kindOf(t, d, "activity:h1"); got != kindPageLine {
		t.Fatalf("kind = %q, want the failed card's page line", got)
	}
}

func TestASucceededOutcomeLeavesItsDroppedStartDropped(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, hookEntry("w1", "h1", startedHook()))
	sweepHooks(t, d)

	// Act
	result := writeOK(t, d, hookEntry("w2", "h1", succeededHook()))

	// Assert
	if got := kindOf(t, d, "activity:h1"); got != kindHookDropped || result.Absorbed != 1 {
		t.Fatalf("kind = %q absorbed = %d, want %q and the write absorbed", got, result.Absorbed, kindHookDropped)
	}
}

func TestALaterSweepReadsOnlyWhatWasWrittenSince(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	legacyHookLine(t, d, "h1")
	sweepHooks(t, d)
	legacyHookLine(t, d, "h2")

	// Act
	result := sweepHooks(t, d)

	// Assert
	if result.Scanned != 1 || result.Dropped != 1 {
		t.Fatalf("scanned = %d dropped = %d, want only the new row, dropped", result.Scanned, result.Dropped)
	}
}
