package db

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"google.golang.org/protobuf/proto"
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

// legacyHookLine stores a hook line as a pre-rule shim left it: a whole page
// line. It is written through the store and then rewritten in place, because
// the store no longer writes one.
func legacyHookLine(t *testing.T, d *DB, hookID string) {
	t.Helper()
	entry := hookEntry("w-"+hookID, hookID, succeededHook())
	writeOK(t, d, entry)
	frame, err := proto.Marshal(entry)
	if err != nil {
		t.Fatalf("marshal: %v", err)
	}
	if _, err := d.sql.Exec(`UPDATE entry SET kind = ?, frame = ? WHERE upsert_key = ?`, kindPageLine, frame, "activity:"+hookID); err != nil {
		t.Fatalf("rewriting the row as a pre-rule page line: %v", err)
	}
}

func sweepHooks(t *testing.T, d *DB) HookSweepResult {
	t.Helper()
	result, err := d.SweepHookLines(ctx())
	if err != nil {
		t.Fatalf("SweepHookLines: %v", err)
	}
	return result
}

// ---- a hook line is delivered and not kept ----

func TestAHookLineIsPublishedToItsBook(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	result := writeOK(t, d, hookEntry("w1", "h1", failedHook()))

	// Assert
	if len(result.Lines) != 1 || result.Lines[0].Line.GetLine().GetAgentItem().GetAgentFrame().GetUpdate().GetActivity().GetHook() == nil {
		t.Fatalf("published lines = %+v, want the hook line", result.Lines)
	}
}

func TestAHookLineIsStoredAsAHookDroppedRow(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	writeOK(t, d, hookEntry("w1", "h1", failedHook()))

	// Assert
	if got := kindOf(t, d, "activity:h1"); got != kindHookDropped {
		t.Fatalf("kind = %q, want %q", got, kindHookDropped)
	}
}

func TestAHookRowKeepsNoneOfTheRecord(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	writeOK(t, d, hookEntry("w1", "h1", failedHook()))

	// Assert
	if stored := storedEntry(t, d, "activity:h1"); stored.GetAgentUpdate() != nil {
		t.Fatalf("stored agent_update = %v, want none", stored.GetAgentUpdate())
	}
}

func TestAHookRowKeepsItsTurnStamp(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	writeOK(t, d, hookEntry("w1", "h1", failedHook()))

	// Assert
	if got := storedEntry(t, d, "activity:h1").GetTurn().GetValue(); got != "turn-1" {
		t.Fatalf("stored turn = %q, want turn-1", got)
	}
}

func TestNoPageServesAHookLine(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, hookEntry("w1", "h1", failedHook()))

	// Act
	opened, err := d.OpenPage(ctx(), "agent-1", Repaint())

	// Assert
	if err != nil {
		t.Fatalf("OpenPage: %v", err)
	}
	if n := len(opened.Page.GetLines()); n != 0 {
		t.Fatalf("page lines = %d, want 0", n)
	}
}

func TestAReplayNeverServesAHookLine(t *testing.T) {
	// Arrange: a watch pinned before the write replays what was written since.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w0", "prompt:u1", "agent-1", promptItem("agent-1")))
	opened, err := d.OpenPage(ctx(), "agent-1", Repaint())
	if err != nil {
		t.Fatalf("OpenPage: %v", err)
	}
	writeOK(t, d, hookEntry("w1", "h1", failedHook()))

	// Act
	lines, err := d.LinesSince(ctx(), "agent-1", opened.PinSeq)

	// Assert
	if err != nil {
		t.Fatalf("LinesSince: %v", err)
	}
	if len(lines) != 0 {
		t.Fatalf("replay = %+v, want nothing", lines)
	}
}

func TestAHookLinesPointerStaysAValidKnownThrough(t *testing.T) {
	// Arrange: a live reader's mark is the hook line it was handed.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w0", "prompt:u1", "agent-1", promptItem("agent-1")))
	result := writeOK(t, d, hookEntry("w1", "h1", failedHook()))
	pointer := result.Lines[0].Line.GetAt()

	// Act
	_, err := d.OpenPage(ctx(), "agent-1", CatchUp(pointer))

	// Assert
	if err != nil {
		t.Fatalf("re-open from the hook line's pointer: %v, want an ordinary page", err)
	}
}

func TestAHookOutcomeSupersedesItsStartInOneRow(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	start := &conversationv1.AgentHook{Result: &conversationv1.AgentHook_Start{Start: &conversationv1.AgentHookStart{HookName: "SessionStart:startup"}}}

	// Act
	writeOK(t, d, hookEntry("w1", "h1", start), hookEntry("w2", "h1", failedHook()))

	// Assert
	if n := scalar[int](t, d, `SELECT COUNT(*) FROM entry WHERE upsert_key = ?`, "activity:h1"); n != 1 {
		t.Fatalf("rows under the key = %d, want 1", n)
	}
}

func TestAHookLineSupersedesAPreRuleHookPageLine(t *testing.T) {
	// Arrange: a pre-rule shim stored the start; the outcome lands after.
	d, _ := newStore(t)
	legacyHookLine(t, d, "h1")

	// Act
	_, err := d.WriteBatch(ctx(), "test-producer", WriteInteractive, batch(hookEntry("w2", "h1", failedHook())), nil)

	// Assert
	if err != nil {
		t.Fatalf("WriteBatch: %v, want the hook line to supersede the stored one", err)
	}
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
