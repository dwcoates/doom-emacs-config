package session

import (
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// pingText is the daemon's keep-alive prompt. The fixtures use the real
// literal so the cut under test is decided the same way production decides it.
const pingText = "respond to this message with only a '.'. make no tool calls or any other changes"

// dropPing is the droppable-text set every test passes.
func dropPing() map[string]bool { return map[string]bool{pingText: true} }

// line renders one transcript record as a JSONL line. It takes raw fragments so
// a fixture can express the exact shapes that matter — a string content versus
// an array content — without a builder hiding the distinction under test.
func line(t *testing.T, fields map[string]any) string {
	t.Helper()
	b, err := json.Marshal(fields)
	if err != nil {
		t.Fatalf("marshal fixture record: %v", err)
	}
	return string(b)
}

// userRecord is a real typed prompt: type "user" with a plain STRING content.
func userRecord(t *testing.T, uuid, parent, text string) string {
	t.Helper()
	return line(t, map[string]any{
		"type": "user", "uuid": uuid, "parentUuid": parentValue(parent),
		"sessionId": "old-session", "message": map[string]any{"role": "user", "content": text},
	})
}

// toolResultRecord is THE TRAP: a tool result, which the CLI also writes as
// type "user", distinguished only by its content being an ARRAY.
func toolResultRecord(t *testing.T, uuid, parent string) string {
	t.Helper()
	return line(t, map[string]any{
		"type": "user", "uuid": uuid, "parentUuid": parentValue(parent),
		"sessionId": "old-session",
		"message": map[string]any{"role": "user", "content": []any{
			map[string]any{"type": "tool_result", "tool_use_id": "tu_1", "content": "ok"},
		}},
	})
}

func assistantRecord(t *testing.T, uuid, parent, text string) string {
	t.Helper()
	return line(t, map[string]any{
		"type": "assistant", "uuid": uuid, "parentUuid": parentValue(parent),
		"sessionId": "old-session",
		"message": map[string]any{"role": "assistant", "content": []any{
			map[string]any{"type": "text", "text": text},
		}},
	})
}

func parentValue(parent string) any {
	if parent == "" {
		return nil
	}
	return parent
}

// writeRewindFixture writes lines to a temp file and returns its path.
func writeRewindFixture(t *testing.T, lines ...string) string {
	t.Helper()
	dir := t.TempDir()
	path := filepath.Join(dir, "old-session.jsonl")
	if err := os.WriteFile(path, []byte(strings.Join(lines, "\n")+"\n"), 0o644); err != nil {
		t.Fatalf("write fixture transcript: %v", err)
	}
	return path
}

// loadFixture writes and parses a fixture in one step.
func loadFixture(t *testing.T, lines ...string) []Record {
	t.Helper()
	records, err := LoadTranscript(writeRewindFixture(t, lines...))
	if err != nil {
		t.Fatalf("LoadTranscript: %v", err)
	}
	return records
}

// ---------------------------------------------------------------------------
// The tool_result trap
// ---------------------------------------------------------------------------

// TOOL RESULTS ARE ALSO type:"user". A boundary detector that accepted any
// type:"user" record would find a "turn boundary" in the middle of a tool-heavy
// assistant turn and cut the conversation in half. The string-content check is
// what makes a boundary a boundary.
func TestUserTextRejectsAToolResult(t *testing.T) {
	// Arrange.
	records := loadFixture(t, toolResultRecord(t, "u2", "a1"))

	// Act.
	_, ok := records[0].UserText()

	// Assert.
	if ok {
		t.Fatal("a tool_result record read as a real user turn; the plain-string check is what prevents a mid-turn cut")
	}
}

// The same fixture as a whole: a tool-heavy real turn followed by a ping must
// cut at the REAL turn's start, retaining every tool result inside it.
func TestPlanRewindDoesNotCutInsideAToolHeavyTurn(t *testing.T) {
	// Arrange.
	records := loadFixture(t,
		userRecord(t, "u1", "", "do the thing"),
		assistantRecord(t, "a1", "u1", "calling a tool"),
		toolResultRecord(t, "tr1", "a1"),
		assistantRecord(t, "a2", "tr1", "done"),
		userRecord(t, "p1", "a2", pingText),
		assistantRecord(t, "a3", "p1", "."),
	)

	// Act.
	plan, err := PlanRewind(records, dropPing())

	// Assert.
	if err != nil {
		t.Fatalf("PlanRewind: %v", err)
	}
	if plan.KeepThrough != 3 {
		t.Fatalf("KeepThrough = %d, want 3 (the real turn's whole body, tool result included)", plan.KeepThrough)
	}
	if plan.RetainedLeafUUID != "a2" {
		t.Fatalf("RetainedLeafUUID = %q, want a2", plan.RetainedLeafUUID)
	}
}

// ---------------------------------------------------------------------------
// The cut itself
// ---------------------------------------------------------------------------

func TestPlanRewindDropsEveryTrailingPing(t *testing.T) {
	// Arrange.
	records := loadFixture(t,
		userRecord(t, "u1", "", "real work"),
		assistantRecord(t, "a1", "u1", "done"),
		userRecord(t, "p1", "a1", pingText),
		assistantRecord(t, "a2", "p1", "."),
		userRecord(t, "p2", "a2", pingText),
		assistantRecord(t, "a3", "p2", "."),
	)

	// Act.
	plan, err := PlanRewind(records, dropPing())

	// Assert.
	if err != nil {
		t.Fatalf("PlanRewind: %v", err)
	}
	if len(plan.DroppedTexts) != 2 {
		t.Fatalf("dropped %d turns, want both trailing pings", len(plan.DroppedTexts))
	}
	if plan.KeepThrough != 1 {
		t.Fatalf("KeepThrough = %d, want 1", plan.KeepThrough)
	}
}

// A TRAILING PARTIAL TURN needs no special case: an interrupted ping — a user
// record with no closing assistant response — is dropped by the same cut,
// because the cut is decided from the boundary in FRONT of it.
func TestPlanRewindDropsATrailingPartialPing(t *testing.T) {
	// Arrange.
	records := loadFixture(t,
		userRecord(t, "u1", "", "real work"),
		assistantRecord(t, "a1", "u1", "done"),
		userRecord(t, "p1", "a1", pingText),
	)

	// Act.
	plan, err := PlanRewind(records, dropPing())

	// Assert.
	if err != nil {
		t.Fatalf("PlanRewind: %v", err)
	}
	if plan.KeepThrough != 1 || plan.RetainedLeafUUID != "a1" {
		t.Fatalf("plan = %+v, want the partial ping dropped and a1 retained as the leaf", plan)
	}
}

// A transcript whose tail is real work needs no rewind, and says so by name so
// the caller can tell "nothing to do" from a refusal.
func TestPlanRewindReportsNothingToDo(t *testing.T) {
	// Arrange.
	records := loadFixture(t,
		userRecord(t, "u1", "", "real work"),
		assistantRecord(t, "a1", "u1", "done"),
	)

	// Act.
	_, err := PlanRewind(records, dropPing())

	// Assert.
	if !errors.Is(err, ErrNoRewindNeeded) {
		t.Fatalf("PlanRewind on an unpolluted tail = %v, want ErrNoRewindNeeded", err)
	}
}

// ---------------------------------------------------------------------------
// The refusals
// ---------------------------------------------------------------------------

// A SIDECHAIN INTERLEAVING THE DROPPED REGION REFUSES THE REWIND. A subagent's
// branch there means the region is not the simple trailing run of pings the cut
// assumes, and cutting anyway could orphan a branch the retained prefix still
// points into. Correctness over cleanliness: the caller submits un-rewound.
func TestPlanRewindRefusesASidechainInTheDroppedRegion(t *testing.T) {
	// Arrange.
	sidechain := line(t, map[string]any{
		"type": "assistant", "uuid": "sc1", "parentUuid": "p1", "isSidechain": true,
		"sessionId": "old-session",
		"message":   map[string]any{"role": "assistant", "content": []any{}},
	})
	records := loadFixture(t,
		userRecord(t, "u1", "", "real work"),
		assistantRecord(t, "a1", "u1", "done"),
		userRecord(t, "p1", "a1", pingText),
		sidechain,
		assistantRecord(t, "a2", "sc1", "."),
	)

	// Act.
	_, err := PlanRewind(records, dropPing())

	// Assert.
	if !errors.Is(err, ErrRewindUnsafe) {
		t.Fatalf("PlanRewind over an interleaved sidechain = %v, want ErrRewindUnsafe", err)
	}
}

// A SUMMARY record — a compaction's product — in the dropped region refuses for
// the same reason.
func TestPlanRewindRefusesASummaryInTheDroppedRegion(t *testing.T) {
	// Arrange.
	summary := line(t, map[string]any{"type": "summary", "summary": "…", "leafUuid": "a1"})
	records := loadFixture(t,
		userRecord(t, "u1", "", "real work"),
		assistantRecord(t, "a1", "u1", "done"),
		userRecord(t, "p1", "a1", pingText),
		summary,
		assistantRecord(t, "a2", "p1", "."),
	)

	// Act.
	_, err := PlanRewind(records, dropPing())

	// Assert.
	if !errors.Is(err, ErrRewindUnsafe) {
		t.Fatalf("PlanRewind over an interleaved summary = %v, want ErrRewindUnsafe", err)
	}
}

// A transcript that is ENTIRELY pings has no real turn to resume from. Cutting
// it to nothing would be a deletion, not a rewind.
func TestPlanRewindRefusesATranscriptOfNothingButPings(t *testing.T) {
	// Arrange.
	records := loadFixture(t,
		userRecord(t, "p1", "", pingText),
		assistantRecord(t, "a1", "p1", "."),
	)

	// Act.
	_, err := PlanRewind(records, dropPing())

	// Assert.
	if !errors.Is(err, ErrRewindUnsafe) {
		t.Fatalf("PlanRewind over an all-ping transcript = %v, want ErrRewindUnsafe", err)
	}
}

// A real turn with no assistant response behind it cannot be the retained tail:
// there is nothing for the next prompt to continue from.
func TestPlanRewindRefusesWhenTheRetainedTurnHasNoResponse(t *testing.T) {
	// Arrange.
	records := loadFixture(t,
		userRecord(t, "u1", "", "real work"),
		userRecord(t, "p1", "u1", pingText),
	)

	// Act.
	_, err := PlanRewind(records, dropPing())

	// Assert.
	if !errors.Is(err, ErrRewindUnsafe) {
		t.Fatalf("PlanRewind with no response to retain = %v, want ErrRewindUnsafe", err)
	}
}

// A malformed line is an ERROR, not a skipped record: the cut is decided by
// counting turn boundaries, so a silently dropped line could move the boundary
// and truncate real conversation.
func TestLoadTranscriptRefusesAMalformedLine(t *testing.T) {
	// Arrange.
	path := writeRewindFixture(t, userRecord(t, "u1", "", "real work"), "{not json")

	// Act.
	_, err := LoadTranscript(path)

	// Assert.
	if err == nil {
		t.Fatal("LoadTranscript over a malformed line = nil, want a refusal rather than a silently shortened transcript")
	}
}

// ---------------------------------------------------------------------------
// Writing the copy
// ---------------------------------------------------------------------------

// The copy carries the NEW session id on every record and repoints the tail
// last-prompt marker at the retained leaf, so the CLI's resume finds a head
// that still exists.
func TestWriteRewoundRewritesIdentityAndLeaf(t *testing.T) {
	// Arrange.
	lastPrompt := line(t, map[string]any{
		"type": "last-prompt", "sessionId": "old-session", "leafUuid": "a2",
	})
	records := loadFixture(t,
		userRecord(t, "u1", "", "real work"),
		assistantRecord(t, "a1", "u1", "done"),
		lastPrompt,
		userRecord(t, "p1", "a1", pingText),
		assistantRecord(t, "a2", "p1", "."),
	)
	plan, err := PlanRewind(records, dropPing())
	if err != nil {
		t.Fatalf("PlanRewind: %v", err)
	}
	dir := t.TempDir()

	// Act.
	dest, err := WriteRewound(records, plan, "new-session", dir)
	if err != nil {
		t.Fatalf("WriteRewound: %v", err)
	}

	// Assert.
	written, err := LoadTranscript(dest)
	if err != nil {
		t.Fatalf("LoadTranscript of the rewound copy: %v", err)
	}
	for i, rec := range written {
		if sid := rec.str("sessionId"); sid != "" && sid != "new-session" {
			t.Fatalf("record %d carries sessionId %q, want the new transcript's id", i, sid)
		}
		if rec.Type() == "last-prompt" && rec.str("leafUuid") != plan.RetainedLeafUUID {
			t.Fatalf("last-prompt leafUuid = %q, want the retained leaf %q", rec.str("leafUuid"), plan.RetainedLeafUUID)
		}
	}
}

// THE RETAINED MESSAGE CONTENT IS BYTE-IDENTICAL. It is what the prompt cache
// is keyed on, so a re-encode that perturbed it would defeat the entire point
// of rewinding rather than starting fresh.
func TestWriteRewoundPreservesMessageBytes(t *testing.T) {
	// Arrange.
	records := loadFixture(t,
		userRecord(t, "u1", "", "real work"),
		assistantRecord(t, "a1", "u1", "done"),
		userRecord(t, "p1", "a1", pingText),
	)
	plan, err := PlanRewind(records, dropPing())
	if err != nil {
		t.Fatalf("PlanRewind: %v", err)
	}
	want := string(records[1].fields["message"])

	// Act.
	dest, err := WriteRewound(records, plan, "new-session", t.TempDir())
	if err != nil {
		t.Fatalf("WriteRewound: %v", err)
	}

	// Assert.
	written, err := LoadTranscript(dest)
	if err != nil {
		t.Fatalf("LoadTranscript of the rewound copy: %v", err)
	}
	if got := string(written[1].fields["message"]); got != want {
		t.Fatalf("retained message bytes = %s, want the original %s", got, want)
	}
}

// THE SOURCE IS LEFT EXACTLY AS IT WAS. The copy is non-destructive, which is
// what makes a crash anywhere in the rewind cost nothing: the registry still
// names the old uuid and the next real prompt re-triggers the rewind.
func TestWriteRewoundLeavesTheSourceUntouched(t *testing.T) {
	// Arrange.
	path := writeRewindFixture(t,
		userRecord(t, "u1", "", "real work"),
		assistantRecord(t, "a1", "u1", "done"),
		userRecord(t, "p1", "a1", pingText),
	)
	before, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read fixture: %v", err)
	}
	records, err := LoadTranscript(path)
	if err != nil {
		t.Fatalf("LoadTranscript: %v", err)
	}
	plan, err := PlanRewind(records, dropPing())
	if err != nil {
		t.Fatalf("PlanRewind: %v", err)
	}

	// Act.
	if _, err := WriteRewound(records, plan, "new-session", filepath.Dir(path)); err != nil {
		t.Fatalf("WriteRewound: %v", err)
	}

	// Assert.
	after, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("re-read fixture: %v", err)
	}
	if string(after) != string(before) {
		t.Fatal("the source transcript was modified; the rewind's copy must be non-destructive")
	}
}

// The chain verifier is the cut's own proof, and it runs before anything is
// installed. A dangling parent means the truncation removed a record something
// retained still points at.
func TestVerifyChainRejectsADanglingParent(t *testing.T) {
	// Arrange.
	records := loadFixture(t,
		userRecord(t, "u1", "", "real work"),
		assistantRecord(t, "a1", "gone", "done"),
	)

	// Act.
	err := VerifyChain(records)

	// Assert.
	if !errors.Is(err, ErrRewindUnsafe) {
		t.Fatalf("VerifyChain over a dangling parent = %v, want ErrRewindUnsafe", err)
	}
}

// A well-formed retained prefix passes, so the verifier is a real check rather
// than one that refuses everything.
func TestVerifyChainAcceptsAWellFormedPrefix(t *testing.T) {
	// Arrange.
	records := loadFixture(t,
		userRecord(t, "u1", "", "real work"),
		assistantRecord(t, "a1", "u1", "done"),
	)

	// Act.
	err := VerifyChain(records)

	// Assert.
	if err != nil {
		t.Fatalf("VerifyChain over a well-formed prefix = %v, want nil", err)
	}
}
