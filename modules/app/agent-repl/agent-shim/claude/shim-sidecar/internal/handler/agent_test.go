package handler

// agent_test.go — the subagent transcript handler, and in particular the
// terminal it spells for a BACKGROUNDED subagent the reader stopped seeing.
//
// A backgrounded agent is a DETACHED RUN delivered through an `a*` task spool.
// When that spool vanishes or goes quiet the reader concludes LOST, and the unit
// left open downstream is the SPAWNING CALL's — `activity:<tool_use_id>`, a line
// in the PARENT's book. Until this handler could spell one, the seam wrote
// `lost-terminal-unsupported` and the spawn drew as still running forever.

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// agentSpoolContext is the attribution an `a*` spool is read under: its book is
// the SPAWNING CALL (never the spool's own task id, which is a locator), and the
// run activity id is that same call.
func agentSpoolContext(path, task, run, owner string) *Context {
	return &Context{
		Path: path, MainAgentID: owner, AgentID: run,
		TaskID: task, RunActivityID: run, Kind: tail.KindAgentTranscript,
		FileID: testFileID(path), SpawnBackgrounded: true,
	}
}

// lostArmOf spells a DetachedLost's arm in the reader's own vocabulary, which is
// the same word the log's `reason` key carries.
func lostArmOf(lost *conversationv1.DetachedLost) string {
	switch {
	case lost.GetFileVanished() != nil:
		return "file_vanished"
	case lost.GetWentSilent() != nil:
		return "went_silent"
	case lost.GetSweptUp() != nil:
		return "swept_up"
	default:
		return "unset"
	}
}

// TestALostSubagentSettlesItsSpawnUnitNamingTheArm pins the whole point: each
// reason the reader can conclude with reaches the wire as its OWN
// AgentSubagentFailure.cause.lost arm, on the spawn unit's key.
func TestALostSubagentSettlesItsSpawnUnitNamingTheArm(t *testing.T) {
	tests := []struct {
		name   string
		reason convert.LostReason
		want   string
	}{
		{name: "the spool disappeared under the reader", reason: convert.LostFileVanished, want: "file_vanished"},
		{name: "the spool stopped growing past its window", reason: convert.LostWentSilent, want: "went_silent"},
		{name: "the spool predates the machine's boot", reason: convert.LostSweptUp, want: "swept_up"},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange: the handler has read a batch of the subagent's spool, so
			// its terminal is stated at real file coordinates.
			h := NewAgentTranscriptHandler(testLogger(t))
			h.Handle(nil, agentSpoolContext("/private/tmp/a1.output", "a15b5267244c1360e", "toolu_spawn", "owner-agent"))

			// Act.
			entries := h.LostTerminal("a15b5267244c1360e", "toolu_spawn", "owner-agent", string(test.reason), false)

			// Assert.
			if len(entries) != 1 {
				t.Fatalf("entries = %d, want exactly the spawn unit's settle", len(entries))
			}
			failure := activityOf(entries[0]).GetSubagent().GetFailure()
			if failure == nil {
				t.Fatalf("a LOST subagent must settle on the failure arm; got %v", activityOf(entries[0]).GetSubagent().GetResult())
			}
			if got := lostArmOf(failure.GetLost()); got != test.want {
				t.Fatalf("the LOST settle names the arm %q, wanted %q", got, test.want)
			}
		})
	}
}

// TestALostSubagentIsNeverBlamedOnAPerson pins the negative the LOST policy
// rests on: `stopped_by_user` names a DECISION, and the reader observed none —
// it only stopped seeing the file.
func TestALostSubagentIsNeverBlamedOnAPerson(t *testing.T) {
	// Arrange.
	h := NewAgentTranscriptHandler(testLogger(t))
	h.Handle(nil, agentSpoolContext("/private/tmp/a2.output", "a2task", "toolu_spawn2", "owner-agent"))

	// Act.
	entries := h.LostTerminal("a2task", "toolu_spawn2", "owner-agent", string(convert.LostWentSilent), false)

	// Assert.
	if activityOf(entries[0]).GetSubagent().GetFailure().GetStoppedByUser() != nil {
		t.Fatal("a LOST subagent's settle blames a person; we only stopped seeing its transcript")
	}
}

// TestALostSubagentSettleIsKeyedByTheSpawningCall pins the key: the unit left
// open is the CALL, and keying the settle on the vendor task id would name a row
// no reader of the conversation could join to it.
func TestALostSubagentSettleIsKeyedByTheSpawningCall(t *testing.T) {
	// Arrange.
	h := NewAgentTranscriptHandler(testLogger(t))
	h.Handle(nil, agentSpoolContext("/private/tmp/a3.output", "a3task", "toolu_spawn3", "owner-agent"))

	// Act.
	entries := h.LostTerminal("a3task", "toolu_spawn3", "owner-agent", string(convert.LostSweptUp), false)

	// Assert.
	if got := entries[0].GetUpsertKey(); got != convert.ActivityKey("toolu_spawn3") {
		t.Fatalf("upsert_key = %q, want the spawning call's activity key", got)
	}
}

// TestALostSubagentSettleIsALineInTheParentsBook pins the book: the subagent's
// own constituents are its own book, but the SPAWN is a line in the caller's.
func TestALostSubagentSettleIsALineInTheParentsBook(t *testing.T) {
	// Arrange.
	h := NewAgentTranscriptHandler(testLogger(t))
	h.Handle(nil, agentSpoolContext("/private/tmp/a4.output", "a4task", "toolu_spawn4", "owner-agent"))

	// Act.
	entries := h.LostTerminal("a4task", "toolu_spawn4", "owner-agent", string(convert.LostWentSilent), false)

	// Assert.
	if got := pageLine(entries[0]).GetPageAgentId().GetValue(); got != "owner-agent" {
		t.Fatalf("page_agent_id = %q, want the spawning agent's book", got)
	}
}

// TestTwoLostSubagentsMintDistinctWriteIdentitiesWithoutAFilePosition pins R-S1
// for a spool this handler never read: without the run-scoped write identity
// both terminals would digest one id and the store would swallow the second as a
// replay, leaving one spawn open forever.
func TestTwoLostSubagentsMintDistinctWriteIdentitiesWithoutAFilePosition(t *testing.T) {
	// Arrange: neither handler has read a byte.
	first := NewAgentTranscriptHandler(testLogger(t))
	second := NewAgentTranscriptHandler(testLogger(t))

	// Act.
	a := first.LostTerminal("a5task", "toolu_spawn5", "owner-agent", string(convert.LostSweptUp), false)
	b := second.LostTerminal("a6task", "toolu_spawn6", "owner-agent", string(convert.LostSweptUp), false)

	// Assert.
	if a[0].GetWriteId() == b[0].GetWriteId() {
		t.Fatalf("two unread subagent spools minted the same write id %q; the second's terminal would be absorbed as a replay", a[0].GetWriteId())
	}
}

// TestALostSubagentIsRefusedWhenNothingNamesTheSpawningCall pins the refusal: a
// settle keyed on an invented identity would upsert no real row.
func TestALostSubagentIsRefusedWhenNothingNamesTheSpawningCall(t *testing.T) {
	// Arrange.
	h := NewAgentTranscriptHandler(testLogger(t))

	// Act.
	entries := h.LostTerminal("a7task", "", "owner-agent", string(convert.LostWentSilent), false)

	// Assert.
	if entries != nil {
		t.Fatalf("entries = %v, want nil: a run with no spawning call must be refused, not keyed on a guess", allKeys(entries))
	}
}

// TestALostSubagentIsRefusedWhenNothingNamesTheOwningBook pins the other
// refusal: the spawn unit is a PAGE LINE, and a page line with no book is
// residue rather than a settle anyone can read.
func TestALostSubagentIsRefusedWhenNothingNamesTheOwningBook(t *testing.T) {
	// Arrange.
	h := NewAgentTranscriptHandler(testLogger(t))

	// Act.
	entries := h.LostTerminal("a8task", "toolu_spawn8", "", string(convert.LostWentSilent), false)

	// Assert.
	if entries != nil {
		t.Fatalf("entries = %v, want nil: a spawn settle naming no book must be refused", allKeys(entries))
	}
}
