package handler

// transcript_test.go — the session transcript handler: the hold protocol and the
// per-kind shapes a converted record must have.

import (
	"testing"

	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// boundary is a compaction boundary line; summary is the line the harness writes
// after it. Held apart so a test can deliver them in separate batches, which is
// the whole situation the hold exists for.
const boundary = `{"type":"system","subtype":"compact_boundary","uuid":"b-1","isSidechain":false,` +
	`"timestamp":"2026-07-21T20:14:05.044Z","compactMetadata":{"trigger":"manual","preTokens":435029,"postTokens":8639,"durationMs":194511}}`

const summary = `{"type":"user","uuid":"s-1","isCompactSummary":true,"isSidechain":false,` +
	`"timestamp":"2026-07-21T20:14:05.040Z","message":{"role":"user","content":"the story so far"}}`

func TestHoldDefersATrailingCompactionBoundary(t *testing.T) {
	// Arrange: a batch ENDING on a boundary, from a reader that redelivers.
	h := NewSessionTranscriptHandler(testLogger(t))
	ctx := sessionContext("/p/s.jsonl", "s")
	ctx.Redelivers = true
	frames := framesFrom(t, boundary)

	// Act.
	entries := h.Handle(frames, ctx)

	// Assert: nothing converted, and the cursor is parked before the boundary.
	if len(entries) != 0 {
		t.Fatalf("entries = %d, want 0: the boundary must be held, not converted on half its evidence", len(entries))
	}
	if ctx.HeldOffset != frames[0].Offset {
		t.Fatalf("HeldOffset = %d, want %d", ctx.HeldOffset, frames[0].Offset)
	}
	if ctx.HeldDeliveries != 1 {
		t.Fatalf("HeldDeliveries = %d, want 1", ctx.HeldDeliveries)
	}
}

func TestHoldIsNotTakenWhenTheReaderWillNotRedeliver(t *testing.T) {
	// Arrange: the same batch from a caller that has no next delivery. Holding
	// there would drop the compaction for good.
	h := NewSessionTranscriptHandler(testLogger(t))
	ctx := sessionContext("/p/s.jsonl", "s")
	ctx.Redelivers = false

	// Act.
	entries := h.Handle(framesFrom(t, boundary), ctx)

	// Assert.
	if len(entries) != 1 {
		t.Fatalf("entries = %d, want 1: a non-redelivering caller must get the summary-less conversion", len(entries))
	}
	if ctx.HeldOffset != 0 {
		t.Fatalf("HeldOffset = %d, want 0", ctx.HeldOffset)
	}
}

func TestHoldConvertsOnTheSecondDeliveryEvenWithNoSummary(t *testing.T) {
	// Arrange: held once already, and the file still says nothing after it. The
	// hold is bounded to ONE redelivery by ruling.
	h := NewSessionTranscriptHandler(testLogger(t))
	ctx := sessionContext("/p/s.jsonl", "s")
	ctx.Redelivers = true
	frames := framesFrom(t, boundary)
	ctx.HeldOffset = frames[0].Offset
	ctx.HeldDeliveries = 1

	// Act.
	entries := h.Handle(frames, ctx)

	// Assert.
	if len(entries) != 1 {
		t.Fatalf("entries = %d, want 1: a bounded hold must convert on redelivery regardless", len(entries))
	}
	if ctx.HeldOffset != 0 {
		t.Fatalf("HeldOffset = %d, want 0: the hold must be released", ctx.HeldOffset)
	}
}

func TestBoundaryAndSummaryCoalesceInFileOrder(t *testing.T) {
	// Arrange: both lines in ONE batch, in FILE order. The summary's timestamp is
	// EARLIER than the boundary's, so a timestamp-ordered assembly would pair
	// them wrongly — this asserts file order wins.
	h := NewSessionTranscriptHandler(testLogger(t))
	ctx := sessionContext("/p/s.jsonl", "s")
	ctx.Redelivers = true

	// Act.
	entries := h.Handle(framesFrom(t, boundary+"\n"+summary), ctx)

	// Assert.
	cut := entryByKey(t, entries, convert.SessionKey("context_cut", "b-1"))
	compacted := frameOf(cut).GetUpdate().GetContextCut().GetCompacted()
	if compacted == nil {
		t.Fatal("the cut did not land on the compacted arm")
	}
	if got := compacted.GetSummary().GetMarkdown(); got != "the story so far" {
		t.Fatalf("summary = %q, want the following line's prose", got)
	}
	if got := compacted.GetTokens().GetTokensBefore(); got != 435029 {
		t.Fatalf("tokens_before = %d, want 435029", got)
	}
	if got := compacted.GetTokens().GetTokensAfter(); got != 8639 {
		t.Fatalf("tokens_after = %d, want 8639", got)
	}
	if compacted.GetRequested() == nil {
		t.Fatal(`trigger "manual" must land on the requested arm, since a manual cut is something the user did`)
	}
	if got := compacted.GetDurationMs(); got != 194511 {
		t.Fatalf("duration_ms = %d, want 194511", got)
	}
}

func TestCompactionCutLandsInTheMainAgentsBook(t *testing.T) {
	// Arrange. A cut is a fact about the SESSION's context, not about whichever
	// subagent happened to be running.
	h := NewSessionTranscriptHandler(testLogger(t))
	ctx := sessionContext("/p/s.jsonl", "main-agent-uuid")

	// Act.
	entries := h.Handle(framesFrom(t, boundary+"\n"+summary), ctx)

	// Assert.
	cut := entryByKey(t, entries, convert.SessionKey("context_cut", "b-1"))
	if got := pageLine(cut).GetPageAgentId().GetValue(); got != "main-agent-uuid" {
		t.Fatalf("page_agent_id = %q, want the main agent", got)
	}
	if got := cut.GetAgentUpdate().GetTopLevel().GetValue(); got != "main-agent-uuid" {
		t.Fatalf("top_level = %q, want the main agent", got)
	}
}

func TestSummaryAloneProducesNothing(t *testing.T) {
	// Arrange: the summary line by itself. It is CONSUMED by the boundary that
	// precedes it; emitting it here would render the summary twice and attribute
	// the harness's text to the person.
	h := NewSessionTranscriptHandler(testLogger(t))

	// Act.
	entries := h.Handle(framesFrom(t, summary), sessionContext("/p/s.jsonl", "s"))

	// Assert.
	if len(entries) != 0 {
		t.Fatalf("entries = %d, want 0 for a bare compaction summary: keys=%v", len(entries), allKeys(entries))
	}
}

func TestUnparsableLineBecomesUnparsedResidueWithEvidence(t *testing.T) {
	// Arrange. A line we cannot READ is a FAILURE, not a gap, and must be
	// investigable rather than merely counted.
	h := NewSessionTranscriptHandler(testLogger(t))
	ctx := sessionContext("/p/broken.jsonl", "s")

	// Act.
	entries := h.Handle(framesFrom(t, `{"type":"user" NOT JSON`), ctx)

	// Assert.
	if len(entries) != 1 {
		t.Fatalf("entries = %d, want 1", len(entries))
	}
	u := entries[0].GetAgentUpdate().GetUnservedItem().GetUnparsed()
	if u == nil {
		t.Fatal("an undecodable line must land on the unparsed arm")
	}
	if u.GetSource() != "/p/broken.jsonl" {
		t.Fatalf("source = %q, want the file it came from", u.GetSource())
	}
	if u.GetParseError() == "" {
		t.Fatal("parse_error must say why decoding failed")
	}
	if u.GetRaw() == "" {
		t.Fatal("raw must carry the bytes so the failure is investigable")
	}
}

func TestAttributionPrefersTheReadersIdentitiesOverThePath(t *testing.T) {
	// Arrange. The reader derived the identities; re-deriving them here would let
	// the two halves of the seam disagree about whose book a record lands in.
	ctx := &Context{
		Path: "/p/projects/proj/path-derived.jsonl", SessionID: "vendor-session",
		MainAgentID: "reader-main", AgentID: "reader-agent", FileID: "dev:9",
		Kind: tail.KindAgentTranscript, SpawnBackgrounded: true,
	}

	// Act.
	at := attribute(ctx, 42)

	// Assert.
	if at.MainAgentID != "reader-main" {
		t.Fatalf("MainAgentID = %q, want the reader's value", at.MainAgentID)
	}
	if at.AgentID != "reader-agent" {
		t.Fatalf("AgentID = %q, want the reader's value", at.AgentID)
	}
	if !at.Backgrounded {
		t.Fatal("Backgrounded must be read from the reader, not inferred from a task-id prefix")
	}
	if at.FileID != "dev:9" || at.Offset != 42 {
		t.Fatalf("FileID/Offset = %q/%d, want dev:9/42", at.FileID, at.Offset)
	}
}

func TestAttributionFallsBackToThePathWhenTheReaderSuppliesNothing(t *testing.T) {
	// Arrange. Every seam field is read DEFENSIVELY: empty means not yet
	// supplied, and a book that cannot be named is a record that cannot be served.
	ctx := &Context{Path: "/p/projects/proj/sess-uuid.jsonl", Kind: tail.KindSessionTranscript}

	// Act.
	at := attribute(ctx, 0)

	// Assert.
	if at.MainAgentID != "sess-uuid" {
		t.Fatalf("MainAgentID = %q, want the transcript basename", at.MainAgentID)
	}
	if at.AgentID != "sess-uuid" {
		t.Fatalf("AgentID = %q, want the main agent for a session transcript", at.AgentID)
	}
}

func TestASubagentsBookIsTheIdentityTheReaderSupplied(t *testing.T) {
	// Arrange. `projects/<proj>/<session>/subagents/agent-<id>.jsonl`. The book is
	// the SPAWNING CALL's id, which the reader read out of the companion meta
	// file; the owning session is still derived from the path.
	ctx := &Context{
		Path:    "/p/projects/proj/sess-uuid/subagents/agent-abc123.jsonl",
		Kind:    tail.KindAgentTranscript,
		AgentID: "toolu_spawn_abc",
	}

	// Act.
	at := attribute(ctx, 0)

	// Assert.
	if at.AgentID != "toolu_spawn_abc" {
		t.Fatalf("AgentID = %q, want the identity the reader supplied", at.AgentID)
	}
	if at.MainAgentID != "sess-uuid" {
		t.Fatalf("MainAgentID = %q, want the owning session two levels up", at.MainAgentID)
	}
}

func TestASubagentsAgentTypeReachesTheAttribution(t *testing.T) {
	// Arrange. The agent's OWN type, read from its meta, is what lets the
	// converter tell an assistant record this agent PRODUCED from one it merely
	// QUOTES from a parent (a fork's inherited context).
	ctx := &Context{
		Path:      "/p/projects/proj/sess-uuid/subagents/agent-abc123.jsonl",
		Kind:      tail.KindAgentTranscript,
		AgentID:   "toolu_spawn_abc",
		AgentType: "fork",
	}

	// Act.
	at := attribute(ctx, 0)

	// Assert.
	if at.AgentType != "fork" {
		t.Fatalf("AgentType = %q, want the reader's value 'fork'", at.AgentType)
	}
}

func TestASessionTranscriptHasNoAgentType(t *testing.T) {
	// Arrange. A session transcript has no meta and so no agent type; the quoted
	// check must therefore never fire for it.
	ctx := &Context{Path: "/p/projects/proj/sess-uuid.jsonl", Kind: tail.KindSessionTranscript}

	// Act.
	at := attribute(ctx, 0)

	// Assert.
	if at.AgentType != "" {
		t.Fatalf("AgentType = %q, want empty for a session transcript", at.AgentType)
	}
}

func TestASubagentsBookIsNeverItsFileName(t *testing.T) {
	// Arrange. `agent-<id>` is a LOCATOR. Falling back to it would give one
	// agent two books — one per plane — that no consumer could reconcile, so an
	// identity-less context yields none rather than the file's name.
	ctx := &Context{
		Path: "/p/projects/proj/sess-uuid/subagents/agent-abc123.jsonl",
		Kind: tail.KindAgentTranscript,
	}

	// Act.
	at := attribute(ctx, 0)

	// Assert.
	if at.AgentID == "abc123" {
		t.Fatal("the book was named by the file name; the identity comes only from the meta file")
	}
	if at.AgentID != "" {
		t.Fatalf("AgentID = %q, want none: the reader supplied no identity", at.AgentID)
	}
}

func TestASidechainWithNoIdentityConvertsNothing(t *testing.T) {
	// Arrange. The reader HOLDS a transcript whose meta has not been read, so
	// arriving here without an identity means the hold was skipped — and
	// converting anyway would file this agent's whole book under the empty id.
	h := NewAgentTranscriptHandler(testLogger(t))
	ctx := &Context{
		Path: "/p/projects/proj/sess-uuid/subagents/agent-abc123.jsonl",
		Kind: tail.KindAgentTranscript,
	}
	lines := `{"type":"assistant","uuid":"a1","isSidechain":true,"timestamp":"2026-07-21T15:36:10.000Z","message":{"id":"m1","role":"assistant","content":[{"type":"text","text":"hello"}]}}`

	// Act.
	entries := h.Handle(framesFrom(t, lines), ctx)

	// Assert.
	if len(entries) != 0 {
		t.Fatalf("entries = %d, want 0: a book with no name must not be written", len(entries))
	}
}

// secondBoundary and secondSummary are a SECOND compaction in the same file.
// BOTH summaries' timestamps are earlier than BOTH boundaries', which is what
// makes a timestamp-ordered assembly indistinguishable from a correct one in a
// session that compacted once — and plainly wrong in one that compacted twice.
const secondBoundary = `{"type":"system","subtype":"compact_boundary","uuid":"b-2","isSidechain":false,` +
	`"timestamp":"2026-07-21T21:30:00.000Z","compactMetadata":{"trigger":"auto","preTokens":400000,"postTokens":9000,"durationMs":1000}}`

const secondSummary = `{"type":"user","uuid":"s-2","isCompactSummary":true,"isSidechain":false,` +
	`"timestamp":"2026-07-21T20:14:05.041Z","message":{"role":"user","content":"the story since then"}}`

func TestTwoCompactionsEachCoalesceWithTheirOwnFollowingSummary(t *testing.T) {
	// Arrange: b1, s1, b2, s2 in FILE order, with EVERY summary timestamped
	// before BOTH boundaries. Pairing by timestamp would hand both boundaries the
	// same earliest summary — or hand each the other's — and a session that
	// compacted once could never tell the difference. File order is the ruling.
	h := NewSessionTranscriptHandler(testLogger(t))
	ctx := sessionContext("/p/s.jsonl", "s")
	ctx.Redelivers = true

	// Act.
	entries := h.Handle(framesFrom(t, boundary+"\n"+summary+"\n"+secondBoundary+"\n"+secondSummary), ctx)

	// Assert: each cut carries the summary that FOLLOWS it in the file.
	first := frameOf(entryByKey(t, entries, convert.SessionKey("context_cut", "b-1"))).
		GetUpdate().GetContextCut().GetCompacted()
	if got := first.GetSummary().GetMarkdown(); got != "the story so far" {
		t.Errorf("the first cut carries the summary %q, wanted the line that follows IT, %q", got, "the story so far")
	}
	second := frameOf(entryByKey(t, entries, convert.SessionKey("context_cut", "b-2"))).
		GetUpdate().GetContextCut().GetCompacted()
	if got := second.GetSummary().GetMarkdown(); got != "the story since then" {
		t.Errorf("the second cut carries the summary %q, wanted the line that follows IT, %q", got, "the story since then")
	}
}
