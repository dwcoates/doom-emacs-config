package convert

import (
	"errors"
	"io"
	"testing"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// testConverter builds a Converter whose log goes nowhere, so a suite asserting
// conversion is not also asserting log formatting.
//
// The diagnostic sink is installed rather than left nil because the logger
// refuses to emit a session-scoped record without one — a guard that exists so a
// diagnostic about a session can never be silently dropped, and one this suite
// must satisfy rather than route around.
func testConverter(t *testing.T) *Converter {
	t.Helper()
	log := logging.New(io.Discard, io.Discard).With(logging.Context{Component: "test"})
	log.SetDiagnosticSink(func(logging.Diagnostic) {})
	return New(log)
}

func testAttribution() Attribution {
	return Attribution{SessionID: "sess-1", Path: "/transcripts/sess-1.jsonl", Offset: 100, ProducedAtMs: 1700000000000}
}

// single asserts a conversion produced exactly one record and returns it.
func single(t *testing.T, entries []*storev1.StoreEntry) *storev1.StoreEntry {
	t.Helper()
	if len(entries) != 1 {
		t.Fatalf("entries = %d, want exactly 1", len(entries))
	}
	return entries[0]
}

// unservedArm names which StoreUnservedItem arm a record was stored under.
func unservedArm(entry *storev1.StoreEntry) string {
	switch entry.GetAgentUpdate().GetUnservedItem().GetUnservedItem().(type) {
	case *storev1.StoreUnservedItem_VendorSpecific:
		return "vendor_specific"
	case *storev1.StoreUnservedItem_Unknown:
		return "unknown"
	case *storev1.StoreUnservedItem_Unparsed:
		return "unparsed"
	default:
		return ""
	}
}

// ---------------------------------------------------------------------------
// the ingestion mandate
// ---------------------------------------------------------------------------

// Every JSON object on disk must end up in the store as a protobuf shape. The
// converter is where that mandate is either kept or broken, so it is asserted
// per line SHAPE rather than only for the shapes that convert cleanly.
//
// IT STILL BINDS WITH THE CONVERSION UNPORTED. A record whose conversion the
// redesign deleted is stored whole rather than dropped, so this assertion is
// exactly as load-bearing now as it was before.
func TestLineAlwaysProducesARecord(t *testing.T) {
	tests := []struct {
		name   string
		record map[string]any
	}{
		{name: "user prompt", record: map[string]any{"type": "user", "uuid": "u1", "message": map[string]any{"content": "hello"}}},
		{name: "assistant response", record: map[string]any{"type": "assistant", "uuid": "a1", "message": map[string]any{"id": "m1", "content": []any{}}}},
		{name: "system subtype", record: map[string]any{"type": "system", "uuid": "s1", "subtype": "turn_duration"}},
		{name: "attachment", record: map[string]any{"type": "attachment", "uuid": "x1", "attachment": map[string]any{"type": "date_change"}}},
		{name: "flat metadata line", record: map[string]any{"type": "ai-title", "aiTitle": "a name"}},
		{name: "unmodeled top-level type", record: map[string]any{"type": "something-new", "field": "value"}},
		{name: "no type discriminator at all", record: map[string]any{"field": "value"}},
		{name: "system with no subtype", record: map[string]any{"type": "system", "uuid": "s2"}},
		{name: "attachment with no attachment object", record: map[string]any{"type": "attachment", "uuid": "x2"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c := testConverter(t)

			// Act.
			entries := c.Line(tc.record, testAttribution(), nil)

			// Assert.
			if len(entries) == 0 {
				t.Fatal("Line produced no record; the line would never reach the store")
			}
		})
	}
}

// The three unserved arms mean three different things, and picking the wrong one
// misdirects whoever follows up: vendor_specific asks for a converter, unknown
// asks for a model, unparsed asks for an investigation.
func TestUnservedArmMatchesTheReason(t *testing.T) {
	tests := []struct {
		name   string
		record map[string]any
		want   string
	}{
		{name: "understood but not carried", record: map[string]any{"type": "ai-title"}, want: "vendor_specific"},
		{name: "parsed but not modeled", record: map[string]any{"type": "brand-new-line"}, want: "unknown"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c := testConverter(t)

			// Act.
			entry := single(t, c.Line(tc.record, testAttribution(), nil))

			// Assert.
			if got := unservedArm(entry); got != tc.want {
				t.Fatalf("unserved arm = %q, want %q", got, tc.want)
			}
		})
	}
}

// A conversion the redesign deleted must be UNMISTAKABLE in stored data, not
// silently indistinguishable from a line this reader never modeled.
func TestUnportedRecordIsDistinguishableFromAnUnmodeledOne(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	record := map[string]any{"type": "user", "uuid": "u1", "message": map[string]any{"content": "hello"}}

	// Act.
	entry := single(t, c.Line(record, testAttribution(), nil))

	// Assert.
	unknown := entry.GetAgentUpdate().GetUnservedItem().GetUnknown()
	if unknown.GetDiscriminatorField() != UnportedField {
		t.Fatalf("discriminator_field = %q, want %q so unported conversions are enumerable",
			unknown.GetDiscriminatorField(), UnportedField)
	}
}

// ---------------------------------------------------------------------------
// detached work
// ---------------------------------------------------------------------------

func TestDetachedWorkMessageIDIsDerivedFromTheTaskID(t *testing.T) {
	tests := []struct {
		name   string
		taskID string
		want   string
	}{
		{name: "agent task", taskID: "agent-1", want: "dw:agent-1"},
		{name: "workflow run", taskID: "wf_abc", want: "dw:wf_abc"},
		{name: "no task identity", taskID: "", want: ""},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act.
			got := DetachedWorkMessageID(tc.taskID)

			// Assert.
			if got != tc.want {
				t.Fatalf("DetachedWorkMessageID(%q) = %q, want %q", tc.taskID, got, tc.want)
			}
		})
	}
}

// ---------------------------------------------------------------------------
// compaction
// ---------------------------------------------------------------------------

func TestIsCompactBoundaryRecognizesOnlyTheBoundary(t *testing.T) {
	tests := []struct {
		name   string
		record map[string]any
		want   bool
	}{
		{name: "the boundary", record: map[string]any{"type": "system", "subtype": "compact_boundary"}, want: true},
		{name: "a different system subtype", record: map[string]any{"type": "system", "subtype": "turn_duration"}, want: false},
		{name: "a user line", record: map[string]any{"type": "user"}, want: false},
		{name: "the summary that follows it", record: map[string]any{"type": "user", "isCompactSummary": true}, want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act.
			got := IsCompactBoundary(tc.record)

			// Assert.
			if got != tc.want {
				t.Fatalf("IsCompactBoundary = %t, want %t", got, tc.want)
			}
		})
	}
}

// ---------------------------------------------------------------------------
// write identity
// ---------------------------------------------------------------------------

func TestWriteIdentityIsStableAcrossReconversion(t *testing.T) {
	// Arrange.
	record := map[string]any{"type": "user", "uuid": "u1", "message": map[string]any{"content": "hello"}}

	// Act: two independent converters, as a restart would produce.
	first := single(t, testConverter(t).Line(record, testAttribution(), nil))
	second := single(t, testConverter(t).Line(record, testAttribution(), nil))

	// Assert.
	if first.GetWriteId() != second.GetWriteId() {
		t.Fatal("a replayed record minted a new write identity, so the store would write it twice")
	}
	if first.GetWriteId() == "" {
		t.Fatal("the record carries no write identity, so it is not replay-idempotent")
	}
}

// Two records read at DIFFERENT positions are two records, and must not collide
// on one write identity — which would silently drop the second at the store.
func TestWriteIdentityDistinguishesPositions(t *testing.T) {
	// Arrange.
	record := map[string]any{"type": "user", "uuid": "u1", "message": map[string]any{"content": "hello"}}
	first, second := testAttribution(), testAttribution()
	second.Offset = 500

	// Act.
	a := single(t, testConverter(t).Line(record, first, nil))
	b := single(t, testConverter(t).Line(record, second, nil))

	// Assert.
	if a.GetWriteId() == b.GetWriteId() {
		t.Fatal("two records at different offsets share one write identity")
	}
}

// Several records produced from ONE line are still several records, so they must
// not collide either.
func TestWriteIdentityDistinguishesRecordsFromOneLine(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	c.toolCallOwner["call-1"] = "m1"
	c.toolCallOwner["call-2"] = "m1"
	record := map[string]any{
		"type": "user", "uuid": "u1",
		"message": map[string]any{"content": []any{
			map[string]any{"type": "tool_result", "tool_use_id": "call-1", "content": "a"},
			map[string]any{"type": "tool_result", "tool_use_id": "call-2", "content": "b"},
		}},
	}

	// Act.
	entries := c.Line(record, testAttribution(), nil)

	// Assert.
	seen := map[string]bool{}
	for _, entry := range entries {
		id := entry.GetWriteId()
		if seen[id] {
			t.Fatalf("two records from one line share write identity %q", id)
		}
		seen[id] = true
	}
}

// ---------------------------------------------------------------------------
// plane
// ---------------------------------------------------------------------------

// Every record this process writes was read from disk, so every one records the
// FILE plane.
func TestEveryRecordIsAttributedToTheFilePlane(t *testing.T) {
	// Arrange.
	c := testConverter(t)
	record := map[string]any{"type": "user", "uuid": "u1", "message": map[string]any{"content": "hi"}}

	// Act.
	entry := single(t, c.Line(record, testAttribution(), nil))

	// Assert.
	if entry.GetPlane().GetFile() == nil {
		t.Fatal("a record read from disk was not attributed to the file plane")
	}
}

// ---------------------------------------------------------------------------
// unparsed records
// ---------------------------------------------------------------------------

// A record we could not READ is a failure rather than a gap, and the fields exist
// so it is investigable rather than merely counted.
func TestUnparsedRecordCarriesItsEvidence(t *testing.T) {
	// Arrange.
	at := testAttribution()
	cause := errors.New("unexpected end of JSON input")

	// Act.
	unparsed := UnparsedEntry(at, []byte(`{"type":"user"`), cause).GetAgentUpdate().GetUnservedItem().GetUnparsed()

	// Assert.
	if unparsed.GetSource() != at.Path {
		t.Fatalf("source = %q, want %q", unparsed.GetSource(), at.Path)
	}
	if unparsed.GetOffset() != uint64(at.Offset) {
		t.Fatalf("offset = %d, want %d", unparsed.GetOffset(), at.Offset)
	}
	if unparsed.GetParseError() != cause.Error() {
		t.Fatalf("parse_error = %q, want %q", unparsed.GetParseError(), cause.Error())
	}
	if unparsed.GetRaw() != `{"type":"user"` {
		t.Fatalf("raw = %q, want the bytes verbatim", unparsed.GetRaw())
	}
}

// A corrupt line is evidence, not a payload: an unbounded copy of a multi-megabyte
// one would be written to the store on every re-read.
func TestUnparsedRawIsBounded(t *testing.T) {
	// Arrange.
	raw := make([]byte, maxUnparsedRaw+4096)

	// Act.
	unparsed := UnparsedEntry(testAttribution(), raw, errors.New("boom")).GetAgentUpdate().GetUnservedItem().GetUnparsed()

	// Assert.
	if len(unparsed.GetRaw()) != maxUnparsedRaw {
		t.Fatalf("raw length = %d, want it capped at %d", len(unparsed.GetRaw()), maxUnparsedRaw)
	}
}

// ---------------------------------------------------------------------------
// journal records
// ---------------------------------------------------------------------------

// A journal record with no run identity has no card to append to, so it is
// stored unconverted rather than given an invented one.
func TestJournalRecordWithoutARunIsStoredUnconverted(t *testing.T) {
	// Arrange.
	c := testConverter(t)

	// Act.
	entry := single(t, c.JournalRecord(map[string]any{"type": "started"}, testAttribution(), ""))

	// Assert.
	if got := unservedArm(entry); got != "unknown" {
		t.Fatalf("unserved arm = %q, want %q", got, "unknown")
	}
}

// A journal record type this reader does not know is stored unconverted rather
// than rendered as a blank step.
func TestUnknownJournalRecordTypeIsStoredUnconverted(t *testing.T) {
	// Arrange.
	c := testConverter(t)

	// Act.
	entry := single(t, c.JournalRecord(map[string]any{"type": "brand-new"}, testAttribution(), "wf_1"))

	// Assert.
	unknown := entry.GetAgentUpdate().GetUnservedItem().GetUnknown()
	if unknown.GetDiscriminator() != "brand-new" {
		t.Fatalf("discriminator = %q, want %q", unknown.GetDiscriminator(), "brand-new")
	}
}
