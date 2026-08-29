package convert

// entry_test.go — the envelope duties: the deterministic write identity, the
// plane, and the arms that decide pageability.

import "testing"

func TestWriteIdIsTheRuledDigestOfItsSourceCoordinates(t *testing.T) {
	// Arrange. THE RECIPE IS THE RULING: sha256 of
	// "producer|path|offset|discriminator", hex. Asserting the recipe rather than
	// a recorded digest is what keeps the shim able to reproduce it.
	at := Attribution{Path: "/p/s.jsonl", Offset: 128}

	// Act.
	got := WriteID(at, "block:0")

	// Assert: recomputing the documented input must reproduce it exactly.
	want := sha256Hex(Producer + "|/p/s.jsonl|128|block:0")
	if got != want {
		t.Fatalf("write_id = %q, want the digest of %q", got, Producer+"|/p/s.jsonl|128|block:0")
	}
}

func TestWriteIdIsStableAcrossTwoRunsOfTheSameBytes(t *testing.T) {
	// Arrange. Replay idempotence at the store rests entirely on this: the same
	// bytes re-read after a restart must mint the same id, so randomness is
	// forbidden.
	at := Attribution{Path: "/p/s.jsonl", Offset: 7}

	// Act.
	first := WriteID(at, "settle:toolu_a")
	second := WriteID(at, "settle:toolu_a")

	// Assert.
	if first != second {
		t.Fatalf("write ids differ across runs: %q vs %q", first, second)
	}
}

func TestWriteIdVariesWithEveryCoordinate(t *testing.T) {
	// Arrange. Each coordinate must change the identity, or two distinct records
	// collide and the store absorbs one as a replay of the other, losing it.
	base := Attribution{Path: "/p/s.jsonl", Offset: 10}
	cases := []struct {
		name string
		at   Attribution
		disc string
	}{
		{name: "baseline", at: base, disc: "d"},
		{name: "a different path", at: Attribution{Path: "/p/other.jsonl", Offset: 10}, disc: "d"},
		{name: "a different offset", at: Attribution{Path: "/p/s.jsonl", Offset: 11}, disc: "d"},
		{name: "a different discriminator", at: base, disc: "e"},
	}

	// Act.
	seen := map[string]string{}
	for _, tc := range cases {
		id := WriteID(tc.at, tc.disc)
		if prior, clash := seen[id]; clash {
			t.Fatalf("%s collides with %s", tc.name, prior)
		}
		seen[id] = tc.name
	}

	// Assert.
	if len(seen) != len(cases) {
		t.Fatalf("distinct ids = %d, want %d", len(seen), len(cases))
	}
}

func TestEveryEntryNamesTheFilePlane(t *testing.T) {
	// Arrange. The sidecar reads what the vendor wrote to DISK; the plane is what
	// tells a reader which producer observed a record.
	at := testAttribution(0)

	// Act.
	entry := VendorSpecificEntry(at, "some/kind", map[string]any{"x": float64(1)})

	// Assert.
	if entry.GetPlane().GetFile() == nil {
		t.Fatal("every entry this producer writes must name the FILE plane")
	}
	if entry.GetPlane().GetStream() != nil {
		t.Fatal("the sidecar must never claim the stream plane, which is the shim's")
	}
}

func TestResidueIsKeyedByItsOwnWriteIdentitySoAReReadSupersedesItself(t *testing.T) {
	// Arrange. Residue has no unit identity of its own, but it still needs a key:
	// without one, re-reading a file would append a second copy of the same bytes
	// instead of superseding the first.
	at := testAttribution(64)

	// Act.
	first := UnknownEntry(at, "brand-new", "type", map[string]any{})
	second := UnknownEntry(at, "brand-new", "type", map[string]any{})

	// Assert.
	if first.GetUpsertKey() == "" {
		t.Fatal("residue must still carry an upsert key")
	}
	if first.GetUpsertKey() != second.GetUpsertKey() {
		t.Fatalf("residue keys differ across reads: %q vs %q", first.GetUpsertKey(), second.GetUpsertKey())
	}
}

func TestUnparsedResidueIsBoundedButKeepsItsEvidence(t *testing.T) {
	// Arrange. A record we could not read is EVIDENCE, not a payload: an
	// unbounded copy of a corrupt multi-megabyte line would be re-written to the
	// store on every re-read.
	at := testAttribution(0)
	huge := make([]byte, maxUnparsedRaw+4096)
	for i := range huge {
		huge[i] = 'x'
	}

	// Act.
	entry := UnparsedEntry(at, huge, errForTest("bad json"))

	// Assert.
	u := entry.GetAgentUpdate().GetUnservedItem().GetUnparsed()
	if len(u.GetRaw()) != maxUnparsedRaw {
		t.Fatalf("raw length = %d, want it bounded to %d", len(u.GetRaw()), maxUnparsedRaw)
	}
	if u.GetParseError() != "bad json" {
		t.Fatalf("parse_error = %q, want the cause preserved", u.GetParseError())
	}
	if u.GetOffset() != 0 || u.GetSource() != at.Path {
		t.Fatalf("source/offset = %q/%d, want the position it can be found again at", u.GetSource(), u.GetOffset())
	}
}

func TestTopLevelIsUnsetWhenGenuinelyUnresolvable(t *testing.T) {
	// Arrange. UNSET ONLY WHEN GENUINELY UNRESOLVABLE — residue that names no
	// agent at all. A sentinel here would be indistinguishable from a real book.
	at := Attribution{Path: "/p/s.jsonl", Offset: 0}

	// Act.
	entry := UnknownEntry(at, "x", "type", map[string]any{})

	// Assert.
	if entry.GetAgentUpdate().TopLevel != nil {
		t.Fatal("a record naming no agent must leave top_level UNSET, never a sentinel")
	}
}

// ---- the key space ----

func TestUpsertKeySpellingsTable(t *testing.T) {
	// Arrange. The key space is the producer's whole statement of "what is one
	// thing", and the shim must mint the IDENTICAL key for the same unit — so the
	// spellings are pinned here rather than left to each call site.
	cases := []struct {
		name string
		got  string
		want string
	}{
		{name: "a tool call is its vendor id", got: ActivityKey("toolu_abc"), want: "activity:toolu_abc"},
		{name: "a content block is message id plus index", got: ActivityKey(BlockActivityID("msg_1", 2)), want: "activity:msg_1:2"},
		{name: "an ask has its own space", got: QuestionKey("toolu_ask"), want: "question:toolu_ask"},
		{name: "a terminal is per agent and record", got: TerminalKey("agent-1", "uuid-9"), want: "terminal:agent-1:uuid-9"},
		{name: "a detached run is keyed by its call", got: BashKey("toolu_run"), want: "bash:toolu_run"},
		{name: "a session fact names its arm", got: SessionKey("context_cut", "uuid-3"), want: "session:context_cut:uuid-3"},
		{name: "an api error names its arm", got: SessionKey("api_error", "uuid-4"), want: "session:api_error:uuid-4"},
	}

	// Act + Assert.
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if tc.got != tc.want {
				t.Fatalf("key = %q, want %q", tc.got, tc.want)
			}
		})
	}
}

func TestBlockIndexIsLoadBearingInTheUnitIdentity(t *testing.T) {
	// Arrange. An assistant message holding three tool calls plus prose becomes
	// four units; collapsing them onto the message id would make the last one
	// written the only one stored.
	// Act + Assert.
	if BlockActivityID("msg_1", 0) == BlockActivityID("msg_1", 1) {
		t.Fatal("two blocks of one response must have distinct unit identities")
	}
}

type errForTest string

func (e errForTest) Error() string { return string(e) }
