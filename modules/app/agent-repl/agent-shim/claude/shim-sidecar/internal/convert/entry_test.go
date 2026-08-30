package convert

// entry_test.go — the envelope duties: the deterministic write identity, the
// plane, and the arms that decide pageability.

import (
	"errors"
	"testing"
)

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
		{name: "a detached run's delta is keyed by its call and its offset", got: BashDeltaKey("toolu_run", 512), want: "bash:toolu_run:512"},
		{name: "a detached run's terminal has one key however often it is restated", got: BashTerminalKey("toolu_run"), want: "bash:toolu_run:terminal"},
		{name: "a detached run's start row is its own key", got: BashStartKey("toolu_run"), want: "bash:toolu_run:start"},
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

func TestResidueIsKeyedByTheVendorsOwnRecordUuid(t *testing.T) {
	// Arrange. BOTH PLANES SEE THE SAME VENDOR RECORD and either may store it as
	// residue. Keyed by the vendor's uuid the two writes land on ONE row and the
	// second supersedes the first; keyed by anything plane-local (a digest of a
	// file position, say) they would stand beside each other as two copies of one
	// unconvertible line, which nothing downstream could reconcile.
	at := testAttribution(4096)
	at.RecordUUID = "91f90641-c528-4a97-aad0-6936bdadea70"

	// Act.
	entry := VendorSpecificEntry(at, "queue-operation", map[string]any{"a": "b"})

	// Assert.
	if got := entry.GetUpsertKey(); got != "residue:91f90641-c528-4a97-aad0-6936bdadea70" {
		t.Fatalf("upsert_key = %q, want the vendor's own record uuid", got)
	}
}

func TestResidueKeysAgreeAcrossTheThreeArms(t *testing.T) {
	// Arrange. vendor_specific, unknown and unparsed are three ACCOUNTS of one
	// record, not three records: whichever arm a plane chose, the row is the
	// same record's.
	at := testAttribution(4096)
	at.RecordUUID = "rec-1"

	// Act.
	vendor := VendorSpecificEntry(at, "queue-operation", map[string]any{})
	unknown := UnknownEntry(at, "weird", "type", map[string]any{})

	// Assert.
	if vendor.GetUpsertKey() != unknown.GetUpsertKey() {
		t.Fatalf("arms disagree on the record's key: %q vs %q", vendor.GetUpsertKey(), unknown.GetUpsertKey())
	}
}

func TestResidueWithNoRecordUuidIsKeyedByItsFileCoordinates(t *testing.T) {
	// Arrange. An unparsed line has no uuid — that is WHY it is unparsed — so
	// there is nothing the other plane could agree on and it keys on where it
	// lives instead.
	at := testAttribution(4096)
	at.Path = "/p/projects/proj/session-uuid.jsonl"
	at.RecordUUID = ""

	// Act.
	entry := UnparsedEntry(at, []byte("{not json"), errTestParse)

	// Assert.
	if got := entry.GetUpsertKey(); got != "residue:file:/p/projects/proj/session-uuid.jsonl:4096" {
		t.Fatalf("upsert_key = %q, want the file coordinates", got)
	}
}

func TestTheFileResidueSpaceCannotCollideWithTheUuidSpace(t *testing.T) {
	// Arrange. A path is arbitrary text and a uuid is arbitrary text; without a
	// distinguishing segment a crafted path could name another record's row.
	at := testAttribution(0)
	at.RecordUUID = ""
	at.Path = "rec-1"

	// Act.
	byPath := UnparsedEntry(at, nil, errTestParse)
	at.RecordUUID = "rec-1"
	byUUID := VendorSpecificEntry(at, "queue-operation", map[string]any{})

	// Assert.
	if byPath.GetUpsertKey() == byUUID.GetUpsertKey() {
		t.Fatalf("a path and a uuid produced the same key %q", byPath.GetUpsertKey())
	}
}

func TestAKeepAliveKeepsTheKeyOfTheItemItWouldHaveBeen(t *testing.T) {
	// Arrange. A keep-alive turn's item is a WELL-FORMED conversation fact with
	// no book — not residue — so it keeps its unit's identity and never falls
	// into the residue space.
	c := newTestConverter(t)
	at := testAttribution(0)
	at.RecordUUID = "rec-1"

	// Act.
	entry := Keepalive(at, "block:0", ActivityKey("toolu_call"), "agent-1", nil)

	// Assert.
	if got := entry.GetUpsertKey(); got != ActivityKey("toolu_call") {
		t.Fatalf("upsert_key = %q, want the unit's own key", got)
	}
	_ = c
}

// errTestParse stands in for a decoder failure.
var errTestParse = errors.New("invalid character 'n'")
