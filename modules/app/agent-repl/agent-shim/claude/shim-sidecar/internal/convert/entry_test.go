package convert

// entry_test.go — the envelope duties: the deterministic write identity, the
// plane, and the arms that decide pageability.

import (
	"errors"
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

func TestWriteIdIsTheRuledDigestOfItsSourceCoordinates(t *testing.T) {
	// Arrange. THE RECIPE IS THE RULING (R-S1): sha256 of
	// "producer|file_id|offset|discriminator", hex. Asserting the recipe rather
	// than a recorded digest is what keeps the shim able to reproduce it.
	at := Attribution{Path: "/p/s.jsonl", FileID: "16777232:9910", Offset: 128}

	// Act.
	got := WriteID(at, "block:0")

	// Assert: recomputing the documented input must reproduce it exactly.
	want := sha256Hex(Producer + "|16777232:9910|128|block:0")
	if got != want {
		t.Fatalf("write_id = %q, want the digest of %q", got, Producer+"|16777232:9910|128|block:0")
	}
}

func TestWriteIdIsStableAcrossTwoRunsOfTheSameBytes(t *testing.T) {
	// Arrange. Replay idempotence at the store rests entirely on this: the same
	// bytes re-read after a restart must mint the same id, so randomness is
	// forbidden.
	at := Attribution{Path: "/p/s.jsonl", FileID: "16777232:9910", Offset: 7}

	// Act.
	first := WriteID(at, "settle:toolu_a")
	second := WriteID(at, "settle:toolu_a")

	// Assert.
	if first != second {
		t.Fatalf("write ids differ across runs: %q vs %q", first, second)
	}
}

func TestWriteIdIsUnchangedByARenameOfTheFile(t *testing.T) {
	// Arrange. THE RULING'S WHOLE POINT (R-S1). The store's cursor is keyed by
	// "dev:inode", so a renamed file keeps its cursor and is RESUMED from it. If
	// the write identity digested the path instead, every record replayed after
	// that rename would mint a fresh id and the store — whose absorption is
	// write_id equality — would store the entire re-read turn a second time.
	before := Attribution{Path: "/p/projects/proj/s.jsonl", FileID: "16777232:9910", Offset: 512}
	after := Attribution{Path: "/p/projects/proj/renamed.jsonl", FileID: "16777232:9910", Offset: 512}

	// Act.
	first := WriteID(before, "block:0")
	second := WriteID(after, "block:0")

	// Assert.
	if first != second {
		t.Fatalf("a rename changed the write identity (%q -> %q); the cursor survived it, so the write id must too", first, second)
	}
}

func TestWriteIdVariesWithEveryCoordinate(t *testing.T) {
	// Arrange. Each coordinate must change the identity, or two distinct records
	// collide and the store absorbs one as a replay of the other, losing it. The
	// PATH is deliberately absent from this table: it is not a coordinate of the
	// identity any more (R-S1), and the rename subject above pins that.
	base := Attribution{Path: "/p/s.jsonl", FileID: "16777232:9910", Offset: 10}
	cases := []struct {
		name string
		at   Attribution
		disc string
	}{
		{name: "baseline", at: base, disc: "d"},
		{name: "a different file id", at: Attribution{Path: "/p/s.jsonl", FileID: "16777232:9911", Offset: 10}, disc: "d"},
		{name: "a different offset", at: Attribution{Path: "/p/s.jsonl", FileID: "16777232:9910", Offset: 11}, disc: "d"},
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

func TestAWriteIdWithoutAFileIdIsRaisedAsAReaderDefect(t *testing.T) {
	// Arrange. Digesting an empty file id would collapse EVERY file's records
	// onto one identity space keyed only by offset, so two unrelated files'
	// first records would absorb each other at the store. The reader always
	// supplies the id, so its absence is a defect and is stated as one rather
	// than defaulted away.
	at := Attribution{Path: "/p/s.jsonl", Offset: 0}

	// Act + Assert.
	defer func() {
		if recover() == nil {
			t.Fatal("a write id minted without a file id must be raised, never digested as an empty string")
		}
	}()
	_ = WriteID(at, "block:0")
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
	at := Attribution{Path: "/p/s.jsonl", FileID: "16777232:9910", Offset: 0}

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
		{name: "a terminal is per agent and record", got: TerminalKey("agent-1", "uuid-9"), want: "terminal:agent-1:uuid-9"},
		{name: "a detached run's rendered tail has one key however often it is superseded", got: BashTailKey("toolu_run"), want: "bash:toolu_run:tail"},
		{name: "a detached run's terminal has one key however often it is restated", got: BashTerminalKey("toolu_run"), want: "bash:toolu_run:terminal"},
		{name: "a detached run's start row is its own key", got: BashStartKey("toolu_run"), want: "bash:toolu_run:start"},
		{name: "a peer message is keyed by the vendor record uuid", got: PeerKey("uuid-p"), want: "peer:uuid-p"},
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

// errTestParse stands in for a decoder failure.
var errTestParse = errors.New("invalid character 'n'")

// ---- a record with no FILE POSITION: the run-scoped write identity ----

func TestARecordWithNoFilePositionIsIdentifiedByItsRun(t *testing.T) {
	// Arrange. A terminal concluded from the ABSENCE of a file — a run swept up
	// at boot — has no file id and no offset to digest. Inventing offset 0 would
	// claim a byte we never saw, so the identity is scoped to the run, which is
	// unique by construction and is already what the terminal's upsert key
	// names.
	at := Attribution{WriteScope: RunScope("toolu_run_0001")}

	// Act.
	got := WriteID(at, "bash_terminal")

	// Assert: the recipe deliberately carries NO offset.
	want := sha256Hex(Producer + "|run:toolu_run_0001|bash_terminal")
	if got != want {
		t.Fatalf("write_id = %q, want the digest of %q", got, Producer+"|run:toolu_run_0001|bash_terminal")
	}
}

func TestTwoRunsWithNoFilePositionNeverShareAWriteIdentity(t *testing.T) {
	// Arrange. THIS IS THE DEFECT THE SCOPE EXISTS FOR: with neither a file id
	// nor a run scope, every inferred terminal in a process digests one id, and
	// the store — whose absorption IS write_id equality — swallows the second
	// run's terminal as a replay of the first. That run then has no terminal at
	// all and stays open in every reader downstream.
	first := Attribution{WriteScope: RunScope("toolu_run_0001")}
	second := Attribution{WriteScope: RunScope("toolu_run_0002")}

	// Act, Assert.
	if WriteID(first, "bash_terminal") == WriteID(second, "bash_terminal") {
		t.Fatal("two runs concluded without a file position share one write identity")
	}
}

func TestAFilePositionBeatsARunScope(t *testing.T) {
	// Arrange. The run scope is for records with NO position, never a fallback
	// for one that has one: a record read off a file must stay identified by
	// where it was read, so a re-read after a restart mints the same id.
	scoped := Attribution{FileID: "16777232:9910", Offset: 512, WriteScope: RunScope("toolu_run_0001")}
	plain := Attribution{FileID: "16777232:9910", Offset: 512}

	// Act, Assert.
	if WriteID(scoped, "block:0") != WriteID(plain, "block:0") {
		t.Fatal("a run scope changed the identity of a record that HAS a file position")
	}
}

func TestAWriteIdWithNeitherAFileIdNorARunScopeIsRaised(t *testing.T) {
	// Arrange, Act + Assert. Neither coordinate means no identity at all, and
	// digesting the empty string would silently collide every such record.
	defer func() {
		if recover() == nil {
			t.Fatal("a write id with no file id and no run scope must be raised, never digested")
		}
	}()
	_ = WriteID(Attribution{Path: "/p/s.jsonl"}, "bash_terminal")
}

// Describe is the log line that says WHAT was not carried, so each residue arm
// has to name itself distinctly: a reader tracing a lost record has nothing
// else to go on.
func TestDescribeNamesTheResidueArm(t *testing.T) {
	cases := []struct {
		name string
		item *storev1.StoreUnservedItem
		want string
	}{
		{
			name: "keepalive",
			item: &storev1.StoreUnservedItem{UnservedItem: &storev1.StoreUnservedItem_Keepalive{
				Keepalive: &storev1.StoreAgentItem{},
			}},
			want: "keepalive",
		},
		{
			name: "vendor specific",
			item: &storev1.StoreUnservedItem{UnservedItem: &storev1.StoreUnservedItem_VendorSpecific{
				VendorSpecific: &storev1.StoreVendorSpecific{Kind: "hook_result"},
			}},
			want: `vendor_specific kind="hook_result"`,
		},
		{
			name: "unknown",
			item: &storev1.StoreUnservedItem{UnservedItem: &storev1.StoreUnservedItem_Unknown{
				Unknown: &storev1.StoreUnknown{Discriminator: "widget", DiscriminatorField: "type"},
			}},
			want: `unknown discriminator="widget" field="type"`,
		},
		{
			name: "unparsed",
			item: &storev1.StoreUnservedItem{UnservedItem: &storev1.StoreUnservedItem_Unparsed{
				Unparsed: &storev1.StoreUnparsed{Offset: 42, ParseError: "bad json"},
			}},
			want: `unparsed offset=42 error="bad json"`,
		},
		{
			name: "an arm nobody set",
			item: &storev1.StoreUnservedItem{},
			want: "",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			entry := &storev1.StoreEntry{Entry: &storev1.StoreEntry_AgentUpdate{
				AgentUpdate: &storev1.StoreAgentUpdate{
					AgentInfo: &storev1.StoreAgentUpdate_UnservedItem{UnservedItem: tc.item},
				},
			}}

			// Act.
			got := Describe(entry)

			// Assert.
			if got != tc.want {
				t.Fatalf("Describe = %q, want %q", got, tc.want)
			}
		})
	}
}

// A CONVERSION FAILURE IS NOT A DROP: a value structpb cannot represent leaves
// the record stored, carrying the failure where the body would have been.
func TestRawStructCarriesTheFailureRatherThanLosingTheRecord(t *testing.T) {
	// Arrange: a value encoding/json can never produce, so structpb refuses it.
	raw := map[string]any{"bad": make(chan int)}

	// Act.
	got := rawStruct(raw)

	// Assert.
	if got == nil {
		t.Fatal("an unconvertible body produced no struct at all, which is the drop this branch exists to prevent")
	}
	if got.GetFields()["__raw_struct_error"].GetStringValue() == "" {
		t.Fatalf("struct = %v, want the conversion failure recorded in its place", got)
	}
}
