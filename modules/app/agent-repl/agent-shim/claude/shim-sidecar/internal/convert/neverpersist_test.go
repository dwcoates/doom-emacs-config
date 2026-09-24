package convert

// neverpersist_test.go — the residue arms the owner ruled are never persisted,
// and the classification that survives the rule.

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

// tokensReminderLine is the vendor's per-turn budget line: one bare `text`
// field, reaching no arm of the conversation vocabulary.
func tokensReminderLine(uuid, text string) string {
	return attachmentLineOf(uuid, `{"type":"total_tokens_reminder","text":"`+text+`"}`)
}

// hookSuccessLine is the transcript's copy of a hook that fired cleanly — the
// row the STREAM plane owns and serves.
func hookSuccessLine(uuid string) string {
	return attachmentLineOf(uuid, `{"type":"hook_success","hookName":"PreToolUse:Read",`+
		`"toolUseID":"toolu_gated","hookEvent":"PreToolUse","command":"/h.sh","exitCode":0}`)
}

func TestIsResidueNamesEveryArmNobodyReads(t *testing.T) {
	at := testAttribution(0)
	cases := []struct {
		name  string
		entry *storev1.StoreEntry
		want  bool
	}{
		{
			name:  "vendor_specific — understood and deliberately not carried",
			entry: VendorSpecificEntry(at, "attachment/hook_success", map[string]any{}),
			want:  true,
		},
		{
			name:  "unknown — parsed and not modelled",
			entry: UnknownEntry(at, "a_new_line_type", "type", map[string]any{}),
			want:  true,
		},
		{
			name:  "unparsed — could not be read at all",
			entry: UnparsedEntry(at, []byte("{"), errTestUnparsed),
			want:  true,
		},
		{
			// NO PRODUCER MINTS THIS ARM ANY LONGER (keepalive.go), but the arm
			// is still in the contract and rows written before stand in the store.
			name: "keepalive — a well-formed fact with no book, not residue",
			entry: &storev1.StoreEntry{Entry: &storev1.StoreEntry_AgentUpdate{AgentUpdate: &storev1.StoreAgentUpdate{
				AgentInfo: &storev1.StoreAgentUpdate_UnservedItem{UnservedItem: &storev1.StoreUnservedItem{
					UnservedItem: &storev1.StoreUnservedItem_Keepalive{Keepalive: &storev1.StoreAgentItem{}},
				}},
			}}},
			want: false,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := IsResidue(tc.entry)

			// Assert.
			if got != tc.want {
				t.Fatalf("IsResidue = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestResidueLabelNamesTheArmAndItsDiscriminator(t *testing.T) {
	at := testAttribution(0)
	cases := []struct {
		name  string
		entry *storev1.StoreEntry
		want  string
	}{
		{
			name:  "vendor_specific carries its kind",
			entry: VendorSpecificEntry(at, "attachment/hook_success", map[string]any{}),
			want:  "vendor_specific/attachment/hook_success",
		},
		{
			name:  "unknown carries the field and the discriminator it read",
			entry: UnknownEntry(at, "a_new_line_type", "type", map[string]any{}),
			want:  "unknown/type:a_new_line_type",
		},
		{
			name:  "unparsed has no discriminator to carry",
			entry: UnparsedEntry(at, []byte("{"), errTestUnparsed),
			want:  "unparsed",
		},
		{
			name:  "a typed entry is not labelled at all",
			entry: PageLine(at, "block:0", "unit:typed", at.AgentID, &conversationv1.AgentFrame{}),
			want:  "",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := ResidueLabel(tc.entry)

			// Assert.
			if got != tc.want {
				t.Fatalf("ResidueLabel = %q, want %q", got, tc.want)
			}
		})
	}
}

// THE CLASSIFICATION SURVIVES THE RULE. The converter still reads every line and
// still files it under its own arm; the reader is what withholds it from the
// store, so a converter that stopped classifying would lose the count and the
// debug record the rule leaves behind as its evidence.
func TestAWithheldKindIsStillClassifiedByTheConverter(t *testing.T) {
	cases := []struct {
		name string
		line string
		want string
	}{
		{
			name: "hook_success",
			line: hookSuccessLine("h1"),
			want: "vendor_specific/attachment/hook_success",
		},
		{
			name: "total_tokens_reminder",
			line: tokensReminderLine("r1", "<total_tokens>18000 tokens left</total_tokens>"),
			want: "vendor_specific/attachment/total_tokens_reminder",
		},
		{
			name: "a line type the vendor added yesterday",
			line: attachmentLineOf("u1", `{"type":"a_kind_the_vendor_added_yesterday","text":"whatever"}`),
			want: "vendor_specific/attachment/a_kind_the_vendor_added_yesterday",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c := newTestConverter(t)

			// Act.
			entries := convertLines(t, c, tc.line)

			// Assert.
			if len(entries) != 1 {
				t.Fatalf("entries = %d (%v), want the line classified", len(entries), allKeys(entries))
			}
			if got := ResidueLabel(entries[0]); got != tc.want {
				t.Fatalf("label = %q, want %q", got, tc.want)
			}
		})
	}
}

// errTestUnparsed is the parse failure the unparsed fixtures carry.
var errTestUnparsed = errTest("truncated object")

type errTest string

func (e errTest) Error() string { return string(e) }
