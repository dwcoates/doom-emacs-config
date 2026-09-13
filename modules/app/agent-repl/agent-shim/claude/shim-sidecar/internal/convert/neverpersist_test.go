package convert

// neverpersist_test.go — the residue kinds the owner ruled are never persisted.

import (
	"encoding/json"
	"strings"
	"testing"
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

func TestTheNamedResidueKindsProduceNoEntry(t *testing.T) {
	cases := []struct {
		name string
		line string
	}{
		{
			// 229,013 rows and 218 MB of the measured store, unjoinable to the
			// hook row a reader is actually served.
			name: "hook_success",
			line: hookSuccessLine("h1"),
		},
		{
			// 57,376 rows and 45 MB, read by nothing.
			name: "total_tokens_reminder",
			line: tokensReminderLine("r1", "<total_tokens>18000 tokens left</total_tokens>"),
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c := newTestConverter(t)

			// Act.
			entries := convertLines(t, c, tc.line)

			// Assert.
			if len(entries) != 0 {
				t.Fatalf("entries = %d (%v), want the line classified and not stored", len(entries), allKeys(entries))
			}
		})
	}
}

func TestAnUnknownAttachmentTypeIsStillPersistedAsResidue(t *testing.T) {
	// Arrange. THE LIST IS NAMED, NEVER A PREDICATE. A residue kind nobody has
	// ruled on is exactly the one whose stored record IS the coverage, so it
	// keeps being written.
	c := newTestConverter(t)
	line := attachmentLineOf("u1", `{"type":"a_kind_the_vendor_added_yesterday","text":"whatever"}`)

	// Act.
	entries := convertLines(t, c, line)

	// Assert.
	if len(entries) != 1 {
		t.Fatalf("entries = %d, want the unruled kind still stored whole", len(entries))
	}
	if got := vendorKindOf(entries[0]); got != "attachment/a_kind_the_vendor_added_yesterday" {
		t.Fatalf("kind = %q, want the unruled kind stored as residue", got)
	}
}

func TestADroppedHookAttachmentMintsNothingInTheServedHookKeySpace(t *testing.T) {
	// Arrange. The STREAM plane owns the served hook row under
	// `activity:<hook_id>` (keys.go). The drop must leave that row alone: not by
	// writing a poorer copy of it, and not by writing anything at all.
	c := newTestConverter(t)

	// Act.
	entries := convertLines(t, c, hookSuccessLine("h1"))

	// Assert.
	for _, key := range allKeys(entries) {
		if strings.HasPrefix(key, "activity:") {
			t.Fatalf("the file plane minted %q; the stream plane's hook row is the only one", key)
		}
	}
	if len(entries) != 0 {
		t.Fatalf("entries = %d (%v), want none", len(entries), allKeys(entries))
	}
}

func TestTheDropIsStatedAtDebugPerLine(t *testing.T) {
	// Arrange. A dropped kind is the STEADY STATE, not news, so the per-line
	// record must never be INFO — but it must exist, so one record can be traced.
	c, sink := loggedConverter(t)

	// Act.
	convertLines(t, c, hookSuccessLine("h1"))

	// Assert.
	if got := levelForMessage(t, sink, "is on the never-persisted list"); got != "debug" {
		t.Fatalf("level = %q, want debug", got)
	}
}

func TestTheDropTallyCarriesCountsByKind(t *testing.T) {
	// Arrange. The summary the reader states at catch-up end is per KIND, so the
	// converter has to keep them apart rather than counting drops in one bucket.
	c := newTestConverter(t)

	// Act.
	convertLines(t, c,
		hookSuccessLine("h1"),
		hookSuccessLine("h2"),
		hookSuccessLine("h3"),
		tokensReminderLine("r1", "<total_tokens>9 tokens left</total_tokens>"),
	)

	// Assert.
	dropped := c.DroppedResidue()
	if got := dropped["attachment/hook_success"]; got != 3 {
		t.Fatalf("hook_success drops = %d, want 3", got)
	}
	if got := dropped["attachment/total_tokens_reminder"]; got != 1 {
		t.Fatalf("total_tokens_reminder drops = %d, want 1", got)
	}
}

func TestAFileThatDroppedNothingHasNoTally(t *testing.T) {
	// Arrange. The reader states one summary per file only for a file that had
	// one, exactly as EndCatchup states nothing for an operation that demoted
	// nothing.
	c := newTestConverter(t)

	// Act.
	convertLines(t, c, attachmentLineOf("u1", `{"type":"date_change","text":"today"}`))

	// Assert.
	if got := c.DroppedResidue(); got != nil {
		t.Fatalf("tally = %v, want nil for a file that dropped nothing", got)
	}
}

func TestTheTallyIsACopyTheCallerCannotMutate(t *testing.T) {
	// Arrange. The summary reads the tally while the converter keeps reading the
	// file, so a handed-out map that aliased the counter would let a reader
	// corrupt the file's own record of what it dropped.
	c := newTestConverter(t)
	convertLines(t, c, hookSuccessLine("h1"))

	// Act.
	c.DroppedResidue()["attachment/hook_success"] = 99

	// Assert.
	if got := c.DroppedResidue()["attachment/hook_success"]; got != 1 {
		t.Fatalf("tally after a caller mutated its copy = %d, want the converter's own 1", got)
	}
}

func TestTheNamedListIsExactlyTheTwoRuledKinds(t *testing.T) {
	// Arrange. Adding a kind is an owner ruling, so the list is pinned here and a
	// silent third entry fails rather than quietly deleting rows.
	want := []string{"attachment/hook_success", "attachment/total_tokens_reminder"}

	// Act.
	got := NeverPersistedResidueKinds()

	// Assert.
	if strings.Join(got, ",") != strings.Join(want, ",") {
		t.Fatalf("never-persisted kinds = %v, want %v", got, want)
	}
}

func TestADroppedLineIsDecidedOnceBecauseItsOffsetRidesTheBatch(t *testing.T) {
	// Arrange. A line that produced no entry leaves NO write_ledger row (the
	// ledger holds one row per APPLIED write), so re-deciding it is prevented by
	// the CURSOR rather than by absorption: the batch's cursor advance is the
	// bytes read, not the entries produced. This test pins the converter's half
	// of that — a dropped line still moves the offset a batch would commit — and
	// tail's TestABatchOfOnlyDroppedLinesStillAdvancesTheCursor pins the reader's.
	c := newTestConverter(t)

	// Act. One line, then the SAME record re-read as a fresh reader would after
	// a cursor that had not advanced.
	first := convertLines(t, c, hookSuccessLine("h1"))
	firstDrops := c.DroppedResidue()["attachment/hook_success"]

	// Assert. The drop is decided once per read; nothing about the record makes
	// the decision sticky, which is exactly why the cursor must carry it.
	if len(first) != 0 || firstDrops != 1 {
		t.Fatalf("entries = %d, drops = %d, want 0 and 1", len(first), firstDrops)
	}
	var record map[string]any
	if err := json.Unmarshal([]byte(hookSuccessLine("h1")), &record); err != nil {
		t.Fatalf("decode: %v", err)
	}
	if entries := c.Line(record, testAttribution(0), nil); len(entries) != 0 {
		t.Fatalf("a re-decided line produced %d entrie(s); it must still store nothing", len(entries))
	}
}
