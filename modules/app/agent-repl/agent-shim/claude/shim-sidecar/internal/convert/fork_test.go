package convert

import (
	"encoding/json"
	"strings"
	"testing"
)

// forkedLine stamps `line` as one the vendor's fork copied from `parentUUID`.
func forkedLine(t *testing.T, line, parentUUID string) string {
	t.Helper()
	record := decode(t, line)
	record["forkedFrom"] = map[string]any{"sessionId": "parent-session", "messageUuid": parentUUID}
	raw, err := json.Marshal(record)
	if err != nil {
		t.Fatalf("marshal forked line: %v", err)
	}
	return string(raw)
}

func TestInheritedByFork(t *testing.T) {
	cases := []struct {
		name   string
		record map[string]any
		want   bool
	}{
		{name: "a record the fork copied", record: map[string]any{"forkedFrom": map[string]any{"sessionId": "p", "messageUuid": "u"}}, want: true},
		{name: "a record of the fork's own", record: map[string]any{"type": "user"}, want: false},
		{name: "a null stamp", record: map[string]any{"forkedFrom": nil}, want: false},
		{name: "a stamp that is not an object", record: map[string]any{"forkedFrom": "p"}, want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := inheritedByFork(tc.record)

			// Assert.
			if got != tc.want {
				t.Fatalf("inheritedByFork = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestAForkedRecordStoresNothingWhileTheForksOwnRecordIsStored(t *testing.T) {
	cases := []struct {
		name       string
		forked     bool
		wantStored bool
	}{
		{name: "a prompt the fork copied from the parent", forked: true, wantStored: false},
		{name: "the same prompt said by the fork itself", forked: false, wantStored: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c := newTestConverter(t)
			line := kaPrompt(t, "u1", "", "p1", "hello")
			if tc.forked {
				line = forkedLine(t, line, "parent-u1")
			}

			// Act.
			entries := convertLines(t, c, line)

			// Assert.
			if stored := len(entries) > 0; stored != tc.wantStored {
				t.Fatalf("stored = %v (%d entries), want %v", stored, len(entries), tc.wantStored)
			}
		})
	}
}

// TestAForkSkipIsLoggedAtDebugWithNoUpsertKey asserts the one record each skip
// leaves: DEBUG, and naming no row, because none was stored.
func TestAForkSkipIsLoggedAtDebugWithNoUpsertKey(t *testing.T) {
	// Arrange.
	c, sink := loggedConverter(t)

	// Act.
	convertLines(t, c,
		forkedLine(t, kaPrompt(t, "u1", "", "p1", "hello"), "parent-u1"),
		forkedLine(t, kaReply(t, "a1", "u1", "msg_1", "hi"), "parent-a1"))

	// Assert.
	var skips []map[string]any
	for _, raw := range strings.Split(strings.TrimSpace(sink.String()), "\n") {
		var rec map[string]any
		if json.Unmarshal([]byte(raw), &rec) != nil {
			continue
		}
		if rec["operation"] == "fork-inherited-skip" {
			skips = append(skips, rec)
		}
	}
	if len(skips) != 2 {
		t.Fatalf("fork-inherited-skip records = %d, want one per record (2); log:\n%s", len(skips), sink.String())
	}
	for _, rec := range skips {
		if rec["level"] != "debug" {
			t.Errorf("fork-inherited-skip level = %v, want debug", rec["level"])
		}
		fields, _ := rec["context"].(map[string]any)
		if _, named := fields["upsert_key"]; named {
			t.Errorf("fork-inherited-skip names upsert_key %v, but no row was stored", fields["upsert_key"])
		}
	}
}
