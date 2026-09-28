package convert

// version_test.go — the rows a re-read record no longer converts to
// (RetiredKeys). The version in every write identity is pinned by
// entry_test.go's ruled-digest subjects.

import (
	"slices"
	"testing"
)

func TestRetiredKeysNamesTheKeysARecordNoLongerConvertsTo(t *testing.T) {
	tests := []struct {
		name    string
		lines   func(t *testing.T) []string
		key     func(uuid string) string
		retired bool
	}{
		{
			name:    "a task notification names the prompt the old conversion minted for it",
			lines:   func(t *testing.T) []string { return append(agentLaunchLines(), corpusLine(t, notificationFile)) },
			key:     PromptKey,
			retired: true,
		},
		{
			name:    "a typed prompt does not name the prompt it still converts to",
			lines:   func(*testing.T) []string { return []string{externalPromptLine("u-typed", "cli", "fix the build")} },
			key:     PromptKey,
			retired: false,
		},
		{
			name: "a peer message does not name the peer row it still converts to",
			lines: func(*testing.T) []string {
				return []string{peerMessageLine("u-peer", "task-1", "", "hello from a peer", false)}
			},
			key:     PeerKey,
			retired: false,
		},
		{
			name:    "a typed prompt names the peer key it never converts to",
			lines:   func(*testing.T) []string { return []string{externalPromptLine("u-typed", "cli", "fix the build")} },
			key:     PeerKey,
			retired: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: every line but the last converts first, so the last
			// record's joins are in place exactly as a re-read has them.
			lines := tt.lines(t)
			c := newTestConverter(t)
			convertLines(t, c, lines[:len(lines)-1]...)
			last := decode(t, lines[len(lines)-1])
			uuid := str(last["uuid"])
			converted := c.Line(last, testAttribution(int64(len(lines)*1000)), nil)

			// Act.
			got := RetiredKeys(last, converted)

			// Assert.
			if named := slices.Contains(got, tt.key(uuid)); named != tt.retired {
				t.Fatalf("RetiredKeys = %v; naming %q = %t, want %t", got, tt.key(uuid), named, tt.retired)
			}
		})
	}
}

func TestRetiredKeysNamesNothingForARecordWithNoUUID(t *testing.T) {
	// Arrange: a record that owns no record-minted key.
	record := decode(t, `{"type":"user","message":{"role":"user","content":"no uuid"}}`)

	// Act.
	got := RetiredKeys(record, nil)

	// Assert.
	if len(got) != 0 {
		t.Fatalf("RetiredKeys = %v, want nothing for a record with no uuid", got)
	}
}

func TestRetiredKeysNeverNamesAKeyAnotherRecordCanProduce(t *testing.T) {
	// Arrange: a compaction boundary's context cut is superseded under the
	// boundary's uuid by a later summary record, so its absence from one
	// record's conversion says nothing about whether it is stale.
	record := decode(t, `{"type":"system","subtype":"compact_boundary","uuid":"u-cut"}`)

	// Act.
	got := RetiredKeys(record, nil)

	// Assert.
	if slices.Contains(got, SessionKey("context_cut", "u-cut")) {
		t.Fatalf("RetiredKeys = %v, want the context cut never named", got)
	}
}
