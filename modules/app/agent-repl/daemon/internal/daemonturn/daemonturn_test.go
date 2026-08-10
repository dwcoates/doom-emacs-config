package daemonturn

import "testing"

// THE CLASSIFICATION IS THE WHOLE EXCLUSION. Everything the conversation
// curator withholds for a daemon-submitted context cut, and everything the
// store reconciliation may infer about one, is decided by this predicate — so
// each direction is pinned here rather than at the readers.
func TestIsContextCut(t *testing.T) {
	tests := []struct {
		name   string
		turnID string
		want   bool
	}{
		{name: "a warm compaction", turnID: WarmCompactPrefix + "s_1:9f0c", want: true},
		{name: "a compact-first revival", turnID: ReviveCompactPrefix + "s_1:9f0c", want: true},
		{name: "a clear-first revival", turnID: ReviveClearPrefix + "s_1:9f0c", want: true},
		{name: "a user's own turn", turnID: "req_5f2a", want: false},
		{name: "the daemon's re-drive of an interrupted turn", turnID: "resume-after-restart:/ws/turn:0", want: false},
		{name: "a keep-alive ping", turnID: "ka_9f0c", want: false},
		// An empty id is what every file-plane record carries, including the
		// `compact_boundary` and clear records the dividers are drawn from.
		// Reading absence as the daemon's would withhold the one thing a cut
		// owes the user.
		{name: "no turn id at all", turnID: "", want: false},
		// The family rides the HEAD of a minted id. An id that merely contains
		// the text was minted by something else.
		{name: "a prefix appearing mid-id", turnID: "req_about_" + WarmCompactPrefix + "s_1", want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange — the id under test is the whole input.

			// Act.
			got := IsContextCut(tt.turnID)

			// Assert.
			if got != tt.want {
				t.Errorf("IsContextCut(%q) = %v, want %v", tt.turnID, got, tt.want)
			}
		})
	}
}

// EVERY DECLARED FAMILY IS IN THE SET. A prefix added as a constant but not
// listed in contextCutPrefixes is a cut the curator would render and the
// reconciliation would never match — the exact drift this package exists to
// prevent, and invisible at its own declaration site.
func TestEveryDeclaredPrefixIsClassifiedAsAContextCut(t *testing.T) {
	// Arrange.
	declared := []string{WarmCompactPrefix, ReviveCompactPrefix, ReviveClearPrefix}

	// Act + Assert.
	for _, prefix := range declared {
		if !IsContextCut(prefix + "s_1:9f0c") {
			t.Errorf("prefix %q is declared but not classified as a context cut", prefix)
		}
	}
	if len(contextCutPrefixes) != len(declared) {
		t.Errorf("contextCutPrefixes holds %d families, want the %d declared constants",
			len(contextCutPrefixes), len(declared))
	}
}
