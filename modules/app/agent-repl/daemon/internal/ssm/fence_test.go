package ssm

import "testing"

// TestParseFenceTellsAnAbsentFenceFromAnUnmintableOne covers the ONE fact
// SplitFence's return shape cannot carry. Both an absent fence and an
// unmintable one split to two empty identities and the daemon owes them
// opposite answers, so `ok` is the only thing that can distinguish them.
func TestParseFenceTellsAnAbsentFenceFromAnUnmintableOne(t *testing.T) {
	tests := []struct {
		name           string
		fence          string
		wantSession    string
		wantGeneration string
		wantOK         bool
	}{
		{"minted", Fence("s_1", "cg_1"), "s_1", "cg_1", true},
		{"minted_over_an_absent_generation", Fence("s_1", ""), "s_1", "", true},
		{"absent", "", "", "", false},
		{"unmintable", "no-workspace-ever-held-this", "", "", false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange + Act
			session, generation, ok := ParseFence(tc.fence)

			// Assert
			if session != tc.wantSession || generation != tc.wantGeneration || ok != tc.wantOK {
				t.Fatalf("ParseFence(%q) = (%q, %q, %v), want (%q, %q, %v)",
					tc.fence, session, generation, ok, tc.wantSession, tc.wantGeneration, tc.wantOK)
			}
		})
	}
}
