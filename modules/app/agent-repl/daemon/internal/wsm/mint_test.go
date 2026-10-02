package wsm_test

import (
	"testing"

	"claude-repld/internal/wsm"
)

func TestMintedIDsHaveTheSocketBudgetLength(t *testing.T) {
	tests := []struct {
		name string
		got  string
	}{
		{name: "workspace", got: string(wsm.NewWorkspaceID())},
		{name: "repo", got: string(wsm.NewRepoID())},
		{name: "instance", got: string(wsm.NewInstanceID())},
		{name: "lease", got: string(wsm.NewLeaseID())},
		{name: "turn", got: string(wsm.NewTurnID())},
		{name: "task", got: string(wsm.NewTaskID())},
		{name: "fault", got: string(wsm.NewFaultID())},
		{name: "news digest", got: wsm.NewNewsDigestID()},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act, Assert.
			if len(tc.got) != wsm.IDLength {
				t.Fatalf("len(%q) = %d, want %d", tc.got, len(tc.got), wsm.IDLength)
			}
		})
	}
}

func TestMintedIDsAreHex(t *testing.T) {
	// Arrange.
	const hexDigits = "0123456789abcdef"

	// Act.
	got := string(wsm.NewWorkspaceID())

	// Assert.
	for i, r := range got {
		if !containsRune(hexDigits, r) {
			t.Fatalf("NewWorkspaceID()[%d] = %q, want a lowercase hex digit", i, r)
		}
	}
}

func TestMintedIDsAreDistinct(t *testing.T) {
	// Arrange.
	seen := make(map[string]struct{}, 1000)

	// Act.
	for i := 0; i < 1000; i++ {
		seen[string(wsm.NewWorkspaceID())] = struct{}{}
	}

	// Assert.
	if len(seen) != 1000 {
		t.Fatalf("minted %d distinct ids out of 1000", len(seen))
	}
}

func containsRune(s string, r rune) bool {
	for _, c := range s {
		if c == r {
			return true
		}
	}
	return false
}
