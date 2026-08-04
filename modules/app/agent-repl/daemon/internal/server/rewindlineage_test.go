package server

import (
	"strings"
	"testing"

	"claude-repld/internal/sessioncontroller"
)

// completeLineage is the one shape the shim accepts.
func completeLineage() sessioncontroller.RewindLineage {
	return sessioncontroller.RewindLineage{
		PreviousVendorSessionID: "old-uuid",
		RetainedLeafUUID:        "leaf-uuid",
		DroppedTurnIDs:          "ka_1,ka_2",
	}
}

// THE FROZEN ARGV CONTRACT. The shim reads exactly these three flags and emits
// SessionRewound from them; a rename here is a wire break, so the test names
// the literals rather than deriving them.
func TestShimArgvRendersTheRewindLineage(t *testing.T) {
	// Arrange.
	opts := CreateOpts{
		CWD:                "/ws",
		Resume:             "new-uuid",
		RewoundFrom:        "old-uuid",
		RewindRetainedLeaf: "leaf-uuid",
		RewindDroppedTurns: "ka_1,ka_2",
	}

	// Act.
	argv := ShimArgv("node", "shim.js", "s1", false, opts)

	// Assert.
	joined := strings.Join(argv, " ")
	for _, want := range []string{
		"--rewound-from old-uuid",
		"--rewind-retained-leaf leaf-uuid",
		"--rewind-dropped-turns ka_1,ka_2",
	} {
		if !strings.Contains(joined, want) {
			t.Fatalf("argv %q is missing the frozen lineage flag %q", joined, want)
		}
	}
}

// AN INCOMPLETE LINEAGE RENDERS NOTHING. The shim rejects an empty dropped-turn
// list, so emitting a partial set would turn an unrecorded rewind into a spawn
// that fails at startup — leaving the session with no shim at all.
func TestShimArgvOmitsAnIncompleteRewindLineage(t *testing.T) {
	tests := []struct {
		name string
		opts CreateOpts
	}{
		{
			name: "no dropped turns",
			opts: CreateOpts{RewoundFrom: "old-uuid", RewindRetainedLeaf: "leaf-uuid"},
		},
		{
			name: "no retained leaf",
			opts: CreateOpts{RewoundFrom: "old-uuid", RewindDroppedTurns: "ka_1"},
		},
		{
			name: "no predecessor",
			opts: CreateOpts{RewindRetainedLeaf: "leaf-uuid", RewindDroppedTurns: "ka_1"},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			argv := ShimArgv("node", "shim.js", "s1", false, tc.opts)

			// Assert.
			if joined := strings.Join(argv, " "); strings.Contains(joined, "--rewound-from") ||
				strings.Contains(joined, "--rewind-retained-leaf") ||
				strings.Contains(joined, "--rewind-dropped-turns") {
				t.Fatalf("argv %q carries a partial lineage; the three travel together or not at all", joined)
			}
		})
	}
}

// An ordinary spawn carries no lineage flags at all.
func TestShimArgvOmitsTheLineageWhenThereWasNoRewind(t *testing.T) {
	// Arrange.
	opts := CreateOpts{CWD: "/ws", Resume: "some-uuid"}

	// Act.
	argv := ShimArgv("node", "shim.js", "s1", false, opts)

	// Assert.
	if joined := strings.Join(argv, " "); strings.Contains(joined, "--rewound-from") {
		t.Fatalf("an ordinary spawn rendered a rewind lineage: %q", joined)
	}
}

// THE LINEAGE IS CONSUMED, NOT READ. A lineage left armed would ride the next
// unrelated respawn, telling the shim it had just been rewound when it had not
// — and the shim would emit a second SessionRewound for a rewind that never
// happened.
func TestArmRewindLineageIsConsumedExactlyOnce(t *testing.T) {
	// Arrange.
	s := &ShimSpawner{logf: func(string, ...any) {}}
	if err := s.ArmRewindLineage("s1", completeLineage()); err != nil {
		t.Fatalf("ArmRewindLineage: %v", err)
	}

	// Act.
	first, firstOK := s.takeRewindLineage("s1")
	_, secondOK := s.takeRewindLineage("s1")

	// Assert.
	if !firstOK || first.PreviousVendorSessionID != "old-uuid" {
		t.Fatalf("first take = %+v ok=%v, want the armed lineage", first, firstOK)
	}
	if secondOK {
		t.Fatal("the lineage survived its spawn; a second spawn would announce a rewind that never happened")
	}
}

// An INCOMPLETE lineage is refused at the arming site rather than stored and
// discovered at spawn time.
func TestArmRewindLineageRefusesAnIncompleteLineage(t *testing.T) {
	tests := []struct {
		name    string
		lineage sessioncontroller.RewindLineage
	}{
		{
			name:    "no dropped turn ids",
			lineage: sessioncontroller.RewindLineage{PreviousVendorSessionID: "old", RetainedLeafUUID: "leaf"},
		},
		{
			name:    "no retained leaf",
			lineage: sessioncontroller.RewindLineage{PreviousVendorSessionID: "old", DroppedTurnIDs: "ka_1"},
		},
		{
			name:    "no predecessor",
			lineage: sessioncontroller.RewindLineage{RetainedLeafUUID: "leaf", DroppedTurnIDs: "ka_1"},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			s := &ShimSpawner{logf: func(string, ...any) {}}

			// Act.
			err := s.ArmRewindLineage("s1", tc.lineage)

			// Assert.
			if err == nil {
				t.Fatal("ArmRewindLineage accepted an incomplete lineage; the shim would reject it at startup and the session would come back with no shim")
			}
			if _, armed := s.takeRewindLineage("s1"); armed {
				t.Fatal("a refused lineage was stored anyway")
			}
		})
	}
}
