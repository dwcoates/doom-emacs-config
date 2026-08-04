package server

import (
	"context"
	"errors"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/registry"
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

// ---------------------------------------------------------------------------
// The lineage is durable, and it rides the flip
// ---------------------------------------------------------------------------

// lineageRig is a registry holding one session that already named a
// conversation, plus the registrar and a spawner over the same store.
func lineageRig(t *testing.T) (*registry.Registry, *RegistryRegistrar, *ShimSpawner, *[]CreateOpts) {
	t.Helper()
	reg := registry.Open(filepath.Join(t.TempDir(), "registry.db"), func(string, ...any) {})
	if err := reg.Put(registry.Record{
		SessionID: "s1", CWD: t.TempDir(), ClaudeSessionID: "old-uuid",
	}); err != nil {
		t.Fatalf("registry Put: %v", err)
	}
	var spawned []CreateOpts
	s := &ShimSpawner{
		reg:       reg,
		logf:      func(string, ...any) {},
		handles:   map[string]ShimHandle{},
		forceFake: true,
		spawn: func(_ string, opts CreateOpts) (ShimHandle, error) {
			spawned = append(spawned, opts)
			return ShimHandle{}, nil
		},
	}
	return reg, &RegistryRegistrar{Reg: reg, Logf: func(string, ...any) {}}, s, &spawned
}

// ONE WRITE. The flip is the rewind's only destructive act and the lineage is
// the only account of what it dropped, so a record that names the new uuid must
// name the lineage too.
func TestAdoptRewoundVendorSessionIDPersistsTheLineageWithTheFlip(t *testing.T) {
	// Arrange.
	reg, registrar, _, _ := lineageRig(t)

	// Act.
	if _, _, adopted := registrar.AdoptRewoundVendorSessionID("s1", "new-uuid", completeLineage()); !adopted {
		t.Fatal("AdoptRewoundVendorSessionID refused a complete lineage")
	}

	// Assert.
	rec, ok := reg.Get("s1")
	if !ok {
		t.Fatal("the session record vanished")
	}
	if rec.ClaudeSessionID != "new-uuid" {
		t.Fatalf("claude_session_id = %q, want the rewound uuid", rec.ClaudeSessionID)
	}
	if rec.Rewind.PreviousVendorSessionID != "old-uuid" || rec.Rewind.DroppedTurnIDs != "ka_1,ka_2" {
		t.Fatalf("record lineage = %+v, want the one the flip accounted for", rec.Rewind)
	}
}

// AN INCOMPLETE LINEAGE REFUSES THE WHOLE WRITE, flip included. Adopting the
// truncated uuid while arming a partial lineage would leave the session naming
// a transcript the shim then refuses to spawn on.
func TestAdoptRewoundVendorSessionIDRefusesAnIncompleteLineage(t *testing.T) {
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
			reg, registrar, _, _ := lineageRig(t)

			// Act.
			_, _, adopted := registrar.AdoptRewoundVendorSessionID("s1", "new-uuid", tc.lineage)

			// Assert.
			if adopted {
				t.Fatal("an incomplete lineage was adopted; the shim would reject it at startup and the session would come back with no shim")
			}
			if rec, _ := reg.Get("s1"); rec.ClaudeSessionID != "old-uuid" {
				t.Fatalf("claude_session_id = %q after a refused rewind flip, want the original", rec.ClaudeSessionID)
			}
		})
	}
}

// AN ORDINARY ADOPTION LEAVES THE LINEAGE ALONE. It is not a rewind and has
// nothing to announce, so it must neither arm nor clear one.
func TestAdoptVendorSessionIDLeavesAnUnconsumedLineageStanding(t *testing.T) {
	// Arrange.
	reg, registrar, _, _ := lineageRig(t)
	if _, _, adopted := registrar.AdoptRewoundVendorSessionID("s1", "new-uuid", completeLineage()); !adopted {
		t.Fatal("AdoptRewoundVendorSessionID refused a complete lineage")
	}

	// Act.
	registrar.AdoptVendorSessionID("s1", "newer-uuid")

	// Assert.
	if rec, _ := reg.Get("s1"); !rec.Rewind.Armed() {
		t.Fatal("an ordinary adoption cleared a rewind lineage nobody had announced yet")
	}
}

// THE RECOVERY, FOR REAL. A daemon that died after the flip left the lineage on
// the record; the next bring-up of that session spawns with the rewind argv, so
// the SessionRewound is emitted late rather than never.
func TestEnsureShimSpawnsAnUnconsumedLineageFromTheRecord(t *testing.T) {
	// Arrange.
	_, registrar, spawner, spawned := lineageRig(t)
	if _, _, adopted := registrar.AdoptRewoundVendorSessionID("s1", "new-uuid", completeLineage()); !adopted {
		t.Fatal("AdoptRewoundVendorSessionID refused a complete lineage")
	}

	// Act.
	if _, err := spawner.EnsureShim(context.Background(), "s1"); err != nil {
		t.Fatalf("EnsureShim: %v", err)
	}

	// Assert.
	if len(*spawned) != 1 {
		t.Fatalf("%d spawns, want 1", len(*spawned))
	}
	got := (*spawned)[0]
	if got.RewoundFrom != "old-uuid" || got.RewindRetainedLeaf != "leaf-uuid" || got.RewindDroppedTurns != "ka_1,ka_2" {
		t.Fatalf("spawn opts = %+v, want the record's lineage replayed onto the argv", got)
	}
}

// ONE-SHOT. A lineage left standing would ride the next unrelated respawn and
// make the shim emit a second SessionRewound for a rewind that never happened.
func TestEnsureShimConsumesTheLineageExactlyOnce(t *testing.T) {
	// Arrange.
	_, registrar, spawner, spawned := lineageRig(t)
	if _, _, adopted := registrar.AdoptRewoundVendorSessionID("s1", "new-uuid", completeLineage()); !adopted {
		t.Fatal("AdoptRewoundVendorSessionID refused a complete lineage")
	}
	if _, err := spawner.EnsureShim(context.Background(), "s1"); err != nil {
		t.Fatalf("first EnsureShim: %v", err)
	}

	// Act.
	if _, err := spawner.EnsureShim(context.Background(), "s1"); err != nil {
		t.Fatalf("second EnsureShim: %v", err)
	}

	// Assert.
	if len(*spawned) != 2 {
		t.Fatalf("%d spawns, want 2", len(*spawned))
	}
	if second := (*spawned)[1]; second.RewoundFrom != "" {
		t.Fatalf("the second spawn announced rewound_from=%q; the lineage survived the spawn that consumed it", second.RewoundFrom)
	}
}

// A SPAWN THAT FAILED ANNOUNCED NOTHING, so the lineage is still owed and stays
// on the record for the next attempt.
func TestEnsureShimKeepsTheLineageWhenTheSpawnFails(t *testing.T) {
	// Arrange.
	reg, registrar, spawner, _ := lineageRig(t)
	if _, _, adopted := registrar.AdoptRewoundVendorSessionID("s1", "new-uuid", completeLineage()); !adopted {
		t.Fatal("AdoptRewoundVendorSessionID refused a complete lineage")
	}
	spawner.spawn = func(string, CreateOpts) (ShimHandle, error) {
		return ShimHandle{}, errors.New("the shim binary is missing")
	}

	// Act.
	if _, err := spawner.EnsureShim(context.Background(), "s1"); err == nil {
		t.Fatal("EnsureShim = nil against a spawn that failed")
	}

	// Assert.
	if rec, _ := reg.Get("s1"); !rec.Rewind.Armed() {
		t.Fatal("a failed spawn consumed the lineage; the rewind it accounts for would never be announced")
	}
}
