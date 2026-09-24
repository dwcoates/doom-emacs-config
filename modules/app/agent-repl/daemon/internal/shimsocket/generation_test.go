package shimsocket

import (
	"os"
	"path/filepath"
	"testing"
)

// touch creates an empty file at path, standing for a socket path that exists.
func touch(t *testing.T, path string) {
	t.Helper()
	if err := os.WriteFile(path, nil, 0o600); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}
}

// scripted answers a probe from a map; an unlisted path is ABSENT, which is
// the ordinary "nothing ever bound here".
func scripted(states map[string]State) func(string) (State, error) {
	return func(path string) (State, error) {
		state, ok := states[path]
		if !ok {
			return StateAbsent, nil
		}
		return state, nil
	}
}

// TestNewestLivePrefersTheBasePathWhenItIsLive pins that a workspace whose
// shim never rolled is answered its own path, with no directory scan deciding
// anything.
func TestNewestLivePrefersTheBasePathWhenItIsLive(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	base := filepath.Join(dir, "ws.sock")
	touch(t, base)
	touch(t, filepath.Join(dir, "ws.n2.sock"))

	// Act.
	got, state, err := NewestLive(scripted(map[string]State{base: StateLive}), base)

	// Assert.
	if err != nil {
		t.Fatalf("NewestLive = error %v", err)
	}
	if got != base || state != StateLive {
		t.Fatalf("NewestLive = (%q, %v), want (%q, StateLive)", got, state, base)
	}
}

// TestNewestLiveResolvesTheRolledGeneration is the owner's wedge in one
// assertion: the base path is gone, the surviving shim listens on `.n1.sock`,
// and a boot that dialed the base path redialed a path nobody has held since
// the relaunch.
func TestNewestLiveResolvesTheRolledGeneration(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	base := filepath.Join(dir, "ws.sock")
	rolled := filepath.Join(dir, "ws.n1.sock")
	touch(t, rolled)

	// Act.
	got, state, err := NewestLive(scripted(map[string]State{rolled: StateLive}), base)

	// Assert.
	if err != nil {
		t.Fatalf("NewestLive = error %v", err)
	}
	if got != rolled || state != StateLive {
		t.Fatalf("NewestLive = (%q, %v), want (%q, StateLive)", got, state, rolled)
	}
}

// TestNewestLiveTakesTheHighestLiveGeneration pins the ordering: a generation
// is only ever bumped, so a higher N is a later shim and a dead lower one is
// never preferred to it.
func TestNewestLiveTakesTheHighestLiveGeneration(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	base := filepath.Join(dir, "ws.sock")
	first := filepath.Join(dir, "ws.n1.sock")
	third := filepath.Join(dir, "ws.n3.sock")
	touch(t, first)
	touch(t, third)

	// Act.
	got, _, err := NewestLive(scripted(map[string]State{first: StateLive, third: StateLive}), base)

	// Assert.
	if err != nil {
		t.Fatalf("NewestLive = error %v", err)
	}
	if got != third {
		t.Fatalf("NewestLive = %q, want the highest live generation %q", got, third)
	}
}

// TestNewestLiveAnswersTheBasePathWhenNothingIsLive pins the fall-back every
// caller's spawn and stale-clear path is written against: with no listener
// anywhere, the answer is the base path and the base path's own probe result.
func TestNewestLiveAnswersTheBasePathWhenNothingIsLive(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	base := filepath.Join(dir, "ws.sock")
	stale := filepath.Join(dir, "ws.n4.sock")
	touch(t, base)
	touch(t, stale)

	// Act.
	got, state, err := NewestLive(scripted(map[string]State{base: StateStale, stale: StateStale}), base)

	// Assert.
	if err != nil {
		t.Fatalf("NewestLive = error %v", err)
	}
	if got != base || state != StateStale {
		t.Fatalf("NewestLive = (%q, %v), want (%q, StateStale)", got, state, base)
	}
}

// TestNewestLiveIgnoresAnUnparsableGenerationSuffix pins that only the minted
// spelling counts: `.nX.sock` is not a generation and must not be dialed as
// one.
func TestNewestLiveIgnoresAnUnparsableGenerationSuffix(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	base := filepath.Join(dir, "ws.sock")
	bogus := filepath.Join(dir, "ws.nX.sock")
	touch(t, bogus)

	// Act.
	got, _, err := NewestLive(scripted(map[string]State{bogus: StateLive}), base)

	// Assert.
	if err != nil {
		t.Fatalf("NewestLive = error %v", err)
	}
	if got != base {
		t.Fatalf("NewestLive = %q, want the base path %q: %q is not a minted generation", got, base, bogus)
	}
}

// TestNewestLiveIgnoresAGenerationOfAnotherWorkspace pins the prefix: two
// workspaces' sockets share one directory, and one's rolled generation is
// never another's.
func TestNewestLiveIgnoresAGenerationOfAnotherWorkspace(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	base := filepath.Join(dir, "ws.sock")
	other := filepath.Join(dir, "other.n1.sock")
	touch(t, other)

	// Act.
	got, _, err := NewestLive(scripted(map[string]State{other: StateLive}), base)

	// Assert.
	if err != nil {
		t.Fatalf("NewestLive = error %v", err)
	}
	if got != base {
		t.Fatalf("NewestLive = %q, want the base path %q: %q belongs to another workspace", got, base, other)
	}
}

// TestNextGenerationMintsPastEveryGenerationOnDisk is the relaunch of an
// already-relaunched adopted shim: the running one holds `.n1.sock`, the
// adopting daemon's counter is zero, and the replacement must get `.n2.sock`.
func TestNextGenerationMintsPastEveryGenerationOnDisk(t *testing.T) {
	tests := []struct {
		name     string
		existing []string
		after    int
		want     string
		wantN    int
	}{
		{name: "no generation exists", after: 0, want: "ws.n1.sock", wantN: 1},
		{name: "a predecessor's generation exists", existing: []string{"ws.n1.sock"}, after: 0, want: "ws.n2.sock", wantN: 2},
		{name: "the counter is already past the disk", existing: []string{"ws.n1.sock"}, after: 3, want: "ws.n4.sock", wantN: 4},
		{name: "the newest of several generations decides", existing: []string{"ws.n2.sock", "ws.n7.sock"}, after: 1, want: "ws.n8.sock", wantN: 8},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			dir := t.TempDir()
			base := filepath.Join(dir, "ws.sock")
			for _, name := range tt.existing {
				touch(t, filepath.Join(dir, name))
			}

			// Act.
			got, n := NextGeneration(base, tt.after)

			// Assert.
			if got != filepath.Join(dir, tt.want) || n != tt.wantN {
				t.Fatalf("NextGeneration = (%q, %d), want (%q, %d)", got, n, filepath.Join(dir, tt.want), tt.wantN)
			}
		})
	}
}
