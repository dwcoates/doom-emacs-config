package convert

// diff_test.go — pins the producer's own diff, which is the ONLY account of what
// a write changed: the vendor leaves `structuredPatch` empty for a creation.

import "testing"

func TestDiffHunksStatesNoHunkForAnUnchangedFile(t *testing.T) {
	// Arrange: two identical versions changed nothing.
	before, after := "one\ntwo\n", "one\ntwo\n"

	// Act
	got := diffHunks(before, after)

	// Assert
	if len(got) != 0 {
		t.Fatalf("hunks = %d, want 0", len(got))
	}
}

func TestDiffHunksMakesEveryLineOfACreationAnAddition(t *testing.T) {
	// Arrange: a creation diffs against the empty file.
	before, after := "", "one\ntwo"

	// Act
	got := diffHunks(before, after)

	// Assert
	if len(got) != 1 {
		t.Fatalf("hunks = %d, want 1", len(got))
	}
	want := []string{"+one", "+two"}
	if lines := got[0].GetLines(); len(lines) != len(want) || lines[0] != want[0] || lines[1] != want[1] {
		t.Fatalf("lines = %q, want %q", lines, want)
	}
}

func TestDiffHunksTrimsTheUnchangedPrefixAndSuffixAroundAReplacement(t *testing.T) {
	// Arrange: only the middle line changed, so the hunk begins in the
	// context above it rather than at the top of a long file.
	before := "a\nb\nc\nd\ne\nf\ng\nh"
	after := "a\nb\nc\nd\nE\nf\ng\nh"

	// Act
	got := diffHunks(before, after)

	// Assert
	if len(got) != 1 {
		t.Fatalf("hunks = %d, want 1", len(got))
	}
	if got[0].GetOldRange().GetStart() != 2 {
		t.Fatalf("old start = %d, want 2", got[0].GetOldRange().GetStart())
	}
	want := []string{" b", " c", " d", "-e", "+E", " f", " g", " h"}
	lines := got[0].GetLines()
	if len(lines) != len(want) {
		t.Fatalf("lines = %q, want %q", lines, want)
	}
	for i := range want {
		if lines[i] != want[i] {
			t.Fatalf("lines = %q, want %q", lines, want)
		}
	}
}

func TestDiffHunksCountsBothRangesOverTheContextItKept(t *testing.T) {
	// Arrange: one line replaced by two, with no context above it.
	before, after := "x", "y\nz"

	// Act
	got := diffHunks(before, after)

	// Assert
	if len(got) != 1 {
		t.Fatalf("hunks = %d, want 1", len(got))
	}
	if got[0].GetOldRange().GetLines() != 1 || got[0].GetNewRange().GetLines() != 2 {
		t.Fatalf("ranges = old %d new %d, want old 1 new 2",
			got[0].GetOldRange().GetLines(), got[0].GetNewRange().GetLines())
	}
}
