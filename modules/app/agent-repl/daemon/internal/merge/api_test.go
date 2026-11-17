package merge

import (
	"os"
	"path/filepath"
	"testing"
)

// TestBriefsFromIsLoudAboutAMissingBrief covers the brief contract: a missing
// file refuses the round rather than sending an empty prompt.
func TestBriefsFromIsLoudAboutAMissingBrief(t *testing.T) {
	// Arrange: a prompts directory with nothing in it.
	dir := t.TempDir()
	load := BriefsFrom(dir)

	// Act.
	_, err := load(BriefConflictResolve, map[string]string{})

	// Assert.
	if err == nil {
		t.Fatal("a missing brief composed a prompt instead of refusing")
	}
}

// TestBriefsFromReadsTheDirectoryAtEveryUse covers why the loader holds a
// directory rather than a cached brief: editing one takes effect without a
// daemon bounce.
func TestBriefsFromReadsTheDirectoryAtEveryUse(t *testing.T) {
	// Arrange: a loader over a directory whose brief appears between uses.
	dir := t.TempDir()
	load := BriefsFrom(dir)
	if _, err := load("late", map[string]string{}); err == nil {
		t.Fatal("the brief was already there; the test's premise is wrong")
	}
	if err := os.WriteFile(filepath.Join(dir, "late.md"), []byte("<!-- used by: internal/merge; placeholders: none -->\na brief\n"), 0o644); err != nil {
		t.Fatalf("writing the brief: %v", err)
	}

	// Act.
	_, err := load("late", map[string]string{})

	// Assert: the loader reached the file system again rather than answering
	// from anything it kept — a cached loader would have answered identically
	// both times.
	if err != nil {
		t.Fatalf("the second load answered %v, want the brief the directory now holds", err)
	}
}
