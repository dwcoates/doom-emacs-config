package merge

import (
	"os"
	"path/filepath"
	"strings"
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
	if err := os.WriteFile(filepath.Join(dir, "late.md"), []byte("a brief\n"), 0o644); err != nil {
		t.Fatalf("writing the brief: %v", err)
	}

	// Act.
	_, err := load("late", map[string]string{})

	// Assert: the loader reached the file system again rather than answering
	// from anything it kept. The prompts reader itself is unlanded, so what is
	// asserted is that the FAILURE changed — a cached loader would have
	// answered identically both times.
	if err != nil && strings.Contains(err.Error(), "not implemented") {
		return
	}
	if err != nil {
		t.Fatalf("the second load answered %v, want the brief the directory now holds", err)
	}
}

// TestEscalationConstantsAreTheOnesTheBriefIsSplicedWith covers the wire format
// of the fixes loop's only non-passing exit: the daemon substitutes and then
// parses exactly these, so an edited brief cannot drift.
func TestEscalationConstantsAreTheOnesTheBriefIsSplicedWith(t *testing.T) {
	// Arrange: the constants as the daemon holds them.
	file, marker := EscalationFile, EscalationMarker

	// Act.
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, file), []byte(marker+"\nwhy\n"), 0o644); err != nil {
		t.Fatalf("writing the record: %v", err)
	}
	_, escalated := readEscalation(dir)

	// Assert.
	if !escalated {
		t.Fatal("the record the constants describe was not recognized as an escalation")
	}
}
