package harness

import (
	"strings"
	"testing"
	"time"
)

func TestReflogEntriesWritesOneGitShapedEntryPerInstant(t *testing.T) {
	// Arrange.
	first, second := time.Unix(1_700_000_000, 0), time.Unix(1_700_000_100, 0)

	// Act.
	got := reflogEntries(first, second)

	// Assert.
	lines := strings.Split(strings.TrimSuffix(got, "\n"), "\n")
	if len(lines) != 2 {
		t.Fatalf("reflogEntries = %q, want two lines", got)
	}
	for i, want := range []string{"> 1700000000 +0000\t", "> 1700000100 +0000\t"} {
		if !strings.Contains(lines[i], want) {
			t.Fatalf("line %d = %q, want %q in git's reflog shape", i, lines[i], want)
		}
	}
}
