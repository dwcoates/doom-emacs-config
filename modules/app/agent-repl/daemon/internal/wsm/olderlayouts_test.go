package wsm

import (
	"os"
	"path/filepath"
	"regexp"
	"testing"
)

func TestEveryOlderLayoutFileIsBuiltForwardByFixtureAt(t *testing.T) {
	// Arrange: every test file of this package but this one, which names the
	// pattern it refuses.
	names, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	undo := regexp.MustCompile(`DROP COLUMN`)

	// Act / Assert: no test takes a migrated file back by undoing a layout's
	// columns. A file at an older layout is built forward with fixtureAt,
	// which replays the migrations themselves, so a new layout never has to
	// be restated as an undo for an older-layout test to stay true.
	for _, name := range names {
		if name == "olderlayouts_test.go" {
			continue
		}
		source, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("read %s: %v", name, err)
		}
		if undo.Match(source) {
			t.Errorf("%s drops a column to take a file back to an older layout, want fixtureAt", name)
		}
	}
}
