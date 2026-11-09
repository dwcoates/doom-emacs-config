package persistentwifi

import (
	"os"
	"path/filepath"
	"slices"
	"testing"
)

func TestPowerPrefsPathsNamesTheDirAndEachPowerFile(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	for _, name := range []string{"com.apple.PowerManagement.plist", "com.apple.PowerManagement.ABC.plist", "com.apple.other.plist"} {
		if err := os.WriteFile(filepath.Join(dir, name), nil, 0o600); err != nil {
			t.Fatal(err)
		}
	}

	// Act.
	got := powerPrefsPaths(dir)

	// Assert.
	want := []string{dir, filepath.Join(dir, "com.apple.PowerManagement.ABC.plist"), filepath.Join(dir, "com.apple.PowerManagement.plist")}
	if !slices.Equal(got, want) {
		t.Fatalf("powerPrefsPaths() = %v, want %v", got, want)
	}
}

func TestAFakeHostsPowerChangesNeverFire(t *testing.T) {
	// Arrange.
	changes, stop := Config{FakeLid: "open"}.PowerChanges()(t.Context())
	defer stop()

	// Act, Assert: nothing is ready, and nothing ever will be.
	select {
	case err := <-changes:
		t.Fatalf("a fake host's power source fired: %v", err)
	default:
	}
}
