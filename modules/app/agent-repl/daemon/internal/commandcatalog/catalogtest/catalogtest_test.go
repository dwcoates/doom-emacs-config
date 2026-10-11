package catalogtest

import (
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestNamed(t *testing.T) {
	tests := []struct {
		name  string
		names []string
		want  []string
	}{
		{name: "no names is an empty catalog", names: nil, want: nil},
		{name: "one entry per name, in order", names: []string{"merge-queue", "compact"}, want: []string{"merge-queue", "compact"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := Named(tt.names...)

			// Assert.
			if got == nil {
				t.Fatal("Named answered nil, want a catalog")
			}
			var names []string
			for _, entry := range got.GetCommands() {
				if entry.Description != nil || entry.ArgumentHint != nil || len(entry.GetAliases()) != 0 {
					t.Fatalf("entry %q carries more than its name: %v", entry.GetName(), entry)
				}
				names = append(names, entry.GetName())
			}
			if len(names) != len(tt.want) {
				t.Fatalf("names = %v, want %v", names, tt.want)
			}
			for i := range names {
				if names[i] != tt.want[i] {
					t.Fatalf("names = %v, want %v", names, tt.want)
				}
			}
		})
	}
}

// TestNoTestHandRollsANamedCatalog pins that every test builds its
// names-only catalogs through Named, so a second fixture cannot drift from it.
func TestNoTestHandRollsANamedCatalog(t *testing.T) {
	// Arrange.
	root := filepath.Join("..", "..")
	var offenders []string

	// Act.
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, "_test.go") || strings.Contains(path, "catalogtest") {
			return nil
		}
		raw, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(raw), "SessionCommandCatalogEntry{Name: name}") {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert.
	if err != nil {
		t.Fatalf("walk the daemon's tests: %v", err)
	}
	if len(offenders) != 0 {
		t.Fatalf("tests hand-roll a names-only catalog instead of catalogtest.Named: %v", offenders)
	}
}
