package dirpath

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestSame(t *testing.T) {
	// Arrange
	root := t.TempDir()
	real := filepath.Join(root, "real")
	if err := os.Mkdir(real, 0o755); err != nil {
		t.Fatal(err)
	}
	link := filepath.Join(root, "link")
	if err := os.Symlink(real, link); err != nil {
		t.Fatal(err)
	}
	other := filepath.Join(root, "other")
	if err := os.Mkdir(other, 0o755); err != nil {
		t.Fatal(err)
	}
	tests := []struct {
		name string
		a, b string
		want bool
	}{
		{name: "one spelling", a: real, b: real, want: true},
		{name: "a trailing slash and a dot segment", a: real + "/", b: filepath.Join(root, ".", "real"), want: true},
		{name: "a symlink to the directory", a: link, b: real, want: true},
		{name: "two directories", a: real, b: other, want: false},
		{name: "two missing spellings of one path", a: filepath.Join(root, "gone"), b: filepath.Join(root, "gone") + "/", want: true},
		{name: "a missing path and an existing one", a: filepath.Join(root, "gone"), b: real, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := Same(tt.a, tt.b)

			// Assert
			if got != tt.want {
				t.Fatalf("Same(%q, %q) = %v, want %v", tt.a, tt.b, got, tt.want)
			}
		})
	}
}

// TestNoPackageRespellsSame pins that every comparison of two directories by
// their resolved symlinks is Same's, so no package keeps a drifting copy.
func TestNoPackageRespellsSame(t *testing.T) {
	// Arrange
	internal, err := filepath.Abs("..")
	if err != nil {
		t.Fatal(err)
	}
	var copies []string

	// Act
	err = filepath.WalkDir(internal, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") || filepath.Dir(path) == filepath.Join(internal, "dirpath") {
			return nil
		}
		raw, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(raw), "func sameDir(") || strings.Contains(string(raw), "func sameDirectory(") {
			copies = append(copies, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(copies) != 0 {
		t.Fatalf("directory comparisons respelled in %v, want dirpath.Same", copies)
	}
}
