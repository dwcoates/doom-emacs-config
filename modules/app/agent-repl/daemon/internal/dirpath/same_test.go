package dirpath

import (
	"os"
	"path/filepath"
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
