package runecap

import (
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"path/filepath"
	"strings"
	"testing"
)

func TestHead(t *testing.T) {
	tests := []struct {
		name string
		s    string
		n    int
		want string
	}{
		{name: "shorter than the bound", s: "abc", n: 5, want: "abc"},
		{name: "exactly the bound", s: "abc", n: 3, want: "abc"},
		{name: "longer than the bound", s: "abcdef", n: 3, want: "abc"},
		{name: "multibyte runes are kept whole", s: "héllo wörld", n: 4, want: "héll"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := Head(tt.s, tt.n)

			// Assert
			if got != tt.want {
				t.Fatalf("Head(%q, %d) = %q, want %q", tt.s, tt.n, got, tt.want)
			}
		})
	}
}

func TestEllipsis(t *testing.T) {
	tests := []struct {
		name string
		s    string
		n    int
		want string
	}{
		{name: "an uncut string has no ellipsis", s: "abc", n: 3, want: "abc"},
		{name: "a cut string ends in one", s: "abcdef", n: 3, want: "abc…"},
		{name: "multibyte runes are kept whole", s: "ääää", n: 2, want: "ää…"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := Ellipsis(tt.s, tt.n)

			// Assert
			if got != tt.want {
				t.Fatalf("Ellipsis(%q, %d) = %q, want %q", tt.s, tt.n, got, tt.want)
			}
		})
	}
}

// TestNoPackageHandRollsItsOwnRuneBound pins that the daemon's packages bound
// text through this package rather than a truncateRunes of their own.
func TestNoPackageHandRollsItsOwnRuneBound(t *testing.T) {
	// Arrange
	var found []string
	files := token.NewFileSet()

	// Act
	err := filepath.WalkDir("..", func(path string, entry fs.DirEntry, walkErr error) error {
		if walkErr != nil {
			return walkErr
		}
		if entry.IsDir() || filepath.Ext(path) != ".go" || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		parsed, err := parser.ParseFile(files, path, nil, 0)
		if err != nil {
			return err
		}
		for _, decl := range parsed.Decls {
			if fn, ok := decl.(*ast.FuncDecl); ok && fn.Name.Name == "truncateRunes" {
				found = append(found, path)
			}
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("scan: %v", err)
	}
	if len(found) > 0 {
		t.Fatalf("hand-rolled truncateRunes in %v, want runecap", found)
	}
}
