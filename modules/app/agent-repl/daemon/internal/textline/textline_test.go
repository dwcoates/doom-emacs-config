package textline

import (
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"path/filepath"
	"strings"
	"testing"
)

func TestFirstNonBlank(t *testing.T) {
	tests := []struct {
		name string
		text string
		want string
	}{
		{name: "one line", text: "hello", want: "hello"},
		{name: "leading blank lines are skipped", text: "\n \n  hello  \nworld", want: "hello"},
		{name: "later lines are dropped", text: "first\nsecond", want: "first"},
		{name: "nothing but blanks", text: " \n\t\n", want: ""},
		{name: "empty text", text: "", want: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := FirstNonBlank(tt.text)

			// Assert
			if got != tt.want {
				t.Fatalf("FirstNonBlank(%q) = %q, want %q", tt.text, got, tt.want)
			}
		})
	}
}

// divergentFirstLines are the packages whose own firstLine keeps a DIFFERENT
// rule than FirstNonBlank, so they are not this helper's call sites: the
// sidebar keeps a prompt's literal first line, and the merge narration names
// a fallback reason for an empty one.
var divergentFirstLines = map[string]bool{
	filepath.Join("..", "resolve", "sidebar", "rows.go"): true,
	filepath.Join("..", "merge", "process.go"):           true,
}

// TestNoPackageHandRollsItsOwnFirstLine pins that the daemon's packages take
// the first non-blank line through this package rather than a firstLine of
// their own.
func TestNoPackageHandRollsItsOwnFirstLine(t *testing.T) {
	// Arrange
	var found []string
	files := token.NewFileSet()

	// Act
	err := filepath.WalkDir("..", func(path string, entry fs.DirEntry, walkErr error) error {
		if walkErr != nil {
			return walkErr
		}
		if entry.IsDir() || filepath.Ext(path) != ".go" || strings.HasSuffix(path, "_test.go") || divergentFirstLines[path] {
			return nil
		}
		parsed, err := parser.ParseFile(files, path, nil, 0)
		if err != nil {
			return err
		}
		for _, decl := range parsed.Decls {
			if fn, ok := decl.(*ast.FuncDecl); ok && (fn.Name.Name == "firstLine" || fn.Name.Name == "firstNonBlankLine") {
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
		t.Fatalf("hand-rolled firstLine in %v, want textline.FirstNonBlank", found)
	}
}
