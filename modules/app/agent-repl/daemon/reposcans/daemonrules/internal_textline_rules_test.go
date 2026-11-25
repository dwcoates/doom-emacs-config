package daemonrules

import (
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalTextlineDir is the package these rules hold, under the daemon module's root.
const internalTextlineDir = "internal/textline"

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
	sourcescan.InPackage(t, internalTextlineDir)
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

func TestNoPackageHandRollsItsOwnLineSplit(t *testing.T) {
	sourcescan.InPackage(t, internalTextlineDir)
	// Arrange
	var found []string

	// Act
	err := filepath.WalkDir("..", func(path string, entry fs.DirEntry, walkErr error) error {
		if walkErr != nil {
			return walkErr
		}
		if entry.IsDir() || filepath.Ext(path) != ".go" || strings.HasSuffix(path, "_test.go") || filepath.Base(filepath.Dir(path)) == "textline" {
			return nil
		}
		source, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(source), `strings.Split(strings.TrimSuffix(`) {
			found = append(found, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("scan: %v", err)
	}
	if len(found) > 0 {
		t.Fatalf("hand-rolled line split in %v, want textline.Lines", found)
	}
}
