package daemonrules

import (
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalRunecapDir is the package these rules hold, under the daemon module's root.
const internalRunecapDir = "internal/runecap"

// TestNoPackageHandRollsItsOwnRuneBound pins that the daemon's packages bound
// text through this package rather than a truncateRunes of their own.
func TestNoPackageHandRollsItsOwnRuneBound(t *testing.T) {
	sourcescan.InPackage(t, internalRunecapDir)
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
