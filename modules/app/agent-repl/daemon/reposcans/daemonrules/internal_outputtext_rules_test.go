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

// internalOutputtextDir is the package these rules hold, under the daemon module's root.
const internalOutputtextDir = "internal/outputtext"

// TestNoPackageHandRollsItsOwnLastLineOrTail fails when a daemon package
// declares its own `lastLine` or `tail` function again instead of calling
// this package.
func TestNoPackageHandRollsItsOwnLastLineOrTail(t *testing.T) {
	sourcescan.InPackage(t, internalOutputtextDir)
	// Arrange.
	root := filepath.Join("..", "..")
	forbidden := map[string]bool{"lastLine": true, "tail": true}
	var found []string

	// Act.
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() && (d.Name() == "testdata" || d.Name() == "vendor") {
			return filepath.SkipDir
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		file, err := parser.ParseFile(token.NewFileSet(), path, nil, parser.SkipObjectResolution)
		if err != nil {
			return err
		}
		for _, decl := range file.Decls {
			if fn, ok := decl.(*ast.FuncDecl); ok && fn.Recv == nil && forbidden[fn.Name.Name] {
				found = append(found, path+": "+fn.Name.Name)
			}
		}
		return nil
	})

	// Assert.
	if err != nil {
		t.Fatalf("walk the daemon sources: %v", err)
	}
	if len(found) > 0 {
		t.Fatalf("hand-rolled output helpers, use outputtext instead: %v", found)
	}
}
