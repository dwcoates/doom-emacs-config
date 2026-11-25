package daemonrules

import (
	"go/ast"
	"go/token"
	"go/types"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalDirpathDir is the package these rules hold, under the daemon module's root.
const internalDirpathDir = "internal/dirpath"

// TestNoPackageRespellsSame pins that every comparison of two directories by
// their resolved symlinks is Same's, so no package keeps a drifting copy.
func TestNoPackageRespellsSame(t *testing.T) {
	sourcescan.InPackage(t, internalDirpathDir)
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

// TestNoDaemonSourceHandRollsWithin pins that every "is this path inside that
// directory" check in the daemon goes through Within: a hand-rolled one is a
// strings.HasPrefix of an identifier against another identifier joined to
// filepath.Separator.
func TestNoDaemonSourceHandRollsWithin(t *testing.T) {
	sourcescan.InPackage(t, internalDirpathDir)
	// Arrange
	var found []string

	// Act
	sourcescan.EachDaemonSource(t, sourcescan.DaemonRoot(t), []string{"internal/dirpath"}, func(fset *token.FileSet, file *ast.File) {
		ast.Inspect(file, func(n ast.Node) bool {
			if call, ok := n.(*ast.CallExpr); ok && handRollsWithin(call) {
				found = append(found, fset.Position(call.Pos()).String())
			}
			return true
		})
	})

	// Assert
	if len(found) > 0 {
		t.Fatalf("these sites hand-roll dirpath.Within: %v", found)
	}
}

// handRollsWithin reports whether CALL is strings.HasPrefix(x, y +
// string(filepath.Separator)) with x and y identifiers.
func handRollsWithin(call *ast.CallExpr) bool {
	sel, ok := call.Fun.(*ast.SelectorExpr)
	if !ok || sel.Sel.Name != "HasPrefix" || len(call.Args) != 2 {
		return false
	}
	if pkg, ok := sel.X.(*ast.Ident); !ok || pkg.Name != "strings" {
		return false
	}
	if _, ok := call.Args[0].(*ast.Ident); !ok {
		return false
	}
	sum, ok := call.Args[1].(*ast.BinaryExpr)
	if !ok {
		return false
	}
	_, lhsIdent := sum.X.(*ast.Ident)
	return lhsIdent && strings.Contains(types.ExprString(sum.Y), "filepath.Separator")
}
