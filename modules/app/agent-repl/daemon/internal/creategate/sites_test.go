package creategate

import (
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
)

// TestEveryWaitOnTheGateGoesThroughAwaitIdle holds every daemon source that
// imports this package to AwaitIdle: one that selected on Idle itself would
// hand-roll its own wait, and drift from the holdout naming every other one
// shares.
func TestEveryWaitOnTheGateGoesThroughAwaitIdle(t *testing.T) {
	// Arrange
	root, err := filepath.Abs("../..")
	if err != nil {
		t.Fatalf("resolve the daemon root: %v", err)
	}
	fset := token.NewFileSet()
	var offenders []string

	// Act
	err = filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			if strings.HasPrefix(d.Name(), ".") || filepath.Base(path) == "creategate" {
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		file, err := parser.ParseFile(fset, path, nil, parser.SkipObjectResolution)
		if err != nil {
			return err
		}
		imports := false
		for _, spec := range file.Imports {
			if p, _ := strconv.Unquote(spec.Path.Value); p == "agent-repld/internal/creategate" {
				imports = true
			}
		}
		if !imports {
			return nil
		}
		ast.Inspect(file, func(n ast.Node) bool {
			if call, ok := n.(*ast.CallExpr); ok {
				if sel, ok := call.Fun.(*ast.SelectorExpr); ok && sel.Sel.Name == "Idle" && len(call.Args) == 0 {
					offenders = append(offenders, fset.Position(call.Pos()).String())
				}
			}
			return true
		})
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk the daemon sources: %v", err)
	}
	if len(offenders) > 0 {
		t.Fatalf("these sites wait on the create gate's Idle themselves; use Gate.AwaitIdle:\n%s", strings.Join(offenders, "\n"))
	}
}
