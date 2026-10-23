package childproc

import (
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"path/filepath"
	"strings"
	"testing"
)

// TestEveryCapturedChildWritesToFiles holds every daemon source outside this
// package to CaptureOutput: a child whose output is an in-memory buffer gets a
// pipe, and Wait then waits on every process that inherited it.
func TestEveryCapturedChildWritesToFiles(t *testing.T) {
	// Arrange
	root, err := filepath.Abs("../..")
	if err != nil {
		t.Fatalf("resolve the daemon root: %v", err)
	}
	var offenders []string
	fset := token.NewFileSet()

	// Act
	err = filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		rel, _ := filepath.Rel(root, path)
		if d.IsDir() {
			if rel == "integration" || rel == "e2e" || rel == "internal/childproc" || strings.HasPrefix(d.Name(), ".") {
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
		// The commands this file builds, by the name each is bound to: only
		// their Output and CombinedOutput are exec's, which pipe the output.
		commands := map[string]bool{}
		ast.Inspect(file, func(n ast.Node) bool {
			if assign, ok := n.(*ast.AssignStmt); ok && len(assign.Lhs) == len(assign.Rhs) {
				for i, rhs := range assign.Rhs {
					if builtHere(rhs) {
						if id, ok := assign.Lhs[i].(*ast.Ident); ok {
							commands[id.Name] = true
						}
					}
				}
			}
			return true
		})
		ast.Inspect(file, func(n ast.Node) bool {
			switch n := n.(type) {
			case *ast.AssignStmt:
				for i, lhs := range n.Lhs {
					sel, ok := lhs.(*ast.SelectorExpr)
					if !ok || (sel.Sel.Name != "Stdout" && sel.Sel.Name != "Stderr") || i >= len(n.Rhs) {
						continue
					}
					if unary, ok := n.Rhs[i].(*ast.UnaryExpr); ok && unary.Op == token.AND {
						offenders = append(offenders, fset.Position(n.Pos()).String())
					}
				}
			case *ast.CallExpr:
				sel, ok := n.Fun.(*ast.SelectorExpr)
				if !ok || (sel.Sel.Name != "Output" && sel.Sel.Name != "CombinedOutput") {
					break
				}
				if id, ok := sel.X.(*ast.Ident); (ok && commands[id.Name]) || builtHere(sel.X) {
					offenders = append(offenders, fset.Position(n.Pos()).String())
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
		t.Fatalf("these sites capture a child's output in memory; use childproc.CaptureOutput:\n%s", strings.Join(offenders, "\n"))
	}
}

// builtHere reports whether expr is a call to this package's command builders.
func builtHere(expr ast.Expr) bool {
	call, ok := expr.(*ast.CallExpr)
	if !ok {
		return false
	}
	sel, ok := call.Fun.(*ast.SelectorExpr)
	if !ok {
		return false
	}
	pkg, ok := sel.X.(*ast.Ident)
	return ok && pkg.Name == "childproc" && (sel.Sel.Name == "Command" || sel.Sel.Name == "CommandContext")
}
