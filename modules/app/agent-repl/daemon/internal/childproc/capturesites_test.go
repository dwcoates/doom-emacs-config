package childproc

import (
	"agent-repld/internal/sourcescan"
	"go/ast"
	"go/token"
	"strings"
	"testing"
)

// TestEveryCapturedChildWritesToFiles holds every daemon source outside this
// package to CaptureOutput: a child whose output is an in-memory buffer gets a
// pipe, and Wait then waits on every process that inherited it.
func TestEveryCapturedChildWritesToFiles(t *testing.T) {
	// Arrange
	var offenders []string

	// Act
	sourcescan.EachDaemonSource(t, sourcescan.DaemonRoot(t), []string{"internal/childproc"}, func(fset *token.FileSet, file *ast.File) {
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
	})

	// Assert
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
