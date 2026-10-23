package creategate

import (
	"agent-repld/internal/sourcescan"
	"go/ast"
	"go/token"
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
	var offenders []string

	// Act
	sourcescan.EachDaemonSource(t, sourcescan.DaemonRoot(t), []string{"internal/creategate"}, func(fset *token.FileSet, file *ast.File) {
		imports := false
		for _, spec := range file.Imports {
			if p, _ := strconv.Unquote(spec.Path.Value); p == "agent-repld/internal/creategate" {
				imports = true
			}
		}
		if !imports {
			return
		}
		ast.Inspect(file, func(n ast.Node) bool {
			if call, ok := n.(*ast.CallExpr); ok {
				if sel, ok := call.Fun.(*ast.SelectorExpr); ok && sel.Sel.Name == "Idle" && len(call.Args) == 0 {
					offenders = append(offenders, fset.Position(call.Pos()).String())
				}
			}
			return true
		})
	})

	// Assert
	if len(offenders) > 0 {
		t.Fatalf("these sites wait on the create gate's Idle themselves; use Gate.AwaitIdle:\n%s", strings.Join(offenders, "\n"))
	}
}
