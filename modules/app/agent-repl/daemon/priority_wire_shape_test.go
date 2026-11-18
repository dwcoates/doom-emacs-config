package daemon_test

import (
	"go/ast"
	"go/token"
	"strings"
	"testing"

	"agent-repld/internal/sourcescan"
)

// TestNoOtherDaemonSourceBuildsAPriorityByHand holds every site to
// prioritywire, the one mapping between wsm.Priority, the wire's
// agentrepl.v1.WorkspacePriority and the -priority spelling: a composite
// literal of a priority arm anywhere else in the daemon is a second mapping
// that can drift from it.
func TestNoOtherDaemonSourceBuildsAPriorityByHand(t *testing.T) {
	// Arrange
	var offenders []string

	// Act
	sourcescan.EachDaemonSource(t, sourcescan.DaemonRoot(t), []string{"internal/prioritywire"}, func(fset *token.FileSet, file *ast.File) {
		ast.Inspect(file, func(n ast.Node) bool {
			if lit, ok := n.(*ast.CompositeLit); ok {
				if sel, ok := lit.Type.(*ast.SelectorExpr); ok && strings.HasPrefix(sel.Sel.Name, "WorkspacePriority_P") {
					offenders = append(offenders, fset.Position(lit.Pos()).String())
				}
			}
			return true
		})
	})

	// Assert
	if len(offenders) > 0 {
		t.Fatalf("these daemon sources build a priority by hand; use prioritywire: %v", offenders)
	}
}
