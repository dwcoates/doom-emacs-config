package harness

import (
	"agent-repld/internal/sourcescan"
	"go/ast"
	"go/token"
	"testing"
)

// TestEveryDaemonTestHolderIsHoldtests pins the one test holder: the daemon's tests stand a holder up only through
// holdtest.Start, never by serving sockethold.Serve themselves.
func TestEveryDaemonTestHolderIsHoldtests(t *testing.T) {
	tests := []struct {
		name string
		dir  string
	}{
		{name: "the integration harness", dir: "."},
		{name: "the command's tests", dir: "../../cmd/agent-repld"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var served []string

			// Act
			sourcescan.EachSourceIn(t, tt.dir, true, func(fset *token.FileSet, f *ast.File) {
				ast.Inspect(f, func(n ast.Node) bool {
					if sourcescan.IsSelector(n, "sockethold", "Serve") {
						served = append(served, fset.Position(n.Pos()).String())
					}
					return true
				})
			})

			// Assert
			if len(served) > 0 {
				t.Fatalf("sockethold.Serve called directly at %v; stand a test holder up with holdtest.Start", served)
			}
		})
	}
}
