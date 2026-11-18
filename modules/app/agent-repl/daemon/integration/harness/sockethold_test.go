package harness

import (
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"strings"
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
			entries, err := os.ReadDir(tt.dir)
			if err != nil {
				t.Fatal(err)
			}
			fset := token.NewFileSet()
			var served []string

			// Act
			for _, e := range entries {
				if !strings.HasSuffix(e.Name(), ".go") {
					continue
				}
				f, err := parser.ParseFile(fset, filepath.Join(tt.dir, e.Name()), nil, 0)
				if err != nil {
					t.Fatal(err)
				}
				ast.Inspect(f, func(n ast.Node) bool {
					if sel, ok := n.(*ast.SelectorExpr); ok {
						if id, ok := sel.X.(*ast.Ident); ok && id.Name == "sockethold" && sel.Sel.Name == "Serve" {
							served = append(served, fset.Position(sel.Pos()).String())
						}
					}
					return true
				})
			}

			// Assert
			if len(served) > 0 {
				t.Fatalf("sockethold.Serve called directly at %v; stand a test holder up with holdtest.Start", served)
			}
		})
	}
}
