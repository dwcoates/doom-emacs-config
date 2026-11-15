package dirpath

import (
	"go/ast"
	"go/token"
	"go/types"
	"strings"
	"testing"

	"agent-repld/internal/sourcescan"
)

func TestWithin(t *testing.T) {
	tests := []struct {
		name      string
		path, dir string
		want      bool
	}{
		{name: "the directory itself", path: "/a/b", dir: "/a/b", want: true},
		{name: "a path beneath it", path: "/a/b/c/d", dir: "/a/b", want: true},
		{name: "a sibling sharing its prefix", path: "/a/bc", dir: "/a/b", want: false},
		{name: "its parent", path: "/a", dir: "/a/b", want: false},
		{name: "an unrelated path", path: "/x/y", dir: "/a/b", want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := Within(tt.path, tt.dir)

			// Assert
			if got != tt.want {
				t.Fatalf("Within(%q, %q) = %v, want %v", tt.path, tt.dir, got, tt.want)
			}
		})
	}
}

// TestNoDaemonSourceHandRollsWithin pins that every "is this path inside that
// directory" check in the daemon goes through Within: a hand-rolled one is a
// strings.HasPrefix of an identifier against another identifier joined to
// filepath.Separator.
func TestNoDaemonSourceHandRollsWithin(t *testing.T) {
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
