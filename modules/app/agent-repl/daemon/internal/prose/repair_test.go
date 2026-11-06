package prose

import (
	"go/ast"
	"go/parser"
	"go/token"
	"path/filepath"
	"strings"
	"testing"
)

func TestHoldsTree(t *testing.T) {
	tests := []struct {
		name string
		body string
		want bool
	}{
		{name: "a tree", body: lines("1 🔧 A", "└── 1.1 a"), want: true},
		{name: "a tree with code under a branch", body: lines("1 🔧 A", "├── 1.1 a", "│   ```go", "│   x := 1", "│   ```", "├── 1.2 c", "├── 1.3 d", "└── 1.4 b"), want: true},
		{name: "code", body: lines("x := 1", "y := 2"), want: false},
		{name: "a lone root", body: "1 🔧 A", want: false},
		{name: "nothing", body: "", want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := holdsTree(tt.body)

			// Assert.
			if got != tt.want {
				t.Errorf("holdsTree(%q) = %v, want %v", tt.body, got, tt.want)
			}
		})
	}
}

// TestEveryFencedTreeTestGoesThroughHoldsTree holds the package to one test of
// whether a fence wraps a tree: every place that asks calls holdsTree, and only
// holdsTree reads a tree region for isMetapromptTree.
func TestEveryFencedTreeTestGoesThroughHoldsTree(t *testing.T) {
	// Arrange.
	fset := token.NewFileSet()
	calls := map[string]map[string]bool{}
	files, err := filepath.Glob("*.go")
	if err != nil {
		t.Fatalf("glob the package's files: %v", err)
	}
	for _, file := range files {
		if strings.HasSuffix(file, "_test.go") {
			continue
		}
		f, err := parser.ParseFile(fset, file, nil, 0)
		if err != nil {
			t.Fatalf("parse %s: %v", file, err)
		}
		for _, decl := range f.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Body == nil {
				continue
			}
			ast.Inspect(fn.Body, func(n ast.Node) bool {
				if call, ok := n.(*ast.CallExpr); ok {
					if id, ok := call.Fun.(*ast.Ident); ok {
						if calls[id.Name] == nil {
							calls[id.Name] = map[string]bool{}
						}
						calls[id.Name][fn.Name.Name] = true
					}
				}
				return true
			})
		}
	}

	// Assert.
	for _, caller := range []string{"plainBlocks"} {
		if !calls["holdsTree"][caller] {
			t.Errorf("%s does not call holdsTree", caller)
		}
	}
	for caller := range calls["isMetapromptTree"] {
		if caller != "holdsTree" {
			t.Errorf("%s calls isMetapromptTree itself instead of holdsTree", caller)
		}
	}
}
