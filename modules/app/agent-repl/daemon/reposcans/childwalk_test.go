package reposcans

import (
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// childGetters reads the generated Go code under GENERATED and answers every
// getter that returns a message whose children are NO TREE of its own kind:
// the getter's name, mapped to true when a message it returns is such a list
// (a refusal's child workspaces, a status's child refs, a dispatch's
// children) and none is a tree. A message with no GetChildren at all cannot
// be the receiver of a GetChildren call that compiles, so it decides nothing.
// A message whose GetChildren answers more of itself (a roster row, an
// arranged workspace) is a tree, which only rosterwalk walks.
func childGetters(t *testing.T, generated string) map[string]bool {
	t.Helper()
	trees, lists := map[string]bool{}, map[string]bool{}
	returns := map[string][]string{}
	err := filepath.WalkDir(generated, func(path string, d os.DirEntry, err error) error {
		if err != nil || d.IsDir() || !strings.HasSuffix(path, ".pb.go") {
			return err
		}
		file, err := parser.ParseFile(token.NewFileSet(), path, nil, parser.SkipObjectResolution)
		if err != nil {
			return err
		}
		for _, decl := range file.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Recv == nil || len(fn.Recv.List) != 1 || fn.Type.Results == nil || len(fn.Type.Results.List) != 1 {
				continue
			}
			recv := typeName(fn.Recv.List[0].Type)
			result := fn.Type.Results.List[0].Type
			if fn.Name.Name == "GetChildren" {
				if elem, ok := result.(*ast.ArrayType); ok && typeName(elem.Elt) == recv {
					trees[recv] = true
				} else {
					lists[recv] = true
				}
				continue
			}
			if strings.HasPrefix(fn.Name.Name, "Get") {
				if name := typeName(result); name != "" {
					returns[fn.Name.Name] = append(returns[fn.Name.Name], name)
				}
			}
		}
		return nil
	})
	if err != nil {
		t.Fatalf("read the generated code under %s: %v", generated, err)
	}
	out := map[string]bool{}
	for getter, types := range returns {
		list, tree := false, false
		for _, ty := range types {
			list = list || lists[ty]
			tree = tree || trees[ty]
		}
		if list && !tree {
			out[getter] = true
		}
	}
	return out
}

// typeName is the message a type expression names (*pkg.T, *T, T), or ""
// for any other expression.
func typeName(expr ast.Expr) string {
	if star, ok := expr.(*ast.StarExpr); ok {
		expr = star.X
	}
	switch e := expr.(type) {
	case *ast.Ident:
		return e.Name
	case *ast.SelectorExpr:
		return e.Sel.Name
	}
	return ""
}

// handChildWalks answers the lines of SRC that call GetChildren on anything
// but a getter in LISTS: a walk of a tree's children by hand.
func handChildWalks(t *testing.T, path string, src []byte, lists map[string]bool) []int {
	t.Helper()
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, path, src, parser.SkipObjectResolution)
	if err != nil {
		t.Fatalf("parse %s: %v", path, err)
	}
	var lines []int
	ast.Inspect(file, func(n ast.Node) bool {
		call, ok := n.(*ast.CallExpr)
		if !ok {
			return true
		}
		sel, ok := call.Fun.(*ast.SelectorExpr)
		if !ok || sel.Sel.Name != "GetChildren" {
			return true
		}
		if inner, ok := sel.X.(*ast.CallExpr); ok {
			if getter, ok := inner.Fun.(*ast.SelectorExpr); ok && lists[getter.Sel.Name] {
				return true
			}
		}
		lines = append(lines, fset.Position(call.Pos()).Line)
		return true
	})
	return lines
}

func TestHandChildWalksTellsAListFromATree(t *testing.T) {
	lists := map[string]bool{"GetHasOpenChildren": true}
	cases := []struct {
		name string
		src  string
		want int
	}{
		{name: "a refusal's child list is no walk", src: "for _, c := range resp.GetError().GetHasOpenChildren().GetChildren() { _ = c }", want: 0},
		{name: "a row's children read by hand are a walk", src: "for _, c := range row.GetChildren() { _ = c }", want: 1},
		{name: "a getter of a tree is a walk", src: "for _, c := range e.GetRow().GetChildren() { _ = c }", want: 1},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			src := "package p\nfunc f() {\n" + tc.src + "\n}\n"

			// Act
			got := handChildWalks(t, "p.go", []byte(src), lists)

			// Assert
			if len(got) != tc.want {
				t.Fatalf("hand walks at lines %v, want %d", got, tc.want)
			}
		})
	}
}

func TestChildGettersReadsTheGeneratedCode(t *testing.T) {
	// Arrange
	generated := filepath.Join(moduleRootDir(t), "proto", "gen")

	// Act
	got := childGetters(t, generated)

	// Assert: the refusals' child lists are lists; a roster row's getter is not.
	// GetDispatch also answers the deploy mediation's dispatch, which has no
	// children, so it is a list getter by the production dispatch's.
	for _, list := range []string{"GetHasOpenChildren", "GetHasChildren", "GetDispatch"} {
		if !got[list] {
			t.Errorf("%s is not read as a getter of a child list; got %v", list, got)
		}
	}
	for _, tree := range []string{"GetRow"} {
		if got[tree] {
			t.Errorf("%s is read as a getter of a child list, want a tree's", tree)
		}
	}
}
