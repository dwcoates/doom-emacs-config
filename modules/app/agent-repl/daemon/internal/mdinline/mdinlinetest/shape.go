// Package mdinlinetest holds what a rewrite of drawn prose's own tests assert
// about its shape: that it walks prose through mdinline.RewriteProse, THE ONE
// WALK, rather than a hand-rolled walk of mdinline.Read's lines that could
// drift from it.
package mdinlinetest

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"testing"
)

// RewriteShape reads function FN of the Go source FILE and answers an error
// unless it calls mdinline.RewriteProse and never calls mdinline.Read.
func RewriteShape(file, fn string) error {
	parsed, err := parser.ParseFile(token.NewFileSet(), file, nil, 0)
	if err != nil {
		return fmt.Errorf("mdinlinetest: parse %s: %w", file, err)
	}
	var body *ast.BlockStmt
	for _, decl := range parsed.Decls {
		if f, ok := decl.(*ast.FuncDecl); ok && f.Recv == nil && f.Name.Name == fn {
			body = f.Body
		}
	}
	if body == nil {
		return fmt.Errorf("mdinlinetest: %s declares no function %s", file, fn)
	}
	walks, reads := false, false
	ast.Inspect(body, func(n ast.Node) bool {
		sel, ok := n.(*ast.SelectorExpr)
		if !ok {
			return true
		}
		if pkg, ok := sel.X.(*ast.Ident); ok && pkg.Name == "mdinline" {
			walks = walks || sel.Sel.Name == "RewriteProse"
			reads = reads || sel.Sel.Name == "Read"
		}
		return true
	})
	switch {
	case reads:
		return fmt.Errorf("mdinlinetest: %s.%s walks mdinline.Read's lines itself; it must rewrite prose through mdinline.RewriteProse", file, fn)
	case !walks:
		return fmt.Errorf("mdinlinetest: %s.%s never calls mdinline.RewriteProse", file, fn)
	}
	return nil
}

// AssertRewriteShape fails T unless FN of FILE has the rewrite shape
// (RewriteShape).
func AssertRewriteShape(t testing.TB, file, fn string) {
	t.Helper()
	if err := RewriteShape(file, fn); err != nil {
		t.Fatal(err)
	}
}
