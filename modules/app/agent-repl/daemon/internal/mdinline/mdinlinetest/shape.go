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
	"strings"
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

// HandWrittenLinks reads the Go source FILE and answers an error when an
// interpreted ("...") string literal of it spells part of a markdown link
// (`](`): a rewrite adds a link only through mdinline.WriteLink. Raw string
// literals are not read, since a regular expression's character class
// followed by a group spells the same bytes.
func HandWrittenLinks(file string) error {
	parsed, err := parser.ParseFile(token.NewFileSet(), file, nil, 0)
	if err != nil {
		return fmt.Errorf("mdinlinetest: parse %s: %w", file, err)
	}
	var found error
	ast.Inspect(parsed, func(n ast.Node) bool {
		if lit, ok := n.(*ast.BasicLit); ok && lit.Kind == token.STRING && strings.HasPrefix(lit.Value, `"`) && strings.Contains(lit.Value, "](") && found == nil {
			found = fmt.Errorf("mdinlinetest: %s spells a markdown link by hand (%s); write it through mdinline.WriteLink", file, lit.Value)
		}
		return true
	})
	return found
}

// AssertNoHandWrittenLinks fails T when FILE spells a markdown link by hand
// (HandWrittenLinks).
func AssertNoHandWrittenLinks(t testing.TB, file string) {
	t.Helper()
	if err := HandWrittenLinks(file); err != nil {
		t.Fatal(err)
	}
}
