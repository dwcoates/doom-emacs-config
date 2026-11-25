package daemonrules

import (
	"go/ast"
	"go/parser"
	"go/token"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalProseDir is the package these rules hold, under the daemon module's root.
const internalProseDir = "internal/prose"

// TestEveryFencedTreeTestGoesThroughHoldsTree holds the package to one test of
// whether a fence wraps a tree: both places that ask call holdsTree, and only
// holdsTree reads a tree region for isMetapromptTree.
func TestEveryFencedTreeTestGoesThroughHoldsTree(t *testing.T) {
	sourcescan.InPackage(t, internalProseDir)
	// Arrange.
	fset := token.NewFileSet()
	calls := map[string]map[string]bool{}
	for _, src := range sourcescan.Production(t, internalProseDir) {
		f, err := parser.ParseFile(fset, src.Name, src.Source, 0)
		if err != nil {
			t.Fatalf("parse %s: %v", src.Name, err)
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
	for _, caller := range []string{"plainBlocks", "repairFencedResponseTree"} {
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
