package mdfence

import (
	"go/ast"
	"go/token"
	"strconv"
	"strings"
	"testing"

	"agent-repld/internal/sourcescan"
)

// TestNoDaemonSourceReadsAFenceByHand holds every fence delimiter the daemon
// reads to the length rule Parse, Opens and Closes keep: a strings call handed
// a literal opening on ``` or ~~~ (strings.HasPrefix(line, "```"), say) reads
// a fence by hand, and toggles where the rule would read content.
func TestNoDaemonSourceReadsAFenceByHand(t *testing.T) {
	// Arrange.
	var sites []string

	// Act.
	sourcescan.EachDaemonSource(t, sourcescan.DaemonRoot(t), []string{"internal/mdfence"}, func(fset *token.FileSet, file *ast.File) {
		ast.Inspect(file, func(n ast.Node) bool {
			call, ok := n.(*ast.CallExpr)
			if !ok {
				return true
			}
			sel, ok := call.Fun.(*ast.SelectorExpr)
			if !ok {
				return true
			}
			if pkg, ok := sel.X.(*ast.Ident); !ok || pkg.Name != "strings" {
				return true
			}
			for _, arg := range call.Args {
				lit, ok := arg.(*ast.BasicLit)
				if !ok || lit.Kind != token.STRING {
					continue
				}
				if s, err := strconv.Unquote(lit.Value); err == nil && (strings.HasPrefix(s, "```") || strings.HasPrefix(s, "~~~")) {
					sites = append(sites, fset.Position(call.Pos()).String())
				}
			}
			return true
		})
	})

	// Assert.
	if len(sites) != 0 {
		t.Errorf("these calls read a fence by hand; read it through mdfence.Parse: %v", sites)
	}
}
