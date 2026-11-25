package daemonrules

import (
	"go/ast"
	"go/token"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalTransportcutDir is the package these rules hold, under the daemon module's root.
const internalTransportcutDir = "internal/transportcut"

// TestNoOtherPackageReadsACutStreamByHand pins Is as the one judgement: a
// daemon source outside this package that reads io.ErrUnexpectedEOF is
// judging a cut link by hand, and drifts from every other client.
func TestNoOtherPackageReadsACutStreamByHand(t *testing.T) {
	sourcescan.InPackage(t, internalTransportcutDir)
	// Arrange
	var sites []string

	// Act
	sourcescan.EachDaemonSource(t, sourcescan.DaemonRoot(t), []string{"internal/transportcut"}, func(fset *token.FileSet, file *ast.File) {
		ast.Inspect(file, func(n ast.Node) bool {
			sel, ok := n.(*ast.SelectorExpr)
			if ok && sel.Sel.Name == "ErrUnexpectedEOF" {
				if pkg, ok := sel.X.(*ast.Ident); ok && pkg.Name == "io" {
					sites = append(sites, fset.Position(sel.Pos()).String())
				}
			}
			return true
		})
	})

	// Assert
	if len(sites) != 0 {
		t.Fatalf("io.ErrUnexpectedEOF is read outside transportcut at %v; judge a cut link with transportcut.Is", sites)
	}
}
