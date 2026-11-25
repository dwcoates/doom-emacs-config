package daemonrules

import (
	"go/ast"
	"go/token"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalGitclientDir is the package these rules hold, under the daemon module's root.
const internalGitclientDir = "internal/gitclient"

// TestNoDaemonSourceHandRollsStoppedBy pins the stopped-step classification to
// StoppedBy: a production `ctx.Err() != nil || gitclient.IsCancelled(err)`
// beside it is a copy that can drift from it, and fails here.
func TestNoDaemonSourceHandRollsStoppedBy(t *testing.T) {
	sourcescan.InPackage(t, internalGitclientDir)
	// Arrange.
	var sites []string

	// Act.
	sourcescan.EachDaemonSource(t, sourcescan.DaemonRoot(t), []string{"internal/gitclient"}, func(fset *token.FileSet, file *ast.File) {
		ast.Inspect(file, func(n ast.Node) bool {
			or, ok := n.(*ast.BinaryExpr)
			if !ok || or.Op != token.LOR {
				return true
			}
			if (isCtxErrCheck(or.X) && isIsCancelledCall(or.Y)) || (isIsCancelledCall(or.X) && isCtxErrCheck(or.Y)) {
				sites = append(sites, fset.Position(or.Pos()).String())
			}
			return true
		})
	})

	// Assert.
	if len(sites) != 0 {
		t.Fatalf("a hand-rolled ctx.Err()-or-IsCancelled classification is at %v; call gitclient.StoppedBy", sites)
	}
}

// isCtxErrCheck reports whether EXPR is `<x>.Err() != nil`.
func isCtxErrCheck(expr ast.Expr) bool {
	cmp, ok := expr.(*ast.BinaryExpr)
	if !ok || cmp.Op != token.NEQ {
		return false
	}
	call, ok := cmp.X.(*ast.CallExpr)
	if !ok {
		return false
	}
	sel, ok := call.Fun.(*ast.SelectorExpr)
	return ok && sel.Sel.Name == "Err" && len(call.Args) == 0
}

// isIsCancelledCall reports whether EXPR is a call to gitclient.IsCancelled.
func isIsCancelledCall(expr ast.Expr) bool {
	call, ok := expr.(*ast.CallExpr)
	if !ok {
		return false
	}
	sel, ok := call.Fun.(*ast.SelectorExpr)
	if !ok || sel.Sel.Name != "IsCancelled" {
		return false
	}
	pkg, ok := sel.X.(*ast.Ident)
	return ok && pkg.Name == "gitclient"
}

// TestEveryFormattedOutputIsSplitByFormattedRecords holds the call sites to
// the one parser: no other code in the package splits a line by fieldSep.
func TestEveryFormattedOutputIsSplitByFormattedRecords(t *testing.T) {
	sourcescan.InPackage(t, internalGitclientDir)
	// Arrange, Act.
	n := sourcescan.Count(t, internalGitclientDir, "strings.Split(line, fieldSep)")

	// Assert.
	if n != 1 {
		t.Fatalf("lines split by fieldSep %d times in the package, want exactly one: formattedRecords's own", n)
	}
}
