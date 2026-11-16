package gitclient

import (
	"context"
	"errors"
	"fmt"
	"go/ast"
	"go/token"
	"testing"

	"agent-repld/internal/sourcescan"
)

func TestStoppedBy(t *testing.T) {
	cancelledGit := &Cancelled{Args: []string{"show-ref"}, Dir: "/repo", Cause: context.Canceled}
	tests := []struct {
		name      string
		cancelCtx bool
		err       error
		want      bool
	}{
		{name: "a git the daemon cancelled under a live context is stopped", err: cancelledGit, want: true},
		{name: "a wrapped cancelled git is stopped", err: fmt.Errorf("retire: %w", cancelledGit), want: true},
		{name: "any error under a done context is stopped", cancelCtx: true, err: errors.New("boom"), want: true},
		{name: "a failure under a live context is not stopped", err: errors.New("boom"), want: false},
		{name: "a failed git under a live context is not stopped", err: &Error{Args: []string{"show-ref"}, ExitCode: 128}, want: false},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange.
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			if test.cancelCtx {
				cancel()
			}

			// Act.
			got := StoppedBy(ctx, test.err)

			// Assert.
			if got != test.want {
				t.Fatalf("StoppedBy(%v) = %t, want %t", test.err, got, test.want)
			}
		})
	}
}

// TestNoDaemonSourceHandRollsStoppedBy pins the stopped-step classification to
// StoppedBy: a production `ctx.Err() != nil || gitclient.IsCancelled(err)`
// beside it is a copy that can drift from it, and fails here.
func TestNoDaemonSourceHandRollsStoppedBy(t *testing.T) {
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
