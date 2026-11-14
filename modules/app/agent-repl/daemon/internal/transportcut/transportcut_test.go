package transportcut

import (
	"errors"
	"fmt"
	"go/ast"
	"go/token"
	"io"
	"testing"

	"agent-repld/internal/sourcescan"

	"connectrpc.com/connect"
)

// TestIs pins which failures can be the link breaking under a call: only
// those this side read off a cut stream, never an answer the peer sent.
func TestIs(t *testing.T) {
	tests := []struct {
		name string
		err  error
		want bool
	}{
		{name: "unavailable", err: connect.NewError(connect.CodeUnavailable, errors.New("unexpected EOF")), want: true},
		{name: "a response cut partway through an envelope",
			err: connect.NewError(connect.CodeInvalidArgument, fmt.Errorf("protocol error: incomplete envelope: %w", io.ErrUnexpectedEOF)), want: true},
		{name: "an invalid_argument with no cut beneath it", err: connect.NewError(connect.CodeInvalidArgument, errors.New("bad request")), want: false},
		{name: "a unary response cut partway through its body",
			err: connect.NewError(connect.CodeUnknown, fmt.Errorf("read message: %w", io.ErrUnexpectedEOF)), want: true},
		{name: "an unknown naming an unexpected EOF with no cut beneath it", err: connect.NewError(connect.CodeUnknown, errors.New("read message: unexpected EOF")), want: false},
		{name: "a refusal the peer answered", err: connect.NewError(connect.CodeInternal, errors.New("the fold came apart")), want: false},
		{name: "not found", err: connect.NewError(connect.CodeNotFound, io.ErrUnexpectedEOF), want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := Is(tt.err)

			// Assert
			if got != tt.want {
				t.Fatalf("Is(%v) = %v, want %v", tt.err, got, tt.want)
			}
		})
	}
}

// TestNoOtherPackageReadsACutStreamByHand pins Is as the one judgement: a
// daemon source outside this package that reads io.ErrUnexpectedEOF is
// judging a cut link by hand, and drifts from every other client.
func TestNoOtherPackageReadsACutStreamByHand(t *testing.T) {
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
