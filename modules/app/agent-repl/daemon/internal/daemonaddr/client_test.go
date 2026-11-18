package daemonaddr

import (
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestClientAddressNamesTheSharedAddressWheneverOneIsShared(t *testing.T) {
	tests := []struct {
		name    string
		shared  string
		private string
		want    string
	}{
		{name: "a shared address is told in place of the private one", shared: "127.0.0.1:1", private: "127.0.0.1:2", want: "127.0.0.1:1"},
		{name: "with no shared address the private one is told", private: "127.0.0.1:2", want: "127.0.0.1:2"},
		{name: "no daemon is told as none, even with a shared address", shared: "127.0.0.1:1", want: ""},
		{name: "no daemon and no shared address is told as none", want: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := ClientAddress(tt.shared, tt.private)

			// Assert
			if got != tt.want {
				t.Fatalf("ClientAddress(%q, %q) = %q, want %q", tt.shared, tt.private, got, tt.want)
			}
		})
	}
}

// TestNoPackageHandRollsTheClientAddress pins the one choice of the address a
// client is told: every package that tells one calls ClientAddress, and none
// keeps a clientAddress of its own deciding it.
func TestNoPackageHandRollsTheClientAddress(t *testing.T) {
	tests := []struct {
		name string
		dir  string
	}{
		{name: "the server's refusals", dir: "../server"},
		{name: "the rollout's announcements and transfer pushes", dir: "../rollout"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			entries, err := os.ReadDir(tt.dir)
			if err != nil {
				t.Fatal(err)
			}
			fset := token.NewFileSet()
			calls, ownDecider := 0, ""

			// Act
			for _, e := range entries {
				if !strings.HasSuffix(e.Name(), ".go") || strings.HasSuffix(e.Name(), "_test.go") {
					continue
				}
				f, err := parser.ParseFile(fset, filepath.Join(tt.dir, e.Name()), nil, 0)
				if err != nil {
					t.Fatal(err)
				}
				ast.Inspect(f, func(n ast.Node) bool {
					switch n := n.(type) {
					case *ast.FuncDecl:
						if n.Name.Name == "clientAddress" {
							ownDecider = fset.Position(n.Pos()).String()
						}
					case *ast.SelectorExpr:
						if id, ok := n.X.(*ast.Ident); ok && id.Name == "daemonaddr" && n.Sel.Name == "ClientAddress" {
							calls++
						}
					}
					return true
				})
			}

			// Assert
			if ownDecider != "" || calls == 0 {
				t.Fatalf("own clientAddress at %q, %d daemonaddr.ClientAddress calls; want none of its own and at least one call", ownDecider, calls)
			}
		})
	}
}
