package daemonrules

import (
	"go/ast"
	"go/token"
	"os"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalDaemonaddrDir is the package these rules hold, under the daemon module's root.
const internalDaemonaddrDir = "internal/daemonaddr"

// TestNoPackageHandRollsTheClientAddress pins the one choice of the address a
// client is told: every package that tells one calls ClientAddress, and none
// keeps a clientAddress of its own deciding it.
func TestNoPackageHandRollsTheClientAddress(t *testing.T) {
	sourcescan.InPackage(t, internalDaemonaddrDir)
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
			calls, ownDecider := 0, ""

			// Act
			sourcescan.EachSourceIn(t, tt.dir, false, func(fset *token.FileSet, f *ast.File) {
				ast.Inspect(f, func(n ast.Node) bool {
					if d, ok := n.(*ast.FuncDecl); ok && d.Name.Name == "clientAddress" {
						ownDecider = fset.Position(d.Pos()).String()
					}
					if sourcescan.IsSelector(n, "daemonaddr", "ClientAddress") {
						calls++
					}
					return true
				})
			})

			// Assert
			if ownDecider != "" || calls == 0 {
				t.Fatalf("own clientAddress at %q, %d daemonaddr.ClientAddress calls; want none of its own and at least one call", ownDecider, calls)
			}
		})
	}
}

// TestEveryServingHolderGoesThroughHoldServing pins that no path takes the
// serving lock by hand: one that did could keep a boot claim with no serving
// lock beside it, which every booting daemon reads as departing.
func TestEveryServingHolderGoesThroughHoldServing(t *testing.T) {
	sourcescan.InPackage(t, internalDaemonaddrDir)
	// Arrange.
	files := []string{"claim.go", "api.go"}
	for _, name := range files {
		// Act.
		raw, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("read %s: %v", name, err)
		}

		// Assert.
		if strings.Contains(string(raw), ".takeServing()") {
			t.Fatalf("%s takes the serving lock by hand; use holdServing", name)
		}
	}
}

// Every lock file this package opens goes through recordlock.OpenFile: a
// hand-rolled open here is a drifted copy, and closing one would drop a held
// record lock.
func TestEveryLockFileOpenGoesThroughRecordlock(t *testing.T) {
	sourcescan.InPackage(t, internalDaemonaddrDir)
	for _, name := range []string{"lock.go", "recordlock.go", "claim.go"} {
		body, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("read %s: %v", name, err)
		}
		for i, line := range strings.Split(string(body), "\n") {
			if strings.Contains(line, "os.OpenFile(") {
				t.Errorf("%s:%d opens a file by hand; use recordlock.OpenFile: %s", name, i+1, strings.TrimSpace(line))
			}
		}
	}
}
