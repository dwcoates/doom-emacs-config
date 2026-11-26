package daemonrules

import (
	"go/ast"
	"go/token"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalRolloutDir is the package these rules hold, under the daemon module's root.
const internalRolloutDir = "internal/rollout"

// TestControllerLockIsReleasedOnlyThroughUnlock pins that no production file
// releases the controller's lock around unlock, which is the one place a
// change of ServesIntake's answer is signaled.
func TestControllerLockIsReleasedOnlyThroughUnlock(t *testing.T) {
	sourcescan.InPackage(t, internalRolloutDir)
	// Arrange
	files := sourcescan.Production(t, internalRolloutDir)

	for _, f := range files {
		t.Run(f.Name, func(t *testing.T) {
			// Act
			got := strings.Count(string(f.Source), "c.mu.Unlock()")

			// Assert: unlock itself holds the one direct release.
			want := 0
			if f.Name == "standing.go" {
				want = 1
			}
			if got != want {
				t.Fatalf("%s releases c.mu directly %d times, want %d; release it through c.unlock()", f.Name, got, want)
			}
		})
	}
}

// EVERY TEST ARRANGES A LOCK THROUGH setLock: a hand-rolled write to the
// harness's lock table skips its lock, and drifts from the one shape.
func TestEveryLockArrangedGoesThroughSetLock(t *testing.T) {
	sourcescan.InPackage(t, internalRolloutDir)
	var stray []string
	sourcescan.EachSourceIn(t, ".", true, func(fset *token.FileSet, file *ast.File) {
		for _, decl := range file.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Name.Name == "setLock" || fn.Body == nil {
				continue
			}
			ast.Inspect(fn.Body, func(n ast.Node) bool {
				assign, ok := n.(*ast.AssignStmt)
				if !ok {
					return true
				}
				for _, lhs := range assign.Lhs {
					index, ok := lhs.(*ast.IndexExpr)
					if !ok {
						continue
					}
					if sel, ok := index.X.(*ast.SelectorExpr); ok && sel.Sel.Name == "lockStates" {
						stray = append(stray, fset.Position(assign.Pos()).String())
					}
				}
				return true
			})
		}
	})
	if len(stray) != 0 {
		t.Fatalf("locks arranged outside setLock at %v", stray)
	}
}
