package daemonrules

import (
	"go/ast"
	"go/token"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalKeyedmutexDir is the package these rules hold, under the daemon module's root.
const internalKeyedmutexDir = "internal/keyedmutex"

// TestEveryPerKeyMutexIsASet fails on every production source outside
// keyedmutex that declares a map whose values are *sync.Mutex: a per-key lock
// is a keyedmutex.Set, so one that mints or keeps its locks differently fails
// here rather than drifting silently.
func TestEveryPerKeyMutexIsASet(t *testing.T) {
	// Arrange
	root := sourcescan.DaemonRoot(t)
	var offenders []string

	// Act
	sourcescan.EachDaemonSource(t, root, []string{internalKeyedmutexDir}, func(fset *token.FileSet, file *ast.File) {
		ast.Inspect(file, func(n ast.Node) bool {
			m, ok := n.(*ast.MapType)
			if !ok {
				return true
			}
			if star, ok := m.Value.(*ast.StarExpr); ok && sourcescan.IsSelector(star.X, "sync", "Mutex") {
				offenders = append(offenders, fset.Position(m.Pos()).String())
			}
			return true
		})
	})

	// Assert
	if len(offenders) > 0 {
		t.Fatalf("these sources keep their own map of mutexes; use a keyedmutex.Set: %v", offenders)
	}
}
