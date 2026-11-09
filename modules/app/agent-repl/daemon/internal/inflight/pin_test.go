package inflight

import (
	"go/ast"
	"go/token"
	"os"
	"strings"
	"testing"

	"agent-repld/internal/sourcescan"
)

// joinedMark is the comment that must stand directly above a bare `go`
// whose goroutine the starting function waits for before it returns.
const joinedMark = "// inflight: joined"

// TestEveryGoroutineIsCountedStandingOrJoined pins the tracker's guarantee:
// no production code starts a goroutine the count does not know about. Each
// is begun through Go (counted), Standing (a long-running loop, said so), or
// is a bare `go` marked joined: its starter waits for it, so its work is the
// starter's own.
func TestEveryGoroutineIsCountedStandingOrJoined(t *testing.T) {
	// Arrange
	root := sourcescan.DaemonRoot(t)
	var offenders []string
	lines := map[string][]string{}

	// Act
	sourcescan.EachDaemonSource(t, root, []string{"internal/inflight"}, func(fset *token.FileSet, file *ast.File) {
		ast.Inspect(file, func(n ast.Node) bool {
			stmt, ok := n.(*ast.GoStmt)
			if !ok {
				return true
			}
			pos := fset.Position(stmt.Pos())
			src, ok := lines[pos.Filename]
			if !ok {
				body, err := os.ReadFile(pos.Filename)
				if err != nil {
					t.Fatalf("read %s: %v", pos.Filename, err)
				}
				src = strings.Split(string(body), "\n")
				lines[pos.Filename] = src
			}
			if pos.Line >= 2 && strings.Contains(src[pos.Line-2], joinedMark) {
				return true
			}
			offenders = append(offenders, pos.String())
			return true
		})
	})

	// Assert
	for _, o := range offenders {
		t.Errorf("a goroutine the tracker does not know about; start it with inflight.Go or inflight.Standing, or mark it %q: %s", joinedMark, o)
	}
}
