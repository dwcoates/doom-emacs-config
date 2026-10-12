package promptqueue

import (
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/internal/ids"
)

func TestTurnInFlightAnswersTheWatchersTurnInFlight(t *testing.T) {
	tests := []struct {
		name      string
		arrange   func(h *harness)
		want      ids.TurnID
		wantFound bool
	}{
		{name: "a turn in flight", arrange: func(h *harness) { h.watcher.running("running-turn") }, want: "running-turn", wantFound: true},
		{name: "no turn in flight", arrange: func(h *harness) { h.watcher.idle() }, want: "", wantFound: false},
		{name: "no watcher", arrange: func(h *harness) { h.watcher.running("running-turn"); h.noWatcher = true }, want: "", wantFound: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tt.arrange(h)

			// Act
			got, found := h.q.turnInFlight(theWorkspace)

			// Assert
			if got != tt.want || found != tt.wantFound {
				t.Fatalf("turnInFlight = (%q, %v), want (%q, %v)", got, found, tt.want, tt.wantFound)
			}
		})
	}
}

// EVERY "IS A TURN IN FLIGHT" READ OF THE WATCHER GOES THROUGH turnInFlight.
// This scans the package's production sources and fails an `if` that looks
// the watcher up in its init and reads TurnInFlight in its condition: the
// hand-rolled shape turnInFlight replaced.
func TestNoSiteHandRollsTheTurnInFlightLookup(t *testing.T) {
	// Arrange
	sources, err := filepath.Glob("*.go")
	if err != nil {
		t.Fatalf("Glob: %v", err)
	}
	fset := token.NewFileSet()
	var violations []string

	// Act
	for _, path := range sources {
		if strings.HasSuffix(path, "_test.go") {
			continue
		}
		src, err := os.ReadFile(path)
		if err != nil {
			t.Fatalf("ReadFile(%s): %v", path, err)
		}
		file, err := parser.ParseFile(fset, path, src, 0)
		if err != nil {
			t.Fatalf("ParseFile(%s): %v", path, err)
		}
		ast.Inspect(file, func(n ast.Node) bool {
			stmt, ok := n.(*ast.IfStmt)
			if ok && stmt.Init != nil && calls(stmt.Init, "Watcher") && calls(stmt.Cond, "TurnInFlight") {
				violations = append(violations, fset.Position(stmt.Pos()).String())
			}
			return true
		})
	}

	// Assert
	if len(violations) > 0 {
		t.Fatalf("hand-rolled turn-in-flight lookups, use q.turnInFlight:\n%s", strings.Join(violations, "\n"))
	}
}

// calls reports whether NODE calls a method named NAME.
func calls(node ast.Node, name string) bool {
	found := false
	ast.Inspect(node, func(n ast.Node) bool {
		if call, ok := n.(*ast.CallExpr); ok {
			if sel, ok := call.Fun.(*ast.SelectorExpr); ok && sel.Sel.Name == name {
				found = true
			}
		}
		return !found
	})
	return found
}
