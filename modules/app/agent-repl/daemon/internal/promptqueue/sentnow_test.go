package promptqueue

import (
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestNoteSentNowStandingClearsTheReplaysShortcut(t *testing.T) {
	// Arrange: the last replay found no "Send now" waiting.
	h := newHarness(t)
	h.q.mu.Lock()
	h.q.stateLocked(theWorkspace).sentNowNone = true
	h.q.mu.Unlock()

	// Act
	h.q.noteSentNowStanding(theWorkspace)

	// Assert
	h.q.mu.Lock()
	defer h.q.mu.Unlock()
	if h.q.stateLocked(theWorkspace).sentNowNone {
		t.Fatal("sentNowNone = true, want the next replay to take the mark")
	}
}

// TestOnlyNoteSentNowStandingClearsTheReplaysShortcut scans the package's
// production sources: a site that clears sentNowNone by hand, rather than
// through noteSentNowStanding, fails here.
func TestOnlyNoteSentNowStandingClearsTheReplaysShortcut(t *testing.T) {
	// Arrange
	paths, err := filepath.Glob("*.go")
	if err != nil {
		t.Fatalf("Glob: %v", err)
	}
	fset := token.NewFileSet()
	var offenders []string

	// Act
	for _, path := range paths {
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
		for _, decl := range file.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Name.Name == "noteSentNowStanding" || fn.Body == nil {
				continue
			}
			ast.Inspect(fn.Body, func(n ast.Node) bool {
				assign, ok := n.(*ast.AssignStmt)
				if !ok {
					return true
				}
				for i, lhs := range assign.Lhs {
					sel, ok := lhs.(*ast.SelectorExpr)
					if !ok || sel.Sel.Name != "sentNowNone" || i >= len(assign.Rhs) {
						continue
					}
					if ident, ok := assign.Rhs[i].(*ast.Ident); ok && ident.Name == "false" {
						offenders = append(offenders, fset.Position(assign.Pos()).String())
					}
				}
				return true
			})
		}
	}

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("sentNowNone cleared outside noteSentNowStanding at %v", offenders)
	}
}
