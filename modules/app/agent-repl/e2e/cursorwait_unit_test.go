package e2e

import (
	"go/ast"
	"go/parser"
	"go/token"
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

func TestCursorIngested(t *testing.T) {
	tests := []struct {
		name    string
		cursors []*storev1.CursorState
		want    bool
	}{
		{name: "a cursor at the file's size has ingested it",
			cursors: []*storev1.CursorState{{Path: "/p/agent-1.jsonl", Offset: 40}}, want: true},
		{name: "a cursor past the file's size has ingested it",
			cursors: []*storev1.CursorState{{Path: "/p/agent-1.jsonl", Offset: 41}}, want: true},
		{name: "a cursor short of the file's size has not",
			cursors: []*storev1.CursorState{{Path: "/p/agent-1.jsonl", Offset: 39}}},
		{name: "no cursor for the file has not",
			cursors: nil},
		{name: "another file's cursor at the size does not count",
			cursors: []*storev1.CursorState{{Path: "/p/main.jsonl", Offset: 40}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := cursorIngested(tt.cursors, "/p/agent-1.jsonl", 40)

			// Assert.
			if got != tt.want {
				t.Fatalf("cursorIngested = %v, want %v", got, tt.want)
			}
		})
	}
}

// TestEveryCursorWaitGoesThroughAwaitCursors pins that each cursor wait hands
// its predicate to awaitCursors and polls nothing itself, so the bound, the
// interval and the failure message stay one loop's.
func TestEveryCursorWaitGoesThroughAwaitCursors(t *testing.T) {
	// Arrange.
	file, err := parser.ParseFile(token.NewFileSet(), "world_test.go", nil, 0)
	if err != nil {
		t.Fatalf("parse world_test.go: %v", err)
	}
	waits := map[string]bool{"awaitCursorAdvanceWithin": false, "awaitFileIngested": false}

	for _, decl := range file.Decls {
		fn, ok := decl.(*ast.FuncDecl)
		if !ok {
			continue
		}
		if _, isWait := waits[fn.Name.Name]; !isWait {
			continue
		}
		// Act.
		ast.Inspect(fn.Body, func(n ast.Node) bool {
			switch n := n.(type) {
			case *ast.ForStmt:
				// A predicate may range over the cursors it is handed; a
				// plain for loop is a poll of the wait's own.
				t.Errorf("%s polls in a loop of its own; hand a predicate to awaitCursors", fn.Name.Name)
			case *ast.CallExpr:
				if id, ok := n.Fun.(*ast.Ident); ok && id.Name == "awaitCursors" {
					waits[fn.Name.Name] = true
				}
			}
			return true
		})
	}

	// Assert.
	for name, calls := range waits {
		if !calls {
			t.Errorf("%s does not wait through awaitCursors", name)
		}
	}
}
