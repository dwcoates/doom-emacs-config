package promptqueue

import (
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
)

// THE DOOR GUARD. A turn row closes only through the prompt queue's door
// (turnclose.go), which draws the turn's ending with the close. This scans
// every production source of the daemon and fails:
//
//   - a call of CloseTurn, CloseOrphans or ClaimDisplacedTurn outside the door
//     whose receiver is not the queue (`…Queue.` anywhere, or the queue's own
//     `q.` inside this package) — i.e. a direct store close;
//   - an SQL statement that writes `closed_at` or `close_kind` anywhere but
//     the three store functions the door calls.

// doorMethods are the store's three ways to close a turn row.
var doorMethods = map[string]bool{"CloseTurn": true, "CloseOrphans": true, "ClaimDisplacedTurn": true}

// doorFile is the one production file that may call the store's closes.
const doorFile = "internal/promptqueue/turnclose.go"

// storeCloseFuncs are the store functions that may hold a closing statement.
var storeCloseFuncs = map[string]bool{"CloseTurn": true, "CloseOrphans": true, "ClaimDisplacedTurn": true}

// doorViolations reports every way FILE (at daemon-relative PATH, in package
// PKG) closes a turn outside the door.
func doorViolations(fset *token.FileSet, file *ast.File, path string) []string {
	var out []string
	pkg := file.Name.Name
	ast.Inspect(file, func(n ast.Node) bool {
		call, ok := n.(*ast.CallExpr)
		if !ok {
			return true
		}
		sel, ok := call.Fun.(*ast.SelectorExpr)
		if !ok || !doorMethods[sel.Sel.Name] || path == doorFile || pkg == "wsm" {
			return true
		}
		if throughQueue(sel.X, pkg) {
			return true
		}
		out = append(out, fset.Position(call.Pos()).String()+": "+sel.Sel.Name+" closes a turn outside the prompt queue's door")
		return true
	})
	for _, decl := range file.Decls {
		fn, isFunc := decl.(*ast.FuncDecl)
		allowed := isFunc && pkg == "wsm" && filepath.Base(path) == "turns.go" && storeCloseFuncs[fn.Name.Name]
		ast.Inspect(decl, func(n ast.Node) bool {
			lit, ok := n.(*ast.BasicLit)
			if !ok || lit.Kind != token.STRING {
				return true
			}
			text, err := strconv.Unquote(lit.Value)
			if err != nil {
				text = lit.Value
			}
			if closesARow(text) && !allowed {
				out = append(out, fset.Position(lit.Pos()).String()+": a statement writes a turn's close outside the store's door functions")
			}
			return true
		})
	}
	return out
}

// throughQueue reports whether a close's receiver is the prompt queue: a
// selector ending in `Queue`, or this package's own queue receiver `q`.
func throughQueue(x ast.Expr, pkg string) bool {
	switch recv := x.(type) {
	case *ast.SelectorExpr:
		return recv.Sel.Name == "Queue"
	case *ast.Ident:
		return pkg == "promptqueue" && recv.Name == "q"
	}
	return false
}

// closesARow reports whether an SQL text writes a turn's close.
func closesARow(text string) bool {
	squeezed := strings.Join(strings.Fields(text), " ")
	return strings.Contains(squeezed, "closed_at = ") || strings.Contains(squeezed, "close_kind = ")
}

func TestNoProductionCodeClosesATurnOutsideTheDoor(t *testing.T) {
	// Arrange
	root, err := filepath.Abs(filepath.Join("..", ".."))
	if err != nil {
		t.Fatalf("daemon root: %v", err)
	}
	fset := token.NewFileSet()
	var violations []string
	scanned := 0

	// Act
	err = filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			switch d.Name() {
			case "testdata", "integration", "node_modules":
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		rel, err := filepath.Rel(root, path)
		if err != nil {
			return err
		}
		file, err := parser.ParseFile(fset, path, nil, 0)
		if err != nil {
			return err
		}
		scanned++
		violations = append(violations, doorViolations(fset, file, filepath.ToSlash(rel))...)
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("scan: %v", err)
	}
	if scanned == 0 {
		t.Fatal("the guard scanned no production source")
	}
	if len(violations) > 0 {
		t.Fatalf("turn rows closed outside the door:\n%s", strings.Join(violations, "\n"))
	}
}

func TestTheDoorGuard(t *testing.T) {
	tests := []struct {
		name string
		path string
		src  string
		want int
	}{
		{
			name: "a direct store close is caught",
			path: "internal/boot/sequence.go",
			src:  "package boot\nfunc f() { s.deps.DB.CloseOrphans(ctx, ws, at) }\n",
			want: 1,
		},
		{
			name: "a store close through a local handle is caught",
			path: "internal/merge/run.go",
			src:  "package merge\nfunc f() { db.ClaimDisplacedTurn(ctx, turn, at) }\n",
			want: 1,
		},
		{
			name: "a close through the queue is the door",
			path: "internal/workspace/teardown.go",
			src:  "package workspace\nfunc f() { v.deps.Queue.CloseOrphans(ctx, ws, at) }\n",
			want: 0,
		},
		{
			name: "the queue's own door method is the door",
			path: "internal/promptqueue/lifecycle.go",
			src:  "package promptqueue\nfunc f() { q.CloseOrphans(ctx, ws, at) }\n",
			want: 0,
		},
		{
			name: "a store close inside the door is allowed",
			path: doorFile,
			src:  "package promptqueue\nfunc f() { q.deps.DB.CloseTurn(ctx, turn, at, how) }\n",
			want: 0,
		},
		{
			name: "a closing statement outside the store's door functions is caught",
			path: "internal/wsm/turns.go",
			src:  "package wsm\nfunc PutTurn() { exec(`UPDATE turns SET closed_at = ?, close_kind = ? WHERE id = ?`) }\n",
			want: 1,
		},
		{
			name: "a closing statement in another package is caught",
			path: "internal/drain/drain.go",
			src:  "package drain\nfunc f() { exec(\"UPDATE turns SET close_kind = 3\") }\n",
			want: 1,
		},
		{
			name: "a closing statement inside the store's CloseTurn is allowed",
			path: "internal/wsm/turns.go",
			src:  "package wsm\nfunc CloseTurn() { exec(`UPDATE turns SET closed_at = ?, close_kind = ? WHERE id = ?`) }\n",
			want: 0,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			fset := token.NewFileSet()
			file, err := parser.ParseFile(fset, tc.path, tc.src, 0)
			if err != nil {
				t.Fatalf("parse: %v", err)
			}

			// Act
			got := doorViolations(fset, file, tc.path)

			// Assert
			if len(got) != tc.want {
				t.Fatalf("violations = %v, want %d", got, tc.want)
			}
		})
	}
}
