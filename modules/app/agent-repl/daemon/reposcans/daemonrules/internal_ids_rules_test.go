package daemonrules

import (
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalIdsDir is the package these rules hold, under the daemon module's root.
const internalIdsDir = "internal/ids"

// TestNoPackageHandRollsOptional fails a non-test source that spells
// an optional id by hand instead of calling Optional.
func TestNoPackageHandRollsOptional(t *testing.T) {
	sourcescan.InPackage(t, internalIdsDir)
	// Arrange
	handRolled := regexp.MustCompile(`if (\w+(\.\w+)?) == nil \{\s*return ""\s*\}\s*return string\(\*(\w+(\.\w+)?)\)`)
	var offenders []string

	// Act
	err := filepath.WalkDir("..", func(path string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return err
		}
		raw, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		for _, m := range handRolled.FindAllStringSubmatch(string(raw), -1) {
			if m[1] == m[3] && !strings.HasSuffix(path, "ids/ids.go") {
				offenders = append(offenders, path)
			}
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk the daemon's packages: %v", err)
	}
	if len(offenders) != 0 {
		t.Fatalf("hand-rolled optional ids in %v; call ids.Optional", offenders)
	}
}

// TestNoPackageHandRollsStrings fails a non-test source that spells a slice
// of string-kinded values as strings with its own loop -- a range whose whole
// body is `out = append(out, string(v))` -- instead of calling Strings.
func TestNoPackageHandRollsStrings(t *testing.T) {
	sourcescan.InPackage(t, internalIdsDir)
	// Arrange
	var offenders []string
	fset := token.NewFileSet()

	// Act
	err := filepath.WalkDir("..", func(path string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") || strings.HasSuffix(path, "ids/ids.go") {
			return err
		}
		file, err := parser.ParseFile(fset, path, nil, 0)
		if err != nil {
			return err
		}
		ast.Inspect(file, func(n ast.Node) bool {
			if loop, ok := n.(*ast.RangeStmt); ok && spellsStrings(loop) {
				offenders = append(offenders, fset.Position(loop.Pos()).String())
			}
			return true
		})
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk the daemon's packages: %v", err)
	}
	if len(offenders) != 0 {
		t.Fatalf("hand-rolled string spellings at %v; call ids.Strings", offenders)
	}
}

// spellsStrings reports whether LOOP is `for _, v := range xs { out =
// append(out, string(v)) }`.
func spellsStrings(loop *ast.RangeStmt) bool {
	key, keyOK := loop.Key.(*ast.Ident)
	value, valueOK := loop.Value.(*ast.Ident)
	if !keyOK || key.Name != "_" || !valueOK || len(loop.Body.List) != 1 {
		return false
	}
	assign, ok := loop.Body.List[0].(*ast.AssignStmt)
	if !ok || len(assign.Lhs) != 1 || len(assign.Rhs) != 1 {
		return false
	}
	call, ok := assign.Rhs[0].(*ast.CallExpr)
	if !ok || len(call.Args) != 2 {
		return false
	}
	if fn, isIdent := call.Fun.(*ast.Ident); !isIdent || fn.Name != "append" {
		return false
	}
	conv, ok := call.Args[1].(*ast.CallExpr)
	if !ok || len(conv.Args) != 1 {
		return false
	}
	fn, isIdent := conv.Fun.(*ast.Ident)
	arg, argIdent := conv.Args[0].(*ast.Ident)
	return isIdent && fn.Name == "string" && argIdent && arg.Name == value.Name
}
