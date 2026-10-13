package daemon_test

import (
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"path/filepath"
	"strings"
	"testing"
)

// TestNoTestHelperHandRollsARecordLookup guards dlog.FindRecord and
// dlog.FilterRecords as the ONE record lookup the daemon's tests share. A
// helper over dlog records that loops comparing their Level, Operation or
// Message against its own parameters is a hand-rolled copy of them: it
// delegates instead, so the matching rule lives in one place. Helpers over
// other record types (the integration harness's parsed log files, a test's
// own recording logger) are not dlog's to serve and are not judged.
func TestNoTestHelperHandRollsARecordLookup(t *testing.T) {
	// Arrange.
	files := token.NewFileSet()
	var copies []string

	// Act.
	for _, root := range []string{"cmd", "internal", "integration"} {
		err := filepath.WalkDir(root, func(path string, entry fs.DirEntry, walkErr error) error {
			if walkErr != nil {
				return walkErr
			}
			if entry.IsDir() || !strings.HasSuffix(path, "_test.go") {
				return nil
			}
			parsed, err := parser.ParseFile(files, path, nil, 0)
			if err != nil {
				return err
			}
			for _, decl := range parsed.Decls {
				fn, ok := decl.(*ast.FuncDecl)
				if ok && fn.Body != nil && handRollsRecordLookup(fn) {
					copies = append(copies, path+": "+fn.Name.Name)
				}
			}
			return nil
		})
		if err != nil {
			t.Fatalf("walk %s: %v", root, err)
		}
	}

	// Assert.
	if len(copies) != 0 {
		t.Fatalf("test helpers hand-roll a record lookup; delegate to dlog.FindRecord instead:\n%s", strings.Join(copies, "\n"))
	}
}

func TestTheRecordLookupGuardRecognizesAHandRolledCopy(t *testing.T) {
	tests := []struct {
		name string
		src  string
		want bool
	}{
		{
			name: "a loop matching the helper's own parameters is a copy",
			src: `package p
func find(records []dlog.Record, level, op string) bool {
	for _, r := range records {
		if r.Level == level && r.Operation == op {
			return true
		}
	}
	return false
}`,
			want: true,
		},
		{
			name: "a delegation to dlog.FindRecord is not",
			src: `package p
func find(records []dlog.Record, level, op string) bool {
	_, ok := dlog.FindRecord(records, dlog.RecordQuery{Level: level, Operation: op})
	return ok
}`,
			want: false,
		},
		{
			name: "a test's inline loop against literals is not a helper",
			src: `package p
func TestX(t *testing.T) {
	for _, r := range records {
		if r.Level == "error" && r.Context["cause"] == "x" {
			return
		}
	}
}`,
			want: false,
		},
		{
			name: "a loop that also matches by substring is its own rule, not a copy",
			src: `package p
func logged(records []dlog.Record, level, substr string) bool {
	for _, r := range records {
		if r.Level == level && strings.Contains(r.Message, substr) {
			return true
		}
	}
	return false
}`,
			want: false,
		},
		{
			name: "a loop over another package's record type is not dlog's to serve",
			src: `package p
func holds(records []harness.LogRecord, operation string) bool {
	for _, r := range records {
		if r.Operation == operation {
			return true
		}
	}
	return false
}`,
			want: false,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			parsed, err := parser.ParseFile(token.NewFileSet(), "x_test.go", tc.src, 0)
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			fn := parsed.Decls[0].(*ast.FuncDecl)

			// Act.
			got := handRollsRecordLookup(fn)

			// Assert.
			if got != tc.want {
				t.Fatalf("handRollsRecordLookup = %t, want %t", got, tc.want)
			}
		})
	}
}

// handRollsRecordLookup reports whether FN, a function that names dlog, ranges
// over something with an if whose condition is nothing but == comparisons of
// a record's Level, Operation or Message against FN's own parameters.
func handRollsRecordLookup(fn *ast.FuncDecl) bool {
	if !namesDlog(fn) {
		return false
	}
	params := map[string]bool{}
	for _, field := range fn.Type.Params.List {
		for _, name := range field.Names {
			params[name.Name] = true
		}
	}
	found := false
	ast.Inspect(fn.Body, func(n ast.Node) bool {
		loop, ok := n.(*ast.RangeStmt)
		if !ok || found {
			return !found
		}
		for _, stmt := range loop.Body.List {
			if cond, ok := stmt.(*ast.IfStmt); ok && onlyRecordFieldsAgainst(cond.Cond, params) {
				found = true
			}
		}
		return !found
	})
	return found
}

// onlyRecordFieldsAgainst reports whether EXPR is a conjunction of
// `x.<Level|Operation|Message> == <parameter>` comparisons alone.
func onlyRecordFieldsAgainst(expr ast.Expr, params map[string]bool) bool {
	bin, ok := expr.(*ast.BinaryExpr)
	if !ok {
		return false
	}
	switch bin.Op {
	case token.LAND:
		return onlyRecordFieldsAgainst(bin.X, params) && onlyRecordFieldsAgainst(bin.Y, params)
	case token.EQL:
		sel, ok := bin.X.(*ast.SelectorExpr)
		if !ok {
			return false
		}
		switch sel.Sel.Name {
		case "Level", "Operation", "Message":
		default:
			return false
		}
		ident, ok := bin.Y.(*ast.Ident)
		return ok && params[ident.Name]
	default:
		return false
	}
}

// namesDlog reports whether FN refers to the dlog package anywhere.
func namesDlog(fn *ast.FuncDecl) bool {
	named := false
	ast.Inspect(fn, func(n ast.Node) bool {
		if sel, ok := n.(*ast.SelectorExpr); ok {
			if pkg, ok := sel.X.(*ast.Ident); ok && pkg.Name == "dlog" {
				named = true
			}
		}
		return !named
	})
	return named
}
