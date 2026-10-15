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

// fieldsHome is the one package that may build a record's fields from other
// fields: dlog.Merge and dlog.WithCause.
const fieldsHome = "internal/dlog"

// fieldsBuilderExempt are the field builders outside dlog that are NOT copies
// of its two, each with the reason it is its own convention.
var fieldsBuilderExempt = map[string]string{
	// The store's records name their error under "error", not "cause": a
	// convention of its own every wsm record and reader keeps.
	"internal/wsm/open.go: withError": "the store's records key their error as error",
}

// TestNoPackageHandRollsARecordFieldsBuilder guards dlog.Merge and
// dlog.WithCause as the ONE pair of record-field builders. A function outside
// dlog shaped (dlog.Context, dlog.Context) dlog.Context or
// (dlog.Context, error) dlog.Context is a hand-rolled copy of one of them;
// such a package calls dlog's instead.
func TestNoPackageHandRollsARecordFieldsBuilder(t *testing.T) {
	// Arrange.
	files := token.NewFileSet()
	var copies []string

	// Act.
	for _, root := range []string{"cmd", "internal"} {
		err := filepath.WalkDir(root, func(path string, entry fs.DirEntry, walkErr error) error {
			if walkErr != nil {
				return walkErr
			}
			if entry.IsDir() || !strings.HasSuffix(path, ".go") || filepath.Dir(path) == fieldsHome {
				return nil
			}
			parsed, err := parser.ParseFile(files, path, nil, 0)
			if err != nil {
				return err
			}
			for _, name := range fieldsBuilders(parsed) {
				site := path + ": " + name
				if _, exempt := fieldsBuilderExempt[site]; !exempt {
					copies = append(copies, site)
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
		t.Fatalf("functions hand-roll a record-fields builder; call dlog.Merge or dlog.WithCause instead:\n%s", strings.Join(copies, "\n"))
	}
}

func TestTheFieldsGuardRecognizesAHandRolledCopy(t *testing.T) {
	tests := []struct {
		name string
		src  string
		want []string
	}{
		{name: "a cause stamper is a copy", src: `package p
func withCause(fields dlog.Context, err error) dlog.Context { return nil }`, want: []string{"withCause"}},
		{name: "a context merger is a copy", src: `package p
func merge(base, extra dlog.Context) dlog.Context { return nil }`, want: []string{"merge"}},
		{name: "a one-field builder is not", src: `package p
func withField(fields dlog.Context, name string, value any) dlog.Context { return nil }`},
		{name: "a method is not", src: `package p
func (r *run) fields(base dlog.Context, err error) dlog.Context { return nil }`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			parsed, err := parser.ParseFile(token.NewFileSet(), "p.go", tt.src, 0)
			if err != nil {
				t.Fatalf("parse: %v", err)
			}

			// Act.
			got := fieldsBuilders(parsed)

			// Assert.
			if strings.Join(got, ",") != strings.Join(tt.want, ",") {
				t.Fatalf("fieldsBuilders = %v, want %v", got, tt.want)
			}
		})
	}
}

// fieldsBuilders names every top-level function FILE declares shaped
// (dlog.Context, dlog.Context) dlog.Context or (dlog.Context, error)
// dlog.Context.
func fieldsBuilders(file *ast.File) []string {
	var names []string
	for _, decl := range file.Decls {
		fn, ok := decl.(*ast.FuncDecl)
		if !ok || fn.Recv != nil {
			continue
		}
		params := flatTypes(fn.Type.Params)
		results := flatTypes(fn.Type.Results)
		if len(params) != 2 || len(results) != 1 || results[0] != "dlog.Context" || params[0] != "dlog.Context" {
			continue
		}
		if params[1] == "dlog.Context" || params[1] == "error" {
			names = append(names, fn.Name.Name)
		}
	}
	return names
}

// flatTypes spells each parameter's type once per name it declares.
func flatTypes(list *ast.FieldList) []string {
	if list == nil {
		return nil
	}
	var out []string
	for _, field := range list.List {
		spelled := typeName(field.Type)
		count := len(field.Names)
		if count == 0 {
			count = 1
		}
		for i := 0; i < count; i++ {
			out = append(out, spelled)
		}
	}
	return out
}

// typeName spells a simple type expression.
func typeName(expr ast.Expr) string {
	switch t := expr.(type) {
	case *ast.Ident:
		return t.Name
	case *ast.SelectorExpr:
		if pkg, ok := t.X.(*ast.Ident); ok {
			return pkg.Name + "." + t.Sel.Name
		}
	}
	return ""
}
