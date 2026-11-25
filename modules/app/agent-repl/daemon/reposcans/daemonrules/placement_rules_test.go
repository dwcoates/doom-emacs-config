package daemonrules

import (
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// EVERY RULE TEST OF THE DAEMON LIVES IN REPOSCANS. A rule test, one that
// reads Go sources to hold them to a structural rule, runs on every branch
// before its merge is admitted to the queue (the roster's rule suites), and a
// rule test beside the package it holds would run only in the gate, which is
// where it failed merge after merge. The source-reading helpers
// (sourcescan, mdinlinetest) are internal to reposcans, so no test elsewhere
// can call them; these scans catch the two other ways a test reads source.

// sourceReaders are the packages that read Go source: the parser, and the
// log-site scan every runtime's log calls are checked by.
var sourceReaders = map[string]bool{"go/parser": true, "agentrepl/logging/logsite/scan": true}

// goSourceReads answers, for one parsed Go file, every reason it reads Go
// source: an import of a source reader (sourceReaders), or a filesystem read whose arguments name a
// .go file or pattern ("resolver.go", "*.go", a walk that keeps paths ending
// ".go"). Each reason is "<decl>: <what>", with "import" for the import.
func goSourceReads(file *ast.File) []string {
	var reasons []string
	for _, spec := range file.Imports {
		if path, _ := strconv.Unquote(spec.Path.Value); sourceReaders[path] {
			reasons = append(reasons, "import: "+path)
		}
	}
	imports := map[string]string{}
	for _, spec := range file.Imports {
		path, _ := strconv.Unquote(spec.Path.Value)
		name := filepath.Base(path)
		if spec.Name != nil {
			name = spec.Name.Name
		}
		imports[name] = path
	}
	reads := map[string][]string{"os": {"ReadFile", "ReadDir", "Open"}, "path/filepath": {"Glob", "WalkDir"}, "io/fs": {"WalkDir", "Glob", "ReadFile"}}
	for _, decl := range file.Decls {
		name := declName(decl)
		ast.Inspect(decl, func(n ast.Node) bool {
			call, ok := n.(*ast.CallExpr)
			if !ok {
				return true
			}
			sel, ok := call.Fun.(*ast.SelectorExpr)
			if !ok {
				return true
			}
			pkg, ok := sel.X.(*ast.Ident)
			if !ok || !slices.Contains(reads[imports[pkg.Name]], sel.Sel.Name) {
				return true
			}
			for _, arg := range call.Args {
				if namesGoSource(arg) {
					reasons = append(reasons, name+": "+imports[pkg.Name]+"."+sel.Sel.Name+" of Go source")
					return true
				}
			}
			return true
		})
	}
	return reasons
}

// namesGoSource reports whether EXPR holds a string literal naming a .go file
// or pattern.
func namesGoSource(expr ast.Expr) bool {
	found := false
	ast.Inspect(expr, func(n ast.Node) bool {
		if lit, ok := n.(*ast.BasicLit); ok && lit.Kind == token.STRING {
			if value, err := strconv.Unquote(lit.Value); err == nil && strings.HasSuffix(value, ".go") {
				found = true
			}
		}
		return !found
	})
	return found
}

// declName names a top-level declaration for a report.
func declName(decl ast.Decl) string {
	switch d := decl.(type) {
	case *ast.FuncDecl:
		return d.Name.Name
	case *ast.GenDecl:
		for _, spec := range d.Specs {
			switch s := spec.(type) {
			case *ast.ValueSpec:
				return s.Names[0].Name
			case *ast.TypeSpec:
				return s.Name.Name
			}
		}
	}
	return "declaration"
}

func TestGoSourceReadsFindsEveryWayATestReadsSource(t *testing.T) {
	cases := []struct {
		name string
		src  string
		want []string
	}{
		{"an import of go/parser", "package p\nimport \"go/parser\"\nvar _ = parser.ParseFile\n", []string{"import: go/parser"}},
		{"an import of the log-site scan", "package p\nimport \"agentrepl/logging/logsite/scan\"\nvar _ = scan.Require\n", []string{"import: agentrepl/logging/logsite/scan"}},
		{"a read of one source file", "package p\nimport \"os\"\nfunc TestX() { os.ReadFile(\"resolver.go\") }\n", []string{"TestX: os.ReadFile of Go source"}},
		{"a glob of the sources", "package p\nimport \"path/filepath\"\nfunc TestX() { filepath.Glob(filepath.Join(\"..\", \"*.go\")) }\n", []string{"TestX: path/filepath.Glob of Go source"}},
		{"a walk keeping .go paths", "package p\nimport (\"path/filepath\"; \"strings\")\nfunc TestX() { filepath.WalkDir(\".\", func(p string, _ any, _ error) error { _ = strings.HasSuffix(p, \".go\"); return nil }) }\n", []string{"TestX: path/filepath.WalkDir of Go source"}},
		{"a read of a fixture that is not Go", "package p\nimport \"os\"\nfunc TestX() { os.ReadFile(\"testdata/run.json\") }\n", nil},
		{"a .go name that is never read", "package p\nimport \"os\"\nfunc TestX() { changed := []string{\"daemon/x.go\"}; _ = changed; os.ReadFile(\"out.log\") }\n", nil},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			file, err := parser.ParseFile(token.NewFileSet(), "x_test.go", tc.src, 0)
			if err != nil {
				t.Fatalf("parse: %v", err)
			}

			// Act
			got := goSourceReads(file)

			// Assert
			if !slices.Equal(got, tc.want) {
				t.Fatalf("goSourceReads = %q, want %q", got, tc.want)
			}
		})
	}
}

// TestNoDaemonCodeOutsideReposcansReadsGoSource fails on every Go file of the
// daemon module outside reposcans that reads Go source: a rule test that
// belongs in reposcans/daemonrules, or a helper that belongs in
// reposcans/internal.
func TestNoDaemonCodeOutsideReposcansReadsGoSource(t *testing.T) {
	// Arrange
	root := sourcescan.DaemonRoot(t)
	fset := token.NewFileSet()
	var offenders []string

	// Act
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		rel, _ := filepath.Rel(root, path)
		if d.IsDir() {
			if rel == "reposcans" || d.Name() == "testdata" || d.Name() == "node_modules" || (rel != "." && strings.HasPrefix(d.Name(), ".")) {
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(path, ".go") {
			return nil
		}
		file, err := parser.ParseFile(fset, path, nil, parser.SkipObjectResolution)
		if err != nil {
			return err
		}
		for _, reason := range goSourceReads(file) {
			offenders = append(offenders, rel+": "+reason)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk the daemon module: %v", err)
	}
	if len(offenders) != 0 {
		t.Fatalf("these read Go source outside reposcans; a rule test goes in reposcans/daemonrules, a source-reading helper in reposcans/internal:\n%s", strings.Join(offenders, "\n"))
	}
}

// TestEveryStringValueReadGoesThroughStringDecls holds the rule tests to the
// one reader of a package's spelled strings (sourcescan.StringDecls): a rule
// that walked a package's declarations for their literal values by hand
// could drift from it.
func TestEveryStringValueReadGoesThroughStringDecls(t *testing.T) {
	// Arrange
	names, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatalf("glob the rule tests: %v", err)
	}
	needle := "spec.Values[i].(*ast." + "BasicLit)"
	var offenders []string

	// Act
	for _, name := range names {
		body, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("read %s: %v", name, err)
		}
		if strings.Contains(string(body), needle) {
			offenders = append(offenders, name)
		}
	}

	// Assert
	if len(names) == 0 || len(offenders) != 0 {
		t.Fatalf("rule tests %v read declared string values by hand among %d files; use sourcescan.StringDecls", offenders, len(names))
	}
}
