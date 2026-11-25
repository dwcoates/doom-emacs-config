// Package sourcescan reads a daemon package's production Go sources, for the
// rule tests that hold a package to a structural rule ("only endTx may roll
// back", "only connected() announces a link").
//
// Those tests each used to glob "*.go", skip "_test.go" and read what was
// left by hand, and a hand-rolled copy that forgot the skip would count a
// test's own mention of the pattern and pass or fail for the wrong reason.
//
// EVERY RULE TEST LIVES IN REPOSCANS (daemonrules), never beside the package
// it holds: the rule suites run on a branch before its merge is admitted to
// the queue, and a rule test anywhere else would only ever run in the gate.
// So each call names the package it reads, by its path under the daemon
// module's root (DaemonRoot), and the package is internal to reposcans.
package sourcescan

import (
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"sort"
	"strconv"
	"strings"
	"testing"
)

// File is one production source file: its name in the package directory and
// its contents.
type File struct {
	Name   string
	Source []byte
}

// Production returns every non-test .go file in the package directory DIR,
// sorted by name: DIR is a path under the daemon module's root
// ("internal/drain"), or an absolute directory. A package with no production
// source fails the test: a structural rule over nothing proves nothing.
func Production(t testing.TB, dir string) []File {
	t.Helper()
	dir = packageDir(t, dir)
	names, err := filepath.Glob(filepath.Join(dir, "*.go"))
	if err != nil {
		t.Fatalf("sourcescan: glob: %v", err)
	}
	sort.Strings(names)
	var files []File
	for _, name := range names {
		if strings.HasSuffix(name, "_test.go") {
			continue
		}
		source, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("sourcescan: read %s: %v", name, err)
		}
		files = append(files, File{Name: filepath.Base(name), Source: source})
	}
	if len(files) == 0 {
		t.Fatalf("sourcescan: no production .go files in %s", dir)
	}
	return files
}

// InPackage runs the rest of the test in the package directory DIR (a path
// under the daemon module's root): the directory `go test` ran the rule test
// in while it stood beside that package, so every path it reads relative to
// its working directory ("resolver.go", "../vocab") keeps its meaning in
// reposcans. It changes the process's working directory, so a test calling
// it never runs in parallel; the testing package restores the directory when
// the test ends.
func InPackage(t testing.TB, dir string) {
	t.Helper()
	t.Chdir(packageDir(t, dir))
}

// packageDir resolves DIR, a path under the daemon module's root or an
// absolute directory.
func packageDir(t testing.TB, dir string) string {
	t.Helper()
	if filepath.IsAbs(dir) {
		return dir
	}
	return filepath.Join(DaemonRoot(t), filepath.FromSlash(dir))
}

// Count is how many times substr occurs across the production sources of the
// package in DIR.
func Count(t testing.TB, dir, substr string) int {
	t.Helper()
	n := 0
	for _, f := range Production(t, dir) {
		n += strings.Count(string(f.Source), substr)
	}
	return n
}

// Sites names the production file of each occurrence of substr, once per
// occurrence and in file order: the one-home tests' list, which reads
// ["keeper.go"] when the rule holds and names every stray site when it does
// not.
func Sites(t testing.TB, dir, substr string) []string {
	t.Helper()
	return sitesBy(t, dir, func(source []byte) int { return strings.Count(string(source), substr) })
}

// SitesMatching is Sites for a pattern: the production file of each match of
// re, once per match and in file order. It is for a one-home rule whose
// shape is not one literal string (an instant a lifetime is ADDED to, say,
// rather than every read of the lifetime).
func SitesMatching(t testing.TB, dir string, re *regexp.Regexp) []string {
	t.Helper()
	return sitesBy(t, dir, func(source []byte) int { return len(re.FindAllIndex(source, -1)) })
}

// sitesBy is the one sites list Sites and SitesMatching share: each production
// file named once per occurrence COUNT finds in it, in file order.
func sitesBy(t testing.TB, dir string, count func(source []byte) int) []string {
	t.Helper()
	var sites []string
	for _, f := range Production(t, dir) {
		for range count(f.Source) {
			sites = append(sites, f.Name)
		}
	}
	return sites
}

// daemonModule is the daemon module's go.mod line, which names its root.
const daemonModule = "module agent-repld\n"

// DaemonRoot answers the daemon module's root: the nearest directory at or
// above the package under test whose go.mod declares the agent-repld module
// (reposcans' own go.mod, between them, declares another).
func DaemonRoot(t testing.TB) string {
	t.Helper()
	dir, err := filepath.Abs(".")
	if err != nil {
		t.Fatalf("sourcescan: resolve the package directory: %v", err)
	}
	for {
		if body, err := os.ReadFile(filepath.Join(dir, "go.mod")); err == nil && strings.HasPrefix(string(body), daemonModule) {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			t.Fatalf("sourcescan: no agent-repld go.mod above the package under test")
		}
		dir = parent
	}
}

// EachDaemonSource parses every production .go file under ROOT and hands each
// to VISIT with its file set: the daemon-wide counterpart of Production, for a
// rule that holds across packages ("every child is built in childproc").
//
// It skips tests, hidden directories, the integration and e2e harnesses
// (they start the daemon under test, so nothing in them runs inside a serving
// daemon), reposcans (the rule tests and their helpers), and every
// ROOT-relative directory in SKIP: typically the package
// that owns the rule, the one place it is allowed to break it.
func EachDaemonSource(t testing.TB, root string, skip []string, visit func(fset *token.FileSet, file *ast.File)) {
	t.Helper()
	fset := token.NewFileSet()
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		rel, err := filepath.Rel(root, path)
		if err != nil {
			return err
		}
		if d.IsDir() {
			if rel == "integration" || rel == "e2e" || rel == "reposcans" || slices.Contains(skip, rel) || (rel != "." && strings.HasPrefix(d.Name(), ".")) {
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		file, err := parser.ParseFile(fset, path, nil, parser.SkipObjectResolution)
		if err != nil {
			return err
		}
		visit(fset, file)
		return nil
	})
	if err != nil {
		t.Fatalf("sourcescan: walk the daemon sources: %v", err)
	}
}

// EachSourceIn parses every .go file in the one directory DIR and hands each
// to VISIT with its file set, sorted by name: production sources only, or with
// WITHTESTS the test files too (a rule over how tests build their fixtures).
// A directory with no such source fails the test: a rule over nothing proves
// nothing.
func EachSourceIn(t testing.TB, dir string, withTests bool, visit func(fset *token.FileSet, file *ast.File)) {
	t.Helper()
	names, err := filepath.Glob(filepath.Join(dir, "*.go"))
	if err != nil {
		t.Fatalf("sourcescan: glob %s: %v", dir, err)
	}
	sort.Strings(names)
	fset := token.NewFileSet()
	visited := 0
	for _, name := range names {
		if !withTests && strings.HasSuffix(name, "_test.go") {
			continue
		}
		file, err := parser.ParseFile(fset, name, nil, parser.SkipObjectResolution)
		if err != nil {
			t.Fatalf("sourcescan: parse %s: %v", name, err)
		}
		visit(fset, file)
		visited++
	}
	if visited == 0 {
		t.Fatalf("sourcescan: no .go files in %s", dir)
	}
}

// IsSelector reports whether N is the qualified reference PKG.NAME.
func IsSelector(n ast.Node, pkg, name string) bool {
	sel, ok := n.(*ast.SelectorExpr)
	if !ok {
		return false
	}
	id, ok := sel.X.(*ast.Ident)
	return ok && id.Name == pkg && sel.Sel.Name == name
}

// StringDecl is one package-level const or var whose value is a string
// literal, or a composite literal ([]string{...}) of string literals.
type StringDecl struct {
	Name string
	// Type is the declared type's name ("Edge"), empty when none is written.
	Type string
	// Const reports a const; false is a var.
	Const bool
	// Values are the literal's strings: one for a string literal, each
	// element for a composite.
	Values []string
}

// StringDecls answers every such declaration of the production sources in
// DIR, in file order: THE ONE READER of a package's spelled strings, for a
// rule that needs a package's constants or lists without importing it (a
// vocabulary's kinds, a harness's operation names).
func StringDecls(t testing.TB, dir string) []StringDecl {
	t.Helper()
	var out []StringDecl
	fset := token.NewFileSet()
	for _, f := range Production(t, dir) {
		file, err := parser.ParseFile(fset, f.Name, f.Source, parser.SkipObjectResolution)
		if err != nil {
			t.Fatalf("sourcescan: parse %s: %v", f.Name, err)
		}
		for _, decl := range file.Decls {
			gen, ok := decl.(*ast.GenDecl)
			if !ok || (gen.Tok != token.CONST && gen.Tok != token.VAR) {
				continue
			}
			for _, raw := range gen.Specs {
				spec := raw.(*ast.ValueSpec)
				typ := ""
				if id, ok := spec.Type.(*ast.Ident); ok {
					typ = id.Name
				}
				for i, name := range spec.Names {
					if i >= len(spec.Values) {
						continue
					}
					if values, ok := stringValues(spec.Values[i]); ok {
						out = append(out, StringDecl{Name: name.Name, Type: typ, Const: gen.Tok == token.CONST, Values: values})
					}
				}
			}
		}
	}
	return out
}

// stringValues answers the strings EXPR spells: a string literal's, or each
// element of a composite literal of string literals. OK is false for any
// other expression.
func stringValues(expr ast.Expr) ([]string, bool) {
	var lits []ast.Expr
	switch v := expr.(type) {
	case *ast.BasicLit:
		lits = []ast.Expr{v}
	case *ast.CompositeLit:
		lits = v.Elts
	default:
		return nil, false
	}
	values := make([]string, 0, len(lits))
	for _, lit := range lits {
		basic, ok := lit.(*ast.BasicLit)
		if !ok || basic.Kind != token.STRING {
			return nil, false
		}
		value, err := strconv.Unquote(basic.Value)
		if err != nil {
			return nil, false
		}
		values = append(values, value)
	}
	return values, len(values) > 0
}
