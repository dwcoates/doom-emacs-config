// Package scan finds every log call in a Go module's source and derives each
// call's site the way its logger does at run time (logsite.Key over the
// runtime, the normalized enclosing function and the template), so a
// runtime's scan test can prove its sites sound: every template parses,
// every placeholder is filled by the call's own context, no two calls share a
// site, and no two sites share a key (logging-contract.md "Log sites").
//
// It reads source only, with no type checking: a call is a log call when its
// selector is one of the Spec's and its template argument is a constant
// expression. The logger's unexported template type already makes the
// compiler refuse any other template, so a call whose argument is no
// constant is not a log call of the logger at all.
package scan

import (
	"errors"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"

	"agentrepl/logging/logsite"
)

// Call is one shape of log call: a method or function of this selector
// name, with this many arguments, the template at Template and the record's
// named values at Context (-1: the call takes none).
type Call struct {
	Selector string
	Args     int
	Template int
	Context  int
}

// MsgCall is logsite.Msg's shape: every module that forwards messages scans
// it too.
var MsgCall = Call{Selector: "Msg", Args: 1, Template: 0, Context: -1}

// Spec is one runtime's scan.
type Spec struct {
	// Runtime is the runtime its records name, part of every key.
	Runtime string
	// Module is the module's path (go.mod's module line) and Root its
	// directory.
	Module, Root string
	// Calls are the log call shapes.
	Calls []Call
	// Skip reports a directory (relative to Root, slash-separated) whose
	// files are not scanned: another module nested inside, or test data.
	Skip func(rel string) bool
}

// Site is one log call found in source.
type Site struct {
	File     string
	Line     int
	Function string
	Template string
	Key      string
	// Fields are the names the call's context literal holds; nil when the
	// call's context is no literal, or it takes none.
	Fields []string
	// LiteralContext reports whether the call's context is a composite
	// literal with constant keys.
	LiteralContext bool
	// TakesContext reports whether the call shape has a context argument.
	TakesContext bool
}

// Position is the site's file and line, for a failure message.
func (s Site) Position() string { return fmt.Sprintf("%s:%d", s.File, s.Line) }

// Scan answers every log call in SPEC's module, in file and line order. A
// file that does not parse, or a template argument that is a constant the
// scan cannot resolve, is an error naming it.
func Scan(spec Spec) ([]Site, error) {
	pkgs := map[string][]*ast.File{}
	fset := token.NewFileSet()
	err := filepath.WalkDir(spec.Root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		rel, _ := filepath.Rel(spec.Root, path)
		rel = filepath.ToSlash(rel)
		if d.IsDir() {
			if rel != "." && (strings.HasPrefix(d.Name(), ".") || d.Name() == "testdata" || d.Name() == "node_modules" || (spec.Skip != nil && spec.Skip(rel))) {
				return filepath.SkipDir
			}
			if rel != "." {
				if _, err := os.Stat(filepath.Join(path, "go.mod")); err == nil {
					return filepath.SkipDir
				}
			}
			return nil
		}
		if !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		f, err := parser.ParseFile(fset, path, nil, parser.SkipObjectResolution)
		if err != nil {
			return fmt.Errorf("scan: parse %s: %w", path, err)
		}
		dir := filepath.ToSlash(filepath.Dir(rel))
		pkgs[dir] = append(pkgs[dir], f)
		return nil
	})
	if err != nil {
		return nil, err
	}
	consts := moduleConsts(spec.Module, pkgs)
	var sites []Site
	var errs []error
	for dir, files := range pkgs {
		pkgPath := spec.Module
		if dir != "." {
			pkgPath = spec.Module + "/" + dir
		}
		for _, f := range files {
			name := pkgPath
			if f.Name.Name == "main" {
				name = "main"
			}
			imports := fileImports(f)
			found, ferrs := scanFile(spec, fset, f, name, pkgPath, imports, consts)
			sites = append(sites, found...)
			errs = append(errs, ferrs...)
		}
	}
	sort.Slice(sites, func(i, j int) bool {
		if sites[i].File != sites[j].File {
			return sites[i].File < sites[j].File
		}
		return sites[i].Line < sites[j].Line
	})
	return sites, errors.Join(errs...)
}

// constTable is every constant expression a module declares, by package
// path and name, unresolved.
type constTable map[string]map[string]ast.Expr

func moduleConsts(module string, pkgs map[string][]*ast.File) constTable {
	table := constTable{}
	for dir, files := range pkgs {
		pkgPath := module
		if dir != "." {
			pkgPath = module + "/" + dir
		}
		named := map[string]ast.Expr{}
		for _, f := range files {
			for _, decl := range f.Decls {
				gen, ok := decl.(*ast.GenDecl)
				if !ok || gen.Tok != token.CONST {
					continue
				}
				for _, spec := range gen.Specs {
					vs := spec.(*ast.ValueSpec)
					for i, n := range vs.Names {
						if i < len(vs.Values) {
							named[n.Name] = vs.Values[i]
						}
					}
				}
			}
		}
		table[pkgPath] = named
	}
	return table
}

// fileImports maps each import's name in F to its path.
func fileImports(f *ast.File) map[string]string {
	out := map[string]string{}
	for _, imp := range f.Imports {
		path, _ := strconv.Unquote(imp.Path.Value)
		name := path[strings.LastIndexByte(path, '/')+1:]
		if imp.Name != nil {
			name = imp.Name.Name
		}
		out[name] = path
	}
	return out
}

// resolver evaluates a constant string expression within one file.
type resolver struct {
	pkgPath string
	imports map[string]string
	consts  constTable
	local   map[string]ast.Expr
}

// value answers E's constant string value; ok is false for an expression
// that is no constant string (a variable, a call).
func (r resolver) value(e ast.Expr, depth int) (string, bool) {
	if depth > 32 {
		return "", false
	}
	switch x := e.(type) {
	case *ast.BasicLit:
		if x.Kind != token.STRING {
			return "", false
		}
		s, err := strconv.Unquote(x.Value)
		return s, err == nil
	case *ast.ParenExpr:
		return r.value(x.X, depth+1)
	case *ast.BinaryExpr:
		if x.Op != token.ADD {
			return "", false
		}
		a, ok := r.value(x.X, depth+1)
		if !ok {
			return "", false
		}
		b, ok := r.value(x.Y, depth+1)
		return a + b, ok
	case *ast.Ident:
		if v, ok := r.local[x.Name]; ok {
			return r.value(v, depth+1)
		}
		if v, ok := r.consts[r.pkgPath][x.Name]; ok {
			return r.value(v, depth+1)
		}
	case *ast.SelectorExpr:
		pkg, ok := x.X.(*ast.Ident)
		if !ok {
			return "", false
		}
		path, ok := r.imports[pkg.Name]
		if !ok {
			return "", false
		}
		if v, ok := r.consts[path][x.Sel.Name]; ok {
			inner := resolver{pkgPath: path, imports: nil, consts: r.consts}
			return inner.value(v, depth+1)
		}
	}
	return "", false
}

func scanFile(spec Spec, fset *token.FileSet, f *ast.File, name, pkgPath string, imports map[string]string, consts constTable) ([]Site, []error) {
	var sites []Site
	var errs []error
	visit := func(function string, root ast.Node) {
		local := map[string]ast.Expr{}
		ast.Inspect(root, func(n ast.Node) bool {
			if decl, ok := n.(*ast.DeclStmt); ok {
				if gen, ok := decl.Decl.(*ast.GenDecl); ok && gen.Tok == token.CONST {
					for _, s := range gen.Specs {
						vs := s.(*ast.ValueSpec)
						for i, n := range vs.Names {
							if i < len(vs.Values) {
								local[n.Name] = vs.Values[i]
							}
						}
					}
				}
			}
			call, ok := n.(*ast.CallExpr)
			if !ok {
				return true
			}
			shape, ok := matchCall(spec.Calls, call)
			if !ok {
				return true
			}
			r := resolver{pkgPath: pkgPath, imports: imports, consts: consts, local: local}
			template, ok := r.value(call.Args[shape.Template], 0)
			if !ok {
				return true
			}
			pos := fset.Position(call.Pos())
			site := Site{File: pos.Filename, Line: pos.Line, Function: function, Template: template,
				Key: logsite.Key(spec.Runtime, function, template), TakesContext: shape.Context >= 0}
			if shape.Context >= 0 {
				site.Fields, site.LiteralContext = literalFields(r, call.Args[shape.Context])
			}
			sites = append(sites, site)
			return true
		})
	}
	for _, decl := range f.Decls {
		switch d := decl.(type) {
		case *ast.FuncDecl:
			visit(funcName(name, d), d)
		case *ast.GenDecl:
			visit(name+".init", d)
		}
	}
	return sites, errs
}

// matchCall answers the shape CALL has, if it is one of CALLS.
func matchCall(calls []Call, call *ast.CallExpr) (Call, bool) {
	var selector string
	switch fn := call.Fun.(type) {
	case *ast.SelectorExpr:
		selector = fn.Sel.Name
	case *ast.Ident:
		selector = fn.Name
	default:
		return Call{}, false
	}
	for _, c := range calls {
		if c.Selector == selector && c.Args == len(call.Args) {
			return c, true
		}
	}
	return Call{}, false
}

// literalFields answers the constant keys of the composite literal E, and
// whether E is one whose every key is a constant string.
func literalFields(r resolver, e ast.Expr) ([]string, bool) {
	lit, ok := e.(*ast.CompositeLit)
	if !ok {
		return nil, false
	}
	fields := []string{}
	for _, elt := range lit.Elts {
		kv, ok := elt.(*ast.KeyValueExpr)
		if !ok {
			return nil, false
		}
		key, ok := r.value(kv.Key, 0)
		if !ok {
			return nil, false
		}
		fields = append(fields, key)
	}
	return fields, true
}

// funcName spells D's function as the runtime spells its frame, before
// normalization: pkg.F, pkg.T.M or pkg.(*T).M.
func funcName(pkg string, d *ast.FuncDecl) string {
	if d.Recv == nil || len(d.Recv.List) == 0 {
		return logsite.NormalizeFunction(pkg + "." + d.Name.Name)
	}
	recv := d.Recv.List[0].Type
	pointer := false
	if star, ok := recv.(*ast.StarExpr); ok {
		pointer, recv = true, star.X
	}
	switch g := recv.(type) {
	case *ast.IndexExpr:
		recv = g.X
	case *ast.IndexListExpr:
		recv = g.X
	}
	typ := recv.(*ast.Ident).Name
	if pointer {
		return pkg + ".(*" + typ + ")." + d.Name.Name
	}
	return pkg + "." + typ + "." + d.Name.Name
}

// Check answers every fault of SITES: a template that does not parse; a
// placeholder the call's own context literal does not hold, or a template
// with placeholders whose call's context is no literal (the scan could not
// prove it filled); two calls in one function writing one template (one
// site); and two sites sharing a key.
func Check(sites []Site) []error {
	var errs []error
	byKey := map[string]Site{}
	for _, s := range sites {
		names, err := logsite.Placeholders(s.Template)
		if err != nil {
			errs = append(errs, fmt.Errorf("%s: %w", s.Position(), err))
			continue
		}
		if len(names) > 0 && s.TakesContext {
			if !s.LiteralContext {
				errs = append(errs, fmt.Errorf("%s: the template %q names placeholders, so its call's context must be a literal holding each of them", s.Position(), s.Template))
			} else {
				have := map[string]bool{}
				for _, f := range s.Fields {
					have[f] = true
				}
				for _, n := range names {
					if !have[n] {
						errs = append(errs, fmt.Errorf("%s: the template %q names {%s}, which the call's context does not hold", s.Position(), s.Template, n))
					}
				}
			}
		}
		if prior, ok := byKey[s.Key]; ok {
			if prior.Function == s.Function && prior.Template == s.Template {
				errs = append(errs, fmt.Errorf("%s and %s: two calls in %s write the template %q, so they are one site; give each its own template", prior.Position(), s.Position(), s.Function, s.Template))
			} else {
				errs = append(errs, fmt.Errorf("%s and %s: two sites hash to the key %s; reword one template", prior.Position(), s.Position(), s.Key))
			}
			continue
		}
		byKey[s.Key] = s
	}
	return errs
}
