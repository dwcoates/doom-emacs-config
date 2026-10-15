// Package gomut is the Go mutator of `testrun redundancy`. It rewrites one
// package's production files into a MUTANT SCHEMA: every mutant is compiled
// into the one binary, each behind a switch read from AGENT_REPL_MUTANT, so a
// thousand mutants cost one compile and the inactive binary behaves exactly as
// the original. The rewrite reaches the compiler through `go -overlay`; no
// source file on disk is ever written.
//
// Every operand is evaluated once and in its original order, and short
// circuits stay short: comparisons and arithmetic go through generic helpers
// that take their operands as arguments, && and || through closures.
//
// The rewrite is syntactic, so a few sites do not type-check (a string `+`, a
// named bool type). Drop removes such a site, and the caller recompiles until
// the schema builds; a dropped site is reported, never silently lost.
package gomut

import (
	"bytes"
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
)

// EnvMutant names the active mutant; unset or empty is none.
const EnvMutant = "AGENT_REPL_MUTANT"

// SwitchFile is the file the schema adds to the package for its helpers.
const SwitchFile = "zz_agent_repl_mutant_switch.go"

// Mutant is one mutation, at its position in the ORIGINAL source.
type Mutant struct {
	ID   int
	File string
	Line int
	Col  int
	// Op says what changed, e.g. "< -> <=".
	Op string
}

// Key is the mutant's identity independent of its numbering.
func (m Mutant) Key() string {
	return fmt.Sprintf("%s:%d:%d:%s", filepath.Base(m.File), m.Line, m.Col, m.Op)
}

// site is one rewritten node; it carries one or more mutants.
type site struct {
	start, end int
	mutants    []int
	render     func(sub func(start, end int) string) string
	dropped    bool
}

type file struct {
	path string
	// overlay is where Overlay last wrote the rewrite; the compiler names
	// that path in its errors.
	overlay string
	src     []byte
	sites   []*site
}

// Schema is a package's rewritten form.
type Schema struct {
	Package string
	Dir     string
	Mutants []Mutant
	files   []*file
	byID    map[int]*site
	first   int
}

// Build parses the named production files of the package in dir and lays out
// every mutant, numbering them from first so that schemas of several packages
// share one binary. files are base names; test and generated files are
// refused.
func Build(dir string, files []string, first int) (*Schema, error) {
	if first < 1 {
		return nil, fmt.Errorf("gomut: mutant ids start at 1, not %d", first)
	}
	s := &Schema{Dir: dir, byID: map[int]*site{}, first: first}
	sort.Strings(files)
	for _, name := range files {
		if strings.HasSuffix(name, "_test.go") {
			return nil, fmt.Errorf("gomut: %s is a test file", name)
		}
		path := filepath.Join(dir, name)
		src, err := os.ReadFile(path)
		if err != nil {
			return nil, fmt.Errorf("gomut: read %s: %w", path, err)
		}
		fset := token.NewFileSet()
		af, err := parser.ParseFile(fset, path, src, parser.ParseComments)
		if err != nil {
			return nil, fmt.Errorf("gomut: parse %s: %w", path, err)
		}
		if ast.IsGenerated(af) {
			return nil, fmt.Errorf("gomut: %s is generated", name)
		}
		if s.Package == "" {
			s.Package = af.Name.Name
		} else if s.Package != af.Name.Name {
			return nil, fmt.Errorf("gomut: %s is package %s, not %s", name, af.Name.Name, s.Package)
		}
		f := &file{path: path, src: src}
		w := &walker{s: s, f: f, fset: fset, listed: map[*ast.ExprStmt]bool{}}
		w.file(af)
		s.files = append(s.files, f)
	}
	return s, nil
}

// Overlay writes the rewritten files and the switch file under a directory
// of scratch that is this package's alone, and returns the `go -overlay`
// replacements, original path to rewrite. Schemas of several packages share
// one scratch directory and one overlay, so a path named only by the file
// would let one package's switch file (its package clause) or rewrite stand
// in for another's.
func (s *Schema) Overlay(scratch string) (map[string]string, error) {
	sum := sha256.Sum256([]byte(s.Dir))
	dir := filepath.Join(scratch, s.Package+"-"+hex.EncodeToString(sum[:6]))
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return nil, fmt.Errorf("gomut: make %s: %w", dir, err)
	}
	replace := map[string]string{}
	for _, f := range s.files {
		out := filepath.Join(dir, filepath.Base(f.path))
		if err := os.WriteFile(out, f.rewrite(), 0o644); err != nil {
			return nil, fmt.Errorf("gomut: write %s: %w", out, err)
		}
		replace[f.path] = out
		f.overlay = out
	}
	sw := filepath.Join(dir, SwitchFile)
	if err := os.WriteFile(sw, []byte(strings.ReplaceAll(switchSource, "PACKAGE", s.Package)), 0o644); err != nil {
		return nil, fmt.Errorf("gomut: write %s: %w", sw, err)
	}
	replace[filepath.Join(s.Dir, SwitchFile)] = sw
	return replace, nil
}

// WriteOverlay writes the `go -overlay` JSON for replace to path.
func WriteOverlay(path string, replace map[string]string) error {
	var b bytes.Buffer
	b.WriteString(`{"Replace":{`)
	keys := make([]string, 0, len(replace))
	for k := range replace {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	for i, k := range keys {
		if i > 0 {
			b.WriteString(",")
		}
		fmt.Fprintf(&b, "%s:%s", strconv.Quote(k), strconv.Quote(replace[k]))
	}
	b.WriteString("}}")
	if err := os.WriteFile(path, b.Bytes(), 0o644); err != nil {
		return fmt.Errorf("gomut: write %s: %w", path, err)
	}
	return nil
}

// Holds says path, original or overlay, is a file of the schema.
func (s *Schema) Holds(path string) bool {
	for _, f := range s.files {
		if f.path == path || f.overlay == path {
			return true
		}
	}
	return false
}

// Live are the mutants whose sites are still in the schema.
func (s *Schema) Live() []Mutant {
	var out []Mutant
	for _, m := range s.Mutants {
		if !s.byID[m.ID].dropped {
			out = append(out, m)
		}
	}
	return out
}

// Dropped are the mutants whose sites did not type-check.
func (s *Schema) Dropped() []Mutant {
	var out []Mutant
	for _, m := range s.Mutants {
		if s.byID[m.ID].dropped {
			out = append(out, m)
		}
	}
	return out
}

// Drop removes, for one compile error at line:col of a rewritten file (named
// by its original path or by the overlay path the compiler reports), the
// innermost live site containing it. It returns
// the dropped site's mutants, and an error when no live site contains the
// position: the rewrite itself is then wrong.
func (s *Schema) Drop(path string, line, col int) ([]int, error) {
	for _, f := range s.files {
		if f.path != path && f.overlay != path {
			continue
		}
		out := f.rewrite()
		fset := token.NewFileSet()
		af, err := parser.ParseFile(fset, path, out, 0)
		if err != nil {
			return nil, fmt.Errorf("gomut: the rewrite of %s does not parse: %w", path, err)
		}
		tf := fset.File(af.Pos())
		if line < 1 || line > tf.LineCount() {
			return nil, fmt.Errorf("gomut: %s:%d is past the rewrite's end", path, line)
		}
		off := tf.Offset(tf.LineStart(line)) + col - 1
		var best ast.Node
		var bestID int
		ast.Inspect(af, func(n ast.Node) bool {
			id, ok := siteID(n)
			if !ok || s.byID[id] == nil || s.byID[id].dropped {
				return true
			}
			if tf.Offset(n.Pos()) <= off && off < tf.Offset(n.End()) {
				if best == nil || n.End()-n.Pos() < best.End()-best.Pos() {
					best, bestID = n, id
				}
			}
			return true
		})
		if best == nil {
			return nil, fmt.Errorf("gomut: a compile error at %s:%d:%d is in no mutation site; the rewrite is wrong", path, line, col)
		}
		st := s.byID[bestID]
		st.dropped = true
		return st.mutants, nil
	}
	return nil, fmt.Errorf("gomut: %s is not a file of the schema", path)
}

// siteID reads the first mutant ID of a rewritten site's root node: a helper
// call, or the `if !agentReplMutOn(N)` that guards a removable statement.
func siteID(n ast.Node) (int, bool) {
	if ifs, ok := n.(*ast.IfStmt); ok {
		if u, ok := ifs.Cond.(*ast.UnaryExpr); ok && u.Op == token.NOT {
			if id, ok := siteID(u.X); ok {
				return id, true
			}
		}
		return 0, false
	}
	call, ok := n.(*ast.CallExpr)
	if !ok || len(call.Args) == 0 {
		return 0, false
	}
	var name string
	switch fn := call.Fun.(type) {
	case *ast.Ident:
		name = fn.Name
	case *ast.IndexExpr:
		if id, ok := fn.X.(*ast.Ident); ok {
			name = id.Name
		}
	}
	if !strings.HasPrefix(name, "agentReplMut") {
		return 0, false
	}
	lit, ok := call.Args[0].(*ast.BasicLit)
	if !ok {
		return 0, false
	}
	id, err := strconv.Atoi(lit.Value)
	if err != nil {
		return 0, false
	}
	return id, true
}

// rewrite renders the file with every live site applied.
func (f *file) rewrite() []byte {
	return []byte(f.render(0, len(f.src), nil))
}

// render is the original text of [start, end) with every outermost live site
// inside it, save self, replaced by that site's rendering. A site renders its
// operands through a sub that excludes the site itself, so a site may render
// its own whole range (an == wrapped in a flip) without meeting itself.
func (f *file) render(start, end int, self *site) string {
	var b strings.Builder
	at := start
	for _, st := range f.sites {
		if st == self || st.dropped || st.start < at || st.end > end {
			continue
		}
		b.Write(f.src[at:st.start])
		st := st
		b.WriteString(st.render(func(s, e int) string { return f.render(s, e, st) }))
		at = st.end
	}
	b.Write(f.src[at:end])
	return b.String()
}

type walker struct {
	s    *Schema
	f    *file
	fset *token.FileSet
	// results is the result list of each enclosing function, innermost last.
	results []*ast.FieldList
	// listed are the expression statements that sit in a statement list,
	// the only place an `if` may replace one (not an if's or a for's init).
	listed map[*ast.ExprStmt]bool
}

func (w *walker) off(p token.Pos) int { return w.fset.Position(p).Offset }

func (w *walker) mutant(n ast.Node, op string) int {
	pos := w.fset.Position(n.Pos())
	id := w.s.first + len(w.s.Mutants)
	w.s.Mutants = append(w.s.Mutants, Mutant{ID: id, File: w.f.path, Line: pos.Line, Col: pos.Column, Op: op})
	return id
}

func (w *walker) add(n ast.Node, mutants []int, render func(sub func(int, int) string) string) {
	st := &site{start: w.off(n.Pos()), end: w.off(n.End()), mutants: mutants, render: render}
	for _, id := range mutants {
		w.s.byID[id] = st
	}
	w.f.sites = append(w.f.sites, st)
}

func (w *walker) file(af *ast.File) {
	for _, d := range af.Decls {
		switch d := d.(type) {
		case *ast.FuncDecl:
			if d.Body != nil {
				w.results = append(w.results, d.Type.Results)
				w.node(d.Body)
				w.results = w.results[:len(w.results)-1]
			}
		case *ast.GenDecl:
			if d.Tok == token.VAR {
				for _, sp := range d.Specs {
					for _, v := range sp.(*ast.ValueSpec).Values {
						w.node(v)
					}
				}
			}
		}
	}
	sort.SliceStable(w.f.sites, func(i, j int) bool {
		if w.f.sites[i].start != w.f.sites[j].start {
			return w.f.sites[i].start < w.f.sites[j].start
		}
		return w.f.sites[i].end > w.f.sites[j].end
	})
}

// node visits n's subtree, laying out a site on every mutable node in it.
func (w *walker) node(n ast.Node) {
	ast.Inspect(n, func(c ast.Node) bool {
		switch c := c.(type) {
		case *ast.FuncLit:
			w.results = append(w.results, c.Type.Results)
			w.node(c.Body)
			w.results = w.results[:len(w.results)-1]
			return false
		case *ast.GenDecl:
			return c.Tok == token.VAR
		case *ast.ArrayType, *ast.TypeSwitchStmt, *ast.MapType, *ast.ChanType, *ast.FuncType, *ast.StructType, *ast.InterfaceType:
			// Types and type switches hold no runtime expression to mutate
			// (an array length must stay constant).
			if ts, ok := c.(*ast.TypeSwitchStmt); ok {
				w.node(ts.Body)
			}
			return false
		case *ast.CompositeLit:
			for _, e := range c.Elts {
				w.node(e)
			}
			return false
		case *ast.KeyValueExpr:
			// A struct literal's key is a field name, not an expression.
			w.node(c.Value)
			if _, isIdent := c.Key.(*ast.Ident); !isIdent {
				w.node(c.Key)
			}
			return false
		case *ast.SelectorExpr:
			w.node(c.X)
			return false
		case *ast.BinaryExpr:
			w.binary(c)
		case *ast.UnaryExpr:
			if c.Op == token.NOT {
				id := w.mutant(c, "drop !")
				w.add(c, []int{id}, func(sub func(int, int) string) string {
					return fmt.Sprintf("agentReplMutFlip(%d, %s)", id, sub(w.off(c.Pos()), w.off(c.End())))
				})
				w.node(c.X)
				return false
			}
		case *ast.Ident:
			if c.Name == "true" || c.Name == "false" {
				id := w.mutant(c, c.Name+" -> "+map[string]string{"true": "false", "false": "true"}[c.Name])
				w.add(c, []int{id}, func(func(int, int) string) string {
					return fmt.Sprintf("agentReplMutFlip(%d, %s)", id, c.Name)
				})
			}
		case *ast.BlockStmt:
			w.list(c.List)
		case *ast.CaseClause:
			w.list(c.Body)
		case *ast.CommClause:
			w.list(c.Body)
		case *ast.ExprStmt:
			// Never a panic: it is the one call Go counts as a terminating
			// statement, so a function ending in one, guarded, no longer
			// compiles ("missing return", at the function's end, outside any
			// site), and removing a panic is a mutant no test can be asked
			// to kill by anything but the panic itself.
			if call, ok := c.X.(*ast.CallExpr); ok && w.listed[c] && !isPanic(call) {
				id := w.mutant(c, "remove call")
				w.add(c, []int{id}, func(sub func(int, int) string) string {
					return fmt.Sprintf("if !agentReplMutOn(%d) { %s }", id, sub(w.off(c.Pos()), w.off(c.End())))
				})
				w.node(c.X)
				return false
			}
		case *ast.ReturnStmt:
			w.ret(c)
		}
		return true
	})
}

func (w *walker) list(stmts []ast.Stmt) {
	for _, st := range stmts {
		if es, ok := st.(*ast.ExprStmt); ok {
			w.listed[es] = true
		}
		if ls, ok := st.(*ast.LabeledStmt); ok {
			if es, ok := ls.Stmt.(*ast.ExprStmt); ok {
				w.listed[es] = true
			}
		}
	}
}

func isConst(e ast.Expr) bool {
	switch e := e.(type) {
	case *ast.BasicLit:
		return true
	case *ast.ParenExpr:
		return isConst(e.X)
	case *ast.UnaryExpr:
		return isConst(e.X)
	case *ast.BinaryExpr:
		return isConst(e.X) && isConst(e.Y)
	}
	return false
}

func isString(e ast.Expr) bool {
	lit, ok := e.(*ast.BasicLit)
	return ok && lit.Kind == token.STRING
}

var (
	boundary = map[token.Token]token.Token{token.LSS: token.LEQ, token.LEQ: token.LSS, token.GTR: token.GEQ, token.GEQ: token.GTR}
	negation = map[token.Token]token.Token{token.LSS: token.GEQ, token.LEQ: token.GTR, token.GTR: token.LEQ, token.GEQ: token.LSS, token.EQL: token.NEQ, token.NEQ: token.EQL}
	arith    = map[token.Token]token.Token{token.ADD: token.SUB, token.SUB: token.ADD, token.MUL: token.QUO, token.QUO: token.MUL, token.REM: token.MUL}
)

func (w *walker) binary(c *ast.BinaryExpr) {
	if isConst(c.X) && isConst(c.Y) {
		return
	}
	x := func(sub func(int, int) string) string { return sub(w.off(c.X.Pos()), w.off(c.X.End())) }
	y := func(sub func(int, int) string) string { return sub(w.off(c.Y.Pos()), w.off(c.Y.End())) }
	switch c.Op {
	case token.LSS, token.LEQ, token.GTR, token.GEQ:
		b := w.mutant(c, fmt.Sprintf("%s -> %s", c.Op, boundary[c.Op]))
		n := w.mutant(c, fmt.Sprintf("%s -> %s", c.Op, negation[c.Op]))
		w.add(c, []int{b, n}, func(sub func(int, int) string) string {
			return fmt.Sprintf("agentReplMutCmp(%d, %d, %q, %s, %s)", b, n, c.Op.String(), x(sub), y(sub))
		})
	case token.EQL, token.NEQ:
		id := w.mutant(c, fmt.Sprintf("%s -> %s", c.Op, negation[c.Op]))
		w.add(c, []int{id}, func(sub func(int, int) string) string {
			return fmt.Sprintf("agentReplMutFlip(%d, %s)", id, sub(w.off(c.Pos()), w.off(c.End())))
		})
	case token.LAND, token.LOR:
		other := map[token.Token]token.Token{token.LAND: token.LOR, token.LOR: token.LAND}[c.Op]
		id := w.mutant(c, fmt.Sprintf("%s -> %s", c.Op, other))
		w.add(c, []int{id}, func(sub func(int, int) string) string {
			return fmt.Sprintf("agentReplMutLogic(%d, %t, func() bool { return %s }, func() bool { return %s })", id, c.Op == token.LAND, x(sub), y(sub))
		})
	case token.ADD, token.SUB, token.MUL, token.QUO, token.REM:
		if isString(c.X) || isString(c.Y) {
			return
		}
		helper := "agentReplMutArith"
		if c.Op == token.REM {
			helper = "agentReplMutRem"
		}
		id := w.mutant(c, fmt.Sprintf("%s -> %s", c.Op, arith[c.Op]))
		w.add(c, []int{id}, func(sub func(int, int) string) string {
			return fmt.Sprintf("%s(%d, %q, %s, %s)", helper, id, c.Op.String(), x(sub), y(sub))
		})
	}
}

// ret lays out `return ..., err` -> `return ..., nil` in a function whose last
// result is an error.
func (w *walker) ret(r *ast.ReturnStmt) {
	results := w.results[len(w.results)-1]
	if results == nil || len(r.Results) == 0 || len(r.Results) != results.NumFields() {
		return
	}
	last := results.List[len(results.List)-1].Type
	if id, ok := last.(*ast.Ident); !ok || id.Name != "error" {
		return
	}
	e := r.Results[len(r.Results)-1]
	if id, ok := e.(*ast.Ident); ok && id.Name == "nil" {
		return
	}
	id := w.mutant(e, "error -> nil")
	w.add(e, []int{id}, func(sub func(int, int) string) string {
		return fmt.Sprintf("agentReplMutNilErr(%d, %s)", id, sub(w.off(e.Pos()), w.off(e.End())))
	})
}

// isPanic says call is the builtin panic.
func isPanic(call *ast.CallExpr) bool {
	id, ok := call.Fun.(*ast.Ident)
	return ok && id.Name == "panic"
}

// ProductionFiles are the files of the package in dir a schema may mutate:
// every non-test Go file that is not generated, in name order.
func ProductionFiles(dir string) ([]string, error) {
	matches, err := filepath.Glob(filepath.Join(dir, "*.go"))
	if err != nil {
		return nil, fmt.Errorf("gomut: list %s: %w", dir, err)
	}
	var out []string
	for _, path := range matches {
		if strings.HasSuffix(path, "_test.go") {
			continue
		}
		af, err := parser.ParseFile(token.NewFileSet(), path, nil, parser.ParseComments|parser.PackageClauseOnly)
		if err != nil {
			return nil, fmt.Errorf("gomut: parse %s: %w", path, err)
		}
		if ast.IsGenerated(af) {
			continue
		}
		out = append(out, filepath.Base(path))
	}
	sort.Strings(out)
	return out, nil
}

// DropKeys removes the sites of every mutant whose Key is in keys (sites an
// earlier schema over the same files found do not type-check), and answers
// how many mutants went with them.
func (s *Schema) DropKeys(keys map[string]bool) int {
	n := 0
	for _, m := range s.Mutants {
		st := s.byID[m.ID]
		if keys[m.Key()] && !st.dropped {
			st.dropped = true
			n += len(st.mutants)
		}
	}
	return n
}
