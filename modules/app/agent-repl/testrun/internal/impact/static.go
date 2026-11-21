package impact

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"go/types"
	"path/filepath"
	"sort"
	"strings"
)

// Analyzer finds the declarations a set of source files uses that the
// coverage of a run cannot see: constants, types and variables are never
// executed, so a test that ran code using them depends on the files that
// declare them though no counter says so. It type-checks the packages from
// source (go/types), so what a file uses is exact, never "every file of the
// package that has a type".
//
// Only declarations inside Repo count: the standard library and the module
// cache are covered by the toolchain fact and go.sum.
type Analyzer struct {
	Repo string
	pkgs map[string]Pkg
	// Policy is how Reach treats a declaration by its package.
	Policy Policy
	// bodies names the packages whose function bodies are checked: the ones
	// holding files the analysis starts from, and the ones it follows whole.
	bodies map[string]bool
	// roots are every file a Reach may start from, declared up front so the
	// bodies are known before any package is checked.
	roots map[string]bool

	fset    *token.FileSet
	parsed  map[string]*ast.File
	checked map[string]*checkedPkg
	edges   map[string]*fileEdges
	// fileKey is the package key each in-repo file belongs to.
	fileKey map[string]string
}

type checkedPkg struct {
	pkg  *types.Package
	info *types.Info
	err  error
}

// edge is one use of a declaration in another file.
type edge struct {
	to      string // absolute path of the declaring file
	pkgPath string // import path of the declaring package
	fn      bool   // the declaration is a function or method
}

// fileEdges are a file's uses: all of them, and those inside its
// constant, type and variable declarations.
type fileEdges struct{ all, decls []edge }

// Policy says how a declaration is treated by the package it is in.
type Policy struct {
	// Tracked reports that coverage tracks the package's function
	// executions: a function or method of it that is used but never ran is no
	// dependency.
	Tracked func(pkgPath string) bool
	// Whole reports that a file of the package reached through a use is
	// followed for every use in it, function bodies included, since nothing
	// says which of its code ran. Otherwise only its constant, type and
	// variable declarations are followed.
	Whole func(pkgPath string) bool
}

// NewAnalyzer is an Analyzer over the listed packages of the checkout at
// repo, which may start from any of roots.
func NewAnalyzer(repo string, pkgs []Pkg, policy Policy, roots []string) *Analyzer {
	a := &Analyzer{Repo: repo, Policy: policy, pkgs: map[string]Pkg{}, bodies: map[string]bool{}, roots: map[string]bool{}, fset: token.NewFileSet(),
		parsed: map[string]*ast.File{}, checked: map[string]*checkedPkg{}, edges: map[string]*fileEdges{}, fileKey: map[string]string{}}
	for _, p := range pkgs {
		a.pkgs[p.key()] = p
		if a.inRepo(p.Dir) {
			for _, f := range append(append([]string(nil), p.GoFiles...), p.CgoFiles...) {
				a.fileKey[filepath.Join(p.Dir, f)] = p.key()
			}
		}
	}
	for _, r := range roots {
		a.roots[r] = true
		if key, ok := a.fileKey[r]; ok {
			a.bodies[key] = true
		}
	}
	return a
}

// needsBodies reports whether the package keyed key is checked with its
// function bodies.
func (a *Analyzer) needsBodies(key string, p Pkg) bool {
	if a.bodies[key] {
		return true
	}
	path := p.ImportPath
	if i := strings.Index(path, " ["); i >= 0 {
		path = path[:i]
	}
	return a.Policy.Whole != nil && a.Policy.Whole(path)
}

func (a *Analyzer) inRepo(path string) bool {
	return path != "" && (path == a.Repo || strings.HasPrefix(path, a.Repo+string(filepath.Separator)))
}

// Rel is path relative to the checkout, with forward slashes.
func (a *Analyzer) Rel(path string) (string, error) {
	rel, err := filepath.Rel(a.Repo, path)
	if err != nil || strings.HasPrefix(rel, "..") {
		return "", fmt.Errorf("impact: %s is not inside the checkout %s", path, a.Repo)
	}
	return filepath.ToSlash(rel), nil
}

// FileOf answers the absolute path of the file named as the coverage tools
// name one, "<import path>/<file>.go". A name no listed package of the
// checkout holds is an error: the recording named code the analysis cannot
// place.
func (a *Analyzer) FileOf(covName string) (string, error) {
	dir, file := filepath.Split(covName)
	importPath := strings.TrimSuffix(dir, "/")
	p, ok := a.pkgs[importPath]
	if !ok {
		return "", fmt.Errorf("impact: the coverage file %s names the package %s, which the listing does not hold", covName, importPath)
	}
	if !a.inRepo(p.Dir) {
		return "", fmt.Errorf("impact: the coverage file %s is in %s, outside the checkout", covName, p.Dir)
	}
	return filepath.Join(p.Dir, file), nil
}

func (a *Analyzer) parse(path string) (*ast.File, error) {
	if f, ok := a.parsed[path]; ok {
		return f, nil
	}
	f, err := parser.ParseFile(a.fset, path, nil, parser.SkipObjectResolution)
	if err != nil {
		return nil, fmt.Errorf("impact: parse %s: %w", path, err)
	}
	a.parsed[path] = f
	return f, nil
}

// importerFor is the importer of the package pkg: it checks the imported
// package from source.
type importerFor struct {
	a   *Analyzer
	pkg Pkg
}

func (i importerFor) Import(path string) (*types.Package, error) {
	if path == "unsafe" {
		return types.Unsafe, nil
	}
	key := path
	if m, ok := i.pkg.ImportMap[path]; ok {
		key = m
	}
	c := i.a.check(key)
	if c.pkg == nil {
		return nil, fmt.Errorf("impact: import %s from %s: %w", path, i.pkg.ImportPath, c.err)
	}
	return c.pkg, nil
}

// check type-checks the package keyed key from source, once.
func (a *Analyzer) check(key string) *checkedPkg {
	if c, ok := a.checked[key]; ok {
		return c
	}
	c := &checkedPkg{}
	a.checked[key] = c // a cycle cannot occur in a Go build; the entry stops one anyway
	p, ok := a.pkgs[key]
	if !ok {
		c.err = fmt.Errorf("the listing does not hold %s", key)
		return c
	}
	var files []*ast.File
	for _, f := range append(append([]string(nil), p.GoFiles...), p.CgoFiles...) {
		af, err := a.parse(filepath.Join(p.Dir, f))
		if err != nil {
			c.err = err
			return c
		}
		files = append(files, af)
	}
	inRepo := a.inRepo(p.Dir)
	var firstErr error
	conf := types.Config{
		Importer:         importerFor{a: a, pkg: p},
		FakeImportC:      true,
		IgnoreFuncBodies: !a.needsBodies(key, p),
		Error: func(err error) {
			// A package of the checkout that does not type-check is a
			// defect; a cgo package's C names and everything outside the
			// checkout are checked as far as they go.
			if inRepo && len(p.CgoFiles) == 0 && firstErr == nil {
				firstErr = err
			}
		},
	}
	path := p.ImportPath
	if i := strings.Index(path, " ["); i >= 0 {
		path = path[:i]
	}
	if inRepo {
		c.info = &types.Info{Uses: map[*ast.Ident]types.Object{}}
	}
	pkg, _ := conf.Check(path, a.fset, files, c.info)
	if firstErr != nil {
		c.err = fmt.Errorf("impact: type-check %s: %w", key, firstErr)
		return c
	}
	c.pkg = pkg
	return c
}

// edgesOf computes (once) the uses in the in-repo file path.
func (a *Analyzer) edgesOf(path string) (*fileEdges, error) {
	if e, ok := a.edges[path]; ok {
		return e, nil
	}
	key, ok := a.fileKey[path]
	if !ok {
		return nil, fmt.Errorf("impact: %s belongs to no listed package of the checkout", path)
	}
	c := a.check(key)
	if c.err != nil {
		return nil, c.err
	}
	af, err := a.parse(path)
	if err != nil {
		return nil, err
	}
	e := &fileEdges{}
	collect := func(n ast.Node, into ...*[]edge) {
		ast.Inspect(n, func(n ast.Node) bool {
			id, ok := n.(*ast.Ident)
			if !ok {
				return true
			}
			obj := c.info.Uses[id]
			if obj == nil || obj.Pkg() == nil {
				return true
			}
			if _, isPkg := obj.(*types.PkgName); isPkg {
				return true
			}
			fn := false
			switch o := obj.(type) {
			case *types.Func:
				fn = true
			case *types.Var:
				if !o.IsField() && o.Parent() != o.Pkg().Scope() {
					return true
				}
			case *types.Const, *types.TypeName:
				if obj.Parent() != obj.Pkg().Scope() {
					return true
				}
			default:
				return true
			}
			at := a.fset.Position(obj.Pos()).Filename
			if at == "" || at == path || !a.inRepo(at) {
				return true
			}
			for _, dst := range into {
				*dst = append(*dst, edge{to: at, pkgPath: obj.Pkg().Path(), fn: fn})
			}
			return true
		})
	}
	for _, d := range af.Decls {
		if g, ok := d.(*ast.GenDecl); ok && g.Tok != token.IMPORT {
			collect(d, &e.all, &e.decls)
		} else {
			collect(d, &e.all)
		}
	}
	a.edges[path] = e
	return e, nil
}

// Reach answers the absolute paths of the files in the checkout that roots'
// code uses declarations of, beyond roots themselves, sorted. Every root must
// be one the analyzer was made for.
//
// A root's every use counts, those in function bodies too. A declaration it
// uses that is a function or method counts only unless Policy.Tracked says
// that package's executions are tracked by coverage: a tracked function that
// never ran is no dependency of the run. A file reached is followed in turn
// for its constant, type and variable declarations, or whole when
// Policy.Whole.
func (a *Analyzer) Reach(roots []string) ([]string, error) {
	tracked, whole := a.Policy.Tracked, a.Policy.Whole
	isRoot := map[string]bool{}
	for _, r := range roots {
		if !a.roots[r] {
			return nil, fmt.Errorf("impact: %s is no root this analyzer was made for", r)
		}
		isRoot[r] = true
	}
	type item struct {
		file  string
		whole bool
	}
	var queue []item
	for _, r := range roots {
		queue = append(queue, item{r, true})
	}
	got := map[string]bool{}
	followedWhole := map[string]bool{}
	followedDecls := map[string]bool{}
	for len(queue) > 0 {
		it := queue[0]
		queue = queue[1:]
		if it.whole {
			if followedWhole[it.file] {
				continue
			}
			followedWhole[it.file] = true
		} else {
			if followedDecls[it.file] || followedWhole[it.file] {
				continue
			}
			followedDecls[it.file] = true
		}
		e, err := a.edgesOf(it.file)
		if err != nil {
			return nil, err
		}
		list := e.decls
		if it.whole {
			list = e.all
		}
		for _, ed := range list {
			if ed.fn && tracked(ed.pkgPath) {
				continue
			}
			if !isRoot[ed.to] {
				got[ed.to] = true
			}
			queue = append(queue, item{ed.to, whole(ed.pkgPath)})
		}
	}
	out := make([]string, 0, len(got))
	for f := range got {
		out = append(out, f)
	}
	sort.Strings(out)
	return out, nil
}

// Embedded answers the absolute paths of the files the packages holding
// files embed (//go:embed), sorted.
func (a *Analyzer) Embedded(files []string) []string {
	seen := map[string]bool{}
	var out []string
	for _, f := range files {
		key, ok := a.fileKey[f]
		if !ok {
			continue
		}
		p := a.pkgs[key]
		for _, e := range p.EmbedFiles {
			full := filepath.Join(p.Dir, e)
			if !seen[full] {
				seen[full] = true
				out = append(out, full)
			}
		}
	}
	sort.Strings(out)
	return out
}
