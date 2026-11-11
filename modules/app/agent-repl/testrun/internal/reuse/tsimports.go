package reuse

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
	"sync"
	"syscall"
)

// ImportGraph resolves the local import graph of TypeScript and JavaScript
// files in a checkout, by reading their import specifiers, the way a vitest
// unit's input set is derived.
//
// THE SCAN IS STATIC AND THE PROFILE IS ITS CHECK. It reads every relative
// specifier in a static import or re-export, a side-effect import, a dynamic
// import or require of a literal, a vi.mock of a literal and a `new URL(...,
// import.meta.url)`, and resolves it the way the bundler does (the file, its
// TypeScript twin of a .js specifier, the extensions, an index file). A
// specifier it cannot resolve (a string in a comment, a computed path) adds
// nothing; a file it misses that a test does read is refused by the unit's
// profile, so the miss fails loudly instead of letting a stale pass through.
// Bare specifiers are packages in node_modules, which the suite declares
// Generated.
type ImportGraph struct {
	repo string
	mu   sync.Mutex
	// direct caches each scanned file's resolved local specifiers.
	direct map[string][]string
}

// NewImportGraph is a graph over the checkout at repo.
func NewImportGraph(repo string) *ImportGraph {
	return &ImportGraph{repo: repo, direct: map[string][]string{}}
}

// specifierPatterns find the module specifiers a file names.
var specifierPatterns = []*regexp.Regexp{
	regexp.MustCompile(`\bfrom\s*["']([^"'\n]+)["']`),
	regexp.MustCompile(`\bimport\s*["']([^"'\n]+)["']`),
	regexp.MustCompile(`\bimport\s*\(\s*["']([^"'\n]+)["']\s*\)`),
	regexp.MustCompile(`\brequire\s*\(\s*["']([^"'\n]+)["']\s*\)`),
	regexp.MustCompile(`\bvi\.(?:mock|doMock|importActual)\s*\(\s*["']([^"'\n]+)["']`),
	regexp.MustCompile(`\bnew\s+URL\s*\(\s*["']([^"'\n]+)["']\s*,\s*import\.meta\.url`),
}

// scannedExts are the files whose own specifiers the graph follows.
var scannedExts = map[string]bool{".ts": true, ".tsx": true, ".mts": true, ".cts": true, ".js": true, ".jsx": true, ".mjs": true, ".cjs": true}

// Specifiers is every module specifier src names, in order, each once.
func Specifiers(src []byte) []string {
	seen := map[string]bool{}
	type hit struct {
		at   int
		spec string
	}
	var hits []hit
	for _, re := range specifierPatterns {
		for _, m := range re.FindAllSubmatchIndex(src, -1) {
			hits = append(hits, hit{m[2], string(src[m[2]:m[3]])})
		}
	}
	sort.Slice(hits, func(i, j int) bool { return hits[i].at < hits[j].at })
	var out []string
	for _, h := range hits {
		if !seen[h.spec] {
			seen[h.spec] = true
			out = append(out, h.spec)
		}
	}
	return out
}

// Closure is every file reachable from entries (repository-relative) through
// local specifiers, entries included, sorted. An entry that is not a file is
// an error: the caller named it from the tree.
func (g *ImportGraph) Closure(entries []string) ([]string, error) {
	seen := map[string]bool{}
	queue := append([]string(nil), entries...)
	for _, e := range entries {
		if ok, err := g.isFile(e); err != nil {
			return nil, err
		} else if !ok {
			return nil, fmt.Errorf("reuse: %s is not a file", e)
		}
	}
	for len(queue) > 0 {
		f := queue[0]
		queue = queue[1:]
		if seen[f] {
			continue
		}
		seen[f] = true
		next, err := g.imports(f)
		if err != nil {
			return nil, err
		}
		decl, err := g.declarations(f)
		if err != nil {
			return nil, err
		}
		queue = append(append(queue, next...), decl...)
	}
	out := make([]string, 0, len(seen))
	for f := range seen {
		out = append(out, f)
	}
	sort.Strings(out)
	return out, nil
}

// imports is f's resolved local specifiers (none for a file that is not
// code: a stylesheet, a JSON document, a picture).
func (g *ImportGraph) imports(f string) ([]string, error) {
	if !scannedExts[path.Ext(f)] {
		return nil, nil
	}
	g.mu.Lock()
	cached, ok := g.direct[f]
	g.mu.Unlock()
	if ok {
		return cached, nil
	}
	src, err := os.ReadFile(g.abs(f))
	if err != nil {
		return nil, fmt.Errorf("reuse: read %s: %w", f, err)
	}
	var out []string
	for _, spec := range Specifiers(src) {
		r, ok, err := g.resolve(path.Dir(f), spec)
		if err != nil {
			return nil, fmt.Errorf("reuse: %s imports %q: %w", f, spec, err)
		}
		if ok {
			out = append(out, r)
		}
	}
	globbed, err := g.globs(f, src)
	if err != nil {
		return nil, err
	}
	out = append(out, globbed...)
	g.mu.Lock()
	g.direct[f] = out
	g.mu.Unlock()
	return out, nil
}

// globCallPattern finds an import.meta.glob call's pattern argument: one
// string, or a list of them.
var globCallPattern = regexp.MustCompile(`\bimport\.meta\.glob(?:Eager)?\s*\(\s*(\[[^\]]*\]|["'][^"'\n]+["'])`)

// globs is every file the import.meta.glob calls in src (the file f) match:
// vite expands each into an import of every match. A negated pattern ("!")
// is skipped, which only places more; a pattern that is not relative names
// no file of the package.
func (g *ImportGraph) globs(f string, src []byte) ([]string, error) {
	var out []string
	for _, call := range globCallPattern.FindAllSubmatch(src, -1) {
		for _, lit := range stringLiteral.FindAllSubmatch(call[1], -1) {
			pattern := string(lit[1])
			if !strings.HasPrefix(pattern, "./") && !strings.HasPrefix(pattern, "../") {
				continue
			}
			matches, err := g.expandGlob(path.Join(path.Dir(f), pattern))
			if err != nil {
				return nil, fmt.Errorf("reuse: %s globs %q: %w", f, pattern, err)
			}
			out = append(out, matches...)
		}
	}
	return out, nil
}

// expandGlob is every file of the checkout the repository-relative glob
// matches: "**/" any directories, "*" and "?" within one name.
func (g *ImportGraph) expandGlob(glob string) ([]string, error) {
	if glob == ".." || strings.HasPrefix(glob, "../") {
		return nil, fmt.Errorf("it reaches outside the checkout")
	}
	segments := strings.Split(glob, "/")
	fixed := 0
	for fixed < len(segments) && !strings.ContainsAny(segments[fixed], "*?[{") {
		fixed++
	}
	if fixed == len(segments) {
		ok, err := g.isFile(glob)
		if err != nil || !ok {
			return nil, err
		}
		return []string{glob}, nil
	}
	base := path.Join(segments[:fixed]...)
	re, err := globRegexp(glob)
	if err != nil {
		return nil, err
	}
	var out []string
	root := g.abs(base)
	err = filepath.WalkDir(root, func(abs string, e fs.DirEntry, err error) error {
		if errors.Is(err, fs.ErrNotExist) && abs == root {
			return filepath.SkipAll
		}
		if err != nil {
			return fmt.Errorf("walk %s: %w", abs, err)
		}
		if e.IsDir() {
			if e.Name() == "node_modules" {
				return filepath.SkipDir
			}
			return nil
		}
		rel, err := filepath.Rel(g.repo, abs)
		if err != nil {
			return err
		}
		if rel = filepath.ToSlash(rel); re.MatchString(rel) {
			out = append(out, rel)
		}
		return nil
	})
	sort.Strings(out)
	return out, err
}

// globRegexp spells a glob as an anchored regular expression.
func globRegexp(glob string) (*regexp.Regexp, error) {
	var b strings.Builder
	b.WriteString("^")
	for i := 0; i < len(glob); i++ {
		switch c := glob[i]; {
		case strings.HasPrefix(glob[i:], "**/"):
			b.WriteString("(?:[^/]+/)*")
			i += 2
		case strings.HasPrefix(glob[i:], "**"):
			b.WriteString(".*")
			i++
		case c == '*':
			b.WriteString("[^/]*")
		case c == '?':
			b.WriteString("[^/]")
		case c == '{' || c == '[':
			return nil, fmt.Errorf("the glob %q uses a brace or class this scan does not read", glob)
		default:
			b.WriteString(regexp.QuoteMeta(string(c)))
		}
	}
	b.WriteString("$")
	return regexp.Compile(b.String())
}

// resolve answers the file a relative specifier from directory dir names,
// and whether it names one. A bare specifier names a package, never a file
// of the checkout.
func (g *ImportGraph) resolve(dir, spec string) (string, bool, error) {
	if !strings.HasPrefix(spec, "./") && !strings.HasPrefix(spec, "../") {
		return "", false, nil
	}
	if i := strings.IndexAny(spec, "?#"); i >= 0 {
		spec = spec[:i]
	}
	base := path.Join(dir, spec)
	if base == ".." || strings.HasPrefix(base, "../") {
		return "", false, fmt.Errorf("it resolves outside the checkout")
	}
	var candidates []string
	ext := path.Ext(base)
	switch ext {
	case ".js", ".jsx", ".mjs", ".cjs":
		stem := strings.TrimSuffix(base, ext)
		twin := map[string][]string{".js": {".ts", ".tsx"}, ".jsx": {".tsx"}, ".mjs": {".mts"}, ".cjs": {".cts"}}[ext]
		candidates = append(candidates, base)
		for _, t := range twin {
			candidates = append(candidates, stem+t)
		}
	default:
		candidates = append(candidates, base)
		for _, e := range []string{".ts", ".tsx", ".mts", ".js", ".mjs", ".json"} {
			candidates = append(candidates, base+e)
		}
		for _, e := range []string{".ts", ".tsx", ".js"} {
			candidates = append(candidates, base+"/index"+e)
		}
	}
	for _, c := range candidates {
		ok, err := g.isFile(c)
		if err != nil {
			return "", false, err
		}
		if ok {
			return c, true, nil
		}
	}
	return "", false, nil
}

// declarationTwins are the type declarations the TypeScript compiler reads
// beside a JavaScript module it is pointed at.
var declarationTwins = map[string][]string{".js": {".d.ts"}, ".mjs": {".d.mts"}, ".cjs": {".d.cts"}}

// declarations is every declaration file beside the JavaScript module f.
func (g *ImportGraph) declarations(f string) ([]string, error) {
	ext := path.Ext(f)
	var out []string
	for _, twin := range declarationTwins[ext] {
		d := strings.TrimSuffix(f, ext) + twin
		ok, err := g.isFile(d)
		if err != nil {
			return nil, err
		}
		if ok {
			out = append(out, d)
		}
	}
	return out, nil
}

// isFile reports whether p is a regular file (or a link to one) of the
// checkout.
func (g *ImportGraph) isFile(p string) (bool, error) {
	info, err := os.Stat(g.abs(p))
	if errors.Is(err, fs.ErrNotExist) || errors.Is(err, syscall.ENOTDIR) {
		return false, nil
	}
	if err != nil {
		return false, fmt.Errorf("reuse: stat %s: %w", p, err)
	}
	return info.Mode().IsRegular(), nil
}

func (g *ImportGraph) abs(p string) string { return filepath.Join(g.repo, filepath.FromSlash(p)) }

// addIfFile places p in set when it is a file of the checkout.
func (g *ImportGraph) addIfFile(set *InputSet, p string) error {
	ok, err := g.isFile(p)
	if err != nil || !ok {
		return err
	}
	return set.AddFile(p)
}

// setupFilesPattern finds a vitest config's setupFiles and globalSetup
// entries: a list, or one string.
var setupFilesPattern = regexp.MustCompile(`\b(?:setupFiles|globalSetup)\s*:\s*(\[[^\]]*\]|["'][^"'\n]+["'])`)

// stringLiteral finds a quoted string.
var stringLiteral = regexp.MustCompile(`["']([^"'\n]+)["']`)

// SetupFiles is every setup and global setup file the vitest config at
// config (repository relative) and the configs it imports name, resolved against the config's
// package directory. A setup file named but missing is an error: vitest
// would refuse it too.
func (g *ImportGraph) SetupFiles(config string) ([]string, error) {
	configs, err := g.Closure([]string{config})
	if err != nil {
		return nil, err
	}
	var out []string
	for _, c := range configs {
		if !strings.HasPrefix(path.Base(c), "vitest.") {
			continue
		}
		src, err := os.ReadFile(g.abs(c))
		if err != nil {
			return nil, fmt.Errorf("reuse: read %s: %w", c, err)
		}
		for _, list := range setupFilesPattern.FindAllSubmatch(src, -1) {
			for _, lit := range stringLiteral.FindAllSubmatch(list[1], -1) {
				f := path.Join(path.Dir(c), string(lit[1]))
				ok, err := g.isFile(f)
				if err != nil {
					return nil, err
				}
				if !ok {
					return nil, fmt.Errorf("reuse: %s names the setup file %s, which is not a file", c, f)
				}
				out = append(out, f)
			}
		}
	}
	return out, nil
}
