package suites

import (
	"bufio"
	"bytes"
	"fmt"
	"go/ast"
	"go/build"
	"go/doc"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strconv"
	"strings"

	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/testenv"
)

// EVERY GO PACKAGE IS SPLIT THE SAME WAY: one unit compiles its test binary,
// and its top-level tests run from that binary in as many chunks as the
// planner chooses. A package of two quick tests stays one chunk; a package
// whose tests run for minutes is spread across cores like any other suite.
//
// `go test` would also vet the package. A compiled binary is not vetted, so
// each module gets one vet unit running exactly the analyzers `go test` runs
// (goTestVetFlags), and nothing `go test` checked is lost.

// goTestVetFlags is the analyzer subset `go test` vets with (`go help test`).
var goTestVetFlags = []string{
	"-atomic", "-bool", "-buildtags", "-directive", "-errorsas",
	"-ifaceassert", "-nilfunc", "-printf", "-stringintconv", "-tests",
}

// goPkg is one Go package's units.
type goPkg struct {
	Suite string
	// Module is the module directory; Rel the package's path under it.
	Module, Rel string
	// Bin is where the build unit writes the test binary.
	Bin string
	// CovDir is where every chunk writes its coverage counters; "" runs
	// without coverage.
	CovDir string
	// Build replaces the default build command (`go test -c`) when the
	// package needs more than its binary built first.
	Build []string
	// ChunkEnv is added to every chunk's environment.
	ChunkEnv []string
	// Timeout is each chunk's -test.timeout.
	Timeout string
}

// sharePrebuilt makes this package's build process compile its test binary,
// run TestMain once to fill a shared directory, and point every chunk there.
func (p *goPkg) sharePrebuilt(dir string, before ...string) {
	lines := append([]string{"set -euo pipefail"}, before...)
	lines = append(lines,
		"go test -c -o "+strconv.Quote(p.Bin)+" "+strconv.Quote(goPackageArg(p.Rel)),
		testenv.Prebuild+"="+strconv.Quote(dir)+" "+strconv.Quote(p.Bin)+" -test.run '^$'",
	)
	p.Build = []string{"bash", "-c", strings.Join(lines, "\n")}
	p.ChunkEnv = append(p.ChunkEnv, testenv.Prebuilt+"="+dir)
}

// group is the package's chunk group, which is also its unit ID prefix.
func (p goPkg) group() string { return p.Suite + ":" + p.Rel }

// units is the package's build unit and, when it has tests, its split.
func (p goPkg) units() (run.Spec, *Split, error) {
	dir := filepath.Join(p.Module, p.Rel)
	tests, err := GoTopLevelTests(dir)
	if err != nil {
		return run.Spec{}, nil, err
	}
	argv := p.Build
	if argv == nil {
		argv = []string{"go", "test", "-c", "-o", p.Bin}
		if p.CovDir != "" {
			argv = append(argv, "-cover", "-coverpkg=./...")
		}
		argv = append(argv, goPackageArg(p.Rel))
	}
	buildUnit := spec(p.group()+":build", p.Suite, p.Module, argv)
	if len(tests) == 0 {
		// Nothing to run: compiling the test files is the whole check.
		return buildUnit, nil, nil
	}
	if p.CovDir != "" {
		if err := os.MkdirAll(p.CovDir, 0o755); err != nil {
			return run.Spec{}, nil, fmt.Errorf("suites: create %s: %w", p.CovDir, err)
		}
	}
	timeout := p.Timeout
	if timeout == "" {
		timeout = "10m"
	}
	chunk := func(id string, items []string) run.Spec {
		argv := []string{p.Bin, "-test.count=1", "-test.v", "-test.parallel=1",
			"-test.timeout=" + timeout, "-test.run=" + runPattern(items)}
		if p.CovDir != "" {
			argv = append(argv, "-test.gocoverdir="+p.CovDir)
		}
		// A test binary runs in its package's directory, as `go test` runs it.
		sp := spec(id, p.Suite, dir, argv, p.ChunkEnv...)
		sp.Items = func(out []byte) (map[string]float64, error) { return ParseGoTestItems(out, items) }
		sp.Display = QuietGoTestOutput
		return sp
	}
	return buildUnit, &Split{Group: p.group(), Suite: p.Suite, Items: tests, Deps: []string{buildUnit.ID}, Chunk: chunk}, nil
}

// GoTopLevelTests lists everything `go test` runs at top level in the package
// in dir under the default build context: Test and Fuzz functions, and the
// examples that carry an output comment. Name order.
func GoTopLevelTests(dir string) ([]string, error) {
	pkg, err := build.ImportDir(dir, 0)
	if err != nil {
		if _, noGo := err.(*build.NoGoError); noGo {
			return nil, nil
		}
		return nil, fmt.Errorf("suites: read the package in %s: %w", dir, err)
	}
	fset := token.NewFileSet()
	var tests []string
	for _, files := range [][]string{pkg.TestGoFiles, pkg.XTestGoFiles} {
		var parsed []*ast.File
		for _, name := range files {
			f, err := parser.ParseFile(fset, filepath.Join(dir, name), nil, parser.ParseComments|parser.SkipObjectResolution)
			if err != nil {
				return nil, fmt.Errorf("suites: parse %s: %w", name, err)
			}
			parsed = append(parsed, f)
			for _, d := range f.Decls {
				fn, ok := d.(*ast.FuncDecl)
				if !ok || fn.Recv != nil {
					continue
				}
				if isTestFunc(fn, "Test", "T") || isTestFunc(fn, "Fuzz", "F") {
					tests = append(tests, fn.Name.Name)
				}
			}
		}
		for _, ex := range doc.Examples(parsed...) {
			if ex.Output != "" || ex.EmptyOutput {
				tests = append(tests, "Example"+ex.Name)
			}
		}
	}
	sort.Strings(tests)
	return tests, nil
}

// isTestFunc reports a func PrefixXxx(x *testing.<param>), the shape `go test`
// runs (TestMain excluded).
func isTestFunc(fn *ast.FuncDecl, prefix, param string) bool {
	name := fn.Name.Name
	if name == "TestMain" || !strings.HasPrefix(name, prefix) {
		return false
	}
	if rest := name[len(prefix):]; rest != "" && rest[0] >= 'a' && rest[0] <= 'z' {
		return false
	}
	params := fn.Type.Params.List
	if len(params) != 1 || len(params[0].Names) > 1 {
		return false
	}
	star, ok := params[0].Type.(*ast.StarExpr)
	if !ok {
		return false
	}
	sel, ok := star.X.(*ast.SelectorExpr)
	return ok && sel.Sel.Name == param
}

// runPattern matches exactly the named top-level tests (and their subtests).
func runPattern(tests []string) string {
	q := make([]string, len(tests))
	for i, t := range tests {
		q[i] = regexp.QuoteMeta(t)
	}
	return "^(" + strings.Join(q, "|") + ")$"
}

var goTestResult = regexp.MustCompile(`^--- (PASS|FAIL|SKIP): (\S+) \(([0-9.]+)s\)$`)

// ParseGoTestItems reads `go test -v` output's top-level result lines.
// Subtests are indented and never match.
func ParseGoTestItems(out []byte, want []string) (map[string]float64, error) {
	got := map[string]float64{}
	sc := bufio.NewScanner(bytes.NewReader(out))
	sc.Buffer(make([]byte, 1024*1024), 64*1024*1024)
	for sc.Scan() {
		m := goTestResult.FindStringSubmatch(sc.Text())
		if m == nil {
			continue
		}
		secs, err := strconv.ParseFloat(m[3], 64)
		if err != nil {
			return nil, fmt.Errorf("unreadable seconds in %q: %w", sc.Text(), err)
		}
		got[m[2]] = secs
	}
	if err := sc.Err(); err != nil {
		return nil, err
	}
	return got, matchItems(got, want)
}

var goTestNoise = regexp.MustCompile(`^(=== (RUN|PAUSE|CONT|NAME) |\s*--- PASS: |PASS$)`)

// QuietGoTestOutput is a passing chunk's output without the -v bookkeeping
// the item timings needed: the run log keeps what a passing `go test` would
// have printed, and a failing chunk shows everything.
func QuietGoTestOutput(out []byte, passed bool) []byte {
	if !passed {
		return out
	}
	var b bytes.Buffer
	sc := bufio.NewScanner(bytes.NewReader(out))
	sc.Buffer(make([]byte, 1024*1024), 64*1024*1024)
	for sc.Scan() {
		if !goTestNoise.MatchString(sc.Text()) {
			b.Write(sc.Bytes())
			b.WriteByte('\n')
		}
	}
	if sc.Err() != nil {
		// A line too long to scan: show the output whole rather than lose it.
		return out
	}
	return b.Bytes()
}
