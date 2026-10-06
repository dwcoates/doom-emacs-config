package suites

import (
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

// goVet is a Go module's vet unit: exactly the analyzers `go test` would have
// run over every package of the module in dir, which the compiled test
// binaries never run.
func goVet(suite, dir string) run.Spec {
	return spec(suite+":vet", suite, dir, append(append([]string{"go", "vet"}, goTestVetFlags...), "./..."))
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
	// Tags are the build tags the package's tests compile under; "" is none.
	Tags string
}

// sharePrebuilt makes this package's build process compile its test binary,
// run TestMain once to fill a shared directory, and point every chunk there.
func (p *goPkg) sharePrebuilt(dir string, before ...string) {
	lines := append([]string{"set -euo pipefail"}, before...)
	tags := ""
	if p.Tags != "" {
		tags = "-tags " + strconv.Quote(p.Tags) + " "
	}
	lines = append(lines,
		"go test -c "+tags+"-o "+strconv.Quote(p.Bin)+" "+strconv.Quote(goPackageArg(p.Rel)),
		testenv.Prebuild+"="+strconv.Quote(dir)+" "+strconv.Quote(p.Bin)+" -test.run '^$'",
	)
	p.Build = []string{"bash", "-c", strings.Join(lines, "\n")}
	p.ChunkEnv = append(p.ChunkEnv, testenv.Prebuilt+"="+dir)
}

// group is the package's chunk group, which is also its unit ID prefix.
// A tagged package's group names its tags, so it never collides with the
// same directory's untagged pass.
func (p goPkg) group() string {
	if p.Tags != "" {
		return p.Suite + ":" + p.Rel + "[" + p.Tags + "]"
	}
	return p.Suite + ":" + p.Rel
}

// units is the package's build unit and, when it has tests, its split.
func (p goPkg) units() (run.Spec, *Split, error) {
	dir := filepath.Join(p.Module, p.Rel)
	tests, err := goTopLevelTestsTagged(dir, p.Tags)
	if err != nil {
		return run.Spec{}, nil, err
	}
	argv := p.Build
	if argv == nil {
		argv = []string{"go", "test", "-c", "-o", p.Bin}
		if p.Tags != "" {
			argv = append(argv, "-tags", p.Tags)
		}
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
	return buildUnit, &Split{
		Group: p.group(), Suite: p.Suite, Items: tests, Deps: []string{buildUnit.ID},
		// Test binaries and every expensive integration binary are already
		// compiled by the dependency unit. Go's -v timings omit time that
		// parallel subtests spend paused, so wall-minus-items can otherwise
		// misclassify that queue time as process startup and suppress splitting.
		OverheadCap: 1,
		Chunk:       chunk,
	}, nil
}

// GoTopLevelTests lists everything `go test` runs at top level in the package
// in dir under the default build context: Test and Fuzz functions, and the
// examples that carry an output comment. Name order.
func GoTopLevelTests(dir string) ([]string, error) {
	return goTopLevelTestsTagged(dir, "")
}

// goTopLevelTestsTagged is GoTopLevelTests under build TAGS (comma-separated).
func goTopLevelTestsTagged(dir, tags string) ([]string, error) {
	ctx := build.Default
	if tags != "" {
		ctx.BuildTags = append(append([]string(nil), ctx.BuildTags...), strings.Split(tags, ",")...)
	}
	pkg, err := ctx.ImportDir(dir, 0)
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

var goTestResult = regexp.MustCompile(`^\s*--- (PASS|FAIL|SKIP): (\S+) \(([0-9.]+)s\)$`)

// ParseGoTestItems reads `go test -v` output's result tree. A parent's own
// duration includes sequential descendants but excludes time that parallel
// descendants spend paused; max(parent, sum(children)) counts either shape
// without double-counting the sequential one.
func ParseGoTestItems(out []byte, want []string) (map[string]float64, error) {
	nodes := map[string]float64{}
	for line := range strings.SplitSeq(string(out), "\n") {
		m := goTestResult.FindStringSubmatch(line)
		if m == nil {
			continue
		}
		secs, err := strconv.ParseFloat(m[3], 64)
		if err != nil {
			return nil, fmt.Errorf("unreadable seconds in %q: %w", line, err)
		}
		nodes[m[2]] = secs
	}
	got := make(map[string]float64, len(want))
	var total func(string) float64
	total = func(name string) float64 {
		children := 0.0
		for child := range nodes {
			if strings.LastIndex(child, "/") == len(name) && strings.HasPrefix(child, name+"/") {
				children += total(child)
			}
		}
		return max(nodes[name], children)
	}
	for _, name := range want {
		if _, ok := nodes[name]; ok {
			got[name] = total(name)
		}
	}
	return got, matchItems(got, want)
}

var goTestNoise = regexp.MustCompile(`^(=== (RUN|PAUSE|CONT|NAME) |\s*--- PASS: |PASS$)`)

// QuietGoTestOutput is a passing chunk's output without the -v bookkeeping
// the item timings needed: the run log keeps what a passing `go test` would
// have printed, and a failing chunk shows everything.
func QuietGoTestOutput(out []byte, passed bool) []byte {
	if !passed || len(out) == 0 {
		return out
	}
	// Split, never scanned: a line has no length limit here, so there is no
	// failure to fall back from and no line can be lost.
	var b bytes.Buffer
	for line := range strings.SplitSeq(strings.TrimSuffix(string(out), "\n"), "\n") {
		if !goTestNoise.MatchString(line) {
			b.WriteString(line)
			b.WriteByte('\n')
		}
	}
	return b.Bytes()
}
