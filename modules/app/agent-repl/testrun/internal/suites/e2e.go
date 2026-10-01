package suites

import (
	"bufio"
	"bytes"
	"fmt"
	"go/ast"
	"go/build"
	"go/parser"
	"go/token"
	"path/filepath"
	"regexp"
	"sort"
	"strconv"
	"strings"

	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/roster"
)

// E2EPrebuiltEnv names the directory holding every binary the e2e suite would
// otherwise build in its own TestMain. The build unit fills it once; every
// chunk reads it, so N chunks cost one build, not N.
const E2EPrebuiltEnv = "AGENT_REPL_E2E_PREBUILT"

// E2EPrebuildEnv tells the e2e test binary to build every binary into the
// named directory and exit without running a test.
const E2EPrebuildEnv = "AGENT_REPL_E2E_PREBUILD"

// GoTopLevelTests lists the Test functions of the package in dir that the
// default build context compiles, in name order.
func GoTopLevelTests(dir string) ([]string, error) {
	pkg, err := build.ImportDir(dir, 0)
	if err != nil {
		return nil, fmt.Errorf("suites: read the package in %s: %w", dir, err)
	}
	fset := token.NewFileSet()
	var tests []string
	for _, name := range append(append([]string(nil), pkg.TestGoFiles...), pkg.XTestGoFiles...) {
		f, err := parser.ParseFile(fset, filepath.Join(dir, name), nil, parser.SkipObjectResolution)
		if err != nil {
			return nil, fmt.Errorf("suites: parse %s: %w", name, err)
		}
		for _, d := range f.Decls {
			fn, ok := d.(*ast.FuncDecl)
			if !ok || fn.Recv != nil || !isTestName(fn.Name.Name) || !takesTestingT(fn) {
				continue
			}
			tests = append(tests, fn.Name.Name)
		}
	}
	sort.Strings(tests)
	return tests, nil
}

func isTestName(name string) bool {
	if name == "TestMain" || !strings.HasPrefix(name, "Test") {
		return false
	}
	rest := name[len("Test"):]
	return rest == "" || !(rest[0] >= 'a' && rest[0] <= 'z')
}

func takesTestingT(fn *ast.FuncDecl) bool {
	params := fn.Type.Params.List
	if len(params) != 1 {
		return false
	}
	star, ok := params[0].Type.(*ast.StarExpr)
	if !ok {
		return false
	}
	sel, ok := star.X.(*ast.SelectorExpr)
	return ok && sel.Sel.Name == "T"
}

// e2eUnits is one build of the test binary and every system binary, the root
// package's top-level tests split into chunks of that one binary, and every
// other package of the e2e module as an ordinary Go test unit.
func e2eUnits(l Layout, s roster.Suite) (Units, error) {
	dir := l.resolve(s.Path)
	tests, err := GoTopLevelTests(dir)
	if err != nil {
		return Units{}, err
	}
	if len(tests) == 0 {
		return Units{}, fmt.Errorf("suites: %s has no top-level tests in %s", s.Name, dir)
	}
	pkgs, err := goTestedPackages(dir)
	if err != nil {
		return Units{}, err
	}
	work := filepath.Join(l.Work, "e2e")
	bin := filepath.Join(work, "e2e.test")
	prebuilt := filepath.Join(work, "bin")
	buildScript := strings.Join([]string{
		"set -euo pipefail",
		strconv.Quote(filepath.Join(l.Module, "bin", "ensure-e2e-deps.sh")),
		"go test -c -o " + strconv.Quote(bin) + " .",
		E2EPrebuildEnv + "=" + strconv.Quote(prebuilt) + " " + strconv.Quote(bin) + " -test.run '^$'",
	}, "\n")
	buildUnit := spec(s.Name+":build", s.Name, dir, []string{"bash", "-c", buildScript})
	var atomic []run.Spec
	atomic = append(atomic, buildUnit)
	for _, rel := range pkgs {
		if rel == "." {
			continue
		}
		atomic = append(atomic, spec(s.Name+":"+rel, s.Name, dir,
			[]string{"go", "test", "-count=1", "-parallel=1", goPackageArg(rel)}))
	}
	chunk := func(id string, items []string) run.Spec {
		argv := []string{bin, "-test.count=1", "-test.v", "-test.parallel=1", "-test.timeout=45m",
			"-test.run=" + runPattern(items)}
		sp := spec(id, s.Name, dir, argv, E2EPrebuiltEnv+"="+prebuilt)
		sp.Items = func(out []byte) (map[string]float64, error) { return ParseGoTestItems(out, items) }
		return sp
	}
	return Units{
		Atomic: atomic,
		Splits: []Split{{Group: s.Name, Suite: s.Name, Items: tests, Deps: []string{buildUnit.ID}, Chunk: chunk}},
	}, nil
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
