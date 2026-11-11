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

	"agentrepl/testrun/gofunc"
	"agentrepl/testrun/internal/gotest"
	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/testtimes"
	"agentrepl/testrun/roster"
	"agentrepl/testrun/testenv"
)

// EVERY GO PACKAGE IS SPLIT THE SAME WAY: its top-level tests run from its
// test binary in as many chunks as the planner chooses. A package of two
// quick tests stays one chunk; a package whose tests run for minutes is
// spread across cores like any other suite.
//
// THE BINARIES ARE BUILT BY ONE UNIT PER MODULE PASS (GoBuild,
// internal/gobuild), never one per package: every chunk depends on its
// module's build, and runs its package's binary through `testrun exec-test`,
// which fails the chunk with the package's compiler output when that one
// package did not compile.
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

// goVetTagged is a vet unit over pattern under one build tag, exactly the
// analyzers goVet runs.
func goVetTagged(suite, dir, tag, pattern string) run.Spec {
	return spec(suite+":vet["+tag+"]", suite, dir,
		append(append([]string{"go", "vet", "-tags", tag}, goTestVetFlags...), pattern))
}

// goVets is a Go suite's vet units: the module's own, and one per VetTags tag
// over the whole module.
func goVets(s roster.Suite, dir string) []run.Spec {
	vets := []run.Spec{goVet(s.Name, dir)}
	for _, tag := range s.VetTags {
		vets = append(vets, goVetTagged(s.Name, dir, tag, "./..."))
	}
	return vets
}

// goPkg is one Go package's units.
type goPkg struct {
	Suite string
	// Module is the module directory; Rel the package's path under it.
	Module, Rel string
	// Bin is where the module build writes the test binary (gobuild.BinPath).
	Bin string
	// Self is the testrun binary every chunk runs the binary through.
	Self string
	// After is the unit that must end before the binary runs: the module's
	// build, or the package's prebuild after it.
	After string
	// CovDir is where every chunk writes its coverage counters; "" runs
	// without coverage.
	CovDir string
	// ChunkEnv is added to every chunk's environment.
	ChunkEnv []string
	// Timeout is each chunk's -test.timeout.
	Timeout string
	// Tags are the build tags the package's tests compile under; "" is none.
	Tags string
	// Parallel is how a chunk overlaps its top-level tests and the cores it
	// holds doing so (roster.Suite.TestParallel); the zero value is one test
	// at a time on one core.
	Parallel roster.Parallel
	// Programs are the shared programs this package's tests run
	// (roster.Suite.Programs): every chunk waits for their builds and is told
	// the run's programs directory.
	Programs []string
	// ProgramsDir is the run's programs directory (Layout.ProgramsDir).
	ProgramsDir string
	// SpawnedCovDir, under coverage, is where the systems this package's
	// tests spawn write their counters (testenv.Coverage): every chunk
	// carries it, with the programs marked instrumented, so the reach of an
	// integration or e2e tier counts. "" spawns uninstrumented systems.
	SpawnedCovDir string
}

// spawnedEnv is the environment a process carries whose spawned systems
// write coverage counters (SpawnedCovDir), or nil.
//
// GOCOVERDIR too, so a system spawned by a harness that only inherits its
// environment (the sidecar's, the store's, the shim's lock helper) has a
// directory for its counters: an instrumented binary started without one
// warns on its stderr, which the tests read. A cover-built test binary writes
// its own counters by its -test.gocoverdir flag, which takes precedence.
func (p goPkg) spawnedEnv() []string {
	if p.SpawnedCovDir == "" {
		return nil
	}
	return []string{testenv.ProgramsInstrumented + "=1", testenv.Coverage + "=" + p.SpawnedCovDir, "GOCOVERDIR=" + p.SpawnedCovDir}
}

// prebuild is the unit that runs the package's TestMain once, after the
// module build, to fill the shared directory dir every chunk then reads
// (testenv.Prebuild), running the shell lines before first. Every chunk of
// the package waits for it.
func (p *goPkg) prebuild(dir string, before ...string) run.Spec {
	lines := append([]string{"set -euo pipefail"}, before...)
	lines = append(lines, testenv.Prebuild+"="+strconv.Quote(dir)+" "+strconv.Quote(p.Self)+" exec-test "+strconv.Quote(p.Bin)+" -test.run '^$'")
	// A shared prebuild builds the systems its chunks spawn, so under
	// coverage it builds them instrumented too.
	sp := spec(p.group()+":prebuild", p.Suite, p.Module, []string{"bash", "-c", strings.Join(lines, "\n")}, p.spawnedEnv()...)
	sp.Deps = []string{p.After}
	p.After = sp.ID
	p.ChunkEnv = append(p.ChunkEnv, testenv.Prebuilt+"="+dir)
	return sp
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

// CompiledUnitID is the unit of a package whose test files hold no test:
// whether they compiled is its whole check (gobuild.Check).
func CompiledUnitID(group string) string { return group + ":compiled" }

// units is the package's split, or, when it has no tests, the unit checking
// that its test files compiled.
func (p goPkg) units() (*run.Spec, *Split, error) {
	dir := filepath.Join(p.Module, p.Rel)
	tests, err := GoTopLevelTestsTagged(dir, p.Tags)
	if err != nil {
		return nil, nil, err
	}
	if len(tests) == 0 {
		// Nothing to run: compiling the test files is the whole check.
		check := spec(CompiledUnitID(p.group()), p.Suite, dir, []string{p.Self, "check-test", p.Bin})
		check.Deps = []string{p.After}
		return &check, nil, nil
	}
	if p.SpawnedCovDir != "" {
		if err := os.MkdirAll(p.SpawnedCovDir, 0o755); err != nil {
			return nil, nil, fmt.Errorf("suites: create %s: %w", p.SpawnedCovDir, err)
		}
		p.ChunkEnv = append(p.ChunkEnv, p.spawnedEnv()...)
	}
	if p.CovDir != "" {
		if err := os.MkdirAll(p.CovDir, 0o755); err != nil {
			return nil, nil, fmt.Errorf("suites: create %s: %w", p.CovDir, err)
		}
	}
	timeout := p.Timeout
	if timeout == "" {
		timeout = "10m"
	}
	if len(p.Programs) > 0 {
		p.ChunkEnv = append(p.ChunkEnv, testenv.Programs+"="+p.ProgramsDir)
	}
	// A NAMED CHUNK RUNS ONE TEST PATH: `-test.run` matches each level of a
	// path separately, so two paths of different depths cannot share one
	// pattern, and a selection gives each path its own chunk.
	named := func(id string, items []string, names []string) run.Spec {
		pattern := gotest.TopPattern(items)
		switch len(names) {
		case 0:
		case 1:
			pattern = gotest.PathPattern(names[0])
		default:
			panic(fmt.Sprintf("suites: a Go chunk runs one named test path, got %d: %v", len(names), names))
		}
		argv := []string{p.Self, "exec-test", p.Bin, "-test.count=1", "-test.v", "-test.parallel=" + strconv.Itoa(max(p.Parallel.Tests, 1)),
			"-test.timeout=" + timeout, "-test.run=" + pattern}
		if p.CovDir != "" {
			argv = append(argv, "-test.gocoverdir="+p.CovDir)
		}
		// A test binary runs in its package's directory, as `go test` runs it.
		sp := spec(id, p.Suite, dir, argv, p.ChunkEnv...)
		sp.Items = func(out []byte) (map[string]float64, error) { return ParseGoTestItems(out, items) }
		sp.Tests = func(out []byte, _ bool) ([]testtimes.Test, error) { return testtimes.ParseGo(p.Suite, p.Rel, out) }
		sp.Display = QuietGoTestOutput
		return sp
	}
	chunk := func(id string, items []string) run.Spec { return named(id, items, nil) }
	deps := []string{p.After}
	for _, name := range p.Programs {
		deps = append(deps, ProgramUnitID(name))
	}
	return nil, &Split{
		Group: p.group(), Suite: p.Suite, Items: tests, Deps: deps, Package: p.Rel, Tags: p.Tags,
		// Test binaries and every expensive integration binary are already
		// compiled by the dependency unit. Go's -v timings omit time that
		// parallel subtests spend paused, so wall-minus-items can otherwise
		// misclassify that queue time as process startup and suppress splitting.
		OverheadCap: 1,
		Chunk:       chunk,
		Named:       named,
		Slots:       p.Parallel.Slots,
	}, nil
}

// GoTopLevelTests lists everything `go test` runs at top level in the package
// in dir under the default build context: Test and Fuzz functions, and the
// examples that carry an output comment. Name order.
func GoTopLevelTests(dir string) ([]string, error) {
	return GoTopLevelTestsTagged(dir, "")
}

// GoTopLevelTestsTagged is GoTopLevelTests under build TAGS (comma-separated).
func GoTopLevelTestsTagged(dir, tags string) ([]string, error) {
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
				if gofunc.IsTestFunc(fn, "Test", "T") || gofunc.IsTestFunc(fn, "Fuzz", "F") {
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

// ParseGoTestItems reads `go test -v` output's result tree. A parent's own
// duration includes sequential descendants but excludes time that parallel
// descendants spend paused; max(parent, sum(children)) counts either shape
// without double-counting the sequential one.
func ParseGoTestItems(out []byte, want []string) (map[string]float64, error) {
	nodes := map[string]float64{}
	for line := range strings.SplitSeq(string(out), "\n") {
		m := gotest.ResultLine.FindStringSubmatch(line)
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
