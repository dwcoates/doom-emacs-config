package impact

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"agentrepl/testrun/gofunc"
	"agentrepl/testrun/testenv"
)

// Logger is the run's canonical log (run.Log).
type Logger interface {
	Infof(format string, args ...any)
	Errorf(format string, args ...any)
}

// Test is one top-level test's result in a recorded chunk.
type Test struct {
	// Name is the top-level test's name, without its package.
	Name string
	// Outcome is testtimes.Pass, Fail or Skip.
	Outcome string
	// CPU is the CPU seconds the run attributed to the test, when HasCPU.
	CPU    float64
	HasCPU bool
}

// Chunk is one recorded unit's results: the tests it ran and the file its
// harness wrote the windows to.
type Chunk struct {
	Windows string
	Tests   []Test
	// UnnamedFailure marks a chunk that failed with no failing test named:
	// a package-level failure, which selection can never skip.
	UnnamedFailure bool
}

// PackagePass is one package pass whose tests are recorded: the daemon's
// integration package, today.
type PackagePass struct {
	Suite, Package, Tags string
	// ModuleDir is the Go module the package and its programs are in
	// (absolute); TestPackage is the package's pattern in it ("./integration"),
	// ProgramPackages the programs whose coverage the windows hold
	// ("./cmd/agent-repld").
	ModuleDir       string
	TestPackage     string
	ProgramPackages []string
	// Instrumented says whether coverage tracks a package of the program (the
	// packages its build was cover-built with, -coverpkg).
	Instrumented func(importPath string) bool
	// HarnessPrefix is the import path prefix of the test tooling beneath the
	// test package ("agent-repld/integration/"): its files are dependencies of
	// every test, and the test process executes them with no counter to say
	// which.
	HarnessPrefix string
	// Data are the repository-relative files every test depends on: what the
	// processes the tests boot read at run time (prompts, vocabulary), which
	// no counter sees.
	Data []string
	// Facts are the non-file dependencies shared by every test of the pass.
	Facts map[string]string
	// Chunks are the pass's recorded chunks.
	Chunks []Chunk
}

// Miss is a test that failed although selection would have skipped it: the
// shadow mode's one finding.
type Miss struct {
	Key    TestKey
	Reason string
}

// Report is what one run's recording and shadow selection found.
type Report struct {
	Decisions []Decision
	// WouldSkip counts the decisions to skip, and WouldSkipCPU is the CPU the
	// run attributed to those tests: what enforcement would have saved.
	WouldSkip    int
	WouldSkipCPU float64
	// WouldSkipCPUKnown is how many would-skip tests the run had a CPU for.
	WouldSkipCPUKnown int
	Misses            []Miss
	// UnnamedFailures are recorded chunks that failed with no test named.
	UnnamedFailures int
	// Incomplete counts tests whose windows could not name everything they
	// executed: never recorded, always would run.
	Incomplete     int
	RecordsWritten int
	// OverheadCPU is what cutting and reducing the windows cost the harnesses.
	OverheadCPU float64
	// Breadth is, per test, the share of the program's files its record
	// executed.
	Breadth map[TestKey]float64
	// ProgramFiles is how many files the program's instrumented packages hold,
	// the denominator of Breadth.
	ProgramFiles int
	Pruned       PruneResult
}

// Finisher turns a run's recorded chunks into records and shadow decisions.
type Finisher struct {
	Repo  string
	Store Store
	// List lists packages (ExecLister).
	List Lister
	Now  time.Time
	Log  Logger
	// Retention is what the run prunes the store to.
	Retention Retention
}

// Finish records every pass's tests and decides, against the store as it was
// BEFORE this run, which of them selection would have skipped. Every decision
// is logged. A failure to read a window, place a file or write a record is
// returned and logged by the caller; nothing is half-recorded for the pass it
// happens in.
func (f Finisher) Finish(passes []PackagePass) (Report, error) {
	rep := Report{Breadth: map[TestKey]float64{}}
	for _, p := range passes {
		if err := f.finishPass(p, &rep); err != nil {
			return rep, fmt.Errorf("impact: %s:%s: %w", p.Suite, p.Package, err)
		}
	}
	pruned, err := f.Store.Prune(f.Now, f.Retention)
	if err != nil {
		return rep, err
	}
	rep.Pruned = pruned
	return rep, nil
}

func (f Finisher) finishPass(p PackagePass, rep *Report) error {
	// 1. What ran, and what its windows say it executed.
	type ran struct {
		Test
		reach *testenv.ImpactReach
		chunk int
	}
	var tests []ran
	for ci, c := range p.Chunks {
		windows, err := testenv.ReadImpactWindows(c.Windows)
		if err != nil {
			return err
		}
		if b := windows[""]; b != nil {
			rep.OverheadCPU += b.CPUSeconds
		}
		if c.UnnamedFailure {
			rep.UnnamedFailures++
			f.Log.Infof("impact: %s:%s: a chunk failed with no failing test named, so no test of it is a miss and none would be skipped on it", p.Suite, p.Package)
		}
		for _, t := range c.Tests {
			reach := windows[t.Name]
			if reach == nil {
				// A test that reached no daemon: its window is empty.
				reach = &testenv.ImpactReach{}
			} else {
				rep.OverheadCPU += reach.CPUSeconds
			}
			tests = append(tests, ran{Test: t, reach: reach, chunk: ci})
		}
	}
	if len(tests) == 0 {
		return nil
	}

	// 2. The listings and analyzers.
	progPkgs, err := f.List(p.ModuleDir, "", false, p.ProgramPackages...)
	if err != nil {
		return err
	}
	testPkgs, err := f.List(p.ModuleDir, p.Tags, true, p.TestPackage)
	if err != nil {
		return err
	}
	all := mergePkgs(progPkgs, testPkgs)
	testPkg, ok := testVariant(testPkgs)
	if !ok {
		return fmt.Errorf("the listing of %s holds no test variant of the package", p.TestPackage)
	}
	testFiles, err := testFileOf(testPkg)
	if err != nil {
		return err
	}
	var executedRoots, testRoots, harnessRoots []string
	probe := NewAnalyzer(f.Repo, all, Policy{}, nil)
	executed := map[string][]string{}
	for _, t := range tests {
		for _, name := range t.reach.Executed {
			abs, err := probe.FileOf(name)
			if err != nil {
				return fmt.Errorf("%s: %w", t.Name, err)
			}
			executed[t.Name] = append(executed[t.Name], abs)
			executedRoots = append(executedRoots, abs)
		}
		if file, ok := testFiles[t.Name]; ok {
			testRoots = append(testRoots, file)
		}
	}
	for _, pk := range all {
		if strings.HasPrefix(pk.ImportPath, p.HarnessPrefix) && !strings.Contains(pk.ImportPath, " [") && !strings.HasSuffix(pk.ImportPath, ".test") {
			for _, g := range pk.GoFiles {
				harnessRoots = append(harnessRoots, filepath.Join(pk.Dir, g))
			}
		}
	}
	for _, file := range testFiles {
		testRoots = append(testRoots, file)
	}
	testPath := strings.Split(testPkg.ImportPath, " [")[0]
	daemonSide := NewAnalyzer(f.Repo, all, Policy{
		Tracked: p.Instrumented,
		Whole:   func(path string) bool { return !p.Instrumented(path) },
	}, executedRoots)
	testSide := NewAnalyzer(f.Repo, all, Policy{
		Tracked: func(string) bool { return false },
		Whole:   func(path string) bool { return path == testPath },
	}, append(append([]string(nil), testRoots...), harnessRoots...))

	// 3. What every test depends on.
	common := map[string]bool{}
	for _, d := range p.Data {
		common[d] = true
	}
	for _, g := range harnessRoots {
		rel, err := daemonSide.Rel(g)
		if err != nil {
			return err
		}
		common[rel] = true
	}
	harnessReach, err := testSide.Reach(harnessRoots)
	if err != nil {
		return err
	}
	for _, g := range append(harnessReach, testSide.Embedded(harnessRoots)...) {
		rel, err := daemonSide.Rel(g)
		if err != nil {
			return err
		}
		common[rel] = true
	}
	for _, m := range moduleFiles(f.Repo, all) {
		common[m] = true
	}
	programFiles := 0
	for _, pk := range progPkgs {
		if p.Instrumented(pk.ImportPath) && daemonSide.inRepo(pk.Dir) {
			programFiles += len(pk.GoFiles)
		}
	}
	rep.ProgramFiles = programFiles

	// 4. Each test's dependency set, hashed against the tree.
	hashes := map[string]string{}
	hashOf := func(rel string) (string, bool, error) {
		if h, ok := hashes[rel]; ok {
			return h, h != "", nil
		}
		h, err := BlobHash(filepath.Join(f.Repo, filepath.FromSlash(rel)))
		if err != nil {
			if os.IsNotExist(unwrapPathErr(err)) {
				hashes[rel] = ""
				return "", false, nil
			}
			return "", false, err
		}
		hashes[rel] = h
		return h, true, nil
	}
	type entry struct {
		Entry
		test ran
	}
	var entries []entry
	decider := NewDecider(f.Store, p.Facts, Tree(hashOf))
	for _, t := range tests {
		key := TestKey{Suite: p.Suite, Package: p.Package, Tags: p.Tags, Test: t.Name}
		if len(t.reach.Incomplete) > 0 {
			rep.Incomplete++
			d := Decision{Key: key, Reason: "its recording is incomplete: " + strings.Join(t.reach.Incomplete, "; ")}
			f.logDecision(rep, d, t.Test)
			continue
		}
		deps := map[string]bool{}
		for c := range common {
			deps[c] = true
		}
		abs := executed[t.Name]
		reach, err := daemonSide.Reach(abs)
		if err != nil {
			return fmt.Errorf("%s: %w", t.Name, err)
		}
		abs = append(abs, reach...)
		abs = append(abs, daemonSide.Embedded(abs)...)
		if file, ok := testFiles[t.Name]; ok {
			abs = append(abs, file)
			own, err := testSide.Reach([]string{file})
			if err != nil {
				return fmt.Errorf("%s: %w", t.Name, err)
			}
			abs = append(abs, own...)
			abs = append(abs, testSide.Embedded(append(own, file))...)
		}
		for _, a := range abs {
			rel, err := daemonSide.Rel(a)
			if err != nil {
				return fmt.Errorf("%s: %w", t.Name, err)
			}
			deps[rel] = true
		}
		var list []string
		for d := range deps {
			list = append(list, d)
		}
		sort.Strings(list)
		daemonFiles := 0
		for _, a := range executed[t.Name] {
			if daemonSide.fileKeyOf(a) != "" {
				daemonFiles++
			}
		}
		if programFiles > 0 {
			rep.Breadth[key] = float64(daemonFiles) / float64(programFiles)
		}
		d, err := decider.Decide(key)
		if err != nil {
			return err
		}
		f.logDecision(rep, d, t.Test)
		if t.Outcome == Fail && d.WouldSkip {
			rep.Misses = append(rep.Misses, Miss{Key: key, Reason: d.Reason})
			f.Log.Errorf("impact: MISS: %s failed although selection would have skipped it (%s)", key, d.Reason)
		}
		if t.Outcome != Pass && t.Outcome != Fail {
			continue
		}
		entries = append(entries, entry{Entry: Entry{Key: key, Outcome: t.Outcome, Deps: list, Facts: p.Facts}, test: t})
	}

	// 5. The records, after the decisions.
	snapshot := map[string]string{}
	for _, e := range entries {
		for _, d := range e.Deps {
			h, exists, err := hashOf(d)
			if err != nil {
				return err
			}
			if !exists {
				return fmt.Errorf("%s depends on %s, which the tree does not hold", e.Key, d)
			}
			snapshot[d] = h
		}
	}
	plain := make([]Entry, len(entries))
	for i, e := range entries {
		plain[i] = e.Entry
	}
	n, err := f.Store.WriteRun(f.Now, snapshot, plain)
	if err != nil {
		return err
	}
	rep.RecordsWritten += n
	f.Log.Infof("impact: %s:%s: wrote %d records of %d tests, shadow decisions logged above (nothing was skipped)", p.Suite, p.Package, n, len(tests))
	return nil
}

func (f Finisher) logDecision(rep *Report, d Decision, t Test) {
	rep.Decisions = append(rep.Decisions, d)
	verb := "would run"
	if d.WouldSkip {
		verb = "would skip"
		rep.WouldSkip++
		if t.HasCPU {
			rep.WouldSkipCPU += t.CPU
			rep.WouldSkipCPUKnown++
		}
	}
	f.Log.Infof("impact: %s: %s: %s", d.Key, verb, d.Reason)
}

// unwrapPathErr digs the *PathError out of BlobHash's wrapped error.
func unwrapPathErr(err error) error {
	for err != nil {
		if pe, ok := err.(*os.PathError); ok {
			return pe
		}
		u, ok := err.(interface{ Unwrap() error })
		if !ok {
			return err
		}
		err = u.Unwrap()
	}
	return err
}

func (a *Analyzer) fileKeyOf(path string) string { return a.fileKey[path] }

// mergePkgs is the listings joined, each package once by its key.
func mergePkgs(lists ...[]Pkg) []Pkg {
	seen := map[string]bool{}
	var out []Pkg
	for _, l := range lists {
		for _, p := range l {
			if !seen[p.key()] {
				seen[p.key()] = true
				out = append(out, p)
			}
		}
	}
	return out
}

// testVariant answers the test variant of the listed package under test
// ("P [P.test]"): its GoFiles include the test files.
func testVariant(pkgs []Pkg) (Pkg, bool) {
	for _, p := range pkgs {
		if p.ForTest != "" && strings.HasSuffix(p.ImportPath, ".test]") && p.ForTest == strings.Split(p.ImportPath, " [")[0] {
			return p, true
		}
	}
	return Pkg{}, false
}

// testFileOf maps each top-level test of the test variant to the file that
// declares it.
func testFileOf(p Pkg) (map[string]string, error) {
	fset := token.NewFileSet()
	out := map[string]string{}
	for _, name := range p.GoFiles {
		if !strings.HasSuffix(name, "_test.go") {
			continue
		}
		path := filepath.Join(p.Dir, name)
		f, err := parser.ParseFile(fset, path, nil, parser.SkipObjectResolution)
		if err != nil {
			return nil, fmt.Errorf("parse %s: %w", path, err)
		}
		for _, d := range f.Decls {
			if fn, ok := d.(*ast.FuncDecl); ok && fn.Recv == nil && gofunc.IsTestFunc(fn, "Test", "T") {
				out[fn.Name.Name] = path
			}
		}
	}
	return out, nil
}

// moduleFiles lists the go.mod and go.sum of every module of the checkout the
// listing builds from, repository-relative.
func moduleFiles(repo string, pkgs []Pkg) []string {
	seen := map[string]bool{}
	var out []string
	for _, p := range pkgs {
		if p.Module == nil || p.Module.Dir == "" || !strings.HasPrefix(p.Module.Dir, repo) {
			continue
		}
		for _, name := range []string{"go.mod", "go.sum"} {
			abs := filepath.Join(p.Module.Dir, name)
			if _, err := os.Stat(abs); err != nil || seen[abs] {
				continue
			}
			seen[abs] = true
			rel, err := filepath.Rel(repo, abs)
			if err == nil {
				out = append(out, filepath.ToSlash(rel))
			}
		}
	}
	sort.Strings(out)
	return out
}

// ExecToolchain answers the facts of the Go toolchain the run builds and
// tests with, from the go command: the version, the platform, cgo.
func ExecToolchain() (map[string]string, error) {
	out, err := exec.Command("go", "env", "GOVERSION", "GOOS", "GOARCH", "CGO_ENABLED").Output()
	if err != nil {
		return nil, fmt.Errorf("impact: ask the go command for its toolchain: %w", err)
	}
	f := strings.Fields(string(out))
	if len(f) != 4 {
		return nil, fmt.Errorf("impact: go env answered %d values, want 4: %q", len(f), out)
	}
	return map[string]string{"go": f[0], "goos": f[1], "goarch": f[2], "cgo": f[3]}, nil
}
