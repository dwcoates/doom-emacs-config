package gomeasure

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"sort"
	"strconv"
	"strings"
	"time"

	"agentrepl/testrun/internal/goprofile"
	"agentrepl/testrun/internal/gotest"
	"agentrepl/testrun/internal/redundancy"
	"agentrepl/testrun/internal/redundancy/gomut"
	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/suites"
	"agentrepl/testrun/roster"
	"agentrepl/testrun/testenv"
)

// Bounds. Each names what it bounds and what it was sized on (M4 Max, the
// daemon's internal/workspace package, nice 19, 2026-10-14).
const (
	// BuildTimeout bounds one `go test -c`, `go build` or `go list`: the
	// coverage build of internal/workspace measured 36s on a cold cache,
	// the schema 14s.
	BuildTimeout = 90 * time.Second
	// ToolTimeout bounds one `go tool covdata`: measured under 0.2s.
	ToolTimeout = 10 * time.Second
	// FirstContactTimeout bounds a test process before anything about it is
	// measured (discovery): the slowest internal/workspace top-level test
	// measured 0.03s and the slowest daemon integration test 14s (the
	// scheduler's history), so a test past this is broken, not slow.
	FirstContactTimeout = 30 * time.Second
	// TimeoutSlack is added to twice a process's measured healthy latency:
	// its start-up varies by more than twice its few milliseconds of tests.
	TimeoutSlack = 500 * time.Millisecond
	// DefaultHangBound confirms a hang: just over twice the longest real
	// wait any internal/workspace test allows itself (5s; its longer bounds
	// run on a fake clock), so a case still running at it has stalled.
	DefaultHangBound = 11 * time.Second
)

// Suite is one test package whose cases the measurement runs.
type Suite struct {
	// Package is the package, relative to Config.Module.
	Package string
	// Tags are the build tags its tests compile under ("" none).
	Tags string
	// Tests are the test files whose top-level tests are its cases; empty
	// is every test of the package under Tags.
	Tests []string
	// Programs are the roster programs its tests spawn (roster.Programs):
	// the measurement builds them itself, instrumented for coverage and from
	// the mutant schema for mutants.
	Programs []string
	// Kept marks the kept population: its cases are measured so that what
	// they catch counts, and none of them is a removal candidate.
	Kept bool
	// Module is the suite's Go module directory, relative to the agent-repl
	// root, when it is not Config.Module (the e2e suite's own module spawns
	// the daemon whose packages are mutated); "" is Config.Module.
	Module string `json:",omitempty"`
	// Prebuild runs the suite's test binary once to fill a shared prebuilt
	// directory (testenv.Prebuild) every case then reads, as the e2e
	// suite's chunks do in a run.
	Prebuild bool `json:",omitempty"`
}

// Config is one measurement.
type Config struct {
	// Module is the Go module's root, where every go command runs.
	Module string
	// Suites are the test packages measured.
	Suites []Suite
	// Mutate are the packages, relative to Module, whose production files
	// the cases reach are mutated and covered.
	Mutate []string
	// Runs is how many times each case's coverage is measured and unioned.
	Runs int
	// PinCalls are the call names that make a test a log-record assertion.
	PinCalls []string
	// Pins are test names (top-level or leaf) kept whatever they cover.
	Pins []string
	// Scratch is a directory the measurement owns.
	Scratch string
	// Go is the go command.
	Go string
	// HangBound confirms a hang: a case that hung at twice its healthy
	// latency reruns alone this long.
	HangBound time.Duration
	// Root is the agent-repl module root the roster's paths are relative to;
	// "" finds it above Module.
	Root string
	// Delete, when set, is a deletion set to verify (verify.go) instead of
	// a whole kill matrix to minimize: case IDs, or top-level tests standing
	// for all their leaves.
	Delete []string
	// Estimate stops a verification after the unit tier, reporting how many
	// mutants it leaves unsettled and what settling them would cost.
	Estimate bool
}

// leaf is one case: a leaf test of one suite.
type leaf struct {
	suite int
	name  string
}

// suiteState is what the measurement built for one suite.
type suiteState struct {
	Suite
	// module is the suite's module directory; dir its package's.
	module   string
	dir      string
	coverBin string
	mutBin   string
	// prebuilt is its shared prebuilt directory, "" without Prebuild.
	prebuilt string
}

type measurer struct {
	cfg    Config
	exec   Exec
	log    *run.Log
	cost   redundancy.Cost
	suites []*suiteState
	// coverPrograms and mutPrograms are the two programs directories, ""
	// when no suite spawns programs.
	coverPrograms, mutPrograms string
	// hangs are the kills that were a lone hang.
	hangs []string
}

// id is a case's identity in the measurement: its leaf name, qualified by
// its package when the measurement spans several.
func (m *measurer) id(l leaf) string {
	if len(m.suites) == 1 {
		return l.name
	}
	return m.suites[l.suite].Package + ":" + l.name
}

// run runs one process. A cancelled measurement (an interrupt) ends here:
// a process the cancellation cut short would otherwise read as a hang, and a
// hang as a kill.
func (m *measurer) run(ctx context.Context, c Cmd) (Result, error) {
	if err := ctx.Err(); err != nil {
		return Result{}, fmt.Errorf("gomeasure: interrupted: %w", err)
	}
	res, err := m.exec(ctx, c)
	m.cost.Processes++
	m.cost.CPU += res.CPU
	if err == nil && ctx.Err() != nil {
		return Result{}, fmt.Errorf("gomeasure: interrupted during %s: %w", filepath.Base(c.Path), ctx.Err())
	}
	return res, err
}

func (m *measurer) gocmd(ctx context.Context, timeout time.Duration, args ...string) (Result, error) {
	return m.run(ctx, Cmd{Dir: m.cfg.Module, Path: m.cfg.Go, Args: args, Timeout: timeout})
}

// Measure runs the whole measurement.
func Measure(ctx context.Context, cfg Config, exec Exec, log *run.Log) (*redundancy.Measurement, error) {
	if cfg.Runs < 1 {
		return nil, fmt.Errorf("gomeasure: runs must be at least 1, got %d", cfg.Runs)
	}
	if len(cfg.Mutate) == 0 {
		return nil, fmt.Errorf("gomeasure: no package to mutate")
	}
	if len(cfg.Suites) == 0 {
		return nil, fmt.Errorf("gomeasure: no suite to measure")
	}
	if cfg.HangBound <= 0 {
		return nil, fmt.Errorf("gomeasure: the hang bound must be positive, got %s", cfg.HangBound)
	}
	m := &measurer{cfg: cfg, exec: exec, log: log}
	start := time.Now()
	phaseStart := start
	phase := func(name string) {
		m.cost.Phases = append(m.cost.Phases, redundancy.Phase{Name: name, Wall: time.Since(phaseStart)})
		phaseStart = time.Now()
	}
	var subjects []string
	for _, s := range cfg.Suites {
		subject := s.Package
		if len(s.Tests) > 0 {
			subject += "/" + strings.Join(s.Tests, ",")
		}
		if s.Kept {
			subject += " (kept)"
		}
		subjects = append(subjects, subject)
	}
	out := &redundancy.Measurement{
		Adapter: "go",
		Subject: filepath.Base(cfg.Module) + ": " + strings.Join(subjects, "; "),
		Scope:   cfg.Mutate,
	}
	if err := os.MkdirAll(filepath.Join(cfg.Scratch, "tmp"), 0o755); err != nil {
		return nil, fmt.Errorf("gomeasure: %w", err)
	}

	tops := make([][]string, len(cfg.Suites))
	pins := map[leaf]string{}
	for i, s := range cfg.Suites {
		st := &suiteState{Suite: s, module: cfg.Module}
		if s.Module != "" {
			root, err := m.root()
			if err != nil {
				return nil, err
			}
			st.module = filepath.Join(root, s.Module)
		}
		st.dir = filepath.Join(st.module, s.Package)
		m.suites = append(m.suites, st)
		var err error
		if len(s.Tests) > 0 {
			tops[i], err = TestFuncs(st.dir, s.Tests)
		} else {
			tops[i], err = suites.GoTopLevelTestsTagged(st.dir, s.Tags)
		}
		if err != nil {
			return nil, err
		}
		if len(tops[i]) == 0 {
			return nil, fmt.Errorf("gomeasure: %s holds no test", s.Package)
		}
		p, err := LogPins(st.dir, tops[i], cfg.PinCalls)
		if err != nil {
			return nil, err
		}
		for top, why := range p {
			pins[leaf{i, top}] = why
		}
		log.Infof("redundancy: %s: %d top-level tests, %d pinned as log-record assertions", s.Package, len(tops[i]), len(p))
	}
	imports, err := m.importPaths(ctx, cfg.Mutate)
	if err != nil {
		return nil, err
	}
	if out.Modules, err = m.modulePath(ctx); err != nil {
		return nil, err
	}

	if err := m.coverBuilds(ctx); err != nil {
		return nil, err
	}
	phase("coverage build")

	leaves, err := m.discover(ctx, tops)
	if err != nil {
		return nil, err
	}
	log.Infof("redundancy: %d leaf cases", len(leaves))
	phase("discovery")

	var deleted map[leaf]bool
	if len(cfg.Delete) > 0 {
		if deleted, err = m.splitDeleted(leaves); err != nil {
			return nil, err
		}
	} else if cfg.Estimate {
		return nil, fmt.Errorf("gomeasure: an estimate is of a deletion set; name one")
	}
	cases, statements, healthy, err := m.coverage(ctx, leaves, deleted)
	if err != nil {
		return nil, err
	}
	phase("per-case coverage")
	out.Statements = statements
	for i, l := range leaves {
		if why := pins[leaf{l.suite, Top(l.name)}]; why != "" {
			cases[i].Pinned = why
		}
		for _, p := range cfg.Pins {
			if l.name == p || Top(l.name) == p || cases[i].ID == p {
				cases[i].Pinned = "named with -pin"
			}
		}
		if m.suites[l.suite].Kept {
			cases[i].Pinned = "the kept population (" + m.suites[l.suite].Package + "), not a candidate"
		}
	}

	schemas, err := m.schema(ctx, cases, imports)
	if err != nil {
		return nil, err
	}
	phase("mutant schema build")

	if err := m.baseline(ctx, leaves, healthy); err != nil {
		return nil, err
	}
	phase("schema baseline")

	if deleted != nil {
		out.Verification, out.Anomalies, err = m.verify(ctx, schemas, imports, leaves, cases, healthy, deleted)
		out.Hangs = m.hangs
		if err != nil {
			return nil, err
		}
		phase("deletion-set verification")
		out.Cases = cases
		m.cost.Wall = time.Since(start)
		out.Cost = m.cost
		return out, nil
	}
	out.Mutants, out.Anomalies, err = m.mutate(ctx, schemas, imports, leaves, cases, healthy)
	out.Hangs = m.hangs
	if err != nil {
		return nil, err
	}
	phase("mutant runs")
	byKey := map[string]int{}
	for i, c := range cases {
		byKey[c.ID] = i
	}
	for _, r := range out.Mutants {
		for _, by := range r.KilledBy {
			cases[byKey[by]].Kills = append(cases[byKey[by]].Kills, r.Key)
		}
	}
	out.Cases = cases
	m.cost.Wall = time.Since(start)
	out.Cost = m.cost
	return out, nil
}

// importPaths maps each package to mutate, relative to the module (a
// pattern such as ./... names several), to its import path and directory.
func (m *measurer) importPaths(ctx context.Context, pkgs []string) (map[string]string, error) {
	res, err := m.gocmd(ctx, BuildTimeout, append([]string{"list", "-f", "{{.ImportPath}}\t{{.Dir}}"}, pkgs...)...)
	if err != nil {
		return nil, err
	}
	if res.Exit != 0 || res.TimedOut {
		return nil, fmt.Errorf("gomeasure: go list %s failed:\n%s", strings.Join(pkgs, " "), res.Text())
	}
	out := map[string]string{}
	for _, l := range res.Lines {
		imp, dir, ok := strings.Cut(l.Text, "\t")
		if !ok {
			return nil, fmt.Errorf("gomeasure: go list printed %q", l.Text)
		}
		out[imp] = dir
	}
	if len(out) == 0 {
		return nil, fmt.Errorf("gomeasure: go list named no package for %s", strings.Join(pkgs, " "))
	}
	return out, nil
}

// modulePath is the measured module's path, the one system its blocks
// belong to.
func (m *measurer) modulePath(ctx context.Context) ([]string, error) {
	res, err := m.gocmd(ctx, BuildTimeout, "list", "-m")
	if err != nil {
		return nil, err
	}
	if res.Exit != 0 || res.TimedOut || len(res.Lines) != 1 {
		return nil, fmt.Errorf("gomeasure: go list -m in %s failed or named no one module:\n%s", m.cfg.Module, res.Text())
	}
	return []string{strings.TrimSpace(res.Lines[0].Text)}, nil
}

// goBuild runs one build in dir and answers its result; a failure to run it
// at all is the error.
func (m *measurer) goBuild(ctx context.Context, dir string, argv []string) (Result, error) {
	return m.run(ctx, Cmd{Dir: dir, Path: m.cfg.Go, Args: argv[1:], Timeout: BuildTimeout})
}

func buildFailed(argv []string, res Result) error {
	return fmt.Errorf("gomeasure: %s failed (exit %d, timed out %t):\n%s", strings.Join(argv, " "), res.Exit, res.TimedOut, res.Text())
}

// testBuild is the `go test -c` of one suite's binary.
func (m *measurer) testBuild(st *suiteState, bin string, flags ...string) []string {
	argv := []string{"go", "test", "-c", "-vet=off", "-o", bin}
	if st.Tags != "" {
		argv = append(argv, "-tags", st.Tags)
	}
	return append(append(argv, flags...), st.Package)
}

// root is the agent-repl module root the roster resolves in.
func (m *measurer) root() (string, error) {
	if m.cfg.Root != "" {
		return m.cfg.Root, nil
	}
	return FindRoot(m.cfg.Module)
}

// FindRoot is the agent-repl module root: the nearest directory at or above
// dir holding the roster.
func FindRoot(dir string) (string, error) {
	abs, err := filepath.Abs(dir)
	if err != nil {
		return "", fmt.Errorf("gomeasure: resolve %s: %w", dir, err)
	}
	for d := abs; ; d = filepath.Dir(d) {
		if _, err := os.Stat(filepath.Join(d, "testrun", "roster", "roster.go")); err == nil {
			return d, nil
		}
		if d == filepath.Dir(d) {
			return "", fmt.Errorf("gomeasure: no directory at or above %s holds testrun/roster", dir)
		}
	}
}

// programs is every roster program some suite spawns, in roster order.
func (m *measurer) programs() ([]roster.Program, error) {
	want := map[string]bool{}
	for _, st := range m.suites {
		for _, name := range st.Programs {
			if _, ok := roster.LookupProgram(name); !ok {
				return nil, fmt.Errorf("gomeasure: %s spawns the program %q, which roster.Programs does not hold", st.Package, name)
			}
			want[name] = true
		}
	}
	var out []roster.Program
	for _, p := range roster.Programs {
		if want[p.Name] {
			out = append(out, p)
		}
	}
	return out, nil
}

// buildPrograms builds every spawned program into the programs directory of
// a layout over work: instrumented when cover, with extra flags otherwise
// (the mutant overlay). It answers the directory, "" when nothing spawns.
func (m *measurer) buildPrograms(ctx context.Context, work string, cover bool, extra ...string) (string, []Result, error) {
	progs, err := m.programs()
	if err != nil || len(progs) == 0 {
		return "", nil, err
	}
	root, err := m.root()
	if err != nil {
		return "", nil, err
	}
	l := suites.Layout{Module: root, Work: work, Coverage: cover}
	if err := os.MkdirAll(l.ProgramsDir(), 0o755); err != nil {
		return "", nil, fmt.Errorf("gomeasure: %w", err)
	}
	var failed []Result
	for _, p := range progs {
		argv, dir, err := suites.ProgramBuild(l, p, extra...)
		if err != nil {
			return "", nil, err
		}
		res, err := m.goBuild(ctx, dir, argv)
		if err != nil {
			return "", nil, err
		}
		if res.Exit != 0 || res.TimedOut {
			if extra == nil {
				return "", nil, buildFailed(argv, res)
			}
			failed = append(failed, res)
		}
	}
	return l.ProgramsDir(), failed, nil
}

// coverBuilds builds each suite's coverage binary and the instrumented
// programs. A suite that spawns programs is measured through them, so its
// own test binary is a plain one.
func (m *measurer) coverBuilds(ctx context.Context) error {
	for i, st := range m.suites {
		st.coverBin = filepath.Join(m.cfg.Scratch, fmt.Sprintf("cover-%02d.test", i))
		var flags []string
		if len(st.Programs) == 0 {
			flags = []string{"-cover", "-covermode=atomic", "-coverpkg=" + strings.Join(m.cfg.Mutate, ",")}
		}
		argv := m.testBuild(st, st.coverBin, flags...)
		res, err := m.goBuild(ctx, st.module, argv)
		if err != nil {
			return err
		}
		if res.Exit != 0 || res.TimedOut {
			return buildFailed(argv, res)
		}
	}
	dir, _, err := m.buildPrograms(ctx, filepath.Join(m.cfg.Scratch, "cover-programs"), true)
	if err != nil {
		return err
	}
	m.coverPrograms = dir
	for i, st := range m.suites {
		if st.Prebuild {
			if err := m.prebuild(ctx, st, i); err != nil {
				return err
			}
		}
	}
	return nil
}

// prebuild runs st's test binary once as its shared prebuild, as a run's
// e2e build unit does. What it builds (the shim bundle) is no system a
// mutant touches, so both phases read the one directory; it runs with the
// coverage phase's programs so the harness builds nothing of its own.
func (m *measurer) prebuild(ctx context.Context, st *suiteState, i int) error {
	dir := filepath.Join(m.cfg.Scratch, fmt.Sprintf("prebuilt-%02d", i))
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return fmt.Errorf("gomeasure: %w", err)
	}
	spawned := filepath.Join(m.cfg.Scratch, "prebuild-spawned")
	// coverEnv names no prebuilt directory yet: a process either fills one
	// or reads one (testenv.BinaryMode).
	env := append([]string{testenv.Prebuild + "=" + dir}, m.coverEnv(st, spawned)...)
	res, err := m.run(ctx, Cmd{Dir: st.dir, Path: st.coverBin, Args: []string{"-test.run", "^$"}, Env: append(m.testEnv(), env...), Timeout: BuildTimeout})
	if err != nil {
		return err
	}
	if res.Exit != 0 || res.TimedOut {
		return fmt.Errorf("gomeasure: the %s prebuild failed (exit %d, timed out %t):\n%s", st.Package, res.Exit, res.TimedOut, res.Text())
	}
	st.prebuilt = dir
	return nil
}

func (m *measurer) testEnv() []string {
	return []string{"GOMAXPROCS=2", "TMPDIR=" + filepath.Join(m.cfg.Scratch, "tmp"), gomut.EnvMutant + "="}
}

// coverEnv is the environment of a coverage-phase run of st whose spawned
// systems write their counters under spawned.
func (m *measurer) coverEnv(st *suiteState, spawned string) []string {
	var env []string
	if st.prebuilt != "" {
		env = append(env, testenv.Prebuilt+"="+st.prebuilt)
	}
	if len(st.Programs) == 0 {
		return env
	}
	return append(env, testenv.Programs+"="+m.coverPrograms, testenv.ProgramsInstrumented+"=1", testenv.Coverage+"="+spawned)
}

// mutEnv is the environment of a mutant-phase run of st.
func (m *measurer) mutEnv(st *suiteState) []string {
	var env []string
	if st.prebuilt != "" {
		env = append(env, testenv.Prebuilt+"="+st.prebuilt)
	}
	if len(st.Programs) == 0 {
		return env
	}
	return append(env, testenv.Programs+"="+m.mutPrograms)
}

func (m *measurer) runTests(ctx context.Context, st *suiteState, bin, pattern string, timeout time.Duration, extra []string, env ...string) (Result, Run, error) {
	args := append([]string{"-test.run", pattern, "-test.v", "-test.parallel=1", "-test.timeout", timeout.String()}, extra...)
	// The outer bound is the backstop for a process -test.timeout cannot
	// end (a hang before or after the tests); the inner one names the test.
	res, err := m.run(ctx, Cmd{Dir: st.dir, Path: bin, Args: args, Env: append(m.testEnv(), env...), Timeout: 2 * timeout})
	if err != nil {
		return res, Run{}, err
	}
	return res, ParseRun(res.Lines), nil
}

// discover runs each top-level test once and returns every leaf case: a test
// that started no subtest of its own.
func (m *measurer) discover(ctx context.Context, tops [][]string) ([]leaf, error) {
	dir := filepath.Join(m.cfg.Scratch, "discover")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return nil, fmt.Errorf("gomeasure: %w", err)
	}
	var leaves []leaf
	for i, st := range m.suites {
		for _, top := range tops[i] {
			res, r, err := m.runTests(ctx, st, st.coverBin, gotest.TopPattern([]string{top}), FirstContactTimeout, []string{"-test.gocoverdir", dir}, m.coverEnv(st, dir)...)
			if err != nil {
				return nil, err
			}
			if res.Exit != 0 || r.Outcomes[top] != Pass && r.Outcomes[top] != Skip {
				return nil, fmt.Errorf("gomeasure: %s %s does not pass with no mutant (exit %d):\n%s", st.Package, top, res.Exit, res.Text())
			}
			var names []string
			for n := range r.Ran {
				names = append(names, n)
			}
			sort.Strings(names)
			for _, n := range names {
				if !isAncestor(n, names) {
					leaves = append(leaves, leaf{i, n})
				}
			}
		}
	}
	return leaves, nil
}

// coverage measures every leaf alone, Runs times (in a verification, only a
// deleted leaf: a kept leaf's reach is measured once, since under-measuring
// it can only put back more), and returns its cases
// (blocks unioned over the runs and over its test binary and the systems it
// spawned; cost the median runtime), every block of the covered packages
// with its statement count, and each leaf's healthy process latency (its
// slowest run's wall).
func (m *measurer) coverage(ctx context.Context, leaves []leaf, deleted map[leaf]bool) ([]redundancy.Case, map[string]int, map[leaf]time.Duration, error) {
	elapsed := make([][]time.Duration, len(leaves))
	healthy := map[leaf]time.Duration{}
	roots := make([]string, len(leaves))
	for r := 0; r < m.cfg.Runs; r++ {
		for i, l := range leaves {
			if r > 0 && deleted != nil && !deleted[l] {
				continue
			}
			st := m.suites[l.suite]
			roots[i] = filepath.Join(m.cfg.Scratch, "cov", strconv.Itoa(i))
			self := filepath.Join(roots[i], strconv.Itoa(r), "self")
			spawned := filepath.Join(roots[i], strconv.Itoa(r), "spawned")
			for _, d := range []string{self, spawned} {
				if err := os.MkdirAll(d, 0o755); err != nil {
					return nil, nil, nil, fmt.Errorf("gomeasure: %w", err)
				}
			}
			res, run, err := m.runTests(ctx, st, st.coverBin, RunPattern(l.name), FirstContactTimeout, []string{"-test.gocoverdir", self}, m.coverEnv(st, spawned)...)
			if err != nil {
				return nil, nil, nil, err
			}
			if !run.Ran[l.name] {
				return nil, nil, nil, fmt.Errorf("gomeasure: the pattern %s did not run %s %s:\n%s", RunPattern(l.name), st.Package, l.name, res.Text())
			}
			if o := run.Outcomes[l.name]; res.Exit != 0 || (o != Pass && o != Skip) {
				return nil, nil, nil, fmt.Errorf("gomeasure: %s failed on measurement run %d with no mutant, a flaky or broken case (exit %d):\n%s", m.id(l), r+1, res.Exit, res.Text())
			}
			elapsed[i] = append(elapsed[i], run.Elapsed[l.name])
			healthy[l] = max(healthy[l], res.Wall)
		}
		m.log.Infof("redundancy: coverage run %d of %d done", r+1, m.cfg.Runs)
	}
	cases := make([]redundancy.Case, len(leaves))
	statements := map[string]int{}
	for i, l := range leaves {
		c := redundancy.Case{ID: m.id(l), Cost: median(elapsed[i])}
		dirs, err := goprofile.CounterDirs(roots[i])
		if err != nil {
			return nil, nil, nil, err
		}
		if len(dirs) > 0 {
			blocks, all, err := m.profile(ctx, dirs, filepath.Join(m.cfg.Scratch, "prof-"+strconv.Itoa(i)+".txt"))
			if err != nil {
				return nil, nil, nil, fmt.Errorf("gomeasure: %s: %w", c.ID, err)
			}
			for _, b := range all {
				statements[b.Key()] = b.Statements
			}
			c.Blocks = blocks
		}
		cases[i] = c
	}
	return cases, statements, healthy, nil
}

// profile merges counter dirs into one profile: the reached block keys of
// the mutated packages, and every block of them.
func (m *measurer) profile(ctx context.Context, dirs []string, out string) ([]string, []goprofile.Block, error) {
	res, err := m.gocmd(ctx, ToolTimeout, "tool", "covdata", "textfmt", "-i="+strings.Join(dirs, ","), "-o="+out)
	if err != nil {
		return nil, nil, err
	}
	if res.Exit != 0 || res.TimedOut {
		return nil, nil, fmt.Errorf("covdata textfmt failed:\n%s", res.Text())
	}
	text, err := os.ReadFile(out)
	if err != nil {
		return nil, nil, fmt.Errorf("read the profile: %w", err)
	}
	all, covered, err := goprofile.ParseProfile(string(text))
	if err != nil {
		return nil, nil, fmt.Errorf("the profile: %w", err)
	}
	var keys []string
	for _, b := range covered {
		keys = append(keys, b.Key())
	}
	return keys, all, nil
}

func median(ds []time.Duration) time.Duration {
	s := slices.Clone(ds)
	slices.Sort(s)
	return s[len(s)/2]
}

var compileErr = regexp.MustCompile(`(?m)^(\S+\.go):(\d+):(\d+): `)

// schema lays out the mutants of every reached production file of the
// mutated packages and builds them into every suite's mutant binary and the
// spawned programs, dropping each site a compile error names until all
// build.
func (m *measurer) schema(ctx context.Context, cases []redundancy.Case, imports map[string]string) ([]*gomut.Schema, error) {
	reached := map[string]map[string]bool{}
	for _, c := range cases {
		for _, key := range c.Blocks {
			file, _, _ := strings.Cut(key, ":")
			imp, base := filepath.Dir(file), filepath.Base(file)
			if _, ok := imports[imp]; !ok {
				continue
			}
			if reached[imp] == nil {
				reached[imp] = map[string]bool{}
			}
			reached[imp][base] = true
		}
	}
	var imps []string
	for imp := range reached {
		imps = append(imps, imp)
	}
	sort.Strings(imps)
	var schemas []*gomut.Schema
	next := 1
	for _, imp := range imps {
		var files []string
		for f := range reached[imp] {
			if !strings.HasSuffix(f, "_test.go") {
				files = append(files, f)
			}
		}
		s, err := gomut.Build(imports[imp], files, next)
		if err != nil {
			return nil, err
		}
		next += len(s.Mutants)
		schemas = append(schemas, s)
	}
	if len(schemas) == 0 {
		return nil, fmt.Errorf("gomeasure: the cases reach no production file of %s", strings.Join(m.cfg.Mutate, ", "))
	}
	for attempt := 0; ; attempt++ {
		dir := filepath.Join(m.cfg.Scratch, "overlay-"+strconv.Itoa(attempt))
		if err := os.MkdirAll(dir, 0o755); err != nil {
			return nil, fmt.Errorf("gomeasure: %w", err)
		}
		replace := map[string]string{}
		for _, s := range schemas {
			r, err := s.Overlay(dir)
			if err != nil {
				return nil, err
			}
			for k, v := range r {
				replace[k] = v
			}
		}
		ov := filepath.Join(dir, "overlay.json")
		if err := gomut.WriteOverlay(ov, replace); err != nil {
			return nil, err
		}
		var failed []Result
		for i, st := range m.suites {
			st.mutBin = filepath.Join(m.cfg.Scratch, fmt.Sprintf("mutants-%02d.test", i))
			res, err := m.goBuild(ctx, st.module, m.testBuild(st, st.mutBin, "-overlay", ov))
			if err != nil {
				return nil, err
			}
			if res.Exit != 0 || res.TimedOut {
				failed = append(failed, res)
			}
		}
		progs, progFailed, err := m.buildPrograms(ctx, filepath.Join(m.cfg.Scratch, "mutant-programs-"+strconv.Itoa(attempt)), false, "-overlay", ov)
		if err != nil {
			return nil, err
		}
		m.mutPrograms = progs
		failed = append(failed, progFailed...)
		if len(failed) == 0 {
			live := 0
			for _, s := range schemas {
				live += len(s.Live())
			}
			m.log.Infof("redundancy: mutant schema built after %d attempts, %d of %d mutants live", attempt+1, live, next-1)
			return schemas, nil
		}
		dropped := 0
		var text strings.Builder
		for _, res := range failed {
			text.WriteString(res.Text())
		}
		for _, mm := range compileErr.FindAllStringSubmatch(text.String(), -1) {
			path := mm[1]
			if !filepath.IsAbs(path) {
				path = filepath.Join(m.cfg.Module, path)
			}
			line, _ := strconv.Atoi(mm[2])
			col, _ := strconv.Atoi(mm[3])
			for _, s := range schemas {
				if !s.Holds(path) {
					continue
				}
				ids, err := s.Drop(path, line, col)
				if err != nil {
					return nil, fmt.Errorf("%w\n%s", err, text.String())
				}
				dropped += len(ids)
			}
		}
		if dropped == 0 {
			return nil, fmt.Errorf("gomeasure: the mutant schema does not build, and no error names a mutated file:\n%s", text.String())
		}
	}
}

// baseline runs every case in the schema binaries with no mutant active.
// They must all pass: anything else is the rewrite changing behavior.
func (m *measurer) baseline(ctx context.Context, leaves []leaf, healthy map[leaf]time.Duration) error {
	for i, st := range m.suites {
		var bound time.Duration
		var mine []leaf
		var tops []string
		for _, l := range leaves {
			if l.suite == i {
				mine = append(mine, l)
				bound += healthy[l]
				if !slices.Contains(tops, Top(l.name)) {
					tops = append(tops, Top(l.name))
				}
			}
		}
		res, r, err := m.runTests(ctx, st, st.mutBin, gotest.TopPattern(tops), 2*bound+TimeoutSlack, nil, m.mutEnv(st)...)
		if err != nil {
			return err
		}
		for _, l := range mine {
			if r.Outcomes[l.name] != Pass && r.Outcomes[l.name] != Skip {
				return fmt.Errorf("gomeasure: %s does not pass in the mutant schema with no mutant active: the rewrite changed behavior (exit %d):\n%s", m.id(l), res.Exit, res.Text())
			}
		}
	}
	return nil
}

// mutate runs every live mutant against the cases that reach it.
func (m *measurer) mutate(ctx context.Context, schemas []*gomut.Schema, imports map[string]string, leaves []leaf, cases []redundancy.Case, healthy map[leaf]time.Duration) ([]redundancy.MutantResult, []string, error) {
	reaching, err := newReachIndex(imports, leaves, cases)
	if err != nil {
		return nil, nil, err
	}
	var results []redundancy.MutantResult
	var anomalies []string
	total := 0
	for _, s := range schemas {
		total += len(s.Mutants)
	}
	done := 0
	for _, s := range schemas {
		for _, d := range s.Dropped() {
			results = append(results, redundancy.MutantResult{Key: d.Key(), Status: "dropped"})
			done++
		}
		for _, mu := range s.Live() {
			done++
			if done%max(1, total/10) == 0 {
				m.log.Infof("redundancy: %d of %d mutants done", done, total)
			}
			covering := reaching.of(mu)
			if covering == nil {
				results = append(results, redundancy.MutantResult{Key: mu.Key(), Status: "unreached"})
				continue
			}
			killed, anom, err := m.killers(ctx, mu, covering, healthy)
			if err != nil {
				return nil, nil, err
			}
			anomalies = append(anomalies, anom...)
			r := redundancy.MutantResult{Key: mu.Key(), Status: "survived"}
			for _, l := range killed {
				r.KilledBy = append(r.KilledBy, m.id(l))
			}
			sort.Strings(r.KilledBy)
			if len(r.KilledBy) > 0 {
				r.Status = "killed"
			}
			results = append(results, r)
		}
	}
	return results, anomalies, nil
}

// reachIndex answers which leaves reach a mutant: those whose blocks include
// the innermost block holding its position.
type reachIndex struct {
	dirImport map[string]string
	reach     map[string][]leaf
	idx       goprofile.Index
}

func newReachIndex(imports map[string]string, leaves []leaf, cases []redundancy.Case) (*reachIndex, error) {
	r := &reachIndex{dirImport: map[string]string{}, reach: map[string][]leaf{}}
	for imp, dir := range imports {
		r.dirImport[dir] = imp
	}
	var all []goprofile.Block
	seen := map[string]bool{}
	for i, c := range cases {
		for _, key := range c.Blocks {
			r.reach[key] = append(r.reach[key], leaves[i])
			if seen[key] {
				continue
			}
			seen[key] = true
			b, _, err := goprofile.ParseLine(key + " 1 1")
			if err != nil {
				return nil, err
			}
			all = append(all, b)
		}
	}
	r.idx = goprofile.NewIndex(all)
	return r, nil
}

// of is the leaves reaching mu; nil when none does.
func (r *reachIndex) of(mu gomut.Mutant) []leaf {
	file := r.dirImport[filepath.Dir(mu.File)] + "/" + filepath.Base(mu.File)
	held := r.idx.Holding(file, mu.Line, mu.Col)
	if len(held) == 0 {
		return nil
	}
	return r.reach[held[0].Key()]
}

// killers runs one mutant against the given leaves, suite by suite, and
// answers the ones that fail.
func (m *measurer) killers(ctx context.Context, mu gomut.Mutant, leaves []leaf, healthy map[leaf]time.Duration) ([]leaf, []string, error) {
	var killed []leaf
	var anomalies []string
	for i := range m.suites {
		var names []string
		bounds := map[string]time.Duration{}
		for _, l := range leaves {
			if l.suite == i {
				names = append(names, l.name)
				bounds[l.name] = healthy[l]
			}
		}
		if len(names) == 0 {
			continue
		}
		k, anom, err := m.killersIn(ctx, m.suites[i], mu, names, bounds)
		if err != nil {
			return nil, nil, err
		}
		for _, n := range k {
			killed = append(killed, leaf{i, n})
		}
		anomalies = append(anomalies, anom...)
	}
	return killed, anomalies, nil
}

// killersIn runs one mutant against the leaves of one suite that reach it
// and returns the leaves that fail. All of them run in one process first; a
// leaf that process never reported (it panicked or hung partway) runs again
// alone, and a hang is a kill only when it hangs alone too.
func (m *measurer) killersIn(ctx context.Context, st *suiteState, mu gomut.Mutant, leaves []string, healthy map[string]time.Duration) ([]string, []string, error) {
	env := append([]string{gomut.EnvMutant + "=" + strconv.Itoa(mu.ID)}, m.mutEnv(st)...)
	pending := map[string]bool{}
	for _, l := range leaves {
		pending[l] = true
	}
	var killed, hung, anomalies []string
	settle := func(r Run, asked []string) {
		parentFailed := map[string]bool{}
		for _, l := range asked {
			if r.Outcomes[Top(l)] == Fail {
				parentFailed[Top(l)] = true
			}
		}
		// A failed subtest fails its parent too, whether or not it was asked.
		anyLeafFailed := map[string]bool{}
		for n, o := range r.Outcomes {
			if o == Fail && strings.Contains(n, "/") {
				anyLeafFailed[Top(n)] = true
			}
		}
		for _, l := range asked {
			switch {
			case r.Outcomes[l] == Fail:
				killed = append(killed, l)
				delete(pending, l)
			case r.Outcomes[l] == Pass || r.Outcomes[l] == Skip:
				if parentFailed[Top(l)] && !anyLeafFailed[Top(l)] {
					// The top-level test's own body failed after its
					// subtests; every leaf's run executes that body.
					killed = append(killed, l)
				}
				delete(pending, l)
			}
		}
		for n, o := range r.Outcomes {
			if o == Fail && !slices.Contains(leaves, n) && !isAncestor(n, leaves) {
				anomalies = append(anomalies, fmt.Sprintf("%s %s failed under %s, which it does not reach", st.Package, n, mu.Key()))
			}
		}
	}
	var tops []string
	var bound time.Duration
	for _, l := range leaves {
		if !slices.Contains(tops, Top(l)) {
			tops = append(tops, Top(l))
		}
		bound += healthy[l]
	}
	_, r, err := m.runTests(ctx, st, st.mutBin, gotest.TopPattern(tops), 2*bound+TimeoutSlack, nil, env...)
	if err != nil {
		return nil, nil, err
	}
	settle(r, leaves)
	for _, l := range leaves {
		if !pending[l] {
			continue
		}
		timeout := 2*healthy[l] + TimeoutSlack
		res, r, err := m.runTests(ctx, st, st.mutBin, RunPattern(l), timeout, nil, env...)
		if err != nil {
			return nil, nil, err
		}
		settle(r, []string{l})
		if !pending[l] {
			continue
		}
		// Unreported alone: it panicked before its result line, or hung.
		if res.Exit == 0 && !res.TimedOut && len(r.TimedOut) == 0 {
			return nil, nil, fmt.Errorf("gomeasure: %s ran clean under %s yet reported no result:\n%s", l, mu.Key(), res.Text())
		}
		delete(pending, l)
		if res.TimedOut || len(r.TimedOut) > 0 {
			hung = append(hung, l)
			continue
		}
		killed = append(killed, l)
	}
	confirmed, err := m.confirmHangs(ctx, st, mu, env, hung)
	if err != nil {
		return nil, nil, err
	}
	killed = append(killed, confirmed...)
	sort.Strings(killed)
	return slices.Compact(killed), anomalies, nil
}

// confirmHangs judges the leaves that hung alone at twice their healthy
// latency. A mutant can merely SLOW a case (a fallback timer), and a slowed
// case is not a kill. So one of them reruns at the hang bound, past every
// real wait the measured tests allow themselves: still hung there, the
// mutant stalls, and every leaf that hung under it is a kill; finished, each
// hung leaf reruns at the hang bound and is a kill only if it fails or hangs
// again.
func (m *measurer) confirmHangs(ctx context.Context, st *suiteState, mu gomut.Mutant, env []string, hung []string) ([]string, error) {
	if len(hung) == 0 {
		return nil, nil
	}
	lone := func(l string) (bool, error) {
		res, r, err := m.runTests(ctx, st, st.mutBin, RunPattern(l), m.cfg.HangBound, nil, env...)
		if err != nil {
			return false, err
		}
		switch r.Outcomes[l] {
		case Pass, Skip:
			return false, nil
		case Fail:
			return true, nil
		}
		if res.Exit == 0 && !res.TimedOut && len(r.TimedOut) == 0 {
			return false, fmt.Errorf("gomeasure: %s ran clean under %s yet reported no result:\n%s", l, mu.Key(), res.Text())
		}
		return true, nil
	}
	first, err := lone(hung[0])
	if err != nil {
		return nil, err
	}
	if first {
		m.hangs = append(m.hangs, fmt.Sprintf("%s: %d cases hung (confirmed on %s at %s): %s", mu.Key(), len(hung), hung[0], m.cfg.HangBound, strings.Join(hung, ", ")))
		return hung, nil
	}
	var killed []string
	for _, l := range hung[1:] {
		k, err := lone(l)
		if err != nil {
			return nil, err
		}
		if k {
			killed = append(killed, l)
		}
	}
	m.hangs = append(m.hangs, fmt.Sprintf("%s: %d cases were only slowed (%s finished within %s); %d failed when rerun", mu.Key(), len(hung), hung[0], m.cfg.HangBound, len(killed)))
	return killed, nil
}

// isAncestor says n is a parent test of one of names.
func isAncestor(n string, names []string) bool {
	for _, l := range names {
		if strings.HasPrefix(l, n+"/") {
			return true
		}
	}
	return false
}
