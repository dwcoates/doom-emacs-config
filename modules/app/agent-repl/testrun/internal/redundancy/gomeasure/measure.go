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

	"agentrepl/testrun/internal/gotest"
	"agentrepl/testrun/internal/redundancy"
	"agentrepl/testrun/internal/redundancy/gomut"
	"agentrepl/testrun/internal/run"
)

// Bounds. Each names what it bounds and what it was sized on (M4 Max, the
// daemon's internal/workspace package, nice 19, 2026-10-14).
const (
	// BuildTimeout bounds one `go test -c` or `go list`: the coverage build
	// of internal/workspace measured 36s on a cold cache, the schema 14s.
	BuildTimeout = 90 * time.Second
	// ToolTimeout bounds one `go tool covdata`: measured under 0.2s.
	ToolTimeout = 10 * time.Second
	// FirstContactTimeout bounds a test process before anything about it is
	// measured (discovery): the slowest top-level test there measured 0.03s
	// and the process 0.1s, so a test past this is broken, not slow.
	FirstContactTimeout = 20 * time.Second
	// TimeoutSlack is added to twice a process's measured healthy latency:
	// its start-up varies by more than twice its few milliseconds of tests.
	TimeoutSlack = 500 * time.Millisecond
	// DefaultHangBound confirms a hang: just over twice the longest real
	// wait any internal/workspace test allows itself (5s; its longer bounds
	// run on a fake clock), so a case still running at it has stalled.
	DefaultHangBound = 11 * time.Second
)

// Config is one measurement.
type Config struct {
	// Module is the Go module's root, where every go command runs.
	Module string
	// Package is the package under measurement, relative to Module.
	Package string
	// Tests are the test files (base names) whose cases are the candidates.
	Tests []string
	// Mutate are the packages, relative to Module, whose production files
	// the cases reach are mutated. The coverage build covers exactly these.
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
}

type measurer struct {
	cfg  Config
	exec Exec
	log  *run.Log
	cost redundancy.Cost
	// hangs are the kills that were a lone hang.
	hangs []string
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
	out := &redundancy.Measurement{
		Adapter: "go",
		Subject: filepath.Join(filepath.Base(cfg.Module), cfg.Package, strings.Join(cfg.Tests, ",")),
		Scope:   cfg.Mutate,
	}
	if err := os.MkdirAll(filepath.Join(cfg.Scratch, "tmp"), 0o755); err != nil {
		return nil, fmt.Errorf("gomeasure: %w", err)
	}

	pkgDir := filepath.Join(cfg.Module, cfg.Package)
	tops, err := TestFuncs(pkgDir, cfg.Tests)
	if err != nil {
		return nil, err
	}
	pins, err := LogPins(pkgDir, tops, cfg.PinCalls)
	if err != nil {
		return nil, err
	}
	imports, err := m.importPaths(ctx, cfg.Mutate)
	if err != nil {
		return nil, err
	}
	log.Infof("redundancy: %d top-level tests in %s, %d pinned as log-record assertions", len(tops), strings.Join(cfg.Tests, ", "), len(pins))

	coverBin := filepath.Join(cfg.Scratch, "cover.test")
	if err := m.build(ctx, coverBin, "-cover", "-covermode=atomic", "-coverpkg="+strings.Join(cfg.Mutate, ",")); err != nil {
		return nil, err
	}
	phase("coverage build")

	leaves, err := m.discover(ctx, coverBin, tops)
	if err != nil {
		return nil, err
	}
	log.Infof("redundancy: %d leaf cases", len(leaves))
	phase("discovery")

	cases, statements, healthy, err := m.coverage(ctx, coverBin, leaves)
	if err != nil {
		return nil, err
	}
	phase("per-case coverage")
	out.Statements = statements
	for i := range cases {
		if why := pins[Top(cases[i].ID)]; why != "" {
			cases[i].Pinned = why
		}
		for _, p := range cfg.Pins {
			if cases[i].ID == p || Top(cases[i].ID) == p {
				cases[i].Pinned = "named with -pin"
			}
		}
	}

	schemas, bin, err := m.schema(ctx, cases, imports)
	if err != nil {
		return nil, err
	}
	phase("mutant schema build")

	if err := m.baseline(ctx, bin, tops, leaves, healthy); err != nil {
		return nil, err
	}
	phase("schema baseline")

	out.Mutants, out.Anomalies, err = m.mutate(ctx, bin, schemas, imports, cases, healthy)
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

// importPaths maps each package to mutate, relative to the module, to its
// import path and directory.
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
	if len(out) != len(pkgs) {
		return nil, fmt.Errorf("gomeasure: go list named %d packages for %d asked", len(out), len(pkgs))
	}
	return out, nil
}

func (m *measurer) build(ctx context.Context, bin string, flags ...string) error {
	args := append([]string{"test", "-c", "-vet=off", "-o", bin}, flags...)
	res, err := m.gocmd(ctx, BuildTimeout, append(args, m.cfg.Package)...)
	if err != nil {
		return err
	}
	if res.Exit != 0 || res.TimedOut {
		return fmt.Errorf("gomeasure: go %s failed (exit %d, timed out %t):\n%s", strings.Join(args, " "), res.Exit, res.TimedOut, res.Text())
	}
	return nil
}

func (m *measurer) testEnv() []string {
	return []string{"GOMAXPROCS=2", "TMPDIR=" + filepath.Join(m.cfg.Scratch, "tmp"), gomut.EnvMutant + "="}
}

func (m *measurer) runTests(ctx context.Context, bin, pattern string, timeout time.Duration, extra []string, env ...string) (Result, Run, error) {
	args := append([]string{"-test.run", pattern, "-test.v", "-test.parallel=1", "-test.timeout", timeout.String()}, extra...)
	// The outer bound is the backstop for a process -test.timeout cannot
	// end (a hang before or after the tests); the inner one names the test.
	res, err := m.run(ctx, Cmd{Dir: filepath.Join(m.cfg.Module, m.cfg.Package), Path: bin, Args: args, Env: append(m.testEnv(), env...), Timeout: 2 * timeout})
	if err != nil {
		return res, Run{}, err
	}
	return res, ParseRun(res.Lines), nil
}

// discover runs each top-level test once and returns every leaf case: a test
// that started no subtest of its own.
func (m *measurer) discover(ctx context.Context, bin string, tops []string) ([]string, error) {
	dir := filepath.Join(m.cfg.Scratch, "discover")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return nil, fmt.Errorf("gomeasure: %w", err)
	}
	var leaves []string
	for _, top := range tops {
		res, r, err := m.runTests(ctx, bin, gotest.TopPattern([]string{top}), FirstContactTimeout, []string{"-test.gocoverdir", dir})
		if err != nil {
			return nil, err
		}
		if res.Exit != 0 || r.Outcomes[top] != Pass && r.Outcomes[top] != Skip {
			return nil, fmt.Errorf("gomeasure: %s does not pass with no mutant (exit %d):\n%s", top, res.Exit, res.Text())
		}
		var names []string
		for n := range r.Ran {
			names = append(names, n)
		}
		sort.Strings(names)
		for _, n := range names {
			leaf := true
			for _, o := range names {
				if strings.HasPrefix(o, n+"/") {
					leaf = false
					break
				}
			}
			if leaf {
				leaves = append(leaves, n)
			}
		}
	}
	return leaves, nil
}

// coverage measures every leaf alone, Runs times, and returns its cases
// (blocks unioned over the runs, cost the median runtime), every block of the
// covered packages with its statement count, and each leaf's healthy process
// latency (its slowest run's wall).
func (m *measurer) coverage(ctx context.Context, bin string, leaves []string) ([]redundancy.Case, map[string]int, map[string]time.Duration, error) {
	elapsed := make([][]time.Duration, len(leaves))
	healthy := map[string]time.Duration{}
	dirs := make([][]string, len(leaves))
	for r := 0; r < m.cfg.Runs; r++ {
		for i, leaf := range leaves {
			dir := filepath.Join(m.cfg.Scratch, "cov", strconv.Itoa(r), strconv.Itoa(i))
			if err := os.MkdirAll(dir, 0o755); err != nil {
				return nil, nil, nil, fmt.Errorf("gomeasure: %w", err)
			}
			res, run, err := m.runTests(ctx, bin, RunPattern(leaf), FirstContactTimeout, []string{"-test.gocoverdir", dir})
			if err != nil {
				return nil, nil, nil, err
			}
			if !run.Ran[leaf] {
				return nil, nil, nil, fmt.Errorf("gomeasure: the pattern %s did not run %s:\n%s", RunPattern(leaf), leaf, res.Text())
			}
			if o := run.Outcomes[leaf]; res.Exit != 0 || (o != Pass && o != Skip) {
				return nil, nil, nil, fmt.Errorf("gomeasure: %s failed on measurement run %d with no mutant, a flaky or broken case (exit %d):\n%s", leaf, r+1, res.Exit, res.Text())
			}
			elapsed[i] = append(elapsed[i], run.Elapsed[leaf])
			healthy[leaf] = max(healthy[leaf], res.Wall)
			dirs[i] = append(dirs[i], dir)
		}
		m.log.Infof("redundancy: coverage run %d of %d done", r+1, m.cfg.Runs)
	}
	cases := make([]redundancy.Case, len(leaves))
	statements := map[string]int{}
	for i, leaf := range leaves {
		prof := filepath.Join(m.cfg.Scratch, "prof-"+strconv.Itoa(i)+".txt")
		res, err := m.gocmd(ctx, ToolTimeout, "tool", "covdata", "textfmt", "-i="+strings.Join(dirs[i], ","), "-o="+prof)
		if err != nil {
			return nil, nil, nil, err
		}
		if res.Exit != 0 || res.TimedOut {
			return nil, nil, nil, fmt.Errorf("gomeasure: covdata textfmt for %s failed:\n%s", leaf, res.Text())
		}
		text, err := os.ReadFile(prof)
		if err != nil {
			return nil, nil, nil, fmt.Errorf("gomeasure: read %s's profile: %w", leaf, err)
		}
		all, covered, err := ParseProfile(string(text))
		if err != nil {
			return nil, nil, nil, fmt.Errorf("gomeasure: %s's profile: %w", leaf, err)
		}
		for _, b := range all {
			statements[b.Key()] = b.Statements
		}
		c := redundancy.Case{ID: leaf, Cost: median(elapsed[i])}
		for _, b := range covered {
			c.Blocks = append(c.Blocks, b.Key())
		}
		cases[i] = c
	}
	return cases, statements, healthy, nil
}

func median(ds []time.Duration) time.Duration {
	s := slices.Clone(ds)
	slices.Sort(s)
	return s[len(s)/2]
}

var compileErr = regexp.MustCompile(`(?m)^(\S+\.go):(\d+):(\d+): `)

// schema lays out the mutants of every reached production file of the
// mutated packages and builds them into one test binary, dropping each site
// a compile error names until it builds.
func (m *measurer) schema(ctx context.Context, cases []redundancy.Case, imports map[string]string) ([]*gomut.Schema, string, error) {
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
			return nil, "", err
		}
		next += len(s.Mutants)
		schemas = append(schemas, s)
	}
	if len(schemas) == 0 {
		return nil, "", fmt.Errorf("gomeasure: the cases reach no production file of %s", strings.Join(m.cfg.Mutate, ", "))
	}
	bin := filepath.Join(m.cfg.Scratch, "mutants.test")
	for attempt := 0; ; attempt++ {
		dir := filepath.Join(m.cfg.Scratch, "overlay-"+strconv.Itoa(attempt))
		if err := os.MkdirAll(dir, 0o755); err != nil {
			return nil, "", fmt.Errorf("gomeasure: %w", err)
		}
		replace := map[string]string{}
		for _, s := range schemas {
			r, err := s.Overlay(dir)
			if err != nil {
				return nil, "", err
			}
			for k, v := range r {
				replace[k] = v
			}
		}
		ov := filepath.Join(dir, "overlay.json")
		if err := gomut.WriteOverlay(ov, replace); err != nil {
			return nil, "", err
		}
		args := []string{"test", "-c", "-vet=off", "-o", bin, "-overlay", ov, m.cfg.Package}
		res, err := m.gocmd(ctx, BuildTimeout, args...)
		if err != nil {
			return nil, "", err
		}
		if res.Exit == 0 && !res.TimedOut {
			live := 0
			for _, s := range schemas {
				live += len(s.Live())
			}
			m.log.Infof("redundancy: mutant schema built after %d attempts, %d of %d mutants live", attempt+1, live, next-1)
			return schemas, bin, nil
		}
		dropped := 0
		for _, mm := range compileErr.FindAllStringSubmatch(res.Text(), -1) {
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
					return nil, "", fmt.Errorf("%w\n%s", err, res.Text())
				}
				dropped += len(ids)
			}
		}
		if dropped == 0 {
			return nil, "", fmt.Errorf("gomeasure: the mutant schema does not build, and no error names a mutated file:\n%s", res.Text())
		}
	}
}

// baseline runs every case in the schema binary with no mutant active. They
// must all pass: anything else is the rewrite changing behavior.
func (m *measurer) baseline(ctx context.Context, bin string, tops, leaves []string, healthy map[string]time.Duration) error {
	var bound time.Duration
	for _, l := range leaves {
		bound += healthy[l]
	}
	res, r, err := m.runTests(ctx, bin, gotest.TopPattern(tops), 2*bound+TimeoutSlack, nil)
	if err != nil {
		return err
	}
	for _, leaf := range leaves {
		if r.Outcomes[leaf] != Pass && r.Outcomes[leaf] != Skip {
			return fmt.Errorf("gomeasure: %s does not pass in the mutant schema with no mutant active: the rewrite changed behavior (exit %d):\n%s", leaf, res.Exit, res.Text())
		}
	}
	return nil
}

// mutate runs every live mutant against the cases that reach it.
func (m *measurer) mutate(ctx context.Context, bin string, schemas []*gomut.Schema, imports map[string]string, cases []redundancy.Case, healthy map[string]time.Duration) ([]redundancy.MutantResult, []string, error) {
	dirImport := map[string]string{}
	for imp, dir := range imports {
		dirImport[dir] = imp
	}
	var all []Block
	reach := map[string][]string{}
	for _, c := range cases {
		for _, key := range c.Blocks {
			reach[key] = append(reach[key], c.ID)
		}
	}
	seen := map[string]bool{}
	for _, c := range cases {
		for _, key := range c.Blocks {
			if seen[key] {
				continue
			}
			seen[key] = true
			b, _, err := parseProfileLine(key + " 1 1")
			if err != nil {
				return nil, nil, err
			}
			all = append(all, b)
		}
	}
	idx := newBlockIndex(all)
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
			file := dirImport[filepath.Dir(mu.File)] + "/" + filepath.Base(mu.File)
			held := idx.holding(file, mu.Line, mu.Col)
			if len(held) == 0 {
				results = append(results, redundancy.MutantResult{Key: mu.Key(), Status: "unreached"})
				continue
			}
			covering := reach[held[0].Key()]
			kills, anom, err := m.killers(ctx, bin, mu, covering, healthy)
			if err != nil {
				return nil, nil, err
			}
			anomalies = append(anomalies, anom...)
			r := redundancy.MutantResult{Key: mu.Key(), Status: "survived", KilledBy: kills}
			if len(kills) > 0 {
				r.Status = "killed"
			}
			results = append(results, r)
		}
	}
	return results, anomalies, nil
}

// killers runs one mutant against the leaves that reach it and returns the
// leaves that fail. All of them run in one process first; a leaf that process
// never reported (it panicked or hung partway) runs again alone, and a hang
// is a kill only when it hangs alone too.
func (m *measurer) killers(ctx context.Context, bin string, mu gomut.Mutant, leaves []string, healthy map[string]time.Duration) ([]string, []string, error) {
	env := gomut.EnvMutant + "=" + strconv.Itoa(mu.ID)
	pending := map[string]bool{}
	for _, l := range leaves {
		pending[l] = true
	}
	var killed, hung, anomalies []string
	settle := func(r Run, res Result, asked []string) {
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
				anomalies = append(anomalies, fmt.Sprintf("%s failed under %s, which it does not reach", n, mu.Key()))
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
	res, r, err := m.runTests(ctx, bin, gotest.TopPattern(tops), 2*bound+TimeoutSlack, nil, env)
	if err != nil {
		return nil, nil, err
	}
	settle(r, res, leaves)
	for _, l := range leaves {
		if !pending[l] {
			continue
		}
		timeout := 2*healthy[l] + TimeoutSlack
		res, r, err := m.runTests(ctx, bin, RunPattern(l), timeout, nil, env)
		if err != nil {
			return nil, nil, err
		}
		settle(r, res, []string{l})
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
	confirmed, err := m.confirmHangs(ctx, bin, mu, env, hung)
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
func (m *measurer) confirmHangs(ctx context.Context, bin string, mu gomut.Mutant, env string, hung []string) ([]string, error) {
	if len(hung) == 0 {
		return nil, nil
	}
	lone := func(l string) (bool, error) {
		res, r, err := m.runTests(ctx, bin, RunPattern(l), m.cfg.HangBound, nil, env)
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

func isAncestor(n string, leaves []string) bool {
	for _, l := range leaves {
		if strings.HasPrefix(l, n+"/") {
			return true
		}
	}
	return false
}
