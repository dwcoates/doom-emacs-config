package vitestmeasure

import (
	"bufio"
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"sort"
	"strconv"
	"strings"
	"time"

	"agentrepl/testrun/internal/redundancy"
	"agentrepl/testrun/internal/redundancy/proc"
	"agentrepl/testrun/internal/run"
)

// Bounds, each naming what it bounds and what it was sized on (the
// scheduler's history, 2026-10-14: webapp's and shim's integration configs
// each ran in one chunk of ~140s; Stryker's runs are not yet measured).
const (
	// TraceTimeout bounds one traced vitest run of a whole config: just
	// over twice the slowest config's measured 140s, with istanbul's
	// instrumentation and one worker on top.
	TraceTimeout = 10 * time.Minute
	// StrykerTimeout bounds one Stryker run. It is unmeasured; a run past
	// it is reported, and the bound is resized from what the first real run
	// measures.
	StrykerTimeout = 6 * time.Hour
	// GitTimeout bounds the clean-tree check: twice its measured 10ms in
	// either package, plus the 500ms a process start can take under load.
	GitTimeout = 520 * time.Millisecond
)

// TraceEnv names the file redundancy-trace.mjs appends to; SkipEnv the
// list redundancy-skip.mjs reads.
const (
	TraceEnv = "AGENT_REPL_REDUNDANCY_TRACE"
	SkipEnv  = "AGENT_REPL_REDUNDANCY_SKIP"
)

// Config is one vitest measurement.
type Config struct {
	// Package is the npm package directory (webapp, the shim).
	Package string
	// System is its coverage system's name (testrun/coverage-baseline.json).
	System string
	// ConfigFile is its vitest config, relative to Package.
	ConfigFile string
	// Mutate are the source globs Stryker mutates, relative to Package.
	Mutate []string
	// Runs is how many traced runs each test's statements are unioned over.
	Runs int
	// Delete, when set, is a deletion set to verify instead of minimizing.
	Delete []string
	// Pins are tests kept whatever they cover.
	Pins []string
	// Trace is testrun/vitest/redundancy-trace.mjs; Skip its
	// redundancy-skip.mjs beside it.
	Trace   string
	Skip    string
	Scratch string
	Gate    proc.Gate
}

// tracedTest is one line redundancy-trace.mjs wrote.
type tracedTest struct {
	File  string   `json:"file"`
	Name  string   `json:"name"`
	State string   `json:"state"`
	MS    float64  `json:"ms"`
	Hit   []string `json:"hit"`
	Read  []string `json:"read"`
}

// skipped is one test Stryker's runs skip (redundancy-skip.mjs).
type skipped struct {
	File string `json:"file"`
	Name string `json:"name"`
}

type measurer struct {
	cfg     Config
	counter *proc.Counter
	log     *run.Log
	cost    redundancy.Cost
	// skips are the tests that read a source Stryker mutates.
	skips []skipped
}

// run runs one process, counted, ending at an interrupt (proc.Counter).
func (m *measurer) run(ctx context.Context, c proc.Cmd) (proc.Result, error) {
	return m.counter.Run(ctx, c)
}

// Measure traces every test's statements, has Stryker kill-matrix every
// mutant of Mutate, and answers the measurement: a verification when Delete
// is set.
func Measure(ctx context.Context, cfg Config, exec proc.Exec, log *run.Log) (*redundancy.Measurement, error) {
	if cfg.Runs < 1 {
		return nil, fmt.Errorf("vitestmeasure: runs must be at least 1, got %d", cfg.Runs)
	}
	if len(cfg.Mutate) == 0 {
		return nil, fmt.Errorf("vitestmeasure: nothing to mutate")
	}
	m := &measurer{cfg: cfg, counter: &proc.Counter{Exec: exec, Name: "vitestmeasure"}, log: log}
	cfg.Gate.Run = m.run
	m.cfg.Gate = cfg.Gate
	if err := cfg.Gate.Check(); err != nil {
		return nil, fmt.Errorf("vitestmeasure: %w", err)
	}
	start := time.Now()
	phaseStart := start
	phase := func(name string) {
		m.cost.Phases = append(m.cost.Phases, redundancy.Phase{Name: name, Wall: time.Since(phaseStart)})
		phaseStart = time.Now()
	}
	out := &redundancy.Measurement{
		Adapter: "vitest",
		Subject: cfg.System + ": " + cfg.ConfigFile,
		Scope:   cfg.Mutate,
		Modules: []string{cfg.System},
	}
	cases, statements, err := m.trace(ctx)
	if err != nil {
		return nil, err
	}
	phase("traced runs")
	out.Statements = statements
	matrix, err := m.stryker(ctx)
	if err != nil {
		return nil, err
	}
	phase("Stryker")
	byID := map[string]int{}
	for i, c := range cases {
		byID[c.ID] = i
	}
	var missing []string
	for _, id := range matrix.Cases {
		if _, ok := byID[id]; !ok {
			missing = append(missing, id)
		}
	}
	if len(missing) > 0 {
		sort.Strings(missing)
		return nil, fmt.Errorf("vitestmeasure: Stryker names tests the traced runs never ran (the two disagree on names): %s", strings.Join(missing, "; "))
	}
	for _, r := range matrix.Mutants {
		for _, id := range r.KilledBy {
			cases[byID[id]].Kills = append(cases[byID[id]].Kills, r.Key)
		}
	}
	out.Mutants = matrix.Mutants
	out.Cases = cases
	if len(cfg.Delete) > 0 {
		v, err := redundancy.VerifyMatrix(out, cfg.Delete, " > ")
		if err != nil {
			return nil, err
		}
		out.Verification = v
		phase("deletion-set verification")
	}
	m.cost.Wall = time.Since(start)
	m.cost.Processes, m.cost.CPU = m.counter.Processes, m.counter.CPU
	out.Cost = m.cost
	return out, nil
}

// traceConfig is the vitest config a traced run uses: the package's own,
// with the trace setup file added, istanbul coverage on, isolated, and one
// worker, so no two tests' counters interleave.
func (m *measurer) traceConfig(dir string) (string, error) {
	return m.wrapConfig(dir, "trace.config.mjs", fmt.Sprintf(`{
    setupFiles: [%q],
    isolate: true,
    maxWorkers: 1,
    minWorkers: 1,
    coverage: { enabled: true, provider: "istanbul", reporter: ["json"], reportsDirectory: %q },
  }`, m.cfg.Trace, filepath.Join(dir, "coverage")))
}

// wrapConfig writes dir/name, a vitest config that is the package's own with
// test merged over it (vite's mergeConfig: arrays such as setupFiles append),
// rooted at the package, and answers its path.
func (m *measurer) wrapConfig(dir, name, test string) (string, error) {
	pkg := m.cfg.Package
	vitestConfig := filepath.Join(pkg, "node_modules", "vitest", "dist", "config.js")
	if _, err := os.Stat(vitestConfig); err != nil {
		return "", fmt.Errorf("vitestmeasure: %s has no vitest installed (run its npm ci): %w", pkg, err)
	}
	src := fmt.Sprintf(`// Generated by testrun redundancy vitest; it lives in a scratch directory.
import { mergeConfig } from %q;
import base from %q;
export default mergeConfig(base, {
  root: %q,
  test: %s,
});
`, vitestConfig, filepath.Join(pkg, m.cfg.ConfigFile), pkg, test)
	path := filepath.Join(dir, name)
	if err := os.WriteFile(path, []byte(src), 0o644); err != nil {
		return "", fmt.Errorf("vitestmeasure: write %s: %w", path, err)
	}
	return path, nil
}

// trace runs the config Runs times and answers each test's case: its
// statements unioned over the runs (keyed "<system>/<source>:<statement>"),
// its median runtime.
func (m *measurer) trace(ctx context.Context) ([]redundancy.Case, map[string]int, error) {
	type acc struct {
		hit map[string]bool
		ms  []float64
		// reads is the first source Stryker mutates the test read.
		reads string
		test  skipped
	}
	mutated, err := newMutateMatcher(m.cfg.Mutate)
	if err != nil {
		return nil, nil, err
	}
	byID := map[string]*acc{}
	var order []string
	statements := map[string]int{}
	for r := 0; r < m.cfg.Runs; r++ {
		if err := m.cfg.Gate.AwaitQuiet(ctx, fmt.Sprintf("traced run %d", r+1)); err != nil {
			return nil, nil, fmt.Errorf("vitestmeasure: %w", err)
		}
		dir := filepath.Join(m.cfg.Scratch, "trace-"+strconv.Itoa(r))
		if err := os.MkdirAll(dir, 0o755); err != nil {
			return nil, nil, fmt.Errorf("vitestmeasure: %w", err)
		}
		cfgPath, err := m.traceConfig(dir)
		if err != nil {
			return nil, nil, err
		}
		tracePath := filepath.Join(dir, "trace.jsonl")
		res, err := m.run(ctx, proc.Cmd{Dir: m.cfg.Package, Path: "npx", Args: []string{"vitest", "run", "--config", cfgPath}, Env: []string{TraceEnv + "=" + tracePath}, Timeout: TraceTimeout})
		if err != nil {
			return nil, nil, err
		}
		if res.Exit != 0 || res.TimedOut {
			return nil, nil, fmt.Errorf("vitestmeasure: the traced run %d of %s failed with no mutant, a broken or flaky suite (exit %d, timed out %t):\n%s", r+1, m.cfg.ConfigFile, res.Exit, res.TimedOut, res.Text())
		}
		tests, err := readTrace(tracePath)
		if err != nil {
			return nil, nil, err
		}
		for _, t := range tests {
			rel, err := filepath.Rel(m.cfg.Package, t.File)
			if err != nil {
				return nil, nil, fmt.Errorf("vitestmeasure: test file %s is not under %s: %w", t.File, m.cfg.Package, err)
			}
			id := rel + ":" + t.Name
			if t.State == "fail" {
				return nil, nil, fmt.Errorf("vitestmeasure: %s failed on traced run %d with no mutant", id, r+1)
			}
			a := byID[id]
			if a == nil {
				a = &acc{hit: map[string]bool{}}
				byID[id] = a
				order = append(order, id)
			}
			a.ms = append(a.ms, t.MS)
			reads := slices.Clone(t.Read)
			sort.Strings(reads)
			for _, r := range reads {
				rel, err := filepath.Rel(m.cfg.Package, r)
				if a.reads == "" && err == nil && !strings.HasPrefix(rel, "..") && mutated.match(rel) {
					a.reads, a.test = rel, skipped{File: t.File, Name: t.Name}
				}
			}
			for _, h := range t.Hit {
				src, sid, ok := strings.Cut(h, ":")
				if !ok {
					return nil, nil, fmt.Errorf("vitestmeasure: the trace holds a malformed statement %q", h)
				}
				srel, err := filepath.Rel(m.cfg.Package, src)
				if err != nil {
					return nil, nil, fmt.Errorf("vitestmeasure: statement file %s is not under %s: %w", src, m.cfg.Package, err)
				}
				key := m.cfg.System + "/" + srel + ":" + sid
				a.hit[key] = true
				statements[key] = 1
			}
		}
		m.log.Infof("redundancy: traced run %d of %d: %d tests", r+1, m.cfg.Runs, len(tests))
	}
	cases := make([]redundancy.Case, 0, len(order))
	for _, id := range order {
		a := byID[id]
		c := redundancy.Case{ID: id, Cost: redundancy.Median(durationsMS(a.ms))}
		for k := range a.hit {
			c.Blocks = append(c.Blocks, k)
		}
		sort.Strings(c.Blocks)
		for _, p := range m.cfg.Pins {
			if id == p || strings.HasPrefix(id, p+" > ") {
				c.Pinned = "named with -pin"
			}
		}
		if a.reads != "" {
			m.skips = append(m.skips, a.test)
			if c.Pinned == "" {
				c.Pinned = "reads " + a.reads + ", a source Stryker mutates in place: its kills are of text, not behavior"
			}
		}
		cases = append(cases, c)
	}
	if len(cases) == 0 {
		return nil, nil, fmt.Errorf("vitestmeasure: the traced runs of %s recorded no test", m.cfg.ConfigFile)
	}
	return cases, statements, nil
}

func readTrace(path string) ([]tracedTest, error) {
	b, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("vitestmeasure: read the trace %s: %w", path, err)
	}
	var out []tracedTest
	sc := bufio.NewScanner(bytes.NewReader(b))
	sc.Buffer(make([]byte, 1024*1024), 64*1024*1024)
	for n := 1; sc.Scan(); n++ {
		var t tracedTest
		if err := json.Unmarshal(sc.Bytes(), &t); err != nil {
			return nil, fmt.Errorf("vitestmeasure: trace line %d: %w", n, err)
		}
		out = append(out, t)
	}
	if err := sc.Err(); err != nil {
		return nil, fmt.Errorf("vitestmeasure: read the trace %s: %w", path, err)
	}
	return out, nil
}

func durationsMS(ms []float64) []time.Duration {
	out := make([]time.Duration, len(ms))
	for i, v := range ms {
		out[i] = time.Duration(v * float64(time.Millisecond))
	}
	return out
}

// strykerConfig is the Stryker config the measurement runs: perTest
// coverage and bail disabled, so every killer of every mutant is named.
func (m *measurer) strykerConfig(report, vitestConfig string) (string, error) {
	c := map[string]any{
		"testRunner":       "vitest",
		"plugins":          []string{"@stryker-mutator/vitest-runner"},
		"vitest":           map[string]any{"configFile": vitestConfig},
		"coverageAnalysis": "perTest",
		"disableBail":      true,
		"mutate":           m.cfg.Mutate,
		"reporters":        []string{"json"},
		"jsonReporter":     map[string]any{"fileName": report},
		"concurrency":      1,
		// IN PLACE: a sandbox copy breaks every import out of the package
		// (vitest.config.ts's ../../../bin/require-background.mjs), so Stryker
		// mutates the package's own sources and restores them; the clean-tree
		// check on both sides of the run makes a leftover mutant loud.
		"inPlace":      true,
		"cleanTempDir": true,
		"tempDirName":  ".stryker-tmp",
		"thresholds":   map[string]any{"break": nil},
		"ignoreStatic": false,
	}
	b, err := json.MarshalIndent(c, "", "  ")
	if err != nil {
		return "", fmt.Errorf("vitestmeasure: encode the Stryker config: %w", err)
	}
	path := filepath.Join(m.cfg.Scratch, "stryker.config.json")
	if err := os.WriteFile(path, b, 0o644); err != nil {
		return "", fmt.Errorf("vitestmeasure: write %s: %w", path, err)
	}
	return path, nil
}

// cleanTree refuses a package whose tracked files differ from HEAD, when
// Stryker mutates it in place: before the run, the mutants would be laid
// over an edit (or a killed run's leftover mutant); after it, Stryker left a
// mutant in the sources.
func (m *measurer) cleanTree(ctx context.Context, when string) error {
	res, err := m.run(ctx, proc.Cmd{Dir: m.cfg.Package, Path: "git", Args: []string{"status", "--porcelain", "--untracked-files=no", "--", "."}, Timeout: GitTimeout})
	if err != nil {
		return err
	}
	if res.Exit != 0 || res.TimedOut {
		return fmt.Errorf("vitestmeasure: git status in %s failed (exit %d, timed out %t):\n%s", m.cfg.Package, res.Exit, res.TimedOut, res.Text())
	}
	if dirty := strings.TrimSpace(res.Text()); dirty != "" {
		return fmt.Errorf("vitestmeasure: %s has tracked changes %s Stryker mutates it in place; run in a worktree nothing else edits:\n%s", m.cfg.Package, when, dirty)
	}
	return nil
}

// stryker runs Stryker and reads its report.
func (m *measurer) stryker(ctx context.Context) (*Matrix, error) {
	if err := m.cfg.Gate.AwaitQuiet(ctx, "Stryker"); err != nil {
		return nil, fmt.Errorf("vitestmeasure: %w", err)
	}
	report := filepath.Join(m.cfg.Scratch, "mutation.json")
	skipList := filepath.Join(m.cfg.Scratch, "skip.json")
	b, err := json.Marshal(append([]skipped{}, m.skips...))
	if err != nil {
		return nil, fmt.Errorf("vitestmeasure: encode the skip list: %w", err)
	}
	if err := os.WriteFile(skipList, b, 0o644); err != nil {
		return nil, fmt.Errorf("vitestmeasure: write %s: %w", skipList, err)
	}
	if len(m.skips) > 0 {
		m.log.Infof("redundancy: %d tests read a source Stryker mutates; Stryker skips them and they are pinned", len(m.skips))
	}
	vitestConfig, err := m.wrapConfig(m.cfg.Scratch, "stryker.vitest.config.mjs", fmt.Sprintf(`{ setupFiles: [%q] }`, m.cfg.Skip))
	if err != nil {
		return nil, err
	}
	cfgPath, err := m.strykerConfig(report, vitestConfig)
	if err != nil {
		return nil, err
	}
	if err := m.cleanTree(ctx, "before"); err != nil {
		return nil, err
	}
	res, err := m.run(ctx, proc.Cmd{Dir: m.cfg.Package, Path: "npx", Args: []string{"stryker", "run", cfgPath}, Env: []string{SkipEnv + "=" + skipList}, Timeout: StrykerTimeout})
	if err != nil {
		return nil, err
	}
	if err := m.cleanTree(ctx, "after"); err != nil {
		return nil, err
	}
	if res.Exit != 0 || res.TimedOut {
		return nil, fmt.Errorf("vitestmeasure: Stryker failed (exit %d, timed out %t):\n%s", res.Exit, res.TimedOut, res.Text())
	}
	b, err = os.ReadFile(report)
	if err != nil {
		return nil, fmt.Errorf("vitestmeasure: read Stryker's report %s: %w", report, err)
	}
	mx, err := ParseStryker(b)
	if err != nil {
		return nil, err
	}
	// Stryker names a test file relative to the package; the trace's IDs
	// are too, so the two meet as they are.
	return mx, nil
}
