package cover

// `testrun coverage-baseline`: the last unit of a `testrun run --coverage`,
// which merges every counter the run wrote, every tier together, into each
// coverage system's statement coverage. Its output is the shape of
// testrun/coverage-baseline.json, the one reference every workspace measures
// a test removal against. The run writes it to its own work directory and
// prints where; it becomes the baseline only when the owner copies it over
// (owner ruling, 2026-10-14: the floor changes only by an explicit decision).

import (
	"encoding/json"
	"flag"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"agentrepl/testrun/internal/goprofile"
	"agentrepl/testrun/internal/redundancy"
)

// BaselineInput is what one merge needs.
type BaselineInput struct {
	// CoverRoot holds every Go counter directory of the run, at any depth.
	CoverRoot string
	// GoSystems maps each Go coverage system's module path to its suite.
	GoSystems map[string]string
	// Vitest maps a vitest coverage system's name to its merged istanbul
	// coverage-final.json.
	Vitest map[string]string
	Commit string
	Now    time.Time
}

// Baseline merges the run's counters into each system's coverage.
func Baseline(goTool Runner, in BaselineInput) (*redundancy.BaselineFile, error) {
	dirs, err := counterDirs(in.CoverRoot)
	if err != nil {
		return nil, err
	}
	if len(dirs) == 0 {
		return nil, fmt.Errorf("cover: %s holds no coverage counters", in.CoverRoot)
	}
	profile := filepath.Join(in.CoverRoot, "baseline.coverprofile")
	if _, err := goTool(in.CoverRoot, "tool", "covdata", "textfmt", "-i="+strings.Join(dirs, ","), "-o="+profile); err != nil {
		return nil, fmt.Errorf("cover: merge the run's counters: %w", err)
	}
	text, err := os.ReadFile(profile)
	if err != nil {
		return nil, fmt.Errorf("cover: read the merged profile: %w", err)
	}
	out := &redundancy.BaselineFile{Commit: in.Commit, Measured: in.Now.UTC().Format(time.RFC3339)}
	if out.Systems, err = goSystems(string(text), in.GoSystems); err != nil {
		return nil, err
	}
	for name, path := range in.Vitest {
		c, err := istanbulStatements(path)
		if err != nil {
			return nil, err
		}
		out.Systems[name] = c
	}
	for mod, suite := range in.GoSystems {
		if _, ok := out.Systems[mod]; !ok {
			return nil, fmt.Errorf("cover: no counter of the run covers %s (suite %s)", mod, suite)
		}
	}
	return out, nil
}

// counterDirs is every directory under root holding a Go coverage meta file.
func counterDirs(root string) ([]string, error) {
	seen := map[string]bool{}
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if !d.IsDir() && strings.HasPrefix(d.Name(), "covmeta.") {
			seen[filepath.Dir(path)] = true
		}
		return nil
	})
	if err != nil {
		return nil, fmt.Errorf("cover: walk %s: %w", root, err)
	}
	var dirs []string
	for d := range seen {
		dirs = append(dirs, d)
	}
	sort.Strings(dirs)
	return dirs, nil
}

// goSystems sums a merged profile's statements per system: the module path
// that is the longest prefix of each block's file. A block of no system (the
// e2e harness, a fake) is not counted; a block met twice counts once,
// covered if either count is.
func goSystems(profile string, systems map[string]string) (map[string]redundancy.SystemBaseline, error) {
	all, covered, err := goprofile.ParseProfile(profile)
	if err != nil {
		return nil, fmt.Errorf("cover: the merged profile: %w", err)
	}
	stmts, ran := map[string]int{}, map[string]bool{}
	for _, b := range all {
		stmts[b.Key()] = b.Statements
	}
	for _, b := range covered {
		ran[b.Key()] = true
	}
	modules := make([]string, 0, len(systems))
	for m := range systems {
		modules = append(modules, m)
	}
	total, hit := map[string]int{}, map[string]int{}
	for key, n := range stmts {
		mod, ok := redundancy.SystemFor(key, modules)
		if !ok {
			continue // the e2e harness's own code, or a fake's
		}
		total[mod] += n
		if ran[key] {
			hit[mod] += n
		}
	}
	out := map[string]redundancy.SystemBaseline{}
	for mod, n := range total {
		out[mod] = baselineOf(hit[mod], n, "go: unit, integration and e2e, spawned systems included")
	}
	return out, nil
}

// baselineOf is a system's baseline line from its statement counts.
func baselineOf(hit, total int, tiers string) redundancy.SystemBaseline {
	pct := 100 * float64(hit) / float64(total)
	return redundancy.SystemBaseline{Percent: pct, Statements: total, Floor: max(0, pct-redundancy.TotalBudget), Tiers: tiers}
}

// istanbulStatements reads a merged istanbul coverage-final.json's
// statements.
func istanbulStatements(path string) (redundancy.SystemBaseline, error) {
	b, err := os.ReadFile(path)
	if err != nil {
		return redundancy.SystemBaseline{}, fmt.Errorf("cover: read %s: %w", path, err)
	}
	var files map[string]struct {
		S map[string]int `json:"s"`
	}
	if err := json.Unmarshal(b, &files); err != nil {
		return redundancy.SystemBaseline{}, fmt.Errorf("cover: decode %s: %w", path, err)
	}
	total, covered := 0, 0
	for _, f := range files {
		for _, n := range f.S {
			total++
			if n > 0 {
				covered++
			}
		}
	}
	if total == 0 {
		return redundancy.SystemBaseline{}, fmt.Errorf("cover: %s holds no statements", path)
	}
	return baselineOf(covered, total, "vitest: the files its --coverage run measures"), nil
}

// BaselineArgs are `testrun coverage-baseline`'s arguments.
type BaselineArgs struct {
	// Module is the agent-repl module root; Work the run's scratch directory.
	Module, Work string
	Commit       string
	// Out is where the merged baseline is written.
	Out string
}

// ParseBaselineArgs reads `testrun coverage-baseline -module M -work W
// -commit C -out F`.
func ParseBaselineArgs(argv []string) (BaselineArgs, error) {
	fs := flag.NewFlagSet("testrun coverage-baseline", flag.ContinueOnError)
	var a BaselineArgs
	fs.StringVar(&a.Module, "module", "", "the agent-repl module root")
	fs.StringVar(&a.Work, "work", "", "the run's scratch directory")
	fs.StringVar(&a.Commit, "commit", "", "the commit the run measured")
	fs.StringVar(&a.Out, "out", "", "where to write the baseline")
	if err := fs.Parse(argv); err != nil {
		return BaselineArgs{}, err
	}
	if a.Module == "" || a.Work == "" || a.Commit == "" || a.Out == "" || fs.NArg() != 0 {
		return BaselineArgs{}, fmt.Errorf("cover: coverage-baseline takes exactly -module, -work, -commit and -out")
	}
	return a, nil
}

// VitestCoverage is where a coverage run's merge unit writes a vitest
// suite's istanbul JSON (suites.vitestUnitsForFiles).
func VitestCoverage(work, suite string) string {
	return filepath.Join(work, "vitest", suite, "coverage", "coverage-final.json")
}

// RunBaseline merges a finished coverage run into a baseline file at a.Out.
func RunBaseline(goTool Runner, a BaselineArgs, goSystems map[string]string, vitest []string, now time.Time) (*redundancy.BaselineFile, error) {
	in := BaselineInput{CoverRoot: filepath.Join(a.Work, "cover"), GoSystems: goSystems, Vitest: map[string]string{}, Commit: a.Commit, Now: now}
	for _, name := range vitest {
		in.Vitest[name] = VitestCoverage(a.Work, name)
	}
	f, err := Baseline(goTool, in)
	if err != nil {
		return nil, err
	}
	if err := redundancy.WriteBaseline(a.Out, f); err != nil {
		return nil, err
	}
	return f, nil
}
