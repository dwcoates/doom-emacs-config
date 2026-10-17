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
	"os"
	"path/filepath"
	"sort"
	"reflect"
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
	// Vitest maps a vitest coverage system's name to its istanbul reports,
	// every tier's (istanbulStatements).
	Vitest map[string]VitestReports
	Commit string
	Now    time.Time
}

// Baseline merges the run's counters into each system's coverage.
func Baseline(goTool Runner, in BaselineInput) (*redundancy.BaselineFile, error) {
	dirs, err := goprofile.CounterDirs(in.CoverRoot)
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
	for name, r := range in.Vitest {
		c, err := istanbulStatements(r)
		if err != nil {
			return nil, fmt.Errorf("cover: %s: %w", name, err)
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

// VitestReports are one vitest system's istanbul reports.
type VitestReports struct {
	// Unit is the unit pass's merged report. Its files and statements are the
	// system: its config's all:true pass over its coverage include.
	Unit string
	// Tiers are every other tier's reports, by the tier's name ("integration",
	// "e2e webapp layer"); their hits count only in Unit's statements.
	Tiers map[string][]string
	// Missing, when set, names a tier the run should have measured and did
	// not: the system's line is then provisional, with this as the reason.
	Missing string
}

// istanbulReport is the part of a coverage-final.json the merge reads, per
// file: each statement's hit count and where it is.
type istanbulReport map[string]struct {
	S            map[string]int `json:"s"`
	StatementMap map[string]struct {
		Start, End struct{ Line, Column int }
	} `json:"statementMap"`
}

func readIstanbul(path string) (istanbulReport, error) {
	b, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("read %s: %w", path, err)
	}
	var r istanbulReport
	if err := json.Unmarshal(b, &r); err != nil {
		return nil, fmt.Errorf("decode %s: %w", path, err)
	}
	return r, nil
}

// istanbulStatements merges a vitest system's reports into its statement
// coverage: a statement of the unit report is covered when any tier's report
// counts it. Every tier instruments the same file with the same istanbul, so
// its statement map must be the unit report's: one that is not would count
// hits against the wrong statements, and is refused.
func istanbulStatements(r VitestReports) (redundancy.SystemBaseline, error) {
	unit, err := readIstanbul(r.Unit)
	if err != nil {
		return redundancy.SystemBaseline{}, err
	}
	covered := map[string]map[string]bool{}
	total := 0
	for file, f := range unit {
		covered[file] = map[string]bool{}
		for id, n := range f.S {
			total++
			if n > 0 {
				covered[file][id] = true
			}
		}
	}
	if total == 0 {
		return redundancy.SystemBaseline{}, fmt.Errorf("%s holds no statements", r.Unit)
	}
	tiers := []string{"unit"}
	names := make([]string, 0, len(r.Tiers))
	for name := range r.Tiers {
		names = append(names, name)
	}
	sort.Strings(names)
	for _, name := range names {
		if len(r.Tiers[name]) == 0 {
			continue
		}
		tiers = append(tiers, name)
		for _, path := range r.Tiers[name] {
			rep, err := readIstanbul(path)
			if err != nil {
				return redundancy.SystemBaseline{}, err
			}
			for file, f := range rep {
				u, ok := unit[file]
				if !ok {
					continue // a file outside the unit config's coverage include
				}
				if !reflect.DeepEqual(f.StatementMap, u.StatementMap) {
					return redundancy.SystemBaseline{}, fmt.Errorf("%s's statements of %s are not %s's: the tiers instrumented it differently", path, file, r.Unit)
				}
				for id, n := range f.S {
					if n > 0 {
						covered[file][id] = true
					}
				}
			}
		}
	}
	hit := 0
	for _, ids := range covered {
		hit += len(ids)
	}
	b := baselineOf(hit, total, "vitest: "+strings.Join(tiers, ", ")+", merged by statement")
	if r.Missing != "" {
		b.Provisional, b.Reason = true, r.Missing
	}
	return b, nil
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

// VitestIntegrationCoverage is the directory under which each of a vitest
// suite's integration chunks writes its istanbul report in a coverage run,
// one subdirectory per chunk (suites.vitestIntegrationSplit).
func VitestIntegrationCoverage(work, suite string) string {
	return filepath.Join(work, "vitest", suite+"-integration", "coverage")
}

// WebappLayerDir is the directory, anywhere under a coverage run's cover
// root, whose driver-* subdirectories hold the e2e webapp layer's istanbul
// reports (e2e/coverage_test.go's webappLayerCoverageEnv).
const WebappLayerDir = "webapp-layer"

// unmergedReach names, per vitest system, a reach no coverage run merges yet:
// the system's line stays provisional with it as the reason.
var unmergedReach = map[string]string{
	"shim": "the shim's e2e reach (its bundle, spawned by the e2e suite) is not merged",
}

// vitestReports finds every tier's istanbul reports of one vitest system in
// a finished coverage run's work directory.
func vitestReports(work, coverRoot, name string) (VitestReports, error) {
	r := VitestReports{Unit: VitestCoverage(work, name), Tiers: map[string][]string{}}
	integ, err := filepath.Glob(filepath.Join(VitestIntegrationCoverage(work, name), "*", "coverage-final.json"))
	if err != nil {
		return VitestReports{}, fmt.Errorf("cover: find %s's integration reports: %w", name, err)
	}
	sort.Strings(integ)
	r.Tiers["integration"] = integ
	var missing []string
	if len(integ) == 0 {
		missing = append(missing, "the run wrote no integration coverage")
	}
	if name == "webapp" {
		layer, err := webappLayerReports(coverRoot)
		if err != nil {
			return VitestReports{}, err
		}
		r.Tiers["e2e webapp layer"] = layer
		if len(layer) == 0 {
			missing = append(missing, "the run measured no e2e webapp layer (run the e2e suite in the same coverage run)")
		}
	}
	if why, ok := unmergedReach[name]; ok {
		missing = append(missing, why)
	}
	r.Missing = strings.Join(missing, "; ")
	return r, nil
}

// webappLayerReports is every run-*.json under every WebappLayerDir/driver-*
// directory beneath root.
func webappLayerReports(root string) ([]string, error) {
	var out []string
	err := filepath.WalkDir(root, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || filepath.Base(filepath.Dir(filepath.Dir(path))) != WebappLayerDir {
			return nil
		}
		if m, _ := filepath.Match("run-*.json", d.Name()); m && strings.HasPrefix(filepath.Base(filepath.Dir(path)), "driver-") {
			out = append(out, path)
		}
		return nil
	})
	if err != nil {
		return nil, fmt.Errorf("cover: find the webapp layer's reports under %s: %w", root, err)
	}
	sort.Strings(out)
	return out, nil
}

// RunBaseline merges a finished coverage run into a baseline file at a.Out.
func RunBaseline(goTool Runner, a BaselineArgs, goSystems map[string]string, vitest []string, now time.Time) (*redundancy.BaselineFile, error) {
	in := BaselineInput{CoverRoot: filepath.Join(a.Work, "cover"), GoSystems: goSystems, Vitest: map[string]VitestReports{}, Commit: a.Commit, Now: now}
	for _, name := range vitest {
		r, err := vitestReports(a.Work, in.CoverRoot, name)
		if err != nil {
			return nil, err
		}
		in.Vitest[name] = r
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
