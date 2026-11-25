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
	"errors"
	"flag"
	"fmt"
	"io/fs"
	"maps"
	"os"
	"path/filepath"
	"reflect"
	"slices"
	"sort"
	"strings"
	"time"

	"agentrepl/testrun/internal/goprofile"
	"agentrepl/testrun/internal/redundancy"
	"agentrepl/testrun/roster"
)

// Selection is which suites a coverage run selected: every suite, or the
// named ones. A coverage system whose own suite the selection ran is held
// to every tier that suite measures, and a missing one fails the merge; any
// other system is reported only as far as the selected suites reached it,
// as a partial measurement no removal may be held to.
type Selection struct {
	Every bool
	// Suites are the selected suites, sorted and unique; empty when Every.
	Suites []string
}

// SelectionOf is a run's selection from its suite narrowing, empty meaning
// every suite: the named suites sorted, each once.
func SelectionOf(selected []string) Selection {
	if len(selected) == 0 {
		return Selection{Every: true}
	}
	return Selection{Suites: slices.Compact(slices.Sorted(slices.Values(selected)))}
}

// Args are the selection as `testrun coverage-baseline`'s flags, the ones
// ParseBaselineArgs reads.
func (s Selection) Args() []string {
	if s.Every {
		return []string{"-every-suite"}
	}
	var out []string
	for _, name := range s.Suites {
		out = append(out, "-suite", name)
	}
	return out
}

// Ran answers whether the selection ran suite.
func (s Selection) Ran(suite string) bool {
	return s.Every || slices.Contains(s.Suites, suite)
}

// partial is why a selection's lines are provisional, or "" for every suite.
func (s Selection) partial() string {
	if s.Every {
		return ""
	}
	return "a partial measurement: only the suites " + strings.Join(s.Suites, ", ") + " ran"
}

// goTiers is the Go systems' Tiers text for the selection.
func (s Selection) goTiers() string {
	if s.Every {
		return "go: unit, integration and e2e, spawned systems included"
	}
	return "go: " + strings.Join(s.Suites, ", ") + " only, spawned systems included"
}

// BaselineInput is what one merge needs.
type BaselineInput struct {
	// CoverRoot holds every Go counter directory of the run, at any depth;
	// "" when the run wrote none, which only a selection may (CoverRoot).
	CoverRoot string
	// GoSystems maps each Go coverage system's module path to its suite.
	GoSystems map[string]string
	// Vitest maps a vitest coverage system's name to its istanbul reports,
	// every tier's (istanbulStatements): only the systems the run measured.
	Vitest map[string]VitestReports
	// Selection is the suites the run selected.
	Selection Selection
	Commit    string
	Now       time.Time
	// ProfileOut is where the run's merged Go profile is written, block by
	// block, so it outlives the run's scratch directory beside the baseline
	// (ProfileFile); "" keeps it in CoverRoot, which the run deletes.
	ProfileOut string
}

// ProfileFile is where a coverage run keeps its merged Go profile: beside its
// baseline file, so a later run's profile can be compared with it block by
// block (a test removal is judged by every block it alone covered, which the
// per-function report cannot show once another test gains a block in the same
// function).
func ProfileFile(baselineOut string) string {
	return strings.TrimSuffix(baselineOut, filepath.Ext(baselineOut)) + ".coverprofile"
}

// Baseline merges the run's counters into each system's coverage. A Go
// system whose suite the selection ran must have a counter; one it did not
// run is reported when some selected suite's counters reach it.
func Baseline(goTool Runner, in BaselineInput) (*redundancy.BaselineFile, error) {
	out := &redundancy.BaselineFile{Commit: in.Commit, Measured: in.Now.UTC().Format(time.RFC3339), Systems: map[string]redundancy.SystemBaseline{}}
	if in.CoverRoot == "" && in.Selection.Every {
		return nil, errors.New("cover: a coverage run of every suite has no cover root")
	}
	var dirs []string
	if in.CoverRoot != "" {
		var err error
		if dirs, err = goprofile.CounterDirs(in.CoverRoot); err != nil {
			return nil, err
		}
	}
	expected := false
	for _, suite := range in.GoSystems {
		expected = expected || in.Selection.Ran(suite)
	}
	if in.CoverRoot == "" && expected {
		return nil, fmt.Errorf("cover: the run wrote no cover root, though it ran the Go suites of %s", strings.Join(in.Selection.Suites, ", "))
	}
	if len(dirs) == 0 && expected {
		return nil, fmt.Errorf("cover: %s holds no coverage counters", in.CoverRoot)
	}
	if len(dirs) > 0 {
		profile := filepath.Join(in.CoverRoot, "baseline.coverprofile")
		if in.ProfileOut != "" {
			profile = in.ProfileOut
		}
		if _, err := goTool(in.CoverRoot, "tool", "covdata", "textfmt", "-i="+strings.Join(dirs, ","), "-o="+profile); err != nil {
			return nil, fmt.Errorf("cover: merge the run's counters: %w", err)
		}
		text, err := os.ReadFile(profile)
		if err != nil {
			return nil, fmt.Errorf("cover: read the merged profile: %w", err)
		}
		if out.Systems, err = goSystems(string(text), in.GoSystems, in.Selection.goTiers()); err != nil {
			return nil, err
		}
	}
	for name, r := range in.Vitest {
		c, err := istanbulStatements(r)
		if err != nil {
			return nil, fmt.Errorf("cover: %s: %w", name, err)
		}
		out.Systems[name] = c
	}
	for mod, suite := range in.GoSystems {
		if _, ok := out.Systems[mod]; !ok && in.Selection.Ran(suite) {
			return nil, fmt.Errorf("cover: no counter of the run covers %s (suite %s)", mod, suite)
		}
	}
	if len(out.Systems) == 0 {
		return nil, fmt.Errorf("cover: the suites %s measured no coverage system", strings.Join(in.Selection.Suites, ", "))
	}
	if why := in.Selection.partial(); why != "" {
		for name, b := range out.Systems {
			b.Provisional, b.Reason = true, joinReasons(b.Reason, why)
			out.Systems[name] = b
		}
	}
	return out, nil
}

// joinReasons is a provisional line's reasons, the empty ones dropped.
func joinReasons(reasons ...string) string {
	return strings.Join(slices.DeleteFunc(reasons, func(r string) bool { return r == "" }), "; ")
}

// goSystems sums a merged profile's statements per system: the module path
// that is the longest prefix of each block's file. A block of no system (the
// e2e harness, a fake) is not counted; a block met twice counts once,
// covered if either count is.
func goSystems(profile string, systems map[string]string, tiers string) (map[string]redundancy.SystemBaseline, error) {
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
		out[mod] = baselineOf(hit[mod], n, tiers)
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
	// system: its config's all:true pass over its coverage include. Empty
	// when the run did not run the system's suite: the system is then the
	// files its tiers loaded, a partial measurement.
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
// its statement map must be the first report's that held the file: one that
// is not would count hits against the wrong statements, and is refused. With
// no unit report the system is every file the tiers loaded, each counted
// once.
func istanbulStatements(r VitestReports) (redundancy.SystemBaseline, error) {
	known, from := istanbulReport{}, map[string]string{}
	covered := map[string]map[string]bool{}
	total := 0
	admit := func(path string, rep istanbulReport, file string) {
		f := rep[file]
		known[file], from[file], covered[file] = f, path, map[string]bool{}
		total += len(f.S)
	}
	hits := func(file string, s map[string]int) {
		for id, n := range s {
			if n > 0 {
				covered[file][id] = true
			}
		}
	}
	var tiers []string
	if r.Unit != "" {
		unit, err := readIstanbul(r.Unit)
		if err != nil {
			return redundancy.SystemBaseline{}, err
		}
		for file, f := range unit {
			admit(r.Unit, unit, file)
			hits(file, f.S)
		}
		if total == 0 {
			return redundancy.SystemBaseline{}, fmt.Errorf("%s holds no statements", r.Unit)
		}
		tiers = append(tiers, "unit")
	}
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
				k, ok := known[file]
				switch {
				case !ok && r.Unit != "":
					continue // a file outside the unit config's coverage include
				case !ok:
					admit(path, rep, file)
				case !reflect.DeepEqual(f.StatementMap, k.StatementMap):
					return redundancy.SystemBaseline{}, fmt.Errorf("%s's statements of %s are not %s's: the tiers instrumented it differently", path, file, from[file])
				}
				hits(file, f.S)
			}
		}
	}
	if total == 0 {
		return redundancy.SystemBaseline{}, fmt.Errorf("no tier's report holds a statement (tiers: %s)", strings.Join(tiers, ", "))
	}
	hit := 0
	for _, ids := range covered {
		hit += len(ids)
	}
	text := "vitest: " + strings.Join(tiers, ", ") + ", merged by statement"
	if r.Unit == "" {
		text = "vitest: " + strings.Join(tiers, ", ") + " only, over the files it loaded, merged by statement"
	}
	b := baselineOf(hit, total, text)
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
	// Selection is the suites the run selected.
	Selection Selection
}

// ParseBaselineArgs reads `testrun coverage-baseline -module M -work W
// -commit C -out F` and the run's selection: -every-suite, or one -suite per
// selected roster suite.
func ParseBaselineArgs(argv []string) (BaselineArgs, error) {
	fs := flag.NewFlagSet("testrun coverage-baseline", flag.ContinueOnError)
	var a BaselineArgs
	fs.StringVar(&a.Module, "module", "", "the agent-repl module root")
	fs.StringVar(&a.Work, "work", "", "the run's scratch directory")
	fs.StringVar(&a.Commit, "commit", "", "the commit the run measured")
	fs.StringVar(&a.Out, "out", "", "where to write the baseline")
	var every bool
	var named []string
	fs.BoolVar(&every, "every-suite", false, "the run selected every suite")
	fs.Func("suite", "a suite the run selected (repeatable)", func(name string) error {
		if _, ok := roster.Lookup(name); !ok {
			return fmt.Errorf("%q is no roster suite", name)
		}
		named = append(named, name)
		return nil
	})
	if err := fs.Parse(argv); err != nil {
		return BaselineArgs{}, err
	}
	if a.Module == "" || a.Work == "" || a.Commit == "" || a.Out == "" || fs.NArg() != 0 {
		return BaselineArgs{}, fmt.Errorf("cover: coverage-baseline takes exactly -module, -work, -commit and -out, and the run's selection")
	}
	if every == (len(named) > 0) {
		return BaselineArgs{}, fmt.Errorf("cover: coverage-baseline takes the run's selection as exactly one of -every-suite or one -suite per selected suite")
	}
	a.Selection = SelectionOf(named)
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
// reports (e2e/kit/webapplayer.go's DriverCoverageEnv).
const WebappLayerDir = "webapp-layer"

// unmergedReach names, per vitest system, a reach no coverage run merges yet:
// the system's line stays provisional with it as the reason.
var unmergedReach = map[string]string{
	"shim": "the shim's e2e reach (its bundle, spawned by the e2e suite) is not merged",
}

// vitestReports finds every tier's istanbul reports of one vitest system in
// a finished coverage run's work directory, false when the run measured none
// of the system. A system whose suite the selection ran is held to its unit
// report; one it did not run is measured only by the e2e webapp layer, when
// the run reached it.
func vitestReports(work, coverRoot, name string, sel Selection) (VitestReports, bool, error) {
	r := VitestReports{Tiers: map[string][]string{}}
	var missing []string
	if sel.Ran(name) {
		r.Unit = VitestCoverage(work, name)
		integ, err := filepath.Glob(filepath.Join(VitestIntegrationCoverage(work, name), "*", "coverage-final.json"))
		if err != nil {
			return VitestReports{}, false, fmt.Errorf("cover: find %s's integration reports: %w", name, err)
		}
		sort.Strings(integ)
		r.Tiers["integration"] = integ
		if len(integ) == 0 {
			missing = append(missing, "the run wrote no integration coverage")
		}
	}
	if name == "webapp" {
		// A run with no cover root ("", CoverRoot) measured no layer.
		var layer []string
		if coverRoot != "" {
			var err error
			if layer, err = webappLayerReports(coverRoot); err != nil {
				return VitestReports{}, false, err
			}
		}
		r.Tiers["e2e webapp layer"] = layer
		if len(layer) == 0 && sel.Ran(name) {
			missing = append(missing, "the run measured no e2e webapp layer (run the e2e suite in the same coverage run)")
		}
	}
	measured := r.Unit != ""
	for _, paths := range r.Tiers {
		measured = measured || len(paths) > 0
	}
	if !measured {
		return VitestReports{}, false, nil
	}
	if why, ok := unmergedReach[name]; ok {
		missing = append(missing, why)
	}
	r.Missing = joinReasons(missing...)
	return r, true, nil
}

// CoverRoot is where a finished coverage run left its Go counters, the e2e
// webapp layer's reports and the shim's v8 profiles: the work directory's
// cover/, or "" when the run wrote none. A selection that ran no Go suite and
// no e2e suite (a webapp-only run) never creates it, and then has no Go or e2e
// reach to report; its lines stay provisional (Selection.partial). A run of
// every suite always writes it, so a missing one fails that run.
func CoverRoot(a BaselineArgs) (string, error) {
	root := filepath.Join(a.Work, "cover")
	_, err := os.Stat(root)
	switch {
	case err == nil:
		return root, nil
	case !errors.Is(err, fs.ErrNotExist):
		return "", fmt.Errorf("cover: read the run's cover root: %w", err)
	case a.Selection.Every:
		return "", fmt.Errorf("cover: a coverage run of every suite wrote no cover root at %s: its Go counters and e2e reports are missing", root)
	}
	return "", nil
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

// SystemLines are a merged baseline's systems as the run's log reports them,
// one line each in name order, a provisional line ending with its reason.
func SystemLines(f *redundancy.BaselineFile) []string {
	names := slices.Sorted(maps.Keys(f.Systems))
	lines := make([]string, 0, len(names))
	for _, name := range names {
		s := f.Systems[name]
		provisional := ""
		if s.Provisional {
			provisional = "; provisional: " + s.Reason
		}
		lines = append(lines, fmt.Sprintf("%s %.2f%% of %d statements (floor %.2f%%; %s%s)", name, s.Percent, s.Statements, s.Floor, s.Tiers, provisional))
	}
	return lines
}

// RunBaseline merges a finished coverage run into a baseline file at a.Out.
func RunBaseline(goTool Runner, a BaselineArgs, goSystems map[string]string, vitest []string, now time.Time) (*redundancy.BaselineFile, error) {
	root, err := CoverRoot(a)
	if err != nil {
		return nil, err
	}
	in := BaselineInput{CoverRoot: root, GoSystems: goSystems, Vitest: map[string]VitestReports{}, Selection: a.Selection, Commit: a.Commit, Now: now, ProfileOut: ProfileFile(a.Out)}
	for _, name := range vitest {
		r, ok, err := vitestReports(a.Work, in.CoverRoot, name, a.Selection)
		if err != nil {
			return nil, err
		}
		if ok {
			in.Vitest[name] = r
		}
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
