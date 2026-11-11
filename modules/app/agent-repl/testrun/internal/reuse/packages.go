package reuse

import (
	"fmt"
	"maps"
	"slices"
	"sort"
	"strings"
	"sync"

	"agentrepl/testrun/internal/gobuild"
	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/suites"
	"agentrepl/testrun/roster"
)

// PER-PACKAGE REUSE (roster.Suite.PackageReuse). A keyed Go suite that runs
// still skips each unit-test package whose own inputs are byte-identical to
// inputs an earlier run passed that package on, so a change to one package
// reruns it and the packages that import it, not all of them.
//
// A PACKAGE'S INPUTS COME FROM THE GO TOOL'S OWN LISTING, never a hand-kept
// list: one `go list -deps -test` of the module names, for each package's test
// binary ("P.test"), every package it links, transitively, inside the module
// and in the generated proto packages; each one's directory, testdata and
// embedded files are placed, with every such module's go.mod and go.sum. The
// toolchain's versions, the keyed environment (the build tags included) and
// the package's roster entry are its facts. What its tests read beyond that
// graph is declared in PackageReuse.Inputs, and the package runs under a
// profile refusing every other read in the checkout: a read the declaration
// misses fails the package, so it can never pass on bytes its key omits.
//
// A package whose reads cannot be enumerated is named in PackageReuse.Never:
// it runs whenever its suite runs, under the suite's own profile.

// PackageVerdict is one unit-test package's reuse decision.
type PackageVerdict struct {
	// Suite and Package (relative to the suite's module) name it.
	Suite, Package string
	// Entry is the suite's roster entry the package was keyed under.
	Entry roster.Suite
	// Keyed is a package this run confines to its own inputs and may record.
	Keyed bool
	Key   Key
	Set   *InputSet
	// Profile is its profile file (keyed packages only).
	Profile string
	// Reused is the record this run reuses instead of running its tests.
	Reused *Record
	// Why says why a package that runs is not reused.
	Why string
}

// Name is the package's record name in the store.
func (v PackageVerdict) Name() string { return PackageRecordName(v.Suite, v.Package) }

// PackageRecordName is package rel of suite's name in the store: the suite,
// "@", and rel with each "/" spelled "%", so every package's records sit in
// one flat directory of their own.
func PackageRecordName(suite, rel string) string {
	return suite + "@" + strings.ReplaceAll(rel, "/", "%")
}

// Group is the package's chunk group (suites' goPkg.group, untagged).
func (v PackageVerdict) Group() string { return v.Suite + ":" + v.Package }

// DecidePackages keys every unit-test package of a keyed Go suite that runs,
// and decides which this run reuses. A suite without PackageReuse, one this
// run does not confine, one it reuses whole, and a targeted run decide no
// package.
func DecidePackages(d Deps, v Verdict, mode Mode, pkgs []string) ([]PackageVerdict, error) {
	s := v.Suite
	if s.PackageReuse == nil || !v.Keyed || v.Reused != nil || mode == Targeted {
		return nil, nil
	}
	if s.Kind != roster.GoModule {
		return nil, fmt.Errorf("reuse: %s declares PackageReuse but is no Go module", s.Name)
	}
	if err := checkPackageDeclarations(s, pkgs); err != nil {
		return nil, err
	}
	dir := roster.Resolve(d.Repo, d.Module, s.Path)
	listing, err := d.GoList(dir, "", true, "./...")
	if err != nil {
		return nil, fmt.Errorf("reuse: list %s's packages: %w", s.Name, err)
	}
	byPath := map[string]GoPackage{}
	for _, p := range listing {
		byPath[p.ImportPath] = p
	}
	modulePath, err := gobuild.ModulePath(dir)
	if err != nil {
		return nil, fmt.Errorf("reuse: %s: %w", s.Name, err)
	}
	runner, err := d.GoList(roster.Resolve(d.Repo, d.Module, "testrun"), "", false, ".")
	if err != nil {
		return nil, err
	}
	tools, err := d.Toolchain(s.Kind)
	if err != nil {
		return nil, fmt.Errorf("reuse: %s's toolchain: %w", s.Name, err)
	}
	out := make([]PackageVerdict, len(pkgs))
	errs := make([]error, len(pkgs))
	var wg sync.WaitGroup
	for i, rel := range pkgs {
		out[i] = PackageVerdict{Suite: s.Name, Package: rel, Entry: s}
		if why, never := s.PackageReuse.Never[rel]; never {
			out[i].Why = "never reused: " + why
			continue
		}
		wg.Add(1)
		go func() {
			defer wg.Done()
			errs[i] = decidePackage(d, &out[i], mode, packageGraph{
				suite: s, main: gobuild.ImportPath(modulePath, rel) + ".test", byPath: byPath, runner: runner, tools: tools,
			})
		}()
	}
	wg.Wait()
	for _, err := range errs {
		if err != nil {
			return nil, err
		}
	}
	return out, nil
}

// checkPackageDeclarations refuses a PackageReuse naming a package the suite
// does not test: the roster has drifted from the tree.
func checkPackageDeclarations(s roster.Suite, pkgs []string) error {
	tested := map[string]bool{}
	for _, p := range pkgs {
		tested[p] = true
	}
	var stale []string
	for rel := range s.PackageReuse.Inputs {
		if !tested[rel] {
			stale = append(stale, rel)
		}
	}
	for rel := range s.PackageReuse.Never {
		if !tested[rel] {
			stale = append(stale, rel)
		}
		if _, both := s.PackageReuse.Inputs[rel]; both {
			return fmt.Errorf("reuse: %s's PackageReuse both declares %s's inputs and names it never reused", s.Name, rel)
		}
	}
	if len(stale) > 0 {
		sort.Strings(stale)
		return fmt.Errorf("reuse: %s's PackageReuse names %s, which it tests no package of", s.Name, strings.Join(stale, ", "))
	}
	return nil
}

// packageGraph is what one package's key is derived from.
type packageGraph struct {
	suite  roster.Suite
	main   string
	byPath map[string]GoPackage
	runner []GoPackage
	tools  map[string]string
}

// PackageInputs is a package's input set: every package its test binary
// links, by the listing, the runner, and its declared inputs.
func PackageInputs(d Deps, s roster.Suite, rel string, main GoPackage, byPath map[string]GoPackage, runner []GoPackage) (*InputSet, error) {
	pkgs := []GoPackage{main}
	for _, dep := range main.Deps {
		p, ok := byPath[dep]
		if !ok {
			return nil, fmt.Errorf("reuse: %s's test binary links %s, which the listing does not hold", main.ImportPath, dep)
		}
		pkgs = append(pkgs, p)
	}
	set := NewInputSet()
	if err := AddGoPackages(set, d.Repo, pkgs); err != nil {
		return nil, err
	}
	if err := AddGoPackages(set, d.Repo, runner); err != nil {
		return nil, err
	}
	for _, p := range s.PackageReuse.Inputs[rel] {
		if err := placeRosterPath(d, set, p, true); err != nil {
			return nil, fmt.Errorf("reuse: %s's package %s: %w", s.Name, rel, err)
		}
	}
	return set, nil
}

func decidePackage(d Deps, v *PackageVerdict, mode Mode, g packageGraph) error {
	main, ok := g.byPath[g.main]
	if !ok {
		return fmt.Errorf("reuse: %s tests %s, but the listing holds no test binary %s", v.Suite, v.Package, g.main)
	}
	set, err := PackageInputs(d, g.suite, v.Package, main, g.byPath, g.runner)
	if err != nil {
		return err
	}
	dec, err := decideKeyed(d, v.Name(), set, packageFacts(d, g.suite, v.Package, g.tools), mode)
	if err != nil {
		return err
	}
	v.Keyed, v.Key, v.Set, v.Profile, v.Reused, v.Why = true, dec.Key, set, dec.Profile, dec.Reused, dec.Why
	return nil
}

// packageFacts are a package's non-file inputs: its suite's toolchain and
// keyed environment (the build tags and every GO variable among it), and the
// parts of its roster entry that decide how its tests build and run.
func packageFacts(d Deps, s roster.Suite, rel string, tools map[string]string) Facts {
	desc := fmt.Sprintf("suite=%q path=%q package=%q parallel=%#v programs=%q inputs=%q",
		s.Name, s.Path, rel, s.TestParallel[rel], s.Programs[rel], s.PackageReuse.Inputs[rel])
	return Facts{Suite: desc, Toolchain: tools, Env: KeyedEnv(d.Environ)}
}

// SkipReusedPackages drops every unit of the reused packages from u, and
// places u's module builds over the packages that remain (suites.Units.Finish).
func SkipReusedPackages(verdicts []PackageVerdict, u suites.Units) suites.Units {
	reused := map[string]bool{}
	for _, v := range verdicts {
		if v.Reused != nil {
			reused[v.Group()] = true
			reused[suites.CompiledUnitID(v.Group())] = true
		}
	}
	if len(reused) == 0 {
		return u
	}
	var splits []suites.Split
	for _, sp := range u.Splits {
		if !reused[sp.Group] {
			splits = append(splits, sp)
		}
	}
	var atomic []run.Spec
	for _, a := range u.Atomic {
		if !reused[a.ID] {
			atomic = append(atomic, a)
		}
	}
	u.Splits, u.Atomic = splits, atomic
	return u.Finish()
}

// packageProfiles maps each keyed package's units (its chunk group and its
// compiled check) to the package's profile.
func packageProfiles(verdicts []PackageVerdict) map[string]string {
	out := map[string]string{}
	for _, v := range verdicts {
		if v.Keyed && v.Reused == nil {
			out[v.Group()] = v.Profile
			out[suites.CompiledUnitID(v.Group())] = v.Profile
		}
	}
	return out
}

// ReportPackages prints each package's line: a reused one's in a shape the
// merge gate's suite parser never reads as a suite's
// (daemon/internal/merge/testgate.go suiteReused), a running one's reason.
func ReportPackages(log Logger, verdicts []PackageVerdict) {
	for _, v := range verdicts {
		if v.Reused != nil {
			log.Infof("%s: reused package %s's green run (inputs %s, passed %s on %s)", v.Suite, v.Package, v.Key.Short(),
				v.Reused.PassedAt.Local().Format("2006-01-02 15:04"), Source(*v.Reused))
			continue
		}
		log.Infof("%s: package %s runs: %s", v.Suite, v.Package, v.Why)
	}
}

// ReusedPackageNames is every reused package, as "<suite>:<package>".
func ReusedPackageNames(verdicts []PackageVerdict) []string {
	var out []string
	for _, v := range verdicts {
		if v.Reused != nil {
			out = append(out, v.Group())
		}
	}
	return out
}

// RecordPackagePasses stores a pass for every keyed package of the run all of
// whose units passed, after re-keying it, as RecordPasses does for a suite.
func RecordPackagePasses(d Deps, mode Mode, verdicts []PackageVerdict, results []run.Result) error {
	if mode == Targeted {
		return nil
	}
	ran, failed := map[string]int{}, map[string]bool{}
	for _, r := range results {
		g := packageOfUnit(r.Spec.ID)
		ran[g]++
		if r.Outcome != run.Passed {
			failed[g] = true
		}
	}
	r := recorder{d: d}
	recorded := map[string]int{}
	tools := map[string]map[string]string{}
	for _, v := range verdicts {
		g := v.Group()
		if !v.Keyed || v.Reused != nil || ran[g] == 0 || failed[g] {
			if v.Keyed && v.Reused == nil && failed[g] {
				d.Log.Infof("%s ran confined to its own inputs: a failure reading a checkout file with \"operation not permitted\", or a listing of a checkout directory that comes back empty (filepath.Glob drops the refusal), is a read its PackageReuse does not declare (testrun/roster; the /agent-repl-tests skill, \"A unit's reuse inputs come from the tool's dependency listing\")", g)
			}
			continue
		}
		if tools[v.Suite] == nil {
			t, err := d.Toolchain(v.Entry.Kind)
			if err != nil {
				return fmt.Errorf("reuse: %s's toolchain: %w", v.Suite, err)
			}
			tools[v.Suite] = t
		}
		ok, err := r.record(v.Name(), g, v.Set, packageFacts(d, v.Entry, v.Package, tools[v.Suite]), v.Key, ran[g])
		if err != nil {
			return err
		}
		if ok {
			recorded[v.Suite]++
		}
	}
	for _, name := range slices.Sorted(maps.Keys(recorded)) {
		d.Log.Infof("%s: recorded the green runs of %d packages", name, recorded[name])
	}
	return nil
}

// UnitPackages are the packages a Go suite's ordinary pass builds in u: the
// packages its untagged module build names.
func UnitPackages(suite string, u suites.Units) []string {
	seen := map[string]bool{}
	var out []string
	for _, b := range u.GoBuilds {
		if b.ID != suites.ModuleBuildID(suite, "") {
			continue
		}
		for _, rel := range b.Needs {
			if !seen[rel] {
				seen[rel] = true
				out = append(out, rel)
			}
		}
	}
	sort.Strings(out)
	return out
}

// packageOfUnit is the chunk group a unit ID belongs to: a chunk's
// "<group>#NN", a compiled check's "<group>:compiled", or the ID itself.
func packageOfUnit(id string) string {
	if i := strings.LastIndex(id, "#"); i >= 0 {
		return id[:i]
	}
	return strings.TrimSuffix(id, ":compiled")
}
