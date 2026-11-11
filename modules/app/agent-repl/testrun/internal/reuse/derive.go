package reuse

import (
	"fmt"
	"io/fs"
	"path"
	"path/filepath"
	"slices"
	"sort"
	"strings"

	"agentrepl/testrun/internal/gobuild"
	"agentrepl/testrun/internal/suites"
	"agentrepl/testrun/roster"
)

// Derive cuts suite s, whose units the run built as u, into its reuse units,
// each with its input set. With check, every unit the roster names (Reuse's
// Units and Never) must be one of them: a roster naming a unit the suite does
// not have has drifted from the tree. A narrowed run (Targeted) passes no
// check, since it holds only some of the suite's units.
//
// A suite that is never reused is one unit, owning every spec, keyed on
// nothing.
func Derive(d Deps, s roster.Suite, u suites.Units, check bool) (SuiteUnits, error) {
	if s.Reuse == nil {
		return SuiteUnits{Suite: s, Units: []Unit{wholeSuite(s, u, nil, s.NeverReused)}}, nil
	}
	env, err := SuiteInputs(d, s)
	if err != nil {
		return SuiteUnits{}, fmt.Errorf("reuse: derive %s's inputs: %w", s.Name, err)
	}
	su := SuiteUnits{Suite: s}
	switch s.Kind {
	case roster.Script, roster.SplitScript, roster.E2E:
		su.Units = []Unit{wholeSuite(s, u, env, "")}
	case roster.GoModule:
		su.Units, su.Infra, err = deriveGo(d, s, u)
	case roster.Vitest:
		su.Units, su.Infra, err = deriveVitest(d, s, u, env)
	case roster.ERT:
		su.Units, su.Infra, err = deriveERT(d, s, u)
	default:
		return SuiteUnits{}, fmt.Errorf("reuse: %s is a kind of suite this runner cannot cut into units", s.Name)
	}
	if err != nil {
		return SuiteUnits{}, err
	}
	if check {
		if err := checkDeclaredUnits(s, su.Units); err != nil {
			return SuiteUnits{}, err
		}
	}
	// The envelope holds every unit's inputs too, so an infra unit (a build
	// that bundles what the units import) reads whatever they do.
	for _, unit := range su.Units {
		if unit.Set != nil {
			env.AddAll(unit.Set)
		}
	}
	su.Envelope = env
	return su, nil
}

// wholeSuite is the one unit of a suite reused whole: every spec of u, keyed
// on set (nil for a never-reused suite).
func wholeSuite(s roster.Suite, u suites.Units, set *InputSet, never string) Unit {
	unit := Unit{Suite: s.Name, Set: set, Desc: Describe(s), Never: never}
	for _, a := range u.Atomic {
		unit.Specs = append(unit.Specs, a.ID)
	}
	for _, sp := range u.Splits {
		unit.Specs = append(unit.Specs, sp.Group)
	}
	return unit
}

// checkDeclaredUnits refuses a Reuse naming a unit the suite does not have.
func checkDeclaredUnits(s roster.Suite, units []Unit) error {
	have := map[string]bool{}
	for _, u := range units {
		have[u.Name] = true
	}
	var stale []string
	for _, m := range []map[string]bool{keysOf(s.Reuse.Units), keysOf(s.Reuse.Never)} {
		for name := range m {
			if !have[name] {
				stale = append(stale, name)
			}
		}
	}
	if len(stale) > 0 {
		sort.Strings(stale)
		return fmt.Errorf("reuse: %s's Reuse names %s, which is no unit of the suite (testrun/roster)", s.Name, strings.Join(stale, ", "))
	}
	return nil
}

func keysOf[V any](m map[string]V) map[string]bool {
	out := map[string]bool{}
	for k := range m {
		out[k] = true
	}
	return out
}

// unitBuilder gathers a suite's units by name, in first-seen order.
type unitBuilder struct {
	suite roster.Suite
	order []string
	units map[string]*Unit
	infra []string
}

func newUnitBuilder(s roster.Suite) *unitBuilder {
	return &unitBuilder{suite: s, units: map[string]*Unit{}}
}

// own gives spec id to the unit named name.
func (b *unitBuilder) own(name, id string) *Unit {
	u, ok := b.units[name]
	if !ok {
		u = &Unit{Suite: b.suite.Name, Name: name, Never: b.suite.Reuse.Never[name]}
		b.units[name] = u
		b.order = append(b.order, name)
	}
	u.Specs = append(u.Specs, id)
	return u
}

// list is the units, in first-seen order.
func (b *unitBuilder) list() []Unit {
	out := make([]Unit, 0, len(b.order))
	for _, n := range b.order {
		out = append(out, *b.units[n])
	}
	return out
}

// unitDesc spells a unit's roster facts: the suite's entry parts that decide
// how the unit is built and run, and what it declares.
func unitDesc(s roster.Suite, name string, extra ...string) string {
	r := s.Reuse
	return strings.Join(append([]string{
		fmt.Sprintf("suite=%q kind=%d path=%q unit=%q inputs=%q generated=%q own=%q",
			s.Name, s.Kind, s.Path, name, r.Inputs, r.Generated, r.Units[name]),
	}, extra...), " ")
}

// unitSet is a unit's input set beneath its derived graph: what the suite
// declares every unit reads, what it declares this one reads, and the
// runner's own code, which decides how every unit is built, split and run.
func unitSet(d Deps, s roster.Suite, name string, runner []GoPackage) (*InputSet, error) {
	set := NewInputSet()
	if err := addDeclared(d, set, s.Reuse); err != nil {
		return nil, fmt.Errorf("reuse: %s: %w", s.Name, err)
	}
	if err := addInputs(d, set, s.Reuse.Units[name]); err != nil {
		return nil, fmt.Errorf("reuse: %s's unit %s: %w", s.Name, name, err)
	}
	if err := AddGoPackages(set, d.Repo, runner); err != nil {
		return nil, err
	}
	return set, nil
}

// runnerPackages is the runner's own build graph.
func runnerPackages(d Deps) ([]GoPackage, error) {
	return d.GoList(roster.Resolve(d.Repo, d.Module, "testrun"), "", false, ".")
}

// GO MODULES. A package's unit is its split and its checks (the compiled
// check of a package whose test files hold no test, its prebuild), named by
// its path and, under a tagged pass, its tags: "internal/foo",
// "integration[integration]". Its inputs are every package its test binary
// links, by the Go tool's own listing (`go list -deps -test`), and the
// programs its tests start. Each vet pass is a unit over the whole module's
// listing; the module builds are infra.

// goGroupUnit is the unit name of a Go package's chunk group
// ("<suite>:<rel>[<tags>]"), its package and its tags.
func goGroupUnit(suite, group string) (name, rel, tags string, err error) {
	name, ok := strings.CutPrefix(group, suite+":")
	if !ok || name == "" {
		return "", "", "", fmt.Errorf("reuse: %q is no package group of %s", group, suite)
	}
	rel = name
	if i := strings.LastIndex(name, "["); i >= 0 && strings.HasSuffix(name, "]") {
		rel, tags = name[:i], name[i+1:len(name)-1]
	}
	return name, rel, tags, nil
}

func deriveGo(d Deps, s roster.Suite, u suites.Units) ([]Unit, []string, error) {
	dir := roster.Resolve(d.Repo, d.Module, s.Path)
	modulePath, err := gobuild.ModulePath(dir)
	if err != nil {
		return nil, nil, fmt.Errorf("reuse: %s: %w", s.Name, err)
	}
	runner, err := runnerPackages(d)
	if err != nil {
		return nil, nil, err
	}
	b := newUnitBuilder(s)
	type pkg struct{ rel, tags string }
	pkgs := map[string]pkg{}
	vets := map[string]string{}
	for _, sp := range u.Splits {
		name, rel, tags, err := goGroupUnit(s.Name, sp.Group)
		if err != nil {
			return nil, nil, err
		}
		b.own(name, sp.Group)
		pkgs[name] = pkg{rel, tags}
	}
	for _, a := range u.Atomic {
		id := a.ID
		switch {
		case id == suites.ModuleBuildID(s.Name, "") || strings.HasPrefix(id, s.Name+":build["):
			b.infra = append(b.infra, id)
		case id == s.Name+":vet":
			b.own("vet", id)
			vets["vet"] = ""
		case strings.HasPrefix(id, s.Name+":vet["):
			tag := strings.TrimSuffix(strings.TrimPrefix(id, s.Name+":vet["), "]")
			b.own("vet["+tag+"]", id)
			vets["vet["+tag+"]"] = tag
		case id == s.Name+":coverage":
			b.own("coverage", id).Never = "a coverage report is rebuilt by every coverage run, which reuses nothing"
		case strings.HasSuffix(id, ":compiled") || strings.HasSuffix(id, ":prebuild"):
			group := id[:strings.LastIndex(id, ":")]
			name, rel, tags, err := goGroupUnit(s.Name, group)
			if err != nil {
				return nil, nil, err
			}
			b.own(name, id)
			pkgs[name] = pkg{rel, tags}
		default:
			return nil, nil, fmt.Errorf("reuse: %s's unit %s is no package's, vet's or build's", s.Name, id)
		}
	}
	listings := map[string]map[string]GoPackage{}
	listed := map[string][]GoPackage{}
	listing := func(tags string) (map[string]GoPackage, error) {
		if m, ok := listings[tags]; ok {
			return m, nil
		}
		pkgs, err := d.GoList(dir, tags, true, "./...")
		if err != nil {
			return nil, fmt.Errorf("reuse: list %s's packages: %w", s.Name, err)
		}
		m := map[string]GoPackage{}
		for _, p := range pkgs {
			m[p.ImportPath] = p
		}
		listings[tags], listed[tags] = m, pkgs
		return m, nil
	}
	modRel, inside, err := relInside(d.Repo, dir)
	if err != nil {
		return nil, nil, err
	}
	if !inside {
		return nil, nil, fmt.Errorf("reuse: %s's module %s is outside the checkout", s.Name, dir)
	}
	for _, name := range b.order {
		unit := b.units[name]
		if unit.Never != "" {
			continue
		}
		set, err := unitSet(d, s, name, runner)
		if err != nil {
			return nil, nil, err
		}
		if tag, ok := vets[name]; ok {
			if _, err := listing(tag); err != nil {
				return nil, nil, err
			}
			if err := AddPatternWalk(set, d.Repo, modRel); err != nil {
				return nil, nil, err
			}
			if err := AddGoPackages(set, d.Repo, listed[tag]); err != nil {
				return nil, nil, err
			}
			unit.Set, unit.Desc = set, unitDesc(s, name, fmt.Sprintf("vet=%q", tag))
			continue
		}
		p := pkgs[name]
		byPath, err := listing(p.tags)
		if err != nil {
			return nil, nil, err
		}
		main := gobuild.ImportPath(modulePath, p.rel) + ".test"
		if err := addTestBinary(d, set, byPath, main); err != nil {
			return nil, nil, fmt.Errorf("reuse: %s tests %s: %w", s.Name, name, err)
		}
		if err := addProgramGraphs(d, set, s, s.Programs[p.rel]); err != nil {
			return nil, nil, err
		}
		unit.Set = set
		unit.Desc = unitDesc(s, name, fmt.Sprintf("tags=%q parallel=%#v programs=%q", p.tags, s.TestParallel[p.rel], s.Programs[p.rel]))
	}
	return b.list(), b.infra, nil
}

// addTestBinary places the test binary main ("<import path>.test") and every
// package it links, by the listing byPath.
func addTestBinary(d Deps, set *InputSet, byPath map[string]GoPackage, main string) error {
	m, ok := byPath[main]
	if !ok {
		return fmt.Errorf("the listing holds no test binary %s", main)
	}
	pkgs := []GoPackage{m}
	for _, dep := range m.Deps {
		p, ok := byPath[dep]
		if !ok {
			return fmt.Errorf("%s links %s, which the listing does not hold", main, dep)
		}
		pkgs = append(pkgs, p)
	}
	return AddGoPackages(set, d.Repo, pkgs)
}

// addProgramGraphs places the build graph of each shared program names: a
// unit that starts a program reads what the program is built from.
func addProgramGraphs(d Deps, set *InputSet, s roster.Suite, names []string) error {
	sorted := append([]string(nil), names...)
	sort.Strings(sorted)
	for _, n := range sorted {
		p, ok := roster.LookupProgram(n)
		if !ok {
			return fmt.Errorf("reuse: %s runs the program %q, which the roster does not declare", s.Name, n)
		}
		pkgs, err := d.GoList(roster.Resolve(d.Repo, d.Module, p.Module), "", false, p.Package)
		if err != nil {
			return err
		}
		if err := AddGoPackages(set, d.Repo, pkgs); err != nil {
			return err
		}
	}
	return nil
}

// VITEST. Each test file is its own unit, run in its own process (a file
// sharing a process with another could read that file's inputs unconfined),
// named by its path relative to the package. Its inputs are its import graph,
// with its config's and the config's setup files', the package's manifests,
// and the runner's reporter. The typecheck is one unit over the package's
// tree and every file's graph; the integration bundle is infra.

// vitestManifests are the package files every vitest unit reads when present.
var vitestManifests = []string{"package.json", "package-lock.json", "tsconfig.json"}

func deriveVitest(d Deps, s roster.Suite, u suites.Units, env *InputSet) ([]Unit, []string, error) {
	pkgRel, _, err := RepoPath(d, s.Path)
	if err != nil {
		return nil, nil, err
	}
	runner, err := runnerPackages(d)
	if err != nil {
		return nil, nil, err
	}
	reporter, _, err := RepoPath(d, "testrun/vitest")
	if err != nil {
		return nil, nil, err
	}
	b := newUnitBuilder(s)
	configOf := map[string]string{}
	for _, sp := range u.Splits {
		file, config, err := suites.VitestFileOfGroup(s, sp.Group)
		if err != nil {
			return nil, nil, err
		}
		if _, dup := configOf[file]; dup {
			return nil, nil, fmt.Errorf("reuse: %s runs %s under two configs", s.Name, file)
		}
		b.own(file, sp.Group)
		configOf[file] = config
	}
	typecheck := ""
	for _, a := range u.Atomic {
		switch a.ID {
		case s.Name + ":typecheck":
			typecheck = "typecheck"
			b.own(typecheck, a.ID)
		case s.Name + ":integration-build":
			b.infra = append(b.infra, a.ID)
		case s.Name + ":coverage":
			b.own("coverage", a.ID).Never = "a coverage report is rebuilt by every coverage run, which reuses nothing"
		default:
			return nil, nil, fmt.Errorf("reuse: %s's unit %s is no file's, typecheck's or build's", s.Name, a.ID)
		}
	}
	graph := NewImportGraph(d.Repo)
	listings, err := packageListings(d.Repo, pkgRel, env)
	if err != nil {
		return nil, nil, err
	}
	all := NewInputSet()
	for _, name := range b.order {
		unit := b.units[name]
		config, ok := configOf[name]
		if !ok || unit.Never != "" {
			continue
		}
		set, err := unitSet(d, s, name, runner)
		if err != nil {
			return nil, nil, err
		}
		if err := set.AddFlatDir(reporter); err != nil {
			return nil, nil, err
		}
		for _, m := range vitestManifests {
			if err := graph.addIfFile(set, path.Join(pkgRel, m)); err != nil {
				return nil, nil, err
			}
		}
		// Vitest walks the whole package to match its include globs, even
		// to run one file: every listing it reads is hashed, so a test file
		// added anywhere reruns the suite's file units, and an edited one
		// only the units whose graph holds it.
		for _, l := range listings {
			if err := set.AddDir(l); err != nil {
				return nil, nil, err
			}
		}
		configRel := path.Join(pkgRel, config)
		setup, err := graph.SetupFiles(configRel)
		if err != nil {
			return nil, nil, err
		}
		files, err := graph.Closure(append([]string{path.Join(pkgRel, name), configRel}, setup...))
		if err != nil {
			return nil, nil, fmt.Errorf("reuse: %s's file %s: %w", s.Name, name, err)
		}
		for _, f := range files {
			if err := set.AddFile(f); err != nil {
				return nil, nil, err
			}
			// A resolver lists a file's directory to find it.
			if err := set.AddDir(path.Dir(f)); err != nil {
				return nil, nil, err
			}
		}
		unit.Set = set
		unit.Desc = unitDesc(s, name, fmt.Sprintf("config=%q", config))
		all.AddAll(set)
	}
	if typecheck != "" && b.units[typecheck].Never == "" {
		// tsc reads the package's whole tree and every file it imports.
		set, err := unitSet(d, s, typecheck, runner)
		if err != nil {
			return nil, nil, err
		}
		set.AddAll(env)
		set.AddAll(all)
		b.units[typecheck].Set = set
		b.units[typecheck].Desc = unitDesc(s, typecheck)
	}
	return b.list(), b.infra, nil
}

// packageListings is every directory of the package at pkgRel that vitest's
// file discovery walks: all of them but the suite's generated subtrees (its
// node_modules, its build output) and dot directories, which its globs skip.
func packageListings(repo, pkgRel string, env *InputSet) ([]string, error) {
	generated := env.Generated()
	var out []string
	root := filepath.Join(repo, filepath.FromSlash(pkgRel))
	err := filepath.WalkDir(root, func(abs string, e fs.DirEntry, err error) error {
		if err != nil {
			return fmt.Errorf("reuse: walk %s: %w", abs, err)
		}
		if !e.IsDir() {
			return nil
		}
		rel, err := filepath.Rel(repo, abs)
		if err != nil {
			return fmt.Errorf("reuse: place %s under %s: %w", abs, repo, err)
		}
		rel = filepath.ToSlash(rel)
		if abs != root && (strings.HasPrefix(e.Name(), ".") || slices.ContainsFunc(generated, func(g string) bool { return under(rel, g) })) {
			return filepath.SkipDir
		}
		out = append(out, rel)
		return nil
	})
	return out, err
}

// ERT. Each test file is its own unit, run in its own Emacs, named by its
// file name. Its inputs are the file, the helpers every file loads first, the
// module files the helpers load (config.el and every lisp module it loads),
// what the file loads beside them, the runner's driver, and the fake daemon
// its tests talk to, whose build (infra) its key covers by build graph.

func deriveERT(d Deps, s roster.Suite, u suites.Units) ([]Unit, []string, error) {
	lispRel, _, err := RepoPath(d, s.Path)
	if err != nil {
		return nil, nil, err
	}
	runner, err := runnerPackages(d)
	if err != nil {
		return nil, nil, err
	}
	driver, _, err := RepoPath(d, "testrun/ert")
	if err != nil {
		return nil, nil, err
	}
	b := newUnitBuilder(s)
	for _, sp := range u.Splits {
		file, ok := strings.CutPrefix(sp.Group, s.Name+"/")
		if !ok || file == "" {
			return nil, nil, fmt.Errorf("reuse: %q is no file group of %s", sp.Group, s.Name)
		}
		b.own(file, sp.Group)
	}
	for _, a := range u.Atomic {
		if a.ID != s.Name+":fakedaemon" {
			return nil, nil, fmt.Errorf("reuse: %s's unit %s is no file's or the fake daemon's build", s.Name, a.ID)
		}
		b.infra = append(b.infra, a.ID)
	}
	fake, err := d.GoList(filepath.Join(roster.Resolve(d.Repo, d.Module, s.Path), "testsupport", "fakedaemon"), "", false, ".")
	if err != nil {
		return nil, nil, err
	}
	loaded, err := ERTModuleSources(d.Repo, lispRel)
	if err != nil {
		return nil, nil, err
	}
	for _, name := range b.order {
		unit := b.units[name]
		if unit.Never != "" {
			continue
		}
		set, err := unitSet(d, s, name, runner)
		if err != nil {
			return nil, nil, err
		}
		if err := set.AddFlatDir(driver); err != nil {
			return nil, nil, err
		}
		// Emacs starts in the suite's directory and reads it as its
		// default directory.
		if err := set.AddDir(lispRel); err != nil {
			return nil, nil, err
		}
		if err := AddGoPackages(set, d.Repo, fake); err != nil {
			return nil, nil, err
		}
		own, err := ERTFileLoads(d.Repo, lispRel, name)
		if err != nil {
			return nil, nil, err
		}
		for _, f := range append(append([]string(nil), loaded...), own...) {
			if err := set.AddFile(f); err != nil {
				return nil, nil, err
			}
		}
		unit.Set, unit.Desc = set, unitDesc(s, name)
	}
	return b.list(), b.infra, nil
}
