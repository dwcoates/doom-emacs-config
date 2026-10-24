// Package selection narrows a run to the tests a testrun.v1.TestRunRequest
// names: it resolves each target to the roster suite that owns it, then keeps
// only the units that run what was named, with the build and program units
// they depend on.
//
// NOTHING IS RUN AS NOTHING. A target no suite owns, a package with no tests,
// a test or file or group its target does not hold, is refused with a reason,
// because a caller that misspelled a test must never be told it passed.
//
// THE NARROWEST RUN. A named Go test path runs alone in its chunk (go's
// -test.run matches each level separately, so two paths cannot share one
// pattern); a file with named tests runs in its own chunk with its names; a
// whole file or a whole test shares its suite's ordinary chunking. A test
// named twice, or named whole and by a subtest, runs once, whole.
package selection

import (
	"errors"
	"fmt"
	"path/filepath"
	"slices"
	"sort"
	"strings"

	testrunv1 "agentrepl/proto/testrun/v1"

	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/suites"
	"agentrepl/testrun/roster"
)

// kind is which framework a target belongs to.
type kind int

const (
	kindGo kind = iota
	kindVitest
	kindERT
	kindHarness
	// kindSuite is a whole roster suite inside a narrowed request.
	kindSuite
)

// target is one resolved selection: where it lives within its suite and what
// of it runs.
type target struct {
	kind kind
	// where is the package directory relative to the suite's module (Go), the
	// file relative to the suite's directory (vitest), the file's base name
	// (ERT), or the script's path (harness).
	where string
	// all runs everything the target holds; otherwise names does.
	all bool
	// names are the named tests: Go test paths (TestFoo or TestFoo/a_case),
	// vitest full names, ERT test symbols, or harness groups.
	names []string
	// what names the target for a refusal.
	what string
}

// Resolved is a request resolved against the roster.
type Resolved struct {
	// Suites are the suites the request touches, in roster order.
	Suites  []roster.Suite
	targets map[string][]target
}

// Whole answers the suites a request names when it names only whole suites:
// the names of its `suite` targets, or none at all for `every_suite` (every
// suite runs). ok is false for a request that narrows anything, which Resolve
// takes instead. `every_suite` must be the request's one selection.
func Whole(req *testrunv1.TestRunRequest) (names []string, ok bool, err error) {
	sels := req.GetSelections()
	if len(sels) == 0 {
		return nil, false, errors.New("the request names no test")
	}
	every := 0
	for _, sel := range sels {
		switch t := sel.GetTarget().(type) {
		case *testrunv1.TestSelection_EverySuite:
			every++
		case *testrunv1.TestSelection_Suite:
			if _, known := roster.Lookup(t.Suite.GetName()); !known {
				return nil, false, fmt.Errorf("suite %q: no such suite in the roster; known suites: %s", t.Suite.GetName(), strings.Join(roster.Names(), " "))
			}
			if !slices.Contains(names, t.Suite.GetName()) {
				names = append(names, t.Suite.GetName())
			}
		default:
			if every > 0 {
				return nil, false, errors.New("every_suite runs every suite, so it is the request's one selection")
			}
			return nil, false, nil
		}
	}
	if every > 0 {
		if len(sels) > 1 {
			return nil, false, errors.New("every_suite runs every suite, so it is the request's one selection")
		}
		return nil, true, nil
	}
	return names, true, nil
}

// Resolve resolves every selection of req to the suite that owns it.
func Resolve(req *testrunv1.TestRunRequest, l suites.Layout) (Resolved, error) {
	if len(req.GetSelections()) == 0 {
		return Resolved{}, errors.New("the request names no test")
	}
	r := Resolved{targets: map[string][]target{}}
	for i, sel := range req.GetSelections() {
		suite, t, err := resolveOne(sel, l)
		if err != nil {
			return Resolved{}, fmt.Errorf("selection %d: %w", i+1, err)
		}
		r.targets[suite.Name] = append(r.targets[suite.Name], t)
	}
	for _, s := range roster.Suites {
		if _, ok := r.targets[s.Name]; ok {
			r.Suites = append(r.Suites, s)
		}
	}
	return r, nil
}

func resolveOne(sel *testrunv1.TestSelection, l suites.Layout) (roster.Suite, target, error) {
	switch t := sel.GetTarget().(type) {
	case *testrunv1.TestSelection_GoPackage:
		return resolveGo(t.GoPackage, l)
	case *testrunv1.TestSelection_VitestFile:
		f := t.VitestFile
		all, names, err := allOrNamed("vitest file "+f.GetFile(), f.GetAll() != nil, f.GetNamed().GetFullNames(), f.GetNamed() != nil)
		if err != nil {
			return roster.Suite{}, target{}, err
		}
		s, rel, err := owningDir(l, f.GetFile(), "vitest file", roster.Vitest)
		return s, target{kind: kindVitest, where: rel, all: all, names: names, what: "vitest file " + f.GetFile()}, err
	case *testrunv1.TestSelection_ErtFile:
		f := t.ErtFile
		all, names, err := allOrNamed("ERT file "+f.GetFile(), f.GetAll() != nil, f.GetNamed().GetTestNames(), f.GetNamed() != nil)
		if err != nil {
			return roster.Suite{}, target{}, err
		}
		s, rel, err := owningDir(l, f.GetFile(), "ERT file", roster.ERT)
		return s, target{kind: kindERT, where: filepath.Base(rel), all: all, names: names, what: "ERT file " + f.GetFile()}, err
	case *testrunv1.TestSelection_Suite:
		suite, known := roster.Lookup(t.Suite.GetName())
		if !known {
			return roster.Suite{}, target{}, fmt.Errorf("suite %q: no such suite in the roster; known suites: %s", t.Suite.GetName(), strings.Join(roster.Names(), " "))
		}
		return suite, target{kind: kindSuite, all: true, what: "suite " + suite.Name}, nil
	case *testrunv1.TestSelection_EverySuite:
		return roster.Suite{}, target{}, errors.New("every_suite runs every suite, so it is the request's one selection")
	case *testrunv1.TestSelection_Harness:
		h := t.Harness
		all, names, err := allOrNamed("harness "+h.GetScript(), h.GetAll() != nil, h.GetNamed().GetNames(), h.GetNamed() != nil)
		if err != nil {
			return roster.Suite{}, target{}, err
		}
		s, err := owningScript(l, h.GetScript())
		return s, target{kind: kindHarness, where: h.GetScript(), all: all, names: names, what: "harness " + h.GetScript()}, err
	}
	return roster.Suite{}, target{}, errors.New("the selection names no target")
}

// allOrNamed reads a target's all-or-named choice, refusing an unset one or
// an empty name.
func allOrNamed(what string, all bool, names []string, named bool) (bool, []string, error) {
	switch {
	case all:
		return true, nil, nil
	case !named:
		return false, nil, fmt.Errorf("%s: says neither all of its tests nor which", what)
	case len(names) == 0:
		return false, nil, fmt.Errorf("%s: names no test", what)
	}
	for _, n := range names {
		if strings.TrimSpace(n) == "" {
			return false, nil, fmt.Errorf("%s: names an empty test", what)
		}
	}
	return false, names, nil
}

func resolveGo(p *testrunv1.TestSelectionGoPackage, l suites.Layout) (roster.Suite, target, error) {
	what := "Go package " + p.GetPackageDir()
	t := target{kind: kindGo, what: what}
	switch {
	case p.GetAll() != nil:
		t.all = true
	case p.GetNamed() == nil:
		return roster.Suite{}, target{}, fmt.Errorf("%s: says neither all of its tests nor which", what)
	case len(p.GetNamed().GetTests()) == 0:
		return roster.Suite{}, target{}, fmt.Errorf("%s: names no test", what)
	}
	for _, test := range p.GetNamed().GetTests() {
		if test.GetName() == "" || strings.Contains(test.GetName(), "/") {
			return roster.Suite{}, target{}, fmt.Errorf("%s: %q is not a top-level test name", what, test.GetName())
		}
		switch {
		case test.GetWhole() != nil:
			t.names = append(t.names, test.GetName())
		case len(test.GetSubtests().GetPaths()) > 0:
			for _, path := range test.GetSubtests().GetPaths() {
				if len(path.GetSegments()) == 0 || slices.Contains(path.GetSegments(), "") {
					return roster.Suite{}, target{}, fmt.Errorf("%s: test %s names an empty subtest path", what, test.GetName())
				}
				t.names = append(t.names, test.GetName()+"/"+strings.Join(path.GetSegments(), "/"))
			}
		default:
			return roster.Suite{}, target{}, fmt.Errorf("%s: test %s says neither whole nor which subtests", what, test.GetName())
		}
	}
	s, rel, err := owningDir(l, p.GetPackageDir(), "Go package", roster.GoModule, roster.E2E)
	t.where = rel
	return s, t, err
}

// owningDir finds the suite of one of kinds whose directory holds path (a
// repository-relative path), the deepest such directory winning, and answers
// path relative to it.
func owningDir(l suites.Layout, path, what string, kinds ...roster.Kind) (roster.Suite, string, error) {
	if path == "" || filepath.IsAbs(path) {
		return roster.Suite{}, "", fmt.Errorf("%s %q: not a path relative to the repository root", what, path)
	}
	abs := filepath.Join(l.Repo, filepath.FromSlash(path))
	var best roster.Suite
	bestRel, bestLen := "", -1
	for _, s := range roster.Suites {
		if !slices.Contains(kinds, s.Kind) {
			continue
		}
		dir := roster.Resolve(l.Repo, l.Module, s.Path)
		rel, err := filepath.Rel(dir, abs)
		if err != nil || rel == ".." || strings.HasPrefix(rel, ".."+string(filepath.Separator)) {
			continue
		}
		if len(dir) > bestLen {
			best, bestRel, bestLen = s, filepath.ToSlash(rel), len(dir)
		}
	}
	if bestLen < 0 {
		return roster.Suite{}, "", fmt.Errorf("%s %s: no suite in the roster runs it", what, path)
	}
	return best, bestRel, nil
}

// owningScript finds the harness suite whose script is path. A script
// several suites run (each with its own arguments) names none of them alone,
// and is refused: the caller names the suite instead.
func owningScript(l suites.Layout, path string) (roster.Suite, error) {
	abs := filepath.Join(l.Repo, filepath.FromSlash(path))
	var owners []roster.Suite
	for _, s := range roster.Suites {
		if (s.Kind == roster.Script || s.Kind == roster.SplitScript) && roster.Resolve(l.Repo, l.Module, s.Path) == abs {
			owners = append(owners, s)
		}
	}
	switch len(owners) {
	case 0:
		return roster.Suite{}, fmt.Errorf("harness %s: no suite in the roster runs it", path)
	case 1:
		return owners[0], nil
	}
	names := make([]string, len(owners))
	for i, s := range owners {
		names[i] = s.Name
	}
	return roster.Suite{}, fmt.Errorf("harness %s: several suites run it (%s); select one by its suite", path, strings.Join(names, ", "))
}

// Narrow keeps only what the request names of one suite's units: the named
// splits, narrowed, and the atomic units and programs they depend on.
func (r Resolved) Narrow(s roster.Suite, u suites.Units) (suites.Units, error) {
	var splits []suites.Split
	var atomic []run.Spec
	for _, t := range r.targets[s.Name] {
		if t.kind == kindSuite {
			// The whole suite runs, whatever else of it was named.
			return u, nil
		}
	}
	for _, t := range r.targets[s.Name] {
		var got []suites.Split
		var err error
		switch {
		case t.kind == kindHarness && s.Kind == roster.Script:
			if !t.all {
				return suites.Units{}, fmt.Errorf("%s: has no groups to name; select all of it", t.what)
			}
			atomic = append(atomic, u.Atomic...)
			continue
		case t.kind == kindGo:
			got, err = narrowGo(s, u, t)
		default:
			got, err = narrowItems(u, t)
		}
		if err != nil {
			return suites.Units{}, err
		}
		splits = append(splits, got...)
	}
	splits = mergeGroups(splits)
	out := suites.Units{Splits: splits}
	needed := map[string]bool{}
	for _, sp := range splits {
		for _, d := range sp.Deps {
			needed[d] = true
		}
	}
	for _, a := range atomic {
		needed[a.ID] = true
	}
	// The closure over the atomic units: a kept unit's own dependencies are
	// kept too.
	for changed := true; changed; {
		changed = false
		for _, a := range u.Atomic {
			if !needed[a.ID] {
				continue
			}
			for _, d := range a.Deps {
				if !needed[d] {
					needed[d], changed = true, true
				}
			}
		}
	}
	for _, a := range u.Atomic {
		if needed[a.ID] {
			out.Atomic = append(out.Atomic, a)
		}
	}
	for _, p := range u.Programs {
		if needed[suites.ProgramUnitID(p)] {
			out.Programs = append(out.Programs, p)
		}
	}
	return out, nil
}

// narrowGo narrows a Go suite to one package's named tests.
func narrowGo(s roster.Suite, u suites.Units, t target) ([]suites.Split, error) {
	group := s.Name + ":" + t.where
	var pkg []suites.Split
	for _, sp := range u.Splits {
		if sp.Group == group || strings.HasPrefix(sp.Group, group+"[") {
			pkg = append(pkg, sp)
		}
	}
	if len(pkg) == 0 {
		return nil, fmt.Errorf("%s: suite %s holds no test in it", t.what, s.Name)
	}
	if t.all {
		return pkg, nil
	}
	whole := map[string]bool{}
	for _, n := range t.names {
		if !strings.Contains(n, "/") {
			whole[n] = true
		}
	}
	var out []suites.Split
	seen := map[string]bool{}
	for _, n := range t.names {
		top, _, _ := strings.Cut(n, "/")
		i := slices.IndexFunc(pkg, func(sp suites.Split) bool { return slices.Contains(sp.Items, top) })
		if i < 0 {
			return nil, fmt.Errorf("%s: defines no test %s", t.what, top)
		}
		sp := pkg[i]
		switch {
		case n == top:
			out = append(out, withItems(sp, sp.Group, []string{top}, nil))
		case whole[top], seen[n]:
			// The whole test runs anyway, or this path is already asked for.
		default:
			out = append(out, withItems(sp, sp.Group+"/"+n, []string{top}, []string{n}))
		}
		seen[n] = true
	}
	return out, nil
}

// narrowItems narrows a vitest, ERT or split-harness suite to one item (a file
// or a group set) and, where named, the tests within it.
func narrowItems(u suites.Units, t target) ([]suites.Split, error) {
	if t.kind == kindHarness {
		for _, sp := range u.Splits {
			if t.all {
				return []suites.Split{sp}, nil
			}
			for _, g := range t.names {
				if !slices.Contains(sp.Items, g) {
					return nil, fmt.Errorf("%s: has no group %s (its --list names %s)", t.what, g, strings.Join(sp.Items, ", "))
				}
			}
			return []suites.Split{withItems(sp, sp.Group, t.names, nil)}, nil
		}
		return nil, fmt.Errorf("%s: splits into no groups", t.what)
	}
	for _, sp := range u.Splits {
		if !slices.Contains(sp.Items, t.where) {
			continue
		}
		if t.all {
			return []suites.Split{withItems(sp, sp.Group, []string{t.where}, nil)}, nil
		}
		if sp.Named == nil {
			return nil, fmt.Errorf("%s: its suite cannot run single tests", t.what)
		}
		return []suites.Split{withItems(sp, sp.Group+"/"+t.where, []string{t.where}, t.names)}, nil
	}
	return nil, fmt.Errorf("%s: its suite does not run it", t.what)
}

// withItems is sp narrowed to items, under group, its chunks running only
// names when there are any.
func withItems(sp suites.Split, group string, items, names []string) suites.Split {
	out := sp
	out.Group = group
	out.Items = items
	if len(names) > 0 {
		named := sp.Named
		out.Chunk = func(id string, items []string) run.Spec { return named(id, items, names) }
	}
	return out
}

// mergeGroups folds narrowed splits that share a group into one, their items
// united in the order first named: each group is one chunkable piece.
func mergeGroups(splits []suites.Split) []suites.Split {
	index := map[string]int{}
	var out []suites.Split
	for _, sp := range splits {
		i, ok := index[sp.Group]
		if !ok {
			index[sp.Group] = len(out)
			out = append(out, sp)
			continue
		}
		for _, it := range sp.Items {
			if !slices.Contains(out[i].Items, it) {
				out[i].Items = append(out[i].Items, it)
			}
		}
	}
	sort.SliceStable(out, func(a, b int) bool { return out[a].Group < out[b].Group })
	return out
}
