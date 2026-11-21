package selection

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strings"

	testrunv1 "agentrepl/proto/testrun/v1"

	"agentrepl/testrun/internal/suites"
	"agentrepl/testrun/roster"
)

// A RERUN NEVER FAILS ON A VANISHED NAME (TestRunRequest.drop_missing). The
// daemon reruns a workspace's recorded cases, and a case renamed, moved or
// deleted since it was recorded is no case of the tree any more: it is
// dropped and reported, never run as a refusal. Whether the tree holds a case
// is the runner's own listing as it plans the very run that follows
// (suites.Lister), so no listing can go stale before the run it judges.

// Lister lists the cases the tree holds (suites.Lister in a real run).
type Lister interface {
	// GoTests answers the top-level tests of the Go package in DIR, under no
	// tag and under TAGS.
	GoTests(dir, tags string) ([]string, error)
	// VitestNames answers, by file relative to DIR, the full names vitest
	// collects in FILES under CONFIG.
	VitestNames(module, dir, config string, files []string) (map[string][]string, error)
	// ERTNames answers, by file, the tests each of FILES in LISPDIR defines.
	ERTNames(module, lispDir string, files []string) (map[string][]string, error)
	// HarnessGroups answers the groups split harness suite S lists.
	HarnessGroups(l suites.Layout, s roster.Suite) ([]string, error)
}

// Dropped is one case a rerun dropped.
type Dropped struct {
	// Selection is the request's selection, as it was sent.
	Selection *testrunv1.TestSelection
	// Detail says why, for a person.
	Detail string
}

// ErrNotOneCase is a rerun's selection that names more than one case, or
// none: the daemon sends each recorded case as its own selection, so one
// that is not is the daemon's defect, refused rather than half dropped.
var ErrNotOneCase = errors.New("a rerun's selection names exactly one case")

// Prune answers REQ, a rerun, with every case the tree no longer holds
// dropped, and the dropped cases. An error wraps ErrNotOneCase for a
// selection the request should never have carried, and is otherwise a
// listing that failed: never a reason to drop a case or to run it.
func Prune(req *testrunv1.TestRunRequest, l suites.Layout, list Lister) (*testrunv1.TestRunRequest, []Dropped, error) {
	p := pruning{l: l, list: list, vitest: map[vitestKey][]string{}, ert: map[string][]string{}}
	for i, sel := range req.GetSelections() {
		if err := oneCase(sel); err != nil {
			return nil, nil, fmt.Errorf("selection %d (%s): %w", i+1, Describe(&testrunv1.TestRunRequest{Selections: []*testrunv1.TestSelection{sel}}), err)
		}
	}
	verdicts := make([]string, len(req.GetSelections()))
	pending := make([]func() string, len(req.GetSelections()))
	for i, sel := range req.GetSelections() {
		detail, later, err := p.locate(sel)
		if err != nil {
			return nil, nil, err
		}
		verdicts[i], pending[i] = detail, later
	}
	if err := p.listNames(); err != nil {
		return nil, nil, err
	}
	kept := cloneOptions(req)
	var dropped []Dropped
	for i, sel := range req.GetSelections() {
		detail := verdicts[i]
		if detail == "" && pending[i] != nil {
			detail = pending[i]()
		}
		if detail != "" {
			dropped = append(dropped, Dropped{Selection: sel, Detail: detail})
			continue
		}
		kept.Selections = append(kept.Selections, sel)
	}
	return kept, dropped, nil
}

// cloneOptions answers REQ's options with no selection.
func cloneOptions(req *testrunv1.TestRunRequest) *testrunv1.TestRunRequest {
	return &testrunv1.TestRunRequest{
		Coverage: req.GetCoverage(), Record: req.GetRecord(), Remeasure: req.GetRemeasure(), NoReuse: req.GetNoReuse(),
		PlanOnly: req.GetPlanOnly(), DebugInfo: req.GetDebugInfo(), DropMissing: req.GetDropMissing(),
	}
}

// oneCase refuses a selection that names no case or several, and a Go
// subtest path, which no listing holds.
func oneCase(sel *testrunv1.TestSelection) error {
	count := 0
	switch t := sel.GetTarget().(type) {
	case *testrunv1.TestSelection_GoPackage:
		if t.GoPackage.GetAll() != nil {
			return nil
		}
		tests := t.GoPackage.GetNamed().GetTests()
		if len(tests) == 1 && tests[0].GetWhole() == nil {
			return fmt.Errorf("%w: a Go case is a top-level test, never a subtest path", ErrNotOneCase)
		}
		count = len(tests)
	case *testrunv1.TestSelection_VitestFile:
		if t.VitestFile.GetAll() != nil {
			return nil
		}
		count = len(t.VitestFile.GetNamed().GetFullNames())
	case *testrunv1.TestSelection_ErtFile:
		if t.ErtFile.GetAll() != nil {
			return nil
		}
		count = len(t.ErtFile.GetNamed().GetTestNames())
	case *testrunv1.TestSelection_Harness:
		if t.Harness.GetAll() != nil {
			return nil
		}
		count = len(t.Harness.GetNamed().GetNames())
	case *testrunv1.TestSelection_Suite:
		return nil
	case *testrunv1.TestSelection_EverySuite:
		return fmt.Errorf("%w: every_suite is the merge queue's gate, never a case", ErrNotOneCase)
	default:
		return fmt.Errorf("%w: it names no target", ErrNotOneCase)
	}
	if count != 1 {
		return fmt.Errorf("%w, and it names %d", ErrNotOneCase, count)
	}
	return nil
}

// vitestKey is one vitest package to list: its directory and its suite's
// integration config ("" for none).
type vitestKey struct{ dir, config string }

// pruning is one rerun's existence check: the names each vitest package and
// the ERT suite must list, gathered so each is listed once.
type pruning struct {
	l    suites.Layout
	list Lister
	// vitest and ert are the files each listing lists; vitestNames and
	// ertNames what it answered.
	vitest      map[vitestKey][]string
	ert         map[string][]string
	vitestNames map[vitestKey]map[string][]string
	ertNames    map[string]map[string][]string
}

// locate judges SEL as far as its place on disk and the roster: a detail
// when the tree no longer holds it, or a judgment to make once the names
// are listed.
func (p *pruning) locate(sel *testrunv1.TestSelection) (string, func() string, error) {
	switch t := sel.GetTarget().(type) {
	case *testrunv1.TestSelection_Suite:
		if _, known := roster.Lookup(t.Suite.GetName()); !known {
			return fmt.Sprintf("suite %s: the roster has no such suite any more", t.Suite.GetName()), nil, nil
		}
		return "", nil, nil
	case *testrunv1.TestSelection_GoPackage:
		return p.locateGo(t.GoPackage)
	case *testrunv1.TestSelection_VitestFile:
		return p.locateVitest(t.VitestFile)
	case *testrunv1.TestSelection_ErtFile:
		return p.locateERT(t.ErtFile)
	case *testrunv1.TestSelection_Harness:
		return p.locateHarness(t.Harness)
	}
	panic(fmt.Sprintf("selection: a rerun's selection passed oneCase with target %T", sel.GetTarget()))
}

// gone answers WHAT's detail when PATH (relative to the repository) is not
// on disk, "" when it is.
func (p *pruning) gone(what, path string) string {
	if _, err := os.Stat(filepath.Join(p.l.Repo, filepath.FromSlash(path))); err != nil {
		return fmt.Sprintf("%s: it is no longer in the tree", what)
	}
	return ""
}

func (p *pruning) locateGo(g *testrunv1.TestSelectionGoPackage) (string, func() string, error) {
	what := "Go package " + g.GetPackageDir()
	if detail := p.gone(what, g.GetPackageDir()); detail != "" {
		return detail, nil, nil
	}
	s, rel, err := owningDir(p.l, g.GetPackageDir(), "Go package", roster.GoModule, roster.E2E)
	if err != nil {
		return err.Error(), nil, nil
	}
	dir := filepath.Join(roster.Resolve(p.l.Repo, p.l.Module, s.Path), filepath.FromSlash(rel))
	tests, err := p.list.GoTests(dir, s.IntegrationTags)
	if err != nil {
		return "", nil, fmt.Errorf("list the tests of %s: %w", what, err)
	}
	if g.GetAll() != nil {
		if len(tests) == 0 {
			return what + ": it holds no test any more", nil, nil
		}
		return "", nil, nil
	}
	name := g.GetNamed().GetTests()[0].GetName()
	if !slices.Contains(tests, name) {
		return fmt.Sprintf("%s: it no longer defines %s", what, name), nil, nil
	}
	return "", nil, nil
}

func (p *pruning) locateVitest(f *testrunv1.TestSelectionVitestFile) (string, func() string, error) {
	what := "vitest file " + f.GetFile()
	if detail := p.gone(what, f.GetFile()); detail != "" {
		return detail, nil, nil
	}
	s, rel, err := owningDir(p.l, f.GetFile(), "vitest file", roster.Vitest)
	if err != nil {
		return err.Error(), nil, nil
	}
	key := vitestKey{dir: roster.Resolve(p.l.Repo, p.l.Module, s.Path), config: s.IntegrationConfig}
	if !slices.Contains(p.vitest[key], rel) {
		p.vitest[key] = append(p.vitest[key], rel)
	}
	return "", func() string {
		names, collected := p.vitestNames[key][rel]
		switch {
		case !collected:
			return what + ": its suite collects no test in it any more"
		case f.GetAll() != nil:
			return ""
		}
		want := f.GetNamed().GetFullNames()[0]
		// A -t name matches a test's full name or a describe block's
		// (suites.VitestNamePattern).
		if slices.ContainsFunc(names, func(n string) bool { return n == want || strings.HasPrefix(n, want+" ") }) {
			return ""
		}
		return fmt.Sprintf("%s: it no longer defines %q", what, want)
	}, nil
}

func (p *pruning) locateERT(f *testrunv1.TestSelectionErtFile) (string, func() string, error) {
	what := "ERT file " + f.GetFile()
	path, err := ertPath(p.l, f.GetFile())
	if err != nil {
		return "", nil, err
	}
	if detail := p.gone(what, path); detail != "" {
		return detail, nil, nil
	}
	s, rel, err := owningDir(p.l, path, "ERT file", roster.ERT)
	if err != nil {
		return err.Error(), nil, nil
	}
	lispDir := roster.Resolve(p.l.Repo, p.l.Module, s.Path)
	files, err := suites.ERTRoster(lispDir)
	if err != nil {
		return "", nil, fmt.Errorf("list the ERT roster for %s: %w", what, err)
	}
	if !slices.Contains(files, rel) {
		return what + ": the ERT roster no longer loads it", nil, nil
	}
	if f.GetAll() != nil {
		return "", nil, nil
	}
	if !slices.Contains(p.ert[lispDir], rel) {
		p.ert[lispDir] = append(p.ert[lispDir], rel)
	}
	return "", func() string {
		want := f.GetNamed().GetTestNames()[0]
		if slices.Contains(p.ertNames[lispDir][rel], want) {
			return ""
		}
		return fmt.Sprintf("%s: it no longer defines %s", what, want)
	}, nil
}

func (p *pruning) locateHarness(h *testrunv1.TestSelectionHarness) (string, func() string, error) {
	what := "harness " + h.GetScript()
	if detail := p.gone(what, h.GetScript()); detail != "" {
		return detail, nil, nil
	}
	s, err := owningScript(p.l, h.GetScript())
	if err != nil {
		return err.Error(), nil, nil
	}
	if h.GetAll() != nil {
		return "", nil, nil
	}
	group := h.GetNamed().GetNames()[0]
	if s.Kind != roster.SplitScript {
		return fmt.Sprintf("%s: it no longer lists groups, so it has no group %s", what, group), nil, nil
	}
	groups, err := p.list.HarnessGroups(p.l, s)
	if err != nil {
		return "", nil, fmt.Errorf("list the groups of %s: %w", what, err)
	}
	if !slices.Contains(groups, group) {
		return fmt.Sprintf("%s: it no longer lists the group %s", what, group), nil, nil
	}
	return "", nil, nil
}

// listNames lists every vitest package's and ERT suite's names the
// selections need, each once.
func (p *pruning) listNames() error {
	p.vitestNames = map[vitestKey]map[string][]string{}
	for key, files := range p.vitest {
		// A package's files are its default config's and, where the suite
		// has one, its integration config's: a file is collected by one.
		configs := []string{""}
		if key.config != "" {
			configs = append(configs, key.config)
		}
		names := map[string][]string{}
		for _, config := range configs {
			got, err := p.list.VitestNames(p.l.Module, key.dir, config, files)
			if err != nil {
				return fmt.Errorf("list the vitest tests of %s: %w", key.dir, err)
			}
			for file, n := range got {
				names[file] = append(names[file], n...)
			}
		}
		p.vitestNames[key] = names
	}
	p.ertNames = map[string]map[string][]string{}
	for lispDir, files := range p.ert {
		names, err := p.list.ERTNames(p.l.Module, lispDir, files)
		if err != nil {
			return fmt.Errorf("list the ERT tests of %s: %w", lispDir, err)
		}
		p.ertNames[lispDir] = names
	}
	return nil
}
