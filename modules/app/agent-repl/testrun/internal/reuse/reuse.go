package reuse

import (
	"errors"
	"fmt"
	"path"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/suites"
	"agentrepl/testrun/roster"
)

// Mode is how a run treats pass records.
type Mode int

const (
	// Reusing skips every keyed suite whose key has a record, and records
	// every keyed suite that passes whole.
	Reusing Mode = iota
	// NoReuse (--no-reuse) runs every selected suite, and still records the
	// keyed ones that pass whole: a full execution is a pass like any other.
	NoReuse
	// Targeted is a run narrowed below whole suites (a test-name filter): it
	// neither reuses nor records, because a pass of some of a suite's tests
	// says nothing about the rest.
	Targeted
)

// Logger is the run's canonical log (run.Log).
type Logger interface {
	Infof(format string, args ...any)
	Errorf(format string, args ...any)
}

// Deps are a reuse decision's seams to the outside.
type Deps struct {
	// Repo and Module are the checkout's root and the agent-repl module root,
	// both resolved through symlinks.
	Repo, Module string
	// DenyRoots are every checkout of the repository on the host (this one
	// included), resolved: the profile refuses reads under all of them.
	DenyRoots []string
	Store     Store
	GoList    GoLister
	// Toolchain answers the versions a keyed suite of kind k is built and run
	// with.
	Toolchain func(k roster.Kind) (map[string]string, error)
	// Environ is the environment the run's units inherit.
	Environ []string
	// Self is the testrun binary, which every confined unit re-executes.
	Self string
	// ProfileDir is where the run writes its suites' profiles.
	ProfileDir string
	// Probe answers whether a seatbelt sandbox can be applied (ProbeSandbox).
	Probe func() error
	// Head answers the checkout's branch, commit and whether its files
	// differ from that commit.
	Head func() (branch, commit string, dirty bool, err error)
	Now  func() time.Time
	Log  Logger
	// Hashes caches the decision's file digests across its units (nil reads
	// every file each time).
	Hashes *HashCache
}

// maxWhyEntries bounds how many changed inputs a reason names.
const maxWhyEntries = 4

func nameEntries(names []string) string {
	if len(names) <= maxWhyEntries {
		return strings.Join(names, ", ")
	}
	return fmt.Sprintf("%s and %d more", strings.Join(names[:maxWhyEntries], ", "), len(names)-maxWhyEntries)
}

// Describe spells a roster entry for the key: every field that decides how
// the suite is built and run, its reuse declaration's contents included
// (never a pointer's address).
func Describe(s roster.Suite) string {
	r := s.Reuse
	s.Reuse = nil
	out := fmt.Sprintf("%#v", s)
	if r != nil {
		out += fmt.Sprintf(" reuse=%#v", *r)
	}
	return out
}

// SuiteInputs is a keyed suite's ENVELOPE: everything any of its units, and
// the infra units serving them, read -- its build graph, the programs its
// tests run, the runner that runs it, and every input its Reuse declares,
// each unit's included. The infra units run under its profile, a whole-suite
// unit (a script or e2e suite) is keyed on it, and the placement check places
// it.
func SuiteInputs(d Deps, s roster.Suite) (*InputSet, error) {
	if s.Reuse == nil {
		return nil, fmt.Errorf("reuse: %s declares no reuse", s.Name)
	}
	set := NewInputSet()
	switch s.Kind {
	case roster.Script, roster.SplitScript:
		// A script suite's call graph is no tool's to derive: it reads its
		// script and what its Reuse declares.
		if err := placeRosterPath(d, set, s.Path, false); err != nil {
			return nil, err
		}
	case roster.GoModule, roster.E2E:
		dir := roster.Resolve(d.Repo, d.Module, s.Path)
		tagSets := []string{""}
		if s.IntegrationTags != "" {
			tagSets = append(tagSets, s.IntegrationTags)
		}
		rel, inside, err := relInside(d.Repo, dir)
		if err != nil {
			return nil, err
		}
		if !inside {
			return nil, fmt.Errorf("reuse: %s's module %s is outside the checkout", s.Name, dir)
		}
		if err := AddPatternWalk(set, d.Repo, rel); err != nil {
			return nil, err
		}
		for _, tags := range tagSets {
			pkgs, err := d.GoList(dir, tags, true, "./...")
			if err != nil {
				return nil, err
			}
			if err := AddGoPackages(set, d.Repo, pkgs); err != nil {
				return nil, err
			}
		}
	case roster.Vitest:
		// A TypeScript package reads its whole tree (sources, tests, configs,
		// package-lock.json), the runner's vitest reporter, and what its
		// Reuse declares beyond it (the generated bindings, the shared
		// logging module); its node_modules is declared Generated.
		if err := addSuiteTree(d, set, s.Path); err != nil {
			return nil, err
		}
		if err := addRunnerDir(d, set, "testrun/vitest"); err != nil {
			return nil, err
		}
	case roster.ERT:
		// The Emacs suite reads its whole tree, the runner's ERT driver, and
		// builds the fake daemon its tests talk to (a Go program in its tree,
		// whose build graph reaches the generated Go bindings).
		if err := addSuiteTree(d, set, s.Path); err != nil {
			return nil, err
		}
		if err := addRunnerDir(d, set, "testrun/ert"); err != nil {
			return nil, err
		}
		pkgs, err := d.GoList(filepath.Join(roster.Resolve(d.Repo, d.Module, s.Path), "testsupport", "fakedaemon"), "", false, ".")
		if err != nil {
			return nil, err
		}
		if err := AddGoPackages(set, d.Repo, pkgs); err != nil {
			return nil, err
		}
	default:
		return nil, fmt.Errorf("reuse: %s is a kind of suite whose inputs this runner cannot derive", s.Name)
	}
	if err := addPrograms(d, set, s); err != nil {
		return nil, err
	}
	// The runner decides how every unit is built, split and run, so its own
	// code is an input of every keyed suite.
	runner, err := d.GoList(roster.Resolve(d.Repo, d.Module, "testrun"), "", false, ".")
	if err != nil {
		return nil, err
	}
	if err := AddGoPackages(set, d.Repo, runner); err != nil {
		return nil, err
	}
	if err := addDeclared(d, set, s.Reuse); err != nil {
		return nil, fmt.Errorf("reuse: %s: %w", s.Name, err)
	}
	for _, unit := range sortedKeys(s.Reuse.Units) {
		if err := addInputs(d, set, s.Reuse.Units[unit]); err != nil {
			return nil, fmt.Errorf("reuse: %s's unit %s: %w", s.Name, unit, err)
		}
	}
	return set, nil
}

// sortedKeys is m's keys, sorted.
func sortedKeys[V any](m map[string]V) []string {
	out := make([]string, 0, len(m))
	for k := range m {
		out = append(out, k)
	}
	sort.Strings(out)
	return out
}

// addSuiteTree places a suite's own directory (a roster path) as a hashed
// subtree.
func addSuiteTree(d Deps, set *InputSet, p string) error {
	rel, _, err := RepoPath(d, p)
	if err != nil {
		return err
	}
	return set.AddTree(rel)
}

// addRunnerDir places one of the runner's per-kind directories (its vitest
// reporter, its ERT driver) flat.
func addRunnerDir(d Deps, set *InputSet, p string) error {
	rel, _, err := RepoPath(d, p)
	if err != nil {
		return err
	}
	return set.AddFlatDir(rel)
}

// addPrograms places the build graph of every shared program the suite's
// tests run.
func addPrograms(d Deps, set *InputSet, s roster.Suite) error {
	names := map[string]bool{}
	for _, ps := range s.Programs {
		for _, p := range ps {
			names[p] = true
		}
	}
	var sorted []string
	for n := range names {
		sorted = append(sorted, n)
	}
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

// RepoPath resolves a roster path (module-relative, or repository-relative
// with a leading "/") to a repository-relative one, and whether it names a
// subtree (a trailing "/").
func RepoPath(d Deps, p string) (string, bool, error) {
	tree := strings.HasSuffix(p, "/")
	rel, err := filepath.Rel(d.Repo, roster.Resolve(d.Repo, d.Module, p))
	if err != nil {
		return "", false, err
	}
	c, err := clean(filepath.ToSlash(rel))
	if err != nil {
		return "", false, fmt.Errorf("the roster path %q: %w", p, err)
	}
	return c, tree, nil
}

// addDeclared places what r declares every unit reads: its Inputs, hashed,
// and its Generated subtrees, readable.
func addDeclared(d Deps, set *InputSet, r *roster.Reuse) error {
	if err := addInputs(d, set, r.Inputs); err != nil {
		return err
	}
	for _, p := range r.Generated {
		rel, tree, err := RepoPath(d, p)
		if err != nil {
			return err
		}
		if !tree {
			return fmt.Errorf("the generated path %q is not a subtree (end it with /)", p)
		}
		if err := set.AddGenerated(rel); err != nil {
			return err
		}
	}
	return nil
}

// addInputs places declared input paths. A module resolver (esbuild, node)
// lists a file's directory to find it, so a declared file's directory
// listing is read too, and hashed.
func addInputs(d Deps, set *InputSet, paths []string) error {
	for _, p := range paths {
		if err := placeRosterPath(d, set, p, true); err != nil {
			return err
		}
	}
	return nil
}

// placeRosterPath places one roster path in set: a subtree when it ends in
// "/", else one file, and with listDir also the file's directory listing.
func placeRosterPath(d Deps, set *InputSet, p string, listDir bool) error {
	rel, tree, err := RepoPath(d, p)
	if err != nil {
		return err
	}
	if tree {
		return set.AddTree(rel)
	}
	if listDir {
		return errors.Join(set.AddFile(rel), set.AddDir(path.Dir(rel)))
	}
	return set.AddFile(rel)
}

// Source names the checkout state a record passed on, never claiming a commit
// holds bytes it does not: a dirty checkout's record says so.
func Source(r Record) string {
	commit := r.Commit
	if len(commit) > 10 {
		commit = commit[:10]
	}
	if r.Dirty {
		return fmt.Sprintf("%s %s with uncommitted changes", r.Branch, commit)
	}
	return fmt.Sprintf("%s %s", r.Branch, commit)
}

// Runner is the run's reuse seam over the real host: Deps, and the checkout's
// file list for the placement check.
type Runner struct {
	Deps  Deps
	Files func() ([]string, error)
}

// Probe is whether this host can confine a unit (Deps.Probe), asked once
// per run: nil when it can.
func (r Runner) Probe() error { return r.Deps.Probe() }

// Decide cuts suite s, whose units the run built as u, into its reuse units
// (Derive) and decides which this run reuses (Decide). sandboxErr is the
// run's Probe answer.
func (r Runner) Decide(s roster.Suite, u suites.Units, mode Mode, sandboxErr error) (SuiteVerdict, error) {
	su, err := Derive(r.Deps, s, u, mode != Targeted)
	if err != nil {
		return SuiteVerdict{}, err
	}
	return Decide(r.Deps, su, mode, sandboxErr)
}

// Unplaced is every file of the checkout that no region places. It places
// the envelope of every keyed suite of the roster, selected or not, reusing
// the envelopes verdicts already hold.
func (r Runner) Unplaced(verdicts []SuiteVerdict) ([]string, error) {
	sets := map[string]*InputSet{}
	for _, v := range verdicts {
		if v.Envelope != nil {
			sets[v.Suite.Name] = v.Envelope
		}
	}
	for _, s := range roster.Suites {
		if s.Reuse == nil || sets[s.Name] != nil {
			continue
		}
		set, err := SuiteInputs(r.Deps, s)
		if err != nil {
			return nil, fmt.Errorf("reuse: derive %s's inputs for the placement check: %w", s.Name, err)
		}
		sets[s.Name] = set
	}
	regions, err := BuildRegions(r.Deps, roster.Suites, roster.AlwaysRunRegions, roster.ReadByNoSuite, sets)
	if err != nil {
		return nil, err
	}
	files, err := r.Files()
	if err != nil {
		return nil, err
	}
	return regions.Unplaced(files), nil
}

// Confine is Confine with r's runner binary.
func (r Runner) Confine(v SuiteVerdict, u suites.Units) suites.Units {
	return Confine(r.Deps.Self, v, u)
}

// Record is RecordOutcomes over r's Deps.
func (r Runner) Record(mode Mode, verdicts []SuiteVerdict, results []run.Result) error {
	return RecordOutcomes(r.Deps, mode, verdicts, results)
}

// staleInstall names the first generated node_modules tree of set that was
// not installed from its package's lock (NodeModulesMismatch), or "".
func staleInstall(repo string, set *InputSet) (string, error) {
	for _, g := range set.Generated() {
		if path.Base(g) != "node_modules" {
			continue
		}
		why, err := NodeModulesMismatch(repo, g)
		if err != nil {
			return "", err
		}
		if why != "" {
			return g + ": " + why, nil
		}
	}
	return "", nil
}
