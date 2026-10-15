package reuse

import (
	"fmt"
	"os"
	"path"
	"path/filepath"
	"sort"
	"strings"
	"sync"
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
}

// Verdict is one selected suite's reuse decision.
type Verdict struct {
	Suite roster.Suite
	// Keyed is a suite with a Reuse declaration that this run confines and
	// may record. A keyed suite whose sandbox is unavailable is not keyed for
	// the run.
	Keyed bool
	Key   Key
	// Set is the suite's input set (keyed suites only).
	Set *InputSet
	// Profile is the suite's profile file (keyed suites only).
	Profile string
	// Reused is the record this run reuses instead of running the suite.
	Reused *Record
	// Why says why a suite that runs is not reused.
	Why string
}

// Decide keys every selected suite and decides which a run reuses. A keyed
// suite whose inputs cannot be derived is an error, never an unkeyed run: the
// roster has drifted from the tree, and a key computed around that would be
// a guess.
func Decide(d Deps, selected []roster.Suite, mode Mode) ([]Verdict, error) {
	verdicts := make([]Verdict, len(selected))
	sandboxErr := d.Probe()
	if sandboxErr != nil {
		d.Log.Infof("reuse is off for this run, every suite runs and none is recorded: %v", sandboxErr)
	}
	errs := make([]error, len(selected))
	var wg sync.WaitGroup
	for i, s := range selected {
		verdicts[i] = Verdict{Suite: s}
		switch {
		case s.Reuse == nil:
			verdicts[i].Why = "always runs"
			continue
		case sandboxErr != nil:
			verdicts[i].Why = "reuse unavailable: no sandbox"
			continue
		}
		wg.Add(1)
		go func() {
			defer wg.Done()
			errs[i] = decideOne(d, &verdicts[i], mode)
		}()
	}
	wg.Wait()
	for _, err := range errs {
		if err != nil {
			return nil, err
		}
	}
	return verdicts, nil
}

func decideOne(d Deps, v *Verdict, mode Mode) error {
	s := v.Suite
	set, err := SuiteInputs(d, s)
	if err != nil {
		return fmt.Errorf("reuse: derive %s's inputs: %w", s.Name, err)
	}
	facts, err := suiteFacts(d, s)
	if err != nil {
		return err
	}
	key, err := ComputeKey(d.Repo, set, facts)
	if err != nil {
		return fmt.Errorf("reuse: key %s: %w", s.Name, err)
	}
	profile, err := Profile(d.Repo, d.DenyRoots, set)
	if err != nil {
		return fmt.Errorf("reuse: %s's profile: %w", s.Name, err)
	}
	path := filepath.Join(d.ProfileDir, s.Name+".sb")
	if err := os.WriteFile(path, []byte(profile), 0o644); err != nil {
		return fmt.Errorf("reuse: write %s's profile: %w", s.Name, err)
	}
	v.Keyed, v.Key, v.Set, v.Profile = true, key, set, path
	switch mode {
	case NoReuse:
		v.Why = "--no-reuse"
		return nil
	case Targeted:
		v.Why = "a targeted run neither reuses nor records"
		return nil
	}
	rec, ok, err := d.Store.Lookup(s.Name, key.Digest)
	if err != nil {
		return err
	}
	if ok {
		v.Reused = &rec
		return nil
	}
	newest, ok, err := d.Store.Newest(s.Name)
	if err != nil {
		return err
	}
	if !ok {
		v.Why = "no record"
		return nil
	}
	v.Why = "changed since its newest green record: " + nameEntries(key.Manifest.Changed(newest.Manifest))
	return nil
}

// maxWhyEntries bounds how many changed inputs a reason names.
const maxWhyEntries = 4

func nameEntries(names []string) string {
	if len(names) <= maxWhyEntries {
		return strings.Join(names, ", ")
	}
	return fmt.Sprintf("%s and %d more", strings.Join(names[:maxWhyEntries], ", "), len(names)-maxWhyEntries)
}

// suiteFacts are a keyed suite's non-file inputs.
func suiteFacts(d Deps, s roster.Suite) (Facts, error) {
	tools, err := d.Toolchain(s.Kind)
	if err != nil {
		return Facts{}, fmt.Errorf("reuse: %s's toolchain: %w", s.Name, err)
	}
	return Facts{Suite: Describe(s), Toolchain: tools, Env: KeyedEnv(d.Environ)}, nil
}

// Describe spells a roster entry for the key: every field that decides how
// the suite is built and run, its reuse declaration's contents included
// (never the pointer's address).
func Describe(s roster.Suite) string {
	r := s.Reuse
	s.Reuse = nil
	if r == nil {
		return fmt.Sprintf("%#v", s)
	}
	return fmt.Sprintf("%#v reuse=%#v", s, *r)
}

// SuiteInputs is a keyed suite's input set: its build graph, the programs its
// tests run, the runner that runs it, and its declared inputs.
func SuiteInputs(d Deps, s roster.Suite) (*InputSet, error) {
	if s.Reuse == nil {
		return nil, fmt.Errorf("reuse: %s declares no reuse", s.Name)
	}
	set := NewInputSet()
	switch s.Kind {
	case roster.GoModule:
		dir := filepath.Join(d.Module, filepath.FromSlash(s.Path))
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
	default:
		return nil, fmt.Errorf("reuse: %s is a kind of suite whose inputs this runner cannot derive yet", s.Name)
	}
	if err := addPrograms(d, set, s); err != nil {
		return nil, err
	}
	// The runner decides how every unit is built, split and run, so its own
	// code is an input of every keyed suite.
	runner, err := d.GoList(filepath.Join(d.Module, "testrun"), "", false, ".")
	if err != nil {
		return nil, err
	}
	if err := AddGoPackages(set, d.Repo, runner); err != nil {
		return nil, err
	}
	if err := addDeclared(d, set, s.Reuse); err != nil {
		return nil, fmt.Errorf("reuse: %s: %w", s.Name, err)
	}
	return set, nil
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
		pkgs, err := d.GoList(filepath.Join(d.Module, filepath.FromSlash(p.Module)), "", false, p.Package)
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
	var abs string
	if strings.HasPrefix(p, "/") {
		abs = filepath.Join(d.Repo, filepath.FromSlash(p[1:]))
	} else {
		abs = filepath.Join(d.Module, filepath.FromSlash(p))
	}
	rel, err := filepath.Rel(d.Repo, abs)
	if err != nil {
		return "", false, err
	}
	c, err := clean(filepath.ToSlash(rel))
	if err != nil {
		return "", false, fmt.Errorf("the roster path %q: %w", p, err)
	}
	return c, tree, nil
}

func addDeclared(d Deps, set *InputSet, r *roster.Reuse) error {
	for _, p := range r.Inputs {
		rel, tree, err := RepoPath(d, p)
		if err != nil {
			return err
		}
		if tree {
			err = set.AddTree(rel)
		} else {
			err = set.AddFile(rel)
		}
		if err != nil {
			return err
		}
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

// Report prints each verdict's line: a reused suite's in the shape the merge
// gate reads, a running keyed suite's reason at INFO.
func Report(log Logger, verdicts []Verdict) {
	for _, v := range verdicts {
		switch {
		case v.Reused != nil:
			log.Infof("%s: reused a green run (inputs %s, passed %s on %s)", v.Suite.Name, v.Key.Short(),
				v.Reused.PassedAt.Local().Format("2006-01-02 15:04"), Source(*v.Reused))
		case v.Keyed:
			log.Infof("%s: runs (inputs %s): %s", v.Suite.Name, v.Key.Short(), v.Why)
		}
	}
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

// Remaining is the selected suites a run still runs: every one not reused.
func Remaining(verdicts []Verdict) []roster.Suite {
	var out []roster.Suite
	for _, v := range verdicts {
		if v.Reused == nil {
			out = append(out, v.Suite)
		}
	}
	return out
}

// Confine starts every unit of each keyed suite under its profile and the
// scrubbed environment (`testrun confine`). Units of other suites are left as
// they are.
func Confine(self string, verdicts []Verdict, name string, u suites.Units) suites.Units {
	var v *Verdict
	for i := range verdicts {
		if verdicts[i].Suite.Name == name {
			v = &verdicts[i]
		}
	}
	if v == nil || !v.Keyed || v.Reused != nil {
		return u
	}
	wrap := func(sp run.Spec) run.Spec {
		sp.Argv = ConfineCommand(self, v.Profile, sp.Env, sp.Argv)
		return sp
	}
	out := suites.Units{Programs: u.Programs}
	for _, sp := range u.Atomic {
		out.Atomic = append(out.Atomic, wrap(sp))
	}
	for _, sp := range u.Splits {
		chunk := sp.Chunk
		sp.Chunk = func(id string, items []string) run.Spec { return wrap(chunk(id, items)) }
		out.Splits = append(out.Splits, sp)
	}
	return out
}

// RecordPasses stores a pass for every keyed suite of the run that passed whole,
// after re-keying it: a suite whose inputs changed while it ran (an edit in
// the checkout, a test writing into its own inputs) passed on bytes no key
// names, so it records nothing and the run says which inputs moved.
func RecordPasses(d Deps, mode Mode, verdicts []Verdict, results []run.SuiteResult, units map[string]int) error {
	if mode == Targeted {
		return nil
	}
	passed := map[string]bool{}
	for _, r := range results {
		if r.Outcome == run.Passed {
			passed[r.Name] = true
		}
	}
	var branch, commit string
	var dirty, headRead bool
	for _, v := range verdicts {
		if !v.Keyed || v.Reused != nil || !passed[v.Suite.Name] {
			continue
		}
		facts, err := suiteFacts(d, v.Suite)
		if err != nil {
			return err
		}
		after, err := ComputeKey(d.Repo, v.Set, facts)
		if err != nil {
			return fmt.Errorf("reuse: re-key %s after its run: %w", v.Suite.Name, err)
		}
		if after.Digest != v.Key.Digest {
			d.Log.Errorf("%s: not recorded, its inputs changed while it ran: %s", v.Suite.Name, nameEntries(after.Manifest.Changed(v.Key.Manifest)))
			continue
		}
		stale, err := staleInstall(d.Repo, v.Set)
		if err != nil {
			return fmt.Errorf("reuse: check %s's installed packages: %w", v.Suite.Name, err)
		}
		if stale != "" {
			d.Log.Errorf("%s: not recorded, it ran on packages its key does not name: %s", v.Suite.Name, stale)
			continue
		}
		if !headRead {
			if branch, commit, dirty, err = d.Head(); err != nil {
				return fmt.Errorf("reuse: read the checkout's HEAD: %w", err)
			}
			headRead = true
		}
		rec := Record{Suite: v.Suite.Name, Key: v.Key.Digest, PassedAt: d.Now(), Checkout: d.Repo,
			Branch: branch, Commit: commit, Dirty: dirty, Units: units[v.Suite.Name], Manifest: v.Key.Manifest}
		if err := d.Store.Put(rec); err != nil {
			return err
		}
		d.Log.Infof("%s: recorded a green run (inputs %s)", v.Suite.Name, v.Key.Short())
	}
	return nil
}

// Runner is the run's reuse seam over the real host: Deps, and the checkout's
// file list for the placement check.
type Runner struct {
	Deps  Deps
	Files func() ([]string, error)
}

// Decide is Decide over r's Deps.
func (r Runner) Decide(selected []roster.Suite, mode Mode) ([]Verdict, error) {
	return Decide(r.Deps, selected, mode)
}

// Unplaced is every file of the checkout that no region places. It derives
// the input set of every keyed suite of the roster, selected or not, reusing
// the sets verdicts already hold.
func (r Runner) Unplaced(verdicts []Verdict) ([]string, error) {
	sets := map[string]*InputSet{}
	for _, v := range verdicts {
		if v.Set != nil {
			sets[v.Suite.Name] = v.Set
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
func (r Runner) Confine(verdicts []Verdict, name string, u suites.Units) suites.Units {
	return Confine(r.Deps.Self, verdicts, name, u)
}

// Record is RecordPasses over r's Deps.
func (r Runner) Record(mode Mode, verdicts []Verdict, results []run.SuiteResult, units map[string]int) error {
	return RecordPasses(r.Deps, mode, verdicts, results, units)
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

// ReusedNames is the reused suites' names, in verdict order.
func ReusedNames(verdicts []Verdict) []string {
	var out []string
	for _, v := range verdicts {
		if v.Reused != nil {
			out = append(out, v.Suite.Name)
		}
	}
	return out
}
