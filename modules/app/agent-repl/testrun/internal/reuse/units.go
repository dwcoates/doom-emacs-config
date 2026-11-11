package reuse

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"sort"
	"strings"
	"sync"

	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/suites"
	"agentrepl/testrun/roster"
)

// REUSE IS PER UNIT. A suite is cut into the units the runner records one
// pass for (roster.Reuse): a Go package, a vitest or ERT test file, a
// suite-wide check, or a whole script suite. Each unit has its own input set
// (Derive), its own key, its own profile and its own records, so a run skips
// every unit whose inputs an earlier run passed it on, a suite whose every
// unit is reused never starts, and a unit that passed is recorded even when
// its suite as a whole failed.

// Unit is one reusable unit of a suite, before its decision.
type Unit struct {
	// Suite is the suite's name; Name the unit's within it ("" for a whole
	// suite).
	Suite, Name string
	// Specs are the split groups and atomic unit IDs whose verdicts are the
	// unit's: every one passed is the unit passed.
	Specs []string
	// Set is what the unit reads; nil for a unit that is never reused.
	Set *InputSet
	// Desc is the unit's roster facts for its key: everything of the suite's
	// entry that decides how the unit is built and run.
	Desc string
	// Never is why the unit is never reused, "" for a keyed one.
	Never string
}

// RecordName is the unit's name in the store.
func (u Unit) RecordName() string { return UnitRecordName(u.Suite, u.Name) }

// Label names the unit in a log line: "unit <name>", or the suite for a
// whole-suite unit.
func (u Unit) Label() string {
	if u.Name == "" {
		return "the whole suite"
	}
	return "unit " + u.Name
}

// UnitRecordName is unit's record name in suite: the suite's own name for a
// whole-suite unit, else the suite, "@", and the unit with each "/" spelled
// "%", so every unit's records sit in one flat directory of their own.
func UnitRecordName(suite, unit string) string {
	if unit == "" {
		return suite
	}
	return suite + "@" + strings.ReplaceAll(unit, "/", "%")
}

// SuiteUnits is one suite cut into units.
type SuiteUnits struct {
	Suite roster.Suite
	Units []Unit
	// Infra are the atomic units that serve the units rather than test
	// anything (a module build, the ERT fake daemon's build, an integration
	// bundle): each runs only while a unit that depends on it runs, under
	// the envelope, and is never recorded.
	Infra []string
	// Envelope is what the suite as a whole reads: the infra units' profile,
	// and the placement check's set.
	Envelope *InputSet
}

// WhyKind is why a unit runs rather than being reused.
type WhyKind int

const (
	// WhyNeverRecorded: no run has recorded an outcome of the unit.
	WhyNeverRecorded WhyKind = iota
	// WhyFailedBefore: its last outcome was a failure.
	WhyFailedBefore
	// WhyInputsChanged: its inputs differ from every green record's.
	WhyInputsChanged
	// WhyNoReuse: the run reuses nothing (--no-reuse, --record, coverage).
	WhyNoReuse
	// WhyTargeted: a run narrowed below whole units neither reuses nor
	// records.
	WhyTargeted
	// WhyNever: the unit, or its suite, is never reused (roster).
	WhyNever
	// WhyNoSandbox: the host cannot confine a unit, so nothing is reused.
	WhyNoSandbox
)

// Why is a running unit's reason.
type Why struct {
	Kind WhyKind
	// Changed are the inputs that differ from the record compared against,
	// sorted (WhyInputsChanged, and WhyFailedBefore after an edit).
	Changed []string
	// Detail is the never-reused reason (WhyNever).
	Detail string
}

// String spells the reason for a log line, naming one changed input.
func (w Why) String() string {
	switch w.Kind {
	case WhyNeverRecorded:
		return "never recorded"
	case WhyFailedBefore:
		if len(w.Changed) == 0 {
			return "failed before, inputs unchanged"
		}
		return "failed before, inputs changed since: " + oneChanged(w.Changed)
	case WhyInputsChanged:
		return "inputs changed since its newest green record: " + oneChanged(w.Changed)
	case WhyNoReuse:
		return "this run reuses nothing (--no-reuse, --record or --coverage)"
	case WhyTargeted:
		return "a targeted run neither reuses nor records"
	case WhyNever:
		return "never reused: " + w.Detail
	case WhyNoSandbox:
		return "reuse unavailable: no sandbox"
	}
	panic(fmt.Sprintf("reuse: no spelling for the reason kind %d", w.Kind))
}

// oneChanged names the first changed input and how many more there are.
func oneChanged(names []string) string {
	if len(names) == 0 {
		panic("reuse: a changed-inputs reason names no input")
	}
	if len(names) == 1 {
		return names[0]
	}
	return fmt.Sprintf("%s and %d more", names[0], len(names)-1)
}

// UnitVerdict is one unit's decision.
type UnitVerdict struct {
	Unit
	// Keyed is a unit this run confines to its own inputs and may record.
	Keyed   bool
	Key     Key
	Profile string
	// Reused is the record this run reuses instead of running the unit.
	Reused *Record
	// Why is why a unit that runs is not reused.
	Why Why
}

// SuiteVerdict is one suite's units decided.
type SuiteVerdict struct {
	SuiteUnits
	Units []UnitVerdict
	// Confined is a suite whose units run under profiles: a keyed suite on a
	// host that can confine.
	Confined bool
	// EnvelopeProfile is the envelope's profile file (confined suites only).
	EnvelopeProfile string
}

// AllReused reports whether every unit of the suite is reused: the suite
// does not run at all.
func (v SuiteVerdict) AllReused() bool {
	if len(v.Units) == 0 {
		return false
	}
	for _, u := range v.Units {
		if u.Reused == nil {
			return false
		}
	}
	return true
}

// Reused counts the reused units.
func (v SuiteVerdict) Reused() int {
	n := 0
	for _, u := range v.Units {
		if u.Reused != nil {
			n++
		}
	}
	return n
}

// Decide keys every unit of one suite and decides which this run reuses.
// sandboxErr is the host's ProbeSandbox answer: without a sandbox nothing is
// confined, so nothing is reused or recorded.
func Decide(d Deps, su SuiteUnits, mode Mode, sandboxErr error) (SuiteVerdict, error) {
	v := SuiteVerdict{SuiteUnits: su, Units: make([]UnitVerdict, len(su.Units))}
	for i, u := range su.Units {
		v.Units[i] = UnitVerdict{Unit: u}
	}
	s := su.Suite
	switch {
	case s.Reuse == nil:
		for i := range v.Units {
			v.Units[i].Why = Why{Kind: WhyNever, Detail: s.NeverReused}
		}
		return v, nil
	case sandboxErr != nil:
		for i := range v.Units {
			v.Units[i].Why = Why{Kind: WhyNoSandbox}
		}
		return v, nil
	case mode == Targeted:
		// A TARGETED RUN IS NEVER CONFINED: it runs some tests of a unit,
		// never the unit, so it neither reuses nor records, and a declaration
		// is proved by the untargeted run that records the unit.
		for i := range v.Units {
			v.Units[i].Why = Why{Kind: WhyTargeted}
		}
		return v, nil
	}
	profile, err := writeProfile(d, s.Name+".envelope", su.Envelope)
	if err != nil {
		return SuiteVerdict{}, err
	}
	v.Confined, v.EnvelopeProfile = true, profile
	tools, err := d.Toolchain(s.Kind)
	if err != nil {
		return SuiteVerdict{}, fmt.Errorf("reuse: %s's toolchain: %w", s.Name, err)
	}
	errs := make([]error, len(v.Units))
	var wg sync.WaitGroup
	for i := range v.Units {
		if v.Units[i].Never != "" {
			v.Units[i].Why = Why{Kind: WhyNever, Detail: v.Units[i].Never}
			continue
		}
		wg.Add(1)
		go func() {
			defer wg.Done()
			errs[i] = decideUnit(d, &v.Units[i], mode, tools)
		}()
	}
	wg.Wait()
	for _, err := range errs {
		if err != nil {
			return SuiteVerdict{}, err
		}
	}
	return v, nil
}

// unitFacts are a unit's non-file inputs.
func unitFacts(d Deps, u Unit, tools map[string]string) Facts {
	return Facts{Suite: u.Desc, Toolchain: tools, Env: KeyedEnv(d.Environ)}
}

func decideUnit(d Deps, v *UnitVerdict, mode Mode, tools map[string]string) error {
	dec, err := decideKeyed(d, v.RecordName(), v.Set, unitFacts(d, v.Unit, tools), mode)
	if err != nil {
		return err
	}
	v.Keyed, v.Key, v.Profile, v.Reused, v.Why = true, dec.Key, dec.Profile, dec.Reused, dec.Why
	return nil
}

// writeProfile writes set's profile into the run's profile directory, as
// "<name>.sb".
func writeProfile(d Deps, name string, set *InputSet) (string, error) {
	profile, err := Profile(d.Repo, d.DenyRoots, set)
	if err != nil {
		return "", fmt.Errorf("reuse: %s's profile: %w", name, err)
	}
	p := filepath.Join(d.ProfileDir, name+".sb")
	if err := os.WriteFile(p, []byte(profile), 0o644); err != nil {
		return "", fmt.Errorf("reuse: write %s's profile: %w", name, err)
	}
	return p, nil
}

// CombinedKey is a wholly reused suite's key: one digest over its units'
// record names and keys, so the gate's line names one key for the suite.
func (v SuiteVerdict) CombinedKey() Key {
	lines := make([]string, 0, len(v.Units))
	for _, u := range v.Units {
		lines = append(lines, u.RecordName()+"="+u.Key.Digest)
	}
	sort.Strings(lines)
	sum := sha256.Sum256([]byte(strings.Join(lines, "\n")))
	return Key{Digest: hex.EncodeToString(sum[:])}
}

// newestReused is the most recently passed of the reused units' records.
func (v SuiteVerdict) newestReused() Record {
	var newest *Record
	for _, u := range v.Units {
		if u.Reused != nil && (newest == nil || u.Reused.PassedAt.After(newest.PassedAt)) {
			newest = u.Reused
		}
	}
	if newest == nil {
		panic(fmt.Sprintf("reuse: %s has no reused unit", v.Suite.Name))
	}
	return *newest
}

// Report prints one suite's decision through the run's log. A wholly reused
// suite gets the line the merge gate reads (daemon/internal/merge/testgate.go
// suiteReused), naming its newest record; a suite that runs gets its tally
// and one line per running unit, naming why it runs.
func Report(log Logger, v SuiteVerdict) {
	name := v.Suite.Name
	if v.AllReused() {
		r := v.newestReused()
		log.Infof("%s: reused a green run (%s)", name, passedClause(v.CombinedKey(), r))
		log.Infof("%s: reuse: all %d units reused", name, len(v.Units))
		return
	}
	reused := v.Reused()
	log.Infof("%s: reuse: %d units, %d reused, %d run (%s)", name, len(v.Units), reused, len(v.Units)-reused, whyTallyOf([]SuiteVerdict{v}))
	for _, u := range v.Units {
		if u.Reused != nil {
			continue
		}
		if u.Keyed {
			log.Infof("%s: %s runs (inputs %s): %s", name, u.Label(), u.Key.Short(), u.Why)
			continue
		}
		log.Infof("%s: %s runs: %s", name, u.Label(), u.Why)
	}
}

// whyOrder is the order a tally names the reasons in.
var whyOrder = []WhyKind{WhyFailedBefore, WhyInputsChanged, WhyNeverRecorded, WhyNoReuse, WhyTargeted, WhyNever, WhyNoSandbox}

// whyTallyOf counts the running units of verdicts by reason: "2 failed
// before, 1 inputs changed", or "none" when every unit is reused.
func whyTallyOf(verdicts []SuiteVerdict) string {
	counts := map[WhyKind]int{}
	for _, v := range verdicts {
		for _, u := range v.Units {
			if u.Reused == nil {
				counts[u.Why.Kind]++
			}
		}
	}
	var why []string
	for _, k := range whyOrder {
		if counts[k] > 0 {
			why = append(why, fmt.Sprintf("%d %s", counts[k], whyTally[k]))
		}
	}
	if len(why) == 0 {
		return "none"
	}
	return strings.Join(why, ", ")
}

// whyTally names each reason in a suite's tally.
var whyTally = map[WhyKind]string{
	WhyFailedBefore:  "failed before",
	WhyInputsChanged: "inputs changed",
	WhyNeverRecorded: "never recorded",
	WhyNoReuse:       "not reusing",
	WhyTargeted:      "targeted",
	WhyNever:         "never reused",
	WhyNoSandbox:     "no sandbox",
}

// unitOfSpec is the unit index owning each of su's specs, and whether each
// spec is infra. Every spec of u is owned by exactly one unit or is infra:
// anything else is a deriver's bug.
func specOwners(su SuiteUnits, u suites.Units) (map[string]int, map[string]bool) {
	owner := map[string]int{}
	for i, unit := range su.Units {
		for _, id := range unit.Specs {
			if j, dup := owner[id]; dup {
				panic(fmt.Sprintf("reuse: %s's %s belongs to both %s and %s", su.Suite.Name, id, su.Units[j].Label(), unit.Label()))
			}
			owner[id] = i
		}
	}
	infra := map[string]bool{}
	for _, id := range su.Infra {
		if _, both := owner[id]; both {
			panic(fmt.Sprintf("reuse: %s's %s is both infra and a unit's", su.Suite.Name, id))
		}
		infra[id] = true
	}
	check := func(id string) {
		if _, ok := owner[id]; !ok && !infra[id] {
			panic(fmt.Sprintf("reuse: %s's %s belongs to no unit and is no infra", su.Suite.Name, id))
		}
	}
	for _, a := range u.Atomic {
		check(a.ID)
	}
	for _, sp := range u.Splits {
		check(sp.Group)
	}
	return owner, infra
}

// SkipReused drops every reused unit's specs from u, then every infra unit no
// remaining spec depends on, and places u's module builds over the packages
// that remain (suites.Units.Finish).
func SkipReused(v SuiteVerdict, u suites.Units) suites.Units {
	owner, infra := specOwners(v.SuiteUnits, u)
	reused := func(id string) bool {
		i, ok := owner[id]
		return ok && v.Units[i].Reused != nil
	}
	var splits []suites.Split
	for _, sp := range u.Splits {
		if !reused(sp.Group) {
			splits = append(splits, sp)
		}
	}
	var kept []run.Spec
	for _, a := range u.Atomic {
		if !reused(a.ID) {
			kept = append(kept, a)
		}
	}
	// An infra unit runs only for the specs that remain: the dependency
	// closure from them.
	byID := map[string]run.Spec{}
	for _, a := range kept {
		byID[a.ID] = a
	}
	needed := map[string]bool{}
	var walk func(deps []string)
	walk = func(deps []string) {
		for _, dep := range deps {
			if needed[dep] {
				continue
			}
			needed[dep] = true
			if a, ok := byID[dep]; ok {
				walk(a.Deps)
			}
		}
	}
	for _, sp := range splits {
		walk(sp.Deps)
	}
	for _, a := range kept {
		if !infra[a.ID] {
			walk(a.Deps)
		}
	}
	var atomic []run.Spec
	for _, a := range kept {
		if !infra[a.ID] || needed[a.ID] {
			atomic = append(atomic, a)
		}
	}
	u.Splits, u.Atomic = splits, atomic
	return u.Finish()
}

// Confine starts every spec of a confined suite under its profile and the
// scrubbed environment (`testrun confine`): a keyed unit's specs under the
// unit's own profile, an infra unit (whose output the keyed units run) under
// the envelope's. A never-reused unit's specs run unconfined: it is never
// recorded, so nothing rests on what it reads, and some cannot run inside a
// sandbox at all (a test that applies profiles itself). An unconfined suite's
// units are left as they are.
//
// CONFINE COMES LAST: its Units carry no GoBuilds, so no later Finish can
// place an unconfined build.
func Confine(self string, v SuiteVerdict, u suites.Units) suites.Units {
	if !v.Confined {
		return u
	}
	owner, infra := specOwners(v.SuiteUnits, u)
	profileOf := func(id string) string {
		if i, ok := owner[id]; ok {
			return v.Units[i].Profile
		}
		if !infra[id] {
			panic(fmt.Sprintf("reuse: %s's %s belongs to no unit and is no infra", v.Suite.Name, id))
		}
		return v.EnvelopeProfile
	}
	wrap := func(profile string, sp run.Spec) run.Spec {
		if profile == "" {
			return sp
		}
		sp.Argv = ConfineCommand(self, profile, sp.Env, sp.Argv)
		return sp
	}
	out := suites.Units{Programs: u.Programs}
	for _, sp := range u.Atomic {
		out.Atomic = append(out.Atomic, wrap(profileOf(sp.ID), sp))
	}
	for _, sp := range u.Splits {
		chunk, profile := sp.Chunk, profileOf(sp.Group)
		sp.Chunk = func(id string, items []string) run.Spec { return wrap(profile, chunk(id, items)) }
		if named := sp.Named; named != nil {
			sp.Named = func(id string, items, names []string) run.Spec { return wrap(profile, named(id, items, names)) }
		}
		out.Splits = append(out.Splits, sp)
	}
	return out
}

// SpecGroup is the split group or atomic unit a run result belongs to: a
// chunk's "<group>#NN", or the unit's own ID.
func SpecGroup(id string) string {
	if i := strings.LastIndex(id, "#"); i >= 0 {
		return id[:i]
	}
	return id
}

// Tally is what one run of a suite's units did, for the run's summary.
type Tally struct {
	Reused, Ran int
}

// RecordOutcomes stores, for every keyed unit of the run that ran, its outcome: a
// pass record when every one of its specs passed, and its last outcome
// either way, so a later run can say it failed before. A unit that passed is
// recorded whatever its suite's other units did. Each is re-keyed first: a
// unit whose inputs changed while it ran passed on bytes no key names, so it
// records nothing and the run says which input moved.
func RecordOutcomes(d Deps, mode Mode, verdicts []SuiteVerdict, results []run.Result) error {
	if mode == Targeted {
		return nil
	}
	ran, failed, cancelled := map[string]int{}, map[string]bool{}, map[string]bool{}
	for _, r := range results {
		g := SpecGroup(r.Spec.ID)
		switch r.Outcome {
		case run.Passed:
			ran[g]++
		case run.Failed:
			ran[g]++
			failed[g] = true
		case run.Cancelled:
			cancelled[g] = true
		default:
			panic(fmt.Sprintf("reuse: %s ended with an outcome %d this runner does not know", r.Spec.ID, r.Outcome))
		}
	}
	rec := newRecorder(d)
	tools := map[roster.Kind]map[string]string{}
	for _, sv := range verdicts {
		recorded, failures := 0, 0
		for _, v := range sv.Units {
			if !v.Keyed || v.Reused != nil {
				continue
			}
			n, bad, skipped := 0, false, false
			for _, id := range v.Specs {
				n += ran[id]
				bad = bad || failed[id]
				skipped = skipped || cancelled[id]
			}
			if n == 0 || (skipped && !bad) {
				// Some of its specs never ran (a dependency failed, the run
				// was interrupted) and none failed: no outcome of the unit.
				continue
			}
			if tools[sv.Suite.Kind] == nil {
				t, err := d.Toolchain(sv.Suite.Kind)
				if err != nil {
					return fmt.Errorf("reuse: %s's toolchain: %w", sv.Suite.Name, err)
				}
				tools[sv.Suite.Kind] = t
			}
			ok, err := rec.record(v.RecordName(), sv.Suite.Name+": "+v.Label(), v.Set, unitFacts(d, v.Unit, tools[sv.Suite.Kind]), v.Key, n, bad)
			if err != nil {
				return err
			}
			switch {
			case ok && bad:
				failures++
			case ok:
				recorded++
			}
		}
		if recorded > 0 || failures > 0 {
			d.Log.Infof("%s: reuse: recorded %d passed and %d failed units", sv.Suite.Name, recorded, failures)
		}
		if failures > 0 {
			d.Log.Infof("%s ran confined to each unit's own inputs: %s (testrun/roster; the /agent-repl-tests skill, \"Every suite declares its inputs; reuse is per unit\")", sv.Suite.Name, confinedRefusal("Reuse"))
		}
	}
	return nil
}

// Summarize prints the run's reuse tally: how many units every selected
// suite was cut into, how many were reused, how many ran, and why they ran.
func Summarize(log Logger, verdicts []SuiteVerdict) {
	total, reused := 0, 0
	for _, v := range verdicts {
		total += len(v.Units)
		reused += v.Reused()
	}
	whole := reusedOnes(verdicts, func(v SuiteVerdict) (string, bool) { return v.Suite.Name, v.AllReused() })
	log.Infof("reuse: %d units, %d reused, %d run (%s)", total, reused, total-reused, whyTallyOf(verdicts))
	if len(whole) > 0 {
		slices.Sort(whole)
		log.Infof("reused an earlier green run of every unit, NOT run: %s (--no-reuse runs them)", strings.Join(whole, " "))
	}
}
