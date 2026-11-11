package reuse

import (
	"errors"
	"fmt"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/suites"
	"agentrepl/testrun/roster"
)

type logLines struct{ info, errs []string }

func (l *logLines) Infof(f string, a ...any)  { l.info = append(l.info, fmt.Sprintf(f, a...)) }
func (l *logLines) Errorf(f string, a ...any) { l.errs = append(l.errs, fmt.Sprintf(f, a...)) }

// fixture is a checkout holding a script suite, "harness" (one whole-suite
// unit, its script bin/test-x.sh), and a Go suite, "lock" (the module lock/,
// one tested package and a vet), in the module at modules/app/agent-repl.
type fixture struct {
	d   Deps
	log *logLines
}

const fixtureModule = "modules/app/agent-repl"

func newFixture(t *testing.T) fixture {
	t.Helper()
	root := writeTree(t, map[string]string{
		fixtureModule + "/lock/go.mod":      "module lock",
		fixtureModule + "/lock/lock.go":     "package lock",
		fixtureModule + "/testrun/go.mod":   "module testrun",
		fixtureModule + "/testrun/main.go":  "package main",
		fixtureModule + "/prompts/p.md":     "brief",
		fixtureModule + "/bin/test-x.sh":    "#!/bin/sh",
		fixtureModule + "/bin/lib-x.sh":     "x() { :; }",
		fixtureModule + "/vocab/words.json": "{}",
	})
	module := filepath.Join(root, filepath.FromSlash(fixtureModule))
	log := &logLines{}
	d := Deps{
		Repo: root, Module: module, DenyRoots: []string{root},
		Store:      Store{Dir: t.TempDir()},
		GoList:     fakeGoList,
		Toolchain:  func(roster.Kind) (map[string]string, error) { return map[string]string{"go": "go1.24"}, nil },
		Environ:    []string{"PATH=/bin", "TMPDIR=/t"},
		Self:       "/bin/testrun",
		ProfileDir: t.TempDir(),
		Probe:      func() error { return nil },
		Head:       func() (string, string, bool, error) { return "feature", "c0ffee", false, nil },
		Now:        func() time.Time { return time.Date(2026, 10, 14, 12, 0, 0, 0, time.UTC) },
		Log:        log,
		Hashes:     NewHashCache(),
	}
	return fixture{d: d, log: log}
}

// fakeGoList answers a listing of dir as one package named for dir's base,
// and, for a test listing, its test binary linking it.
func fakeGoList(dir, _ string, tests bool, _ ...string) ([]GoPackage, error) {
	name := filepath.Base(dir)
	mod := &GoModule{GoMod: filepath.Join(dir, "go.mod")}
	pkgs := []GoPackage{{ImportPath: name, Dir: dir, Module: mod}}
	if tests {
		pkgs = append(pkgs, GoPackage{ImportPath: name + ".test", Dir: dir, Module: mod, Deps: []string{name}})
	}
	return pkgs, nil
}

var harnessSuite = roster.Suite{Name: "harness", Kind: roster.Script, Path: "bin/test-x.sh", Reuse: &roster.Reuse{Inputs: []string{"bin/lib-x.sh"}}}
var lockSuite = roster.Suite{Name: "lock", Kind: roster.GoModule, Path: "lock", Reuse: &roster.Reuse{Units: map[string][]string{".": {"prompts/"}}}}

// harnessUnits is a script suite's built units: its one script.
func harnessUnits() suites.Units {
	return suites.Units{Atomic: []run.Spec{spec("harness")}}
}

// lockUnits is the lock module's built units: its root package's split, its
// vet and its module build.
func lockUnits() suites.Units {
	return suites.Units{
		Atomic: []run.Spec{spec("lock:vet"), spec("lock:build")},
		Splits: []suites.Split{{Group: "lock:.", Suite: "lock", Package: ".", Chunk: func(id string, _ []string) run.Spec { return spec(id) }}},
	}
}

func spec(id string) run.Spec {
	sp := run.Spec{Argv: []string{"go", "test"}, Env: []string{"GOMAXPROCS=2"}}
	sp.ID = id
	return sp
}

func (f fixture) decide(t *testing.T, mode Mode, s roster.Suite, u suites.Units) SuiteVerdict {
	t.Helper()
	su, err := Derive(f.d, s, u, mode != Targeted)
	if err != nil {
		t.Fatal(err)
	}
	v, err := Decide(f.d, su, mode, f.d.Probe())
	if err != nil {
		t.Fatal(err)
	}
	return v
}

// outcome runs every spec of verdicts' units with outcome o.
func outcome(verdicts []SuiteVerdict, o run.Outcome) []run.Result {
	var out []run.Result
	for _, v := range verdicts {
		for _, u := range v.Units {
			for _, id := range u.Specs {
				r := run.Result{Outcome: o}
				r.Spec.ID = id + "#00"
				out = append(out, r)
			}
		}
	}
	return out
}

func (f fixture) record(t *testing.T, mode Mode, o run.Outcome, verdicts ...SuiteVerdict) {
	t.Helper()
	if err := RecordOutcomes(f.d, mode, verdicts, outcome(verdicts, o)); err != nil {
		t.Fatal(err)
	}
}

func TestDecide(t *testing.T) {
	tests := []struct {
		name string
		// before is the outcome an earlier run recorded, nil for none.
		before *run.Outcome
		edit   bool
		mode   Mode
		suite  roster.Suite
		probe  error
		keyed  bool
		reused bool
		want   WhyKind
		// changed is the input the reason names, if any.
		changed string
	}{
		{name: "a unit with no record runs", mode: Reusing, suite: harnessSuite, keyed: true, want: WhyNeverRecorded},
		{name: "a passed unit is reused on the same inputs", before: ptr(run.Passed), mode: Reusing, suite: harnessSuite, keyed: true, reused: true},
		{name: "a changed input runs it and says which", before: ptr(run.Passed), edit: true, mode: Reusing, suite: harnessSuite, keyed: true,
			want: WhyInputsChanged, changed: fixtureModule + "/bin/lib-x.sh"},
		{name: "a failed unit runs again", before: ptr(run.Failed), mode: Reusing, suite: harnessSuite, keyed: true, want: WhyFailedBefore},
		{name: "a failed unit that was edited says what changed", before: ptr(run.Failed), edit: true, mode: Reusing, suite: harnessSuite, keyed: true,
			want: WhyFailedBefore, changed: fixtureModule + "/bin/lib-x.sh"},
		{name: "--no-reuse runs a recorded unit", before: ptr(run.Passed), mode: NoReuse, suite: harnessSuite, keyed: true, want: WhyNoReuse},
		{name: "a targeted run runs a recorded unit", before: ptr(run.Passed), mode: Targeted, suite: harnessSuite, keyed: true, want: WhyTargeted},
		{name: "a never-reused suite runs unkeyed", mode: Reusing, suite: roster.Suite{Name: "harness", Kind: roster.Script, Path: "bin/test-x.sh", NeverReused: "it measures"},
			want: WhyNever},
		{name: "no sandbox means no reuse", before: ptr(run.Passed), mode: Reusing, suite: harnessSuite, probe: errors.New("nested"), want: WhyNoSandbox},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			if tt.before != nil {
				f.record(t, Reusing, *tt.before, f.decide(t, Reusing, harnessSuite, harnessUnits()))
			}
			if tt.edit {
				write(t, f.d.Repo, fixtureModule+"/bin/lib-x.sh", "x() { true; }")
			}
			f.d.Probe = func() error { return tt.probe }

			// Act
			v := f.decide(t, tt.mode, tt.suite, harnessUnits())

			// Assert
			u := v.Units[0]
			if u.Keyed != tt.keyed || (u.Reused != nil) != tt.reused {
				t.Fatalf("verdict = %+v, want keyed=%v reused=%v", u, tt.keyed, tt.reused)
			}
			if !tt.reused && u.Why.Kind != tt.want {
				t.Fatalf("why = %+v, want kind %d", u.Why, tt.want)
			}
			if tt.changed != "" && !slices.Contains(u.Why.Changed, tt.changed) {
				t.Fatalf("changed = %v, want %s among them", u.Why.Changed, tt.changed)
			}
			if tt.want == WhyFailedBefore && tt.changed == "" && len(u.Why.Changed) != 0 {
				t.Fatalf("changed = %v, want none for an unedited failure", u.Why.Changed)
			}
		})
	}
}

func TestAUnitTheRosterNamesNeverReusedRunsUnkeyed(t *testing.T) {
	// Arrange
	f := newFixture(t)
	s := lockSuite
	s.Reuse = &roster.Reuse{Never: map[string]string{".": "it walks the module"}}

	// Act
	v := f.decide(t, Reusing, s, lockUnits())

	// Assert
	for _, u := range v.Units {
		if u.Name == "." && (u.Keyed || u.Why.Kind != WhyNever || u.Why.Detail != "it walks the module") {
			t.Fatalf("unit . = %+v, want it never reused with its reason", u)
		}
	}
}

func TestDecideFailures(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fixture) roster.Suite
		want    string
	}{
		{"an underivable input set fails the decision", func(f *fixture) roster.Suite {
			f.d.GoList = func(string, string, bool, ...string) ([]GoPackage, error) { return nil, errors.New("go list broke") }
			return lockSuite
		}, "go list broke"},
		{"a missing declared input fails the decision", func(f *fixture) roster.Suite {
			s := harnessSuite
			s.Reuse = &roster.Reuse{Inputs: []string{"gone/"}}
			return s
		}, "gone"},
		{"an unreadable toolchain fails the decision", func(f *fixture) roster.Suite {
			f.d.Toolchain = func(roster.Kind) (map[string]string, error) { return nil, errors.New("no go") }
			return harnessSuite
		}, "no go"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			s := tt.arrange(&f)
			u := harnessUnits()
			if s.Kind == roster.GoModule {
				u = lockUnits()
			}

			// Act
			su, err := Derive(f.d, s, u, true)
			if err == nil {
				_, err = Decide(f.d, su, Reusing, nil)
			}

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("err = %v, want %q", err, tt.want)
			}
		})
	}
}

func TestWhyNamesOneChangedInput(t *testing.T) {
	tests := []struct {
		why  Why
		want string
	}{
		{Why{Kind: WhyNeverRecorded}, "never recorded"},
		{Why{Kind: WhyFailedBefore}, "failed before, inputs unchanged"},
		{Why{Kind: WhyFailedBefore, Changed: []string{"a.go"}}, "failed before, inputs changed since: a.go"},
		{Why{Kind: WhyInputsChanged, Changed: []string{"a.go"}}, "inputs changed since its newest green record: a.go"},
		{Why{Kind: WhyInputsChanged, Changed: []string{"a.go", "b.go", "c.go"}}, "inputs changed since its newest green record: a.go and 2 more"},
		{Why{Kind: WhyNoReuse}, "this run reuses nothing (--no-reuse, --record or --coverage)"},
		{Why{Kind: WhyTargeted}, "a targeted run neither reuses nor records"},
		{Why{Kind: WhyNever, Detail: "it measures"}, "never reused: it measures"},
		{Why{Kind: WhyNoSandbox}, "reuse unavailable: no sandbox"},
	}
	for _, tt := range tests {
		t.Run(tt.want, func(t *testing.T) {
			// Act
			got := tt.why.String()

			// Assert
			if got != tt.want {
				t.Fatalf("String = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestAChangedReasonThatNamesNoInputIsABug(t *testing.T) {
	// Arrange
	defer func() {
		if recover() == nil {
			t.Fatal("a changed-inputs reason with no input did not panic")
		}
	}()

	// Act
	_ = Why{Kind: WhyInputsChanged}.String()
}

func TestUnitRecordName(t *testing.T) {
	tests := []struct{ suite, unit, want string }{
		{"e2e", "", "e2e"},
		{"daemon", "internal/merge", "daemon@internal%merge"},
		{"daemon", "integration[integration]", "daemon@integration[integration]"},
		{"webapp", "test/feed/x.test.ts", "webapp@test%feed%x.test.ts"},
	}
	for _, tt := range tests {
		t.Run(tt.want, func(t *testing.T) {
			// Act
			got := UnitRecordName(tt.suite, tt.unit)

			// Assert
			if got != tt.want {
				t.Fatalf("UnitRecordName = %q, want %q", got, tt.want)
			}
			if !suiteName.MatchString(got) {
				t.Fatalf("%q is not a name the store takes", got)
			}
		})
	}
}

func TestSpecGroup(t *testing.T) {
	tests := []struct{ id, want string }{
		{"daemon:internal/merge#03", "daemon:internal/merge"},
		{"webapp/test/a.test.ts#00", "webapp/test/a.test.ts"},
		{"lock:vet", "lock:vet"},
	}
	for _, tt := range tests {
		t.Run(tt.id, func(t *testing.T) {
			// Act
			got := SpecGroup(tt.id)

			// Assert
			if got != tt.want {
				t.Fatalf("SpecGroup = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestTheCombinedKeyIsTheUnitsKeysInAnyOrder(t *testing.T) {
	// Arrange
	a := UnitVerdict{Unit: Unit{Suite: "s", Name: "a"}, Key: Key{Digest: "1"}}
	b := UnitVerdict{Unit: Unit{Suite: "s", Name: "b"}, Key: Key{Digest: "2"}}

	// Act
	ab := SuiteVerdict{Units: []UnitVerdict{a, b}}.CombinedKey()
	ba := SuiteVerdict{Units: []UnitVerdict{b, a}}.CombinedKey()
	other := SuiteVerdict{Units: []UnitVerdict{a}}.CombinedKey()

	// Assert
	if ab.Digest != ba.Digest || ab.Digest == other.Digest {
		t.Fatalf("keys = %s %s %s, want order-free and unit-sensitive", ab.Digest, ba.Digest, other.Digest)
	}
}

// TestTheReusedLineIsTheOneTheMergeGateReads pins the line literally: the
// daemon's merge gate parses it (daemon/internal/merge testgate.go, whose
// test pins the same literal).
func TestTheReusedLineIsTheOneTheMergeGateReads(t *testing.T) {
	// Arrange
	log := &logLines{}
	older := &Record{PassedAt: time.Date(2026, 10, 13, 9, 0, 0, 0, time.Local), Branch: "old", Commit: "fedcba9876543210"}
	newer := &Record{PassedAt: time.Date(2026, 10, 14, 23, 37, 0, 0, time.Local), Branch: "master", Commit: "0123456789abcdef", Dirty: true}
	v := SuiteVerdict{SuiteUnits: SuiteUnits{Suite: roster.Suite{Name: "daemon"}}, Units: []UnitVerdict{
		{Unit: Unit{Suite: "daemon", Name: "a"}, Keyed: true, Key: Key{Digest: strings.Repeat("1", 64)}, Reused: older},
		{Unit: Unit{Suite: "daemon", Name: "b"}, Keyed: true, Key: Key{Digest: strings.Repeat("2", 64)}, Reused: newer},
	}}

	// Act
	Report(log, v)

	// Assert
	want := []string{
		"daemon: reused a green run (inputs " + v.CombinedKey().Short() + ", passed 2026-10-14 23:37 on master 0123456789 with uncommitted changes)",
		"daemon: reuse: all 2 units reused",
	}
	if !slices.Equal(log.info, want) {
		t.Fatalf("info = %q, want %q", log.info, want)
	}
}

func TestReportTalliesAPartlyReusedSuiteAndSaysWhyEachUnitRuns(t *testing.T) {
	// Arrange
	log := &logLines{}
	v := SuiteVerdict{SuiteUnits: SuiteUnits{Suite: roster.Suite{Name: "webapp"}}, Units: []UnitVerdict{
		{Unit: Unit{Suite: "webapp", Name: "test/a.test.ts"}, Keyed: true, Reused: &Record{}},
		{Unit: Unit{Suite: "webapp", Name: "test/b.test.ts"}, Keyed: true, Key: Key{Digest: "abcdef0123456789"}, Why: Why{Kind: WhyFailedBefore}},
		{Unit: Unit{Suite: "webapp", Name: "test/c.test.ts"}, Keyed: true, Key: Key{Digest: "0123456789abcdef"}, Why: Why{Kind: WhyInputsChanged, Changed: []string{"src/c.ts"}}},
		{Unit: Unit{Suite: "webapp", Name: "coverage"}, Why: Why{Kind: WhyNever, Detail: "rebuilt"}},
	}}

	// Act
	Report(log, v)

	// Assert
	want := []string{
		"webapp: reuse: 4 units, 1 reused, 3 run (1 failed before, 1 inputs changed, 1 never reused)",
		"webapp: unit test/b.test.ts runs (inputs abcdef012345): failed before, inputs unchanged",
		"webapp: unit test/c.test.ts runs (inputs 0123456789ab): inputs changed since its newest green record: src/c.ts",
		"webapp: unit coverage runs: never reused: rebuilt",
	}
	if !slices.Equal(log.info, want) {
		t.Fatalf("info = %q, want %q", log.info, want)
	}
}

func TestSummarizeTalliesTheWholeRun(t *testing.T) {
	// Arrange
	log := &logLines{}
	whole := SuiteVerdict{SuiteUnits: SuiteUnits{Suite: roster.Suite{Name: "lock"}}, Units: []UnitVerdict{{Reused: &Record{}}, {Reused: &Record{}}}}
	part := SuiteVerdict{SuiteUnits: SuiteUnits{Suite: roster.Suite{Name: "ert"}}, Units: []UnitVerdict{{Reused: &Record{}}, {Why: Why{Kind: WhyNeverRecorded}}}}

	// Act
	Summarize(log, []SuiteVerdict{part, whole})

	// Assert
	want := []string{
		"reuse: 4 units, 3 reused, 1 run (1 never recorded)",
		"reused an earlier green run of every unit, NOT run: lock (--no-reuse runs them)",
	}
	if !slices.Equal(log.info, want) {
		t.Fatalf("info = %q, want %q", log.info, want)
	}
}

func TestSummarizeOfARunThatReusedNothingSaysNoneWholly(t *testing.T) {
	// Arrange
	log := &logLines{}
	v := SuiteVerdict{SuiteUnits: SuiteUnits{Suite: roster.Suite{Name: "e2e"}}, Units: []UnitVerdict{{Why: Why{Kind: WhyNoReuse}}}}

	// Act
	Summarize(log, []SuiteVerdict{v})

	// Assert
	if !slices.Equal(log.info, []string{"reuse: 1 units, 0 reused, 1 run (1 not reusing)"}) {
		t.Fatalf("info = %q", log.info)
	}
}

// infraUnits is a suite of two file units and the build both depend on.
func infraUnits() (SuiteUnits, suites.Units) {
	split := func(g string) suites.Split {
		return suites.Split{Group: g, Deps: []string{"s:build"}, Chunk: func(id string, _ []string) run.Spec { return spec(id) }}
	}
	su := SuiteUnits{Suite: roster.Suite{Name: "s"}, Infra: []string{"s:build"}, Units: []Unit{
		{Suite: "s", Name: "a", Specs: []string{"s/a"}},
		{Suite: "s", Name: "b", Specs: []string{"s/b"}},
	}}
	return su, suites.Units{Atomic: []run.Spec{spec("s:build")}, Splits: []suites.Split{split("s/a"), split("s/b")}}
}

func TestSkipReused(t *testing.T) {
	tests := []struct {
		name       string
		reused     []bool
		wantSplits []string
		wantAtomic []string
	}{
		{"nothing reused keeps every unit and the build", []bool{false, false}, []string{"s/a", "s/b"}, []string{"s:build"}},
		{"a reused unit is dropped, the build stays for the other", []bool{true, false}, []string{"s/b"}, []string{"s:build"}},
		{"the build goes with the last unit that needs it", []bool{true, true}, nil, nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			su, u := infraUnits()
			v := SuiteVerdict{SuiteUnits: su}
			for i, unit := range su.Units {
				uv := UnitVerdict{Unit: unit}
				if tt.reused[i] {
					uv.Reused = &Record{}
				}
				v.Units = append(v.Units, uv)
			}

			// Act
			got := SkipReused(v, u)

			// Assert
			var splits, atomic []string
			for _, sp := range got.Splits {
				splits = append(splits, sp.Group)
			}
			for _, a := range got.Atomic {
				atomic = append(atomic, a.ID)
			}
			if !slices.Equal(splits, tt.wantSplits) || !slices.Equal(atomic, tt.wantAtomic) {
				t.Fatalf("splits=%v atomic=%v, want %v %v", splits, atomic, tt.wantSplits, tt.wantAtomic)
			}
		})
	}
}

func TestASpecNoUnitOwnsIsADerivationBug(t *testing.T) {
	tests := []struct {
		name string
		su   SuiteUnits
	}{
		{"owned by no unit", SuiteUnits{Suite: roster.Suite{Name: "s"}, Units: []Unit{{Name: "a", Specs: []string{"s/a"}}}}},
		{"owned twice", SuiteUnits{Suite: roster.Suite{Name: "s"}, Units: []Unit{{Name: "a", Specs: []string{"s/a", "s/b"}}, {Name: "b", Specs: []string{"s/b"}}}, Infra: []string{"s:build"}}},
		{"both infra and a unit's", SuiteUnits{Suite: roster.Suite{Name: "s"}, Units: []Unit{{Name: "a", Specs: []string{"s/a", "s:build"}}, {Name: "b", Specs: []string{"s/b"}}}, Infra: []string{"s:build"}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			_, u := infraUnits()
			defer func() {
				if recover() == nil {
					t.Fatal("no panic")
				}
			}()

			// Act
			SkipReused(SuiteVerdict{SuiteUnits: tt.su, Units: make([]UnitVerdict, len(tt.su.Units))}, u)
		})
	}
}

func TestConfineWrapsEachSpecInItsUnitsProfile(t *testing.T) {
	// Arrange
	su, u := infraUnits()
	v := SuiteVerdict{SuiteUnits: su, Confined: true, EnvelopeProfile: "/p/s.sb", Units: []UnitVerdict{
		{Unit: su.Units[0], Keyed: true, Profile: "/p/a.sb"},
		{Unit: su.Units[1]},
	}}

	// Act
	got := Confine("/bin/testrun", v, u)

	// Assert
	want := func(profile string) []string {
		return []string{"/bin/testrun", "confine", "--profile", profile, "--keep", "GOMAXPROCS", "--", "go", "test"}
	}
	if argv := got.Atomic[0].Argv; !slices.Equal(argv, want("/p/s.sb")) {
		t.Errorf("the build's argv = %v, want the envelope", argv)
	}
	if argv := got.Splits[0].Chunk("s/a#00", nil).Argv; !slices.Equal(argv, want("/p/a.sb")) {
		t.Errorf("a keyed unit's argv = %v, want its own profile", argv)
	}
	if argv := got.Splits[1].Chunk("s/b#00", nil).Argv; !slices.Equal(argv, []string{"go", "test"}) {
		t.Errorf("a never-reused unit's argv = %v, want it unconfined", argv)
	}
}

func TestAnUnconfinedSuiteIsLeftAsItIs(t *testing.T) {
	// Arrange
	su, u := infraUnits()

	// Act
	got := Confine("/bin/testrun", SuiteVerdict{SuiteUnits: su}, u)

	// Assert
	if argv := got.Atomic[0].Argv; !slices.Equal(argv, []string{"go", "test"}) {
		t.Fatalf("argv = %v, want it untouched", argv)
	}
}

func TestRecordOutcomes(t *testing.T) {
	tests := []struct {
		name     string
		mode     Mode
		outcome  run.Outcome
		edit     bool
		wantPass bool
		// wantLast is the last outcome's Failed, nil for none.
		wantLast *bool
		wantErr  string
	}{
		{name: "a passed unit records a pass and its last outcome", mode: Reusing, outcome: run.Passed, wantPass: true, wantLast: ptr(false)},
		{name: "a --no-reuse run records", mode: NoReuse, outcome: run.Passed, wantPass: true, wantLast: ptr(false)},
		{name: "a failed unit records only that it failed", mode: Reusing, outcome: run.Failed, wantLast: ptr(true)},
		{name: "a targeted run records nothing", mode: Targeted, outcome: run.Passed},
		{name: "inputs that moved during the run record nothing", mode: Reusing, outcome: run.Passed, edit: true, wantErr: "its inputs changed while it ran"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			v := f.decide(t, tt.mode, harnessSuite, harnessUnits())
			if tt.edit {
				write(t, f.d.Repo, fixtureModule+"/bin/lib-x.sh", "edited mid-run")
			}

			// Act
			err := RecordOutcomes(f.d, tt.mode, []SuiteVerdict{v}, outcome([]SuiteVerdict{v}, tt.outcome))

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			_, ok, err := f.d.Store.Lookup("harness", v.Units[0].Key.Digest)
			if err != nil || ok != tt.wantPass {
				t.Fatalf("pass recorded = %v (%v), want %v", ok, err, tt.wantPass)
			}
			last, hasLast, err := f.d.Store.Last("harness")
			if err != nil || hasLast != (tt.wantLast != nil) || (hasLast && last.Failed != *tt.wantLast) {
				t.Fatalf("last = %+v %v (%v), want %v", last, hasLast, err, tt.wantLast)
			}
			if tt.wantErr != "" && (len(f.log.errs) != 1 || !strings.Contains(f.log.errs[0], tt.wantErr)) {
				t.Fatalf("errors = %v, want %q", f.log.errs, tt.wantErr)
			}
		})
	}
}

func ptr[T any](v T) *T { return &v }

func TestAPassedUnitIsRecordedThoughItsSuiteFailed(t *testing.T) {
	// Arrange
	f := newFixture(t)
	v := f.decide(t, Reusing, lockSuite, lockUnits())
	var results []run.Result
	for _, u := range v.Units {
		r := run.Result{Outcome: run.Passed}
		if u.Name == "vet" {
			r.Outcome = run.Failed
		}
		r.Spec.ID = u.Specs[0]
		results = append(results, r)
	}

	// Act
	err := RecordOutcomes(f.d, Reusing, []SuiteVerdict{v}, results)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	for _, u := range v.Units {
		_, ok, err := f.d.Store.Lookup(u.RecordName(), u.Key.Digest)
		if err != nil || ok != (u.Name == ".") {
			t.Errorf("%s recorded = %v (%v), want only the passed package", u.Name, ok, err)
		}
	}
	if !slices.Contains(f.log.info, "lock: reuse: recorded 1 passed and 1 failed units") {
		t.Errorf("info = %v, want the tally", f.log.info)
	}
}

func TestAUnitNoneOfWhoseSpecsRanRecordsNothing(t *testing.T) {
	// Arrange
	f := newFixture(t)
	v := f.decide(t, Reusing, harnessSuite, harnessUnits())

	// Act
	err := RecordOutcomes(f.d, Reusing, []SuiteVerdict{v}, nil)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if _, ok, _ := f.d.Store.Last("harness"); ok {
		t.Fatal("a unit that never ran has a last outcome")
	}
}

func TestARecordFromADirtyCheckoutSaysSo(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.d.Head = func() (string, string, bool, error) { return "wip", "c0ffee", true, nil }
	v := f.decide(t, Reusing, harnessSuite, harnessUnits())

	// Act
	f.record(t, Reusing, run.Passed, v)

	// Assert
	rec, _, err := f.d.Store.Lookup("harness", v.Units[0].Key.Digest)
	if err != nil || !rec.Dirty || rec.Commit != "c0ffee" {
		t.Fatalf("record = %+v, %v; want the dirty marker", rec, err)
	}
}

func TestAnUnreadableHeadFailsTheRecording(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.d.Head = func() (string, string, bool, error) { return "", "", false, errors.New("no git") }
	v := f.decide(t, Reusing, harnessSuite, harnessUnits())

	// Act
	err := RecordOutcomes(f.d, Reusing, []SuiteVerdict{v}, outcome([]SuiteVerdict{v}, run.Passed))

	// Assert
	if err == nil || !strings.Contains(err.Error(), "no git") {
		t.Fatalf("err = %v, want the HEAD failure", err)
	}
	if _, ok, _ := f.d.Store.Lookup("harness", v.Units[0].Key.Digest); ok {
		t.Fatal("a record was written without its source")
	}
}

func TestAFailedConfinedSuiteIsToldItRanConfined(t *testing.T) {
	// Arrange
	f := newFixture(t)
	v := f.decide(t, Reusing, harnessSuite, harnessUnits())

	// Act
	f.record(t, Reusing, run.Failed, v)

	// Assert
	if !slices.ContainsFunc(f.log.info, func(l string) bool { return strings.HasPrefix(l, "harness ran confined to each unit's own inputs") }) {
		t.Fatalf("info = %v", f.log.info)
	}
}

func TestDescribeSpellsTheReuseDeclarationNotItsAddress(t *testing.T) {
	// Arrange
	s := lockSuite
	s.Reuse = &roster.Reuse{Inputs: []string{"prompts/"}, Units: map[string][]string{"p": {"vocab/"}}, Never: map[string]string{"q": "it walks the module"}}

	// Act
	got := Describe(s)

	// Assert
	for _, want := range []string{`Inputs:[]string{"prompts/"}`, `"p":[]string{"vocab/"}`, `"q":"it walks the module"`} {
		if !strings.Contains(got, want) {
			t.Errorf("Describe = %s, want %s in it", got, want)
		}
	}
	if strings.Contains(got, "0x") {
		t.Errorf("Describe = %s spells an address", got)
	}
}

func TestSourceNeverClaimsACommitHoldsDirtyBytes(t *testing.T) {
	tests := []struct {
		name string
		rec  Record
		want string
	}{
		{"a clean checkout", Record{Branch: "master", Commit: "0123456789abcdef"}, "master 0123456789"},
		{"a dirty checkout", Record{Branch: "wip", Commit: "0123456789abcdef", Dirty: true}, "wip 0123456789 with uncommitted changes"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := Source(tt.rec)

			// Assert
			if got != tt.want {
				t.Fatalf("Source = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestAScriptSuitesEnvelopeIsItsScriptAndItsDeclaredInputs(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	set, err := SuiteInputs(f.d, harnessSuite)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	for _, p := range []string{"/bin/test-x.sh", "/bin/lib-x.sh"} {
		if !set.Contains(fixtureModule + p) {
			t.Errorf("the envelope does not place %s", p)
		}
	}
	if set.Contains(fixtureModule + "/prompts/p.md") {
		t.Error("the envelope places what the suite never declared")
	}
}

func TestAnEnvelopeHoldsEveryUnitsDeclaredInputs(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	set, err := SuiteInputs(f.d, lockSuite)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if !set.Contains(fixtureModule + "/prompts/p.md") {
		t.Fatal("the envelope does not place a unit's declared input")
	}
}

func TestAVitestSuitesEnvelopeIsItsTreeAndTheRunnersReporter(t *testing.T) {
	// Arrange
	f := newFixture(t)
	write(t, f.d.Repo, fixtureModule+"/web/src/a.ts", "export {}")
	write(t, f.d.Repo, fixtureModule+"/web/node_modules/dep/index.js", "x")
	write(t, f.d.Repo, fixtureModule+"/testrun/vitest/items-reporter.mjs", "r")
	s := roster.Suite{Name: "web", Kind: roster.Vitest, Path: "web", Reuse: &roster.Reuse{Generated: []string{"web/node_modules/"}}}

	// Act
	set, err := SuiteInputs(f.d, s)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	for _, p := range []string{"/web/src/a.ts", "/testrun/vitest/items-reporter.mjs", "/web/node_modules/dep/index.js"} {
		if !set.Contains(fixtureModule + p) {
			t.Errorf("the set does not place %s", p)
		}
	}
	k, err := ComputeKey(f.d.Repo, set, Facts{})
	if err != nil {
		t.Fatal(err)
	}
	if _, hashed := k.Manifest[fixtureModule+"/web/node_modules/dep/index.js"]; hashed {
		t.Error("a generated node_modules file was hashed")
	}
}

func TestAnUnknownSuiteKindHasNoEnvelope(t *testing.T) {
	// Arrange
	f := newFixture(t)
	s := roster.Suite{Name: "x", Kind: roster.Kind(99), Path: "bin/x.sh", Reuse: &roster.Reuse{}}

	// Act
	_, err := SuiteInputs(f.d, s)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "cannot derive") {
		t.Fatalf("err = %v, want the kind refused", err)
	}
}

func TestADeclaredFilesDirectoryListingIsPlacedToo(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	set, err := SuiteInputs(f.d, harnessSuite)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if !slices.Contains(set.Dirs(), fixtureModule+"/bin") {
		t.Fatalf("Dirs = %v, want the declared file's directory listed", set.Dirs())
	}
}

// TestReuseHoldsAcrossCheckouts pins that a unit's key names its inputs'
// bytes and nothing of where they sit: a pass one workspace recorded is
// reused by another checkout on the host whose inputs for that unit are
// byte-identical, and not reused when one of them differs.
func TestReuseHoldsAcrossCheckouts(t *testing.T) {
	tests := []struct {
		name   string
		edit   bool
		reused bool
	}{
		{"identical inputs in another checkout reuse the pass", false, true},
		{"one differing input in another checkout runs the unit", true, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			a, b := newFixture(t), newFixture(t)
			b.d.Store = a.d.Store
			b.d.Head = func() (string, string, bool, error) { return "other-branch", "beef", false, nil }
			a.record(t, Reusing, run.Passed, a.decide(t, Reusing, harnessSuite, harnessUnits()))
			if tt.edit {
				write(t, b.d.Repo, fixtureModule+"/bin/lib-x.sh", "x() { false; }")
			}

			// Act
			v := b.decide(t, Reusing, harnessSuite, harnessUnits())

			// Assert
			if a.d.Repo == b.d.Repo {
				t.Fatal("the two fixtures share a checkout")
			}
			u := v.Units[0]
			if (u.Reused != nil) != tt.reused {
				t.Fatalf("reused = %v, want %v (why %s)", u.Reused != nil, tt.reused, u.Why)
			}
			if tt.reused && u.Reused.Checkout != a.d.Repo {
				t.Fatalf("the reused record names %s, want the checkout that passed it", u.Reused.Checkout)
			}
			if !tt.reused && u.Why.Kind != WhyInputsChanged {
				t.Fatalf("why = %s, want the changed input named", u.Why)
			}
		})
	}
}
