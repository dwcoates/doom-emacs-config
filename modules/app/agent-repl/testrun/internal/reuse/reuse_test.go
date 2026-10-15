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

// fixture is a checkout whose one keyed suite, "lock", is the Go package
// lock/ of the module at modules/app/agent-repl.
type fixture struct {
	d   Deps
	log *logLines
}

const fixtureModule = "modules/app/agent-repl"

func newFixture(t *testing.T) fixture {
	t.Helper()
	root := writeTree(t, map[string]string{
		fixtureModule + "/lock/go.mod":     "module lock",
		fixtureModule + "/lock/lock.go":    "package lock",
		fixtureModule + "/testrun/go.mod":  "module testrun",
		fixtureModule + "/testrun/main.go": "package main",
		fixtureModule + "/prompts/p.md":    "brief",
	})
	module := filepath.Join(root, filepath.FromSlash(fixtureModule))
	log := &logLines{}
	d := Deps{
		Repo: root, Module: module, DenyRoots: []string{root},
		Store: Store{Dir: t.TempDir()},
		GoList: func(dir, _ string, _ bool, _ ...string) ([]GoPackage, error) {
			return []GoPackage{{ImportPath: filepath.Base(dir), Dir: dir, Module: &GoModule{GoMod: filepath.Join(dir, "go.mod")}}}, nil
		},
		Toolchain:  func(roster.Kind) (map[string]string, error) { return map[string]string{"go": "go1.24"}, nil },
		Environ:    []string{"PATH=/bin", "TMPDIR=/t"},
		Self:       "/bin/testrun",
		ProfileDir: t.TempDir(),
		Probe:      func() error { return nil },
		Head:       func() (string, string, bool, error) { return "feature", "c0ffee", false, nil },
		Now:        func() time.Time { return time.Date(2026, 10, 14, 12, 0, 0, 0, time.UTC) },
		Log:        log,
	}
	return fixture{d: d, log: log}
}

var lockSuite = roster.Suite{Name: "lock", Kind: roster.GoModule, Path: "lock", Reuse: &roster.Reuse{Inputs: []string{"prompts/"}}}
var harnessSuite = roster.Suite{Name: "background-harness", Kind: roster.Script, Path: "bin/test-background.sh"}

func (f fixture) decide(t *testing.T, mode Mode, suites ...roster.Suite) []Verdict {
	t.Helper()
	v, err := Decide(f.d, suites, mode)
	if err != nil {
		t.Fatal(err)
	}
	return v
}

// pass runs a whole run of verdicts in which every suite passed.
func (f fixture) pass(t *testing.T, mode Mode, verdicts []Verdict) {
	t.Helper()
	var results []run.SuiteResult
	for _, v := range verdicts {
		results = append(results, run.SuiteResult{Name: v.Suite.Name, Outcome: run.Passed})
	}
	if err := RecordPasses(f.d, mode, verdicts, results, map[string]int{"lock": 3}); err != nil {
		t.Fatal(err)
	}
}

func TestAKeyedSuiteWithNoRecordRuns(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	v := f.decide(t, Reusing, lockSuite)

	// Assert
	if !v[0].Keyed || v[0].Reused != nil || v[0].Why != "no record" {
		t.Fatalf("verdict = %+v, want a keyed suite that runs for want of a record", v[0])
	}
}

func TestAPassedSuiteIsReusedOnTheSameInputs(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.pass(t, Reusing, f.decide(t, Reusing, lockSuite))

	// Act
	v := f.decide(t, Reusing, lockSuite)

	// Assert
	if v[0].Reused == nil || v[0].Reused.Branch != "feature" || v[0].Reused.Units != 3 {
		t.Fatalf("verdict = %+v, want the earlier pass reused", v[0])
	}
}

func TestAChangedInputRunsTheSuiteAndSaysWhich(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.pass(t, Reusing, f.decide(t, Reusing, lockSuite))
	write(t, f.d.Repo, fixtureModule+"/prompts/p.md", "new brief")

	// Act
	v := f.decide(t, Reusing, lockSuite)

	// Assert
	want := "changed since its newest green record: " + fixtureModule + "/prompts/p.md"
	if v[0].Reused != nil || v[0].Why != want {
		t.Fatalf("verdict = %+v, want %q", v[0], want)
	}
}

func TestDecideModes(t *testing.T) {
	tests := []struct {
		name    string
		mode    Mode
		suite   roster.Suite
		probe   error
		keyed   bool
		reused  bool
		wantWhy string
	}{
		{"--no-reuse runs a recorded suite", NoReuse, lockSuite, nil, true, false, "--no-reuse"},
		{"a targeted run runs a recorded suite", Targeted, lockSuite, nil, true, false, "a targeted run neither reuses nor records"},
		{"a suite with no Reuse always runs", Reusing, harnessSuite, nil, false, false, "always runs"},
		{"no sandbox means no reuse", Reusing, lockSuite, errors.New("nested"), false, false, "reuse unavailable: no sandbox"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.pass(t, Reusing, f.decide(t, Reusing, lockSuite))
			f.d.Probe = func() error { return tt.probe }

			// Act
			v := f.decide(t, tt.mode, tt.suite)

			// Assert
			if v[0].Keyed != tt.keyed || (v[0].Reused != nil) != tt.reused || v[0].Why != tt.wantWhy {
				t.Fatalf("verdict = %+v, want keyed=%v reused=%v why=%q", v[0], tt.keyed, tt.reused, tt.wantWhy)
			}
		})
	}
}

func TestNoSandboxIsSaidOnce(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.d.Probe = func() error { return errors.New("nested") }

	// Act
	f.decide(t, Reusing, lockSuite)

	// Assert
	if len(f.log.info) != 1 || !strings.Contains(f.log.info[0], "reuse is off for this run") {
		t.Fatalf("info = %v", f.log.info)
	}
}

func TestAnUnderivableInputSetFailsTheDecision(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.d.GoList = func(string, string, bool, ...string) ([]GoPackage, error) { return nil, errors.New("go list broke") }

	// Act
	_, err := Decide(f.d, []roster.Suite{lockSuite}, Reusing)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "derive lock's inputs") || !strings.Contains(err.Error(), "go list broke") {
		t.Fatalf("err = %v, want the derivation failure named", err)
	}
}

func TestAMissingDeclaredInputFailsTheDecision(t *testing.T) {
	// Arrange
	f := newFixture(t)
	s := lockSuite
	s.Reuse = &roster.Reuse{Inputs: []string{"vocab/"}}

	// Act
	_, err := Decide(f.d, []roster.Suite{s}, Reusing)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "vocab") {
		t.Fatalf("err = %v, want the missing input named", err)
	}
}

func TestRecordPasses(t *testing.T) {
	tests := []struct {
		name    string
		mode    Mode
		outcome run.Outcome
		edit    bool
		want    bool
		wantErr string
	}{
		{"a whole green run records", Reusing, run.Passed, false, true, ""},
		{"a --no-reuse run records", NoReuse, run.Passed, false, true, ""},
		{"a failed suite records nothing", Reusing, run.Failed, false, false, ""},
		{"a targeted run records nothing", Targeted, run.Passed, false, false, ""},
		{"inputs that moved during the run record nothing", Reusing, run.Passed, true, false, "its inputs changed while it ran"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			v := f.decide(t, tt.mode, lockSuite)
			if tt.edit {
				write(t, f.d.Repo, fixtureModule+"/lock/lock.go", "package lock // edited mid-run")
			}

			// Act
			err := RecordPasses(f.d, tt.mode, v, []run.SuiteResult{{Name: "lock", Outcome: tt.outcome}}, nil)

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			_, ok, err := f.d.Store.Lookup("lock", v[0].Key.Digest)
			if err != nil || ok != tt.want {
				t.Fatalf("recorded = %v (%v), want %v", ok, err, tt.want)
			}
			if tt.wantErr != "" && (len(f.log.errs) != 1 || !strings.Contains(f.log.errs[0], tt.wantErr)) {
				t.Fatalf("errors = %v, want %q", f.log.errs, tt.wantErr)
			}
		})
	}
}

func TestARecordFromADirtyCheckoutSaysSo(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.d.Head = func() (string, string, bool, error) { return "wip", "c0ffee", true, nil }
	v := f.decide(t, Reusing, lockSuite)

	// Act
	f.pass(t, Reusing, v)

	// Assert
	rec, _, err := f.d.Store.Lookup("lock", v[0].Key.Digest)
	if err != nil || !rec.Dirty || rec.Commit != "c0ffee" {
		t.Fatalf("record = %+v, %v; want the dirty marker", rec, err)
	}
}

func TestAnUnreadableHeadFailsTheRecording(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.d.Head = func() (string, string, bool, error) { return "", "", false, errors.New("no git") }
	v := f.decide(t, Reusing, lockSuite)

	// Act
	err := RecordPasses(f.d, Reusing, v, []run.SuiteResult{{Name: "lock", Outcome: run.Passed}}, nil)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "no git") {
		t.Fatalf("err = %v, want the HEAD failure", err)
	}
	if _, ok, _ := f.d.Store.Lookup("lock", v[0].Key.Digest); ok {
		t.Fatal("a record was written without its source")
	}
}

func TestConfineWrapsOnlyAKeyedSuitesUnits(t *testing.T) {
	// Arrange
	f := newFixture(t)
	v := f.decide(t, Reusing, lockSuite, harnessSuite)
	unit := func(id string) run.Spec {
		sp := run.Spec{Argv: []string{"go", "test"}, Env: []string{"GOMAXPROCS=2"}}
		sp.ID = id
		return sp
	}
	u := suites.Units{
		Atomic: []run.Spec{unit("build")},
		Splits: []suites.Split{{Chunk: func(id string, _ []string) run.Spec { return unit(id) }}},
	}

	// Act
	keyed := Confine(f.d.Self, v, "lock", u)
	other := Confine(f.d.Self, v, "background-harness", u)

	// Assert
	wantPrefix := []string{"/bin/testrun", "confine", "--profile", v[0].Profile, "--keep", "GOMAXPROCS", "--", "go", "test"}
	if got := keyed.Atomic[0].Argv; !slices.Equal(got, wantPrefix) {
		t.Errorf("atomic argv = %v, want %v", got, wantPrefix)
	}
	if got := keyed.Splits[0].Chunk("c", nil).Argv; !slices.Equal(got, wantPrefix) {
		t.Errorf("chunk argv = %v, want %v", got, wantPrefix)
	}
	if got := other.Atomic[0].Argv; !slices.Equal(got, []string{"go", "test"}) {
		t.Errorf("an always-run suite's argv = %v, want it untouched", got)
	}
}

func TestReportSaysWhatWasReusedAndWhyTheRestRuns(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.pass(t, Reusing, f.decide(t, Reusing, lockSuite))
	v := f.decide(t, Reusing, lockSuite, harnessSuite)
	log := &logLines{}

	// Act
	Report(log, v)

	// Assert
	if len(log.info) != 1 || !strings.HasPrefix(log.info[0], "lock: reused a green run (inputs "+v[0].Key.Short()+", passed ") ||
		!strings.HasSuffix(log.info[0], " on feature c0ffee)") {
		t.Fatalf("info = %v", log.info)
	}
}

func TestDescribeSpellsTheReuseDeclarationNotItsAddress(t *testing.T) {
	// Act
	got := Describe(lockSuite)

	// Assert
	if !strings.Contains(got, `Inputs:[]string{"prompts/"}`) || strings.Contains(got, "0x") {
		t.Fatalf("Describe = %s", got)
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

// TestTheReusedLineIsTheOneTheMergeGateReads pins the line literally: the
// daemon's merge gate parses it (daemon/internal/merge testgate.go, whose
// test pins the same literal).
func TestTheReusedLineIsTheOneTheMergeGateReads(t *testing.T) {
	// Arrange
	log := &logLines{}
	v := []Verdict{{Suite: roster.Suite{Name: "daemon"}, Keyed: true, Key: Key{Digest: "4ad2933f8e55" + strings.Repeat("0", 52)},
		Reused: &Record{PassedAt: time.Date(2026, 10, 14, 23, 37, 0, 0, time.Local), Branch: "master", Commit: "0123456789abcdef", Dirty: true}}}

	// Act
	Report(log, v)

	// Assert
	want := "daemon: reused a green run (inputs 4ad2933f8e55, passed 2026-10-14 23:37 on master 0123456789 with uncommitted changes)"
	if len(log.info) != 1 || log.info[0] != want {
		t.Fatalf("info = %q, want %q", log.info, want)
	}
}

func TestAVitestSuiteReadsItsTreeAndTheRunnersReporter(t *testing.T) {
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

func TestAnERTSuiteReadsItsTreeTheDriverAndItsFakeDaemonsBuild(t *testing.T) {
	// Arrange
	f := newFixture(t)
	write(t, f.d.Repo, fixtureModule+"/lisp/test-a.el", "(ert-deftest a ())")
	write(t, f.d.Repo, fixtureModule+"/lisp/testsupport/fakedaemon/go.mod", "module fakedaemon")
	write(t, f.d.Repo, fixtureModule+"/testrun/ert/driver.el", "d")
	var listed []string
	inner := f.d.GoList
	f.d.GoList = func(dir, tags string, tests bool, patterns ...string) ([]GoPackage, error) {
		listed = append(listed, dir)
		return inner(dir, tags, tests, patterns...)
	}
	s := roster.Suite{Name: "ert", Kind: roster.ERT, Path: "lisp", Reuse: &roster.Reuse{}}

	// Act
	set, err := SuiteInputs(f.d, s)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	for _, p := range []string{"/lisp/test-a.el", "/testrun/ert/driver.el"} {
		if !set.Contains(fixtureModule + p) {
			t.Errorf("the set does not place %s", p)
		}
	}
	if want := filepath.Join(f.d.Module, "lisp", "testsupport", "fakedaemon"); !slices.Contains(listed, want) {
		t.Errorf("go list ran in %v, want the fake daemon's directory %s", listed, want)
	}
}

func TestASuiteKindWithoutDerivableInputsIsRefused(t *testing.T) {
	// Arrange
	f := newFixture(t)
	s := roster.Suite{Name: "x", Kind: roster.Script, Path: "bin/x.sh", Reuse: &roster.Reuse{}}

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
	write(t, f.d.Repo, fixtureModule+"/bin/x.sh", "#!/bin/sh")
	s := lockSuite
	s.Reuse = &roster.Reuse{Inputs: []string{"bin/x.sh"}}

	// Act
	set, err := SuiteInputs(f.d, s)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if !slices.Contains(set.Dirs(), fixtureModule+"/bin") {
		t.Fatalf("Dirs = %v, want the declared file's directory listed", set.Dirs())
	}
}

func TestAFailedKeyedSuiteIsToldItRanConfined(t *testing.T) {
	// Arrange
	f := newFixture(t)
	v := f.decide(t, Reusing, lockSuite)

	// Act
	err := RecordPasses(f.d, Reusing, v, []run.SuiteResult{{Name: "lock", Outcome: run.Failed}}, nil)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(f.log.info) != 1 || !strings.HasPrefix(f.log.info[0], "lock ran confined to its inputs") {
		t.Fatalf("info = %v", f.log.info)
	}
}
