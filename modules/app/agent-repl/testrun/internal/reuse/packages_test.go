package reuse

import (
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"slices"
	"strings"
	"testing"
	"time"

	"agentrepl/testrun/internal/gobuild"
	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/suites"
	"agentrepl/testrun/roster"
)

// pkgFixture is a checkout whose keyed suite "daemon" (module d at
// modules/app/agent-repl/daemon) tests the packages a (importing b), b
// (embedding sub/e.txt), c (reading prompts/ as declared data) and n (never
// reused).
type pkgFixture struct {
	fixture
	root  string
	suite roster.Suite
	// listing is what the fake `go list -deps -test` of the module answers.
	listing []GoPackage
}

var pkgs = []string{"a", "b", "c", "n"}

func newPkgFixture(t *testing.T) *pkgFixture {
	t.Helper()
	f := &pkgFixture{fixture: newFixture(t)}
	f.root = f.d.Repo
	mod := fixtureModule + "/daemon/"
	for p, body := range map[string]string{
		mod + "go.mod":                       "module d\n",
		mod + "a/a.go":                       "package a",
		mod + "a/a_test.go":                  "package a",
		mod + "a/testdata/x.txt":             "x",
		mod + "b/b.go":                       "package b",
		mod + "b/sub/e.txt":                  "embedded",
		mod + "c/c.go":                       "package c",
		mod + "n/n.go":                       "package n",
		fixtureModule + "/docs/unrelated.md": "notes",
	} {
		write(t, f.root, p, body)
	}
	dir := func(rel string) string { return filepath.Join(f.d.Module, "daemon", rel) }
	gomod := &GoModule{Path: "d", GoMod: dir("go.mod")}
	f.listing = []GoPackage{
		{ImportPath: "fmt", Standard: true},
		{ImportPath: "d/b", Dir: dir("b"), Module: gomod, EmbedFiles: []string{"sub/e.txt"}},
		{ImportPath: "d/a [d/a.test]", Dir: dir("a"), Module: gomod, Deps: []string{"d/b", "fmt"}},
		{ImportPath: "d/a.test", Dir: dir("a"), Module: gomod, Deps: []string{"d/a [d/a.test]", "d/b", "fmt"}},
		{ImportPath: "d/b [d/b.test]", Dir: dir("b"), Module: gomod, EmbedFiles: []string{"sub/e.txt"}},
		{ImportPath: "d/b.test", Dir: dir("b"), Module: gomod, Deps: []string{"d/b [d/b.test]"}},
		{ImportPath: "d/c [d/c.test]", Dir: dir("c"), Module: gomod},
		{ImportPath: "d/c.test", Dir: dir("c"), Module: gomod, Deps: []string{"d/c [d/c.test]"}},
	}
	runner := f.d.GoList
	f.d.GoList = func(d, tags string, tests bool, patterns ...string) ([]GoPackage, error) {
		if d == dir(".") {
			return f.listing, nil
		}
		return runner(d, tags, tests, patterns...)
	}
	f.suite = roster.Suite{Name: "daemon", Kind: roster.GoModule, Path: "daemon", Reuse: &roster.Reuse{},
		PackageReuse: &roster.PackageReuse{
			Inputs: map[string][]string{"c": {"prompts/"}},
			Never:  map[string]string{"n": "it walks the whole module"},
		}}
	return f
}

func (f *pkgFixture) decidePackages(t *testing.T, mode Mode) []PackageVerdict {
	t.Helper()
	v := f.decide(t, mode, f.suite)
	pv, err := DecidePackages(f.d, v[0], mode, pkgs)
	if err != nil {
		t.Fatal(err)
	}
	return pv
}

// passAll records a run in which every package's one chunk passed.
func (f *pkgFixture) passAll(t *testing.T, pv []PackageVerdict) {
	t.Helper()
	var results []run.Result
	for _, v := range pv {
		var r run.Result
		r.Spec.ID, r.Outcome = v.Group()+"#00", run.Passed
		results = append(results, r)
	}
	if err := RecordPackagePasses(f.d, Reusing, pv, results); err != nil {
		t.Fatal(err)
	}
}

func reusedOf(pv []PackageVerdict) []string {
	var out []string
	for _, v := range pv {
		if v.Reused != nil {
			out = append(out, v.Package)
		}
	}
	return out
}

func TestAPassedPackageIsReusedOnTheSameInputs(t *testing.T) {
	// Arrange
	f := newPkgFixture(t)
	f.passAll(t, f.decidePackages(t, Reusing))

	// Act
	pv := f.decidePackages(t, Reusing)

	// Assert
	if got := reusedOf(pv); !slices.Equal(got, []string{"a", "b", "c"}) {
		t.Fatalf("reused %v, want a, b and c", got)
	}
	if n := pv[3]; n.Keyed || n.Why != "never reused: it walks the whole module" {
		t.Fatalf("n = %+v, want it never keyed", n)
	}
	if !slices.Contains(f.log.info, "daemon: recorded the green runs of 3 packages") {
		t.Fatalf("info = %q, want the recording counted", f.log.info)
	}
}

// THE RULE (the /agent-repl-tests skill, "A unit's reuse inputs come from the
// tool's dependency listing"): each kind of input moves exactly the keys of
// the packages that read it.
func TestEachInputMovesOnlyTheKeysOfThePackagesThatReadIt(t *testing.T) {
	mod := fixtureModule + "/daemon/"
	tests := []struct {
		name   string
		change func(t *testing.T, f *pkgFixture)
		reused []string
	}{
		{"an imported package's source", func(t *testing.T, f *pkgFixture) { write(t, f.root, mod+"b/b.go", "package b // changed") }, []string{"c"}},
		{"a package's own test file", func(t *testing.T, f *pkgFixture) { write(t, f.root, mod+"a/a_test.go", "package a // changed") }, []string{"b", "c"}},
		{"a package's testdata", func(t *testing.T, f *pkgFixture) { write(t, f.root, mod+"a/testdata/x.txt", "y") }, []string{"b", "c"}},
		{"an embedded file beneath an imported package", func(t *testing.T, f *pkgFixture) { write(t, f.root, mod+"b/sub/e.txt", "changed") }, []string{"c"}},
		{"a declared data input", func(t *testing.T, f *pkgFixture) { write(t, f.root, fixtureModule+"/prompts/p.md", "changed") }, []string{"a", "b"}},
		{"the module's go.mod", func(t *testing.T, f *pkgFixture) { write(t, f.root, mod+"go.mod", "module d\n\ngo 1.25\n") }, nil},
		{"the build tags", func(t *testing.T, f *pkgFixture) { f.d.Environ = append(f.d.Environ, "GOFLAGS=-tags=x") }, nil},
		{"the toolchain", func(t *testing.T, f *pkgFixture) {
			f.d.Toolchain = func(roster.Kind) (map[string]string, error) { return map[string]string{"go": "go1.25"}, nil }
		}, nil},
		{"the runner's own code", func(t *testing.T, f *pkgFixture) {
			write(t, f.root, fixtureModule+"/testrun/main.go", "package main // changed")
		}, nil},
		{"a file no package reads", func(t *testing.T, f *pkgFixture) { write(t, f.root, fixtureModule+"/docs/unrelated.md", "changed") }, []string{"a", "b", "c"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newPkgFixture(t)
			f.passAll(t, f.decidePackages(t, Reusing))
			tt.change(t, f)

			// Act
			pv := f.decidePackages(t, Reusing)

			// Assert
			if got := reusedOf(pv); !slices.Equal(got, tt.reused) {
				t.Fatalf("reused %v, want %v", got, tt.reused)
			}
			for _, v := range pv {
				if v.Keyed && v.Reused == nil && !strings.HasPrefix(v.Why, "changed since its newest green record: ") {
					t.Fatalf("%s runs because %q, want what changed named", v.Package, v.Why)
				}
			}
		})
	}
}

// THE RULE's enforcement against the kernel: a package's tests run under its
// own profile, so a file its recorded inputs omit cannot be read, and the run
// fails instead of recording a pass on bytes the key does not name.
func TestAPackagesProfileRefusesAFileItsInputsOmit(t *testing.T) {
	mod := fixtureModule + "/daemon/"
	tests := []struct {
		name, pkg, file string
		allowed         bool
	}{
		{"its own testdata", "a", mod + "a/testdata/x.txt", true},
		{"an imported package's embedded file", "a", mod + "b/sub/e.txt", true},
		{"its declared data", "c", fixtureModule + "/prompts/p.md", true},
		{"another package's declared data", "a", fixtureModule + "/prompts/p.md", false},
		{"a package it does not import", "c", mod + "a/a.go", false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newPkgFixture(t)
			pv := f.decidePackages(t, Reusing)
			var profile string
			for _, v := range pv {
				if v.Package == tt.pkg {
					profile = v.Profile
				}
			}

			// Act
			out, err := exec.Command(SandboxExec, "-f", profile, "/bin/cat", filepath.Join(f.root, filepath.FromSlash(tt.file))).CombinedOutput()

			// Assert
			if allowed := err == nil; allowed != tt.allowed {
				t.Fatalf("allowed = %v, want %v: %v\n%s", allowed, tt.allowed, err, out)
			}
			if !tt.allowed && !strings.Contains(string(out), "Operation not permitted") {
				t.Fatalf("the refusal was not the sandbox's:\n%s", out)
			}
		})
	}
}

func TestDecidePackagesDecidesNoneWhen(t *testing.T) {
	tests := []struct {
		name    string
		mode    Mode
		verdict func(Verdict) Verdict
	}{
		{"the run is targeted", Targeted, func(v Verdict) Verdict { return v }},
		{"the suite is reused whole", Reusing, func(v Verdict) Verdict { v.Reused = &Record{}; return v }},
		{"the suite is not confined", Reusing, func(v Verdict) Verdict { v.Keyed = false; return v }},
		{"the suite declares no package reuse", Reusing, func(v Verdict) Verdict { v.Suite.PackageReuse = nil; return v }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newPkgFixture(t)
			v := tt.verdict(f.decide(t, Reusing, f.suite)[0])

			// Act
			pv, err := DecidePackages(f.d, v, tt.mode, pkgs)

			// Assert
			if err != nil || pv != nil {
				t.Fatalf("DecidePackages = %+v, %v; want no decision", pv, err)
			}
		})
	}
}

func TestDecidePackagesUnderNoReuseKeysButNeverReuses(t *testing.T) {
	// Arrange
	f := newPkgFixture(t)
	f.passAll(t, f.decidePackages(t, Reusing))

	// Act
	pv := f.decidePackages(t, NoReuse)

	// Assert
	if got := reusedOf(pv); got != nil || !pv[0].Keyed || pv[0].Why != "--no-reuse" {
		t.Fatalf("reused %v, a = %+v; want every package keyed and run", got, pv[0])
	}
}

func TestDecidePackagesFails(t *testing.T) {
	tests := []struct {
		name  string
		setup func(f *pkgFixture)
		pkgs  []string
		want  string
	}{
		{"the listing lacks a package's test binary", func(f *pkgFixture) {}, append(slices.Clone(pkgs), "z"), "tests z, but the listing holds no test binary d/z.test"},
		{"a test binary links a package the listing lacks", func(f *pkgFixture) {
			f.listing[3].Deps = append(f.listing[3].Deps, "d/ghost")
		}, pkgs, "d/a.test's test binary links d/ghost, which the listing does not hold"},
		{"the declaration names a package the suite does not test", func(f *pkgFixture) {
			f.suite.PackageReuse.Inputs["gone"] = []string{"prompts/"}
		}, pkgs, "names gone, which it tests no package of"},
		{"a package both declared and never reused", func(f *pkgFixture) {
			f.suite.PackageReuse.Inputs["n"] = []string{"prompts/"}
		}, pkgs, "both declares n's inputs and names it never reused"},
		{"a declared input that is not there", func(f *pkgFixture) {
			f.suite.PackageReuse.Inputs["c"] = []string{"missing.md"}
		}, pkgs, "missing.md"},
		{"the listing fails", func(f *pkgFixture) {
			f.d.GoList = func(string, string, bool, ...string) ([]GoPackage, error) { return nil, errors.New("go list exited 1") }
		}, pkgs, "go list exited 1"},
		{"the toolchain cannot be read", func(f *pkgFixture) {
			f.d.Toolchain = func(roster.Kind) (map[string]string, error) { return nil, errors.New("no go") }
		}, pkgs, "daemon's toolchain: no go"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newPkgFixture(t)
			v := f.decide(t, Reusing, f.suite)[0]
			tt.setup(f)
			v.Suite = f.suite

			// Act
			_, err := DecidePackages(f.d, v, Reusing, tt.pkgs)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("DecidePackages = %v, want %q", err, tt.want)
			}
		})
	}
}

func TestRecordPackagePassesRecordsOnlyWholePasses(t *testing.T) {
	// Arrange
	f := newPkgFixture(t)
	pv := f.decidePackages(t, Reusing)
	result := func(id string, o run.Outcome) run.Result {
		var r run.Result
		r.Spec.ID, r.Outcome = id, o
		return r
	}
	results := []run.Result{
		result("daemon:a#00", run.Passed), result("daemon:a#01", run.Passed),
		result("daemon:b#00", run.Passed), result("daemon:b#01", run.Failed),
		// c never ran.
	}

	// Act
	err := RecordPackagePasses(f.d, Reusing, pv, results)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if got := reusedOf(f.decidePackages(t, Reusing)); !slices.Equal(got, []string{"a"}) {
		t.Fatalf("reused %v, want only a, whose every chunk passed", got)
	}
	want := `daemon:b ran confined to its own inputs: a failure reading a checkout file with "operation not permitted", or a listing of a checkout directory that comes back empty (filepath.Glob drops the refusal), is a read its PackageReuse does not declare`
	if !slices.ContainsFunc(f.log.info, func(l string) bool { return strings.HasPrefix(l, want) }) {
		t.Fatalf("info = %q, want the failed package told it ran confined", f.log.info)
	}
}

func TestRecordPackagePassesSkipsAPackageWhoseInputsMovedWhileItRan(t *testing.T) {
	// Arrange
	f := newPkgFixture(t)
	pv := f.decidePackages(t, Reusing)
	write(t, f.root, fixtureModule+"/daemon/a/a.go", "package a // edited mid-run")

	// Act
	f.passAll(t, pv)

	// Assert
	if !slices.Contains(f.log.errs, "daemon:a: not recorded, its inputs changed while it ran: "+fixtureModule+"/daemon/a/a.go") {
		t.Fatalf("errors = %q, want a's moved input named", f.log.errs)
	}
	if got := reusedOf(f.decidePackages(t, Reusing)); !slices.Equal(got, []string{"b", "c"}) {
		t.Fatalf("reused %v, want b and c", got)
	}
}

func TestRecordPackagePassesFails(t *testing.T) {
	tests := []struct {
		name  string
		setup func(f *pkgFixture)
		want  string
	}{
		{"an unreadable HEAD", func(f *pkgFixture) {
			f.d.Head = func() (string, string, bool, error) { return "", "", false, errors.New("no git") }
		}, "read the checkout's HEAD: no git"},
		{"an unreadable toolchain", func(f *pkgFixture) {
			f.d.Toolchain = func(roster.Kind) (map[string]string, error) { return nil, errors.New("no go") }
		}, "daemon's toolchain: no go"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newPkgFixture(t)
			pv := f.decidePackages(t, Reusing)
			tt.setup(f)
			var r run.Result
			r.Spec.ID, r.Outcome = "daemon:a#00", run.Passed

			// Act
			err := RecordPackagePasses(f.d, Reusing, pv, []run.Result{r})

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("RecordPackagePasses = %v, want %q", err, tt.want)
			}
			entries, _ := os.ReadDir(f.d.Store.Dir)
			if len(entries) != 0 {
				t.Fatalf("the store holds %d entries after a failed recording, want none", len(entries))
			}
		})
	}
}

func TestATargetedRunRecordsNoPackage(t *testing.T) {
	// Arrange
	f := newPkgFixture(t)
	pv := f.decidePackages(t, Reusing)
	var r run.Result
	r.Spec.ID, r.Outcome = "daemon:a#00", run.Passed

	// Act
	err := RecordPackagePasses(f.d, Targeted, pv, []run.Result{r})

	// Assert
	if err != nil || reusedOf(f.decidePackages(t, Reusing)) != nil {
		t.Fatalf("a targeted run recorded a package: %v", err)
	}
}

func goBuildUnits(t *testing.T) suites.Units {
	t.Helper()
	b := suites.GoBuild{ID: "daemon:build", Suite: "daemon", Options: gobuild.Options{Module: "/d", BinDir: "/b", Self: "/testrun"},
		Needs: map[string]string{"daemon:a": "a", "daemon:b": "b", "daemon:c:compiled": "c"}}
	chunk := func(id string, _ []string) run.Spec {
		var sp run.Spec
		sp.ID, sp.Argv = id, []string{"/testrun", "exec-test"}
		return sp
	}
	var check run.Spec
	check.ID, check.Argv = "daemon:c:compiled", []string{"/testrun", "check-test"}
	return suites.Units{
		Splits:   []suites.Split{{Group: "daemon:a", Package: "a", Chunk: chunk}, {Group: "daemon:b", Package: "b", Chunk: chunk}},
		Atomic:   []run.Spec{check},
		GoBuilds: []suites.GoBuild{b},
	}.Finish()
}

func TestSkipReusedPackagesDropsTheirUnitsAndNarrowsTheBuild(t *testing.T) {
	// Arrange
	u := goBuildUnits(t)
	pv := []PackageVerdict{
		{Suite: "daemon", Package: "a", Reused: &Record{}},
		{Suite: "daemon", Package: "b"},
		{Suite: "daemon", Package: "c", Reused: &Record{}},
	}

	// Act
	got := SkipReusedPackages(pv, u)

	// Assert
	if len(got.Splits) != 1 || got.Splits[0].Group != "daemon:b" {
		t.Fatalf("splits = %+v, want b's alone", got.Splits)
	}
	if len(got.Atomic) != 1 || got.Atomic[0].ID != "daemon:build" || !slices.Equal(got.Atomic[0].Argv[len(got.Atomic[0].Argv)-2:], []string{"--", "b"}) {
		t.Fatalf("atomic = %+v, want the build over b alone", got.Atomic)
	}
}

func TestConfineHoldsAKeyedPackagesUnitsToItsOwnProfile(t *testing.T) {
	// Arrange
	f := newPkgFixture(t)
	v := f.decide(t, Reusing, f.suite)
	pv := []PackageVerdict{{Suite: "daemon", Package: "a", Keyed: true, Profile: "/p/a.sb"}, {Suite: "daemon", Package: "c", Keyed: true, Profile: "/p/c.sb"}}

	// Act
	got := Confine(f.d.Self, v, pv, "daemon", goBuildUnits(t))

	// Assert
	profileOf := func(sp run.Spec) string { return sp.Argv[slices.Index(sp.Argv, "--profile")+1] }
	want := map[string]string{"daemon:a#00": "/p/a.sb", "daemon:b#00": v[0].Profile}
	for _, sp := range got.Splits {
		c := sp.Chunk(sp.Group+"#00", nil)
		if profileOf(c) != want[c.ID] {
			t.Errorf("%s runs under %s, want %s", c.ID, profileOf(c), want[c.ID])
		}
	}
	for _, a := range got.Atomic {
		wantProfile := v[0].Profile
		if a.ID == "daemon:c:compiled" {
			wantProfile = "/p/c.sb"
		}
		if profileOf(a) != wantProfile {
			t.Errorf("%s runs under %s, want %s", a.ID, profileOf(a), wantProfile)
		}
	}
	if got.GoBuilds != nil {
		t.Fatalf("confined units carry GoBuilds %+v, which a later Finish would place unconfined", got.GoBuilds)
	}
}

func TestUnitPackagesAreTheOrdinaryBuildsPackages(t *testing.T) {
	// Arrange
	u := goBuildUnits(t)
	u.GoBuilds = append(u.GoBuilds, suites.GoBuild{ID: "daemon:build[integration]", Needs: map[string]string{"daemon:integration[integration]": "integration"}})

	// Act
	got := UnitPackages("daemon", u)

	// Assert
	if !slices.Equal(got, []string{"a", "b", "c"}) {
		t.Fatalf("UnitPackages = %v, want a, b and c, never the integration pass's", got)
	}
}

func TestPackageRecordNameIsAStoreName(t *testing.T) {
	tests := []struct{ rel, want string }{
		{".", "daemon@."},
		{"internal/merge", "daemon@internal%merge"},
	}
	for _, tt := range tests {
		t.Run(tt.rel, func(t *testing.T) {
			// Act
			got := PackageRecordName("daemon", tt.rel)

			// Assert
			if got != tt.want || !suiteName.MatchString(got) {
				t.Fatalf("PackageRecordName = %q (store name: %v), want %q", got, suiteName.MatchString(got), tt.want)
			}
		})
	}
}

func TestPackageOfUnit(t *testing.T) {
	for id, want := range map[string]string{"daemon:a#03": "daemon:a", "daemon:c:compiled": "daemon:c", "daemon:build": "daemon:build"} {
		if got := packageOfUnit(id); got != want {
			t.Errorf("packageOfUnit(%s) = %s, want %s", id, got, want)
		}
	}
}

// suiteReusedLine is the merge gate's suite-reuse pattern
// (daemon/internal/merge/testgate.go suiteReused): a package's line must
// never read as a suite's, or the tests tab would draw a row for it.
var suiteReusedLine = regexp.MustCompile(`(?m)^.*?([A-Za-z0-9_-]+): reused a green run \(inputs [0-9a-f]+, (passed .+)\)\s*$`)

func TestReportPackagesNamesEachPackageInALineNoSuiteParserReads(t *testing.T) {
	// Arrange
	log := &logLines{}
	pv := []PackageVerdict{
		{Suite: "daemon", Package: "internal/merge", Key: Key{Digest: "4ad2933f8e55" + strings.Repeat("0", 52)},
			Reused: &Record{PassedAt: mustTime(t, "2026-11-10 09:30"), Branch: "master", Commit: "0123456789abcdef"}},
		{Suite: "daemon", Package: "internal/x", Why: "no record"},
	}

	// Act
	ReportPackages(log, pv)

	// Assert
	want := []string{
		"daemon: reused package internal/merge's green run (inputs 4ad2933f8e55, passed 2026-11-10 09:30 on master 0123456789)",
		"daemon: package internal/x runs: no record",
	}
	if !slices.Equal(log.info, want) {
		t.Fatalf("info = %q, want %q", log.info, want)
	}
	if suiteReusedLine.MatchString(log.info[0]) {
		t.Fatalf("the merge gate would read %q as a suite's reuse", log.info[0])
	}
	if got := ReusedPackageNames(pv); !slices.Equal(got, []string{"daemon:internal/merge"}) {
		t.Fatalf("ReusedPackageNames = %v", got)
	}
}

func mustTime(t *testing.T, s string) time.Time {
	t.Helper()
	at, err := time.ParseInLocation("2006-01-02 15:04", s, time.Local)
	if err != nil {
		t.Fatal(err)
	}
	return at
}
