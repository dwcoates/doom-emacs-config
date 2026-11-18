package selection

import (
	"reflect"
	"strings"
	"testing"

	testrunv1 "agentrepl/proto/testrun/v1"

	"agentrepl/testrun/internal/gobuild"
	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/sched"
	"agentrepl/testrun/internal/suites"
	"agentrepl/testrun/roster"
)

// layout places the module where the repository keeps it; resolution is path
// arithmetic, so no file needs to exist.
var layout = suites.Layout{Repo: "/r", Module: "/r/modules/app/agent-repl"}

const mod = "modules/app/agent-repl/"

func goAll(dir string) *testrunv1.TestSelection {
	return &testrunv1.TestSelection{Target: &testrunv1.TestSelection_GoPackage{GoPackage: &testrunv1.TestSelectionGoPackage{
		PackageDir: dir, Tests: &testrunv1.TestSelectionGoPackage_All{All: &testrunv1.TestSelectionGoAllTests{}}}}}
}

func goNamed(dir string, tests ...*testrunv1.TestSelectionGoTest) *testrunv1.TestSelection {
	return &testrunv1.TestSelection{Target: &testrunv1.TestSelection_GoPackage{GoPackage: &testrunv1.TestSelectionGoPackage{
		PackageDir: dir, Tests: &testrunv1.TestSelectionGoPackage_Named{Named: &testrunv1.TestSelectionGoNamedTests{Tests: tests}}}}}
}

func whole(name string) *testrunv1.TestSelectionGoTest {
	return &testrunv1.TestSelectionGoTest{Name: name, Scope: &testrunv1.TestSelectionGoTest_Whole{Whole: &testrunv1.TestSelectionGoWholeTest{}}}
}

func sub(name string, paths ...[]string) *testrunv1.TestSelectionGoTest {
	var ps []*testrunv1.TestSelectionGoSubtestPath
	for _, p := range paths {
		ps = append(ps, &testrunv1.TestSelectionGoSubtestPath{Segments: p})
	}
	return &testrunv1.TestSelectionGoTest{Name: name, Scope: &testrunv1.TestSelectionGoTest_Subtests{Subtests: &testrunv1.TestSelectionGoSubtests{Paths: ps}}}
}

func vitestNamed(file string, names ...string) *testrunv1.TestSelection {
	return &testrunv1.TestSelection{Target: &testrunv1.TestSelection_VitestFile{VitestFile: &testrunv1.TestSelectionVitestFile{
		File: file, Tests: &testrunv1.TestSelectionVitestFile_Named{Named: &testrunv1.TestSelectionVitestNamedTests{FullNames: names}}}}}
}

func ertAll(file string) *testrunv1.TestSelection {
	return &testrunv1.TestSelection{Target: &testrunv1.TestSelection_ErtFile{ErtFile: &testrunv1.TestSelectionErtFile{
		File: file, Tests: &testrunv1.TestSelectionErtFile_All{All: &testrunv1.TestSelectionErtAllTests{}}}}}
}

func harness(script string, groups ...string) *testrunv1.TestSelection {
	h := &testrunv1.TestSelectionHarness{Script: script, Groups: &testrunv1.TestSelectionHarness_All{All: &testrunv1.TestSelectionHarnessAllGroups{}}}
	if len(groups) > 0 {
		h.Groups = &testrunv1.TestSelectionHarness_Named{Named: &testrunv1.TestSelectionHarnessNamedGroups{Names: groups}}
	}
	return &testrunv1.TestSelection{Target: &testrunv1.TestSelection_Harness{Harness: h}}
}

func suiteSel(name string) *testrunv1.TestSelection {
	return &testrunv1.TestSelection{Target: &testrunv1.TestSelection_Suite{Suite: &testrunv1.TestSelectionSuite{Name: name}}}
}

func everySuite() *testrunv1.TestSelection {
	return &testrunv1.TestSelection{Target: &testrunv1.TestSelection_EverySuite{EverySuite: &testrunv1.TestSelectionEverySuite{}}}
}

func request(sels ...*testrunv1.TestSelection) *testrunv1.TestRunRequest {
	return &testrunv1.TestRunRequest{Selections: sels}
}

func suiteNamed(t *testing.T, name string) roster.Suite {
	t.Helper()
	for _, s := range roster.Suites {
		if s.Name == name {
			return s
		}
	}
	t.Fatalf("no roster suite %s", name)
	return roster.Suite{}
}

func names(ss []roster.Suite) []string {
	var out []string
	for _, s := range ss {
		out = append(out, s.Name)
	}
	return out
}

func TestResolveFindsTheSuiteThatOwnsEachTarget(t *testing.T) {
	cases := []struct {
		name string
		sel  *testrunv1.TestSelection
		want string
	}{
		{name: "a daemon package", sel: goAll(mod + "daemon/internal/merge"), want: "daemon"},
		{name: "the e2e module's root package", sel: goAll(mod + "e2e"), want: "e2e"},
		{name: "a webapp vitest file", sel: vitestNamed(mod+"webapp/src/a.test.ts", "a test"), want: "webapp"},
		{name: "an ERT file", sel: ertAll(mod + "lisp/test-status.el"), want: "ert"},
		{name: "a whole harness", sel: harness(mod + "bin/test-test-all.sh"), want: "orchestrator-harness"},
		{name: "a whole suite", sel: suiteSel("webapp"), want: "webapp"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			r, err := Resolve(request(tc.sel), layout)

			// Assert
			if err != nil || !reflect.DeepEqual(names(r.Suites), []string{tc.want}) {
				t.Fatalf("Resolve = %v, %v; want suite %s", names(r.Suites), err, tc.want)
			}
		})
	}
}

func TestResolveRefuses(t *testing.T) {
	cases := []struct {
		name string
		req  *testrunv1.TestRunRequest
		want string
	}{
		{name: "no selection", req: request(), want: "names no test"},
		{name: "a selection with no target", req: request(&testrunv1.TestSelection{}), want: "names no target"},
		{name: "a directory no suite runs", req: request(goAll("docs/nowhere")), want: "no suite in the roster runs it"},
		{name: "an absolute path", req: request(goAll("/r/" + mod + "daemon")), want: "not a path relative to the repository root"},
		{name: "a script no harness suite runs", req: request(harness(mod + "bin/nope.sh")), want: "no suite in the roster runs it"},
		{name: "a Go package saying neither all nor which", req: request(&testrunv1.TestSelection{Target: &testrunv1.TestSelection_GoPackage{GoPackage: &testrunv1.TestSelectionGoPackage{PackageDir: mod + "daemon"}}}), want: "neither all of its tests nor which"},
		{name: "a Go package naming no test", req: request(goNamed(mod + "daemon")), want: "names no test"},
		{name: "a top-level name with a slash", req: request(goNamed(mod+"daemon", whole("TestA/x"))), want: "not a top-level test name"},
		{name: "an empty subtest segment", req: request(goNamed(mod+"daemon", sub("TestA", []string{"x", ""}))), want: "empty subtest path"},
		{name: "a test saying neither whole nor subtests", req: request(goNamed(mod+"daemon", &testrunv1.TestSelectionGoTest{Name: "TestA"})), want: "neither whole nor which subtests"},
		{name: "a vitest file naming an empty test", req: request(vitestNamed(mod+"webapp/src/a.test.ts", " ")), want: "names an empty test"},
		{name: "a suite the roster lacks", req: request(goAll(mod+"daemon"), suiteSel("nope")), want: `suite "nope": no such suite`},
		{name: "a script several suites run", req: request(harness(mod + "bin/coverage-honesty.sh")), want: "several suites run it (webapp-coverage-honesty, shim-coverage-honesty); select one by its suite"},
		{name: "every suite beside a narrowed target", req: request(goAll(mod+"daemon"), everySuite()), want: "the request's one selection"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			_, err := Resolve(tc.req, layout)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("Resolve = %v, want an error containing %q", err, tc.want)
			}
		})
	}
}

// chunkArgv builds a split's first chunk and answers its command line.
func chunkArgv(sp suites.Split) []string {
	return sp.Chunk(sp.Group+"#00", sp.Items).Argv
}

// goUnits is a Go suite's units for the package rel and one other: a vet,
// the module build over both, each package's split, and a program rel's
// tests run.
func goUnits(suite, rel string) suites.Units {
	build := suites.GoBuild{ID: suites.ModuleBuildID(suite, ""), Suite: suite,
		Options: gobuild.Options{Module: "/m", BinDir: "/bin", Self: "/testrun"}}
	vet := run.Spec{Unit: sched.Unit{ID: suite + ":vet"}}
	named := func(id string, items []string, names map[string][]string) run.Spec {
		return run.Spec{Argv: append(append([]string{"run"}, items...), suites.FlatNames(items, names)...)}
	}
	split := suites.Split{
		Group: suite + ":" + rel, Suite: suite, Items: []string{"TestA", "TestB", "TestC"}, Package: rel,
		Deps:  []string{build.ID, suites.ProgramUnitID("fakeshim")},
		Named: named, Chunk: func(id string, items []string) run.Spec { return named(id, items, nil) },
	}
	other := split
	other.Group, other.Items, other.Package = suite+":other", []string{"TestZ"}, "other"
	return suites.Units{Atomic: []run.Spec{vet}, Splits: []suites.Split{split, other}, Programs: []string{"fakeshim", "git"}, GoBuilds: []suites.GoBuild{build}}.Finish()
}

// builtPackages are the packages u's module build units build.
func builtPackages(t *testing.T, u suites.Units) [][]string {
	t.Helper()
	var out [][]string
	for _, a := range u.Atomic {
		if len(a.Argv) < 2 || a.Argv[1] != "build-tests" {
			continue
		}
		o, err := gobuild.ParseBuildArgs(a.Argv[2:])
		if err != nil {
			t.Fatalf("build unit %s: %v", a.ID, err)
		}
		out = append(out, o.Packages)
	}
	return out
}

func TestNarrowRunsOnlyTheNamedGoTests(t *testing.T) {
	// Arrange
	r, err := Resolve(request(goNamed(mod+"daemon/internal/merge", whole("TestA"), sub("TestB", []string{"case", "inner"}))), layout)
	if err != nil {
		t.Fatal(err)
	}
	u := goUnits("daemon", "internal/merge")

	// Act
	got, err := r.Narrow(suiteNamed(t, "daemon"), u)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(got.Splits) != 2 {
		t.Fatalf("splits = %+v, want the whole test's and the subtest path's", got.Splits)
	}
	if !reflect.DeepEqual(got.Splits[0].Items, []string{"TestA"}) || !reflect.DeepEqual(chunkArgv(got.Splits[0]), []string{"run", "TestA"}) {
		t.Fatalf("whole split = %v running %v, want TestA alone", got.Splits[0].Items, chunkArgv(got.Splits[0]))
	}
	if got.Splits[1].Group != "daemon:internal/merge/TestB/case/inner" || !reflect.DeepEqual(chunkArgv(got.Splits[1]), []string{"run", "TestB", "TestB/case/inner"}) {
		t.Fatalf("subtest split = %s running %v, want TestB/case/inner alone", got.Splits[1].Group, chunkArgv(got.Splits[1]))
	}
	if len(got.Atomic) != 1 || got.Atomic[0].ID != "daemon:build" || !reflect.DeepEqual(builtPackages(t, got), [][]string{{"internal/merge"}}) {
		t.Fatalf("atomic = %v, want only the module build, over the named package alone (no vet)", got.Atomic)
	}
	if !reflect.DeepEqual(got.Programs, []string{"fakeshim"}) {
		t.Fatalf("programs = %v, want only the one the package's tests run", got.Programs)
	}
}

func TestNarrowToOneSubtestBuildsItsPackage(t *testing.T) {
	// Arrange
	r, err := Resolve(request(goNamed(mod+"daemon/cmd/agent-repld", sub("TestB", []string{"an_accepted_move"}))), layout)
	if err != nil {
		t.Fatal(err)
	}

	// Act
	got, err := r.Narrow(suiteNamed(t, "daemon"), goUnits("daemon", "cmd/agent-repld"))

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(got.Splits) != 1 || got.Splits[0].Group != "daemon:cmd/agent-repld/TestB/an_accepted_move" {
		t.Fatalf("splits = %+v, want the subtest path's own", got.Splits)
	}
	if b := builtPackages(t, got); !reflect.DeepEqual(b, [][]string{{"cmd/agent-repld"}}) {
		t.Fatalf("builds = %v, want the subtest's package built", b)
	}
}

func TestAWholeTestSubsumesItsSubtests(t *testing.T) {
	// Arrange
	r, err := Resolve(request(goNamed(mod+"daemon/internal/merge", sub("TestA", []string{"x"}), whole("TestA"))), layout)
	if err != nil {
		t.Fatal(err)
	}

	// Act
	got, err := r.Narrow(suiteNamed(t, "daemon"), goUnits("daemon", "internal/merge"))

	// Assert
	if err != nil || len(got.Splits) != 1 || !reflect.DeepEqual(got.Splits[0].Items, []string{"TestA"}) {
		t.Fatalf("splits = %+v, %v; want TestA whole, once", got.Splits, err)
	}
}

func TestAPackageSelectedTwiceRunsOnce(t *testing.T) {
	// Arrange
	r, err := Resolve(request(goAll(mod+"daemon/internal/merge"), goAll(mod+"daemon/internal/merge")), layout)
	if err != nil {
		t.Fatal(err)
	}

	// Act
	got, err := r.Narrow(suiteNamed(t, "daemon"), goUnits("daemon", "internal/merge"))

	// Assert
	if err != nil || len(got.Splits) != 1 || len(got.Splits[0].Items) != 3 {
		t.Fatalf("splits = %+v, %v; want the package's one split, whole", got.Splits, err)
	}
}

func TestNarrowRefuses(t *testing.T) {
	cases := []struct {
		name  string
		sel   *testrunv1.TestSelection
		suite string
		units suites.Units
		want  string
	}{
		{name: "a test the package does not define", sel: goNamed(mod+"daemon/internal/merge", whole("TestNope")), suite: "daemon", units: goUnits("daemon", "internal/merge"), want: "defines no test TestNope"},
		{name: "a package with no tests", sel: goAll(mod + "daemon/internal/empty"), suite: "daemon", units: goUnits("daemon", "internal/merge"), want: "holds no test in it"},
		{name: "a vitest file its suite does not run", sel: vitestNamed(mod+"webapp/src/nope.test.ts", "x"), suite: "webapp", units: suites.Units{Splits: []suites.Split{{Group: "webapp", Items: []string{"src/a.test.ts"}}}}, want: "its suite does not run it"},
		{name: "groups of a harness that has none", sel: harness(mod+"bin/test-test-all.sh", "g"), suite: "orchestrator-harness", units: suites.Units{Atomic: []run.Spec{{Unit: sched.Unit{ID: "orchestrator-harness"}}}}, want: "has no groups to name"},
		{name: "a group the harness does not list", sel: harness(mod+"bin/test-build-frontend.sh", "nope"), suite: "build-frontend-harness", units: suites.Units{Splits: []suites.Split{{Group: "build-frontend-harness", Items: []string{"a", "b"}}}}, want: "has no group nope"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			r, err := Resolve(request(tc.sel), layout)
			if err != nil {
				t.Fatal(err)
			}

			// Act
			_, err = r.Narrow(suiteNamed(t, tc.suite), tc.units)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("Narrow = %v, want an error containing %q", err, tc.want)
			}
		})
	}
}

func TestNarrowRunsANamedVitestFileWithItsNames(t *testing.T) {
	// Arrange
	r, err := Resolve(request(vitestNamed(mod+"webapp/src/a.test.ts", "suite one", "suite two case")), layout)
	if err != nil {
		t.Fatal(err)
	}
	named := func(id string, items []string, names map[string][]string) run.Spec {
		return run.Spec{Argv: append(append([]string{"vitest"}, items...), suites.FlatNames(items, names)...)}
	}
	u := suites.Units{
		Atomic: []run.Spec{{Unit: sched.Unit{ID: "webapp:typecheck"}}},
		Splits: []suites.Split{{Group: "webapp", Items: []string{"src/a.test.ts", "src/b.test.ts"}, Named: named,
			Chunk: func(id string, items []string) run.Spec { return named(id, items, nil) }}},
	}

	// Act
	got, err := r.Narrow(suiteNamed(t, "webapp"), u)

	// Assert
	if err != nil || len(got.Splits) != 1 || len(got.Atomic) != 0 {
		t.Fatalf("Narrow = %+v, %v; want one split and no typecheck", got, err)
	}
	if argv := chunkArgv(got.Splits[0]); !reflect.DeepEqual(argv, []string{"vitest", "src/a.test.ts", "suite one", "suite two case"}) {
		t.Fatalf("chunk = %v, want the file with its names", argv)
	}
}

func TestNarrowKeepsAWholeHarnessUnit(t *testing.T) {
	// Arrange
	r, err := Resolve(request(harness(mod+"bin/test-test-all.sh")), layout)
	if err != nil {
		t.Fatal(err)
	}
	u := suites.Units{Atomic: []run.Spec{{Unit: sched.Unit{ID: "orchestrator-harness"}}}}

	// Act
	got, err := r.Narrow(suiteNamed(t, "orchestrator-harness"), u)

	// Assert
	if err != nil || len(got.Atomic) != 1 || got.Atomic[0].ID != "orchestrator-harness" {
		t.Fatalf("Narrow = %+v, %v; want the harness's unit", got, err)
	}
}

func TestNarrowRunsNamedHarnessGroups(t *testing.T) {
	// Arrange
	r, err := Resolve(request(harness(mod+"bin/test-build-frontend.sh", "b")), layout)
	if err != nil {
		t.Fatal(err)
	}
	u := suites.Units{Splits: []suites.Split{{Group: "build-frontend-harness", Items: []string{"a", "b"},
		Chunk: func(id string, items []string) run.Spec { return run.Spec{Argv: items} }}}}

	// Act
	got, err := r.Narrow(suiteNamed(t, "build-frontend-harness"), u)

	// Assert
	if err != nil || len(got.Splits) != 1 || !reflect.DeepEqual(got.Splits[0].Items, []string{"b"}) {
		t.Fatalf("Narrow = %+v, %v; want group b alone", got, err)
	}
}

func TestWhole(t *testing.T) {
	cases := []struct {
		name      string
		req       *testrunv1.TestRunRequest
		wantNames []string
		wantOK    bool
	}{
		{name: "named suites, each once", req: request(suiteSel("webapp"), suiteSel("daemon"), suiteSel("webapp")), wantNames: []string{"webapp", "daemon"}, wantOK: true},
		{name: "every suite", req: request(everySuite()), wantNames: nil, wantOK: true},
		{name: "a narrowed target is no whole-suite request", req: request(suiteSel("webapp"), goAll(mod+"daemon")), wantNames: nil, wantOK: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, ok, err := Whole(tc.req)

			// Assert
			if err != nil || ok != tc.wantOK || !reflect.DeepEqual(got, tc.wantNames) {
				t.Fatalf("Whole = %v, %v, %v; want %v, %v", got, ok, err, tc.wantNames, tc.wantOK)
			}
		})
	}
}

func TestWholeRefuses(t *testing.T) {
	cases := []struct {
		name string
		req  *testrunv1.TestRunRequest
		want string
	}{
		{name: "no selection", req: request(), want: "names no test"},
		{name: "a suite the roster lacks", req: request(suiteSel("nope")), want: `suite "nope": no such suite`},
		{name: "every suite beside a named suite", req: request(everySuite(), suiteSel("webapp")), want: "the request's one selection"},
		{name: "every suite beside a narrowed target", req: request(everySuite(), goAll(mod+"daemon")), want: "the request's one selection"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			_, _, err := Whole(tc.req)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("Whole = %v, want an error containing %q", err, tc.want)
			}
		})
	}
}

func TestNarrowKeepsAWholeSuiteNamedBesideItsTests(t *testing.T) {
	// Arrange
	r, err := Resolve(request(goNamed(mod+"daemon/internal/merge", whole("TestA")), suiteSel("daemon")), layout)
	if err != nil {
		t.Fatal(err)
	}
	u := goUnits("daemon", "internal/merge")

	// Act
	got, err := r.Narrow(suiteNamed(t, "daemon"), u)

	// Assert
	groups := func(u suites.Units) []string {
		var out []string
		for _, sp := range u.Splits {
			out = append(out, sp.Group+"="+strings.Join(sp.Items, ","))
		}
		for _, a := range u.Atomic {
			out = append(out, a.ID)
		}
		return append(out, u.Programs...)
	}
	if err != nil || !reflect.DeepEqual(groups(got), groups(u)) {
		t.Fatalf("Narrow = %v, %v; want every unit of the suite, %v", groups(got), err, groups(u))
	}
}

func ertNamed(file string, names ...string) *testrunv1.TestSelection {
	return &testrunv1.TestSelection{Target: &testrunv1.TestSelection_ErtFile{ErtFile: &testrunv1.TestSelectionErtFile{
		File: file, Tests: &testrunv1.TestSelectionErtFile_Named{Named: &testrunv1.TestSelectionErtNamedTests{TestNames: names}}}}}
}

// ertUnits is the ERT suite's units as suites.fileSplits makes them: one
// split per file, grouped "ert/<file>", running its file and named tests.
func ertUnits(files ...string) suites.Units {
	var u suites.Units
	for _, f := range files {
		u.Splits = append(u.Splits, suites.Split{Group: "ert/" + f, Suite: "ert", Items: []string{f}, Named: ertNamedChunk, ScopesNamesPerItem: true,
			Chunk: func(id string, items []string) run.Spec { return ertNamedChunk(id, items, nil) }})
	}
	return u
}

// ertNamedChunk is an ERT chunk's argv: each item followed by its own names,
// as the driver's alist pairs them.
func ertNamedChunk(id string, items []string, names map[string][]string) run.Spec {
	argv := []string{"emacs"}
	for _, item := range items {
		argv = append(argv, item)
		argv = append(argv, names[item]...)
	}
	return run.Spec{Argv: argv}
}

func TestNarrowRunsANamedERTFileUnderItsOwnGroup(t *testing.T) {
	cases := []struct {
		name string
		sels []*testrunv1.TestSelection
		want string
		argv []string
	}{
		{name: "a path inside the worktree", sels: []*testrunv1.TestSelection{ertNamed(mod+"lisp/test-wire-verbs.el", "case-a")},
			want: "ert/test-wire-verbs.el", argv: []string{"emacs", "test-wire-verbs.el", "case-a"}},
		{name: "a bare file name", sels: []*testrunv1.TestSelection{ertNamed("test-wire-verbs.el", "case-a")},
			want: "ert/test-wire-verbs.el", argv: []string{"emacs", "test-wire-verbs.el", "case-a"}},
		{name: "a path and a bare name of one file fold their names", sels: []*testrunv1.TestSelection{ertNamed(mod+"lisp/test-wire-verbs.el", "case-a"), ertNamed("test-wire-verbs.el", "case-b")},
			want: "ert/test-wire-verbs.el", argv: []string{"emacs", "test-wire-verbs.el", "case-a", "case-b"}},
		{name: "a whole file beside a named one runs the whole file", sels: []*testrunv1.TestSelection{ertNamed("test-wire-verbs.el", "case-a"), ertAll(mod + "lisp/test-wire-verbs.el")},
			want: "ert/test-wire-verbs.el", argv: []string{"emacs", "test-wire-verbs.el"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			r, err := Resolve(request(tc.sels...), layout)
			if err != nil {
				t.Fatal(err)
			}

			// Act
			got, err := r.Narrow(suiteNamed(t, "ert"), ertUnits("test-status.el", "test-wire-verbs.el"))

			// Assert
			if err != nil || len(got.Splits) != 1 {
				t.Fatalf("Narrow = %+v, %v; want the one file's split", got, err)
			}
			if got.Splits[0].Group != tc.want {
				t.Fatalf("group = %q, want %q: the reuse step reads the file off the group", got.Splits[0].Group, tc.want)
			}
			if argv := chunkArgv(got.Splits[0]); !reflect.DeepEqual(argv, tc.argv) {
				t.Fatalf("chunk = %v, want %v", argv, tc.argv)
			}
		})
	}
}

func TestResolveRefusesAnERTFileItsSuiteDoesNotRun(t *testing.T) {
	cases := []struct {
		name string
		sel  *testrunv1.TestSelection
		want string
	}{
		{name: "a file below the suite's directory", sel: ertAll(mod + "lisp/testsupport/test-x.el"), want: "its suite does not run it"},
		{name: "a bare name the suite does not load", sel: ertAll("test-nope.el"), want: "its suite does not run it"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			r, err := Resolve(request(tc.sel), layout)
			if err != nil {
				t.Fatal(err)
			}

			// Act
			_, err = r.Narrow(suiteNamed(t, "ert"), ertUnits("test-wire-verbs.el"))

			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("Narrow error = %v, want %q", err, tc.want)
			}
		})
	}
}

func TestDescribe(t *testing.T) {
	cases := []struct {
		name string
		req  *testrunv1.TestRunRequest
		want string
	}{
		{name: "a named ERT file", req: request(ertNamed("test-x.el", "a-test", "b-test")), want: "ERT file test-x.el=a-test,b-test"},
		{name: "a whole ERT file", req: request(ertAll(mod + "lisp/test-x.el")), want: "ERT file " + mod + "lisp/test-x.el"},
		{name: "Go tests and a subtest path", req: request(goNamed("daemon/internal/merge", whole("TestA"), sub("TestB", []string{"case", "inner"}))), want: "Go package daemon/internal/merge=TestA,TestB/case/inner"},
		{name: "a named vitest file", req: request(vitestNamed("webapp/src/a.test.ts", "one")), want: "vitest file webapp/src/a.test.ts=one"},
		{name: "harness groups", req: request(harness("bin/t.sh", "g1")), want: "harness bin/t.sh=g1"},
		{name: "several targets in order", req: request(suiteSel("daemon"), everySuite()), want: "suite daemon; every suite"},
		{name: "no selection", req: request(), want: "no test"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := Describe(tc.req)

			// Assert
			if got != tc.want {
				t.Fatalf("Describe = %q, want %q", got, tc.want)
			}
		})
	}
}

// batchedUnits is one split holding FILES as one batch, grouped BATCH, of a
// suite that SCOPES its names per file or not.
func batchedUnits(batch string, scopes bool, files ...string) suites.Units {
	return suites.Units{Splits: []suites.Split{{Group: batch, Suite: "ert", Items: files, Named: ertNamedChunk, ScopesNamesPerItem: scopes,
		Chunk: func(id string, items []string) run.Spec { return ertNamedChunk(id, items, nil) }}}}
}

func TestNarrowScopesEachFilesNamesWithinOneBatch(t *testing.T) {
	cases := []struct {
		name   string
		sels   []*testrunv1.TestSelection
		scopes bool
		want   map[string][]string
	}{
		{name: "two named files of a scoping suite share the batch, each with its own names",
			sels:   []*testrunv1.TestSelection{ertNamed("test-wire-verbs.el", "case-a"), ertNamed("test-wire-common.el", "case-b")},
			scopes: true,
			want:   map[string][]string{"ert/wire": {"emacs", "test-wire-verbs.el", "case-a", "test-wire-common.el", "case-b"}}},
		{name: "a whole file beside a named one of a scoping suite runs whole",
			sels:   []*testrunv1.TestSelection{ertNamed("test-wire-verbs.el", "case-a"), ertAll("test-wire-common.el")},
			scopes: true,
			want:   map[string][]string{"ert/wire": {"emacs", "test-wire-verbs.el", "case-a", "test-wire-common.el"}}},
		{name: "two named files of a suite with one pattern each take a group of their own",
			sels:   []*testrunv1.TestSelection{ertNamed("test-wire-verbs.el", "case-a"), ertNamed("test-wire-common.el", "case-b")},
			scopes: false,
			want: map[string][]string{"ert/wire/test-wire-verbs.el": {"emacs", "test-wire-verbs.el", "case-a"},
				"ert/wire/test-wire-common.el": {"emacs", "test-wire-common.el", "case-b"}}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			r, err := Resolve(request(tc.sels...), layout)
			if err != nil {
				t.Fatal(err)
			}

			// Act
			got, err := r.Narrow(suiteNamed(t, "ert"), batchedUnits("ert/wire", tc.scopes, "test-wire-common.el", "test-wire-verbs.el"))

			// Assert
			if err != nil {
				t.Fatalf("Narrow: %v", err)
			}
			argvs := map[string][]string{}
			for _, sp := range got.Splits {
				argvs[sp.Group] = chunkArgv(sp)
			}
			if !reflect.DeepEqual(argvs, tc.want) {
				t.Fatalf("chunks = %v, want %v", argvs, tc.want)
			}
		})
	}
}

func TestMergeGroupsPanicsOnNamedSplitsOfASuiteThatCannotScopeThem(t *testing.T) {
	// Arrange: two named splits forced into one group of a suite with one
	// pattern, which narrowItems never builds.
	a := withItems(batchedUnits("g", false, "a").Splits[0], "g", []string{"a"}, []string{"x"})
	b := withItems(batchedUnits("g", false, "b").Splits[0], "g", []string{"b"}, []string{"y"})
	defer func() {
		if recover() == nil {
			t.Fatal("mergeGroups folded two named splits whose suite cannot scope names per item")
		}
	}()

	// Act
	mergeGroups([]suites.Split{a, b})
}

func TestANarrowedChunkNamesItsTestsAndAWholeOneDoesNot(t *testing.T) {
	// Arrange
	r, err := Resolve(request(ertNamed("test-wire-verbs.el", "case-a"), ertAll("test-wire-common.el")), layout)
	if err != nil {
		t.Fatal(err)
	}

	// Act
	named, err := r.Narrow(suiteNamed(t, "ert"), batchedUnits("ert/wire", true, "test-wire-common.el", "test-wire-verbs.el"))
	if err != nil {
		t.Fatal(err)
	}
	r2, err := Resolve(request(ertAll("test-wire-common.el")), layout)
	if err != nil {
		t.Fatal(err)
	}
	whole, err := r2.Narrow(suiteNamed(t, "ert"), batchedUnits("ert/wire", true, "test-wire-common.el", "test-wire-verbs.el"))
	if err != nil {
		t.Fatal(err)
	}

	// Assert
	if s := named.Splits[0].Chunk("ert/wire#00", named.Splits[0].Items); !s.NamesTests {
		t.Fatalf("a chunk narrowed to named tests is not marked so")
	}
	if s := whole.Splits[0].Chunk("ert/wire#00", whole.Splits[0].Items); s.NamesTests {
		t.Fatalf("a whole file's chunk is marked as naming tests")
	}
}
