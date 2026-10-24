package selection

import (
	"reflect"
	"strings"
	"testing"

	testrunv1 "agentrepl/proto/testrun/v1"

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

// goUnits is a Go suite's units for one package: a vet, the package's build,
// its split, and a program its tests run.
func goUnits(suite, rel string) suites.Units {
	build := run.Spec{Unit: sched.Unit{ID: suite + ":" + rel + ":build"}}
	vet := run.Spec{Unit: sched.Unit{ID: suite + ":vet"}}
	named := func(id string, items, names []string) run.Spec {
		return run.Spec{Argv: append(append([]string{"run"}, items...), names...)}
	}
	split := suites.Split{
		Group: suite + ":" + rel, Suite: suite, Items: []string{"TestA", "TestB", "TestC"},
		Deps:  []string{build.ID, suites.ProgramUnitID("fakeshim")},
		Named: named, Chunk: func(id string, items []string) run.Spec { return named(id, items, nil) },
	}
	other := split
	other.Group, other.Items = suite+":other", []string{"TestZ"}
	return suites.Units{Atomic: []run.Spec{vet, build}, Splits: []suites.Split{split, other}, Programs: []string{"fakeshim", "git"}}
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
	if len(got.Atomic) != 1 || got.Atomic[0].ID != "daemon:internal/merge:build" {
		t.Fatalf("atomic = %v, want only the package's build (no vet)", got.Atomic)
	}
	if !reflect.DeepEqual(got.Programs, []string{"fakeshim"}) {
		t.Fatalf("programs = %v, want only the one the package's tests run", got.Programs)
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
	named := func(id string, items, names []string) run.Spec {
		return run.Spec{Argv: append(append([]string{"vitest"}, items...), names...)}
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
