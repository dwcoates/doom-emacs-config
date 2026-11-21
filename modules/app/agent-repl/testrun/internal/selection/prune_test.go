package selection

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	testrunv1 "agentrepl/proto/testrun/v1"

	"google.golang.org/protobuf/proto"

	"agentrepl/testrun/internal/suites"
	"agentrepl/testrun/roster"
)

// fakeLister answers listings from fixed tables, counting its vitest calls.
type fakeLister struct {
	goTests     map[string][]string
	vitest      map[string]map[string][]string // by config
	ert         map[string][]string
	groups      map[string][]string
	err         error
	errOn       string
	vitestCalls int
}

func (f *fakeLister) fail(kind string) error {
	if f.errOn == kind {
		return f.err
	}
	return nil
}

func (f *fakeLister) GoTests(dir, _ string) ([]string, error) {
	return f.goTests[filepath.Base(dir)], f.fail("go")
}

func (f *fakeLister) VitestNames(_, _, config string, files []string) (map[string][]string, error) {
	f.vitestCalls++
	out := map[string][]string{}
	for _, file := range files {
		if names, ok := f.vitest[config][file]; ok {
			out[file] = names
		}
	}
	return out, f.fail("vitest")
}

func (f *fakeLister) ERTNames(_, _ string, files []string) (map[string][]string, error) {
	out := map[string][]string{}
	for _, file := range files {
		out[file] = f.ert[file]
	}
	return out, f.fail("ert")
}

func (f *fakeLister) HarnessGroups(_ suites.Layout, s roster.Suite) ([]string, error) {
	return f.groups[s.Name], f.fail("harness")
}

// pruneTree is a repository holding the files a rerun's cases name, laid out
// where the roster's suites keep them.
func pruneTree(t *testing.T) suites.Layout {
	t.Helper()
	repo := t.TempDir()
	module := filepath.Join(repo, "modules", "app", "agent-repl")
	files := map[string]string{
		"daemon/internal/merge/merge_test.go": "package merge\n",
		"daemon/internal/empty/empty.go":      "package empty\n",
		"webapp/test/feed.test.ts":            "",
		"webapp/test/uncollected.test.ts":     "",
		"lisp/test-agent-repl.el":             `(load (expand-file-name "test-helpers.el" dir)) (load (expand-file-name "test-wire.el" dir))`,
		"lisp/test-helpers.el":                "",
		"lisp/test-wire.el":                   "",
		"lisp/test-orphan.el":                 "",
		"bin/test-readiness-report.sh":        "",
		"bin/test-background.sh":              "",
	}
	for rel, body := range files {
		path := filepath.Join(module, filepath.FromSlash(rel))
		if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(path, []byte(body), 0o755); err != nil {
			t.Fatal(err)
		}
	}
	return suites.Layout{Repo: repo, Module: module}
}

// aLister is the listing of pruneTree's cases as they stand.
func aLister() *fakeLister {
	return &fakeLister{
		goTests: map[string][]string{"merge": {"TestLands", "TestFails"}},
		vitest: map[string]map[string][]string{
			"": {"test/feed.test.ts": {"feed draws a row", "feed scrolls"}},
		},
		ert:    map[string][]string{"test-wire.el": {"test-wire-decodes"}},
		groups: map[string][]string{"readiness-harness": {"report", "summary"}},
	}
}

func vitestAll(file string) *testrunv1.TestSelection {
	return &testrunv1.TestSelection{Target: &testrunv1.TestSelection_VitestFile{VitestFile: &testrunv1.TestSelectionVitestFile{
		File: file, Tests: &testrunv1.TestSelectionVitestFile_All{All: &testrunv1.TestSelectionVitestAllTests{}}}}}
}

func TestPruneKeepsOrDropsEachCase(t *testing.T) {
	tests := []struct {
		name       string
		sel        *testrunv1.TestSelection
		wantDetail string // "" keeps the case
	}{
		{name: "a Go package that holds tests is kept", sel: goAll(mod + "daemon/internal/merge")},
		{name: "a Go test the package defines is kept", sel: goNamed(mod+"daemon/internal/merge", whole("TestLands"))},
		{name: "a renamed Go test is dropped", sel: goNamed(mod+"daemon/internal/merge", whole("TestLandsOld")), wantDetail: "no longer defines TestLandsOld"},
		{name: "a deleted Go package is dropped", sel: goAll(mod + "daemon/internal/gone"), wantDetail: "no longer in the tree"},
		{name: "a Go package with no test left is dropped", sel: goAll(mod + "daemon/internal/empty"), wantDetail: "holds no test any more"},
		{name: "a vitest test the file collects is kept", sel: vitestNamed(mod+"webapp/test/feed.test.ts", "feed draws a row")},
		{name: "a vitest describe block the file collects is kept", sel: vitestNamed(mod+"webapp/test/feed.test.ts", "feed")},
		{name: "a whole vitest file the suite collects is kept", sel: vitestAll(mod + "webapp/test/feed.test.ts")},
		{name: "a renamed vitest test is dropped", sel: vitestNamed(mod+"webapp/test/feed.test.ts", "feed draws a column"), wantDetail: `no longer defines "feed draws a column"`},
		{name: "a name that only prefixes a word of a test is dropped", sel: vitestNamed(mod+"webapp/test/feed.test.ts", "feed draw"), wantDetail: "no longer defines"},
		{name: "a deleted vitest file is dropped", sel: vitestAll(mod + "webapp/test/gone.test.ts"), wantDetail: "no longer in the tree"},
		{name: "a vitest file no config collects is dropped", sel: vitestAll(mod + "webapp/test/uncollected.test.ts"), wantDetail: "collects no test in it"},
		{name: "an ERT file the roster loads is kept", sel: ertAll("test-wire.el")},
		{name: "an ERT test the file defines is kept", sel: ertNamed("test-wire.el", "test-wire-decodes")},
		{name: "a renamed ERT test is dropped", sel: ertNamed("test-wire.el", "test-wire-encodes"), wantDetail: "no longer defines test-wire-encodes"},
		{name: "an ERT file the roster no longer loads is dropped", sel: ertAll("test-orphan.el"), wantDetail: "roster no longer loads it"},
		{name: "a deleted ERT file is dropped", sel: ertAll("test-gone.el"), wantDetail: "no longer in the tree"},
		{name: "a harness group it lists is kept", sel: harness(mod+"bin/test-readiness-report.sh", "report")},
		{name: "a whole harness is kept", sel: harness(mod + "bin/test-background.sh")},
		{name: "a harness group it no longer lists is dropped", sel: harness(mod+"bin/test-readiness-report.sh", "gone"), wantDetail: "no longer lists the group gone"},
		{name: "a group of a harness that lists none is dropped", sel: harness(mod+"bin/test-background.sh", "report"), wantDetail: "no longer lists groups"},
		{name: "a deleted harness is dropped", sel: harness(mod + "bin/test-gone.sh"), wantDetail: "no longer in the tree"},
		{name: "a roster suite is kept", sel: suiteSel("daemon")},
		{name: "a suite the roster dropped is dropped", sel: suiteSel("retired-suite"), wantDetail: "no such suite any more"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			l := pruneTree(t)
			req := &testrunv1.TestRunRequest{Selections: []*testrunv1.TestSelection{tt.sel}, DropMissing: true}

			// Act
			kept, dropped, err := Prune(req, l, aLister())

			// Assert
			if err != nil {
				t.Fatalf("Prune: %v", err)
			}
			if tt.wantDetail == "" {
				if len(dropped) != 0 || len(kept.GetSelections()) != 1 || !proto.Equal(kept.GetSelections()[0], tt.sel) {
					t.Fatalf("kept %v, dropped %v; want the case kept as sent", kept.GetSelections(), dropped)
				}
				return
			}
			if len(kept.GetSelections()) != 0 || len(dropped) != 1 || !proto.Equal(dropped[0].Selection, tt.sel) || !strings.Contains(dropped[0].Detail, tt.wantDetail) {
				t.Fatalf("kept %v, dropped %+v; want the case dropped as sent, saying %q", kept.GetSelections(), dropped, tt.wantDetail)
			}
		})
	}
}

func TestPruneKeepsTheRequestsOptionsAndOrder(t *testing.T) {
	// Arrange
	l := pruneTree(t)
	a, gone, b := goNamed(mod+"daemon/internal/merge", whole("TestLands")), suiteSel("retired-suite"), ertAll("test-wire.el")
	req := &testrunv1.TestRunRequest{Selections: []*testrunv1.TestSelection{a, gone, b}, DropMissing: true, PlanOnly: true, Remeasure: true}

	// Act
	kept, dropped, err := Prune(req, l, aLister())

	// Assert
	want := &testrunv1.TestRunRequest{Selections: []*testrunv1.TestSelection{a, b}, DropMissing: true, PlanOnly: true, Remeasure: true}
	if err != nil || !proto.Equal(kept, want) || len(dropped) != 1 {
		t.Fatalf("Prune = %v, %v, %v; want %v with one dropped", kept, dropped, err, want)
	}
}

func TestPruneListsAVitestPackageOncePerConfig(t *testing.T) {
	// Arrange
	l := pruneTree(t)
	list := aLister()
	req := &testrunv1.TestRunRequest{DropMissing: true, Selections: []*testrunv1.TestSelection{
		vitestNamed(mod+"webapp/test/feed.test.ts", "feed draws a row"),
		vitestNamed(mod+"webapp/test/feed.test.ts", "feed scrolls"),
		vitestAll(mod + "webapp/test/uncollected.test.ts"),
	}}

	// Act
	_, _, err := Prune(req, l, list)

	// Assert: the webapp suite has an integration config, so its default
	// and its integration config are each listed once.
	if err != nil || list.vitestCalls != 2 {
		t.Fatalf("Prune err %v, vitest listed %d times; want once per config", err, list.vitestCalls)
	}
}

func TestPruneFindsAVitestFileItsIntegrationConfigCollects(t *testing.T) {
	// Arrange
	l := pruneTree(t)
	list := aLister()
	list.vitest["vitest.integration.config.ts"] = map[string][]string{"test/uncollected.test.ts": {"integration boots"}}
	sel := vitestNamed(mod+"webapp/test/uncollected.test.ts", "integration boots")

	// Act
	kept, dropped, err := Prune(&testrunv1.TestRunRequest{DropMissing: true, Selections: []*testrunv1.TestSelection{sel}}, l, list)

	// Assert
	if err != nil || len(dropped) != 0 || len(kept.GetSelections()) != 1 {
		t.Fatalf("Prune = %v, %v, %v; want the integration file's case kept", kept, dropped, err)
	}
}

func TestPruneRefusesASelectionThatIsNotOneCase(t *testing.T) {
	tests := []struct {
		name string
		sel  *testrunv1.TestSelection
	}{
		{name: "two Go tests", sel: goNamed(mod+"daemon/internal/merge", whole("TestLands"), whole("TestFails"))},
		{name: "a Go subtest path", sel: goNamed(mod+"daemon/internal/merge", sub("TestLands", []string{"a_case"}))},
		{name: "two vitest names", sel: vitestNamed(mod+"webapp/test/feed.test.ts", "feed scrolls", "feed draws a row")},
		{name: "two harness groups", sel: harness(mod+"bin/test-readiness-report.sh", "report", "summary")},
		{name: "every suite", sel: &testrunv1.TestSelection{Target: &testrunv1.TestSelection_EverySuite{EverySuite: &testrunv1.TestSelectionEverySuite{}}}},
		{name: "no target", sel: &testrunv1.TestSelection{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			l := pruneTree(t)

			// Act
			kept, dropped, err := Prune(&testrunv1.TestRunRequest{DropMissing: true, Selections: []*testrunv1.TestSelection{tt.sel}}, l, aLister())

			// Assert
			if !errors.Is(err, ErrNotOneCase) || kept != nil || dropped != nil {
				t.Fatalf("Prune = %v, %v, %v; want ErrNotOneCase and nothing else", kept, dropped, err)
			}
		})
	}
}

func TestPruneFailsOnAListingThatFailsRatherThanDropping(t *testing.T) {
	tests := []struct {
		name  string
		errOn string
		sel   *testrunv1.TestSelection
	}{
		{name: "the Go listing", errOn: "go", sel: goAll(mod + "daemon/internal/merge")},
		{name: "the vitest listing", errOn: "vitest", sel: vitestAll(mod + "webapp/test/feed.test.ts")},
		{name: "the ERT listing", errOn: "ert", sel: ertNamed("test-wire.el", "test-wire-decodes")},
		{name: "the harness listing", errOn: "harness", sel: harness(mod+"bin/test-readiness-report.sh", "report")},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			l := pruneTree(t)
			list := aLister()
			list.err, list.errOn = errors.New("the lister broke"), tt.errOn

			// Act
			kept, dropped, err := Prune(&testrunv1.TestRunRequest{DropMissing: true, Selections: []*testrunv1.TestSelection{tt.sel}}, l, list)

			// Assert
			if !errors.Is(err, list.err) || errors.Is(err, ErrNotOneCase) || kept != nil || dropped != nil {
				t.Fatalf("Prune = %v, %v, %v; want the listing's failure and nothing kept or dropped", kept, dropped, err)
			}
		})
	}
}
