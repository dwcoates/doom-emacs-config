package sourcescan

import (
	"go/ast"
	"go/token"
	"os"
	"path/filepath"
	"reflect"
	"regexp"
	"slices"
	"sort"
	"strings"
	"testing"
)

// fakeTB records a Fatalf instead of ending the test, so the failure path is
// observable.
type fakeTB struct {
	testing.TB
	failed  bool
	message string
}

func (f *fakeTB) Helper() {}
func (f *fakeTB) Fatalf(format string, args ...any) {
	f.failed = true
	f.message = format
}

func fixturePackage(t *testing.T, files map[string]string) string {
	t.Helper()
	dir := t.TempDir()
	for name, body := range files {
		if err := os.WriteFile(filepath.Join(dir, name), []byte(body), 0o600); err != nil {
			t.Fatalf("WriteFile: %v", err)
		}
	}
	return dir
}

func TestProductionSkipsTestFilesAndSortsByName(t *testing.T) {
	// Arrange
	dir := fixturePackage(t, map[string]string{"b.go": "package p // b", "a.go": "package p // a", "a_test.go": "package p // t", "notes.txt": "x"})

	// Act
	files := Production(t, dir)

	// Assert
	if len(files) != 2 || files[0].Name != "a.go" || files[1].Name != "b.go" || string(files[0].Source) != "package p // a" {
		t.Fatalf("Production = %+v, want a.go then b.go with their sources", files)
	}
}

func TestProductionOfAPackageWithNoProductionSourceFails(t *testing.T) {
	// Arrange
	dir := fixturePackage(t, map[string]string{"only_test.go": "package p"})
	fake := &fakeTB{TB: t}

	// Act
	Production(fake, dir)

	// Assert
	if !fake.failed || !strings.Contains(fake.message, "no production .go files") {
		t.Fatalf("failed=%v message=%q, want the empty package refused", fake.failed, fake.message)
	}
}

func TestCountSumsAcrossProductionFilesAndIgnoresTests(t *testing.T) {
	// Arrange
	dir := fixturePackage(t, map[string]string{"a.go": "x.Rollback() x.Rollback()", "b.go": "x.Rollback()", "a_test.go": "x.Rollback()"})

	// Act
	n := Count(t, dir, ".Rollback()")

	// Assert
	if n != 3 {
		t.Fatalf("Count = %d, want 3 from production files only", n)
	}
}

func TestSitesNamesEachOccurrencesFileInOrderAndIgnoresTests(t *testing.T) {
	// Arrange
	dir := fixturePackage(t, map[string]string{"b.go": "x.Rollback()", "a.go": "x.Rollback() x.Rollback()", "a_test.go": "x.Rollback()"})

	// Act
	sites := Sites(t, dir, ".Rollback()")

	// Assert
	if !slices.Equal(sites, []string{"a.go", "a.go", "b.go"}) {
		t.Fatalf("Sites = %v, want a.go twice then b.go, from production files only", sites)
	}
}

func TestSitesOfAnAbsentPatternIsEmpty(t *testing.T) {
	// Arrange
	dir := fixturePackage(t, map[string]string{"a.go": "x.Commit()"})

	// Act
	sites := Sites(t, dir, ".Rollback()")

	// Assert
	if len(sites) != 0 {
		t.Fatalf("Sites = %v, want none", sites)
	}
}

func TestSitesMatchingNamesEachMatchsFileInOrderAndIgnoresTests(t *testing.T) {
	// Arrange
	dir := fixturePackage(t, map[string]string{"b.go": "at += ttl", "a.go": "at += ttl; at.Add(ttl); read(ttl)", "a_test.go": "at += ttl"})

	// Act
	sites := SitesMatching(t, dir, regexp.MustCompile(`(\+= |\.Add\()ttl`))

	// Assert
	if !slices.Equal(sites, []string{"a.go", "a.go", "b.go"}) {
		t.Fatalf("SitesMatching = %v, want a.go twice then b.go, from production files only", sites)
	}
}

func TestSitesMatchingAnAbsentPatternIsEmpty(t *testing.T) {
	// Arrange
	dir := fixturePackage(t, map[string]string{"a.go": "read(ttl)"})

	// Act
	sites := SitesMatching(t, dir, regexp.MustCompile(`\+= ttl`))

	// Assert
	if len(sites) != 0 {
		t.Fatalf("SitesMatching = %v, want none", sites)
	}
}

func TestSitesAndSitesMatchingShareOneList(t *testing.T) {
	// Arrange: this package's own directory.
	dir, err := filepath.Abs(".")
	if err != nil {
		t.Fatalf("Abs: %v", err)
	}

	// Act
	shared := Count(t, dir, "return sitesBy(t, dir, ")
	own := Count(t, dir, "sites = append(sites, f.Name)")

	// Assert
	if shared != 2 || own != 1 {
		t.Fatalf("sitesBy is returned %d times and a site appended %d times; want Sites and SitesMatching both through sitesBy's one loop", shared, own)
	}
}

// Every one-home test lists its sites through Sites, so no hand-rolled copy
// can drift from it.
func TestNoDaemonTestHandRollsASitesList(t *testing.T) {
	// Arrange
	root := DaemonRoot(t)
	var offenders []string

	// Act
	err := filepath.WalkDir(root, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, "_test.go") || strings.Contains(path, string(filepath.Separator)+"sourcescan"+string(filepath.Separator)) {
			return nil
		}
		body, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(body), "sites = append(sites, file.Name)") {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("WalkDir: %v", err)
	}
	if len(offenders) != 0 {
		t.Fatalf("these tests list their sites by hand instead of with sourcescan.Sites: %v", offenders)
	}
}

// Every daemon test that reads its package's production sources goes through
// this package, so no hand-rolled copy can drift from the skip-the-tests rule.
func TestNoDaemonTestHandRollsAProductionSourceScan(t *testing.T) {
	// Arrange
	root := DaemonRoot(t)
	var offenders []string

	// Act
	err := filepath.WalkDir(root, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, "_test.go") || strings.Contains(path, string(filepath.Separator)+"sourcescan"+string(filepath.Separator)) {
			return nil
		}
		body, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(body), `filepath.Glob("*.go")`) {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("WalkDir: %v", err)
	}
	if len(offenders) != 0 {
		t.Fatalf("these tests glob their own sources instead of using sourcescan: %v", offenders)
	}
}

func TestEachDaemonSourceVisitsOnlyServingProductionSources(t *testing.T) {
	cases := []struct {
		name string
		skip []string
		want []string
	}{
		{name: "the harnesses, reposcans, tests, hidden directories and other files are skipped", want: []string{"cmd/main.go", "internal/owner/z.go", "internal/pkg/b.go"}},
		{name: "a named directory is skipped too", skip: []string{"internal/owner"}, want: []string{"cmd/main.go", "internal/pkg/b.go"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			root := t.TempDir()
			for _, rel := range []string{
				"cmd/main.go", "internal/pkg/b.go", "internal/owner/z.go",
				"internal/pkg/b_test.go", "integration/x.go", "e2e/y.go", ".hidden/w.go", "internal/pkg/notes.md", "reposcans/r.go",
			} {
				path := filepath.Join(root, rel)
				if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
					t.Fatalf("mkdir: %v", err)
				}
				if err := os.WriteFile(path, []byte("package p\n"), 0o600); err != nil {
					t.Fatalf("write %s: %v", rel, err)
				}
			}
			var visited []string

			// Act
			EachDaemonSource(t, root, tc.skip, func(fset *token.FileSet, file *ast.File) {
				rel, _ := filepath.Rel(root, fset.Position(file.Pos()).Filename)
				visited = append(visited, rel)
			})

			// Assert
			sort.Strings(visited)
			if !slices.Equal(visited, tc.want) {
				t.Fatalf("visited %v, want %v", visited, tc.want)
			}
		})
	}
}

func TestDaemonRootIsTheModuleRoot(t *testing.T) {
	// Arrange: this package sits three levels below the daemon module, under
	// reposcans, whose own go.mod is not the daemon's.
	want, err := filepath.Abs("../../..")
	if err != nil {
		t.Fatalf("Abs: %v", err)
	}

	// Act
	root := DaemonRoot(t)

	// Assert
	if root != want {
		t.Fatalf("DaemonRoot = %q, want %q", root, want)
	}
}

func TestEachSourceInVisitsTheDirectorysSourcesInOrder(t *testing.T) {
	cases := []struct {
		name      string
		withTests bool
		want      []string
	}{
		{name: "production only skips the tests", want: []string{"a.go", "b.go"}},
		{name: "with tests visits the tests too", withTests: true, want: []string{"a.go", "a_test.go", "b.go"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			dir := t.TempDir()
			for name, body := range map[string]string{"b.go": "package p\n", "a.go": "package p\n", "a_test.go": "package p\n", "notes.txt": "x"} {
				if err := os.WriteFile(filepath.Join(dir, name), []byte(body), 0o600); err != nil {
					t.Fatal(err)
				}
			}
			var visited []string

			// Act
			EachSourceIn(t, dir, tc.withTests, func(fset *token.FileSet, file *ast.File) {
				visited = append(visited, filepath.Base(fset.Position(file.Pos()).Filename))
			})

			// Assert
			if !slices.Equal(visited, tc.want) {
				t.Fatalf("visited %v, want %v", visited, tc.want)
			}
		})
	}
}

func TestEachSourceInOverNothingFails(t *testing.T) {
	// Arrange
	fake := &fakeTB{TB: t}

	// Act
	EachSourceIn(fake, t.TempDir(), true, func(*token.FileSet, *ast.File) {})

	// Assert
	if !fake.failed || !strings.Contains(fake.message, "no .go files") {
		t.Fatalf("failed=%v message=%q, want the empty directory refused", fake.failed, fake.message)
	}
}

func TestIsSelectorMatchesOnlyTheQualifiedReference(t *testing.T) {
	cases := []struct {
		name string
		expr ast.Node
		want bool
	}{
		{name: "the reference itself", expr: &ast.SelectorExpr{X: ast.NewIdent("pkg"), Sel: ast.NewIdent("Name")}, want: true},
		{name: "another package's", expr: &ast.SelectorExpr{X: ast.NewIdent("other"), Sel: ast.NewIdent("Name")}},
		{name: "another name", expr: &ast.SelectorExpr{X: ast.NewIdent("pkg"), Sel: ast.NewIdent("Other")}},
		{name: "no selector", expr: ast.NewIdent("Name")},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := IsSelector(tc.expr, "pkg", "Name")

			// Assert
			if got != tc.want {
				t.Fatalf("IsSelector = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestInPackageRunsTheTestInThePackageDirectory(t *testing.T) {
	// Arrange
	dir := fixturePackage(t, map[string]string{"a.go": "package p // a"})

	// Act
	InPackage(t, dir)
	body, err := os.ReadFile("a.go")

	// Assert
	if err != nil || string(body) != "package p // a" {
		t.Fatalf("ReadFile(a.go) after InPackage = %q, %v; want the package's own a.go", body, err)
	}
}

func TestInPackageResolvesADirectoryUnderTheDaemonRoot(t *testing.T) {
	// Arrange
	want := filepath.Join(DaemonRoot(t), "reposcans", "internal", "sourcescan")

	// Act
	InPackage(t, "reposcans/internal/sourcescan")
	got, err := os.Getwd()

	// Assert
	if err != nil {
		t.Fatalf("Getwd: %v", err)
	}
	if resolved, _ := filepath.EvalSymlinks(want); got != want && got != resolved {
		t.Fatalf("working directory = %q, want %q", got, want)
	}
}

func TestStringDeclsReadsEverySpelledStringOfThePackage(t *testing.T) {
	// Arrange
	dir := fixturePackage(t, map[string]string{
		"a.go":      "package p\ntype Edge string\nconst (\n\tKindX = \"x\"\n\tEdgeY Edge = \"y\"\n\tn = 3\n)\nvar List = []string{\"a\", \"b\"}\nvar mixed = []any{\"a\", 1}\nvar computed = f()\n",
		"a_test.go": "package p\nconst KindTest = \"t\"\n",
	})

	// Act
	got := StringDecls(t, dir)

	// Assert
	want := []StringDecl{
		{Name: "KindX", Const: true, Values: []string{"x"}},
		{Name: "EdgeY", Type: "Edge", Const: true, Values: []string{"y"}},
		{Name: "List", Values: []string{"a", "b"}},
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("StringDecls = %+v, want %+v", got, want)
	}
}
