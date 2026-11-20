package suites

import (
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"agentrepl/testrun/roster"
)

// sizes is the batching the cases batch under: the default, so the sizes the
// roster's suites take when they declare none are the ones pinned.
var sizes = roster.DefaultBatching

// byDir is an areaOf that answers a file's first directory.
func byDir(f string) string {
	dir, _, _ := strings.Cut(f, "/")
	return dir
}

// numbered is n files under dir, named so they sort in order.
func numbered(dir string, n int) []string {
	out := make([]string, 0, n)
	for i := range n {
		out = append(out, fmt.Sprintf("%s/f%02d.test.ts", dir, i))
	}
	return out
}

func names(batches []FileBatch) []string {
	out := make([]string, 0, len(batches))
	for _, b := range batches {
		out = append(out, b.Name)
	}
	return out
}

func TestBatchFilesKeepsAnAreaOfEnoughFilesAsOneBatch(t *testing.T) {
	// Arrange
	files := numbered("sidebar", sizes.Min)

	// Act
	got := BatchFiles(files, byDir, nil, PooledArea, sizes)

	// Assert
	if len(got) != 1 || got[0].Name != "sidebar" || !slices.Equal(got[0].Files, files) {
		t.Fatalf("batches = %+v, want one sidebar batch of every file", got)
	}
}

func TestBatchFilesCutsAnAreaTooLargeIntoNearEqualNumberedBatches(t *testing.T) {
	// Arrange
	files := numbered("feed", 2*sizes.Max+1)

	// Act
	got := BatchFiles(files, byDir, nil, PooledArea, sizes)

	// Assert
	if want := []string{"feed.1", "feed.2", "feed.3"}; !slices.Equal(names(got), want) {
		t.Fatalf("batches = %v, want %v", names(got), want)
	}
	var all []string
	for _, b := range got {
		if len(b.Files) > sizes.Max || len(b.Files) < (2*sizes.Max+1)/3 {
			t.Errorf("%s holds %d files, want at most %d and near an equal share", b.Name, len(b.Files), sizes.Max)
		}
		all = append(all, b.Files...)
	}
	if !slices.Equal(all, files) {
		t.Fatalf("the batches hold %v, want every file once, in order", all)
	}
}

func TestBatchFilesPoolsAreasTooSmallForABatchOfTheirOwn(t *testing.T) {
	// Arrange: two small areas and one large enough.
	files := append(append(numbered("login", 1), numbered("held", sizes.Min-1)...), numbered("sidebar", sizes.Min)...)

	// Act
	got := BatchFiles(files, byDir, nil, PooledArea, sizes)

	// Assert
	if want := []string{PooledArea, "sidebar"}; !slices.Equal(names(got), want) {
		t.Fatalf("batches = %v, want %v", names(got), want)
	}
	if len(got[0].Files) != sizes.Min {
		t.Fatalf("the pooled batch holds %v, want both small areas' files", got[0].Files)
	}
}

func TestBatchFilesGivesAFileItsSuiteNeverReusesABatchOfItsOwn(t *testing.T) {
	// Arrange
	files := numbered("sidebar", sizes.Min+1)
	never := files[2]

	// Act
	got := BatchFiles(files, byDir, func(f string) bool { return f == never }, PooledArea, sizes)

	// Assert
	i := slices.IndexFunc(got, func(b FileBatch) bool { return b.Name == never })
	if i < 0 || !slices.Equal(got[i].Files, []string{never}) {
		t.Fatalf("batches = %+v, want %s alone in a batch named by it", got, never)
	}
	for _, b := range got {
		if b.Name != never && slices.Contains(b.Files, never) {
			t.Fatalf("%s also holds %s", b.Name, never)
		}
	}
}

func TestBatchFilesAnswersTheSameBatchesWhateverTheFilesOrder(t *testing.T) {
	// Arrange
	files := append(numbered("feed", sizes.Max+3), numbered("sidebar", sizes.Min)...)
	reversed := slices.Clone(files)
	slices.Reverse(reversed)

	// Act
	a, b := BatchFiles(files, byDir, nil, PooledArea, sizes), BatchFiles(reversed, byDir, nil, PooledArea, sizes)

	// Assert
	if fmt.Sprint(a) != fmt.Sprint(b) {
		t.Fatalf("batches differ by input order:\n%v\n%v", a, b)
	}
}

func TestBatchFilesOfNoFilesIsNoBatch(t *testing.T) {
	// Act
	got := BatchFiles(nil, byDir, nil, PooledArea, sizes)

	// Assert
	if len(got) != 0 {
		t.Fatalf("batches = %+v, want none", got)
	}
}

func TestVitestAreaIsTheSourceDirectoryAFileImportsMost(t *testing.T) {
	tests := []struct {
		name, file, body, want string
	}{
		{name: "its most imported directory under src",
			file: "test/x.test.ts",
			body: "import { a } from \"../src/sidebar/a\";\nimport { b } from '../src/sidebar/b';\nimport { c } from \"../src/feed/c\";\n",
			want: "sidebar"},
		{name: "a file directly under src is its own area",
			file: "test/x.test.ts", body: "import { s } from \"../src/styles\";\n", want: "styles"},
		{name: "a tie goes to the alphabetically first",
			file: "test/x.test.ts", body: "import \"../src/topbar/a\";\nimport \"../src/feed/b\";\n", want: "feed"},
		{name: "a dynamic import counts",
			file: "test/x.test.ts", body: "const m = await import(\"../src/rpc/m\");\n", want: "rpc"},
		{name: "a file importing nothing under src takes its test directory",
			file: "test/footer/x.test.ts", body: "import { helper } from \"../helpers\";\n", want: "footer"},
		{name: "a file at the test root importing nothing under src is test",
			file: "test/x.test.ts", body: "import { describe } from \"vitest\";\n", want: "test"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			dir := t.TempDir()
			path := filepath.Join(dir, tt.file)
			if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
				t.Fatal(err)
			}
			if err := os.WriteFile(path, []byte(tt.body), 0o644); err != nil {
				t.Fatal(err)
			}

			// Act
			got, err := VitestArea(dir, tt.file)

			// Assert
			if err != nil || got != tt.want {
				t.Fatalf("VitestArea = %q, %v; want %q", got, err, tt.want)
			}
		})
	}
}

func TestVitestAreaOfAFileThatCannotBeReadFails(t *testing.T) {
	// Act
	_, err := VitestArea(t.TempDir(), "test/gone.test.ts")

	// Assert
	if err == nil || !strings.Contains(err.Error(), "test/gone.test.ts") {
		t.Fatalf("err = %v, want the unreadable file named", err)
	}
}

func TestERTAreaIsTheModuleFamily(t *testing.T) {
	tests := []struct{ file, want string }{
		{file: "test-wire-verbs.el", want: "wire"},
		{file: "test-roster.el", want: "roster"},
		{file: "test-helpers.el", want: "helpers"},
	}
	for _, tt := range tests {
		t.Run(tt.file, func(t *testing.T) {
			// Act
			got := ERTArea(tt.file)

			// Assert
			if got != tt.want {
				t.Fatalf("ERTArea(%s) = %q, want %q", tt.file, got, tt.want)
			}
		})
	}
}

func TestVitestBatchesPrefixEveryAreaAndKeepANeverReusedFileAlone(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	files := numbered("test/feed", sizes.Min)
	for _, f := range files {
		if err := os.MkdirAll(filepath.Join(dir, filepath.Dir(f)), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(filepath.Join(dir, f), []byte("import \"../../src/feed/a\";\n"), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	s := roster.Suite{Name: "webapp", Reuse: &roster.Reuse{Never: map[string]string{files[0]: "reads the clock"}}}

	// Act
	got, err := vitestBatches(s, dir, files, IntegrationAreaPrefix)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if want := []string{IntegrationAreaPrefix + PooledArea, files[0]}; !slices.Equal(names(got), want) {
		t.Fatalf("batches = %v, want %v", names(got), want)
	}
}

func TestBatchFilesCutsBySuitesOwnSizes(t *testing.T) {
	// Arrange: a suite of its own sizes, max 4 and min 3.
	own := roster.Batching{Max: 4, Min: 3}
	files := append(append(numbered("feed", 9), numbered("held", 2)...), numbered("sidebar", 3)...)

	// Act
	got := BatchFiles(files, byDir, nil, PooledArea, own)

	// Assert: feed's 9 cut into three of 3, held's 2 pooled, sidebar's 3 kept.
	if want := []string{"feed.1", "feed.2", "feed.3", "pooled", "sidebar"}; !slices.Equal(names(got), want) {
		t.Fatalf("batches = %v, want %v", names(got), want)
	}
}

func TestBatchingLabelNamesEveryBatchedSuitesSizesInRosterOrder(t *testing.T) {
	// Arrange
	suites := []roster.Suite{
		{Name: "ert", Kind: roster.ERT},
		{Name: "daemon", Kind: roster.GoModule},
		{Name: "webapp", Kind: roster.Vitest, Batch: &roster.Batching{Max: 30, Min: 8}},
	}

	// Act
	got := BatchingLabel(suites)

	// Assert
	if want := "ert=max48-min12,webapp=max30-min8"; got != want {
		t.Fatalf("BatchingLabel = %q, want %q", got, want)
	}
}

func TestVitestBatchesCutBySuitesOwnBatching(t *testing.T) {
	// Arrange: five feed files in a suite whose batches hold at most two.
	dir := t.TempDir()
	files := numbered("test/feed", 5)
	for _, f := range files {
		if err := os.MkdirAll(filepath.Join(dir, filepath.Dir(f)), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(filepath.Join(dir, f), []byte("import \"../../src/feed/a\";\n"), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	s := roster.Suite{Name: "webapp", Kind: roster.Vitest, Reuse: &roster.Reuse{}, Batch: &roster.Batching{Max: 2, Min: 1}}

	// Act
	got, err := vitestBatches(s, dir, files, "")

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if want := []string{"feed.1", "feed.2", "feed.3"}; !slices.Equal(names(got), want) {
		t.Fatalf("batches = %v, want %v", names(got), want)
	}
}
