package impact

import (
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"
)

// fixture writes a module "fx" holding files (path to content) and answers
// its checkout and its listed packages.
func fixture(t *testing.T, files map[string]string, patterns ...string) (string, []Pkg) {
	t.Helper()
	dir, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}
	files["go.mod"] = "module fx\n\ngo 1.24\n"
	for name, content := range files {
		p := filepath.Join(dir, filepath.FromSlash(name))
		if err := os.MkdirAll(filepath.Dir(p), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(p, []byte(content), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	if len(patterns) == 0 {
		patterns = []string{"./..."}
	}
	pkgs, err := ExecLister(dir, "", false, patterns...)
	if err != nil {
		t.Fatalf("list the fixture: %v", err)
	}
	return dir, pkgs
}

// policyTrackingAll tracks every package's functions and follows no file whole.
var policyTrackingAll = Policy{Tracked: func(string) bool { return true }, Whole: func(string) bool { return false }}

var fxFiles = func() map[string]string {
	return map[string]string{
		"a/a.go": `package a

import "fx/gen"

func A() int {
	var m gen.M
	return int(K) + m.Get() + Hidden()
}
`,
		"a/consts.go": `package a

type T struct{}

const K = Z2

var V = 1
`,
		"a/chain.go":  "package a\n\nconst Z2 = 3\n",
		"a/fnonly.go": "package a\n\nfunc Hidden() int { return 2 }\n",
		"a/unused.go": "package a\n\nconst Unused = 9\n\nfunc Never() {}\n",
		"gen/gen.go": `package gen

type M struct{}

func (M) Get() int { return G }
`,
		"gen/gen2.go": "package gen\n\nconst G = 4\n",
	}
}

func relSet(t *testing.T, a *Analyzer, abs []string) string {
	t.Helper()
	var rel []string
	for _, f := range abs {
		r, err := a.Rel(f)
		if err != nil {
			t.Fatal(err)
		}
		rel = append(rel, r)
	}
	sort.Strings(rel)
	return strings.Join(rel, ",")
}

func TestReachCases(t *testing.T) {
	cases := []struct {
		name    string
		tracked func(string) bool
		whole   func(string) bool
		want    string
	}{
		{
			name:    "a tracked package's unexecuted function is no dependency but its constants are, transitively",
			tracked: func(p string) bool { return p == "fx/a" },
			whole:   func(string) bool { return false },
			// K (consts.go) is a const defined from Z2 (chain.go); gen's method is in a package coverage does not track, so its file counts; G in gen2.go is used inside that method's body, which a declarations-only follow does not read.
			want: "a/chain.go,a/consts.go,gen/gen.go",
		},
		{
			name:    "an untracked function counts too",
			tracked: func(string) bool { return false },
			whole:   func(string) bool { return false },
			want:    "a/chain.go,a/consts.go,a/fnonly.go,gen/gen.go",
		},
		{
			name:    "a package followed whole reaches what its functions use",
			tracked: func(p string) bool { return p == "fx/a" },
			whole:   func(p string) bool { return p == "fx/gen" },
			want:    "a/chain.go,a/consts.go,gen/gen.go,gen/gen2.go",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			repo, pkgs := fixture(t, fxFiles(), "./a")
			root := filepath.Join(repo, "a", "a.go")
			a := NewAnalyzer(repo, pkgs, Policy{Tracked: tc.tracked, Whole: tc.whole}, []string{root})
			// Act
			got, err := a.Reach([]string{root})
			// Assert
			if err != nil {
				t.Fatalf("Reach: %v", err)
			}
			if rel := relSet(t, a, got); rel != tc.want {
				t.Fatalf("Reach = %s, want %s", rel, tc.want)
			}
		})
	}
}

func TestReachNeverReturnsTheRootsOrUnusedFiles(t *testing.T) {
	// Arrange
	repo, pkgs := fixture(t, fxFiles(), "./a")
	roots := []string{filepath.Join(repo, "a", "a.go"), filepath.Join(repo, "a", "consts.go")}
	a := NewAnalyzer(repo, pkgs, policyTrackingAll, roots)
	// Act
	got, err := a.Reach(roots)
	// Assert
	if err != nil {
		t.Fatal(err)
	}
	rel := relSet(t, a, got)
	if strings.Contains(rel, "a/a.go") || strings.Contains(rel, "a/consts.go") || strings.Contains(rel, "unused.go") {
		t.Fatalf("Reach = %s, want neither a root nor a file nothing uses", rel)
	}
}

func TestFileOfMapsACoverageNameToItsFile(t *testing.T) {
	// Arrange
	repo, pkgs := fixture(t, fxFiles(), "./a")
	pkgs = append(pkgs, Pkg{ImportPath: "fmt", Dir: "/goroot/src/fmt", Standard: true})
	a := NewAnalyzer(repo, pkgs, policyTrackingAll, nil)
	tests := []struct{ name, cov, wantErr string }{
		{"a listed package's file", "fx/a/a.go", ""},
		{"an unlisted package", "fx/zzz/z.go", "which the listing does not hold"},
		{"a standard package is outside the checkout", "fmt/print.go", "outside the checkout"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := a.FileOf(tt.cov)
			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("FileOf = (%s, %v), want %q", got, err, tt.wantErr)
				}
				return
			}
			if err != nil || got != filepath.Join(repo, "a", "a.go") {
				t.Fatalf("FileOf = (%s, %v)", got, err)
			}
		})
	}
}

func TestEmbeddedAnswersWhatARootsPackageEmbeds(t *testing.T) {
	// Arrange
	files := fxFiles()
	files["a/e.go"] = "package a\n\nimport _ \"embed\"\n\n//go:embed data.txt\nvar Data string\n"
	files["a/data.txt"] = "x"
	repo, pkgs := fixture(t, files, "./a")
	a := NewAnalyzer(repo, pkgs, policyTrackingAll, nil)
	// Act
	got := a.Embedded([]string{filepath.Join(repo, "a", "a.go"), filepath.Join(repo, "gen", "gen.go")})
	// Assert
	if relSet(t, a, got) != "a/data.txt" {
		t.Fatalf("Embedded = %v, want the package's embedded file", got)
	}
}

func TestReachRefusesWhatItCannotCheck(t *testing.T) {
	cases := []struct {
		name    string
		files   map[string]string
		wantErr string
	}{
		{"a file that does not parse", map[string]string{"a/a.go": "package a\nfunc {"}, "parse"},
		{"a package of the checkout that does not type-check", map[string]string{"a/a.go": "package a\n\nfunc A() int { return undefinedName }\n"}, "type-check fx/a"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			repo, pkgs := fixtureAllowingBadList(t, tc.files)
			root := filepath.Join(repo, "a", "a.go")
			a := NewAnalyzer(repo, pkgs, policyTrackingAll, []string{root})
			// Act
			_, err := a.Reach([]string{root})
			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
				t.Fatalf("Reach = %v, want an error containing %q", err, tc.wantErr)
			}
		})
	}
}

// fixtureAllowingBadList lists a module whose source is broken, which go
// list answers with the files it found.
func fixtureAllowingBadList(t *testing.T, files map[string]string) (string, []Pkg) {
	t.Helper()
	dir, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}
	files["go.mod"] = "module fx\n\ngo 1.24\n"
	for name, content := range files {
		p := filepath.Join(dir, filepath.FromSlash(name))
		if err := os.MkdirAll(filepath.Dir(p), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(p, []byte(content), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	return dir, []Pkg{{ImportPath: "fx/a", Dir: filepath.Join(dir, "a"), GoFiles: []string{"a.go"}}}
}

func TestReachRefusesAFileOfNoListedPackage(t *testing.T) {
	// Arrange
	repo, pkgs := fixture(t, fxFiles(), "./a")
	elsewhere := filepath.Join(repo, "elsewhere.go")
	a := NewAnalyzer(repo, pkgs, policyTrackingAll, []string{elsewhere})
	// Act
	_, err := a.Reach([]string{elsewhere})
	_, notARoot := a.Reach([]string{filepath.Join(repo, "a", "a.go")})
	// Assert
	if err == nil || !strings.Contains(err.Error(), "belongs to no listed package") {
		t.Fatalf("Reach = %v, want a refusal of a file no package holds", err)
	}
	if notARoot == nil || !strings.Contains(notARoot.Error(), "no root this analyzer was made for") {
		t.Fatalf("Reach of a file outside the declared roots = %v, want a refusal", notARoot)
	}
}

func TestRelRefusesAPathOutsideTheCheckout(t *testing.T) {
	a := NewAnalyzer("/repo", nil, policyTrackingAll, nil)
	if _, err := a.Rel("/elsewhere/x.go"); err == nil || !strings.Contains(err.Error(), "not inside the checkout") {
		t.Fatalf("Rel = %v, want a refusal", err)
	}
}

func TestExecListerRefusesAFailingList(t *testing.T) {
	// Act
	_, err := ExecLister(t.TempDir(), "", false, "./nothing")
	// Assert
	if err == nil || !strings.Contains(err.Error(), "impact: go list") {
		t.Fatalf("ExecLister = %v, want the failed go list named", err)
	}
}
