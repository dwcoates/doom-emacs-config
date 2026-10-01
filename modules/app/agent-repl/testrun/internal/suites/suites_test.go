package suites

import (
	"os"
	"path/filepath"
	"reflect"
	"regexp"
	"strings"
	"testing"

	"agentrepl/testrun/roster"
)

func write(t *testing.T, path, body string, mode os.FileMode) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte(body), mode); err != nil {
		t.Fatal(err)
	}
}

func TestPinnedEnvBoundsEveryUnitsParallelism(t *testing.T) {
	tests := []struct {
		name    string
		goflags string
		want    []string
	}{
		{"no caller flags", "", []string{"GOFLAGS=-p=1", "GOMAXPROCS=2"}},
		{"caller flags are kept ahead of the pin", "-mod=mod", []string{"GOFLAGS=-mod=mod -p=1", "GOMAXPROCS=2"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			t.Setenv("GOFLAGS", tt.goflags)

			// Act
			got := PinnedEnv()

			// Assert
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("PinnedEnv = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestEveryBuiltSpecCarriesThePins(t *testing.T) {
	// Arrange: a script suite and a hand-built spec share one constructor.
	dir := t.TempDir()
	write(t, filepath.Join(dir, "bin", "t.sh"), "#!/bin/sh\n", 0o755)
	l := Layout{Repo: dir, Module: dir}

	// Act
	u, err := Build(l, roster.Suite{Name: "s", Kind: roster.Script, Path: "bin/t.sh"})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	for _, pin := range PinnedEnv() {
		found := false
		for _, e := range u.Atomic[0].Env {
			found = found || e == pin
		}
		if !found {
			t.Fatalf("spec env %v lacks the pin %q", u.Atomic[0].Env, pin)
		}
	}
}

func TestScriptUnits(t *testing.T) {
	tests := []struct {
		name     string
		suite    roster.Suite
		arrange  func(t *testing.T, repo string)
		wantArgv func(repo string) []string
		wantErr  string
	}{
		{
			name:  "a module-relative script runs with its arguments",
			suite: roster.Suite{Name: "proto", Kind: roster.Script, Path: "bin/r.sh", Args: []string{"proto"}},
			arrange: func(t *testing.T, repo string) {
				write(t, filepath.Join(repo, "m", "bin", "r.sh"), "#!/bin/sh\n", 0o755)
			},
			wantArgv: func(repo string) []string { return []string{filepath.Join(repo, "m", "bin", "r.sh"), "proto"} },
		},
		{
			name:  "a leading slash is relative to the repository",
			suite: roster.Suite{Name: "hook", Kind: roster.Script, Path: "/.githooks/t.sh"},
			arrange: func(t *testing.T, repo string) {
				write(t, filepath.Join(repo, ".githooks", "t.sh"), "#!/bin/sh\n", 0o755)
			},
			wantArgv: func(repo string) []string { return []string{filepath.Join(repo, ".githooks", "t.sh")} },
		},
		{
			name:    "a missing script is refused",
			suite:   roster.Suite{Name: "x", Kind: roster.Script, Path: "bin/none.sh"},
			arrange: func(*testing.T, string) {},
			wantErr: "x's runner",
		},
		{
			name:  "a script that is not executable is refused",
			suite: roster.Suite{Name: "x", Kind: roster.Script, Path: "bin/r.sh"},
			arrange: func(t *testing.T, repo string) {
				write(t, filepath.Join(repo, "m", "bin", "r.sh"), "#!/bin/sh\n", 0o644)
			},
			wantErr: "is not executable",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			repo := t.TempDir()
			tt.arrange(t, repo)
			l := Layout{Repo: repo, Module: filepath.Join(repo, "m")}

			// Act
			u, err := scriptUnits(l, tt.suite)

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want it to mention %q", err, tt.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatal(err)
			}
			if got := u.Atomic[0].Argv; !reflect.DeepEqual(got, tt.wantArgv(repo)) {
				t.Fatalf("argv = %v, want %v", got, tt.wantArgv(repo))
			}
		})
	}
}

func TestScriptUnitsCarryTheDeclineRight(t *testing.T) {
	// Arrange
	repo := t.TempDir()
	write(t, filepath.Join(repo, "bin", "e.sh"), "#!/bin/sh\n", 0o755)

	// Act
	u, err := scriptUnits(Layout{Repo: repo, Module: repo}, roster.Suite{Name: "e", Path: "bin/e.sh", MayDecline: true})

	// Assert
	if err != nil || !u.Atomic[0].MayDecline {
		t.Fatalf("spec = %+v, %v; want MayDecline", u.Atomic, err)
	}
}

func TestERTRoster(t *testing.T) {
	tests := []struct {
		name    string
		body    string
		want    []string
		wantErr string
	}{
		{
			name: "the aggregator's load list in its order",
			body: `(load (expand-file-name "test-helpers.el" dir) nil t)
  (load (expand-file-name "test-b.el" dir) nil t)
  (load (expand-file-name "test-a.el" dir) nil t)`,
			want: []string{"test-helpers.el", "test-b.el", "test-a.el"},
		},
		{
			name:    "the helpers must come first",
			body:    `(load (expand-file-name "test-a.el" dir) nil t)`,
			wantErr: "must load test-helpers.el first",
		},
		{
			name: "a file loaded twice is refused",
			body: `(load (expand-file-name "test-helpers.el" dir) nil t)
(load (expand-file-name "test-a.el" dir) nil t)
(load (expand-file-name "test-a.el" dir) nil t)`,
			wantErr: "loads test-a.el twice",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			dir := t.TempDir()
			write(t, filepath.Join(dir, ertAggregator), tt.body, 0o644)

			// Act
			got, err := ERTRoster(dir)

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want it to mention %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("ERTRoster = %v, %v; want %v", got, err, tt.want)
			}
		})
	}
}

func TestERTRosterOfAMissingAggregatorFails(t *testing.T) {
	// Act
	_, err := ERTRoster(t.TempDir())

	// Assert
	if err == nil || !strings.Contains(err.Error(), "read the ERT aggregator") {
		t.Fatalf("err = %v", err)
	}
}

func TestTheRealERTRosterMatchesEveryTestFile(t *testing.T) {
	// Arrange: the aggregator must load every lisp/test-*.el that defines tests,
	// or the chunk driver's own check fails the run; this pins it statically.
	lisp := filepath.Join("..", "..", "..", "lisp")
	files, err := filepath.Glob(filepath.Join(lisp, "test-*.el"))
	if err != nil {
		t.Fatal(err)
	}

	// Act
	got, err := ERTRoster(lisp)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	onRoster := map[string]bool{}
	for _, f := range got {
		onRoster[f] = true
	}
	deftest := regexp.MustCompile(`(?m)^\(ert-deftest `)
	for _, f := range files {
		data, err := os.ReadFile(f)
		if err != nil {
			t.Fatal(err)
		}
		if deftest.Match(data) && !onRoster[filepath.Base(f)] {
			t.Errorf("%s defines tests but lisp/test-agent-repl.el does not load it", filepath.Base(f))
		}
	}
}

func TestParseERTItems(t *testing.T) {
	tests := []struct {
		name    string
		out     string
		want    []string
		wantErr string
	}{
		{
			name: "one line per file",
			out:  "Ran 3 tests\nTESTRUN-ITEM test-a.el 1.500000\nnoise\nTESTRUN-ITEM test-b.el 0.250000\n",
			want: []string{"test-a.el", "test-b.el"},
		},
		{name: "a file with no line fails", out: "TESTRUN-ITEM test-a.el 1.0\n", want: []string{"test-a.el", "test-b.el"}, wantErr: "no timing reported for [test-b.el]"},
		{name: "a file reported twice fails", out: "TESTRUN-ITEM test-a.el 1.0\nTESTRUN-ITEM test-a.el 1.0\n", want: []string{"test-a.el"}, wantErr: "reported twice"},
		{name: "an unrequested file fails", out: "TESTRUN-ITEM test-a.el 1.0\nTESTRUN-ITEM test-z.el 1.0\n", want: []string{"test-a.el"}, wantErr: "timings reported for 2 items"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := ParseERTItems([]byte(tt.out), tt.want)

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want it to mention %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || got["test-a.el"] != 1.5 || got["test-b.el"] != 0.25 {
				t.Fatalf("ParseERTItems = %v, %v", got, err)
			}
		})
	}
}

func TestLispList(t *testing.T) {
	// Act
	got := lispList([]string{"a.el", `q"uote.el`})

	// Assert
	if want := `'("a.el" "q\"uote.el")`; got != want {
		t.Fatalf("lispList = %s, want %s", got, want)
	}
}

func TestParseGoTestItems(t *testing.T) {
	tests := []struct {
		name    string
		out     string
		want    []string
		wantErr string
	}{
		{
			name: "top-level results only, whatever the verdict",
			out:  "=== RUN   TestA\n--- PASS: TestA (1.25s)\n    --- PASS: TestA/sub (9.00s)\n--- SKIP: TestB (0.00s)\n",
			want: []string{"TestA", "TestB"},
		},
		{name: "a test with no result line fails", out: "--- PASS: TestA (1.00s)\n", want: []string{"TestA", "TestC"}, wantErr: "no timing reported for [TestC]"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := ParseGoTestItems([]byte(tt.out), tt.want)

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want it to mention %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || got["TestA"] != 1.25 || got["TestB"] != 0 {
				t.Fatalf("ParseGoTestItems = %v, %v", got, err)
			}
		})
	}
}

func TestGoTopLevelTests(t *testing.T) {
	// Arrange: a package with a tagged-out file, helpers, a benchmark, TestMain
	// and a lowercase "Testing" lookalike.
	dir := t.TempDir()
	write(t, filepath.Join(dir, "go.mod"), "module x\n\ngo 1.24\n", 0o644)
	write(t, filepath.Join(dir, "x.go"), "package x\n", 0o644)
	write(t, filepath.Join(dir, "a_test.go"), `package x
import "testing"
func TestMain(m *testing.M) {}
func TestB(t *testing.T) {}
func TestA(t *testing.T) {}
func Testing(t *testing.T) {}
func TestHelper(x int) {}
func BenchmarkX(b *testing.B) {}
type s struct{}
func (s) TestMethod(t *testing.T) {}
`, 0o644)
	write(t, filepath.Join(dir, "tagged_test.go"), "//go:build perf\n\npackage x\nimport \"testing\"\nfunc TestPerf(t *testing.T) {}\n", 0o644)
	write(t, filepath.Join(dir, "ext_test.go"), "package x_test\nimport \"testing\"\nfunc TestExternal(t *testing.T) {}\n", 0o644)

	// Act
	got, err := GoTopLevelTests(dir)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if want := []string{"TestA", "TestB", "TestExternal"}; !reflect.DeepEqual(got, want) {
		t.Fatalf("GoTopLevelTests = %v, want %v", got, want)
	}
}

func TestRunPatternMatchesExactlyTheNamedTests(t *testing.T) {
	// Arrange
	re := regexp.MustCompile(runPattern([]string{"TestA", "TestB"}))

	// Act / Assert
	for name, want := range map[string]bool{"TestA": true, "TestB": true, "TestAB": false, "XTestA": false} {
		if got := re.MatchString(name); got != want {
			t.Errorf("match(%s) = %v, want %v", name, got, want)
		}
	}
}

func TestParseVitestItems(t *testing.T) {
	tests := []struct {
		name    string
		body    string
		want    []string
		wantErr string
	}{
		{name: "costs keyed relative to the package", body: `{"/p/test/a.test.ts": 2.5, "/p/test/b.test.ts": 0.5}`, want: []string{"test/a.test.ts", "test/b.test.ts"}},
		{name: "not json fails", body: `{`, want: []string{"test/a.test.ts"}, wantErr: "is not an items-reporter file"},
		{name: "a negative cost fails", body: `{"/p/test/a.test.ts": -1}`, want: []string{"test/a.test.ts"}, wantErr: "negative cost"},
		{name: "a missing file fails", body: `{"/p/test/a.test.ts": 1}`, want: []string{"test/a.test.ts", "test/b.test.ts"}, wantErr: "no timing reported"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := filepath.Join(t.TempDir(), "items.json")
			write(t, path, tt.body, 0o644)

			// Act
			got, err := ParseVitestItems(path, "/p", tt.want)

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want it to mention %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || got["test/a.test.ts"] != 2.5 || got["test/b.test.ts"] != 0.5 {
				t.Fatalf("ParseVitestItems = %v, %v", got, err)
			}
		})
	}
}

func TestParseVitestItemsOfAMissingReportFails(t *testing.T) {
	// Act
	_, err := ParseVitestItems(filepath.Join(t.TempDir(), "none.json"), "/p", nil)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "read the chunk's per-file costs") {
		t.Fatalf("err = %v", err)
	}
}

func TestEveryRosterSuiteHasAKnownKind(t *testing.T) {
	// Arrange / Act / Assert: Build panics on an unknown kind, so every roster
	// kind must be one of the five it switches on.
	for _, s := range roster.Suites {
		switch s.Kind {
		case roster.Script, roster.ERT, roster.GoModule, roster.Vitest, roster.E2E:
		default:
			t.Errorf("suite %s has unknown kind %d", s.Name, s.Kind)
		}
	}
}
