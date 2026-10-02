package suites

import (
	"os"
	"path/filepath"
	"reflect"
	"regexp"
	"slices"
	"strings"
	"testing"

	"agentrepl/testrun/internal/run"
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

func TestScriptUnitsCarryTheirWidth(t *testing.T) {
	tests := []struct {
		name      string
		slots     int
		wantSlots int
		wantEnv   string
		wantErr   string
	}{
		{name: "an ordinary script is one slot and is told so", slots: 0, wantSlots: 0, wantEnv: "AGENT_REPL_UNIT_SLOTS=1"},
		{name: "a wide script holds its width and is told it", slots: 4, wantSlots: 4, wantEnv: "AGENT_REPL_UNIT_SLOTS=4"},
		{name: "a negative width is refused", slots: -1, wantErr: "suites: e has a negative width -1"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			repo := t.TempDir()
			write(t, filepath.Join(repo, "bin", "e.sh"), "#!/bin/sh\n", 0o755)

			// Act
			u, err := scriptUnits(Layout{Repo: repo, Module: repo}, roster.Suite{Name: "e", Path: "bin/e.sh", Slots: tt.slots})

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want %q", err, tt.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatal(err)
			}
			sp := u.Atomic[0]
			if sp.Slots != tt.wantSlots || !slices.Contains(sp.Env, tt.wantEnv) {
				t.Fatalf("spec slots %d env %v, want slots %d and %q", sp.Slots, sp.Env, tt.wantSlots, tt.wantEnv)
			}
		})
	}
}

func TestParseScriptList(t *testing.T) {
	tests := []struct {
		name    string
		out     string
		want    []string
		wantErr string
	}{
		{name: "ordered names", out: "core\nstamps\n", want: []string{"core", "stamps"}},
		{name: "no names", wantErr: "no items listed"},
		{name: "empty line", out: "core\n\nstamps\n", wantErr: "invalid item name"},
		{name: "space", out: "not one\n", wantErr: "invalid item name"},
		{name: "comma", out: "not,one\n", wantErr: "invalid item name"},
		{name: "duplicate", out: "core\ncore\n", wantErr: "listed twice"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := parseScriptList([]byte(tt.out))

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want it to mention %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("parseScriptList = %v, %v; want %v", got, err, tt.want)
			}
		})
	}
}

func TestSplitScriptUnitsChunkSelectedItems(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "bin", "harness.sh")
	suite := roster.Suite{Name: "harness", Kind: roster.SplitScript}
	units := splitScriptUnitsForItems(path, suite, []string{"core", "stamps"})

	// Act
	spec := units.Splits[0].Chunk("harness-1", []string{"stamps", "core"})
	got, err := spec.Items([]byte("noise\nTESTRUN-ITEM stamps 2.5\nTESTRUN-ITEM core 1.25\n"))

	// Assert
	if want := []string{path, "--only", "stamps,core"}; !reflect.DeepEqual(spec.Argv, want) {
		t.Fatalf("argv = %v, want %v", spec.Argv, want)
	}
	if spec.Dir != filepath.Dir(path) {
		t.Fatalf("dir = %q, want %q", spec.Dir, filepath.Dir(path))
	}
	if err != nil || got["stamps"] != 2.5 || got["core"] != 1.25 {
		t.Fatalf("Items = %v, %v", got, err)
	}
}

func TestParseItemLinesRejectsInvalidReports(t *testing.T) {
	tests := []struct {
		name    string
		out     string
		want    []string
		wantErr string
	}{
		{name: "missing", out: "TESTRUN-ITEM core 1\n", want: []string{"core", "stamps"}, wantErr: "no timing reported"},
		{name: "duplicate", out: "TESTRUN-ITEM core 1\nTESTRUN-ITEM core 2\n", want: []string{"core"}, wantErr: "reported twice"},
		{name: "not numeric", out: "TESTRUN-ITEM core slow\n", want: []string{"core"}, wantErr: "unreadable item timing"},
		{name: "negative", out: "TESTRUN-ITEM core -1\n", want: []string{"core"}, wantErr: "unreadable item timing"},
		{name: "unrequested", out: "TESTRUN-ITEM core 1\nTESTRUN-ITEM extra 2\n", want: []string{"core"}, wantErr: "timings reported for 2 items"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, err := ParseItemLines([]byte(tt.out), tt.want)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
				t.Fatalf("err = %v, want it to mention %q", err, tt.wantErr)
			}
		})
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

func TestParseItemLines(t *testing.T) {
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
			got, err := ParseItemLines([]byte(tt.out), tt.want)

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want it to mention %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || got["test-a.el"] != 1.5 || got["test-b.el"] != 0.25 {
				t.Fatalf("ParseItemLines = %v, %v", got, err)
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
		wantA   float64
		wantB   float64
		wantErr string
	}{
		{
			name:  "parallel subtest time belongs to its top-level item",
			out:   "=== RUN   TestA\n--- PASS: TestA (1.25s)\n    --- PASS: TestA/sub (9.00s)\n--- SKIP: TestB (0.00s)\n",
			want:  []string{"TestA", "TestB"},
			wantA: 9,
		},
		{
			name:  "a sequential parent's inclusive time is not double counted",
			out:   "--- PASS: TestA (10.00s)\n    --- PASS: TestA/one (4.00s)\n    --- PASS: TestA/two (5.00s)\n--- PASS: TestB (0.50s)\n",
			want:  []string{"TestA", "TestB"},
			wantA: 10,
			wantB: 0.5,
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
			if err != nil || got["TestA"] != tt.wantA || got["TestB"] != tt.wantB {
				t.Fatalf("ParseGoTestItems = %v, %v", got, err)
			}
		})
	}
}

func TestGoTopLevelTests(t *testing.T) {
	// Arrange: a package with a tagged-out file, helpers, a benchmark, TestMain,
	// a lowercase "Testing" lookalike, a fuzz target, and two examples of which
	// only the one with an output comment is run by go test.
	dir := t.TempDir()
	write(t, filepath.Join(dir, "go.mod"), "module x\n\ngo 1.24\n", 0o644)
	write(t, filepath.Join(dir, "x.go"), "package x\n\nfunc F() {}\n", 0o644)
	write(t, filepath.Join(dir, "a_test.go"), `package x
import "testing"
func TestMain(m *testing.M) {}
func TestB(t *testing.T) {}
func TestA(t *testing.T) {}
func Testing(t *testing.T) {}
func TestHelper(x int) {}
func BenchmarkX(b *testing.B) {}
func FuzzParse(f *testing.F) {}
type s struct{}
func (s) TestMethod(t *testing.T) {}
func ExampleF() {
	F()
	// Output:
}
func ExampleF_quiet() {
	F()
}
`, 0o644)
	write(t, filepath.Join(dir, "tagged_test.go"), "//go:build perf\n\npackage x\nimport \"testing\"\nfunc TestPerf(t *testing.T) {}\n", 0o644)
	write(t, filepath.Join(dir, "ext_test.go"), "package x_test\nimport \"testing\"\nfunc TestExternal(t *testing.T) {}\n", 0o644)

	// Act
	got, err := GoTopLevelTests(dir)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if want := []string{"ExampleF", "FuzzParse", "TestA", "TestB", "TestExternal"}; !reflect.DeepEqual(got, want) {
		t.Fatalf("GoTopLevelTests = %v, want %v", got, want)
	}
}

func TestGoTopLevelTestsOfADirectoryWithoutGoIsEmpty(t *testing.T) {
	// Act
	got, err := GoTopLevelTests(t.TempDir())

	// Assert
	if err != nil || len(got) != 0 {
		t.Fatalf("GoTopLevelTests = %v, %v; want nothing", got, err)
	}
}

func goPackageFixture(t *testing.T, tests string) string {
	t.Helper()
	module := t.TempDir()
	write(t, filepath.Join(module, "go.mod"), "module x\n\ngo 1.24\n", 0o644)
	write(t, filepath.Join(module, "p", "p.go"), "package p\n", 0o644)
	write(t, filepath.Join(module, "p", "p_test.go"), "package p\nimport \"testing\"\n"+tests, 0o644)
	return module
}

func TestGoPkgUnits(t *testing.T) {
	tests := []struct {
		name      string
		covDir    bool
		wantBuild []string
		wantChunk []string
	}{
		{
			name:      "a covered package compiles instrumented and writes counters",
			covDir:    true,
			wantBuild: []string{"go", "test", "-c", "-o", "BIN", "-cover", "-coverpkg=./...", "./p"},
			wantChunk: []string{"BIN", "-test.count=1", "-test.v", "-test.parallel=1", "-test.timeout=10m", "-test.run=^(TestA)$", "-test.gocoverdir=COV"},
		},
		{
			name:      "an uncovered package compiles plain",
			wantBuild: []string{"go", "test", "-c", "-o", "BIN", "./p"},
			wantChunk: []string{"BIN", "-test.count=1", "-test.v", "-test.parallel=1", "-test.timeout=10m", "-test.run=^(TestA)$"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			module := goPackageFixture(t, "func TestA(t *testing.T) {}\nfunc TestB(t *testing.T) {}\n")
			bin := filepath.Join(t.TempDir(), "p.test")
			cov := ""
			if tt.covDir {
				cov = filepath.Join(t.TempDir(), "cov")
			}
			p := goPkg{Suite: "m", Module: module, Rel: "p", Bin: bin, CovDir: cov}

			// Act
			build, split, err := p.units()

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			subst := func(argv []string) []string {
				out := []string{}
				for _, a := range argv {
					a = strings.ReplaceAll(a, bin, "BIN")
					if cov != "" {
						a = strings.ReplaceAll(a, cov, "COV")
					}
					out = append(out, a)
				}
				return out
			}
			if build.ID != "m:p:build" || !reflect.DeepEqual(subst(build.Argv), tt.wantBuild) {
				t.Fatalf("build = %s %v, want m:p:build %v", build.ID, subst(build.Argv), tt.wantBuild)
			}
			if split == nil || split.Group != "m:p" || !reflect.DeepEqual(split.Items, []string{"TestA", "TestB"}) || !reflect.DeepEqual(split.Deps, []string{"m:p:build"}) {
				t.Fatalf("split = %+v", split)
			}
			chunk := split.Chunk("m:p#00", []string{"TestA"})
			if !reflect.DeepEqual(subst(chunk.Argv), tt.wantChunk) || chunk.Dir != filepath.Join(module, "p") {
				t.Fatalf("chunk = %v in %s, want %v in the package dir", subst(chunk.Argv), chunk.Dir, tt.wantChunk)
			}
			if tt.covDir {
				if info, err := os.Stat(cov); err != nil || !info.IsDir() {
					t.Fatalf("the coverage directory was not created: %v", err)
				}
			}
		})
	}
}

func TestGoPkgUnitsOfAPackageWithoutTestsIsItsBuildAlone(t *testing.T) {
	// Arrange: test files with helpers only.
	module := goPackageFixture(t, "func helper(t *testing.T) {}\n")

	// Act
	build, split, err := goPkg{Suite: "m", Module: module, Rel: "p", Bin: "/b"}.units()

	// Assert
	if err != nil || split != nil || build.ID != "m:p:build" {
		t.Fatalf("units = %s, %+v, %v; want the build alone", build.ID, split, err)
	}
}

func TestGoPkgUnitsUsesACustomBuild(t *testing.T) {
	// Arrange
	module := goPackageFixture(t, "func TestA(t *testing.T) {}\n")
	p := goPkg{Suite: "e2e", Module: module, Rel: "p", Bin: "/b", Build: []string{"bash", "-c", "make it"}, ChunkEnv: []string{"K=V"}, Timeout: "45m"}

	// Act
	build, split, err := p.units()

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if !reflect.DeepEqual(build.Argv, []string{"bash", "-c", "make it"}) {
		t.Fatalf("build argv = %v", build.Argv)
	}
	chunk := split.Chunk("e2e:p#00", []string{"TestA"})
	if !strings.Contains(strings.Join(chunk.Argv, " "), "-test.timeout=45m") || chunk.Env[len(chunk.Env)-1] != "K=V" {
		t.Fatalf("chunk argv %v env %v", chunk.Argv, chunk.Env)
	}
}

func TestGoPkgSharedPrebuild(t *testing.T) {
	// Arrange
	p := goPkg{Module: "/module with space", Rel: "integration", Bin: "/work/pkg.test"}

	// Act
	p.sharePrebuilt("/work/shared", "prepare deps")

	// Assert
	wantScript := "set -euo pipefail\nprepare deps\ngo test -c -o \"/work/pkg.test\" \"./integration\"\n" +
		"AGENT_REPL_TEST_PREBUILD=\"/work/shared\" \"/work/pkg.test\" -test.run '^$'"
	if !reflect.DeepEqual(p.Build, []string{"bash", "-c", wantScript}) {
		t.Fatalf("build = %q, want %q", p.Build, wantScript)
	}
	if !reflect.DeepEqual(p.ChunkEnv, []string{"AGENT_REPL_TEST_PREBUILT=/work/shared"}) {
		t.Fatalf("chunk env = %v", p.ChunkEnv)
	}
}

func TestGoModulePrebuildPackage(t *testing.T) {
	for _, coverage := range []bool{false, true} {
		t.Run(map[bool]string{false: "ordinary run shares prebuilt binaries", true: "coverage run builds in every process"}[coverage], func(t *testing.T) {
			// Arrange
			module := goPackageFixture(t, "func TestA(t *testing.T) {}\n")
			l := Layout{Module: module, Work: t.TempDir(), Coverage: coverage}
			s := roster.Suite{Name: "sidecar", PrebuildPackage: "p"}

			// Act
			u, err := goModuleUnitsForPackages(l, s, module, []string{"p"})

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			build := u.Atomic[1]
			usesPrebuild := len(build.Argv) == 3 && build.Argv[0] == "bash" && strings.Contains(build.Argv[2], "AGENT_REPL_TEST_PREBUILD")
			chunk := u.Splits[0].Chunk("sidecar:p#00", []string{"TestA"})
			usesPrebuilt := strings.Contains(strings.Join(chunk.Env, " "), "AGENT_REPL_TEST_PREBUILT")
			if usesPrebuild != !coverage || usesPrebuilt != !coverage {
				t.Fatalf("coverage=%v: prebuild command=%v prebuilt env=%v", coverage, usesPrebuild, usesPrebuilt)
			}
		})
	}
}

func TestGoModuleRefusesAMissingPrebuildPackage(t *testing.T) {
	// Arrange
	module := goPackageFixture(t, "func TestA(t *testing.T) {}\n")
	l := Layout{Module: module, Work: t.TempDir()}
	s := roster.Suite{Name: "sidecar", PrebuildPackage: "not-present"}

	// Act
	_, err := goModuleUnitsForPackages(l, s, module, []string{"p"})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "prebuild package \"not-present\" is not a tested package") {
		t.Fatalf("err = %v", err)
	}
}

func TestGoModuleCoverageModes(t *testing.T) {
	for _, coverage := range []bool{false, true} {
		t.Run(map[bool]string{false: "ordinary run", true: "coverage run"}[coverage], func(t *testing.T) {
			// Arrange
			module := goPackageFixture(t, "func TestA(t *testing.T) {}\n")
			l := Layout{Module: module, Work: t.TempDir(), Self: "/testrun", Coverage: coverage}
			s := roster.Suite{Name: "m"}

			// Act
			u, err := goModuleUnitsForPackages(l, s, module, []string{"p"})

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			var build, report bool
			for _, unit := range u.Atomic {
				build = build || unit.ID == "m:p:build" && strings.Contains(strings.Join(unit.Argv, " "), "-cover")
				report = report || unit.ID == "m:coverage"
			}
			if build != coverage || report != coverage {
				t.Fatalf("coverage=%v: instrumented build=%v report=%v", coverage, build, report)
			}
		})
	}
}

func TestVitestCoverageModes(t *testing.T) {
	for _, coverage := range []bool{false, true} {
		t.Run(map[bool]string{false: "ordinary run", true: "coverage run"}[coverage], func(t *testing.T) {
			// Arrange
			dir := t.TempDir()
			l := Layout{Module: dir, Work: t.TempDir(), Coverage: coverage}
			s := roster.Suite{Name: "webapp"}

			// Act
			u, err := vitestUnitsForFiles(l, s, dir, []string{"src/a.test.ts"})

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			chunk := u.Splits[0].Chunk("webapp#00", []string{"src/a.test.ts"})
			chunkCoverage := false
			for _, arg := range chunk.Argv {
				chunkCoverage = chunkCoverage || arg == "--coverage"
			}
			report := false
			for _, unit := range u.Atomic {
				report = report || unit.ID == "webapp:coverage"
			}
			if chunkCoverage != coverage || report != coverage {
				t.Fatalf("coverage=%v: chunk coverage=%v report=%v argv=%v", coverage, chunkCoverage, report, chunk.Argv)
			}
		})
	}
}

func TestVitestTypecheckHoldsTheTwoCoresItUses(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	l := Layout{Module: dir, Work: t.TempDir()}

	// Act
	u, err := vitestUnitsForFiles(l, roster.Suite{Name: "webapp"}, dir, []string{"src/a.test.ts"})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	for _, unit := range u.Atomic {
		if unit.ID == "webapp:typecheck" {
			if unit.Width() != 2 {
				t.Fatalf("typecheck width = %d, want 2", unit.Width())
			}
			return
		}
	}
	t.Fatalf("no typecheck unit in %v", u.Atomic)
}

func TestQuietGoTestOutput(t *testing.T) {
	// Arrange
	out := []byte("=== RUN   TestA\n=== PAUSE TestA\n=== CONT  TestA\n    a_test.go:3: a log line\n--- PASS: TestA (0.10s)\n    --- PASS: TestA/sub (0.00s)\nPASS\ncoverage: 50.0% of statements\n")

	// Act
	quiet := QuietGoTestOutput(out, true)
	loud := QuietGoTestOutput(out, false)

	// Assert
	if want := "    a_test.go:3: a log line\ncoverage: 50.0% of statements\n"; string(quiet) != want {
		t.Fatalf("a passing chunk shows %q, want %q", quiet, want)
	}
	if string(loud) != string(out) {
		t.Fatal("a failing chunk's output was trimmed")
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
	// kind must be one of the six it switches on.
	for _, s := range roster.Suites {
		switch s.Kind {
		case roster.Script, roster.SplitScript, roster.ERT, roster.GoModule, roster.Vitest, roster.E2E:
		default:
			t.Errorf("suite %s has unknown kind %d", s.Name, s.Kind)
		}
	}
}

func TestEveryItemChunkReadsTheSharedItemLine(t *testing.T) {
	// Arrange: an ERT chunk and a split-harness chunk, built by their suites.
	module := t.TempDir()
	write(t, filepath.Join(module, "lisp", "test-agent-repl.el"),
		"(load (expand-file-name \"test-helpers.el\" dir) nil t)\n(load (expand-file-name \"x\" dir) nil t)\n", 0o644)
	ert, err := ertUnits(Layout{Repo: module, Module: module, Work: t.TempDir()}, roster.Suite{Name: "ert", Kind: roster.ERT, Path: "lisp"})
	if err != nil {
		t.Fatal(err)
	}
	harness := splitScriptUnitsForItems(filepath.Join(module, "bin", "h.sh"), roster.Suite{Name: "h", Kind: roster.SplitScript}, []string{"x"})
	chunks := map[string]func([]byte) (map[string]float64, error){
		"ert":     ert.Splits[0].Chunk("ert#00", []string{"x"}).Items,
		"harness": harness.Splits[0].Chunk("h#00", []string{"x"}).Items,
	}
	tests := []struct {
		name    string
		out     string
		wantErr string
	}{
		{name: "a well-formed line is read", out: "TESTRUN-ITEM x 2.5\n"},
		{name: "a malformed timing is refused", out: "TESTRUN-ITEM x slow\n", wantErr: "unreadable item timing"},
		{name: "a line with extra fields is refused", out: "TESTRUN-ITEM x 1 2\n", wantErr: "unreadable item timing"},
	}
	for _, tt := range tests {
		for kind, items := range chunks {
			t.Run(tt.name+"/"+kind, func(t *testing.T) {
				// Act
				got, err := items([]byte(tt.out))

				// Assert
				if tt.wantErr != "" {
					if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
						t.Fatalf("err = %v, want it to mention %q", err, tt.wantErr)
					}
					return
				}
				if err != nil || got["x"] != 2.5 {
					t.Fatalf("Items = %v, %v", got, err)
				}
			})
		}
	}
}

func TestQuietGoTestOutputKeepsALineLongerThanAnyScanBuffer(t *testing.T) {
	// Arrange: one test log line past the 64 MiB a scanner would have held.
	long := strings.Repeat("x", 65*1024*1024)
	out := []byte("=== RUN   TestA\n" + long + "\n--- PASS: TestA (0.10s)\nPASS\n")

	// Act
	quiet := QuietGoTestOutput(out, true)

	// Assert
	if want := long + "\n"; string(quiet) != want {
		t.Fatalf("a passing chunk shows %d bytes, want exactly the %d-byte log line", len(quiet), len(want))
	}
}

func TestQuietGoTestOutputTerminatesAnUnterminatedLastLine(t *testing.T) {
	// Arrange
	out := []byte("--- PASS: TestA (0.10s)\nok tail")

	// Act
	quiet := QuietGoTestOutput(out, true)

	// Assert
	if string(quiet) != "ok tail\n" {
		t.Fatalf("quiet = %q", quiet)
	}
}

func TestParseGoTestItemsReadsPastALineLongerThanAnyScanBuffer(t *testing.T) {
	// Arrange
	out := []byte(strings.Repeat("y", 65*1024*1024) + "\n--- PASS: TestA (1.50s)\n")

	// Act
	got, err := ParseGoTestItems(out, []string{"TestA"})

	// Assert
	if err != nil || got["TestA"] != 1.5 {
		t.Fatalf("ParseGoTestItems = %v, %v", got, err)
	}
}

func TestEveryGoModuleKindIsVetted(t *testing.T) {
	// Arrange: the same module built as an ordinary Go module and as the e2e
	// suite; each must carry exactly goVet's unit, since a compiled test
	// binary vets nothing.
	module := goPackageFixture(t, "func TestA(t *testing.T) {}\n")
	build := map[string]func(Layout, roster.Suite, string, []string) (Units, error){
		"go module": goModuleUnitsForPackages,
		"e2e":       e2eUnitsForPackages,
	}
	for kind, units := range build {
		t.Run(kind, func(t *testing.T) {
			l := Layout{Module: module, Work: t.TempDir()}

			// Act
			u, err := units(l, roster.Suite{Name: "m"}, module, []string{"p"})

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			want := goVet("m", module)
			var vets []run.Spec
			for _, a := range u.Atomic {
				if a.ID == want.ID {
					vets = append(vets, a)
				}
			}
			if len(vets) != 1 || !reflect.DeepEqual(vets[0].Argv, want.Argv) || vets[0].Dir != module {
				t.Fatalf("vet units = %+v, want exactly one with argv %v in %s", vets, want.Argv, module)
			}
		})
	}
}

func TestGoVetRunsTheAnalyzersGoTestRuns(t *testing.T) {
	// Act
	v := goVet("m", "/mod")

	// Assert
	want := []string{"go", "vet", "-atomic", "-bool", "-buildtags", "-directive", "-errorsas",
		"-ifaceassert", "-nilfunc", "-printf", "-stringintconv", "-tests", "./..."}
	if v.ID != "m:vet" || v.Suite != "m" || v.Dir != "/mod" || !reflect.DeepEqual(v.Argv, want) {
		t.Fatalf("goVet = %+v", v)
	}
}

func TestUnder(t *testing.T) {
	tests := []struct {
		name, path, want, wantErr string
	}{
		{name: "a file inside", path: "/p/test/a.ts", want: "test/a.ts"},
		{name: "the directory itself", path: "/p", want: "."},
		{name: "a sibling", path: "/q/a.ts", wantErr: "/q/a.ts is not under /p"},
		{name: "the parent", path: "/", wantErr: "/ is not under /p"},
		{name: "a name that only starts with dots", path: "/p/..a", want: "..a"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := under("/p", tt.path)

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want it to mention %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || got != tt.want {
				t.Fatalf("under = %q, %v; want %q", got, err, tt.want)
			}
		})
	}
}

func TestParseGoList(t *testing.T) {
	tests := []struct {
		name    string
		out     string
		want    []string
		wantErr string
	}{
		{name: "packages relative and sorted", out: "/m/z\n/m\n\n/m/a/b\n", want: []string{".", "a/b", "z"}},
		{name: "nothing tested", out: "\n", want: nil},
		{name: "a package outside the module fails", out: "/m/a\n/elsewhere\n", wantErr: "/elsewhere is not under /m"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := parseGoList("/m", []byte(tt.out))

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want it to mention %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("parseGoList = %v, %v; want %v", got, err, tt.want)
			}
		})
	}
}

func TestParseVitestList(t *testing.T) {
	tests := []struct {
		name    string
		out     string
		want    []string
		wantErr string
	}{
		{name: "files relative and sorted", out: `[{"file":"/p/test/b.test.ts"},{"file":"/p/src/a.test.ts"}]`, want: []string{"src/a.test.ts", "test/b.test.ts"}},
		{name: "not json fails", out: `vitest crashed`, wantErr: "printed no file list"},
		{name: "a file outside the package fails", out: `[{"file":"/other/a.test.ts"}]`, wantErr: "/other/a.test.ts is not under /p"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := parseVitestList("/p", []byte(tt.out))

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want it to mention %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("parseVitestList = %v, %v; want %v", got, err, tt.want)
			}
		})
	}
}
