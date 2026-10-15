package gomeasure

import (
	"bytes"
	"context"
	"os"
	"path/filepath"
	"reflect"
	"regexp"
	"strings"
	"testing"
	"time"

	"agentrepl/testrun/internal/redundancy"
)

func TestParseArgsDefaults(t *testing.T) {
	// Act
	a, err := ParseArgs([]string{"-module", "/m", "-pkg", "./p", "-tests", "a_test.go, b_test.go", "-out", "/o"}, &bytes.Buffer{})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if !reflect.DeepEqual(a.Suites, []Suite{{Package: "./p", Tests: []string{"a_test.go", "b_test.go"}}}) {
		t.Fatalf("suites = %+v", a.Suites)
	}
	if !reflect.DeepEqual(a.Mutate, []string{"./p"}) || a.Runs != 3 || a.Common.Universe != redundancy.KillsOnly || a.Common.Overhead != DefaultOverhead || a.HangBound != DefaultHangBound || a.Common.Budget.MaxDrop != redundancy.DefaultMaxDrop {
		t.Fatalf("defaults = %+v", a)
	}
	if !reflect.DeepEqual(a.PinCalls, DefaultPinCalls) {
		t.Fatalf("pin calls = %v", a.PinCalls)
	}
}

func TestParseArgsRefuses(t *testing.T) {
	base := []string{"-module", "/m", "-pkg", "./p", "-tests", "a_test.go", "-out", "/o"}
	tests := []struct {
		name string
		argv []string
		want string
	}{
		{name: "no -module", argv: []string{"-pkg", "./p", "-tests", "a_test.go", "-out", "/o"}, want: "-module is required"},
		{name: "neither -pkg nor -suites", argv: []string{"-module", "/m", "-out", "/o"}, want: "exactly one of -pkg, -suites and -roster-suite"},
		{name: "both -pkg and -suites", argv: []string{"-module", "/m", "-pkg", "./p", "-suites", "/s.json", "-out", "/o"}, want: "exactly one of -pkg, -suites and -roster-suite"},
		{name: "-tests beside -suites", argv: []string{"-module", "/m", "-suites", "/s.json", "-tests", "a_test.go", "-out", "/o"}, want: "each suite says its own"},
		{name: "an unreadable -suites", argv: []string{"-module", "/m", "-suites", "/gone.json", "-out", "/o"}, want: "read -suites /gone.json"},
		{name: "an unknown universe", argv: append(base, "-universe", "lines"), want: "unknown universe"},
		{name: "a stray argument", argv: append(base, "extra"), want: "unexpected arguments: extra"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, err := ParseArgs(tt.argv, &bytes.Buffer{})

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("err = %v, want %q", err, tt.want)
			}
		})
	}
}

func TestCommandRefusesToRunAtNormalPriority(t *testing.T) {
	// Arrange
	t.Setenv(EnvBackground, "")
	log, out := quietLog()

	// Act
	code := Command(context.Background(), log, nil, nil)

	// Assert
	if code != 78 || !strings.Contains(out.String(), "ERROR: redundancy: refused at normal priority") {
		t.Fatalf("code %d, log %q", code, out.String())
	}
}

func TestCommandLogsAFailedMeasurementOnce(t *testing.T) {
	// Arrange
	t.Setenv(EnvBackground, "1")
	cfg, dir := fakeConfig(t)
	f := &fakeGo{dir: dir, reaches: map[string]bool{"TestA": true}, coverFail: "TestA"}
	log, out := quietLog()
	outDir := t.TempDir()

	// Act
	code := Command(context.Background(), log, []string{"-module", cfg.Module, "-pkg", ".", "-tests", "p_test.go", "-out", outDir, "-runs", "1"}, f.exec)

	// Assert
	if code != 1 {
		t.Fatalf("code %d", code)
	}
	if n := strings.Count(out.String(), "ERROR:"); n != 1 || !strings.Contains(out.String(), "redundancy: measure .: gomeasure: TestA failed on measurement run 1") {
		t.Fatalf("log = %q, want one error naming the package and the cause", out.String())
	}
	if _, err := os.Stat(filepath.Join(outDir, "measurement.json")); !os.IsNotExist(err) {
		t.Fatalf("a failed measurement wrote measurement.json (stat err %v)", err)
	}
}

func TestCommandWritesTheMeasurementAndTheReport(t *testing.T) {
	// Arrange
	t.Setenv(EnvBackground, "1")
	cfg, dir := fakeConfig(t)
	f := &fakeGo{dir: dir, reaches: map[string]bool{"TestA": true}}
	pass := func(string) Result { return Result{Lines: passLines("TestA")} }
	f.mutant = map[string]func(string) Result{"1": pass, "2": pass}
	log, _ := quietLog()
	outDir := t.TempDir()

	// Act
	code := Command(context.Background(), log, []string{"-module", cfg.Module, "-pkg", ".", "-tests", "p_test.go", "-out", outDir, "-runs", "1"}, f.exec)

	// Assert
	if code != 0 {
		t.Fatalf("code %d", code)
	}
	m, err := redundancy.ReadMeasurement(filepath.Join(outDir, "measurement.json"))
	if err != nil {
		t.Fatal(err)
	}
	if len(m.Cases) != 4 {
		t.Fatalf("%d cases saved, want 4", len(m.Cases))
	}
	report, err := os.ReadFile(filepath.Join(outDir, "report.md"))
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(report), "- Cases: 4, pinned: 1,") {
		t.Fatalf("report = %s", report)
	}
}

func TestParseArgsReadsASuitesFile(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "suites.json")
	body := `[{"Package":"./internal/workspace"},{"Package":"./integration","Tags":"integration","Programs":["agent-repld"],"Kept":true}]`
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	a, err := ParseArgs([]string{"-module", "/m", "-suites", path, "-mutate", "./internal/workspace", "-out", "/o"}, &bytes.Buffer{})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	want := []Suite{{Package: "./internal/workspace"}, {Package: "./integration", Tags: "integration", Programs: []string{"agent-repld"}, Kept: true}}
	if !reflect.DeepEqual(a.Suites, want) {
		t.Fatalf("suites = %+v", a.Suites)
	}
}

func TestParseArgsRefusesASuitesFile(t *testing.T) {
	tests := []struct {
		name, body, want string
		mutate           bool
	}{
		{name: "several suites with no -mutate", body: `[{"Package":"a"},{"Package":"b"}]`, want: "-mutate is required"},
		{name: "no suite", body: `[]`, mutate: true, want: "names no suite"},
		{name: "a suite with no package", body: `[{"Tags":"x"}]`, mutate: true, want: "suite 0 names no Package"},
		{name: "not JSON", body: `[`, mutate: true, want: "decode -suites"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := filepath.Join(t.TempDir(), "suites.json")
			if err := os.WriteFile(path, []byte(tt.body), 0o644); err != nil {
				t.Fatal(err)
			}
			argv := []string{"-module", "/m", "-suites", path, "-out", "/o"}
			if tt.mutate {
				argv = append(argv, "-mutate", "a")
			}

			// Act
			_, err := ParseArgs(argv, &bytes.Buffer{})

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("err = %v, want %q", err, tt.want)
			}
		})
	}
}

func TestParseArgsReadsADeletionSet(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "delete.txt")
	if err := os.WriteFile(path, []byte("# the audit's rows\n./integration:TestA\n\n./integration:TestB\n"), 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	a, err := ParseArgs([]string{"-module", "/m", "-pkg", "./p", "-out", "/o", "-delete", path, "-estimate"}, &bytes.Buffer{})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if !reflect.DeepEqual(a.Delete, []string{"./integration:TestA", "./integration:TestB"}) || !a.Estimate {
		t.Fatalf("delete %v estimate %t", a.Delete, a.Estimate)
	}
}

func TestParseArgsRefusesADeletionSet(t *testing.T) {
	tests := []struct {
		name string
		body *string
		argv []string
		want string
	}{
		{name: "an estimate of nothing", argv: []string{"-estimate"}, want: "-estimate needs -delete"},
		{name: "an empty deletion set", body: strPtr("# nothing\n"), want: "names no case"},
		{name: "a missing deletion set", want: "read -delete"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			argv := append([]string{"-module", "/m", "-pkg", "./p", "-out", "/o"}, tt.argv...)
			if tt.argv == nil {
				path := filepath.Join(t.TempDir(), "delete.txt")
				if tt.body != nil {
					if err := os.WriteFile(path, []byte(*tt.body), 0o644); err != nil {
						t.Fatal(err)
					}
				}
				argv = append(argv, "-delete", path)
			}

			// Act
			_, err := ParseArgs(argv, &bytes.Buffer{})

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("err = %v, want %q", err, tt.want)
			}
		})
	}
}

func strPtr(s string) *string { return &s }

func TestParseArgsMeasuresARosterSuitesEveryPackage(t *testing.T) {
	// Act: the daemon, found from this test's own place in the repository.
	a, err := ParseArgs([]string{"-roster-suite", "daemon", "-mutate", "./internal/...", "-out", "/o"}, &bytes.Buffer{})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if filepath.Base(a.Module) != "daemon" {
		t.Fatalf("module = %s, want the daemon's", a.Module)
	}
	var sawUnit, sawIntegration bool
	for _, s := range a.Suites {
		if s.Package == "./internal/workspace" && s.Tags == "" && len(s.Programs) == 0 {
			sawUnit = true
		}
		if s.Package == "./integration" && s.Tags == "integration" && reflect.DeepEqual(s.Programs, []string{"agent-repld", "fakeshim", "git"}) {
			sawIntegration = true
		}
	}
	if !sawUnit || !sawIntegration {
		t.Fatalf("suites lack the workspace unit package (%t) or the tagged integration pass with its programs (%t): %+v", sawUnit, sawIntegration, a.Suites)
	}
}

func TestParseArgsRefusesARosterSuiteThatIsNoGoSuite(t *testing.T) {
	for _, name := range []string{"webapp", "no-such"} {
		// Act
		_, err := ParseArgs([]string{"-roster-suite", name, "-out", "/o"}, &bytes.Buffer{})

		// Assert
		if err == nil || !strings.Contains(err.Error(), "is no Go suite of the roster") {
			t.Errorf("%s: err = %v", name, err)
		}
	}
}

func TestFindRootRefusesADirectoryOutsideTheRepository(t *testing.T) {
	// Act
	_, err := FindRoot(t.TempDir())

	// Assert
	if err == nil || !strings.Contains(err.Error(), "holds testrun/roster") {
		t.Fatalf("err = %v", err)
	}
}

func TestParseArgsMeasuresTheE2ESuiteBesideTheDaemon(t *testing.T) {
	// Arrange
	root, err := FindRoot(".")
	if err != nil {
		t.Fatal(err)
	}

	// Act
	a, err := ParseArgs([]string{"-roster-suite", "daemon,e2e", "-module", filepath.Join(root, "daemon"), "-mutate", "./internal/...", "-out", "/o"}, &bytes.Buffer{})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	sawE2E := false
	for _, s := range a.Suites {
		if s.Module == "e2e" && s.Package == "./." && s.Prebuild && len(s.Programs) == 6 {
			sawE2E = true
		}
		if s.Module == "" {
			t.Errorf("suite %s names no module though the measurement spans two", s.Package)
		}
	}
	if !sawE2E {
		t.Fatalf("no e2e root suite with its prebuild and six programs: %+v", a.Suites)
	}
}

func TestParseArgsRefusesSeveralRosterSuitesWithNoModule(t *testing.T) {
	// Act
	_, err := ParseArgs([]string{"-roster-suite", "daemon,e2e", "-out", "/o"}, &bytes.Buffer{})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "-module must name the mutated one") {
		t.Fatalf("err = %v", err)
	}
}

func TestParseArgsHasTheParentsLoadGateByDefault(t *testing.T) {
	// Act
	a, err := ParseArgs([]string{"-module", "/m", "-pkg", "./p", "-out", "/o"}, &bytes.Buffer{})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if a.MaxLoad != 32 || a.QuietWait != time.Hour {
		t.Fatalf("max load %g, quiet wait %s", a.MaxLoad, a.QuietWait)
	}
}

// TestEveryDefaultPinCallIsAHarnessOrLoggerFunction holds the default pin
// list to the code: a renamed helper would silently stop pinning.
func TestEveryDefaultPinCallIsAHarnessOrLoggerFunction(t *testing.T) {
	root, err := FindRoot(".")
	if err != nil {
		t.Fatal(err)
	}
	var src strings.Builder
	for _, glob := range []string{"daemon/internal/dlog/*.go", "daemon/integration/harness/*.go"} {
		files, err := filepath.Glob(filepath.Join(root, glob))
		if err != nil {
			t.Fatal(err)
		}
		for _, f := range files {
			b, err := os.ReadFile(f)
			if err != nil {
				t.Fatal(err)
			}
			src.Write(b)
		}
	}
	for _, name := range DefaultPinCalls {
		if !regexp.MustCompile(`func (\([^)]*\) )?` + name + `\(`).MatchString(src.String()) {
			t.Errorf("no dlog or harness function is named %s", name)
		}
	}
}
