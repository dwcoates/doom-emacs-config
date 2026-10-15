package gomeasure

import (
	"bytes"
	"context"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"

	"agentrepl/testrun/internal/redundancy"
)

func TestParseArgsDefaults(t *testing.T) {
	// Act
	a, err := ParseArgs([]string{"-module", "/m", "-pkg", "./p", "-tests", "a_test.go, b_test.go", "-out", "/o"}, &bytes.Buffer{})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if !reflect.DeepEqual(a.Mutate, []string{"./p"}) || a.Runs != 3 || a.Universe != redundancy.KillsOnly || a.Overhead != DefaultOverhead || a.HangBound != DefaultHangBound || a.Budget.MaxDrop != redundancy.DefaultMaxDrop {
		t.Fatalf("defaults = %+v", a)
	}
	if !reflect.DeepEqual(a.Tests, []string{"a_test.go", "b_test.go"}) || !reflect.DeepEqual(a.PinCalls, []string{"FindRecord", "Records"}) {
		t.Fatalf("lists = %v, %v", a.Tests, a.PinCalls)
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
		{name: "no -tests", argv: []string{"-module", "/m", "-pkg", "./p", "-out", "/o"}, want: "-tests is required"},
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
	if n := strings.Count(out.String(), "ERROR:"); n != 1 || !strings.Contains(out.String(), "redundancy: measure . p_test.go: gomeasure: TestA failed on measurement run 1") {
		t.Fatalf("log = %q, want one error naming the package, files and cause", out.String())
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
