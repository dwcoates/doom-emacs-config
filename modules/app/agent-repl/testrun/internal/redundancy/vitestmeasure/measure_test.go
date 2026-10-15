package vitestmeasure

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
	"time"

	"agentrepl/testrun/internal/redundancy/proc"
	"agentrepl/testrun/internal/run"
)

// fakeNpx answers the traced runs and Stryker for a package with two tests.
type fakeNpx struct {
	pkg string
	// traced are the lines each traced run writes; failRun fails it.
	traced  []tracedTest
	failRun bool
	report  string
	calls   []string
	// dirty is git status's answer on its nth call (0 before Stryker, 1
	// after); gitFail fails it.
	dirty   []string
	gitFail bool
	gits    int
}

func (f *fakeNpx) exec(_ context.Context, c proc.Cmd) (proc.Result, error) {
	f.calls = append(f.calls, c.Path+" "+strings.Join(c.Args, " "))
	switch {
	case c.Path == "sysctl":
		return proc.Result{Lines: []proc.Line{{Text: "{ 1.00 1.00 1.00 }"}}}, nil
	case c.Path == "npx" && c.Args[0] == "vitest":
		if f.failRun {
			return proc.Result{Exit: 1}, nil
		}
		var path string
		for _, e := range c.Env {
			if k, v, _ := strings.Cut(e, "="); k == TraceEnv {
				path = v
			}
		}
		var b bytes.Buffer
		for _, t := range f.traced {
			line, _ := json.Marshal(t)
			b.Write(append(line, '\n'))
		}
		return proc.Result{}, os.WriteFile(path, b.Bytes(), 0o644)
	case c.Path == "git":
		n := f.gits
		f.gits++
		if f.gitFail {
			return proc.Result{Exit: 128, Lines: []proc.Line{{Text: "fatal: not a git repository"}}}, nil
		}
		if n < len(f.dirty) && f.dirty[n] != "" {
			return proc.Result{Lines: []proc.Line{{Text: f.dirty[n]}}}, nil
		}
		return proc.Result{}, nil
	case c.Path == "npx" && c.Args[0] == "stryker":
		var cfg struct {
			JSONReporter struct {
				FileName string `json:"fileName"`
			} `json:"jsonReporter"`
		}
		b, err := os.ReadFile(c.Args[2])
		if err != nil {
			return proc.Result{}, err
		}
		if err := json.Unmarshal(b, &cfg); err != nil {
			return proc.Result{}, err
		}
		return proc.Result{}, os.WriteFile(cfg.JSONReporter.FileName, []byte(f.report), 0o644)
	}
	return proc.Result{}, fmt.Errorf("fake: unexpected %s %v", c.Path, c.Args)
}

func fakePackage(t *testing.T) (Config, *fakeNpx) {
	t.Helper()
	pkg := t.TempDir()
	if err := os.MkdirAll(filepath.Join(pkg, "node_modules", "vitest", "dist"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(pkg, "node_modules", "vitest", "dist", "config.js"), nil, 0o644); err != nil {
		t.Fatal(err)
	}
	test := filepath.Join(pkg, "test", "a.test.ts")
	src := filepath.Join(pkg, "src", "a.ts")
	f := &fakeNpx{pkg: pkg,
		traced: []tracedTest{
			{File: test, Name: "a > adds", State: "pass", MS: 2, Hit: []string{src + ":0", src + ":1"}},
			{File: test, Name: "a > compares", State: "pass", MS: 1, Hit: []string{src + ":1"}},
		},
		report: `{"files": {"src/a.ts": {"mutants": [
  {"id": "1", "mutatorName": "EqualityOperator", "replacement": "<=", "status": "Killed", "killedBy": ["t1", "t2"], "coveredBy": ["t1", "t2"], "location": {"start": {"line": 3, "column": 9}}},
  {"id": "2", "mutatorName": "BooleanLiteral", "replacement": "false", "status": "Killed", "killedBy": ["t1"], "coveredBy": ["t1"], "location": {"start": {"line": 4, "column": 1}}}
]}}, "testFiles": {"test/a.test.ts": {"tests": [{"id": "t1", "name": "a > adds"}, {"id": "t2", "name": "a > compares"}]}}}`,
	}
	var log bytes.Buffer
	cfg := Config{
		Package: pkg, System: "webapp", ConfigFile: "vitest.config.ts", Mutate: []string{"src/**/*.ts"}, Runs: 2,
		Trace: "/m/testrun/vitest/redundancy-trace.mjs", Scratch: t.TempDir(),
		Gate: proc.Gate{Log: &run.Log{Out: &log, Err: &log}, MaxLoad: 32, QuietWait: time.Minute},
	}
	return cfg, f
}

func quiet() *run.Log {
	var b bytes.Buffer
	return &run.Log{Out: &b, Err: &b}
}

func TestMeasureJoinsTheTraceAndStrykersKills(t *testing.T) {
	// Arrange
	cfg, f := fakePackage(t)

	// Act
	m, err := Measure(context.Background(), cfg, f.exec, quiet())

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	got := map[string][]string{}
	blocks := map[string][]string{}
	for _, c := range m.Cases {
		got[c.ID] = c.Kills
		blocks[c.ID] = c.Blocks
	}
	want := map[string][]string{
		"test/a.test.ts:a > adds":     {"src/a.ts:3:9:EqualityOperator:<=", "src/a.ts:4:1:BooleanLiteral:false"},
		"test/a.test.ts:a > compares": {"src/a.ts:3:9:EqualityOperator:<="},
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("kills = %v", got)
	}
	if !reflect.DeepEqual(blocks["test/a.test.ts:a > adds"], []string{"webapp/src/a.ts:0", "webapp/src/a.ts:1"}) || len(m.Statements) != 2 {
		t.Fatalf("blocks %v, statements %v", blocks, m.Statements)
	}
	traced := 0
	for _, c := range f.calls {
		if strings.HasPrefix(c, "npx vitest run --config") {
			traced++
		}
	}
	if traced != 2 {
		t.Fatalf("%d traced runs, want 2", traced)
	}
}

func TestMeasureVerifiesADeletionSet(t *testing.T) {
	// Arrange: deleting "compares" loses nothing, "adds" kills it too.
	cfg, f := fakePackage(t)
	cfg.Delete = []string{"test/a.test.ts:a > compares"}

	// Act
	m, err := Measure(context.Background(), cfg, f.exec, quiet())

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if m.Verification == nil || !reflect.DeepEqual(m.Verification.Proven, []string{"test/a.test.ts:a > compares"}) {
		t.Fatalf("verification = %+v", m.Verification)
	}
}

func TestMeasureRefuses(t *testing.T) {
	tests := []struct {
		name  string
		setup func(*Config, *fakeNpx)
		want  string
	}{
		{name: "a traced run that fails", setup: func(_ *Config, f *fakeNpx) { f.failRun = true }, want: "traced run 1 of vitest.config.ts failed with no mutant"},
		{name: "a test that fails in the trace", setup: func(_ *Config, f *fakeNpx) { f.traced[0].State = "fail" }, want: "a > adds failed on traced run 1"},
		{name: "a Stryker test the trace never ran", setup: func(_ *Config, f *fakeNpx) { f.traced = f.traced[:1] }, want: "Stryker names tests the traced runs never ran"},
		{name: "no vitest installed", setup: func(c *Config, _ *fakeNpx) {
			if err := os.RemoveAll(filepath.Join(c.Package, "node_modules")); err != nil {
				t.Fatal(err)
			}
		}, want: "has no vitest installed"},
		{name: "nothing to mutate", setup: func(c *Config, _ *fakeNpx) { c.Mutate = nil }, want: "nothing to mutate"},
		{name: "no load gate", setup: func(c *Config, _ *fakeNpx) { c.Gate.MaxLoad = 0 }, want: "the load gate needs"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			cfg, f := fakePackage(t)
			tt.setup(&cfg, f)

			// Act
			_, err := Measure(context.Background(), cfg, f.exec, quiet())

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("err = %v, want %q", err, tt.want)
			}
		})
	}
}

func TestTheTraceConfigAddsTheTracerToThePackagesOwn(t *testing.T) {
	// Arrange
	cfg, f := fakePackage(t)
	m := &measurer{cfg: cfg, counter: &proc.Counter{Exec: f.exec, Name: "vitestmeasure"}, log: quiet()}

	// Act
	path, err := m.traceConfig(t.TempDir())

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	b, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{
		`import base from "` + filepath.Join(cfg.Package, "vitest.config.ts") + `"`,
		`setupFiles: ["/m/testrun/vitest/redundancy-trace.mjs"]`,
		"isolate: true", "maxWorkers: 1", `provider: "istanbul"`, `root: "` + cfg.Package + `"`,
	} {
		if !strings.Contains(string(b), want) {
			t.Errorf("the trace config lacks %q:\n%s", want, b)
		}
	}
}

func TestStrykerMutatesThePackageInPlace(t *testing.T) {
	// Arrange
	cfg, f := fakePackage(t)

	// Act
	if _, err := Measure(context.Background(), cfg, f.exec, quiet()); err != nil {
		t.Fatal(err)
	}

	// Assert
	b, err := os.ReadFile(filepath.Join(cfg.Scratch, "stryker.config.json"))
	if err != nil {
		t.Fatal(err)
	}
	var c struct {
		InPlace bool `json:"inPlace"`
	}
	if err := json.Unmarshal(b, &c); err != nil || !c.InPlace {
		t.Fatalf("config %s: want inPlace (err %v)", b, err)
	}
	if f.gits != 2 {
		t.Fatalf("%d clean-tree checks, want one each side of Stryker", f.gits)
	}
}

func TestMeasureRefusesATreeThatIsNotClean(t *testing.T) {
	tests := []struct {
		name    string
		dirty   []string
		gitFail bool
		want    string
		stryker bool
	}{
		{name: "an edit before Stryker", dirty: []string{" M src/a.ts"}, want: "has tracked changes before Stryker", stryker: false},
		{name: "a mutant left after Stryker", dirty: []string{"", " M src/a.ts"}, want: "has tracked changes after Stryker", stryker: true},
		{name: "git status failing", gitFail: true, want: "git status in", stryker: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			cfg, f := fakePackage(t)
			f.dirty, f.gitFail = tt.dirty, tt.gitFail

			// Act
			_, err := Measure(context.Background(), cfg, f.exec, quiet())

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) || !strings.Contains(err.Error(), cfg.Package) {
				t.Fatalf("err = %v, want %q naming the package", err, tt.want)
			}
			ran := false
			for _, c := range f.calls {
				ran = ran || strings.HasPrefix(c, "npx stryker")
			}
			if ran != tt.stryker {
				t.Fatalf("Stryker ran: %t, want %t", ran, tt.stryker)
			}
		})
	}
}
