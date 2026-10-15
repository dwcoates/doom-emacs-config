package gomeasure

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"regexp"
	"slices"
	"strings"
	"testing"
	"time"

	"agentrepl/testrun/internal/redundancy"
	"agentrepl/testrun/internal/redundancy/gomut"
	"agentrepl/testrun/internal/run"
)

// The fake package: one production function whose comparison is two
// mutants, and three top-level tests, one with two subtests and one that
// asserts a log record.
const fakeProd = "package p\n\nfunc F(a int) bool { return a < 1 }\n"

const fakeTests = `package p

import "testing"

func FindRecord() {}

func TestA(t *testing.T) {}

func TestB(t *testing.T) {
	t.Run("x", func(t *testing.T) {})
	t.Run("y", func(t *testing.T) {})
}

func TestLog(t *testing.T) { FindRecord() }
`

// fakeBlock is F's body; the comparison's mutants sit at 3.29.
const fakeBlock = "fix/p/p.go:3.20,3.37"

// fakeGo answers the go commands and test binaries a measurement runs.
type fakeGo struct {
	dir string
	// reaches says which leaves reach F.
	reaches map[string]bool
	// coverFail fails a leaf's coverage run.
	coverFail string
	// baselineFail fails a leaf in the schema with no mutant active.
	baselineFail string
	// schemaErr is the schema build's output; it fails once with it.
	schemaErr string
	// mutant scripts each mutant's group run, then each lone rerun.
	mutant map[string]func(pattern string) Result
	// mutantCmd, when it holds the mutant, scripts it from the whole command.
	mutantCmd map[string]func(Cmd) Result
	calls  []string
}

func passLines(names ...string) []Line {
	var texts []string
	for _, n := range names {
		texts = append(texts, "=== RUN   "+n)
	}
	for _, n := range names {
		texts = append(texts, "--- PASS: "+n+" (0.00s)")
	}
	return lines(texts...)
}

func argAfter(args []string, flag string) string {
	for i, a := range args {
		if a == flag && i+1 < len(args) {
			return args[i+1]
		}
	}
	return ""
}

func envOf(env []string, key string) string {
	v := ""
	for _, e := range env {
		if k, val, ok := strings.Cut(e, "="); ok && k == key {
			v = val
		}
	}
	return v
}

// leavesOf maps a -test.run pattern back to the fake's leaves it selects,
// level by level as the go test runner matches.
func leavesOf(pattern string) []string {
	levels := strings.Split(pattern, "/")
	var out []string
	for _, l := range []string{"TestA", "TestB/x", "TestB/y", "TestLog"} {
		parts := strings.Split(l, "/")
		ok := true
		for i, lv := range levels {
			if i >= len(parts) || !regexp.MustCompile(lv).MatchString(parts[i]) {
				ok = false
				break
			}
		}
		if ok {
			out = append(out, l)
		}
	}
	return out
}

func withParents(leaves []string) []string {
	var out []string
	for _, l := range leaves {
		if strings.Contains(l, "/") && !slices.Contains(out, Top(l)) {
			out = append(out, Top(l))
		}
		out = append(out, l)
	}
	return out
}

func (f *fakeGo) exec(_ context.Context, c Cmd) (Result, error) {
	base := filepath.Base(c.Path)
	f.calls = append(f.calls, base+" "+strings.Join(c.Args, " "))
	switch {
	case base == "go" && c.Args[0] == "list":
		return Result{Lines: lines("fix/p\t" + f.dir)}, nil
	case base == "go" && c.Args[0] == "test":
		if slices.Contains(c.Args, "-overlay") && f.schemaErr != "" {
			out := f.schemaErr
			f.schemaErr = ""
			return Result{Exit: 1, Lines: lines(strings.Split(out, "\n")...)}, nil
		}
		return Result{}, nil
	case base == "go" && c.Args[0] == "tool":
		out := strings.TrimPrefix(c.Args[len(c.Args)-1], "-o=")
		in := strings.TrimPrefix(c.Args[len(c.Args)-2], "-i=")
		leaf := ""
		for _, l := range []string{"TestA", "TestB/x", "TestB/y", "TestLog"} {
			if strings.Contains(in, "/"+fmt.Sprint(slices.Index([]string{"TestA", "TestB/x", "TestB/y", "TestLog"}, l))+",") || strings.HasSuffix(in, "/"+fmt.Sprint(slices.Index([]string{"TestA", "TestB/x", "TestB/y", "TestLog"}, l))) {
				leaf = l
			}
		}
		count := 0
		if f.reaches[leaf] {
			count = 1
		}
		prof := fmt.Sprintf("mode: atomic\n%s 1 %d\n", fakeBlock, count)
		return Result{}, os.WriteFile(out, []byte(prof), 0o644)
	case base == "cover.test":
		pattern := argAfter(c.Args, "-test.run")
		leaves := leavesOf(pattern)
		if f.coverFail != "" && pattern == RunPattern(f.coverFail) {
			return Result{Exit: 1, Lines: lines("=== RUN   "+f.coverFail, "--- FAIL: "+f.coverFail+" (0.00s)")}, nil
		}
		return Result{Lines: passLines(withParents(leaves)...), Wall: 50 * time.Millisecond}, nil
	case base == "mutants.test":
		pattern := argAfter(c.Args, "-test.run")
		id := envOf(c.Env, gomut.EnvMutant)
		if id == "" {
			leaves := leavesOf(pattern)
			if slices.Contains(leaves, f.baselineFail) {
				return Result{Exit: 1, Lines: lines("=== RUN   "+f.baselineFail, "--- FAIL: "+f.baselineFail+" (0.00s)")}, nil
			}
			return Result{Lines: passLines(withParents(leaves)...)}, nil
		}
		if fn, ok := f.mutantCmd[id]; ok {
			return fn(c), nil
		}
		return f.mutant[id](pattern), nil
	}
	return Result{}, fmt.Errorf("fake: unexpected command %s %v", c.Path, c.Args)
}

func fakeConfig(t *testing.T) (Config, string) {
	t.Helper()
	dir := writeFiles(t, map[string]string{"p.go": fakeProd, "p_test.go": fakeTests})
	return Config{
		Module: dir, Package: ".", Tests: []string{"p_test.go"}, Mutate: []string{"."},
		Runs: 2, PinCalls: []string{"FindRecord"}, Scratch: t.TempDir(), Go: "go", HangBound: 3 * time.Second,
	}, dir
}

func quietLog() (*run.Log, *bytes.Buffer) {
	var b bytes.Buffer
	return &run.Log{Out: &b, Err: &b}, &b
}

func TestMeasureAttributesEachMutantsKillsToTheLeavesThatFail(t *testing.T) {
	// Arrange: mutant 1 fails TestA and makes TestB/y panic in the group
	// run (it then fails alone); mutant 2 hangs TestA in the group run, and
	// alone TestA passes, so the hang was load, not a kill.
	cfg, dir := fakeConfig(t)
	f := &fakeGo{dir: dir, reaches: map[string]bool{"TestA": true, "TestB/x": true, "TestB/y": true}}
	// A subtest's result line comes with its parent's, so the panic leaves
	// TestB/x unreported too, and it reruns alone.
	lone := map[string]Result{
		RunPattern("TestA"):   {Lines: passLines("TestA")},
		RunPattern("TestB/x"): {Lines: passLines("TestB", "TestB/x")},
		RunPattern("TestB/y"): {Lines: passLines("TestB", "TestB/y")},
	}
	f.mutant = map[string]func(string) Result{
		"1": func(p string) Result {
			if p == RunPattern("TestB/y") {
				return Result{Exit: 1, Lines: lines("=== RUN   TestB", "=== RUN   TestB/y", "--- FAIL: TestB (0.00s)", "    --- FAIL: TestB/y (0.00s)")}
			}
			if r, ok := lone[p]; ok {
				return r
			}
			return Result{Exit: 2, Lines: lines("=== RUN   TestA", "--- FAIL: TestA (0.00s)", "=== RUN   TestB", "=== RUN   TestB/x", "=== RUN   TestB/y", "panic: boom")}
		},
		"2": func(p string) Result {
			if r, ok := lone[p]; ok {
				return r
			}
			return Result{Exit: 2, Lines: lines("=== RUN   TestA", "panic: test timed out after 1s", "\trunning tests:", "\t\tTestA (1s)")}
		},
	}
	log, _ := quietLog()

	// Act
	m, err := Measure(context.Background(), cfg, f.exec, log)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	kills := map[string][]string{}
	for _, c := range m.Cases {
		kills[c.ID] = c.Kills
	}
	want := map[string][]string{
		"TestA":   {"p.go:3:29:< -> <="},
		"TestB/x": nil,
		"TestB/y": {"p.go:3:29:< -> <="},
		"TestLog": nil,
	}
	if !reflect.DeepEqual(kills, want) {
		t.Fatalf("kills = %v\nwant    %v", kills, want)
	}
	status := map[string]string{}
	for _, r := range m.Mutants {
		status[r.Key] = r.Status
	}
	if !reflect.DeepEqual(status, map[string]string{"p.go:3:29:< -> <=": "killed", "p.go:3:29:< -> >=": "survived"}) {
		t.Fatalf("mutant statuses = %v", status)
	}
}

func TestMeasurePinsTheLogAssertionAndUnionsTheBlocks(t *testing.T) {
	// Arrange
	cfg, dir := fakeConfig(t)
	cfg.Pins = []string{"TestB/x"}
	f := &fakeGo{dir: dir, reaches: map[string]bool{"TestA": true}}
	f.mutant = map[string]func(string) Result{
		"1": func(string) Result { return Result{Lines: passLines("TestA")} },
		"2": func(string) Result { return Result{Lines: passLines("TestA")} },
	}
	log, _ := quietLog()

	// Act
	m, err := Measure(context.Background(), cfg, f.exec, log)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	pinned := map[string]string{}
	for _, c := range m.Cases {
		pinned[c.ID] = c.Pinned
		if c.ID == "TestA" && !reflect.DeepEqual(c.Blocks, []string{fakeBlock}) {
			t.Errorf("TestA's blocks = %v", c.Blocks)
		}
	}
	want := map[string]string{
		"TestA": "", "TestB/x": "named with -pin", "TestB/y": "",
		"TestLog": "asserts a log record: TestLog -> FindRecord",
	}
	if !reflect.DeepEqual(pinned, want) {
		t.Fatalf("pins = %v, want %v", pinned, want)
	}
	if got := len(m.Cost.Phases); got != 6 {
		t.Errorf("%d phases timed, want 6", got)
	}
}

func TestMeasureDropsTheSiteASchemaCompileErrorNames(t *testing.T) {
	// Arrange
	cfg, dir := fakeConfig(t)
	f := &fakeGo{dir: dir, reaches: map[string]bool{"TestA": true},
		schemaErr: filepath.Join(dir, "p.go") + ":3:29: cannot use x\n"}
	f.mutant = map[string]func(string) Result{}
	log, _ := quietLog()

	// Act
	m, err := Measure(context.Background(), cfg, f.exec, log)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	for _, r := range m.Mutants {
		if r.Status != "dropped" {
			t.Errorf("%s is %s, want dropped", r.Key, r.Status)
		}
	}
}

func TestMeasureRefuses(t *testing.T) {
	tests := []struct {
		name  string
		setup func(cfg *Config, f *fakeGo)
		want  string
	}{
		{
			name:  "a case that fails a coverage run with no mutant",
			setup: func(_ *Config, f *fakeGo) { f.coverFail = "TestB/y" },
			want:  "TestB/y failed on measurement run 1 with no mutant",
		},
		{
			name:  "a case the schema fails with no mutant active",
			setup: func(_ *Config, f *fakeGo) { f.baselineFail = "TestA" },
			want:  "TestA does not pass in the mutant schema with no mutant active",
		},
		{
			name: "a schema compile error in no mutated file",
			setup: func(_ *Config, f *fakeGo) {
				f.schemaErr = "/elsewhere/q.go:1:1: undefined: x\n"
			},
			want: "no error names a mutated file",
		},
		{
			name:  "zero runs",
			setup: func(cfg *Config, _ *fakeGo) { cfg.Runs = 0 },
			want:  "runs must be at least 1",
		},
		{
			name:  "no package to mutate",
			setup: func(cfg *Config, _ *fakeGo) { cfg.Mutate = nil },
			want:  "no package to mutate",
		},
		{
			name:  "cases that reach no mutated file",
			setup: func(_ *Config, f *fakeGo) { f.reaches = nil },
			want:  "reach no production file",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			cfg, dir := fakeConfig(t)
			f := &fakeGo{dir: dir, reaches: map[string]bool{"TestA": true}, mutant: map[string]func(string) Result{}}
			tt.setup(&cfg, f)
			log, _ := quietLog()

			// Act
			_, err := Measure(context.Background(), cfg, f.exec, log)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("err = %v, want %q", err, tt.want)
			}
		})
	}
}

func TestMeasureFeedsTheMinimization(t *testing.T) {
	// Arrange: TestA and TestB/y kill the same mutant; one of them goes.
	cfg, dir := fakeConfig(t)
	f := &fakeGo{dir: dir, reaches: map[string]bool{"TestA": true, "TestB/y": true}}
	fail := func(string) Result {
		return Result{Exit: 1, Lines: lines("=== RUN   TestA", "--- FAIL: TestA (0.00s)", "=== RUN   TestB", "=== RUN   TestB/y", "--- FAIL: TestB (0.00s)", "    --- FAIL: TestB/y (0.00s)")}
	}
	f.mutant = map[string]func(string) Result{"1": fail, "2": fail}
	log, _ := quietLog()
	m, err := Measure(context.Background(), cfg, f.exec, log)
	if err != nil {
		t.Fatal(err)
	}

	// Act
	r, err := redundancy.Minimize(m.Cases, redundancy.KillsOnly, 0)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	var removed []string
	for _, rm := range r.Removed {
		removed = append(removed, rm.Case)
	}
	slices.Sort(removed)
	// TestB/x kills nothing, and of TestA and TestB/y exactly one stays.
	if len(removed) != 2 || removed[0] != "TestB/x" {
		t.Fatalf("removed = %v, want TestB/x and one of TestA, TestB/y", removed)
	}
}

func TestMeasureCountsAHangAloneAsAKill(t *testing.T) {
	// Arrange: under mutant 1 TestA hangs in the group run and alone.
	cfg, dir := fakeConfig(t)
	f := &fakeGo{dir: dir, reaches: map[string]bool{"TestA": true}}
	hang := func(string) Result {
		return Result{Exit: 2, Lines: lines("=== RUN   TestA", "panic: test timed out after 1s", "\trunning tests:", "\t\tTestA (1s)")}
	}
	pass := func(string) Result { return Result{Lines: passLines("TestA")} }
	f.mutant = map[string]func(string) Result{"1": hang, "2": pass}
	log, _ := quietLog()

	// Act
	m, err := Measure(context.Background(), cfg, f.exec, log)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	for _, c := range m.Cases {
		if c.ID == "TestA" && !reflect.DeepEqual(c.Kills, []string{"p.go:3:29:< -> <="}) {
			t.Fatalf("TestA kills %v, want the mutant it hangs under", c.Kills)
		}
	}
	if len(m.Hangs) != 1 || !strings.HasPrefix(m.Hangs[0], "p.go:3:29:< -> <=: 1 cases hung (confirmed on TestA at 3s)") {
		t.Fatalf("hangs = %v, want TestA's confirmed hang listed", m.Hangs)
	}
}

func TestMeasureCountsACaseAMutantOnlySlowsAsNoKill(t *testing.T) {
	// Arrange: under mutant 1 TestA and TestB/y run past their healthy bound
	// but finish within the hang bound; TestB/y then fails, TestA passes.
	cfg, dir := fakeConfig(t)
	f := &fakeGo{dir: dir, reaches: map[string]bool{"TestA": true, "TestB/y": true}}
	slow := func(c Cmd) Result {
		p := argAfter(c.Args, "-test.run")
		if argAfter(c.Args, "-test.timeout") != cfg.HangBound.String() {
			return Result{Exit: 2, TimedOut: true}
		}
		if p == RunPattern("TestB/y") {
			return Result{Exit: 1, Lines: lines("=== RUN   TestB", "=== RUN   TestB/y", "--- FAIL: TestB (0.00s)", "    --- FAIL: TestB/y (0.00s)")}
		}
		return Result{Lines: passLines("TestA")}
	}
	f.mutantCmd = map[string]func(Cmd) Result{"1": slow}
	pass := func(string) Result { return Result{Lines: passLines("TestA", "TestB", "TestB/y")} }
	f.mutant = map[string]func(string) Result{"2": pass}
	log, _ := quietLog()

	// Act
	m, err := Measure(context.Background(), cfg, f.exec, log)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	kills := map[string][]string{}
	for _, c := range m.Cases {
		kills[c.ID] = c.Kills
	}
	if kills["TestA"] != nil || !reflect.DeepEqual(kills["TestB/y"], []string{"p.go:3:29:< -> <="}) {
		t.Fatalf("kills = %v, want TestB/y's alone", kills)
	}
	if len(m.Hangs) != 1 || !strings.Contains(m.Hangs[0], "2 cases were only slowed (TestA finished within 3s); 1 failed when rerun") {
		t.Fatalf("hangs = %v", m.Hangs)
	}
}

func TestMeasureRefusesALeafThatRunsCleanAloneWithNoResult(t *testing.T) {
	// Arrange: TestA's lone run exits 0 yet prints no result line.
	cfg, dir := fakeConfig(t)
	f := &fakeGo{dir: dir, reaches: map[string]bool{"TestA": true}}
	f.mutant = map[string]func(string) Result{"1": func(string) Result { return Result{Lines: lines("=== RUN   TestA")} }}
	log, _ := quietLog()

	// Act
	_, err := Measure(context.Background(), cfg, f.exec, log)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "TestA ran clean under p.go:3:29:< -> <= yet reported no result") {
		t.Fatalf("err = %v, want the silent leaf named", err)
	}
}

func TestMeasureRecordsAFailureOutsideTheMutantsReachAsAnAnomaly(t *testing.T) {
	// Arrange: TestB/x does not reach F, yet fails in TestB's group run.
	cfg, dir := fakeConfig(t)
	f := &fakeGo{dir: dir, reaches: map[string]bool{"TestB/y": true}}
	out := func(string) Result {
		return Result{Exit: 1, Lines: lines("=== RUN   TestB", "=== RUN   TestB/x", "=== RUN   TestB/y", "--- FAIL: TestB (0.00s)", "    --- FAIL: TestB/x (0.00s)", "    --- PASS: TestB/y (0.00s)")}
	}
	f.mutant = map[string]func(string) Result{"1": out, "2": out}
	log, _ := quietLog()

	// Act
	m, err := Measure(context.Background(), cfg, f.exec, log)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(m.Anomalies) != 2 || !strings.Contains(m.Anomalies[0], "TestB/x failed under p.go:3:29:< -> <=, which it does not reach") {
		t.Fatalf("anomalies = %v", m.Anomalies)
	}
	for _, c := range m.Cases {
		if len(c.Kills) != 0 {
			t.Errorf("%s kills %v, want nothing credited", c.ID, c.Kills)
		}
	}
}

func TestMeasureEndsAtAnInterruptRatherThanReadingItAsAHang(t *testing.T) {
	// Arrange: the interrupt lands while mutant 1's group run is running.
	cfg, dir := fakeConfig(t)
	f := &fakeGo{dir: dir, reaches: map[string]bool{"TestA": true}}
	ctx, cancel := context.WithCancel(context.Background())
	f.mutant = map[string]func(string) Result{"1": func(string) Result {
		cancel()
		return Result{Exit: -1, TimedOut: true}
	}}
	log, _ := quietLog()

	// Act
	_, err := Measure(ctx, cfg, f.exec, log)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "interrupted during mutants.test") {
		t.Fatalf("err = %v, want the interrupt", err)
	}
}

func TestMeasureRefusesANonPositiveHangBound(t *testing.T) {
	// Arrange
	cfg, dir := fakeConfig(t)
	cfg.HangBound = 0
	f := &fakeGo{dir: dir}
	log, _ := quietLog()

	// Act
	_, err := Measure(context.Background(), cfg, f.exec, log)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "hang bound must be positive") {
		t.Fatalf("err = %v", err)
	}
}
