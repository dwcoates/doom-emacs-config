package cli

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"strconv"
	"strings"
	"sync"
	"testing"
	"time"

	"agentrepl/testrun/internal/events"
	"agentrepl/testrun/internal/ramdisk"
	"agentrepl/testrun/internal/reuse"
	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/sched"
	"agentrepl/testrun/internal/suites"
	"agentrepl/testrun/internal/testtimes"
	"agentrepl/testrun/roster"

	"google.golang.org/protobuf/proto"

	testrunv1 "agentrepl/proto/testrun/v1"
)

func TestParseArgs(t *testing.T) {
	tests := []struct {
		name    string
		argv    []string
		want    Args
		wantErr string
	}{
		{name: "module only selects every suite", argv: []string{"--module", "/m"}, want: Args{Module: "/m"}},
		{name: "record and a suite list", argv: []string{"--module", "/m", "--record", "--record-out", "/r", "--suites", "ert,daemon"}, want: Args{Module: "/m", Record: true, RecordOut: "/r", Selected: []string{"ert", "daemon"}}},
		{name: "coverage is explicit", argv: []string{"--module", "/m", "--coverage"}, want: Args{Module: "/m", Coverage: true}},
		{name: "the = spelling", argv: []string{"--module", "/m", "--suites=e2e"}, want: Args{Module: "/m", Selected: []string{"e2e"}}},
		{name: "an unknown argument", argv: []string{"--module", "/m", "--bogus"}, wantErr: "unknown argument '--bogus', expected --record, --record-out, --coverage, --no-reuse, --suites <list>, --selection <path> or --events-fd <n>"},
		{name: "a selection is a targeted run", argv: []string{"--module", "/m", "--selection", "/s.pb"}, want: Args{Module: "/m", Selection: "/s.pb", Targeted: true}},
		{name: "an events stream", argv: []string{"--module", "/m", "--events-fd", "4"}, want: Args{Module: "/m", EventsFD: 4}},
		{name: "an events fd that is a standard stream", argv: []string{"--module", "/m", "--events-fd", "2"}, wantErr: "is not a file descriptor past stderr"},
		{name: "an events fd that is no number", argv: []string{"--module", "/m", "--events-fd", "x"}, wantErr: "is not a file descriptor past stderr"},
		{name: "--events-fd without a value", argv: []string{"--module", "/m", "--events-fd"}, wantErr: "--events-fd needs a file descriptor"},
		{name: "--selection without a value", argv: []string{"--module", "/m", "--selection"}, wantErr: "--selection needs a path"},
		{name: "a selection with a suite list", argv: []string{"--module", "/m", "--selection", "/s.pb", "--suites", "ert"}, wantErr: "--selection cannot be combined"},
		{name: "a selection with coverage", argv: []string{"--module", "/m", "--selection", "/s.pb", "--coverage"}, wantErr: "--selection cannot be combined"},
		{name: "an unknown suite", argv: []string{"--module", "/m", "--suites", "nope"}, wantErr: "--suites names an unknown suite 'nope'; known suites: "},
		{name: "an empty suite list", argv: []string{"--module", "/m", "--suites", ""}, wantErr: "--suites needs at least one suite name"},
		{name: "an empty name in the list", argv: []string{"--module", "/m", "--suites", "ert,,daemon"}, wantErr: "--suites contains an empty suite name: 'ert,,daemon'"},
		{name: "--suites without a value", argv: []string{"--module", "/m", "--suites"}, wantErr: "--suites needs a comma-separated suite list"},
		{name: "--module without a value", argv: []string{"--module"}, wantErr: "--module needs a directory"},
		{name: "no module", argv: []string{"--record"}, wantErr: "--module is required"},
		{name: "record with coverage", argv: []string{"--module", "/m", "--record", "--coverage"}, wantErr: "--record and --coverage cannot be combined"},
		{name: "record without record-out", argv: []string{"--module", "/m", "--record"}, wantErr: "--record needs --record-out"},
		{name: "record-out offered to a run that may not record", argv: []string{"--module", "/m", "--record-out", "/r"}, want: Args{Module: "/m", RecordOut: "/r"}},
		{name: "--record-out without a value", argv: []string{"--module", "/m", "--record", "--record-out"}, wantErr: "--record-out needs a path"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := ParseArgs(tt.argv)

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want it to mention %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("ParseArgs = %+v, %v; want %+v", got, err, tt.want)
			}
		})
	}
}

func TestSelects(t *testing.T) {
	// Arrange
	all, some := Args{}, Args{Selected: []string{"ert"}}

	// Act / Assert
	if !all.Selects("daemon") || !some.Selects("ert") || some.Selects("daemon") {
		t.Fatal("an empty selection must select everything and a list only its names")
	}
}

// freeCores is a host core pool that is never short: the cli's tests are
// about the run, and the pool's own waits are run.Runner's and cores.Host's.
type freeCores struct{}

func (freeCores) Hold(string, int) (func() error, error) { return func() error { return nil }, nil }

func csvFile(t *testing.T, rows ...string) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "test_time.csv")
	body := CSVHeader + "\n" + strings.Join(rows, "")
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		t.Fatal(err)
	}
	return path
}

func TestValidateCSV(t *testing.T) {
	tests := []struct {
		name    string
		body    *string
		wantErr string
	}{
		{name: "a valid header", body: ptr(CSVHeader + "\n")},
		{name: "a missing file", body: nil, wantErr: "canonical timing file is missing"},
		{name: "an empty file", body: ptr(""), wantErr: "canonical timing file is unreadable"},
		{name: "a wrong header", body: ptr("a,b\n"), wantErr: "canonical timing header is invalid: a,b"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := filepath.Join(t.TempDir(), "test_time.csv")
			if tt.body != nil {
				if err := os.WriteFile(path, []byte(*tt.body), 0o644); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			err := ValidateCSV(path)

			// Assert
			if tt.wantErr == "" && err != nil || tt.wantErr != "" && (err == nil || !strings.Contains(err.Error(), tt.wantErr)) {
				t.Fatalf("err = %v, want %q", err, tt.wantErr)
			}
		})
	}
}

func ptr(s string) *string { return &s }

func TestAppendTimings(t *testing.T) {
	// Arrange
	path := csvFile(t)
	rows := []TimingRow{{RunID: "r", RecordedAt: "t", Commit: "c", Branch: "b", Suite: "ert", Seconds: 1.5, Measure: MeasureUnitWallSum}}

	// Act
	err := AppendTimings(path, rows)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	data, _ := os.ReadFile(path)
	if want := CSVHeader + "\nr,t,c,b,ert,1.500,unit-wall-sum\n"; string(data) != want {
		t.Fatalf("csv = %q, want %q", data, want)
	}
	if _, err := os.Stat(path + ".lock"); !os.IsNotExist(err) {
		t.Fatal("the lock outlived the write")
	}
}

func TestAppendTimingsRefusals(t *testing.T) {
	tests := []struct {
		name    string
		row     TimingRow
		locked  bool
		wantErr string
	}{
		{name: "a comma in a field", row: TimingRow{RunID: "r", RecordedAt: "t", Commit: "c", Branch: "a,b", Suite: "s", Measure: MeasureUnitWallSum}, wantErr: "branch is not CSV-safe"},
		{name: "an empty field", row: TimingRow{RunID: "r", RecordedAt: "t", Commit: "", Branch: "b", Suite: "s", Measure: MeasureUnitWallSum}, wantErr: "commit is empty"},
		{name: "a concurrent writer", row: TimingRow{RunID: "r", RecordedAt: "t", Commit: "c", Branch: "b", Suite: "s", Measure: MeasureUnitWallSum}, locked: true, wantErr: "another timing writer holds"},
		{name: "no measure", row: TimingRow{RunID: "r", RecordedAt: "t", Commit: "c", Branch: "b", Suite: "s"}, wantErr: `suite s has an unknown timing measure ""`},
		{name: "an unknown measure", row: TimingRow{RunID: "r", RecordedAt: "t", Commit: "c", Branch: "b", Suite: "s", Measure: "span"}, wantErr: `suite s has an unknown timing measure "span"`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := csvFile(t)
			if tt.locked {
				if err := os.Mkdir(path+".lock", 0o755); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			err := AppendTimings(path, []TimingRow{tt.row})

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
				t.Fatalf("err = %v, want %q", err, tt.wantErr)
			}
			data, _ := os.ReadFile(path)
			if string(data) != CSVHeader+"\n" {
				t.Fatalf("a refused append changed the file: %q", data)
			}
		})
	}
}

func TestRegressions(t *testing.T) {
	tests := []struct {
		name string
		rows []string
		want []string
	}{
		{
			name: "a big regression is surfaced",
			rows: []string{"p1,t,c,main,ert,10.000,unit-wall-sum\n", "p2,t,c,main,ert,10.000,unit-wall-sum\n", "p3,t,c,main,ert,10.000,unit-wall-sum\n", "now,t,c,main,ert,20.000,unit-wall-sum\n"},
			want: []string{"TIMING REGRESSION: ert 20.000s vs 10.000s recent average unit-wall-sum (+100.0%, +10.000s)"},
		},
		{
			name: "a small slowdown is not",
			rows: []string{"p1,t,c,main,ert,10.000,unit-wall-sum\n", "p2,t,c,main,ert,10.000,unit-wall-sum\n", "p3,t,c,main,ert,10.000,unit-wall-sum\n", "now,t,c,main,ert,10.900,unit-wall-sum\n"},
			want: []string{"no big timing regressions detected"},
		},
		{
			name: "fewer than three priors is no baseline",
			rows: []string{"p1,t,c,main,ert,10.000,unit-wall-sum\n", "now,t,c,main,ert,99.000,unit-wall-sum\n"},
			want: []string{"ert: only 1 prior main unit-wall-sum timing entries, regression baseline needs 3", "no big timing regressions detected"},
		},
		{
			name: "a prior of another measure is never a baseline",
			rows: []string{
				"s1,t,c,main,ert,1.000,serial-wall\n", "s2,t,c,main,ert,1.000,serial-wall\n", "s3,t,c,main,ert,1.000,serial-wall\n",
				"now,t,c,main,ert,20.000,unit-wall-sum\n",
			},
			want: []string{
				"ert: only 0 prior main unit-wall-sum timing entries, regression baseline needs 3 (3 prior entries of another measure are not comparable)",
				"no big timing regressions detected",
			},
		},
		{
			name: "a run of the other measure compares with its own priors",
			rows: []string{
				"u1,t,c,main,ert,99.000,unit-wall-sum\n",
				"s1,t,c,main,ert,10.000,serial-wall\n", "s2,t,c,main,ert,10.000,serial-wall\n", "s3,t,c,main,ert,10.000,serial-wall\n",
				"now,t,c,main,ert,20.000,serial-wall\n",
			},
			want: []string{"TIMING REGRESSION: ert 20.000s vs 10.000s recent average serial-wall (+100.0%, +10.000s)"},
		},
		{
			name: "only the same branch counts, and only the last five",
			rows: []string{
				"o,t,c,other,ert,1.000,unit-wall-sum\n",
				"p0,t,c,main,ert,100.000,unit-wall-sum\n", "p1,t,c,main,ert,10.000,unit-wall-sum\n", "p2,t,c,main,ert,10.000,unit-wall-sum\n",
				"p3,t,c,main,ert,10.000,unit-wall-sum\n", "p4,t,c,main,ert,10.000,unit-wall-sum\n", "p5,t,c,main,ert,10.000,unit-wall-sum\n",
				"now,t,c,main,ert,20.000,unit-wall-sum\n",
			},
			want: []string{"TIMING REGRESSION: ert 20.000s vs 10.000s recent average unit-wall-sum (+100.0%, +10.000s)"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := csvFile(t, tt.rows...)

			// Act
			got, err := Regressions(path, "now", "main")

			// Assert
			if err != nil || !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("Regressions = %v, %v; want %v", got, err, tt.want)
			}
		})
	}
}

func TestRegressionsRefusals(t *testing.T) {
	tests := []struct {
		name    string
		rows    []string
		wantErr string
	}{
		{name: "a row of the wrong width", rows: []string{"only,three,fields\n"}, wantErr: "line 2 has 3 fields, want 7"},
		{name: "a legacy six-field row", rows: []string{"p,t,c,main,ert,1.000\n"}, wantErr: "line 2 has 6 fields, want 7"},
		{name: "unreadable seconds", rows: []string{"p,t,c,main,ert,soon,unit-wall-sum\n"}, wantErr: `line 2: unreadable seconds "soon"`},
		{name: "an unknown measure", rows: []string{"p,t,c,main,ert,1.000,span\n"}, wantErr: `line 2: unknown timing measure "span"`},
		{
			name:    "a run recorded under two measures",
			rows:    []string{"now,t,c,main,ert,1.000,unit-wall-sum\n", "now,t,c,main,daemon,1.000,serial-wall\n"},
			wantErr: "line 3: run now recorded both unit-wall-sum and serial-wall rows",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := csvFile(t, tt.rows...)

			// Act
			_, err := Regressions(path, "now", "main")

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
				t.Fatalf("err = %v, want %q", err, tt.wantErr)
			}
		})
	}
}

// TestCanonicalTimingFileParses holds the committed history to the schema the
// reader enforces: a row --record could never compare fails here, at commit
// time, rather than in the post-merge record run.
func TestCanonicalTimingFileParses(t *testing.T) {
	// Arrange
	path := filepath.Join("..", "..", "..", "test_time.csv")

	// Act
	validateErr := ValidateCSV(path)
	_, readErr := Regressions(path, "no-such-run", "master")

	// Assert
	if validateErr != nil || readErr != nil {
		t.Fatalf("the canonical timing file is unreadable: header %v, rows %v", validateErr, readErr)
	}
}

func TestOverCores(t *testing.T) {
	at := time.Date(2026, 10, 2, 0, 0, 0, 0, time.UTC)
	result := func(wall, cpu float64, slots int) run.Result {
		var sp run.Spec
		sp.ID, sp.Slots = "u", slots
		return run.Result{Spec: sp, Start: at, End: at.Add(time.Duration(wall * float64(time.Second))), Usage: run.Usage{CPU: cpu}}
	}
	tests := []struct {
		name     string
		res      run.Result
		wantOver bool
		wantLine string
	}{
		{name: "one core in one slot", res: result(4, 4, 0)},
		{name: "1.5 cores in one slot is the limit", res: result(4, 6, 0)},
		{
			name: "past 1.5 cores in one slot", res: result(4, 7, 0), wantOver: true,
			wantLine: "unit u used 1.8 cores on average (7.0s cpu in 4.0s) but holds 1 core slot(s): it is not pinned to its width",
		},
		{name: "two cores in two slots", res: result(4, 8, 2)},
		{
			name: "past 2.5 cores in two slots", res: result(4, 11, 2), wantOver: true,
			wantLine: "unit u used 2.8 cores on average (11.0s cpu in 4.0s) but holds 2 core slot(s): it is not pinned to its width",
		},
		{name: "a unit of a second or less is too short to judge", res: result(1, 5, 0)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			line, over := overCores(tt.res)

			// Assert
			if over != tt.wantOver || line != tt.wantLine {
				t.Fatalf("overCores = %q, %v; want %q, %v", line, over, tt.wantLine, tt.wantOver)
			}
		})
	}
}

func TestRunID(t *testing.T) {
	// Act
	got := RunID(time.Date(2026, 10, 1, 12, 0, 0, 0, time.UTC), "0123456789abcdef", 42)

	// Assert
	if got != "20261001T120000Z-0123456789ab-42" {
		t.Fatalf("RunID = %s", got)
	}
}

// ---- The whole run, through fakes -------------------------------------------

type fakeExec struct {
	mu    sync.Mutex
	exits map[string]int
	ran   []string
}

type fakeProc struct{ exit int }

func (p fakeProc) Wait() (int, run.Usage, error) { return p.exit, run.Usage{CPU: 0.5}, nil }
func (p fakeProc) Kill()                         {}

func (e *fakeExec) Start(s run.Spec, out *bytes.Buffer) (run.Process, error) {
	e.mu.Lock()
	defer e.mu.Unlock()
	e.ran = append(e.ran, s.ID)
	out.WriteString("output of " + s.ID + "\n")
	return fakeProc{exit: e.exits[s.ID]}, nil
}

type tick struct {
	mu sync.Mutex
	t  time.Time
}

func (c *tick) Now() time.Time {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.t = c.t.Add(time.Second)
	return c.t
}

// fakeReuser reuses the suites named in reused, reports unplaced as the
// placement check's finding, and remembers what it was asked to record.
type fakeReuser struct {
	reused   map[string]bool
	unplaced []string
	mode     *reuse.Mode
	recorded *[]string
}

func (f fakeReuser) Decide(selected []roster.Suite, mode reuse.Mode) ([]reuse.Verdict, error) {
	*f.mode = mode
	var out []reuse.Verdict
	for _, s := range selected {
		v := reuse.Verdict{Suite: s, Why: "always runs"}
		if f.reused[s.Name] {
			v.Keyed = true
			v.Reused = &reuse.Record{Suite: s.Name, Branch: "master", Commit: "0123456789abcdef"}
		}
		out = append(out, v)
	}
	return out, nil
}

func (f fakeReuser) Unplaced([]reuse.Verdict) ([]string, error) { return f.unplaced, nil }

func (f fakeReuser) Confine(_ []reuse.Verdict, _ string, u suites.Units) suites.Units { return u }

func (f fakeReuser) Record(mode reuse.Mode, _ []reuse.Verdict, results []run.SuiteResult, _ map[string]int) error {
	for _, r := range results {
		if r.Outcome == run.Passed {
			*f.recorded = append(*f.recorded, r.Name)
		}
	}
	return nil
}

type harness struct {
	deps      Deps
	reuser    *fakeReuser
	exec      *fakeExec
	out       *bytes.Buffer
	errOut    *bytes.Buffer
	module    string
	recordOut string
	git       *[]string
	// prepared holds, per PrepareWebappDist call, its module root and how
	// many units had started before it.
	prepared *[]webappDistCall
}

type webappDistCall struct {
	module      string
	unitsBefore int
}

// newHarness is a module root with a valid test_time.csv, a history file, and
// a Build that turns every suite into one unit named after it.
func newHarness(t *testing.T, exits map[string]int, buildErr map[string]error) harness {
	t.Helper()
	root := t.TempDir()
	module := filepath.Join(root, "modules", "app", "agent-repl")
	if err := os.MkdirAll(module, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(module, "test_time.csv"), []byte(CSVHeader+"\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	out, errOut := &bytes.Buffer{}, &bytes.Buffer{}
	e := &fakeExec{exits: exits}
	gitCalls := &[]string{}
	d := Deps{
		Log:         &run.Log{Out: out, Err: errOut},
		Exec:        e,
		Clock:       &tick{},
		Slots:       4,
		Cores:       freeCores{},
		HistoryPath: filepath.Join(root, "history.json"),
		Build: func(_ suites.Layout, s roster.Suite) (suites.Units, error) {
			if err := buildErr[s.Name]; err != nil {
				return suites.Units{}, err
			}
			sp := run.Spec{Argv: []string{s.Name}, MayDecline: s.MayDecline}
			sp.ID, sp.Suite = s.Name, s.Name
			return suites.Units{Atomic: []run.Spec{sp}}, nil
		},
		Git: func(string) (string, string, error) {
			*gitCalls = append(*gitCalls, "git")
			return "main", "abc123", nil
		},
		Pid: 7,
	}
	prepared := &[]webappDistCall{}
	d.PrepareWebappDist = func(m string) error {
		e.mu.Lock()
		defer e.mu.Unlock()
		*prepared = append(*prepared, webappDistCall{module: m, unitsBefore: len(e.ran)})
		return nil
	}
	fr := &fakeReuser{mode: new(reuse.Mode), recorded: &[]string{}}
	d.Reuse = fr
	return harness{deps: d, reuser: fr, exec: e, out: out, errOut: errOut, module: module, recordOut: filepath.Join(root, "pending-record.json"), git: gitCalls, prepared: prepared}
}

func (h harness) run(t *testing.T, argv ...string) int {
	t.Helper()
	a, err := ParseArgs(append([]string{"--module", h.module}, argv...))
	if err != nil {
		t.Fatal(err)
	}
	return Run(context.Background(), h.deps, a)
}

func TestRunEverySuitePassing(t *testing.T) {
	// Arrange
	h := newHarness(t, nil, nil)

	// Act
	code := h.run(t)

	// Assert
	if code != 0 {
		t.Fatalf("exit = %d, stderr:\n%s", code, h.errOut)
	}
	if len(h.exec.ran) != len(roster.Suites) {
		t.Fatalf("ran %d units, want one per roster suite", len(h.exec.ran))
	}
	for _, want := range []string{
		"[agent-repl-tests] ert: passed in ",
		"timings were not recorded, pass --record only for a canonical history run",
		"[agent-repl-tests] all agent-repl test suites passed",
		"[agent-repl-tests] distribution: ",
		"timing summary, most unit time first (unit-wall-sum is what --record keeps; the span depends on the interleaving)",
		"[agent-repl-tests] timing: ert ",
		"s of units over a ",
	} {
		if !strings.Contains(h.out.String(), want) {
			t.Errorf("stdout lacks %q", want)
		}
	}
	if h.errOut.Len() != 0 {
		t.Errorf("a clean run wrote to stderr:\n%s", h.errOut)
	}
	data, _ := os.ReadFile(filepath.Join(h.module, "test_time.csv"))
	if string(data) != CSVHeader+"\n" {
		t.Errorf("a run without --record changed test_time.csv")
	}
	if _, err := os.Stat(h.deps.HistoryPath); err != nil {
		t.Errorf("the host history was not written: %v", err)
	}
}

func TestRunWithAFailureContinuesAndExitsNonZero(t *testing.T) {
	// Arrange
	h := newHarness(t, map[string]int{"daemon": 3}, nil)

	// Act
	code := h.run(t, "--record", "--record-out", h.recordOut)

	// Assert
	if code != 1 {
		t.Fatalf("exit = %d, want 1", code)
	}
	if len(h.exec.ran) != len(roster.Suites) {
		t.Fatal("a failure stopped the run before every suite ran")
	}
	for _, want := range []string{
		"daemon failed after ",
		fmt.Sprintf("failure summary, 1 of %d suites failed", len(roster.Suites)),
		"failed: daemon exit code 3 after ",
	} {
		if !strings.Contains(h.errOut.String(), want) {
			t.Errorf("stderr lacks %q:\n%s", want, h.errOut)
		}
	}
	data, _ := os.ReadFile(filepath.Join(h.module, "test_time.csv"))
	if string(data) != CSVHeader+"\n" {
		t.Errorf("a failed run recorded timings")
	}
	if _, err := os.Stat(h.recordOut); err == nil {
		t.Errorf("a failed run staged a pending record")
	}
}

func TestRunWithADeclinedSuite(t *testing.T) {
	// Arrange
	h := newHarness(t, map[string]int{"e2e-emacs": run.ExitDeclined}, nil)

	// Act
	code := h.run(t)

	// Assert
	if code != 0 {
		t.Fatalf("exit = %d, want 0: a decline is not a failure", code)
	}
	for _, want := range []string{
		"e2e-emacs: DECLINED after ",
		"declined: e2e-emacs did not run (precondition unmet)",
		"every agent-repl suite that could run passed; DECLINED: e2e-emacs",
	} {
		if !strings.Contains(h.out.String(), want) {
			t.Errorf("stdout lacks %q", want)
		}
	}
	if strings.Contains(h.out.String(), "e2e-emacs: passed") {
		t.Error("a declined suite was called passed")
	}
}

func TestRunWithSuitesRunsOnlyThoseAndNamesTheRest(t *testing.T) {
	// Arrange
	h := newHarness(t, nil, nil)

	// Act
	code := h.run(t, "--suites", "ert,daemon")

	// Assert
	if code != 0 {
		t.Fatalf("exit = %d", code)
	}
	if got := len(h.exec.ran); got != 2 {
		t.Fatalf("ran %v, want only ert and daemon", h.exec.ran)
	}
	for _, want := range []string{
		"webapp: not selected by --suites, skipping",
		"selected agent-repl suites passed: ert daemon",
		"not selected, NOT run: orchestrator-harness",
	} {
		if !strings.Contains(h.out.String(), want) {
			t.Errorf("stdout lacks %q", want)
		}
	}
}

func TestRunWithTheStoreAndShimSuitesRunsExactlyTheNamedFour(t *testing.T) {
	// Arrange: the selection a hook-records change ran on 2026-10-06, whose
	// report showed proto, webapp and ert instead of shim and store. That
	// report was another agent's run writing the same scratch log; this pins
	// that the runner itself runs exactly what was named.
	h := newHarness(t, nil, nil)

	// Act
	code := h.run(t, "--suites", "shim,store,daemon,e2e")

	// Assert
	if code != 0 {
		t.Fatalf("exit = %d", code)
	}
	if got := len(h.exec.ran); got != 4 {
		t.Fatalf("ran %v, want only shim, store, daemon and e2e", h.exec.ran)
	}
	if !strings.Contains(h.out.String(), "selected agent-repl suites passed: shim store daemon e2e") {
		t.Fatalf("stdout lacks the four named suites as the selection:\n%s", h.out.String())
	}
}

func TestRunRecordStagesEveryPassingSuiteWithoutTouchingTheCSV(t *testing.T) {
	// Arrange
	h := newHarness(t, nil, nil)
	// One slot runs the units one after another, so the tick clock gives
	// each exactly 1s.
	h.deps.Slots = 1

	// Act
	code := h.run(t, "--record", "--record-out", h.recordOut, "--suites", "ert,daemon")

	// Assert
	if code != 0 {
		t.Fatalf("exit = %d, stderr:\n%s", code, h.errOut)
	}
	// `run --record` never writes test_time.csv: see run.go's "testrun run
	// only STAGES --record". The write is `finish-record`'s alone, so that
	// it can happen OUTSIDE the git-state net the suite run itself runs
	// inside, instead of looking like drift the run caused.
	data, _ := os.ReadFile(filepath.Join(h.module, "test_time.csv"))
	if string(data) != CSVHeader+"\n" {
		t.Fatalf("run --record touched test_time.csv: %q", data)
	}
	p, err := ReadPendingRecord(h.recordOut)
	if err != nil {
		t.Fatalf("ReadPendingRecord: %v", err)
	}
	if p.Branch != "main" || p.Commit != "abc123" || p.RunID == "" {
		t.Fatalf("staged record identity = %+v", p)
	}
	// Roster order: ert is listed before daemon.
	if len(p.Rows) != 2 ||
		p.Rows[0].Suite != "ert" || p.Rows[0].Seconds != 1.0 || p.Rows[0].Measure != "unit-wall-sum" ||
		p.Rows[1].Suite != "daemon" || p.Rows[1].Seconds != 1.0 {
		t.Fatalf("staged rows = %+v", p.Rows)
	}
	if len(*h.git) != 2 {
		t.Fatalf("git was asked %d times, want before and after", len(*h.git))
	}
	if !strings.Contains(h.out.String(), "staged 2 suite timings") {
		t.Errorf("stdout lacks the staging line")
	}
}

func TestRunRecordRefusesAMovedHeadAndStagesNothing(t *testing.T) {
	// Arrange
	h := newHarness(t, nil, nil)
	n := 0
	h.deps.Git = func(string) (string, string, error) {
		n++
		return "main", map[int]string{1: "old", 2: "new"}[n], nil
	}

	// Act
	code := h.run(t, "--record", "--record-out", h.recordOut, "--suites", "ert")

	// Assert
	if code != 1 || !strings.Contains(h.errOut.String(), "git commit changed during tests: old -> new") {
		t.Fatalf("exit = %d, stderr:\n%s", code, h.errOut)
	}
	if _, err := os.Stat(h.recordOut); err == nil {
		t.Errorf("a moved commit still staged a pending record")
	}
}

func TestFinishRecordAppendsStagedRowsAndReportsRegressions(t *testing.T) {
	// Arrange: run --record stages, then finish-record commits.
	h := newHarness(t, nil, nil)
	h.deps.Slots = 1
	if code := h.run(t, "--record", "--record-out", h.recordOut, "--suites", "ert,daemon"); code != 0 {
		t.Fatalf("staging run exit = %d, stderr:\n%s", code, h.errOut)
	}

	// Act
	code := FinishRecord(h.deps, h.module, h.recordOut)

	// Assert
	if code != 0 {
		t.Fatalf("exit = %d, stderr:\n%s", code, h.errOut)
	}
	data, _ := os.ReadFile(filepath.Join(h.module, "test_time.csv"))
	lines := strings.Split(strings.TrimSpace(string(data)), "\n")
	if len(lines) != 3 || !strings.HasSuffix(lines[1], ",abc123,main,ert,1.000,unit-wall-sum") || !strings.HasSuffix(lines[2], ",abc123,main,daemon,1.000,unit-wall-sum") {
		t.Fatalf("csv = %q", data)
	}
	if !strings.Contains(h.out.String(), "recorded 2 suite timings") {
		t.Errorf("stdout lacks the record line")
	}
	if !strings.Contains(h.out.String(), "no big timing regressions detected") {
		t.Errorf("stdout lacks the regression report")
	}
}

func TestFinishRecordRefusesAMovedCommitAndRecordsNothing(t *testing.T) {
	// Arrange: stage under one commit, then move it before finish-record runs.
	h := newHarness(t, nil, nil)
	h.deps.Slots = 1
	if code := h.run(t, "--record", "--record-out", h.recordOut, "--suites", "ert"); code != 0 {
		t.Fatalf("staging run exit = %d, stderr:\n%s", code, h.errOut)
	}
	h.deps.Git = func(string) (string, string, error) { return "main", "def456", nil }

	// Act
	code := FinishRecord(h.deps, h.module, h.recordOut)

	// Assert
	if code != 1 || !strings.Contains(h.errOut.String(), "git branch/commit moved since the staged run finished: main@abc123 -> main@def456; recording nothing") {
		t.Fatalf("exit = %d, stderr:\n%s", code, h.errOut)
	}
	data, _ := os.ReadFile(filepath.Join(h.module, "test_time.csv"))
	if string(data) != CSVHeader+"\n" {
		t.Errorf("a moved-commit finish-record recorded timings")
	}
}

func TestFinishRecordRefusesAMissingPendingRecord(t *testing.T) {
	// Arrange
	h := newHarness(t, nil, nil)

	// Act
	code := FinishRecord(h.deps, h.module, h.recordOut)

	// Assert
	if code != 1 || !strings.Contains(h.errOut.String(), "read the staged record") {
		t.Fatalf("exit = %d, stderr:\n%s", code, h.errOut)
	}
}

func TestRunFailsBeforeRunningAnything(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		argv    []string
		wantErr string
	}{
		{
			name:    "a malformed canonical history",
			arrange: func(h *harness) { os.WriteFile(filepath.Join(h.module, "test_time.csv"), []byte("bad\n"), 0o644) },
			wantErr: "canonical timing header is invalid: bad",
		},
		{
			name: "no git metadata for --record",
			arrange: func(h *harness) {
				h.deps.Git = func(string) (string, string, error) { return "", "", errors.New("not a repository") }
			},
			argv:    []string{"--record", "--record-out", "/does-not-matter"},
			wantErr: "could not resolve the initial git branch and commit: not a repository",
		},
		{
			name:    "an unreadable host history",
			arrange: func(h *harness) { os.WriteFile(h.deps.HistoryPath, []byte("{"), 0o644) },
			wantErr: "is not a test history",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, nil, nil)
			tt.arrange(&h)

			// Act
			code := h.run(t, tt.argv...)

			// Assert
			if code != 1 || !strings.Contains(h.errOut.String(), tt.wantErr) {
				t.Fatalf("exit = %d, stderr:\n%s", code, h.errOut)
			}
			if len(h.exec.ran) != 0 {
				t.Fatalf("units ran before the refusal: %v", h.exec.ran)
			}
		})
	}
}

func TestRunBuildsTheWebappDistOnceBeforeAnyUnitWhenASuiteNeedsIt(t *testing.T) {
	tests := []struct {
		name   string
		reused map[string]bool
		argv   []string
		want   int
	}{
		{name: "every suite, e2e-emacs among them", want: 1},
		{name: "e2e-emacs alone", argv: []string{"--suites", "e2e-emacs"}, want: 1},
		{name: "no suite that reads the dist", argv: []string{"--suites", "ert,daemon"}, want: 0},
		{name: "e2e-emacs covered by an earlier pass", reused: map[string]bool{"e2e-emacs": true}, argv: []string{"--suites", "e2e-emacs,ert"}, want: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, nil, nil)
			h.reuser.reused = tt.reused

			// Act
			code := h.run(t, tt.argv...)

			// Assert
			if code != 0 {
				t.Fatalf("exit = %d, stderr:\n%s", code, h.errOut)
			}
			if len(*h.prepared) != tt.want {
				t.Fatalf("PrepareWebappDist calls = %v, want %d", *h.prepared, tt.want)
			}
			for _, c := range *h.prepared {
				if c.module != h.module || c.unitsBefore != 0 {
					t.Fatalf("PrepareWebappDist call = %+v, want module %s with no unit started before it", c, h.module)
				}
			}
		})
	}
}

func TestRunWhoseWebappDistBuildFailsRunsNothing(t *testing.T) {
	// Arrange
	h := newHarness(t, nil, nil)
	h.deps.PrepareWebappDist = func(string) error { return errors.New("vite build exited 1") }

	// Act
	code := h.run(t)

	// Assert
	want := "webapp dist: building it before the run failed, so no suite that reads it can run: vite build exited 1"
	if code != 1 || !strings.Contains(h.errOut.String(), want) {
		t.Fatalf("exit = %d, stderr:\n%s", code, h.errOut)
	}
	if len(h.exec.ran) != 0 {
		t.Fatalf("units ran after the failed dist build: %v", h.exec.ran)
	}
}

func TestRunASuiteThatCannotBePlannedFailsAloneAndTheRestRun(t *testing.T) {
	// Arrange
	h := newHarness(t, nil, map[string]error{"webapp": errors.New("vitest list exploded")})

	// Act
	code := h.run(t)

	// Assert
	if code != 1 {
		t.Fatalf("exit = %d, want 1", code)
	}
	for _, want := range []string{
		"webapp could not be planned: vitest list exploded",
		"webapp failed after 0.000s with exit code 1",
		"failed: webapp exit code 1",
	} {
		if !strings.Contains(h.errOut.String(), want) {
			t.Errorf("stderr lacks %q:\n%s", want, h.errOut)
		}
	}
	if len(h.exec.ran) != len(roster.Suites)-1 {
		t.Fatalf("ran %d units, want every other suite", len(h.exec.ran))
	}
}

func TestRunInterruptedKillsAndExits130(t *testing.T) {
	// Arrange
	h := newHarness(t, nil, nil)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	a, err := ParseArgs([]string{"--module", h.module, "--suites", "ert"})
	if err != nil {
		t.Fatal(err)
	}

	// Act
	code := Run(ctx, h.deps, a)

	// Assert
	if code != 130 || !strings.Contains(h.errOut.String(), "the run was interrupted") {
		t.Fatalf("exit = %d, stderr:\n%s", code, h.errOut)
	}
}

func TestParseFinishRecordArgs(t *testing.T) {
	tests := []struct {
		name    string
		argv    []string
		want    FinishRecordArgs
		wantErr string
	}{
		{name: "both flags", argv: []string{"--module", "/m", "--record-out", "/r"}, want: FinishRecordArgs{Module: "/m", RecordOut: "/r"}},
		{name: "missing module", argv: []string{"--record-out", "/r"}, wantErr: "--module is required"},
		{name: "missing record-out", argv: []string{"--module", "/m"}, wantErr: "--record-out is required"},
		{name: "--module without a value", argv: []string{"--module"}, wantErr: "--module needs a directory"},
		{name: "--record-out without a value", argv: []string{"--module", "/m", "--record-out"}, wantErr: "--record-out needs a path"},
		{name: "an unknown argument", argv: []string{"--bogus"}, wantErr: "unknown argument '--bogus', expected --module or --record-out"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := ParseFinishRecordArgs(tt.argv)

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want it to mention %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || got != tt.want {
				t.Fatalf("ParseFinishRecordArgs = %+v, %v; want %+v", got, err, tt.want)
			}
		})
	}
}

func TestParseCoverArgs(t *testing.T) {
	tests := []struct {
		name    string
		argv    []string
		want    CoverArgs
		wantErr string
	}{
		{name: "every flag", argv: []string{"-name", "daemon", "-module", "/m", "-covdirs", "/c"}, want: CoverArgs{Name: "daemon", Module: "/m", CovDirs: "/c"}},
		{name: "a missing flag", argv: []string{"-name", "daemon", "-module", "/m"}, wantErr: "needs -name, -module and -covdirs"},
		{name: "an unknown flag", argv: []string{"-bogus"}, wantErr: "flag provided but not defined: -bogus"},
		{name: "a stray argument", argv: []string{"-name", "d", "-module", "/m", "-covdirs", "/c", "extra"}, wantErr: "unexpected arguments [extra]"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := ParseCoverArgs(tt.argv)

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want it to mention %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || got != tt.want {
				t.Fatalf("ParseCoverArgs = %+v, %v; want %+v", got, err, tt.want)
			}
		})
	}
}

func TestParseWardenArgs(t *testing.T) {
	tests := []struct {
		name    string
		argv    []string
		wantErr string
	}{
		{name: "every one given", argv: []string{"--runner", "77", "--since", "1760400000", "--sentinel", "76"}},
		{name: "no runner", argv: []string{"--since", "1760400000", "--sentinel", "76"}, wantErr: "warden needs --runner, --since and --sentinel"},
		{name: "no since", argv: []string{"--runner", "77", "--sentinel", "76"}, wantErr: "warden needs --runner, --since and --sentinel"},
		{name: "no sentinel", argv: []string{"--runner", "77", "--since", "1760400000"}, wantErr: "warden needs --runner, --since and --sentinel"},
		{name: "an unknown flag", argv: []string{"--runner", "77", "--x"}, wantErr: "warden: flag provided but not defined: -x"},
		{name: "a stray argument", argv: []string{"--runner", "77", "--since", "1", "--sentinel", "76", "x"}, wantErr: "warden: unexpected arguments [x]"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got, err := ParseWardenArgs(tt.argv)

			// Assert.
			if tt.wantErr != "" {
				if err == nil || err.Error() != tt.wantErr {
					t.Fatalf("ParseWardenArgs() error = %v, want %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || got.Runner != 77 || got.Sentinel != 76 || got.Since.Unix() != 1760400000 {
				t.Fatalf("ParseWardenArgs() = %+v, %v", got, err)
			}
		})
	}
}

func TestRunConditions(t *testing.T) {
	cases := []struct {
		name   string
		prefix []string
		want   string
	}{
		{name: "a run on the RAM disk names its units' I/O tier", prefix: ramdisk.UnitPrefix(), want: "ramdisk-io-" + ramdisk.UnitIOTier},
		{name: "a run on the disk names the tier it inherited", prefix: nil, want: "disk-io-inherited"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := RunConditions(tc.prefix)

			// Assert
			if got != tc.want {
				t.Fatalf("RunConditions = %q, want %q", got, tc.want)
			}
		})
	}
}

// pushed is one PushTimes call.
type pushed struct {
	target testtimes.Target
	report testtimes.Report
}

// withTestTimes makes every suite's one unit report a test named after it,
// and records every push.
func withTestTimes(h *harness, target testtimes.Target, err error) *[]pushed {
	calls := &[]pushed{}
	build := h.deps.Build
	h.deps.Build = func(l suites.Layout, s roster.Suite) (suites.Units, error) {
		u, berr := build(l, s)
		for i := range u.Atomic {
			name := s.Name
			u.Atomic[i].Tests = func([]byte, bool) ([]testtimes.Test, error) {
				return []testtimes.Test{{Suite: name, Name: name + ".TestA", Outcome: testtimes.Pass, Seconds: 1}}, nil
			}
		}
		return u, berr
	}
	h.deps.Metrics = target
	h.deps.PushTimes = func(_ context.Context, target testtimes.Target, report testtimes.Report, _ time.Time) error {
		*calls = append(*calls, pushed{target: target, report: report})
		return err
	}
	return calls
}

func TestARunToldNoMetricsTargetPushesNothing(t *testing.T) {
	// Arrange
	h := newHarness(t, nil, nil)
	calls := withTestTimes(&h, testtimes.Target{}, nil)

	// Act
	code := h.run(t)

	// Assert
	if code != 0 || len(*calls) != 0 {
		t.Fatalf("exit = %d, pushes = %d; want a clean run that pushed nothing", code, len(*calls))
	}
}

func TestARunToldAMetricsTargetPushesEveryTestAndSuite(t *testing.T) {
	// Arrange
	h := newHarness(t, map[string]int{"ert": 1}, nil)
	target := testtimes.Target{Root: "/m", Trigger: "merge_queue"}
	calls := withTestTimes(&h, target, nil)

	// Act
	h.run(t)

	// Assert
	if len(*calls) != 1 || (*calls)[0].target != target {
		t.Fatalf("pushes = %+v, want one to %+v", *calls, target)
	}
	got := (*calls)[0].report
	if len(got.Tests) != len(roster.Suites) || len(got.Suites) != len(roster.Suites) || len(got.Units) != len(roster.Suites) {
		t.Fatalf("pushed %d tests, %d suites and %d units, want one of each per roster suite", len(got.Tests), len(got.Suites), len(got.Units))
	}
	for _, u := range got.Units {
		if u.CPU != 0.5 || u.Wall <= 0 {
			t.Errorf("unit %s pushed cpu %v wall %v, want the unit's own figures", u.ID, u.CPU, u.Wall)
		}
	}
	for _, s := range got.Suites {
		want := testtimes.Pass
		if s.Name == "ert" {
			want = testtimes.Fail
		}
		if s.Outcome != want {
			t.Errorf("suite %s pushed %s, want %s", s.Name, s.Outcome, want)
		}
	}
	if !strings.Contains(h.out.String(), "metrics: pushed "+strconv.Itoa(len(roster.Suites))+" test, ") {
		t.Errorf("stdout does not say what was pushed:\n%s", h.out)
	}
}

func TestARunWhosePushFailsSaysSoAndFails(t *testing.T) {
	// Arrange
	h := newHarness(t, nil, nil)
	withTestTimes(&h, testtimes.Target{Root: "/m", Trigger: "merge_queue"}, errors.New("the store is down"))

	// Act
	code := h.run(t)

	// Assert
	if code != 1 || !strings.Contains(h.errOut.String(), "recording this run's test times in the metrics store failed: the store is down") {
		t.Fatalf("exit = %d, stderr:\n%s", code, h.errOut)
	}
}

func TestAPushLeavesOutACancelledUnit(t *testing.T) {
	// Arrange
	at := time.Unix(100, 0)
	results := []run.Result{
		{Spec: run.Spec{Unit: sched.Unit{ID: "a", Suite: "s"}}, Outcome: run.Failed, Start: at, End: at.Add(time.Second)},
		{Spec: run.Spec{Unit: sched.Unit{ID: "b", Suite: "s"}}, Outcome: run.Cancelled, Start: at, End: at},
	}
	var got testtimes.Report
	d := Deps{
		Log:     &run.Log{Out: &bytes.Buffer{}, Err: &bytes.Buffer{}},
		Metrics: testtimes.Target{Root: "/m", Trigger: "merge_queue"},
		PushTimes: func(_ context.Context, _ testtimes.Target, report testtimes.Report, _ time.Time) error {
			got = report
			return nil
		},
	}

	// Act
	err := pushTimes(context.Background(), d, results, nil, at)

	// Assert
	if err != nil || len(got.Units) != 1 || got.Units[0].ID != "a" || got.Units[0].Outcome != testtimes.Fail || got.Units[0].Wall != 1 {
		t.Fatalf("pushTimes = %v, units %+v; want only the failed unit, with its wall time", err, got.Units)
	}
}

// writeSelection writes req where a --selection run reads it.
func writeSelection(t *testing.T, req *testrunv1.TestRunRequest) string {
	t.Helper()
	data, err := proto.Marshal(req)
	if err != nil {
		t.Fatal(err)
	}
	path := filepath.Join(t.TempDir(), "selection.pb")
	if err := os.WriteFile(path, data, 0o644); err != nil {
		t.Fatal(err)
	}
	return path
}

func harnessSelection(groups ...string) *testrunv1.TestRunRequest {
	h := &testrunv1.TestSelectionHarness{Script: "modules/app/agent-repl/bin/test-test-all.sh",
		Groups: &testrunv1.TestSelectionHarness_All{All: &testrunv1.TestSelectionHarnessAllGroups{}}}
	if len(groups) > 0 {
		h.Groups = &testrunv1.TestSelectionHarness_Named{Named: &testrunv1.TestSelectionHarnessNamedGroups{Names: groups}}
	}
	return &testrunv1.TestRunRequest{Selections: []*testrunv1.TestSelection{{Target: &testrunv1.TestSelection_Harness{Harness: h}}}}
}

// eventsOf reads every event a run wrote.
func eventsOf(t *testing.T, buf *bytes.Buffer) []*testrunv1.TestRunEvent {
	t.Helper()
	var out []*testrunv1.TestRunEvent
	for buf.Len() > 0 {
		ev, err := events.Read(buf)
		if err != nil {
			t.Fatal(err)
		}
		out = append(out, ev)
	}
	return out
}

func TestRunASelectionRunsOnlyWhatItNames(t *testing.T) {
	// Arrange
	h := newHarness(t, nil, nil)
	var stream bytes.Buffer
	h.deps.Events = events.New(&stream, h.deps.Log)
	path := writeSelection(t, harnessSelection())

	// Act
	code := h.run(t, "--selection", path)

	// Assert
	if code != 0 {
		t.Fatalf("exit = %d, stderr:\n%s", code, h.errOut)
	}
	if len(h.exec.ran) != 1 || h.exec.ran[0] != "orchestrator-harness" {
		t.Fatalf("ran %v, want only the selected harness", h.exec.ran)
	}
	evs := eventsOf(t, &stream)
	if len(evs) != 3 || evs[0].GetPlanned() == nil || evs[1].GetUnitEnded().GetPassed() == nil || evs[2].GetEnded().GetPassed() == nil {
		t.Fatalf("events = %v, want planned, a passing unit_ended, ended passed", evs)
	}
	if *h.reuser.mode != reuse.Targeted {
		t.Fatalf("reuse mode = %v, want Targeted: a run of some tests records no pass", *h.reuser.mode)
	}
}

func TestRunAFailingSelectionEndsFailed(t *testing.T) {
	// Arrange
	h := newHarness(t, map[string]int{"orchestrator-harness": 1}, nil)
	var stream bytes.Buffer
	h.deps.Events = events.New(&stream, h.deps.Log)

	// Act
	code := h.run(t, "--selection", writeSelection(t, harnessSelection()))

	// Assert
	evs := eventsOf(t, &stream)
	if code != 1 || evs[len(evs)-1].GetEnded().GetFailed().GetFailedUnits() != 1 {
		t.Fatalf("exit = %d, events = %v; want 1 and ended failed", code, evs)
	}
}

func TestRunRefusesASelectionItCannotRun(t *testing.T) {
	cases := []struct {
		name string
		req  *testrunv1.TestRunRequest
		want string
	}{
		{name: "a target no suite runs", req: &testrunv1.TestRunRequest{Selections: []*testrunv1.TestSelection{{Target: &testrunv1.TestSelection_Harness{Harness: &testrunv1.TestSelectionHarness{
			Script: "nowhere.sh", Groups: &testrunv1.TestSelectionHarness_All{All: &testrunv1.TestSelectionHarnessAllGroups{}}}}}}}, want: "no suite in the roster runs it"},
		{name: "a name its target does not hold", req: harnessSelection("g"), want: "has no groups to name"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, nil, nil)
			var stream bytes.Buffer
			h.deps.Events = events.New(&stream, h.deps.Log)

			// Act
			code := h.run(t, "--selection", writeSelection(t, tc.req))

			// Assert
			evs := eventsOf(t, &stream)
			if code != 1 || len(h.exec.ran) != 0 || len(evs) != 1 || !strings.Contains(evs[0].GetRefused().GetDetail(), tc.want) {
				t.Fatalf("exit = %d, ran %v, events = %v; want 1, nothing run, one refusal mentioning %q", code, h.exec.ran, evs, tc.want)
			}
		})
	}
}

func TestRunRefusesASelectionFileItCannotRead(t *testing.T) {
	// Arrange
	h := newHarness(t, nil, nil)
	var stream bytes.Buffer
	h.deps.Events = events.New(&stream, h.deps.Log)

	// Act
	code := h.run(t, "--selection", filepath.Join(t.TempDir(), "missing.pb"))

	// Assert
	evs := eventsOf(t, &stream)
	if code != 1 || len(evs) != 1 || !strings.Contains(evs[0].GetRefused().GetDetail(), "read the selection") {
		t.Fatalf("exit = %d, events = %v; want 1 and a refusal naming the read", code, evs)
	}
}

func suiteSel(name string) *testrunv1.TestSelection {
	return &testrunv1.TestSelection{Target: &testrunv1.TestSelection_Suite{Suite: &testrunv1.TestSelectionSuite{Name: name}}}
}

func everySuiteSel() *testrunv1.TestSelection {
	return &testrunv1.TestSelection{Target: &testrunv1.TestSelection_EverySuite{EverySuite: &testrunv1.TestSelectionEverySuite{}}}
}

func TestWithRequestLowersWholeSuites(t *testing.T) {
	targeted := Args{Module: "/m", Selection: "/s.pb", Targeted: true, RecordOut: "/r"}
	cases := []struct {
		name string
		req  *testrunv1.TestRunRequest
		want Args
	}{
		{name: "named suites run as a suite list", req: &testrunv1.TestRunRequest{Selections: []*testrunv1.TestSelection{suiteSel("ert"), suiteSel("daemon")}},
			want: Args{Module: "/m", RecordOut: "/r", Selected: []string{"ert", "daemon"}}},
		{name: "every suite with coverage is a full coverage run", req: &testrunv1.TestRunRequest{Selections: []*testrunv1.TestSelection{everySuiteSel()}, Coverage: true},
			want: Args{Module: "/m", RecordOut: "/r", Coverage: true}},
		{name: "every suite recording is a record run", req: &testrunv1.TestRunRequest{Selections: []*testrunv1.TestSelection{everySuiteSel()}, Record: true},
			want: Args{Module: "/m", RecordOut: "/r", Record: true}},
		{name: "named suites with coverage", req: &testrunv1.TestRunRequest{Selections: []*testrunv1.TestSelection{suiteSel("webapp")}, Coverage: true},
			want: Args{Module: "/m", RecordOut: "/r", Selected: []string{"webapp"}, Coverage: true}},
		{name: "a narrowed request stays targeted", req: harnessSelection(), want: targeted},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := targeted.WithRequest(tc.req)

			// Assert
			if err != nil || !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("WithRequest = %+v, %v; want %+v", got, err, tc.want)
			}
		})
	}
}

func TestWithRequestRefuses(t *testing.T) {
	cases := []struct {
		name      string
		recordOut string
		req       *testrunv1.TestRunRequest
		want      string
	}{
		{name: "coverage of tests within a suite", recordOut: "/r", req: func() *testrunv1.TestRunRequest { r := harnessSelection(); r.Coverage = true; return r }(), want: "coverage measures whole suites"},
		{name: "a record of named suites", recordOut: "/r", req: &testrunv1.TestRunRequest{Selections: []*testrunv1.TestSelection{suiteSel("ert")}, Record: true}, want: "the request's one selection must be every_suite"},
		{name: "a record of tests", recordOut: "/r", req: func() *testrunv1.TestRunRequest { r := harnessSelection(); r.Record = true; return r }(), want: "the request's one selection must be every_suite"},
		{name: "record and coverage", recordOut: "/r", req: &testrunv1.TestRunRequest{Selections: []*testrunv1.TestSelection{everySuiteSel()}, Record: true, Coverage: true}, want: "record and coverage cannot be combined"},
		{name: "a record with nowhere to stage it", req: &testrunv1.TestRunRequest{Selections: []*testrunv1.TestSelection{everySuiteSel()}, Record: true}, want: "record needs --record-out"},
		{name: "every suite beside another selection", recordOut: "/r", req: &testrunv1.TestRunRequest{Selections: []*testrunv1.TestSelection{everySuiteSel(), suiteSel("ert")}}, want: "the request's one selection"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			_, err := Args{Module: "/m", Selection: "/s.pb", Targeted: true, RecordOut: tc.recordOut}.WithRequest(tc.req)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("WithRequest = %v, want an error containing %q", err, tc.want)
			}
		})
	}
}

func TestRunAWholeSuiteRequestRunsAsASuiteRun(t *testing.T) {
	// Arrange
	h := newHarness(t, nil, nil)
	var stream bytes.Buffer
	h.deps.Events = events.New(&stream, h.deps.Log)
	path := writeSelection(t, &testrunv1.TestRunRequest{Selections: []*testrunv1.TestSelection{suiteSel("orchestrator-harness")}})

	// Act
	code := h.run(t, "--selection", path)

	// Assert
	if code != 0 || len(h.exec.ran) != 1 || h.exec.ran[0] != "orchestrator-harness" {
		t.Fatalf("exit = %d, ran %v, stderr:\n%s; want only the named suite", code, h.exec.ran, h.errOut)
	}
	if *h.reuser.mode != reuse.Reusing {
		t.Fatalf("reuse mode = %v, want Reusing: a whole suite's pass is a suite run's", *h.reuser.mode)
	}
	if evs := eventsOf(t, &stream); evs[len(evs)-1].GetEnded().GetPassed() == nil {
		t.Fatalf("events = %v, want the run to end passed", evs)
	}
}

func TestRunRefusesARequestItCannotLower(t *testing.T) {
	// Arrange
	h := newHarness(t, nil, nil)
	var stream bytes.Buffer
	h.deps.Events = events.New(&stream, h.deps.Log)
	req := harnessSelection()
	req.Coverage = true

	// Act
	code := h.run(t, "--selection", writeSelection(t, req))

	// Assert
	evs := eventsOf(t, &stream)
	if code != 1 || len(h.exec.ran) != 0 || len(evs) != 1 || !strings.Contains(evs[0].GetRefused().GetDetail(), "coverage measures whole suites") {
		t.Fatalf("exit = %d, ran %v, events = %v; want a refusal and nothing run", code, h.exec.ran, evs)
	}
}
