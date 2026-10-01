package cli

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"sync"
	"testing"
	"time"

	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/suites"
	"agentrepl/testrun/roster"
)

func TestParseArgs(t *testing.T) {
	tests := []struct {
		name    string
		argv    []string
		want    Args
		wantErr string
	}{
		{name: "module only selects every suite", argv: []string{"--module", "/m"}, want: Args{Module: "/m"}},
		{name: "record and a suite list", argv: []string{"--module", "/m", "--record", "--suites", "ert,daemon"}, want: Args{Module: "/m", Record: true, Selected: []string{"ert", "daemon"}}},
		{name: "the = spelling", argv: []string{"--module", "/m", "--suites=e2e"}, want: Args{Module: "/m", Selected: []string{"e2e"}}},
		{name: "an unknown argument", argv: []string{"--module", "/m", "--bogus"}, wantErr: "unknown argument '--bogus', expected --record or --suites <list>"},
		{name: "an unknown suite", argv: []string{"--module", "/m", "--suites", "nope"}, wantErr: "--suites names an unknown suite 'nope'; known suites: "},
		{name: "an empty suite list", argv: []string{"--module", "/m", "--suites", ""}, wantErr: "--suites needs at least one suite name"},
		{name: "an empty name in the list", argv: []string{"--module", "/m", "--suites", "ert,,daemon"}, wantErr: "--suites contains an empty suite name: 'ert,,daemon'"},
		{name: "--suites without a value", argv: []string{"--module", "/m", "--suites"}, wantErr: "--suites needs a comma-separated suite list"},
		{name: "--module without a value", argv: []string{"--module"}, wantErr: "--module needs a directory"},
		{name: "no module", argv: []string{"--record"}, wantErr: "--module is required"},
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

func TestSlotsForHost(t *testing.T) {
	for cpus, want := range map[int]int{16: 14, 3: 1, 2: 1, 1: 1} {
		if got := SlotsForHost(cpus); got != want {
			t.Errorf("SlotsForHost(%d) = %d, want %d", cpus, got, want)
		}
	}
}

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
	rows := []TimingRow{{RunID: "r", RecordedAt: "t", Commit: "c", Branch: "b", Suite: "ert", Seconds: 1.5}}

	// Act
	err := AppendTimings(path, rows)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	data, _ := os.ReadFile(path)
	if want := CSVHeader + "\nr,t,c,b,ert,1.500\n"; string(data) != want {
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
		{name: "a comma in a field", row: TimingRow{RunID: "r", RecordedAt: "t", Commit: "c", Branch: "a,b", Suite: "s"}, wantErr: "branch is not CSV-safe"},
		{name: "an empty field", row: TimingRow{RunID: "r", RecordedAt: "t", Commit: "", Branch: "b", Suite: "s"}, wantErr: "commit is empty"},
		{name: "a concurrent writer", row: TimingRow{RunID: "r", RecordedAt: "t", Commit: "c", Branch: "b", Suite: "s"}, locked: true, wantErr: "another timing writer holds"},
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
			rows: []string{"p1,t,c,main,ert,10.000\n", "p2,t,c,main,ert,10.000\n", "p3,t,c,main,ert,10.000\n", "now,t,c,main,ert,20.000\n"},
			want: []string{"TIMING REGRESSION: ert 20.000s vs 10.000s recent average (+100.0%, +10.000s)"},
		},
		{
			name: "a small slowdown is not",
			rows: []string{"p1,t,c,main,ert,10.000\n", "p2,t,c,main,ert,10.000\n", "p3,t,c,main,ert,10.000\n", "now,t,c,main,ert,10.900\n"},
			want: []string{"no big timing regressions detected"},
		},
		{
			name: "fewer than three priors is no baseline",
			rows: []string{"p1,t,c,main,ert,10.000\n", "now,t,c,main,ert,99.000\n"},
			want: []string{"ert: only 1 prior main timing entries, regression baseline needs 3", "no big timing regressions detected"},
		},
		{
			name: "only the same branch counts, and only the last five",
			rows: []string{
				"o,t,c,other,ert,1.000\n",
				"p0,t,c,main,ert,100.000\n", "p1,t,c,main,ert,10.000\n", "p2,t,c,main,ert,10.000\n",
				"p3,t,c,main,ert,10.000\n", "p4,t,c,main,ert,10.000\n", "p5,t,c,main,ert,10.000\n",
				"now,t,c,main,ert,20.000\n",
			},
			want: []string{"TIMING REGRESSION: ert 20.000s vs 10.000s recent average (+100.0%, +10.000s)"},
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

func TestRegressionsRefusesAMalformedRow(t *testing.T) {
	// Arrange
	path := csvFile(t, "only,three,fields\n")

	// Act
	_, err := Regressions(path, "now", "main")

	// Assert
	if err == nil || !strings.Contains(err.Error(), "has 3 fields, want 6") {
		t.Fatalf("err = %v", err)
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

func (p fakeProc) Wait() (int, float64, error) { return p.exit, 0.5, nil }
func (p fakeProc) Kill()                       {}

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

type harness struct {
	deps   Deps
	exec   *fakeExec
	out    *bytes.Buffer
	errOut *bytes.Buffer
	module string
	git    *[]string
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
	return harness{deps: d, exec: e, out: out, errOut: errOut, module: module, git: gitCalls}
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
		"[agent-repl-tests] all agent-repl tests and coverage suites passed",
		"[agent-repl-tests] distribution: ",
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
	code := h.run(t, "--record")

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

func TestRunRecordAppendsEveryPassingSuite(t *testing.T) {
	// Arrange
	h := newHarness(t, nil, nil)

	// Act
	code := h.run(t, "--record", "--suites", "ert,daemon")

	// Assert
	if code != 0 {
		t.Fatalf("exit = %d, stderr:\n%s", code, h.errOut)
	}
	data, _ := os.ReadFile(filepath.Join(h.module, "test_time.csv"))
	lines := strings.Split(strings.TrimSpace(string(data)), "\n")
	// Roster order: ert is listed before daemon.
	if len(lines) != 3 || !strings.Contains(lines[1], ",abc123,main,ert,") || !strings.Contains(lines[2], ",abc123,main,daemon,") {
		t.Fatalf("csv = %q", data)
	}
	if len(*h.git) != 2 {
		t.Fatalf("git was asked %d times, want before and after", len(*h.git))
	}
	if !strings.Contains(h.out.String(), "recorded 2 suite timings") {
		t.Errorf("stdout lacks the record line")
	}
}

func TestRunRecordRefusesAMovedHead(t *testing.T) {
	// Arrange
	h := newHarness(t, nil, nil)
	n := 0
	h.deps.Git = func(string) (string, string, error) {
		n++
		return "main", map[int]string{1: "old", 2: "new"}[n], nil
	}

	// Act
	code := h.run(t, "--record", "--suites", "ert")

	// Assert
	if code != 1 || !strings.Contains(h.errOut.String(), "git commit changed during tests: old -> new") {
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
			argv:    []string{"--record"},
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
