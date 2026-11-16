package merge

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// reuseLog is a gate log whose run reused REUSED of UNITS.
func reuseLog(units, reused int) string {
	return fmt.Sprintf("[agent-repl-tests] daemon: reuse: %d units, %d reused, %d run (%d inputs changed)\n[agent-repl-tests] reuse: %d units, %d reused, %d run (%d inputs changed)\n",
		units, reused, units-reused, units-reused, units, reused, units-reused, units-reused)
}

// writeGate writes gate log NAME into DIR, last written AT.
func writeGate(t *testing.T, dir, name, body string, at time.Time) {
	t.Helper()
	path := filepath.Join(dir, name)
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		t.Fatal(err)
	}
	if err := os.Chtimes(path, at, at); err != nil {
		t.Fatal(err)
	}
}

func TestParseGateReuseReadsTheRunsReuse(t *testing.T) {
	tests := []struct {
		name   string
		text   string
		want   GateReuse
		wantOK bool
	}{
		{name: "the run's total line wins over its suites'", wantOK: true,
			text: "[agent-repl-tests] a: reuse: 4 units, 1 reused, 3 run (3 inputs changed)\n[agent-repl-tests] reuse: 10 units, 6 reused, 4 run (1 failed before, 3 inputs changed)\n",
			want: GateReuse{Units: 10, Reused: 6, Run: 4, Why: map[string]int{"failed before": 1, "inputs changed": 3}}},
		{name: "a run that printed no total sums its suites", wantOK: true,
			text: "[agent-repl-tests] a: reuse: 4 units, 1 reused, 3 run (3 inputs changed)\n[agent-repl-tests] b: reuse: 1 units, 0 reused, 1 run (1 never reused)\n",
			want: GateReuse{Units: 5, Reused: 1, Run: 4, Why: map[string]int{"inputs changed": 3, "never reused": 1}}},
		{name: "a log with no reuse line", text: "[agent-repl-tests] the run's session is 1\n"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, ok := ParseGateReuse(tt.text)

			// Assert
			if ok != tt.wantOK || (ok && fmt.Sprint(got) != fmt.Sprint(tt.want)) {
				t.Fatalf("ParseGateReuse = (%+v, %v), want (%+v, %v)", got, ok, tt.want, tt.wantOK)
			}
		})
	}
}

func TestUnexplainedCountsTheUnitsRunForNoReasonOfTheirOwn(t *testing.T) {
	tests := []struct {
		name string
		why  map[string]int
		want int
	}{
		{name: "changed inputs, earlier failures and never-reused units explain their runs",
			why: map[string]int{"inputs changed": 326, "failed before": 1, "never reused": 12}},
		{name: "every other reason is unexplained", want: 10,
			why: map[string]int{"never recorded": 3, "not reusing": 4, "targeted": 2, "no sandbox": 1, "inputs changed": 5}},
		{name: "a gate that ran nothing", why: map[string]int{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := GateReuse{Why: tt.why}.Unexplained()

			// Assert
			if got != tt.want {
				t.Fatalf("Unexplained = %d, want %d", got, tt.want)
			}
		})
	}
}

func TestJudgeReuseFindsAGateThatRanManyUnitsForNoReasonOfTheirOwn(t *testing.T) {
	tests := []struct {
		name string
		gate GateReuse
		want bool
	}{
		{name: "a gate reusing nothing because every unit's inputs changed",
			gate: GateReuse{Units: 339, Run: 339, Why: map[string]int{"inputs changed": 326, "failed before": 1, "never reused": 12}}},
		{name: "a gate with a few units never recorded",
			gate: GateReuse{Units: 339, Reused: 330, Run: 9, Why: map[string]int{"never recorded": 3, "inputs changed": 6}}},
		{name: "a gate one unit short of the bound",
			gate: GateReuse{Units: 339, Reused: 330, Run: 9, Why: map[string]int{"never recorded": 9}}},
		{name: "a gate at the bound", want: true,
			gate: GateReuse{Units: 339, Reused: 329, Run: 10, Why: map[string]int{"never recorded": 10}}},
		{name: "a gate whose records were lost", want: true,
			gate: GateReuse{Units: 339, Run: 339, Why: map[string]int{"never recorded": 339}}},
		{name: "a gate that reused nothing at all", want: true,
			gate: GateReuse{Units: 339, Run: 339, Why: map[string]int{"not reusing": 339}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := JudgeReuse(tt.gate)

			// Assert
			if got != tt.want {
				t.Fatalf("JudgeReuse = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestRenderReuseStatesTheLastGatesUnexplainedRuns(t *testing.T) {
	tests := []struct {
		name string
		gate GateReuse
		ok   bool
		want string
	}{
		{name: "no gate before the deploy", want: "No gate logged its test reuse before this deploy."},
		{name: "a gate with many unexplained runs", ok: true,
			gate: GateReuse{Path: "/l/a-tests-1.log", Units: 670, Run: 670, Why: map[string]int{"never recorded": 670}},
			want: "- NEW: the last gate before the deploy (/l/a-tests-1.log) reused 0 of 670 units (0%) and ran 670 (670 never recorded); 670 of them ran for no reason of their own (not inputs changed, failed before or never reused), and 10 or more is NEW."},
		{name: "a gate whose runs are all explained", ok: true,
			gate: GateReuse{Path: "/l/a-tests-1.log", Units: 670, Run: 670, Why: map[string]int{"inputs changed": 670}},
			want: "- SEEN BEFORE: the last gate before the deploy (/l/a-tests-1.log) reused 0 of 670 units (0%) and ran 670 (670 inputs changed); 0 of them"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := RenderReuse(tt.gate, tt.ok)

			// Assert
			if !strings.Contains(got, tt.want) {
				t.Fatalf("RenderReuse = %q, want it to hold %q", got, tt.want)
			}
		})
	}
}

func TestLastGateReuseBeforeReadsTheNewestGateBeforeTheDeploy(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	writeGate(t, dir, "aa-tests-1.log", reuseLog(600, 300), deployStarted.Add(-2*time.Hour))
	writeGate(t, dir, "bb-tests-1.log", reuseLog(670, 0), deployStarted.Add(-time.Minute))
	writeGate(t, dir, "cc-tests-1.log", reuseLog(600, 500), deployStarted.Add(time.Minute))
	writeGate(t, dir, "dd-tests-1.log", "no reuse here\n", deployStarted.Add(-10*time.Second))
	writeGate(t, dir, "release-1.log", reuseLog(1, 1), deployStarted.Add(-time.Second))

	// Act
	got, ok, err := lastGateReuseBefore(dir, deployStarted)

	// Assert: cc was written after the deploy began, dd states no reuse and
	// release-1 is no gate's log.
	if err != nil || !ok || filepath.Base(got.Path) != "bb-tests-1.log" || got.Units != 670 {
		t.Fatalf("lastGateReuseBefore = (%+v, %v, %v), want bb", got, ok, err)
	}
}

func TestLastGateReuseBeforeReadsNoGateWhereNoneWasLogged(t *testing.T) {
	// Act
	got, ok, err := lastGateReuseBefore(filepath.Join(t.TempDir(), "none"), deployStarted)

	// Assert
	if err != nil || ok {
		t.Fatalf("lastGateReuseBefore = (%+v, %v, %v), want none", got, ok, err)
	}
}

func TestAGateLogDirectoryThatCannotBeReadKeepsTheQueueHeld(t *testing.T) {
	// Arrange: the logs directory is a file.
	h := checked(t)
	owe(t, h)
	if err := os.MkdirAll(h.stateDir, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(LogsDir(h.stateDir), []byte("not a directory"), 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	h.o.Mediate(context.Background())

	// Assert
	if owedCheck(t, h) == nil || len(h.queue.submissions) != 0 {
		t.Fatalf("submitted %d; want no brief and the check still owed", len(h.queue.submissions))
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation, Message: "the intake's brief could not be composed; the queue stays held"}); !ok {
		t.Fatalf("records = %v, want the failed brief at ERROR", h.logs.Records())
	}
}

func TestTheIntakesBriefStatesTheGatesReuse(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	writeGate(t, mkdir(t, LogsDir(h.stateDir)), "aa-tests-1.log", reuseLog(670, 0), deployStarted.Add(-time.Minute))
	q, _, _ := h.o.checkedQueueOf(context.Background(), mediationKey(h))

	// Act
	_, _, err := h.o.intakeBrief(context.Background(), q, *owedCheck(t, h))

	// Assert
	values := h.briefValues[len(h.briefValues)-1]
	if err != nil || !strings.Contains(values["reuse"], "reused 0 of 670 units") {
		t.Fatalf("intakeBrief = %v, reuse %q; want the gate's reuse stated", err, values["reuse"])
	}
}

// mkdir makes DIR and answers it.
func mkdir(t *testing.T, dir string) string {
	t.Helper()
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	return dir
}
