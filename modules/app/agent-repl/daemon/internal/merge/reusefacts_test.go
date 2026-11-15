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

func TestJudgeReuseFindsAGateFarBelowTheGatesBeforeIt(t *testing.T) {
	recent := []GateReuse{{Units: 600, Reused: 300}, {Units: 600, Reused: 240}, {Units: 600, Reused: 360}}
	tests := []struct {
		name   string
		this   GateReuse
		recent []GateReuse
		want   bool
	}{
		{name: "a large gate reusing nothing", this: GateReuse{Units: 670}, recent: recent, want: true},
		{name: "a large gate in line with the others", this: GateReuse{Units: 600, Reused: 270}, recent: recent},
		{name: "a small gate reusing nothing is never judged", this: GateReuse{Units: 40}, recent: recent},
		{name: "a gate with none before it is never judged", this: GateReuse{Units: 670}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, _ := JudgeReuse(tt.this, tt.recent)

			// Assert
			if got != tt.want {
				t.Fatalf("JudgeReuse = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestRenderReuseStatesTheLastGateAgainstTheOthers(t *testing.T) {
	tests := []struct {
		name  string
		gates []GateReuse
		want  string
	}{
		{name: "no gate before the deploy", want: "No gate logged its test reuse before this deploy."},
		{name: "a gate far below", gates: []GateReuse{{Path: "/l/a-tests-1.log", Units: 670, Run: 670, Why: map[string]int{"inputs changed": 670}}, {Units: 600, Reused: 300}},
			want: "- NEW: the last gate before the deploy (/l/a-tests-1.log) reused 0 of 670 units (0%) and ran 670 (670 inputs changed); the 1 gates before it reused a median 50%"},
		{name: "a gate in line", gates: []GateReuse{{Path: "/l/a-tests-1.log", Units: 600, Reused: 300, Run: 300}, {Units: 600, Reused: 300}},
			want: "- SEEN BEFORE: the last gate before the deploy"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := RenderReuse(tt.gates)

			// Assert
			if !strings.Contains(got, tt.want) {
				t.Fatalf("RenderReuse = %q, want it to hold %q", got, tt.want)
			}
		})
	}
}

func TestGateReuseBeforeReadsTheNewestGatesBeforeTheDeploy(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	writeGate(t, dir, "aa-tests-1.log", reuseLog(600, 300), deployStarted.Add(-2*time.Hour))
	writeGate(t, dir, "bb-tests-1.log", reuseLog(670, 0), deployStarted.Add(-time.Minute))
	writeGate(t, dir, "cc-tests-1.log", reuseLog(600, 500), deployStarted.Add(time.Minute))
	writeGate(t, dir, "dd-tests-1.log", "no reuse here\n", deployStarted.Add(-30*time.Minute))
	writeGate(t, dir, "release-1.log", reuseLog(1, 1), deployStarted.Add(-time.Second))

	// Act
	got, err := gateReuseBefore(dir, deployStarted)

	// Assert
	if err != nil || len(got) != 2 || filepath.Base(got[0].Path) != "bb-tests-1.log" || filepath.Base(got[1].Path) != "aa-tests-1.log" {
		t.Fatalf("gateReuseBefore = (%+v, %v), want bb then aa", got, err)
	}
}

func TestGateReuseBeforeReadsNoGatesWhereNoneWereLogged(t *testing.T) {
	// Act
	got, err := gateReuseBefore(filepath.Join(t.TempDir(), "none"), deployStarted)

	// Assert
	if err != nil || len(got) != 0 {
		t.Fatalf("gateReuseBefore = (%+v, %v), want none", got, err)
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
	_, err := h.o.intakeBrief(context.Background(), q, *owedCheck(t, h))

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
