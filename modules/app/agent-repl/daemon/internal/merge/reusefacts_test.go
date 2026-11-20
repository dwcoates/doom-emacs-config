package merge

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
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

func TestAGateLogDirectoryThatCannotBeReadEscalatesTheCheck(t *testing.T) {
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
	assertBriefFailureEscalated(t, h, "the deploy check's brief could not be composed", "")
}

func TestTheIntakesBriefNamesTheLastGatesLog(t *testing.T) {
	cases := []struct {
		name string
		// gate is whether a gate logged its reuse before the deploy.
		gate bool
		want string
	}{
		{name: "the gate that ran last before the deploy is named by its log", gate: true, want: "aa-tests-1.log`"},
		{name: "a deploy no gate ran before says so", want: "none logged one"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			if tc.gate {
				writeGate(t, mkdir(t, LogsDir(h.stateDir)), "aa-tests-1.log", reuseLog(670, 0), deployStarted.Add(-time.Minute))
			}
			q, _, _ := h.o.checkedQueueOf(context.Background(), mediationKey(h))

			// Act
			_, _, err := h.o.intakeBrief(context.Background(), q, *owedCheck(t, h))

			// Assert
			values := h.briefValues[len(h.briefValues)-1]
			if err != nil || !strings.HasSuffix(values["gate_log"], tc.want) {
				t.Fatalf("intakeBrief = %v, gate_log %q; want it to end %q", err, values["gate_log"], tc.want)
			}
		})
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
