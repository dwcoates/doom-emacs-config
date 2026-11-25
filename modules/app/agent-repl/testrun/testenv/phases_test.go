package testenv

import (
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"syscall"
	"testing"
)

func TestRecordPhaseAppendsWhatReadPhasesSums(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "phases.jsonl")
	getenv := func(k string) string { return map[string]string{PhasesOut: path}[k] }

	// Act
	errs := []error{
		RecordPhase(getenv, PhaseDaemonBootShared, 1.5),
		RecordPhase(getenv, PhaseDaemonBootFresh, 0.25),
		RecordPhase(getenv, PhaseDaemonBootFresh, 0.5),
		RecordPhase(getenv, PhaseHarnessSetup, 2),
	}
	got, err := ReadPhases(path)

	// Assert
	for _, e := range errs {
		if e != nil {
			t.Fatal(e)
		}
	}
	want := map[string]float64{PhaseDaemonBootShared: 1.5, PhaseDaemonBootFresh: 0.75, PhaseHarnessSetup: 2}
	if err != nil || !reflect.DeepEqual(got, want) {
		t.Fatalf("ReadPhases = %v, %v; want %v", got, err, want)
	}
}

func TestRecordPhaseOutsideARunThatAskedDoesNothing(t *testing.T) {
	// Act
	err := RecordPhase(func(string) string { return "" }, PhaseHarnessSetup, 1)

	// Assert
	if err != nil {
		t.Fatalf("RecordPhase = %v, want nothing done", err)
	}
}

func TestRecordPhaseRefusesAPhaseOutsideTheVocabulary(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "phases.jsonl")

	// Act
	err := RecordPhase(func(string) string { return path }, "daemon_serving", 1)

	// Assert: refused before anything is written.
	if err == nil || !strings.Contains(err.Error(), `"daemon_serving" is no phase`) {
		t.Fatalf("err = %v, want the phase refused", err)
	}
	if _, statErr := os.Stat(path); !os.IsNotExist(statErr) {
		t.Fatalf("the refused phase wrote %s (%v)", path, statErr)
	}
}

func TestRecordPhaseFailsWhereItCannotWrite(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "gone", "phases.jsonl")

	// Act
	err := RecordPhase(func(string) string { return path }, PhaseHarnessSetup, 1)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "open the phase CPU "+path) {
		t.Fatalf("err = %v, want the file named", err)
	}
}

func TestReadPhases(t *testing.T) {
	dir := t.TempDir()
	write := func(name, body string) string {
		p := filepath.Join(dir, name)
		if err := os.WriteFile(p, []byte(body), 0o644); err != nil {
			t.Fatal(err)
		}
		return p
	}
	cases := []struct {
		name    string
		path    string
		want    map[string]float64
		wantErr string
	}{
		{name: "a file nobody wrote answers none", path: filepath.Join(dir, "none.jsonl")},
		{name: "a line that is not JSON is an error", path: write("bad.jsonl", "nope\n"), wantErr: "line 1 of"},
		{name: "a phase outside the vocabulary is an error", path: write("unknown.jsonl", `{"phase":"other","cpu_seconds":1}`+"\n"), wantErr: "is no phase CPU"},
		{name: "lines sum by phase", path: write("ok.jsonl", `{"phase":"harness_setup","cpu_seconds":1}`+"\n"+`{"phase":"harness_setup","cpu_seconds":0.5}`+"\n"), want: map[string]float64{PhaseHarnessSetup: 1.5}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := ReadPhases(tc.path)

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("err = %v, want %q", err, tc.wantErr)
				}
				return
			}
			if err != nil || !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("ReadPhases = %v, %v; want %v", got, err, tc.want)
			}
		})
	}
}

func TestRusageCPUSumsUserAndSystemSeconds(t *testing.T) {
	cases := []struct {
		name string
		ru   syscall.Rusage
		want float64
	}{
		{"nothing used is zero", syscall.Rusage{}, 0},
		{"user and system add, microseconds included", syscall.Rusage{Utime: syscall.Timeval{Sec: 1, Usec: 250000}, Stime: syscall.Timeval{Sec: 2, Usec: 500000}}, 3.75},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := RusageCPU(&tc.ru)

			// Assert
			if got != tc.want {
				t.Fatalf("RusageCPU = %v; want %v", got, tc.want)
			}
		})
	}
}
