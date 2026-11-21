package profiling

import (
	"bytes"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"runtime/pprof"
	"slices"
	"strings"
	"testing"
)

// THESE TESTS NEVER RUN IN PARALLEL: Go holds one CPU profile per process.

// recorder keeps every event a profile records.
type recorder struct{ events []Event }

func (r *recorder) record(e Event) { r.events = append(r.events, e) }

func (r *recorder) kinds() []EventKind {
	out := make([]EventKind, len(r.events))
	for i, e := range r.events {
		out[i] = e.Kind
	}
	return out
}

func TestStartExitProfileWritesItsProfileWhenStopped(t *testing.T) {
	// Arrange
	dir := filepath.Join(t.TempDir(), "unit", "TestX")
	rec := &recorder{}
	want := filepath.Join(dir, fmt.Sprintf("daemon-%d.cpu.pprof", os.Getpid()))

	// Act
	stop, err := StartExitProfile("daemon", dir, rec.record)
	if err != nil {
		t.Fatal(err)
	}
	stop()
	stop()

	// Assert
	if !slices.Equal(rec.kinds(), []EventKind{ExitStarted, ExitStopped}) {
		t.Fatalf("events = %v, want one start and one stop however often stop is called", rec.events)
	}
	if !slices.Equal(rec.events[0].Files, []string{want}) || !slices.Equal(rec.events[1].Files, []string{want}) || rec.events[1].Reason != "exit" {
		t.Fatalf("events = %+v, want both naming %s and the stop's reason exit", rec.events, want)
	}
	raw, err := os.ReadFile(want)
	if err != nil || len(raw) == 0 {
		t.Fatalf("the profile %s = %d bytes, %v; want a written profile", want, len(raw), err)
	}
}

func TestStartExitProfileFailsLoudly(t *testing.T) {
	blocked := func(t *testing.T) string {
		file := filepath.Join(t.TempDir(), "a-file")
		if err := os.WriteFile(file, nil, 0o644); err != nil {
			t.Fatal(err)
		}
		return filepath.Join(file, "sub")
	}
	running := func(t *testing.T) string {
		var buf bytes.Buffer
		if err := pprof.StartCPUProfile(&buf); err != nil {
			t.Fatal(err)
		}
		t.Cleanup(pprof.StopCPUProfile)
		return t.TempDir()
	}
	tests := []struct {
		name    string
		dir     func(t *testing.T) string
		wantErr string
	}{
		{name: "a directory that cannot be made", dir: blocked, wantErr: "create the whole-life profile's directory"},
		{name: "a process already profiling", dir: running, wantErr: "start the whole-life cpu profile"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			rec := &recorder{}

			// Act
			stop, err := StartExitProfile("store", tt.dir(t), rec.record)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.wantErr) || stop != nil {
				t.Fatalf("stop %v, err %v; want no stop and an error containing %q", stop != nil, err, tt.wantErr)
			}
			if len(rec.events) != 1 || rec.events[0].Kind != Failed || rec.events[0].Reason != "exit_profile" || !errors.Is(rec.events[0].Err, err) {
				t.Fatalf("events = %+v, want one failure recording the cause", rec.events)
			}
			if d := Describe("store", rec.events[0]); d.Level != "error" || !strings.Contains(d.Text(), tt.wantErr) {
				t.Fatalf("the failure is recorded as %+v, want an error naming the cause", d)
			}
		})
	}
}

func TestArmProfilesTheWholeLifeWhenAskedAndArmsNoSession(t *testing.T) {
	// Arrange
	root, dir := t.TempDir(), t.TempDir()
	getenv := func(k string) string {
		return map[string]string{RootEnvironment: root, ExitProfileEnvironment: dir}[k]
	}
	rec := &recorder{}

	// Act
	stop, err := Arm("sidecar", &fakeLevel{}, rec.record, getenv, os.UserHomeDir)
	if err != nil {
		t.Fatal(err)
	}
	stop()

	// Assert
	if !slices.Equal(rec.kinds(), []EventKind{ExitStarted, ExitStopped}) {
		t.Fatalf("events = %v, want the whole-life profile started and written", rec.events)
	}
	if _, err := os.Stat(filepath.Join(dir, fmt.Sprintf("sidecar-%d.cpu.pprof", os.Getpid()))); err != nil {
		t.Fatalf("the whole-life profile was not written: %v", err)
	}
	if _, err := os.Stat(filepath.Join(root, ArmedDir)); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("the armed directory stat = %v, want none: a whole-life profile arms no session", err)
	}
}

func TestArmFailsWhenTheWholeLifeProfileCannotStart(t *testing.T) {
	// Arrange
	file := filepath.Join(t.TempDir(), "a-file")
	if err := os.WriteFile(file, nil, 0o644); err != nil {
		t.Fatal(err)
	}
	getenv := func(k string) string { return map[string]string{ExitProfileEnvironment: file}[k] }
	rec := &recorder{}

	// Act
	_, err := Arm("daemon", &fakeLevel{}, rec.record, getenv, os.UserHomeDir)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "start the whole-life profile") {
		t.Fatalf("err = %v, want the whole-life profile's failure", err)
	}
	if !slices.Equal(rec.kinds(), []EventKind{Failed}) {
		t.Fatalf("events = %v, want the failure recorded", rec.events)
	}
}
