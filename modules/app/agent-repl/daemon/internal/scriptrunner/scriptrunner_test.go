package scriptrunner

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/dlog"
)

// writeScript writes an executable shell script named name into dir and
// returns its path.
func writeScript(t *testing.T, dir, name, body string) string {
	t.Helper()
	path := filepath.Join(dir, name)
	if err := os.WriteFile(path, []byte("#!/bin/sh\n"+body), 0o755); err != nil {
		t.Fatalf("write script %s: %v", name, err)
	}
	return path
}

func newRunner(t *testing.T) *Runner {
	t.Helper()
	r, err := New(dlog.NewTestLogger())
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	return r
}

func TestRunSuccessCapturesOutputAndZeroExit(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	script := writeScript(t, dir, "ok.sh", "echo hello\n")
	r := newRunner(t)

	// Act.
	output, code, err := r.Run(context.Background(), dir, []string{script})

	// Assert.
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if code != 0 {
		t.Fatalf("code = %d, want 0", code)
	}
	if !strings.Contains(output, "hello") {
		t.Fatalf("output = %q, want it to contain %q", output, "hello")
	}
}

func TestRunNonZeroExitIsNotAnError(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	script := writeScript(t, dir, "fail.sh", "exit 7\n")
	r := newRunner(t)

	// Act.
	_, code, err := r.Run(context.Background(), dir, []string{script})

	// Assert: a script that ran and failed is a non-zero code and a nil
	// error, never an error.
	if err != nil {
		t.Fatalf("Run: %v, want nil error for a script that ran and failed", err)
	}
	if code != 7 {
		t.Fatalf("code = %d, want 7", code)
	}
}

// TestRunNonZeroExitLeavesTheLevelToItsCaller: the runner cannot know whether
// an exit was expected (`launchctl print` of a stopped service exits 113), so a
// non-zero exit is recorded at DEBUG and never at WARN or ERROR; the caller
// that asked logs its own judgement of the code.
func TestRunNonZeroExitLeavesTheLevelToItsCaller(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	script := writeScript(t, dir, "fail.sh", "exit 113\n")
	log := dlog.NewTestLogger()
	r, err := New(log)
	if err != nil {
		t.Fatalf("New: %v", err)
	}

	// Act.
	_, code, err := r.Run(context.Background(), dir, []string{script})

	// Assert.
	if err != nil || code != 113 {
		t.Fatalf("Run = %d, %v; want 113 and nil", code, err)
	}
	var levels []string
	for _, rec := range log.Records() {
		levels = append(levels, rec.Level)
	}
	if strings.Join(levels, ",") != "debug" {
		t.Fatalf("record levels = %v, want exactly one debug record", levels)
	}
}

func TestRunCombinesStderrIntoOutput(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	script := writeScript(t, dir, "both.sh", "echo out-line\necho err-line 1>&2\n")
	r := newRunner(t)

	// Act.
	output, _, err := r.Run(context.Background(), dir, []string{script})

	// Assert.
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if !strings.Contains(output, "out-line") || !strings.Contains(output, "err-line") {
		t.Fatalf("output = %q, want both stdout and stderr lines", output)
	}
}

func TestRunRefusesEmptyArgv(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	r := newRunner(t)

	// Act.
	_, _, err := r.Run(context.Background(), dir, nil)

	// Assert.
	if err == nil {
		t.Fatalf("Run: nil error, want a refusal for an empty argv")
	}
}

func TestRunRefusesEmptyDir(t *testing.T) {
	// Arrange.
	r := newRunner(t)

	// Act.
	_, _, err := r.Run(context.Background(), "", []string{"/bin/echo", "hi"})

	// Assert.
	if err == nil {
		t.Fatalf("Run: nil error, want a refusal for an empty dir")
	}
}

func TestRunMissingScriptIsAnErrorNotAnExitCode(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	missing := filepath.Join(dir, "does-not-exist.sh")
	r := newRunner(t)

	// Act.
	_, code, err := r.Run(context.Background(), dir, []string{missing})

	// Assert: a script that could not be spawned could not be classified, so
	// it must surface as an error rather than a fabricated exit code.
	if err == nil {
		t.Fatalf("Run: nil error, want an error for a script that cannot be spawned")
	}
	if code != 0 {
		t.Fatalf("code = %d, want 0 alongside the error", code)
	}
}

func TestRunScrubsGitEnvironmentVariables(t *testing.T) {
	// Arrange.
	t.Setenv("GIT_DIR", "/leaked/git/dir")
	dir := t.TempDir()
	script := writeScript(t, dir, "echo-git-dir.sh", "echo \"${GIT_DIR:-unset}\"\n")
	r := newRunner(t)

	// Act.
	output, _, err := r.Run(context.Background(), dir, []string{script})

	// Assert.
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if strings.TrimSpace(output) != "unset" {
		t.Fatalf("output = %q, want GIT_DIR scrubbed to unset", output)
	}
}

func TestNewRequiresALogger(t *testing.T) {
	// Arrange, Act.
	r, err := New(nil)

	// Assert.
	if err == nil {
		t.Fatalf("New: nil error, want a refusal for a nil logger")
	}
	if r != nil {
		t.Fatalf("New: %+v, want nil runner alongside the error", r)
	}
}

func TestRunLinesHandsOverEveryLineInOrder(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	script := writeScript(t, dir, "lines.sh", "echo one\necho two >&2\necho three\n")
	r := newRunner(t)
	var got []string

	// Act.
	_, _, err := r.RunLines(context.Background(), dir, []string{script}, func(line string) { got = append(got, line) })

	// Assert.
	if err != nil {
		t.Fatalf("RunLines: %v", err)
	}
	if want := []string{"one", "two", "three"}; strings.Join(got, ",") != strings.Join(want, ",") {
		t.Fatalf("lines = %q, want %q", got, want)
	}
}

func TestRunLinesHandsOverALastLineWithNoNewline(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	script := writeScript(t, dir, "tail.sh", "printf 'first\\nlast'\n")
	r := newRunner(t)
	var got []string

	// Act.
	_, _, err := r.RunLines(context.Background(), dir, []string{script}, func(line string) { got = append(got, line) })

	// Assert.
	if err != nil {
		t.Fatalf("RunLines: %v", err)
	}
	if want := []string{"first", "last"}; strings.Join(got, ",") != strings.Join(want, ",") {
		t.Fatalf("lines = %q, want %q", got, want)
	}
}

func TestRunLinesStillAnswersTheWholeOutput(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	script := writeScript(t, dir, "whole.sh", "echo one\necho two\n")
	r := newRunner(t)

	// Act.
	output, _, err := r.RunLines(context.Background(), dir, []string{script}, func(string) {})

	// Assert.
	if err != nil {
		t.Fatalf("RunLines: %v", err)
	}
	if output != "one\ntwo\n" {
		t.Fatalf("output = %q, want the whole run", output)
	}
}

func TestLineWriterHandsOverALineSplitAcrossWrites(t *testing.T) {
	// Arrange.
	var got []string
	w := &lineWriter{onLine: func(line string) { got = append(got, line) }}

	// Act.
	w.Write([]byte("hal"))
	w.Write([]byte("f\nwhole\n"))

	// Assert.
	if want := []string{"half", "whole"}; strings.Join(got, ",") != strings.Join(want, ",") {
		t.Fatalf("lines = %q, want %q", got, want)
	}
}
