package scriptrunner

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
	"time"

	"agent-repld/internal/dlog"
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

func TestRunCancelledByItsCallerIsAnError(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	script := writeScript(t, dir, "ok.sh", "exit 0\n")
	r := newRunner(t)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	_, _, err := r.Run(ctx, dir, []string{script})

	// Assert.
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("Run error = %v, want one wrapping context.Canceled", err)
	}
}

func TestRunCancelledByItsCallerIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	script := writeScript(t, dir, "ok.sh", "exit 0\n")
	log := dlog.NewTestLogger()
	r, err := New(log)
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	_, _, _ = r.Run(ctx, dir, []string{script})

	// Assert.
	var levels []string
	for _, rec := range log.Records() {
		levels = append(levels, rec.Level)
	}
	if strings.Join(levels, ",") != "info" {
		t.Fatalf("record levels = %v, want exactly one info record", levels)
	}
}

func TestRunPastItsDeadlineIsRecordedAtError(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	script := writeScript(t, dir, "ok.sh", "exit 0\n")
	log := dlog.NewTestLogger()
	r, err := New(log)
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	ctx, cancel := context.WithDeadline(context.Background(), time.Unix(0, 0))
	defer cancel()

	// Act.
	_, _, _ = r.Run(ctx, dir, []string{script})

	// Assert.
	var levels []string
	for _, rec := range log.Records() {
		levels = append(levels, rec.Level)
	}
	if strings.Join(levels, ",") != "error" {
		t.Fatalf("record levels = %v, want exactly one error record", levels)
	}
}

// runCancelledMidway runs body, cancels the run once the script prints
// "started", and answers what Run answered.
func runCancelledMidway(t *testing.T, r *Runner, body string) (int, error) {
	t.Helper()
	dir := t.TempDir()
	script := writeScript(t, dir, "blocks.sh", body)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	_, code, err := r.RunLines(ctx, dir, []string{script}, func(line string) {
		if line == "started" {
			cancel()
		}
	})
	return code, err
}

func TestRunKilledByItsCancelledContextIsACancellationNotAnExitCode(t *testing.T) {
	// Arrange: a script that blocks until it is killed.
	r := newRunner(t)

	// Act.
	_, err := runCancelledMidway(t, r, "echo started\nexec sleep 600\n")

	// Assert.
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("Run error = %v, want one wrapping context.Canceled", err)
	}
}

func TestRunKilledByItsCancelledContextIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	r, err := New(log)
	if err != nil {
		t.Fatalf("New: %v", err)
	}

	// Act.
	_, _ = runCancelledMidway(t, r, "echo started\nexec sleep 600\n")

	// Assert.
	var levels []string
	for _, rec := range log.Records() {
		levels = append(levels, rec.Level)
	}
	if strings.Join(levels, ",") != "info" {
		t.Fatalf("record levels = %v, want exactly one info record", levels)
	}
}

func TestRunCancelledTakesTheChildHoldingItsOutputWithIt(t *testing.T) {
	// Arrange: the script starts a child that inherits its output and blocks
	// reading a fifo; the fifo is written at cleanup so the child then exits.
	dir := t.TempDir()
	fifo := filepath.Join(dir, "hold")
	if err := syscall.Mkfifo(fifo, 0o600); err != nil {
		t.Fatalf("mkfifo: %v", err)
	}
	t.Cleanup(func() {
		// A child still reading is released; one the cancellation killed
		// leaves no reader, which a non-blocking open answers with ENXIO.
		f, err := os.OpenFile(fifo, os.O_WRONLY|syscall.O_NONBLOCK, 0)
		if errors.Is(err, syscall.ENXIO) {
			return
		}
		if err != nil {
			t.Errorf("release the child: %v", err)
			return
		}
		_ = f.Close()
	})
	r := newRunner(t)

	// Act: Run returning at all is the assertion's subject.
	_, err := runCancelledMidway(t, r, "cat '"+fifo+"' &\necho started\nwait\n")

	// Assert.
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("Run error = %v, want one wrapping context.Canceled", err)
	}
}

func TestRunWaitsForTheWholeOutputOfAScriptWhoseChildOutlivesIt(t *testing.T) {
	// Arrange: the script exits at once; a child it started writes after it.
	dir := t.TempDir()
	script := writeScript(t, dir, "late.sh", "(read _ < /dev/null; echo late) &\nexit 0\n")
	r := newRunner(t)

	// Act.
	out, code, err := r.Run(context.Background(), dir, []string{script})

	// Assert: an ordinary exit is never cut short of its output.
	if err != nil || code != 0 || strings.TrimSpace(out) != "late" {
		t.Fatalf("Run = (%q, %d, %v), want the child's late line, 0, nil", out, code, err)
	}
}

func TestKillGroupWithReadsTheKernelsAnswers(t *testing.T) {
	tests := []struct {
		name   string
		answer error
		want   error
	}{
		{name: "a group with no members has finished", answer: syscall.ESRCH, want: os.ErrProcessDone},
		{name: "a group of members awaiting reaping has finished", answer: syscall.EPERM, want: os.ErrProcessDone},
		{name: "a killed group was killed", answer: nil, want: nil},
		{name: "any other answer is the kill failing", answer: syscall.EINVAL, want: syscall.EINVAL},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			kill := func(int, syscall.Signal) error { return tt.answer }

			// Act
			got := killGroupWith(kill, 4242)

			// Assert
			if !errors.Is(got, tt.want) && got != tt.want {
				t.Fatalf("killGroupWith = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestKillGroupWithSignalsTheWholeGroup(t *testing.T) {
	// Arrange
	var pid int
	var sig syscall.Signal
	kill := func(p int, s syscall.Signal) error { pid, sig = p, s; return nil }

	// Act
	_ = killGroupWith(kill, 4242)

	// Assert
	if pid != -4242 || sig != syscall.SIGKILL {
		t.Fatalf("kill(%d, %v), want kill(-4242, SIGKILL): the group, not the leader alone", pid, sig)
	}
}

func TestRunLinesMeasuredCountsTheProcessorTimeOfTheScriptsChildren(t *testing.T) {
	// Arrange: the script itself only waits; its child does the work.
	dir := t.TempDir()
	script := writeScript(t, dir, "child.sh", "sh -c 'i=0; while [ $i -lt 100000 ]; do i=$((i+1)); done'\n")
	r := newRunner(t)

	// Act.
	run, err := r.RunLinesMeasured(context.Background(), dir, []string{script}, nil)

	// Assert.
	if err != nil {
		t.Fatalf("RunLinesMeasured: %v", err)
	}
	if run.CPU <= 0 || run.Wall <= 0 {
		t.Fatalf("measured cpu %v over wall %v, want both counted, the child's work included", run.CPU, run.Wall)
	}
}

func TestRunLinesMeasuredAnswersTheExitCodeAndOutput(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	script := writeScript(t, dir, "fail.sh", "echo said\nexit 3\n")
	r := newRunner(t)

	// Act.
	run, err := r.RunLinesMeasured(context.Background(), dir, []string{script}, nil)

	// Assert.
	if err != nil || run.ExitCode != 3 || !strings.Contains(run.Output, "said") {
		t.Fatalf("run = %+v, %v; want exit 3 with its output", run, err)
	}
}

func TestRunCores(t *testing.T) {
	tests := []struct {
		name string
		run  Run
		want float64
	}{
		{name: "two cores", run: Run{CPU: 4 * time.Second, Wall: 2 * time.Second}, want: 2},
		{name: "a fraction of one", run: Run{CPU: time.Second, Wall: 4 * time.Second}, want: 0.25},
		{name: "no time taken", run: Run{CPU: time.Second}, want: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := tt.run.Cores()

			// Assert.
			if got != tt.want {
				t.Fatalf("Cores = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestRunLinesWithAddsItsEnvironment(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	script := writeScript(t, dir, "env.sh", "echo \"$AGENT_REPL_HOOK_PROBE\"\n")
	r := newRunner(t)

	// Act.
	run, err := r.RunLinesWith(context.Background(), dir, []string{script}, Options{Env: []string{"AGENT_REPL_HOOK_PROBE=set-by-caller"}}, nil)

	// Assert.
	if err != nil || run.ExitCode != 0 {
		t.Fatalf("RunLinesWith = %+v, %v", run, err)
	}
	if !strings.Contains(run.Output, "set-by-caller") {
		t.Fatalf("output = %q, want the added variable", run.Output)
	}
}

func TestRunLinesWithStillScrubsGitVariablesBesideItsOwn(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	t.Setenv("GIT_DIR", "/leaked")
	script := writeScript(t, dir, "env.sh", "echo \"git=${GIT_DIR:-unset}\"\n")
	r := newRunner(t)

	// Act.
	run, err := r.RunLinesWith(context.Background(), dir, []string{script}, Options{Env: []string{"X=1"}}, nil)

	// Assert.
	if err != nil {
		t.Fatalf("RunLinesWith: %v", err)
	}
	if !strings.Contains(run.Output, "git=unset") {
		t.Fatalf("output = %q, want GIT_DIR scrubbed", run.Output)
	}
}

func TestRunLinesWithPastItsTimeoutIsATimedOutAnswer(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	script := writeScript(t, dir, "slow.sh", "echo started\nexec sleep 30\n")
	logger := dlog.NewTestLogger()
	r, err := New(logger)
	if err != nil {
		t.Fatal(err)
	}

	// Act.
	run, err := r.RunLinesWith(context.Background(), dir, []string{script}, Options{Timeout: 200 * time.Millisecond}, nil)

	// Assert.
	if err != nil {
		t.Fatalf("RunLinesWith = %v, want the timeout answered, not an error", err)
	}
	if !run.TimedOut || run.ExitCode != -1 {
		t.Fatalf("run = %+v, want TimedOut with exit -1", run)
	}
	if !strings.Contains(run.Output, "started") {
		t.Fatalf("output = %q, want what the script wrote before it was killed", run.Output)
	}
	for _, rec := range logger.Records() {
		if rec.Level == dlog.LevelError || rec.Level == dlog.LevelWarn {
			t.Fatalf("record %+v: a timeout the caller set is its to judge, never the runner's WARN or ERROR", rec)
		}
	}
}

func TestRunLinesWithCancelledByItsCallerIsStillAnError(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	script := writeScript(t, dir, "slow.sh", "exec sleep 30\n")
	r := newRunner(t)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	run, err := r.RunLinesWith(ctx, dir, []string{script}, Options{Timeout: time.Minute}, nil)

	// Assert.
	if !errors.Is(err, context.Canceled) || run.TimedOut {
		t.Fatalf("RunLinesWith = %+v, %v; want the caller's cancellation, not a timeout", run, err)
	}
}
