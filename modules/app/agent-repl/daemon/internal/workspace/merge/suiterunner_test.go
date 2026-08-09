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

// writeEntrypoint plants an executable test entrypoint at the repository's
// canonical path, whose body is script.
func writeEntrypoint(t *testing.T, repo, script string) {
	t.Helper()
	dir := filepath.Join(repo, "modules", "app", "agent-repl", "bin")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("mkdir entrypoint dir: %v", err)
	}
	if err := os.WriteFile(filepath.Join(dir, "test-all.sh"), []byte(script), 0o755); err != nil {
		t.Fatalf("write entrypoint: %v", err)
	}
}

func newTestSuiteRunner(t *testing.T) (*RepoSuiteRunner, *[]string) {
	t.Helper()
	var logged []string
	r, err := NewRepoSuiteRunner(func(format string, args ...any) {
		logged = append(logged, format)
		t.Logf(format, args...)
	})
	if err != nil {
		t.Fatalf("NewRepoSuiteRunner: %v", err)
	}
	return r, &logged
}

func TestNewRepoSuiteRunnerRequiresALogger(t *testing.T) {
	// Act.
	_, err := NewRepoSuiteRunner(nil)

	// Assert — a runner that cannot say it skipped the gate is refused.
	if err == nil {
		t.Fatalf("NewRepoSuiteRunner(nil) err = nil, want a construction error")
	}
}

func TestRunSuiteSkipsARepositoryWithNoEntrypoint(t *testing.T) {
	// Arrange — a repository that declares no test suite at all.
	repo := initTarget(t)
	r, logged := newTestSuiteRunner(t)

	// Act.
	got, err := r.RunSuite(context.Background(), repo, SuiteRun{Attempt: 1})

	// Assert — skipped, not passed, and named out loud.
	if err != nil {
		t.Fatalf("RunSuite() err = %v", err)
	}
	if !got.Skipped || got.Passed {
		t.Fatalf("RunSuite() = %+v, want skipped and not passed", got)
	}
	if !strings.Contains(got.Reason, "no test entrypoint") {
		t.Errorf("Reason = %q, want it to name the absence", got.Reason)
	}
	var loud bool
	for _, line := range *logged {
		if strings.Contains(line, "suite SKIPPED") {
			loud = true
		}
	}
	if !loud {
		t.Errorf("the skip was not loud-logged; got %v", *logged)
	}
}

func TestRunSuitePassesWhenTheEntrypointSucceeds(t *testing.T) {
	// Arrange.
	repo := initTarget(t)
	writeEntrypoint(t, repo, "#!/usr/bin/env bash\necho all good\n")
	r, _ := newTestSuiteRunner(t)

	// Act.
	got, err := r.RunSuite(context.Background(), repo, SuiteRun{Attempt: 1})

	// Assert.
	if err != nil {
		t.Fatalf("RunSuite() err = %v", err)
	}
	if got.Skipped || !got.Passed {
		t.Fatalf("RunSuite() = %+v, want a passing verdict", got)
	}
}

func TestRunSuiteFailsWithTheOutputTailWhenTheEntrypointExitsNonZero(t *testing.T) {
	// Arrange.
	repo := initTarget(t)
	writeEntrypoint(t, repo, "#!/usr/bin/env bash\necho 'FAIL: three cases'\nexit 3\n")
	r, _ := newTestSuiteRunner(t)

	// Act.
	got, err := r.RunSuite(context.Background(), repo, SuiteRun{Attempt: 1})

	// Assert — a verdict, never an error, carrying what failed.
	if err != nil {
		t.Fatalf("RunSuite() err = %v; a failing suite is a verdict, not an error", err)
	}
	if got.Skipped || got.Passed {
		t.Fatalf("RunSuite() = %+v, want a failing verdict", got)
	}
	if !strings.Contains(got.Tail, "FAIL: three cases") {
		t.Errorf("Tail = %q, want the suite's own output", got.Tail)
	}
}

func TestRunSuiteArchivesAFailingRunsCompleteOutput(t *testing.T) {
	// Arrange — a runner that keeps going after its failure, so the retained
	// tail ends on the LATER suites and the failure itself is only in the
	// archive. This is the real multi-suite entrypoint's shape.
	repo := initTarget(t)
	writeEntrypoint(t, repo, "#!/usr/bin/env bash\necho 'FAIL: the one that broke'\n"+
		"head -c 5000 /dev/zero | tr '\\0' 'x'\necho\nexit 1\n")
	r, _ := newTestSuiteRunner(t)

	// Act.
	got, err := r.RunSuite(context.Background(), repo, SuiteRun{Attempt: 1})
	if err != nil {
		t.Fatalf("RunSuite() err = %v", err)
	}
	t.Cleanup(func() { _ = os.Remove(got.OutputPath) })

	// Assert — the tail lost the failure; the archive kept it.
	if strings.Contains(got.Tail, "FAIL: the one that broke") {
		t.Fatalf("the arrangement did not push the failure out of the tail")
	}
	if got.OutputPath == "" {
		t.Fatalf("OutputPath = %q, want the archive of the full run", got.OutputPath)
	}
	full, readErr := os.ReadFile(got.OutputPath)
	if readErr != nil {
		t.Fatalf("read the archive: %v", readErr)
	}
	if !strings.Contains(string(full), "FAIL: the one that broke") {
		t.Errorf("the archive does not carry the failure the tail dropped")
	}
}

func TestRunSuiteArchivesNothingWhenTheSuitePasses(t *testing.T) {
	// Arrange.
	repo := initTarget(t)
	writeEntrypoint(t, repo, "#!/usr/bin/env bash\necho ok\n")
	r, _ := newTestSuiteRunner(t)

	// Act.
	got, err := r.RunSuite(context.Background(), repo, SuiteRun{Attempt: 1})
	if err != nil {
		t.Fatalf("RunSuite() err = %v", err)
	}

	// Assert — nothing to diagnose, nothing written.
	if got.OutputPath != "" {
		t.Errorf("OutputPath = %q, want none for a passing suite", got.OutputPath)
	}
}

func TestRunSuiteResolvesTheEntrypointFromTheRepositoryToplevel(t *testing.T) {
	// Arrange — the target dir is a SUBDIRECTORY of the checkout, which is what
	// a real worktree path can be.
	repo := initTarget(t)
	writeEntrypoint(t, repo, "#!/usr/bin/env bash\nexit 0\n")
	sub := filepath.Join(repo, "modules", "app")
	r, _ := newTestSuiteRunner(t)

	// Act.
	got, err := r.RunSuite(context.Background(), sub, SuiteRun{Attempt: 1})

	// Assert.
	if err != nil {
		t.Fatalf("RunSuite() err = %v", err)
	}
	if !got.Passed {
		t.Fatalf("RunSuite() = %+v, want the toplevel entrypoint found from a subdirectory", got)
	}
}

func TestRunSuiteRunsTheEntrypointFromTheRepositoryToplevel(t *testing.T) {
	// The suite's own relative paths only resolve from the toplevel, so the
	// working directory is pinned rather than inherited.
	// Arrange.
	repo := initTarget(t)
	writeEntrypoint(t, repo, "#!/usr/bin/env bash\npwd\n")
	r, _ := newTestSuiteRunner(t)
	// A failing run is what carries the output back, so make it fail on purpose.
	writeEntrypoint(t, repo, "#!/usr/bin/env bash\npwd\nexit 1\n")

	// Act.
	got, err := r.RunSuite(context.Background(), repo, SuiteRun{Attempt: 1})

	// Assert.
	if err != nil {
		t.Fatalf("RunSuite() err = %v", err)
	}
	resolved, symErr := filepath.EvalSymlinks(repo)
	if symErr != nil {
		t.Fatalf("resolve repo path: %v", symErr)
	}
	if !strings.Contains(got.Tail, resolved) {
		t.Errorf("Tail = %q, want the run's cwd to be the toplevel %q", got.Tail, resolved)
	}
}

func TestRunSuiteRequiresATargetDirectory(t *testing.T) {
	// Arrange.
	r, _ := newTestSuiteRunner(t)

	// Act.
	_, err := r.RunSuite(context.Background(), "", SuiteRun{Attempt: 1})

	// Assert.
	if err == nil {
		t.Fatalf("RunSuite(\"\") err = nil, want a refusal")
	}
}

func TestRunSuiteSurfacesAnUnresolvableRepository(t *testing.T) {
	// Arrange — a directory that is not a git checkout at all.
	r, _ := newTestSuiteRunner(t)

	// Act.
	_, err := r.RunSuite(context.Background(), t.TempDir(), SuiteRun{Attempt: 1})

	// Assert — the toplevel probe's failure is surfaced, never reported as a
	// skipped (and therefore harmless) gate.
	if err == nil {
		t.Fatalf("RunSuite() err = nil, want the unresolvable repository surfaced")
	}
	if !strings.Contains(err.Error(), "toplevel") {
		t.Errorf("err = %v, want it to name the toplevel resolution", err)
	}
}

func TestRunSuiteIgnoresANonExecutableEntrypoint(t *testing.T) {
	// Arrange — the file exists but cannot be run, which is not a suite.
	repo := initTarget(t)
	dir := filepath.Join(repo, "modules", "app", "agent-repl", "bin")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	if err := os.WriteFile(filepath.Join(dir, "test-all.sh"), []byte("#!/usr/bin/env bash\n"), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	r, _ := newTestSuiteRunner(t)

	// Act.
	got, err := r.RunSuite(context.Background(), repo, SuiteRun{Attempt: 1})

	// Assert.
	if err != nil {
		t.Fatalf("RunSuite() err = %v", err)
	}
	if !got.Skipped {
		t.Fatalf("RunSuite() = %+v, want a skip for a non-executable entrypoint", got)
	}
}

func TestTailOfKeepsTheEndOfTheOutput(t *testing.T) {
	// Arrange — output longer than the clamp, whose verdict is at the end.
	body := strings.Repeat("noise\n", 100) + "VERDICT"

	// Act.
	got := tailOf(body, 20)

	// Assert.
	if !strings.HasSuffix(got, "VERDICT") {
		t.Fatalf("tailOf() = %q, want it to end with the verdict", got)
	}
	if !strings.Contains(got, "truncated") {
		t.Errorf("tailOf() = %q, want the elision named", got)
	}
}

// --- suite selection reaches the entrypoint ---------------------------------

func TestRunSuitePassesTheSelectionAsSuitesArgument(t *testing.T) {
	// Arrange — an entrypoint that reports the arguments it was handed.
	repo := initTarget(t)
	writeEntrypoint(t, repo, "#!/usr/bin/env bash\necho \"args:$*\"\nexit 1\n")
	r, _ := newTestSuiteRunner(t)

	// Act.
	got, err := r.RunSuite(context.Background(), repo, SuiteRun{Suites: []string{"webapp", "daemon"}, Attempt: 1})
	if err != nil {
		t.Fatalf("RunSuite() err = %v", err)
	}
	t.Cleanup(func() { _ = os.Remove(got.OutputPath) })

	// Assert.
	if !strings.Contains(got.Tail, "args:--suites webapp,daemon") {
		t.Fatalf("entrypoint args = %q, want --suites webapp,daemon", got.Tail)
	}
}

// An empty selection means EVERY suite, and it must reach the entrypoint as no
// arguments at all — a foreign repository's entrypoint may know no such flag.
func TestRunSuitePassesNoArgumentsForAnEmptySelection(t *testing.T) {
	// Arrange.
	repo := initTarget(t)
	writeEntrypoint(t, repo, "#!/usr/bin/env bash\necho \"argc:$#\"\nexit 1\n")
	r, _ := newTestSuiteRunner(t)

	// Act.
	got, err := r.RunSuite(context.Background(), repo, SuiteRun{Attempt: 1})
	if err != nil {
		t.Fatalf("RunSuite() err = %v", err)
	}
	t.Cleanup(func() { _ = os.Remove(got.OutputPath) })

	// Assert.
	if !strings.Contains(got.Tail, "argc:0") {
		t.Fatalf("entrypoint argc = %q, want 0 for an empty selection", got.Tail)
	}
}

func TestRunSuiteRefusesASelectionNamingAnUnknownSuite(t *testing.T) {
	// Arrange.
	repo := initTarget(t)
	writeEntrypoint(t, repo, "#!/usr/bin/env bash\nexit 0\n")
	r, _ := newTestSuiteRunner(t)

	// Act.
	_, err := r.RunSuite(context.Background(), repo, SuiteRun{Suites: []string{"nope"}, Attempt: 1})

	// Assert — a mapping typo is a construction error here, not an unrunnable
	// gate at the entrypoint.
	if err == nil || !strings.Contains(err.Error(), "nope") {
		t.Fatalf("RunSuite() err = %v, want the unknown suite named", err)
	}
}

func TestRunSuiteRefusesARunWithNoAttempt(t *testing.T) {
	// Arrange.
	repo := initTarget(t)
	writeEntrypoint(t, repo, "#!/usr/bin/env bash\nexit 0\n")
	r, _ := newTestSuiteRunner(t)

	// Act.
	_, err := r.RunSuite(context.Background(), repo, SuiteRun{})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "Attempt") {
		t.Fatalf("RunSuite() err = %v, want a refusal naming Attempt", err)
	}
}

// A flake's evidence is the PAIR of runs, and the passing half is the one
// nothing would otherwise keep.
func TestRunSuiteArchivesAPassingRerun(t *testing.T) {
	// Arrange.
	repo := initTarget(t)
	writeEntrypoint(t, repo, "#!/usr/bin/env bash\necho 'the quiet run'\nexit 0\n")
	r, _ := newTestSuiteRunner(t)

	// Act.
	got, err := r.RunSuite(context.Background(), repo, SuiteRun{Attempt: 2})
	if err != nil {
		t.Fatalf("RunSuite() err = %v", err)
	}
	t.Cleanup(func() { _ = os.Remove(got.OutputPath) })

	// Assert.
	if got.OutputPath == "" {
		t.Fatalf("OutputPath = %q, want the re-run's output archived whatever its verdict", got.OutputPath)
	}
	full, readErr := os.ReadFile(got.OutputPath)
	if readErr != nil {
		t.Fatalf("read the archive: %v", readErr)
	}
	if !strings.Contains(string(full), "the quiet run") {
		t.Errorf("the re-run archive = %q, want the run's own output", string(full))
	}
}

func TestRunSuiteReportsTheRunsDuration(t *testing.T) {
	// Arrange.
	repo := initTarget(t)
	writeEntrypoint(t, repo, "#!/usr/bin/env bash\nexit 0\n")
	r, _ := newTestSuiteRunner(t)

	// Act.
	got, err := r.RunSuite(context.Background(), repo, SuiteRun{Attempt: 1})
	if err != nil {
		t.Fatalf("RunSuite() err = %v", err)
	}

	// Assert — a flake report is a comparison of two durations, so neither may
	// be zero-valued.
	if got.Duration <= 0 {
		t.Fatalf("Duration = %v, want the measured run time", got.Duration)
	}
}

// --- a suite that leaks a background process must not wedge the gate ---------
//
// THE PRODUCTION FAILURE THESE COVER. The runner used to capture the suite's
// output into a bytes.Buffer, which makes os/exec manufacture an os.Pipe and
// wait for it to reach EOF. Every descendant of the suite inherits the pipe's
// write end, so one leaked background daemon kept Wait blocked forever: the
// merge queue head logged "suite RUNNING" and then nothing at all, with the
// suite process already gone from the process table. Writing straight to an
// *os.File removes the pipe, and with it the wait.

// boundedRunSuite runs RunSuite off the test goroutine and fails the test the
// moment ctx expires.
//
// A plain call cannot express this: the regression it guards blocks inside
// cmd.Wait with no cancellation path at all (cancelling the context kills the
// suite process, which is already dead — it is the GRANDCHILD holding the
// pipe), so a regressed runner would hang the whole package until the go test
// binary's own timeout fired.
func boundedRunSuite(ctx context.Context, t *testing.T, r *RepoSuiteRunner, dir string, run SuiteRun) (SuiteResult, error) {
	t.Helper()
	type outcome struct {
		res SuiteResult
		err error
	}
	done := make(chan outcome, 1)
	go func() {
		res, err := r.RunSuite(ctx, dir, run)
		done <- outcome{res: res, err: err}
	}()
	select {
	case got := <-done:
		return got.res, got.err
	case <-ctx.Done():
		t.Fatalf("RunSuite did not return before the deadline (%v): a background process the suite leaked is holding the run open", ctx.Err())
		return SuiteResult{}, nil
	}
}

// suiteDeadline bounds every leak test. It is generous against a loaded
// machine and still orders of magnitude below the wedge it guards, which held
// the production queue head for twenty minutes.
const suiteDeadline = 30 * time.Second

func TestRunSuitePassesWhenTheSuiteLeavesABackgroundProcessRunning(t *testing.T) {
	// Arrange — an entrypoint that spawns a background process inheriting its
	// stdout and stderr and then exits cleanly, which is what the daemon e2e
	// suites do whenever one of the daemons or shims they start outlives them.
	repo := initTarget(t)
	writeEntrypoint(t, repo, "#!/usr/bin/env bash\nsleep 300 &\necho all good\nexit 0\n")
	r, _ := newTestSuiteRunner(t)
	ctx, cancel := context.WithTimeout(context.Background(), suiteDeadline)
	defer cancel()

	// Act.
	got, err := boundedRunSuite(ctx, t, r, repo, SuiteRun{Attempt: 1})

	// Assert — the verdict is the child's, not its descendants'.
	if err != nil {
		t.Fatalf("RunSuite() err = %v", err)
	}
	if got.Skipped || !got.Passed {
		t.Fatalf("RunSuite() = %+v, want a passing verdict", got)
	}
}

func TestRunSuiteFailsWithTheTailWhenALeakingSuiteExitsNonZero(t *testing.T) {
	// Arrange — the same leak, on the path where the output actually matters.
	repo := initTarget(t)
	writeEntrypoint(t, repo, "#!/usr/bin/env bash\nsleep 300 &\necho 'FAIL: the leaking suite'\nexit 1\n")
	r, _ := newTestSuiteRunner(t)
	ctx, cancel := context.WithTimeout(context.Background(), suiteDeadline)
	defer cancel()

	// Act.
	got, err := boundedRunSuite(ctx, t, r, repo, SuiteRun{Attempt: 1})
	if err != nil {
		t.Fatalf("RunSuite() err = %v", err)
	}
	t.Cleanup(func() { _ = os.Remove(got.OutputPath) })

	// Assert — the tail read back off the file still carries what failed.
	if got.Passed {
		t.Fatalf("RunSuite() = %+v, want a failing verdict", got)
	}
	if !strings.Contains(got.Tail, "FAIL: the leaking suite") {
		t.Errorf("Tail = %q, want the suite's own output read back from the file", got.Tail)
	}
}

// --- the tail now comes off the file -----------------------------------------

func TestRunSuiteClampsTheTailToTheLastSuiteTailBytes(t *testing.T) {
	// Arrange — far more output than the clamp, with the verdict at the very
	// end where a test runner puts it.
	repo := initTarget(t)
	writeEntrypoint(t, repo, "#!/usr/bin/env bash\n"+
		fmt.Sprintf("head -c %d /dev/zero | tr '\\0' 'x'\n", suiteTailBytes*2)+
		"echo\necho VERDICT-END\nexit 1\n")
	r, _ := newTestSuiteRunner(t)

	// Act.
	got, err := r.RunSuite(context.Background(), repo, SuiteRun{Attempt: 1})
	if err != nil {
		t.Fatalf("RunSuite() err = %v", err)
	}
	t.Cleanup(func() { _ = os.Remove(got.OutputPath) })

	// Assert — exactly the last suiteTailBytes, behind the elision marker.
	notice := truncationNotice(suiteTailBytes)
	if !strings.HasPrefix(got.Tail, notice) {
		t.Fatalf("Tail = %q..., want it to open with the elision marker", got.Tail[:min(len(got.Tail), 80)])
	}
	if len(got.Tail) != len(notice)+suiteTailBytes {
		t.Errorf("len(Tail) = %d, want %d", len(got.Tail), len(notice)+suiteTailBytes)
	}
	if !strings.HasSuffix(got.Tail, "VERDICT-END\n") {
		t.Errorf("Tail does not end on the verdict; got %q", got.Tail[max(0, len(got.Tail)-40):])
	}
}

// --- the output file is a precondition of the run, not a diagnostic after it -

func TestRunSuiteRefusesTheRunWhenTheOutputFileCannotBeCreated(t *testing.T) {
	// Arrange — a temp directory that does not exist, so the file the suite
	// would write its output to cannot be created.
	repo := initTarget(t)
	writeEntrypoint(t, repo, "#!/usr/bin/env bash\nexit 0\n")
	r, _ := newTestSuiteRunner(t)
	t.Setenv("TMPDIR", filepath.Join(t.TempDir(), "does-not-exist"))

	// Act.
	_, err := r.RunSuite(context.Background(), repo, SuiteRun{Attempt: 1})

	// Assert — an unrunnable suite, never a verdict: the run never happened.
	if err == nil {
		t.Fatalf("RunSuite() err = nil, want the missing output file surfaced")
	}
	if !strings.Contains(err.Error(), "output file") {
		t.Errorf("err = %v, want it to name the output file", err)
	}
}

func TestRunSuiteDoesNotReportRunningWhenTheOutputFileCannotBeCreated(t *testing.T) {
	// Arrange — the same failure, checked at the log.
	repo := initTarget(t)
	writeEntrypoint(t, repo, "#!/usr/bin/env bash\nexit 0\n")
	r, logged := newTestSuiteRunner(t)
	t.Setenv("TMPDIR", filepath.Join(t.TempDir(), "does-not-exist"))

	// Act.
	_, _ = r.RunSuite(context.Background(), repo, SuiteRun{Attempt: 1})

	// Assert — the refusal precedes the spawn, so nothing may claim a run began.
	for _, line := range *logged {
		if strings.Contains(line, "suite RUNNING") {
			t.Fatalf("logged %q, want no RUNNING line for a suite that was never spawned", line)
		}
	}
}

func TestRunSuiteRemovesTheOutputOfAPassingFirstAttempt(t *testing.T) {
	// Arrange — a private temp directory, so what the run leaves behind in it
	// is exactly what this run wrote.
	repo := initTarget(t)
	writeEntrypoint(t, repo, "#!/usr/bin/env bash\necho ok\n")
	r, _ := newTestSuiteRunner(t)
	tmp := t.TempDir()
	t.Setenv("TMPDIR", tmp)

	// Act.
	got, err := r.RunSuite(context.Background(), repo, SuiteRun{Attempt: 1})
	if err != nil {
		t.Fatalf("RunSuite() err = %v", err)
	}

	// Assert — nothing to diagnose, nothing kept: the output file that carried
	// the run is gone rather than leaked into the temp directory.
	if got.OutputPath != "" {
		t.Fatalf("OutputPath = %q, want none for a passing first attempt", got.OutputPath)
	}
	left, globErr := filepath.Glob(filepath.Join(tmp, suiteOutputPattern))
	if globErr != nil {
		t.Fatalf("glob: %v", globErr)
	}
	if len(left) != 0 {
		t.Errorf("temp directory holds %v, want the passing run's output removed", left)
	}
}
