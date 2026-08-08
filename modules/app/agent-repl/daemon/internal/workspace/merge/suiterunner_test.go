package merge

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"
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
	got, err := r.RunSuite(context.Background(), repo)

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
	got, err := r.RunSuite(context.Background(), repo)

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
	got, err := r.RunSuite(context.Background(), repo)

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
	got, err := r.RunSuite(context.Background(), repo)
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
	got, err := r.RunSuite(context.Background(), repo)
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
	got, err := r.RunSuite(context.Background(), sub)

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
	got, err := r.RunSuite(context.Background(), repo)

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
	_, err := r.RunSuite(context.Background(), "")

	// Assert.
	if err == nil {
		t.Fatalf("RunSuite(\"\") err = nil, want a refusal")
	}
}

func TestRunSuiteSurfacesAnUnresolvableRepository(t *testing.T) {
	// Arrange — a directory that is not a git checkout at all.
	r, _ := newTestSuiteRunner(t)

	// Act.
	_, err := r.RunSuite(context.Background(), t.TempDir())

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
	got, err := r.RunSuite(context.Background(), repo)

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
