package merge

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"claude-repld/internal/dlog"
	"claude-repld/internal/gitexec"
)

// This file is merge.Driver's outbound port onto the TARGET REPOSITORY'S TEST
// SUITE, plus its production implementation.
//
// WHY THE MERGE OWNS THE TEST GATE. The gate used to live in the repository's
// pre-commit hook, which ran the whole suite before any agent-authored commit
// was allowed. That is the wrong place for it twice over: it taxes every
// intermediate commit an agent makes on its own branch, and it says nothing
// about the state of the TARGET, which is the tree anyone else actually works
// from. Gating per cherry-picked commit at merge time inverts both — an agent
// commits freely on its branch, and nothing reaches the target without the
// suite having passed on the exact tree the pick produced.

// SuiteResult is one run of the target repository's test suite.
//
// Skipped and Passed are deliberately separate: "there was no suite to run" is
// not "the suite passed", and conflating them would let a repository with no
// entrypoint report a green gate it never had.
type SuiteResult struct {
	// Skipped is true when the target repository has no test entrypoint. The
	// merge continues UNTESTED, which is why the runner logs it loudly and why
	// Reason must name the absence concretely.
	Skipped bool
	// Reason explains a Skipped result. Empty otherwise.
	Reason string
	// Passed is the suite's verdict. Meaningless when Skipped.
	Passed bool
	// Tail is the clamped tail of the run's combined output on a failure. It is
	// what the resolution prompt and the merge_failed cause carry, so it is the
	// only thing a user may ever see about why the merge was refused.
	Tail string
}

// SuiteRunner runs the target repository's test suite in the target worktree.
//
// It is a port rather than a direct call because the merge subsystem serves
// repositories that have no agent-repl suite (and, in tests, no suite at all):
// what counts as "the suite" is a property of the target repository, not of the
// merge machinery.
type SuiteRunner interface {
	// RunSuite runs the suite for the repository containing targetDir and
	// reports its verdict.
	//
	// A returned error means the run could not be CLASSIFIED (the entrypoint
	// could not be resolved, the process could not be spawned). A suite that
	// ran and failed is (SuiteResult{Passed: false}, nil) — a verdict, not an
	// error.
	RunSuite(ctx context.Context, targetDir string) (SuiteResult, error)
}

// suiteEntrypoints are the repository-relative test entrypoints the production
// runner looks for, most current first. The claude-repl spelling is the
// pre-rename module path, which sibling worktrees of this repository still
// carry.
var suiteEntrypoints = []string{
	"modules/app/agent-repl/bin/test-all.sh",
	"modules/app/claude-repl/bin/test-all.sh",
}

// suiteTailBytes is how much of a failing run's output is retained. It is a
// TAIL because a test runner's verdict is at the end of its output, and it is
// clamped because the string travels into a prompt and into the merge_failed
// cause the frontends render.
const suiteTailBytes = 4000

// RepoSuiteRunner is the production SuiteRunner: it resolves the target
// repository's toplevel and runs whichever entrypoint that repository declares.
type RepoSuiteRunner struct {
	logf dlog.Logf
}

var _ SuiteRunner = (*RepoSuiteRunner)(nil)

// NewRepoSuiteRunner validates its dependency and returns the runner. A nil
// logger is a hard construction error: a runner that cannot say it SKIPPED the
// gate would let a merge land untested in silence.
func NewRepoSuiteRunner(logf dlog.Logf) (*RepoSuiteRunner, error) {
	if logf == nil {
		return nil, fmt.Errorf("merge: RepoSuiteRunner Logf is required")
	}
	return &RepoSuiteRunner{logf: logf}, nil
}

// RunSuite implements SuiteRunner.
//
// The entrypoint is resolved against the target repository's TOPLEVEL rather
// than against targetDir, because targetDir is a worktree directory that may be
// any depth inside the checkout, while the suite is declared once per
// repository.
func (r *RepoSuiteRunner) RunSuite(ctx context.Context, targetDir string) (SuiteResult, error) {
	if targetDir == "" {
		return SuiteResult{}, fmt.Errorf("merge: RunSuite needs a target directory")
	}
	top, err := r.toplevel(ctx, targetDir)
	if err != nil {
		return SuiteResult{}, err
	}
	entry, rel := r.entrypoint(top)
	if entry == "" {
		reason := fmt.Sprintf("no test entrypoint in %s (looked for %v)", top, suiteEntrypoints)
		r.logf("merge: suite SKIPPED {target=%s toplevel=%s}: %s — this repository declares NO test suite, so the merge proceeds UNTESTED",
			targetDir, top, reason)
		return SuiteResult{Skipped: true, Reason: reason}, nil
	}

	r.logf("merge: suite RUNNING {target=%s toplevel=%s entrypoint=%s}", targetDir, top, rel)
	cmd := exec.CommandContext(ctx, entry)
	cmd.Dir = top
	// The daemon may be running under a git hook, whose exported repository
	// bindings would point the suite's own git at the WRONG repository. The
	// same strip the daemon's git goes through applies here.
	cmd.Env = gitexec.StripEnv(os.Environ())
	var out bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &out
	runErr := cmd.Run()
	if runErr == nil {
		r.logf("merge: suite PASSED {target=%s entrypoint=%s}", targetDir, rel)
		return SuiteResult{Passed: true}, nil
	}
	var exitErr *exec.ExitError
	if !errors.As(runErr, &exitErr) {
		// The suite could not be RUN. That is not a verdict, so it is surfaced
		// as an error rather than reported as a failing suite.
		return SuiteResult{}, fmt.Errorf("merge: run test suite %s in %s: %w", rel, top, runErr)
	}
	tail := tailOf(out.String(), suiteTailBytes)
	r.logf("merge: suite FAILED {target=%s entrypoint=%s exit=%d} tail:\n%s", targetDir, rel, exitErr.ExitCode(), tail)
	return SuiteResult{Passed: false, Tail: tail}, nil
}

// toplevel resolves the repository root that owns dir.
func (r *RepoSuiteRunner) toplevel(ctx context.Context, dir string) (string, error) {
	cmd := gitexec.Command(ctx, dir, "rev-parse", "--show-toplevel")
	var out, errb bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errb
	if err := cmd.Run(); err != nil {
		return "", fmt.Errorf("merge: resolve the test-suite toplevel of %s: %w (stderr: %s)",
			dir, err, dlog.Clamp(errb.String(), 400))
	}
	return strings.TrimSpace(out.String()), nil
}

// entrypoint returns the absolute path of the first declared entrypoint that
// exists and is executable under top, plus its repository-relative spelling. It
// returns ("", "") when the repository declares none.
func (r *RepoSuiteRunner) entrypoint(top string) (string, string) {
	for _, rel := range suiteEntrypoints {
		abs := filepath.Join(top, rel)
		info, err := os.Stat(abs)
		if err != nil || info.IsDir() || info.Mode().Perm()&0o111 == 0 {
			continue
		}
		return abs, rel
	}
	return "", ""
}

// tailOf returns the last max bytes of s, prefixed with an elision marker when
// anything was dropped. Unlike dlog.Clamp (which keeps the HEAD of a string), a
// test runner's verdict lives at the END of its output.
func tailOf(s string, max int) string {
	if len(s) <= max {
		return s
	}
	return "... (output truncated to the last " + fmt.Sprint(max) + " bytes) ...\n" + s[len(s)-max:]
}
