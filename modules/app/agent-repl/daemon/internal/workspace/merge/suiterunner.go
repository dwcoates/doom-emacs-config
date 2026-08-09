package merge

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"

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
	// OutputPath names the file holding the failing run's COMPLETE output.
	//
	// THE TAIL ALONE CANNOT ANSWER "WHICH TEST FAILED". A multi-suite runner
	// keeps going after a suite fails, so by the time it exits, the failure's
	// own output is thousands of lines back and the retained tail is whatever
	// the LAST suites printed — coverage tables, timing summaries. The verdict
	// survives, the diagnosis does not. Archiving the whole run and naming the
	// file is what makes the failure reconstructible after the fact rather than
	// only re-runnable.
	//
	// It is empty when the run passed on its first attempt, when the run was
	// skipped, and when the run could not be classified at all. It is NEVER
	// empty for a run that produced a verdict worth keeping: the file is the
	// child's stdout, so it exists before the suite is even spawned, and a
	// failure to create it is an unrunnable suite rather than a lost archive.
	//
	// A RE-RUN IS ARCHIVED WHATEVER ITS VERDICT (SuiteRun.Attempt > 1), because a
	// flake's evidence is the PAIR: the failing run's output alone cannot show
	// what the passing one did differently, and the passing one is exactly the
	// run nothing would otherwise keep.
	OutputPath string
	// Duration is how long the run took. It is what makes a flake report legible
	// — a suite that failed in 4s and passed in 90s on the same tree is a
	// different story from one where the two runs took the same time.
	Duration time.Duration
}

// SuiteRun is one request to run the target repository's suite.
type SuiteRun struct {
	// Suites narrows the run to these suite names, which the entrypoint receives
	// as `--suites a,b,c`. EMPTY MEANS EVERY SUITE, which is the entrypoint's own
	// default and the answer for any repository whose paths the selection does
	// not recognize — so a foreign repository, whose entrypoint may know no such
	// flag, is never handed one.
	Suites []string
	// Attempt is 1 for the gate's first run and 2 for its single re-run. It is
	// carried rather than inferred because the runner's archiving policy depends
	// on it: see SuiteResult.OutputPath.
	Attempt int
}

func (r SuiteRun) validate() error {
	if r.Attempt < 1 {
		return fmt.Errorf("merge: SuiteRun Attempt must be 1 or more, got %d", r.Attempt)
	}
	return validateSuiteNames(r.Suites)
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
	RunSuite(ctx context.Context, targetDir string, run SuiteRun) (SuiteResult, error)
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
func (r *RepoSuiteRunner) RunSuite(ctx context.Context, targetDir string, run SuiteRun) (SuiteResult, error) {
	if targetDir == "" {
		return SuiteResult{}, fmt.Errorf("merge: RunSuite needs a target directory")
	}
	if err := run.validate(); err != nil {
		return SuiteResult{}, err
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

	args := suiteArgs(run.Suites)
	// THE OUTPUT FILE IS CREATED BEFORE THE SPAWN because it IS the child's
	// stdout; see spawnSuite. A suite that has nowhere to write has not run, so
	// this failure is an unrunnable suite — loudly, before anything is logged as
	// RUNNING — rather than the "archive failed, keep the verdict" degradation
	// this used to have after the fact.
	out, err := os.CreateTemp("", suiteOutputPattern)
	if err != nil {
		return SuiteResult{}, fmt.Errorf("merge: create the output file for test suite %s in %s: %w", rel, top, err)
	}
	outPath := out.Name()

	r.logf("merge: suite RUNNING {target=%s toplevel=%s entrypoint=%s attempt=%d suites=%s}",
		targetDir, top, rel, run.Attempt, selectionLabel(run.Suites))
	started := time.Now()
	runErr := spawnSuite(ctx, top, entry, args, out)
	elapsed := time.Since(started)

	// The child is gone; the file is complete. Everything below reads it back.
	var outNote string
	if err := out.Close(); err != nil {
		r.logf("merge: suite output CLOSE FAILED {target=%s entrypoint=%s file=%s}: %v — the retained output may be short",
			targetDir, rel, outPath, err)
		outNote += fmt.Sprintf("\n[merge] the output file could NOT be closed cleanly: %v\n", err)
	}

	if runErr == nil {
		res := SuiteResult{Passed: true, Tail: outNote, Duration: elapsed}
		if run.Attempt > 1 {
			// The re-run's output is the half of a flake's evidence nothing else
			// keeps; see SuiteResult.OutputPath.
			res.OutputPath = outPath
		} else {
			r.discardOutput(targetDir, rel, outPath)
		}
		r.logf("merge: suite PASSED {target=%s entrypoint=%s attempt=%d suites=%s duration=%s full_output=%s}",
			targetDir, rel, run.Attempt, selectionLabel(run.Suites), elapsed.Round(time.Millisecond), res.OutputPath)
		return res, nil
	}
	var exitErr *exec.ExitError
	if !errors.As(runErr, &exitErr) {
		// The suite could not be RUN. That is not a verdict, so it is surfaced
		// as an error rather than reported as a failing suite, and the empty
		// output it produced is not worth keeping.
		r.discardOutput(targetDir, rel, outPath)
		return SuiteResult{}, fmt.Errorf("merge: run test suite %s in %s: %w", rel, top, runErr)
	}
	tail := r.tail(targetDir, rel, outPath) + outNote
	r.logf("merge: suite FAILED {target=%s entrypoint=%s attempt=%d suites=%s exit=%d duration=%s full_output=%s} tail:\n%s",
		targetDir, rel, run.Attempt, selectionLabel(run.Suites), exitErr.ExitCode(), elapsed.Round(time.Millisecond), outPath, tail)
	return SuiteResult{Passed: false, Tail: tail, OutputPath: outPath, Duration: elapsed}, nil
}

// suiteWaitDelay bounds how long Wait may linger AFTER the suite process itself
// has exited or been cancelled. See spawnSuite for why it is belt-and-braces
// rather than the fix.
const suiteWaitDelay = 5 * time.Second

// spawnSuite runs the entrypoint with out as BOTH its stdout and its stderr,
// and returns cmd.Run's error unchanged.
//
// WHY THE OUTPUT GOES TO AN *os.File AND NEVER TO A bytes.Buffer. This used to
// hand os/exec an in-memory buffer. os/exec cannot give a child an arbitrary
// io.Writer, so it manufactures an os.Pipe and a goroutine that copies the read
// end into the buffer — and Wait does not return until that copy reaches EOF.
// EOF requires EVERY holder of the WRITE end to close it, and the suite's own
// children inherit that fd: one leaked background daemon or shim that outlives
// the entrypoint keeps the pipe open forever. The observed production shape was
// exactly that — the suite process gone from the process table, no PASSED, no
// FAILED, no error, and the merge queue head held until the daemon was bounced.
//
// An *os.File has no such intermediary: os/exec dup2s the descriptor straight
// into the child, there is no copying goroutine, and Wait returns the moment
// the CHILD is reaped no matter what its descendants inherited. The hang is not
// made less likely, it is made unrepresentable.
//
// WaitDelay is the belt to that braces. It does nothing on this path (there is
// no pipe to outlive the process), and it exists so that a future
// StdoutPipe/StderrPipe reintroduced here degrades into a bounded wait and a
// loud ErrWaitDelay instead of returning to the silent wedge.
func spawnSuite(ctx context.Context, top, entry string, args []string, out *os.File) error {
	cmd := exec.CommandContext(ctx, entry, args...)
	cmd.Dir = top
	// The daemon may be running under a git hook, whose exported repository
	// bindings would point the suite's own git at the WRONG repository. The
	// same strip the daemon's git goes through applies here.
	cmd.Env = gitexec.StripEnv(os.Environ())
	cmd.Stdout = out
	cmd.Stderr = out
	cmd.WaitDelay = suiteWaitDelay
	return cmd.Run()
}

// tail reads the clamped tail of the run's output back off the file the suite
// wrote it to, reporting an unreadable file where the run itself is read.
//
// The verdict is real and must stand; failing the gate because a diagnostic
// file could not be read back would turn a test failure into an unrunnable
// suite. So the tail's absence is surfaced in the tail, never dropped.
func (r *RepoSuiteRunner) tail(targetDir, rel, path string) string {
	tail, err := readSuiteTail(path, suiteTailBytes)
	if err == nil {
		return tail
	}
	r.logf("merge: suite output READ FAILED {target=%s entrypoint=%s file=%s}: %v — the verdict stands but its tail is lost",
		targetDir, rel, path, err)
	return fmt.Sprintf("\n[merge] the run's output could NOT be read back from %s: %v\n", path, err)
}

// discardOutput removes the output of a run nothing needs to reconstruct. A
// removal that fails leaks a temp file rather than a verdict, so it is logged
// and not returned.
func (r *RepoSuiteRunner) discardOutput(targetDir, rel, path string) {
	if err := os.Remove(path); err != nil {
		r.logf("merge: suite output DISCARD FAILED {target=%s entrypoint=%s file=%s}: %v",
			targetDir, rel, path, err)
	}
}

// suiteArgs renders a selection as the entrypoint's arguments. An empty
// selection passes NO arguments at all, which is both the entrypoint's own
// "everything" default and the only shape a foreign repository's entrypoint is
// guaranteed to accept.
func suiteArgs(suites []string) []string {
	if len(suites) == 0 {
		return nil
	}
	return []string{"--suites", strings.Join(suites, ",")}
}

// selectionLabel names a selection for the log.
func selectionLabel(suites []string) string {
	if len(suites) == 0 {
		return "ALL"
	}
	return strings.Join(suites, ",")
}

// suiteOutputPattern names the file one suite run writes its output to, and
// which becomes that run's archive when the run is worth keeping. It lands in
// the process temp directory, which is where the daemon already keeps the
// per-workspace logs a workspace's .claude/emacs symlinks point at.
const suiteOutputPattern = "agent-repl-merge-suite-*.log"

// readSuiteTail returns the last max bytes of the run's output file, in exactly
// the shape tailOf gives an in-memory string. It SEEKS rather than reading the
// whole file, because the file is the complete output of a full suite run and
// only its end is ever wanted.
func readSuiteTail(path string, max int) (string, error) {
	f, err := os.Open(path)
	if err != nil {
		return "", fmt.Errorf("merge: open the suite output %s: %w", path, err)
	}
	defer f.Close()
	info, err := f.Stat()
	if err != nil {
		return "", fmt.Errorf("merge: stat the suite output %s: %w", path, err)
	}
	if info.Size() <= int64(max) {
		body, err := io.ReadAll(f)
		if err != nil {
			return "", fmt.Errorf("merge: read the suite output %s: %w", path, err)
		}
		return string(body), nil
	}
	if _, err := f.Seek(info.Size()-int64(max), io.SeekStart); err != nil {
		return "", fmt.Errorf("merge: seek to the tail of the suite output %s: %w", path, err)
	}
	body := make([]byte, max)
	if _, err := io.ReadFull(f, body); err != nil {
		return "", fmt.Errorf("merge: read the tail of the suite output %s: %w", path, err)
	}
	return truncationNotice(max) + string(body), nil
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
	return truncationNotice(max) + s[len(s)-max:]
}

// truncationNotice is the elision marker a clamped tail carries. It is one
// function so that a tail read off a file and a tail taken from a string cannot
// drift into two different shapes.
func truncationNotice(max int) string {
	return "... (output truncated to the last " + fmt.Sprint(max) + " bytes) ...\n"
}
