package merge

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"regexp"
	"strings"

	"claude-repld/internal/dlog"
)

// Config constructs an Driver. Both fields are required: a merge driver that
// cannot record state or log its git work is a silent-fallback risk, so a
// nil either is a hard construction error rather than a defaulted logger.
type Config struct {
	// Logf receives the driver's git-driver and transition logging. Required.
	Logf dlog.Logf
	// Sink receives every merge-state transition. Bound to the SSM at stitch.
	// Required.
	Sink StateSink
}

// Driver runs cherry-pick merges via `git -C <dir>` and emits every
// resulting state transition through its StateSink. It holds no per-merge
// state; each Merge / Resume call is self-contained and drives one request.
type Driver struct {
	logf dlog.Logf
	emit *stateEmitter
}

// NewDriver validates cfg and returns the driver, or an error when a
// required dependency is missing.
func NewDriver(cfg Config) (*Driver, error) {
	if cfg.Logf == nil {
		return nil, fmt.Errorf("merge: Logf is required")
	}
	if cfg.Sink == nil {
		return nil, fmt.Errorf("merge: Sink is required")
	}
	return &Driver{
		logf: cfg.Logf,
		emit: &stateEmitter{sink: cfg.Sink, logf: cfg.Logf},
	}, nil
}

// Request describes one cherry-pick merge: replay the commits unique to
// SourceBranch (since it diverged from the target) onto the target worktree
// at TargetDir. In the real system SourceDir and TargetDir are sibling
// worktrees of the same repo (shared object store and refs), which is why
// TargetDir can resolve SourceBranch by name.
type Request struct {
	// Workspace is the daemon's WORKSPACE KEY — the session cwd, the same
	// string every other axis of the SSM files its rows under. The merge state
	// transitions are emitted on it, so a merge's rows join the composite the
	// session already populates.
	//
	// It used to be the bare Emacs workspace name, which filed merge rows under
	// a key nothing else used. Such a key has no session_connectivity history,
	// so its WorkspaceState composited to an ABSENT connectivity verdict, and
	// Emacs's (correctly strict) resolver refused the frame — the merge landed
	// on disk and its workspace was never torn down.
	Workspace string
	// Name is the workspace's DISPLAY name, used for the merge/<name>
	// completion tag and for logs. Never a state key: a path makes a wretched
	// git tag, which is why the two identities are separate fields.
	Name string
	// SourceBranch is the branch whose commits are cherry-picked (the right
	// side of the HEAD...branch range computed in TargetDir).
	SourceBranch string
	// SourceDir is the source workspace's worktree. Its cleanliness is the
	// merge precondition: uncommitted changes there would be silently dropped
	// (they belong to no commit and never get cherry-picked).
	SourceDir string
	// TargetDir is the worktree the cherry-pick lands in (git -C TargetDir).
	TargetDir string
}

func (r Request) validate() error {
	switch {
	case r.Workspace == "":
		return fmt.Errorf("merge: request Workspace is required")
	case r.Name == "":
		return fmt.Errorf("merge: request Name is required")
	case r.SourceBranch == "":
		return fmt.Errorf("merge: request SourceBranch is required")
	case r.SourceDir == "":
		return fmt.Errorf("merge: request SourceDir is required")
	case r.TargetDir == "":
		return fmt.Errorf("merge: request TargetDir is required")
	}
	return nil
}

// Outcome is the terminal classification of a Merge / Resume call.
type Outcome string

const (
	// OutcomeMerged: the work landed (or was already incorporated).
	OutcomeMerged Outcome = "merged"
	// OutcomeConflict: a conflict is left in the target tree awaiting a human
	// resolve + Resume.
	OutcomeConflict Outcome = "conflict"
	// OutcomeFailed: git aborted the pick with no conflict to resolve.
	OutcomeFailed Outcome = "failed"
)

// Result is what a Merge / Resume call reports back to the caller (the stitch
// orchestrator). It mirrors the emitted state but is returned so the caller
// can act (e.g. tell Emacs to open magit on a conflict) without re-querying
// the SSM.
type Result struct {
	Outcome Outcome
	// ConflictCommit is the short SHA of the conflicting commit
	// (CHERRY_PICK_HEAD) when Outcome is OutcomeConflict.
	ConflictCommit string
	// AlreadyIncorporated is true when Outcome is OutcomeMerged but the merge
	// was a no-op (the range was empty or every patch was already present).
	AlreadyIncorporated bool
	// Tag is the merge/<ws> tag written on a merged outcome.
	Tag string
}

// cherryPickAnnotationRE matches the "(cherry picked from commit <sha>)"
// annotation that `git cherry-pick -x` writes, mirroring
// agent-repl--extract-cherry-pick-shas.
var cherryPickAnnotationRE = regexp.MustCompile(`\(cherry picked from commit ([0-9a-f]{40})\)`)

// Merge runs the cherry-pick driver for req against a clean precondition and
// returns the terminal Result. It emits (in order): merging, then exactly one
// of merged / merge_conflict / merge_failed.
//
// A failed precondition (bad request, dirty source worktree, missing branch)
// aborts BEFORE any transition is emitted, so a rejected merge leaves the
// workspace's state exactly as it was — no half-transition.
//
// A conflict is LEFT IN THE TARGET TREE (never aborted); the caller resumes
// it via Resume once a human has resolved it.
func (e *Driver) Merge(ctx context.Context, req Request) (Result, error) {
	if err := req.validate(); err != nil {
		return Result{}, err
	}
	e.logf("merge: Merge start {ws=%s key=%s branch=%s source=%s target=%s}",
		req.Name, req.Workspace, req.SourceBranch, req.SourceDir, req.TargetDir)

	// Preconditions run before the first transition so a rejection leaves
	// state intact. Uncommitted changes in the source worktree would be
	// silently lost by the merge (they belong to no commit), so they abort it.
	if err := e.assertCleanWorktree(ctx, req.SourceDir); err != nil {
		return Result{}, fmt.Errorf("merge: precondition for %q: %w", req.Name, err)
	}
	exists, err := e.branchExists(ctx, req.TargetDir, req.SourceBranch)
	if err != nil {
		return Result{}, err
	}
	if !exists {
		return Result{}, fmt.Errorf("merge: branch %q not found in %s", req.SourceBranch, req.TargetDir)
	}

	if err := e.emit.emit(req.Workspace, PhaseMerging, "cherry-pick starting for "+req.SourceBranch); err != nil {
		return Result{}, err
	}

	base, err := e.cherryPickBase(ctx, req.TargetDir, req.SourceBranch)
	if err != nil {
		return Result{}, err
	}
	rng := base + ".." + req.SourceBranch

	// Empty SHA range: the workspace's contribution is already on the target
	// by ancestry — a successful no-op merge.
	count, err := e.gitString(ctx, req.TargetDir, "rev-list", "--count", rng)
	if err != nil {
		return Result{}, err
	}
	if count == "0" {
		e.logf("merge: range %s empty — already incorporated {ws=%s}", rng, req.Name)
		return e.finalizeMerged(ctx, req, true)
	}

	// Drive the pick. A non-zero exit is not a spawn error; the exit code and
	// the presence of CHERRY_PICK_HEAD classify the outcome.
	exit, out, err := e.gitExit(ctx, req.TargetDir, "cherry-pick", "-x", rng)
	if err != nil {
		return Result{}, err
	}
	e.logf("merge: cherry-pick -x %s exit=%d {ws=%s} %s", rng, exit, req.Name, dlog.Clamp(out, 400))

	inProgress, err := e.cherryPickInProgress(ctx, req.TargetDir)
	if err != nil {
		return Result{}, err
	}
	if inProgress {
		return e.markConflict(ctx, req)
	}
	if exit == 0 {
		return e.finalizeMerged(ctx, req, false)
	}

	// Non-zero exit, no CHERRY_PICK_HEAD. Distinguish the ALREADY-MERGED case
	// (a prior `cherry-pick -x` rewrote our commits under new SHAs, so the
	// SHA-keyed range probe missed them and the pick ran only to go empty)
	// from a genuine failure via `git cherry` (patch-id comparison).
	incorporated, err := e.rangeAlreadyIncorporated(ctx, req.TargetDir, base, req.SourceBranch)
	if err != nil {
		return Result{}, err
	}
	if incorporated {
		e.logf("merge: range %s already incorporated by patch-id {ws=%s}", rng, req.Name)
		return e.finalizeMerged(ctx, req, true)
	}
	return e.markFailed(ctx, req, fmt.Sprintf("cherry-pick exited %d with no conflict to resolve", exit))
}

// Resume continues a cherry-pick that a human has resolved in the target
// worktree (the resolve-and-continue handoff arrives as a FrontendCommand
// with conflict_resolved_continue at stitch). It stages the resolved files
// (`git add -u`) and runs `git cherry-pick --continue`, mirroring
// agent-repl--continue-cherry-pick-after-resolve.
//
// It emits merging (the conflict is clearing), then exactly one of merged /
// merge_conflict (another commit in the range conflicted) / merge_failed.
func (e *Driver) Resume(ctx context.Context, req Request) (Result, error) {
	if err := req.validate(); err != nil {
		return Result{}, err
	}
	e.logf("merge: Resume start {ws=%s key=%s target=%s}", req.Name, req.Workspace, req.TargetDir)

	inProgress, err := e.cherryPickInProgress(ctx, req.TargetDir)
	if err != nil {
		return Result{}, err
	}
	if !inProgress {
		// No paused cherry-pick to continue: the caller's premise (a resolved
		// conflict awaiting continue) is false. Fail loudly rather than
		// silently report success.
		return Result{}, fmt.Errorf("merge: no cherry-pick in progress in %s — nothing to resume for %q",
			req.TargetDir, req.Name)
	}

	if err := e.emit.emit(req.Workspace, PhaseMerging, "resuming cherry-pick after resolve"); err != nil {
		return Result{}, err
	}

	if exit, out, err := e.gitExit(ctx, req.TargetDir, "add", "-u"); err != nil {
		return Result{}, err
	} else if exit != 0 {
		return Result{}, fmt.Errorf("merge: `git add -u` exited %d in %s: %s", exit, req.TargetDir, dlog.Clamp(out, 400))
	}

	// -c core.editor=true + --no-edit keep the original commit message
	// (including the -x annotation) without opening $EDITOR in a headless run.
	exit, out, err := e.gitExit(ctx, req.TargetDir,
		"-c", "core.editor=true", "cherry-pick", "--continue", "--no-edit")
	if err != nil {
		return Result{}, err
	}
	e.logf("merge: cherry-pick --continue exit=%d {ws=%s} %s", exit, req.Name, dlog.Clamp(out, 400))

	stillInProgress, err := e.cherryPickInProgress(ctx, req.TargetDir)
	if err != nil {
		return Result{}, err
	}
	if stillInProgress {
		return e.markConflict(ctx, req)
	}
	if exit == 0 {
		return e.finalizeMerged(ctx, req, false)
	}
	return e.markFailed(ctx, req, fmt.Sprintf("cherry-pick --continue exited %d with no conflict to resolve", exit))
}

// finalizeMerged is the shared "the target now carries this work" tail:
// tag the completion (merge/<ws>) and emit merged. Ported from
// agent-repl--tag-merge-completion + the :merged tail of
// agent-repl--workspace-merge-do. A tag-write failure is non-fatal (warned,
// not signaled) exactly as in the elisp: the cherry-pick already landed, so a
// tag failure must not undo it.
func (e *Driver) finalizeMerged(ctx context.Context, req Request, alreadyIncorporated bool) (Result, error) {
	tag := "merge/" + req.Name
	exit, out, err := e.gitExit(ctx, req.TargetDir, "tag", "-f", tag, "HEAD")
	if err != nil {
		return Result{}, err
	}
	if exit != 0 {
		e.logf("merge: WARNING tag %s failed (exit=%d) — merge already landed, not reverting {ws=%s} %s",
			tag, exit, req.Name, dlog.Clamp(out, 200))
	} else {
		e.logf("merge: tagged completion %s {ws=%s}", tag, req.Name)
	}
	cause := "cherry-pick landed on target"
	if alreadyIncorporated {
		cause = "range already incorporated (no-op merge)"
	}
	if err := e.emit.emit(req.Workspace, PhaseMerged, cause); err != nil {
		return Result{}, err
	}
	return Result{Outcome: OutcomeMerged, AlreadyIncorporated: alreadyIncorporated, Tag: tag}, nil
}

// markConflict emits merge_conflict for a pick left paused in the target
// tree. It never aborts the cherry-pick: the conflict stays in-tree so a
// human can resolve it and Resume can continue it.
func (e *Driver) markConflict(ctx context.Context, req Request) (Result, error) {
	short, err := e.gitString(ctx, req.TargetDir, "rev-parse", "--short", "CHERRY_PICK_HEAD")
	if err != nil {
		return Result{}, err
	}
	if err := e.emit.emit(req.Workspace, PhaseMergeConflict,
		"conflict cherry-picking "+short+" (left in tree for resolve)"); err != nil {
		return Result{}, err
	}
	return Result{Outcome: OutcomeConflict, ConflictCommit: short}, nil
}

// markFailed emits merge_failed for a pick git aborted with no conflict to
// resolve (the elisp silent-failure sentinel).
func (e *Driver) markFailed(_ context.Context, req Request, cause string) (Result, error) {
	if err := e.emit.emit(req.Workspace, PhaseMergeFailed, cause); err != nil {
		return Result{}, err
	}
	return Result{Outcome: OutcomeFailed}, nil
}

// MarkQueued emits merge_queued for a merge the stitch orchestrator defers
// because another cherry-pick is already in flight against the same target
// worktree. The queue and its drain live at stitch; this method only records
// the transition so it is never invisible.
func (e *Driver) MarkQueued(ws, cause string) error {
	return e.emit.emit(ws, PhaseMergeQueued, cause)
}

// cherryPickBase computes the cherry-pick start point for incorporating
// targetBranch into TargetDir's HEAD, mirroring agent-repl--cherry-pick-base:
// scan HEAD's unique commits for -x annotations and return the most recent
// targetBranch commit already incorporated; otherwise fall back to the
// merge-base. Unlike the elisp (which swallows git failures into an empty
// string), a git failure here is surfaced as an error.
func (e *Driver) cherryPickBase(ctx context.Context, dir, targetBranch string) (string, error) {
	symmetric := "HEAD..." + targetBranch
	rightOut, err := e.gitString(ctx, dir, "log", "--right-only", "--pretty=%H", "--no-merges", symmetric)
	if err != nil {
		return "", err
	}
	targetCommits := splitLines(rightOut)

	leftOut, err := e.gitString(ctx, dir, "log", "--left-only", "--pretty=%B", symmetric)
	if err != nil {
		return "", err
	}
	incorporated := extractCherryPickSHAs(leftOut)

	// git log lists newest first, so the first match is the most recent
	// targetBranch commit already incorporated.
	for _, sha := range targetCommits {
		if incorporated[sha] {
			e.logf("merge: cherry-pick-base resolved to incorporated %s {branch=%s}", sha, targetBranch)
			return sha, nil
		}
	}
	mb, err := e.gitString(ctx, dir, "merge-base", "HEAD", targetBranch)
	if err != nil {
		return "", err
	}
	e.logf("merge: cherry-pick-base fell to merge-base %s {branch=%s}", mb, targetBranch)
	return mb, nil
}

// rangeAlreadyIncorporated reports whether every commit in base..targetBranch
// is already present on dir's HEAD by patch-id, mirroring
// agent-repl--range-already-incorporated-p. `git cherry` prints one line per
// range commit: `-` when an equivalent already exists on HEAD, `+` when it is
// genuinely new. All `-` means the range fully landed (a no-op merge); any
// `+` is real un-applied work. A blank/error output is NOT treated as
// incorporated, so a probe failure never masquerades as an already-merged
// success.
func (e *Driver) rangeAlreadyIncorporated(ctx context.Context, dir, base, targetBranch string) (bool, error) {
	out, err := e.gitString(ctx, dir, "cherry", "HEAD", targetBranch, base)
	if err != nil {
		return false, err
	}
	lines := splitLines(out)
	if len(lines) == 0 {
		return false, nil
	}
	for _, line := range lines {
		if !strings.HasPrefix(strings.TrimLeft(line, " "), "-") {
			return false, nil
		}
	}
	return true, nil
}

// cherryPickInProgress reports whether a cherry-pick is paused in dir,
// mirroring agent-repl--cherry-pick-in-progress-p: CHERRY_PICK_HEAD resolves
// as a rev when present. `rev-parse --verify --quiet` exits non-zero (with no
// stderr) when it is absent, which is the normal not-in-progress signal
// rather than an error.
func (e *Driver) cherryPickInProgress(ctx context.Context, dir string) (bool, error) {
	exit, _, err := e.gitExit(ctx, dir, "rev-parse", "--verify", "--quiet", "CHERRY_PICK_HEAD")
	if err != nil {
		return false, err
	}
	return exit == 0, nil
}

// branchExists mirrors agent-repl--git-branch-exists-p.
func (e *Driver) branchExists(ctx context.Context, dir, branch string) (bool, error) {
	exit, _, err := e.gitExit(ctx, dir, "rev-parse", "--verify", "--quiet", "refs/heads/"+branch)
	if err != nil {
		return false, err
	}
	return exit == 0, nil
}

// assertCleanWorktree signals an error when dir has uncommitted changes,
// mirroring agent-repl--assert-clean-worktree (`diff --quiet` +
// `diff --cached --quiet`). A non-zero exit from either means dirty.
func (e *Driver) assertCleanWorktree(ctx context.Context, dir string) error {
	unstaged, _, err := e.gitExit(ctx, dir, "diff", "--quiet")
	if err != nil {
		return err
	}
	staged, _, err := e.gitExit(ctx, dir, "diff", "--cached", "--quiet")
	if err != nil {
		return err
	}
	if unstaged != 0 || staged != 0 {
		return fmt.Errorf("uncommitted changes in %s (unstaged=%t staged=%t) — commit or stash before merging",
			dir, unstaged != 0, staged != 0)
	}
	return nil
}

// gitCmd builds every merge.Driver git invocation. The repository is ALWAYS
// the `-C dir` argument, so the environment's repository bindings are
// stripped: a daemon launched from a Git hook inherits GIT_DIR /
// GIT_INDEX_FILE / GIT_WORK_TREE pointing at the HOOK'S repository, and an
// inherited binding would silently retarget this cherry-pick at that repo
// instead of dir's. That is not a hypothetical — the pre-commit hook runs
// this very suite, and the leak both hung the merge e2e tests and is the kind
// of misdirected git write that can flip core.bare on a live checkout.
func gitCmd(ctx context.Context, dir string, args ...string) *exec.Cmd {
	full := append([]string{"-C", dir}, args...)
	cmd := exec.CommandContext(ctx, "git", full...)
	env := os.Environ()
	kept := env[:0]
	for _, entry := range env {
		switch {
		case strings.HasPrefix(entry, "GIT_DIR="),
			strings.HasPrefix(entry, "GIT_INDEX_FILE="),
			strings.HasPrefix(entry, "GIT_WORK_TREE="),
			strings.HasPrefix(entry, "GIT_OBJECT_DIRECTORY="),
			strings.HasPrefix(entry, "GIT_COMMON_DIR="),
			strings.HasPrefix(entry, "GIT_PREFIX="):
		default:
			kept = append(kept, entry)
		}
	}
	cmd.Env = kept
	return cmd
}

// gitExit runs `git -C dir args...` and returns the exit code plus combined
// output. A non-zero exit is NOT an error (the caller classifies it); only a
// failure to spawn/run git (binary missing, dir gone) returns an error.
// Mirrors the exit-code role of agent-repl--git-exit-code.
func (e *Driver) gitExit(ctx context.Context, dir string, args ...string) (int, string, error) {
	cmd := gitCmd(ctx, dir, args...)
	var out bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &out
	err := cmd.Run()
	if err != nil {
		var ee *exec.ExitError
		if errors.As(err, &ee) {
			return ee.ExitCode(), out.String(), nil
		}
		return -1, out.String(), fmt.Errorf("merge: git %v in %s: %w", args, dir, err)
	}
	return 0, out.String(), nil
}

// gitString runs `git -C dir args...` and returns trimmed stdout. Unlike the
// elisp agent-repl--git-string (which returns whatever stdout was, even on a
// non-zero exit — a silent-fallback risk), this treats a non-zero exit as an
// error so a git failure in base/range computation aborts loudly.
func (e *Driver) gitString(ctx context.Context, dir string, args ...string) (string, error) {
	cmd := gitCmd(ctx, dir, args...)
	var out, errb bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errb
	if err := cmd.Run(); err != nil {
		return "", fmt.Errorf("merge: git %v in %s: %w (stderr: %s)", args, dir, err, dlog.Clamp(errb.String(), 400))
	}
	return strings.TrimSpace(out.String()), nil
}

// splitLines splits on newlines and drops empty fields, mirroring the elisp
// (split-string ... "\n" t) used throughout the merge helpers.
func splitLines(s string) []string {
	var lines []string
	for _, line := range strings.Split(s, "\n") {
		if strings.TrimSpace(line) != "" {
			lines = append(lines, strings.TrimSpace(line))
		}
	}
	return lines
}

// extractCherryPickSHAs returns the set of SHAs from "(cherry picked from
// commit <sha>)" annotations in logText, mirroring
// agent-repl--extract-cherry-pick-shas.
func extractCherryPickSHAs(logText string) map[string]bool {
	shas := map[string]bool{}
	for _, m := range cherryPickAnnotationRE.FindAllStringSubmatch(logText, -1) {
		shas[m[1]] = true
	}
	return shas
}
