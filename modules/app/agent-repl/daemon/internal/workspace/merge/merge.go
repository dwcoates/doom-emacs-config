package merge

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/gitexec"
)

// Config constructs a merge.Driver. EVERY field is required: a merge driver
// that cannot record state, log its git work, or test what it lands is a
// silent-fallback risk, so a nil any of them is a hard construction error
// rather than a defaulted dependency.
type Config struct {
	// Logf receives the driver's git-driver and transition logging. Required.
	Logf dlog.Logf
	// Sink receives every merge-state transition. Bound to the SSM at stitch.
	// Required.
	Sink StateSink
	// Suite runs the target repository's test suite ONCE PER MERGE, on the fully
	// rebased head, immediately before the target moves. Required, on the same
	// footing as the rest: that gate is the only thing standing between a
	// workspace's branch and the tree every other workspace cuts from, and a
	// driver constructed without one would silently land untested work on the
	// target. A target repository with no test entrypoint is the RUNNER's
	// business to report (SuiteResult.Skipped), not a reason to omit the
	// dependency.
	Suite SuiteRunner
}

// Driver runs cherry-pick merges via `git -C <dir>` and emits every
// resulting state transition through its StateSink. It holds no per-merge
// state; each Merge / Resume call is self-contained and drives one request.
type Driver struct {
	logf  dlog.Logf
	emit  *stateEmitter
	suite SuiteRunner
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
	if cfg.Suite == nil {
		return nil, fmt.Errorf("merge: Suite runner is required")
	}
	return &Driver{
		logf:  cfg.Logf,
		emit:  &stateEmitter{sink: cfg.Sink, logf: cfg.Logf},
		suite: cfg.Suite,
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
	// Run publishes this RUN's phase-level MergeStatus. It is minted by
	// merge.Coordinator.Enqueue when the request is admitted, and is DELIBERATELY
	// NOT PART OF THE DURABLE QUEUE PAYLOAD (the queue persists its own entry
	// struct, field by field): it is a live publisher bound to this process's
	// sink, clock and logger, and none of that survives a bounce.
	//
	// It is nil only for a Request that has not reached the pipeline yet (an
	// Enqueue's validation, a queue round-trip). Every merge.Driver entry point
	// requires it, because a driver that could publish nothing would land commits
	// no frontend could watch.
	Run *RunStatus
	// RunID is the run's IDENTITY as RESTORED FROM THE DURABLE ENTRY, and it is
	// the half of a run that survives the process: the queue writes the id into
	// the entry file, so a boot replay resumes publishing under the id the user
	// has been watching since the admission rather than looking like an abandoned
	// merge followed by a new one.
	//
	// IT IS SET BY THE QUEUE AND BY NOTHING ELSE. While a publisher is alive, Run
	// is the sole authority on the id (runIdentity below reads it off there, which
	// is what the durable write records), so the two can never name different
	// runs. A run's PROGRESS is deliberately not persisted beside it — the commit
	// cursor of a dead process describes work the resumed one has not done — only
	// its name.
	RunID string
	// StatusWatermarkMs is the HIGHEST updated_at_ms the run had published when
	// the durable entry was last written, and it is the other half of a run that
	// survives the process.
	//
	// A resumed RunStatus seeds its clock from max(now, watermark+1). Without it
	// the resume re-seeds from now() alone, and a wall clock that stepped
	// BACKWARDS across the bounce (an ntp correction, a suspended laptop, a
	// container's clock settling) would publish a merge_status sorting beneath
	// the ones the pre-bounce process already sent — which is a receiver
	// ordering on updated_at_ms silently rendering stale progress.
	//
	// LIKE RunID, IT IS SET BY THE QUEUE AND BY NOTHING ELSE, and it is the
	// value the entry was HYDRATED with rather than a live figure. The live one
	// belongs to the RunStatus, which is the sole writer of the durable field.
	StatusWatermarkMs int64
	// WorkDir is the REBASE WORKTREE: a temporary linked worktree of the
	// target's repository, created by merge.Driver.Merge, in which the branch's
	// commits are replayed onto the target's head and gated by the suite. It is
	// NEITHER the user's workspace worktree (whose uncommitted state must stay
	// untouched) NOR the target checkout (which this pipeline modifies exactly
	// once, at the very end).
	//
	// IT IS DELIBERATELY NOT PART OF THE DURABLE QUEUE PAYLOAD. A temp worktree
	// does not survive the process that made it, so a boot replay starts a fresh
	// rebase rather than resuming into a directory that is gone.
	//
	// merge.Driver.Merge SETS it (on the Result it returns); merge.Coordinator
	// echoes it back into the Resume / ContinueAfterTestFix / Cleanup calls that
	// follow, which is how a resolution turn and its resume address the same tree.
	WorkDir string
	// BaseHead is the target's HEAD as it stood when the rebase based itself on
	// it, and it is THE GUARD ON THE ONE TARGET-MUTATING STEP: before the merge
	// commit is made, the target must still be exactly here.
	//
	// The window between reading it and using it is unbounded (a conflict
	// resolution and a test fix are both agent turns), and nothing this pipeline
	// holds keeps the target still meanwhile: the merge lease claims the
	// merging workspace's SESSION (internal/ssm/mergelease.go), and the
	// per-repository queue only serializes merges against each other, so a human
	// or another agent committing straight into the target checkout is a write
	// this subsystem neither excludes nor sees. Recording the head the rebase
	// based itself on, and refusing the merge unless the target is still on it,
	// is what turns "no one else should be writing here" into something the
	// pipeline can actually verify. A refusal is recoverable rather than fatal:
	// merge.Driver.Merge re-rebases ONCE onto the new head.
	BaseHead string
}

// runIdentity is the id this request's run publishes under: the live publisher's
// while there is one, and the id read back off the durable entry when there is
// not. ONE authority in each direction, so a request cannot carry two names.
func (r Request) runIdentity() string {
	if r.Run != nil {
		return r.Run.RunID()
	}
	return r.RunID
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

// validateRebase is the precondition of every driver entry point that works
// INSIDE an existing rebase worktree (Resume, ContinueAfterTestFix, the landing
// step). Such a call cannot make one: the tree it must address is the one a
// previous call parked a conflict or a test failure in, and a missing WorkDir
// means the caller lost track of it. Guessing at the target instead is exactly
// the target-mutating behavior this design removed, so it is a loud error.
func (r Request) validateRebase() error {
	if err := r.validateRun(); err != nil {
		return err
	}
	if r.WorkDir == "" {
		return fmt.Errorf("merge: request %q carries no rebase worktree; this step works in the temporary worktree a previous step created, and there is nothing to fall back to that would not modify the target", r.Name)
	}
	if r.BaseHead == "" {
		return fmt.Errorf("merge: request %q carries no BaseHead; the target head this rebase based itself on is what guards the one target-mutating step, and a merge that cannot check it must not make it", r.Name)
	}
	return nil
}

// validateRun is the merge.Driver's extra precondition: a request the driver is
// about to execute MUST carry the run it publishes through. It is separate from
// validate() because merge.Coordinator.Enqueue validates a request BEFORE any
// run exists — the run is minted when the entry is popped, not when it is
// published.
func (r Request) validateRun() error {
	if err := r.validate(); err != nil {
		return err
	}
	if r.Run == nil {
		return fmt.Errorf("merge: request %q carries no RunStatus; a merge that publishes no phase status lands commits no frontend can watch", r.Name)
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
	// OutcomeTestFailed: a commit landed on the REBASE WORKTREE's line and the
	// repository's test suite then failed there. It is NOT terminal at the
	// driver level — the coordinator drives one resolution attempt through the
	// workspace's own session and continues, or fails the merge. Either way the
	// target is never modified: nothing has reached it yet.
	OutcomeTestFailed Outcome = "test_failed"
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
	// WorkDir is the REBASE WORKTREE this call worked in, set on EVERY outcome
	// so merge.Coordinator holds it no matter how the merge later develops. It
	// is the tree a conflict is parked in, the tree a test fix is staged in, and
	// the tree the coordinator must clean up when the run ends — see
	// Request.WorkDir.
	WorkDir string
	// BaseHead is the target head the rebase based itself on, set on EVERY
	// outcome. It is the guard the single target-mutating step checks — see
	// Request.BaseHead.
	BaseHead string
	// FailingCommit is the short SHA the broken test gate is ATTRIBUTED to, set
	// when Outcome is OutcomeTestFailed. The gate runs once, on the fully rebased
	// head, so it is that head's sha — and it is deliberately STABLE across the
	// one resolution attempt: a re-gate after a committed fix reports the same
	// sha the first failure did, which is what keeps merge.Coordinator's
	// one-attempt-per-failure accounting from handing out a fresh attempt every
	// time a fix moves the head.
	FailingCommit string
	// TestFailureTail is the clamped tail of the failing suite's output, set
	// when Outcome is OutcomeTestFailed. It travels into the resolution prompt
	// and into the merge_failed cause.
	TestFailureTail string
	// TestFailureOutputPath names the file holding the failing suite's COMPLETE
	// output, set when Outcome is OutcomeTestFailed and the run could be
	// archived. The tail the cause carries is clamped and, for a multi-suite
	// runner, frequently does not contain the failure at all — so the path is
	// what makes the failure diagnosable after the merge is over.
	TestFailureOutputPath string
}

// cherryPickAnnotationRE matches the "(cherry picked from commit <sha>)"
// annotation that `git cherry-pick -x` writes, mirroring
// agent-repl--extract-cherry-pick-shas.
var cherryPickAnnotationRE = regexp.MustCompile(`\(cherry picked from commit ([0-9a-f]{40})\)`)

// errTargetMoved reports that the target's HEAD is no longer the head this
// rebase based itself on, so the merge commit would incorporate a line that was
// never tested against what the target now carries.
//
// IT IS A RECOVERABLE REFUSAL, not a failure, which is why it is a sentinel
// rather than a plain error: Merge answers it by re-rebasing ONCE onto the new
// head. A second occurrence is a target being written to faster than a gated
// merge can keep up with, and that fails loudly rather than spinning.
var errTargetMoved = errors.New("merge: the target moved off the head this rebase based itself on")

// errMergeRefused reports that git would not make the merge commit. It cannot
// be a conflict — the target is on the head the rebased line descends from — so
// it is always the target holding work of its own that the merge would
// overwrite: an uncommitted edit, or an untracked file. It is a classified
// merge OUTCOME (merge_failed) rather than a pipeline error, and the target
// keeps whatever it was holding.
var errMergeRefused = errors.New("merge: git refused the merge commit")

// rebaseAttempts bounds Merge's re-rebase loop. TWO: the first rebase, plus
// exactly ONE automatic re-rebase for a target that moved underneath it while
// the gate ran. The bound is what keeps a repository with a busy target from
// turning one merge into an unbounded sequence of full test-suite runs.
const rebaseAttempts = 2

// headGate is what the merge's ONE test gate needs beyond the Request. It
// travels with a re-entered replay so the gate at the end of it judges the same
// merge the gate before it judged.
type headGate struct {
	// failingCommit PINS the identity a head-gate failure is attributed to,
	// empty on the first pass through a merge (where the rebased head's own sha
	// is used). It is set only by ContinueAfterTestFix, which re-gates a head the
	// resolution turn has moved: reporting the new head would look to
	// merge.Coordinator like a brand-new failure and earn a second resolution
	// attempt, and then a third, for as long as the agent keeps committing
	// something that does not fix the suite.
	failingCommit string
	// extraPaths widens the suite selection beyond the merge's own range. A
	// resolution turn may have edited a file no replayed commit touched, and a
	// selection made from the range alone would then run nothing that covers the
	// fix. The selection can only widen.
	extraPaths []string
}

// Merge REBASES the branch's commits onto the target's head IN A TEMPORARY
// WORKTREE, gates the fully rebased head on the repository's test suite there,
// and then moves the target exactly ONCE — a `git merge --no-ff` of the rebased
// branch.
//
// THE TARGET IS NOT THE WORKBENCH. The driver this replaced cherry-picked
// commit by commit INTO the target checkout and reset it back on failure, so
// every merge opened a window in which the tree every other workspace cuts from
// carried untested commits, and a `reset --hard` at the end of an unbounded
// agent turn could destroy an external commit that arrived meanwhile (twice
// observed live). Rebasing off-target closes the window structurally: nothing
// reaches the target until every commit has landed and passed, and a failure at
// any earlier stage leaves the target BYTE-FOR-BYTE as it was found.
//
// THE MERGE COMMIT IS THE POINT, not an artifact. `--no-ff` records the branch
// as the merge commit's SECOND PARENT, so the workspace's topology is visible in
// git porcelain rather than being flattened into a run of cherry-picks nothing
// links back to the branch.
//
// It emits one `merging` per landed commit (naming the commit and its position
// in the range) and one more for the head gate, then exactly one of
// merged / merge_conflict / merge_failed — or
// nothing further when the run ends on OutcomeTestFailed, which is the
// coordinator's to classify.
//
// A failed precondition (bad request, dirty source worktree, missing branch)
// aborts BEFORE any transition is emitted, so a rejected merge leaves the
// workspace's state exactly as it was — no half-transition.
//
// A conflict is LEFT IN THE REBASE WORKTREE (never aborted); the caller resumes
// it via Resume once the conflict has been resolved. The rebase worktree
// therefore OUTLIVES this call on a parked outcome, and the caller cleans it up
// through Cleanup when the run reaches its terminal.
func (e *Driver) Merge(ctx context.Context, req Request) (Result, error) {
	if err := req.validateRun(); err != nil {
		return Result{}, err
	}
	e.logf("merge: Merge start {ws=%s key=%s branch=%s source=%s target=%s}",
		req.Name, req.Workspace, req.SourceBranch, req.SourceDir, req.TargetDir)

	if err := e.assertMergeable(ctx, req); err != nil {
		return Result{}, err
	}
	if err := e.pruneStaleRebaseWorktrees(ctx, req); err != nil {
		return Result{}, err
	}

	var moved error
	for attempt := 1; attempt <= rebaseAttempts; attempt++ {
		res, err := e.rebaseOnto(ctx, req, attempt)
		if err == nil {
			return res, nil
		}
		if !errors.Is(err, errTargetMoved) {
			return Result{}, err
		}
		moved = err
		if attempt < rebaseAttempts {
			e.logf("merge: target MOVED under the rebase, RE-REBASING once {ws=%s target=%s attempt=%d/%d}: %v",
				req.Name, req.TargetDir, attempt, rebaseAttempts, err)
		}
	}
	return Result{}, fmt.Errorf("merge: %q rebased onto the target %d times and the target moved every time — the target is being written to faster than a gated merge can land; the target was NEVER MODIFIED: %w",
		req.Name, rebaseAttempts, moved)
}

// assertMergeable runs every precondition of a rebase merge, before the first
// transition is emitted, so a rejection leaves the workspace's state exactly as
// it was.
func (e *Driver) assertMergeable(ctx context.Context, req Request) error {
	// Uncommitted changes in the source worktree would be silently lost by the
	// merge (they belong to no commit), and the branch ref move at the end would
	// leave that worktree's HEAD disagreeing with its own index.
	if err := e.assertCleanWorktree(ctx, req.SourceDir); err != nil {
		return fmt.Errorf("merge: precondition for %q: %w", req.Name, err)
	}
	exists, err := e.branchExists(ctx, req.TargetDir, req.SourceBranch)
	if err != nil {
		return err
	}
	if !exists {
		return fmt.Errorf("merge: branch %q not found in %s", req.SourceBranch, req.TargetDir)
	}
	// Sequencer residue in the TARGET is now always somebody else's: this
	// pipeline never cherry-picks there. It still refuses every new operation
	// with an opaque exit 128, so it is rejected as a NAMED precondition with
	// the operator's way out in the message.
	stale, err := e.staleSequencerState(ctx, req.TargetDir)
	if err != nil {
		return err
	}
	if stale {
		return fmt.Errorf("merge: target %s has an unfinished cherry-pick (stale sequencer state, no conflicted commit) — run `git -C %s cherry-pick --quit` before merging %q",
			req.TargetDir, req.TargetDir, req.Name)
	}
	// An unfinished merge in the target would make the final `git merge --no-ff`
	// refuse, and it would do so AFTER a full rebase and every suite run. It is
	// cheaper and far clearer to say so first.
	merging, err := e.mergeInProgress(ctx, req.TargetDir)
	if err != nil {
		return err
	}
	if merging {
		return fmt.Errorf("merge: target %s has an unfinished merge (MERGE_HEAD present) — finish or `git -C %s merge --abort` it before merging %q",
			req.TargetDir, req.TargetDir, req.Name)
	}
	return nil
}

// rebaseOnto runs ONE rebase attempt: a fresh temporary worktree based on the
// target's current head, the per-commit replay in it, the single test gate on
// the head the replay produced, and — when that gate passes — the single target
// move.
//
// THE TEMP WORKTREE IS CLEANED UP ON EVERY PATH THIS CALL OWNS. It survives
// exactly one way: a parked outcome (conflict or test failure), which hands it
// to merge.Coordinator along with the responsibility to Cleanup it.
func (e *Driver) rebaseOnto(ctx context.Context, req Request, attempt int) (res Result, err error) {
	baseHead, err := e.gitString(ctx, req.TargetDir, "rev-parse", "HEAD")
	if err != nil {
		return Result{}, err
	}
	work, err := e.createRebaseWorktree(ctx, req, baseHead)
	if err != nil {
		return Result{}, err
	}
	req.WorkDir, req.BaseHead = work, baseHead
	e.logf("merge: REBASE WORKTREE created {ws=%s attempt=%d/%d work=%s base=%s target=%s} — the target is NOT touched until every commit has landed and passed",
		req.Name, attempt, rebaseAttempts, work, shortSHA(baseHead), req.TargetDir)

	defer func() {
		// A parked outcome keeps the tree; anything else has no further use for
		// it, and a temp worktree left behind accumulates one per failed merge.
		if err == nil && (res.Outcome == OutcomeConflict || res.Outcome == OutcomeTestFailed) {
			return
		}
		if cerr := e.Cleanup(ctx, req); cerr != nil {
			e.logf("merge: rebase worktree cleanup FAILED {ws=%s work=%s}: %v", req.Name, work, cerr)
		}
	}()

	// NOTHING IS PUBLISHED HERE. The opening used to publish a cherry_picking
	// status before the plan was computed, which put commits_total=0 and an empty
	// current_sha on the wire as the run's FIRST word about its picks — a
	// progress bar reading 0 of 0 for a merge that is about to land three
	// commits, and the only cherry_picking frame a frontend saw until the next
	// one replaced it. A cherry_picking status exists to say which commit of how
	// many is landing, so it is published from inside the loop, once the plan is
	// the run's denominator and a commit is genuinely in flight.
	base, err := e.cherryPickBase(ctx, work, req.SourceBranch)
	if err != nil {
		return Result{}, err
	}
	res, err = e.rebaseLoop(ctx, req, base, false, headGate{})
	if err != nil {
		return Result{}, err
	}
	res.WorkDir, res.BaseHead = work, baseHead
	return res, nil
}

// rebaseWorktreeMarker is the path component every rebase worktree carries. It
// is what makes an abandoned one RECOGNIZABLE to the next merge, which is the
// only way a process-local temp tree gets cleaned up after the process that
// made it died.
const rebaseWorktreeMarker = "agent-repl-merge-rebase-"

// pruneStaleRebaseWorktrees removes every rebase worktree the target's
// repository still has registered before this merge makes its own.
//
// ANY REBASE WORKTREE FOUND HERE IS ABANDONED, and that is a fact about the
// pipeline rather than an assumption: merges are serialized per REPOSITORY by
// merge.Coordinator's queue, so at the moment one starts there can be no other
// merge of this repository holding a tree open. What there CAN be is the tree of
// a daemon that was killed mid-merge — a bounce, a crash, a kill during a parked
// conflict — whose directory is gone or soon will be but whose registration
// outlives it and accumulates one entry per interrupted merge.
//
// A REMOVAL THAT FAILS DOES NOT FAIL THE MERGE. The new tree gets a fresh path
// either way, so a stale registration this could not clear is loud-logged and
// left for a human. A failure to LIST is different and is an error: this is
// `git -C target`, and a target whose worktrees cannot be enumerated is a target
// no part of the merge that follows should be trusted against.
func (e *Driver) pruneStaleRebaseWorktrees(ctx context.Context, req Request) error {
	out, err := e.gitString(ctx, req.TargetDir, "worktree", "list", "--porcelain")
	if err != nil {
		return err
	}
	for _, line := range splitLines(out) {
		path, ok := strings.CutPrefix(strings.TrimSpace(line), "worktree ")
		if !ok || !strings.Contains(path, rebaseWorktreeMarker) {
			continue
		}
		e.logf("merge: ABANDONED rebase worktree found, removing it {ws=%s target=%s stale=%s} — merges are serialized per repository, so this belongs to a daemon that died mid-merge",
			req.Name, req.TargetDir, path)
		stale := req
		stale.WorkDir = path
		if cerr := e.Cleanup(ctx, stale); cerr != nil {
			e.logf("merge: abandoned rebase worktree could NOT be removed {ws=%s target=%s stale=%s}: %v — the merge proceeds with a fresh tree; this one is left for a human",
				req.Name, req.TargetDir, path, cerr)
		}
	}
	return nil
}

// createRebaseWorktree adds a detached temporary worktree of the target's
// repository at baseHead. It is the tree every commit is replayed and tested in.
//
// IT IS A LINKED WORKTREE OF THE TARGET'S OWN REPOSITORY, so it shares the
// object store and the refs — which is what lets it resolve SourceBranch by
// name, and what makes the resulting commits already present in the target's
// repository when the merge commit finally references them.
func (e *Driver) createRebaseWorktree(ctx context.Context, req Request, baseHead string) (string, error) {
	parent, err := os.MkdirTemp("", rebaseWorktreeMarker)
	if err != nil {
		return "", fmt.Errorf("merge: creating the rebase worktree's parent directory for %q: %w", req.Name, err)
	}
	// git refuses to add a worktree at an existing non-empty path, and it
	// creates the leaf itself. The parent is what this owns and removes.
	dir := filepath.Join(parent, "rebase")
	exit, out, err := e.gitExit(ctx, req.TargetDir, "worktree", "add", "--detach", dir, baseHead)
	if err != nil {
		_ = os.RemoveAll(parent)
		return "", err
	}
	if exit != 0 {
		_ = os.RemoveAll(parent)
		return "", fmt.Errorf("merge: `git worktree add --detach %s %s` for %q exited %d in %s: %s",
			dir, shortSHA(baseHead), req.Name, exit, req.TargetDir, dlog.Clamp(out, 400))
	}
	return dir, nil
}

// Cleanup removes the rebase worktree req carries, and is a no-op for a request
// that has none.
//
// IT RUNS ON EVERY TERMINAL PATH — merged, failed, abandoned, shut down — because
// the worktree is the one piece of a merge that outlives the call that made it.
// A failure to remove it is REPORTED, never swallowed: the caller logs it, and a
// merge that landed is not un-landed by a leftover directory.
//
// THE FILESYSTEM REMOVAL RUNS EVEN WHEN `git worktree remove` FAILS, and the
// prune that follows is what stops the repository's administrative record from
// outliving the directory. A tree parked on a conflict is precisely the case
// `remove` needs `--force` for.
func (e *Driver) Cleanup(ctx context.Context, req Request) error {
	if req.WorkDir == "" {
		return nil
	}
	var errs []error
	exit, out, err := e.gitExit(ctx, req.TargetDir, "worktree", "remove", "--force", req.WorkDir)
	switch {
	case err != nil:
		errs = append(errs, err)
	case exit != 0:
		errs = append(errs, fmt.Errorf("merge: `git worktree remove --force %s` exited %d in %s: %s",
			req.WorkDir, exit, req.TargetDir, dlog.Clamp(out, 400)))
	}
	if rmErr := os.RemoveAll(filepath.Dir(req.WorkDir)); rmErr != nil {
		errs = append(errs, fmt.Errorf("merge: removing the rebase worktree directory %s: %w", req.WorkDir, rmErr))
	}
	if pexit, pout, perr := e.gitExit(ctx, req.TargetDir, "worktree", "prune"); perr != nil {
		errs = append(errs, perr)
	} else if pexit != 0 {
		errs = append(errs, fmt.Errorf("merge: `git worktree prune` exited %d in %s: %s", pexit, req.TargetDir, dlog.Clamp(pout, 400)))
	}
	if len(errs) > 0 {
		return errors.Join(errs...)
	}
	e.logf("merge: rebase worktree REMOVED {ws=%s work=%s}", req.Name, req.WorkDir)
	return nil
}

// rebaseLoop replays base..SourceBranch onto the rebase worktree's head ONE
// COMMIT AT A TIME and then runs the repository's test suite ONCE, on the head
// the replay produced.
//
// THE REPLAY IS PER-COMMIT; THE GATE IS NOT, and the two facts have different
// reasons. The replay is per-commit because a conflict has to be PARKED on the
// commit that caused it — that is what a resolver is handed and what Resume
// continues — and because the loop's restartability is derived from the `-x`
// annotation each pick leaves behind. The gate runs once at the head because the
// user chose ONE suite run per merge over per-commit attribution: a full suite
// per replayed commit is the merge's whole cost, and the tree that actually
// reaches the target is the head, not any intermediate.
//
// IT IS A REBASE, and `cherry-pick -x` is how it is spelled. `git rebase` is
// itself a sequence of picks, and driving them individually is what makes the
// conflict park and the resume possible at all: `git rebase` offers no point
// between two commits at which this pipeline could hand a conflict to an agent
// and come back. The `-x` annotation is retained because the loop's own
// restartability reads it (see below) and because it records which commit of the
// workspace's branch each rebased commit came from.
//
// IT IS RESTARTABLE BY CONSTRUCTION, GATE INCLUDED. The loop derives its work
// from git alone —
// the rebase base (which advances past every `-x` annotation already on the
// rebased line) plus a per-commit patch-id probe — so re-entering it after a
// resume or a test fix skips what already landed rather than replaying it. A
// re-entry that finds the whole range already replayed therefore lands on the
// gate directly: the gate is the loop's tail, not a step inside it, so "the
// replay is done but the gate never passed" re-enters where it left off without
// a single byte of side-channel state.
//
// MERGE COMMITS ARE FLATTENED. `--no-merges` drops them: a merge commit carries
// no patch of its own, and every commit on both of its sides is already in the
// range and picked individually.
//
// landedEarlier tells the loop whether work has ALREADY landed on the rebased
// line during this merge (a resolved conflict, a committed test fix). It is what
// keeps a re-entered loop that finds nothing left to do from reporting the
// merge as a no-op when it plainly was not one — and it is what tells the tail
// that a replay with nothing left still owes the target a gated head.
//
// gate carries the head gate's own context across a re-entry (see headGate).
func (e *Driver) rebaseLoop(ctx context.Context, req Request, base string, landedEarlier bool, gate headGate) (Result, error) {
	rng := base + ".." + req.SourceBranch
	plan, err := e.plan(ctx, req.WorkDir, rng)
	if err != nil {
		return Result{}, err
	}
	// THE PLAN IS THE RUN'S DENOMINATOR. Every phase from here on reports
	// commits_total from it, so the figure a frontend renders is the plan being
	// executed rather than whatever each call site happened to count.
	//
	// A RE-ENTERED loop reconciles instead of replacing. Its base has advanced
	// past everything already on the rebased line, so `plan` here is only the
	// REMAINDER of the run's range — recording it as the plan would make
	// commits_total shrink as the run progressed, and a re-entry that finds
	// nothing left would report a merge of zero commits.
	if landedEarlier {
		if err := e.resumeRunPlan(ctx, req, plan); err != nil {
			return Result{}, err
		}
	} else {
		req.Run.SetPlan(plan)
	}
	commits := plan.Commits
	if len(commits) == 0 {
		// NOTHING LEFT TO REPLAY IS NOT NOTHING LEFT TO DO. On a FIRST pass it is
		// the no-op merge (the workspace's contribution is already on the target by
		// ancestry) and the tail below short-circuits it without a suite run. On a
		// RE-ENTRY it is a replay that finished, whose head has not passed the gate
		// yet — so the tail runs the gate rather than the loop.
		e.logf("merge: range %s empty — nothing left to replay {ws=%s landed_earlier=%t}", rng, req.Name, landedEarlier)
	}

	landed := 0
	for i, commit := range commits {
		progress := fmt.Sprintf("%d/%d", i+1, len(commits))
		sha, short := commit.SHA, commit.Short

		already, err := e.commitAlreadyIncorporated(ctx, req.WorkDir, sha)
		if err != nil {
			return Result{}, err
		}
		if already {
			// Replaying it would go empty, which wedges git's sequencer on a
			// state only `--skip` or `--quit` clears. Skipping it here keeps
			// that state unrepresentable rather than recoverable.
			e.logf("merge: SKIPPING %s (%s), already on the rebased line by patch-id {ws=%s}", short, progress, req.Name)
			// IT COUNTS AS LANDED. The commit is on the rebased line — and a
			// progress figure that skipped it would count down toward a total it
			// could never reach.
			req.Run.CommitLanded()
			continue
		}

		// The phase is published BEFORE the replay, not after: the replay is the
		// slow part, and a user watching a merge should see which commit is
		// landing while it lands rather than only once it has.
		if err := req.Run.CherryPicking(commit, "rebasing "+progress+": "+short); err != nil {
			return Result{}, err
		}

		// A non-zero exit is not a spawn error; the exit code and the presence
		// of CHERRY_PICK_HEAD classify the outcome.
		exit, out, err := e.gitExit(ctx, req.WorkDir, "cherry-pick", "-x", sha)
		if err != nil {
			return Result{}, err
		}
		e.logf("merge: rebase replay of %s (%s) exit=%d {ws=%s work=%s} %s", short, progress, exit, req.Name, req.WorkDir, dlog.Clamp(out, 400))

		inProgress, err := e.cherryPickInProgress(ctx, req.WorkDir)
		if err != nil {
			return Result{}, err
		}
		if inProgress {
			// CHERRY_PICK_HEAD ALONE DOES NOT MEAN CONFLICT. git parks the same
			// marker for a replay that went EMPTY — the commit's change is already
			// in the rebased line's tree, so there is nothing to apply and nothing
			// to resolve — and only `--skip` or `--quit` clears that state.
			// Classifying it as a conflict parks a merge on work no resolver can
			// do, and the resume that follows can only fail: `--continue`
			// refuses an empty pick and re-parks the same marker.
			empty, err := e.pickWentEmpty(ctx, req.WorkDir)
			if err != nil {
				return Result{}, err
			}
			if empty {
				if err := e.finishEmptyPick(ctx, req, short, progress); err != nil {
					return Result{}, err
				}
				// IT COUNTS AS LANDED, for the same reason the patch-id skip
				// above does: the change is on the rebased line, and a progress
				// figure that skipped it would count toward a total it could never
				// reach.
				req.Run.CommitLanded()
				continue
			}
			return e.markConflict(ctx, req)
		}
		if exit != 0 {
			// No CHERRY_PICK_HEAD. Distinguish the ALREADY-MERGED case (a prior
			// `cherry-pick -x` rewrote our commits under new SHAs, so the
			// SHA-keyed probes missed them) from a genuine failure via
			// `git cherry` (patch-id comparison) over the whole range.
			incorporated, err := e.rangeAlreadyIncorporated(ctx, req.WorkDir, base, req.SourceBranch)
			if err != nil {
				return Result{}, err
			}
			if incorporated {
				e.logf("merge: range %s already incorporated by patch-id {ws=%s}", rng, req.Name)
				return e.finalizeMerged(ctx, req, true)
			}
			return e.markFailed(ctx, req,
				fmt.Sprintf("rebase replay of %s (%s) exited %d with no conflict to resolve", short, progress, exit))
		}
		landed++
		req.Run.CommitLanded()
	}

	// THE GATE IS THE LOOP'S TAIL. Every commit of the range is on the rebased
	// line now, so the tree the suite judges is exactly the tree `git merge
	// --no-ff` is about to put on the target.
	if landed == 0 && !landedEarlier {
		// Nothing this merge did changed the rebased line's TREE: every planned
		// commit was already incorporated, or carried no change against it. There
		// is nothing for a suite to testify about that the target has not already
		// been carrying, and the pre-existing no-op path is the honest answer.
		return e.finalizeMerged(ctx, req, true)
	}
	failure, err := e.gateHead(ctx, req, gate)
	if err != nil {
		return Result{}, err
	}
	if failure != nil {
		return *failure, nil
	}
	return e.finalizeMerged(ctx, req, false)
}

// gateHead runs the target repository's test suite ONCE, on the HEAD of the
// rebase worktree, and reports a test failure as a Result or nil when the merge
// may proceed to the target move.
//
// ONE RUN PER MERGE, ON THE TREE THAT ACTUALLY LANDS. The gate used to run after
// every replayed commit, which spent a full suite per commit to attribute a
// failure to one of them. The user chose the cheaper contract: the head is the
// only tree `git merge --no-ff` puts on the target, so the head is what the
// suite has to testify about, and an intermediate commit that would have failed
// on its own is not a fact about what the target receives.
//
// IT RUNS IN THE REBASE WORKTREE, which is a full checkout of the repository at
// exactly the tree the merge proposes to make. Running it there rather than in
// the target is what keeps the target's own checkout unmodified — and untested
// code out of it — for the whole of the merge.
//
// The `merging` transition is emitted BEFORE the run, not after, because the
// run is the slow part: a user watching a merge should see "testing the rebased
// head abc123 after 7 commits" while it happens rather than only once it is
// over.
//
// A SKIPPED suite continues the merge. That is the documented behavior for a
// target repository that declares no test entrypoint — the merge subsystem
// serves repositories that have no agent-repl suite — and it is loud-logged by
// the runner AND here, because a merge landing untested is precisely the fact a
// user later needs to find in the log.
func (e *Driver) gateHead(ctx context.Context, req Request, gate headGate) (*Result, error) {
	head, err := e.gitString(ctx, req.WorkDir, "rev-parse", "--short", "HEAD")
	if err != nil {
		return nil, err
	}
	sel, err := e.selectSuites(ctx, req, gate.extraPaths)
	if err != nil {
		return nil, err
	}
	progress := fmt.Sprintf("the rebased head %s after %d commits", head, req.Run.CommitsLanded())
	// The testing phase carries the SAME commit context the cherry_picking phases
	// did — the run holds it, so the two cannot disagree — which is what lets a
	// frontend render one progress figure across both. It also carries the
	// SELECTION, which is the merge's own record of which suites were asked to
	// testify about it.
	if err := req.Run.Testing("testing " + progress + " [" + selectionLabel(sel.Suites) + "]"); err != nil {
		return nil, err
	}
	sr, err := e.suite.RunSuite(ctx, req.WorkDir, SuiteRun{Suites: sel.Suites, Attempt: 1})
	if err != nil {
		e.logf("merge: test gate UNRUNNABLE at the rebased head %s {ws=%s work=%s}: %v", head, req.Name, req.WorkDir, err)
		return nil, fmt.Errorf("merge: test gate for %q at the rebased head %s: %w", req.Name, head, err)
	}
	if sr.Skipped {
		e.logf("merge: test gate SKIPPED at the rebased head %s {ws=%s work=%s}: %s — this merge lands UNTESTED",
			head, req.Name, req.WorkDir, sr.Reason)
		return nil, nil
	}
	if sr.Passed {
		e.logf("merge: test gate PASSED at the rebased head %s (%s) {ws=%s suites=%s duration=%s}",
			head, progress, req.Name, selectionLabel(sel.Suites), sr.Duration.Round(time.Millisecond))
		return nil, nil
	}
	e.logf("merge: test gate FAILED at the rebased head %s (%s) {ws=%s work=%s suites=%s duration=%s full_output=%s} tail:\n%s",
		head, progress, req.Name, req.WorkDir, selectionLabel(sel.Suites),
		sr.Duration.Round(time.Millisecond), sr.OutputPath, sr.Tail)

	rerun, err := e.rerunAfterFailure(ctx, req, sel, progress, head, sr)
	if err != nil {
		return nil, err
	}
	if rerun == nil {
		// The re-run passed on the same tree: a flake, already reported loudly.
		return nil, nil
	}
	sr = *rerun
	// A RE-GATE AFTER A RESOLUTION TURN KEEPS THE ORIGINAL IDENTITY. See headGate:
	// the head has moved by the fix commit, and reporting the new sha would earn
	// the merge a second resolution attempt it is not entitled to.
	failing := gate.failingCommit
	if failing == "" {
		failing = head
	}
	return &Result{
		Outcome:               OutcomeTestFailed,
		FailingCommit:         failing,
		TestFailureTail:       sr.Tail,
		TestFailureOutputPath: sr.OutputPath,
		WorkDir:               req.WorkDir,
		BaseHead:              req.BaseHead,
	}, nil
}

// rerunAfterFailure runs the same suites over the same tree exactly ONCE more
// and classifies the pair.
//
// WHY ONE RE-RUN AND ONLY ONE. A merge gate that denies on a first failure
// denies on every flake, and this gate's suites include ones that share a
// machine with a live daemon, a browser build and whatever else the user is
// running — a suite that fails under that load and passes on a quiet tree told
// the merge nothing about the code. One re-run separates those two stories at
// the cost of one run. It stops at one because a suite that fails twice on an
// unchanged tree is not a flake by any reading, and a gate that keeps retrying
// is a gate that eventually passes anything.
//
// A PASS ON THE RE-RUN IS REPORTED, NOT SWALLOWED: both verdicts, both
// durations and both archives are logged, because the merge proceeding is
// exactly when nobody goes looking for the failure.
//
// It returns (nil, nil) for a flake — the gate proceeds — and the SECOND run's
// result for a genuine failure.
func (e *Driver) rerunAfterFailure(ctx context.Context, req Request, sel SuiteSelection, progress, head string, first SuiteResult) (*SuiteResult, error) {
	e.logf("merge: test gate RE-RUNNING once on the SAME tree to separate a flake from a genuine failure {ws=%s work=%s suites=%s head=%s}",
		req.Name, req.WorkDir, selectionLabel(sel.Suites), head)
	if err := req.Run.Testing("re-testing " + progress + " [" + selectionLabel(sel.Suites) + "] after a failure"); err != nil {
		return nil, err
	}
	second, err := e.suite.RunSuite(ctx, req.WorkDir, SuiteRun{Suites: sel.Suites, Attempt: 2})
	if err != nil {
		e.logf("merge: test gate RE-RUN UNRUNNABLE at the rebased head %s {ws=%s work=%s}: %v", head, req.Name, req.WorkDir, err)
		return nil, fmt.Errorf("merge: test gate re-run for %q at the rebased head %s: %w", req.Name, head, err)
	}
	if second.Skipped {
		// The first run produced a verdict, so the entrypoint was there. Its
		// disappearance between the two runs is a contradiction, not a skip to
		// wave through.
		return nil, fmt.Errorf("merge: test gate re-run for %q at the rebased head %s reported the suite SKIPPED (%s) when the first run had a verdict",
			req.Name, head, second.Reason)
	}
	if second.Passed {
		e.logf("merge: test gate FLAKE at the rebased head %s (%s) {ws=%s work=%s suites=%s} — FAILED in %s then PASSED in %s on the UNCHANGED tree; the merge proceeds. first_output=%s rerun_output=%s",
			head, progress, req.Name, req.WorkDir, selectionLabel(sel.Suites),
			first.Duration.Round(time.Millisecond), second.Duration.Round(time.Millisecond),
			first.OutputPath, second.OutputPath)
		return nil, nil
	}
	e.logf("merge: test gate FAILED TWICE at the rebased head %s (%s) {ws=%s work=%s suites=%s} — %s then %s, a genuine failure. first_output=%s rerun_output=%s",
		head, progress, req.Name, req.WorkDir, selectionLabel(sel.Suites),
		first.Duration.Round(time.Millisecond), second.Duration.Round(time.Millisecond),
		first.OutputPath, second.OutputPath)
	// The re-run's tail is the freshest evidence and is what the resolution turn
	// reads, but the first run's archive is half the record and must not vanish
	// with it.
	if first.OutputPath != "" {
		second.Tail += fmt.Sprintf("\n[merge] this suite failed TWICE on the same tree; the first run's complete output is at %s\n", first.OutputPath)
	}
	return &second, nil
}

// selectSuites decides which of the target repository's suites this gate runs,
// from the paths the merge's own range touches plus any extra paths the caller
// knows about.
//
// THE RANGE, NOT THE HEAD COMMIT. The suites are the ones the WHOLE merge can
// affect: selecting from the head commit alone would let a webapp-and-daemon
// branch whose last commit touches the webapp be gated on the webapp suite,
// while the daemon change it also carries goes untested. Using the range also
// makes the selection identical across a re-gate after a resolution turn, which
// is what lets the second gate answer the same question the first one did.
func (e *Driver) selectSuites(ctx context.Context, req Request, extraPaths []string) (SuiteSelection, error) {
	base, err := e.gitString(ctx, req.WorkDir, "merge-base", "HEAD", req.SourceBranch)
	if err != nil {
		return SuiteSelection{}, err
	}
	paths, err := e.changedPaths(ctx, req.WorkDir, base+".."+req.SourceBranch)
	if err != nil {
		return SuiteSelection{}, err
	}
	paths = append(paths, extraPaths...)
	sel := SelectSuites(paths)
	e.logf("merge: suite SELECTION {ws=%s work=%s range=%s..%s paths=%d full=%t suites=%s}: %s",
		req.Name, req.WorkDir, shortSHA(base), req.SourceBranch, len(paths), sel.Full,
		selectionLabel(sel.Suites), sel.Reason)
	return sel, nil
}

// changedPaths lists the repository-relative paths the given `git log`
// revision arguments touch, de-duplicated.
//
// A PATH THE GATE CANNOT READ IS AN ERROR, never an empty list: an empty list
// means "this change touches nothing", which SelectSuites answers with the full
// set — so a swallowed git failure would look like the conservative answer while
// actually being a guess.
func (e *Driver) changedPaths(ctx context.Context, dir string, revArgs ...string) ([]string, error) {
	args := append([]string{"log", "--no-merges", "--name-only", "--pretty=format:"}, revArgs...)
	out, err := e.gitString(ctx, dir, args...)
	if err != nil {
		return nil, err
	}
	seen := map[string]bool{}
	var paths []string
	for _, line := range splitLines(out) {
		line = strings.TrimSpace(line)
		if line == "" || seen[line] {
			continue
		}
		seen[line] = true
		paths = append(paths, line)
	}
	return paths, nil
}

// Resume continues a rebase replay that a human (or the workspace's own agent)
// has resolved IN THE REBASE WORKTREE (the resolve-and-continue handoff arrives
// as a FrontendCommand with conflict_resolved_continue at stitch). It stages the
// resolved files (`git add -u`) and runs `git cherry-pick --continue`.
//
// IT NEVER TOUCHES THE TARGET. The conflict was parked in the temporary rebase
// worktree, which is where the resolution happened and where the continue lands;
// the target is still exactly as the merge found it and stays that way until the
// whole rebase has passed its gates.
//
// It emits merging (the conflict is clearing) and then RE-ENTERS the per-commit
// replay for whatever is left of the range — so a resume is not the end of a
// merge, it is the middle of one. The resolved commit is NOT gated on its own:
// the merge's single suite run happens at the head the re-entered replay
// produces, which necessarily contains it.
func (e *Driver) Resume(ctx context.Context, req Request) (Result, error) {
	if err := req.validateRebase(); err != nil {
		return Result{}, err
	}
	e.logf("merge: Resume start {ws=%s key=%s work=%s target=%s}", req.Name, req.Workspace, req.WorkDir, req.TargetDir)

	inProgress, err := e.cherryPickInProgress(ctx, req.WorkDir)
	if err != nil {
		return Result{}, err
	}
	if !inProgress {
		// No paused replay to continue: the caller's premise (a resolved
		// conflict awaiting continue) is false. Fail loudly rather than
		// silently report success.
		return Result{}, fmt.Errorf("merge: no rebase replay in progress in %s — nothing to resume for %q",
			req.WorkDir, req.Name)
	}

	// THE CURSOR IS REBUILT BEFORE THE FIRST PUBLICATION. A resume may be driven
	// by a publisher that lost its progress with the process that held it (a boot
	// replay), and the first thing it does is publish — so a cursor rebuilt any
	// later would put "0 of 0" on the wire as the resumed run's opening word.
	remaining, err := e.remainingPlan(ctx, req)
	if err != nil {
		return Result{}, err
	}
	if err := e.resumeRunPlan(ctx, req, remaining); err != nil {
		return Result{}, err
	}

	if err := req.Run.PickingCurrent("resuming the rebase after resolve"); err != nil {
		return Result{}, err
	}

	if exit, out, err := e.gitExit(ctx, req.WorkDir, "add", "-u"); err != nil {
		return Result{}, err
	} else if exit != 0 {
		return Result{}, fmt.Errorf("merge: `git add -u` exited %d in %s: %s", exit, req.WorkDir, dlog.Clamp(out, 400))
	}

	// A RESOLUTION CAN EMPTY A REPLAY, and `--continue` cannot commit one. Both
	// the resolution that discards the whole change and the replay that was empty
	// the moment it parked leave the staged tree identical to HEAD, where
	// `--continue` exits non-zero and leaves CHERRY_PICK_HEAD exactly where it
	// was — a fixpoint that re-parks the merge on every resume.
	empty, err := e.pickWentEmpty(ctx, req.WorkDir)
	if err != nil {
		return Result{}, err
	}
	if empty {
		emptied, err := e.gitString(ctx, req.WorkDir, "rev-parse", "--short", "CHERRY_PICK_HEAD")
		if err != nil {
			return Result{}, err
		}
		if err := e.finishEmptyPick(ctx, req, emptied, "resumed"); err != nil {
			return Result{}, err
		}
		req.Run.CommitLanded()
		return e.continueRange(ctx, req, headGate{})
	}

	// -c core.editor=true + --no-edit keep the original commit message
	// (including the -x annotation) without opening $EDITOR in a headless run.
	exit, out, err := e.gitExit(ctx, req.WorkDir,
		"-c", "core.editor=true", "cherry-pick", "--continue", "--no-edit")
	if err != nil {
		return Result{}, err
	}
	e.logf("merge: rebase replay --continue exit=%d {ws=%s work=%s} %s", exit, req.Name, req.WorkDir, dlog.Clamp(out, 400))

	stillInProgress, err := e.cherryPickInProgress(ctx, req.WorkDir)
	if err != nil {
		return Result{}, err
	}
	if stillInProgress {
		return e.markConflict(ctx, req)
	}
	if exit != 0 {
		return e.markFailed(ctx, req, fmt.Sprintf("rebase replay --continue exited %d with no conflict to resolve", exit))
	}

	// The resolved commit is now the rebased line's HEAD. It is NOT gated here:
	// a hand-resolved conflict is exactly the kind of landing that breaks a
	// suite, and the merge's one gate — which runs at the head the re-entered
	// replay finishes on — is where that shows up. Gating here as well would be
	// the per-commit gate this pipeline no longer runs, reintroduced for one
	// commit.
	resumed, err := e.gitString(ctx, req.WorkDir, "rev-parse", "--short", "HEAD")
	if err != nil {
		return Result{}, err
	}
	e.logf("merge: rebase replay RESOLVED and committed {ws=%s work=%s head=%s} — re-entering the replay; the suite runs once, at the head it reaches",
		req.Name, req.WorkDir, resumed)
	return e.continueRange(ctx, req, headGate{})
}

// ContinueAfterTestFix commits whatever the resolution turn staged in the
// REBASE WORKTREE and re-enters the replay, which re-runs the suite on the head
// the fix produced.
//
// THIS IS THE REMEDIATION LOOP'S RETURN EDGE. The gate that failed judged the
// head; the fix commit moves the head; the re-entered loop replays anything the
// range still owes and gates the NEW head. The loop turns until the gate passes
// or merge.Coordinator's one-attempt accounting fails the merge, and the
// identity that accounting keys on is carried across (see headGate).
//
// THE FIX LANDS AS A FOLLOW-UP COMMIT, NOT AN AMEND. Both would work, and the
// follow-up is the mechanically simpler of the two:
//   - It leaves the replayed commit's SHA and its `-x` annotation exactly
//     as `cherry-pick -x` wrote them. Those annotations are what
//     cherryPickBase reads to know what already landed, so an amend would
//     rewrite the one record the replay's restartability depends on.
//   - It needs no interaction with the sequencer or with the commit message of
//     a commit the driver did not author.
//   - It keeps the rebased line honest: the replayed commit and the fix it
//     required are two different pieces of work by two different authors.
//
// A resolution turn that staged NOTHING is not an error here. It is reported
// loudly and the suite is re-run anyway, which will fail again and fail the
// merge — the honest outcome for an agent that did not fix anything. The target
// is untouched throughout, so there is nothing to undo.
func (e *Driver) ContinueAfterTestFix(ctx context.Context, req Request, failingCommit string) (Result, error) {
	if err := req.validateRebase(); err != nil {
		return Result{}, err
	}
	if failingCommit == "" {
		return Result{}, fmt.Errorf("merge: ContinueAfterTestFix for %q needs the failing commit", req.Name)
	}
	e.logf("merge: ContinueAfterTestFix start {ws=%s key=%s failing=%s work=%s}", req.Name, req.Workspace, failingCommit, req.WorkDir)

	if err := e.commitTestFix(ctx, req, failingCommit); err != nil {
		return Result{}, err
	}
	// A FIX IS NOT BOUND BY THE SOURCE RANGE. The resolution turn may have edited
	// a file no replayed commit touched, and a gate narrowed to the range's
	// suites alone would then run nothing that covers the fix. The rebased line's
	// HEAD is read here — the fix commit when there was one, the replayed commit
	// when the turn staged nothing, and in-range either way — so the selection can
	// only widen.
	fixPaths, err := e.changedPaths(ctx, req.WorkDir, "HEAD", "-1")
	if err != nil {
		return Result{}, err
	}
	return e.continueRange(ctx, req, headGate{failingCommit: failingCommit, extraPaths: fixPaths})
}

// commitTestFix stages everything the resolution turn touched and commits it as
// a follow-up commit. A clean tree is loud-logged and left alone.
func (e *Driver) commitTestFix(ctx context.Context, req Request, failingCommit string) error {
	if exit, out, err := e.gitExit(ctx, req.WorkDir, "add", "-A"); err != nil {
		return err
	} else if exit != 0 {
		return fmt.Errorf("merge: `git add -A` exited %d in %s: %s", exit, req.WorkDir, dlog.Clamp(out, 400))
	}
	staged, _, err := e.gitExit(ctx, req.WorkDir, "diff", "--cached", "--quiet")
	if err != nil {
		return err
	}
	if staged == 0 {
		e.logf("merge: test fix staged NOTHING {ws=%s failing=%s work=%s} — the resolution turn changed no files; the suite is re-run as-is",
			req.Name, failingCommit, req.WorkDir)
		return nil
	}
	msg := fmt.Sprintf("fix tests after rebasing %s (%s)", failingCommit, req.Name)
	exit, out, err := e.gitExit(ctx, req.WorkDir, "-c", "core.editor=true", "commit", "-m", msg)
	if err != nil {
		return err
	}
	if exit != 0 {
		return fmt.Errorf("merge: committing the test fix for %q exited %d in %s: %s",
			req.Name, exit, req.WorkDir, dlog.Clamp(out, 400))
	}
	e.logf("merge: test fix COMMITTED {ws=%s failing=%s work=%s} %s", req.Name, failingCommit, req.WorkDir, dlog.Clamp(out, 200))
	return nil
}

// continueRange re-enters the per-commit replay for whatever is left of the
// range, and — because the gate is the replay's tail — for the head gate the
// merge still owes even when nothing is left. The base is recomputed rather than
// carried, which is what lets the resumed loop skip every commit that already
// landed.
func (e *Driver) continueRange(ctx context.Context, req Request, gate headGate) (Result, error) {
	base, err := e.cherryPickBase(ctx, req.WorkDir, req.SourceBranch)
	if err != nil {
		return Result{}, err
	}
	return e.rebaseLoop(ctx, req, base, true, gate)
}

// remainingPlan is what the replay still has to land: the range from the
// recomputed rebase base, which has advanced past every `-x` annotation already
// on the rebased line.
func (e *Driver) remainingPlan(ctx context.Context, req Request) (CommitPlan, error) {
	base, err := e.cherryPickBase(ctx, req.WorkDir, req.SourceBranch)
	if err != nil {
		return CommitPlan{}, err
	}
	return e.plan(ctx, req.WorkDir, base+".."+req.SourceBranch)
}

// resumeRunPlan re-establishes a RE-ENTERED run's commit cursor from git.
//
// The remainder alone cannot say how far a run has got — it is what is LEFT, and
// a run one commit from done and a run that never started can both have one
// commit left. The denominator comes from the whole range the workspace
// contributes (merge-base..branch), which is the only figure on disk that
// survives a daemon bounce, and RunStatus.ResumePlan decides which of the two
// the run actually needs: its own total when it still has one, this one when its
// publisher was rebuilt from nothing.
//
// The merge base is read directly rather than through cherryPickBase, because
// the two answer different questions: cherryPickBase deliberately skips forward
// over what already landed, and skipping forward is precisely what makes the
// remainder unable to count it.
func (e *Driver) resumeRunPlan(ctx context.Context, req Request, remaining CommitPlan) error {
	mergeBase, err := e.gitString(ctx, req.WorkDir, "merge-base", "HEAD", req.SourceBranch)
	if err != nil {
		return err
	}
	full, err := e.plan(ctx, req.WorkDir, mergeBase+".."+req.SourceBranch)
	if err != nil {
		return err
	}
	if err := req.Run.ResumePlan(full, remaining); err != nil {
		return err
	}
	e.logf("merge: run cursor REBUILT {ws=%s run=%s merge_base=%s range=%d remaining=%d}",
		req.Name, req.Run.RunID(), shortSHA(mergeBase), len(full.Commits), len(remaining.Commits))
	return nil
}

// THERE IS NO ROLLBACK, AND ITS ABSENCE IS THE POINT OF THIS DESIGN.
//
// merge.Driver.Rollback used to reset the TARGET back to its pre-merge head
// after a test gate failed for good, because the driver had been cherry-picking
// into the target all along. That reset fired at the end of an agent turn — an
// unbounded window in which nothing this subsystem holds keeps the target
// still — and it twice destroyed commits that reached the target meanwhile. It
// was subsequently guarded by the head the merge had left the target on
// (Result.TestedHead), which converted the hazard into a refusal but left the
// structure intact: a merge in progress still meant a shared trunk carrying
// untested commits.
//
// THE REBASE REMOVES THE THING ROLLBACK UNDID. Every commit lands and is gated
// in a temporary worktree, and the target moves exactly once, at the end, when
// the whole line has already passed. A failure at any earlier stage — a
// conflict nobody resolved, a suite that failed twice, an abandoned merge, a
// daemon bounce — leaves the target BYTE-FOR-BYTE as the merge found it, so
// there is nothing to reset and no window in which resetting could destroy
// somebody else's work.
//
// THE GUARD MIGRATED RATHER THAN BEING DROPPED. Its refusal logic — "the target
// must still be exactly where this merge last observed it" — now protects the
// one target-mutating step: landOnTarget compares the target's HEAD against
// Request.BaseHead and refuses the merge commit when they differ. What was a
// last-resort protection against a destructive reset is now a precondition of a
// purely additive one.

// commitAlreadyIncorporated reports whether sha's patch is already on dir's
// HEAD, probed over the single-commit range sha^..sha.
//
// A ROOT COMMIT (no parent) bounds no such range, so it is reported as not
// incorporated: there is nothing it could be a replay of.
func (e *Driver) commitAlreadyIncorporated(ctx context.Context, dir, sha string) (bool, error) {
	parent, _, err := e.gitExit(ctx, dir, "rev-parse", "--verify", "--quiet", sha+"^")
	if err != nil {
		return false, err
	}
	if parent != 0 {
		return false, nil
	}
	return e.rangeAlreadyIncorporated(ctx, dir, sha+"^", sha)
}

// planFieldSep is the ASCII unit separator `git log --pretty` writes between a
// commit's sha and its subject. It is used rather than a space or a tab because
// a subject may legitimately contain either, and a delimiter a commit message
// can forge is a parser that mis-splits on somebody's commit.
const planFieldSep = "\x1f"

// plan lists the commits rng contributes, oldest first, each with the subject
// line the phase status carries.
//
// IT IS THE SAME SELECTION THE REPLAY USED TO MAKE (`--reverse --no-merges`),
// now carrying the subject too: a frontend rendering "3/7 — fix the parser" has
// no other source for that text, and asking git a second time per commit would
// make the plan and the replay two lists that can disagree.
func (e *Driver) plan(ctx context.Context, dir, rng string) (CommitPlan, error) {
	out, err := e.gitString(ctx, dir, "log", "--reverse", "--no-merges",
		"--pretty=%H"+planFieldSep+"%s", rng)
	if err != nil {
		return CommitPlan{}, err
	}
	var plan CommitPlan
	for _, line := range splitLines(out) {
		sha, subject, found := strings.Cut(line, planFieldSep)
		if !found {
			// git wrote a line in a shape this format cannot produce. Guessing at
			// it would put a sha-shaped subject (or a subject-shaped sha) into the
			// replay, which picks by that string.
			return CommitPlan{}, fmt.Errorf("merge: commit plan line %q in %s has no %q separator", line, dir, planFieldSep)
		}
		plan.Commits = append(plan.Commits, PlannedCommit{
			SHA:     sha,
			Short:   shortSHA(sha),
			Subject: subject,
		})
	}
	return plan, nil
}

// shortSHA abbreviates a full SHA for logs and causes. It is a pure string
// operation rather than a `git rev-parse --short` call because it runs once per
// commit in the range and the abbreviation is only ever read by a human.
func shortSHA(sha string) string {
	if len(sha) <= 12 {
		return sha
	}
	return sha[:12]
}

// finalizeMerged is the shared "the rebase is complete" tail: land the rebased
// line on the target (the ONE target-mutating step of the whole pipeline), tag
// the completion (merge/<ws>), and leave the terminal `merged` status to
// merge.Coordinator.
//
// A tag-write failure is non-fatal (warned, not signaled): the merge commit is
// already on the target, so a tag failure must not undo it.
func (e *Driver) finalizeMerged(ctx context.Context, req Request, alreadyIncorporated bool) (Result, error) {
	landed, err := e.landOnTarget(ctx, req)
	if errors.Is(err, errMergeRefused) {
		// GIT REFUSED THE MERGE COMMIT, which by construction cannot be a
		// conflict: the target is on the head this line descends from. It is a
		// target-side obstruction — an uncommitted edit or an untracked file the
		// merge would overwrite — and the target is left holding it, untouched.
		// That is a terminal merge_failed, not a driver error, because it is a
		// classified outcome of the merge rather than a broken pipeline.
		return e.markFailed(ctx, req, err.Error())
	}
	if err != nil {
		return Result{}, err
	}
	if !landed {
		// The branch contributed nothing over the target's head. Marking it
		// merged is the pre-existing no-op path, and an empty merge commit would
		// record a topology that says work arrived when none did.
		alreadyIncorporated = true
	}
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
	// THE TERMINAL `merged` STATUS IS NOT PUBLISHED HERE, and its absence is the
	// contract rather than an omission. The run is not over when the last commit
	// lands: the workspace's after-action still has to run, and its failure rides
	// on the terminal status as after_action_error. Publishing `merged` from here
	// put the run's terminal word on the wire BEFORE the after_action phase
	// existed, so every frontend saw the merge finish and then watched a phase
	// begin after it — and the after-action's error reached a SECOND merged
	// status nothing was still reading. merge.Coordinator publishes it once, with
	// the action's outcome already on it (coordinator.go completeMergedRun).
	return Result{
		Outcome:             OutcomeMerged,
		AlreadyIncorporated: alreadyIncorporated,
		Tag:                 tag,
		WorkDir:             req.WorkDir,
		BaseHead:            req.BaseHead,
	}, nil
}

// landOnTarget is THE ONE STEP OF THIS PIPELINE THAT MODIFIES THE TARGET, and
// everything before it exists to make this step safe. It reports whether a merge
// commit was made (false means the branch contributed nothing).
//
// Three acts, in this order and no other:
//
//  1. THE GUARD. The target must still be on Request.BaseHead, the head this
//     rebase based itself on. Everything the gate certified was certified
//     against that head, so a target that moved has never been tested against
//     this line at all. A moved target is errTargetMoved — recoverable, and
//     merge.Driver.Merge answers it by re-rebasing once.
//
//  2. THE BRANCH REF MOVES TO THE REBASED LINE, compare-and-swap against the
//     value it is expected to hold, and logged with both SHAs. This is what
//     makes the merge commit's second parent the workspace's BRANCH rather than
//     an anonymous commit: after it, `git log --graph` on the target shows the
//     workspace by name. The source worktree is re-synced to the moved ref, and
//     a source worktree that has become dirty since the precondition FAILS the
//     merge before the target is touched — a checkout whose HEAD disagrees with
//     its own index is a worse outcome than a refused merge.
//
//  3. `git merge --no-ff`. It cannot conflict: the target is on BaseHead and the
//     rebased line descends from BaseHead, so this is a fast-forward that
//     --no-ff renders as a merge commit. A non-zero exit is therefore a genuine
//     surprise (an untracked file in the way, a hook refusing) and is reported
//     after aborting the half-made merge.
func (e *Driver) landOnTarget(ctx context.Context, req Request) (bool, error) {
	if err := req.validateRebase(); err != nil {
		return false, err
	}
	rebased, err := e.gitString(ctx, req.WorkDir, "rev-parse", "HEAD")
	if err != nil {
		return false, err
	}
	if rebased == req.BaseHead {
		e.logf("merge: rebase produced NO commits over the target head {ws=%s base=%s} — the branch is fully incorporated; no merge commit is made",
			req.Name, shortSHA(req.BaseHead))
		return false, nil
	}

	current, err := e.gitString(ctx, req.TargetDir, "rev-parse", "HEAD")
	if err != nil {
		return false, err
	}
	if current != req.BaseHead {
		return false, fmt.Errorf("%w: target %s moved from %s to %s while %q rebased and tested onto it, so nothing this merge tested was tested against what the target now carries; the target was NOT MODIFIED",
			errTargetMoved, req.TargetDir, shortSHA(req.BaseHead), shortSHA(current), req.Name)
	}

	moved, err := e.moveBranchRef(ctx, req, rebased)
	if err != nil {
		return false, err
	}

	msg := fmt.Sprintf("merge workspace %s (branch %s, merge run %s)", req.Name, req.SourceBranch, req.runIdentity())
	exit, out, err := e.gitExit(ctx, req.TargetDir,
		"-c", "core.editor=true", "merge", "--no-ff", "--no-edit", "-m", msg, req.SourceBranch)
	if err != nil {
		return false, err
	}
	if exit != 0 {
		// The merge cannot conflict by construction, so a non-zero exit left
		// something half-done that must not be inherited by the next merge, and
		// the branch must go back to where this merge found it.
		if aexit, aout, aerr := e.gitExit(ctx, req.TargetDir, "merge", "--abort"); aerr != nil {
			e.logf("merge: `git merge --abort` after a refused merge commit could not run {ws=%s target=%s}: %v", req.Name, req.TargetDir, aerr)
		} else if aexit != 0 {
			e.logf("merge: `git merge --abort` after a refused merge commit exited %d {ws=%s target=%s}: %s", aexit, req.Name, req.TargetDir, dlog.Clamp(aout, 200))
		}
		if rerr := e.restoreBranchRef(ctx, req, moved); rerr != nil {
			return false, rerr
		}
		return false, fmt.Errorf("%w: `git merge --no-ff %s` for %q exited %d in %s — the target is holding work of its own that the merge would overwrite, and it was NOT MODIFIED: %s",
			errMergeRefused, req.SourceBranch, req.Name, exit, req.TargetDir, dlog.Clamp(out, 400))
	}
	head, err := e.gitString(ctx, req.TargetDir, "rev-parse", "HEAD")
	if err != nil {
		return false, err
	}
	e.logf("merge: TARGET MOVED ONCE {ws=%s target=%s from=%s to=%s second_parent=%s branch=%s} — a --no-ff merge commit; the branch is the second parent",
		req.Name, req.TargetDir, shortSHA(req.BaseHead), shortSHA(head), shortSHA(rebased), req.SourceBranch)
	return true, nil
}

// moveBranchRef force-moves the workspace's branch to the rebased line and
// re-syncs the source worktree onto it.
//
// THE MOVE IS A COMPARE-AND-SWAP. `git update-ref <ref> <new> <old>` fails when
// the ref is not where this merge last read it, which is the same class of
// protection the target head guard gives: a branch somebody committed to while
// the gate ran holds work this rebase never replayed, and force-moving over it
// would make that work unreachable.
//
// THE SOURCE WORKTREE IS RE-SYNCED, and its cleanliness is re-checked first.
// That worktree has the branch checked out, so moving the ref moves its HEAD;
// leaving its index and files on the old line would show the user a worktree
// full of phantom reverse-diffs. A worktree that has become dirty since the
// precondition fails the merge here, BEFORE the target is touched, because the
// alternative is either destroying uncommitted work or leaving that worktree
// inconsistent with its own HEAD.
func (e *Driver) moveBranchRef(ctx context.Context, req Request, rebased string) (branchMove, error) {
	old, err := e.gitString(ctx, req.TargetDir, "rev-parse", "refs/heads/"+req.SourceBranch)
	if err != nil {
		return branchMove{}, err
	}
	if old == rebased {
		e.logf("merge: branch ref already ON the rebased line {ws=%s branch=%s sha=%s} — nothing to move",
			req.Name, req.SourceBranch, shortSHA(rebased))
		return branchMove{}, nil
	}
	checkedOut, err := e.sourceIsOnBranch(ctx, req)
	if err != nil {
		return branchMove{}, err
	}
	if checkedOut {
		if err := e.assertCleanWorktree(ctx, req.SourceDir); err != nil {
			return branchMove{}, fmt.Errorf("merge: the source worktree %s became dirty while %q was rebasing, and moving its branch to the rebased line would leave its checkout disagreeing with its own HEAD — the target was NOT MODIFIED: %w",
				req.SourceDir, req.Name, err)
		}
	}
	exit, out, err := e.gitExit(ctx, req.TargetDir, "update-ref", "refs/heads/"+req.SourceBranch, rebased, old)
	if err != nil {
		return branchMove{}, err
	}
	if exit != 0 {
		return branchMove{}, fmt.Errorf("merge: force-moving branch %s from %s to the rebased %s for %q exited %d in %s (the ref is not where this merge read it, so something committed to the branch meanwhile); the target was NOT MODIFIED: %s",
			req.SourceBranch, shortSHA(old), shortSHA(rebased), req.Name, exit, req.TargetDir, dlog.Clamp(out, 400))
	}
	e.logf("merge: branch ref FORCE-MOVED to the rebased line {ws=%s branch=%s old=%s new=%s}",
		req.Name, req.SourceBranch, shortSHA(old), shortSHA(rebased))
	move := branchMove{from: old, to: rebased, resync: checkedOut}

	if !checkedOut {
		return move, nil
	}
	if err := e.syncSourceWorktree(ctx, req, rebased); err != nil {
		return move, err
	}
	return move, nil
}

// branchMove records a completed branch ref move so a refused merge commit can
// put the ref back exactly where it was. A zero value means nothing moved.
type branchMove struct {
	from   string
	to     string
	resync bool
}

// restoreBranchRef undoes a branch move whose merge commit git then refused.
//
// LEAVING THE MOVE STANDING WOULD BE A SILENT REWRITE. The user asked for a
// merge, the merge did not happen, and their branch would nonetheless have been
// rewritten onto a base it was never merged into — with the source worktree
// re-synced to match. The restore is compare-and-swap for the same reason the
// move is, and a restore that itself fails is an ERROR: the state it leaves is
// one only a human can reconcile.
func (e *Driver) restoreBranchRef(ctx context.Context, req Request, move branchMove) error {
	if move.from == "" {
		return nil
	}
	exit, out, err := e.gitExit(ctx, req.TargetDir, "update-ref", "refs/heads/"+req.SourceBranch, move.from, move.to)
	if err != nil {
		return err
	}
	if exit != 0 {
		return fmt.Errorf("merge: restoring branch %s to %s after the merge commit for %q was refused exited %d in %s: %s",
			req.SourceBranch, shortSHA(move.from), req.Name, exit, req.TargetDir, dlog.Clamp(out, 400))
	}
	e.logf("merge: branch ref RESTORED after a refused merge commit {ws=%s branch=%s back_to=%s}", req.Name, req.SourceBranch, shortSHA(move.from))
	if !move.resync {
		return nil
	}
	return e.syncSourceWorktree(ctx, req, move.from)
}

// syncSourceWorktree points the source worktree's checkout at sha. It runs only
// on a worktree that has the branch checked out and was verified clean, so the
// hard reset can discard nothing.
func (e *Driver) syncSourceWorktree(ctx context.Context, req Request, sha string) error {
	exit, out, err := e.gitExit(ctx, req.SourceDir, "reset", "--hard", sha)
	if err != nil {
		return err
	}
	if exit != 0 {
		return fmt.Errorf("merge: re-syncing the source worktree %s onto %s for %q exited %d: %s",
			req.SourceDir, shortSHA(sha), req.Name, exit, dlog.Clamp(out, 400))
	}
	e.logf("merge: source worktree RE-SYNCED {ws=%s source=%s sha=%s}", req.Name, req.SourceDir, shortSHA(sha))
	return nil
}

// sourceIsOnBranch reports whether the source worktree actually has
// SourceBranch checked out. A source directory that is detached, or on some
// other branch, is not re-synced: this pipeline moves one ref and owns nothing
// else about that checkout.
func (e *Driver) sourceIsOnBranch(ctx context.Context, req Request) (bool, error) {
	exit, out, err := e.gitExit(ctx, req.SourceDir, "symbolic-ref", "--quiet", "HEAD")
	if err != nil {
		return false, err
	}
	if exit != 0 {
		return false, nil
	}
	return strings.TrimSpace(out) == "refs/heads/"+req.SourceBranch, nil
}

// markConflict emits merge_conflict for a replay left paused in the REBASE
// WORKTREE. It never aborts the replay: the conflict stays in that tree so the
// workspace's agent (or a human) can resolve it and Resume can continue it. The
// target is untouched and stays that way while the conflict is parked.
func (e *Driver) markConflict(ctx context.Context, req Request) (Result, error) {
	short, err := e.gitString(ctx, req.WorkDir, "rev-parse", "--short", "CHERRY_PICK_HEAD")
	if err != nil {
		return Result{}, err
	}
	// THE STATUS CARRIES THE FULL SHA, the logs and the Result carry the short
	// one. A conflict's entire purpose is to hand work to a human, and an
	// abbreviated sha is ambiguous by construction: it is the prefix git happened
	// to consider unique in THIS repository at THIS moment, so a frontend cannot
	// use it to address the commit anywhere else. The short form stays where it
	// has always been — the cause text a human reads, and ConflictCommit, which
	// the resume path matches on.
	full, err := e.gitString(ctx, req.WorkDir, "rev-parse", "CHERRY_PICK_HEAD")
	if err != nil {
		return Result{}, err
	}
	if err := req.Run.Conflict(full, "conflict rebasing "+short+" (left in the rebase worktree for resolve)"); err != nil {
		return Result{}, err
	}
	return Result{Outcome: OutcomeConflict, ConflictCommit: short, WorkDir: req.WorkDir, BaseHead: req.BaseHead}, nil
}

// markFailed emits merge_failed for a pick git aborted with no conflict to
// resolve (the elisp silent-failure sentinel).
func (e *Driver) markFailed(_ context.Context, req Request, cause string) (Result, error) {
	if err := req.Run.Failed(cause + " — the target was NEVER MODIFIED"); err != nil {
		return Result{}, err
	}
	return Result{Outcome: OutcomeFailed, WorkDir: req.WorkDir, BaseHead: req.BaseHead}, nil
}

// MarkQueued emits merge_queued for a merge the stitch orchestrator defers
// because another merge is already in flight against the same target
// worktree. The queue and its drain live at stitch; this method only records
// the transition so it is never invisible.
func (e *Driver) MarkQueued(ws, cause string) error {
	return e.emit.emit(ws, PhaseMergeQueued, cause)
}

// cherryPickBase computes the replay start point for incorporating
// targetBranch into dir's HEAD, mirroring agent-repl--cherry-pick-base:
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

// pickWentEmpty reports whether the cherry-pick parked in dir has NOTHING to
// resolve and NOTHING to commit: the change it carries is already in the
// target's tree, so git parked CHERRY_PICK_HEAD over an empty patch.
//
// IT ASKS GIT WHAT HAPPENED RATHER THAN PREDICTING IT. The patch-id probes that
// run BEFORE a pick (commitAlreadyIncorporated, rangeAlreadyIncorporated) can
// only recognize a change that landed as its own commit — a change absorbed into
// a LARGER commit on the target has a different patch-id, so the probes clear
// the pick and git then finds nothing to apply. Observed live on 2026-08-06: a
// test fix committed by the merge's own resolution turn carried the next
// commit's one-line change plus an unrelated file, the patch-id probe missed it,
// the pick went empty, and the empty pick was reported as a conflict.
//
// The two conditions are both necessary. Zero unmerged paths distinguishes an
// empty pick from a real conflict; an index identical to HEAD distinguishes it
// from a conflict a resolver has already staged, which `--continue` must commit
// rather than skip.
func (e *Driver) pickWentEmpty(ctx context.Context, dir string) (bool, error) {
	unmerged, err := e.gitString(ctx, dir, "diff", "--name-only", "--diff-filter=U")
	if err != nil {
		return false, err
	}
	if strings.TrimSpace(unmerged) != "" {
		return false, nil
	}
	// `--quiet` exits 0 when the staged tree matches HEAD and 1 when it does
	// not, which is the whole question, so the exit code IS the answer.
	exit, _, err := e.gitExit(ctx, dir, "diff", "--cached", "--quiet", "HEAD")
	if err != nil {
		return false, err
	}
	return exit == 0, nil
}

// finishEmptyPick completes an empty parked pick as an EMPTY COMMIT, with
// `git commit --allow-empty --no-edit` — the spelling git's own hint offers,
// which keeps CHERRY_PICK_HEAD's message and therefore its `-x` annotation.
//
// THE COMMIT IS WHAT MAKES THE ACCOUNTING DURABLE, and it is why `--skip` is
// wrong here. The replay derives its remaining work from git alone, by advancing
// cherryPickBase past every `-x` annotation on the rebased line, so a skipped
// commit leaves no trace and the very next re-entry — a resume, a test fix —
// plans it again. For a replay that went empty because the change is already
// present that is merely wasted; for one emptied by a resolution that dropped
// the change it re-plays a commit that then conflicts for real, which is an
// endless loop rather than a merge. The empty commit records "accounted for,
// carried no change" in the only place the replay reads.
//
// A FAILING FINISH IS A HARD ERROR. It is the sole exit from a state no resolve
// and no resume can clear, so a finish that did not take leaves the rebase
// worktree wedged with nothing able to advance it — the failure must reach the
// caller loudly rather than fall through to a conflict park that cannot be
// honored.
func (e *Driver) finishEmptyPick(ctx context.Context, req Request, short, progress string) error {
	e.logf("merge: rebase replay of %s (%s) went EMPTY — it carries no change against the rebased line, recording it as an empty commit {ws=%s work=%s}",
		short, progress, req.Name, req.WorkDir)
	exit, out, err := e.gitExit(ctx, req.WorkDir,
		"-c", "core.editor=true", "commit", "--allow-empty", "--no-edit")
	if err != nil {
		return err
	}
	if exit != 0 {
		return fmt.Errorf("merge: `git commit --allow-empty` for the empty replay of %s exited %d in %s: %s",
			short, exit, req.WorkDir, dlog.Clamp(out, 400))
	}
	stillParked, err := e.cherryPickInProgress(ctx, req.WorkDir)
	if err != nil {
		return err
	}
	if stillParked {
		return fmt.Errorf("merge: `git commit --allow-empty` for the empty replay of %s left CHERRY_PICK_HEAD parked in %s",
			short, req.WorkDir)
	}
	return nil
}

// staleSequencerState reports whether dir carries sequencer residue WITHOUT a
// parked CHERRY_PICK_HEAD. That exact state — a multi-commit pick interrupted
// between commits, or an abandoned pick's leftovers — makes git refuse every
// new cherry-pick with an opaque "already in progress" exit 128, and nothing
// can resume it (there is no conflicted commit to resolve). Observed live: an
// abandoned merge's sequencer wedged every later merge.
//
// It is DELIBERATELY narrower than "any in-progress pick": a target parked on
// a real conflict (CHERRY_PICK_HEAD present) must NOT be refused, because the
// boot-time replay of a durable merge re-enters exactly that target and
// re-parks its conflict — that is how a conflicted merge survives a daemon
// bounce.
func (e *Driver) staleSequencerState(ctx context.Context, dir string) (bool, error) {
	inProgress, err := e.cherryPickInProgress(ctx, dir)
	if err != nil || inProgress {
		return false, err
	}
	seqDir, err := e.gitString(ctx, dir, "rev-parse", "--git-path", "sequencer")
	if err != nil {
		return false, err
	}
	if !filepath.IsAbs(seqDir) {
		seqDir = filepath.Join(dir, seqDir)
	}
	if _, statErr := os.Stat(seqDir); statErr == nil {
		return true, nil
	} else if !errors.Is(statErr, os.ErrNotExist) {
		return false, fmt.Errorf("merge: probe sequencer state %s: %w", seqDir, statErr)
	}
	return false, nil
}

// mergeInProgress reports whether dir has an unfinished merge (MERGE_HEAD).
// `git merge` refuses to start another one, and this pipeline's whole target
// interaction is a single `git merge --no-ff` at the very end, so an unfinished
// merge there is a precondition failure rather than something to discover after
// a full rebase and every suite run.
func (e *Driver) mergeInProgress(ctx context.Context, dir string) (bool, error) {
	exit, _, err := e.gitExit(ctx, dir, "rev-parse", "--verify", "--quiet", "MERGE_HEAD")
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

// gitExit runs `git -C dir args...` and returns the exit code plus combined
// output. A non-zero exit is NOT an error (the caller classifies it); only a
// failure to spawn/run git (binary missing, dir gone) returns an error.
// Mirrors the exit-code role of agent-repl--git-exit-code.
func (e *Driver) gitExit(ctx context.Context, dir string, args ...string) (int, string, error) {
	cmd := gitexec.Command(ctx, dir, args...)
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
	cmd := gitexec.Command(ctx, dir, args...)
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
