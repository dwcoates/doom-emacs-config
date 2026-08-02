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
	// Suite runs the target repository's test suite after each cherry-picked
	// commit. Required, on the same footing as the rest: the whole point of
	// picking commit by commit is the gate between the picks, and a driver
	// constructed without one would silently land untested work on the target.
	// A target repository with no test entrypoint is the RUNNER's business to
	// report (SuiteResult.Skipped), not a reason to omit the dependency.
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
	// OutcomeTestFailed: a commit landed on the target and the repository's
	// test suite then failed. It is NOT terminal at the driver level — the
	// coordinator drives one resolution attempt through the workspace's own
	// session and continues, or fails the merge and rolls the target back.
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
	// PreMergeHead is the target's HEAD as it stood BEFORE this Merge landed
	// anything. It is the rollback point the coordinator resets to when the
	// test gate fails for good, and it is set by Merge on EVERY outcome so the
	// coordinator holds it no matter how the merge later develops.
	//
	// A Merge that is itself a boot-time REPLAY records the head it finds,
	// which already carries whatever the previous daemon landed before it died.
	// That is the honest rollback point for the attempt being made now: the
	// replay is a fresh merge from the target's current state, not a resumption
	// of the dead process's transaction.
	PreMergeHead string
	// FailingCommit is the short SHA of the commit whose landing broke the test
	// suite, set when Outcome is OutcomeTestFailed.
	FailingCommit string
	// TestFailureTail is the clamped tail of the failing suite's output, set
	// when Outcome is OutcomeTestFailed. It travels into the resolution prompt
	// and into the merge_failed cause.
	TestFailureTail string
}

// cherryPickAnnotationRE matches the "(cherry picked from commit <sha>)"
// annotation that `git cherry-pick -x` writes, mirroring
// agent-repl--extract-cherry-pick-shas.
var cherryPickAnnotationRE = regexp.MustCompile(`\(cherry picked from commit ([0-9a-f]{40})\)`)

// Merge runs the cherry-pick driver for req against a clean precondition and
// returns the Result. It emits an opening `merging`, then one further
// `merging` per landed commit (naming the commit and its position in the
// range), then exactly one of merged / merge_conflict / merge_failed — or
// nothing further when the run ends on OutcomeTestFailed, which is the
// coordinator's to classify.
//
// A failed precondition (bad request, dirty source worktree, missing branch)
// aborts BEFORE any transition is emitted, so a rejected merge leaves the
// workspace's state exactly as it was — no half-transition.
//
// A conflict is LEFT IN THE TARGET TREE (never aborted); the caller resumes
// it via Resume once the conflict has been resolved.
func (e *Driver) Merge(ctx context.Context, req Request) (Result, error) {
	if err := req.validateRun(); err != nil {
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
	// Stale sequencer residue (no parked commit) refuses every new pick with
	// an opaque exit 128 and can never be resumed, so it is rejected here as
	// a NAMED precondition — before any transition — with the operator's way
	// out in the message. A target parked on a REAL conflict is deliberately
	// not refused: the bounce-replay path re-enters it to re-park.
	stale, err := e.staleSequencerState(ctx, req.TargetDir)
	if err != nil {
		return Result{}, err
	}
	if stale {
		return Result{}, fmt.Errorf("merge: target %s has an unfinished cherry-pick (stale sequencer state, no conflicted commit) — run `git -C %s cherry-pick --quit` before merging %q",
			req.TargetDir, req.TargetDir, req.Name)
	}

	// The rollback point. It is read BEFORE the first transition so it reflects
	// the target exactly as the merge found it.
	preHead, err := e.gitString(ctx, req.TargetDir, "rev-parse", "HEAD")
	if err != nil {
		return Result{}, err
	}

	// NOTHING IS PUBLISHED HERE. The opening used to publish a cherry_picking
	// status before the plan was computed, which put commits_total=0 and an empty
	// current_sha on the wire as the run's FIRST word about its picks — a
	// progress bar reading 0 of 0 for a merge that is about to land three
	// commits, and the only cherry_picking frame a frontend saw until the next
	// one replaced it. A cherry_picking status exists to say which commit of how
	// many is landing, so it is published from inside the loop, once the plan is
	// the run's denominator and a commit is genuinely in flight.
	base, err := e.cherryPickBase(ctx, req.TargetDir, req.SourceBranch)
	if err != nil {
		return Result{}, err
	}
	res, err := e.pickLoop(ctx, req, base, false)
	if err != nil {
		return Result{}, err
	}
	res.PreMergeHead = preHead
	return res, nil
}

// pickLoop replays base..SourceBranch onto the target ONE COMMIT AT A TIME,
// running the repository's test suite after each commit lands.
//
// COMMIT BY COMMIT IS THE WHOLE POINT. A single `cherry-pick -x base..branch`
// can only be tested once, at the end, which tells a user that "the merge broke
// the suite" without saying which commit did it and leaves a resolution attempt
// nothing narrower than the whole range to reason about. Picking individually
// makes each landing testable, names the culprit, and gives the conflict path
// exactly the same shape it always had (a conflict on commit N parks with
// commits 1..N-1 already on the target).
//
// IT IS RESTARTABLE BY CONSTRUCTION. The loop derives its work from git alone —
// the cherry-pick base (which advances past every `-x` annotation already on the
// target) plus a per-commit patch-id probe — so re-entering it after a resume, a
// test fix, or a whole daemon bounce skips what already landed rather than
// replaying it. That is what makes the durable queue's boot replay a no-op for
// the commits the previous daemon got through.
//
// MERGE COMMITS ARE FLATTENED. `--no-merges` drops them: a merge commit carries
// no patch of its own, and every commit on both of its sides is already in the
// range and picked individually. The pre-per-commit driver instead handed the
// whole range to git, which refuses a range containing a merge outright, so a
// branch with an internal merge used to fail the whole attempt.
// landedEarlier tells the loop whether work has ALREADY landed on the target
// during this merge (a resolved conflict, a committed test fix). It is what
// keeps a re-entered loop that finds nothing left to do from reporting the
// merge as a no-op when it plainly was not one.
func (e *Driver) pickLoop(ctx context.Context, req Request, base string, landedEarlier bool) (Result, error) {
	rng := base + ".." + req.SourceBranch
	plan, err := e.plan(ctx, req.TargetDir, rng)
	if err != nil {
		return Result{}, err
	}
	// THE PLAN IS THE RUN'S DENOMINATOR. Every phase from here on reports
	// commits_total from it, so the figure a frontend renders is the plan being
	// executed rather than whatever each call site happened to count.
	req.Run.SetPlan(plan)
	commits := plan.Commits
	if len(commits) == 0 {
		// The workspace's contribution is already on the target by ancestry —
		// a successful no-op merge.
		e.logf("merge: range %s empty — nothing left to pick {ws=%s}", rng, req.Name)
		return e.finalizeMerged(ctx, req, !landedEarlier)
	}

	landed := 0
	for i, commit := range commits {
		progress := fmt.Sprintf("%d/%d", i+1, len(commits))
		sha, short := commit.SHA, commit.Short

		already, err := e.commitAlreadyIncorporated(ctx, req.TargetDir, sha)
		if err != nil {
			return Result{}, err
		}
		if already {
			// Picking it would go empty, which wedges git's sequencer on a
			// state only `--skip` or `--quit` clears. Skipping it here keeps
			// that state unrepresentable rather than recoverable.
			e.logf("merge: SKIPPING %s (%s), already on the target by patch-id {ws=%s}", short, progress, req.Name)
			// IT COUNTS AS LANDED. The commit is on the target — by this run or by
			// the one the previous daemon died in the middle of — and a progress
			// figure that skipped it would count down toward a total it could
			// never reach.
			req.Run.CommitLanded()
			continue
		}

		// The phase is published BEFORE the pick, not after: the pick is the slow
		// part, and a user watching a merge should see which commit is landing
		// while it lands rather than only once it has.
		if err := req.Run.CherryPicking(commit, "cherry-picking "+progress+": "+short); err != nil {
			return Result{}, err
		}

		// A non-zero exit is not a spawn error; the exit code and the presence
		// of CHERRY_PICK_HEAD classify the outcome.
		exit, out, err := e.gitExit(ctx, req.TargetDir, "cherry-pick", "-x", sha)
		if err != nil {
			return Result{}, err
		}
		e.logf("merge: cherry-pick -x %s (%s) exit=%d {ws=%s} %s", short, progress, exit, req.Name, dlog.Clamp(out, 400))

		inProgress, err := e.cherryPickInProgress(ctx, req.TargetDir)
		if err != nil {
			return Result{}, err
		}
		if inProgress {
			return e.markConflict(ctx, req)
		}
		if exit != 0 {
			// No CHERRY_PICK_HEAD. Distinguish the ALREADY-MERGED case (a prior
			// `cherry-pick -x` rewrote our commits under new SHAs, so the
			// SHA-keyed probes missed them) from a genuine failure via
			// `git cherry` (patch-id comparison) over the whole range.
			incorporated, err := e.rangeAlreadyIncorporated(ctx, req.TargetDir, base, req.SourceBranch)
			if err != nil {
				return Result{}, err
			}
			if incorporated {
				e.logf("merge: range %s already incorporated by patch-id {ws=%s}", rng, req.Name)
				return e.finalizeMerged(ctx, req, true)
			}
			return e.markFailed(ctx, req,
				fmt.Sprintf("cherry-pick of %s (%s) exited %d with no conflict to resolve", short, progress, exit))
		}
		landed++
		req.Run.CommitLanded()

		failure, err := e.gateOnSuite(ctx, req, progress+" after cherry-pick of "+short, short)
		if err != nil {
			return Result{}, err
		}
		if failure != nil {
			return *failure, nil
		}
	}
	return e.finalizeMerged(ctx, req, landed == 0 && !landedEarlier)
}

// gateOnSuite runs the target repository's test suite for the commit that just
// landed and reports a test failure as a Result, or nil when the merge may
// continue.
//
// The `merging` transition is emitted BEFORE the run, not after, because the
// run is the slow part: a user watching a merge should see "testing 3/7 after
// cherry-pick of abc123" while it happens rather than only once it is over.
//
// A SKIPPED suite continues the merge. That is the documented behavior for a
// target repository that declares no test entrypoint — the merge subsystem
// serves repositories that have no agent-repl suite — and it is loud-logged by
// the runner AND here, because a merge landing untested is precisely the fact a
// user later needs to find in the log.
func (e *Driver) gateOnSuite(ctx context.Context, req Request, progress, short string) (*Result, error) {
	// The testing phase carries the SAME commit context the cherry_picking phase
	// just did — the run holds it, so the two cannot disagree — which is what
	// lets a frontend render one progress figure across both.
	if err := req.Run.Testing("testing " + progress); err != nil {
		return nil, err
	}
	sr, err := e.suite.RunSuite(ctx, req.TargetDir)
	if err != nil {
		e.logf("merge: test gate UNRUNNABLE after cherry-pick of %s {ws=%s target=%s}: %v", short, req.Name, req.TargetDir, err)
		return nil, fmt.Errorf("merge: test gate for %q after cherry-picking %s: %w", req.Name, short, err)
	}
	if sr.Skipped {
		e.logf("merge: test gate SKIPPED after cherry-pick of %s {ws=%s target=%s}: %s — this merge lands UNTESTED",
			short, req.Name, req.TargetDir, sr.Reason)
		return nil, nil
	}
	if sr.Passed {
		e.logf("merge: test gate PASSED after cherry-pick of %s (%s) {ws=%s}", short, progress, req.Name)
		return nil, nil
	}
	e.logf("merge: test gate FAILED after cherry-pick of %s (%s) {ws=%s target=%s} tail:\n%s",
		short, progress, req.Name, req.TargetDir, sr.Tail)
	return &Result{Outcome: OutcomeTestFailed, FailingCommit: short, TestFailureTail: sr.Tail}, nil
}

// Resume continues a cherry-pick that a human (or the workspace's own agent)
// has resolved in the target
// worktree (the resolve-and-continue handoff arrives as a FrontendCommand
// with conflict_resolved_continue at stitch). It stages the resolved files
// (`git add -u`) and runs `git cherry-pick --continue`, mirroring
// agent-repl--continue-cherry-pick-after-resolve.
//
// It emits merging (the conflict is clearing), runs the test gate on the
// commit the continue landed, and then RE-ENTERS the per-commit replay for
// whatever is left of the range — so a resume is not the end of a merge, it is
// the middle of one.
func (e *Driver) Resume(ctx context.Context, req Request) (Result, error) {
	if err := req.validateRun(); err != nil {
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

	if err := req.Run.PickingCurrent("resuming cherry-pick after resolve"); err != nil {
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
	if exit != 0 {
		return e.markFailed(ctx, req, fmt.Sprintf("cherry-pick --continue exited %d with no conflict to resolve", exit))
	}

	// The resolved commit is now the target's HEAD, and it is subject to the
	// same gate every other landing is: a conflict a human (or an agent) hand
	// resolved is exactly the kind of landing that breaks a suite.
	resumed, err := e.gitString(ctx, req.TargetDir, "rev-parse", "--short", "HEAD")
	if err != nil {
		return Result{}, err
	}
	failure, err := e.gateOnSuite(ctx, req, "the resumed cherry-pick of "+resumed, resumed)
	if err != nil {
		return Result{}, err
	}
	if failure != nil {
		return *failure, nil
	}
	return e.continueRange(ctx, req)
}

// ContinueAfterTestFix commits whatever the resolution turn staged in the
// target worktree and re-runs the suite, then continues the per-commit replay.
//
// THE FIX LANDS AS A FOLLOW-UP COMMIT, NOT AN AMEND. Both would work, and the
// follow-up is the mechanically simpler of the two:
//   - It leaves the cherry-picked commit's SHA and its `-x` annotation exactly
//     as `cherry-pick -x` wrote them. Those annotations are what
//     cherryPickBase reads to know what already landed, so an amend would
//     rewrite the one record the replay's restartability depends on.
//   - It needs no interaction with the sequencer or with the commit message of
//     a commit the driver did not author.
//   - It keeps the target's history honest: the pick and the fix the pick
//     required are two different pieces of work by two different authors.
//
// A resolution turn that staged NOTHING is not an error here. It is reported
// loudly and the suite is re-run anyway, which will fail again and take the
// merge down the rollback path — the honest outcome for an agent that did not
// fix anything.
func (e *Driver) ContinueAfterTestFix(ctx context.Context, req Request, failingCommit string) (Result, error) {
	if err := req.validateRun(); err != nil {
		return Result{}, err
	}
	if failingCommit == "" {
		return Result{}, fmt.Errorf("merge: ContinueAfterTestFix for %q needs the failing commit", req.Name)
	}
	e.logf("merge: ContinueAfterTestFix start {ws=%s key=%s failing=%s target=%s}", req.Name, req.Workspace, failingCommit, req.TargetDir)

	if err := e.commitTestFix(ctx, req, failingCommit); err != nil {
		return Result{}, err
	}
	failure, err := e.gateOnSuite(ctx, req, "again after the resolution attempt on "+failingCommit, failingCommit)
	if err != nil {
		return Result{}, err
	}
	if failure != nil {
		return *failure, nil
	}
	return e.continueRange(ctx, req)
}

// commitTestFix stages everything the resolution turn touched and commits it as
// a follow-up commit. A clean tree is loud-logged and left alone.
func (e *Driver) commitTestFix(ctx context.Context, req Request, failingCommit string) error {
	if exit, out, err := e.gitExit(ctx, req.TargetDir, "add", "-A"); err != nil {
		return err
	} else if exit != 0 {
		return fmt.Errorf("merge: `git add -A` exited %d in %s: %s", exit, req.TargetDir, dlog.Clamp(out, 400))
	}
	staged, _, err := e.gitExit(ctx, req.TargetDir, "diff", "--cached", "--quiet")
	if err != nil {
		return err
	}
	if staged == 0 {
		e.logf("merge: test fix staged NOTHING {ws=%s failing=%s target=%s} — the resolution turn changed no files; the suite is re-run as-is",
			req.Name, failingCommit, req.TargetDir)
		return nil
	}
	msg := fmt.Sprintf("fix tests after cherry-pick of %s (%s)", failingCommit, req.Name)
	exit, out, err := e.gitExit(ctx, req.TargetDir, "-c", "core.editor=true", "commit", "-m", msg)
	if err != nil {
		return err
	}
	if exit != 0 {
		return fmt.Errorf("merge: committing the test fix for %q exited %d in %s: %s",
			req.Name, exit, req.TargetDir, dlog.Clamp(out, 400))
	}
	e.logf("merge: test fix COMMITTED {ws=%s failing=%s target=%s} %s", req.Name, failingCommit, req.TargetDir, dlog.Clamp(out, 200))
	return nil
}

// continueRange re-enters the per-commit replay for whatever is left of the
// range. The base is recomputed rather than carried, which is what lets the
// resumed loop skip every commit that already landed.
func (e *Driver) continueRange(ctx context.Context, req Request) (Result, error) {
	base, err := e.cherryPickBase(ctx, req.TargetDir, req.SourceBranch)
	if err != nil {
		return Result{}, err
	}
	return e.pickLoop(ctx, req, base, true)
}

// Rollback resets the target worktree to head, undoing every commit this merge
// landed.
//
// WHY A MERGE THAT FAILS ITS TEST GATE ROLLS BACK. The target worktree is what
// every other workspace cuts from and merges into, so leaving it carrying
// commits that break its suite converts one workspace's failure into everyone
// else's. Nothing is lost by resetting: the source branch still holds every
// commit, and the merge can be retried once the work is fixed on that branch.
// The alternative — leaving the broken state in tree for a human to look at —
// buys a diagnostic convenience the daemon's log already provides (the failing
// commit and the suite's output tail are both recorded) at the price of a
// poisoned shared trunk.
//
// It deliberately does NOT `git clean`: untracked files in the target may be a
// human's own work, and a merge failure is no license to delete them.
func (e *Driver) Rollback(ctx context.Context, req Request, head string) error {
	if err := req.validate(); err != nil {
		return err
	}
	if head == "" {
		return fmt.Errorf("merge: rollback of %q needs the pre-merge HEAD", req.Name)
	}
	exit, out, err := e.gitExit(ctx, req.TargetDir, "reset", "--hard", head)
	if err != nil {
		return err
	}
	if exit != 0 {
		return fmt.Errorf("merge: rolling %s back to %s exited %d: %s", req.TargetDir, head, exit, dlog.Clamp(out, 400))
	}
	e.logf("merge: ROLLED BACK {ws=%s target=%s head=%s} %s", req.Name, req.TargetDir, head, dlog.Clamp(out, 200))
	return nil
}

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
	// THE TERMINAL `merged` STATUS IS NOT PUBLISHED HERE, and its absence is the
	// contract rather than an omission. The run is not over when the last commit
	// lands: the workspace's after-action still has to run, and its failure rides
	// on the terminal status as after_action_error. Publishing `merged` from here
	// put the run's terminal word on the wire BEFORE the after_action phase
	// existed, so every frontend saw the merge finish and then watched a phase
	// begin after it — and the after-action's error reached a SECOND merged
	// status nothing was still reading. merge.Coordinator publishes it once, with
	// the action's outcome already on it (coordinator.go completeMergedRun).
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
	// THE STATUS CARRIES THE FULL SHA, the logs and the Result carry the short
	// one. A conflict's entire purpose is to hand work to a human, and an
	// abbreviated sha is ambiguous by construction: it is the prefix git happened
	// to consider unique in THIS repository at THIS moment, so a frontend cannot
	// use it to address the commit anywhere else. The short form stays where it
	// has always been — the cause text a human reads, and ConflictCommit, which
	// the resume path matches on.
	full, err := e.gitString(ctx, req.TargetDir, "rev-parse", "CHERRY_PICK_HEAD")
	if err != nil {
		return Result{}, err
	}
	if err := req.Run.Conflict(full, "conflict cherry-picking "+short+" (left in tree for resolve)"); err != nil {
		return Result{}, err
	}
	return Result{Outcome: OutcomeConflict, ConflictCommit: short}, nil
}

// markFailed emits merge_failed for a pick git aborted with no conflict to
// resolve (the elisp silent-failure sentinel).
func (e *Driver) markFailed(_ context.Context, req Request, cause string) (Result, error) {
	if err := req.Run.Failed(cause); err != nil {
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
