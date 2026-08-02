package merge

import (
	"crypto/rand"
	"encoding/hex"
	"fmt"
	"sync"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
	"claude-repld/internal/dlog"
)

// This file is the merge pipeline's PHASE-LEVEL publishing spine.
//
// merge.Phase is one word, and one word cannot say which commit of how many is
// landing, what the before-action asked the agent to do, or why a run died.
// frontendv1.MergeStatus carries all of it, and merge.RunStatus is the object
// that fills it in: one per RUN, holding the run's identity and the commit
// context every phase after the plan shares, so no call site has to reassemble
// them and no two call sites can assemble them differently.
//
// A RUN, NOT A WORKSPACE. The id is minted when merge.Coordinator ADMITS the
// request and lives until that entry reaches a terminal outcome — across a
// daemon bounce included, because the queue entry carries the id and a boot
// replay resumes it. A retry submitted after a failure is a different run and
// gets a different id. A frontend keying progress on the workspace alone would
// blend two attempts into one nonsensical commit count.

// StatusSink receives every MergeStatus the pipeline publishes, together with
// the merge-axis transition it accompanies. The SSM implements it at stitch;
// nothing here imports internal/ssm.
//
// IT IS ONE CALL, NOT TWO. The axis row and the status describe the same event,
// and letting a caller record one without the other is how a frontend ends up
// rendering a phase word that disagrees with the progress beneath it.
type StatusSink interface {
	RecordMergeStatus(ws string, phase Phase, cause string, status *frontendv1.MergeStatus) error
}

// statusEmitter binds a StatusSink to the pipeline's logger, exactly as
// stateEmitter binds a StateSink: every publication is loud-logged with its
// workspace, run, phase and cause before it hits the sink, and a sink failure is
// loud-logged and returned rather than dropped.
type statusEmitter struct {
	sink StatusSink
	logf dlog.Logf
}

func (e *statusEmitter) emit(ws string, phase Phase, cause string, status *frontendv1.MergeStatus) error {
	if !validPhases[phase] {
		e.logf("merge: status REFUSING unknown phase {ws=%s phase=%s cause=%s}", ws, phase, cause)
		return fmt.Errorf("merge: unknown status phase %q for workspace %q", phase, ws)
	}
	if status.GetPhase() == nil {
		// A MergeStatus with no phase set describes nothing. It is a construction
		// bug at the call site, and recording it would put an empty status on the
		// wire that every frontend has to special-case.
		e.logf("merge: status REFUSING an unset phase oneof {ws=%s phase=%s cause=%s run=%s}", ws, phase, cause, status.GetRunId())
		return fmt.Errorf("merge: status for %q phase %s carries no phase payload", ws, phase)
	}
	e.logf("merge: status {ws=%s run=%s phase=%s cause=%s phase_started_at_ms=%d updated_at_ms=%d}",
		ws, status.GetRunId(), phase, cause, status.GetPhaseStartedAtMs(), status.GetUpdatedAtMs())
	if err := e.sink.RecordMergeStatus(ws, phase, cause, status); err != nil {
		e.logf("merge: status SINK FAILED {ws=%s run=%s phase=%s cause=%s}: %v", ws, status.GetRunId(), phase, cause, err)
		return fmt.Errorf("merge: record %s status for %q: %w", phase, ws, err)
	}
	return nil
}

// CommitPlan is the cherry-pick plan a run computed, in replay order. It is
// computed AFTER the before-action, because the before-action may create the
// very commits the plan has to carry.
type CommitPlan struct {
	// Commits are the plan's commits, oldest first. Each carries the short sha
	// and the subject line a frontend renders.
	Commits []PlannedCommit
}

// PlannedCommit is one commit of a CommitPlan.
type PlannedCommit struct {
	// SHA is the FULL sha, which is what the replay picks by.
	SHA string
	// Short is the abbreviated sha the status and the logs carry.
	Short string
	// Subject is the commit's subject line.
	Subject string
}

// RunStatus publishes one merge run's phase-level status.
//
// IT OWNS THE TWO TIMESTAMPS, and the distinction between them is the whole
// reason it is an object rather than a helper function:
//
//   - phase_started_at_ms moves ONLY when the phase changes, so a frontend can
//     render "cherry-picking for 40s" without the figure resetting every time a
//     commit lands.
//   - updated_at_ms moves on EVERY publication, and is forced strictly
//     increasing within the run, so two statuses sharing a phase are still
//     orderable. A clock that does not advance between two ticks would
//     otherwise publish two indistinguishable statuses.
//
// It also owns the commit context (total / landed / current), which
// cherry_picking, testing, conflict, merged and failed all report. Holding it in
// one place is what keeps a `testing` status from disagreeing with the
// `cherry_picking` status that immediately preceded it.
type RunStatus struct {
	emit      *statusEmitter
	logf      dlog.Logf
	runID     string
	workspace string
	now       func() int64

	mu               sync.Mutex
	phase            Phase
	phaseStartedAtMs int64
	updatedAtMs      int64
	commitsTotal     int32
	commitsLanded    int32
	currentShort     string
	currentSubject   string
}

// NewRunStatus mints a run and its publisher. now returns unix millis and is
// injectable so a test can pin both timestamps.
func NewRunStatus(sink StatusSink, logf dlog.Logf, workspace string, now func() int64) (*RunStatus, error) {
	return newRunStatus(sink, logf, workspace, now, newRunID())
}

// ResumeRunStatus rebuilds the publisher for a run that ALREADY HAS AN IDENTITY:
// the durable queue entry a boot replays carries the id its admission published
// under, and the resumed run must keep publishing under it.
//
// ONLY THE NAME IS RESTORED, never the progress. The commit cursor and the phase
// clock start clean, because the process that was advancing them is gone and its
// figures describe work this one has not done. What the caller gets is a run a
// frontend can still recognize, reporting what the resumed drain actually
// observes.
func ResumeRunStatus(sink StatusSink, logf dlog.Logf, workspace string, now func() int64, runID string) (*RunStatus, error) {
	if runID == "" {
		return nil, fmt.Errorf("merge: resuming a RunStatus needs the run id it was admitted under")
	}
	return newRunStatus(sink, logf, workspace, now, runID)
}

func newRunStatus(sink StatusSink, logf dlog.Logf, workspace string, now func() int64, runID string) (*RunStatus, error) {
	switch {
	case sink == nil:
		return nil, fmt.Errorf("merge: RunStatus needs a StatusSink")
	case logf == nil:
		return nil, fmt.Errorf("merge: RunStatus needs a Logf")
	case workspace == "":
		return nil, fmt.Errorf("merge: RunStatus needs a workspace")
	case now == nil:
		return nil, fmt.Errorf("merge: RunStatus needs a clock")
	}
	return &RunStatus{
		emit:      &statusEmitter{sink: sink, logf: logf},
		logf:      logf,
		runID:     runID,
		workspace: workspace,
		now:       now,
	}, nil
}

// RunID is the run's identity, stable for the whole run.
func (r *RunStatus) RunID() string { return r.runID }

// SetPlan records the cherry-pick plan's size. Every later phase reports
// commits_total from here, so the figure a frontend renders comes from the plan
// the run is actually executing rather than from whatever each call site
// happened to know.
func (r *RunStatus) SetPlan(plan CommitPlan) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.commitsTotal = int32(len(plan.Commits))
	r.commitsLanded = 0
	r.logf("merge: run plan {ws=%s run=%s commits_total=%d}", r.workspace, r.runID, r.commitsTotal)
}

// Enqueued publishes the run's ADMISSION: the `enqueued` status arm, carrying
// the place the entry landed at on its repository's queue.
//
// THE ARM AND THE AXIS TOKEN ARE NOT THE SAME WORD, which is why the caller
// passes the token. Every admission is `enqueued` to a frontend — the run is on
// a queue, at some position, and that is what the arm says — while the coarse
// merge axis distinguishes the two admissions it has always distinguished: a
// merge admitted at the HEAD stays at `merge_enqueuing` until the cherry-pick
// itself moves it to `merging`, and only one deferred behind another merge is
// `merge_queued`. Publishing `merge_queued` for a head admission would tell every
// merge_phase reader that a merge starting immediately is waiting on something.
//
// Any other token is refused rather than published: it would put an `enqueued`
// arm on a phase that has nothing to do with admission.
func (r *RunStatus) Enqueued(phase Phase, position, depth int32, cause string) error {
	if phase != PhaseMergeEnqueuing && phase != PhaseMergeQueued {
		r.logf("merge: status REFUSING an enqueued arm on a non-admission phase {ws=%s run=%s phase=%s}", r.workspace, r.runID, phase)
		return fmt.Errorf("merge: enqueued status for %q needs %s or %s, got %q", r.workspace, PhaseMergeEnqueuing, PhaseMergeQueued, phase)
	}
	return r.publish(phase, cause, func(s *frontendv1.MergeStatus) {
		s.Phase = &frontendv1.MergeStatus_Enqueued{Enqueued: &frontendv1.MergeStatusEnqueued{
			Position: position,
			Depth:    depth,
		}}
	})
}

// BeforeAction publishes the before_action phase with the recorded prompt.
func (r *RunStatus) BeforeAction(prompt, cause string) error {
	return r.publish(PhaseMergeBeforeAction, cause, func(s *frontendv1.MergeStatus) {
		s.Phase = &frontendv1.MergeStatus_BeforeAction{BeforeAction: &frontendv1.MergeStatusBeforeAction{
			Prompt: prompt,
		}}
	})
}

// CherryPicking publishes the cherry_picking phase for the commit about to be
// picked, and records it as the run's current commit.
func (r *RunStatus) CherryPicking(commit PlannedCommit, cause string) error {
	r.mu.Lock()
	r.currentShort, r.currentSubject = commit.Short, commit.Subject
	r.mu.Unlock()
	return r.PickingCurrent(cause)
}

// PickingCurrent republishes the cherry_picking phase for whatever the run's
// current commit already is. It serves the two moments that are picking without
// selecting a new commit: the run opening (nothing is current yet, and the
// counts are honestly zero until the plan is computed) and a resume continuing
// the commit a conflict parked on.
func (r *RunStatus) PickingCurrent(cause string) error {
	return r.publish(PhaseMerging, cause, func(s *frontendv1.MergeStatus) {
		s.Phase = &frontendv1.MergeStatus_CherryPicking{CherryPicking: &frontendv1.MergeStatusCherryPicking{
			CommitsTotal:   r.commitsTotal,
			CommitsLanded:  r.commitsLanded,
			CurrentSha:     r.currentShort,
			CurrentSubject: r.currentSubject,
		}}
	})
}

// CommitLanded records that the run's current commit is now on the target. It
// publishes nothing on its own: the very next phase (testing) reports the new
// count, and a publication here would put two statuses on the wire for one
// event.
func (r *RunStatus) CommitLanded() {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.commitsLanded++
}

// Testing publishes the testing phase for the commit that just landed. It
// carries the SAME commit context cherry_picking did, which is what lets a
// frontend render one progress figure across both.
func (r *RunStatus) Testing(cause string) error {
	return r.publish(PhaseMerging, cause, func(s *frontendv1.MergeStatus) {
		s.Phase = &frontendv1.MergeStatus_Testing{Testing: &frontendv1.MergeStatusTesting{
			CommitsTotal:   r.commitsTotal,
			CommitsLanded:  r.commitsLanded,
			CurrentSha:     r.currentShort,
			CurrentSubject: r.currentSubject,
		}}
	})
}

// Conflict publishes the conflict phase for a pick left paused in the target.
//
// sha is the FULL sha of the commit that collided, and the driver reads it as
// such: conflicted_sha is the one field a human uses to go find the collision,
// and an abbreviated sha is only unique in the repository that abbreviated it.
// An empty sha leaves the run's current commit standing, which is what a
// caller that already published the pick means.
func (r *RunStatus) Conflict(sha, cause string) error {
	r.mu.Lock()
	if sha != "" {
		r.currentShort = sha
	}
	r.mu.Unlock()
	return r.publish(PhaseMergeConflict, cause, func(s *frontendv1.MergeStatus) {
		s.Phase = &frontendv1.MergeStatus_Conflict{Conflict: &frontendv1.MergeStatusConflict{
			ConflictedSha:     r.currentShort,
			ConflictedSubject: r.currentSubject,
			CommitsTotal:      r.commitsTotal,
			CommitsLanded:     r.commitsLanded,
		}}
	})
}

// AfterAction publishes the after_action phase with the recorded prompt.
func (r *RunStatus) AfterAction(prompt, cause string) error {
	return r.publish(PhaseMergeAfterAction, cause, func(s *frontendv1.MergeStatus) {
		s.Phase = &frontendv1.MergeStatus_AfterAction{AfterAction: &frontendv1.MergeStatusAfterAction{
			Prompt: prompt,
		}}
	})
}

// Merged publishes the TERMINAL merged phase. afterActionErr is the after-action's
// failure, empty when it landed or when there was none: a failed after-action
// does not fail the run, because the commits are on the target either way and
// reporting `failed` would make the status lie about the tree.
func (r *RunStatus) Merged(afterActionErr, cause string) error {
	return r.publish(PhaseMerged, cause, func(s *frontendv1.MergeStatus) {
		s.Phase = &frontendv1.MergeStatus_Merged{Merged: &frontendv1.MergeStatusMerged{
			CommitsTotal:     r.commitsTotal,
			AfterActionError: afterActionErr,
		}}
	})
}

// Failed publishes the TERMINAL failed phase, carrying the commit context the
// run died with so a user can see how far it got.
func (r *RunStatus) Failed(cause string) error {
	return r.publish(PhaseMergeFailed, cause, func(s *frontendv1.MergeStatus) {
		s.Phase = &frontendv1.MergeStatus_Failed{Failed: &frontendv1.MergeStatusFailed{
			Cause:          cause,
			CommitsTotal:   r.commitsTotal,
			CommitsLanded:  r.commitsLanded,
			FailingSha:     r.currentShort,
			FailingSubject: r.currentSubject,
		}}
	})
}

// publish stamps the two timestamps and hands the status to the sink.
//
// phase_started_at_ms moves only on a phase CHANGE; updated_at_ms moves on every
// call and is forced strictly increasing, so a clock that does not advance
// between two ticks still produces two orderable statuses.
func (r *RunStatus) publish(phase Phase, cause string, fill func(*frontendv1.MergeStatus)) error {
	r.mu.Lock()
	now := r.now()
	if phase != r.phase {
		r.phase = phase
		r.phaseStartedAtMs = now
	}
	if now <= r.updatedAtMs {
		now = r.updatedAtMs + 1
	}
	r.updatedAtMs = now
	status := &frontendv1.MergeStatus{
		RunId:            r.runID,
		PhaseStartedAtMs: r.phaseStartedAtMs,
		UpdatedAtMs:      r.updatedAtMs,
	}
	fill(status)
	r.mu.Unlock()
	return r.emit.emit(r.workspace, phase, cause, status)
}

// newRunID mints a run identity. It is random rather than derived from the
// workspace and a clock: two runs of one workspace can start in the same
// millisecond after a fast retry, and a colliding id would blend them.
func newRunID() string {
	var b [12]byte
	if _, err := rand.Read(b[:]); err != nil {
		// crypto/rand failing is a broken machine, not a condition to degrade
		// through: a non-unique run id silently merges two runs' progress.
		panic(fmt.Sprintf("merge: minting a run id: %v", err))
	}
	return "run-" + hex.EncodeToString(b[:])
}
