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

// StatusWatermarkSink persists a run's last published updated_at_ms somewhere
// that outlives the process. merge.FileQueue implements it against the run's
// durable queue entry; the interface exists so RunStatus needs no knowledge of
// the queue, its repo keys, or its files.
//
// THE RUNSTATUS IS THE ONLY WRITER. The watermark is a floor a resume seeds
// above, and a second writer could only lower it — which is precisely the
// regression the field exists to make impossible.
type StatusWatermarkSink interface {
	RecordStatusWatermark(runID string, updatedAtMs int64) error
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

	mu sync.Mutex
	// watermark persists updatedAtMs so the run RESUMED from this one's durable
	// entry can seed above it. Nil for a publisher with no durable entry behind
	// it (a unit fixture), which simply has nothing to resume.
	watermark StatusWatermarkSink
	// arm is the oneof case of the LAST published status, and it — not the
	// coarse merge axis — is what phaseStartedAtMs is keyed on. See publish.
	arm              string
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
	return newRunStatus(sink, logf, workspace, now, newRunID(), 0)
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
//
// THE CURSOR IS RE-ESTABLISHED FROM GIT, NOT REMEMBERED: the resumed driver
// reads the range the workspace contributes and what is left of it, and hands
// both to ResumePlan before it publishes anything. A clean cursor is the
// starting point of that reconstruction, not the figure a frontend ends up
// rendering.
//
// THE ONE FIGURE THAT IS RESTORED IS THE UPDATED_AT_MS WATERMARK, and it is
// restored because it is not progress — it is the run's place in a total order
// receivers already read. Seeding the clock from now() alone left that order at
// the mercy of the wall clock: a backwards step across the bounce (an ntp
// correction, a suspended laptop, a container's clock settling) published a
// resumed status BELOW the pre-bounce ones, and a receiver ordering on
// updated_at_ms then renders the dead process's progress as the newer word.
//
// watermarkMs is the highest updated_at_ms this run published before the bounce,
// read off the durable entry. The resumed clock starts at max(now, watermark+1),
// so the regression is not unlikely, it is unrepresentable. Zero means the run
// published nothing (or predates the durable field), which needs no floor.
func ResumeRunStatus(sink StatusSink, logf dlog.Logf, workspace string, now func() int64, runID string, watermarkMs int64) (*RunStatus, error) {
	if runID == "" {
		return nil, fmt.Errorf("merge: resuming a RunStatus needs the run id it was admitted under")
	}
	if watermarkMs < 0 {
		return nil, fmt.Errorf("merge: resuming run %s with a negative status watermark %d", runID, watermarkMs)
	}
	return newRunStatus(sink, logf, workspace, now, runID, watermarkMs)
}

func newRunStatus(sink StatusSink, logf dlog.Logf, workspace string, now func() int64, runID string, watermarkMs int64) (*RunStatus, error) {
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
		// The watermark IS the previous updatedAtMs as far as publish is
		// concerned: its existing "force strictly increasing" step then yields
		// max(now, watermark+1) with no second rule to keep in step with it.
		updatedAtMs: watermarkMs,
	}, nil
}

// RunID is the run's identity, stable for the whole run.
func (r *RunStatus) RunID() string { return r.runID }

// BindWatermark attaches the durable store this run records its updated_at_ms
// watermark into. The coordinator calls it once the run's queue entry exists —
// which is after the durable Publish for a fresh admission, and immediately for
// a resumed one.
//
// IT BINDS ONCE. Two sinks would mean two durable floors for one run, and the
// resume would seed from whichever the queue happened to read. A rebind is a
// wiring bug, so it is refused rather than accepted as the newer intent.
func (r *RunStatus) BindWatermark(w StatusWatermarkSink) error {
	if w == nil {
		return fmt.Errorf("merge: binding a nil watermark sink to run %s", r.runID)
	}
	r.mu.Lock()
	defer r.mu.Unlock()
	if r.watermark != nil {
		return fmt.Errorf("merge: run %s already has a watermark sink bound", r.runID)
	}
	r.watermark = w
	return nil
}

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

// ResumePlan re-establishes the commit cursor of a run whose replay is being
// RE-ENTERED: a conflict a human resolved and continued, a committed test fix,
// a queue entry a boot replayed.
//
// SetPlan CANNOT SERVE THIS MOMENT. A re-entered replay recomputes its
// cherry-pick base, which has advanced past everything already on the target, so
// the plan it reads is only what is LEFT. Recording that as the run's plan makes
// commits_total SHRINK as the run progresses, and a run re-entered after its
// last commit landed reads a plan of zero — which is how a `merged` status came
// to report commits_total=0 for a merge that plainly landed something.
//
// So the two figures are reconstructed rather than replaced:
//
//   - total is the run's OWN total wherever it still has one. The run was
//     admitted to land a particular range and that range does not change
//     because part of it is now on the target. Only a publisher that lost its
//     cursor with the process that held it (total zero, which is what
//     ResumeRunStatus hands back) falls to full — the workspace's whole
//     contribution — because it has nothing else to count from.
//   - landed is whatever total no longer has left to do. It is read off git
//     rather than remembered, so it is right across a bounce as well as within
//     a process.
//
// A remainder LARGER than the total is refused rather than clamped: the caller
// handed two plans that cannot describe one run, and publishing the nearest
// sane-looking pair would put a fabricated progress figure on the wire.
func (r *RunStatus) ResumePlan(full, remaining CommitPlan) error {
	r.mu.Lock()
	defer r.mu.Unlock()
	total := r.commitsTotal
	if total == 0 {
		total = int32(len(full.Commits))
	}
	left := int32(len(remaining.Commits))
	if left > total {
		return fmt.Errorf("merge: run %s for %q has %d commits left of a %d-commit plan", r.runID, r.workspace, left, total)
	}
	r.commitsTotal, r.commitsLanded = total, total-left
	r.logf("merge: run plan RESUMED {ws=%s run=%s commits_total=%d commits_landed=%d remaining=%d}",
		r.workspace, r.runID, r.commitsTotal, r.commitsLanded, left)
	return nil
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
// `merge_queued`. Publishing `merge_queued` for a head admission would resolve a
// render state saying a merge starting immediately is waiting on something.
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
// PHASE_STARTED_AT_MS IS KEYED ON THE PUBLISHED ARM, NOT ON THE MERGE AXIS.
// The axis is coarser than the status by design — cherry_picking and testing
// both ride PhaseMerging, because the render state the axis resolves has no
// use for the distinction — so keying the clock on it made
// cherry_picking -> testing a change of arm that claimed to have begun when the
// PREVIOUS arm did. A user watching "testing for 40s" was reading the age of the
// pick that preceded it. The arm is what a frontend renders, so the arm is what
// the phase clock has to track: any arm change restarts it, and a repeat of the
// same arm leaves it exactly where it was.
//
// updated_at_ms moves on every call and is forced strictly increasing, so a
// clock that does not advance between two ticks still produces two orderable
// statuses. phase_started_at_ms is stamped from that FORCED value rather than
// from the raw clock, which is what makes an arm change under a stopped clock
// still land strictly later than the arm it replaced.
//
// THE WATERMARK IS PERSISTED BEFORE THE STATUS IS EMITTED, and the clocks only
// move once it is. Emitting first would let a bounce land between the two and
// leave a published status ABOVE the durable floor its own resume seeds from —
// the exact regression the floor exists to prevent. A persist failure therefore
// refuses the publication and leaves the clocks untouched, exactly as an unset
// arm does: a status that never reaches the sink must not burn an updated_at_ms.
func (r *RunStatus) publish(phase Phase, cause string, fill func(*frontendv1.MergeStatus)) error {
	r.mu.Lock()
	status := &frontendv1.MergeStatus{RunId: r.runID}
	fill(status)
	arm := statusArm(status)
	if arm == armNone {
		// A fill that set no arm is a construction bug at the call site, and the
		// emitter refuses it. The clocks are left untouched on the way out: a
		// status that never reaches the sink must not burn an updated_at_ms, or
		// the run's stream develops a gap no receiver can account for.
		r.mu.Unlock()
		return r.emit.emit(r.workspace, phase, cause, status)
	}
	now := r.now()
	if now <= r.updatedAtMs {
		now = r.updatedAtMs + 1
	}
	if r.watermark != nil {
		if err := r.watermark.RecordStatusWatermark(r.runID, now); err != nil {
			r.mu.Unlock()
			r.logf("merge: status watermark FAILED {ws=%s run=%s phase=%s cause=%s updated_at_ms=%d}: %v — the status is NOT published, because one above an unrecorded floor is one a resume could publish beneath",
				r.workspace, r.runID, phase, cause, now, err)
			return fmt.Errorf("merge: record status watermark for %q run %s: %w", r.workspace, r.runID, err)
		}
	}
	r.updatedAtMs = now
	if arm != r.arm {
		r.arm = arm
		r.phaseStartedAtMs = now
	}
	status.PhaseStartedAtMs = r.phaseStartedAtMs
	status.UpdatedAtMs = r.updatedAtMs
	r.mu.Unlock()
	return r.emit.emit(r.workspace, phase, cause, status)
}

// The oneof arm names, as the proto declares them. They are the words a
// frontend switches on, and phase_started_at_ms is keyed on them.
const (
	armEnqueued      = "enqueued"
	armBeforeAction  = "before_action"
	armCherryPicking = "cherry_picking"
	armTesting       = "testing"
	armConflict      = "conflict"
	armAfterAction   = "after_action"
	armMerged        = "merged"
	armFailed        = "failed"
	// armNone is what an UNSET oneof reports as. It is never a phase: a status
	// exists precisely to say which phase a run is in.
	armNone = "<none>"
)

// statusArm reports which oneof arm a MergeStatus carries.
func statusArm(status *frontendv1.MergeStatus) string {
	switch status.GetPhase().(type) {
	case *frontendv1.MergeStatus_Enqueued:
		return armEnqueued
	case *frontendv1.MergeStatus_BeforeAction:
		return armBeforeAction
	case *frontendv1.MergeStatus_CherryPicking:
		return armCherryPicking
	case *frontendv1.MergeStatus_Testing:
		return armTesting
	case *frontendv1.MergeStatus_Conflict:
		return armConflict
	case *frontendv1.MergeStatus_AfterAction:
		return armAfterAction
	case *frontendv1.MergeStatus_Merged:
		return armMerged
	case *frontendv1.MergeStatus_Failed:
		return armFailed
	default:
		return armNone
	}
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
