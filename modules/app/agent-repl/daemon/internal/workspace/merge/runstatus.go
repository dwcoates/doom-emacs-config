package merge

import (
	"crypto/rand"
	"encoding/hex"
	"fmt"
	"sync"
	"time"

	"google.golang.org/protobuf/encoding/protojson"

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

// statusPublishBound is how long ONE status publication may spend inside the
// StatusSink before the pipeline stops waiting on it.
//
// IT IS A FAILURE BOUND, NOT A TUNED DELAY. The sink is the SSM, whose append
// serializes on a process-wide mutex shared with every other daemon subsystem;
// a publication therefore waits on work this package neither owns nor can see.
// The observed production shape was a drain blocked in completeMergedRun inside
// that sink with no bound at all: the merge had landed, the queue head was held,
// and nothing short of a daemon bounce moved it.
//
// A merge run must reach an observable update within two minutes, and this bound
// is a quarter of that so a publication that expires still leaves the run's
// terminal handling time to be seen. Expiring publishes nothing and returns an
// error, which is the SAFE direction: an unpublished terminal keeps its durable
// queue entry (keepForTerminalReplay), so the word is owed rather than lost.
const statusPublishBound = 30 * time.Second

// statusEmitter binds a StatusSink to the pipeline's logger, exactly as
// stateEmitter binds a StateSink: every publication is loud-logged with its
// workspace, run, phase and cause before it hits the sink, and a sink failure is
// loud-logged and returned rather than dropped.
//
// IT IS ALSO THE ONE PLACE A PUBLICATION IS BOUNDED. Every status the pipeline
// emits goes through emit, so bounding it HERE is what makes "no merge status
// publication can wedge a drain" a property of the package rather than a
// discipline every call site has to remember.
type statusEmitter struct {
	sink StatusSink
	logf dlog.Logf
	// bound is how long one sink call may take. Zero means statusPublishBound;
	// a test pins it small so the expiry path is exercised without waiting on
	// the production figure.
	bound time.Duration
}

// record calls the sink under statusEmitter's bound and reports the sink's own
// error, or the expiry.
//
// THE SINK CALL IS NOT ABANDONED, IT IS ONLY STOPPED BEING WAITED ON. There is
// no way to cancel a call already inside the SSM's mutex, and pretending
// otherwise would be a lie about what the timeout did. The goroutine survives
// until the sink returns and its result lands in a BUFFERED channel, so it
// cannot leak on a send nobody is receiving any more.
//
// A publication that expired is REPORTED AS FAILED even if the sink later
// succeeds. That is the honest reading: the pipeline stopped knowing, and the
// terminal-replay path it feeds is built to say a word twice rather than never.
func (e *statusEmitter) record(ws string, phase Phase, cause string, status *frontendv1.MergeStatus) error {
	bound := e.bound
	if bound <= 0 {
		bound = statusPublishBound
	}
	done := make(chan error, 1)
	go func() { done <- e.sink.RecordMergeStatus(ws, phase, cause, status) }()
	timer := time.NewTimer(bound)
	defer timer.Stop()
	select {
	case err := <-done:
		return err
	case <-timer.C:
		e.logf("merge: status SINK TIMED OUT {ws=%s run=%s phase=%s cause=%s bound=%s} — the sink has not returned; the pipeline stops waiting on it so the drain is not held by a publication, and the run's terminal word is treated as UNPUBLISHED (its durable queue entry is kept and replayed)",
			ws, status.GetRunId(), phase, cause, bound)
		return fmt.Errorf("merge: record %s status for %q: sink did not return within %s", phase, ws, bound)
	}
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
	if err := e.record(ws, phase, cause, status); err != nil {
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

// CommitsLanded is how many of the run's commits are on the rebased line.
//
// IT EXISTS FOR THE HEAD GATE'S CAUSE TEXT. The gate runs once, after the whole
// range has replayed, and "testing the rebased head abc123 after 3 commits" is
// only sayable by whoever holds the cursor. The run does, so the figure in the
// cause and the figure in the status arm are one value rather than two that can
// drift.
func (r *RunStatus) CommitsLanded() int32 {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.commitsLanded
}

// Testing publishes the testing phase for the run's landed commits, gated as one
// tree at the rebased head, and records that head as the run's current commit.
//
// THE ARM NAMES THE HEAD THE GATE JUDGES. It used to carry whatever commit
// cherry_picking last named — the last PLANNED pick — on the reasoning that the
// two share one cursor. They do share the counts, and those are unchanged here.
// They do not share the commit: the gate runs on the rebased HEAD, and after a
// remediation turn's fix commit that head is not the last planned pick at all, so
// the arm pointed at a commit the suite was demonstrably not run on. The head the
// caller hands in is the same one its cause text describes, so the arm's
// current_subject and the sentence beside it can never name two commits.
func (r *RunStatus) Testing(head PlannedCommit, cause string) error {
	r.mu.Lock()
	r.currentShort, r.currentSubject = head.Short, head.Subject
	r.mu.Unlock()
	return r.publish(PhaseMerging, cause, func(s *frontendv1.MergeStatus) {
		s.Phase = &frontendv1.MergeStatus_Testing{Testing: &frontendv1.MergeStatusTesting{
			CommitsTotal:  r.commitsTotal,
			CommitsLanded: r.commitsLanded,
			// current_sha is the INTERNAL correlation handle for the gated head;
			// current_subject is what any sentence about it says. A frontend that
			// renders the sha into prose is rendering the wrong field.
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
//
// THE COMMIT IT NAMES IS WHATEVER THE RUN WAS LAST WORKING ON, and that is now
// the right commit at the moment it matters most. A run that dies at its test
// gate is the common case, and Testing above records the gated HEAD — so the
// failed arm names the tree the suite actually judged rather than the last
// planned pick, which after a fix commit is neither the failing tree nor
// anything the user can act on. failing_sha is the correlation handle;
// failing_subject is what the cause and every rendering of it say.
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
	if arm == armFailed {
		if err := stampFailedJSON(status.GetFailed()); err != nil {
			r.mu.Unlock()
			r.logf("merge: status failed_json FAILED {ws=%s run=%s phase=%s cause=%s}: %v — the status is NOT published, because a failed arm without its own record is the report the field exists to replace",
				r.workspace, r.runID, phase, cause, err)
			return fmt.Errorf("merge: serialize failed status for %q run %s: %w", r.workspace, r.runID, err)
		}
	}
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

// failedJSON is the marshaler that produces MergeStatusFailed.failed_json.
//
// DEFAULT OPTIONS ON PURPOSE, which is to say the same protojson mapping the
// frontend wire itself uses (internal/frontend.marshalFrame): zero-valued
// fields are omitted, and a reader that already reads this daemon's frames
// reads this record by the identical rule. Emitting unpopulated fields instead
// would give the record a second, private convention AND write an empty
// `failedJson` key into it — the one field that is necessarily unset at the
// instant the record is taken.
var failedJSON = protojson.MarshalOptions{}

// stampFailedJSON fills failed's own protojson serialization into its
// failed_json field.
//
// CALLED FROM publish, NOT FROM THE CALL SITE THAT BUILT THE ARM, so a failed
// status cannot reach a frontend without its record: the stamping is a
// property of publishing a failed arm rather than a step a producer has to
// remember. Every failed arm goes through publish, so there is nowhere else to
// forget it.
//
// The field is stamped onto a message whose failed_json is still empty, which
// is what makes the serialization the record WITHOUT itself nested inside it.
// A non-empty failed_json on entry means the arm was published twice or
// stamped by hand, and is refused rather than re-serialized over: the second
// serialization would carry the first one inside it.
func stampFailedJSON(failed *frontendv1.MergeStatusFailed) error {
	if failed == nil {
		return fmt.Errorf("merge: failed arm reported present with no payload")
	}
	if failed.GetFailedJson() != "" {
		return fmt.Errorf("merge: failed arm already carries failed_json (%d bytes) — it would nest inside its own serialization", len(failed.GetFailedJson()))
	}
	encoded, err := failedJSON.Marshal(failed)
	if err != nil {
		return fmt.Errorf("merge: marshal failed status as json: %w", err)
	}
	failed.FailedJson = string(encoded)
	return nil
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
