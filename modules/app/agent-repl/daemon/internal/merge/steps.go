package merge

import (
	"context"
	"fmt"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/resolve/footer"
)

// This file is a running merge's FOOTER FACTS: the step it is on (the merging
// substatus), the step's own salient activity line, and the merge tests
// panel's rows.
//
// THE ACTIVITY LINE BELONGS TO ITS STEP (owner, 2026-09-29): it is the step's
// own salient detail and it is cleared when the step ends, so every step change
// starts from no line and the step supplies its own. Footer text is for a
// human user: plain words, never an identifier spelling.

// setStep moves the merge onto a step, clearing the last step's line and the
// merge tests panel, applies the step's own facts, and publishes them. A
// changed step is told to every merge waiting behind this one, whose
// "enqueued" line names it.
func (r *run) setStep(ctx context.Context, step footer.MergeStep, apply func(*footer.MergeFacts)) {
	if r.exiting() {
		return
	}
	now := r.o.deps.Now()
	r.mu.Lock()
	left, leftAt := r.facts.Step, r.stepAt
	changed := left != step
	facts := footer.MergeFacts{State: StateMerging, QueuedAt: time.UnixMilli(r.queuedMS), Step: step, LineAt: now}
	if apply != nil {
		apply(&facts)
	}
	r.facts = facts
	if changed {
		r.stepAt = now
	}
	r.mu.Unlock()
	// A HEAD AT ITS FIXING STEP LETS THE NEXT MERGE TEST (windowFor). Marked
	// here, where every run enters a step, so a head that RESUMES fixing --
	// taken back after losing a race to land, or after a restart -- widens
	// the chain behind it as a fresh failure does (owner's report,
	// 2026-10-30: a resumed fixing head held the next merge out).
	if changed && step == footer.StepFixing {
		r.o.markFailedAtHead(ctx, r)
	}
	if changed {
		r.timeStep(left, leftAt, now)
	}
	r.o.publish(r.repo, r.ws, facts)
	if changed && step != footer.StepEnqueued {
		if err := r.o.republishQueue(ctx, r.repo); err != nil {
			recordFailed(r.o.log(ctx, r.ws), "daemon.merge.step", "could not tell the merges waiting behind this one its new step",
				"the merges waiting behind this one were not told its new step: its context ended under the read",
				dlog.Context{"workspace": string(r.ws), "step": string(step)}, err)
		}
	}
}

// timeStep records how long the run spent on step, which it left at now. A
// step with no start (the run's first, or one a resumed run never saw begin)
// is not timed.
func (r *run) timeStep(step footer.MergeStep, began, now time.Time) {
	if step == "" || began.IsZero() {
		return
	}
	r.o.deps.Timings.ObserveMergeStep(string(step), now.Sub(began))
}

// updateFacts changes the standing step's facts in place -- a rebase's
// progress, a suite's edge -- and publishes them. The step, and so the waiting
// merges' "enqueued" line, is unchanged.
func (r *run) updateFacts(apply func(*footer.MergeFacts)) {
	if r.exiting() {
		return
	}
	now := r.o.deps.Now()
	r.mu.Lock()
	facts := r.facts
	apply(&facts)
	if facts.Line != r.facts.Line {
		facts.LineAt = now
	}
	r.facts = facts
	r.mu.Unlock()
	r.o.publish(r.repo, r.ws, facts)
}

// enqueuedLine is the line every merge waiting behind this one draws: this
// merge's requester, and its step in plain words. It changes only when the
// step does, dated when the step began.
func (r *run) enqueuedLine(name string) (*frontendv1.FooterStatusActivityMergeStep, time.Time) {
	r.mu.Lock()
	defer r.mu.Unlock()
	return &frontendv1.FooterStatusActivityMergeStep{Step: &frontendv1.FooterStatusActivityMergeStep_Enqueued{
		Enqueued: &frontendv1.FooterMergeStepEnqueued{WorkspaceName: name, Step: r.facts.Step.Words()},
	}}, r.stepAt
}

// promptLine is a configured prompt's line: the prompt's text itself.
func promptLine(kind string, text string) *frontendv1.FooterStatusActivityMergeStep {
	return hookLine(stageNamed(kind), &frontendv1.FooterMergeStepHook{Hook: &frontendv1.FooterMergeStepHook_Prompt{
		Prompt: &frontendv1.FooterMergeStepPrompt{Text: firstLine(text)}}})
}

// rebaseCommandLine is the rebase's line while a commit is replayed: the
// command replaying it.
func rebaseCommandLine(text string) *frontendv1.FooterStatusActivityMergeStep {
	return &frontendv1.FooterStatusActivityMergeStep{Step: &frontendv1.FooterStatusActivityMergeStep_Rebasing{
		Rebasing: &frontendv1.FooterMergeStepRebasing{Line: &frontendv1.FooterMergeStepRebasing_Running{
			Running: &frontendv1.FooterMergeStepRebaseCommand{Text: text}}}}}
}

// rebaseFailureLine is the rebase's line once a command failed: its first
// error line.
func rebaseFailureLine(text string) *frontendv1.FooterStatusActivityMergeStep {
	return &frontendv1.FooterStatusActivityMergeStep{Step: &frontendv1.FooterStatusActivityMergeStep_Rebasing{
		Rebasing: &frontendv1.FooterMergeStepRebasing{Line: &frontendv1.FooterMergeStepRebasing_Failed{
			Failed: &frontendv1.FooterMergeStepRebaseFailure{Text: text}}}}}
}

// conflictLine is conflict resolution's line: the commit that conflicted and
// how many files.
func conflictLine(subject string, files int) *frontendv1.FooterStatusActivityMergeStep {
	return &frontendv1.FooterStatusActivityMergeStep{Step: &frontendv1.FooterStatusActivityMergeStep_ConflictResolution{
		ConflictResolution: &frontendv1.FooterMergeStepConflict{CommitSubject: subject, Files: uint32(files)}}}
}

// suiteLine is testing's line: one suite's edge.
func suiteLine(name string, edge suiteState) *frontendv1.FooterStatusActivityMergeStep {
	suite := &frontendv1.FooterMergeStepSuite{Name: name}
	switch edge {
	case suiteStatePassed, suiteStateDeclined:
		suite.Edge = &frontendv1.FooterMergeStepSuite_Passed{Passed: &frontendv1.FooterMergeStepSuitePassed{}}
	case suiteStateFailed:
		suite.Edge = &frontendv1.FooterMergeStepSuite_Failed{Failed: &frontendv1.FooterMergeStepSuiteFailed{}}
	default:
		suite.Edge = &frontendv1.FooterMergeStepSuite_Started{Started: &frontendv1.FooterMergeStepSuiteStarted{}}
	}
	return &frontendv1.FooterStatusActivityMergeStep{Step: &frontendv1.FooterStatusActivityMergeStep_Testing{Testing: suite}}
}

// fixingLine is fixing's line: the suites being fixed.
func fixingLine(suites []string) *frontendv1.FooterStatusActivityMergeStep {
	return &frontendv1.FooterStatusActivityMergeStep{Step: &frontendv1.FooterStatusActivityMergeStep_Fixing{
		Fixing: &frontendv1.FooterMergeStepFixing{Suites: suites}}}
}

// committingLine is committing's line: the merge commit's first line.
func committingLine(subject string) *frontendv1.FooterStatusActivityMergeStep {
	return &frontendv1.FooterStatusActivityMergeStep{Step: &frontendv1.FooterStatusActivityMergeStep_Committing{
		Committing: &frontendv1.FooterMergeStepCommitting{Subject: subject}}}
}

// commitLine names one replayed commit the way the narration does.
func commitLine(sha, subject string) string {
	return fmt.Sprintf("%s %s", short(sha), subject)
}

// currentStep is the step the merge stands on, for a record.
func (r *run) currentStep() footer.MergeStep {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.facts.Step
}
