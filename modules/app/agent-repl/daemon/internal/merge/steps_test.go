package merge

import (
	"context"
	"testing"

	"agent-repld/internal/resolve/footer"
)

// steppedRun is a run of the harness workspace that publishes facts.
func steppedRun(h *harness) *run {
	return &run{o: h.o, ws: theWorkspace, repo: h.repoKey(), git: newGatedGit(h.o.deps.Git, h.o.deps.Now)}
}

func TestANewStepClearsTheLastStepsLine(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	r := steppedRun(h)
	r.setStep(context.Background(), footer.StepCommitting, func(f *footer.MergeFacts) { f.Line = committingLine("merge(x): y") })

	// Act.
	r.setStep(context.Background(), footer.StepPostprocessing, nil)

	// Assert.
	if got := h.footer.last(); got.Line != nil || got.Step != footer.StepPostprocessing {
		t.Fatalf("facts = %+v, want postprocessing with no line", got)
	}
}

func TestEveryStepCarriesWhenTheMergeWasQueued(t *testing.T) {
	queued := int64(1_700_000_000_000)
	tests := []struct {
		name string
		act  func(r *run)
	}{
		{name: "a new step", act: func(r *run) { r.setStep(context.Background(), footer.StepPostprocessing, nil) }},
		{name: "progress on the standing step", act: func(r *run) { r.updateFacts(func(f *footer.MergeFacts) { f.Replayed = 1 }) }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			r := steppedRun(h)
			r.queuedMS = queued
			r.setStep(context.Background(), footer.StepRebasing, nil)

			// Act.
			tc.act(r)

			// Assert.
			if got := h.footer.last().QueuedAt.UnixMilli(); got != queued {
				t.Fatalf("facts are queued at %d, want the run's %d", got, queued)
			}
		})
	}
}

func TestTheEnqueuedLineNamesTheRequesterAndItsStepInPlainWords(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	r := steppedRun(h)
	r.setStep(context.Background(), footer.StepConflictResolution, nil)

	// Act.
	line, _ := r.enqueuedLine("fix-reconnect")

	// Assert.
	if got := line.GetEnqueued(); got.GetWorkspaceName() != "fix-reconnect" || got.GetStep() != "conflict resolution" {
		t.Fatalf("enqueued line = %+v, want fix-reconnect: conflict resolution", got)
	}
}

func TestTheEnqueuedLineIsDatedWhenTheStepBegan(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	r := steppedRun(h)
	r.setStep(context.Background(), footer.StepRebasing, nil)
	_, began := r.enqueuedLine("x")

	// Act: progress on the same step.
	r.updateFacts(func(f *footer.MergeFacts) { f.Replayed = 1 })
	_, after := r.enqueuedLine("x")

	// Assert.
	if !after.Equal(began) {
		t.Fatalf("the line's date moved from %v to %v within one step", began, after)
	}
}

func TestPromptLinesCarryTheFirstLineOfThePrompt(t *testing.T) {
	// Act.
	pre := promptLine(footer.StepPreprocessing, "run the review\nthen more")
	post := promptLine(footer.StepPostprocessing, "tidy up")

	// Assert.
	if pre.GetPreprocessing().GetText() != "run the review" || post.GetPostprocessing().GetText() != "tidy up" {
		t.Fatalf("prompt lines = %+v and %+v", pre, post)
	}
}
