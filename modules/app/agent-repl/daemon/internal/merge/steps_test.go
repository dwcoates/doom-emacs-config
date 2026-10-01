package merge

import (
	"context"
	"testing"

	"claude-repld/internal/resolve/footer"
)

// steppedRun is a run of the harness workspace that publishes facts.
func steppedRun(h *harness) *run {
	return &run{o: h.o, ws: theWorkspace, repo: h.repoKey()}
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

func TestANewStepEmptiesTheMergeTestsPanel(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	r := steppedRun(h)
	r.setStep(context.Background(), footer.StepTesting, func(f *footer.MergeFacts) {
		f.Tests = append(f.Tests, testRow("daemon", waitingRowState()))
		f.TestsRound++
	})

	// Act.
	r.setStep(context.Background(), footer.StepCommitting, nil)

	// Assert.
	if got := h.footer.last(); len(got.Tests) != 0 || got.TestsRound != 1 {
		t.Fatalf("facts = %+v, want no rows and the round kept", got)
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
