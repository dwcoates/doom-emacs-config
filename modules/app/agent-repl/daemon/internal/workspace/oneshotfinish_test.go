package workspace

import (
	"context"
	"errors"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/prompts"
	"claude-repld/internal/wsm"
)

// concludingFixture arranges a one-shot workspace whose finish is still owed.
func concludingFixture(t *testing.T, finish string) *fixture {
	t.Helper()
	f := newFixture(t)
	oneShotBriefs(f)
	f.workspace("w1", t.TempDir())
	f.db.jobs["w1"] = wsm.CreationJob{Workspace: "w1", OneShot: true, Finish: finish}
	return f
}

func TestOnOneShotTurnConcludedEnqueuesTheSelfMerge(t *testing.T) {
	// Arrange.
	f := concludingFixture(t, "self_merge")

	// Act.
	if err := f.verbs.OnOneShotTurnConcluded(context.Background(), "w1", "t1"); err != nil {
		t.Fatalf("OnOneShotTurnConcluded: %v", err)
	}

	// Assert.
	if len(f.merge.enqueued) != 1 || f.merge.enqueued[0] != "w1" {
		t.Fatalf("enqueued merges = %v, want w1", f.merge.enqueued)
	}
}

func TestOnOneShotTurnConcludedSubmitsThePullRequestFollowup(t *testing.T) {
	// Arrange.
	f := concludingFixture(t, "open_pr+add_to_merge_queue")

	// Act.
	if err := f.verbs.OnOneShotTurnConcluded(context.Background(), "w1", "t1"); err != nil {
		t.Fatalf("OnOneShotTurnConcluded: %v", err)
	}

	// Assert.
	if len(f.queue.submissions) != 1 {
		t.Fatalf("submissions = %v, want exactly one", f.queue.submissions)
	}
	sent := f.queue.submissions[0].Said.GetContent().GetBlocks()[0].GetText().GetText()
	if !strings.Contains(sent, WorkspaceSkill+" close") {
		t.Fatalf("follow-up = %q, want the CICD-gated wrap-up", sent)
	}
}

func TestOnOneShotTurnConcludedMetaWrapsTheFollowup(t *testing.T) {
	// Arrange: the user typed none of it.
	f := concludingFixture(t, "open_pr")

	// Act.
	if err := f.verbs.OnOneShotTurnConcluded(context.Background(), "w1", "t1"); err != nil {
		t.Fatalf("OnOneShotTurnConcluded: %v", err)
	}

	// Assert.
	sent := f.queue.submissions[0].Said.GetContent().GetBlocks()[0].GetText().GetText()
	if !strings.HasPrefix(sent, prompts.MetaOpen) || !strings.HasSuffix(sent, prompts.MetaClose) {
		t.Fatalf("follow-up = %q, want it meta-wrapped whole", sent)
	}
}

func TestOnOneShotTurnConcludedStampsTheDeferredOrigin(t *testing.T) {
	// Arrange: it is a prompt the daemon held until the turn ended.
	f := concludingFixture(t, "open_pr")

	// Act.
	if err := f.verbs.OnOneShotTurnConcluded(context.Background(), "w1", "t1"); err != nil {
		t.Fatalf("OnOneShotTurnConcluded: %v", err)
	}

	// Assert.
	if f.queue.submissions[0].Origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_DEFERRED_PROMPT {
		t.Fatalf("origin = %v, want the deferred-prompt origin", f.queue.submissions[0].Origin)
	}
}

func TestOnOneShotTurnConcludedSpendsTheFinish(t *testing.T) {
	// Arrange.
	f := concludingFixture(t, "self_merge")

	// Act.
	if err := f.verbs.OnOneShotTurnConcluded(context.Background(), "w1", "t1"); err != nil {
		t.Fatalf("OnOneShotTurnConcluded: %v", err)
	}

	// Assert.
	if f.db.jobs["w1"].Finish != "" {
		t.Fatalf("recorded finish = %q, want it spent", f.db.jobs["w1"].Finish)
	}
}

func TestOnOneShotTurnConcludedTakesTheFinishExactlyOnce(t *testing.T) {
	// Arrange: a follow-up turn concludes too, and it owes nothing.
	f := concludingFixture(t, "self_merge")
	if err := f.verbs.OnOneShotTurnConcluded(context.Background(), "w1", "t1"); err != nil {
		t.Fatalf("first conclusion: %v", err)
	}

	// Act.
	if err := f.verbs.OnOneShotTurnConcluded(context.Background(), "w1", "t2"); err != nil {
		t.Fatalf("second conclusion: %v", err)
	}

	// Assert.
	if len(f.merge.enqueued) != 1 {
		t.Fatalf("enqueued merges = %v, want exactly one", f.merge.enqueued)
	}
}

func TestOnOneShotTurnConcludedSpendsTheFinishBeforeTakingIt(t *testing.T) {
	// Arrange: acting first and clearing second would re-enqueue the merge if
	// the clear failed, and one merge enqueued twice is the worse outcome.
	f := concludingFixture(t, "self_merge")
	f.db.putJobErr = errors.New("the database is locked")

	// Act.
	err := f.verbs.OnOneShotTurnConcluded(context.Background(), "w1", "t1")

	// Assert.
	if err == nil {
		t.Fatal("OnOneShotTurnConcluded() = nil error, want the spend failure surfaced")
	}
	if len(f.merge.enqueued) != 0 {
		t.Fatalf("enqueued merges = %v, want none when the finish could not be spent", f.merge.enqueued)
	}
}

func TestOnOneShotTurnConcludedIgnoresAWorkspaceThatIsNotAOneShot(t *testing.T) {
	// Arrange: the queue calls this on EVERY conclusion.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.jobs["w1"] = wsm.CreationJob{Workspace: "w1"}

	// Act.
	err := f.verbs.OnOneShotTurnConcluded(context.Background(), "w1", "t1")

	// Assert.
	if err != nil {
		t.Fatalf("OnOneShotTurnConcluded: %v", err)
	}
	if len(f.merge.enqueued) != 0 || len(f.queue.submissions) != 0 {
		t.Fatal("a non-one-shot conclusion took an action")
	}
}

func TestOnOneShotTurnConcludedIgnoresAWorkspaceWithNoCreationJob(t *testing.T) {
	// Arrange: a registered workspace never went through creation.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	err := f.verbs.OnOneShotTurnConcluded(context.Background(), "w1", "t1")

	// Assert.
	if err != nil {
		t.Fatalf("OnOneShotTurnConcluded: %v", err)
	}
}

func TestOnOneShotTurnConcludedRefusesAnUnparseableFinish(t *testing.T) {
	// Arrange.
	f := concludingFixture(t, "teleport")

	// Act.
	err := f.verbs.OnOneShotTurnConcluded(context.Background(), "w1", "t1")

	// Assert.
	if err == nil {
		t.Fatal("OnOneShotTurnConcluded() = nil error, want the unparseable finish surfaced")
	}
	if f.db.jobs["w1"].Finish != "teleport" {
		t.Fatalf("recorded finish = %q, want it left alone", f.db.jobs["w1"].Finish)
	}
}

func TestOnOneShotTurnConcludedIsLoudWhenTheFollowupBriefIsMissing(t *testing.T) {
	// Arrange.
	f := concludingFixture(t, "open_pr")
	delete(f.briefs, BriefOneShotCreatePrFollowup)

	// Act.
	err := f.verbs.OnOneShotTurnConcluded(context.Background(), "w1", "t1")

	// Assert.
	asRefusal(t, err, ArmBriefMissing)
}

func TestOnOneShotTurnConcludedSurfacesAMergeRefusal(t *testing.T) {
	// Arrange.
	f := concludingFixture(t, "self_merge")
	f.merge.enqueueErr = errors.New("no layout facts were recorded")

	// Act.
	err := f.verbs.OnOneShotTurnConcluded(context.Background(), "w1", "t1")

	// Assert.
	if err == nil {
		t.Fatal("OnOneShotTurnConcluded() = nil error, want the merge refusal surfaced")
	}
}

func TestOnOneShotTurnConcludedRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	err := f.verbs.OnOneShotTurnConcluded(context.Background(), "nope", "t1")

	// Assert.
	asRefusal(t, err, ArmUnknownWorkspace)
}
