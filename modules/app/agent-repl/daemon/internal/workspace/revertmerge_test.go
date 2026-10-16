package workspace

import (
	"context"
	"errors"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/prompts"
	"agent-repld/internal/wsm"
)

// revertMessage is the record a queued merge-revert brief ends on.
const revertMessage = "reopened the merged workspace and queued its merge-revert brief"

// mergedForRevert registers w1 as a landed requester and the merge-revert
// brief it is given.
func mergedForRevert(t *testing.T, f *fixture) wsm.Workspace {
	t.Helper()
	ws := mergedWorkspace(t, f)
	f.briefs[BriefMergeRevert] = prompts.Prompt{
		Name:         BriefMergeRevert,
		Body:         "revert {{branch}} from {{parent_branch}} landed {{landed_commit}} at {{merged_at}}",
		Placeholders: []string{"branch", "parent_branch", "landed_commit", "merged_at"},
	}
	return ws
}

func TestOpenToRevertMergeOpensTheMergedWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	mergedForRevert(t, f)

	// Act.
	err := f.verbs.OpenToRevertMerge(context.Background(), "w1", nil)

	// Assert.
	if err != nil {
		t.Fatalf("OpenToRevertMerge: %v", err)
	}
	if len(f.fleet.started) != 1 || len(f.git.restored) != 1 {
		t.Fatalf("started = %v, restored = %v, want the worktree restored and the session up", f.fleet.started, f.git.restored)
	}
}

func TestOpenToRevertMergeQueuesTheBriefAsADeferredMergeRevert(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	mergedForRevert(t, f)

	// Act.
	if err := f.verbs.OpenToRevertMerge(context.Background(), "w1", nil); err != nil {
		t.Fatalf("OpenToRevertMerge: %v", err)
	}

	// Assert.
	if len(f.queue.submissions) != 1 {
		t.Fatalf("submissions = %+v, want the one brief", f.queue.submissions)
	}
	sub := f.queue.submissions[0]
	if sub.WS != "w1" || sub.Origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_REVERT || sub.Delivery != wsm.DeliveryDeferred {
		t.Fatalf("submission = %+v, want w1's deferred MERGE_REVERT brief", sub)
	}
}

func TestOpenToRevertMergeNamesTheBranchItsParentAndTheLandedCommit(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := mergedForRevert(t, f)

	// Act.
	if err := f.verbs.OpenToRevertMerge(context.Background(), "w1", nil); err != nil {
		t.Fatalf("OpenToRevertMerge: %v", err)
	}

	// Assert.
	text := submittedText(f.queue.submissions[0].Said)
	for _, want := range []string{"revert " + ws.Branch, "`master`", "sha-of-refs/heads/" + ws.Branch} {
		if !strings.Contains(text, want) {
			t.Fatalf("brief = %q, want it to carry %q", text, want)
		}
	}
}

func TestOpenToRevertMergeNamesAnUnrecordedParentInWords(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := mergedForRevert(t, f)
	ws.ParentBranch = ""
	f.db.with(ws)

	// Act.
	if err := f.verbs.OpenToRevertMerge(context.Background(), "w1", nil); err != nil {
		t.Fatalf("OpenToRevertMerge: %v", err)
	}

	// Assert.
	if text := submittedText(f.queue.submissions[0].Said); !strings.Contains(text, unrecordedParent) {
		t.Fatalf("brief = %q, want the unrecorded parent named in words", text)
	}
}

func TestOpenToRevertMergeRecordsTheQueuedBriefAtInfo(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	mergedForRevert(t, f)

	// Act.
	if err := f.verbs.OpenToRevertMerge(context.Background(), "w1", nil); err != nil {
		t.Fatalf("OpenToRevertMerge: %v", err)
	}

	// Assert.
	record, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opRevertMerge})
	if !ok || record.Message != revertMessage {
		t.Fatalf("record = %+v, want %q", record, revertMessage)
	}
}

// TestOpenToRevertMergeRefusesBeforeOpening pins every refusal that leaves the
// workspace as it was: nothing started, nothing restored, nothing queued.
func TestOpenToRevertMergeRefusesBeforeOpening(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, f *fixture)
		arm     string
	}{
		{name: "a workspace whose merge has not landed", arrange: func(t *testing.T, f *fixture) {
			ws := mergedForRevert(t, f)
			ws.MergedAt = nil
			f.db.with(ws)
		}, arm: ArmNotMerged},
		{name: "a brief the prompts directory does not hold", arrange: func(t *testing.T, f *fixture) {
			mergedForRevert(t, f)
			delete(f.briefs, BriefMergeRevert)
		}, arm: ArmBriefMissing},
		{name: "a branch that no longer exists", arrange: func(t *testing.T, f *fixture) {
			ws := mergedForRevert(t, f)
			delete(f.git.existingBranches, ws.Branch)
		}, arm: ArmWorktreeUnrestorable},
		{name: "a workspace that recorded no branch", arrange: func(t *testing.T, f *fixture) {
			ws := mergedForRevert(t, f)
			ws.Branch = ""
			f.db.with(ws)
		}, arm: ArmWorktreeUnrestorable},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			tc.arrange(t, f)

			// Act.
			err := f.verbs.OpenToRevertMerge(context.Background(), "w1", nil)

			// Assert.
			asRefusal(t, err, tc.arm)
			if len(f.fleet.started) != 0 || len(f.git.restored) != 0 || len(f.queue.submissions) != 0 || len(f.db.reopened) != 0 {
				t.Fatalf("started = %v, restored = %v, submissions = %d, reopened = %v, want nothing done",
					f.fleet.started, f.git.restored, len(f.queue.submissions), f.db.reopened)
			}
		})
	}
}

// TestOpenToRevertMergeFailsLoudlyWhenGitCannotAnswer pins that a git that
// cannot say what landed is an ERROR naming the step and a returned failure,
// with nothing opened.
func TestOpenToRevertMergeFailsLoudlyWhenGitCannotAnswer(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fixture)
		step    string
	}{
		{name: "the branch probe fails", arrange: func(f *fixture) {
			f.git.branchExistsErr = errors.New("git exploded")
		}, step: "probe branch"},
		{name: "the branch tip read fails", arrange: func(f *fixture) {
			f.git.resolveErr = errors.New("git exploded")
		}, step: "resolve branch"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			mergedForRevert(t, f)
			tc.arrange(f)

			// Act.
			err := f.verbs.OpenToRevertMerge(context.Background(), "w1", nil)

			// Assert.
			if err == nil || !strings.Contains(err.Error(), tc.step) || !strings.Contains(err.Error(), "git exploded") {
				t.Fatalf("OpenToRevertMerge = %v, want a failure naming %q and its cause", err, tc.step)
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opRevertMerge}); !ok {
				t.Fatalf("no ERROR record of the failed read; records = %+v", f.log.logger.Records())
			}
			if len(f.fleet.started) != 0 || len(f.queue.submissions) != 0 {
				t.Fatalf("started = %v, submissions = %d, want nothing opened", f.fleet.started, len(f.queue.submissions))
			}
		})
	}
}

// TestOpenToRevertMergeFailsLoudlyWhenTheQueueDoesNotTakeTheBrief pins that a
// brief the queue will not take is an ERROR and a returned failure; the
// workspace stays open, as the open already happened.
func TestOpenToRevertMergeFailsLoudlyWhenTheQueueDoesNotTakeTheBrief(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fixture)
		want    string
	}{
		{name: "the submit fails", arrange: func(f *fixture) {
			f.queue.submitErr = errors.New("queue exploded")
		}, want: "queue exploded"},
		{name: "the submit is refused", arrange: func(f *fixture) {
			f.queue.submitRefusedArm = "merging"
		}, want: "merging"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			mergedForRevert(t, f)
			tc.arrange(f)

			// Act.
			err := f.verbs.OpenToRevertMerge(context.Background(), "w1", nil)

			// Assert.
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("OpenToRevertMerge = %v, want a failure carrying %q", err, tc.want)
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opRevertMerge}); !ok {
				t.Fatalf("no ERROR record of the untaken brief; records = %+v", f.log.logger.Records())
			}
			if len(f.fleet.started) != 1 {
				t.Fatalf("started = %v, want the workspace left open", f.fleet.started)
			}
		})
	}
}
