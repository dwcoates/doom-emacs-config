package merge

import (
	"strings"
	"testing"

	"claude-repld/internal/prompts"
)

func TestConflictResolutionRequiresEveryFact(t *testing.T) {
	complete := func() ConflictResolution {
		return ConflictResolution{
			Workspace:      "/ws/a",
			RequestID:      "merge_resolve_1",
			ConflictCommit: "abc1234",
			SourceBranch:   "feature/a",
			TargetDir:      "/target",
		}
	}
	tests := []struct {
		name    string
		mutate  func(*ConflictResolution)
		wantErr bool
	}{
		{name: "complete", mutate: func(*ConflictResolution) {}, wantErr: false},
		{name: "no workspace", mutate: func(r *ConflictResolution) { r.Workspace = "" }, wantErr: true},
		{name: "no request id", mutate: func(r *ConflictResolution) { r.RequestID = "" }, wantErr: true},
		{name: "no conflict commit", mutate: func(r *ConflictResolution) { r.ConflictCommit = "" }, wantErr: true},
		{name: "no source branch", mutate: func(r *ConflictResolution) { r.SourceBranch = "" }, wantErr: true},
		{name: "no target dir", mutate: func(r *ConflictResolution) { r.TargetDir = "" }, wantErr: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			res := complete()
			tc.mutate(&res)

			// Act.
			err := res.validate()

			// Assert.
			if (err != nil) != tc.wantErr {
				t.Fatalf("validate() error = %v, wantErr %v", err, tc.wantErr)
			}
		})
	}
}

func TestConflictResolutionPromptNamesTheConflict(t *testing.T) {
	tests := []struct {
		name string
		want string
	}{
		{name: "names the commit", want: "abc1234"},
		{name: "names the source branch", want: "feature/a"},
		{name: "names the target worktree", want: "/target"},
		{name: "asks for the resolutions to be staged", want: "git add"},
	}
	res := ConflictResolution{
		Workspace: "/ws/a", RequestID: "merge_resolve_1",
		ConflictCommit: "abc1234", SourceBranch: "feature/a", TargetDir: "/target",
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange + Act.
			usePrompts(t)
			got := mustPrompt(t)(res.Prompt())

			// Assert.
			if !strings.Contains(got, tc.want) {
				t.Fatalf("prompt = %q, want it to contain %q", got, tc.want)
			}
		})
	}
}

func TestConflictResolutionPromptForbidsContinuingThePick(t *testing.T) {
	// Arrange — the coordinator, not the agent, runs the continue.
	res := ConflictResolution{
		Workspace: "/ws/a", RequestID: "merge_resolve_1",
		ConflictCommit: "abc1234", SourceBranch: "feature/a", TargetDir: "/target",
	}

	// Act.
	usePrompts(t)
	got := mustPrompt(t)(res.Prompt())

	// Assert.
	if !strings.Contains(got, "Do NOT run `git cherry-pick --continue`") {
		t.Fatalf("prompt = %q, want it to forbid `git cherry-pick --continue`", got)
	}
	if !strings.Contains(got, "do NOT commit") {
		t.Fatalf("prompt = %q, want it to forbid committing", got)
	}
}

func TestResolutionRequestIDsAreUnique(t *testing.T) {
	// Arrange + Act — two attempts, whose receipts must not reconcile onto one
	// another's transcript line.
	first, second := newResolutionRequestID(), newResolutionRequestID()

	// Assert.
	if first == second {
		t.Fatalf("resolution request ids collided: %q", first)
	}
	if !strings.HasPrefix(first, "merge_resolve_") {
		t.Fatalf("resolution request id = %q, want a merge_resolve_ prefix", first)
	}
}

// TestConflictResolutionPromptMatchesTheGolden pins the text of
// prompts/merge-conflict-resolve.md, so any drift from this literal is either
// an intentional edit that should update the golden or an accident that must
// not reach an agent unnoticed.
//
// AMENDED for the rebase pipeline. The previous golden described a cherry-pick
// into the merge TARGET, which is no longer what happens: the conflict parks in
// a temporary rebase worktree and the target is untouched. A golden that still
// said "cherry-pick" would be pinning an instruction that sends the agent to
// the wrong tree. The assertion is unchanged in strength — one literal, exact
// equality.
func TestConflictResolutionPromptMatchesTheGolden(t *testing.T) {
	// Arrange.
	usePrompts(t)
	res := ConflictResolution{
		Workspace: "/ws/a", RequestID: "merge_resolve_1",
		ConflictCommit: "abc1234", SourceBranch: "feature/a", TargetDir: "/target",
	}
	want := "A rebase of commit abc1234 from branch feature/a onto the merge target is CONFLICTED in the worktree at /target.\n" +
		"\n" +
		"That worktree is a TEMPORARY REBASE WORKTREE, not the merge target and not your own workspace. The merge target has not been modified at all and will not be until every commit of this rebase has landed and passed the test suite, so nothing you do here can break the tree anybody else is working from.\n" +
		"\n" +
		"Resolve every conflict in that worktree and stage each resolution with `git add`.\n" +
		"\n" +
		"Then STOP. Do NOT run `git cherry-pick --continue` or `git rebase --continue`, do NOT commit, do NOT amend, and do NOT run `git cherry-pick --abort`, `git rebase --abort` or `git reset`. The daemon continues the rebase itself as soon as your turn ends, and it can only do that against a replay that is still paused.\n" +
		"\n" +
		"If the conflicts cannot be resolved, say so plainly and leave the tree as you found it — a human takes it from there."

	// Act.
	got := mustPrompt(t)(res.Prompt())

	// Assert.
	if got != want {
		t.Fatalf("prompt drifted from its golden.\n got: %q\nwant: %q", got, want)
	}
}

func TestConflictResolutionPromptErrorsWhenItsFileIsMissing(t *testing.T) {
	// Arrange — an empty prompts directory stands for a deleted or misnamed file.
	t.Setenv(prompts.DirEnv, t.TempDir())
	res := ConflictResolution{
		Workspace: "/ws/a", RequestID: "merge_resolve_1",
		ConflictCommit: "abc1234", SourceBranch: "feature/a", TargetDir: "/target",
	}

	// Act.
	got, err := res.Prompt()

	// Assert — never a baked-in fallback copy.
	if err == nil {
		t.Fatalf("Prompt() = %q, nil; want a loud error when %s is unreadable", got, ConflictPromptFile)
	}
	if got != "" {
		t.Fatalf("Prompt() returned text %q alongside an error", got)
	}
}
