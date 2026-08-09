package merge

import (
	"strings"
	"testing"

	"claude-repld/internal/prompts"
)

func completeTestFailureResolution() TestFailureResolution {
	return TestFailureResolution{
		Workspace:     "/ws/a",
		RequestID:     "merge_testfix_abc",
		FailingCommit: "abc1234",
		SourceBranch:  "feature/a",
		TargetDir:     "/target",
		FailureTail:   "FAIL: agent-repl-suite",
	}
}

func TestTestFailureResolutionRejectsAnIncompleteFactSet(t *testing.T) {
	tests := []struct {
		name   string
		mutate func(*TestFailureResolution)
	}{
		{"no workspace", func(r *TestFailureResolution) { r.Workspace = "" }},
		{"no request id", func(r *TestFailureResolution) { r.RequestID = "" }},
		{"no failing commit", func(r *TestFailureResolution) { r.FailingCommit = "" }},
		{"no source branch", func(r *TestFailureResolution) { r.SourceBranch = "" }},
		{"no target dir", func(r *TestFailureResolution) { r.TargetDir = "" }},
		{"no failure tail", func(r *TestFailureResolution) { r.FailureTail = "" }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			res := completeTestFailureResolution()
			tc.mutate(&res)

			// Act.
			err := res.validate()

			// Assert.
			if err == nil {
				t.Fatalf("validate() = nil, want a refusal for %s", tc.name)
			}
		})
	}
}

func TestTestFailureResolutionAcceptsACompleteFactSet(t *testing.T) {
	// Act.
	err := completeTestFailureResolution().validate()

	// Assert.
	if err != nil {
		t.Fatalf("validate() = %v, want nil", err)
	}
}

func TestTestFailurePromptNamesTheCommitBranchTargetAndTail(t *testing.T) {
	// Act.
	usePrompts(t)
	got := mustPrompt(t)(completeTestFailureResolution().Prompt())

	// Assert — the agent cannot act on a prompt missing any of these.
	for _, want := range []string{"abc1234", "feature/a", "/target", "FAIL: agent-repl-suite"} {
		if !strings.Contains(got, want) {
			t.Errorf("prompt is missing %q:\n%s", want, got)
		}
	}
}

func TestTestFailurePromptForbidsCommittingAndRewriting(t *testing.T) {
	// The daemon commits the staged fix itself, so an agent that committed or
	// amended would duplicate that commit or rewrite the SHA the replay's
	// already-incorporated probe keys on.
	// Act.
	usePrompts(t)
	got := mustPrompt(t)(completeTestFailureResolution().Prompt())

	// Assert.
	for _, want := range []string{"Do NOT commit", "do NOT amend", "git reset", "git add"} {
		if !strings.Contains(got, want) {
			t.Errorf("prompt is missing the %q instruction:\n%s", want, got)
		}
	}
}

func TestTestFailurePromptWarnsThatTheAttemptIsTheOnlyOne(t *testing.T) {
	// Act.
	usePrompts(t)
	got := mustPrompt(t)(completeTestFailureResolution().Prompt())

	// Assert — the agent is told the stakes. AMENDED: the second half used to
	// require the words "rolled back", which the pipeline no longer does and
	// which would now be a lie told to an agent. The consequence it stands for
	// is unchanged in strength — what happens to the merge target when the one
	// attempt fails — so the assertion names the new truth instead.
	if !strings.Contains(got, "EXACTLY ONE attempt") {
		t.Errorf("prompt does not say the attempt is the only one:\n%s", got)
	}
	if !strings.Contains(got, "never modified") {
		t.Errorf("prompt does not name what becomes of the merge target:\n%s", got)
	}
}

func TestTestFixRequestIDsAreDistinctFromConflictResolutionIDs(t *testing.T) {
	// Act.
	fix, conflict := newTestFixRequestID(), newResolutionRequestID()

	// Assert — the durable record tells the two kinds of merge-driven turn
	// apart by prefix, and no two attempts share an id.
	if !strings.HasPrefix(fix, "merge_testfix_") {
		t.Errorf("test-fix request id = %q, want the merge_testfix_ prefix", fix)
	}
	if !strings.HasPrefix(conflict, "merge_resolve_") {
		t.Errorf("conflict request id = %q, want the merge_resolve_ prefix", conflict)
	}
	if fix == newTestFixRequestID() {
		t.Errorf("two test-fix request ids collided")
	}
}

// TestTestFailurePromptMatchesTheGolden pins prompts/merge-test-failure-resolve.md.
//
// AMENDED for the rebase pipeline, for the same reason its conflict sibling was:
// the previous golden told the agent its commit had been cherry-picked into the
// merge target and that a second failure would ROLL THE TARGET BACK. Neither is
// true now — the commit is in a temporary rebase worktree and the target was
// never modified — so the golden would have been pinning a false statement to an
// agent. Exact equality against one literal, unchanged.
//
// AMENDED AGAIN for the single head gate. The opening line said one COMMIT had
// been rebased and broke the suite; the suite now runs once, on the head of the
// whole rebased line, so the sha the agent is given names that head. Telling it
// otherwise would send it hunting through one commit for a failure the merge
// attributes to the range.
func TestTestFailurePromptMatchesTheGolden(t *testing.T) {
	// Arrange.
	usePrompts(t)
	want := "Every commit of branch feature/a was just rebased onto the merge target in the worktree at /target, and the repository's test suite FAILS on the resulting head abc1234. The suite runs once per merge, on that head, so the failure is a fact about the whole rebased line rather than about any one commit of it.\n" +
		"\n" +
		"That worktree is a TEMPORARY REBASE WORKTREE, not the merge target and not your own workspace. The merge target has not been modified at all and will not be until the whole rebase passes, so the failing state exists only in that worktree.\n" +
		"\n" +
		"Failing output (tail):\n" +
		"---\n" +
		"FAIL: agent-repl-suite\n" +
		"---\n" +
		"\n" +
		"Fix it in that worktree: change the tests or the code so the suite passes again, and stage every fix with `git add`.\n" +
		"\n" +
		"Then STOP. Do NOT commit, do NOT amend, do NOT run `git reset`, `git rebase`, `git cherry-pick`, or any other history-rewriting command. The daemon commits your staged fix as a follow-up commit and re-runs the suite as soon as your turn ends.\n" +
		"\n" +
		"You get EXACTLY ONE attempt. If the suite still fails after it, the merge is failed, the rebase worktree is discarded, and the merge target is left exactly as it was — it was never modified. Your branch keeps all of its work either way. If you cannot fix it, say so plainly."

	// Act.
	got := mustPrompt(t)(completeTestFailureResolution().Prompt())

	// Assert.
	if got != want {
		t.Fatalf("prompt drifted from its golden.\n got: %q\nwant: %q", got, want)
	}
}

func TestTestFailurePromptErrorsWhenItsFileIsMissing(t *testing.T) {
	// Arrange.
	t.Setenv(prompts.DirEnv, t.TempDir())

	// Act.
	got, err := completeTestFailureResolution().Prompt()

	// Assert.
	if err == nil {
		t.Fatalf("Prompt() = %q, nil; want a loud error when %s is unreadable", got, TestFailurePromptFile)
	}
}

func TestTestFailurePromptClampsTheFailureTail(t *testing.T) {
	// Arrange — the tail is the one unbounded input, and the prompt's own clamp
	// is what keeps a runaway suite from filling the turn.
	usePrompts(t)
	res := completeTestFailureResolution()
	res.FailureTail = strings.Repeat("Z", testFailureTailPromptBytes*2)

	// Act.
	got := mustPrompt(t)(res.Prompt())

	// Assert.
	if strings.Count(got, "Z") > testFailureTailPromptBytes {
		t.Fatalf("prompt carries %d tail bytes, want at most %d", strings.Count(got, "Z"), testFailureTailPromptBytes)
	}
}
