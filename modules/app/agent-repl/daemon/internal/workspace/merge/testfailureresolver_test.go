package merge

import (
	"strings"
	"testing"
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
	got := completeTestFailureResolution().Prompt()

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
	got := completeTestFailureResolution().Prompt()

	// Assert.
	for _, want := range []string{"Do NOT commit", "do NOT amend", "git reset", "git add"} {
		if !strings.Contains(got, want) {
			t.Errorf("prompt is missing the %q instruction:\n%s", want, got)
		}
	}
}

func TestTestFailurePromptWarnsThatTheAttemptIsTheOnlyOne(t *testing.T) {
	// Act.
	got := completeTestFailureResolution().Prompt()

	// Assert — the agent is told the stakes, including the rollback.
	if !strings.Contains(got, "EXACTLY ONE attempt") {
		t.Errorf("prompt does not say the attempt is the only one:\n%s", got)
	}
	if !strings.Contains(got, "rolled back") {
		t.Errorf("prompt does not name the rollback consequence:\n%s", got)
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
