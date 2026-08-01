package merge

import (
	"strings"
	"testing"
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
			got := res.Prompt()

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
	got := res.Prompt()

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
