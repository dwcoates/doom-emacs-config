package create

import (
	"errors"
	"strings"
	"testing"
)

// bothPromptsJob is a created workspace carrying BOTH creation-time prompts, so
// a test can watch the two accessors answer from one record.
func bothPromptsJob(id, worktree, before, post string) Job {
	return Job{
		ID:           id,
		State:        StateReady,
		WorktreePath: worktree,
		Request: Request{
			Name:                 id,
			GitRoot:              "/repo",
			BeforeWSMerge:        before,
			PostprocessingPrompt: post,
		},
	}
}

// THE GUARANTEE: both creation-time prompts resolve through ONE lookup, so they
// cannot key, normalize, or tie-break differently and answer for two different
// workspaces. Two jobs share a requested name here; only the worktree separates
// them.
func TestBothCreationPromptAccessorsResolveTheSameJob(t *testing.T) {
	// Arrange.
	store := newPostprocessingStore(t,
		bothPromptsJob("a", "/ws/dup", "gate one", "checklist one"),
		bothPromptsJob("b", "/ws/dup-1", "gate two", "checklist two"),
	)

	tests := []struct {
		name     string
		worktree string
		resolve  func(JobStore, string) (string, error)
		want     string
	}{
		{name: "before-merge action", worktree: "/ws/dup-1/", resolve: BeforeWSMergePromptFor, want: "gate two"},
		{name: "postprocessing", worktree: "/ws/dup-1/", resolve: PostprocessingPromptFor, want: "checklist two"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got, err := tt.resolve(store, tt.worktree)

			// Assert.
			if err != nil {
				t.Fatalf("resolve() error = %v", err)
			}
			if got != tt.want {
				t.Fatalf("resolve() = %q, want %q from the job the worktree names", got, tt.want)
			}
		})
	}
}

// An unreadable store is a LOUD failure for either prompt, never an empty
// answer: "the records could not be read" and "the record says nothing" are
// different answers.
func TestBothCreationPromptAccessorsSurfaceAnUnreadableStore(t *testing.T) {
	// Arrange.
	boom := errors.New("the job file is unreadable")
	store := failingListStore{err: boom}

	tests := []struct {
		name    string
		resolve func(JobStore, string) (string, error)
		wantIn  string
	}{
		{name: "before-merge action", resolve: BeforeWSMergePromptFor, wantIn: "before-merge action"},
		{name: "postprocessing", resolve: PostprocessingPromptFor, wantIn: "postprocessing"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got, err := tt.resolve(store, "/ws/a")

			// Assert.
			if !errors.Is(err, boom) {
				t.Fatalf("resolve() error = %v, want the store failure surfaced", err)
			}
			if !strings.Contains(err.Error(), tt.wantIn) {
				t.Fatalf("resolve() error = %v, want it to name the %s lookup", err, tt.wantIn)
			}
			if got != "" {
				t.Fatalf("resolve() = %q, want empty alongside the error", got)
			}
		})
	}
}
