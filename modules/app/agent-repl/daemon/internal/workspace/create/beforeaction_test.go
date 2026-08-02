package create

import (
	"errors"
	"testing"
)

// beforeActionJob is a created workspace carrying a before_ws_merge action.
func beforeActionJob(id, worktree, prompt string) Job {
	return Job{
		ID:           id,
		State:        StateReady,
		WorktreePath: worktree,
		Request: Request{
			Name:          id,
			GitRoot:       "/repo",
			BeforeWSMerge: prompt,
		},
	}
}

func TestBeforeWSMergePromptForReturnsTheRecordedPrompt(t *testing.T) {
	// Arrange — a job whose workspace was created with a before-merge action.
	store := newPostprocessingStore(t, beforeActionJob("a", "/ws/a", "run the gate"))

	// Act.
	got, err := BeforeWSMergePromptFor(store, "/ws/a")

	// Assert.
	if err != nil {
		t.Fatalf("BeforeWSMergePromptFor() error = %v", err)
	}
	if got != "run the gate" {
		t.Fatalf("BeforeWSMergePromptFor() = %q, want the recorded prompt", got)
	}
}

func TestBeforeWSMergePromptForIsKeyedByWorktreeNotName(t *testing.T) {
	// Arrange — two jobs sharing a requested name, resolved onto two worktrees.
	store := newPostprocessingStore(t,
		beforeActionJob("dup-1", "/ws/dup-1", "the first workspace's gate"),
		beforeActionJob("dup-2", "/ws/dup-2", "the second workspace's gate"),
	)

	// Act.
	got, err := BeforeWSMergePromptFor(store, "/ws/dup-2")

	// Assert.
	if err != nil {
		t.Fatalf("BeforeWSMergePromptFor() error = %v", err)
	}
	if got != "the second workspace's gate" {
		t.Fatalf("BeforeWSMergePromptFor() = %q, want the prompt of the matching worktree", got)
	}
}

func TestBeforeWSMergePromptForNormalizesThePath(t *testing.T) {
	// Arrange — the caller's key carries a trailing separator the record does not.
	store := newPostprocessingStore(t, beforeActionJob("a", "/ws/a", "run the gate"))

	// Act.
	got, err := BeforeWSMergePromptFor(store, "/ws/a/")

	// Assert.
	if err != nil {
		t.Fatalf("BeforeWSMergePromptFor() error = %v", err)
	}
	if got != "run the gate" {
		t.Fatalf("BeforeWSMergePromptFor() = %q, want the recorded prompt", got)
	}
}

func TestBeforeWSMergePromptForReportsNoneForAJobWithoutOne(t *testing.T) {
	// Arrange — a created workspace with no before-merge action.
	store := newPostprocessingStore(t, beforeActionJob("a", "/ws/a", ""))

	// Act.
	got, err := BeforeWSMergePromptFor(store, "/ws/a")

	// Assert.
	if err != nil {
		t.Fatalf("BeforeWSMergePromptFor() error = %v", err)
	}
	if got != "" {
		t.Fatalf("BeforeWSMergePromptFor() = %q, want none", got)
	}
}

func TestBeforeWSMergePromptForReportsNoneForAnUnknownWorktree(t *testing.T) {
	// Arrange — a store that knows nothing about the hand-made worktree asked for.
	store := newPostprocessingStore(t, beforeActionJob("a", "/ws/a", "run the gate"))

	// Act.
	got, err := BeforeWSMergePromptFor(store, "/ws/handmade")

	// Assert.
	if err != nil {
		t.Fatalf("BeforeWSMergePromptFor() error = %v, want no error for an unknown workspace", err)
	}
	if got != "" {
		t.Fatalf("BeforeWSMergePromptFor() = %q, want none", got)
	}
}

func TestBeforeWSMergePromptForSurfacesAnUnreadableStore(t *testing.T) {
	// Arrange — a store whose records cannot be listed at all.
	boom := errors.New("records unreadable")

	// Act.
	got, err := BeforeWSMergePromptFor(failingListStore{err: boom}, "/ws/a")

	// Assert — the failure is surfaced, never collapsed into "has none".
	if !errors.Is(err, boom) {
		t.Fatalf("BeforeWSMergePromptFor() error = %v, want the store failure surfaced", err)
	}
	if got != "" {
		t.Fatalf("BeforeWSMergePromptFor() = %q, want empty alongside the error", got)
	}
}

func TestBeforeWSMergePromptForRefusesAMissingStore(t *testing.T) {
	// Arrange, Act.
	_, err := BeforeWSMergePromptFor(nil, "/ws/a")

	// Assert.
	if err == nil {
		t.Fatal("BeforeWSMergePromptFor(nil store) error = nil, want error")
	}
}

func TestBeforeWSMergePromptForRefusesAnEmptyWorktreePath(t *testing.T) {
	// Arrange.
	store := newPostprocessingStore(t, beforeActionJob("a", "/ws/a", "run the gate"))

	// Act.
	_, err := BeforeWSMergePromptFor(store, "")

	// Assert.
	if err == nil {
		t.Fatal("BeforeWSMergePromptFor(\"\") error = nil, want error")
	}
}
