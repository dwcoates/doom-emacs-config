package create

import (
	"errors"
	"path/filepath"
	"testing"
)

// failingListStore is a JobStore whose List always fails, modeling an
// unreadable durable record.
type failingListStore struct {
	JobStore
	err error
}

func (s failingListStore) List() ([]Job, error) { return nil, s.err }

func newPostprocessingStore(t *testing.T, jobs ...Job) JobStore {
	t.Helper()
	store, err := OpenJobStore(filepath.Join(t.TempDir(), "jobs.json"), func(string, ...any) {})
	if err != nil {
		t.Fatalf("OpenJobStore: %v", err)
	}
	for _, job := range jobs {
		if _, _, err := store.Enqueue(job); err != nil {
			t.Fatalf("Enqueue %s: %v", job.ID, err)
		}
	}
	return store
}

func postprocessingJob(id, worktree, prompt string) Job {
	return Job{
		ID:           id,
		State:        StateReady,
		WorktreePath: worktree,
		Request: Request{
			Name:                 id,
			GitRoot:              "/repo",
			PostprocessingPrompt: prompt,
		},
	}
}

func TestPostprocessingPromptForReturnsTheRecordedPrompt(t *testing.T) {
	// Arrange — a job whose workspace was created with a postprocessing task.
	store := newPostprocessingStore(t, postprocessingJob("a", "/ws/a", "run the checklist"))

	// Act.
	got, err := PostprocessingPromptFor(store, "/ws/a")

	// Assert.
	if err != nil {
		t.Fatalf("PostprocessingPromptFor() error = %v", err)
	}
	if got != "run the checklist" {
		t.Fatalf("PostprocessingPromptFor() = %q, want the recorded prompt", got)
	}
}

func TestPostprocessingPromptForIsKeyedByWorktreeNotName(t *testing.T) {
	// Arrange — two jobs whose requested names collide but whose worktrees do
	// not, which is exactly what a name collision produces.
	store := newPostprocessingStore(t,
		postprocessingJob("a", "/ws/dup", "first"),
		postprocessingJob("b", "/ws/dup-1", "second"),
	)

	// Act.
	got, err := PostprocessingPromptFor(store, "/ws/dup-1")

	// Assert.
	if err != nil {
		t.Fatalf("PostprocessingPromptFor() error = %v", err)
	}
	if got != "second" {
		t.Fatalf("PostprocessingPromptFor() = %q, want the prompt of the matching worktree", got)
	}
}

func TestPostprocessingPromptForNormalizesThePath(t *testing.T) {
	// Arrange — the merge geometry may spell the same directory with a
	// trailing separator.
	store := newPostprocessingStore(t, postprocessingJob("a", "/ws/a", "run the checklist"))

	// Act.
	got, err := PostprocessingPromptFor(store, "/ws/a/")

	// Assert.
	if err != nil {
		t.Fatalf("PostprocessingPromptFor() error = %v", err)
	}
	if got != "run the checklist" {
		t.Fatalf("PostprocessingPromptFor() = %q, want the recorded prompt", got)
	}
}

func TestPostprocessingPromptForReportsNoneForAJobWithoutOne(t *testing.T) {
	// Arrange — the ordinary case.
	store := newPostprocessingStore(t, postprocessingJob("a", "/ws/a", ""))

	// Act.
	got, err := PostprocessingPromptFor(store, "/ws/a")

	// Assert.
	if err != nil {
		t.Fatalf("PostprocessingPromptFor() error = %v", err)
	}
	if got != "" {
		t.Fatalf("PostprocessingPromptFor() = %q, want none", got)
	}
}

func TestPostprocessingPromptForReportsNoneForAnUnknownWorktree(t *testing.T) {
	// Arrange — a workspace the daemon's creation never made.
	store := newPostprocessingStore(t, postprocessingJob("a", "/ws/a", "run the checklist"))

	// Act.
	got, err := PostprocessingPromptFor(store, "/ws/handmade")

	// Assert — not an error: a workspace with no job record simply has none.
	if err != nil {
		t.Fatalf("PostprocessingPromptFor() error = %v, want no error for an unknown workspace", err)
	}
	if got != "" {
		t.Fatalf("PostprocessingPromptFor() = %q, want none", got)
	}
}

func TestPostprocessingPromptForSkipsJobsWithNoWorktreeYet(t *testing.T) {
	// Arrange — a queued job whose worktree identity is not planned yet must
	// not match an empty-ish lookup by accident.
	store := newPostprocessingStore(t,
		Job{ID: "pending", State: StateQueued, Request: Request{Name: "pending", GitRoot: "/repo", PostprocessingPrompt: "never"}},
		postprocessingJob("a", "/ws/a", "run the checklist"),
	)

	// Act.
	got, err := PostprocessingPromptFor(store, "/ws/a")

	// Assert.
	if err != nil {
		t.Fatalf("PostprocessingPromptFor() error = %v", err)
	}
	if got != "run the checklist" {
		t.Fatalf("PostprocessingPromptFor() = %q, want the planned job's prompt", got)
	}
}

func TestPostprocessingPromptForSurfacesAnUnreadableStore(t *testing.T) {
	// Arrange — "the records could not be read" must never collapse into
	// "there is no prompt".
	boom := errors.New("store unreadable")

	// Act.
	got, err := PostprocessingPromptFor(failingListStore{err: boom}, "/ws/a")

	// Assert.
	if !errors.Is(err, boom) {
		t.Fatalf("PostprocessingPromptFor() error = %v, want the store failure surfaced", err)
	}
	if got != "" {
		t.Fatalf("PostprocessingPromptFor() = %q, want empty alongside the error", got)
	}
}

func TestPostprocessingPromptForRefusesAMissingStore(t *testing.T) {
	// Act.
	_, err := PostprocessingPromptFor(nil, "/ws/a")

	// Assert.
	if err == nil {
		t.Fatalf("PostprocessingPromptFor(nil store) error = nil, want error")
	}
}

func TestPostprocessingPromptForRefusesAnEmptyWorktreePath(t *testing.T) {
	// Arrange.
	store := newPostprocessingStore(t, postprocessingJob("a", "/ws/a", "run the checklist"))

	// Act.
	_, err := PostprocessingPromptFor(store, "")

	// Assert.
	if err == nil {
		t.Fatalf("PostprocessingPromptFor(\"\") error = nil, want error")
	}
}
