package create

import (
	"context"
	"errors"
	"testing"
)

// stageJob is the shape both callers hand the stage: a request, and no worktree
// identity yet.
func stageJob() Job {
	return Job{ID: "job-1", Request: Request{Name: "feature", GitRoot: "/repo", BaseCommit: "HEAD"}}
}

// applyPlan is the one-off command's checkpoint: nothing durable, just the plan
// applied to the in-memory job.
func applyPlan(job Job) func(context.Context, WorktreeResult) (Job, error) {
	return func(_ context.Context, result WorktreeResult) (Job, error) {
		job.WorktreePath = result.Path
		job.FinalName = result.FinalName
		job.Branch = result.Branch
		job.ResolvedBaseCommit = result.BaseCommit
		job.Request.ForkSessionID = result.ForkSessionID
		return job, nil
	}
}

func newStage(worktrees *fakeWorktrees, geometry *fakeGeometry) WorktreeStage {
	return WorktreeStage{Planner: worktrees, Worktrees: worktrees, Geometry: geometry, Logf: func(string, ...any) {}}
}

func TestWorktreeStageMaterializesTheCheckpointedIdentity(t *testing.T) {
	// Arrange.
	worktrees := &fakeWorktrees{path: "/repo-worktrees/feature", name: "feature", branch: "feature", base: "abc123"}
	geometry := &fakeGeometry{}
	stage := newStage(worktrees, geometry)

	// Act.
	got, err := stage.Materialize(context.Background(), stageJob(), applyPlan(stageJob()))

	// Assert.
	if err != nil {
		t.Fatalf("Materialize: %v", err)
	}
	if got.WorktreePath != "/repo-worktrees/feature" || got.Branch != "feature" || got.ResolvedBaseCommit != "abc123" {
		t.Fatalf("job = %+v, want the planned identity", got)
	}
	if worktrees.plans != 1 || worktrees.calls != 1 || len(geometry.jobs) != 1 {
		t.Fatalf("plans=%d creates=%d geometry=%d, want 1 of each", worktrees.plans, worktrees.calls, len(geometry.jobs))
	}
}

func TestWorktreeStageRecordsGeometryFromTheCheckpointedJob(t *testing.T) {
	// Arrange — a caller whose checkpoint rewrites the path (the durable store
	// is free to normalize it) must see the geometry recorded from that value.
	worktrees := &fakeWorktrees{path: "/planned", name: "feature", branch: "feature", base: "abc123"}
	geometry := &fakeGeometry{}
	stage := newStage(worktrees, geometry)
	checkpoint := func(_ context.Context, result WorktreeResult) (Job, error) {
		job := stageJob()
		job.WorktreePath = "/persisted"
		job.FinalName, job.Branch, job.ResolvedBaseCommit = result.FinalName, result.Branch, result.BaseCommit
		return job, nil
	}

	// Act.
	if _, err := stage.Materialize(context.Background(), stageJob(), checkpoint); err != nil {
		t.Fatalf("Materialize: %v", err)
	}

	// Assert.
	if geometry.jobs[0].WorktreePath != "/persisted" {
		t.Fatalf("geometry worktree = %q, want the checkpointed /persisted", geometry.jobs[0].WorktreePath)
	}
}

func TestWorktreeStageSkipsPlanningAnAlreadyPlannedJob(t *testing.T) {
	// Arrange — a resumed job already carries its durable identity, and
	// replanning it would resolve a second name for one workspace.
	worktrees := &fakeWorktrees{path: "/unused", name: "other", branch: "other", base: "def456"}
	geometry := &fakeGeometry{}
	job := stageJob()
	job.WorktreePath, job.FinalName, job.Branch, job.ResolvedBaseCommit = "/repo-worktrees/feature", "feature", "feature", "abc123"

	// Act.
	got, err := newStage(worktrees, geometry).Materialize(context.Background(), job, applyPlan(job))

	// Assert.
	if err != nil {
		t.Fatalf("Materialize: %v", err)
	}
	if worktrees.plans != 0 {
		t.Fatalf("plans = %d, want 0 for an already-planned job", worktrees.plans)
	}
	if got.Branch != "feature" {
		t.Fatalf("branch = %q, want the already-persisted feature", got.Branch)
	}
}

func TestWorktreeStageClassifiesSubStepFailures(t *testing.T) {
	tests := []struct {
		name       string
		worktrees  *fakeWorktrees
		geometry   *fakeGeometry
		wantAction string
	}{
		{
			name:       "planner fails",
			worktrees:  &fakeWorktrees{err: errors.New("git is unavailable")},
			geometry:   &fakeGeometry{},
			wantAction: "plan worktree",
		},
		{
			name:       "planner returns an incomplete identity",
			worktrees:  &fakeWorktrees{path: "", name: "feature", branch: "feature", base: "abc123"},
			geometry:   &fakeGeometry{},
			wantAction: "plan worktree",
		},
		{
			name:       "geometry recording fails",
			worktrees:  &fakeWorktrees{path: "/repo-worktrees/feature", name: "feature", branch: "feature", base: "abc123"},
			geometry:   &fakeGeometry{err: errors.New("state store is down")},
			wantAction: "record merge geometry",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			stage := newStage(tc.worktrees, tc.geometry)

			// Act.
			_, err := stage.Materialize(context.Background(), stageJob(), applyPlan(stageJob()))

			// Assert.
			var stageErr *StageError
			if !errors.As(err, &stageErr) {
				t.Fatalf("error = %v, want a *StageError", err)
			}
			if stageErr.Action != tc.wantAction {
				t.Fatalf("action = %q, want %q", stageErr.Action, tc.wantAction)
			}
		})
	}
}

func TestWorktreeStageClassifiesAWorktreeCreationFailure(t *testing.T) {
	// Arrange — the creation failure needs its own fake, because fakeWorktrees
	// shares one error field between planning and creation.
	worktrees := &failingCreator{fakeWorktrees: fakeWorktrees{path: "/repo-worktrees/feature", name: "feature", branch: "feature", base: "abc123"}, createErr: errors.New("worktree add refused")}
	stage := WorktreeStage{Planner: worktrees, Worktrees: worktrees, Geometry: &fakeGeometry{}, Logf: func(string, ...any) {}}

	// Act.
	_, err := stage.Materialize(context.Background(), stageJob(), applyPlan(stageJob()))

	// Assert.
	var stageErr *StageError
	if !errors.As(err, &stageErr) || stageErr.Action != "ensure worktree" {
		t.Fatalf("error = %v, want a *StageError for ensure worktree", err)
	}
}

// failingCreator plans successfully and refuses to create, which is the split
// fakeWorktrees' single error field cannot express.
type failingCreator struct {
	fakeWorktrees
	createErr error
}

func (f *failingCreator) EnsureWorktree(context.Context, Job) error { return f.createErr }

func TestWorktreeStageReturnsACheckpointFailureUnclassified(t *testing.T) {
	// Arrange — a checkpoint failure belongs to the caller's storage, not to
	// the job, so it must not be reported as a job-level stage failure.
	stage := newStage(&fakeWorktrees{path: "/repo-worktrees/feature", name: "feature", branch: "feature", base: "abc123"}, &fakeGeometry{})
	broken := errors.New("job store is unwritable")

	// Act.
	_, err := stage.Materialize(context.Background(), stageJob(), func(context.Context, WorktreeResult) (Job, error) {
		return Job{}, broken
	})

	// Assert.
	var stageErr *StageError
	if errors.As(err, &stageErr) {
		t.Fatalf("error = %v, want it NOT classified as a stage failure", err)
	}
	if !errors.Is(err, broken) {
		t.Fatalf("error = %v, want the checkpoint's own error", err)
	}
}

func TestWorktreeStageRefusesAnIncompleteConfiguration(t *testing.T) {
	// Arrange.
	stage := WorktreeStage{Planner: &fakeWorktrees{}, Worktrees: &fakeWorktrees{}, Logf: func(string, ...any) {}}

	// Act.
	_, err := stage.Materialize(context.Background(), stageJob(), applyPlan(stageJob()))

	// Assert.
	if err == nil {
		t.Fatal("Materialize succeeded, want a refusal without a geometry recorder")
	}
	if got := err.Error(); got != "workspace create: worktree stage needs a planner, a worktree creator, a geometry recorder, and a logger" {
		t.Fatalf("error = %q, want the configuration refusal", got)
	}
}

func TestWorktreeStageRefusesAMissingCheckpoint(t *testing.T) {
	// Arrange.
	stage := newStage(&fakeWorktrees{}, &fakeGeometry{})

	// Act.
	_, err := stage.Materialize(context.Background(), stageJob(), nil)

	// Assert.
	if err == nil {
		t.Fatal("Materialize succeeded, want a refusal without a checkpoint")
	}
	if got := err.Error(); got != "workspace create: worktree stage needs a checkpoint for job job-1" {
		t.Fatalf("error = %q, want the checkpoint refusal", got)
	}
}
