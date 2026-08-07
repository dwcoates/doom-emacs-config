package create

import (
	"context"
	"fmt"
)

// WorktreeStage is THE worktree half of workspace creation: resolve the
// workspace's identity, make the worktree exist, and record the merge geometry
// that identity implies.
//
// It is a separate value from Manager because it has two callers with
// deliberately different lifecycles:
//
//   - The daemon's creation worker, which drives it as one stage of the durable
//     job state machine and continues on to session, health, host
//     materialization, and initial-prompt delivery.
//   - The one-off `claude-repld create-workspace` command, which wants exactly
//     this stage and nothing after it — no agent-repl session, no Emacs, no
//     daemon runtime.
//
// Both must resolve the final name, the branch slug, and the base commit the
// SAME way, and both must record geometry, or a command-line workspace would be
// one the daemon cannot merge. Sharing the value is what makes that mechanical
// rather than a promise that two copies stay in step.
type WorktreeStage struct {
	Planner   WorktreePlanner
	Worktrees WorktreeCreator
	Geometry  WorkspaceGeometryRecorder
	Logf      func(string, ...any)
}

// StageError names WHICH sub-step of the stage failed, so a caller that
// classifies failures per action (the manager records the action in the job's
// durable last_error) does not have to re-derive it from message text.
type StageError struct {
	Action string
	Err    error
}

func (e *StageError) Error() string { return fmt.Sprintf("%s: %v", e.Action, e.Err) }
func (e *StageError) Unwrap() error { return e.Err }

// Materialize plans the job's worktree identity when it has none, hands the
// plan to checkpoint, creates the worktree, and records its merge geometry.
//
// CHECKPOINT IS THE CALLER'S DURABILITY BOUNDARY. The daemon persists the plan
// there before any git mutation, so a crash cannot turn the job's own branch and
// path into a fresh collision on recovery; the one-off command has nothing to
// persist and simply returns the job with the plan applied. Either way the job
// Materialize continues with is the one checkpoint returned, so the worktree is
// created from the identity the caller committed to and never from a second
// in-memory copy of it.
//
// A failure from any of the three sub-steps is returned as *StageError. A
// failure from checkpoint is returned verbatim: it belongs to the caller's own
// storage, not to this stage.
func (s WorktreeStage) Materialize(ctx context.Context, job Job, checkpoint func(context.Context, WorktreeResult) (Job, error)) (Job, error) {
	if s.Planner == nil || s.Worktrees == nil || s.Geometry == nil || s.Logf == nil {
		return Job{}, fmt.Errorf("workspace create: worktree stage needs a planner, a worktree creator, a geometry recorder, and a logger")
	}
	if checkpoint == nil {
		return Job{}, fmt.Errorf("workspace create: worktree stage needs a checkpoint for job %s", job.ID)
	}
	if job.WorktreePath == "" {
		s.Logf("workspace-create: planning worktree job=%s name=%q git_root=%s", job.ID, job.Request.Name, job.Request.GitRoot)
		result, err := s.Planner.PlanWorktree(ctx, job)
		if err != nil {
			return Job{}, &StageError{Action: "plan worktree", Err: err}
		}
		if result.Path == "" || result.FinalName == "" || result.Branch == "" || result.BaseCommit == "" {
			return Job{}, &StageError{Action: "plan worktree", Err: fmt.Errorf("planner returned incomplete worktree identity path=%q final_name=%q branch=%q base=%q", result.Path, result.FinalName, result.Branch, result.BaseCommit)}
		}
		job, err = checkpoint(ctx, result)
		if err != nil {
			return Job{}, err
		}
	} else {
		s.Logf("workspace-create: worktree already planned job=%s path=%s branch=%q base=%s", job.ID, job.WorktreePath, job.Branch, job.ResolvedBaseCommit)
	}
	if err := s.Worktrees.EnsureWorktree(ctx, job); err != nil {
		return Job{}, &StageError{Action: "ensure worktree", Err: err}
	}
	// The worktree now exists, so its merge geometry is an observed fact: this
	// branch, this directory, and the worktree it was cut from. Record it
	// BEFORE the stage reports success — a workspace that reaches the user
	// without geometry is one nobody can ever merge. The recorder is
	// idempotent, so a crash between this call and the caller's next
	// checkpoint re-records the same facts.
	if err := s.Geometry.RecordWorkspaceGeometry(ctx, job); err != nil {
		return Job{}, &StageError{Action: "record merge geometry", Err: err}
	}
	return job, nil
}
