package main

import (
	"fmt"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// THE RUNTIME, REPOSITORY AND EDITOR VERBS' REFUSALS: deploy holds, a deploy
// hook's rollout, the storage sweep, the worktree reaper, registering a
// repository and evaluating in the editor. Each describer says every arm of
// its verb's error in one plain sentence.

// describeDeployHoldRefusal says in one plain sentence why the repository's
// deploys were not held or released. Every UpdateDeployHoldError arm has its
// own sentence.
func describeDeployHoldRefusal(e *agentreplv1.UpdateDeployHoldError) (string, error) {
	switch e.GetCause().(type) {
	case *agentreplv1.UpdateDeployHoldError_AlreadyHeld:
		return "the repository's deploys are already held", nil
	case *agentreplv1.UpdateDeployHoldError_NotHeld:
		return "the repository's deploys are not held", nil
	case *agentreplv1.UpdateDeployHoldError_UnknownRepository:
		return noRepositoryFlag, nil
	}
	return "", undescribed("holding deploys", e)
}

// describeRollOutRefusal says in one plain sentence why DEPLOY's rollout was
// not started. Every RollOutLandingError arm has its own sentence.
func describeRollOutRefusal(deploy string, e *agentreplv1.RollOutLandingError) (string, error) {
	switch e.GetCause().(type) {
	case *agentreplv1.RollOutLandingError_UnknownDeploy:
		return fmt.Sprintf("no deploy %q runs in this daemon: it ended, or its ledger was never minted here", deploy), nil
	}
	return "", undescribed("rolling out "+deploy, e)
}

// describePruneRefusal says in one plain sentence why the storage sweep did
// not run. Every PruneStorageError arm has its own sentence.
func describePruneRefusal(e *agentreplv1.PruneStorageError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.PruneStorageError_AlreadyRunning:
		return "a storage sweep is already running in this daemon", nil
	case *agentreplv1.PruneStorageError_HeldElsewhere:
		return "another daemon, the other half of a handover, holds the storage sweep's lock", nil
	case *agentreplv1.PruneStorageError_Failed:
		return fmt.Sprintf("the storage sweep could not run or ended early (%s)", r.Failed.GetDetail()), nil
	}
	return "", undescribed("pruning storage", e)
}

// describeReapRefusal says in one plain sentence why the worktree reaper did
// not run. Every ReapWorktreesError arm has its own sentence.
func describeReapRefusal(e *agentreplv1.ReapWorktreesError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.ReapWorktreesError_AlreadyRunning:
		return "a worktree reap is already running in this daemon", nil
	case *agentreplv1.ReapWorktreesError_HeldElsewhere:
		return "another daemon, the other half of a handover, holds the worktree reaper's lock", nil
	case *agentreplv1.ReapWorktreesError_Failed:
		return fmt.Sprintf("the worktree reap could not run or ended early (%s)", r.Failed.GetDetail()), nil
	}
	return "", undescribed("reaping worktrees", e)
}

// describeRegisterRefusal says in one plain sentence why the repository PATH
// is inside was not registered. Every RegisterRepositoryError arm has its own
// sentence.
func describeRegisterRefusal(path string, e *agentreplv1.RegisterRepositoryError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.RegisterRepositoryError_NotInARepository:
		return fmt.Sprintf("%s lies inside no git work tree, or inside a bare repository, which has no main worktree", path), nil
	case *agentreplv1.RegisterRepositoryError_UnreadablePath:
		return fmt.Sprintf("%s cannot be read: it is not there, or its directory cannot be stat'ed", path), nil
	case *agentreplv1.RegisterRepositoryError_InsideTemporaryDirectory:
		return fmt.Sprintf("the repository's main worktree %s lies inside the temporary directory %s", r.InsideTemporaryDirectory.GetDir(), r.InsideTemporaryDirectory.GetTemporaryRoot()), nil
	}
	return "", undescribed("registering "+path, e)
}

// describeEvalRefusal says in one plain sentence why the editor did not
// evaluate the text in WS. Every EvaluateInEditorError arm has its own
// sentence.
func describeEvalRefusal(ws string, e *agentreplv1.EvaluateInEditorError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.EvaluateInEditorError_UnknownWorkspace:
		return sayNoWorkspace(ws), nil
	case *agentreplv1.EvaluateInEditorError_NoEditor:
		return fmt.Sprintf("no editor serves %s, so nothing can evaluate the text", ws), nil
	case *agentreplv1.EvaluateInEditorError_TimedOut:
		return fmt.Sprintf("the editor did not answer within %dms", r.TimedOut.GetDeadlineMs()), nil
	}
	return "", undescribed("evaluating in "+ws+"'s editor", e)
}
