package create

import (
	"fmt"
	"path/filepath"
)

// PostprocessingPromptFor resolves the postprocessing prompt the workspace at
// worktreePath was CREATED with, or "" when that workspace has none.
//
// It exists because `postprocessing_prompt` is a creation-time fact whose
// consumer runs much later: the prompt is meant to run in the PARENT session
// once the created workspace's merge has fully landed (see
// internal/workspace/postmerge). The durable job record is the only place that
// fact survives from creation to merge, so the post-merge handoff reads it back
// out of the same store the creation wrote it into.
//
// THE KEY IS THE WORKTREE PATH, because that is the identity the two ends
// share: the job store records it as Job.WorktreePath, and the daemon's
// workspace key IS the worktree the session runs in. The requested name is not
// usable as a key — a colliding name is resolved to a different FinalName, so
// two jobs can carry one requested name while no two carry one worktree.
//
// A workspace with no job record (created before the daemon owned creation, or
// by hand) is NOT an error: it reports "", nil. An unreadable store IS an
// error, because "the records could not be read" and "the record says nothing"
// are different answers and must not collapse into one.
func PostprocessingPromptFor(store JobStore, worktreePath string) (string, error) {
	if store == nil {
		return "", fmt.Errorf("workspace create: postprocessing lookup needs a job store")
	}
	if worktreePath == "" {
		return "", fmt.Errorf("workspace create: postprocessing lookup needs a worktree path")
	}
	jobs, err := store.List()
	if err != nil {
		return "", fmt.Errorf("workspace create: list jobs for postprocessing lookup: %w", err)
	}
	want := filepath.Clean(worktreePath)
	for _, job := range jobs {
		if job.WorktreePath == "" {
			continue
		}
		if filepath.Clean(job.WorktreePath) == want {
			return job.Request.PostprocessingPrompt, nil
		}
	}
	return "", nil
}
