package create

import (
	"fmt"
	"path/filepath"
)

// BeforeWSMergePromptFor resolves the before_ws_merge action the workspace at
// worktreePath was CREATED with, or "" when that workspace has none.
//
// It is the exact sibling of PostprocessingPromptFor, and deliberately so. Both
// prompts are fields of the SAME create Request (`before_ws_merge`,
// `postprocessing_prompt`), both are recorded when the workspace is created, and
// both are read back at merge time keyed by the worktree path. Resolving one
// from the creation records and the other from a second store would give the
// merge run two sources of one creation-time fact, which is exactly how the two
// come to disagree.
//
// THE KEY IS THE WORKTREE PATH, for the reason PostprocessingPromptFor gives:
// it is the identity the creation job and the daemon's workspace key share, and
// the requested name is not usable as one because a collision resolves to a
// different FinalName.
//
// A workspace with no job record (created before the daemon owned creation, or
// by hand) is NOT an error: it reports "", nil. An unreadable store IS an error,
// because "the records could not be read" and "the record says nothing" are
// different answers and must not collapse into one.
func BeforeWSMergePromptFor(store JobStore, worktreePath string) (string, error) {
	if store == nil {
		return "", fmt.Errorf("workspace create: before-merge action lookup needs a job store")
	}
	if worktreePath == "" {
		return "", fmt.Errorf("workspace create: before-merge action lookup needs a worktree path")
	}
	jobs, err := store.List()
	if err != nil {
		return "", fmt.Errorf("workspace create: list jobs for before-merge action lookup: %w", err)
	}
	want := filepath.Clean(worktreePath)
	for _, job := range jobs {
		if job.WorktreePath == "" {
			continue
		}
		if filepath.Clean(job.WorktreePath) == want {
			return job.Request.BeforeWSMerge, nil
		}
	}
	return "", nil
}
