package create

import (
	"fmt"
	"path/filepath"
)

// creationRequestFor resolves the create Request the workspace at worktreePath
// was CREATED with, reporting found=false when no job record names that
// worktree.
//
// IT IS THE ONE LOOKUP every creation-time-fact accessor in this package runs
// on. `before_ws_merge` and `postprocessing_prompt` are both fields of one
// Request, recorded once at creation and read back much later by a merge; each
// resolving the job with its own copy of this loop is how two readers of one
// record come to key, normalize, or tie-break differently and answer
// differently. `what` names the caller in the errors so a failure still says
// which lookup failed.
//
// THE KEY IS THE WORKTREE PATH. It is the identity the creation job and the
// daemon's workspace key share; the requested name is not usable as one because
// a collision is resolved to a different FinalName, so two jobs can carry one
// requested name while no two carry one worktree.
//
// A workspace with no job record (created before the daemon owned creation, or
// by hand) is NOT an error: it is found=false. An unreadable store IS an error,
// because "the records could not be read" and "the record says nothing" are
// different answers and must not collapse into one.
func creationRequestFor(store JobStore, worktreePath, what string) (Request, bool, error) {
	if store == nil {
		return Request{}, false, fmt.Errorf("workspace create: %s lookup needs a job store", what)
	}
	if worktreePath == "" {
		return Request{}, false, fmt.Errorf("workspace create: %s lookup needs a worktree path", what)
	}
	jobs, err := store.List()
	if err != nil {
		return Request{}, false, fmt.Errorf("workspace create: list jobs for %s lookup: %w", what, err)
	}
	want := filepath.Clean(worktreePath)
	for _, job := range jobs {
		if job.WorktreePath == "" {
			continue
		}
		if filepath.Clean(job.WorktreePath) == want {
			return job.Request, true, nil
		}
	}
	return Request{}, false, nil
}
