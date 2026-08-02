package create

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
// two jobs can carry one requested name while no two carry one worktree. That
// keying lives in creationRequestFor, the ONE lookup this accessor shares with
// BeforeWSMergePromptFor.
//
// A workspace with no job record (created before the daemon owned creation, or
// by hand) is NOT an error: it reports "", nil. An unreadable store IS an
// error, because "the records could not be read" and "the record says nothing"
// are different answers and must not collapse into one.
func PostprocessingPromptFor(store JobStore, worktreePath string) (string, error) {
	req, found, err := creationRequestFor(store, worktreePath, "postprocessing")
	if err != nil || !found {
		return "", err
	}
	return req.PostprocessingPrompt, nil
}
