package create

// BeforeWSMergePromptFor resolves the before_ws_merge action the workspace at
// worktreePath was CREATED with, or "" when that workspace has none.
//
// IT IS THE ONE ACCESSOR for that prompt, on both ends. The create command
// writes it as a field of the job's Request; every reader — the merge pipeline,
// through server.MergeBeforeActionSource and
// WorkspaceCreationBridge.BeforeWSMergePrompt — comes back through here. The
// prompt was once ALSO copied into a geometry column that nothing read, and a
// duplicated creation-time fact is how a writer and a reader end up naming two
// different places; there is now one place and one way in.
//
// It is the exact sibling of PostprocessingPromptFor, and shares its lookup
// (creationRequestFor) for that reason: both prompts are fields of the SAME
// create Request, so resolving them two different ways would be the same drift
// one level down.
//
// A workspace with no job record (created before the daemon owned creation, or
// by hand) is NOT an error: it reports "", nil. An unreadable store IS an error,
// because "the records could not be read" and "the record says nothing" are
// different answers and must not collapse into one.
func BeforeWSMergePromptFor(store JobStore, worktreePath string) (string, error) {
	req, found, err := creationRequestFor(store, worktreePath, "before-merge action")
	if err != nil || !found {
		return "", err
	}
	return req.BeforeWSMerge, nil
}
