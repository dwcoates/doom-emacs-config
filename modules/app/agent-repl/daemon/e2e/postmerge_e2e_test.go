package e2e

// PostprocessingPrompt satisfies postmerge.PostprocessingSource on the e2e's
// workspace-creation stub.
//
// WireAgentShim derives the merge after-action's postprocessing source from the
// WorkspaceCreation bridge and refuses to construct without it, so an e2e whose
// stub could not answer would fail at wiring rather than at the behavior it is
// actually testing.
//
// The stub records no creation jobs, so the answer is whatever the harness was
// configured with (withMergeActions) and otherwise empty — "no workspace here
// was created with one", which is the honest answer for every e2e that does
// not configure an action.
func (e *emptyWorkspaceCreation) PostprocessingPrompt(string) (string, error) {
	return e.postprocessing, nil
}
