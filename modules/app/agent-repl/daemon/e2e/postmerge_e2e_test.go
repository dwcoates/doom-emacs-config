package e2e

// PostprocessingPrompt satisfies postmerge.PostprocessingSource on the e2e's
// workspace-creation stub.
//
// WireAgentShim derives the post-merge handoff's postprocessing source from the
// WorkspaceCreation bridge and refuses to construct without it, so an e2e whose
// stub could not answer would fail at wiring rather than at the behavior it is
// actually testing. The stub records no creation jobs, so the honest answer is
// "no workspace here was created with one".
func (e *emptyWorkspaceCreation) PostprocessingPrompt(string) (string, error) { return "", nil }
