package wsm

import workspacev1 "agentrepl/proto/workspace/v1"

// Ref is the workspace as every wire names it: its id and its dir. It is the
// ONE place a workspace record becomes a workspace.v1.WorkspaceRef, so no
// caller can name one with half its identity.
func (ws Workspace) Ref() *workspacev1.WorkspaceRef {
	return FamilyRef{ID: ws.ID, Dir: ws.Dir}.Ref()
}

// Ref is the family member as every wire names it (Workspace.Ref).
func (r FamilyRef) Ref() *workspacev1.WorkspaceRef {
	return &workspacev1.WorkspaceRef{Id: string(r.ID), Dir: r.Dir}
}
