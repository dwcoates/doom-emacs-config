package wsm

// derivationDDL is the layout-60 addition: what a workspace was DERIVED FROM,
// recorded at its creation (agentrepl.v1.CreateWorkspaceRequest).
//
//   - owner_lands: the workspace is a child whose context was not forked from
//     its owner (parent_id), so only the owner -- or the user -- lands it
//     (agentrepl.v1.WorkspaceTopologyChild).
//   - context_from: the workspace whose conversation was forked into this one
//     (agentrepl.v1.WorkspaceContextOrigin.fork), NULL for a fresh context.
//   - requested_by: the workspace whose agent asked for the create
//     (agentrepl.v1.WorkspaceRequester.workspace), NULL when the user asked.
//
// They are what the derived-workspace notices are addressed by: parent_id
// (the branch source) and context_from are told when this workspace is
// created, lands, or closes without landing, and requested_by is never told
// about its own act.
//
// IT IS ADDITIVE: the previous build's INSERT names none of the columns, so a
// row it writes takes the defaults -- a workspace that lands itself, with no
// recorded context source and no requesting workspace, which is exactly what
// that build knew about it. Both references are SET NULL on delete, like
// parent_id: a forgotten source leaves the derived workspace standing.
const derivationDDL = `
ALTER TABLE workspaces ADD COLUMN owner_lands INTEGER NOT NULL DEFAULT 0;
ALTER TABLE workspaces ADD COLUMN context_from TEXT REFERENCES workspaces(id) ON DELETE SET NULL;
ALTER TABLE workspaces ADD COLUMN requested_by TEXT REFERENCES workspaces(id) ON DELETE SET NULL;
`

// MayAct reports whether ASKER may act on the workspace's own work -- land
// it, or mark it a one-shot: the user (an empty ASKER), the workspace itself,
// its owner (its parent), or the workspace its merge lands into. Whether the
// workspace itself may land is narrowed further by OwnerLands, which the act
// decides.
func (ws Workspace) MayAct(asker WorkspaceID) bool {
	return asker == "" || asker == ws.ID || (ws.Parent != nil && asker == *ws.Parent) ||
		(ws.LandsInto != nil && asker == *ws.LandsInto)
}

// landsIntoColumn reads, beside a workspace row, the workspace its merge
// lands into: the one whose worktree is its creation job's target directory
// (Workspace.LandsInto). A target directory that is no workspace's (the
// repository's main checkout, whose main workspace a merge onto the primary
// branch never names as a target workspace) reads NULL, and so does a
// workspace with no creation job. Worktree directories are unique, so at most
// one row matches.
const landsIntoColumn = `(SELECT target_ws.id FROM creation_jobs JOIN workspaces AS target_ws ON target_ws.dir = creation_jobs.target_dir ` +
	`JOIN repositories ON repositories.id = workspaces.repo_id ` +
	`WHERE creation_jobs.workspace_id = workspaces.id AND target_ws.id != workspaces.id AND target_ws.dir != repositories.dir)`
