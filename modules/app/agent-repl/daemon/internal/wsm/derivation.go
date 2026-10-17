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
