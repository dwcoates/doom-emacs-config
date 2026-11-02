package wsm

// walkLandingDDL is the layout-98 addition: walk_landed_at, the instant a
// step of Emacs's recency walk made the workspace current
// (SetCurrentKeepingLastSelected), in nanoseconds; NULL on every workspace
// the walk does not stand on, which every existing row is.
//
// THE WALK'S LANDING IS A SELECTION ONCE THE WALK ENDS. A step leaves
// last_selected_at standing, so the walk never reorders the history it walks
// (agentrepl.v1 SelectWorkspaceRequest.keep_selection_history). But the step
// the user stopped on is the workspace they then looked at, and a recorded
// selection that leaves it (`SPC p p`, a tab chord, an agent's select) must
// leave it as the workspace selected before: without this column its
// last_selected_at kept the walk's starting order, and the next walk skipped
// it (owner report, 2026-11-01). The instant is kept here, not in memory,
// because the walk and the selection that ends it may land on two daemons
// across a handover. At most one row carries it: setCurrent clears it on
// every selection. IT IS ADDITIVE: the previous build's writes name no such
// column. The fresh-file schema appends this same text (schema.go).
const walkLandingDDL = `
ALTER TABLE workspaces ADD COLUMN walk_landed_at INTEGER;
`
