package wsm

// pageResidencyDDL is the layout-120 addition: whether Emacs last held each
// workspace's page. RETIRED: a page is loaded exactly while its workspace is
// visible, and the visibility is persisted (visibilitystate.go), so nothing
// reads or writes the table. It stays in the layout, since a fresh file and a
// migrated one must hold the same tables.
const pageResidencyDDL = `
CREATE TABLE workspace_page_residency (
  workspace_id TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  loaded       INTEGER NOT NULL CHECK (loaded IN (0, 1)),
  reported_at  INTEGER NOT NULL
);
`
