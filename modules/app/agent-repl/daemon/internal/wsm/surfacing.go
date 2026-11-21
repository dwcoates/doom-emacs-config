package wsm

// surfacedWorkDDL is the layout-153 addition: workspaces.surfaced, which once
// marked a workspace that started a turn while a fold hid it. RETIRED: the
// persisted visibility (visibilitystate.go) states what a fold leaves drawn,
// and nothing reads or writes the column. It stays in the layout, since a
// fresh file and a migrated one must hold the same columns.
const surfacedWorkDDL = `
ALTER TABLE workspaces ADD COLUMN surfaced INTEGER NOT NULL DEFAULT 0;
`
