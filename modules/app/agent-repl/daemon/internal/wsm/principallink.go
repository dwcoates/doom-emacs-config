package wsm

// principalLinkDDL is the layout-71 addition: principal_link, the one url a
// workspace's topbar summary once opened. RETIRED: no statement of this build
// reads or writes it, because workspace_links (workspacelinks.go, layout 85)
// replaced it. Layout 137 drops it (retiredColumnsDDL).
const principalLinkDDL = `
ALTER TABLE workspaces ADD COLUMN principal_link TEXT;
`
