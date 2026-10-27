package wsm

// principalLinkDDL is the layout-71 addition: principal_link, the one url a
// workspace's topbar summary once opened. RETIRED: no statement of this build
// reads or writes it, because workspace_links (workspacelinks.go, layout 85)
// replaced it. It stays in the fresh-file DDL until a later breaking step drops it, which
// waits for no running build to read it (migrate.go, A BREAKING CHANGE IS
// SHIPPED AS TWO DEPLOYS).
const principalLinkDDL = `
ALTER TABLE workspaces ADD COLUMN principal_link TEXT;
`
