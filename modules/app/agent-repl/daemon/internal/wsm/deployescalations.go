package wsm

// deployEscalationsDDL is the layout-147 addition: each deploy escalation
// workspace the daemon made, with the queue whose deploy it settles. It is
// RETIRED with the escalation workspaces (owner ruling, 2026-11-17: one
// deploy mediator): the layout-168 restart empties it and nothing reads or
// writes it, and it stays only for the layouts that name it.
const deployEscalationsDDL = `
CREATE TABLE deploy_escalations (
  workspace_id TEXT PRIMARY KEY,
  repo_key     TEXT NOT NULL,
  made_at      INTEGER NOT NULL
);
`
