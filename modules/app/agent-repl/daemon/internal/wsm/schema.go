package wsm

// schemaDDL is the whole layout, created in one transaction on a fresh file
// and never migrated. Instants are integer unix nanoseconds; a nullable
// instant column is NULL when the fact has not happened.
//
// Two ordering facts shape the foreign keys. A CREATION JOB precedes its
// workspace's registration (a workspace is registered only after its worktree
// is materialized), so creation_jobs carries no reference to workspaces and
// Forget deletes it explicitly. Everything else — sessions, leases, held
// prompts, turns, idempotency claims, the merge ledger, the per-repo merge
// queue, faults — exists only after registration and cascades with the
// workspace row.
const schemaDDL = `
CREATE TABLE layout (
  id      INTEGER PRIMARY KEY CHECK (id = 1),
  version INTEGER NOT NULL
);

CREATE TABLE repositories (
  id             TEXT PRIMARY KEY,
  dir            TEXT NOT NULL UNIQUE,
  name           TEXT NOT NULL,
  default_branch TEXT NOT NULL
);

CREATE TABLE tasks (
  id         TEXT PRIMARY KEY,
  title      TEXT NOT NULL,
  done       INTEGER NOT NULL,
  created_at INTEGER NOT NULL
);

CREATE TABLE workspaces (
  id               TEXT PRIMARY KEY,
  repo_id          TEXT NOT NULL REFERENCES repositories(id),
  dir              TEXT NOT NULL UNIQUE,
  name             TEXT NOT NULL,
  branch           TEXT NOT NULL,
  parent_branch    TEXT NOT NULL,
  closed           INTEGER NOT NULL,
  attention        INTEGER NOT NULL,
  priority         INTEGER,
  task_id          TEXT REFERENCES tasks(id) ON DELETE SET NULL,
  is_current       INTEGER NOT NULL,
  last_selected_at INTEGER,
  merged_at        INTEGER,
  serving_instance TEXT,
  created_at       INTEGER NOT NULL
);

CREATE UNIQUE INDEX workspaces_one_current ON workspaces(is_current) WHERE is_current = 1;

CREATE TABLE creation_jobs (
  workspace_id           TEXT PRIMARY KEY,
  source_branch          TEXT NOT NULL,
  source_dir             TEXT NOT NULL,
  target_dir             TEXT NOT NULL,
  layout_origin          TEXT NOT NULL,
  actions_before         TEXT NOT NULL,
  actions_after          TEXT NOT NULL,
  base_ref               TEXT NOT NULL,
  materialized           INTEGER NOT NULL,
  one_shot               INTEGER NOT NULL,
  initial_prompt         TEXT NOT NULL,
  consented_ungated_mode TEXT NOT NULL,
  created_at             INTEGER NOT NULL
);

CREATE TABLE sessions (
  workspace_id       TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  vendor_session_id  TEXT NOT NULL,
  config_dir         TEXT NOT NULL,
  model              TEXT NOT NULL,
  permission_mode    TEXT NOT NULL,
  started_at         INTEGER NOT NULL,
  last_engagement_at INTEGER NOT NULL,
  terminal_kind      TEXT,
  terminal_detail    TEXT,
  terminal_at        INTEGER
);

CREATE TABLE leases (
  id           TEXT PRIMARY KEY,
  workspace_id TEXT NOT NULL UNIQUE REFERENCES workspaces(id) ON DELETE CASCADE,
  holder       INTEGER NOT NULL,
  policy       INTEGER NOT NULL,
  acquired_at  INTEGER NOT NULL
);

CREATE TABLE held_prompts (
  turn_id                  TEXT PRIMARY KEY,
  workspace_id             TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  text                     TEXT NOT NULL,
  origin                   TEXT NOT NULL,
  target                   TEXT,
  hold_kind                INTEGER,
  classification_interject INTEGER,
  classification_reason    TEXT,
  classification_failed    INTEGER,
  classification_at        INTEGER,
  tombstone_kind           TEXT,
  tombstone_at             INTEGER,
  queued_at                INTEGER NOT NULL
);

CREATE INDEX held_prompts_by_workspace ON held_prompts(workspace_id);

CREATE TABLE turns (
  id             TEXT PRIMARY KEY,
  workspace_id   TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  text           TEXT NOT NULL,
  origin         TEXT NOT NULL,
  address        TEXT,
  displaced      INTEGER NOT NULL,
  started_at     INTEGER NOT NULL,
  closed_at      INTEGER,
  close_kind     INTEGER
);

CREATE INDEX turns_by_workspace ON turns(workspace_id);

CREATE TABLE idempotency_keys (
  workspace_id    TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  idempotency_key TEXT NOT NULL,
  turn_id         TEXT NOT NULL,
  claimed_at      INTEGER NOT NULL,
  PRIMARY KEY (workspace_id, idempotency_key)
);

CREATE TABLE merge_ledger (
  lease_id     TEXT PRIMARY KEY,
  workspace_id TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  opened_at    INTEGER NOT NULL
);

CREATE INDEX merge_ledger_by_workspace ON merge_ledger(workspace_id);

CREATE TABLE merge_tab_intervals (
  lease_id   TEXT NOT NULL REFERENCES merge_ledger(lease_id) ON DELETE CASCADE,
  round      INTEGER NOT NULL,
  kind       TEXT NOT NULL,
  started_at INTEGER NOT NULL,
  ended_at   INTEGER,
  outcome    TEXT NOT NULL,
  PRIMARY KEY (lease_id, round, kind)
);

CREATE TABLE faults (
  id           TEXT PRIMARY KEY,
  workspace_id TEXT REFERENCES workspaces(id) ON DELETE CASCADE,
  kind         TEXT NOT NULL,
  detail       TEXT NOT NULL,
  opened_at    INTEGER NOT NULL,
  resolved_at  INTEGER
);

CREATE TABLE merge_queue_repos (
  repo_key TEXT PRIMARY KEY,
  paused   INTEGER NOT NULL
);

CREATE TABLE merge_queue (
  repo_key     TEXT NOT NULL,
  workspace_id TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  seq          INTEGER NOT NULL,
  state        INTEGER NOT NULL,
  enqueued_at  INTEGER NOT NULL,
  PRIMARY KEY (repo_key, workspace_id)
);

CREATE UNIQUE INDEX merge_queue_seq ON merge_queue(repo_key, seq);

CREATE TABLE drain_schedule (
  id       INTEGER PRIMARY KEY CHECK (id = 1),
  reason   TEXT NOT NULL,
  deadline INTEGER NOT NULL,
  set_at   INTEGER NOT NULL
);
`
