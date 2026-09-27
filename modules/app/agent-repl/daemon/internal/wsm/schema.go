package wsm

// schemaDDL is the whole layout, created in one transaction on a FRESH file.
// An EXISTING file is never recreated from it: a file stamped with an older
// layout is carried forward by the ordered list in migrate.go, so the user's
// workspace state survives a schema change. Instants are integer unix nanoseconds; a nullable
// instant column is NULL when the fact has not happened.
//
// Two ordering facts shape the foreign keys. A CREATION JOB precedes its
// workspace's registration (a workspace is registered only after its worktree
// is materialized), so creation_jobs carries no reference to workspaces and
// Forget deletes it explicitly. Everything else — sessions, leases, held
// prompts, turns, idempotency claims, the merge ledger, the per-repo merge
// queue, faults, a fork's ported prompts — exists only after registration and cascades with the
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
  -- A WORKSPACE WHOSE REPOSITORY IS UNREGISTERED IS AN INVARIANT VIOLATION and
  -- must be impossible (owner ruling, 2026-09-13). This reference is where
  -- SQLite holds it; the layout-3 fixture carries the same declaration, so no
  -- migration adds it and no migration may rebuild the table without it. See
  -- repoinvariant.go for the other three layers and the boot-time report.
  repo_id          TEXT NOT NULL REFERENCES repositories(id),
  dir              TEXT NOT NULL UNIQUE,
  name             TEXT NOT NULL,
  branch           TEXT NOT NULL,
  parent_branch    TEXT NOT NULL,
  parent_id        TEXT REFERENCES workspaces(id) ON DELETE SET NULL,
  closed           INTEGER NOT NULL,
  attention        INTEGER NOT NULL,
  priority         INTEGER,
  task_id          TEXT REFERENCES tasks(id) ON DELETE SET NULL,
  is_current       INTEGER NOT NULL,
  last_selected_at INTEGER,
  -- When the workspace LAST DID REAL WORK: the most recent turn start or turn
  -- close, stamped inside the turn writes (PutTurn/CloseTurn) at the genuine
  -- activity edge. NULL until the workspace takes its first turn. It is the
  -- roster when-column's source and is NOT last_selected_at, which times
  -- VIEWING recency: the column must be stable across selection, so it never
  -- reads the selection stamp. See lastActivityAtDDL and resolve/sidebar's
  -- when().
  last_activity_at INTEGER,
  merged_at        INTEGER,
  serving_instance TEXT,
  -- The pid of a shim a daemon SPAWNED for this workspace, written at the
  -- instant the fork returned and cleared when that spawn is stood down. It
  -- is NOT sessions.shim_pid: that one names the shim SERVING A SESSION and
  -- exists only once a session row does, so it says nothing at all about the
  -- window between the fork and the shim's first bound socket -- the window
  -- in which a successor daemon reads "lock free, socket absent" and spawns a
  -- SECOND shim onto one session socket. See spawnedShimPidDDL and
  -- boot.sequence's starting-survivor wait.
  spawned_shim_pid INTEGER,
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
  -- The DAEMON-minted session identity the host stream echoes. It is not the
  -- vendor's: sessions rotate under one workspace and Emacs correlates
  -- transcripts, health probes and fault windows against this one.
  host_session_id    TEXT NOT NULL,
  vendor_session_id  TEXT NOT NULL,
  config_dir         TEXT NOT NULL,
  -- The root the USER CHOSE (SelectAccount), '' when nobody chose one and the
  -- path routing decides. Distinct from config_dir, which records where the
  -- session actually came up.
  selected_config_dir TEXT NOT NULL DEFAULT '',
  model              TEXT NOT NULL,
  permission_mode    TEXT NOT NULL,
  started_at         INTEGER NOT NULL,
  last_engagement_at INTEGER NOT NULL,
  shim_pid           INTEGER,
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
  said                     BLOB NOT NULL,
  origin                   TEXT NOT NULL,
  target                   TEXT,
  hold_kind                INTEGER,
  hold_schedule_id         TEXT,
  classification_arm       INTEGER,
  classification_reason    TEXT,
  classification_command   INTEGER,
  classification_at        INTEGER,
  accepted                 INTEGER NOT NULL,
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

` + portedPromptsDDL + `
CREATE TABLE idempotency_keys (
  workspace_id    TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  idempotency_key TEXT NOT NULL,
  turn_id         TEXT NOT NULL,
  claimed_at      INTEGER NOT NULL,
  -- When the prompt queue ACCEPTED the bound turn's submission (delivered it
  -- or durably held it); NULL while it has not. Only an accepted claim refuses
  -- a retry as a duplicate. See acceptedAtDDL and ClaimIdempotencyKey.
  accepted_at     INTEGER,
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
  evidence     TEXT NOT NULL,
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
` + feedTextScaleDDL

// feedTextScaleDDL is the layout-10 addition — the single daemon-global feed
// text zoom (frontend.v1.FeedTextScale), persisted so the zoom survives a
// daemon restart. It is a SINGLETON like drain_schedule: the CHECK (id = 1)
// makes "one preference" structural rather than conventional. Kept apart from
// the rest of the schema for the reason portedPromptsDDL is: TWO paths write it
// — a fresh file gets it as part of schemaDDL, a layout-9 file gets it from the
// 9 -> 10 migration — so one text keeps a migrated file and a created one from
// drifting into two shapes. An absent row reads as the default scale (1.0).
const feedTextScaleDDL = `
CREATE TABLE feed_text_scale (
  id    INTEGER PRIMARY KEY CHECK (id = 1),
  scale REAL NOT NULL
);
`

// portedPromptsDDL is the layout-4 addition, kept apart from the rest of the
// schema because TWO paths write it: a fresh file gets it as part of schemaDDL
// above, and a layout-3 file gets it from the 3 -> 4 migration. One text, so a
// migrated file and a created one cannot drift into two different shapes.
const portedPromptsDDL = `
CREATE TABLE ported_prompts (
  workspace_id TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  turn_id      TEXT NOT NULL,
  ordinal      INTEGER NOT NULL,
  text         TEXT NOT NULL,
  origin       TEXT NOT NULL,
  started_at   INTEGER NOT NULL,
  PRIMARY KEY (workspace_id, turn_id)
);

CREATE INDEX ported_prompts_by_workspace ON ported_prompts(workspace_id, ordinal);
`
