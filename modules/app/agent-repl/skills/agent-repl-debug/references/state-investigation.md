# State investigation

Use this playbook for wrong workspace colors, stuck states, unexpected
transitions, or questions about why one state outranks another.

## Database and safety

The SSM database is:

```text
~/.cache/agent-repl/ssm/state.db
```

It is SQLite in WAL mode and is owned by `daemon/internal/ssm`. Query it
read-only:

```sh
sqlite3 -readonly ~/.cache/agent-repl/ssm/state.db "SELECT 1;"
```

Never write to this database. The SSM owns a single ordered writer and its
ordering is part of correctness.

## Schema

The SSM keeps three append-only histories:

```sql
CREATE TABLE workspace_state (
  workspace TEXT NOT NULL,
  session_id TEXT,
  state TEXT NOT NULL,
  cause_kind TEXT NOT NULL,
  cause_seq INTEGER,
  at INTEGER NOT NULL,
  task_id TEXT,
  PRIMARY KEY (workspace, at)
);

CREATE TABLE session_connectivity (
  workspace TEXT NOT NULL,
  agent_repl_session_id TEXT NOT NULL,
  controller_generation_id TEXT NOT NULL,
  state TEXT NOT NULL,
  cause_kind TEXT NOT NULL,
  at INTEGER NOT NULL,
  PRIMARY KEY (workspace, at)
);

CREATE TABLE session_fault (
  workspace TEXT NOT NULL,
  agent_repl_session_id TEXT NOT NULL,
  controller_generation_id TEXT NOT NULL,
  component TEXT NOT NULL,
  fault_type TEXT NOT NULL,
  impact TEXT NOT NULL,
  open INTEGER NOT NULL,
  cause_kind TEXT NOT NULL,
  at INTEGER NOT NULL
);
```

`workspace` is an absolute directory. `at` is Unix milliseconds.

## Resolution model

Resolve two independent current facts:

- Session connectivity: `hibernated`, `connecting`, `operational`,
  `degraded`, or `unavailable`.
- Session status: `ready`, `thinking`, `permission`, `done`, `interrupted`,
  `vendor-blocked`, or `monitoring`.

The latest `session_connectivity` row selects the current agent-repl session
and controller generation. `degraded` is derived only when that generation's
latest open fault windows include at least one `impact='connectivity'` record.
A fault from any retired generation is historical evidence and cannot affect
the current result. Feature, command, and turn-terminal faults remain
diagnostic and do not change connectivity.

Session status is resolved from the current session's lifecycle rows.
`monitoring` is derived from live background-task count. Connectivity outranks
status only for the primary color/word; status remains independently
queryable.

Rows using `wired`, `starting`, `severed`, `dormant`, `degraded`, and
`degraded_clear` are legacy evidence. They are useful when reconstructing an
older daemon's decisions, but they do not control current composite
connectivity once a controller generation exists.

Merge states outrank the color ladder:

1. `merge_conflict`
2. `merge_failed`
3. `merged`
4. `merging`
5. `merge_queued`

The primary projection is:

1. `hibernated`: teal, asleep.
2. `connecting`: blue, starting.
3. `degraded`: blue, impaired, with the active connectivity fault summary.
4. `unavailable`: blue, unavailable.
5. `operational`: the session-status presentation.

Read the current implementation in `daemon/internal/ssm/resolve.go` when the
exact precedence is under investigation. The SQL query is authoritative.

## Query a workspace

Set the exact absolute workspace path:

```sh
WS=/absolute/workspace
```

Read connectivity lifecycle:

```sh
sqlite3 -readonly ~/.cache/agent-repl/ssm/state.db "
  SELECT datetime(at/1000,'unixepoch','localtime') AS time,
         state, cause_kind, agent_repl_session_id,
         controller_generation_id
  FROM session_connectivity
  WHERE workspace='$WS'
  ORDER BY at DESC
  LIMIT 40;"
```

Read every fault edge for the current generation:

```sh
sqlite3 -readonly ~/.cache/agent-repl/ssm/state.db "
  WITH current_generation AS (
    SELECT controller_generation_id
    FROM session_connectivity
    WHERE workspace='$WS'
    ORDER BY at DESC LIMIT 1
  )
  SELECT datetime(at/1000,'unixepoch','localtime') AS time,
         component, fault_type, impact, open, cause_kind,
         agent_repl_session_id, controller_generation_id
  FROM session_fault
  WHERE workspace='$WS'
    AND controller_generation_id = (
      SELECT controller_generation_id FROM current_generation
    )
  ORDER BY at DESC
  LIMIT 100;"
```

Then read the current session's status lifecycle from `workspace_state`.
Group fault rows by `(component, fault_type)` and take the latest edge for each
key. Record both resolved dimensions, the selected session/controller
identity, and every open fault. Never summarize the result as a single opaque
state.

## Diagnostic cautions

- Start with session connectivity for a blue workspace.
- A newly visible color may expose an older winning row after a higher-priority
  projection clears.
- `vendor-blocked` is a session-status turn outcome rather than a permanent latch.
- `monitoring` is derived and will not appear as a stored row.
- A matching recovery must close the same generation, component, and fault
  type. Do not let one component's close stand in for another's.
- Legacy connectivity/degradation rows remain inert historical evidence.
- `memory-state.el` does not decide the rendered color.

## Cross-check

After resolving the composite:

1. Resolve both session and controller-generation identity.
2. Read daemon logs for lifecycle and every active fault's `cause_kind`.
3. Check readiness before comparing source behavior with runtime behavior.
4. Audit whether the transition log contains the inputs and branch decision
   needed to explain both dimensions and each fault window.

If the database has a row but no corresponding transition evidence, record a
logging gap. If the user-visible state has no explainable winning row, record
a state-observability or resolution defect rather than inventing a cause.
