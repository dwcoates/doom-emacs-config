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

The state history is append-only:

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
```

`workspace` is an absolute directory. `at` is Unix milliseconds.

## Resolution model

Resolved color is not one timeline. Independent axes contribute their latest
row and SQL precedence chooses the winning candidate:

| Axis | States | Clear token |
|---|---|---|
| Agent | `thinking`, `permission`, `done`, `interrupted`, `ready`, `idle`, `dead`, `vendor_blocked` | none |
| Wired | `dormant`, `starting` | `wired` |
| Backfill | `backfill_failed` | `backfill_ok` |
| Degraded | `degraded` | `degraded_clear` |
| Merge | `merge_conflict`, `merge_failed`, `merged`, `merging`, `merge_queued` | `merge_none` |

An absent wired row resolves as dormant. A clear token contributes no
candidate for its axis.

Merge states outrank the color ladder:

1. `merge_conflict`
2. `merge_failed`
3. `merged`
4. `merging`
5. `merge_queued`

The color precedence is:

1. Blue: `dead`, `degraded`, `dormant`, `backfill_failed`, `starting`.
2. Purple: `vendor_blocked`.
3. Red: `thinking`.
4. Yellow: `idle_async`, derived from live task count rather than stored.
5. Green: `permission`, `done`, `interrupted`, `ready`, `idle`.

Read the current implementation in `daemon/internal/ssm/resolve.go` when the
exact precedence is under investigation. The SQL query is authoritative.

## Query a workspace

Set the exact absolute workspace path:

```sh
WS=/absolute/workspace
```

Read recent transitions:

```sh
sqlite3 -readonly ~/.cache/agent-repl/ssm/state.db "
  SELECT datetime(at/1000,'unixepoch','localtime') AS time,
         state, cause_kind, cause_seq, session_id, task_id
  FROM workspace_state
  WHERE workspace='$WS'
  ORDER BY at DESC
  LIMIT 40;"
```

Read the latest row on each axis rather than assuming the newest overall row
explains the rendered result:

```sh
sqlite3 -readonly ~/.cache/agent-repl/ssm/state.db "
  SELECT datetime(at/1000,'unixepoch','localtime') AS time,
         state, cause_kind, cause_seq, session_id
  FROM workspace_state
  WHERE workspace='$WS'
    AND state IN (
      'thinking','permission','done','interrupted','ready','idle','dead',
      'vendor_blocked','wired','starting','dormant','backfill_failed',
      'backfill_ok','degraded','degraded_clear','merge_conflict',
      'merge_failed','merged','merging','merge_queued','merge_none'
    )
  ORDER BY at DESC
  LIMIT 100;"
```

Group the returned rows by axis and select the latest row for each axis. Apply
the current SQL precedence. Record the winning row's timestamp, cause, and
session.

## Diagnostic cautions

- Start with the wired axis for a blue workspace.
- A newly visible color may expose an older winning row after a higher-priority
  axis clears.
- `vendor_blocked` is an agent-axis turn outcome rather than a permanent latch.
- `idle_async` is derived and will not appear as a stored row.
- Legacy rows for removed tokens may remain inert in old databases.
- `memory-state.el` does not decide the rendered color.

## Cross-check

After identifying the winning row:

1. Resolve its session identity.
2. Read daemon logs for the row's `cause_kind` and `cause_seq`.
3. Check readiness before comparing source behavior with runtime behavior.
4. Audit whether the transition log contains the inputs and branch decision
   needed to explain the row.

If the database has a row but no corresponding transition evidence, record a
logging gap. If the user-visible state has no explainable winning row, record
a state-observability or resolution defect rather than inventing a cause.
