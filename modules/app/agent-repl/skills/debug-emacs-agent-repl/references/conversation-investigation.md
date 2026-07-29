# Conversation investigation

Use this playbook for missing, duplicated, garbled, truncated, or unreplayed
conversation content.

## Data-plane model

Two producers feed the same vendor conversation:

| Producer | Path |
|---|---|
| Stream plane | shim to store |
| File plane | sidecar tails vendor transcript files to store |

The store deduplicates overlap with `dedup_key`, assigns gapless sequence
numbers per vendor session, persists durable classes, and fans out ephemeral
events without storing them.

The store's `session_id` is the vendor session ID. It is not the daemon's
agent-repl session ID. Resolve the mapping through
`identity-correlation.md` before querying.

## Database and safety

The store database is:

```text
~/.cache/agent-repl/store/events.db
```

Query it read-only:

```sh
sqlite3 -readonly ~/.cache/agent-repl/store/events.db "SELECT 1;"
```

Never write to it. Always filter by session and always use a bounded result.
The payload is an opaque protobuf blob. SQLite can answer envelope questions
but cannot decode message content.

## Schema surface

Relevant tables:

```sql
CREATE TABLE event (
  session_id TEXT NOT NULL,
  seq INTEGER NOT NULL,
  plane INTEGER NOT NULL,
  class INTEGER NOT NULL,
  kind TEXT NOT NULL,
  task_id TEXT,
  uuid TEXT,
  dedup_key TEXT,
  produced_at INTEGER NOT NULL,
  payload BLOB NOT NULL,
  PRIMARY KEY (session_id, seq)
);

CREATE TABLE cursor (
  file_id TEXT PRIMARY KEY,
  path TEXT NOT NULL,
  offset INTEGER NOT NULL,
  carry BLOB,
  updated_at INTEGER NOT NULL
);
```

Ephemeral events are absent by design. Absence from `event` is not proof that
an ephemeral event never occurred.

## Bounded queries

Set the vendor session ID:

```sh
DB=~/.cache/agent-repl/store/events.db
SID=<vendor-session-id>
```

Check count and sequence range:

```sh
sqlite3 -readonly "$DB" "
  SELECT count(*) AS events, min(seq) AS first_seq, max(seq) AS last_seq
  FROM event
  WHERE session_id='$SID';"
```

Read recent envelope data:

```sh
sqlite3 -readonly "$DB" "
  SELECT datetime(produced_at/1000,'unixepoch','localtime') AS time,
         seq, plane, class, kind, task_id, uuid, dedup_key
  FROM event
  WHERE session_id='$SID'
  ORDER BY seq DESC
  LIMIT 40;"
```

Read the kind histogram:

```sh
sqlite3 -readonly "$DB" "
  SELECT kind, count(*)
  FROM event
  WHERE session_id='$SID'
  GROUP BY kind
  ORDER BY count(*) DESC
  LIMIT 30;"
```

Inspect recent cursors only when the file plane is suspect:

```sh
sqlite3 -readonly "$DB" "
  SELECT path, offset, length(carry), datetime(updated_at/1000,'unixepoch','localtime')
  FROM cursor
  ORDER BY updated_at DESC
  LIMIT 40;"
```

## Investigation sequence

1. Confirm the vendor session mapping.
2. Run the health sweep and inspect store integrity.
3. Query event count and sequence range.
4. Inspect recent kinds around the symptom.
5. Read `shim.log` for the stream producer.
6. Read workspace `sidecar.log` for session-bound file diagnostics.
7. Read genuine global store and sidecar logs for service-owned failures.
8. Compare producer evidence, dedup outcomes, cursor progress, and daemon
   replay or subscription evidence.
9. Check readiness before attributing behavior to current source.

Use `agent-repl-log-discovery.sh` rather than legacy human-text grep recipes.
All durable runtime records follow the canonical JSONL contract.

## Interpretation

- Zero rows with a known durable conversation means ingest, identity, or
  deployment requires investigation.
- Sequence holes require checking whether the absent material is ephemeral
  before declaring loss.
- Store rows without frontend replay evidence localize the problem downstream
  of persistence.
- Shim evidence without store rows localizes the problem to stream ingest or
  identity.
- Sidecar cursor movement without expected events points to conversion,
  classification, or dedup behavior.
- A missing cursor after reconnect requires checking connection-scoped cursor
  recovery.
- A successful write under the wrong session ID can make replay appear empty.

When the database and logs cannot show which producer accepted or rejected a
record, report the missing producer or dedup telemetry through
`observability-gaps.md`.
