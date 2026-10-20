# Conversation investigation

Use this playbook for missing, duplicated, garbled, truncated, or unreplayed
conversation content.

## Data-plane model

Two producers feed the same vendor conversation:

| Producer | Path |
|---|---|
| Stream plane | shim to store |
| File plane | sidecar tails vendor transcript files to store |

The store absorbs replayed writes through `write_ledger` (asked by `write_id`,
never by `entry.write_id`), bumps a global `write_seq` on every insert or
upsert, and persists rows into `entry` — the spine, ordered by first-insert
`position`. Ephemeral traffic is fanned out and not stored.

The store's `book_agent_id` is an agent lineage id (the vendor session that
owns the book), not the daemon's agent-repl session ID. Resolve the mapping
through `identity-correlation.md` before querying.

## Database and safety

The store database is:

```text
~/.cache/agent-repl/store/events.db
```

Query it read-only:

```sh
sqlite3 -readonly ~/.cache/agent-repl/store/events.db "SELECT 1;"
```

Never write to it. Always scope by `book_agent_id` (or `run_id` / `owner_agent`
for a bash run or detached-work row) and always use a bounded `LIMIT`. The
database is on the order of a gigabyte or more, so an unscoped or unindexed
predicate can run for minutes — check `EXPLAIN QUERY PLAN` for a `SEARCH …
USING INDEX` line before trusting a new query. The payload (`entry.frame`) is
an opaque serialized `StoreEntry`; SQLite can answer envelope questions but
cannot decode message content.

## Schema surface

THE SCHEMA IS NOT RESTATED HERE. The authority is
`agent-shim/shim-store/AGENTS.md`, "## The tables" (`entry`, `agent`,
`write_ledger`, `detached_work`, `workflow`) and its "## The write ledger is
retained only as long as absorption can ask" section; the literal DDL is
`agent-shim/shim-store/internal/db/db.go`. Read those before writing a new
query — this file only gives bounded, read-only examples against the CURRENT
schema.

## Bounded queries

Set the book's owning agent id (a main agent or subagent id from
`identity-correlation.md`):

```sh
DB=~/.cache/agent-repl/store/events.db
BOOK=<book_agent_id>
```

Check count and position range for that book:

```sh
sqlite3 -readonly "$DB" "
  SELECT count(*) AS entries, min(position) AS first_pos, max(position) AS last_pos
  FROM entry
  WHERE book_agent_id='$BOOK';"
```

Read recent entries (`entry_book_position` makes this a seek, not a scan):

```sh
sqlite3 -readonly "$DB" "
  SELECT position, plane, kind, run_id, top_level,
         datetime(last_written_at_ms/1000,'unixepoch','localtime') AS ts
  FROM entry
  WHERE book_agent_id='$BOOK'
  ORDER BY position DESC
  LIMIT 40;"
```

Read the kind histogram for that book:

```sh
sqlite3 -readonly "$DB" "
  SELECT kind, count(*)
  FROM entry
  WHERE book_agent_id='$BOOK'
  GROUP BY kind
  ORDER BY count(*) DESC
  LIMIT 30;"
```

Check live (open) detached work for an owning agent — a bash run or a
workflow the record holds no terminal for:

```sh
sqlite3 -readonly "$DB" "
  SELECT work_id, kind, origin_unit,
         datetime(announced_at_ms/1000,'unixepoch','localtime') AS announced
  FROM detached_work
  WHERE owner_agent='$BOOK' AND ended_at_ms IS NULL
  LIMIT 20;"
```

Inspect recent cursors only when the file plane is suspect:

```sh
sqlite3 -readonly "$DB" "
  SELECT path, offset, length(carry),
         datetime(updated_at_ms/1000,'unixepoch','localtime') AS updated
  FROM cursor
  ORDER BY updated_at_ms DESC
  LIMIT 40;"
```

## Investigation sequence

1. Confirm the vendor session mapping (resolves to the book's `book_agent_id`).
2. Run the health sweep and inspect store integrity.
3. Query entry count and position range for the book.
4. Inspect recent kinds around the symptom.
5. Read `shim.log` for the stream producer.
6. Read workspace `sidecar.log` for session-bound file diagnostics.
7. Read genuine global store and sidecar logs for service-owned failures.
8. Compare producer evidence, write-ledger absorption, cursor progress, and
   daemon replay or subscription evidence.
9. Check readiness before attributing behavior to current source.

Use `agent-repl-log-discovery.sh` rather than legacy human-text grep recipes.
All durable runtime records follow the canonical JSONL contract.

## Interpretation

- Zero rows with a known durable conversation means ingest, identity, or
  deployment requires investigation.
- A gap in `position` for a book requires checking whether the absent material
  was ephemeral (never stored) before declaring loss — `position` is
  AUTOINCREMENT across the whole table, not per-book, so a book's own rows are
  never contiguous and a gap alone proves nothing.
- Store rows without frontend replay evidence localize the problem downstream
  of persistence.
- Shim evidence without store rows localizes the problem to stream ingest or
  identity.
- Sidecar cursor movement (`cursor.offset` advancing) without the expected new
  `entry` rows points to conversion, classification, or absorption behavior —
  check `write_ledger` for the write's `write_id` before concluding it was
  dropped: an already-applied `write_id` is absorbed (silently, by design) and
  never re-upserts the row or bumps `write_seq`.
- A missing cursor after reconnect requires checking connection-scoped cursor
  recovery.
- A successful write under the wrong `book_agent_id` can make replay appear
  empty.

When the database and logs cannot show which producer accepted or rejected a
record, report the missing producer or absorption telemetry through
`observability-gaps.md`.
