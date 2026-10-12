# Conversation place and fork seeding

Feeds were ordered and bounded by arrival: the store positions rows by first
insert and the daemon ranks rows by (plane, publication seq). Any history that
reaches the store out of its own order — a transcript re-read, a resume, a
fork's copied transcript — was drawn after the conversation that followed it,
and a fork's feed was a re-ingested copy under new identities instead of its
parent's feed. The owner (2026-09-27) asked for both to be fixed and delegated
the proto decisions to the lead.

## Settled decisions

- **Every entry carries a conversation place** (`ConversationPlace{at_ms,
  ordinal}`), stamped by the producer: the sidecar from the vendor record's
  timestamp and the entry's index within the record (re-reads mint identical
  places); the shim from the instant it first observed the fact. The store
  keeps the FIRST stated place (like the first turn), pages books by place, and
  serves it on every line/entry as `recorded_place`, or its first-insert
  instant as `received_place` when no write stated one. The daemon ranks rows
  by place, so the newest-cut bound is the newest cut in conversation order.
  Why a timestamp: transcript line offsets are per-file and parentUuid depth
  resets at compaction; the vendor timestamp survives every copy including
  remint, and all producers share the host clock.
- **A book can be read as it stood at an instant** (`ConversationThrough`
  position on store ReadAgentPage and shim ReadHistory), which is how a fork's
  daemon reads its parent's feed up to the fork point.
- **The durable fork point is daemon persistence, not wire**: wsm
  `fork_segments(workspace_id, depth, book_agent_id, above_at_ms,
  through_at_ms)` copied down the ancestor chain at fork time, plus
  `workspace_books(workspace_id, main_agent_id)`. The daemon composes a fork's
  feed from its own book after the fork instant plus each ancestor's book
  through its fork instant.
- **Store migration (lead's decision, under the owner's delegation):** a
  one-time IN-PLACE column add (place columns filled from
  first_inserted_at_ms, recorded=0, plus the place index), NOT a schema-version
  nuke — a nuke loses stream-only rows (agent-repl's own prompts, asks,
  stream-only frames) that no producer can rebuild. Old rows are served as
  `received_place` until the conversion-version heal re-derives them; that heal
  must mint NEW write ids for re-derived writes or ledger absorption swallows
  them.
- **Copied-prefix marker (lead's decision):** a sidecar-side manifest file
  (`<session>.agent-repl-fork.json` naming the parent session and the copied
  prefix's byte length), written before the forked transcript is renamed into
  place — NOT a field injected into vendor transcript records, so the vendor's
  own file format is never altered and resume needs no proof that the CLI
  tolerates foreign fields. The sidecar skips the marked prefix (convert,
  store nothing, count). Place filtering alone already yields correct feeds
  for legacy forks; skipping is cost and hygiene.
- Copied records do NOT keep their uuids (PortTranscript re-mints every
  identity: daemon/internal/account/transcript.go:367-390,
  daemon/internal/remint/remint.go:43-86), so aliasing copied rows to the
  parent's rows by uuid is impossible; timestamps survive remint.

## Landed changes

- **2026-09-27, branch `proto/conversation-place`** — conversation.v1
  history.proto (ConversationPlace, ConversationThrough, HistoryEntryAt.place,
  place-ordered HistoryPage), store.v1 store.proto (StoreEntry.place,
  StoreLineAt.place, StoreItemPointer names an item not a position),
  endpoint_read_agent_page.proto (position oneof with `through`,
  `unknown_agent`), endpoint_open_agent_session.proto and shim
  endpoint_watch_agent.proto (known_through = written-since, not place),
  shim endpoint_read_history.proto (`through`). Consequences: the daemon
  removes the planePorted/planeInherited heuristics and boundHides' cross-plane
  guess; ported prompts stop drawing (kept for fork naming); a watched entry
  may arrive placed before delivered rows and is filed by place (earlier-placed
  rows of closed turns are served by pages, not pushed); clock skew between
  the shim's observation clock and vendor timestamps can misorder adjacent
  rows by sub-second amounts; a wrong first-stated place is permanent short of
  re-derivation.
