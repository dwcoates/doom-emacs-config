# Resumed subagent identity

A subagent resumed with SendMessage runs as the same vendor task, under a new
tool call (the send). The footer drew such a resumed agent as a bare
"subagent" row with 0 tokens, and clicking it said "not on screen" (owner
report, workspace footer-activity-updates, 2026-09-30). What must hold: a
resumed subagent's footer row carries the same identity as its original launch
(label, description, tokens, and a jump to its drawn feed entry), whether or
not the daemon or the shim was replaced since the launch.

Shapes ruled by the lead on the owner's behalf (owner cleared the work
unfettered), from the implementing agent's proposal.

## Core design principles

### A writer never claims a book it does not know

- In the lead's terms: when the shim writes a task-stream frame whose owning
  agent it does not know, the write says so explicitly, and the store places
  it in the book that already holds that upsert key; a new key with an unknown
  owner is an ERROR the store reports, never a guess.
- Consequences for the contract: the store's page line states its book as a
  oneof of a known book or "owner unknown" (never a sentinel string), and the
  write-batch success reports every entry it could not place.
- Decisions it reopens: none landed before it. It changes the shim's long-
  standing practice of booking every task-stream frame under the main agent
  (`convertDetached`'s `agentId = known?.agentId ?? context.mainAgentId`).
- What it does not claim: it does not let a producer leave a book unknown when
  it knows it; it does not apply to prompts or peer messages, which always name
  their recipient; it does not move an existing row between books.

### Two facts never share one upsert key

- In the lead's terms: the SendMessage call's own row holds the send, and the
  resumed run's beats and terminal are written under a key that is NOT the
  send's.
- What it does not claim: it does not change the run's activity id or its
  detached-work handle (see the upsert key decision below).

## Landed changes

### 1. `DetachedWorkKindSubagent.commission`

- WHAT: every subagent detached-work announcement states the commission (the
  spawn's `AgentSubagentPrompt`) of the agent it names, resumes included.
  Optional: unset only when the producer holds no record of the spawn.
- WHY: the `detached` origin carries no description, by design, because its
  unit describes the work; a resumed run is detached from the send, which
  describes nothing about the agent. A daemon that came up after the launch
  (the incident: a deploy handover; the launch row was 226 entries behind a
  200-entry replay page) had no source of the agent's identity until a running
  beat arrived, and a beat restated the send's empty input.
- Consequences:
  - The footer describes a resumed row from the announcement itself
    (`daemon/internal/resolve/footer/chips.go` `bindDetachedAgent`,
    `takeCommission`), so the "take the label from a beat" path was deleted.
  - The watcher refuses a `created` announcement whose commission disagrees
    with the start it describes (`daemon.sessionwatcher.detached_commission_conflict`),
    exactly as it refuses a kind conflict.
  - The shim asks the store before folding whenever it lacks the commission
    of a subagent task whose spawn it did not observe
    (`convert/detached.ts` `taskAwaitingAgent`): a resume after a shim
    restart, and a backgrounded subagent's own nested spawn, whose call never
    reaches the stream. Without the ask every nested spawn's announcement
    would record an ERROR.
  - The shim must state it on every subagent announcement, including the
    `created` re-announcements of `store/reconcile.ts` (`announceLiveWork`,
    `resumedAgentAnnouncement`), where it is the recorded start's prompt.
  - A producer that cannot state it records ERROR (`taskCommission`).

### 2. `GetAgentByVendorTaskSuccess.commission` (not the whole start)

- WHAT: the store's answer to "which agent does this vendor task locator name"
  also carries that agent's recorded commission; unset when the store holds the
  agent's lineage but no start.
- WHY: a shim restarted since the spawn learns the agent only from the store,
  and needs the commission to restate it on the resume's announcement and the
  run's frames.
- Deviation from the ruled shape, and why: the lead approved
  `AgentSubagentStart spawn = 2`. The store keeps a start only as the agent
  row's unpacked columns, and `started_at` is not among them (the row's
  `started_at_ms` is the store's receipt clock). A whole start served from the
  record would state an instant nobody observed, and keeping the start whole
  needs a schema change, and the store's lifecycle is nuke-never-migrate
  (`shim-store/internal/db/db.go`), which would wipe every stored conversation
  on deploy. The commission is all any consumer needs, and every field of it
  is recorded except a remote isolation's handles, which a spawn's request
  never states either.
- Consequences: the store answers the commission from the `agent` row; "a start
  was recorded" is the row's `forked_from_caller` column being non-NULL, which
  only the start's own write (`createSpawnedAgent`) sets.

### 3. The resumed run's upsert key (no proto change)

- WHAT: a resumed run's subagent frames (running beats and terminal) are stored
  under `resumed-run:<send activity id>` (shim `store/keys.ts`
  `resumedRunUpsertKey`); the send's own card stays `activity:<send>`.
- WHY: under one key the run's frames replaced the SendMessage card in the
  store (verified: `activity:toolu_017kU1ffaW2CCehor3Na4mze` held a subagent
  update with 379388 tokens and an empty prompt, no send), so a replay drew
  neither the send nor the run's commission.
- Why the run's ACTIVITY ID stays the send's: the contract's minting rule makes
  a detached handle the same bytes as its unit's activity id, so the unit's
  terminal retires the handle by equality. The footer binds the resumed run by
  that handle, the watcher retires it by equality, and the feed draws the run's
  bubble under the send's unit with the agent its announcement named. Minting a
  new activity id would have needed a new handle, breaking all three. The
  collision was storage-level only (one row per upsert key), so the remedy is
  storage-level.
- Verified: the sidecar never writes a resumed run's frames. Its
  task-notification settle refuses any call whose launch it did not read
  (`shim-sidecar/internal/convert/tasknotification.go`), and a send is never a
  launch. So the new key has one writer.
- Consequences: a replayed book now serves two entries with the send's activity
  id, a SendMessage card and a subagent unit, exactly as the live stream already
  delivered them; the daemon's feed keys them as different rows.

### 4. `StorePageLine.book` oneof and `WriteBatchSuccess.unplaced`

- WHAT: a page line's book is `page_agent_id` (known) or `owner_unknown`. An
  unowned line must be an agent frame with its `agent_id` unset; the store files
  it in the book already holding its upsert key, stamps the frame's
  `agent_id` with that book, keeps the stored `top_level`, and from then on it
  is exactly the write a known book would have made. A key no row holds is
  reported in `WriteBatchSuccess.unplaced` and recorded at ERROR by both the
  store and the shim; nothing of it is stored.
- WHY: the task stream is session-wide; a backgrounded subagent's own spawn
  reaches it with no owner. The shim booked such frames under the main agent,
  and the store skipped them because the sidecar had already booked the spawn
  unit in the spawner's book (production: `activity:toolu_01CieP7uiZSR86ZztFV134Gv`
  in book `toolu_016fJ1MXgpBhD13bnUrwNzPE`, three skips at 23:02:58-23:03:10),
  so the nested run's beats and terminal never reached the record.
- Consequences:
  - Shim: `PersistEntry` is a union of a known book and an unknown owner; the
    task-stream's unit frames (beats, terminals, restated shell starts) are
    written owner-unknown exactly when no owner was observed
    (`taskKinds.ownerOf`, the open call); the writer maps the arm and records
    every unplaced entry at ERROR.
  - Store: `classify` validates the arm; `applyEntry` resolves an unowned line
    against the existing row before the identity policy, so every later step
    (ledger, lifecycle tables, watchers) sees a placed line.
  - Sidecar: writes only known books; its three page-line constructors use the
    `page_agent_id` arm.
  - Accepted cost: an unowned beat that arrives before any producer wrote its
    unit's row is lost and reported at ERROR. In production the sidecar wrote
    the nested spawn's row five seconds before the shim's first beat.
  - Accepted cost: a run resumed by a send that a BACKGROUND subagent made has
    no observed owner and a new key (`resumed-run:<send>`), so its frames are
    reported unplaced at ERROR rather than filed in a guessed book.
