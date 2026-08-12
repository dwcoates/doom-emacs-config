# FROZEN CONTRACT: slash-command durability and the ephemeral class

**User pre-approved. This is the frozen contract plus the DETERMINED e2e suite.
Every implementation agent receives it byte-for-byte and may not deviate. A
deviation discovered mid-implementation is surfaced, never silently absorbed.**

The e2e suite is specified here rather than authored alongside the
implementation, deliberately: the tests are the OPERATIONAL READING of this
contract, so every implementation scope builds against one interpretation
instead of each resolving the ambiguities its own way.

---

## Part 1 — What the CLI actually emits, verified

Read from a live transcript rather than reasoned about.

Running a slash command makes the Claude CLI append synthetic records. There are
**two distinct shapes**, and only one was previously documented.

### Shape A — user-type records

Observed for `/model`, `/effort`, `/compact`.

Fields: `cwd`, `entrypoint`, `gitBranch`, `isSidechain`, `message`, `parentUuid`,
`promptId`, `sessionId`, `timestamp`, `type`, `userType`, `uuid`, `version`.

- `type` is `"user"`, and the record is **NOT** flagged `isMeta`.
- The content head is the only envelope signal, e.g.
  `<command-name>/model</command-name>\n<command-message>model</command-message>`.
- Carries `promptId`, which GROUPS a submission's records. Measured: 57 records
  collapsed to 4 distinct `promptId` values, one group holding 48 records.

### Shape B — system/local_command records

Observed for `/context`.

Fields: `content`, `cwd`, `entrypoint`, `gitBranch`, `isMeta`, `isSidechain`,
`level`, `parentUuid`, `sessionId`, `subtype`, `timestamp`, `type`, `userType`,
`uuid`, `version`.

- `type` is `"system"`, `subtype` is `"local_command"`, and it **IS** flagged
  `isMeta`.
- Carries **no** `promptId`.

### The documentation defect this corrects

`machinery.go` states these records "are NOT flagged isMeta, so nothing in the
envelope distinguishes them from something the user typed; the content head is
the only signal the CLI leaves."

That is TRUE for Shape A and FALSE for Shape B. Shape B carries both `isMeta`
and `subtype: "local_command"`, which are structural envelope signals. The
comment must be corrected as part of this work.

### `prompt_id` is already parsed and never read

`transcript.proto:180` declares `string prompt_id = 12` ("user only"), so the
sidecar already carries it. A repo-wide search for `GetPromptId` across
`daemon/internal/` and the shim returns nothing. The correlation handle has been
arriving and being discarded.

---

## Part 2 — The instant prompt render is REMOVED

The webapp files an optimistic prompt bubble at submit so the user's words appear
without a round trip. That bubble then has to be reconciled against the durable
line the CLI later writes, which is the entire source of the two-identity
problem and its duplicate bubbles.

**The requirement is killed at the source.** A prompt renders when it round-trips
through the SDK, not before.

- `addLocalPrompt` and the optimistic bubble are removed.
- The adoption and collapse path that reconciled it is removed with it.
- `dropUnackedPrompt` and the unacked concept go with them.
- Prompt receipts (`promptecho.go`) are removed: their only job was to stand in
  for a durable line that had not yet arrived.

Consequence, accepted deliberately: a prompt appears after its round trip rather
than instantly. This is the correct trade, because the alternative is two
identities for one prompt and a correlation that can attach the wrong record.

Nothing about this removes the daemon's *inbound* interception of slash
commands, which is Part 3.

---

## Part 3 — Slash commands become durable where Claude sees them

### The split, which is the core of this contract

- **CLI-handled commands** reach the SDK and the CLI writes records for them.
  `/compact` and `/clear` additionally produce first-class `Event` arms
  (`ContextCleared`, `ContextCompacted`). These become **DURABLE** messages.
- **Daemon-handled commands** never reach the CLI, so nothing durable can exist.
  These stay **EPHEMERAL**, permanently.

**CORRECTION, applied 2026-08-11 during implementation.** This part originally
read "`/model` and its siblings stay EPHEMERAL, permanently", and the code does
not bear that out. The class is decided by WHO ANSWERED the command, which for
`/model` turns on whether an argument followed it:

- `/model <name>` is performed by the daemon through `Manager.SetModel` and
  never reaches the CLI. **EPHEMERAL.**
- Bare `/model` is FORWARDED to the CLI, which answers it and writes a Shape A
  record. **DURABLE.**

This is not a change of rule, it is the same rule stated precisely: the
predicate is `performsLocally()` at the dispatch site, and naming commands
instead of the predicate produced a list that disagreed with the routing. Part 1
observing a Shape A record for `/model` and Part 4 calling `/model` permanently
ephemeral could not both be true, and this is which one survives. Part 5 test 2
therefore drives `/model <name>`.

### Outbound classification replaces discarding

`machinery.go` currently recognizes the CLI's records and throws them away. It
must instead classify them into durable `Message`s carrying
`daemon_intercepted_command`.

- Shape A is identified by content head, as today.
- Shape B is identified by its envelope (`isMeta` + `subtype: "local_command"`),
  which is more reliable than the content head and must be used in preference to
  it for that shape.
- Shape B records are EPHEMERAL, per Part 4, since they carry no `promptId` and
  the user has ruled them out of the durable set.

### The rename

`SessionCommandItem`'s arm becomes `daemon_intercepted_command`. The current name
reads as a command *to* the session rather than one intercepted *from* the user's
input.

---

## Part 4 — The ephemeral class

A message is EPHEMERAL when no durable record exists for it, and never because
the daemon minted its id. The daemon already mints `"detached-work:" + taskID`
over a durable `TaskStarted`, and that stays durable.

### Membership, exhaustively

- Daemon-handled commands Claude never sees — the ones `performsLocally()`
  answers, which is `/model <name>` and its siblings but NOT the bare forms the
  daemon forwards (see the correction in Part 3).
- Shape B system/local_command records.

**CORRECTION, applied 2026-08-11 during implementation.** This list originally
also carried "daemon-synthesized failure cards (e.g. `startFailedCardUUID`)".
It is wrong, and by this part's own definition:
`publishTerminalStartFailure` calls `persistTerminalStartFailure` BEFORE
pushing, and that function's comment states the durable record is the source of
truth for every later reader. A durable record exists, so the card is DURABLE.

The example was drawn from "the daemon minted its id", which this part
explicitly rejects as the test. Whether a record exists is the only test, and a
daemon-synthesized card that the daemon also persists passes it. A
daemon-synthesized card that is NOT persisted would be ephemeral; none is known
in the current tree.

### NOT members

- Permissions. Claude asks for them via `canUseTool`, so they are a real
  conversational fact. The SHIM writes them as events, since it is already a
  store producer and already holds the request while blocking on it. (Scoped to
  the pagination wave, not this one — recorded here so the class is not
  mis-drawn.)
- Prompt receipts, which cease to exist per Part 2.

### The lineage rules, which are structural and not advisory

1. An ephemeral message is ALWAYS a feed row: `parent_message_id` empty,
   `top_level_message_id` equal to its own id.
2. An ephemeral message is NEVER a parent. A durable child naming an ephemeral
   `top_level_message_id` would be unreachable by any store query, and is
   impossible by origin since a store record can only name ids that exist in the
   store.
3. An ephemeral message NEVER names a durable parent either. Otherwise an
   ephemeral card could attach itself into a paged conversation it will vanish
   from.
4. Enforced at the ephemeral constructor, refusing violations, rather than
   checked afterwards.

### Why the class must be structural rather than a flag

Without it, "no durable record exists" is indistinguishable from "the record was
not found". A page missing a permission looks exactly like a page that lost one.
The class makes the absence a stated property that can be checked.

A `oneof` arm rather than a boolean, per the repo's discipline: a flag would let
a durable message claim ephemerality and an ephemeral one claim a record.

---

## Part 5 — The DETERMINED e2e suite

These are the tests. They are specified now so implementation is measured
against one reading. The e2e-authoring agent implements exactly these and may
add more, but may not weaken or reinterpret any of them.

1. **A `/compact` produces a durable record that survives a reload.**
   Issue `/compact`, let it settle, reconnect a frontend, and assert the command
   appears from the STORE rather than from local retention.

2. **A `/model` produces an ephemeral item that does NOT survive a reload.**
   Issue `/model`, reconnect, assert the item is re-pushed from daemon retention
   and that no store record exists for it.

3. **An ephemeral message never appears in a store page query.**
   Assert directly that a query over durable records returns nothing for an
   ephemeral id, and that this is reported as correct rather than as a miss.

4. **A durable message absent from the store is a LOUD failure.**
   The converse of (3), and the one that keeps (3) from being a licence to lose
   things.

5. **A durable child may not name an ephemeral parent.**
   Construct one and assert the constructor refuses it.

6. **An ephemeral message may not name a durable parent.**
   Same, the other direction.

7. **A prompt renders only after its round trip.**
   Submit a prompt and assert no bubble exists before the durable line arrives,
   and exactly one after. This pins Part 2 and is the regression guard against
   the duplicate bubbles.

8. **Shape B is classified by envelope, not content head.**
   Feed a `system`/`local_command` record whose content head does NOT match the
   machinery prefixes and assert it is still classified correctly.

9. **A human prompt quoting a machinery tag stays a prompt.**
   Feed a user prompt whose body begins with `<command-name>` mid-sentence and
   assert it reaches the feed untouched. `machinery.go` already warns about this
   case and it must not regress.

10. **Two slash commands issued close together do not swap identities.**
    The ordering guard. With `promptId` available this should hold by identity
    rather than by order, and the test pins that it does.

---

## Part 6 — Out of scope for this wave

- The store's ownership COLUMNS and the page query itself, which belong to the
  pagination wave (`PLANNED-message-pagination.md`).
- The shim writing permissions as events, recorded in Part 4 so the class is
  drawn correctly but implemented later.
- Deleting `ResyncCmd.from_seq`, `Subscribe`'s unboundedness, and the old
  paging messages, which close in the pagination wave AFTER a bounded tail read
  demonstrably works.

---

## Design principles this contract is held to

- State enums are forbidden; any state is a `oneof` of dedicated messages.
- Every field and message carries a semantic comment explaining purpose,
  behaviour and motivation, never a restatement of its name.
- Absence renders absence: never a zero, a default, or a sentinel standing in
  for a value that did not arrive.
- A bound belongs in the shape of the type, never in a check applied to it.
- NEVER remove, weaken, or bypass error-handling coverage. A refactor that
  changes how a failure manifests adapts the coverage rather than dropping it.
