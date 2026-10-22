# A live detached run is answered with its owner

`store.v1.GetLiveWork` answers a session's open obligations across its whole
lineage: the main agent and every subagent at any depth. Its detached runs
were answered as bare ids, and the shim's reconciliation described every one
from the MAIN agent's book. A run a SUBAGENT announced (a background shell the
subagent started) has its start in the subagent's book, so it was never found,
and every daemon handover logged `shim.engine.session` ERROR "live work of
this session has no describable record" for a description the record never
lost (2026-10-22 13:29:11.976, `dashboard-game-review-metrics`, work
`toolu_01NyWesHAyPdAGhHvjKHdoWC`, owned by subagent `a12c46f241564681d`).

## Landed changes

### 1. `store.v1.GetLiveWorkSuccess.live_detached_runs` carries each run with its owner

- **What.** Field 3 (`live_detached`, bare `conversation.v1.DetachedWorkId`s)
  is reserved by number and name. Field 4, `live_detached_runs`, is a list of
  the new `store.v1.GetLiveWorkDetached { work, owner }`.
- **Why.** The owner is the only fact that says which book describes a run,
  and the store already holds it (`detached_work.owner_agent`, the column the
  lineage scope joins on). A run answered without it is the defect, so an
  ownerless run is not a shape the success can carry: both fields are
  required, and a reader treats an item missing either as malformed.
- **Why a message per item, and not a map or two parallel lists.** Parallel
  lists make a length mismatch representable; a map keyed by owner reorders
  the store's announcement order and keys by a value a caller never looks up
  by. One message per item keeps the store's order and makes the pair the unit.
- **Why a new field number, not a type change on field 3.** The store and the
  shim are deployed separately (the store is a launchd service). A type change
  on field 3 would make a skewed reader decode a message as an id; a new number
  makes a skewed reader see an empty list, which reconciliation already treats
  as an idle session.
- **Consequences.**
  - The store's live-work read selects `owner_agent` beside `work_id`
    (`agent-shim/shim-store/internal/db/live.go`, `liveDetachedSQL`).
  - The shim describes each run from its owner's book, one book per distinct
    owner, read back page by page as `readBookFor` already does; an in-process
    run's closing terminal is written into its owner's book.
  - A live handle the shim's own table or level holds, but the record does not
    name, is still described from the main agent's book.
  - `reportUndescribableWork` keeps firing for a handle NO owner's book
    describes, which is the record-plane loss it exists for.
  - Accepted cost: during a deploy that updates the shim before the store (or
    the reverse), one reconciliation sees no live detached runs; the next
    session start after both are updated sees them again.
- **Sweep.** `live_detached`'s only producer and readers are this store read,
  the store's own server log line and integration suite, and the shim's two
  reconciliation sites; nothing else names it. No other declaration is
  obviated.
