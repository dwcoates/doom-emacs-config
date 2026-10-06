# GetDetachedWork: a task's kind and end, from the store

A task-stream message the shim cannot type is typed by the store's record of
the work, and a re-report of work the record holds as ended writes nothing.

## The defect (2026-10-02 onward, workspace ship-gns)

- About every keep-alive, the shim logged ERROR "a task names no kind this shim
  knows; its announcement is malformed and is refused".
  - Task `bujbjom65`, tool_use_id `toolu_018xxcFy2bK97h1Xrh1KqbE2`, `task_type`
    empty.
  - Twelve notifications for the task: the first (01:45) was genuine and typed;
    eleven later ones were `status: "stopped"` for a task the live set no
    longer knew.
- Cause: each keep-alive rewind replaces the vendor query, and the
  replacement re-reports the old backgrounded shell as stopped.
  - A `task_notification` never states its kind.
  - The fold learns a kind only from `task_started`, into the in-memory
    `TaskKindRegistry`, which forgets it once the task settles and is empty
    after a shim restart.
  - Typed by the long-standing default instead, the notification would have
    settled a shell as an agent run.
- The store knew: `detached_work` row `work_id = origin_unit =
  toolu_018xxcFy2bK97h1Xrh1KqbE2`, kind `bash`, ended.

## Landed shape

1. `store.v1` rpc `GetDetachedWork`, under Recovery in `service.proto`
   (`endpoint_get_detached_work.proto`).
   - Request: `conversation.v1.AgentActivityId unit = 1`, the spawning call's
     activity id, which is the row's `origin_unit` (every task message of the
     run carries it as `tool_use_id`).
     - Unset or empty is refused `invalid_request` naming `unit` (site
       `unit_empty`).
   - Response: `oneof result { success | not_found | failure }`, the store's
     established outcome shape.
   - Success: `GetDetachedWorkKind kind = 1` (oneof `subagent | bash |
     workflow | monitor | unstated`) and `oneof state { live | ended
     { int64 ended_at_ms } }`.
   - Failure: `detail` plus `oneof kind { invalid_request{field} |
     storage_failure }`.
2. Unscoped, like `GetRunSettlements`.
   - The key is the vendor's own call id, compared for equality, and no two
     sessions share one.
   - A lineage scope over `owner_agent` would refuse exactly the runs this
     exists for: a backgrounded subagent's own shells, whose row records no
     owner (seven such `bash` rows in the owner's store on 2026-10-06).
3. The kind is served as recorded, never derived.
   - The `detached` marker (an announcement on the `detached` origin, which
     states no kind the store classifies; every subagent announcement the shim
     writes today) is the `unstated` arm.
   - Two rows for one unit, or a kind the store never writes, is a storage
     failure recorded at ERROR.
4. Shim (`src/store/detached-work.ts`, `convert/detached.ts`
   `taskAwaitingKind`, `engine/session.ts` `kindFromStore`).
   - Asked before folding a `task_started`, a backgrounding `task_updated`, or a
     `task_notification` whose kind nothing this process holds names, once per
     task, awaited in the serial message loop before the resumed-agent ask.
   - A recorded kind becomes the task's kind.
   - A recorded end makes a notification a re-report: INFO, nothing written.
   - Not on record keeps the ERROR refusal, with the store's answer.
   - A store that cannot answer is an ERROR, never masked.

## What it does not claim

- It does not make the store classify a `detached`-origin announcement by its
  `AgentDetachedWork.kind`; a live subagent the shim cannot type is still
  refused, now with `unstated` named in the record.
- It does not change how a notification that no one can type settles.
