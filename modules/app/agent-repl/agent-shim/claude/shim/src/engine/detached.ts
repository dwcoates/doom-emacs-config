/**
 * engine/detached.ts — the live set of detached work, and the verbs that
 * address it.
 *
 * OWNER: the detached-work agent.
 *
 * RESPONSIBILITY. Fold `task_started`, `background_tasks_changed`,
 * `task_updated` and `task_notification` into the set of work that is currently
 * live, maintain the `DetachedWorkId` ↔ underlying-identity mapping, and serve
 * `WatchBash`/`StopBash` from it.
 *
 * THE MAPPING IS ONE LOOKUP, NEVER A SCAN. `task_started` carries BOTH the
 * vendor task id and the tool_use_id it belongs to, so the association is
 * recorded once when it is stated. Reconstructing it later by matching commands
 * or timings would be a guess, and a wrong guess routes one run's output onto
 * another run's stream.
 *
 * WHAT A STOP ACTUALLY IS. `query.stopTask` — the SDK's native per-task stop.
 * There is no process to kill at this boundary: detached work runs INSIDE the
 * agent binary, not as a child of the shim, so the shim owns no process
 * boundary that could reach it. The stopped task's own
 * `system:task_notification` is the terminal fact everything settles on.
 *
 * RE-ADOPTION AFTER A BOUNCE. A re-announced start recovers its ORIGINAL
 * instant FROM THE STORE by unit id, never from memory — memory is exactly what
 * a bounce lost, and restamping the start would make a long-running command
 * look like it just began.
 */
export {};
