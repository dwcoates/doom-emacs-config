# Restarting, interrupting and selecting

- **CRITICAL: `workspace interrupt` and `workspace select` run ONLY when the
  user asked for them.** Each acts on what the user is looking at or waiting
  on.
- `workspace restart WS` relaunches WS's shim at once, interrupting its
  running turn and detached work, resumes the SAME session and reloads its
  page. It never compacts or clears context.
  - An agent restarts ANOTHER workspace on its own judgment, without asking,
    in exactly two cases.
  - Case one is unsticking a stuck workspace (a turn that never ends, a vendor
    that will not answer or will not start).
    - Run `workspace restart WS`, which is immediate and the default.
    - Waiting on a stuck workspace is waiting forever.
  - Case two is bringing a workspace up to date with a changed harness
    (metaprompt, system-prompt standing notice, skills, CLAUDE.md), which a
    restart rebuilds.
    - Run `workspace restart WS -when-idle`, so its running turn is not cut.
    - The daemon runs it at once when WS is idle and answers `success`.
    - Otherwise it answers `scheduled`, naming `turn_in_flight`,
      `detached_work` or `already_pending`, and runs the restart when the turn
      and detached work end.
    - On `scheduled`, do not wait, poll or retry.
  - Restarting this workspace itself is not one of these cases.
  - Tell the user in the same response which workspace you restarted (or
    scheduled) and why.
- `workspace interrupt WS` (the bare form) stops WS's running turn.
  - While WS has live detached agents it is refused once, naming them.
  - `-confirm-agents` stops the turn and those agents together.
- `workspace interrupt WS -all-agents` stops WS's live detached agents and
  shells only, never the turn.
  - It answers `turnStillRunning`, naming the turn, when no detached work was
    live but the turn runs: stop that turn with the bare form.
  - `nothingRunning` means WS has neither a turn nor detached work running.
- `workspace select WS` makes WS the workspace the editor and the webapp show.
