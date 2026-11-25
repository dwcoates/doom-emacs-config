# Finishing, opening and closing

- `workspace finished WS -placement back|front` says WS's work is done and
  runs its repository's finish hooks once the turn ends, then queues its
  DEPLOY; its repository's lifecycle config decides both (`lifecycle-config.md`). Run
  it on `here` when this workspace is done (see `/agent-repl-merge-queue` for
  when, and for the version increment a deploy onto a versioned main branch
  carries), or on a child whose owner you are, after reviewing its work
  against yours. WS closes once its deploy succeeds.
  - A deploy remediation or escalation names no `-placement`: its finish
    lands directly, never in the queue, and its brief gives the command.
- `workspace open WS` reopens a closed workspace. Reopening is also how rework
  reaches a closed workspace whose change needs it (the metaprompt's "Rework"
  section).
- `workspace close WS` closes a workspace whose work is done or abandoned; its
  worktree stays. `close here` closes this workspace once the current turn
  ends, answering `closesWhenTurnEnds`.
  - **A close something still blocks STANDS.** Blocked by open children or
    workspaces landing into it, a turn running, live detached work, held
    prompts or a queued merge, the close is registered as the workspace's
    standing close request and answers `stands` with what it waits on (also
    said on stderr). agent-repl closes the workspace the moment nothing blocks
    it any more, judging it again on every edge that can clear a blocker, and
    across a daemon restart. So a workspace never promises to "close again
    once the child closes"; its close already stands.
  - The request is withdrawn by `workspace close WS -cancel`, by any prompt
    the user sends the workspace, and by the user releasing one of its held
    prompts. A notice or another agent's prompt does not withdraw it: the
    close waits for that turn to end.
  - While it stands, the footer reads `waiting to close` with what it waits
    on, and `workspace status WS` reports it as `closeRequest`.
  - The editor's close gesture never stands: it is refused at once in the
    footer instead.
- **A workspace closes, without asking, a workspace it created once that
  workspace has fulfilled its goal.** When this workspace has judged (from the
  child's reply, its status, or its landing) that the created workspace did
  what it was asked, it runs `workspace close WS` at once, never `workspace
  kill`, and needs no permission from the user. It then tells the user in the
  same response which workspace it closed and why. A workspace whose goal is
  not yet met, or whose work this workspace has not reviewed, stays open.
  A child that landed or finished through `workspace finished` closes itself,
  so this covers the children that do not.
