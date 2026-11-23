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
  ends, answering `closesWhenTurnEnds`. A close that would be refused (held
  prompts, live detached work, a queued merge, open children or workspaces
  landing into this one) is refused at once, naming what blocks it, and
  nothing is marked. A close that something during the rest of the turn
  makes refused is told to this workspace as a notice when the turn ends,
  and the workspace stays open.
- **A workspace closes, without asking, a workspace it created once that
  workspace has fulfilled its goal.** When this workspace has judged (from the
  child's reply, its status, or its landing) that the created workspace did
  what it was asked, it runs `workspace close WS` at once, never `workspace
  kill`, and needs no permission from the user. It then tells the user in the
  same response which workspace it closed and why. A workspace whose goal is
  not yet met, or whose work this workspace has not reviewed, stays open.
  A child that landed or finished through `workspace finished` closes itself,
  so this covers the children that do not.
