# Creating a workspace: context, forking and cross-repository work

- `-context fresh` starts an empty conversation. `-context fork -fork WS`
  copies WS's conversation (cut at its last concluded turn).
  - A fork carries only FINISHED turns, never the one in flight. Forking
    `here` mid-turn leaves out everything this turn has found so far, so a
    diagnosis made in this turn goes into the `-prompt`.
  - A fork of ANOTHER workspace still in its FIRST turn would carry nothing,
    so the daemon refuses it (`fork_source_has_no_finished_turn`) before
    anything is made. Use `-context fresh` with everything the new workspace
    needs in the `-prompt`.
  - Forking `here` from inside this workspace's FIRST turn is not refused: the
    create waits until this turn ends (the answer is
    `waiting_for_requester_turn`, not an error), then the fork is made and
    carries this whole turn. Nothing is built until then. So a workspace can
    read and decide in its first turn, run the fork create, and end its turn.
- **A workspace created in another repository forks only to MOVE this
  workspace.** `-context fork -fork here` with another `-repository` is the
  move in `reorganize.md`, where this workspace's
  conversation continues in the repository its work belongs to. Every other
  cross-repository create takes `-context fresh`, and its `-prompt` carries
  everything the new workspace needs.
  - The forked conversation is about the source's repository: its files,
    paths, branch, worktree, and its own `AGENTS.md`, `CLAUDE.md` and
    skills. Only the agent that is moving knows which of that still applies,
    so nobody forks another workspace's conversation into a repository.
  - The moved workspace's first message tells it which repository its
    worktree is of, so it reads that repository's `AGENTS.md` before working.
- **Cross-repository work goes through a workspace in the other repository.**
  When workspace A in repository foo needs work in both foo and another
  repository baz:
  - A does the foo work itself in its own workspace, and does the baz work
    by creating a workspace B as its child:
    `workspace create -repository <baz> -relationship child -parent here`.
    B nests under A in the sidebar, under a divider naming baz, and A owns
    and coordinates it (B lands its own work). A NEVER edits baz's checkout or cuts branches in it by
    hand. Work in baz that is truly separate from A's is a `peer` instead.
  - If baz is not yet registered with agent-repl, A first registers it with
    `repository register` (`repository-register.md`), then
    creates B.
  - B is created `-context fresh` (per the cross-repository rule above),
    with everything B needs carried in its `-prompt`.
  - A coordinates with B through `workspace prompt` and the awaiting mark
    (`workspace set -await`), e.g. integrating B's result; A is given B's
    one update (its conclusion) without subscribing, and subscribes
    continually only if it drives B turn by turn. Example: baz owns the protobufs and foo holds the
    C++ implementation: A asks B for the protobuf change, marks itself
    awaiting, then builds A's C++ integration against B's protobufs.
  - B lands its own change through baz's deploy (`workspace finished here`)
    when its work is done; A may also land it (`workspace finished B`), and
    A lands foo's own.

