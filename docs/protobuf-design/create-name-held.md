# A supplied create name an unlanded workspace holds (`name_held`)

Settled 2026-10-30, implemented in the `ramdisk-investigation-dedup-fix` workspace.

## The failure it closes

- The RAM disk runner keeps exactly one investigation workspace by creating it under the stable name `ramdisk-leak-investigation`.
- It decided "one is already open" by looking for a worktree on that branch, and relied on git refusing an existing branch to stop duplicates.
- The investigation landed on 2026-10-20, and its branch was never deleted.
  - From then on, every leak detection found no worktree and created again.
  - Each create failed in git (`fatal: a branch named ramdisk-leak-investigation already exists`).
  - Each failure also fired the daemon's failed-create report into the main workspace (2026-10-30 18:55:04).

## Decisions

- `agentrepl.v1.CreateWorkspaceError.name_held` (tag 47) carries `agentrepl.v1.CreateWorkspaceNameHeld`.
  - `branch` is the branch the supplied name resolves to.
  - `workspace` is the workspace holding the name.
  - `open` says whether that workspace is open.
- The daemon checks it in `claimSupplied`, under the naming lock that claims the name.
  - This makes "is it already there" and "take the name" one atomic step, so two concurrent creates can never both pass it.
- Only a workspace that has NOT LANDED holds a name.
  - A landed workspace's branch is deleted at its landing (`landedbranch.Retire`), so its name is free to use again.
- A closed workspace that never landed still holds its name (`open` false).
  - Its branch carries work, and reopening it restores from that branch.
- It is a caller-input arm, like `name_reserved`.
  - So it is answered to the caller and never fires the failed-create report.
- The runner reads `name_held` as "the investigation is already there".
  - That replaces the worktree check, and git's refusal is no longer how duplicates are stopped.
