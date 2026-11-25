# Renaming a workspace

- **RENAME.** `workspace set here -name NAME` renames this workspace
  totally: its branch and its worktree directory move with its name, so the
  old name, branch and directory are free afterwards.
  - **When it moves.** The name changes at once. The move of the branch and
    directory runs when no turn and no detached work runs in the workspace.
    The daemon holds the workspace's prompts while it moves, then resumes
    the same conversation in the new directory. An agent renaming its own
    workspace mid-turn sees the move run after its turn ends.
  - **The answer** says where the move stands:
    - `moved`: the branch and directory carry the new name.
    - `owed`: the move runs at the next free point.
    - `none`: the branch and directory already carry the name, or this is a
      repository's main checkout, which keeps its branch and directory.
  - **Refusals** beyond `name_taken` and `invalid_name`:
    - `merging`: a merge holds or names the workspace. Rename it once the
      merge has landed or left the queue.
    - `branch_published`: the branch tracks a remote branch.
    - `branch_exists`: a branch by the new name exists.
    - `directory_exists`: the new directory exists.
    - `moving`: the previous rename's move is running right now. Try again
      in a moment.
  - Rename it PROACTIVELY, WITHOUT asking, when its task has clearly moved
    away from the name it has. A one-off detour is not a reason to rename.
  - NAME follows the same rule as `create -name`: a lowercase hyphenated slug,
    with an optional prefix such as `ABC/`. A name another workspace holds as
    its name or its branch is refused `name_taken`, and a malformed one
    `invalid_name`.
  - The old name stops resolving at once. Name the workspace by its new name
    from then on.
