# Renaming a workspace

- **RENAME.** `workspace set here -name NAME` renames this workspace: its
  name only. Its branch and its worktree directory keep the names they were
  created with.
  - Rename it PROACTIVELY, WITHOUT asking, when its task has clearly moved
    away from the name it has. A one-off detour is not a reason to rename.
  - NAME follows the same rule as `create -name`: a lowercase hyphenated slug,
    with an optional prefix such as `ABC/`. A name another workspace holds as
    its name or its branch is refused `name_taken`, and a malformed one
    `invalid_name`.
  - The old name stops resolving at once. Name the workspace by its new name
    from then on, and read its branch from `workspace status`, never from
    its name.
