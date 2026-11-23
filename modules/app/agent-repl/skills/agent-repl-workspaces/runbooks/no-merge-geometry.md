# A workspace with no merge geometry

- **NO MERGE GEOMETRY.** A workspace whose creation job records no merge
  geometry (the editor registered it, or a create was cut short before it
  finished) is refused `no_layout_facts` by `workspace finished` and
  `no_creation_job` by `-one-shot`. The daemon never guesses where it lands.
  - Restate where it lands with `workspace set WS -lands-into main` (or
    `-lands-into W`). That one statement records its creation job, with its
    own branch and worktree as the source.
  - After that, `-one-shot` marks it and `workspace finished` lands it.
  - A repository's main workspace never gets geometry, so it stays refused
    `no_merge_layout`.
