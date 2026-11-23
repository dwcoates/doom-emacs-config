# Compacting and setting a model

- `workspace compact WS [-instructions TEXT]` compacts WS's conversation.
  **CRITICAL: Compacting another workspace is NEVER autonomous.** Ask the user
  for permission first, every time, naming the workspace, and run it only
  after the user approves that specific compaction. Clearing a workspace's
  context is likewise never done on an agent's own judgment, and is asked for
  the same way.
- `workspace model WS -model M [-as-user]` sets WS's model, and is the ONE way
  an agent sets another workspace's model: a `/model` prompt is refused, like
  every slash command an agent sends (`workspace prompt WS "/model sonnet"`
  fails even with `-as-user`).
  - It checks M against the catalog the daemon offers WS's repository
    (`ListCreationModels`) first: an unknown model is refused
    `model_not_in_catalog` with the catalog's names, and nothing is compacted.
  - It then compacts WS exactly as `workspace compact` does, waits for that
    compaction to finish (after WS's running turn ends, if one runs), and only
    then sets the model, so the command may block for as long as the
    compaction takes. A compaction that fails is `compaction_failed` and
    leaves the model unchanged.
  - **CRITICAL: Because it compacts, setting another workspace's model is
    NEVER autonomous.** Ask the user for permission first, every time, naming
    the workspace and the model, exactly as for `workspace compact`.
- A slash command an agent sends as a prompt is refused with "slash commands
  are not supported from agents", naming `workspace compact` for `/compact`
  and `workspace model` for `/model`.
