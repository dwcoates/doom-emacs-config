# Create progress: the session bring-up stage

A background workspace create reports its progress to Emacs's minibuffer as a
sequence of stages on the daemon's `mutationProgress` push: accepted, deriving
the name, creating the worktree, then created or failed. After the worktree
exists the daemon still registers the workspace, brings its session up (forking
the parent's transcript when asked) and submits the initial prompt — and none of
that is reported, so the minibuffer sits on "creating the workspace's git
worktree…" through the slowest part of the create. The owner ruled
(2026-09-27) that this part of the sequence gets its own stage, so the whole
create reaches Emacs.

## Iteration sequence

Two steps, owner-accepted 2026-09-27: (1) the stage's purpose and scope in
prose, (2) the shape.

## Settled decisions

- **One new stage, "starting the session".** It covers everything after the
  worktree exists up to the terminal outcome: registering the workspace,
  bringing the session up (forking the parent's transcript first when asked)
  and submitting the initial prompt. Every create that passes the worktree
  step emits it, with or without an initial prompt. A separate
  "delivering the initial prompt" stage was rejected: the submission is a
  quick queue handoff, and its failure already arrives as the create's
  `failed` terminal.
- **The stage becomes a oneof of dedicated messages, not a third enum value.**
  `WorkspaceCreateStage` was a state enum, which the proto conventions forbid;
  the owner chose to convert it while adding the stage. Consequence accepted:
  both existing consumers change — the daemon's stage emitter
  (`daemon/internal/server/workspaces.go:237-239`) and Emacs's decoder
  (`lisp/wire-host.el:1149-1150`, rendered by `lisp/mutation-progress.el:49-50`).

## Landed changes

- **2026-09-27, branch `proto/create-bring-up-stage`.** `WorkspaceCreateStage`
  is now a message holding a oneof of three empty arm messages —
  `deriving_name`, `creating_worktree`, `starting_session` — and
  `WorkspaceCreateProgress` carries it on a NEW field, `entered_stage = 4`,
  with field 1 and the name `stage` reserved. Why: the owner wants the whole
  create to reach Emacs, and chose the oneof over a third enum value.
  Consequences:
  - Emacs decodes this message from JSON by field NAME, so reusing `stage`
    for a message-typed field would have broken an Emacs still running the
    old decoder during a deploy; the new name makes an old Emacs skip the
    stage (forward skew) until the deploy's `reload_elisp` lands.
  - The daemon emitter (`daemon/internal/server/workspaces.go:237-239`) and
    the Emacs decoder (`lisp/wire-host.el:1149-1150`) must both switch to the
    arm shape; Emacs's line table (`lisp/mutation-progress.el:49-50`) gains
    a `starting_session` line.
  - The daemon must report the new stage after the worktree step
    (`daemon/internal/workspace/create.go` steps 5–7), for every create that
    reaches it, with or without an initial prompt.
  - The `failed` terminal's comment now names the initial prompt's
    submission among the post-ack failures.
  - Sweep: `WorkspaceOpenStage` in the same file is also a state enum (it has
    its own `STARTING_SESSION`). Out of scope for this change; flagged to the
    owner, not converted.

- **Reconciled and merged 2026-09-27** (merge on master after branch `proto/create-bring-up-stage`): the daemon reports `starting_session` right after the worktree for every create form, and Emacs decodes `enteredStage` and shows "starting the workspace's session…". Stage sequence per form is pinned by `TestCreateReportsItsStageSequencePerForm`.
