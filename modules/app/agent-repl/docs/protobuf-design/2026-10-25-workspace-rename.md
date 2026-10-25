# Workspace rename

The owner asked on 2026-10-25 whether the agent-repld harness supports
renaming a workspace, and if not, to add it. Before this change a workspace's
name was the slug it was created with, and nothing changed it afterward.

Every proto change here is pre-approved by the owner's standing ruling
(2026-10-13: decide contract changes and carry them through, report after), so
this record is written without per-increment selections.

## Core design principles

### A rename changes the name and nothing else

- THE PRINCIPLE (the brief's words): "The rename changes the workspace's NAME
  only. It does not rename its branch or move its worktree directory."
- CONSEQUENCES FOR THE CONTRACT: the request carries a name and nothing about
  branches or directories. After a rename, `WorkspaceStatus.name` and
  `WorkspaceStatus.branch` may differ, so every reader that needs the branch
  reads the branch field.
- WHY: moving the directory would break the live session's working directory
  and its vendor transcript path, and the branch is what merges and deploys
  name.
- WHAT IT DOES NOT CLAIM: it does not say name and branch were ever meant to
  differ elsewhere. Every place that relied on them agreeing is listed under
  the landed change below.

## Landed changes

### `agentrepl.v1.SetWorkspaceName` (new rpc)

- WHAT: a new endpoint file `endpoint_set_workspace_name.proto` and its rpc in
  `service.proto`. The request names the workspace and the new `name`. The
  success carries the stored name, with the workspace-name prefix applied.
  - The error arms are the four every workspace-subject rpc shares
    (`unknown_workspace`, `workspace_ref_mismatch`, `transferring_away`,
    `not_yet_adopted`).
  - `invalid_name { name, component, rule { slug | prefix } }` is the same
    shape as `CreateWorkspaceInvalidName`.
  - `name_taken { name, holder { workspace | pending } }` is the remaining
    arm.
- WHY ITS OWN RPC: `agentrepld workspace set` calls one rpc per setting
  (`SetWorkspaceHidden`, `SetWorkspacePriority`, `SetWorkspaceParent`,
  `SetWorkspaceOneShot`, `SetWorkspaceCorrespondence`,
  `SetWorkspacePrincipalLink`). The brief asked the rename to match that.
- WHY NO REQUESTER: like the principal link, the name is a display fact. The
  user from a shell, the sidebar, and the workspace's own agent may all
  correct it.
- WHY THE NAMING RULE IS THE SUPPLIED-CREATE RULE (`ValidateSuppliedName`):
  the user or an agent chooses the name, as with a supplied create name, so a
  name legal for `workspace create -name` is legal here. The same
  prefixing applies (`suppliedBranch`): a bare slug takes the configured
  workspace-name prefix.
- WHY NAMES AND BRANCHES ARE ONE NAMESPACE FOR THE TAKEN CHECK:
  - `takenLocked` (`daemon/internal/workspace/mergenames.go`) is the existing
    uniqueness rule minted names and async merge names follow. It is global
    across repositories and covers open and closed workspaces.
  - It read only registered NAMES, which was the same as reading their
    branches while every name equaled its branch.
  - After a rename it must also read each workspace's BRANCH. Otherwise a
    later create or async merge could mint a name equal to a renamed
    workspace's branch, and git would refuse it late.
  - The renamed workspace's own name and branch never count against it.
- WHY `holder` IS A ONEOF: a registered workspace has a ref the client can
  name, while a reservation or an in-flight claim has none.
- Consequences:
  - The daemon's `workspace.Verbs` gains `SetName`, which checks and records
    under the naming lock (`v.names.mu`). A create or async merge that
    claims a name and the rename that checks it therefore never race.
  - A rename republishes everything that drew the old name:
    - the roster, which also renames the Emacs tab in place through
      `agent-repl-roster--rename-tab`, keyed by the row's id;
    - the topbar's workspace-name line, through a new
      `topbar.Resolver.SetName`;
    - the host view, whose `HostWorkspaceNaming.slug` names Emacs buffers;
    - the footer's "awaiting `<name>`" line of every workspace that awaits the
      renamed one;
    - the merge queue's labels of the renamed workspace's repository.

## Places that read the name, checked against the principle

The audit read every `.Name` read of a `wsm.Workspace` in the daemon
(`grep -rn '\.Name\b'`, 33 sites) plus the CLI, Emacs, the webapp and
`bin/logs.sh`.

- DISPLAY ONLY, so they read the new name and need nothing:
  - the sidebar rows, `promptqueue/sentby.go`, `server/arrange.go`,
    `server/chat.go`;
  - `workspacenotice`, `desktopnotify`, `opentabs`, `runtimemetrics`;
  - the feed's peer-delivery names;
  - `bin/logs.sh`'s `workspace_name`, which reads the id-to-name table from
    `wsm.db`.
- `merge/terminal.go:212` (`DetachedOutcome.Branch = record.Name`):
  - The field is named Branch, but it carries the merge workspace's NAME, as
    its comment says, and every reader in `mergeworkspace` uses it only as a
    label.
  - A renamed merge workspace's hand-back names it by its new name. No branch
    operation reads the field.
- `workspace/sessions.go:841` (`StartSessionRequest.workspace_name`): the
  session's system prompt states the name at bring-up. A live session keeps
  the old name until its next bring-up. This is recorded in `org/todo.md` as
  a deficiency, because telling a live session about the change is a notice
  feature of its own, like the retarget notice.
- CLI resolution (`cliCall.named`) reads roster names, so the new name
  resolves at once and the old one stops resolving.
- Emacs keys tabs, buffers and host state by the roster name. It already
  treats a changed name under the same row id as a rename
  (`agent-repl-roster--rename-tab`), so nothing new is needed there.
- `workspace.verbs.claimSupplied` (`create -name`) checks a supplied name only
  against reservations, claims and git's own branch refusal. After a rename,
  a create can therefore take a name a renamed workspace holds. This place
  relied on name equal to branch, so the decision on its rule was reported
  to `agent-repl-skill-migration` rather than made here.
