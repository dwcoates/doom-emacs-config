# Principal link

The owner asked for a workspace subcommand that names a workspace's "principal
link": when one is set, the purple summary in the topbar is underlined and
clickable, and a click opens the link in the canonical browser for the login.
Agents set it proactively, without asking, whenever a link supplied in the
conversation best reflects the workspace's current summary or task, replacing
an earlier link when a better one appears. The link is persisted with the
workspace.

Every proto change here is pre-approved by the owner's standing ruling
(2026-10-13: decide contract changes and carry them through, report after), so
this record is written without per-increment selections.

## Landed changes

### `agentrepl.v1.SetWorkspacePrincipalLink` (new rpc)

- WHAT: a new endpoint file `endpoint_set_workspace_principal_link.proto` and
  its rpc in `service.proto`. The request names the workspace and a `change`
  oneof: `set { url }` or `clear {}`. The response is the standing
  success/error oneof; the error arms are the four every workspace-subject
  rpc shares (`unknown_workspace`, `workspace_ref_mismatch`,
  `transferring_away`, `not_yet_adopted`) plus `invalid_link { url, reason }`.
- WHY: the brief asked that the setter ride an existing workspace-attribute
  setter if one existed. None does: `agent-repld workspace set` calls one rpc
  per attribute (`SetWorkspaceHidden`, `SetWorkspacePriority`,
  `SetWorkspaceParent`, `SetWorkspaceOneShot`, `SetWorkspaceCorrespondence`),
  so a dedicated rpc is the existing pattern. The brief required that clearing
  be expressed explicitly rather than as an empty string, hence the two-arm
  oneof, shaped like `SetWorkspaceCorrespondence`'s `mark`.
- Alternatives rejected: an `optional string url` with absence meaning clear
  (`SetWorkspacePriority`'s spelling) makes "forgot the field" and "clear" the
  same message. A `requester` field like `SetWorkspaceCorrespondence`'s was
  left out: the link is a display fact anyone may correct (the user from a
  shell, the agent of the workspace), not a mark whose ownership matters.
- `invalid_link` is a typed refusal, not an error: the daemon runs
  `externalbrowser.Validate` (http/https, no whitespace) so every stored link
  is one `OpenExternal` can open. `reason` is the validator's sentence.
- Consequences: the daemon's `workspace.Verbs` gains `SetPrincipalLink`; the
  refusal maps through the reflective `answerRefusal` by arm name
  (`invalid_link`, fields `url` and `reason`).

### `frontend.v1.TopbarTitle.link` (new field) and `frontend.v1.TopbarTitleLink`

- WHAT: `optional TopbarTitleLink link = 2` on `TopbarTitle`;
  `TopbarTitleLink { string url = 1; }`.
- WHY: the link is drawn as the summary's clickable part, so it is a prop of
  the title element (figma→idl: one element message per drawn box, with its own
  wrapper). Riding the title rather than `TopbarView` makes "no summary,
  nothing clickable" structural: `TopbarView.title` is unset without a summary,
  so the link cannot be published.
- Consequences: the topbar resolver keeps the link in its per-workspace state
  and composes it into `title()`. The webapp draws the title through the one
  external-link component (`webapp/src/link.ts renderExternalLink` →
  `OpenExternal`), whose click handler already stops propagation, so the
  heading's session-line reveal does not also fire.

### `agentrepl.v1.WorkspaceStatus.principal_link` (new field)

- WHAT: `optional string principal_link = 10`.
- WHY: `agent-repld workspace status` must show the link, and an agent judges
  a new link against the standing one by reading it. A bare optional string
  matches the message's own spelling (`name`, `branch`); `WorkspaceStatus` is
  not a figma→idl view.

### Persistence (no proto)

- The link is a nullable `principal_link` column on `workspaces`, wsm layout
  71, additive (`ALTER TABLE ... ADD COLUMN`), like layout 64's
  `awaiting_correspondence`. It is read with the workspace record
  (`wsm.Workspace.PrincipalLink`), so `Status` and the topbar's seeding at
  registration need no second read. A close does not clear it: a reopened
  workspace still has the same task.
