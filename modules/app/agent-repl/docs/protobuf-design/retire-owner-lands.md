# Retire owner-lands: every workspace may land itself

Owner request, 2026-11-09: remove the owner-lands restriction from agent-repl
entirely, so a child workspace may always land itself. Before this change a
child whose context was not forked from its owner, and that was not a
one-shot, was refused finishing itself (`owner_lands`); only its parent or the
user could land it. Observed from `sidebar-extra-repo-blocks`, a fresh-context
non-one-shot child of `main (doom)`, whose `workspace finished here` failed.

Design mode: autonomous (the owner pre-approved the proto changes).

## Assumptions

- Objective: a workspace may always land itself; its owner (its parent), the
  workspace it lands into, and the user still may too
  (`wsm.Workspace.MayAct`). Any other requester is still refused
  (`requester_may_not_land`).
- Systems involved: daemon (wsm, merge, workspace, workspacenotice,
  workspacesubs, server, CLI), editor lisp (wire decoders), webapp (sidebar
  refusal text), shim (metaprompt), skills and AGENTS.md.
- Non-additive changes only: everything here is a removal.

## Iteration sequence

One stage: retire the owner-lands contract and everything that exists only
because of it, file by file.

## Landed changes

### 1. `agentrepl.v1.WorkspaceStatusOwner.owner_lands` retired (tag 2 reserved)

- WHAT: the bool is gone; `WorkspaceStatusOwner` names only the owner.
- WHY: no workspace's landing is reserved to its owner any more, so the fact
  has no true value.
- CONSEQUENCES: `daemon/internal/workspace/status.go` stops setting it;
  `daemon/internal/workspacesubs/workspacesubs.go` stops writing "and is landed
  by its owner" into a subscriber's status line.

### 2. `agentrepl.v1.FinishWorkspaceError.owner_lands` and `FinishWorkspaceOwnerLands` retired (tag 12 reserved)

- WHAT: the refusal arm and its message are gone.
- WHY: the refusal it carried no longer exists.
- CONSEQUENCES: `merge.ArmOwnerLands` and its `checkAsker` case go; the CLI
  refusal text (`daemon/cmd/agent-repld/workspacerefusal.go`), the lisp
  decoder (`lisp/wire-verbs.el`) and the webapp's sidebar refusal text
  (`webapp/src/sidebar/verbs.ts`) drop the arm.

### 3. The owner notice retired

- WHAT: `CreateWorkspaceStandard.owner_notice` (tag 9 reserved),
  `WorkspaceOwnerNotice`, and `CreateWorkspaceError.owner_notice_required` /
  `owner_notice_unexpected` (tags 22, 23 reserved, with their messages).
- WHY: the owner notice existed ONLY for a child its owner lands: it was
  required exactly when the owner landed the child and refused otherwise, and
  its delivered brief told the owner that landing the child was its
  responsibility. With owner-lands gone, the notice is never owed and its
  brief would state something false.
- WHY THE ALTERNATIVE LOST: keeping the notice as an optional "heads-up to the
  parent" was rejected. The parent is already told about a derived child's
  creation by the derived-workspace notice when it did not ask for the create
  itself, and when it did ask it already knows; an optional free-text second
  channel would duplicate that notice with no rule for when to use it.
- CONSEQUENCES: the CLI `-owner-notice` flag, `CreateSpec.OwnerNotice`, the
  pending-create journal's `owner_notice` field (an old journal entry with it
  still decodes; the field is ignored), `workspacenotice.BriefOwner`, the
  `prompts/workspace-owner-notice.md` brief, and the lisp encoder/decoders
  all go.

### 4. `conversation.v1.PromptOrigin.PROMPT_ORIGIN_OWNER_NOTICE` kept, no longer sent

- WHAT: the enum value stays, its comment now says no producer sends it.
- WHY: conversations recorded before this change hold prompts with this
  origin, and the feed still draws them (`daemon/internal/resolve/feed/prompt.go`
  labels them "Owner notice"). Reserving the value would make those recorded
  prompts decode as an unknown origin.

### 5. Comment-only: `endpoint_set_workspace_one_shot.proto` and `endpoint_finish_workspace.proto`

- The one-shot mark no longer decides who lands a workspace, only whether it
  finishes unattended; the finish endpoint's WHO MAY ASK paragraph says every
  workspace may finish itself.

## Implementation consequences

- A create no longer reopens a CLOSED owner (`daemon/internal/workspace/create.go`,
  the former owner-lands block of its derivation checks). The reopen existed so
  the owner could receive the owner notice and land the child; with neither
  owed, a closed owner stays closed, exactly as it already did for a child
  forked from its owner.
- `promptqueue.waitsOutColdGate` drops `PROMPT_ORIGIN_OWNER_NOTICE`: no such
  prompt is submitted any more.
- An owner told of a fresh child's creation now gets the brief
  derived-workspace notice (`prompts/derived-workspace-notice.md`), as every
  other source does, unless it asked for the create itself.
- A `workspace subscribe` status line no longer says "and is landed by its
  owner" (`daemon/internal/workspacesubs`).

## Storage

The `workspaces.owner_lands` column (`daemon/internal/wsm/derivation.go`) is
left in place and INERT: nothing reads it after this change, and new rows
write its default. No migration drops it, because the daemon's layout
migrations are additive and a previous build still reads the table during a
rolling restart.

## Obviated-declaration sweep

Package-wide: `FinishWorkspaceOwnerLands`, `WorkspaceOwnerNotice`,
`CreateWorkspaceOwnerNoticeRequired` and `CreateWorkspaceOwnerNoticeUnexpected`
had no other referent and are deleted. Nothing else was obviated.
