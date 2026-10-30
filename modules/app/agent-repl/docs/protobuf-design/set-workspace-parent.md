# SetWorkspaceParent: retargeting a workspace's merge

A workspace's parent, and so its merge target, was fixed at creation. A create
whose `source_ws` is a registered workspace makes a CHILD, and a child's merge
lands in its parent's checkout rather than on the default branch. Nothing could
change that afterwards, and telling a workspace about a new target was a
separate manual step that an orchestrating agent forgot in practice:
`feed-tick-flicker-ncg` was created as a child of
`desktop-notification-turn-sounds`, but its fix must land on master.

## Core design principles

- **The retarget and its notice are one operation** (owner, 2026-10-11). On
  success the daemon itself queues a prompt into the retargeted workspace's
  session, through the same submission path the command file's `prompt` entry
  uses, so "retargeted but not told" cannot happen.
  - Consequence: the notice is a `conversation.v1.PromptOrigin` of its own
    (`PROMPT_ORIGIN_WORKSPACE_RETARGETED`), because every daemon submit site
    has its own origin value.
  - Consequence: a notice that cannot be queued undoes the retarget and the
    rpc fails.
  - Does NOT claim: that a queued or running merge is rewritten. A merge reads
    its target at admission.

## Landed changes

### 1. The rpc, its request arms and its refusals

- **What**: `agentrepl.v1.SetWorkspaceParent` in the SIDEBAR section, with
  `endpoint_set_workspace_parent.proto`. The request's `parent` oneof is
  `child_of` (a `WorkspaceRef` of the new parent) or `top_level`. Refusal arms:
  `unknown_workspace`, `workspace_ref_mismatch`, `transferring_away`,
  `not_yet_adopted`, `unknown_parent`, `parent_ref_mismatch`,
  `parent_in_other_repository` (retired, see 3), `cycle`, `no_merge_layout`.
- **Why**: the owner's agreed design (2026-10-11): set `wsm.Workspace.Parent`
  to another workspace of the same repository or clear it, with typed arms for
  unknown workspace, unknown parent, cross-repository parent, a cycle, and the
  host endpoints' ownership arms.
- **Process note**: the owner pre-agreed the rpc's shape and arm set in the
  commissioning prompt and asked that only genuine contract disputes be
  surfaced, so the interactive sketch rounds were not run.
- **Arms beyond the agreed list, and why**:
  - `parent_ref_mismatch` is the parent ref's twin of `workspace_ref_mismatch`;
    the parent arrives as a `WorkspaceRef`, so its echoed dir can disagree too.
  - `no_merge_layout`: the merge target is NOT `wsm.Workspace.Parent`; it is
    the creation job's `MergeLayout.TargetDir`
    (`daemon/internal/merge/subject.go` `resolveSubject`). A workspace the
    editor registered has no creation job, so there is nothing to retarget,
    and the merge orchestrator already refuses such a workspace's merge.
  - `standing_down` is NOT carried: on the sibling host endpoints it exists
    only where the daemon would start a session (SelectWorkspace). The notice
    is a queued prompt, which a session that is not up holds until bring-up.
  - `cold_gate` (tag 10): the notice goes through the prompt queue, which
    refuses any prompt while the session is parked at its cold gate
    (`daemon/internal/promptqueue/submit.go`, `ColdGateRefusal`). By the
    one-operation principle a notice that cannot be queued means no
    retarget, so the gate is answered by name rather than as a failure.
- **Unset parent arm is InvalidArgument, never "top-level"**: clearing a parent
  changes where work lands, so it is asked for by name.
- **Consequences**:
  - The daemon writes `workspaces.parent_id` and `creation_jobs.target_dir` in
    ONE transaction, so the roster's nesting and the merge target cannot
    disagree. The same transaction checks the parent's repository and the
    cycle, so a concurrent retarget cannot create a cycle between the check
    and the write.
  - Retargeting to the parent a workspace already has is not refused; it
    re-sends the notice.

### 2. `conversation.v1.PromptOrigin.PROMPT_ORIGIN_WORKSPACE_RETARGETED = 32`

- **What**: the origin the notice is submitted under, delivered deferred, and
  drawn as an ordinary prompt bubble.
- **Why**: principle above; one origin per daemon submit site.
- **Consequences**: `lisp/wire-common.el`'s
  `agent-repl-wire-shim-only-prompt-origins` lists it, since no client sends
  it.

### 3. A parent of another repository nests and owns, and `parent_in_other_repository` is retired

- **What**: `agentrepl.v1.SetWorkspaceParentError.parent_in_other_repository`
  (field 7) and its message `SetWorkspaceParentParentInOtherRepository` are
  retired (`reserved 7; reserved "parent_in_other_repository";`). A parent of
  another repository is recorded as the workspace's parent: the roster nests
  the workspace under it and it owns the workspace, but the merge target stays
  the workspace's own repository's main checkout. `frontend.v1.RosterOpenRow.
  repository_divider` names the repository a nested row begins.
- **Why**: owner ruling (2026-10-29): workspaces of different repositories
  may stand in a parent/child relation so a governing workspace keeps its
  cross-repository work nested under it, drawn under a repository divider like
  the deploy queue's. No branch crosses repositories, so only the landing
  stays per repository.

## Open question for the owner (not a contract dispute)

- Clearing `Parent` hands a workspace's roster nesting back to the branch
  lineage fallback (`daemon/internal/resolve/sidebar/nest.go`), which nests by
  `ParentBranch`. A workspace cut from a branch NAME another workspace holds
  would still nest under that workspace after a top-level retarget. Today's
  producers write the default branch (rpc creates) or a commit SHA (the skill's
  `base_commit`), neither of which derives a family, so the case does not arise
  for `feed-tick-flicker-ncg`. Making it impossible means recording "top-level"
  explicitly for daemon-created workspaces, which changes nesting for every
  daemon-created top-level workspace and is the owner's call.
