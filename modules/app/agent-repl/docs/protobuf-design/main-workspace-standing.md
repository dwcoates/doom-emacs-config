# Main-workspace standing

The workspace on a repository's main checkout (`main (<repository>)`) behaves differently from every created workspace: it sits on the main branch, lands nothing itself and splits separate work off into created workspaces. Two facts follow and become contract: its agent must be told so, at the very end of its system prompt, and it can never be marked a one-shot.

Design ran autonomously (owner pre-approved contract changes, 2026-10-30).

## Assumptions

- Objective: every agent knows whether its workspace is its repository's main workspace, and a main workspace can never become a one-shot.
- Frontend: none. Systems: daemon (decides the standing, refuses the mark), shim (composes the system prompt), harness text.
- Non-additive changes: none on the wire. `SessionProfileAgent` stops being empty, and the shim now refuses an agent profile without a standing.

## Iteration sequence

Two increments, each self-contained: (1) the `main_workspace` refusal arm on `agentrepl.v1.SetWorkspaceOneShotError`; (2) the standing on `shim.v1.SessionProfileAgent`.

## Landed changes

### 1. `agentrepl.v1.SetWorkspaceOneShotError.main_workspace`

- WHAT: a new arm, tag 8 (7 stays reserved), with the empty `SetWorkspaceOneShotMainWorkspace`. The `no_creation_job` comment no longer names the main checkout.
- WHY: a main workspace was refused only incidentally, as `no_creation_job`, because it happens to have no creation job. That answer says nothing about why, and would stop holding if a main workspace ever acquired a creation job.
- CONSEQUENCES: the daemon checks the standing before every other decision about a mark. The store refuses the mark at every write (`wsm.ErrMainWorkspaceOneShot`, in `SetOneShot` and `PutCreationJob`), so a create or internal setter that tried would fail hard. No `CreateWorkspaceError` arm was added: a create always makes a new linked worktree, so a create marking a main workspace is an invariant violation, not a refusal a caller can meet.

### 2. `shim.v1.SessionProfileAgent.standing`

- WHAT: `SessionProfileAgentStanding`, a oneof of `main_workspace` and `linked_workspace` (both empty), required on every agent profile.
- WHY: the shim composes the system prompt and cannot know the standing; the daemon owns it (`wsm.IsMainWorktree`). A typed fact rather than daemon-worded text keeps every system-prompt section composed in one place, the way the workspace name already is.
- CONSEQUENCES: the shim appends the standing notice as the LAST system-prompt section, with a comment at the assembly site that it must stay last, and a test pins that for both standings. The shim refuses an agent profile with no standing.
