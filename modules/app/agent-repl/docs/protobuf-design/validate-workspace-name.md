# ValidateWorkspaceName

A user typed a paragraph into the static create's `Name: ` question (2026-10-27). Emacs refused only a blank name, so the paragraph went to the daemon, which refused it as `agentrepl.v1.CreateWorkspaceError.invalid_name` after the minibuffer had closed, and the text was lost. The owner wants the name judged while the minibuffer is still open, with the slug rule kept in one place: the daemon.

The owner pre-approved the contract change and asked the agent to decide and carry it through (task brief, 2026-10-27), so the decisions below were made by the agent and are reported after landing.

## Landed changes

### `agentrepl.v1.ValidateWorkspaceName`, a new unary read in the SIDEBAR section

- **What**: a new rpc and its endpoint file `endpoint_validate_workspace_name.proto`. The request carries the supplied name. The success arm's `verdict` oneof is either `ValidateWorkspaceNameLegal` (empty) or `agentrepl.v1.CreateWorkspaceInvalidName`, imported. The error arm is empty, following `agentrepl.v1.DaemonHealthError`.
- **Why**: the editor must not hold a copy of the naming rule (the owner's requirement), so it needs to ask the daemon before the minibuffer closes. No existing rpc answers "is this name legal" without creating or renaming something.
- **Why the breach is imported rather than respelled**: the verdict is exactly "would CreateWorkspace refuse this as invalid_name", so the encompassing message is `agentrepl.v1.CreateWorkspaceInvalidName`. Importing it lets Emacs word the pre-check and the create's refusal with the one sentence it already has (`agent-repl.verbs.agent-repl-verbs--create-invalid-name-sentence()`). `agentrepl.v1.SetWorkspaceNameInvalidName` is an existing respelling of the same breach and was left alone, because touching it is outside this change.
- **Why a legal name is a success, and so is an illegal one**: an illegal name is a legitimate answer to the question asked, so it is a success arm, not an error.
- **What it does not answer**: whether the name is free. A name-taken answer given before the create would be stale by the time the create runs under its naming lock, so the create stays the authority for that.
- **Consequences**:
  - The daemon's server answers it from `agent-repld.internal.workspace.ValidateSuppliedName()`, the check `refuseSuppliedName()` uses, so the create, the rename and this read share one rule.
  - Emacs gains a sync call on its minibuffer exit path. A transport failure or an error arm there is surfaced loudly and keeps the minibuffer open, never read as a legal name.
