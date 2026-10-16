# Reopening a merged workspace to revert its merge

Clicking a row under the sidebar's Recently Merged section offers two
choices: reopen the workspace (to ask it about what it landed), or reopen it
and have it revert its merge. Either way the workspace comes back open and
selected, its worktree checked out again from its branch when the directory
is gone. The user follows the revert from the workspace's own feed.

## Landed changes

### `agentrepl.v1.OpenWorkspaceRequest.revert_merge`

- **What.** An optional, empty `OpenWorkspaceRevertMerge` on the open request.
  Present means "reopen this merged workspace and revert its merge"; absent
  is the ordinary open. Two refusal arms joined `OpenWorkspaceError`:
  `not_merged` (no landed merge to revert) and `brief_missing` (the
  merge-revert brief could not be composed). Both refuse before anything is
  opened.
- **Why.** The revert is an open plus one prompt. Expressing it as an option
  of the open, rather than a second rpc or a client-side open followed by a
  client-composed SubmitPrompt, keeps the brief's text and the landed commit
  it names on the daemon, which alone knows them, and makes the pair one
  operation the client cannot half-perform.
- **Why presence and not a bool.** The daemon knows every fact the revert
  needs (branch, parent branch, landed commit), so the ask carries nothing;
  the message is where a future revert option lands without a new field
  beside it.
- **Consequences.**
  - The daemon reads the commit the branch holds BEFORE the open, because the
    open fast-forwards a merged workspace's branch to its parent's tip
    (`daemon/internal/workspace/open.go` `advanceMerged`), after which the
    landed commit is no longer the tip.
  - A branch that no longer exists is refused `worktree_unrestorable`, the arm
    the open itself uses when there is nothing to restore from.
  - When the open succeeds and the queue then refuses the brief, the rpc
    fails loudly and the workspace stays open. The open is not undone: an
    open is idempotent and harmless, and the user can ask again.

### `conversation.v1.PromptOrigin.PROMPT_ORIGIN_MERGE_REVERT`

- **What.** The origin of the merge-revert brief. It is drawn in the feed as
  a prompt bubble authored "Merge revert", and it is the one origin the prompt
  queue HOLDS at a standing cold gate instead of refusing
  (`daemon/internal/promptqueue/submit.go` `waitsOutColdGate`).
- **Why.** A merged workspace is often reopened soon after it landed, with a
  large conversation, and the open's own bring-up is exactly where such a
  session parks at its cold gate. Refusing the brief there would silently drop
  the revert the user asked for. Held at the gate, it meets the fate the
  gate's answer gives every prompt held there: compact-and-resume sends it,
  pay or compact keep it for the user to send, clear discards it.
- **Alternatives rejected.**
  - Submitting the brief before the open: a submission to a session that is
    not up starts a background revival, which would race the open's own
    bring-up and skip the worktree restore.
  - Answering the gate on the user's behalf: a prompt never answers the gate
    (owner ruling, REALTEST-JUDGEMENT-CALLS call 11).
- **Consequences.** The elisp vocabulary lists the value among the origins no
  client sends (`lisp/wire-common.el`).
