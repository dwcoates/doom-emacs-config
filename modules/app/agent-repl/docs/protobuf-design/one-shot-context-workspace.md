# One-shot context workspace

The owner wants `SPC TAB C-o` to create a one-shot that fixes something wrong
with the workspace the command was run from. The commission the user types is
about that workspace (for example a bug in its feed), so the daemon's opening
note must name that workspace as the one the cited issue corresponds to.

## Landed changes

### `agentrepl.v1.CreateWorkspaceOneShot.context` and the `unknown_context_workspace` refusal

- **What:** `agentrepl.v1.CreateWorkspaceOneShot` gains
  `optional CreateWorkspaceOneShotContext context = 4`, a wrapper holding the
  context workspace's `workspace.v1.WorkspaceRef`.
  `agentrepl.v1.CreateWorkspaceError` gains
  `unknown_context_workspace = 19` (`CreateWorkspaceUnknownContextWorkspace`,
  naming the ref), mirroring `unknown_fork_source`.
- **Why:** the owner asked that the preamble the daemon gives the generated
  workspace specify the context workspace. The daemon composes every one-shot
  decoration, so the daemon must be told the workspace, not handed a
  pre-composed note from Emacs.
- **Why on the one-shot arm and not on the request:** only a one-shot is asked
  for this way, and placing it inside the arm makes a context on a standard
  create unrepresentable. A later standard-create use would move it, which is a
  contract change of its own.
- **Why not `parent`:** a parent makes the workspace a child that merges into
  the parent's branch. The fix belongs on the primary branch like every other
  one-shot; the context workspace is only what the commission is about.
- **Consequences (accepted):** the note is a daemon brief
  (`prompts/workspace-oneshot-context-preamble.md`), loaded from the daemon's
  own corpus like the fork-into-primary note, because it is the daemon's
  statement of fact about agent-repl workspaces rather than repository policy.
  Every client that decodes `CreateWorkspaceError` (`lisp/wire-verbs.el`,
  `webapp/src/sidebar/create.ts`) learns the new arm.

### The context workspace's selected bubble (comment-only change, 2026-10-30)

- **What:** when the context workspace's feed has a bubble selected as the
  create request arrives, the daemon quotes it after the context note: its
  kind, its `frontend.v1.FeedId` value and its text, through the brief
  `prompts/workspace-oneshot-context-selection.md` and `replyquote.Fenced`.
  `agentrepl.v1.CreateWorkspaceOneShot.context`'s comment says so. No field
  is added.
- **Why no field:** the daemon is the only holder of a workspace's feed
  selection (`SelectFeedRow`), so a field would only let a client send a copy
  that could disagree with it. Emacs holds the kind and the text alone, never
  the row.
- **When it is read:** once, as the request arrives, before the create
  detaches. A held create's durable row stores the selection with the rest of
  its spec, so a create resumed after a restart quotes what was selected when
  it was asked for.
