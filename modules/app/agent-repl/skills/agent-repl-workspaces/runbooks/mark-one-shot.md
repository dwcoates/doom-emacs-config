# Marking a workspace a one-shot

- **ONE-SHOT.** When to mark this workspace a one-shot is the metaprompt's
  rule ("One-shot workspaces"). The verb is `workspace set here -one-shot`,
  and its answer carries the daemon's completion directive: follow it when the
  work is done. A repository's main workspace cannot be marked (refused
  `main_workspace`); what a main
  workspace told to one-shot itself does instead is the metaprompt's rule
  ("A repository's main workspace is never a one-shot").
- **BACK FROM A ONE-SHOT.** A workspace that keeps the mark would finish
  itself again, unattended, when the user meant to keep working in it.
  - The daemon clears the mark itself when the user takes the workspace back:
    a prompt the user submits from the editor or the webapp clears it before
    the prompt is delivered, and so does reopening a closed one-shot.
  - A prompt from another workspace, or one the daemon submits, leaves it.
    When such a prompt asks a one-shot that finished for more work, or the
    workspace was restored to do more, run `workspace set here -not-one-shot`
    at once, without asking, before doing the new work.
