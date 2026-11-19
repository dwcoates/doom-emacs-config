# One set of workspace mention targets: completable equals linkable

The owner's rule (2026-11-19): if a workspace name is completable in the
composer, it is a clickable mention wherever it appears in a response bubble
or a prompt bubble, and a name a response wraps in inline code is drawn as
the canonical mention with its code markers stripped.

## Assumptions (autonomous design)

- Before this change the composer completed every roster row that was not
  closed (Emacs, `agent-repl-roster-open-workspaces`), while the page linked
  only the Open section's rows less the viewer and less shared names
  (daemon, `mentionTargets`). Two lists, built by two systems.
- The page's response linker searched only TLDR trees, so a prose response
  linked nothing. The format drift of one session (prose since its compaction
  on 2026-11-19) is what made this visible; no code change caused it.
- Selecting a hidden workspace is not refused by the daemon
  (`selectSubject` reveals only merge-hidden workspaces specially), so a
  hidden workspace can be a link target.

## Landed changes

### `frontend.v1.WorkspaceMentionTargets` is the composer's set too

- WHAT: the set's comment now states it is what the viewer's page links AND
  what the viewer's composer completes. Hidden workspaces are now targets
  (after the Open section's, in drawn order). A shared name names the one
  carrier in the viewer's own repository when exactly one is there, and
  otherwise no one. The viewer is never a target, and a name the viewer
  carries names no one else either.
- `frontend.v1.WorkspaceMentionTarget.repository` (field 5, optional
  `frontend.v1.WorkspaceMentionRepository`) carries the repository name the
  composer annotates a completion with. Fields 3 and 4 are left to the
  in-flight `closed-workspace-name-links` branch's `standing` oneof.
- WHY: one resolve for both readers makes "completable but not linkable"
  impossible rather than kept in step by hand.
- Consequences:
  - The composer no longer completes the viewer's own name, nor a name shared
    across repositories that the viewer's repository does not settle. A
    composer annotation always names one repository.
  - `agent-repl-roster-open-workspaces`, `agent-repl-roster-open-workspace-names`
    and `agent-repl-roster-row-repository-name` (`lisp/roster.el`) have no
    reader left and were removed with their tests.

### `agentrepl.v1.WatchHostWorkspaceResponse.mention_targets` (field 22)

- WHAT: the host stream carries the viewer's `WorkspaceMentionTargets` as
  state: the set in force first, then each change.
- WHY: Emacs's composer must read the daemon's set rather than derive its own
  from the roster.
- Consequences: `daemon.internal.server.serveHost()` subscribes the roster
  topic and filters unchanged sets (`mentionTargetsTracker`); the page's
  stream keeps its own frame filter (`changedOnlySink`), since it pumps one
  topic through `serveTopic`.

## Non-proto consequence recorded here for implementers

- The webapp's response prose is now a mention scope (`MENTION_TEXT_CLASS`),
  linked whole; a code block (`pre`) joins links, non-whole code spans, and
  tree rails and labels as never searched.
