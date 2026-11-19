# Emoji-ack workspaces wear a thumbs up before their name

The owner asked that a workspace the Slack emoji-ack intake creates have a
thumbs up emoji before its name wherever the name is drawn (the sidebar, the
`SPC p p` list, and so on), at the name's own font size rather than larger.

Design mode: autonomous.

## Assumptions

- The emoji-ack intake already states, at creation, that the workspace is an
  emoji-ack one: `explanation-engine`'s `gns-sockets` `dispatch-workspace.sh`
  creates the work workspace with `-originator emoji-ack`, and the daemon
  records it (`workspace_originators`). That flag is the creation-time
  specification the owner asked for, so the intake needs no new flag. Evidence
  tier: existing code (the script's `FORK_COMMAND`, `wsm.SetWorkspaceOriginator`).
- Systems: the daemon (roster and topbar resolvers), the webapp (sidebar row,
  topbar name line), and the editor lisp (the roster decoder, the `SPC p p` /
  `SPC p P` switcher).

## Landed changes

### `frontend.v1.RosterRowName.glyph` and `frontend.v1.TopbarWorkspaceName.glyph`

- WHAT: each name element gains an optional glyph message
  (`frontend.v1.RosterRowNameGlyph`, `frontend.v1.TopbarWorkspaceNameGlyph`)
  carrying the emoji drawn just before the name. The daemon sets it to 👍 for a
  workspace whose originator is emoji-ack, and leaves it unset otherwise.
- WHY the glyph is not part of the name: a workspace name is a slug that every
  verb resolves (`workspace prompt NAME`, renames, branch and worktree
  directory). The naming rule (`workspace.ValidateSuppliedName`) refuses an
  emoji, and a name with one in it could not be typed back. The glyph is a
  drawn mark, so it lives beside the name in the name's element.
- WHY the daemon resolves the glyph text instead of a client mapping an
  originator flag to an emoji: figma→idl, the server decides what is drawn, so
  the sidebar, the topbar and the editor never disagree on the glyph.
- WHY two messages: the sidebar row and the topbar draw the name in separate
  elements, so each wraps the fact in its own message.
- CONSEQUENCE: a client that draws the name elsewhere from `RosterRowName.text`
  (the switcher) draws the glyph from the same row; one that uses the name as
  an identity (rename prefill, completion) keeps using `text` alone.
- Sweep: nothing obviated.
