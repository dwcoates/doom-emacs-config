# A workspace's name wears its priority icon as a prefix

The owner asked that `SPC j p` set a workspace's priority (it was `SPC j m p`),
and that the priority icons (`images/p05.png` … `images/p3.png`) show as a
prefix of the workspace's name everywhere the name is drawn: the `SPC p p`
switcher, the sidebar, and so on. The emoji-ack glyph (branch
`emoji-ack-sidebar-prefix`) is also a prefix of the name; the two are not
mutually exclusive, the ack glyph draws to the right of the priority icon, and
this shape supersedes the ack branch's.

Design mode: autonomous.

## Landed changes

### `frontend.v1.WorkspaceNamePrefix`

- WHAT: one message (`frontend/v1/workspace_name_prefix.proto`) carrying every
  mark drawn before a name, in drawn order. Today one field,
  `frontend.v1.WorkspaceNamePrefix.priority` (`frontend.v1.WorkspaceNamePriority`,
  a oneof whose arm is the level, mirroring `agentrepl.v1.WorkspacePriority`).
  Field 2 is held for the emoji-ack glyph.
- `frontend.v1.RosterRowName.prefix` and `frontend.v1.TopbarWorkspaceName.prefix`
  (field 2 of each) carry it. UNSET when the name wears no mark, never set empty.
- WHY one shared message rather than a field per mark per surface: the drawn
  order (priority, then ack) is then stated once, in field order, and each
  client draws it through ONE helper. A per-surface glyph message (the ack
  branch's `frontend.v1.RosterRowNameGlyph` / `frontend.v1.TopbarWorkspaceNameGlyph`)
  makes every new mark a change to every surface.
- WHY the arm and not a label or a URL: the pictures are module assets the
  webapp bundles and Emacs loads by the same file names (`images/<arm>.png`),
  as the status glyphs are (`images/glyphs/<name>.svg`).

### `frontend.v1.RosterRow.priority` retired

- The text badge (`frontend.v1.RosterRowPriorityBadge`, drawn after the sidebar
  name and before the Emacs tab's `[N]`) is replaced by the icon in the name's
  prefix. Field 33 and the name `priority` are reserved.

## The emoji-ack glyph joined the prefix (2026-11-19)

The emoji-ack work landed on master first, with its own
`frontend.v1.RosterRowNameGlyph` and `frontend.v1.TopbarWorkspaceNameGlyph` at
field 2 of each name element. Merging master folded it into this shape:

- Its glyph is `frontend.v1.WorkspaceNamePrefix.emoji_ack`
  (`frontend.v1.WorkspaceNameEmojiAck`), drawn to the right of the priority
  icon. The two glyph messages are gone.
- Its daemon decision (`internal/resolve/nameglyph`) is now part of
  `agent-repld.internal.resolve.nameprefix.Of()`.
- Its drawings (the webapp's `name-glyph.ts`, the editor's
  `agent-repl-name-glyph-string`) are the emoji-ack branch of
  `src/name-prefix.ts` and of `agent-repl-name-prefix-string` in
  `lisp/status.el`. Its sizing (1em in the webapp, the font-ratio scale of
  `agent-repl-name-glyph-height` in Emacs) is unchanged, and the Emacs tab
  bar now draws the glyph too.

## Implementation consequences

- Daemon: `internal/resolve/nameprefix` is the one decision; the sidebar row
  and the topbar's name call it. The topbar learns a priority change through
  `topbar.Resolver.SetNamePrefix`, called by the priority verb.
- Webapp: `src/name-prefix.ts` draws the prefix; the sidebar row, its stand-in
  and the topbar name use it.
- Emacs: `agent-repl-name-prefix-string` draws the prefix; the tab bar's badge
  run and the `SPC p p` / `SPC p P` switcher use it.
- Not covered: the composer's workspace-name completion and the merge queue's
  rows draw names from other messages.
