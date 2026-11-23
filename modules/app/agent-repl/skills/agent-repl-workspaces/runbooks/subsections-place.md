# Filing a workspace into a subsection

- `workspace subsections WS` prints the sidebar arrangement of WS's
  repository: its subsections by name, each with the workspaces in it, and
  its workspaces in no subsection, in drawn order.
- `workspace place WS... -subsection NAME` files the named workspaces'
  families at the bottom of their repository's subsection named NAME. An
  unknown or ambiguous NAME is refused, listing the subsections there are.
- `workspace place WS... -new-subsection NAME` creates a subsection named
  NAME as this workspace's agent, with the named workspaces in it. A
  subsection an agent creates is removed on its own once none of its
  workspaces is open; one the user creates stays even when empty, unless
  the user's own close, kill or nuke of its last open workspace emptied it.
- A repository's main workspace is never filed into a subsection by an agent,
  nor is any workspace whose family has it for a root: the daemon refuses
  `place` with `main_workspace_not_sortable`. Only the user arranges the main
  workspace, by drag or the sidebar's menus. Leave it out of every `place`.
- A new top-level workspace whose repository has a subsection, or at least
  two other open workspaces in no subsection, is told so at the end of its
  first message, a one-shot included: a one-shot is filed like any other
  workspace, and marking or unmarking it never moves it. File it once,
  without asking: into the one subsection that clearly fits its task, or
  else with two or more clearly related unsectioned workspaces into a new
  subsection, or else leave it where it is.
