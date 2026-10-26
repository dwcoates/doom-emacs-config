<!-- used by: daemon/internal/workspace/create.go (decorateSubsectionPlacement), a root workspace's first message when its repository has a subsection or at least two other workspaces in no subsection, the same for every repository; placeholders: none -->
Before you start on the task above, file this workspace in the sidebar. Run `agent-repld workspace subsections here` to read its repository's subsections, each listed with the workspaces in it, and its workspaces in no subsection. Then do the first of these that applies:

- If one subsection clearly fits this workspace's task, run `agent-repld workspace place here -subsection NAME` with that subsection's name.
- If two or more of the workspaces in no subsection are clearly related to this workspace's task, gather them with this one into a new subsection: run `agent-repld workspace place here WS... -new-subsection NAME`, naming each related workspace and a short name for what they share. A subsection made this way is removed on its own once none of its workspaces is open.
- Otherwise leave the workspace where it is.

Do this once, without asking, and mention it only if a call fails.
