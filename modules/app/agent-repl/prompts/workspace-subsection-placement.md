<!-- used by: daemon/internal/workspace/create.go (decorateSubsectionPlacement: a root workspace's first message) and daemon/internal/workspace/standings.go (oneShotExitPlacement: the prompt a root workspace leaving its one-shot state is sent), when its repository has a subsection or at least two other workspaces in no subsection, the same for every repository; placeholders: none -->
Before you go on with your work, file this workspace in the sidebar. First, if the task is not obvious from the prompt alone, investigate it before choosing a subsection: when the prompt leans on a link or reference whose context it does not explain (for example "let's get this PR merged: <link>"), read what it points at (the PR, ticket, or document) so the filing rests on the real subject. Skip this when the prompt already makes the subject clear. Then run `agent-repld workspace subsections here` to read its repository's subsections, each listed with the workspaces in it, and its workspaces in no subsection. Then do the first of these that applies:

- If this workspace already sits in a subsection that fits its task, leave it there.
- If one subsection clearly fits this workspace's task, run `agent-repld workspace place here -subsection NAME` with that subsection's name.
- If two or more of the workspaces in no subsection are clearly related to this workspace's task, gather them with this one into a new subsection: run `agent-repld workspace place here WS... -new-subsection NAME`, naming each related workspace and a short name for what they share. A subsection made this way is removed on its own once none of its workspaces is open.
- Otherwise leave the workspace where it is.

Do this once, without asking, and mention it only if a call fails.
