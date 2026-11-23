# Creating a workspace: the remaining flags

- `-originator emoji-ack|user-or-workspace` states who is creating the
  workspace, which decides the kind of workspace it is (`lifecycle-stages.md`). The default is `user-or-workspace`. `emoji-ack` is only for
  the Slack emoji-ack reaction system's dispatch (gns-sockets), requires
  `-slack-thread URL`, and is refused beside an async merge.
  - The daemon never infers the kind: `-slack-thread` alone does not make a
    workspace emoji-ack. It records the thread and stores it as a reference
    link labelled "Originating Slack thread".
- `-one-shot -prompt TEXT` makes a workspace that finishes its commission
  unattended and then runs `workspace finished here`, as the daemon's
  completion directive tells it.
  Without `-one-shot`, `-prompt` is the opening prompt and the workspace stays
  open for the user.
- **CRITICAL: Separate work ALWAYS starts in a created workspace.** Never cut a
  worktree or a branch by hand for it (`git worktree add`, `git checkout -b`,
  `git switch -c`): a hand-cut worktree is one no workspace owns and nothing
  cleans up.
- A create is SYNTHESIS, not investigation. Write what the new workspace must
  do and put the investigation into its prompt: run no tests, crawl no
  threads and read nothing large first. At most one shallow lookup to name
  the work.
- Create ONE workspace unless the user asked for more. A process-style ask
  ("one per failing test") is one workspace whose prompt does the enumerating
  and creates the rest.
- Omit `-name` and the daemon names the workspace from its prompt, in either
  form (standard or `-one-shot`). A name you
  give is a short lowercase hyphenated slug naming the SUBJECT (never the
  channel, the person or the thread it came from), with no prefix or suffix of
  your own.
- Give `-priority` or `-model` only when the user named one.
- `-hidden` creates the workspace BORN HIDDEN: its row is inserted hidden, so
  no sidebar draws it even for an instant. Use it for a workspace nobody is
  meant to open, such as an intake that works out a request and hands it on.
  `workspace set -hidden` after the create leaves a window in which the
  workspace is drawn, so a create that wants none uses `-hidden`.

- `-pre-deploy-prompt TEXT`, `-pre-deploy-script PATH:SECONDS`,
  `-post-deploy-prompt TEXT` and `-post-deploy-script PATH:SECONDS` (each
  repeatable, kept in order) REPLACE the repository's hooks of that stage for
  every deploy of the workspace; a stage given none runs the repository's.
  Give them only when the user asked for that workspace's deploys to differ.
- To hand uncommitted edits to the new workspace, snapshot them with
  `SHA=$(git stash create "<what>")` (which touches neither the working tree
  nor the shared stash stack) and open its prompt with "First, run
  `git stash apply <SHA>`". Always the SHA, never `stash@{N}`, and `apply`,
  never `pop`. Store or tag the SHA if the gap will be long, since `git gc`
  may prune it.
- Afterwards, tell the user each workspace's name and what it was asked to do.
- The workspaces a new one derives from (its parent, and the one it forks) are
  told when it is created, when it lands and when it closes without landing.
  Nothing needs to be relayed by hand.
- A named branch, such as a subagent's, is landed through an async merge,
  not a create: see `/agent-repl-merge-queue`.
