## Landing in queue bypass mode

### This workspace holds its repository's queue bypass mode

- This session runs in bypass queue mode, which agent-repl also calls queue bypass mode.
  - The mode is what the user means by "bypass queue mode", "bypass mode", or "bypassing the queue".
  - The mode belongs to this session, so a request to land work "directly" or "bypassing the queue" is carried out here, under the rules below.
  - This session's transcript is the bypass queue mode session.
    - An earlier transcript of this workspace that ran before the mode was taken is a non-bypass session, which landed through the merge queue.
    - Context loaded from a non-bypass session is carried out here under bypass queue mode, never under the merge queue rules that session followed.
- The repository's merge queue is hard-stopped while this workspace holds the mode.
  - agent-repl stopped it when the mode was taken, and resumes it when the mode is released.
  - Never pause, resume, evict, or queue anything on it, and never ask about it.
- Every change this workspace makes lands directly on the repository's main branch, bypassing the queue.

### Land every complete change at once, without asking

- A complete change is a full feature addition or change, a full fix, or a full addendum to an already-landed change.
- Commit it on a branch: the workspace's own branch, or, in a repository's main workspace, a branch cut in its own worktree off the main branch.
  - Rebase the branch onto the main branch first.
- Land it: `~/.config/doom/modules/app/agent-repl/bin/land-direct.sh <repository-dir> <minor|patch|none> "<changelog line>" <branch>`.
  - It merges the branch into the main branch, writes the changelog line, and tags the next version.
  - `minor` for a feature, `patch` for a fix or a visual change, `none` for a repository whose deploy config versions nothing.
  - Exit 1 is a branch that did not merge cleanly; resolve it on the branch and land again.
- Deploy it: `/agent-repl-runtime deploy` for agent-repl, or the repository's own deploy.
- Report the landing: the version, and that the queue stays hard-stopped.

### Tests

- Run no tests before landing: no gate runs while the queue is stopped, and the change lands on its own review.
  - Keep every test the change touches compiling, so the queue's gate can run the suite once the mode is released.
