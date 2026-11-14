## Landing in queue bypass mode

### This workspace holds its repository's queue bypass mode

- This session runs in bypass queue mode, which agent-repl also calls queue bypass mode.
  - The mode is what the user means by "bypass queue mode", "bypass mode", or "bypassing the queue".
  - The mode belongs to this session, so a request to land work "directly" or "bypassing the queue" is carried out here, under the rules below.
  - This session's transcript is the bypass queue mode session.
    - An earlier transcript of this workspace that ran before the mode was taken is a non-bypass session, which landed through the merge queue.
    - Context loaded from a non-bypass session is carried out here under bypass queue mode, never under the merge queue rules that session followed.
- The repository's merge queue runs between this workspace's turns while it holds the mode.
  - The user watches the queue work while the mode is on.
  - agent-repl hard-stopped it in this workspace's name when the mode was taken.
  - agent-repl lets it through at the end of each of this workspace's turns, and resumes it when the mode is released.
  - agent-repl lets through only a pause this workspace made, so a pause by the user or another workspace stands.
- Every change this workspace makes lands directly on the repository's main branch, bypassing the queue.

### Pause the queue before changing anything

- Before the first change of every prompt, pause the queue: `agent-repld merge-queue pause -repository here`.
  - A change is any file edit, commit, landing, or deploy.
  - Pause before the first change, never after it, so no queue landing moves the main branch under this work.
  - An `already_paused` refusal means the queue stands paused already, so go on and leave that pause as it stands.
- A prompt that only reads or answers leaves the queue running.
- Never resume the queue yourself, since agent-repl resumes it when the turn ends.
- Never evict, move, or queue anything on it, and never ask about it.

### Land every complete change at once, without asking

- A complete change is a full feature addition or change, a full fix, or a full addendum to an already-landed change.
- Commit it on a branch: the workspace's own branch, or, in a repository's main workspace, a branch cut in its own worktree off the main branch.
  - Rebase the branch onto the main branch first.
- Land it: `~/.config/doom/modules/app/agent-repl/bin/land-direct.sh <repository-dir> <minor|patch|none> "<changelog line>" <branch>`.
  - It merges the branch into the main branch, writes the changelog line, and tags the next version.
  - `minor` for a feature, `patch` for a fix or a visual change, `none` for a repository whose deploy config versions nothing.
  - Exit 1 is a branch that did not merge cleanly; resolve it on the branch and land again.
- Deploy it: `/agent-repl-runtime deploy` for agent-repl, or the repository's own deploy.
- Watch every deploy until it has settled, and fix what it broke before calling it done.
  - Wait for the new build to serve, then read every system's warnings and errors since the deploy began, `--doctor`, and the latencies against the versions before it.
  - Each defect the deploy shows is fixed on the branch, landed and deployed again the same way, and that deploy is watched in turn.
  - The work is done only once a deploy shows nothing to fix.
- Report the landing: the version, what the watch found and fixed, and that agent-repl lets the queue through when this turn ends.

### One agent does the work: no new workspace, no async merge, few subagents

- Bypass queue mode exists so this one agent fixes things quickly itself, without orchestration.
- This workspace never creates a one-shot, never marks one, never queues an async merge, and never creates a child workspace.
  - A one-shot and an async merge land through the merge queue, which this workspace pauses whenever it works, so their landings would contend with this session's own.
  - A child is the orchestration the mode exists to skip.
  - The daemon refuses each one with `requester_bypassing_queue`, and its `detail` repeats this reminder.
- Every rule that would dispatch one (a handed failure's fix, a todo row's fix, work a main workspace splits off) is carried out here, by this workspace itself.
- A child workspace that already existed when the mode was taken may still be prompted and answered.
  - It stops when its work is committed and tells this workspace its branch, never finishing or queueing anything.
  - This workspace lands that branch with `land-direct.sh`, as it lands its own.
- Avoid subagents: do the work in this conversation.
  - Dispatch one only where the work genuinely cannot be done here, and say why in the response.

### Tests

- Every test run is governed by default: the CPU governor holds it to its share of the host.
  - Run one ungoverned only when the user asks for it, by adding `-ungoverned -as-user`.
  - An ungoverned run takes every core, which the user feels on their machine.
- While writing a change, run only the tests it adds or changes, and keep every test it touches compiling.
- Before each landing, run the full suite on the branch, already rebased onto the main branch: `agent-repld test run -every-suite`.
  - Land only when it passes: a failure is fixed on the branch, and the full suite runs again.
  - If the main branch moved after the run, rebase onto it and run the full suite again before landing.
  - A change can break a test it never touches, which no targeted run sees (2026-11-14: 0.436.0 landed a broken test this way).
  - The full suite took 4m47s, measured 2026-11-14.
- Bypass queue mode never leaves the main branch broken.
  - Never call the session's work done, or ask for the mode to be released, while the main branch fails a tracked test.
