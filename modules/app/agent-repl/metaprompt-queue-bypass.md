## Landing in queue bypass mode

### This workspace holds its repository's queue bypass mode

- This session runs in bypass queue mode, which agent-repl also calls queue bypass mode.
  - The mode is what the user means by "bypass queue mode", "bypass mode", or "bypassing the queue".
  - The mode belongs to this session, so a request to land work "directly" or "bypassing the queue" is carried out here, under the rules below.
  - This session's transcript is the bypass queue mode session.
    - An earlier transcript of this workspace that ran before the mode was taken is a non-bypass session, which landed through the merge queue.
    - Context loaded from a non-bypass session is carried out here under bypass queue mode, never under the merge queue rules that session followed.
- The repository's merge queue keeps running while this workspace holds the mode.
  - It is paused only while this workspace lands a change (below), and for no longer.
  - agent-repl lets a pause this workspace made to land through at the end of each of its turns, in case one was left standing.
  - agent-repl lets through only a pause this workspace made with `-to-land`, so a pause the user asked for, or one by the host or another workspace, stands.
- Every change this workspace makes lands directly on the repository's main branch, bypassing the queue.

### Pause the queue only to land

- Work on a branch with the queue running: writing, committing and targeted tests never pause it.
- Pause the queue only once a change is complete and about to land: `agent-repld merge-queue pause -repository here -to-land`.
- A pause the user asks for (to save power, to look at the queue) is made without `-to-land`, so it outlives this turn: `agent-repld merge-queue pause -repository here`.
  - Resume it only when the user says so.
  - An `already_paused` refusal means someone else's pause stands: go on, and leave that pause as it stands.
- Resume the queue the moment the landing is done: `agent-repld merge-queue resume -repository here`.
  - Resume only a pause this workspace made.
  - A landing that fails its tests resumes the queue too, while the failure is fixed on the branch.
- Never evict, move, or queue anything on it, and never ask about it.

### Land every complete change at once, without asking

- A complete change is a full feature addition or change, a full fix, or a full addendum to an already-landed change.
- Commit it on a branch: the workspace's own branch, or, in a repository's main workspace, a branch cut in its own worktree off the main branch.
- Land it in this order:
  1. Pause the queue (above).
  2. If the main branch moved since the work began, rebase onto it and resolve every conflict.
  3. Run the full suite (Tests, below).
  4. Land it with `land-direct.sh` (below).
  5. Resume the queue.
- Land it: `~/.config/doom/modules/app/agent-repl/bin/land-direct.sh <repository-dir> <minor|patch|none> "<changelog line>" <branch>`.
  - It merges the branch into the main branch, writes the changelog line, and tags the next version.
  - `minor` for a feature, `patch` for a fix or a visual change, `none` for a repository whose deploy config versions nothing.
  - Exit 1 is a branch that did not merge cleanly; resolve it on the branch and land again.
- Deploy it: `/agent-repl-runtime deploy` for agent-repl, or the repository's own deploy.
- Watch every deploy until it has settled, and fix what it broke before calling it done.
  - Wait for the new build to serve, then read every system's warnings and errors since the deploy began, `--doctor`, and the latencies against the versions before it.
  - Each defect the deploy shows is fixed on the branch, landed and deployed again the same way, and that deploy is watched in turn.
  - The work is done only once a deploy shows nothing to fix.
- Report the landing: the version, and what the watch found and fixed.

### This workspace dispatches like any other, and alone manages the queue

- This workspace creates child workspaces, one-shots and async merges as any workspace does, and they land through the merge queue, which keeps running.
  - Every rule that dispatches work (a handed failure's fix, a noticed deficiency's fix) dispatches it from here, as a child of this workspace.
- This workspace never marks itself a one-shot: it lands its own work with `land-direct.sh`, never through the queue.
  - The daemon refuses the mark with `requester_bypassing_queue`.
- Only this workspace pauses or resumes the merge queue (owner ruling, 2026-11-14).
  - The workspaces it creates and the subagents it runs never pause, resume, evict or move anything on the queue, and their briefs say so.
  - One that needs the queue paused to land directly asks this workspace, which pauses it and resumes it when told the landing is done.

### Tests

- Every test run is governed by default: the CPU governor holds it to its share of the host.
  - Run one ungoverned only when the user asks for it, by adding `-ungoverned -as-user`.
  - An ungoverned run takes every core, which the user feels on their machine.
- While writing a change, run only the tests it adds or changes, and keep every test it touches compiling.
- Before each landing, with the queue paused, run the full suite on the branch rebased onto the main branch: `agent-repld test run -every-suite`.
  - Land only when it passes: a failure is fixed on the branch, with the queue resumed meanwhile, and the landing starts over.
  - A pass is the run's ended state AND its `test-all.log` free of any failure, the placement check's included.
  - A change can break a test it never touches, which no targeted run sees (2026-11-14: 0.436.0 landed a broken test this way).
  - The full suite took 4m47s, measured 2026-11-14.
- Bypass queue mode never leaves the main branch broken.
  - Never call the session's work done, or ask for the mode to be released, while the main branch fails a tracked test.
