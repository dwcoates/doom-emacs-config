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
  1. Wait until no deploy check is at work (Defer to the deploy mediator, below).
  2. Pause the queue (above).
  3. If the main branch moved since the work began, rebase onto it and resolve every conflict.
  4. Land it with `land-direct.sh` (below).
  5. Resume the queue.
- Land it: `~/.config/doom/modules/app/agent-repl/bin/land-direct.sh <repository-dir> <minor|patch|none> "<changelog line>" <branch>`.
  - It lands one branch, and only through its gate: the branch checked out and committed in its worktree, rebased onto the main branch, and the full suite passing on it (`agent-repld test run -every-suite`, run by the script itself).
  - It then merges the branch into the main branch, writes the changelog line, and tags the next version.
  - `minor` for a feature, `patch` for a fix or a visual change, `none` for a repository whose lifecycle config versions nothing.
  - Exit 4 is the gate's refusal, which it names: fix it on the branch, with the queue resumed meanwhile, and land again.
  - Exit 1 is a branch that did not merge cleanly; resolve it on the branch and land again.
- Deploy it: `/agent-repl-runtime deploy` for agent-repl, or the repository's own deploy.
- Watch every deploy until it has settled, and fix what it broke before calling it done.
  - Wait for the new build to serve, then read every system's warnings and errors since the deploy began, `--doctor`, and the latencies against the versions before it.
  - Each defect the deploy shows is fixed on the branch, landed and deployed again the same way, and that deploy is watched in turn.
  - The work is done only once a deploy shows nothing to fix.
- Report the landing: the version, and what the watch found and fixed.

### Defer to the deploy mediator, and watch it as it runs

- The repository's deploy mediator checks every deploy, and this workspace defers to it in procedure (owner ruling, 2026-11-17).
  - A deploy check is at work while a deploy rolls out, the mediator assesses or fixes it, or the mediator's fix finishes.
  - `agent-repld merge-queue status -repository here` shows it: its `advance` stands `mediating`, or the mediator's finish is landing.
- While a deploy check is at work, this workspace never contends with it.
  - It never lands, deploys, bounces or restarts anything, pauses the queue, or runs a full suite.
  - It never prompts, closes, compacts or otherwise acts on the mediator's workspace.
  - A direct landing's deploy replaces the check the mediator is deciding, and its handover moves the mediator's session mid-turn; a full suite takes the host's cores from the mediator's own gate.
  - It works its next change meanwhile, running only the tests that change adds or changes.
- Watch the mediator as it runs: follow every check through its stages until it ends, with `merge-queue status`, the logs, and the mediator's own turns.
  - Subscribe to it while a check is at work: `agent-repld workspace subscribe <the mediator> -events turn_ended,needs_input`.
- A mediator that stalls, fails a round, cannot be made, or decides against the evidence needs remediation itself: diagnose it from its records and fix agent-repl on this workspace's branch.
  - That fix lands like any other, once no check is at work.
  - When the mediator's own failure is what keeps the check at work, nothing else can end it: land the fix anyway, naming that failure in the changelog line.

### Bypass mode never uses the merge queue, and does its own work

- Bypass mode exists to skip the merge queue, so nothing this workspace lands or dispatches ever goes through it (owner ruling, 2026-11-15).
  - Every change lands directly on the main branch with `land-direct.sh`, never as a queued merge, an async merge, or a one-shot's finish.
  - This workspace never marks itself a one-shot: the daemon refuses the mark with `requester_bypassing_queue`.
- This workspace does the work itself: it creates no child workspace and runs no subagent unless the user explicitly asks for that particular thing to be done in a workspace or a subagent.
  - Every rule that would dispatch work (a handed failure's fix, a noticed deficiency's fix) is carried out here, by this workspace itself.
  - A child the user asked for never finishes into the queue: it commits its work on its branch, runs only the tests its changes add or change, and tells this workspace its branch, which this workspace lands directly as it lands its own.
  - Several such branches are merged into one branch and land at once: one full suite, one landing, one deploy.
- The full suite runs only as the gate of a landing: the merge queue's, or this workspace's own before it lands directly (Tests, below).
  - No child or subagent of this workspace ever runs it: the one run on the combined branch covers every branch in it.
- Only this workspace pauses or resumes the merge queue (owner ruling, 2026-11-14).
  - The workspaces and subagents it runs never pause, resume, evict or move anything on the queue, and their briefs say so.

### Tests

- Every test run is governed by default: the CPU governor holds it to its share of the host.
  - Run one ungoverned only when the user asks for it, by adding `-ungoverned -as-user`.
  - An ungoverned run takes every core, which the user feels on their machine.
- While writing a change, run only the tests it adds or changes, and keep every test it touches compiling.
- `land-direct.sh` runs the full suite before every landing, so never run it by hand before one.
  - A change can break a test it never touches, which no targeted run sees (2026-11-14: 0.436.0 landed a broken test this way).
  - The full suite took 4m47s, measured 2026-11-14.
- Bypass queue mode never leaves the main branch broken.
  - Never call the session's work done, or ask for the mode to be released, while the main branch fails a tracked test.
