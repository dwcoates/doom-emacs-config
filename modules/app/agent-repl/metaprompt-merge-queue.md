## Landing through the merge queue

This workspace lands its work through its repository's merge queue.

### When to run which tests

- Tests are used actively throughout: written alongside the code and run often to drive the work.
- Outside the merge queue, run only the tests your own work added or changed, named one by one.
  - Never an existing test the work did not touch, even one covering the code it changes.
  - Never a whole suite, never a suite of another system, and never a repeat run for confidence.
  - A finished change and a git merge you made yourself are verified the same way, by the tests that work added or changed.
- Every test beyond those runs only in the merge queue, which runs the full suite as the gate of every landing.
  - The gate runs them anyway, so running them yourself duplicates its work and adds load the host's other workspaces contend with.
  - A failure the gate finds is remediated in the merge's own test-failure turn, as a targeted loop over the cases it hands over.
  - The rare exception is a change with systemic knock-on effects, and the run states that reason.
- Where the repository's daemon runs tests (`agent-repld test run`), every test run goes through it, never a hand-run test command.
  - The repository's tests skill says how to name the tests.

### Master moves only through the merge queue

- Merge into master only through the merge-queue skill (`/agent-repl-merge-queue`).
  - This covers my own workspace's branch and every subagent's branch alike.
  - Never merge, commit, cherry-pick, rebase, reset, or push onto master by hand.
  - On a parked, failed, or refused merge, report the reason and stop rather than landing the work some other way.
- Land every complete change as an async merge into master WITHOUT asking.
  - A complete change is a full feature addition or change, a full fix, or a full addendum to an already-merged change, that is sensible and regresses nothing.
  - Never ask whether to merge or whether to add work to the merge queue; merge it, then report the merge.
  - Only incomplete work waits: a partial feature, or an addendum not yet finished.
  - A fix or feature is often landed async and then tweaked in later changes, and each later complete change lands the same way.
- Every offer to merge a workspace or branch MUST name which mode the merge would run in.
  - An offer is made only where the merge is not already prescribed above, such as a one-shot that would close this workspace.
  - A one-shot merge makes the workspace unavailable while the merge runs, and closes the workspace once it lands.
  - An async merge keeps the workspace open and available, both while the merge runs and after it lands.
  - Never offer a merge without its mode, because the two modes leave me with different workspaces afterwards.
