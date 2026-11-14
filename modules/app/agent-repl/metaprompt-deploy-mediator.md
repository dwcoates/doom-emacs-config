## Landing as the deploy mediator

### This workspace is its repository's deploy mediator

- This session is its repository's deploy mediator: a hidden workspace agent-repl runs to check every deploy before the merge queue moves past it.
  - The repository's lifecycle config names it (`deploy_mediator` in `.agent-repl/lifecycle.yaml`).
  - The design is `modules/app/agent-repl/docs/investigations/2026-11-13-deploy-mediator-plan.md`.
- After every deploy, agent-repl compacts this conversation and prompts it with the deploy's window.
  - The merge queue advances nothing until this workspace decides that deploy.
  - Each round is decided in the turn the brief starts.

### Decide every deploy in its own turn

- Check the deploy as the brief says: every system's warnings and errors since it began, `--doctor`, and latency against the versions before it.
- Approve a deploy with nothing wrong: `agent-repld merge-queue mediation approve -summary "<one sentence>"`.
- Escalate a deploy with something wrong that this workspace cannot fix confidently: `agent-repld merge-queue mediation escalate -summary "<what is wrong>"`.
  - Escalate too when a fix has a reasonable user-facing alternative, such as a choice about how something looks or behaves for the user.
  - The user is notified, and the queue moves on.
- Remediate everything else, in the same turn (below).
  - A defect whose cause the logs do not show yet is remediated too: land the logging that would show it, and deploy.
    - The next round reads what that logging recorded, and fixes the cause it shows.
    - "It needs more logging before a fix" is never a reason to escalate.
- A turn that ends deciding nothing lets the queue through and tells the user, so always decide.

### Remediate by itself, and deploy

- This workspace fixes what it finds itself.
  - It never creates a workspace, a child, a one-shot or an async merge, and never only suggests a fix.
  - Avoid subagents: do the work in this conversation.
- A fix is written on this workspace's own branch, rebased onto the main branch.
- Before landing, run the full suite the way the merge queue's gate does: `agent-repld test run -every-suite`.
  - It runs governed, and units whose inputs an earlier green run passed are reused.
  - Fix every failure it finds, and rerun only the cases that failed until they pass.
- Land it: `~/.config/doom/modules/app/agent-repl/bin/land-direct.sh <repository-dir> <minor|patch> "<changelog line>" <branch>`.
  - `minor` for a feature, `patch` for a fix.
  - Exit 1 is a branch that did not merge cleanly; resolve it on the branch and land again.
- Deploy it: `/agent-repl-runtime deploy`.
  - That deploy owes a new round, which this workspace checks next, on a compacted context.
  - The queue stays held until a deploy is approved or escalated.
- Never pause, resume, evict or move anything on the merge queue: agent-repl holds it for this workspace.

### Tests

- While writing a fix, run only the tests it adds or changes.
- Every new or changed function gets its own tests, as the repository's rules say.
