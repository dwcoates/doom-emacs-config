# Lifecycle: failures, holds, queue_drained and gate checks

- **A failed hook fails what its stage belongs to** (the create, the finish
  or the deploy): a script that exits non-zero, runs past its timeout or
  cannot start, or a prompt whose turn does not complete. A failed deploy
  leaves the workspace open, handed the failure to investigate. A failure
  after the local merge landed keeps the landing.
- **Hooks act on the repository as it stands when they run** (its current
  tip), not on a snapshot of the deploy that queued them. A script hook a
  restart cut short runs again, so a script must bear running twice.
- **A repository's deploy stage can be HELD** (`/agent-repl-runtime`,
  `runtime deploy-hold`): its deploys still run `pre_deploy` (agent-repl's
  merges still land on master) and `post_deploy`, and SKIP their deploy hook.
  Releasing the hold runs the deploy stage's `user_or_workspace` main-scoped
  script, else the `emoji_ack` one, once.
- **`queue_drained:` lists scripts the daemon runs once the repository's
  queue FULLY DEPLETES**: nothing waits in it or runs, it is not paused, and a
  merge was admitted since the scripts last all succeeded. Each entry is a
  script's `path:` and `timeout_seconds:`; prompts are refused, since an
  empty queue has no workspace to prompt.
  - The debt is durable: it is owed from each merge's admission and cleared
    only once every script succeeded. A restart in between (a landing's own
    rollout) leaves it to the next daemon, which runs the scripts once it
    finds the queue empty.
  - Nothing lands while they run. A merge enqueued meanwhile is admitted
    after them.
  - A failed script stops the rest, is recorded at ERROR under
    `daemon.merge.queue_drained` with its log's path, and leaves the debt
    owed, so the queue's next emptying runs them all again. Each must bear
    running twice on the same state.
- **`pull_requests: {gate_checks: [NAME, ...]}` names the repository's gate
  checks**: check runs that fail until a person acts (an approval, a label),
  such as a required-review gate. They are left out of every commit's CI
  outcome, so a followed pull request still waiting on its reviewer is told
  its CI passed.
  - Unlike every other key, it is read from GitHub, from the file at the
    pull request's head commit, never from a checkout.
  - A followed pull request is told its head's CI run once per outcome, never
    once per check run: `CI on head SHA failed: `lint` failed.` as soon as a
    check fails, or `CI on head SHA passed: all 48 checks succeeded.` once
    every one passed.
  - A head whose file is unusable fails the subscription's read, naming the
    file, until a commit fixes it.
