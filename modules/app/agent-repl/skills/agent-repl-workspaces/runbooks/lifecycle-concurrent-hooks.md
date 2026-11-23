# Lifecycle: concurrent stages and hook entries

- **`concurrent:` names a leading run of stages that every deploy waiting in
  the repository's queue runs side by side, ahead of its turn.** Its one key
  is the stage the run goes through: `through_pre_deploy: {}`, or
  `through_deploy: {}` for `pre_deploy` and `deploy`. Unset, every stage runs
  one deploy at a time.
  - After its concurrent stages, a deploy waits until every deploy queued
    ahead of it has finished, then runs the rest one at a time. A deploy whose
    concurrent stages finish first still enters the next stage in queue
    order. Its footer reads `awaiting turn: N ahead` while it waits.
  - Use it when a stage waits on something outside the daemon, such as a
    deploy hook that opens a pull request and waits for it to merge: that
    wait then holds no other deploy back.
  - A concurrent stage between two one-at-a-time stages cannot be written.
    `concurrent:` beside `pre_deploy.local_merge` is refused
    (`deploy_config_invalid`), because a local merge lands on the target,
    which one deploy at a time moves. So is `concurrent: {}` with no stage.

- Every hook entry is `hook:` (exactly one of `script:`, `prompt:` or
  `builtin: {}`) plus `runs_for:` (`main: {}`, only when the workspace lands
  on the repository's main branch in its main checkout, or `any_target: {}`,
  also when it lands in a parent workspace's checkout). For creation and
  finish it reads the workspace's landing target.
  - A **script** hook is `path:` (relative to the main checkout root) and
    `timeout_seconds:`. It runs from the main checkout root, with
    `AGENT_REPL_CALLING_WORKSPACE` (the workspace's worktree),
    `AGENT_REPL_HOOK_STAGE` (`creation`, `finish`, `pre_deploy`, `deploy` or
    `post_deploy`; it replaces the old `AGENT_REPL_DEPLOY_STAGE`) and
    `AGENT_REPL_SLACK_THREAD` (only for a workspace with a Slack origin) set.
    In deploy stages it also has `AGENT_REPL_DEPLOY_LEDGER` (the deploy's
    ledger) and `AGENT_REPL_DEPLOY_DAEMON`. In a deploy stage its tab shows
    the tail of its output, its exit and its log's path.
  - A **prompt** hook is `text:`. It is sent to the workspace's own session,
    and to no other workspace, ever. In a deploy stage its tab is that turn.
    Its text carries one placeholder, `{{slack_thread}}`, the link of the
    Slack message the workspace was created for. A workspace without a Slack
    origin cannot resolve it and the hook fails; any other `{{...}}` is
    refused when the config is read.
- `local_merge:` is the built-in local landing: `tests:` names the test
  runner (`agent_repl_suites: {script_path: ...}`), and `version_tag: {}`
  makes every agent's deploy onto main carry a version increment and tag the
  landing. A local merge's steps run only at the head (waiting merges
  are only rebased in the background, git alone); a repository with
  `concurrent:` runs its concurrent stages ahead of its turn.
