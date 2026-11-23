# Lifecycle: agent-repl's own config and what the config decides

- A deploy hook asks the runtime to roll out what landed with
  `agent-repld runtime roll-out-landing -deploy "$AGENT_REPL_DEPLOY_LEDGER"`.
- agent-repl's own config:

```yaml
pre_deploy:
  local_merge:
    tests:
      agent_repl_suites:
        script_path: modules/app/agent-repl/bin/test-all.sh
    version_tag: {}
deploy:
  emoji_ack: &redeploy
    hook:
      script:
        path: modules/app/agent-repl/bin/deploy-landing.sh
        timeout_seconds: 60
    runs_for:
      main: {}
  user_or_workspace: *redeploy
finish:
  emoji_ack:
    hooks: []
post_deploy:
  emoji_ack:
    hooks:
      - hook:
          prompt:
            text: "Your work has landed on master. Post one brief reply in the thread of the Slack message this workspace was created for ({{slack_thread}}), in your own words, saying the work is done and what it changed. Post it with the send-to-slack skill."
        runs_for:
          main: {}
# Once the queue fully depletes, master is pushed to origin as a release.
queue_drained:
  - path: modules/app/agent-repl/bin/push-release.sh
    timeout_seconds: 120
```

- WHAT THE CONFIG DECIDES, AND WHAT THE DAEMON ALWAYS DOES:

| configured per repository | fixed in the daemon |
|---|---|
| every creation, finish, pre-deploy, deploy and post-deploy hook, per kind of workspace, and each one's scope | the queue: its order, placement, pause, eviction and three slots (the head, fixing tests, resolving conflicts) |
| which leading stages run concurrently (`concurrent`) | that the rest are entered one deploy at a time, in queue order |
| whether a local merge runs, and its test runner | the local merge's own steps: rebase with conflict resolution, the test gate with fixing attempts, the non-fast-forward merge commit |
| whether the repository is versioned (`version_tag`) | the version arithmetic and the tag's form |
| the rollout, as a deploy hook calling `roll-out-landing` | when a requested rollout fires (after the deploy's report) |
| what runs once the queue fully depletes (`queue_drained`) | when it is owed, that it runs with the queue's admissions held, and that a failure leaves it owed |
| whether a kind keeps its built-in default, replaces it, or keeps it beside its own hooks (`builtin: {}`) | the defaults' text, and that the originator is the creator's to state, never inferred |
| | holding the deploy stage, the hand-back of a failure, closing the workspace on success, the bubble, footer and roster |
