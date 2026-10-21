# The deploy queue

The merge queue knows how to land exactly one repository's work: agent-repl's
own. For agent-repl's repository it rebases, tests, fixes, makes a merge
commit, tags a version and redeploys the running runtime; for every other
repository it runs the before- and after-merge prompts that repository's
`.agent-repl/prompts/` states, and nothing else. The switch between the two is
one check, whether the target is the daemon's own repository
(`agent-repld.internal.merge.run.emacsRepo`).

The owner wants the queue to be generic: each repository describes, in a file
it commits (`.agent-repl/deploy.yaml`), what happens when one of its
workspaces is finished. agent-repl's repository is described in that file
exactly as it behaves today, with no change in what it does. A workspace says
it is finished with one call, `agent-repld workspace finished`, and the
daemon does the rest from the repository's file. explanation-engine (create a
pull request, watch it until it merges, then update the local master from
origin) is the next repository to be described this way, and is deferred.

## Core design principles

### A finished workspace says so with one call, and its repository decides the rest

- **Principle.** "The workspace itself just needs to call the same old
  `finished` command"; `SPC TAB o`'s prompt tells every one-shot to call it
  when its work is done, whatever the repository, and the daemon then does
  what the repository's config says.
- **Consequences.** `agentrepl.v1.FinishWorkspace` replaces
  `agentrepl.v1.MergeWorkspace` entirely. The one-shot completion directive is
  the daemon's own, the same for every repository; a repository's
  `.agent-repl/prompts/oneshot-completion-directive.md` is no longer read. A
  repository with no `.agent-repl/deploy.yaml` refuses `workspace finished`
  loudly; there is no default destination.
- **Reopens.** `MergeWorkspaceOneShot`'s `merged_upstream` arm (a pull request
  merged upstream: update main and close) has no home: it is
  explanation-engine's flow, which becomes that repository's `post_deploy`
  hooks once it is ported.
- **Does not claim.** It does not make the agent choose a destination, and it
  does not change async merges (`CreateWorkspaceAsyncMerge`), which run the
  same repository config in their hidden merge workspace.

### agent-repl's own behavior does not change

- **Principle.** "For agent-repl that should be exactly what we currently do,
  with no changes at all. We are only genericizing the setup."
- **Consequences.** agent-repl's `deploy.yaml` turns on exactly the steps the
  own-repository check turns on today, and every existing queue test passes
  against it. The steps that are agent-repl's Go code (its suite table, its
  `test-all.sh` output parsing, its rerun commands) stay daemon code as named
  step kinds the config selects; they do not move into scripts, because a
  script could not reproduce them exactly.
- **Does not claim.** It does not forbid the visible additions the owner asked
  for: the bubble draws a tab for each hook that runs, so agent-repl's bubble
  gains a `deploy` tab for its deploy script.

### A hook is a script or a prompt, as equal arms, and a prompt goes only to the deploy's owner

- **Principle.** "Anywhere you can provide a script, you can provide a
  prompt." The workspace a prompt goes to is defined higher up: "it should
  always be applying to the workspace that's owning the deploy, never anything
  else, and never configurable."
- **Consequences.** `repoconfig.v1.DeployHook` is a oneof of a script and a
  prompt. No hook names a workspace. The deploy's owner is the workspace that
  runs it: the finished workspace itself, or an async merge's hidden merge
  workspace. Closing the owner and removing its worktree are the deploy's last
  steps, after every hook, so no prompt hook can reach a closed owner and no
  script can see `AGENT_REPL_CALLING_WORKSPACE` point at a removed worktree.
- **Does not claim.** Telling an async merge's requester how the deploy ended
  is not a hook: it goes to a different workspace than the owner, and stays
  the daemon's built-in report.

### A script hook runs from the repository's main root

- **Principle.** "The script hook should always be run from the main
  repository's root", with `AGENT_REPL_CALLING_WORKSPACE` set to the full path
  of the calling workspace.
- **Consequences.** The script path in the config is relative to the main
  root; the runner sets the working directory there and the environment
  variable to the owner's worktree. The runner also sets the deploy's identity
  (`AGENT_REPL_DEPLOY_LEDGER`) so a script can call back into the daemon about
  this deploy (agent-repl's own deploy script does).

### A failed hook is reported to the workspace, which investigates it

- **Principle.** "Failure of any scripts should result in the workspace being
  notified by daemon (as we do for example with merge completion) and the
  workspace should be prompted to investigate the reason for the failure."
- **Consequences.** A failed hook in any stage fails the deploy. The deploy's
  failure report, the same handback a failed merge sends today, tells the
  workspace and prompts it to investigate; a failed deploy never closes its
  owner, so the owner is there to investigate in. A deploy whose `deploy` or
  `post_deploy` hook failed after its local merge landed still records the
  commit it landed.
- **Reopens.** Today a failed after-merge prompt only writes a WARN and the
  merge still counts as landed. Under this principle it fails the deploy.

### Holds are per repository, and hold only the deploy stage

- **Principle.** "Rollout pauses should be made repo-specific", and "we only
  need to hold the deploy hook itself": for agent-repl that is the hot
  reload; the merge into master keeps landing.
- **Consequences.** The daemon-wide rollout hold becomes a hold per
  repository on the `deploy` stage. While a repository is held its deploys
  still run their `pre_deploy` stage (agent-repl's merges land), skip their
  `deploy` hook, and record the skip. Releasing a hold runs the repository's
  deploy hook once when a deploy was skipped and the hook is a script, which
  is what keeps agent-repl's release deploying what landed during the hold,
  exactly as it does today. A held prompt deploy hook has no owner to run in at
  release, so it is skipped for good; the skill documents that a deploy hook
  acts on the current tip, never on one landing.
- **Does not claim.** It does not hold `pre_deploy` or `post_deploy` hooks.

### Stages, and which run one at a time

- **Principle.** Three stages, `pre_deploy`, `deploy` and `post_deploy`.
  Concurrent stages may only come before or after one unbroken run of
  one-at-a-time stages, so the queue keeps holding at most one deploy per
  repository.
- **Consequences.** This landing has no concurrency setting at all: every
  stage of every repository runs one at a time, inside the queue, which is the
  queue as it is. The setting arrives with explanation-engine's port, whose
  `pre_deploy` and `deploy` run concurrently and whose `post_deploy` runs one
  at a time.

## Landed changes

### 1. `repoconfig.v1`, the files a repository commits under `.agent-repl/`

- **What.** A new package, `repoconfig.v1`, whose `DeployConfig` is the schema
  of `.agent-repl/deploy.yaml`. The daemon reads the YAML, converts it to JSON
  and decodes it with `protojson` with unknown fields refused, so the file is
  fully typed and a misspelled key is an error, not a silent default.
- **Why.** The owner asked for the repository config to be generic and
  documented; a proto schema is the one place the shape and its comments live.
- **Shape decisions.**
  - The built-in steps are one optional message, `local_merge`: rebase, the
    tests and their fixing loop, the merge commit and the optional version
    tag, which is exactly what the own-repository check selects today. Only
    the combination that exists is modelled; explanation-engine's rebase
    without a local merge is a new arm when it is ported.
  - The tests are a oneof with one arm, `agent_repl_suites`, naming the step
    kind; its script path replaces the fixed `test-all.sh` path in
    `agent-repld.internal.merge.TestCommandFor()`.
  - The version tag is an optional message; present means a landing in the
    repository's main worktree requires a `minor` or `patch` increment from
    an agent, which replaces the own-repository check in
    `agent-repld.internal.merge.checkVersion()`.
  - A script hook is a message with its path and a required timeout; a
    timeout means nothing for a prompt, so it lives only on the script arm.
  - Every hook entry states which landings it runs for, `main` or
    `any_target`, with no default.
  - `deploy` is optional: a repository with no deploy step leaves it unset.
- **Consequences.**
  - `prompts.PolicyMergeBefore` and `prompts.PolicyMergeAfter`
    (`daemon/internal/prompts/policy.go`) are retired; their content moves to
    `pre_deploy` and `post_deploy` hooks.
  - A request's own `--before-merge` / `--after-merge` prompts still replace
    the repository's hooks for that stage, as they replace the policy briefs
    today.

### 2. `agentrepl.v1.FinishWorkspace` replaces `agentrepl.v1.MergeWorkspace`

- **What.** The rpc a workspace calls when it is finished. It names the
  workspace, the requester, the placement in the queue, and the version
  increment a versioned repository requires. It has no mode: finishing always
  runs the repository's deploy and closes the workspace once the deploy
  succeeds.
- **Why.** Principle 1.
- **Consequences.** `MergeWorkspaceOneShot` and its `merged_upstream` arm are
  gone. The CLI verb `workspace merge here` becomes `workspace finished`, and
  Emacs's merge action calls the new rpc.

### 3. `agentrepl.v1.RollOutLanding`, the deploy script's call

- **What.** The rpc agent-repl's deploy script calls. With a deploy's ledger
  it registers that the deploy's landing rolls out; the daemon fires the
  rollout at the same point of the deploy's teardown it fires today, after the
  deploy's report is owed to it. With no ledger (a hold released) it rolls out
  at once.
- **Why.** The rollout is the daemon's own machinery and stays so ("the daemon
  owns deploys"). Firing it from inside the hook would let a rollout pay the
  owed reports before this deploy's report was owed, and the report would wait
  for the next rollout; registering keeps today's order exactly.

### 4. `agentrepl.v1.UpdateDeployHold` replaces `agentrepl.v1.UpdateRollouts`

- **What.** Hold or release one repository's deploy stage.
- **Why.** Principle 6.
- **Consequences.** The store's rollout hold, one row for the daemon, becomes
  one row per repository with the skip recorded on it, a store layout
  migration.

### 5. The bubble's hook tabs

- **What.** `frontend.v1.FeedMergeTab` gains `hook_prompt` (agentic, a prompt
  hook's turn) and `hook_script` (resolved: the script, its exit, its log and
  the tail of its output). The `pre_prompt` and `post_prompt` arms are retired:
  every prompt a deploy runs from config or a request is a prompt hook, and
  its stage is in the label the daemon resolves.

### 6. Request hooks and the footer's stages use the new names

- **What.** `CreateWorkspaceMergeActions` (`before_ws_merge`,
  `postprocessing_prompt`) is replaced by `CreateWorkspaceDeployHooks`, whose
  `pre_deploy` and `post_deploy` are lists of `repoconfig.v1.DeployHook`, so a
  request's hooks are scripts or prompts exactly as a repository's are. The
  footer's `preprocessing` and `postprocessing` substatus and activity arms are
  retired for `pre_deploy`, `deploying` and `post_deploy`, each naming the
  hook it runs (`FooterMergeStepHook`), and a failed hook is its own failure
  area (`FooterSubStatusMergeFailedHook`).
- **Why.** The owner: "the existing names for hooks in the old skill are
  terrible. We should use our new nomenclature." And a prompt may be given
  anywhere a script may, so a request's hooks take either.
- **Consequences.** The CLI's `-before-merge`/`-after-merge` become
  `-pre-deploy-*`/`-post-deploy-*`; the stored creation job's hooks change
  shape, a store layout migration.

### 7. The merged-upstream finish is retired

- **What.** `MergeWorkspaceOneShotMergedUpstream`, the footer's
  `updating_main` steps and the version increment's `merged_upstream` refusal
  are removed. The bubble's `updating_main` tab stays as a history-only arm:
  no deploy draws it, but bubbles recorded before the deploy queue carry it,
  and those rows are drawn again from the store, and no new tab is the same
  fact, so there is nothing to rewrite them into.
- **Why.** Its only entry point was `MergeWorkspace`'s `merged_upstream` arm,
  which has no home once finishing always runs the repository's deploy;
  explanation-engine's "update main once the PR merged" becomes its
  `post_deploy` hook when it is ported.
- **Consequences.** `wsm.MergeSourceMergedUpstream` and the queue's
  `updateMain` path are dead code and are deleted; the stored source kind's
  number is kept reserved.

### 8. Stored bubbles are rewritten, not read through a second vocabulary

- **What.** The bubble's tab rows are stored encoded
  (`durable_feed_rows`). Layout 68 rewrites every stored tab whose kind is the
  retired `pre_prompt` (3) or `post_prompt` (8) arm as `hook_prompt`, whose
  fields are the same numbers (`state`: live 1, settled 2), so the arm's bytes
  carry over unchanged. Migration steps gained a Go `Rewrite` beside their DDL
  for this (`daemon/internal/wsm/migrate.go`).
- **Why.** Without it, every bubble recorded with a before- or after-merge
  prompt (every explanation-engine merge has one) would draw with no tab kind,
  which the webapp refuses as malformed.
- **Verification.** `TestTheMigrationRewritesStoredRetiredPromptTabs` and the
  `rewriteRetiredMergeTab` tests in `daemon/internal/wsm/deployhooks_test.go`.

### 9. The one-shot mark's `policy_missing` refusal is retired

- **What.** `SetWorkspaceOneShotError.policy_missing` (tag 7) is removed.
- **Why.** It refused a mark whose repository's policy lacked a completion
  directive; the directive is now the daemon's own (principle 1), so the
  refusal cannot arise. A daemon corpus without it is the daemon's fault,
  answered as an internal error.
