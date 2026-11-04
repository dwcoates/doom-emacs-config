# Lifecycle hooks (2026-11-03)

A repository's hooks ran only after `workspace finished`: four stages
(`pre_deploy`, `deploy`, `post_deploy`, `queue_drained`) in
`.agent-repl/deploy.yaml`. The owner settled an overhaul:

- two new stages, one when a workspace first comes online (creation) and one
  when it calls `workspace finished` (finish);
- every workspace stage says different things for two kinds of workspace,
  decided only by who created it: the Slack emoji-ack system, or the user or
  another workspace;
- built-in defaults for emoji-ack workspaces replace the daemon's own Slack
  posts, which retire from code;
- a new link relation lets a stored Slack message name the pull request it
  announces, so a post-deploy script reacts `:merged:` on it without
  searching the thread.

Designed in the skill's autonomous mode: every assumption and ruling below is
the designer's, recorded for review after the fact.

## Context (step 1, assumptions)

- OBJECTIVE: a repository says what happens at each point of a workspace's
  life, per kind of workspace, and the daemon stops speaking in Slack itself.
- SYSTEMS: the daemon (config reader, create verb, merge queue, links, CLI),
  the elisp client (its CreateWorkspaceRequest encoder), the skills and the
  metaprompt, agent-repl's own config. No webapp component changes: the
  finish stage draws no bubble (see the finish-stage entry).
- NON-ADDITIVE: the config file is renamed; `repoconfig.v1.DeployConfig`
  becomes `repoconfig.v1.LifecycleConfig`, its stage fields change shape
  (old tags reserved); the daemon's Slack posts and the `slack_post_failed`
  fault kind are deleted; `CreateWorkspaceRequest` gains a REQUIRED field, so
  every creator must state it.
- OUTSIDE THIS REPOSITORY: gns-sockets' `dispatch-workspace.sh` lives in the
  explanation-engine repository, which this change must not edit. Until it
  passes `-originator emoji-ack`, its workspaces are created as
  `user_or_workspace` workspaces; the explanation-engine follow-up carries
  that change with its config migration.

## Iteration sequence (step 2)

Settled autonomously, highest abstraction first: (1) the config file's name
and root message, (2) the stage shape shared by every workspace stage, (3)
the hook arms (built-in), (4) the create request's originator, (5) the create
refusals, (6) the announcing link relation, (7) the finish-hook prompt origin.

## Landed changes

### 1. The file is `.agent-repl/lifecycle.yaml`; the message is `repoconfig.v1.LifecycleConfig`

- WHAT: `repoconfig/v1/deploy_config.proto` became `lifecycle_config.proto`;
  `DeployConfig` became `LifecycleConfig`; the hook messages lost their
  `Deploy` prefix (`Hook`, `HookEntry`, `HookScript`, `HookPrompt`,
  `HookRunsFor*`), because creation and finish hooks are not deploy hooks.
  The stage-specific messages (`DeployPreDeployStage`, `DeployLocalMerge`,
  `DeployConcurrentStages`, ...) keep their names: they ARE about the deploy.
  `agentrepl.v1.DeployConfigMissing`/`DeployConfigInvalid` became
  `LifecycleConfigMissing`/`LifecycleConfigInvalid`; the oneof ARM names that
  carry them (`no_deploy_config`, `deploy_config_invalid`,
  `async_merge_no_deploy_config`, `async_merge_deploy_config_invalid`) keep
  their spellings, because arm names are protojson wire names the elisp
  client decodes.
- WHY: the file now says what happens when a workspace is created and when it
  finishes, neither of which is a deploy. `hooks.yaml` was rejected because
  the file also holds the local merge and the concurrency rule, which are not
  hooks; `workspace.yaml` was rejected because `queue_drained` is a
  repository's, not a workspace's. "Lifecycle" names what every key is about.
- CONSEQUENCES: the daemon reads ONLY the new path. A repository still holding
  `deploy.yaml` is refused as having no config (`no_deploy_config`, naming
  `lifecycle.yaml`), loudly, until it migrates; no fallback read of the old
  path exists. agent-repl's own config and its second clone migrate with this
  change. explanation-engine migrates in its own follow-up (it may not be
  edited here), and until then its `workspace finished` is refused.
- The repoconfig package version stays `v1`: the old stage tags are
  `reserved`, never reused, and the YAML is read only by the daemon, at use,
  from a file whose name changed, so no reader meets the old shape under the
  new schema.

### 2. Every workspace stage holds one hook list per kind of workspace

- WHAT: `creation`, `finish` and `post_deploy` are `LifecycleStageHooks`
  (`emoji_ack`, `user_or_workspace`, each an optional `LifecycleHookList`);
  `pre_deploy` carries the same two lists beside its local merge; `deploy` is
  `LifecycleDeployStage` with one optional `HookEntry` per kind. `concurrent`
  and `queue_drained` are unchanged.
- WHY: the owner's rule is that each stage may say different things for the
  two kinds, decided only by who created the workspace.
- UNSET IS THE BUILT-IN DEFAULT, uniformly: a kind left unset runs the stage's
  built-in default for that kind, which is empty except for emoji-ack
  creation, finish and post_deploy. A kind set (even to an empty list)
  replaces the default. Alternatives rejected: one list with a per-entry
  "for" selector (a repository adding a hook for everyone would silently
  drop the emoji-ack default, or not, depending on a rule nobody can see);
  emoji-ack hooks running IN ADDITION to the other kind's (the owner named two
  disjoint kinds).
- `queue_drained` has no kind: it runs when a repository's queue empties, for
  no workspace.
- The deploy hold's release (`agentrepl.v1.UpdateDeployHold`) runs the deploy
  stage's script ONCE for skipped deploys of either kind: it runs the
  `user_or_workspace` script when it is a main-scoped script, else the
  emoji-ack one. CONSEQUENCE accepted: a repository whose two kinds name
  different deploy scripts gets only one of them run on release.

### 3. A hook may be the stage's built-in default (`Hook.builtin`)

- WHAT: `repoconfig.v1.Hook` gained a third arm, `builtin`
  (`HookBuiltIn`, empty): "run this stage's built-in emoji-ack default here".
  Valid only in an `emoji_ack` list of a stage that has a default; refused
  anywhere else, and refused in a request's own hooks
  (`agentrepl.v1.CreateWorkspaceDeployHooks`).
- WHY: a repository overriding the emoji-ack finish stage (explanation-engine
  creating its pull request there) can keep the default Slack announcement
  after its own hook without copying its text.
- THE DEFAULTS (daemon code, `internal/lifecycleconfig`):
  - creation: a prompt asking the agent to post a brief message, in its own
    words, to `{{slack_thread}}` saying work has started;
  - finish: a prompt asking the agent to post to the thread that its pull
    request was created, with a hyperlink, and to store the post's permalink
    as an owned link announcing the PR
    (`workspace link here -url <permalink> -label ... -owned -announces <PR>`);
  - post_deploy: the script `bin/hooks/slack-merged-reaction.sh` in the
    daemon's agent-repl checkout, which reads the workspace's links, and for
    every owned link announcing a pull request that GitHub reports merged,
    adds `:merged:` to that Slack message. It never searches the thread.
  - every default runs for `any_target`.
- THE PLACEHOLDER `{{slack_thread}}` is the one placeholder a prompt hook may
  carry; scripts get `AGENT_REPL_SLACK_THREAD`. Both are set only for a
  workspace with a Slack origin. A prompt carrying the placeholder for a
  workspace without one FAILS its hook (a creation hook fails the create);
  any other `{{...}}` in a prompt is refused when the config is read.

### 4. `CreateWorkspaceRequest.originator` is REQUIRED

- WHAT: `agentrepl.v1.WorkspaceOriginator` (oneof `emoji_ack` /
  `user_or_workspace`), field 14, REQUIRED on every form. `emoji_ack` requires
  `slack_origin` and is refused beside `async_merge`. Recorded on the
  workspace (`workspaces.originator`, an additive column defaulting to
  `user_or_workspace`).
- WHY: the owner ruled the originator a first-class fact the creator states,
  never inferred (in particular never from `slack_origin`, which any creator
  may pass).
- The CLI's `-originator emoji-ack|user-or-workspace` defaults to
  `user-or-workspace`; the elisp client always sends `user_or_workspace`.
- Existing workspaces read `user_or_workspace`: no backfill infers a past
  originator from a recorded Slack origin.

### 5. Two create refusals

- `CreateWorkspaceError.lifecycle_config_invalid` (52, `LifecycleConfigInvalid`):
  the repository's config is unusable when the create reads it for its
  creation stage. A MISSING config is no refusal at create: the stage then
  runs only the built-in defaults.
- `CreateWorkspaceError.creation_hook_failed` (53,
  `CreateWorkspaceCreationHookFailed`: the hook's account and its log path): a
  creation script hook failed. The create is undone (worktree, branch and
  registration removed) and the failure is reported to the repository's main
  workspace through the existing failed-create report. Both arms are faults
  of the repository, not of the caller's input.
- AMENDED while implementing: `creation_hook_failed` also carries a creation
  PROMPT hook whose `{{slack_thread}}` its workspace cannot resolve (path and
  log path empty), because that is the only way a creation prompt fails and
  it must fail the create, not reach the agent with a hole in it. An async
  merge's hidden merge workspace runs no creation stage; the comment on
  `LifecycleConfig.creation` now says so.

### 6. An owned link may announce another link (`WorkspaceLinkOwned.announces`)

- WHAT: `agentrepl.v1.WorkspaceLinkAnnouncement { url }` as an optional field
  of `WorkspaceLinkOwned`, read and written; refusal
  `ChangeWorkspaceLinksError.unknown_announced_link` when the announced url is
  not a link the workspace holds. Recorded as `workspace_links.announces`, an
  additive nullable column.
- WHY: the post-deploy reaction must find the Slack message that announced a
  merged pull request from stored facts alone. Putting the relation inside the
  `owned` arm makes a reference that announces something unrepresentable.

### 7. `conversation.v1.PromptOrigin.PROMPT_ORIGIN_FINISH_HOOK` (49)

- A finish-stage prompt hook's turn. The finish stage runs after the
  requesting turn ends and BEFORE the deploy is put in line, so it has no
  ledger and no bubble; its prompts are drawn in the workspace's own feed.
  Creation prompt hooks need no origin: their text is part of the initial
  prompt (`PROMPT_ORIGIN_WORKSPACE_CREATED`).

## The finish stage's runtime (no schema)

- Runs in the merge request's wait (`internal/merge/request.go`), after the
  requesting turn ends and before `queueRequest` puts the deploy in line, for
  every in-place deploy of a workspace; never for an async merge.
- Its progress is durable (`merge_finish_progress`, additive): a restart
  resumes at the recorded hook and reattaches to a prompt hook's turn rather
  than sending it again.
- A failed hook withdraws the request (nothing is put in line) and hands the
  failure back to the workspace as a prompt, as a failed deploy hands its
  failure back.

## Retired

- `internal/workspace/slackorigin.go`'s "work has begun" post and end post,
  the merge-end observer they hung off, `internal/slackpost` and
  `internal/slackfmt` where nothing else uses them, and the footer fault kind
  `slack_post_failed`. The originating thread is still recorded and stored as
  a reference link.

## Renumbered at rebase

- `PROMPT_ORIGIN_FINISH_HOOK` is tag 49: tag 48 landed first as
  `PROMPT_ORIGIN_COMMAND_THREAD_BACKGROUND`.
- The store additions are layout 101 (`lifecycle_hooks`), since
  `slack-origin-pr-posts` landed layout 100 first; layout 102
  (`drop_workspace_slack_posts`, BREAKING, a stop-then-start deploy) drops
  what layout 100 added, with the posts that wrote it.
- The two create refusals are tags 52 and 53: `name_branch_exists` landed as
  51 first.
