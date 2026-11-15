# GitHub workflow-run subscriptions: protobuf design record

A workspace waiting on a GitHub Actions workflow run had no way to learn the
run finished short of polling `gh run view <id>` in a loop itself (the
`eval-cache-torch-human` workspace did so for a CEAC sandbox release,
2026-11-15). The owner asked for a subscription, beside `pr-subscribe`, that
tells a workspace exactly once, when the whole run completes: the workflow's
name, the run's conclusion, the head SHA and the run's URL. No progress
notice of any kind. The subscription ends with that notice, and a run
already complete when subscribed to is told at once. The design ran
autonomously: the owner did not ask to design the protobufs together.

## Context (autonomous assumptions)

- Objective: a workspace is told, under the hood and once, that a workflow
  run it waits on finished, and how.
- What is built: a third GitHub subscription kind beside the pull request and
  the merge queue, with its three agent-API rpcs (subscribe, unsubscribe,
  list), a prompt origin, and the `run-subscribe`, `run-unsubscribe` and
  `run-subscriptions` verbs.
- Frontend component: none. The notice is a deferred prompt the feed draws no
  row for, like the other GitHub notices.
- Systems: daemon (`prwatch` watcher, `ghclient` leaf, the server's agent-API
  handlers, prompt-origin classification, the feed's author label), state
  store `wsm` (two new tables), the `agent-repld` CLI, Emacs
  (`lisp/wire-common.el`'s list of origins no client sends), the fake daemon
  (`lisp/testsupport/fakedaemon`), harness (the `/agent-repl-workspaces`
  skill, a new brief).
- Non-additive changes: none. Every change is a new message, rpc, file or
  enum value.

## Core design principles

- **The daemon collects, the agent is told** (carried from
  `github-event-subscriptions.md`). Consequence: the run is read by the
  daemon's poller through the one `ghclient` leaf; the agent is told by a
  notice. Does not claim the agent never runs `gh` itself.
- **One notice, at completion** (owner's words: exactly ONE notice, when the
  whole run completes, no progress notices of any kind). Consequence: the
  contract has no event set, no activity arms and no per-job vocabulary; the
  only other sentence ever owed is the shared credential-rejected sentence,
  which the owner required to behave as it does for pull requests.

## Iteration sequence

Transport: the existing ConnectRPC `agentrepl.v1.AgentRepl` service. Suite:
the three rpcs join the AGENT API section after the GitHub merge-queue rpcs.
Order walked top-down by containment: the shared file
`github_workflow_run.proto`, then the three endpoints, then the prompt
origin. Conventions carried forward: every response is `oneof result {
success; error; }`; the subscription's health reuses
`PullRequestSubscriptionHealth` and its failure `PullRequestGitHubFailure`,
as the merge-queue subscription does.

## Landed changes

### 1. `agentrepl/v1/github_workflow_run.proto` (new, shared)

WHAT: `GithubWorkflowRunRef` (repository plus `uint64 run_id`),
`GithubWorkflowRunSubscription`, `GithubWorkflowRun` (workflow name, head
SHA, URL, and a `progress` oneof of unfinished or completed),
`GithubWorkflowRunConclusion` with one empty arm per conclusion GitHub
documents.

Reasoning:
- `run_id` is 64 bits: run ids already exceed 32 bits (43771255095).
- The repository reuses `GithubRepositoryRef` from the merge-queue file,
  importing the encompassing type rather than respelling owner and repo.
- Conclusion is a oneof of empty arms per the no-state-enum rule. Its arms
  are GitHub's documented workflow-run conclusions (success, failure,
  cancelled, timed_out, skipped, neutral, action_required, stale,
  startup_failure; REST docs tier). A conclusion GitHub adds later is the set
  wrapper with no arm, and the notice still carries GitHub's word.
- Unfinished carries GitHub's status word verbatim rather than arms: no
  reader acts on queued versus in progress, and the owner forbade telling it.
- No notice message is declared: the notice is sentences composed from a
  brief, as the other GitHub notices are, and its fields are the
  `GithubWorkflowRun` facts.

### 2. Endpoints

`endpoint_subscribe_github_workflow_run.proto`,
`endpoint_unsubscribe_github_workflow_run.proto`,
`endpoint_list_github_workflow_run_subscriptions.proto`.

Reasoning:
- The run is named three ways, the arm is the way: the URL (parsed by the
  daemon, so one parser serves every caller and an unreadable one is the
  `unparsable` answer), a repository and id, or an id in the subscriber's
  own repository (its `origin`), mirroring the merge queue's `subscribers`
  arm.
- An already finished run is an answer (`already_completed`), not an error.
  Its notice is owed in the store and the watcher is woken to deliver it at
  once, through the same delivery as every other notice.
- A failed read at subscribe time refuses with `unreadable` and the typed
  `PullRequestGitHubFailure`, as `SubscribePullRequest` does, so an expired
  token reads `unauthorized` there too.
- Unsubscribe and list exist for parity with the other GitHub kinds and for
  a workspace that stops caring before the run ends.

### 3. Prompt origin (`conversation/v1/prompt_origin.proto`)

`PROMPT_ORIGIN_GITHUB_WORKFLOW_RUN_NOTICE = 53`, drawn as no row and waiting
out a cold gate, like the merge-queue notice.

### Obviated-declaration sweep

Every change is additive; nothing is obviated.

## Consequences

- The store removes the subscription row in the same transaction that owes
  its completion notice (`wsm`), so the notice is owed exactly once per
  subscription and a completed run is never read again.
- The watcher's loop gains a wake channel the subscribe handler signals, so
  an already finished run's notice is delivered by the one goroutine that
  delivers every notice, never by a second, concurrent delivery.
- Every `gh` call the daemon makes is bounded (`ghclient`), so a hung call
  cannot stall the tick that reads every subscription.
