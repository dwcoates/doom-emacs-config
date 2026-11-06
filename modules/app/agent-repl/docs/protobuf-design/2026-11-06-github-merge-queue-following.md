# GitHub merge queue following: protobuf design record

A workspace that lands work through a repository's GITHUB merge queue (not
agent-repl's own landing queue) has no way to learn that its pull request
joined the queue, moved up it, was removed from it, or landed, short of
reading GitHub itself. The owner asked (2026-11-06) for the daemon to follow
the GitHub merge queue in two ways: per pull request, so a workspace learns
when its own pull request advances, lands or is removed (the main case), and
per queue, so a workspace watches a repository's whole queue move. Both are
built at once. The design ran autonomously: the owner did not ask to design
the protobufs together.

## Context (autonomous assumptions)

- Objective: a workspace is told, under the hood, every movement of a pull
  request through its base branch's GitHub merge queue, and may follow a
  whole queue.
- What is built: a new event on the existing pull-request subscription
  (`agentrepl.v1.PullRequestNoticeEvent`), new activity arms on
  `agentrepl.v1.PullRequestActivity`, and a new subscription kind, to a
  repository branch's merge queue, with its own three agent-API rpcs and its
  own notice.
- Frontend component: none. Both notices are deferred prompts the feed draws
  no row for, as the pull-request notice already promises.
- Systems: daemon (`prwatch` watcher and reader, the `ghclient` leaf which
  gains a GraphQL call, the server's agent-API handlers, prompt-origin
  classification, the feed's author labels), state store `wsm` (one new event
  bit, two new tables), the `agent-repld` CLI (new verbs and the new event
  name), Emacs (`lisp/wire-common.el`'s list of origins no client sends), the
  fake daemon (`lisp/testsupport/fakedaemon`), harness (the
  `/agent-repl-workspaces` skill, a new brief).
- Non-additive changes: none to the wire. Every proto change is a new enum
  value, a new oneof arm, a new optional field, a new message or a new rpc.
  An owned link's subscription asks for every event, so every owned pull
  request is followed through its merge queue from this change on; that is
  intended.

## Core design principles

- **The daemon collects, the agent is told** (carried from
  `github-event-subscriptions.md`). Consequence: queue facts reach agents only
  as notices and subscription answers; no rpc lets an agent read the queue
  from GitHub. Does not claim the agent never runs `gh` itself.
- **Structurally impossible beats very improbable** (the metaprompt's
  rule). Consequence: an enqueue or a removal is detected from GitHub's
  durable timeline events, never by comparing two polls' queue membership,
  so an enqueue followed by a removal inside one poll gap is still told.
  Positions are derived from the queue's own entry order, never from
  `MergeQueueEntry.position`, whose base GitHub does not document.

## Iteration sequence

Transport: the existing ConnectRPC `agentrepl.v1.AgentRepl` service. Suites:
the three new rpcs join the AGENT API section beside the pull-request rpcs.
Order walked, top-down by containment: (1) the shared merge-queue vocabulary
in `pull_request.proto` (entry state, position) and the pull request's new
event, activity arms and status field; (2) the queue subscription's shared
file `github_merge_queue.proto`; (3) the three endpoints, one at a time;
(4) the new prompt origin. Conventions carried forward: every response is
`oneof result { success; error; }`; a subscription's lifetime copies
`SubscribePullRequest`'s.

## Landed changes

### 1. The pull request's way through its merge queue (`pull_request.proto`)

WHAT: `PULL_REQUEST_NOTICE_EVENT_MERGE_QUEUE = 6`; five new
`PullRequestActivity` arms (`merge_queue_entered`, `merge_queue_moved`,
`merge_queue_entry_state_changed`, `merge_queue_removed`,
`merge_group_check_finished`); `PullRequestStatus.merge_queue`
(`PullRequestStatusMergeQueue`); the shared vocabulary
`PullRequestMergeQueuePosition` and `PullRequestMergeQueueEntryState`.

WHY: the owner's main case is learning when the workspace's own pull request
advances, lands or is evicted from the GitHub merge queue.

Reasoning that settled it:
- A new event, not a new subscription kind: the pull request is already the
  subject the workspace follows, and an owned link's subscription asks for
  every event, so every owned pull request is followed through its queue
  with no new call.
- Landing is NOT a new arm. A merge by the queue is the pull request's
  `state_changed` to merged (whose actor is already documented as unset for a
  queue merge), and it ends the subscription. A second "landed" arm would be
  a second spelling of the same fact.
- Joining and removal come from GitHub's timeline events
  (`AddedToMergeQueueEvent`, `RemovedFromMergeQueueEvent`), each told once by
  its node id. Rejected: comparing queue membership between two reads, which
  cannot see a join and a removal inside one poll gap. That gap is the
  failure the metaprompt's structural rule forbids.
- Position is `entries_ahead` and `entries_total`, derived from the order of
  the queue's own entry list. Rejected: `MergeQueueEntry.position`, whose
  base (0 or 1) GitHub's schema does not state (introspected 2026-11-06:
  "The position of this entry in the queue").
- The entry state is a oneof of empty arm messages per the no-state-enum
  rule. Its arms are exactly GitHub's `MergeQueueEntryState` values,
  introspected 2026-11-06: QUEUED, AWAITING_CHECKS, MERGEABLE, UNMERGEABLE,
  LOCKED.
- Merge-group checks are their own arm reusing
  `PullRequestActivityCheckFinished`, because they run on the queue's merge
  group commit, not the head, and they are what decides a removal. Reusing
  the message keeps one spelling of "a check finished".
- Actors reuse `PullRequestActivityAuthor` with the association UNSPECIFIED,
  following the precedent of `state_changed.actor` from REST's `merged_by`.
  Accepted cost: UNSPECIFIED there means "the timeline names no standing",
  which the field comments state.
- `PullRequestStatus.merge_queue` is set only for a subscription asking for
  the event. Accepted cost: its absence is ambiguous across subscriptions
  that did not ask, which the comment states, so no reader treats UNSET as
  "not queued" without having asked.

### 2. A whole queue (`github_merge_queue.proto`, new)

WHAT: `GithubRepositoryRef`, `GithubMergeQueueRef`,
`GithubMergeQueueSubscription`, `GithubMergeQueueStatus`,
`GithubMergeQueueEntry`, `GithubMergeQueueNotice`, and
`GithubMergeQueueActivity` with arms joined, merged, removed, head changed,
and emptied.

WHY: the owner's second case, responding to the queue whenever it moves.

Reasoning that settled it:
- Every name carries `Github` so nothing reads as agent-repl's own landing
  queue (`MergeQueue*`, `SubscribeMergeQueue`), which the owner named as a
  different thing.
- The subscription has no event filter. Every change to a queue is the same
  kind of news, and nothing a subscriber could ask to drop was evident.
- A departure is classified by reading the departed pull request once:
  merged (with its merge commit) or removed (with the latest removal reason).
  Accepted cost: an entry that joined and left between two reads is never
  told to a whole-queue subscriber. The file header states it, and points to
  the per-pull-request event, which misses no joining.
- Health reuses `PullRequestSubscriptionHealth` and its
  `PullRequestGitHubFailure`. Both are reads of GitHub through the same `gh`
  login, so a second health vocabulary would be a respelling. Accepted cost:
  the name says "pull request".
- `GithubRepositoryRef` is new. `PullRequestRef` spells owner and repo
  inline, and embedding the new ref there would be a breaking wire change for
  no consumer's benefit, so the two spellings stand side by side.

### 3. Endpoints

`endpoint_subscribe_github_merge_queue.proto`,
`endpoint_unsubscribe_github_merge_queue.proto`,
`endpoint_list_github_merge_queue_subscriptions.proto` (a read); three rpcs
in `service.proto`'s AGENT API section after `CommentOnPullRequest`.

Reasoning:
- The repository is a oneof: named exactly, or the subscriber's own through
  its `origin` remote. The branch is optional; unset is the repository's
  default branch, resolved in the same GraphQL read that anchors the
  subscription.
- Subscribing again is `already_subscribed`, an answer. A queue
  subscription has nothing to replace, unlike a pull request's events.
- A branch with no merge queue is the `no_merge_queue` refusal. GitHub
  answers `mergeQueue: null`, and following it would only ever fail.
- An origin that is not on github.com is the `not_on_github` refusal,
  carrying the remote verbatim.

### 4. Prompt origin (`conversation/v1/prompt_origin.proto`)

`PROMPT_ORIGIN_GITHUB_MERGE_QUEUE_NOTICE = 50`, unseen like a workspace
notice. Its own origin rather than reusing `PULL_REQUEST_NOTICE`, because a
queue notice is not about a followed pull request.

CONSEQUENCE found while placing it: `PROMPT_ORIGIN_PULL_REQUEST_NOTICE` is
documented "drawn as a workspace notice is", but
`daemon/internal/promptorigin/promptorigin.go:54` (`Unseen`),
`daemon/internal/promptqueue/submit.go:331` (`waitsOutColdGate`) and
`daemon/internal/resolve/feed/prompt.go:404` (`AuthorLabel`) never list it,
so a pull-request notice was drawn as a prompt the user said. The
implementation lists both origins in all three.

### Obviated-declaration sweep

Every change is additive, so nothing is obviated.

## Implementation notes (2026-11-06)

- Reads (`daemon/internal/prwatch/queueread.go`): each tick, the queue facts
  of every followed pull request asking for the merge queue are read in
  ONE GraphQL query per repository, each pull request under its own alias.
  Unlike REST reads, which a 304 makes free, every GraphQL query spends the
  `gh` login's rate limit. A queue's entries are read once a tick for every
  pull-request and queue subscription that needs them (`queueReads`).
- `ghclient.Client.GraphQL` posts the query and variables as a JSON body on
  stdin (`gh api graphql --input -`). A field GitHub nulled comes back as a
  per-path error that the caller maps to that pull request's own failure, so
  one deleted pull request does not fail its repository's batch.
- Timeline window: the latest 100 merge-queue events per pull request. The
  told snapshot keeps the ids in the current window, so it stays bounded.
- The told snapshot's new fields are `omitempty`. A subscription that never
  asks for the queue therefore encodes exactly as before, and its
  compare-and-set is unaffected.
- Store (`wsm`): layout 104 (additive) adds the two queue tables. Layout 105
  (BREAKING) gives every owned-link subscription the new event bit (64),
  because an owned link asks for every event. The previous build refuses a
  row holding an event it does not declare, so that deploy restarts. The
  owed-notice SQL and read health are shared by both kinds through
  `githubTables`.
- Delivery: one `owedNotice` path delivers both notices. The queue notice
  uses the brief `prompts/github-merge-queue-notice.md` and
  `PROMPT_ORIGIN_GITHUB_MERGE_QUEUE_NOTICE`.
- CLI: `pr-subscribe -events` spellings derive from `wsm`'s event list, so
  no second list of names exists.
