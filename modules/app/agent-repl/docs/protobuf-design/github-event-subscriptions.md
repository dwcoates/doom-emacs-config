# GitHub event subscriptions: protobuf design record

The daemon, with no help from any agent, follows GitHub for two things: (1)
it fast-forwards each registered repository's main checkout when its main
branch moves upstream, prompting the repository's main workspace only when
the fast-forward fails in the index or worktree; and (2) it tells a workspace
about the activity on every pull request the workspace created, ending that
on merge or close. The full design, its options and the spike results are
`docs/investigations/2026-10-30-github-event-subscriptions-design.md`.

The owner asked the orchestrator to run the proto stage entirely itself
(2026-10-30), so no increment below waited on the owner's agreement; each
records the reasoning that settled it.

## Context

- Objective: GitHub activity reaches the daemon programmatically and reaches
  agents under the hood, with no event ever lost.
- Frontend component: none. The owner ruled out a sidebar sync outcome. The
  notices are deferred prompts the feed draws no row for; the sync repair is
  an ordinary prompt bubble.
- Systems: daemon (poller, `ghclient` leaf, main sync, notice delivery,
  links), state store `wsm` (subscriptions, told cursors, own-comment ids),
  `agent-repld` CLI (verbs), harness (metaprompt "Workspace links", the
  `/agent-repl-workspaces` skill). The webapp and Emacs only meet the new
  prompt origins.
- Non-additive change: `agentrepl.v1.ChangeWorkspaceLinksSet.relation` is
  REQUIRED, so every existing `workspace link` call must name `-owned` or
  `-referenced`. The verb and both harness homes change together.

## Core design principles

- **The daemon collects, the agent is told.** In the owner's words: it is the
  daemon that programmatically collects events and sends them to the agent
  under the hood. Consequence: no agent-facing rpc reads GitHub; notices are
  deferred prompts. Does not claim the agent never touches GitHub: it still
  posts, through `CommentOnPullRequest`.
- **Workspaces never unsubscribe themselves.** The daemon ends a subscription
  on merge or close. Consequence: `UnsubscribePullRequest` exists but is not
  taught to agents. Does not claim the rpc is removed.
- **Ownership decides following.** A link the workspace OWNS (a PR it
  created) is followed; a link it only references (a merged PR it amends) is
  not. Consequence: the relation is a required oneof on every link.

## Iteration sequence

Transport: the existing ConnectRPC `agentrepl.v1.AgentRepl` service. Suites:
one new run of rpcs inside the existing AGENT API section, beside
`SubscribeWorkspace`. Inventory, then shapes one family at a time: shared
pull-request vocabulary, the four endpoints, the link relation, main sync,
prompt origins. Conventions carried forward: every response is `oneof
result { success; error; }`; subscriptions copy `SubscribeWorkspace`'s
lifetime and "nothing slips" contract.

## Landed changes

### 1. `agentrepl/v1/pull_request.proto` (new, shared)

WHAT: `PullRequestRef`; `PullRequestSubscription` with its origin (owned
link or requested) and health (healthy or failing, with a typed
`PullRequestGitHubFailure`); `PullRequestStatus` and `PullRequestState`;
`PullRequestNotice` and `PullRequestActivity`, whose arms are comment,
review, review comment, edited, deleted, state changed and check finished.

WHY: one vocabulary for the subscription, the links that own it, and the
notice, so the answer to a subscribe, a link read and a notice describe a
pull request identically.

Reasoning that settled it:
- Activity kinds a subscription asks for are an enum
  (`PullRequestNoticeEvent`), matching `WorkspaceNoticeEvent`. They are kinds,
  not a state, and no sibling field depends on the value. Each notice item's
  kind is its oneof arm.
- Check runs carry only name and conclusion: the owner asked for checks to be
  very brief, a title and a result, so many checks cost a line each.
- Edits and deletions are reported (the owner asked for them if possible).
  Deletion has no timestamp because the daemon learns of it only by absence.
  Consequence: the poller must read each followed PR's FULL comment lists
  (`issues/{n}/comments`, `pulls/{n}/comments`, `pulls/{n}/reviews`) with
  ETags, not the repository-wide `since=` lists the investigation first
  proposed. A `since=` list cannot show a deletion. Unchanged lists answer
  `304`, which costs nothing (spike, investigation section 10).
- Author association is an enum, because a role is not a state of the
  activity. UNSPECIFIED stands for an association this schema revision does
  not know.
- The failure type was first written `PullRequestReadFailure` and renamed
  `PullRequestGitHubFailure` before landing, because `CommentOnPullRequest`
  reports a failed POST with it.
- Accepted cost: a subscription whose anchor read failed (the link was stored
  while GitHub was unreachable) is told everything already on the PR at its
  first successful read, flagged as such by the brief. That is lossless and,
  for a PR the workspace just created, small. The alternative, anchoring by
  local time, could drop items through clock skew.

### 2. Endpoints

`endpoint_subscribe_pull_request.proto`,
`endpoint_unsubscribe_pull_request.proto`,
`endpoint_list_pull_request_subscriptions.proto` (a read),
`endpoint_comment_on_pull_request.proto`; four rpcs in `service.proto`'s
AGENT API section.

Reasoning:
- Subscribing to a merged or closed PR is an answer (`already_ended`), not an
  error.
- `CommentOnPullRequest` exists because the agent posts under the user's
  login, and the user's own comments must keep waking the agent (owner
  ruling). The daemon records each created comment id as the workspace's
  own, in the answering transaction. That match is exact. The rejected
  alternatives: a marker in the text, which an edit can strip, and dropping
  the user's login, which loses the user's requests.

### 3. Link relation (`workspace_links.proto`, `endpoint_change_workspace_links.proto`)

WHAT: `WorkspaceLink.relation` and the required
`ChangeWorkspaceLinksSet.relation`, a `WorkspaceLinkRelation` oneof of owned
and referenced. An owned link carries `WorkspaceLinkOwnedFollow`, whose arms
are following (with the subscription), ended (with the merged or closed
state), unsubscribed, and not followable.

WHY: the owner's ruling that the daemon subscribes by itself, and that a
workspace storing a merged PR it amends must not be subscribed.

Consequences: removing an owned link, or setting it referenced, ends the
subscription. Every existing caller of `workspace link` breaks until it names
a relation. Harness edits land through `/agent-repl-update-harness`.

### 4. `agentrepl/v1/main_sync.proto` (new)

WHAT: `MainSyncFailure` and its cause: diverged (ahead and behind), local
changes (paths), or git failed (command, exit status, stderr).

WHY: owner ruling that main sync is the daemon's alone, that a fetch failure
from connectivity is only logged, and that a workspace is told only of a
fast-forward that fails in the index or worktree. There is no rpc; the
message is the typed content of the repair brief.

### 5. Prompt origins (`conversation/v1/prompt_origin.proto`)

`PROMPT_ORIGIN_MAIN_SYNC_REPAIR = 45` is drawn as a prompt bubble, and a
closed main workspace is reopened for it (owner ruling).
`PROMPT_ORIGIN_PULL_REQUEST_NOTICE = 46` is drawn as no row, like a workspace
notice.

### Obviated-declaration sweep

Every change was additive except the required relation, so nothing is
obviated. `SetWorkspacePrincipalLink` predates the links menu and is not
touched by this change.

## Not yet done

Bindings are not regenerated and nothing is implemented. `protoc` accepts
every file (`make lint`). The new rpcs will break the daemon's handler build
once bindings regenerate, until the daemon implements them.

### 6. `agentrepl.v1.PullRequestGitHubFailure.refused` (implementation increment)

WHAT: a fifth cause arm, `refused`, carrying the HTTP status and GitHub's
message.

WHY: while writing the `gh` leaf, a comment POST GitHub rejects (422, or a
403 that is not a spent rate limit) fit none of the four arms. Folding it into
`unreachable` would have reported a request GitHub answered as a network
failure.

## Implementation notes (2026-10-30)

- Reads: the poller reads each followed PR's full lists per tick
  (`pulls/{n}`, `issues/{n}/comments`, `pulls/{n}/comments`,
  `pulls/{n}/reviews`, and `commits/{head}/check-runs` only when a
  subscriber asks for checks), page by page, each page conditional on its own
  ETag. A full page is always followed by the next, so an item past a full
  last page is never missed. Pages are cached in memory; a restart reads them
  whole once.
- Told snapshot (`wsm.PullRequestSubscription.Told`): JSON of id-to-updated-at
  maps, review state and body digest, state, and per-name check results. Maps
  marshal sorted, so the store's compare-and-set compares states. Own comments
  are written into it by `prwatch.Watcher.Comment` with a compare-and-set
  loop, and the loop never writes into an unanchored row.
- Owed notices (`pull_request_untold_notices.pending`) are appended in the
  same transaction as the snapshot advance, and delivered by the next step of
  the same tick. A failed submission leaves them pending, so the next tick
  retries; the superseded notice's sentences are kept with an empty turn.
- Main sync: `repodivergence.DefaultEvery` dropped from 5 minutes to 1. A fetch
  that finds nothing new costs one round trip, the same as `ls-remote`, so no
  separate probe was added. The sync holds the merge queue's target through
  `merge.Orchestrator.HoldTargetForSync`, which marks first and reads the
  durable queue second. It holds the main workspace's turn sequence through
  `promptqueue.Queue.Exclusively`, deferring to the next sweep when a turn
  (or a waiting question or permission) is open.
- Test daemon builds link `ghclient.ghBinary` to a path no file can hold
  (`harness.NoGitHubLink`), so no test daemon can reach GitHub.
- The CLI resolves `-pr` for `pr-unsubscribe` and `pr-comment` itself, so it
  takes a url or `OWNER/REPO#N` only. `pr-subscribe` also takes `#N`, which the
  daemon resolves through the subscriber's `origin` remote.

### 7. The link relation folded into master's `WorkspaceLinkOwnership` (2026-10-31)

CORRECTION to entry 3. While this branch was in flight, master landed the
same fact under another name: `agentrepl.v1.WorkspaceLink.ownership`, a
`WorkspaceLinkOwnership` whose arms are `owned` and `reference`, on the same
field numbers (4 on `WorkspaceLink`, 3 on `ChangeWorkspaceLinksSet`). Master
uses it to post a Slack-created workspace's owned links into its thread. Two
types for one fact break the one-canonical-form rule, so entry 3's
`WorkspaceLinkRelation`, `WorkspaceLinkRelationOwned` and
`WorkspaceLinkRelationReferenced` are gone.

WHAT: master's `WorkspaceLinkOwned` gains `optional WorkspaceLinkOwnedFollow
follow = 1`, this branch's follow standing, unchanged. Nothing else of
master's shape moved.

CONSEQUENCES:
- The CLI flags are master's: `-owned` and `-reference` (not `-referenced`).
- `wsm` reuses master's `workspace_links.owned` column (layout 92). This
  branch's layout adds only the pull-request key columns and its own tables,
  and moved to layout 95, after master's 93 and 94.
- `workspace.LinkOwnershipView(bool)` stays master's request builder;
  `workspace.ownershipView(link)` adds the follow standing to an owned link's
  answer.
- An owned link that is no github.com pull request (a doc, a branch) answers
  `follow.not_followable`, since master's owned arm covers more than pull
  requests.
- ROOT CAUSE of the clash: two workspaces modeled one fact in parallel, and
  the branch ran far behind master. The merge queue's per-commit replay could
  not reconcile it, because the code that follows the type arrived in later
  commits.
