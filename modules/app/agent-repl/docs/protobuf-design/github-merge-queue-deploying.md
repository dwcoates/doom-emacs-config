# A GitHub merge queue place drawn as an agent-repl merge queue place

When a workspace's pull request enters its repository's GitHub merge queue, the
workspace reads as deploying (the merging status, purple) and stands in the
sidebar's Merge Queue at the pull request's GitHub place. The daemon manages
all of it: a workspace only OWNS the pull request (an owned pull-request link,
`agent-repld workspace links`), and the daemon follows the queue and reflects
it. A pull request has at most one owning workspace, which the daemon ensures.

## Core design principles

- **The reflection is the daemon's implementation detail.** A workspace never
  sets its own place or status; it only owns pull requests, at its own
  volition. Consequences: no new agent-facing verb; the only agent-visible
  change is the `pull_request_owned_elsewhere` refusal of a second owner. Does
  not claim: that owning a pull request does anything while it is outside a
  GitHub merge queue.
- **Only one workspace merges at a time per place.** GitHub's order is the
  order drawn, so two owning workspaces can never stand at one place.

## Assumptions (autonomous design)

- The claim is the existing owned pull-request link: the user's "workspaces
  only owning PRs" names that mechanism. Evidence tier: the user's wording plus
  existing code (`daemon/internal/workspace/workspacelinks.go`).
- The place is GitHub's entry order (`agentrepl.v1.PullRequestMergeQueuePosition`),
  already read by the pull-request poller for every owned link
  (`daemon/internal/prwatch/poller.go`, `readSnapshot`).

## Landed changes

### `agentrepl.v1.ChangeWorkspaceLinksError.pull_request_owned_elsewhere`

- WHAT: a refusal arm, `agentrepl.v1.ChangeWorkspaceLinksPullRequestOwnedElsewhere`
  (url, owner ref, owner name).
- WHY: the daemon guarantees one owner per pull request.
- CONSEQUENCES: the store enforces it with a partial unique index over the
  owned-link pull-request subscriptions (one per open owner; a closed workspace's
  subscriptions are deleted at close, so its ownership ends with it). Existing
  duplicate owners are demoted by the migration to `requested` subscriptions:
  they keep their notices but no longer own. Accepted cost.

### `frontend.v1.FooterStatusMerging.github_merge_queue`

- WHAT: substatus `frontend.v1.FooterSubStatusMergingGithubMergeQueue`
  (place, total), drawn "deploying · GitHub queue 3/5".
- WHY: the owning workspace reads as deploying, purple, like any merge.
- CONSEQUENCES: the merge orchestrator composes these facts with its own; an
  agent-repl merge of the same workspace takes precedence, since it is the
  merge the workspace runs.

### `frontend.v1.RosterRepoMergeQueue.github`

- WHAT: `frontend.v1.RosterMergeQueueGithubEntry` (subject, place, queued-at)
  and `frontend.v1.RosterMergeQueueGithubPlace` (place, total).
- WHY: the owning workspace is drawn in its repository's Merge Queue group at
  its GitHub place.
- CONSEQUENCES: a group may be listed with no head (only GitHub entries).
  Reusing `head`/`waiting` was rejected: the head is the merge agent-repl runs,
  with stages and a bubble, and GitHub's place 1 may be a pull request nobody
  here owns.
