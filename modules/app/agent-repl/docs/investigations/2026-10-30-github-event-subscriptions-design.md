# GitHub event subscriptions: design (2026-10-30)

Status: DESIGN, nothing implemented, no `.proto` touched. Owner rulings of
2026-10-30 are recorded in section 9. The proto shapes
below are proposals; per the `create-or-update-protobufs` skill each one lands
in the real `.proto` files as the owner agrees it.

Two features:

1. **Main-branch sync (repository level).** When a merge lands on a registered
   repository's main branch on GitHub, the daemon fast-forwards that
   repository's main checkout. When it cannot, it prompts the repository's main
   workspace with the error and asks it to remediate.
2. **Pull request subscriptions (workspace level).** A workspace subscribes
   itself to a pull request through `agent-repld workspace ...` and is told,
   as a deferred prompt, about every comment, review and review comment there.

## 1. What already exists and is reused

| Piece | Where | Reused for |
|---|---|---|
| Notice delivery to an agent as a deferred prompt under its own origin, waiting behind a running turn, waking an idle agent, no feed row | `daemon/internal/workspacesubs`, `daemon/internal/mergenotice`, `conversation.v1.PromptOrigin` | both features' notices |
| Ordered, non-blocking report loop that drains on exit | `daemon/internal/noticeloop` | both features |
| One untold notice per (subscriber, subject), superseded by the next and recorded durably | `daemon/internal/untoldnotice`, `wsm.UntoldWorkspaceNotice` | PR notices never pile up behind a busy agent |
| Subscription contract: answer carries a snapshot, nothing slips between read and follow, durable across restart and handover, ends on subscriber close | `proto/src/agentrepl/v1/endpoint_subscribe_workspace.proto` | the PR subscription copies the contract |
| Background sweep of every registered repository: resolve primary branch, fetch its upstream, count ahead/behind | `daemon/internal/repodivergence` (startup, then every 5 minutes) | detection for feature 1 |
| `git merge --ff-only` in the main worktree | `daemon/internal/gitclient/gitclient.go:208` (merge queue landing) | the sync step |
| Which daemon may act during a handover | `daemon/internal/intakegate`, `rollout.Controller.ServesIntake` | only the serving daemon polls |
| Account routing (work vs personal) | `daemon/internal/account`, `daemon/internal/pullrefs` | which `gh` account's token a repository is read with |
| Workspace links, which already store every PR the workspace creates | `agentrepl.v1` links verb | candidate auto-subscription (question Q3) |

## 2. How a laptop learns about GitHub events: options on merit

The deciding property is whether an event can be lost. GitHub webhooks are
**edge-triggered**: GitHub sends each delivery once, does not retry a failed
one automatically (redelivery is a manual API call, kept 3 days), and the
laptop is asleep, offline or mid-restart a large share of the time. Anything
built on delivered edges therefore needs a second mechanism to recover lost
edges, and that second mechanism is exactly the polling design below. Reading
**authoritative state** against a durable cursor is **level-triggered**: a
missed tick is caught by the next one, because the next one reads everything
since the cursor. Loss becomes structurally impossible rather than improbable.

| Option | Latency | Can an event be lost? | Needs | Verdict |
|---|---|---|---|---|
| A. Repository or org webhook to a tunnel on the laptop (cloudflared, ngrok, Tailscale Funnel) | seconds | **Yes**, every delivery while the laptop or tunnel is down | admin on every repo (ChessCom repos: org admin), public endpoint on the laptop, webhook secret | rejected: lossy, inbound exposure |
| B. `gh webhook forward` (cli/gh-webhook) | seconds | **Yes**, the temporary hook only exists while the command runs | admin on the repo or org, one repo/org per process, documented as a development tool | rejected |
| C. GitHub App or webhook to a hosted relay with a durable queue (Cloudflare Worker + Durable Object, Hookdeck); daemon holds an outbound WebSocket and acks by cursor | seconds | Only when the relay itself is down, since GitHub does not retry | cloud service to run and secure; org admin to install an App on ChessCom repos; still needs state reconciliation for relay outages | not now; possible later as a wake-up signal (Q1) |
| D. Actions workflow per repo POSTing to an endpoint | tens of seconds | Same as A, plus fork PRs cannot reach secrets | a workflow file in every repo, Actions minutes, an endpoint | rejected |
| E. **Poll authoritative state with conditional requests** (`gh`/REST with `If-None-Match`; `git ls-remote` for the main tip) | the poll interval | **No**: state is re-read against a durable cursor | only the user's existing `gh` login (scopes `repo`, `read:org` are present) | **recommended** |
| F. Notifications API (`GET /notifications`) | ≥60 s (`X-Poll-Interval`) | No, but thread-level only, says "something happened", not what | user token; marks threads read, which changes the user's GitHub UI | rejected as the source |
| G. Repository events API (`GET /repos/{o}/{r}/events`) | documented 30 s to 6 h | Capped at 300 events | user token | rejected: latency unbounded |
| H. Slack GitHub app | seconds | Slack is the source of truth, not GitHub | Slack read access, parsing bot messages | rejected |

**Recommendation: E.** It needs no infrastructure, no admin rights, no
inbound port, and makes loss impossible. Its cost is latency of one poll
interval. `304 Not Modified` answers to authenticated conditional requests do
not count against the 5000/hour REST limit, so a short interval is cheap.

C is the only option that buys lower latency. If it is ever added it must not
be a second delivery path: the relay's message would only wake the poller
early (the poller still reads state), and the relay's own health (connected,
last message) must be a loud state of its own, never silently absorbed by the
poll. That keeps one source of truth.

## 3. Feature 1: main-branch sync

### 3.1 Detection

`repodivergence` already owns "how far does each registered repository's
primary branch stand from its upstream" and already fetches. Main-branch sync
becomes a consumer of its outcome, not a second fetcher:

- `repodivergence.Counted{Ahead: 0, Behind: n>0}`: upstream moved, local main
  has nothing of its own, so fast-forward (3.2).
- `Ahead > 0, Behind > 0`: diverged; prompt the main workspace (3.3).
- `Ahead > 0, Behind == 0`: local work not pushed (agent-repl's own master,
  which lands locally); nothing to do.
- `Failed`: the fetch failed; logged by `repodivergence` already; the sync
  does nothing and says nothing to the agent (Q5).

Detection is provider-agnostic (it works for any git remote) and the
5 minute cadence is the open latency choice (Q2). A cheaper probe,
`git ls-remote <remote> refs/heads/<main>` every N seconds with a fetch only
when the tip changed, would make a short cadence affordable.

### 3.2 The sync step

`git merge --ff-only <upstream>` in the main worktree, after the fetch the
sweep already did. Never `git pull`: `pull` obeys the user's
`pull.rebase`/`pull.ff` config, so its behavior would differ per machine.

Two writers can touch the main checkout at once, and both races are made
impossible, not unlikely:

- **The merge queue's own local landing** (`gitclient` `merge --ff-only` at
  `gitclient.go:208`). The sync runs as a step owned by that repository's
  merge queue, so the queue is the one writer of the main branch ref.
- **The main workspace's agent editing files mid-turn.** The sync runs only
  between that workspace's turns, under the same exclusion a deferred prompt
  waits on, so no agent turn ever sees HEAD move under it. A dirty tree is
  then git's own call: `merge --ff-only` refuses atomically when local
  changes would be overwritten, leaving nothing half-applied, and that
  refusal is a failure handled as in 3.3.

### 3.3 Failure: prompt the main workspace

On any failed fast-forward (diverged, dirty overlap, hook failure, lock held),
the daemon submits one deferred prompt to the repository's main workspace
under a new origin `PROMPT_ORIGIN_MAIN_SYNC_REPAIR`, carrying: the repository,
local and upstream SHAs, ahead/behind, the git command, its exit status and
stderr, and the dirty paths. The brief asks the agent to bring main to the
upstream tip and says the daemon will retry on the next sweep.

Dedupe: the main workspace holds at most one untold sync notice per
repository (`untoldnotice`), and a notice is sent once per (upstream SHA,
cause). The same failure at the same SHA on the next sweep is logged, never
re-prompted. A new upstream SHA supersedes the untold notice.

### 3.4 Proto surface

No new rpc. Additions:

```protobuf
// conversation.v1.PromptOrigin
PROMPT_ORIGIN_MAIN_SYNC_REPAIR = 41;
```

```protobuf
// frontend.v1 roster: the repository section can show the sync's last outcome
// beside the divergence it already draws (only if the owner wants it, Q6).
message RosterRepoSync {
  oneof outcome {
    RosterRepoSyncCurrent current = 1;          // main is at its upstream
    RosterRepoSyncFastForwarded forwarded = 2;  // from, to SHAs
    RosterRepoSyncRefused refused = 3;          // cause, prompted workspace
  }
}
```

## 4. Feature 2: pull request subscriptions

### 4.1 Verbs

```text
agent-repld workspace subscribe-pr here -pr <url|#n> [-events comments,reviews,review-comments,state,checks]
agent-repld workspace unsubscribe-pr here -pr <url|#n>
agent-repld workspace pr-subscriptions here
```

`#n` resolves against the workspace's repository's GitHub owner and name,
taken from its `origin` remote URL, not from the directory name (see the todo
row on `pullrefs`).

### 4.2 Messages

Same contract as `agentrepl.v1.SubscribeWorkspace`: arms are outcomes, at most
one subscription per (subscriber, pull request), resubscribing replaces the
events, durable across restart and handover, ends when the subscriber closes
or unsubscribes, and the answer carries the snapshot the cursor starts from.

```protobuf
// endpoint_subscribe_pull_request.proto
message PullRequestRef {
  string owner = 1;
  string repo = 2;
  uint32 number = 3;
}

enum PullRequestNoticeEvent {
  PULL_REQUEST_NOTICE_EVENT_UNSPECIFIED = 0;
  PULL_REQUEST_NOTICE_EVENT_COMMENT = 1;         // conversation comment
  PULL_REQUEST_NOTICE_EVENT_REVIEW = 2;          // review submitted, with its state
  PULL_REQUEST_NOTICE_EVENT_REVIEW_COMMENT = 3;  // line comment or reply
  PULL_REQUEST_NOTICE_EVENT_STATE = 4;           // merged, closed, reopened, ready
  PULL_REQUEST_NOTICE_EVENT_CHECKS = 5;          // a check suite finished
}

message SubscribePullRequestRequest {
  workspace.v1.WorkspaceRef subscriber = 1;
  // Either a ref or text the daemon parses (a URL or `#n`).
  oneof pull_request {
    PullRequestRef ref = 2;
    string text = 3;
  }
  repeated PullRequestNoticeEvent events = 4;    // at least one
}

message SubscribePullRequestResponse {
  oneof result {
    SubscribePullRequestSuccess success = 1;
    SubscribePullRequestError error = 2;
  }
}
message SubscribePullRequestSuccess {
  oneof taken {
    SubscribePullRequestSubscribed subscribed = 1;
    SubscribePullRequestResubscribed resubscribed = 2;
  }
  PullRequestRef pull_request = 3;
  // The pull request as it stands when the cursor was taken: title, state,
  // head SHA, counts of comments/reviews, and the newest item's id.
  PullRequestStatus status = 4;
}
message SubscribePullRequestError {
  oneof cause {
    SubscribePullRequestUnknownSubscriber unknown_subscriber = 1;
    SubscribePullRequestSubscriberClosed subscriber_closed = 2;
    SubscribePullRequestUnparsable unparsable = 3;        // text is no PR
    SubscribePullRequestNotFound not_found = 4;           // 404 from GitHub
    SubscribePullRequestNoAccount no_account = 5;         // no gh login can read it
    SubscribePullRequestGitHubUnreachable unreachable = 6;// network, 5xx, with cause
  }
}
```

`UnsubscribePullRequest` and `ListPullRequestSubscriptions` follow
`endpoint_unsubscribe_workspace.proto` exactly. New origin
`PROMPT_ORIGIN_PULL_REQUEST_NOTICE = 42`.

### 4.3 Polling and routing

- **One poller per pull request, not per subscription.** Subscriptions are
  stored in `wsm` keyed by (subscriber, PR). The poller set is the distinct
  PRs with at least one live subscription. Two workspaces following one PR
  cost one poll.
- **Reads per tick, every one conditional (`If-None-Match`)**, chosen by
  the 2026-10-30 spike (section 10), which found the timeline does NOT carry
  line comments:
  - per repository with a live subscription:
    `GET /repos/{o}/{r}/issues/comments?since=` and
    `GET /repos/{o}/{r}/pulls/comments?since=`, sorted by update, which
    cover every conversation and line comment on every PR in one read each;
  - per subscribed PR: `GET /repos/{o}/{r}/pulls/{n}` (state, merge, head)
    and `GET /repos/{o}/{r}/pulls/{n}/reviews` (review bodies and states,
    which no repository-wide endpoint lists);
  - check suites (`GET /repos/{o}/{r}/commits/{head}/check-suites`) only when
    a subscription asks for `CHECKS`.
  Every read is of the authoritative list itself. The PR's `updated_at` is
  deliberately NOT used as a gate, because nothing documents which activity
  moves it, and a gate that misses one kind would lose that kind silently.
- **Routing:** on a changed read, the poller computes the items newer than
  each subscription's cursor, filters them by that subscription's events,
  and reports one notice per subscriber to `noticeloop`.
- **Only the serving daemon polls** (`intakegate`), and see 4.4 for why a
  second poller during a handover still cannot double-notify.
- **Lifetime ends:** subscriber close (as `SubscribeWorkspace`), and the
  daemon itself unsubscribing every subscriber when its poll sees the PR
  merged or closed, after the final `STATE` notice is told (ruling R3).
  `UnsubscribePullRequest` exists in the API, but no workspace calls it as a
  matter of policy.

### 4.4 Dedupe and ordering

- The cursor per subscription is the newest timeline item id told. The
  untold notice and the cursor advance are written in ONE `wsm` transaction,
  conditioned on the cursor still being the value the poll read
  (compare-and-set). A daemon restart between the read and the write repeats
  the read; a second poller during a handover loses the compare-and-set.
  Duplicate notices are therefore impossible, not unlikely.
- A busy subscriber's untold notice is superseded by the next one, which
  lists every item since the subscriber was last told, in timeline order.
- Items inside one notice are in GitHub's timeline order; notices for
  different PRs carry no order relative to each other.
- Edits and deletions of a comment already told: not reported in this design
  (Q8).

### 4.5 The notice body and prompt injection

A PR comment is text written by whoever can comment on the repository, and it
arrives as a prompt to an agent that has permissions. The notice therefore
wraps each item's body as untrusted data (the same treatment the harness gives
pasted content), names its author and their association
(`OWNER`/`MEMBER`/`COLLABORATOR`/`CONTRIBUTOR`/`NONE`), links the item, and the
brief tells the agent that the body is a request to weigh, never an
instruction to follow. Comments from anyone on the PR are told (ruling R6).

### 4.6 Subscription by ownership (ruling R7)

No workspace subscribes by hand. A link stored with the workspace's links
gains a relation:

```protobuf
// agentrepl.v1.WorkspaceLink and agentrepl.v1.ChangeWorkspaceLinksSet
WorkspaceLinkRelation relation = 4;  // REQUIRED on set

message WorkspaceLinkRelation {
  oneof relation {
    // The workspace made the resource (it created this PR). The daemon acts
    // on it: an owned GitHub PR is subscribed for every event.
    WorkspaceLinkRelationOwned owned = 1;
    // The resource bears on the task but is not the workspace's (a merged
    // PR it amends, a ticket, a thread). The daemon does nothing with it.
    WorkspaceLinkRelationReferenced referenced = 2;
  }
}
```

- `workspace link here -url URL -label TEXT -owned` stores an owned link,
  `-referenced` a referenced one. The verb requires one of the two, so the
  harness's choice is never a silent default.
- Storing an owned link to an OPEN PR subscribes the workspace. The answer
  carries the subscription (or `already_closed` when the PR is merged or
  closed, in which case nothing is subscribed).
- The subscription ends when the daemon's poll sees the PR merged or closed
  (R3, R8), when the owned link is removed, and when the workspace closes.
- The harness change, made through `/agent-repl-update-harness`: the
  metaprompt's "Workspace links" section says a PR the workspace creates is
  stored `-owned`, and every other link `-referenced`.
- `SubscribePullRequest` stays in the API (ruling R3) for programs and
  tests, and is not taught to agents.

### 4.7 The agent's own comments

The daemon alone collects events and tells the agent. But an agent that
answers a review by replying on the PR (`gh pr comment`, `gh api` on a review
thread) writes a comment under the user's own login, and the daemon's next
poll sees that comment as a new event and tells the agent about its own
reply. The daemon cannot tell that reply from the user's comment by author,
and the user's own comments must keep waking the agent: commenting on the
agent's PR is how the user talks to the agent in public (ruling R6).

So the agent posts through the daemon:

```text
agent-repld workspace pr-comment here -pr <url|#n> -body-file <path>
agent-repld workspace pr-comment here -pr <url|#n> -in-reply-to <review comment id> -body-file <path>
```

The daemon posts with `gh api`, records the created comment's id against the
workspace's subscription in the same `wsm` transaction that answers, and the
poller never tells a subscriber an item recorded as its own. The match is by
id, so it is exact. A reply the agent posts with `gh` directly instead is
told back to it as an ordinary comment: visible, never lost. The harness says
replies on owned PRs go through `pr-comment`.

## 5. Auth

- A `ghclient` leaf beside `gitclient`: runs `gh api --include` (so status,
  `ETag` and rate-limit headers are read) under the account the repository's
  work is routed to (`account.Resolver`), choosing the `gh` login with
  `gh auth token --user <login>` when more than one is logged in.
- The token is never logged and never stored by agent-repl; `gh` owns it in
  the keyring.
- `401` is a user-setup error (`usersetup.Errorf`) naming the account and the
  `gh auth login` to run. `403` with rate limit exhausted backs the poller off
  until the `X-RateLimit-Reset` time and says so in the subscription's status.
- One `gh` login (`dwcoates`) reads every repository, including `ChessCom`'s
  (ruling R5), so no owner-to-login map is needed: the leaf uses the active
  `gh` login.

## 6. Error logging

All records go through `dlog` under `daemon.githubwatch.*` and
`daemon.mainsync.*`, one record at the ownership point (the poller for a
read, the sync step for git), never re-logged on propagation. Every record
carries: operation, repository id and dir, `owner/repo#n`, subscriber
workspace name, endpoint, HTTP status, rate-limit remaining and reset, the
cursor before, the git command and exit status with stderr for the sync, and
the outcome branch (`not_modified`, `changed`, `cas_lost`, `rate_limited`,
`auth_failed`, `not_found`, `unreachable`, `ff_refused`, `diverged`).

A failing poll is never silent to the agent either: the subscription carries
a health state (`healthy`, `failing since T: cause`) in
`ListPullRequestSubscriptions` and in the next notice, and the first failure
after a healthy stretch is told to the subscriber once. The poll keeps
trying on its schedule; there is no shadow path.

## 7. Test strategy

Per `~/.config/doom/modules/app/agent-repl/AGENTS.md`: no real git, no real
GitHub, a fake `gh` executable for the leaf, `clocktest` for ticks, no
`time.Sleep`. Table-driven, AAA, one case per edge.

- `ghclient`: parses status, ETag, rate-limit headers; `304` is
  `NotModified`; `401`/`403`/`404`/`5xx`/exec failure each map to their error
  with the logged context.
- poller: unchanged read reports nothing; a new comment reports one notice;
  events filter; two subscribers to one PR share one read and each get one
  notice; compare-and-set loss reports nothing and logs `cas_lost`; restart
  between read and write repeats without duplicate; busy subscriber is
  superseded with both items in order; unsubscribe and subscriber close end
  polling when no subscriber is left; merged PR tells `STATE` then ends.
- notice composition: untrusted wrapping, author association, links.
- mainsync: behind only fast-forwards; ahead only does nothing; diverged
  prompts once; same SHA and cause does not re-prompt; a new SHA supersedes;
  dirty overlap refusal prompts with paths; the sync waits for the main
  workspace's running turn to end; the sync and a queue landing never run
  together.
- verbs: `subscribe-pr` with a URL, with `#n` resolved from the remote URL,
  and each error arm rendered.

## 8. Open questions for the owner

- Q1. Is the poll interval's latency acceptable, or is near-instant delivery
  worth running a hosted relay (option C) as a wake-up signal?
- Q2. Poll cadence: PRs every 30 s and main every 60 s via `ls-remote`, or
  other values?
- Q3. Should a PR the workspace stores as a link (every PR it creates) be
  subscribed automatically, or only on an explicit `subscribe-pr`?
- Q4. Does a subscription end when its PR merges or closes, or keep
  following post-merge comments?
- Q5. A failed fetch (network, auth) for main sync: log only, or also tell the
  main workspace?
- Q6. Should the sidebar show the sync's outcome (`frontend.v1.RosterRepoSync`)
  or is the existing divergence count enough?
- Q7. Approve a read-only spike against a real PR to verify the timeline's
  coverage and ETag behavior? It calls GitHub with the user's token and posts
  nothing.
- Q8. Report edits and deletions of comments already told?
- Q9. Which comment authors wake the agent: everyone, only
  `OWNER`/`MEMBER`/`COLLABORATOR`, or a configured list?
- Q10. The agent's own replies share the user's login. Options: (a) the
  daemon gives each workspace a marker it appends to what it posts and drops
  items carrying it; (b) items by the authenticated user are dropped, so the
  user's own comments never wake the agent; (c) the agent posts through an
  `agent-repld` verb that records the comment id it created. (c) is the only
  one that is exact; (a) can be stripped by an edit; (b) loses the user's own
  requests.
- Q11. Which `gh` login reads `ChessCom` repositories on this machine, and
  should the owner-to-login map live in an env var like
  `AGENT_REPL_GITHUB_OWNERS` or in `.agent-repl/` repository config?
- Q12. When the main workspace is closed at sync failure, open it to deliver
  the prompt, or hold the notice until it opens?

## 9. Owner rulings (2026-10-30)

- R1 (Q1, Q2). Polling latency is acceptable; no relay. PRs every 30 s, main
  every 60 s via `ls-remote`.
- R2. The daemon, not the agent, collects events and delivers them under the
  hood. Q10 stays open: it concerns the agent's own replies on the PR being
  read back as events.
- R3 (Q3, Q4). Every workspace ALWAYS subscribes to every PR it creates, by
  standing harness policy. The daemon detects the merge in its poll and
  unsubscribes the workspace itself. Workspaces never unsubscribe as policy;
  the API still supports it.
- R4 (Q5). Main sync is the daemon's alone. A workspace is prompted only when
  the fast-forward fails in the index or worktree (conflicts, divergence,
  local changes in the way). A fetch failure from connectivity is logged and
  told to no workspace.
- R5 (Q11). `dwcoates` reads every repository, `ChessCom`'s included.
- R6 (Q9, Q10). Comments from anyone on the PR wake the agent, the user's
  own included. The agent's replies go through a daemon verb that records
  their ids (4.7).
- R7 (Q3). The daemon subscribes by itself, from a link the workspace stores
  as OWNED, distinct from a link that is only referenced (4.6).
- R8. A PR closed without merging ends its subscriptions too.
- R9 (Q12). A closed main workspace is reopened to deliver a sync failure.
- R10 (Q7). The read-only spike was approved and run (section 10).

## 10. Spike results (2026-10-30, read-only, `dwcoates`)

Against `ChessCom/explanation-engine`:

- ETag works: a repeat `GET .../issues/144/timeline` with `If-None-Match`
  answered `304 Not Modified`, and `X-RateLimit-Remaining` stayed 4990, so
  unchanged polls are free.
- The timeline is incomplete for this purpose: PR 144 has 53 review comments
  and 34 reviews, but its timeline has 0 `line-commented` events and only 2
  `reviewed` events. The timeline is therefore dropped (4.3).
- `GET .../pulls/comments?sort=updated&direction=desc` lists line comments
  across all PRs of the repository (PRs 8023 and 8047 seen), so one read per
  repository covers them.
- On PRs 8023 and 8047, `updated_at` was later than the latest review and
  line comment. That is consistent with comments moving it but proves
  nothing, so it is not used as a gate.
