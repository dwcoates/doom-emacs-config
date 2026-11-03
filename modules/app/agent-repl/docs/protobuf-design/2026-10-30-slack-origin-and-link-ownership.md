# Slack origin and link ownership (2026-10-30)

A workspace created for a Slack message (the `:agent-ack:` and
`:oneshot-ack:` reactions) is spoken for in that thread by the daemon, not by
its agent. Only `:sockets-ack:` keeps a `gns-sockets` subscription, so for
the other two the daemon's posts are the thread's only word on the work.

Revised 2026-11-03 (see "The posts, revised" below): the fixed "has begun"
opening and the end-of-merge "done" post are gone. The daemon now posts the
workspace's first summary as the opening, every owned link as it is stored,
a `:merged:` reaction when an owned pull request merges, and a post only
when a finish's deploy fails.

## Landed decisions

- `agentrepl.v1.CreateWorkspaceRequest.slack_origin`
  (`agentrepl.v1.CreateWorkspaceSlackOrigin.thread_url`) names the message.
  - A link that is not an https `<workspace>.slack.com/archives/<channel>/p<ts>`
    message link is Connect InvalidArgument; so is the field beside
    `async_merge`.
  - The daemon records it (`workspace_slack_origins`, layout 93) and stores it
    among the links as a reference labelled "Originating Slack thread".
- `agentrepl.v1.WorkspaceLink.ownership` and
  `agentrepl.v1.ChangeWorkspaceLinksSet.ownership`
  (`agentrepl.v1.WorkspaceLinkOwnership`: `owned` or `reference`).
  - REQUIRED on a set, so every link says which it is; a relabel sets it anew.
  - A link stored before ownership existed reads `reference`
    (`workspace_links.owned`, layout 92, default 0).
- SUPERSEDED 2026-11-03: the end post waited for the finish's DEPLOY to end,
  since a repository that lands through a pull request opens it during the
  deploy. A thread so saw no pull request until the work had fully landed,
  and never saw it merge. Only a failed deploy now posts at the end.
- A post or a reaction that fails never fails the create or the finish: it is recorded under
  `daemon.workspace.slack_origin` and drawn as the transient `slack_post_failed`
  footer line.

## The posts, revised (2026-11-03)

No proto change: every post is the daemon's, keyed off facts it already
holds. The store records what was posted (`workspace_slack_origins.opened`
and `workspace_slack_link_posts`, layout 100), so a restart neither posts
twice nor loses the message a reaction goes on.

- THE OPENING is the workspace's one-sentence summary (`internal/titlesynth`),
  posted once that summary first exists. Two edges need it: the thread
  recorded at create, and a summary installed
  (`titlesynth.SummaryObserver`). Each tries it, and the store's atomic claim
  (`wsm` `ClaimSlackOpening`) lets exactly one post, whatever their order. No
  placeholder is posted while the summary is missing. Origins recorded before
  layout 100 had their "has begun" post, so the migration marks them opened.
- EVERY OWNED LINK is posted as `workspace link -owned` stores it, a pull
  request as its label and `#<number>` link. The post is claimed first
  (`ClaimSlackLinkPost`), so a relabel or any set again never reposts it, and
  the posted message's ts is recorded with the link (`RecordSlackLinkPost`).
  A failed post is not retried.
- THE `:merged:` REACTION goes on that message when the pull-request watcher
  ends a subscription merged (`prwatch.MergeObserver`). The merge and the ts
  can land in either order: the merge marks the row (`MarkSlackLinkMerged`)
  and answers the ts when one is recorded, and the ts's record answers
  whether the merge was already marked, so exactly one of them reacts.
- THE END posts only a failed deploy: "Work on this stopped: its deploy
  failed." with where to look (the workspace, left open where it stopped, and
  every owned link). A landing and an abandoned merge post nothing.
- `slackpost` reads the posted message's ts from `gns slack send --output
  json` (an answer without one is an error) and reacts through `gns slack
  react`.

## Where it lives

- `daemon/internal/slackfmt`: the port of send-to-slack's `format_message.py`.
- `daemon/internal/slackpost`: thread link parsing and the `gns slack send`
  poster.
- `daemon/internal/workspace/slackorigin.go`: the posts and the reaction.
- `daemon/internal/wsm/slackorigin.go`: the origin, and the claims and
  records of what was posted.
- `daemon/internal/merge` `Deps.Ends`: the merge-end observer the failed
  deploy's post hangs off.
- `daemon/internal/titlesynth` `Deps.Installed` and `daemon/internal/prwatch`
  `Deps.Merges`: the observers the opening and the reaction hang off.
