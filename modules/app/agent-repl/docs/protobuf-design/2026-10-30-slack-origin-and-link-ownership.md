# Slack origin and link ownership (2026-10-30)

A workspace created for a Slack message (the `:agent-ack:` and
`:oneshot-ack:` reactions) is spoken for in that thread by the daemon, not by
its agent. The daemon posts twice: once the workspace is created, a one-line
reply that work began; once a finish's deploy ends, a reply saying whether it
landed or failed, with every link the workspace owns as a bullet. Only
`:sockets-ack:` keeps a `gns-sockets` subscription.

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
- The end post waits for the finish's DEPLOY to end, not for the
  `FinishWorkspace` call: a repository that lands through a pull request opens
  it during the deploy, and the post's point is to show what the work made.
  An abandoned merge posts nothing.
- A post that fails never fails the create or the finish: it is recorded under
  `daemon.workspace.slack_origin` and drawn as the transient `slack_post_failed`
  footer line.

## Where it lives

- `daemon/internal/slackfmt`: the port of send-to-slack's `format_message.py`.
- `daemon/internal/slackpost`: thread link parsing and the `gns slack send`
  poster.
- `daemon/internal/workspace/slackorigin.go`: the two posts.
- `daemon/internal/merge` `Deps.Ends`: the merge-end observer the end post
  hangs off.
