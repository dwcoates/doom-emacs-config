# Pull request badges on the summary's link menu

The topbar summary's link menu should say, for each GitHub pull request it
links, whether that pull request is open, a draft, merged or closed, as a
small colored badge in the row's right margin. The daemon learns the state
through the pull-request watcher it already runs, keeps it durably, and
backfills the links that existed before.

Design mode: autonomous (the owner asked for the result, not to co-design).

## Iteration sequence

One increment: the menu row's badge in `frontend.v1` (the transport, the
endpoint and the response conventions are the existing `WatchTopbar` stream's,
unchanged).

## Landed changes

### `frontend.v1.TopbarTitleLinkMenuItem.badge`, a oneof of the four states

- WHAT: the menu row gains an optional `frontend.v1.TopbarTitleLinkBadge`,
  whose `state` oneof has one empty arm per state the badge draws: open,
  draft, merged, closed. Unset means the row draws no badge: the url names no
  pull request, or the daemon has not read it yet. Set with no arm is a state
  this revision does not name, and also draws none.
- WHY: the owner wants closed, merged and open pull requests told apart at a
  glance in the menu ("green merged, white open, red closed").
- Shape reasoning: a state is a oneof of dedicated messages, never an enum
  (`frontend.v1.TopbarTitleLinkIcon` stays an enum because a service icon is
  no state). The badge is its own element message under figma-to-idl, beside
  the row's icon, because it is its own drawn box in the row's margin. The
  arms are the menu's vocabulary, not a copy of
  `agentrepl.v1.PullRequestState`: the badge draws no merge commit, and the
  agent-facing state may grow arms the badge does not distinguish.
- Only the menu row carries it, not `frontend.v1.TopbarTitleLink` (the single
  link a click opens directly): the owner asked for the dropdown, and the
  single-link case draws no row to put a badge in.
- Consequences:
  - The resolver's `linkBadge` (`daemon/internal/resolve/topbar/resolver.go`)
    is the one mapping from the store's `wsm.PullRequestState` to the arm,
    and panics on a state it has no badge for.
  - The client's `drawLinkBadge` (`webapp/src/topbar/link-badge.ts`) draws
    the arm's word and refuses an arm this build cannot draw, as the icon
    does.

## Where the state comes from (no contract change)

- `wsm.pull_request_states` (layout 175) holds one state per pull request,
  whoever links it, apart from the subscriptions: a reference link is
  followed for its state but never told about the pull request's activity.
- The watcher records the state on every successful subscription read, and
  reads the state alone (one conditional GET of the pull request) for every
  other pull request an open workspace's link names, skipping merged ones,
  which never change again. Closed ones stay read, since a close can be
  undone.
- A changed state redraws every open workspace linking it
  (`prwatch.Deps.LinksRedrawn`, `workspace.Verbs.RepublishLinks`).
- Backfill: the layout-175 rewrite fills the pull request columns of links
  stored before layout 96 (22 on the owner's store) from their urls, and
  seeds the merged or closed state of every pull request an ended
  subscription saw end. Every other linked pull request is read on the first
  tick after the deploy.
