# FindPullRequestOwner: shift-click a pull request link to switch to its workspace

A shift-click on a link to a pull request in a response bubble should not open
the link in the browser, as a plain click does. It should select the workspace
that owns that pull request as its canonical owned link.

Designed autonomously (the default mode); every assumption below was derived
from the code and is stated as such.

## Assumptions

- **Objective.** One gesture in the feed reaches the workspace that made a
  pull request from any link to it.
- **Systems.** `agentrepl.v1` (one new unary endpoint), the daemon (the store
  query and the handler), and the webapp (the prose link router). Emacs is
  untouched: the switch is the feed's existing jump (`SelectInEditor`).
- **No non-additive change.** Every existing message and endpoint is kept.
- **"Owns" means the canonical owned link.** A workspace owns a pull request
  when it stored the pull request's link owned
  (`agentrepl.v1.WorkspaceLinkOwned`), which is the same set
  `wsm.OwnedPullRequestLinks` names. Evidence tier: existing code.

## Iteration sequence

Protocol (Connect, as every endpoint) → inventory (one endpoint,
`FindPullRequestOwner`, in the WEB LINK section beside `OpenExternal` and
`SelectInEditor`) → conventions (the package's `oneof result { success; ... }`)
→ the endpoint's shape.

## Landed changes

### FindPullRequestOwner

- **What.** `endpoint_find_pull_request_owner.proto` and its rpc in
  `service.proto`. The request carries the clicked url. The success arm says
  `owned` (with the workspace and an `open`/`closed` standing), `unowned`, or
  `not_pull_request`.
- **Why a daemon lookup rather than a pushed set.** The page could have been
  served each mention target's pull requests on `WatchWorkspaceMentions`, but
  that set leaves out the viewer's own workspace and every workspace whose name
  is ambiguous, neither of which bears on who owns a pull request. A lookup at
  click time reads the store at the moment of the click, so the answer is never
  a stale copy.
- **Why the daemon says `not_pull_request`.** Whether a url names a GitHub pull
  request is decided by `pullrefs.ParseURL` in the daemon. A page-side regular
  expression would be a second spelling of that rule. On `not_pull_request` the
  page opens the link as a plain click does, so shift keeps no meaning on other
  links.
- **Which workspace answers.** Among open workspaces, the one holding the pull
  request's owned-link subscription, which the store's partial unique index
  (`pull_request_subscriptions_one_owner`, `wsm/pullrequestowner.go`) admits
  for one workspace only, so two open answers are unrepresentable. A close
  deletes the subscription but keeps the owned link, so when no open workspace
  owns the pull request the most recently closed workspace that stored it owned
  answers, and the click reopens it. This mirrors a workspace mention's closed
  target, which also reopens.
- **Consequences.** An open workspace whose owned link lost its subscription by
  some other path is not found as an owner; the one-owner rule already treats
  the subscription as the ownership. Store read failures are Connect Internal,
  not an arm, as `LocateWorkspace` does.
- **No response error arm.** Every domain answer is a success arm; there is no
  refusal the daemon makes. The request carries no workspace, since the answer
  is the same whichever page asks.
