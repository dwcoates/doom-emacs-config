# Workspace mentions in response trees

The owner asked (2026-11-01) that every open workspace's name, wherever a
response's TLDR tree mentions it in the webapp feed, be a link whose click
switches to that workspace.

## Assumptions (autonomous design)

- **Objective**: a workspace mention in a response tree is a link that selects
  the mentioned workspace.
- **What is built**: a daemon-served set of mention targets per page, and a
  webapp pass that links whole-name occurrences of them in drawn trees.
- **Frontend**: yes. The mention is an inline link inside an existing tree
  line, drawn in the response bubble's link style; no new layout box.
- **Systems**: proto, daemon (serving), webapp (drawing and clicking). Emacs is
  untouched: a click is the existing `SelectWorkspace`.
- **Non-additive changes**: none. Two arms and one rpc are added.

## Iteration sequence

Default order, walked alone: transport (a standing `Watch*` rpc that a page
subscribes to over its one `WatchPage` stream, like every other page view),
then the endpoint, then its view message.

## Landed changes

### `WatchWorkspaceMentions` and `frontend.v1.WorkspaceMentionTargets`

- **What**: a new rpc and page-mux arm pair (`SubscribePageRequest.workspace_mentions`,
  `PageFrame.workspace_mentions`) serving `frontend.v1.WorkspaceMentionTargets`:
  each target a name and the `workspace.v1.WorkspaceRef` a click selects.
- **Why a daemon-served set and not the page's own roster**: in production the
  page holds no roster. The sidebar is a separate web view
  (`webapp/src/host/sidebar-page.ts`) and every workspace page is an iframe at
  its own origin inside the host, so the roster would have to be relayed
  sidebar → BroadcastChannel → host → postMessage → page with a replay on every
  hello. Subscribing each page to the full roster instead would push the whole
  roster to every page on every change. A small resolved set on the page's
  existing stream has one author and no relay to order.
- **Why the set is resolved by the daemon**: the client must not decide which
  word names a workspace. The daemon drops the viewer's own workspace (the
  request's `viewer`), hidden workspaces (a switch would select a workspace the
  sidebar does not show), and any name two open workspaces share (names collide
  across repositories, per `frontend.v1.RosterRow.workspace`'s comment; a
  shared name names no one workspace).
- **Why a push only on change**: the roster topic publishes on every row change
  (attention, when-column, viewed), and nearly none of those change the set. The
  daemon derives the set from each roster and sends it only when it differs
  from the last one sent on that stream, so a page re-links its trees only when
  a link actually changes.
- **Consequences**:
  - The set is derived from the roster view (`frontend.v1.WorkspaceRoster`), so
    it shows exactly the names and order the sidebar's Open section shows.
  - A response written while a workspace was open loses its link once that
    workspace closes, and gains links for workspaces opened later: the page
    re-links every drawn tree on each push.
