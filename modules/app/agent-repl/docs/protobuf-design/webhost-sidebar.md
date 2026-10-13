# The sidebar moves into the webhost

The sidebar is identical in every workspace page, and every page draws its own
copy. A copy in a page that was cold is out of date, so switching to that page
showed a stale sidebar that then redrew (the selected row jumped). Every copy
also redraws on every sidebar update, and the sidebar is 91% of each page's
elements (docs/investigations/2026-10-13-page-cost-by-component.md). The owner
asked (2026-10-13) for the sidebar to be drawn once, in the webhost, the one web
view Emacs shows that holds every workspace page as an iframe.

## Core design principles

- **The sidebar is drawn once, by the webhost; workspace pages draw only their
  own workspace's topbar, feed and footer.** Consequence: everything the
  sidebar needs from the daemon (the roster stream, the sidebar's verbs, its
  logging) is reached from the webhost, which owns no workspace. It does not
  claim anything about warm and cold pages; those stay as they are until the
  owner revisits switching and page caching.

## Landed changes

### `agentrepl.v1.ClientLogRequest.source` (2026-10-13)

- WHAT: the request's `workspace` field moved into a `source` oneof, beside a
  new `webhost` arm (`agentrepl.v1.ClientLogWebhost`, empty). The field number
  of `workspace` is unchanged, so the wire is compatible with every existing
  sender.
- WHY: the webhost draws the sidebar and must log like any page, but it owns
  no workspace, and `ClientLog` filed every record under a workspace.
- CONSEQUENCES:
  - The daemon files a webhost record in its central log
    (`dlog.Surfaces.CentralClientLog`), with `client_source: webhost` in the
    record's context and no workspace fields. `bin/logs.sh --central` reads it.
  - A request with neither arm is refused InvalidArgument
    (`daemon/internal/server/validate.go` `validateClientLogRequest`).
  - The webapp's sink moved out of `main.ts` into
    `webapp/src/rpc/client-log-sink.ts` and takes the source, so the page and
    the webhost share it; the lint rule naming the one sanctioned `ClientLog`
    caller moved with it (`webapp/eslint.config.js`).
  - Every Go `Surfaces` double gained `CentralClientLog`.
- Approved by the owner's standing ruling that contract changes are decided and
  carried through without stopping to ask (2026-10-13).

### The sidebar moves into the webhost (2026-10-13; no proto change beyond the one above)

- WHAT: the webhost draws the one sidebar over every workspace page
  (webapp/src/host/sidebar.ts); a page in the webhost mounts no sidebar. The
  page/webhost messages (webapp/src/host/protocol.ts, not a proto) gained
  `sidebarDock` and `webhostFailures` (webhost to page) and `gateBand` (page to
  webhost).
- CONSEQUENCES:
  - The webapp's context splits: `DaemonContext` (no workspace) under
    `AppContext`; the sidebar takes a `DaemonContext` and a parent-workspace
    provider (the roster's `current` in the webhost).
  - A page's "switched to" edge is the webhost showing it, not the roster's
    `current`; a page's seen state counts the webhost showing it.
  - The webhost follows a handover to the new daemon by reloading there
    (lisp/webhost.el); every page of a handover reloads anyway.
  - Cold pages still stand their topbar and footer down; their staleness on a
    switch is the owner's to revisit with switching and page caching.
