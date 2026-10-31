# Three windows: the sidebar in a full-height window of its own

Status: plan, not started (2026-10-31).

## Today: two windows

Emacs lays out two windows per workspace (`lisp/frontend.el`
`agent-repl--frontend-display-webview`): the webview window filling the main
area, and the composer (input) window split below it. The webview window shows
the ONE host web view (`lisp/webhost.el`, `webapp/src/host/host.ts`), which
holds every workspace page as an iframe and draws the workspaces sidebar
ONCE, over the pages, from its own daemon connection
(`webapp/src/host/sidebar.ts`). Each page lays out an empty sidebar column
under it (`webapp/src/host/embed.ts` `WEBHOST_SIDEBAR_CLASS`). The sidebar
ends above the composer (or above a docked cold gate's band).

```text
+--------------------------------------------------------------+
| webview window (host xwidget)                                |
| +-----------+------------------------------------------------+
| | sidebar   | topbar                                         |
| | (host     +------------------------------------------------+
| |  page,    |                                                |
| |  floats   | feed (shown workspace's iframe)                |
| |  over the |                                                |
| |  page's   |                                                |
| |  empty    +------------------------------------------------+
| |  column)  | footer                                         |
+-+-----------+------------------------------------------------+
| composer window (input buffer, full frame width)             |
+--------------------------------------------------------------+
```

## Proposed: three windows

```text
+-------------+------------------------------------------------+
| sidebar     | feed window (host xwidget)                     |
| window      | +----------------------------------------------+
| (its own    | | topbar                                       |
|  xwidget,   | +----------------------------------------------+
|  sidebar    | |                                              |
|  page,      | | feed (shown workspace's iframe)              |
|  full frame | |                                              |
|  height)    | +----------------------------------------------+
|             | | footer                                       |
|             +------------------------------------------------+
|             | composer window (input buffer)                 |
+-------------+------------------------------------------------+
```

## The constraint that shapes the design

One xwidget cannot show in two windows on macOS. Emacs's NS port has ONE
`xwWindow` per xwidget and re-parents it into whichever window displayed it
last (`~/src/emacs-agent-repl-patched/src/nsxwidget.m:579`,
`[xv->xvWindow addSubview:xw->xwWindow]`). So the sidebar window needs a
SECOND web view loading its own sidebar page. It is always on screen, so
WebKit never suspends it. It costs one more WebKit content process.

## Plan

### Emacs side (lisp)

1. A new `lisp/sidebar-window.el` owns one GLOBAL sidebar buffer and its
   xwidget, modeled on `lisp/webhost.el` (hidden buffer, killed only by its
   owner, reload onto a deploy's build, follow a daemon handover).
2. The sidebar window is a LEFT SIDE WINDOW
   (`display-buffer-in-side-window`, `side left`). Recommended because:
   - A side window spans the whole frame height by construction, which is
     the requirement.
   - It is global, not per-workspace, matching the sidebar, which is the same
     for every workspace.
   - The per-workspace panel PAIR (view + input) stays a pair in the main
     area, so `agent-repl-window--ensure-layout`, the mount recipe, persp
     save/restore and `--delete-where` (which already skips side windows) keep
     their invariant. The alternative (a third per-workspace window in the
     main area) turns every one of those into a triple and fights
     `delete-other-windows`.
   - It collides with treemacs, also a left side window; give the sidebar
     `slot -1` so the two stack, or decide the sidebar replaces treemacs while
     panels show (an owner decision).
3. Width is the window's width, fixed (`window-size-fixed 'width`,
   `window-preserved-size`), resizable by dragging the divider, persisted as a
   setting (`lisp/settings.el`).
4. Shown whenever any workspace's panels are shown, hidden with them
   (`SPC o c`). `ensure-layout` gains one check: panels shown implies the
   sidebar side window is live.
5. A switch tells BOTH web views: the host which iframe to show (as today),
   the sidebar which row is shown (`markShown`, today a same-document call
   from `host.onShown`). Both calls go out in the same Emacs command, so
   the switch latency target (low tens of ms) holds.
6. Keyboard focus: clicking the sidebar selects its Emacs window. Every
   "focus the input" path (`agent-repl-window-focus-input-when-panels-alone`,
   the row click's `SelectInEditor`) must move focus back to the composer.

### Webapp side

1. A sidebar page (`?sidebar=1`, a new entry beside `host/main.ts`) runs what
   `startWebhostSidebar` runs today: logger, failures, census, daemon context,
   `mountWebhostSidebar`.
2. The host page KEEPS a daemon connection, because the cold gate
   (`host/gate.ts`) and the load button (`host/load-button.ts`) are started
   from the sidebar's start today and read its context. They split out into
   the host's own start.
3. Pages drop the reserved column: `WEBHOST_SIDEBAR_CLASS`, the
   `sidebarDock` message, `host.setSidebarDocked`, and the host's ending of
   the sidebar above the gate band all retire, since the sidebar no longer
   sits inside the host.
4. The sidebar's failures (today `host.setFailures`, into every page's warning
   chip) need a new route: the sidebar page has no pages. Either the sidebar
   draws its own chip, or Emacs relays its failures to the host. Owner call.
5. Popups clip at the sidebar window's edge. The name popup
   (`sidebar/name-popup.ts:48`), dropdowns, row-hover cards, tooltips and the
   merge-queue view all position inside the document, and today can extend
   over the feed. In their own web view they stop at the divider. Each needs
   a re-anchor that fits inside the rail.
6. The floating look (`--sidebar-shadow` over the feed) has nothing to float
   over. A look change, so the owner rules on its replacement.

### Contract (proto)

- `agentrepl.v1.ClientLogRequest.source` gains a `sidebar` arm
  (`proto/src/agentrepl/v1/endpoint_client_log.proto`), so the daemon files
  the sidebar's logs apart from the host's.
- `agentrepl.v1.WatchPageRequest.webhost`
  (`endpoint_watch_page.proto:69`) gains a sibling for the sidebar page, so
  the daemon counts both pages and reloads both on a build change
  (`agentrepl.v1.DaemonReloadWebhost` gets a sidebar counterpart, or names
  which page).

### Tests

- ERT: the side-window mount, the hide/show with the panels, the width
  setting, the switch telling both web views, the handover moving both
  (`test-window.el`, `test-webhost.el`, a new `test-sidebar-window.el`).
- vitest: the sidebar page's start, the host's start without a sidebar, the
  page without the reserved column.
- Daemon: the new source and page arms.
- e2e/realtest screenshots of the layout change.

## Assessment of the lift

Moderate to large. No single part is hard, but the change cuts across Emacs,
the webapp and the daemon, and undoes a deliberate consolidation (the
2026-10-13 "draw the sidebar once, in the host" work).

| Area | Lift | Why |
|---|---|---|
| Emacs window layout | Medium | Side window keeps the panel-pair invariant, so mostly additive; treemacs and focus are the risks. |
| Second web view lifecycle | Medium | A copy of `webhost.el`'s lifecycle (create, reload, handover, kill guard); extract the shared parts rather than duplicate. |
| Webapp split | Medium | Moving the start is mechanical; splitting the gate and load button off the sidebar's context is the real work. |
| Popups and look | Medium, owner-gated | Every popup that crossed into the feed must fit the rail; the floating dock's look needs a ruling. |
| Proto and daemon | Small | Two arms and their handling. |
| Cost at runtime | Small | One more WebKit content process; no suspension, since it is on screen. |

Gains: the sidebar runs the full frame height, is unaffected by the composer
or a gate, can show before any workspace page loads, and its redraws no
longer run in the process drawing the feed.

## Decisions for the owner

1. Sidebar and treemacs together: stack them in the left side, or the sidebar
   replaces treemacs while panels show.
2. Where the sidebar's failures show once it leaves the host.
3. The sidebar's look without the floating shadow over the feed.
