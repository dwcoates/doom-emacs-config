# Open tabs: the editor asks, the daemon opens, the editor shows

## Problem

At startup every open workspace's session was brought up at once, hidden ones
included (48 concurrent vendor starts on 2026-10-12, 50s of retries), go-aheads
were sent strictly in registry order (one slow workspace held every tab behind
it), the startup was implicit in an editor connecting, and the readiness of a
workspace's web page was checked by Emacs alone, never by the daemon.

## Core design principles (owner, 2026-10-12)

1. ONLY OPEN AND VISIBLE WORKSPACES RUN. The daemon restores and starts the
   sessions of open, visible workspaces only. A hidden workspace (or one in a
   collapsed repository section) runs nothing; it is started when shown and
   stood down when hidden.
   - Consequence: boot's bring-up and every open skip invisible workspaces;
     hide and collapse stand sessions down; show and expand start nothing by
     themselves, the editor asks with OpenTabs.
   - Reopens: the 2026-09-13 / 2026-10-02 ruling "an open workspace is never
     session-less" now reads "an open VISIBLE workspace".
   - Does not claim: that a hidden workspace's registry entry, worktree or
     conversation is dropped. They are kept.
2. THE EDITOR SHOWS A TAB ONLY WHEN THE DAEMON SAYS THE WORKSPACE IS OPEN.
   Open means served by agent-repl's services AND its web page drawn. The
   editor asks explicitly (OpenTabs), loads the page at the moment it asks,
   and is told every step as a typed frame, which it prints in the minibuffer.
3. TABS APPEAR AS THEIR WORKSPACES OPEN, PLACED IN REGISTRY ORDER. No
   workspace waits on another.
4. A DAEMON-ONLY RESTART NEVER CLOSES A TAB. Hot reloads keep every open tab
   standing while sessions and pages reconnect beneath them.
5. A WORKSPACE BEING OPENED IS NOT SELECTABLE. Its sidebar row is grey
   (`opening`) and a click does nothing until it is open.

## Landed changes

### OpenTabs (endpoint_open_tabs.proto), server stream
- WHAT: the editor names workspaces; the daemon answers per-workspace typed
  steps (starting_session, waking, resuming, vendor_retrying, vendor_rejected,
  vendor_failed, cold_gate, offline, served, page_connected, page_drawn), one
  `opened` per workspace (ready, failed, hidden, refused), then `finished`.
- WHY: principles 1-3.
- CONSEQUENCES:
  - `opened` is sent as each workspace opens, not in registry order; the
    editor places tabs by the roster's tab order.
  - A service-level start failure still opens (arm `failed`): the tab shows
    the fault (owner delegated this ruling; recorded as the agent's decision).
  - Vendor and network state never gate `opened` (unchanged from the go-ahead).
  - A hidden or collapsed workspace is answered `hidden` at once.

### ReportPageDrawn (endpoint_report_page_drawn.proto), unary
- WHAT: the page reports its conversation drawn, and again on every page
  stream re-attach while drawn; the fact lives with the page connection.
- WHY: principle 2 needs the daemon to know the page drew. Re-reporting on
  re-attach is what lets a daemon that restarted mid-open learn of a page that
  drew before it came up.

### WatchDaemon.startup (field 10) retired
- WHAT: the implicit startup run and its DaemonStartup* messages are gone.
- WHY: OpenTabs replaces it; the open is asked for, not implied by a
  connection.

### RosterRow.status `opening` (field 60)
- WHAT: grey, not selectable; drawn only while an OpenTabs names the
  workspace and has not answered it `opened`.
- WHY: principle 5. Keyed on an open in flight (not "not acked since boot") so
  a daemon restart under open tabs never greys them (principle 4).

### Hide and collapse stand sessions down
- WHAT: SetWorkspaceHidden(hide) and a repository collapse stand the session
  down as the idle sweep does (hibernate, conversation kept), deferred while a
  turn runs and taken once it ends (owner delegated the timing; recorded as
  the agent's decision: never interrupt a turn).
- WHY: principle 1.

## Decisions taken for the owner (delegated 2026-10-12, to confirm)
- Collapsed repository sections count as not visible, like hidden ones.
- A workspace whose session fails to start still opens, showing its fault.
- Hiding with a turn in flight stands the session down once the turn ends.
