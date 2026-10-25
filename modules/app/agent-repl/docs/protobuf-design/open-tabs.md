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
   stood down when hidden. Amended 2026-10-13 (hidden-cache-lapse.md): a
   hidden workspace's session is stood down when its prompt cache lapses,
   not at the hide, so it can be shown again warm until then. Amended
   2026-10-17: the workspace the user is looking at (the selection) runs even
   hidden or collapsed (`wsm.Workspace.Runs`), and keeps its cache warm
   (`wsm.Workspace.KeepaliveHeld`). Selecting one revives it, and the old
   rule then stood that session down the moment it came up, so its cold gate
   stood again after every answer. Moving off it withholds it again.
   Amended 2026-10-25 (owner ruling, verbatim): "folding a repository should
   have no effect on workspaces with a turn active or background work in
   flight, or in the merge queue, either. it should hide them from
   visiblity, but not change their statuses". A fold hides every workspace
   it holds, and spares the session of a one-shot or of a workspace with a
   merge pending (`wsm.Workspace.FoldSpare`); see "Hide and collapse stand
   sessions down".
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

### OpenTabsWorkspace.page: loading or none
- WHAT: each requested workspace says whether the editor loads a web page
  for it; `none` (a frontend other than the web page, or an editor with no
  webview) opens once served, `loading` once served and drawn.
- WHY: without it the daemon would wait forever on a page that never comes.

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
- AMENDED 2026-10-13 for the hide (hidden-cache-lapse.md): the hide holds the
  session's keep-alives and the stand-down below happens when its cache lapses.
- WHAT: SetWorkspaceHidden(hide) and a repository collapse stand the session
  down as the idle sweep does (hibernate, conversation kept), deferred while a
  turn runs and taken once it ends (owner delegated the timing; recorded as
  the agent's decision: never interrupt a turn).
- WHY: principle 1.
- AMENDED 2026-10-25 for the collapse (owner ruling, quoted under principle
  1): a fold HIDES; it never changes a workspace's status.
  - A fold never stands down a workspace its own record spares
    (`wsm.Workspace.FoldSpare`): a one-shot, or one with a merge queue entry
    standing (requested, waiting or running, its deploy hooks included). A
    hidden or closed workspace is withheld by that act, not a fold, and is
    spared nothing.
  - A workspace with a turn or detached work in flight is held back by the
    stand-down gate, as before, and nothing reaches its turn or its session.
  - THE IDLE-LATER CASE, decided by structure: a workspace held back while
    busy is asked again on the free edge its last piece of work raises
    (`lifecycleSink.OnFree`). Each attempt re-reads the record, so one that
    became a one-shot or queued a merge meanwhile is spared; one that is now
    idle, with nothing in flight and nothing queued, is stood down, exactly
    as a workspace idle at the fold is. Why: the ruling protects work in
    flight from the fold; a folded workspace with no work left is the idle
    folded workspace principle 1 already stands down, and no timer decides it.
  - A spared one-shot or merge-pending workspace is not asked again: it keeps
    running until it is closed (its cache lapse then stands it down) or the
    section is folded again.
  - Every fold records who asked (the rpc and the client's request id), what
    it hid, what it spared with the reason, and what is standing down
    (`daemon.workspace.fold_repository`, `daemon.workspace.fold_subsection`).

## Decisions taken for the owner (delegated 2026-10-12, to confirm)
- Collapsed repository sections count as not visible, like hidden ones.
- A workspace whose session fails to start still opens, showing its fault.
- Hiding with a turn in flight stands the session down once the turn ends.

## Landed change: `opened` means everything is done (owner, 2026-10-12)

- WHAT: `ready` is sent only once the workspace's session is served, its
  conversation's newest page was read whole (step `conversation_read`), any
  turn the daemon's own stand-down cut was resumed (step `resuming_cut_turn`;
  started again, not finished), and its page drew when one was loading.
  `ready` and `failed` carry the workspace's `RosterRow` as of the answer.
- WHY: "everything specific to the workspace should be done by the time the
  daemon acks the workspace open", and the tab's status lands with the ack.
- CONSEQUENCE: the cut-turn resume moves from "whenever the next session
  serves" to inside the open, so a workspace is never shown before its cut
  work restarted.

### Roster revision orders the answer's row against the roster
- WHAT: `WorkspaceRoster.revision` rises with every resolved roster; `ready`
  and `failed` carry the `roster_revision` their row was taken from. A
  consumer applies the answer's row only when it is newer than the roster it
  holds.
- WHY: the answer and the roster ride separate streams, and a row applied
  out of order would roll a status back.
