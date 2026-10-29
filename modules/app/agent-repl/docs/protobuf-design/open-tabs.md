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
   - Consequence: every open skips invisible workspaces (a boot's bring-up
     follows only the record of which shims were up, owner ruling
     2026-10-28: see shim-lifecycle.md);
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

### A fold drops no page (owner ruling, 2026-10-26)
- WHAT: folding a repository section or a subsection takes its workspaces'
  tabs off the Emacs bar and drops no web page; only the user's hide does
  (`agent-repl-open-tabs-withheld-p`). A folded workspace's page is held
  like a drawn one's, by its status (page-unload.el), which the fold and the
  unfold never touch.
- Does not claim: anything about the daemon's sessions. A repository fold
  still stands sessions down as above.

### A page is held by its workspace's status (owner ruling, 2026-10-29)
- WHAT: a tab opens with a page only when it is on screen or its workspace
  is working (`agent-repl-page-unload-wanted-p`); every other tab opens with
  `page: none`, so its open answers once its conversation is read. Its page
  loads the instant the user switches to it, and is unloaded the instant the
  user switches away unless its status holds it: working always; idle for an
  hour past its last prompt, unless it was loaded only for that visit;
  merging, faulted, cold-gated, transitional and merged never off screen.
  A workspace that starts working while unloaded loads its page in the
  background.
- WHY: every page lives in one WebKit web content process, whose memory-
  pressure collections every 30s stalled switches by up to 1.4s with 42
  pages held (2026-10-28).
- Does not claim: anything about the shim, which runs by its own rules
  (`docs/developer/shim-lifecycle.md`).

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

## Landed change: an open never waits on a shim, and starts none (owner, 2026-10-28)

- WHAT: `ready` is sent once the workspace's conversation's newest page is read
  whole from the store (`conversation_read`) and its page drew when one was
  loading. The open starts no session and waits on none: no `served`, no cut
  turn's resumption. Steps of a bring-up some other cause has under way (a
  boot's, a prompt's) and a resumption's start are relayed as progress.
  `failed` now means the conversation could not be read.
- WHY: "No UX ever waits on a shim"; the daemon reads history from the store
  (2026-10-27-store-read-history.md), so a shim is not needed to draw a
  workspace. A prompt brings the shim up and is held "after loading" until
  it is.
- REPLACES: the 2026-10-12 change below as far as it waited on the session
  and the cut turn, and principle 2's "served by agent-repl's services".
  A cut turn is resumed by the session-up release of whatever next brings
  the session up.
- Does not change: the message shapes. Only their comments and the
  daemon's behavior changed.
