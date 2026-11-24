# Viewed and recency from user interaction

A workspace is viewed or not viewed, and only the user's own interaction with
it marks it viewed: scrolling its feed, typing in its composer, moving through
its feed with the editor's keys, or following a link in its page. Every status
change makes it not viewed again, on every status. No dwell timer decides it
any more. The same interaction is what the recency walk orders workspaces by,
so the editor reads recency from the daemon instead of keeping its own
dwell-filtered history.

## Assumptions (autonomous design)

- OBJECTIVE: viewed is set only by a registered user interaction and reset by
  every status change; recency comes from the daemon.
- WHAT IS BUILT: one interaction report the editor and the page send, the
  daemon's viewed fact and recency instant derived from it, and the removal
  of every dwell.
- FRONTEND: yes. The sidebar row and the Emacs tab bar draw viewed; no new UI
  element is added, one is removed (the view-dwell fade).
- SYSTEMS: daemon (resolver, wsm, server), Emacs (status, host, input,
  workspace, roster, wire codecs), webapp (sidebar rows, feed scroll owner).
- NON-ADDITIVE: `agentrepl.v1.MarkWorkspaceViewed` and
  `agentrepl.v1.ReportViewDwell` are deleted; `frontend.v1.RosterRowViewDwell`
  and `frontend.v1.RosterRow.view_dwell` are deleted;
  `agentrepl.v1.HostWorkspaceLinkFollowed` is deleted, because the daemon now
  records a followed link as an interaction itself; the durable
  `workspaces.result_read` column is retired.
- Clicking a link in a page and moving through the feed with Emacs keys count
  as interaction (the user's own acts on the feed). A completed `/clear`,
  compaction or interrupt no longer marks the row viewed by itself: the status
  change that ends it resets viewed like any other.

## Iteration sequence

Default sequence, walked autonomously: transport (Connect, unchanged) ->
endpoint inventory (one new rpc, two deleted) -> conventions (the canonical
`oneof result { success; error; }`) -> the one new endpoint's shapes -> the
roster row's markers.

## Core design principles

- ONE FACT, TWO READINGS. The interaction report is the only input. The
  daemon derives viewed (the row's status at the last interaction is still
  the row's status) and recency (when the last interaction was) from it. No
  client derives either.
  - Consequence: the report carries no viewed verdict and no instant; the
    daemon stamps both.
  - It does NOT claim that selection is interaction: switching to a workspace
    marks nothing.

## Landed changes

### `agentrepl.v1.ReportUserInteraction` replaces the two viewed verbs

- WHAT: a new rpc and endpoint file; `MarkWorkspaceViewed` and
  `ReportViewDwell` are deleted with their endpoint files.
- WHY: the user wants viewed set only by real interaction, with no time
  limit, and one source of truth for recency.
- CONSEQUENCES: Emacs's whole dwell machinery (status.el) and its visit
  history (workspace.el) go; the page gains a reporter on the feed scroll
  owner's user-input edge (`webapp/src/scroll.ts` `onInput`), which already
  separates the reader's own input from programmatic scrolls.
- The request names the kind of interaction for the record only; every kind
  has the same effect. A `oneof` of empty arms is kept so the record names
  it and a later kind lands as an arm.

### `agentrepl.v1.HostWorkspaceLinkFollowed` is deleted

- WHAT: the host-stream event and its field (23, reserved).
- WHY: the daemon receives every followed link itself (OpenExternal,
  OpenInEditor, SelectInEditor's `followed_from`), so it records the
  interaction there rather than telling Emacs to report it back.

### `frontend.v1.RosterRowViewed` stands on any status

- WHAT: the contract text changes; the message shape does not. The marker is
  present when the user interacted with the workspace since its status last
  changed, on any arm.
- WHY: the user ruled viewed resets on every status change and every status
  can be viewed. A running workspace stays VISIBLE because it is working
  (`frontend.v1.RosterRowVisibility`), so viewing it only greys its name.
- The visibility rule's "unviewed" clause keeps its meaning: a turn-end row
  not viewed.

### `frontend.v1.RosterRowViewDwell` and `RosterRow.view_dwell` are deleted

- WHAT: the message, the field (75, reserved).
- WHY: no dwell exists any more.

### `frontend.v1.RosterRowLastInteracted` is added

- WHAT: `RosterRow.last_interacted`, the instant of the user's latest
  interaction with the workspace, durable.
- WHY: the recency walk and the landing after a close order by it, read from
  the daemon.
- `frontend.v1.RosterRowLastSelected` stays: the switcher orders by
  selection by owner request (2026-11-11), which is a different order.

### Durable record (not proto)

- `workspaces.viewed_status` holds the status arm the user last interacted
  on, cleared on every status change, so a restart draws the row as it
  stood. `workspaces.last_interacted_at` holds the recency instant.
  `workspaces.result_read` is retired by a contract step.
