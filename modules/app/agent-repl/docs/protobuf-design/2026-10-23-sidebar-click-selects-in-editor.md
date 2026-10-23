# A sidebar click switches as fast as a key

The owner asked (2026-10-23) that switching workspaces by clicking a sidebar
row be exactly as fast as switching between the same two workspaces by `M-{`
or `M-<number>`.

## The cause, from the logs

A key switch is Emacs's own request (`agent-repl-host-request-switch`): the
frame moves at once, and the daemon's SelectWorkspace and roster push only
confirm it. A sidebar click called SelectWorkspace from the page, so the frame
moved only when the roster push the select provoked reached Emacs. The daemon
re-reads its whole registry before that push (`selectCurrent`'s
`readRegistry`), which took about 300ms with 728 workspaces:

```text
17:17:17.257 webapp sidebar.row.select           selecting a workspace from the rail
17:17:17.261 daemon daemon.workspace.select      selected the workspace
17:17:17.573 daemon daemon.workspace.select      republished the roster registry
17:17:17.602 emacs  elisp.roster.current: followed
17:17:17.610 emacs  elisp.webhost.show
```

The same switch by key moved the frame within about 10ms of the request.

## Landed changes

### `agentrepl.v1.SelectInEditor` (new RPC)

- **What changed.** A new WEB-section RPC, `endpoint_select_in_editor.proto`.
  The request names the clicked workspace. Success says which party made the
  switch: `relayed`, or `selected` carrying `agentrepl.v1.SelectWorkspaceSuccess`.
  The error arms are SelectWorkspace's, arm for arm.
- **Why.** The editor must make the switch, as it makes a key's, so the
  click reaches Emacs as a request and not as a roster push.
- **Why the daemon decides the route by the event's own delivery.** Emacs
  holds a host stream for exactly the workspaces it has a tab for, and a row
  can be clicked without one: a closed row, or an open one whose tab has not
  opened yet. `publish.Events.Emit` answers how many host streams it handed
  the event to, under the lock that hands it over. The daemon selects only
  when that count is zero. A click is therefore never both relayed and
  selected, and never neither.

### `agentrepl.v1.WatchHostWorkspaceResponse.push` gains `select_requested`

- **What changed.** A new event arm, `select_requested` (field 12), carrying
  the empty `agentrepl.v1.HostWorkspaceSelectRequested`.
- **Why an event.** A click belongs to the moment it was made. Replaying it
  to a late subscriber would switch the frame on every reconnect. This
  follows `notification_clicked`, the other click relayed as a host event,
  which also ends in `agent-repl-host-request-switch`.
- **Consequences.**
  - Emacs requests the switch with the `sidebar` trigger. It is counted as
    an editor switch (`agent-repl-host--editor-switches`), so a sidebar click
    now withdraws a pending landing on a freshly minted workspace, as a key
    does.
  - Every rail click goes through the new RPC: a grouping row's click, and
    the select that follows a hidden or merged band's return.
  - A merge-queue entry's jump (`webapp/src/feed/merge/jump.ts`) still calls
    SelectWorkspace, because it carries `reveal`, which the editor's request
    has no spelling for.
