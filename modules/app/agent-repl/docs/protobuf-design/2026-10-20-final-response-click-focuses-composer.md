# A clicked final response focuses the composer

The owner asked (2026-10-20) that selecting a final response bubble put the
keyboard in the Emacs composer, in insert state, so the reply the selection
sets up can be typed at once.

## Landed changes

### `agentrepl.v1.WatchHostWorkspaceResponse.push` gains `final_response_clicked`

- **What changed.** A new event arm, `final_response_clicked` (field 11),
  carrying the empty `agentrepl.v1.HostWorkspaceFinalResponseClicked`.
- **Why.** Selecting a final response makes it the reply target, and the
  reader's next act is typing that reply. The keyboard is in the webview
  after a click, so Emacs must be told to move it.
- **Why an event and not the `selection` push.** The `selection` arm is
  state: the daemon's selection topic replays the selection in force to every
  new subscriber, so every reconnect, daemon handover and Emacs restart would
  re-deliver a selected response. Focusing from it would take the keyboard
  away from wherever the reader was on each of those, a focus theft that only
  happens to be rare. An event on the host event stream is never replayed, so
  the focus happens exactly once per click and cannot recur on a resubscribe.
  This follows `notification_clicked`, the other click relayed as a host
  event.
- **Why only a click.** SelectFeedRow's `turn_boundary` step is sent from the
  composer (`M-s-j` / `M-s-k`), which already holds the keyboard. Forcing
  insert state on a step would also take away the normal-state `/` and `?`
  that search the selected bubble while a selection stands. A click that
  deselects the selected response (success `none`) selects nothing, so it
  sends no event. A click on any other selectable bubble (a prompt, an interim
  response) sends none either: the owner asked for final responses
  specifically.
- **Consequences.**
  - The daemon emits the event from `selectBubble`
    (`daemon/internal/server/select_feed_row.go`) after the selection is set,
    on the host event stream, so its order relative to the `selection` push
    is not fixed. Emacs's handler reads nothing from the selection, so the
    order does not matter.
  - Emacs focuses the composer only when this workspace's composer window is
    on the selected frame. A gated workspace (no composer) or a composer not
    on screen is an expected case, recorded at INFO and not focused, because
    mounting panels from a click would move the layout.
