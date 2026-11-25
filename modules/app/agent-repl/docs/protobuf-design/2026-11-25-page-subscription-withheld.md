# A page subscription withheld for a workspace that is not visible

A deploy's webapp reload booted pages for workspaces the daemon then stopped
calling visible. `WatchWebWorkspace` refuses a workspace that is not visible
with `not_visible`, and a page met that refusal through its multiplexed page
subscription, which carried only a Connect code and a sentence. The page read
it as the daemon being unreachable: it logged `rpc.stream-transport-failure`
and `rpc.unary-transport-failure` at ERROR and filed the `daemonUnreachable`
warning chip (2026-11-25 12:33:11, 12:33:13, 12:33:14, 12:33:17). The refusal
is planned, so the page must be able to tell it apart from an outage by type.

## Assumptions (autonomous design)

- Objective: a planned refusal files no failure on the page.
- Systems: the daemon (`internal/server`, which builds the failure) and the
  webapp (`src/rpc`, which reads it). No editor or shim change.
- Purely additive: one optional field and one empty message.

## Iteration sequence

One increment: the failure frame's new arm-like field. No transport,
inventory or convention stage is touched.

## Landed changes

### `agentrepl.v1.PageSubscriptionFailed.withheld` (field 4)

- WHAT: an optional empty message, set when the dedicated rpc's refusal was
  `not_visible`.
- WHY: the handover case (`handed_away`) already set the precedent of naming a
  planned refusal by type rather than by its sentence; parsing the message
  text would make the page's verdict depend on a log sentence.
- CONSEQUENCES: the daemon marks the refusal on the dedicated stream's Connect
  error itself (a metadata header, beside the successor header), so a direct
  stream and a page subscription carry the same fact; the page has one reader
  for it, beside `successorOf`. A withheld subscription reopens on its backoff
  rather than stopping, because the workspace may become visible again before
  the editor releases the page; the editor's release cancels it.
- Why not an end arm of its own (`PageSubscriptionEnded.how`): the
  subscription did fail in the dedicated rpc's terms (a refused open), and the
  page's stream layer already reopens failed subscriptions on its backoff; a
  new end arm would need its own reopen path.

Sweep: nothing obviated.

### `agentrepl.v1.ReportPageStateLoadReason` and `ReportPageStateUnloadReason`: an unnamed arm is recorded, never refused

- WHAT: comments only. A reason message whose `oneof` is unassigned is now
  read as a reason this daemon's revision does not name, and the daemon
  records the load or unload with its reason `unnamed` (`pageheld.LoadUnnamed`,
  `pageheld.UnloadUnnamed`, a new `reason` label on the page metrics). A
  missing reason message is still `invalid_argument`.
- WHY: the 2026-11-25 12:32 deploy retired the `dormant` unload arm, and the
  Emacs the deploy had not reloaded yet reported a `dormant` unload to the
  successor at 12:32:54.997. The successor read the oneof as unset, refused
  the report as invalid, and Emacs recorded `elisp.host.page-state-failed` at
  ERROR. Set-but-unassigned is the one signal every reader decodes across
  schema skew, so it is the unknown, not a malformed request.
- How the skew is detected and handled: the daemon cannot see which arm an
  older editor meant (the wire drops the unknown field), so the report's fact
  (the page loaded or unloaded) is recorded and its reason is labelled
  `unnamed`, at INFO, and counted. Every other arm of the report is unchanged.
