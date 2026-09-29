# Daemon-owned desktop notifications

## Problem

Desktop notifications for workspaces are posted by Emacs today. The daemon
relays typed notifications (`agentrepl.v1.HostWorkspaceNotification`) over the
host stream and Emacs decides whether to post a banner, blink a tab, or do
nothing. Turn-end banners do not go through that path at all: Emacs derives
them by diffing successive roster pushes (`lisp/roster.el`
`agent-repl-roster--run-finish-edges`, `lisp/session.el`
`agent-repl--maybe-notify-finished`), so a turn that settles while Emacs is not
watching produces no banner.

The owner asked why Emacs is involved at all. The premise the contract was
built on ("only Emacs knows whether it is focused") does not hold: any process
can ask macOS which application is frontmost, and the daemon already learns
tab selection through `SelectWorkspace`. The owner's direction: move banner
spawning into the daemon; Emacs keeps only the click action (select the
workspace's tab) and the tab blink.

## Context

- **Objective.** The daemon posts every workspace desktop notification,
  turn end included; Emacs keeps only the click action (select the
  workspace's tab) and the tab blink.
- **Frontend component.** None in the webapp's figma→idl views; the tab blink
  is Emacs's tab bar.
- **Systems.** `proto` (`agentrepl.v1` host stream), `daemon` (banner
  decision, banner spawning, click read-back, turn-end firing), Emacs `lisp`
  (click → tab selection, blink), test support (`lisp/testsupport/fakedaemon`,
  `e2e`). Owner confirmed this list.
- **Non-additive changes.** The banner backends leave `lisp/notifications.el`;
  the roster-diff turn-end banner (`agent-repl--maybe-notify-finished`, via
  `agent-repl-roster-finish-functions`) is removed; the host stream's
  notification arm stops asking Emacs to decide presentation; Emacs's focus
  check leaves the notification path. Owner confirmed.
- **Trigger is the TURN, not the workspace.** A turn-end notification fires
  when a turn ends. Detached work and queued prompts have nothing to do with
  it: the owner's words, "it has nothing to do with any detached work at all,
  only turns".
- **Focus policy is Emacs-wide, and only Emacs.** Emacs focused (whatever
  workspace is open in it) → no desktop notification; Emacs not focused →
  desktop notification. External-browser webapp viewers are out of scope.
- **Platforms.** macOS AND Linux, not macOS only.
- **The attention marker is untouched.** Turn-end notifications neither set
  nor clear `frontend.v1.RosterRow.attention`. The owner: Emacs and the
  webapp already learn of turn completion and update their statuses; that
  already works and is out of band of this change.
- **The banner rides the existing turn-completion path.** The desktop
  notification is raised from the same daemon code path that tells clients a
  turn completed, not from a second derivation of the same fact.
- **Banner content.** A turn that ended normally:
  `✅ <workspace title> turn completed <timestamp>` over a one-to-three-line
  Sonnet summary of the turn's final response message (the content of the
  green-bordered response bubble the webapp shows). A turn that ended in
  failure: `❌ <workspace title> turn errored <timestamp>` over the daemon's
  error message for why the turn failed.
- **The banner follows the roster status the turn end resolves to.** A turn
  end resolving to `done` (green) raises the ✅ banner; one resolving to
  `turn_failed` (turquoise) or `vendor_blocked` (blue) raises the ❌ banner,
  whose line is the reason the feed shows for the failed turn. Owner agreed.
- **An interrupted turn raises the ✅ banner.** The owner notes it is
  impossible in practice (an interrupt means Emacs is focused), but the rule
  is: it gets the done notification.
- **`ExpectedStop` is `done`.** A Stop hook that prevented continuing, or a
  deferred tool, resolves to the `done` roster status and raises the ✅
  banner. Where the resolver does not already resolve it to `done`, that is
  fixed as part of this change.
- **Emacs reports its own focus to the daemon (option B).** Emacs pushes its
  focus changes (`after-focus-change-function`) and the daemon decides on the
  latest reported state. Chosen over the daemon asking the OS, which has no
  standard answer on Wayland. The daemon still owns the decision and the
  banner; Emacs only reports a fact.
- **Prefer augmenting existing RPCs.** New RPCs only where no existing one is
  suitable.

## Iteration sequence

Accepted as proposed, no amendments:

1. Transport — the existing ConnectRPC `agentrepl.v1` service; no new
   transport.
2. Endpoint inventory — which existing rpcs are augmented and whether any new
   rpc is needed (Emacs's focus report, the banner click reaching Emacs, the
   retirement of what Emacs no longer consumes); names and one-line purposes
   only.
3. Conventions — the package's existing ones stand (`oneof result { success;
   error; }`; validation failures are Connect `InvalidArgument`, never an arm).
4. Shapes — one endpoint at a time, each landed on agreement.

## Landed changes

### 1. Emacs reports its focus: `agentrepl.v1.EditorFocus`, `WatchDaemonEmacs.focus`, `ReportEditorFocus`

- **What.** New shared `editor_focus.proto` declaring `EditorFocus` (a oneof
  of focused / unfocused). `WatchDaemonEmacs` gains a REQUIRED `focus`
  (field 2). New unary rpc `ReportEditorFocus` in the HOST section, with one
  refusal arm, `no_emacs_stream`.
- **Why.** The owner chose option B: Emacs is the one process that can see
  its own focus on macOS, X11 and Wayland alike. Prefer augmenting existing
  rpcs: the connect-time focus rides the existing WatchDaemon request; a
  server stream's request is sent once, so later changes need their own rpc,
  and `SelectWorkspace` (tab selection) is a different fact.
- **Consequences.**
  - The focus is scoped to Emacs's WatchDaemon stream: known from the stream's
    first instant (no window where a banner is decided on an unreported
    focus), forgotten when it ends, after which the daemon treats Emacs as
    unfocused. A report with no Emacs stream standing is refused
    (`no_emacs_stream`), never silently dropped.
  - Emacs must compute its focus before opening WatchDaemon and call
    `ReportEditorFocus` from `after-focus-change-function`.
  - The daemon holds one focus value (Emacs opens exactly one WatchDaemon).
- **Alternatives rejected.** The daemon asking the OS for the frontmost app:
  impossible under Wayland. A focus field on `SelectWorkspace`: conflates tab
  selection with desktop focus and misnames the rpc.

### 2. The banner click reaches Emacs: `WatchHostWorkspaceResponse.notification_clicked`

- **What.** New event arm `notification_clicked = 7` carrying the empty
  `HostWorkspaceNotificationClicked`.
- **Why.** The daemon posts the banner and reads the click back itself;
  selecting the workspace's tab is the one thing only Emacs can do. The
  per-workspace host stream is the natural home (the click is about one
  workspace, and the stream already follows handover).
- **Consequences.** An event, never replayed to a late subscriber. Emacs
  raises its frame and selects the tab on it.

### 3. Retired: `WatchHostWorkspaceResponse.notification`

- **What.** Arm 2 is removed and reserved (number and name), together with
  `HostWorkspaceNotification`, `HostNotificationKind`,
  `HostNotificationAgentAddressed`, `HostNotificationPermissionRequested` and
  `HostNotificationQuestionAsked`. Nothing else in `proto/src` referenced them
  (grep).
- **Why.** Its only consumer was Emacs's presentation policy
  (`agent-repl-host--notify`, `lisp/host.el`): banner or tab blink. The daemon
  now posts banners, and the tab blink is already drawn from the roster's
  attention marker (`agent-repl-status-sync-attention`,
  `lisp/status.el:1145`), so the host-stream blink was a second trigger for
  the same blink.
- **Consequences.**
  - The daemon's internal `sessionwatcher.HostNotification` stops being a
    wire relay and becomes the input of the daemon's own banner poster; its
    attention-marker side (`workspace.verbs.Notify`) is untouched.
  - Permission, question and agent-push notifications move to the daemon's
    banner too, under the same focus rule.
  - Elisp removed: `agent-repl-host--notify` and its notification decoding,
    the banner backends in `lisp/notifications.el`, and the roster-diff
    turn-end banner (`agent-repl--maybe-notify-finished` on
    `agent-repl-roster-finish-functions`).

## Implementation notes

- **The turn-end trigger is the prompt queue's live turn end**
  (`promptqueue.OnTurnEnded` → `queue.endTurn`), after the door
  (`closeTurn`) has installed the roster's close and drawn the feed's ending.
  The ending's content (final-answer markdown, errored line, failure class) is
  filed by the feed on the LIVE plane only (`feed.TakeTurnEnding`), so a history
  replay raises no banner. Orphaned turns closed at boot raise none.
- **The banner kind reads the roster's own table.** The sidebar's `closeArm`
  and its expected-stop override were extracted into `ladder.ResolveTurnEnd`
  (behavior-preserving, own commit); the banner calls the same function, so the
  tab colour and the banner cannot disagree.
- **Emacs serializes its focus reports** (one call in flight; a change during
  it is owed and sends the focus held when it is answered) and re-reports on
  link-up and promotion, closing the gap between building the WatchDaemon
  request and its acceptance.
- **Agent notifications keep Emacs's former banner shape** (workspace name over
  the notification's line); only the turn-end banner has the new ✅/❌ title.
