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
