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

## Landed changes
