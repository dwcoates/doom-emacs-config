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

## Landed changes
