# Daemon-owned desktop notifications — vetting register

Assumptions the landed design rests on about systems outside the contract.

## 1. `notify-send --action=default=Open --wait` reports a body click on stdout

- **Assumption.** On Linux, `notify-send` (libnotify) accepts `--action` and
  `--wait`, blocks until the notification closes, and prints the invoked
  action's key (`default`) to stdout when the banner body is clicked.
- **Affected.** `desktopnotify.notifySendArgs` / `notifySendClicked`, and so
  whether a Linux click reaches `WatchHostWorkspaceResponse.notification_clicked`.
- **How to verify.** On a Linux desktop with the installed libnotify
  (`notify-send --version`, need ≥ 0.7.10 for `--action`/`--wait`): run
  `notify-send --app-name=Emacs --action=default=Open --wait T B`, click the
  banner body, and confirm stdout is `default`; repeat with a dismissal and
  confirm stdout is empty. Check GNOME Shell and a wlroots compositor (mako,
  dunst), since the notification server decides whether body clicks invoke the
  `default` action.
- **Status.** OPEN.

## 2. Emacs's `after-focus-change-function` fires on focus changes under Wayland

- **Assumption.** A pgtk Emacs on Wayland runs `after-focus-change-function`
  when its frames gain or lose focus, and `frame-focus-state` answers
  accordingly.
- **Affected.** `agent-repl--focus-changed` and so every `ReportEditorFocus`
  on Wayland; without it the daemon keeps the connect-time focus.
- **How to verify.** In a pgtk Emacs under a Wayland session, add a logging
  function to `after-focus-change-function`, switch to another application and
  back, and confirm both edges log with `frame-focus-state` nil then t.
- **Status.** OPEN.

## 3. `alerter` attributed to `org.gnu.Emacs` reports clicks as `@CONTENTCLICKED`

- **Assumption.** Unchanged from the Emacs-side backend this replaces: alerter
  with `--sender org.gnu.Emacs` posts through UNUserNotificationCenter and
  prints `@CONTENTCLICKED` / `@ACTIONCLICKED` on a click.
- **Affected.** `desktopnotify.alerterArgs` / `alerterClicked`.
- **How to verify.** Post one banner from the running daemon's environment
  (`alerter --title T --message B --sender org.gnu.Emacs --timeout 60`), click
  it, and confirm the token. The daemon is spawned by Emacs, so it inherits the
  GUI login session; confirm the banner appears when the daemon, not Emacs,
  execs it.
- **Status.** OPEN.
