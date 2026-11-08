# Zettel desktop notifier — protobuf design record

The owner's desktop banners stopped showing because a macOS Focus silenced
`terminal-notifier`, an anonymous Homebrew tool whose banners carry its own
terminal icon. The owner wants agent-repl to post its banners the way Slack or
Chrome do: from an app bundle of its own, named Zettel (the coming name of
agent-repl), with its own icon (a New York serif `Z` in the canonical purple on
white), so that it has its own row in System Settings → Notifications and in
each Focus's allowed apps. The owner ruled (2026-11-08): the app is named
Zettel, it is RESIDENT (a long-lived background app connected to the daemon),
it lives in `~/Applications` so Spotlight and Finder show it, and opening it by
hand brings Emacs forward. Design ran in autonomous mode; every contract call
below is the implementer's, recorded with its reasoning.

## Core design principles

- **The daemon owns every banner** (standing rule, `internal/desktopnotify`).
  Consequences: the app decides nothing; it posts what it is handed and relays
  the only two facts it alone observes, whether the platform took a banner and
  the user's click. It does not claim the app chooses titles, replacement, or
  what a click selects.

## Landed changes

### 1. A DESKTOP NOTIFIER section in `agentrepl.v1`

What: `desktop_banner.proto` (the banner and its echoed identities) and three
endpoints: `WatchDesktopBanners` (server stream of banners to post),
`ReportDesktopBannerPosted` (the app's answer per banner) and
`ReportDesktopBannerClicked` (a click).

Why: the app is a client of the daemon's one Connect listener, exactly like
Emacs, so its surface is `agentrepl.v1`; `proto/AGENTS.md` now lists the app
among the package's importers.

Consequences and reasoning:

- **Connect over HTTP/1.1 rules out a bidirectional stream**, so the post's
  answer is a unary call keyed by `DesktopBannerPostId` instead of a frame on
  the same stream. The daemon waits for that answer, bounded, before it rings
  the chime and holds the banner for a focus claim, preserving the existing
  invariant that a sound and a claim belong only to a banner that was made.
- **The click does not reuse `SelectWorkspace`.** Its request imports
  `frontend/v1/feed.proto`, whose import closure is 23 files and about 13,000
  lines of schema; generating Swift for that into a small app was rejected.
  More importantly, a banner click is not a plain selection: it must also let
  go of every held banner and raise Emacs's frame through the host stream's
  `notification_clicked`, which is exactly what the Linux read-back path
  already does (`Notifier.show`, `Clicked`). So the click reports to the
  daemon, which runs that one path.
- **The click token names a workspace, not a post** (typed echo token). A
  banner sits in Notification Center across daemon restarts and deploys; a
  token naming a post would be meaningless to the daemon serving at the click.
- **One stream stands at a time; a later one supersedes**, with a
  `superseded` last frame so the displaced app exits instead of reconnecting
  and fighting for the slot. Mirrors `desktopnotify.Focus`'s streamslot.
- **No queueing while no app is attached.** A banner wanted while no stream
  stands is recorded as unposted (ERROR, with the reason) and dropped: a late
  banner would steer the user to a stale workspace.
- **`DaemonStreamEnding` is reused** as the planned-exit last frame, so the
  app tells a deploy from a crash the same way Emacs does.
- **Permission is an outcome arm, not a stream fact.** `not_allowed` is
  reported per post, since the user can change it at any moment; the daemon
  logs it with the remedy.
