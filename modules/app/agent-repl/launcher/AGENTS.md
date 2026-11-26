# launcher/ — Zettel.app, agent-repl's face to macOS

Zettel.app is the launcher: the app that connects agent-repl to macOS. Opened by
hand (Finder, Spotlight, the Dock) it opens Emacs, and Emacs's own startup
brings up the backend exactly as opening Emacs does. Resident under launchd
(`--resident`) it posts every desktop banner through `UNUserNotificationCenter`,
reports each banner click, and closes each Slack notification after its time on
screen (below). It is a background app (`LSUIElement`: no window,
no Dock icon), bundle ID `app.zettel`, run by launchd as `app.zettel.launcher`
(the `service-launcher` entry of `launcher/requirements.json`, rendered by
`bin/requirements.sh`; `services/AGENTS.md`) and installed at
`~/Applications/Zettel.app`, where Spotlight and Finder find it. Linux has no
launcher: there the daemon's banner program (`notify-send`) posts.

None of its names carries `agentrepl`: agent-repl is being renamed zettel
(`docs/protobuf-design/zettel-launcher.md`).

## What it does, and what it never does

THE DAEMON OWNS EVERY BANNER (`daemon/AGENTS.md`, desktop banners). The launcher
decides nothing about one: not whether it is wanted, not its text, not what its
click selects. It is the SERVER of the posts and the reporter of the clicks.

- It accepts on `~/.cache/agent-repl/sock/launcher.sock`, a unix socket a socket
  holder (`app.zettel.sockethold.launcher`, control socket
  `~/.cache/agent-repl/sock/launcher-holder.sock`) owns for good: the same
  sockethold program and pattern as the daemon's and the store's holders
  (`docs/investigations/2026-11-18-stable-sockets-design.md`). The launcher takes
  the listening fd from the holder at boot (`takeHeldSocket`, SCM_RIGHTS). A
  dial made while the launcher restarts queues in the held socket's backlog.
- Each connection is one exchange on the `launcher.v1` wire (`proto/src/launcher/v1/launcher.proto`),
  protobuf length-delimited (a varint length, then the message): the daemon
  writes a `PostBannerRequest` (an `agentrepl.v1.DesktopBanner` and the posting
  daemon's pid), the launcher posts it through `UNUserNotificationCenter`, using
  the banner's group as the notification's identifier (a newer banner of a
  workspace replaces its older one), with no sound (the daemon rings the one
  chime), and answers a `PostBannerResponse` once macOS's notification center
  took or refused it: an `agentrepl.v1.DesktopBannerPostOutcome` (posted,
  not_allowed or failed) or a `PostBannerRefused`. `posted` means Zettel's
  settings draw the banner on screen: before each post it reads them
  (`offScreenSetting`), and a banner they keep off (denied, alerts off, style
  None, provisional) is never handed to macOS and answers `not_allowed`
  naming that setting, because macOS's own `add` takes such a banner without
  an error. `--check-notifications` asks the same function. A banner macOS
  took while a Focus silences Zettel answers `silenced` (the Focus's name, a
  WARN), read from the Do Not Disturb store's private files
  (`Focus.swift`), whose unrecognized layout is an ERROR, never a guess: either
  way the daemon rings no chime for it.
- On a click it brings Emacs forward (the click is the one moment macOS lets it
  activate another app) and reports `ReportDesktopBannerClicked` with the
  banner's click token, to whichever daemon `<state>/daemon.addr` advertises at
  that moment, following one redirect (`ClickReporter`).
- Opened by hand it opens Emacs. When no resident launcher runs, the copy opened
  by hand opens Emacs, runs `launchctl kickstart gui/<uid>/app.zettel.launcher`
  and exits; while the resident one runs, macOS reopens that instance
  (`applicationShouldHandleReopen`), which opens Emacs.
- One launcher runs at a time: a `flock` on `~/.cache/agent-repl/run/launcher.lock`
  (the kernel releases it when the process dies).
- It writes a build report, `~/.cache/agent-repl/run/launcher.build.json`
  (`{"pid","build"}`, the shared `agentrepl/logging/buildreport` shape,
  `buildreport.ServiceLauncher`), so the deploy judges it with the same
  `serviceStaleness` every service uses.

Because every daemon dials the socket per banner, a daemon standing down (a
drain) or both daemons of a handover post exactly like a serving one. A banner
is never lost to a stop or a handover.

## Slack's notifications close after a set time

macOS shows a temporary notification for about five seconds and offers no
setting for longer. So the user sets Slack's alert style to Persistent, and the
resident launcher closes each Slack notification once it has been on screen for
`SlackNotificationSeconds` (defaults domain `app.zettel`, default 15; set with
`defaults write app.zettel SlackNotificationSeconds -int <seconds>`, read anew
for every notification, so no restart is needed). Slack still decides which
messages notify, so its per-channel settings hold.

- `SlackNotificationCloser` (`SlackNotifications.swift`) decides: a Slack
  notification seen for the first time gets its deadline, one gone from the
  screen is forgotten, and one whose time is up is closed with its `Close`
  action. Unit-tested against a fake screen and a fake clock.
- `AccessibilityScreen` (`AccessibilityScreen.swift`) reads Notification
  Center's windows through the Accessibility API and acts on them. It watches
  the Notification Center process (`AXObserver`, window creation and layout
  changes, coalesced to one scan per run-loop turn), and attaches again when
  macOS grants Zettel Accessibility or Notification Center restarts. The tree
  reading itself (`scanWindows`) is pure and unit-tested against the layout
  measured on macOS 26.2.
- Apple does not document that layout, so EVERY WAY IT CAN STOP MATCHING IS
  RECORDED at WARN or ERROR (`launcher.slack_notifications`): a window holding
  no notification the launcher recognizes, a notification naming no app, one
  offering no `Close`, a failed scan or close, Accessibility not allowed, an
  invalid `SlackNotificationSeconds`. A macOS update that breaks the closer
  shows in the log the first time, never as Slack notifications that silently
  stay up.
- The Accessibility permission is the installer's
  `launcher-accessibility-permission` entry (`--check-accessibility`,
  `--request-accessibility`). macOS keys it to the app's signature, which is
  why Zettel is signed with the local identity (`bin/lib-signing.sh`) rather
  than ad hoc: an ad hoc signature changes with every build, and every deploy
  would lose the permission.

## Layout

| Path | What |
|---|---|
| `Package.swift`, `Package.resolved` | the SwiftPM package; `swift-protobuf` pinned exactly |
| `Sources/ZettelLauncher/` | everything the launcher does, unit-tested: `BannerServer` (the exchange and the accept loop), `Delimited` (the framing, `SocketChannel`), `HolderSocket` (`takeHeldSocket`), `Resident` (the instance lock, the build report), `Clicks` (`ClickReporter`, `ConnectDaemon`), `Connect` (the unary Connect client over HTTP/1.1, binary protobuf), `DaemonAddress`, `Focus` (the Do Not Disturb store's reader), `NotificationCenter`, `SlackNotifications` (`SlackNotificationCloser`), `AccessibilityScreen`, `Options` (`--resident`, `--log`, `--holder`, `--run-dir`, `--state-dir`, `--check-notifications`, `--request-notifications`, `--check-accessibility`, `--request-accessibility`), `Log` |
| `Sources/Zettel/main.swift` | the AppKit shell: the app delegate, the notification delegate, the permission modes |
| `Sources/ZettelIcon/` | renders the icon at one size (`bin/build-launcher.sh` packs every size) |
| `Sources/ZettelProto` | a link to `proto/gen/swift`, the generated bindings |
| `Bundle/Info.plist` | the bundle's identity; **`CFBundleIdentifier` (`app.zettel`) is permanent** (macOS keys the notification permission and every Focus's allowed apps to it) |
| `Tests/ZettelLauncherTests/` | XCTest cases; `test.sh` runs them (the `launcher` suite) |

## The icon

A New York serif `Z` in the canonical purple (`AGENTS.md`, "The canonical
purple": its fixed dark value `#7d4bcf`) on a rounded square of the macOS
app-icon grid filled with the composer's background (the webapp's
`--sidebar-ground` in the dark theme, `#171717`). It is rendered from `Sources/ZettelIcon` at build time and
never committed as an image.

## Building and installing

`bin/build-launcher.sh` builds the release products, assembles the bundle,
signs it with the local identity (`bin/lib-signing.sh`; the build in the cache bin stands at `~/.cache/agent-repl/bin/Zettel.app.staged`, never named `*.app`, because macOS registers any `.app` it finds and System Settings then listed two Zettels; a leftover cache `Zettel.app` is removed and named at the next build or deploy. A deploy's staging copy is `cache-bin/Zettel.app` in the transient staging directory: the path the running daemon, built from the previous revision, reads, which never changes), and installs it at `~/Applications/Zettel.app` (swapped in by
rename), registers it with LaunchServices, and restarts the service. The deploy
stages it through `bin/build-frontend.sh launcher` (`--out`, no install) and
installs it itself (`daemon/AGENTS.md`, Deploy). `bin/requirements.sh` installs
the launchd agent and its socket holder and asks for the notification permission
and Accessibility (`launcher/requirements.json`: `service-launcher`,
`socket-holder-launcher`, `launcher-notification-permission`,
`launcher-accessibility-permission`).

## The Swift bindings

`make -C proto swift` generates `proto/gen/swift` for the DESKTOP BANNERS
section of `agentrepl.v1`, `launcher.v1` and their imports only (`SWIFT_PROTOS`
in `proto/Makefile`), with the `protoc-gen-swift` built from this package's own
pinned `swift-protobuf`, so the generator and the runtime library are one
version by construction. A file the launcher comes to import is added to
`SWIFT_PROTOS`.

## Logging

Every record goes through `FileLogger`, the launcher's one logging path, into
`~/.cache/agent-repl/log/launcher.log` (runtime `launcher`), in the shared
JSONL shape and timestamp (`logging-contract.md`; `LogTests` holds the
timestamp to `proto/vocab/log-timestamp.json`).

Every log call follows "Log sites" in `logging-contract.md`: `log.<level>(operation,
"literal template with {named} placeholders", ["named": value])`. The template
is a `StaticString`, so the compiler refuses anything but a literal; its
placeholders are filled from the named values, which stay `context` fields,
and every record carries `site` (`logSiteKey` over the runtime, the calling
file and function, and the template) and `template`. Never interpolate a
value into the template (`\(x)`) and never write one template twice in one
function. `LogSiteTests` pins the rendering, the key (the contract's
`074892b7d933` vector) and scans every call in `Sources/` for both rules.

## Testing

`launcher/test.sh` (`swift build --build-tests`, then `swift test`) under
background priority; the roster's `launcher` suite runs it. The server is
tested against fakes of the daemon and the notification center; a test awaits
the event its act raises (`Signals`), never a sleep or a poll.
