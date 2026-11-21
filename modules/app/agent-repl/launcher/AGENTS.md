# launcher/ — Zettel.app, agent-repl's face to macOS

Zettel.app is the launcher: the app that connects agent-repl to macOS. Opened by
hand (Finder, Spotlight, the Dock) it opens Emacs, and Emacs's own startup
brings up the backend exactly as opening Emacs does. Resident under launchd
(`--resident`) it posts every desktop banner through `UNUserNotificationCenter`
and reports each banner click. It is a background app (`LSUIElement`: no window,
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
  not_allowed or failed) or a `PostBannerRefused`.
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

## Layout

| Path | What |
|---|---|
| `Package.swift`, `Package.resolved` | the SwiftPM package; `swift-protobuf` pinned exactly |
| `Sources/ZettelLauncher/` | everything the launcher does, unit-tested: `BannerServer` (the exchange and the accept loop), `Delimited` (the framing, `SocketChannel`), `HolderSocket` (`takeHeldSocket`), `Resident` (the instance lock, the build report), `Clicks` (`ClickReporter`, `ConnectDaemon`), `Connect` (the unary Connect client over HTTP/1.1, binary protobuf), `DaemonAddress`, `NotificationCenter`, `Options` (`--resident`, `--log`, `--holder`, `--run-dir`, `--state-dir`, `--check-notifications`, `--request-notifications`), `Log` |
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
signs it ad hoc, and installs it at `~/Applications/Zettel.app` (swapped in by
rename), registers it with LaunchServices, and restarts the service. The deploy
stages it through `bin/build-frontend.sh launcher` (`--out`, no install) and
installs it itself (`daemon/AGENTS.md`, Deploy). `bin/requirements.sh` installs
the launchd agent and its socket holder and asks for the notification permission
(`launcher/requirements.json`: `service-launcher`, `socket-holder-launcher`,
`launcher-notification-permission`).

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

## Testing

`launcher/test.sh` (`swift build --build-tests`, then `swift test`) under
background priority; the roster's `launcher` suite runs it. The server is
tested against fakes of the daemon and the notification center; a test awaits
the event its act raises (`Signals`), never a sleep or a poll.
