# notifier/ — Zettel.app, the macOS desktop notifier

Zettel.app posts every agent-repl desktop banner on macOS, under its own name
and icon, the way any Mac app posts its own notifications. It is a resident
background app (`LSUIElement`: no window, no Dock icon) run by launchd as
`com.agentrepl.notifier` (`launchd/com.agentrepl.notifier.plist`) and
installed at `~/Applications/Zettel.app`, where Spotlight and Finder find it.
Linux has no app: there the daemon's banner program (`notify-send`) posts.

## What it does, and what it never does

THE DAEMON OWNS EVERY BANNER (`daemon/AGENTS.md`, desktop banners). The app
decides nothing about one: not whether it is wanted, not its text, not what
its click selects. It relays the two facts only it can observe.

- It holds the daemon's `WatchDesktopBanners` stream (the DESKTOP NOTIFIER
  section of `agentrepl.v1`), reporting its pid and its build (the SHA-256 of
  its own executable, so a deploy can tell a stale app).
- It posts each `DesktopBanner` through `UNUserNotificationCenter`, using the
  banner's group as the notification's identifier (a newer banner of a
  workspace replaces its older one), with no sound (the daemon rings the one
  chime), and answers `ReportDesktopBannerPosted` with what the platform did.
- On a click it brings Emacs forward (the click is the one moment macOS lets
  it activate another app) and reports `ReportDesktopBannerClicked` with the
  banner's click token, to whichever daemon is advertised at that moment.
- Opened by hand (Spotlight, Finder, or a reopen while running) it brings
  Emacs forward. Started by launchd (`--resident`) it stays in the background.

## Following the daemon

The daemon's address is `<state>/daemon.addr` (`logging-contract.md`, "Daemon
address advertisement"). The app watches the state root's directory and opens
the stream to each NEW advertisement, dropping the one it held; the same
advertisement again changes nothing. A stream that ends is not redialed on its
own: a planned end (`ending`) is followed by the next daemon's advertisement,
and an unplanned one is an ERROR recovered by the next advertisement. A
`superseded` frame (another instance took the stream) makes the app exit.

## Layout

| Path | What |
|---|---|
| `Package.swift`, `Package.resolved` | the SwiftPM package; `swift-protobuf` pinned exactly |
| `Sources/ZettelNotifier/` | everything the app does, unit-tested: `Relay` (the stream, posts, clicks), `Connect` (Connect over HTTP/1.1, binary protobuf), `DaemonAddress`, `NotificationCenter`, `Options`, `Log` |
| `Sources/Zettel/main.swift` | the AppKit shell: the app delegate, the notification delegate, the permission modes |
| `Sources/ZettelIcon/` | renders the icon at one size (`bin/build-notifier.sh` packs every size) |
| `Sources/ZettelProto` | a link to `proto/gen/swift`, the generated bindings |
| `Bundle/Info.plist` | the bundle's identity; **`CFBundleIdentifier` is permanent** (macOS keys the notification permission and every Focus's allowed apps to it) |
| `Tests/ZettelNotifierTests/` | XCTest cases; `test.sh` runs them (the `notifier` suite) |

## The icon

A New York serif `Z` in the canonical purple (`AGENTS.md`, "The canonical
purple": its fixed dark value `#7d4bcf`) on a white rounded square of the
macOS app-icon grid. It is rendered from `Sources/ZettelIcon` at build time and
never committed as an image.

## Building and installing

`bin/build-notifier.sh` builds the release products, assembles the bundle,
signs it ad hoc, and installs it at `~/Applications/Zettel.app` (swapped in by
rename), registers it with LaunchServices, and restarts the agent. The deploy
stages it through `bin/build-frontend.sh notifier` (`--out`, no install) and
installs it itself (`daemon/AGENTS.md`, Deploy). `bin/requirements.sh` installs
the launchd agent and asks for the notification permission
(`notifier/requirements.json`).

## The Swift bindings

`make -C proto swift` generates `proto/gen/swift` for the DESKTOP NOTIFIER
section and its imports only (`SWIFT_PROTOS` in `proto/Makefile`), with the
`protoc-gen-swift` built from this package's own pinned `swift-protobuf`, so
the generator and the runtime library are one version by construction. A file
the section comes to import is added to `SWIFT_PROTOS`.

## Logging

Every record goes through `FileLogger`, the app's one logging path, into
`~/.cache/agent-repl/log/notifier.log` (runtime `notifier`), in the shared
JSONL shape and timestamp (`logging-contract.md`; `LogTests` holds the
timestamp to `proto/vocab/log-timestamp.json`).

## Testing

`notifier/test.sh` (`swift build --build-tests`, then `swift test`) under
background priority; the roster's `notifier` suite runs it. The relay is
tested against fakes of the daemon and the notification center; a test awaits
the event its act raises (`Signals`), never a sleep or a poll.
