# Zettel launcher — protobuf design record

The owner's desktop banners stopped showing because a macOS Focus silenced
`terminal-notifier`, an anonymous Homebrew tool whose banners carry its own
terminal icon. The owner wants agent-repl to post its banners the way Slack or
Chrome do: from an app bundle of its own, named Zettel (the coming name of
agent-repl), with its own icon (a New York serif `Z` in the canonical purple),
so that it has its own row in System Settings → Notifications and in each
Focus's allowed apps. The owner ruled (2026-11-08): the app is named Zettel,
it is RESIDENT, it lives in `~/Applications` so Spotlight and Finder show it,
and opening it by hand brings Emacs forward. Design ran in autonomous mode;
every contract call below is the implementer's, recorded with its reasoning.

The first design (the app as a client holding a `WatchDesktopBanners` stream
to the daemon) was replaced the same month by the launcher design below. The
reason is recorded first because every later decision follows from it.

## Core design principles

- **The daemon owns every banner** (standing rule, `internal/desktopnotify`).
  Consequences: the launcher decides nothing; it posts what it is handed and
  relays the only two facts it alone observes, whether the platform took a
  banner and the user's click. It does not claim the launcher chooses titles,
  replacement, or what a click selects.
- **The launcher outlives every daemon, so the launcher serves.** A banner
  must never be lost to a daemon's stop or a handover. That is held by
  structure, not by timing: nothing a daemon does or fails to do can leave a
  banner with nowhere to go.

## Why the stream design was replaced

In the first design the app opened a `WatchDesktopBanners` server stream to
the daemon, answered each post on a unary `ReportDesktopBannerPosted`, and
followed the daemon's address advertisement to hold a stream to the newest
daemon. A later stream superseded the one before it.

That design lost banners. A daemon standing down (a drain) no longer had a
stream the app was serving, yet it could still wish to post a banner, and so
could either daemon of a handover: the one handing over and the one taking
over. A banner wanted while no stream stood was dropped (it was recorded as
unposted, an ERROR, never queued). The window was narrow, but a narrow window
is a probability, not a guarantee, and the banners lost were the ones that
mark a turn ending during a deploy.

The launcher design removes the window instead of shrinking it. The launcher
is the server, accepting on a unix socket that a socket holder owns for good,
so that socket exists whether or not any daemon, or the launcher itself, is
running. Every daemon dials it per banner. A dial made while the launcher
restarts queues in the held socket's backlog until the launcher accepts, so a
stopping daemon and both daemons of a handover post exactly like a serving
one. Banner loss at a stop or handover is structurally impossible; the stream
design could only make it unlikely.

## Naming

Owner ruling (2026-11-21): the app is "the launcher", registered as
Zettel.app. Its bundle ID is `app.zettel`, its launchd label
`app.zettel.launcher`, and its socket holder `app.zettel.sockethold.launcher`.
No name of the launcher carries `agentrepl`, because agent-repl is being
renamed zettel; the permanent bundle ID in particular (macOS keys the
notification permission and every Focus's allowed apps to it) must not name a
system that is about to stop being called that. The rest follow the one name:
`launcher/`, `bin/build-launcher.sh`, the `launcher` build-frontend target and
test suite, `ZettelLauncher`, `launcher.log` (runtime `launcher`), the
requirement ids `service-launcher`, `socket-holder-launcher` and
`launcher-notification-permission`, `DEPLOY_COMPONENT_LAUNCHER` and
`FooterStatusActivityUpdateComponent.launcher`.

"Launcher" names what the app is to macOS: opened by hand (Finder, Spotlight,
the Dock) it opens Emacs, and Emacs's own startup brings up the backend exactly
as opening Emacs does. Resident under launchd (`--resident`), it also posts
every banner and reports each click.

## Landed changes

### 1. `launcher.v1`: what a daemon says to the launcher

What: `proto/src/launcher/v1/launcher.proto`, with `PostBannerRequest` (an
`agentrepl.v1.DesktopBanner` and `PostingDaemon.pid`), `PostBannerResponse`
(either `agentrepl.v1.DesktopBannerPostOutcome`, one of posted, not_allowed or
failed, or `PostBannerRefused`) and the socket at
`~/.cache/agent-repl/sock/launcher.sock`.

Why: the launcher is a server of its own socket, so what a daemon says to it
is its own package, not part of the daemon's Connect surface.

The framing: one exchange per connection, each message in protobuf's standard
length-delimited form (a varint byte count, then the message; Go `protodelim`,
Swift hand-rolled in `Delimited.swift`). The daemon writes one request; the
launcher answers once macOS's notification center took or refused the banner,
then closes. The daemon's read of the answer is the post. A plain unary Connect
call was not used because the launcher is a socket server with no HTTP stack,
and a hand-rolled one is more than the framing needs.

Consequences and reasoning:

- **The post's answer needs no key.** The first design keyed the answer by
  `DesktopBannerPostId` because Connect over HTTP/1.1 rules out a bidirectional
  stream. One exchange per connection makes the answer belong to its request
  by construction, so `DesktopBanner` lost its `post_id`.
- **The daemon waits for that answer, bounded** (`desktopnotify.LauncherPostBound`,
  5s, not yet measured; each post records `duration_ms`), before it rings the
  chime and holds the banner for a focus claim, preserving the invariant that a
  sound and a claim belong only to a banner that was made. A failed post is
  recorded once as `ERROR daemon.desktopnotify.post`.
- **Permission is an outcome arm, not a connection fact.** `not_allowed` is
  reported per post, since the user can change it at any moment; the daemon
  logs it with the remedy.
- **`DesktopBannerPostOutcome` moved into `desktop_banner.proto`**, beside the
  banner, so `launcher.v1` imports one file for both.

### 2. The DESKTOP BANNERS section of `agentrepl.v1` has one RPC

What: `WatchDesktopBanners` and `ReportDesktopBannerPosted` are deleted. The
section (formerly DESKTOP NOTIFIER) keeps `ReportDesktopBannerClicked`.

Why: the stream and the post answer existed only to carry banners from the
daemon to the app, which the socket now does.

Consequences and reasoning:

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
- **Clicks are unchanged in shape.** The launcher reports to whichever daemon
  `<state>/daemon.addr` advertises at the click, following one redirect
  (`ClickReporter`).

### 3. The click's refusals follow the registry's resolution

What: `ReportDesktopBannerClickedError` gained `workspace_ref_mismatch`,
`transferring_away` and `not_yet_adopted` beside `unknown_workspace`.

Why: the daemon resolves the click token's workspace through the same
registered-workspace resolution every workspace-addressed rpc uses, which can
answer any of the four; an arm the endpoint lacked would surface as an
unlanded-arm transport error instead of a typed answer. A click lands at any
moment, including mid-handover, so the launcher needs `transferring_away`'s
address to report the click to the successor.

### 4. The launcher writes a build report; the launcher is a deploy component

What: `~/.cache/agent-repl/run/launcher.build.json` (`{"pid","build"}`, the
shared `agentrepl/logging/buildreport` shape, `buildreport.ServiceLauncher`);
`DEPLOY_COMPONENT_LAUNCHER` in `agentrepl.v1.DeployComponent`;
`FooterStatusActivityUpdateComponent.launcher` in `frontend.v1`.

Why: with no stream, the launcher reports its build the way the store and the
sidecar do, by a build-report file, so the deploy judges it with the same
`serviceStaleness` every service uses (the first design's stream report and
`deploy.Notifier.RunningBuild` are gone). The deploy then names the component
on its own wire (`Deploy`'s outcomes, Emacs's `agent-repl-wire-deploy-components`)
and on the footer's update line, which renders arm names generically.
`Restarter.RestartLauncher` ensures the launcher's socket holder
(`Restarter.LauncherHolder`), registers the bundle with `lsregister -f`, then
starts or restarts the service.

Consequences: the launcher is built and installed only on macOS
(`deploy.Deps.Launcher` nil elsewhere); the footer's arm and the deploy enum
value exist on every platform but are never sent on Linux.

### 5. One launcher at a time

A `flock` on `~/.cache/agent-repl/run/launcher.lock` (released by the kernel
when the process dies) keeps one launcher running. A copy opened by hand while
no resident launcher runs opens Emacs, runs `launchctl kickstart
gui/<uid>/app.zettel.launcher` and exits; while the resident one runs, macOS
reopens that instance (`applicationShouldHandleReopen`), which opens Emacs.

### 6. The integration harness

`FakeLauncher` (`daemon/integration/harness/launcher.go`) accepts on its own
socket for the whole test, outliving every daemon of the test, records banners,
answers posted, and writes the launcher build report so deploys judge it
current. It replaced `FakeNotifierApp`, which held a stream and so could not
show the loss this design removes.
