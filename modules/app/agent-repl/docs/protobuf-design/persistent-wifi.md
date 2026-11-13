# Persistent wifi mode (lid-closed operation)

The owner's `laptop_keep_alive` script (in `~/bin`) toggles lid-closed operation:
it joins a phone hotspot, disables system sleep with `pmset`, and dims the
display, or undoes all three. The owner asked for it to move into agent-repl:
the daemon owns the reading and the change, Emacs triggers it over its one
daemon-level connection (`agent-repl-persistent-wifi-mode-toggle`, `SPC j w`),
and both Emacs and the webapp's topbar show the standing. The owner directed
that every protobuf decision be made without their input (2026-10-01).

## Core design principles

- **The standing is a fact about the machine, never a workspace.** It rides the
  daemon-level stream and a daemon-scoped rpc; the topbar repeats it on every
  workspace's strip as that component's own resolved copy.
  - Consequence: no `WorkspaceRef` anywhere in the new messages.
  - Does not claim: that the webapp may change the mode. The chip is a status.

## Landed changes

### 1. `agentrepl.v1.PersistentWifiState` (shared, `persistent_wifi.proto`)

- WHAT: two independent oneofs, `wifi {joined, not_joined}` and
  `mode {on, off}`. Each left unassigned means the daemon could not read that
  fact. `PersistentWifiJoined.network_name` is optional.
- WHY: the owner's chip has four drawn states which are exactly the product of
  "on a network?" and "would closing the lid keep it?"; neither fact implies the
  other, so one combined oneof would have had to enumerate the product.
- Consequences:
  - "Mode on" is read as `SleepDisabled 1` in `pmset -g`, the same test the
    script's `status` verb uses; `networkoversleep` and `womp` are written by the
    change but not read back.
  - "Joined" is read as `LinkStatusActive : TRUE` on the Wi-Fi interface in
    `ipconfig getsummary`, NOT from the network name. Verified on this host
    (macOS 26.2): `ipconfig getsummary`, `system_profiler SPAirPortDataType` and
    `wifi-util ssid` all withhold the name (`<redacted>` / empty), and
    `networksetup -getairportnetwork en0` wrongly answers "not associated" while
    en0 holds a DHCP lease. So the name is optional and usually absent.

### 2. `UpdatePersistentWifiMode` rpc (HOST MACHINE section)

- WHAT: request oneof `on | off | toggle`; success carries the re-read state
  plus the hotspot step's and the display step's outcomes; error oneof
  `power_settings_refused | mode_unreadable`.
- WHY: the script's `on|off|toggle` verbs, with the toggle resolved inside the
  daemon under the controller's lock so two quick toggles cannot both act on
  one stale reading (structurally impossible, not merely unlikely).
- Consequences:
  - The step order (hotspot, power, display) and the "only the power step can
    fail the request" rule are the script's, kept as the contract. The energy
    mode step (5) slots in before the display under the same rule.
  - Because network names are withheld on this host, turning OFF cannot tell
    whether the joined network is the hotspot: it answers
    `network_unreadable` and leaves nothing. Turning ON with an unreadable name
    attempts the join anyway, exactly as the script does. This is an accepted
    cost of porting the script faithfully; the script has the same blind spot.

### 3. `WatchDaemonResponse.persistent_wifi = 8`

- WHAT: the standing pushed as state on Emacs streams only, replayed to late
  subscribers, like `faults_standing`.
- WHY: Emacs has no topbar; the webview draws the same standing from its
  topbar view.
- Consequences:
  - The daemon reads the standing in its Prime step, before anything is
    served, so an Emacs `WatchDaemon` stream now ALWAYS opens with a
    `persistent_wifi` frame. The integration tests that asserted a fresh
    daemon's stream carries no frame now assert that of a webview stream, and
    the late-subscriber invariant test compares the first frame of the driven
    topic only, since an Emacs stream merges several standing topics.
  - Every harness that boots a real daemon (daemon integration, e2e Emacs
    layer) points `AGENT_REPL_PERSISTENT_WIFI_TOOLS_DIR` at stateful fake
    `sudo`/`pmset`/`networksetup`/`ipconfig`, so no test touches the real
    machine.

### 4. `frontend.v1.TopbarView.persistent_wifi = 13` (`TopbarPersistentWifi`)

- WHAT: a chip between the context chip and the warning chip, with its own
  `wifi` and `mode` oneofs (frontend vocabulary, resolved from the state above by
  the topbar resolver) and a daemon-composed tooltip.
- WHY: figma-to-idl; the frontend package cannot import `agentrepl.v1`, and the
  chip's arms are what it paints: wifi joined is a green glyph, not joined red,
  unread muted; mode on puts the glyph on a blue disc it is just inscribed in.
- Consequence: the chip's red and green are its own paints, not the
  render-colors status vocabulary (whose red means "agent working"); the chip
  is not a workspace status.

### 5. `UpdatePersistentWifiModeSuccess.energy_mode = 4` (2026-10-17)

- WHAT: a fourth step between the power settings and the display. Turning the
  mode on runs `pmset -a powermode 1` (Low Power Mode). Turning it off runs
  `pmset -a powermode 2` (High Power Mode). The outcome oneof is
  `low_power | high_power | failed`.
- WHY: the owner asked that lid-closed operation also save battery, and that
  leaving it restore full speed.
- Consequences:
  - The energy step runs only after the power step took, so a refused power
    step still leaves everything as it was.
  - A failed energy step never fails the request: the machine stays awake
    whichever energy mode it runs in. It is reported inside the success and
    recorded at WARN, exactly like a failed display step.
  - The write goes through the same `sudo -n pmset -a` helper as the power
    settings, so the sudoers grant needs `powermode 1` and `powermode 2` lines
    (see the Setup section of `docs/USER-GUIDE.md`).
  - The energy mode is written, never read back, so the standing does not
    carry it.

### 6. The hotspot failover (2026-10-19, no wire change)

- WHAT: while the mode is on, every re-read that finds the Wi-Fi interface
  joined to nothing joins the configured hotspot
  (`daemon/internal/persistentwifi/failover.go`). A join that does not take is
  tried again on the next re-read, so the hotspot is rejoined within one
  cadence of it coming back in reach.
- WHY: the owner asked that persistent wifi mode guarantee the hotspot is
  joined once Wi-Fi connectivity is lost.
- Consequences:
  - The failover runs on the controller's re-read beat, under the same lock as
    `UpdatePersistentWifiMode`, so a beat never joins on a reading a mode change
    overturned.
  - The boot's Prime read (`Refresh`) never joins, so serving never waits on a
    Wi-Fi join; the first beat after boot does.
  - Only "joined to nothing" triggers it. Another joined network, including one
    whose name macOS withholds, is left alone.
  - Each distinct cause of a failover that cannot take is recorded once (WARN,
    or ERROR for an unconfigured hotspot), repeats at DEBUG, and the end of the
    failover at INFO.
  - The standing it publishes is the existing `PersistentWifiState`, so Emacs
    echoes the rejoin and the topbar chip turns green with no new message.

### 7. The hotspot failover backs off (2026-11-06, no wire change)

- WHAT: a failed failover join is retried on a schedule measured from the
  beat that first found Wi-Fi lost (`failoverBackoff` in
  `daemon/internal/persistentwifi/failover.go`): five seconds after the failure
  for the first five minutes, then a minute, then an hour from thirty minutes,
  and no join is started once three hours have passed.
- WHY: the owner asked that the hotspot be tried continually right after the
  loss, then less often, and not at all after three hours.
- Consequences:
  - The retry runs on the re-read beat, which waits five seconds after the
    previous beat returns, so the first tier's retry is exactly five seconds
    after the failure, and a later tier's within one beat of its due time.
  - The schedule restarts from nothing once a network is joined or the mode is
    off, so a give-up lasts until then.
  - Each back-off is recorded once at INFO, and the give-up once at WARN.

### 8. The failover rejoins a known network first (2026-11-13, no wire change)

- WHAT: each failover attempt first cycles the Wi-Fi radio's power
  (`networksetup -setairportpower <device> off`, then `on`), reads the
  standing, and when no network is joined waits `knownNetworkSettle` (20s)
  for macOS's own auto-join before joining the hotspot
  (`rejoinKnownLocked` and `joinHotspotLocked` in
  `daemon/internal/persistentwifi/failover.go`). A radio that cannot be
  cycled, or a machine with no Wi-Fi interface, goes straight to the hotspot.
  The first read that finds Wi-Fi lost also records the radio's power
  (`networksetup -getairportpower`), the link summary, the lid, the quiet
  state, the display and the energy mode, once per loss.
- WHY: on 2026-11-12 Wi-Fi dropped with the lid closed, the failover tried
  only the hotspot, which an idle iPhone does not advertise, and the machine
  stayed offline for about 2.5 hours until the lid opened; the home network
  then rejoined by itself 9s later. macOS withholds the joined network's name
  (section 1), so no known network can be named to join; the radio cycle is
  the one request that makes macOS run its auto-join over every preferred
  network, and it is already how turning the mode off leaves the hotspot.
- Consequences:
  - The settle is two to three times the measured 9s rejoin. A network change
    that finds a network joined during the settle ends the failover, recorded
    at INFO with `awaiting_known_network`.
  - Each step's distinct cause is recorded once: a refused radio cycle at WARN
    (`knownCause`), a hotspot that cannot be joined at WARN, an unconfigured
    hotspot at ERROR. Without a hotspot the radio cycle still runs on the
    retry schedule.
  - The failover's outcomes stay records, never wire: the standing it
    publishes is the existing `PersistentWifiState`, so no proto arm was added.
