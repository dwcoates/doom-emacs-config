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
    fail the request" rule are the script's, kept as the contract.
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
