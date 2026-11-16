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

Superseded in part by section 10: the energy mode now follows the lid-closed
quiet state, not the switch.

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

### 8. The failover rejoins a known network first (2026-11-13, no wire change; SUPERSEDED by section 11)

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

### 9. The failover never gives up (2026-11-13, no wire change)

- WHAT: `failoverBackoff` keeps its five-second tier for the first five
  minutes, then retries every minute for as long as Wi-Fi stays lost with the
  mode on. The hourly tier and the three-hour give-up of section 7 are gone.
- WHY: on 2026-11-12 the machine stayed offline for about 2.5 hours with the
  lid closed, past the point the hourly tier retried at all usefully; an
  attempt a minute costs one radio cycle and one join, which is cheap against
  hours of lost agent work.
- Consequences:
  - Supersedes section 7's hourly tier and give-up; its back-off record is
    kept (INFO, once, at the step to a minute), and a hotspot cause is still
    warned once per distinct cause.

### 10. The energy mode follows the lid-closed quiet state (2026-11-13)

- WHAT: the energy mode is written only on the quiet state's two edges
  (`daemon/internal/persistentwifi/energy.go`). Entering it records the mode
  in use (the user's) in `persistent-wifi/energy-mode.json` under the state
  root, atomically and before the write, then sets Low Power Mode
  (`powermode 1`). Leaving it (the lid opened, the mode turned off, the lid
  unwatched) restores the recorded mode, then removes the record. The
  `UpdatePersistentWifiModeEnergyMode` arm `high_power = 2` is retired
  (`reserved`), and `restored = 4` (the `power_mode` restored, verbatim) and
  `unchanged = 5` are added.
- WHY: the owner ruling of 2026-11-13: "persistent wifi mode should only
  update the low vs high when the lid closes (switching to low when closing,
  restoring to whatever it was when opening)". Before it, every beat held the
  energy mode to section 5's rule (Low Power Mode with the mode on, High
  Power Mode under a closed lid on AC), so the owner's own switch to High
  Power Mode with the lid open was undone by the next beat.
- Consequences:
  - Supersedes section 5's directions: turning the mode on with the lid
    open, turning it off with no quiet state, any beat with the lid open, and
    the power source never write the energy mode. The AC rule is gone:
    closing always means Low Power Mode. Section 5's "written, never read
    back" no longer holds: the mode in use is read with the standing.
  - The record survives a restart, a handover or a crash while the lid is
    closed, exactly as the frozen set does: a daemon that enters the quiet
    state finding a record adopts it instead of recording the Low Power Mode
    it finds. With no record on leaving (a quiet state entered by an earlier
    build, or a record that could not be kept), nothing is restored, at INFO.
    A record that cannot be read writes nothing and is recorded at ERROR.
  - Every step decides by state and is retried on the next beat: while the
    quiet state stands and Low Power Mode does not, it is set again; while it
    does not stand and a record is kept, the restore is tried again, and the
    record kept until it takes. A restore whose mode is no longer the Low
    Power Mode the quiet state set (the user chose another since) forgets the
    record, so the user's choice stands. Each distinct failure is recorded
    once at ERROR (`daemon.persistentwifi.energy`).
  - The thaw guard restores the record too (`GuardRestoreEnergy`), once no
    daemon owns the quiet state, by the same decision: it exists to undo
    what the quiet state leaves on the host when no daemon is left to undo
    it, and an opened lid with no daemon would otherwise stay in Low Power
    Mode. It runs after the guard releases the frozen set's exclusive hold,
    so a booting daemon's bounded wait covers the thaw alone; a daemon
    booting meanwhile reaches the same answer by state.
  - The sudoers grant gains `powermode 0`, since the mode restored may be
    Automatic (`docs/USER-GUIDE.md`).

### 10a. Low Power Mode under a closed lid follows the power source (2026-11-14, comments only)

- WHAT: the power source is a heard fact, like the lid
  (`daemon/internal/persistentwifi/power.go`): IOKit's limited-power
  notification (`IOPSCreateLimitedPowerNotification`, read by
  `IOPSGetProvidingPowerSourceType`) in `notify_darwin.go`, a fake socket or
  fixed state through `AGENT_REPL_PERSISTENT_WIFI_POWER_SOURCE` beside the
  fake lid. `energyWanted` (`energy.go`) is the one decision: the quiet state
  on battery lowers (recording the mode in use first, as section 10); on AC
  power, or with the quiet state left, a recorded mode is restored; with the
  lid, or under a closed lid the power source, not yet heard, nothing is
  written. An unwatched power source is never a battery.
- WHY: the owner ruling of 2026-11-14: with the lid closed on AC power Low
  Power Mode must not be engaged; losing AC power under the closed lid
  engages it; regaining AC power with the lid still closed disengages it.
- Consequences:
  - Supersedes section 10's "the power source never writes the energy mode"
    and "closing always means Low Power Mode". Every change of power source
    is an edge (`WatchPowerSource`), held by state like the lid's, so a
    failed write is retried on the next beat.
  - Durable across a restart by the same record: a successor booting under
    a closed lid writes nothing until both facts are heard, then adopts the
    record on battery or restores it on AC. Before this, a beat that ran
    before the lid was heard restored the record and the lid then lowered
    again.
  - Every engage and disengage is recorded at INFO under
    `daemon.persistentwifi.energy` with the trigger (`lid_closed`,
    `power_source_battery`, `power_source_ac`, `lid_open`, ...) and the
    decision's reason (`because`); every change of power source at INFO
    under `daemon.persistentwifi.power_source`, and a watch that failed at
    ERROR.
  - No wire change: the standing carries no power source.

### 11. The failover rejoins saved networks by name, lid closed only (2026-11-13, no wire change; SUPERSEDED by section 12)

- WHAT: the failover acts only while the lid-closed quiet state stands, and
  only once Wi-Fi has stayed lost for `failoverLossGrace` (20s). Each attempt
  joins the owner's saved networks by name, in the preference order
  `networksetup -listpreferredwirelessnetworks` lists them, with `networksetup
  -setairportnetwork` (macOS supplies each password from the keychain), and
  then the configured hotspot, which is never tried twice
  (`rejoinSavedLocked` and `joinHotspotLocked` in
  `daemon/internal/persistentwifi/failover.go`). It never power-cycles the
  radio. Section 8's radio cycle and settle are removed.
- WHY: at 13:25 on 2026-11-13, with the lid open, the radio cycle ran 71ms into
  an ordinary network switch and cut the owner's connection twice. The cycle
  rested on an unverified assumption, that macOS will not rejoin a known
  network while the lid is closed, and section 8's premise was wrong: the
  saved networks' names ARE readable without location permission; only the
  joined network's name is withheld.
- Consequences:
  - With the lid open the failover does nothing: macOS roams on its own and
    the owner is present. A loss that ends within the grace is recorded at
    DEBUG and acted on not at all.
  - Saved network names are the owner's: the failover's records name how
    many there are and which index was tried, never a name.
  - The loss record (radio power, link summary, lid, quiet state, display,
    energy mode) stays, now written for a lid-closed loss, so the next one
    shows whether macOS powered the radio down or only disassociated.
  - Turning the mode off still leaves the hotspot with `wifi-util disconnect`
    and a radio restart when that does not take; that is the owner's own
    request, not the failover.

### 12. Only known networks in sight, at once; the hotspot left after a minute (2026-11-14, no wire change)

- WHAT: with the lid closed and the mode on, a Wi-Fi interface joined to
  nothing is answered AT ONCE, with no grace: a saved network that a scan sees
  now, in the owner's preference order, then the hotspot. No saved network out
  of sight is tried, and no unknown network ever is
  (`joinVisibleKnown` in `daemon/internal/persistentwifi/failover.go`). While
  the quiet state stands on the hotspot, a saved network that has stayed in
  sight for a minute is joined in its place (`hotspotdwell.go`).
- WHY: on 2026-11-14 an attempt joined every saved network in turn, each
  unbounded and recorded only at DEBUG; it ran three minutes with the lid
  closed, never reached the hotspot, and the owner lost the connection. The
  owner ruled: join only what macOS knows and can see, then the hotspot;
  connect at once with no connection, and leave the hotspot for a known
  network only once it has been in sight for a minute.
- HOW THE NAMES ARE READ: macOS names Wi-Fi networks only to an app allowed
  Location Services, so the scan is agent-repl's Wi-Fi helper app
  (`wifi-helper/`, CoreWLAN), launched with `open` so the permission is the
  helper's own, answering in a file (`scan.go`). The installer builds it
  (`app_build`, rebuilt only when its sources change, so macOS keeps the
  permission) and asks for the permission once (`location_permission`).
- Consequences:
  - locationd serves one instance of the helper's bundle at a time, so an
    instance becomes its client only from the client seat, an flock
    (`wifi-helper/seats.swift`): an ask holds every seat while it waits on
    the owner, and a scan launched meanwhile answers `awaiting_owner` at
    once; a scan waits out at most one scan ahead of it, bounded at 9s. On
    2026-11-16 an ask left waiting held the bundle and every scan timed out
    unheard. A deploy that installs a rebuilt helper stops the replaced
    build's waiting asks (`persistentwifi.StopAsks`), and the next scan asks
    again from the installed helper.
  - A scan is bounded at 20s (one measured 3.9s, two back to back 7.8s). Each join is recorded at INFO
    with its duration, bounded at 30s until those records measure it
    (`org/todo.md`).
  - Without the permission a scan names nothing: it is recorded once at
    ERROR, and only the hotspot is joined.
  - While on the hotspot with the lid closed, the helper scans every 15s; it
    scans once when the quiet state begins joined, to learn whether that is
    the hotspot, and not at all otherwise.
  - Supersedes section 11's saved-network walk and the 20s grace.
