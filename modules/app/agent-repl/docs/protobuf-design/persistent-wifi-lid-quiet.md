# Persistent wifi mode: the lid-closed quiet state

Owner request, 2026-11-11. While the lid is closed with persistent wifi mode
on, the machine does no work for a screen nobody sees, so the most agent work
is done per unit of battery energy. One daemon-owned state, "lid closed and
mode on" (the QUIET state), freezes a configured list of applications, stops
agent-repl's own drawing (daemon view resolution, webapp animation and
polling, Emacs repaint timers), and sleeps the display instead of dimming it.
Every effect is undone when the lid opens or the mode goes off.

Design mode: AUTONOMOUS.

## Assumptions (step 1)

- Objective: the battery goes to agent work while the lid is closed.
- Built: a quiet decision in `daemon/internal/persistentwifi`, an application
  freezer with a durable frozen set, a view gate the four view resolvers
  (sidebar, footer, topbar, feed tails) follow, a display sleep replacing the
  lid dim, Emacs and webapp idling, a Spotlight exclusion.
- Frontend: the quiet state rides `agentrepl.v1.PersistentWifiState`, which
  Emacs takes on the daemon-level stream. The webapp pages learn it from
  Emacs through the host page (the same channel presence rides), not from a
  frontend.v1 view, because the views stop resolving while quiet. The topbar
  chip needs no new element: nobody sees it while the lid is closed, and the
  quiet state is over by the time anyone can.
- Systems: proto (agentrepl.v1), daemon (persistentwifi, a new freezer
  package, the four resolvers, the composition root), Emacs
  (`lisp/persistent-wifi.el` and the repaint timers), webapp (host, presence,
  the shared ticker), the fake daemon and the integration harness.
- Non-additive changes: `PersistentWifiState.lid_dim` (field 9) and
  `PersistentWifiLidDim` are removed; the display step's `dimmed`,
  `restored` and `tool_missing` arms are removed. The brightness tool is no
  longer used except to undo a dim a previous build left in force.

## Iteration sequence

1. `agentrepl.v1.PersistentWifiState` (the shared standing).
2. `agentrepl.v1.UpdatePersistentWifiModeDisplay` (the step outcome).

## Landed changes

### 1. The quiet state on the standing

WHAT: `PersistentWifiState.lid_dim` (9) is reserved; `optional
PersistentWifiQuiet quiet = 10` is added. `PersistentWifiQuiet` carries the
frozen applications (name and process count, configured order) and a
`display` oneof (`display_asleep`, `display_awake` with the failure detail).

WHY: the quiet state is one fact the daemon owns, and every reader that idles
for it (Emacs, through it the webapp pages) needs it on the stream it already
takes. The frozen applications are what the user would ask about ("why is
Slack not answering?") and what the INFO record names.

Consequences:
- UNSET whenever the lid is not heard closed: an unheard or unwatched lid
  never freezes anything, so a failed lid watch can only ever thaw.
- The display oneof inside the quiet message, not beside it, because the
  display is asleep only as part of the quiet state.
- The frozen set itself is kept durably by the daemon (pids per
  application), outside the proto: the proto carries what readers draw, the
  durable record carries what a later daemon must thaw.
- Readers learn the state through Emacs (the daemon-level stream); the
  webapp is told by the host page, so a webapp page never needs a new stream.

### 2. The display step's outcome

WHAT: `UpdatePersistentWifiModeDisplay` reserves arms 1 to 3 (`dimmed`,
`restored`, `tool_missing`) and adds `slept` (6) and `woke` (7). `failed` and
`unchanged` stand.

WHY: the display sleeps for the closed lid (`pmset displaysleepnow`, no sudo)
and wakes when it opens, at the brightness it had, so there is no level to
keep and no optional tool to be missing.

Consequences:
- A dim a previous build left in force (the wsm `persistent_wifi_lid_dim`
  row) is restored once at boot with the brightness tool and the row is
  cleared; nothing ever writes the row again.
- Waking uses `caffeinate -u -t 1` (declared user activity), a system tool,
  so no step can report a missing tool.

## Sweep

Package-wide sweep after both increments: `PersistentWifiLidDim`,
`UpdatePersistentWifiModeDisplayDimmed`, `...Restored` and `...ToolMissing`
are referenced by nothing and were removed with their arms. Nothing else is
obviated.
