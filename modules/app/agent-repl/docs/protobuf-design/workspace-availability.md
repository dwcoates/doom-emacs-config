# Workspace availability: open each workspace only once the daemon has its session

Emacs opened every registered workspace the moment the roster arrived, before
any shim existed behind it. On 2026-09-30 a cold start with the store booted
out of launchd left all five workspaces teal with blank feeds and an
unanswered prompt for over a minute. The owner wants the editor to open the
workspaces only once the daemon is online, in registry order, each one only
after the daemon says that workspace's shim is connected.

## Core design principles

- **The daemon decides availability; the editor only waits on it.** The owner's terms: "when the workspace is available, it's available, and the daemon determines that by waiting for the shim connection."
  - Consequences: availability is a resolved roster fact, so Emacs derives nothing from status arms or links.
  - What it does not claim: it does not make the daemon decide the open ORDER, which is the editor walking the registry.

- **Workspaces open strictly in registry order.** Emacs opens row N only once rows 1..N-1 are resolved; a later row that is ready first still waits (owner ruling 2026-09-30, answer 1 and 5). The last-selected workspace does not jump the queue.

- **The daemon starts every shim at once.** Bring-up is concurrent; the order constraint is the editor's only.

- **A start that fails opens blue, and a hibernated workspace is woken first.** A failed start opens with its `start_failed` arm; the boot wakes hibernated workspaces before they can become available (owner answers 2 and 3).

## Landed changes

### `RosterRow.availability` (frontend/v1/sidebar.proto)

- WHAT: a required `RosterRowAvailability availability = 38` on `RosterRow`, a oneof of empty arms `pending`, `available`, `unavailable`.

- WHY: the row's status arm cannot carry it. A perspective-less workspace resolves `inactive`, which dominates every other arm, and at cold start no workspace has a perspective yet, so the status says nothing about whether a session is behind the row.

- Daemon semantics (the sidebar resolver derives it from facts it already holds):
  - `available` is the shim link having connected at least once under this daemon (`wsState.everConnected`), and it is sticky.
  - `unavailable` is the link dead with `everConnected` false: the same condition that draws `start_failed`.
  - `pending` is everything else.

- Boot's UNDETERMINED workspaces (lock or socket probe could not tell) produce no link facts on their own, so they would stay `pending` forever and block every later row. The boot therefore marks each one's link dead on the views, which draws `start_failed` (blue) and resolves `unavailable`.

- Consequences, accepted:
  - The elisp decoder (`lisp/wire-roster.el`) is strict about unknown keys, so the daemon and Emacs must be deployed together; an old Emacs rejects every roster push from a new daemon.
  - The rule is uniform, not startup-only: a workspace created at runtime gets its tab once its shim connects, a moment later than before.
  - A closed row also carries the field, and the editor ignores it there because a closed row gets no tab.

- Evidence: `sessionlock`/boot paths verified by reading `daemon/internal/boot/sequence.go` (undetermined set) and `daemon/internal/workspace/sessions.go` (every failed bring-up and every kill calls `Sinks.Sidebar.OnLink(LinkDead)`; adoption and spawn call `OnLink(LinkConnected)`).

### Correction: `pending` means a bring-up under way, not "no link yet"

- WHAT: `pending` now means a bring-up of the workspace's session is under way, and `available` also covers a workspace with no bring-up under way and none failed.
  - The earlier entry defined `pending` as "no link has connected and none has died", which is retracted.

- ROOT CAUSE of the error: the first definition assumed every open workspace has a session coming, but RegisterWorkspace starts none for a freshly registered workspace (`daemon/internal/workspace/register.go`, `reviveRecordedConversation` returns early when `created`), and its session starts only on first use. Such a row was `pending` forever, so it and every row after it never got a tab.
  - The sandbox e2e suite caught it: five Emacs e2e tests failed on "await the workspace to appear in Emacs's registry".

- Daemon semantics now:
  - The sidebar resolver tracks a per-workspace bring-up-in-flight flag.
  - The boot raises it for every workspace it names for bring-up before it serves, and the bring-up clears it on every path.
  - `Fleet.start` raises it for its own duration.
  - Availability is: `available` if the link ever connected; else `pending` while a bring-up is under way; else `unavailable` if the link is dead; else `available`.
