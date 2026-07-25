# Design: invert the shim↔daemon transport (topology C)

Status: **proposed**, not implemented. Supersedes nothing until accepted.

## The problem

Today the **shim listens** and the **daemon dials**:

- The daemon spawns `node …/main.js --uds-socket …/session-<id>.sock`.
- The shim creates that socket file and listens on it.
- The daemon dials the path.

The dialer therefore starts *before* the listener exists. For the ~300ms the
`node` process takes to boot and call `listen()`, the socket file is absent and
the dial fails with `ENOENT`. Everything downstream is machinery compensating
for that inversion:

1. A reconnect loop that retries the dial every 100ms until the file appears.
2. `ReattachDecision` — a probe that dials the path and reads a frame,
   requiring a `ShimHello`, to answer "is a shim alive for this session?".
3. One socket file per session, left on disk, needing lifecycle management.
4. The bring-up race that rejected prompts with `no live shim connection`
   (fixed by the readiness gate, but the race itself remains).

## The change

**The daemon listens on one socket. Shims dial out to it.**

- The daemon owns a single listening socket, as it already does for its
  frontend (`daemon-frontend.sock`).
- Each spawned shim dials that socket and announces itself with `ShimHello`,
  which already carries the session identity the probe reads today.
- The daemon accepts N independent connections and keys them by session id.

A listening socket is a rendezvous point, not a channel: every `connect()`
yields a separate, private, bidirectional stream via `accept()`. Many shims on
one socket is ordinary server behaviour, not contention.

### This is the pattern the system already uses twice

1. **The shim-store.** Every shim dials the same `store.sock` — two connections
   each (producer + subscription) — and the store classifies each connection by
   its first frame. Eleven shims were connected to it simultaneously today.
2. **The daemon's frontend.** `daemon-frontend.sock` is one listening socket
   serving Emacs and every webapp client at once.

C makes the shim↔daemon link work the way the shim↔store link already does.

## What it removes

1. **The spawn race.** The dialer is now the newly spawned child and the
   listener is the long-running daemon, which has been listening for ages.
   Retrying becomes the dialer's job, which is where retry belongs.
2. **`ReattachDecision` entirely.** "Is there a live shim for session X?"
   becomes a lookup of currently-connected shims rather than a filesystem dial
   plus handshake read.
3. **Per-session socket files.** One daemon socket replaces N.
4. **The `ENOENT` reconnect churn.**

It moves complexity rather than adding it: the shim's server code becomes
client code, the daemon's client code becomes server code, and the reconnect
loop the daemon has today moves to the shim.

## What it must preserve: §4.4 survival

> claude-shim listens on its own `session-<id>.sock` and OUTLIVES a dead
> daemon: a UDS disconnect does not end the SDK turn.

The requirement is that **the turn survives**, not that the shim holds the
listening socket. Under C the shim keeps running its SDK turn while
disconnected and reconnects when the daemon returns — arguably stronger, since
a surviving shim actively re-finds the daemon instead of waiting to be
rediscovered by a probe.

The shim needs the reconnect loop the daemon has today: retry with backoff,
indefinitely, because the daemon may be down for an arbitrary period.

## What it loses, and the replacement

Under the current topology the **filesystem enforces session uniqueness for
free**: only one process can `bind()` `session-<id>.sock`, so two shims can
never claim the same session. `EADDRINUSE` makes it unreachable.

Under C there is no path to bind. Both shims connect, both announce the same
session id, and nothing has stopped them. The realistic path is exactly what
the reattach probe exists to prevent:

1. A shim for `s_abc` is alive and mid-turn.
2. The daemon dies and restarts.
3. The fresh daemon does not know that shim exists.
4. A prompt arrives for `s_abc` and the daemon spawns a shim for it.
5. Two shims now run one conversation — two writers on one transcript, the
   thing `supersedeResumeConflicts` exists to prevent at the session level.

"Do I already have a connection for `s_abc`?" does not close this: on a fresh
boot the survivor may not have dialled in yet, so the answer is *no* when the
truth is *not yet*.

### Replacement: a per-session `flock` lock file

The shim takes an exclusive `flock` on `…/sock/session-<id>.lock` at startup
and holds it for its lifetime. The daemon tests the lock before spawning.

This restores exactly the guarantee `bind()` was providing, made explicit and
separated from transport:

- **Kernel-enforced.** Mutual exclusion is not advisory bookkeeping the daemon
  has to get right.
- **Auto-released on death.** The kernel drops the lock when the holder exits,
  however it exits. No stale state, no cleanup path, no PID-reuse hazard.
- **Synchronously testable.** The daemon can answer "is a shim alive for this
  session?" without waiting for anything to dial in — which is precisely the
  boot-window gap that connection-tracking alone cannot cover.

Rejected alternatives:

1. **A settling window on boot** — wait before spawning so survivors can dial
   in. Time-based guessing, and wrong on both sides.
2. **Resolve after the fact by supersede** — accept both, stand one down.
   Reuses existing policy but picks the wrong victim in the case that matters:
   the survivor holds the live turn, and killing it is what §4.4 forbids.
   `ShimHello` does carry `turn_in_flight` so it could prefer the busy one, but
   resolving a collision that was preventable is the weaker design.
3. **PID liveness in the registry** — simple, but PID reuse is a real hazard
   and it leaves stale state to reap.

## Decisions

**Lock file location.** `~/.cache/agent-repl/run/session-<id>.lock`. A lock is
not a socket, so it gets its own directory rather than living among them —
matching the usual split between `/var/run` locks and socket paths. Created by
the daemon's cache-root setup alongside `sock/` and `store/`.

**Lock held but nothing dials in.** The daemon waits a bounded period for a
connection claiming that session, and if none arrives it **fails the command
loudly with a log naming the session and the lock holder**. It does NOT
silently respawn: the lock says a shim is alive, so spawning a second one is
the exact duplicate this mechanism exists to prevent, and killing the holder
would destroy the in-flight turn §4.4 protects. A shim holding the lock without
talking is a bug to surface, not a state to paper over.

**The daemon's socket path is fixed and well-known**, exactly like
`daemon-frontend.sock` today. There is one daemon (AGENTS.md forbids
hand-spawning a second on the configured address), so a surviving shim
reconnects to the same path it was given at spawn. Nothing moves.

## One deployment note

The shim and the daemon change together, so a shim built before this change is
listening on its own socket and will never dial the daemon. Any shim still
running across the upgrade is orphaned: the new daemon does not know it, and it
is waiting for a dial that will not come.

This is a one-time deploy concern with no design consequence. Those shims are
hibernated by the idle sweep within minutes, and a session whose shim is gone
respawns on its next prompt. Worth doing the deploy when nothing is mid-turn.

The store and the sidecar are unaffected — they talk over `store.sock`, which
this change does not touch.

## Relationship to the readiness gate

Independent, and the gate is required either way. Under C a caller must still
wait for the spawned shim to dial in and complete its hello before sending, so
`ensure()`'s readiness contract stands unchanged. The gate does not become
redundant, and C does not weaken it.
