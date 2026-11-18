# Stable sockets for the daemon and the store

Owner rulings of 2026-11-18. The deploy mediator builds this in its own session.

## The defects

- At a daemon handover, Emacs called the outgoing daemon after it stopped accepting (03:10Z deploy: ReportPageUnloading refused on port 52160 0.35s before that workspace's `transferred` push arrived), and calls timed out across handovers with no handler record of them (00:21Z, 02:00Z deploys).
- At a store restart (the owner's requirements install at 09:46:57 local), `store.sock` did not exist for 10s, so 31 shims gave up reading agent books, the sidecar suspended production, and the daemon severed 31 sessions.
- Both have one cause: each socket exists only while its server process runs.

## The design

1. A socket holder per socket. A small process kept alive by launchd (systemd --user on Linux), whose plist and program never change, binds the socket and keeps it open for good. It never accepts. It hands the listening fd, over a control socket (SCM_RIGHTS), to the one server entitled to accept. Since the holder's definition never changes, no install, deploy or service reload ever closes the socket; connects queue in the kernel backlog while no server accepts.
2. The store takes its listener from its holder at boot. Its singleton guarantee moves from "the socket path is live" (singletonsocket.Listen) to a flock on its database, which dies with its holder.
3. The daemon listens on one stable address, advertised in daemon.addr, from its holder.
4. Exactly one daemon is capable of accepting: the canonical daemon. The switch is part of the handover handshake. When the successor acks that it is the official successor, the incumbent stops accepting and closes its listener, and passes the socket to the successor, which then accepts. Connects in the gap queue in the backlog.
5. Forwarding runs only from the successor to the incumbent. A call the successor accepts for a workspace the incumbent still serves is forwarded to the incumbent over its private address. The incumbent exits only after its last workspace transferred, so no call is ever forwarded to a disposed daemon; a forward that finds no incumbent is an invariant violation that fails loudly. The incumbent never forwards, because it never accepts again after the handshake.

## Owner constraints

- Full, structural solutions only; no partial landing. This lands as one finish.
- The deploy mediator creates no workspace; all of this is built in its own session.

## Refinements from the several-daemons plan (docs/investigations/2026-11-16-several-daemons-plan.md)

- Several generations live at once: the newest (canonical) daemon and elders serving their busy holdouts until each frees.
- THE ACCEPT RIGHT IS THE BOOT CLAIM. Only the daemon holding the boot claim (an exclusive kernel lock, daemonaddr) accepts on the shared listener. A daemon takes the listener from the holder only while it holds the boot claim, and stops accepting and closes its copy before it releases the claim (Yield, or exit). Exclusivity is the kernel lock's, so two daemons can never accept at once.
- FORWARDING FOLLOWS THE SERVING ROW. The accepting daemon reads the serving owner of the call's workspace; a call for a workspace another live instance serves is forwarded (reverse-proxied, streams included) to that instance's private listener. Before the yield that is incumbent to successor (workspaces already transferred); after it, newest to elder (holdouts). The target is alive by invariant (a serving row names a live instance: the successor is serving in joining mode, and an elder lives while it serves any workspace); a forward that finds the instance dead is an invariant violation, recorded at ERROR and failed loudly.
- Each daemon keeps a private loopback listener for forwards, recorded per instance in the state root beside its life lock.
- THE DAEMON'S SHARED ADDRESS IS TCP, because the webapp's pages reach the daemon over HTTP and cannot dial a unix socket. Its holder binds 127.0.0.1 once; daemon.addr advertises the holder's address for the holder's life.
- Emacs addresses every call to the one address. The per-workspace daemon resolution (lisp/host.el agent-repl-host-conn, the link's successor promotion) collapses to the one link; a `transferred` push ends a stream that the client re-opens on the same address, which routes to the new owner. The per-daemon address machinery it replaces is removed.

## Build order

1. The holder program (one binary for both sockets), its launchd/systemd entries in requirements.json, and its tests.
2. The store: listener from its holder; singleton by a flock on its database.
3. The daemon: shared listener from the holder under the boot claim; the private listener per instance; forwarding by serving row; the accept loop dropped before the claim is released.
4. Emacs and the other clients: the one address; the per-daemon resolution removed.

## The holder's definition never changes

- A holder is its own requirement kind, `socket_holder`, rendered by its own template (never `rendered_service`, whose shared settings change for every service: the 2026-11-17 ProcessType change re-rendered every plist and restarted the store). Its rendering is pinned by a golden test, so it changes only on purpose.
- On macOS the plist declares the sockets in `Sockets` (the control socket and each held one); launchd creates them at load and keeps them while the job is loaded, so not even a crash of the holder closes one. On Linux a `.socket` unit names each with `FileDescriptorName`, and the holder's service takes them through `LISTEN_FDS`.
- The daemon's TCP port is chosen once, at the first install, and kept in the cache (`sockethold/daemon.port`), so every rendering names the same port.
- A holder's binary rebuilt by a deploy is never restarted into: the running holder keeps serving, and the new binary runs from its next start. Its control protocol is therefore kept stable (sockethold's one-line request, one status byte).

## Progress

- Done: the sockethold library (agentrepl/logging/sockethold), activation (sockethold/activate), the holder program (agent-shim/sockethold), its roster suite.
- Done: the `socket_holder` requirement kind, its pinned renderer and tests; the build target; the store's holder entry; the store taking its listeners (`--holder`) and its database flock; lib-service's load no longer kickstarts after its bootstrap (that kickstart was the 10s outage at every reload).
- Migration: the first install loads the store's holder (launchd takes over the socket paths) and then restarts the store onto it; the entries are ordered holder first.
- Next: the daemon (its holder entry with the tcp socket; the shared listener taken under the boot claim; the per-instance private listener; forwarding by serving row; the accept loop dropped before the claim is released), then the clients (Emacs, webapp, CLI, sidecar) on the one address.

## The daemon's build plan (forwarding replaces the redirects)

Today a call for a workspace this daemon does not serve is REFUSED with a typed redirect (`transferring_away{address}` naming the successor, `not_yet_adopted{address}` naming the incumbent; internal/server/refuse.go fill), and the client follows it (cmd/agent-repld/daemonclient.go successorRedirect, lisp/host.el's transferred and reattach path). With one shared address a redirect cannot work, so the daemon FORWARDS instead, to the same target the refusal names.

- D1. Each daemon records its private address beside its life lock (`daemons/<instance>.addr` in the state root, written when its listener binds, removed at exit; daemonaddr), so any daemon can reach any live instance by name.
- D2. The shared listener. A daemon holding the boot claim (a fresh boot, or a successor once AwaitBootClaim returns) takes the shared listener from the daemon's holder (socket name `daemon`) and serves the same handler on it beside its private listener; daemon.addr advertises the shared address. Before it releases the boot claim (Yield, or exit), it stops accepting on the shared listener and closes it.
- D3. Forwarding in the request wrappers (internal/server/requestlog_server.go serveUnary and serveStream): the call's workspace standing names the target (a workspace transferred away: the instance it went to; one not yet adopted: the instance whose serving row holds it, read with that instance's recorded address). The call is re-issued as the same Connect procedure to the target's private address (unary: CallUnary; server stream: CallServerStream, each message relayed), with the request's headers. A target that is not alive is an invariant violation: recorded at ERROR and failed loudly.
- D4. The clients address the one shared address only: the CLI's successorRedirect, Emacs's per-workspace daemon resolution and successor link, and the webapp's page reload at a successor's address are removed, because no call is ever redirected again.

## Progress (continued)

- Done: D1 (instance addresses), D2 (the shared listener under the boot claim, advertised, stopped first at shutdown), D3 (forwarding by standing, unary and streams, the loop guard), Emacs passing `--holder`, the daemon holder's manifest entry.
- Next, D4, in this order:
  1. The daemon announces the SHARED address to clients: the stand-down announcement (`DaemonShutdownAnnounced.address`, internal/rollout/handover.go) and the web stream's `transferred{address}` name the advertised shared address whenever a holder is in use, while `recordTransfer` keeps the successor's private address as the forwarding target. No client ever learns a private address.
  2. Emacs, on an announcement naming its own primary's address (lisp/daemon-link.el `agent-repl-link--attach-successor` already logs `successor-is-primary` and attaches nothing): a `transferred` push (lisp/host.el `agent-repl-host--transferred`) re-attaches the workspace on the primary link, whose adopt call the incumbent forwards to the successor. Today it would wait on a successor acceptance that never comes.
  3. The primary link's end at the incumbent's exit reconnects to the same shared address, which the successor then accepts; check that path promotes nothing and respawns nothing.
  4. The CLI's successorRedirect and Emacs's successor link are removed once nothing dials a private address.
     - Done for the CLI: every verb dials the shared address and follows no refusal (the unary redirect, the test run's and the redundancy measurement's resends are gone).
     - Emacs's successor link stays: on the shared address the primary's end redials the same address as a pending successor (`agent-repl-link--shared-reconnect`), and its acceptance is the promotion.
     - The webapp's move notice stays: Emacs reloads the page at the primary's shared address once the adopt lands (`agent-repl-host--handover-reload-webview`), so the quiesced page is replaced, never redialed.
  5. e2e: `TestEmacsHandoverTransfersAtFreeness` and the rollout e2e run against a holder, so a handover is proved end to end on the shared address.
