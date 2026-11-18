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
- Next: the `socket_holder` requirement kind and its renderer and golden test; the store's holder entry; the store taking its listeners and its database flock; then the daemon.
