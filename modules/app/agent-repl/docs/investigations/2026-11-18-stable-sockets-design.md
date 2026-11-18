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
