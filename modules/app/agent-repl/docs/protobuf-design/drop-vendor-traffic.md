# Drop the vendor traffic from agent-repl's session

The topbar's connectivity dropdown stated agent-repl's session: how long it
has run, what began it (a login through agent-repl, or this Emacs starting),
and the vendor network traffic counted since. The owner ruled on 2026-10-06:
"drop that network traffic change in its entirety". The session's start, its
cause, and the dropdown's duration stay; only the traffic goes.

## Landed changes

### `frontend.v1.TopbarAgentReplSession` no longer carries traffic

- **What changed.**
  - Fields 4 (`bytes_received`) and 5 (`bytes_sent`) are removed and
    reserved, by number and by name.
  - The message comment states that the session carries no traffic and why
    the numbers are reserved.
  - `started_at_ms` and the `began` oneof (`login`, `editor_start`) are
    unchanged.
- **Why, in the owner's terms.**
  - The sampler that produced the counts (`daemon/internal/vendortraffic`)
    opened one `com.apple.network.statistics` kernel-control socket per vendor
    process, with a 1 MB receive buffer and a subscription to a removal notice
    for every socket closing on the machine.
  - It opened them through `unix.Socket` without close-on-exec, so every
    daemon generation leaked them into its successor and its children, where
    nobody read them.
  - They filled, and are the inferred cause of the kernel's mbuf exhaustion
    (`mbuf alloc failed (err 12)`) that froze the owner's keyboard three
    times.
- **Consequences, including those accepted as costs.**
  - The dropdown draws one row (the span labeled by its cause); there is no
    traffic line and no byte formatting in the webapp.
  - The daemon's wsm drops the persisted columns in a new layout (26,
    `agent_repl_session_drop_traffic`). The step is BREAKING: the build
    before it reads and writes both columns, so a deploy across it restarts
    rather than hands over. Accepted, because the owner asked for the feature
    gone entirely rather than left as dead columns for a later two-deploy
    removal.
  - The layout-21 step keeps its text as shipped (with the traffic columns),
    so a file older than layout 21 still migrates through 21 and then 26; a
    test pins that the migrated table's columns equal a fresh file's.
  - Fields 4 and 5 are never reused: a skewed peer still on the old schema
    would read any new field there as byte counts.
  - Removed alongside: the `vendortraffic` package, the fleet's `ShimPIDs`
    (its only consumer was the sampler), the session tracker's
    `AddTraffic`/`FlushTraffic` and pending counts, the topbar resolver's
    `bytes_received`/`bytes_sent` log fields, and the webapp's
    `formatTraffic`/`formatBytes`.
