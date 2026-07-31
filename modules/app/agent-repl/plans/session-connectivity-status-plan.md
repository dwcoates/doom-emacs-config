# Session connectivity, session status, and scoped runtime faults

Design and implementation plan. Nothing in this document is implemented merely
by checking it in.

## Problem statement

The SSM currently resolves one workspace render state from several append-only
"axes." Two of those axes make incompatible claims:

- The legacy connectivity projection calls `wired` proof that the complete session route is
  current and operational.
- The legacy impairment projection can remain open while the legacy connectivity projection still reads `wired`.

That is not a valid pair of current facts. In practice `wired` means only that
the most recent bring-up gate succeeded, while a newer degraded row masks that
historical assertion through precedence.

The legacy impairment projection is also too coarse. A failure card is keyed by session and
component, but the SSM reduces every failure to one workspace-wide latch. This
permits:

1. An obsolete session controller to degrade its replacement.
2. One component's recovery to clear another component's active fault.
3. A missing recovery edge to leave a workspace degraded forever.
4. Feature-local or command-local errors to paint the entire session as
   unavailable.

The observed failures in
`slack-core-chess-ai-cxo` and `verify-brilliant-fanout-uen` have exactly the
first and third shape. A rejected handshake wrote a workspace-wide degradation
without session-controller identity. A later controller became operational,
but nothing retired the obsolete fault.

## Settled vocabulary

The user-facing model has two independent dimensions:

### Session connectivity

Answers: can this session be used reliably now?

Closed states:

- `hibernated`: intentionally offline and recoverable on demand.
- `connecting`: establishment or re-establishment is in progress.
- `operational`: the current controller generation passed readiness and has no
  connectivity-impacting fault.
- `degraded`: the current controller generation exists, but at least one
  connectivity-impacting fault is open.
- `unavailable`: no operating controller route remains because establishment
  or operation failed.

### Session status

Answers: what is the session doing?

Closed states:

- `ready`
- `thinking`
- `permission`
- `done`
- `interrupted`
- `vendor-blocked`
- `monitoring`

`monitoring` remains derived from live background-task count. A connectivity
state other than `operational` outranks session status for primary rendering,
but the status fact remains queryable for diagnosis.

### Runtime faults

Runtime faults explain connectivity. They are not a third competing workspace
status axis.

Each open fault is identified by:

- absolute workspace
- agent-repl session ID
- session-controller generation ID
- component
- stable fault type

A closing edge closes only the matching open fault. A fault belonging to a
retired controller generation can never affect its replacement.

## Required invariants

1. `operational` means the current controller generation passed readiness and
   has no open connectivity-impacting fault.
2. A connectivity-impacting fault moves connectivity away from `operational`
   in the same serialized state transaction that records the fault.
3. Closing the final connectivity-impacting fault returns an attached,
   ready controller generation to `operational`.
4. Closing one component's fault cannot close another component's fault.
5. Replacing a controller generation makes every fault from the retired
   generation ineligible for current connectivity resolution.
6. A rejected handshake from a non-current session or generation cannot mutate
   the current workspace's connectivity.
7. Feature-local, command-local, and turn-terminal failures do not become
   connectivity faults merely because they use the failure-card transport.
8. Every frontend receives the same resolved connectivity and session status.
9. No renderer reconstructs either dimension from lower-level fields.
10. Every invariant violation fails loudly and writes one canonical,
    identity-complete error record.

## State storage

### Controller generations

Mint an opaque controller-generation ID in
`sessioncontroller.Manager.bringUp`. Thread it through the controller,
consumer, readiness, fault, and SSM calls. The generation is daemon-local, but
its identifier is persisted on state and fault rows so an obsolete event is
queryably attributable.

Persist the current connectivity lifecycle as a dedicated append-only table:

```sql
CREATE TABLE session_connectivity (
  workspace TEXT NOT NULL,
  agent_repl_session_id TEXT NOT NULL,
  controller_generation_id TEXT NOT NULL,
  state TEXT NOT NULL,
  cause_kind TEXT NOT NULL,
  at INTEGER NOT NULL,
  PRIMARY KEY (workspace, at)
);
```

Only `hibernated`, `connecting`, `operational`, and `unavailable` are written
as lifecycle edges. `degraded` is derived from an operational lifecycle row
plus active connectivity-impacting faults for the same controller generation.

### Fault windows

Persist component-scoped fault edges:

```sql
CREATE TABLE session_fault (
  workspace TEXT NOT NULL,
  agent_repl_session_id TEXT NOT NULL,
  controller_generation_id TEXT NOT NULL,
  component TEXT NOT NULL,
  fault_type TEXT NOT NULL,
  impact TEXT NOT NULL,
  open INTEGER NOT NULL,
  cause_kind TEXT NOT NULL,
  at INTEGER NOT NULL,
  PRIMARY KEY (
    workspace,
    controller_generation_id,
    component,
    fault_type,
    at
  )
);
```

`impact` is a closed enum owned by the daemon:

- `connectivity`
- `feature`
- `command`
- `turn-terminal`

The resolver considers only the newest edge for each
`(controller_generation_id, component, fault_type)` key. Only open
`connectivity` faults belonging to the current controller generation derive
`degraded`.

Do not silently infer impact from free-form reason text. Either carry a typed
impact on the originating protocol or classify a closed component and fault
type at one daemon-owned boundary. Unknown values fail loudly.

### Session status

Retain the append-only session-status lifecycle, but rename the current
session-status lifecycle vocabulary and query helpers. Move `dead` out of session status:
terminal controller or session loss contributes `unavailable` connectivity.

Legacy wired and degraded rows remain immutable historical evidence. A schema
versioned migration creates the new tables and seeds no fabricated live
controller generation. At daemon boot every workspace begins `hibernated`
until a current controller generation establishes itself.

## Producer classification

Audit every current `DegradedState` producer and assign exactly one impact:

- Store subscription or persistence outage: `connectivity`.
- Daemon-to-shim heartbeat loss: connectivity lifecycle moves to
  `connecting`; the link's failure card explains the cause.
- Rejected handshake from a stale or mismatched controller:
  connection-scoped rejection only. It cannot mutate workspace connectivity.
- Bring-up gate failure: connectivity moves to `unavailable`.
- SDK stream death: turn-terminal failure followed by the appropriate
  connectivity or session-terminal lifecycle edge.
- Model-catalog publication failure: `feature`.
- Interrupt or command rejection: `command`.
- Converter or file-plane loss: classify from the actual user-visible
  capability lost. Do not use a universal degraded default.

The audit must enumerate every producer in tests. Adding a new producer without
an explicit impact classification is a compile-time or test-time failure.

## Resolver and wire contract

Replace the single opaque workspace render state as the authoritative wire
fact with a composite state:

```protobuf
message WorkspaceState {
  string workspace = 1;
  string session_id = 2;
  SessionConnectivity connectivity = 3;
  SessionStatus status = 4;
  string controller_generation_id = 5;
  repeated RuntimeFault active_faults = 6;
  // Existing merge and task fields retain their assigned numbers or move only
  // through an explicit coordinated protocol migration.
}
```

Exact field numbers must be selected against the current proto before editing.
The sketch above expresses ownership, not reserved-number authority.

The daemon alone resolves connectivity and status. Emacs and the webapp adopt
the composite verdict unchanged.

During coordinated migration, regenerate all checked-in Go and TypeScript
bindings. Do not add compatibility fallbacks. A protocol mismatch fails
loudly through the existing readiness and wire-version mechanisms.

## UX projection

The tab bar, sidebar, and footer derive one primary presentation:

| Connectivity | Primary presentation |
|---|---|
| `hibernated` | teal, sleeping, no error |
| `connecting` | blue, starting, animated |
| `operational` | session-status color and word |
| `degraded` | blue, impaired, active fault summary |
| `unavailable` | blue, unavailable, not animated |

The footer and failure cards name the component and concrete cause. Avoid
backend vocabulary such as "legacy connectivity projection" or "legacy impairment projection" in user-facing
text.

When connectivity is not operational, retain but visually subordinate session
status. For example, a transport fault during a turn can report "impaired"
with secondary status "thinking" rather than deleting the turn fact.

## Observability

Every lifecycle and fault edge logs:

- workspace
- agent-repl session ID
- controller-generation ID
- component and fault type when applicable
- impact
- prior state
- next state
- branch decision
- cause

The SSM resolution log records:

- selected controller generation
- lifecycle top
- active connectivity-fault keys
- derived connectivity
- session status
- final UX projection

The debug state report and state-investigation runbook must expose both
dimensions and every active fault. A database row without controller and
component identity is a logging and state-observability defect.

## Tests

Add focused tests for:

1. A healthy ready controller resolves `operational + ready`.
2. A turn resolves `operational + thinking`.
3. One store fault resolves `degraded` while preserving `thinking`.
4. Closing that store fault restores `operational`.
5. Two simultaneous component faults remain degraded after only one closes.
6. A feature fault creates a card but leaves connectivity operational.
7. A command fault creates a card but leaves connectivity operational.
8. A stale session handshake cannot affect the current generation.
9. A stale generation of the same session cannot affect its replacement.
10. A controller replacement makes old open faults ineligible.
11. Bring-up failure resolves unavailable.
12. Hibernation resolves hibernated without fabricating a fault.
13. Daemon restart resolves hibernated until a new generation is operational.
14. Emacs tab bar, webapp sidebar, and footer consume the same composite state.
15. Error paths log full identity and perform no partial state mutation.

The two observed workspace histories become regression fixtures:

- an obsolete `turn handshake has no matching live session controller` edge
  followed by a successful new generation
- a successful readiness edge after a historical connection degradation with
  no recovery edge

Both must resolve operational for the new generation without deleting or
rewriting historical evidence.

## Implementation sequence

1. Add typed connectivity, status, fault-impact, and controller-generation
   vocabulary.
2. Add schema migration and append/query APIs.
3. Thread controller-generation identity through the session controller and
   every fault producer.
4. Classify every current degraded producer.
5. Replace SSM resolution with composite connectivity and status resolution.
6. Update frontend protobufs and regenerate bindings.
7. Update Emacs state adoption and rendering.
8. Update webapp state adoption, sidebar, footer, and failure summaries.
9. Extend debug reports and the state-investigation runbook.
10. Add unit, integration, cross-language, and observed-history regression
    tests.
11. Run daemon coverage, logging-density review, the mandatory diff audits,
    and `modules/app/agent-repl/bin/test-all.sh`.
12. Build the frontend and daemon, bounce the daemon through the approved Emacs
    path, and verify both formerly stale workspaces from canonical state and
    logs.

## Completion criteria

The work is complete only when:

1. The words `agent axis`, `wired axis`, and `degraded axis` are absent from
   active implementation and operator documentation except historical design
   discussion.
2. No current connectivity claim can coexist with a contradictory critical
   fault.
3. Fault opening and closing are component- and generation-scoped.
4. Obsolete sessions and generations cannot affect current workspace state.
5. All clients render identical connectivity and session status.
6. The full suite passes with no timing regression.
7. The deployed daemon reports the new source revision.
8. The two affected workspaces no longer resolve from obsolete degraded rows.
