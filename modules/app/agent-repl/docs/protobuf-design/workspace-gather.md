# Workspace gather

One workspace asks many workspaces the same question and gets their answers
back as ONE update (owner ruling, 2026-11-10). It replaces the `/standup`
skill's flow, where every target replied on purpose with its own
`workspace prompt` and the caller took one turn per reply.

## Assumptions (autonomous design)

- OBJECTIVE: a caller prompts N workspaces and is woken once with every
  answer, then once per late answer.
- WHAT IS BUILT: a daemon gather service (durable in wsm), two agent API
  rpcs, two CLI verbs (`workspace gather`, `workspace gather-status`).
- FRONTEND: none. Gather is agent API only, so figma→idl does not govern it.
- SYSTEMS: the daemon only (server, wsm, prompt queue, merge orchestrator,
  feed resolver, CLI). No shim, store, sidecar, webapp or elisp change.
- NON-ADDITIVE CHANGES: none on the wire. In the daemon, every site that moves
  a prompt to a new turn records the move (a turn succession).

## Iteration sequence

1. Transport: ConnectRPC, the existing `agentrepl.v1.AgentRepl` service.
2. Inventory: `GatherWorkspaces`, `GetGatherStatus`, both in the AGENT API
   suite.
3. Conventions: the canonical `oneof result { success; error; }`.
4. Shapes: the shared gather shapes (`gather.proto`), then each endpoint.

## Landed changes

### `gather.proto`: the gather's shared shapes

- WHAT: `GatherRef`, the target identity, the refusal cause, the resolved and
  outstanding target, `GatherStatus`, and `GatherNotice` with its two arms
  (first update, later answer).
- WHY: the verb's answer, the status read and both notices describe the same
  targets, so one set of shapes serves all four.
- CONSEQUENCES:
  - A notice reaches the caller as a prompt (origin
    `PROMPT_ORIGIN_WORKSPACE_NOTICE`, the workspace notice path), and its
    words carry the `GatherNotice` as protojson. The caller reads typed arms
    from that JSON rather than parsing sentences.
  - A refused target carries the `SubmitPromptError` verbatim when the
    submission itself refused it, so `workspace prompt`'s refusals are reused
    rather than respelled. The gather adds only what it checks before
    submitting: unknown name, closed, the caller itself, a question or
    permission waiting on the user, a merge in progress.
  - Outstanding reasons are read when a notice or status is made, from the
    target's durable state, so they never go stale in storage.

### `gather.proto`: the `ambiguous_name` refusal

- WHAT: `GatherRefusal.ambiguous_name`, carrying every open workspace with the
  name.
- WHY: the registry does not hold names unique, so a name can match several
  open workspaces, and the daemon resolves targets by name.
- CONSEQUENCES: a name is resolved among OPEN workspaces first; only when none
  is open does it answer `closed` (some closed workspace has it) or
  `unknown_workspace` (none does).

### `endpoint_gather_workspaces.proto`

- WHAT: the request names targets by workspace NAME, carries the prompt as a
  `conversation.v1.UserSaid`, `as_user`, and a client-minted idempotency key.
- WHY: names, not refs, because an unknown or closed target must be reported
  with its typed reason, and a client can only mint refs for workspaces its
  roster draws.
- CONSEQUENCES:
  - A retry with the same key answers the gather it already made and submits
    nothing twice (each target's submission key derives from it).
  - A gather that accepted no target is not made: the answer is the error arm
    `none_accepted` carrying every refusal.

### `endpoint_get_gather_status.proto`

- WHAT: reads one gather's `GatherStatus` by its ref.
- WHY: the caller may want the answers outside a notice.

## How a target's answer is found (daemon design, not wire)

- The prompt's own turn is recorded per target. Every site that moves the
  prompt to another turn writes a durable TURN SUCCESSION (wsm
  `turn_successions`): a re-hold (withdrawn join, blocked try), a folded
  prompt resubmitted after its turn failed, a resumption after a stand-down,
  a merge's displaced turn put back, and a fold into the running turn.
- The fold is recorded from the feed resolver's stream, which tells the fold
  before the joined turn's answer, so the two can never arrive out of order.
  The other successions are written before the new turn is delivered, so its
  answer always follows them.
- The answer is the feed's concluded answer for the turn at the chain's end.
- REJECTED: recording the fold from the prompt queue's `onFolded`. It runs on
  the session watcher's path while the feed tells the answer on its own path,
  so the answer could be seen before the fold.
- REJECTED: delivering gathered prompts DEFERRED (never joining). It would
  make the join case impossible, but the owner's contract names a prompt that
  joins a running turn as a case the daemon follows.
