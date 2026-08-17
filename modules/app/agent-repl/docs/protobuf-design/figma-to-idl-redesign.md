# DESIGN: the figma→idl redesign of the agent-repl contract

## The problem, in the user's terms

`frontend.v1` is the figma→idl specification for the frontend: one file per UI
component, each component's props one message, resolved by the server and
rendered verbatim. `agentrepl.v1` is the API the webapp actually talks to.
Therefore `agentrepl.v1` should be COMPOSED of `frontend.v1` messages, passed
along component-dedicated endpoints — the sidebar talks to an endpoint that
ships `frontend.v1` sidebar messages, likewise the topbar, the footer, the feed
— not a god-endpoint that handles all shapes.

That is NOT how it works today. `service AgentRepl` is a pure command surface:
34 unary RPCs, 30 of them returning an empty `Success`, none returning a
component view. With the one multiplexed push stream deleted (see the
superseded record), no `frontend.v1` component data has any path to the webapp
at all. The composition rule is nowhere in force.

The redesign walks every surface, ONE `.proto` FILE AT A TIME, in the order
`frontend.v1` → `agentrepl.v1` → `conversation.v1` → `shim.v1` → `store.v1` →
`state.v1`. For `agentrepl.v1` the RPC inventory is hashed out explicitly first,
then the shapes.

## The record this supersedes

`proto/DESIGN-protobuf-surfaces.superseded.md` is the previous design record,
kept as a backup. Every decision it records STANDS unless an entry below
reopens it by name. In particular the following are settled and are NOT
re-litigated here:

- The six surfaces and the package-boundary-is-surface-boundary rule.
- Package names encode ownership, not routing; the drawn/called test between
  `frontend` and `agentrepl`.
- `agentrepl.v1` is a Connect service, not gRPC, because of the clients (an
  xwidget WebKit view and elisp).
- Every endpoint owns its request and response, one `endpoint_` file each.
- Every response is a two-arm `oneof { success; error }`; errors are typed
  messages in band, never status codes; the async boundary keeps
  `FailureCardView` as a push.
- One canonical form per message; import the encompassing message; re-spelling
  in its four forms is a defect; depth is not synthesis.
- The one multiplexed push stream is REVERSED: one server-streaming endpoint
  per UI component, with the two surviving ordering failures (typing-cut vs
  conversation-delta; cross-stream workspace references) accepted by the user
  as the price and owed a conventions-stage answer.

## Landed changes

### `agentrepl.v1` starts from a clean slate: every RPC and every `endpoint_*.proto` is deleted

**What changed.** All 34 `endpoint_*.proto` files under `src/agentrepl/v1/`
are deleted, and `service AgentRepl` is emptied to `{}`. `service.proto`
survives as the file the RPCs will be re-added to, one at a time, each with
its own `endpoint_<snake_case_method>.proto`. `shared.proto` is NOT deleted:
it is the only `agentrepl.v1` file anything outside the package imports
(`frontend/v1/footer.proto` reads it for `MergeStatus`, `MergeDequeueOffer`,
`HibernationDetail`), and its fate is decided at the `frontend.v1` footer step
and the `agentrepl.v1` conventions step, not by this deletion.

**Why, in the user's terms.** We are going to end up nuking a lot of
`agentrepl.v1` RPCs and their `endpoint_*` files anyway; what is there now is
so far removed from what we want to land that it is better to start from
scratch than to confuse ourselves with preexisting junk.

**Consequences, stated so they are not silently lost.**

- The old `service.proto` header carried normative prose that is NOT
  automatically carried forward: the "no paint attestation on this service"
  invariant, the `request_id`/`workspace`/`client_id` envelope-field
  semantics, and the protojson-on-the-wire note. Each re-enters at the
  `agentrepl.v1` conventions sub-stage as its own question. None is settled
  by having once been written.
- The 34 deleted files were the ONLY spelling of the per-method error arms
  (`Refusal*` messages derived from daemon handlers, per the superseded
  record's "Per-method errors, DERIVED not invented"). That derivation
  evidence — which handler emits which refusal — is in the superseded
  record's prose and in git history (`4b0d6aa4c^`), not on disk. When an
  endpoint is re-added, its error arms are re-derived, and the old file is
  reference material, not a template.
- The build was already broken by the transport reversal; this widens the
  break to every daemon, webapp and elisp site that named a request or
  response type. That is intended, per the land-whether-or-not-it-breaks rule.
- Ten deleted endpoint files carried the comment "see
  DESIGN-protobuf-surfaces.md for why a directory is not available here" (the
  `--go_opt=paths=source_relative` argument for the `endpoint_` prefix). The
  argument still holds and lives in the superseded record; re-added files
  cite the new record.
