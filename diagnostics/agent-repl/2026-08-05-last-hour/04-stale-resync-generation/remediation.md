# Remediation: make resync generation-conditional

1. Add expected `session_id` and `controller_generation_id` to `ResyncCmd`.
2. Have each frontend copy both values from its authoritative workspace snapshot when requesting resync.
3. Validate both preconditions atomically before allocating a replay or reading from store or shim. Reject stale generations with a classified superseded response that triggers adoption of the newest snapshot.
4. Revoke resync eligibility at the same state transition that detaches the shim and publishes the new connectivity state.
5. Test detach, reconnect, supersession, vendor rotation, and a delayed old-client command crossing each boundary.
6. Log request generation, live generation, request session, live session, replay source, and the precondition decision.

Success criteria: no replay starts for a mismatched generation, and ordinary supersession does not surface as an internal history failure.

Protobuf decision: recommended. Server-only connection checks reduce the race but cannot prove the caller acted on the current generation.
