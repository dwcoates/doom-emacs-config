# Diagnosis: stale resync crosses a controller-generation boundary

Severity: Medium. Confidence: High.

In `doom`, the old session `s_63499d7dc607c154` rotated its vendor identity at `13:39:24.478`. It became ready again at `13:39:26.920`, detached at `13:39:27.267`, and received an intentional shutdown at `13:39:27.565`. At that same millisecond, a webapp resync from sequence 0 was rejected because the shim connection was no longer live. Emacs surfaced `session.superseded` roughly 21 seconds later.

`frontend.v1.ResyncCmd` at `modules/app/agent-repl/proto/agentshim/frontend/v1/frontend.proto:2212` carries only `from_seq`. It does not bind the request to the session or `controller_generation_id` shown in `WorkspaceState`.

Root cause: command eligibility changes when a controller detaches or is superseded, but a previously rendered client can still submit a workspace-only resync. The daemon discovers staleness only after beginning the history path.

Impact: a normal session replacement produces a visible command rejection and can leave history unavailable until the new generation reconnects and requests again.
