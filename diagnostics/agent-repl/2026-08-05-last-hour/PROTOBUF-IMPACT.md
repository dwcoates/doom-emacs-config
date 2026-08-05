# Protobuf impact review

| ID | Decision | Rationale |
| --- | --- | --- |
| 01 | Required | `core.v1.ContentDelta` carries message UUID and block index but no `tool_use_id`. A durable tool record can arrive before its ephemeral input deltas, and the consumer cannot prove which finalized tool owns those deltas. Add an optional stable tool-use identity to `ContentDelta` and relay it unchanged through `TypingDelta`. |
| 02 | No change | The existing `ModelTokenUtilization.model` field already expresses the required identity. Validation and durable-ingress enforcement are missing. |
| 03 | Conditional | The existing reset timestamps are sufficient if the vendor contract defines a canonical boundary granularity. Add a field only if the vendor exposes a stable window ID that cannot be derived from the timestamp. Do not invent an ID locally. |
| 04 | Recommended | `ResyncCmd` contains only `from_seq`. Add `session_id` and `controller_generation_id` so the daemon can reject a stale request before starting replay against a newer or retired controller. |
| 05 | No change | This is local process and pipe lifecycle classification. |
| 06 | No change | `WorkspaceAvailable`, `WorkspaceMaterializedCmd`, workspace path, and session identity already define the ordering gate. The implementation publishes state too early. |
| 07 | No core change | Attribution can be repaired with existing task and session identities. A future user-facing backlog health surface could add fields, but that is not required to stop indefinite holds. |
| 08 | No change | Unix connection closure and server cancellation state are sufficient. No new wire command is needed for a client that closes its socket. |
| 09 | No change | The source and deployed artifact differ by one commit. |
| 10 | No change | `core.v1.HealthCheck` and `HealthStatus` already provide a correlated store health exchange. The doctor is not using them. |
| 11 | No change | The defects are logging argument shape, payload minimization, and hot-path rate policy. |
| 12 | No change | The operation label is a logger-binding concern. |

## Schema sequencing

Issue 01 should own `core.proto` field allocation and generated outputs. Issue 04 should own `frontend.proto` field allocation and generated outputs. Their orchestrators should merge schema commits one at a time into the program integration branch and regenerate from the combined schema before downstream consumer commits are rebased. This avoids generated-file conflict churn while preserving parallel implementation work in shim, daemon, and webapp worktrees.
