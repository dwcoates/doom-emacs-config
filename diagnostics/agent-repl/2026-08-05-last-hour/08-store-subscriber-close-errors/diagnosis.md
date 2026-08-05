# Diagnosis: subscriber closure creates duplicate store errors

Severity: Medium. Confidence: Certain.

The store logged eight transport errors in the hour. Seven correlate to the focused workspaces by Claude session ID: replay writes failed for GNS, two `doom` identities, bubble, and Slack, and two of those also emitted `subscriber-read` errors for the server's own closed connection. One additional live-tail broken pipe belonged to another session.

The focused replay failures occurred immediately after client connection and after delivering between 2 and 680 records. The store continued to emit health PASS records, so this was not a store outage.

`modules/app/agent-repl/agent-shim/shim-store/internal/server/server.go:503` runs replay and a close-detection read loop concurrently. A write failure returns, deferred unsubscribe closes `subr.done`, and the done goroutine closes the connection. `subReadLoop` then classifies `use of closed network connection` as another error.

Root cause: client cancellation, replay write ownership, server-initiated close, and read-loop reporting are not represented as one connection terminal state.

Impact: expected short-lived subscriptions look like store failures, duplicate alerts are emitted, and genuine replay failures are harder to distinguish.
