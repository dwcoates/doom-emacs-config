# Remediation: use the store's correlated health protocol

1. Replace the `nc` status check with a small owned client that sends `HealthCheck` with a unique request ID and validates the matching healthy `HealthStatus`.
2. Distinguish missing socket, connect failure, write failure, timeout, decode failure, mismatched request ID, and unhealthy response.
3. Keep the socket-presence check as a separate filesystem fact.
4. Add shell-harness coverage using a protocol-compatible local test server and negative fixtures for each failure class.
5. Include request ID, latency, component, and exact classified failure in JSON doctor output.

Success criteria: the current healthy store passes, each synthetic failure reports its actual class, and the doctor never labels a generic nonzero client exit as connection refused.

Protobuf decision: no change. The required health messages already exist.
