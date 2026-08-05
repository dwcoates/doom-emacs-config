# Remediation: separate component and fatal operations

1. Bind a normal `shim.main.lifecycle` logger for startup, lock, signal, and shutdown records.
2. Keep a dedicated `shim.main.fatal` logger used only by `reportFatal` and unrecoverable entrypoint termination.
3. Make fatal records error-level and require a classified cause and exit outcome.
4. Add tests that enumerate every main entrypoint log and assert its stable operation and level.
5. Update operation-query documentation or dashboards that relied on message matching.

Success criteria: an operation-only query for `shim.main.fatal` returns only unrecoverable failures.

Protobuf decision: no change.
