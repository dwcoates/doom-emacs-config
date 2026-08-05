# Diagnosis: doctor uses a transport probe that reports a healthy store as down

Severity: Medium. Confidence: Certain.

Two post-window doctor runs reported `store-socket-connectable` FAIL with a connection-refused diagnosis while simultaneously reporting the store launchd service alive. During the same period, the store log emitted recurring `health PASS` records, and active producers and subscribers continued using PID 5111.

`modules/app/agent-repl/scripts/agent-shim-doctor.sh:128` runs `nc -U -w 2` with immediate stdin EOF and treats the command exit status as store health. The store already implements correlated `core.v1.HealthCheck` and `HealthStatus` at `shim-store/internal/server/server.go:255`.

Root cause: an untyped connect-and-close probe is being interpreted as application readiness. Its failure message further collapses every nonzero `nc` result into connection refused.

Impact: the primary diagnostic instructs operators to treat a functioning store as stale, which can trigger unnecessary restarts during an incident.
