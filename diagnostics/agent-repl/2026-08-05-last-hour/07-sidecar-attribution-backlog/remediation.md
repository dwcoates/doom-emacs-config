# Remediation: resolve or terminally classify every spool

1. Define authoritative ownership sources in priority order: live launch observation, durable open-task state, and validated spool metadata.
2. On discovery, record a structured reason for unresolved ownership and retry only while a named authoritative source can still arrive.
3. Move provably historical, closed, and unattributable files into an explicit terminal classification instead of retaining them forever. Do not assign a guessed session.
4. Expose bounded samples of held path hashes, task IDs, age buckets, roots, and reason codes in diagnostics.
5. Add a readiness threshold for active unresolved spools separate from the historical terminal count.
6. Test restart with cursors past launch lines, late ownership arrival, closed historical spools, and two sessions with similar filenames.

Success criteria: active held count converges to zero, historical files reach a terminal state, and every warning explains which ownership source is missing.

Protobuf decision: no core change. Existing task and session identities are sufficient. A later frontend health surface may add backlog counters separately.
