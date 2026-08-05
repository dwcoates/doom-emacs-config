# Diagnosis: empty token model poisons frontend frames

Severity: High. Confidence: Certain.

`doom` rejected seven frontend frames between `13:39:59.347` and `13:40:35.365` with `SessionTokenUtilization.models[0] requires model identity and totals`. Both session-view and snapshot frames were rejected, so a single invalid aggregate prevented unrelated state in the same frame from being adopted.

`modules/app/agent-repl/daemon/internal/frontend/tokenutilization.go:272` indexes models using `record.GetModel()` without rejecting the empty string. `modules/app/agent-repl/daemon/internal/tokenutilization/validation.go:80` validates session, message, actor, usage, and request identity but not model identity. The webapp correctly enforces nonblank model identity at `modules/app/agent-repl/webapp/src/frontend-proto.ts:2949`.

Root cause: invalid response evidence crosses the durable boundary, aggregation creates a model bucket keyed by `""`, and the strict consumer rejects the entire frame.

Impact: token utilization and all co-framed session state can disappear until the invalid aggregate is absent. The current webapp error context is empty and its truncated frame head does not identify the offending response record.
