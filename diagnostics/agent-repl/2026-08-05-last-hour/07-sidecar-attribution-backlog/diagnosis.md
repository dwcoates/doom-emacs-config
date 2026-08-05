# Diagnosis: sidecar spool attribution backlog grows indefinitely

Severity: Medium. Confidence: Certain for the backlog, limited for workspace attribution.

The global sidecar issued eight warnings from `13:46:39` through `13:49:08`. Held spools rose from 1,818 to 1,821, and the stale count rose from 1,817 to all 1,821. The warning states that these spools are not tailed rather than assigned to a guessed session.

`modules/app/agent-repl/agent-shim/claude/shim-sidecar/main.go:361` retains every unattributed target in `s.held`. Its own comment notes that historical `/tmp` spools can form a permanent backlog because their launch lines are behind restored cursors and will never be reread.

Root cause: ownership recovery is incomplete for historical and newly discovered spools, while the held set has no terminal classification or bounded retirement policy.

Impact: events in a held active spool are delayed or absent from the store. The aggregate warning does not name paths, task IDs, candidate roots, or rejection reasons, so the four newly held spools cannot be assigned to the focused workspaces from telemetry alone.
