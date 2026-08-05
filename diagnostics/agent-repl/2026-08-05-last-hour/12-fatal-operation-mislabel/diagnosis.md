# Diagnosis: normal shim lifecycle uses a fatal operation label

Severity: Low. Confidence: Certain.

Normal informational records such as `validated shim startup arguments`, `selecting shim query implementation`, `exclusive session lock acquired`, and authorized shutdown were emitted with operation `shim.main.fatal`. The focused logs contained 22 informational records under that operation during the hour.

`modules/app/agent-repl/agent-shim/claude/shim/src/main.ts:68` creates one logger bound to `shim.main.fatal`. Both `reportFatal` and ordinary startup and shutdown paths use that logger, including lines 482, 490, 520, and 601.

Root cause: component scope and fatal operation scope are represented by the same logger binding.

Impact: operation-based queries produce false fatal incidents, and actual fatal entrypoint failures cannot be isolated without also filtering level and message text.
