# Diagnosis: logging leaks large internal values and floods hot paths

Severity: High. Confidence: Certain.

The focused Emacs logs contained 18,443 records for `doom`, 8,957 for bubble, and 16,000 for Slack in the one-hour window. Several state and rendering operations emitted roughly 1,700 to 1,900 records each.

Five `should-prepend-metaprompt-p` records were approximately 31 KB apiece and contained the full metaprompt. At `modules/app/agent-repl/input.el:348`, `system-active-p` is assigned the final truthy string from an `and` expression instead of a boolean, then logged with `%s` at line 355.

Autosave emitted complete `#s(perspective ...)` structures, including buffer and window state, through `%S` at `modules/app/agent-repl/autosave.el:63` and line 71. Individual records exceeded 5 KB.

Repeated processing of the same assistant API response also amplified cache warnings and usage diagnostics, even though durable accounting deduplicated exact API message IDs.

Root cause: diagnostic arguments are not minimized, boolean state is not normalized, and several high-frequency paths log every poll rather than state changes or bounded summaries.

Impact: sensitive instruction and editor-state content is persisted, useful errors are buried, and file I/O grows with polling and redelivery rather than meaningful transitions.
