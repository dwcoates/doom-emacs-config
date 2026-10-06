# UX rulings pending

User-facing choices the lead met while landing work and did not decide. The owner rules; a ruled entry moves to its design record and leaves this file.

Ruled 2026-10-06 (moved out of the pending list):
- startup + focus park → tabs open immediately even unseen; the page loads once Emacs is focused (lisp/startup.el).
- mid-session vendor block → prompts are held "after reconnect" at once, unclassified, and classified only when released (branch vendor-block-hold).
- feed failure cards → the concept goes; failures surface as salient footer errors; a catalogue of every error/warning feed item comes first for a ruling (docs/investigations/2026-10-06-feed-error-items-catalogue.md).
- empty vendor-started turns → not hidden: suspected lost data, investigated (with the unresolved final answer and the missing subagent start), and the shim gains proactive data-loss checks.
- feed outcome markers → ruled and landed (branch feed-outcome-markers): one inline pill per non-message event, faults expand, the workspace status takes a failed turn's fault (vendor fault, or agent-repl `turn_died`); a failed compaction is its marker alone, a failed merge is its merge bubble alone; nothing in the feed spans past the stream area (feed.proto FeedOutcomeMarker; docs/USER-GUIDE.md "Outcome markers in the feed").

## Pending (2026-10-06)

1. **A vendor block with no automatic end.** Auth, billing, or a usage limit with no further rate-limit event: nothing the vendor sends ends the block, so held prompts wait until the user restarts the workspace (`SPC o C-c`). Should a usage limit release its held prompts at its reported reset time on its own?
