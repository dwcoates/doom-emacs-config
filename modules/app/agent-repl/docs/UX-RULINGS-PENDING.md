# UX rulings pending

User-facing choices the lead met while landing work and did not decide. The owner rules; a ruled entry moves to its design record and leaves this file.

Ruled 2026-10-06 (moved out of the pending list):
- startup + focus park → tabs open immediately even unseen; the page loads once Emacs is focused (lisp/startup.el).
- mid-session vendor block → prompts are held "after reconnect" at once, unclassified, and classified only when released (branch vendor-block-hold).
- feed failure cards → the concept goes; failures surface as salient footer errors; a catalogue of every error/warning feed item comes first for a ruling (docs/investigations/2026-10-06-feed-error-items-catalogue.md).
- empty vendor-started turns → not hidden: suspected lost data, investigated (with the unresolved final answer and the missing subagent start), and the shim gains proactive data-loss checks.

## Pending (2026-10-06)

1. **A vendor block with no automatic end.** Auth, billing, or a usage limit with no further rate-limit event: nothing the vendor sends ends the block, so held prompts wait until the user restarts the workspace (`SPC o C-c`). Should a usage limit release its held prompts at its reported reset time on its own?
2. **Feed outcome markers (proposed, owner leaning yes).** One inline pill per non-message event in the feed column: glyph + label + optional detail; neutral grey for the user's own acts (interrupted, Stop hook), turquoise for vendor faults, blue for agent-repl faults; faults expand (vendor error/message, request id, retries, model/account, sign-in action; or what died, exit code, last output, open-log action), neutral markers do not. Replaces the turn-ended bubble, the interrupted bubble, the denial error badge and the plan card's failed badge. Awaiting a mockup review.
