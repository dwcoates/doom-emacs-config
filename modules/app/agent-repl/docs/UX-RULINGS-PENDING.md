# UX rulings pending

User-facing choices the lead met while landing work and did not decide. The owner rules; a ruled entry moves to its design record and leaves this file.

- 2026-10-03 · startup + focus park: when Emacs is visible but unfocused at startup, held tabs open only once Emacs is looked at (the focus park keeps Emacs from taking focus). Alternative: open tabs without the webview until focused.
- 2026-10-03 · vendor fault mid-session: a prompt sent while the vendor is blocked mid-session (usage limit, auth) goes to the vendor as usual; only a failed vendor START holds prompts "after reconnect". Alternative: hold every prompt while a vendor fault stands.
- 2026-10-03 · feed failure cards: the `failure_sides` vendor color stays purple while vendor statuses are now turquoise.
- 2026-10-03 · empty vendor-started turns: ship-gns already stores 13 turns from the keep-alive loop (now fixed), each an empty stand-in prompt + a SessionStart:resume hook row + a turn end, no response. Hide vendor-started turns that drew no response, tool or subagent row (live and on replay)? Today they draw as empty prompts.
