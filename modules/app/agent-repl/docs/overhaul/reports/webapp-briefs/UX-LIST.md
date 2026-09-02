# Webapp UX items for the user's review (running list; goes in the final report)

Working rulings (project lead, keep cheap to move):
- Turn stop control draws in the footer strip beside the clock while a turn is live (host composer stop chord remains primary); "stop all agents" in the agents panel header.

Implementer-chosen UX (feed cards A):
- Tool-card diagnostics capped at 3 lines with a "+N more" toggle.
- Denied badge reads "denied" in the permission badge style; hook exit chip with code 0 stays plain (non-zero red).
- A gated-call reveal that cannot reach its row marks the link muted/struck.
- Broken response bubble shows "cut short" under the prose, no reason text (the reason is turn_ended's).
- Response bubbles have no timestamp any more (no wire source) — only the usage stamp corner.
- Every tool input line uses the shell-line look until landing 3's input form arrives (Q7 ruled: command|path|query).

Implementer-chosen UX (feed core):
- Rolling highlight: legacy wave kept on every prompt bubble; the newest prompt only carries a marker (no visible change). Alternative: wave only the newest bubble (changes the existing look).
- User prompt text now renders as MARKDOWN (per FeedTextBlock's comment); legacy drew prompts verbatim in a pre with fenced code lifted. Fidelity vs formatting — user's call.
- Turn-ended error sentences become daemon-composed headlines with landing 3 (Q8 ruled).
- Retry countdowns at second resolution ("retrying in 12 s").

Cold-gate copy: see feed-asks report (COLD_GATE_COPY) — forwarded for review.

Open questions to put to the user (collect from each agent's report):
- (footer) should the expanded panel close when a new turn starts?
- (merge) which tab auto-selects on a settled merge (terminal tab vs tests)?
- (topbar) hover vs click for the context breakdown reveal.
- (sidebar) where the create-workspace form lives; typed-name confirm for nuke.
- (tray) auto-collapse when empty.

Implementer-chosen UX (tray / composer / panels):
- Empty tray: draws the daemon's heading plus a "nothing held" line (A); alternatives: draw nothing (B) or heading only (C).
- Held prompt with an image by host PATH cannot be shown (file lives on the daemon's host): the card names the path in a muted monospace line; a daemon-served image route would be a contract addition.
- Shutdown-hold schedule id and keep-alive turn id ride tooltips only (no visible treatment prescribed).
- /context section headings and ordering, and the empty-panel sentences for /todos, /mcp, /agents, /help, are implementer wording.
- The composer clears its box on every SubmitPrompt success arm (turn, command_panel, command_refused).
- RequestCommandSupport success draws a brief "support workspace created" note (the roster shows the workspace).

Implementer-chosen UX (footer):
- Turn stop control sits inside the clock cell while a turn is live; fan-wide stop in the agents panel header (working rulings, one-line moves).
- An open expanded panel stays open across pushes and a new turn (A); alternatives: close on entering thinking (B); only the tokens panel auto-closes on a new turn (C).
- A panel whose chip becomes unset stays open drawing its empty line ("no live agents"); alternative: auto-close.
- Activity datum colours (sha blue; attempt/count/position/percent yellow) are outside the vocabulary file; a `footer_datums` section would make them contract (daemon-lead change).
- Chip glyphs: monitors ◉, crons ◷ (schema spells emoji); agents ⚙, tasks ☑, shells $; task glyphs ☐ / ◐ breathing / ☑.
- Both failing token verdicts draw ✗ (distinguished by title); alternative: hollow for incomplete, solid for invalid.
- A second click on the open chip closes the section (no explicit close control).
- Expanded section row ceiling 8 (legacy), then it scrolls internally.

Implementer-chosen UX (sidebar):
- Create-workspace form opens inline under its repo section header ("+"); the new-task form sits at the head of the task pane.
- Nuke requires the workspace name typed back; kill takes a single confirm.
- Merge glyphs as plain characters (≡ queue, ⟳ recycle, ⇄ conflict, ✕ failed, ✓ merged); inactive "?"; none invisible.
- The legacy rail-head row count is gone (no message carries a count; counting rows would be client derivation).
- (sidebar, open) Where a row's verb menu should live long-term: currently inline under the row line, opening downward, delimited.
- (sidebar, open) Should the create form offer a repo-level default model/priority?
- (sidebar, open) Does the legacy ten-row cap on the recently-merged band still apply now that the daemon resolves the section?

Rulings on cards-B/asks concerns (project lead): relative ages stay; the permission card's waiting clock ticks from first draw (no arrival instant on the wire; may gain one after playtests); findings folds keyed by row position (possible landing-6 identity field, daemon side); the compact submenu pre-selects the first served model/scope (serving order is the default by design).

Implementer-chosen UX (merge bubble; all currently at option a):
- Which tab a settled merge auto-selects: (a) the terminal (last) tab; (b) the last failed tab when any failed; (c) the tests tab when one exists.
- Where a settled-failed tab's summary sits: (a) a line under the strip, above the tab body; (b) inline at the top of the tab body.
- Whether the composer slot appears on live agentic tabs: (a) parked only, per the spec's parked wording; (b) any agentic tab, disabled unless parked.
- The "you are here" marker on the current queue entry: currently a muted suffix on the entry's line; wording and placement open.
- Tab selection release rule: the reader's pick stands across pushes and is released only when a never-drawn tab id appears.

Implementer-chosen UX (topbar; ruled by the project lead 2026-09-01): context breakdown opens on click (hover vs click stays with the user); `data-reveal="mode"` blessed as the sixth reveal name.
