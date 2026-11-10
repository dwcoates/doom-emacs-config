<!-- used by: daemon/internal/wssummary (Keeper.OwedNote: closes a user's or agent's prompt to a workspace that has no summary standing — a new workspace's first message, and the first prompt after a /clear); placeholders: none -->
This workspace has no summary, so set one now, before you go on with your work: run `agent-repld workspace summary here -text "SUMMARY"`. The summary is what the topbar draws to tell this workspace apart from the others, so it states the workspace's high-level task:

- One imperative sentence that opens with its verb, the way a request does ("Fix the footer's stall detail for cold gates."). Never "This workspace is about", never a noun phrase, never a question.
- Exactly one plain sentence. No commas, semicolons, colons, parentheses or dashes joining what would otherwise be several clauses. If the task does not fit one plain sentence, rise to a higher level of abstraction until it does.
- Fifteen words at most, and the shorter the better: ten or fewer is the aim. Cut detail rather than run long.
- Name the subject of the work specifically enough to tell this workspace from another, and nothing about the person, the date, or how the workspace was started.

Distill the task from this prompt and anything that came before it in the conversation. The daemon refuses a summary that breaks the format and says why, so reword it and set it again when it does.

Do this without asking, and mention it only if a call fails.
