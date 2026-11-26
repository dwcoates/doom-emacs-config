<!-- used by: daemon/internal/wssummary (Keeper.OwedNote: closes a user's or agent's prompt to a workspace that has no whole summary standing — a new workspace's first message, the first prompt after a /clear, and a workspace whose summary has no long part); placeholders: none -->
This workspace has no summary, so set one now, before you go on with your work: run `agent-repld workspace summary here -long "LONG SUMMARY" -short "SHORT SUMMARY"`. The summary has two parts, and the topbar draws both: the short summary is the title that tells this workspace apart from the others, and the long summary is the popup a hover on that title shows. Write the long summary first, then distill the short summary from it.

The long summary describes the workspace's task in more detail than a title can:

- Two to four sentences, in one paragraph with no line break, and eighty words at most.
- Say what the work is, what it touches, and what done looks like, specifically enough that a reader who has not seen the conversation knows what this workspace is for.
- Nothing about the person, the date, or how the workspace was started.

The short summary states the workspace's high-level task:

- One imperative sentence that opens with its verb, the way a request does ("Fix the footer's stall detail for cold gates."). Never "This workspace is about", never a noun phrase, never a question.
- Exactly one plain sentence. No commas, semicolons, colons, parentheses or dashes joining what would otherwise be several clauses. If the task does not fit one plain sentence, rise to a higher level of abstraction until it does.
- Fifteen words at most, and the shorter the better: ten or fewer is the aim. Cut detail rather than run long.
- Name the subject of the work specifically enough to tell this workspace from another, and nothing about the person, the date, or how the workspace was started.

Distill the task from this prompt and anything that came before it in the conversation. The daemon refuses a summary whose long or short part breaks its format and says which part and why, so reword that part and set the summary again when it does.

Do this without asking, and mention it only if a call fails.
