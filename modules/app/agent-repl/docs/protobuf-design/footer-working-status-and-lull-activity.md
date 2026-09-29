# Footer: the `working` status and an activity line through every lull

## Problem

Between the moments the feed moves, a turn can go quiet: a tool call's result
has landed and is on its way back to the model, and nothing new is drawn until
the model's next response starts. The footer's activity line is usually empty
then, so the user cannot tell that anything is happening. The owner wants the
invariant: while a turn runs, EITHER the feed is visibly moving (a tool call
running, a response streaming) OR the activity line says something — for
example "✅ Bash finished — the agent is handling the result", salient, and
cleared the moment the next feed item lands.

Separately, the footer's in-turn status is `thinking` with a `thinking`
substatus that stands for the whole turn. The owner wants the status called
`working`, and its substatus to name what the main agent is doing right now:
`thinking` only while the agent is running a (sync) inference call,
`executing` while a (sync) tool call runs, `writing` for a file write,
`reading` for a read, and so on.

## Context

- **The working steps.** While a turn is in flight the footer's step names
  what the MAIN agent is doing now: `thinking` (an inference call, no sync
  tool running), `executing` (bash, MCP tools, and any tool without its own
  step), `reading` (read), `writing` (write, edit), `searching` (grep, glob,
  web search), `fetching` (web fetch), `delegating` (a sync subagent);
  `submitting`, `clearing` and `compacting` stand as they are. Owner agreed to
  the separate searching / fetching / delegating steps.
- **The lull line: from the moment a feed item LANDS until the next SURFACES.**
  The quiet stretch is between one feed item landing fully and the next feed
  item surfacing partially. The activity line names what just landed (for a
  tool call: ✅ or ❌, the tool, and that the agent is handling it) and CLEARS
  THE MOMENT THE NEXT ITEM SURFACES — a response that starts streaming clears
  it at its first fragment, not when it finishes. The owner: this stretch
  between items is "the main point".
- **Not only while working.** The lull line also serves the `background`
  status (for example a subagent's message to the main agent surfacing).
  While a turn is in flight the footer reads `working` even if background work
  also runs, and background items are NOT surfaced on the activity line then.
- **Every activity line is one line.** An activity update is always a single
  line, truncated with an ellipsis when it would overflow; codified in the
  webapp's AGENTS.md.

## Landed changes
