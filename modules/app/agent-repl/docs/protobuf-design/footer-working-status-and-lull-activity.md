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

## Landed changes
