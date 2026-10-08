# A permission or question gate is green, and the status names the gate

Owner request, 2026-10-08: "When a workspace is in a permission or question
gate, the status should be DONE (green) across the board, not working (red).
The footer status name should be the corresponding gate type, question or
permission, not waiting." Every contract change below was pre-approved by the
owner; this records what changed and why, in plain words.

## Why a gate read red

The daemon's footer learns that an ask is open from the ask's start, which
arrives once, live, on the agent's stream. A daemon that takes a workspace over
while an ask is open (a rollout's handover above all, which runs on every
landed merge) was never shown that start: the successor opens its watches at
the stream's tail, and history reaches it only when a reader loads a page,
which only the feed takes up. The shim's re-announcement told the successor
that a turn was running, so the footer drew `working`, the roster `thinking`,
and the tab red, while the feed showed the open card. It stayed that way until
the user answered.

The owner's report was exactly this: a question opened at 15:02:36, the
successor daemon took the workspace at 15:02:44 and drew `working · thinking`
until the answer at 15:06:08.

## 1. The session's facts name every open ask

`conversation.v1.SessionStarted` gains `open_asks`: every consent ask and
question batch the shim holds open, oldest first, each a new
`SessionOpenAsk` naming the agent that asked and the ask itself as its start
carried it (`permission` or `question`, its result always `start`).

The shim states it on every re-announcement, beside `turn_in_flight` and
`live_work`, from the asks its permission gate holds pending. A daemon that
takes the facts restates each ask to the views a gate stands on (the footer,
the roster and the merge queue) exactly as a live start would have reached
them. The feed and the host notification are not told again: the card was
drawn and announced when the ask opened.

An ask a keep-alive raised on the main agent is never listed, because the
keep-alive's turn is never served either.

## 2. The footer names the gate

`frontend.v1.FooterStatus` gains two arms:

- `permission` (`FooterStatusPermission`): a tool call waits on the user's
  consent. Its line is the gated call.
- `question` (`FooterStatusQuestion`): a question batch waits on the user's
  answers. Its line is the batch's lead.

Each carries its own always-salient activity cell, which also takes the two
status-independent lines (a deploy's progress and the agent's push
notification), as every status arm's salient cell does.

What was retired: the `waiting` status's `permission` and `question`
substatuses, and its salient line kinds `gated_call` and `question_lead`. Their
tags and names are reserved. `waiting` keeps the cold gate, an interrupt
landing, and the self-scheduled wakeup.

## 3. The roster names the question too

`frontend.v1.RosterRow.status` gains `question` (`RosterRowStatusQuestion`),
beside the `permission` arm it already had, so the rail and the tab bar can
tell the two gates apart as the footer does. The roster's `waiting` arm now
means only a cold gate or an interrupt landing.

## Colors and the ladder

Both new footer arms and both roster gate arms are green in
`proto/vocab/render-colors.json`, the same green as `done` and `idle`. On the
status ladder they stand on the existing waiting-on-the-user rung, above a
running turn, so a gate is never drawn as working. Within the rung the order
is unchanged: an interrupt landing, then a permission, then a question, then a
cold gate.

## What did not change

- The cold gate, an interrupt landing, and the wakeup keep the `waiting` name
  and its green. They are not permission or question gates.
- A merge's own agent asking the user still reads `merging · waiting on user`,
  purple, because the merge holds the workspace.
- The ladder's order is unchanged. A gate under a stronger standing claim (a
  fault, a merge in flight, or a merge that landed or failed and still stands)
  is drawn as that claim.
