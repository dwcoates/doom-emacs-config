# ReadNotifications as a modeled tool

Claude Code's built-in `ReadNotifications` tool reached the producers' unmodeled
converters, so every call drew the daemon's `daemon.sessionwatcher.unmodeled_activity`
warning ("an activity the schema does not model") and drew no feed card. The
tool is a built-in, so its absence from the schema is a producer defect, not a
genuinely unknowable tool. This change models it as a first-class activity kind.

Mode: autonomous (the owner pre-approved contract changes for this work).

## Assumptions

- The call's input is always `{}` (observed in real transcripts). Evidence tier:
  observed behavior.
- The result is plain text: "No queued notifications." when the queue is empty,
  and presumably a text listing otherwise. Evidence tier: observed behavior for
  the empty case; the non-empty wording is not observed. Because the shape
  carries the text verbatim and never parses it, the unobserved wording cannot
  invalidate the shape.
- No typed output (`tool_use_result` object) is relied upon.

## Iteration sequence

One increment: the activity arm and its message family in
`conversation/v1/agent_activity.proto`. No frontend message changes: the feed
draws the call with the existing shared tool card (`FeedSimpleToolCall`), whose
text output form already carries a returned text.

## Landed changes

### AgentActivity.read_notifications (tag 38) and the AgentReadNotifications family

- WHAT: a new `item` arm `read_notifications`, and `AgentReadNotifications` with
  the standard tool-call lifecycle oneof `result { start, success, failure }`.
  The start carries only the start instant; the success carries the returned
  text verbatim (optional, unset when the vendor returned no text) and the
  settle instant; the failure carries the shared `AgentToolFailure`.
- WHY: the tool is a built-in and must not be graded unmodeled; the reader
  should see what the agent read from its queue.
- Tag 37 is taken by `AgentActivity.model`, so the arm takes 38. Tags 4, 13 and
  17 are retired arms and were not reused.
- The lifecycle oneof is spelled `result`, as `AgentChessBoard` and the other
  recent tool families spell it, not `state` as the older cron and push families
  do.
- Alternatives rejected: an outcome oneof (`empty` versus `listed`) on the
  success. The vendor states the queue only in prose, so telling the arms apart
  would mean matching the vendor's sentence, which a wording change would
  silently break; the text is carried and drawn as returned instead.
- No restatement obligation: the call has no input, so a settled frame stands
  alone with the tool name and its returned text. No new
  `AgentActivityContract` revision is needed.
- Consequences: every producer (the shim's stream converter, the sidecar's file
  converter) maps the vendor name `ReadNotifications` onto the arm; the daemon's
  feed resolver draws it as a `ReadNotifications` tool card with the returned
  text as its output, and `ActivityToolName` names it. The footer has no meaning
  for it and stays unchanged. The webapp draws the generic tool card and needs
  no change.

Obviated-declaration sweep: nothing obviated; the change is purely additive.
