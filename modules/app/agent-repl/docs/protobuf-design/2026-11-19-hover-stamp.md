# Every hovered bubble shows its footer activity

Owner request, 2026-11-19 (the `?` control on a fix gate card in the
`deploy-mediator` feed): hovering a merge bubble, a mediator's fix gate, a
tool call or a prompt should show in the footer what hovering a response
already showed, its time since being sent and the rest.

Only response bubbles had a hover line. The webapp read it off
`frontend.v1.FeedResponse.usage`, recorded per bubble by the response card's
own draw. Every other card carried its instants in a shape of its own
(`frontend.v1.FeedToolCardClock`, `frontend.v1.FeedMergeRuntime`,
`frontend.v1.FeedShellRuntime`, `frontend.v1.FeedSubagentRuntime`, the fix
gate's `frontend.v1.FeedMergeTabLive` and `frontend.v1.FeedMergeTabSettled`),
and a prompt carried no instant at all.

Owner rulings: contract changes are decided and carried through by the
implementing agent (2026-10-13).

## Assumptions

- The hover line belongs to the ROW, whatever card the row draws, so one
  field on `frontend.v1.FeedRow` carries it for every kind.
- The daemon composes it (figma→idl: the client derives nothing per kind).
  Deriving it client-side from each card's own fields would be one rule per
  kind in the client, the failure mode the rule exists to prevent.
- A prompt's instant is the conversation place of the first entry that drew
  its row: the same instant `order.go` mints the row's order key from, so a
  replay serves the same stamp.

## Landed changes

### `frontend.v1.FeedRow.hover`

- WHAT: `optional frontend.v1.FeedHoverStamp hover = 25` on
  `frontend.v1.FeedRow`, with `frontend.v1.FeedHoverStamp { label,
  started_at_ms, at_ms, optional tokens }`,
  `frontend.v1.FeedHoverStampLabel { text }` and
  `frontend.v1.FeedHoverStampTokens { text, heat }`. Additive.
- SET on a user or agent prompt, a response, a tool call, a skill, a hook, a
  permission, a question, a shell head, a subagent (sync or detached), a merge
  and a fix gate. UNSET on every other kind.
- `at_ms` is when the bubble was sent (a prompt) or settled (everything else),
  zero while under way. `started_at_ms` is when its span began, zero when it
  has none. A stamp whose span settled before it began is a malformed view.
- Consequences: the daemon stamps it in `feed.stampHover`, on the one write
  path (`upsert`), beside the other per-publication stamps. The webapp records
  it on the row's element on every push (`hover-stamp.ts`
  `recordRowHover`, called by the feed view's row chrome alone), and the
  footer draws it in the activity cell. A re-push that changes only the stamp
  is adopted without a redraw.

### `frontend.v1.FeedResponse.usage` kept

- RULING: kept, though no client draws it directly any more. It is the
  response's own resolved fact: the fold restamps it as usage arrives and
  re-stamps the final answer with the turn's total, and every durable row
  already stored carries it. The hover stamp restates its figure, heat and
  span by the one rule.
