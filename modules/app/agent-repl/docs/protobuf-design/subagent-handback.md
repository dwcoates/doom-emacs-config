# SubagentHandback as a modeled activity

Every background subagent ends by calling the vendor's `SubagentHandback`
tool, whose single input field `message` is the subagent's final report to
its parent. The schema did not model it, so it travelled as
`conversation.v1.AgentUnmodeled` and the daemon logged
`daemon.sessionwatcher.unmodeled_activity` (WARN,
`daemon/internal/sessionwatcher/route.go:677`) once per subagent — three
times in one hour on 2026-09-27. The owner approved modeling it (2026-09-27).

## Core design principles

None stated for this change.

## Iteration sequence

Owner-delegated small additive change (2026-09-27): designed and landed by the
orchestrator in one increment, with no per-increment agreement gate.

## Landed changes

### 2026-09-27 — `conversation.v1.AgentActivity.item.subagent_handback`

WHAT
- New arm `AgentSubagentHandback subagent_handback = 35` on the
  `AgentActivity.item` oneof.
- New messages: `AgentSubagentHandback` (oneof `result`: `start`, `progress`,
  `success`, `failure`), `AgentSubagentHandbackStart`,
  `AgentSubagentHandbackReport`, `AgentSubagentHandbackSuccess`,
  `AgentSubagentHandbackFailure`, placed beside the `AgentSendMessage`
  family (cross-agent communication).

WHY
- The unmodeled carrier's own contract (`AgentUnmodeledStart.tool_name`) says a
  tool seen often enough gets its own arm; this one fires once per subagent.
- The report IS the subagent's result, so it is typed and drawn, unlike
  `AgentSendMessageBody`, which is deliberately never drawn.

WHY THE ALTERNATIVES LOST
- Re-levelling the WARN for this one tool name: hides a schema gap the WARN
  exists to surface.
- Folding the report into `AgentCompleted.answer`: that field names the
  agent's last top-level PROSE unit, and a tool call is not one; stretching it
  would make "answer" mean two different kinds of unit.

ARCHITECTURAL CONSEQUENCES
- Producers: the live shim (`agent-shim/claude/shim/src/convert/`) and the
  transcript sidecar (`agent-shim/claude/shim-sidecar/internal/convert/activity.go`)
  must both convert the `SubagentHandback` tool_use/tool_result pair to this
  arm, with identical output, so live and replay agree.
- Consumer: the daemon feed resolver draws the report as the subagent's
  returned result. Whether the frontend feed contract already has an element
  for a subagent's result is NOT yet verified; if it lacks one, that is a
  separate small `frontend.v1` addition, to be landed by the orchestrator
  before the drawing is implemented.
- Success/failure restate the report so a settled frame stands alone, the same
  rule `AgentSendMessage` follows.
- Additive: `make validate` green; daemon `go build`, webapp typecheck and
  shim typecheck all pass with the arm present and unhandled.

VERIFICATION EVIDENCE
- A real call (subagent transcript
  `~/.claude/projects/-Users-dodgecoates--config-doom/6a1b0e3a-.../subagents/agent-a1d968043b47deee9.jsonl`)
  has input `{"message": "<report markdown>"}` and no other field.

OBVIATED-DECLARATION SWEEP
- Nothing obviated: additive.

### 2026-09-27 — hand-backs leave a badge in the main feed, not a bubble

WHAT
- `conversation.v1.PeerMessage` gains `oneof kind { inter_session = 5; subagent_handback = 6; }`
  (new empty messages `PeerMessageInterSession`, `PeerMessageSubagentHandback`),
  set from the vendor's `origin.handback`. UNSET = not stated, read as inter-session.
- `frontend.v1.FeedRow.row` gains `FeedSubagentHandbackBadge subagent_handback = 20`
  (with element `FeedSubagentHandbackBadgeLabel`). `FeedPeerMessage`'s comments
  now say it is inter-session only.

WHY
- Owner (2026-09-27): "we shouldnt see subagent responses surfaced in the feed";
  "a little marker in the feed is good, but it shouldnt be a bubble or card, just a little badge".
- Evidence: in 90 minutes of workspace `3e2d9cadc6794e13` logs, subagent activity
  rows all landed in per-subagent feeds, but each of the 6 finished subagents
  produced one root-feed `peer` row (`feed.draw-peer-message` x6) carrying its
  full report.

WHY THE ALTERNATIVES LOST
- A flag on `FeedPeerMessage` selecting "draw as badge": a bool selecting how
  adjacent data is drawn is a two-arm oneof, and a badge is a different UI
  component from the peer bubble, so under figma-to-idl it is its own row arm
  with its own element messages.
- Drawing nothing: the owner wants a small marker.

ARCHITECTURAL CONSEQUENCES
- Shim and sidecar set `PeerMessage.kind` from `origin.handback` on both planes.
- The daemon feed resolver emits `subagent_handback` (badge) instead of
  `peer_message` for a hand-back, keeping the row id minted from `PeerMessage.id`
  so live/adopted supersession is unchanged.
- Cost accepted: hand-back records stored before this change have no `kind`
  and rebuild as peer bubbles.
