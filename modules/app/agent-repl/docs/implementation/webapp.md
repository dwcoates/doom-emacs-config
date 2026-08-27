# Webapp implementation planning

## The prescribed transport approach (settled with the user)
The port uses the STANDARD CONNECT STACK end to end: @bufbuild/protobuf +
connect-web GENERATED clients for every endpoint (unary and streams). The
library owns (de)serialization, typing, and unknown-field refusal; the
hand-rolled protojson decoder, its runtime strictness layer, and the
build-time anchoring tables (invariant I5) are all SUPERSEDED by generated
code — do not port them. Codec choice (binary vs JSON) is the client's
config, not hand-written framing.

## Dead code to remove (with the transport port)
- The hand-decoded FrontendFrame/FrontendCommand transport whole: the webapp
  still speaks the deleted multiplexed push stream and WILL NOT talk to the
  new daemon — the port to per-endpoint Connect rpcs (WatchFeed/WatchFooter/
  WatchTopbar/WatchWorkspaceRoster/WatchDaemonHolds/GetFeedPage/OpenFeed/…)
  replaces it wholesale.
- Every UNANCHORED decode table (grep `unanchoredFieldSet` / `UNANCHORED`
  markers in async-bubble.ts, agent-emission.ts, frontend-proto.ts,
  proto-names.ts): each call site is re-anchored to a generated message or
  deleted with the port; invariant I5 (build-time anchoring) is restored then.
- progress-footer.ts's cold-keep-alive warning row: compares against the
  retired PROMPT_ORIGIN_CACHE_KEEP_ALIVE literal and can never draw.
- async-bubble.ts's detached-work vocabulary and 3-tier identity ladder:
  superseded by FeedRow's FeedDetachedShell/FeedDetachedSubagent + FeedId
  routing.
- Token computation from usage figures where the daemon now ships composed
  strings (ResponseUsageStamp → FeedResponseUsageStamp.text): rendering
  replaces computing per the server-driven-UI move.
- render-colors.json RENDER_STATE_* trim (CROSS-SYSTEM with elisp+daemon).

## Early correctness items (live behavior changes made at reconciliation)
- local-failure.ts's three loud-throw stubs (commandUnsentFailure,
  heldPromptUnsentFailure, commandRejectionUnclassifiedFailure) sit on LIVE
  paths — most critically main.ts:639, where a held prompt that never
  reached the wire now THROWS instead of filing a card. The port must give
  these an honest home (the new refusal surfaces / daemon-hold tray) FIRST:
  that card's whole purpose was preventing silent prompt loss.
- FailureKind lost its vendor band: failureTone can never return purple;
  vendor failures now arrive as FeedTurnError* arms — the renderer's triage
  re-derives from the frozen vocabulary.

## Replacement integration-test specs
(Unit specs deliberately absent per the mapping convention.)
- Per-endpoint stream decode: each Watch* stream's frames decode into the
  view verbatim and unknown fields are refused loudly (replaces the
  frame-oneof anchoring suites' subjects).
- Feed routing by FeedId: rows upsert whole by id; sub-feed open/collapse
  lifecycle (OpenFeed token, WatchFeed tail, GetFeedPage walk).
- Failure rendering: every FailureKind arm the frozen contract declares maps
  to a tone/class; every FeedTurnError* arm renders (replaces the deleted
  vendor-band and query-termination suites' subjects).
- Command dispatch over unary rpcs: refusal surfaces per endpoint error arm
  (replaces the deleted nack-classification suites' subjects).
