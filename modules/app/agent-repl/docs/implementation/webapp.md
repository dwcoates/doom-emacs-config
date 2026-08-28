# Webapp implementation planning

## The prescribed transport approach (settled with the user)
The port uses the STANDARD CONNECT STACK end to end: @bufbuild/protobuf +
connect-web GENERATED clients for every endpoint (unary and streams). The
library owns (de)serialization, typing, and unknown-field refusal; the
hand-rolled protojson decoder, its runtime strictness layer, and the
build-time anchoring tables (invariant I5) are all SUPERSEDED by generated
code — do not port them. Codec choice (binary vs JSON) is the client's
config, not hand-written framing.

## Composer refusal treatment (settled with the user)
Wherever the composer lives (Emacs host-native today; browser dev mode if
ever enabled), it CLOSES on the merging state (the host stream's composer
gate / the footer's merging status) — that is the PRIMARY defense against
post-merge-start prompts. SubmitPromptError arms are the RACE FALLBACK for
a submission already in flight when the state flipped: rendered inline at
the composer, per typed arm, with the text preserved. The refusal is the
submitter's own — no pushed view carries it.

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

## The merge bubble (settled at the merge-flow remediation, 2026-08-28)
- PARITY INVARIANT: the merge bubble uses the SAME sub-feed plumbing as
  the subagent bubble — expand → OpenFeed(bubble FeedId) → WatchFeed;
  collapse → abandon the token; a merge-specific nested-content loader
  is a DEFECT.
- The body renderer is the one legitimate difference: a TAB STRIP over
  the sub-feed's FeedMergeTab rows. RESOLVED tabs (queue, tests,
  landing) draw the row's own content — the queue snapshot, the test
  suites with PAINT-CLASS COLORED SPANS (the client paints classes,
  never parses ANSI), the landing narration lines. AGENTIC tabs
  (rebase, remediation, action) draw the sub-feed rows parented to
  them, exactly the subagent-feed rendering path.
- PARKED tabs show the composed standing line plus a paused badge; the
  user's prompts (typed into the ordinary composer while the host
  stream says merge_parked) land in that tab as ordinary user-prompt
  rows.
- LAZY: a collapsed merge bubble transfers only its head; a settled
  merge pages like any settled bubble. Rounds are separate tabs
  ("tests (2)").
