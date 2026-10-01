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
ever enabled), it stays OPEN through a merge (owner ruling, 2026-10-01):
what is submitted is held until the merge ends. It closes on an unusable
workspace (blue). SubmitPromptError arms are the RACE FALLBACK for
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

## Removals ruled 2026-08-29 (final-audit triage)
- The chess-board widget (chess-game.ts) and its marker-splitting: DEAD
  with the daemon's widget surface; /show-chess-game degrades to text.
- The ENTIRE window.agentRepl* host-hook surface (host.ts, nav.ts hooks,
  search.ts hook, recovery-probe.ts hook): DEAD — the webview is purely
  daemon-driven; Emacs never drives it by JS eval.
- Incremental in-feed search (search.ts, 857 lines): DEAD.
- Bubble navigation (nav.ts data-nav tokens and cycling): DEAD.
- Copy key chords (copy.ts): DEAD.
- The topbar subagent/task counter chips and turn-aged retention
  (counter-menu.ts, agents.ts, tasks.ts, turn-clock.ts): DEAD — the
  footer's live-work chips are the successor, live work only.
- The client-side outage prompt queue (prompt-queue.ts): DEAD — the
  composer-less webview submits nothing; the Emacs composer's
  hold-and-replay is the outage absorber.
- The gns-sockets bridge fold (gns.ts): DEAD — bridge subagents draw as
  ordinary subagent bubbles.
- The standalone element catalogue page (catalogue.html/catalogue.ts):
  DEAD — the project lead may recreate something similar at its choice.
- Metaprompt-span stripping (meta.ts): DEAD client-side — the DAEMON's
  feed resolver strips the sentinel-marked spans from the drawn prompt
  row.
- The ungated-session standing banner (ungated.ts): DEAD — the mode is
  visible in the permission-mode picker itself.
- Document-title model tracking and the localStorage verbose-log toggle:
  DEAD.

## Kept behaviors blessed 2026-08-29
- The metaprompt TLDR-tree re-render (metaprompt-tree.ts): KEPT — a
  rendering-layer nicety (interpreting prose is the renderer's job).
- Capped sections: click-to-expand N-line previews, edge-gated inner
  scroll, and tail-follow while streaming: all three KEPT as rendering
  strategy.
- The LOGIN overlay: KEPT — the pty login path is ported (OpenLogin verb
  + duplex byte stream with resize + close); the webapp renders it
  xterm-style full-screen over the new stream, per-account idempotent,
  and re-probes the account chip on close.
- The permission-mode PICKER: KEPT, over the new SetPermissionMode verb
  (mirrors SetModel).

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
  the sub-feed's FeedMergeTab rows — queue | pre-prompt | merge |
  conflicts | tests | fixes | post-prompt, every tab conditional on its
  work having begun. RESOLVED tabs (queue, merge, tests) draw the
  row's own content — the queue snapshot, the merge commit's narration
  lines, the test suites with PAINT-CLASS COLORED SPANS (the client
  paints classes, never parses ANSI). AGENTIC tabs (pre-prompt,
  conflicts, fixes, post-prompt) draw the sub-feed rows parented to
  them, exactly the subagent-feed rendering path; PARKED exists only
  on conflicts and fixes.
- PARKED tabs show the composed standing line plus a paused badge; the
  user's prompts (typed into the ordinary composer while the host
  stream says merge_parked) land in that tab as ordinary user-prompt
  rows.
- LAZY: a collapsed merge bubble transfers only its head; a settled
  merge pages like any settled bubble. Rounds are separate tabs
  ("tests (2)").

## The topbar (settled 2026-08-28)
- Thin strip; left tight (account, connectivity dot), centered flexing
  title, right tight (model selector, context chip, warning chip).
- Every reveal renders BELOW the strip, clamped in-viewport.
- The context chip renders the current context size as a YELLOW number;
  hover shows the session-scoped breakdown (no turn figures — footer's).
- The account label draws the email, or "logged out" as a warning state;
  clicking the logged-out entry opens the ported LOGIN overlay (above).
- A drain-scheduled WatchDaemon push (reason + at_ms) draws the standing
  page-wide restart banner (ruled 2026-08-29).

## Visual quality and fidelity directives (ruled 2026-08-29)

- EXISTING LOOK AND FEEL DOES NOT CHANGE, and existing elements do not
  change in ways the new feature set or a prescription does not
  necessitate. The expanded footer must change (agents and tasks now
  live there); the look of an ordinary bash tool call must not (nothing
  in the API suggests it should). The same rule holds for UX behaviors:
  the rolling highlight of prompt bubbles needn't change just because
  the message carrying prompts changed — nothing about that change
  suggests a UI change.
- GLYPHS: where the proto schemas or documentation suggest glyphs, use
  them — but NEVER as emoticons or emojis; always graphical
  glyphs/elements. Prefer simple and sleek (minimalist) over noisy and
  opinionated. Color is great and carries SEMANTIC value (orange =
  warnings, red = errors, …); blinking for status/activity; hollowed-out
  for done. CONSISTENCY is important.
- NEW UI features and changes are SLICK and PROFESSIONAL-GRADE — this
  is a user application meant to be pleasing to use and look at.
- DROPDOWNS AND HOVER MENUS open in the RIGHT DIRECTION (topbar
  dropdowns drop DOWNWARD, never upward) and never clip off any edge of
  the screen.
- LISTS anywhere in the UI (topbar dropdowns, the expanded footer, …)
  get SUBTLE THIN GREY DELIMITER LINES between rows — subtle, and
  CONSISTENT across elements (the same look in the expanded footer as
  in a topbar dropdown).
- The webapp teamlead should FREELY SURFACE UI/UX questions and
  concerns for further discussion (via the project lead, to the user)
  whenever it senses a gap in the UI/UX prescription the user could
  fill.

## Code-level consistency requirements (from the conventions walk)
- ONE renderer subroutine draws EVERY FeedSessionSeparation arm; the
  arm selects only accent color and label/payload text — a per-arm
  divider renderer is a defect.
- ONE shared link component backs every jump-to-file affordance.
- THE BLINK CADENCE is implemented exactly from the one spec on
  RosterRowAttention (two blinks, 500 ms on/off, then steady);
  divergence from the Emacs tab-bar is a defect.


## Contract context (for implementers)

Orientation for implementation agents. This section explains the API's ideas and layout; it
never substitutes for the protos. **Implementers do not touch protobufs** — the contract is
frozen; anything that looks like a schema gap is surfaced upward, never patched locally.

### Where the truth lives

- The protos live under `proto/src/` — packages `frontend/v1`, `agentrepl/v1`,
  `conversation/v1`, `workspace/v1` (plus `shim/v1` and `store/v1`, which the webapp never
  speaks).
- Proto comments are the authoritative per-field documentation: every oneof arm states when a
  producer sets it and what a consumer does with it. Read the file you implement against.
- `the teamlead prompt (standing conventions) and the proto comments` holds the cross-system conventions
  (identity spaces, echo tokens, response-outcome and bounded-stream conventions, presence
  rules, push cadence, package model). Not repeated here — read it once before implementing.
- The full design record is the PROJECT LEAD's context; escalate rather than consulting it
  when a proto comment leaves a "why" open.

### Design philosophy bearing on the webapp

- Server-driven UI. The daemon resolves everything into drawn view messages; the client is a
  renderer. No client-side derivation: no phase→word tables, no state→color mapping, no
  counting rows to label a chip, no per-tool headline assembly, no token arithmetic, no ANSI
  parsing. Where the daemon composed a sentence, the client draws the sentence.
- Whole-view pushes. Every component stream replaces its unit WHOLE on change — the topbar
  view, the footer view, the roster, the hold tray, each feed row. Nothing is a delta; the
  client accumulates nothing across pushes. Bursts are coalescible because the wire carries
  states, not events — never rely on seeing every intermediate push.
- Upserts by FeedId. Every feed row carries an opaque daemon-minted `FeedId`; a push with an
  already-seen id replaces that row whole. That is how a response grows, how many tracker acts
  feed one task bubble, how a redeployed artifact updates its bubble. Ids are echoed, never
  parsed or constructed.
- Accumulate daemon-side; complete snapshots. Resolver state is daemon-memory; the client is
  never expected to assemble partial state. Reconnect = re-open the stream (state is "now"),
  no resume tokens, no fences, no epochs — a stream's own order is the only order.
- Drawn vs called. `frontend.v1` is what is DRAWN; `agentrepl.v1` is what is CALLED. A click
  is always an agentrepl request with plain fields; a request type is never frontend.
- Liveness is structural, in the data. No keepalive frames exist anywhere. The current turn
  is live precisely while its feed has no `turn_ended` row; a connection dying without a
  terminal frame is a transport failure, never meaning.
- Clocks tick client-side. The daemon ships instants (start, next-fire, last-progress); the
  client animates count-up/countdown/"quiet for N s" itself. Settled figures (a tool card's
  "ran 4.2 s") arrive composed; an unset runtime means draw nothing, never tick.
- Message tree = UI tree. Every drawn box is one message, no value rides bare; the schema's
  nesting IS the component partition. The same fact in two components gets each component's
  own wrapper — that duplication is information.
- Typed arms, no fallbacks. Failure kinds, tool identities, states are closed oneofs; an
  unfamiliar arm fails to match LOUDLY rather than rendering as something else. Unset
  non-optional fields in a received push are a malformed frame — raise a loud error, never
  default.

### Package layout and relationships

- `workspace/v1` — the leaf identity vocabulary: daemon-minted opaque `WorkspaceRef` /
  `RepositoryRef` echo tokens, imported by both frontend and agentrepl; imports nothing.
- `frontend/v1` — the drawn components (see map below). Flows daemon→client only. May not
  import `agentrepl.v1`; `agentrepl.v1` may import it.
- `agentrepl/v1` — the Connect service the webapp calls: `service.proto` lists every RPC,
  one `endpoint_<method>.proto` per RPC, shared tokens (e.g. `feed_token.proto`) alongside.
- `conversation/v1` — the vendor-fidelity record layer (turns, content blocks, tool calls,
  session facts). The webapp meets it only where agentrepl requests/responses embed it:
  `UserSaid` (the prompt form), `TurnId`, `AgentModel`, `SessionCompactScope`. Fields marked
  "EXPECTED UNMAPPED" are real vendor data with no UI yet — never build speculative UI for
  them.

### frontend/v1 — the component map

One file per drawn component; each file's header comment is its spec.

- **The feed** (`feed.proto`) — the conversation surface and the one SELF-SIMILAR component:
  a subagent bubble IS a feed (same row vocabulary, its own pages, its own live tail), so a
  workspace has a UNIVERSE of feeds — the root feed plus one per bubble. A bubble row's
  `FeedId` is simultaneously the sub-feed's address.
  - Row taxonomy (the `FeedRow` oneof): `user_prompt` / `agent_prompt` (same shape, different
    author; agent-addressed wears the orange border); `activity` (synchronous turn progress —
    responses, tool cards, skills, hooks, plan/findings/artifact bubbles, tracker acts, and
    the daemon-orchestrated merge); `turn_ended` (the terminal fact as a row — history
    replays it, liveness hangs on it); `detached_*` wrappers (async work wrapping the SAME
    drawn component its sync form uses — sync-vs-detached is placement, never a second
    drawing); `permission` / `question` cards; `separation` (meta dividers: context
    cleared/compacted, worktree entered/left — ONE renderer subroutine for every arm);
    merge tabs.
  - Nesting: the connection is the placement — a sub-feed's rows arrive on the bubble's own
    feed and never name the bubble. `parent` exists only for presentation nesting WITHIN one
    feed (merge-phase rows, work under a skill heading).
  - Pages: `FeedPage` with `has_more` / `at_start` edges and resolved breadcrumbs. NO cursor
    anywhere — the daemon holds each feed's walk position; the client only asks first/next.
  - Cards to know: `FeedSimpleToolCall` (generic shell: composed input line + output-form
    oneof text/code/diff/lines/links — the client holds no per-tool knowledge);
    `FeedResponse` (usage stamp rides every state, error carries no reason — the reason
    lives on `turn_ended`); `FeedSubagent`/`FeedShell` heads with live/settled arms;
    `FeedPermission` (the standing-allow token never reaches the client — only a presence
    marker gates the button); `FeedQuestion` (free-text escape always drawn; expiry drawn as
    expired, never pending); `FeedColdGate` — the ONE deliberate exception where the client
    formats and ticks from raw facts; `FeedPlan` / `FeedFindings` / `FeedArtifact` (purple
    response-styled bubbles; plan-edit and finding locations use the ONE shared jump-to-file
    link component); `FeedMerge` + `FeedMergeTab` (see the merge-bubble section above —
    same sub-feed plumbing as subagents, tab strip is the only legitimate difference; test
    output is paint-class spans, never ANSI).
  - Answered/settled cards carry their resolution on the same row — a cold repaint from the
    row alone renders both open and answered states.
  - NOT in the feed: thinking (footer only), unmodeled tools (topbar warnings), the exempt
    built-ins (dropped at the shim entirely), held prompts (the tray's), crons (footer only).
- **Footer** (`footer.proto`) — the per-workspace status strip: Status → SubStatus →
  StatusActivity (resolution increases left to right, all typed, legality-by-construction:
  each status arm declares which substeps/activities are legal under it), clock, tokens cell,
  live-work chips. The expanded panels ship FULLY RESOLVED on every push; which panel is open
  is webview-local (the folded-menu convention) — opening one costs no round trip. Panel rows
  jump to feed bubbles by `FeedId`.
- **Topbar** (`topbar.proto`) — thin strip: account + connectivity left, flexing title
  center, model selector + context chip + warning chip right. Reveals render below the strip,
  clamped in-viewport. The warning dropdown is the home of unmodeled-tool and session-fault
  surfacing. Token-breakdown menu content rides the view (session-scoped only — turn figures
  are the footer's).
- **Sidebar** (`sidebar.proto`) — the workspace roster, entirely daemon-resolved, the ONE
  global stream (no workspace scoping). Both groupings (by repository, by task) arrive
  resolved as siblings; the client's local preference picks which to render. Grouping mode,
  folds, and nav cursor are webview-local, not wire state. `RosterRowAttention` blink cadence
  is implemented exactly from the one spec on the message.
- **Daemon-hold tray** (`daemon_hold.proto`) — held prompts and daemon-posed offers, drawn as
  its own region at the feed's tail, whole-list-replaced. A held prompt IS a
  `conversation.v1.UserSaid` the daemon has not yet delivered — never a feed row.
- **Failure vocabulary** (`failure.proto`) — not a component: the closed kind oneof plus
  typed evidence for spontaneous failures. The arm carries the machinery-vs-vendor side; the
  same evidence messages are named by agentrepl error arms for answer-shaped failures.
- **Command panels** (`status_panel.proto`, `context_panel.proto`, `mcp_panel.proto`,
  `todos_panel.proto`, `agents_panel.proto`, `help_panel.proto`) — daemon-resolved rows for
  programmatically answered slash commands; the webapp owns only the rendering. They arrive
  as `SubmitPrompt` success arms, never via a stream.

### agentrepl/v1 — the verbs the webapp calls

`service.proto` is the index; sections mirror the components.

- Feed: `SubmitPrompt` (a `UserSaid` whole; the daemon recognizes commands transparently —
  success forks into minted-turn vs command-panel; carries the contract's ONLY client-minted
  idempotency key; optional `feed` addresses a subagent bubble's composer), `OpenFeed`
  (answers newest page + mints the `FeedWatchToken`), `WatchFeed` (pure tail, echoes the
  token — page/tail seam cannot gap), `GetFeedPage` (first/next, no cursor), `Interrupt`
  (one verb; the target arm picks turn vs detached bubble), `AnswerPermission`,
  `AnswerQuestion`, `AnswerColdGate` — answers echo the served values; the card's new state
  arrives on the feed push, not in the RPC response.
- Sidebar: `WatchWorkspaceRoster` (the global stream), `CreateWorkspace`, `OpenWorkspace`,
  `CloseWorkspace` (soft, refuses while busy), `KillWorkspace` (forced, never blocks),
  `NukeWorkspace` (kill + delete worktree/branch), `MergeWorkspace` (enqueue; life thereafter
  is the feed's merge bubble), `RestartWorkspace`.
- Topbar/footer/tray: `WatchTopbar`, `SetModel` (echoes the served `AgentModel` token),
  `WatchFooter`, `WatchDaemonHolds`, `UpdateHeldPrompt` (deliver-now or discard; "accept" has
  no verb — a hold delivers itself when it clears), `AnswerHeldOffer`.
- Admin/diagnics: `DaemonHealth`, `SessionHealth` (unhealthy is an ANSWER, not an error),
  `ClientLog` (the webapp's console-less diagnostic relay into the durable log),
  `UpdateShutdownSchedule`, `UpdateMergeQueue` (operator tooling).
- Web link: `WatchWebWorkspace` — the webview's standing per-workspace daemon-link stream,
  never drawn; it carries the graceful-rollout pushes (`transferred` → call
  `AdoptWebWorkspace` on the new daemon then drop the old stream; reload signals are
  daemon-pushed, never a client heuristic). Host-section verbs (`RegisterWorkspace`,
  `SelectWorkspace`, `WatchHostWorkspace`, `WatchDaemon`, `AdoptHostWorkspace`) are Emacs's.
- Every response is `oneof result { success | error }`; error arms are per-method typed
  (`<Method>Error`), refusals render at the call site (composer, tray, clicked control).
  Domain outcomes — deny, nothing-running, empty result — are SUCCESS arms.

### Gotchas (learned during design; will bite)

- Multiple responses per turn is the NORMAL case; never assume one.
- The response error arm carries no reason — triage from `turn_ended`'s typed error arms
  (note `max_tokens` mid-arrival truncation vs `max_output_tokens` outright refusal — drawn
  differently).
- A non-zero shell exit is still `completed`; the exit code is a badge, not a failure arm.
- Deletion in whole-list pushes is ROW OMISSION on the next push — never synthesize deletion
  events.
- A collapsed bubble transfers only its head; expand = `OpenFeed` on the row's id, collapse =
  abandon the token. Collapse cancels only the client leg — production continues daemon-side.
- Stopping anything is always an RPC (`Interrupt`), never a stream close; closing a watch
  stream is a normal client act that ends nothing.
- No hibernation exists anywhere in the contract; a parked workspace is indistinguishable
  from idle except through the cold gate. Build no such UI state.
- The composer is host-native (Emacs); the webview runs composer-less. Merge parked-state
  prompts route through the ordinary composer path and land in the parked tab.
- Vendor identities (uuids, message ids, tool_use ids) never reach the webapp; the one typed
  survivor on feed rows is `TurnId`, matched against the client's own submission.
- Money/cost figures appear nowhere in the contract.

## Additional rulings (final-audit triage, 2026-08-29)
- /CONTEXT PANEL IS CUSTOM: the contract's rich context schema
  (orchestrator-designed from the vendor's full get_context_usage answer)
  is rendered CUSTOM in the panel bubble — tool calls in an automatically
  folded foldable render; the rest of the presentation is the
  implementer's.
- /STATUS DEGRADES BY DESIGN: version + spliced account/model/mode rows
  only (the handshake fields are deferred) — a thin panel is the settled
  consequence, not a bug.
- WORKFLOW IS KICKED: no workflow feed row, bubble, or chip exists,
  deliberately; a workflow's constituent agents draw as ordinary subagent
  bubbles. Do not invent a surface.
- ADD-SUPPORT SURVIVES: the unsupported-command refusal card carries the
  "engineer support for it" offer, spawning a support workspace through
  the ordinary creation verb.

## Kickoff increments and rulings (2026-08-29, project lead)

- LANDED: `OpenInEditor` — the ONE shared link component calls it for the
  plan edit button, findings locations and worktree divider paths (the click
  is relayed to Emacs; the webapp draws nothing); FeedRow `command_panel`
  (panel oneof over status/todos/mcp/context views) and `command_refused`
  (command + composed reason + optional add-support offer whose button calls
  `RequestCommandSupport`); WatchLoginTerminal is a SERVER stream of bytes
  and `SendLoginInput` carries keystrokes/resize (a WKWebView cannot speak
  Connect bidi); `TopbarView.permission_mode_picker` (current + options; the
  picker echoes an option's `mode` to SetPermissionMode); `UpdateHeldPrompt.
  accept` (button only on hold_for_turn_end entries); `SubmitPromptRequest.
  origin` is REQUIRED (the dev-mode composer sends WEBAPP_USER_SENT).
- The webview URL carries BOTH `workspace=<id>` and `dir=<dir>`; every
  request echoes the full WorkspaceRef from that one place.
- R2 fold: the shipped fold value is the INITIAL state on a row's first
  draw; the local toggle wins thereafter. R4: FailureKind has no carrier —
  the six client-local arms are drawn by the webapp's own failure overlay.
  R5: merge-tab badge = label (+round) + state glyph, no counts. R6: sub-feed
  expansion is INLINE (existing bubble look); breadcrumbs draw only when
  non-empty as an inner header; no drill-in. R7: bubble composers stay in
  the webapp (SubmitPrompt{feed}) and disable while the footer is merging/
  closing/disconnected. R8: sidebar and merge-queue navigation call
  SelectWorkspace. A jump into a collapsed shell bubble degrades to
  scroll-if-rendered. proto/vocab/render-colors.json + paint-classes.json
  (the daemon's) are the color and paint-class vocabularies.

- CROSS-SYSTEM PROCESS CONTRACTS (project lead, kickoff): one state root
  `$AGENT_REPL_STATE_DIR` (default ~/.claude-emacs); the daemon binds ONE
  loopback TCP listener serving Connect (HTTP/1.1 + h2c, binary + JSON) and
  the webapp assets on one origin, writes `127.0.0.1:<port>` to
  `$AGENT_REPL_STATE_DIR/daemon.addr` (atomic replace; removed on orderly
  exit; a joining successor writes it only after it owns every workspace);
  the webview URL is `http://<daemon.addr>/?workspace=<id>&dir=<dir>`
  (`&composer=1` only in dev mode); the shim is spawned as `node
  agent-shim/claude/shim/dist/main.js --listen <uds> --store-socket <uds>
  --log-fd 3 [--fake]` with CLAUDE_CONFIG_DIR, AGENT_REPL_OWNED=1,
  AGENT_REPL_STATE_DIR, SHIM_BUILD_SHA (tests add
  AGENT_REPL_FORBID_VENDOR_CALLS=1), cwd = the workspace; session facts
  travel only in StartSession; readiness = the first healthy `diagnostics`
  push on WatchSession; the store serves on ~/.cache/agent-repl/sock/
  store.sock (tests: env AGENT_REPL_STORE_SOCKET, a flag beats it); kernel
  locks live in ~/.cache/agent-repl/run/ — `workspace-<md5hex(clean abs
  dir)[:8]>.lock` (shim-held from startup; the daemon probes ONLY this one,
  flock LOCK_EX|LOCK_NB) and `session-<vendor session id>.lock` (taken
  inside StartSession; pre-minted on a fresh start); proto/vocab/
  render-colors.json + paint-classes.json are the daemon's, consumed by
  webapp and Emacs; Go modules pin connectrpc.com/connect v1.17.0 and
  golang.org/x/net v0.43.0 (Go 1.24 on this machine; every module stays
  `go 1.23`).

## Landing 3 relay (2026-08-29, project lead)

- A per-bubble prompt to a subagent may come back as a refusal (`not_deliverable`) this wave; render the refusal honestly, no client-side disabling until the user rules.
- The detach of in-flight foreground work (no client verb today) may be refused `unsupported` on the pinned SDK; same policy.

## Landing 4 relay (2026-08-29, project lead)

- Every agentrepl.v1 refusal is typed; refusal rendering and arm-coverage guards cover the new arms (cross-cutting four on every per-workspace rpc; per-rpc arms).
- DaemonFault/SessionFault/HostFault kinds are typed; the failure overlay and footer fault rows draw by arm.
- FooterAllowance.status is a typed oneof; color by arm from render-colors.json.

## Landing 6 relay (2026-09-01, project lead)

- SubmitPromptSuccess.command_acted: a recognized act with no turn; the composer clears its text and draws nothing (the effect arrives on the topbar/footer streams). SubmitPromptError.duplicate_submission: refusal at the composer, text preserved, worded as "already submitted".
- SubmitPromptError.turn_already_open is retired; remove its sentence and arm guard (schema-driven enumeration should already drop it).
- UpdateMergeQueueError.unknown_repository: refusal at the merge-queue control.

## Health surfaces ruling (2026-09-01, project lead)

- The webapp draws NO pull-driven health surface this wave: DaemonHealth and
  SessionHealth are operator/doctor pulls (Emacs-side) and WatchHostWorkspace is
  Emacs's stream. No `[data-daemon-health]`, `[data-daemon-fault]`,
  `[data-session-fault]` or `[data-host-fault]` hook exists.
- Session faults have their one prescribed home: the topbar warning dropdown,
  fed by the PUSHED TopbarView (the daemon routes diagnostics into it), drawn by
  typed arm — that is what the Landing 4 relay line meant.
- The integration suite's three extrapolated fault blocks (33 tests) are deleted.

## UpdateMergeQueue ruling (2026-09-01, project lead)

- UpdateMergeQueue is Emacs-side operator tooling with no webapp surface: no
  merge-queue control exists in the webapp, and its refusal arms (incl. landing
  6's `unknown_repository`) render nowhere here. The integration suite's seven
  UpdateMergeQueue refusal cases are deleted.

## Landing 7 relay (2026-09-02, project lead)

Adapt to protos ab7e681f2 / bindings c10714a41 (see PROTO-CHANGES.md):
- SubmitPromptError.bubble_refused{detail, kind}: the bubble's refusal
  rendering keys on kind (not_deliverable | agent_busy); `detail` is the
  human line. Replaces any interim rendering of the transport fault.
- CloseWorkspaceBlocked now carries fields; the webapp still draws the
  footer's close-blocked state (pushed), NOT this response — no new surface,
  only the decoder/type update.
- FeedMergeAbandoned.summary: draw it on the collapsed merge line exactly as
  FeedMergeFailed.summary is drawn.

## Landing 8 relay (2026-09-02, project lead; user-approved)

Adapt to protos 1fdf85e63 / bindings 3791cd630 (PROTO-CHANGES.md "Landing 8"):
- FeedSessionSeparation.kind.compaction_failed: the separation renderer's
  exhaustive accent map and label path gain the arm (typecheck currently fails
  on src/feed/rows/separation.ts); drawn as a divider with a failure accent
  and the daemon's label; no tokens line.
- FeedTurnEndedErrored.error: five new arms (max_turns, max_budget,
  execution_error, turn_failed, stop_hook_prevented) render through the
  existing headline path (the daemon composes the sentence); the schema-driven
  arm enumeration must pick them up; one test per arm.

## Landing 11 relay (2026-09-04, project lead)

- FeedShellLost.how / FeedSubagentLost.how: the settled outcome names WHICH
  lost it was — "lost sight of: file vanished" / ": went silent" / ": swept up
  at boot" — one clause appended to the word both surfaces already said.
- The cause is a clause, never a register change: `lost` keeps its own dot and
  its own class, and still never reads as failure.
- An UNSET `how` is an older daemon that never ruled, not a malformed row: the
  outcome stays the plain "lost sight of" it drew before this landing.

## Landing 10 relay (2026-09-04, project lead)

- FeedAgentPrompt.delivery: the sender's agent-prompt row shows the delivery
  outcome (queued / resumed the recipient) when set.
- FeedPermissionAnswered.denied_undecidable: a verdict treatment distinct
  from denied_by_policy, wording drawn verbatim.
- FeedTurnErrorQueryDied.cause: the turn-error line names the cause.
- SetModelError.cold: the model picker's refusal routes attention to the
  cold gate row instead of "daemon could not be reached".

## Landing 9 relay (2026-09-03, project lead)

- OpenWorkspaceError.vendor_start_failed{detail}: the schema-driven refusal
  enumeration picks it up; wording "the vendor failed to start the session"
  with detail appended when non-empty; one test.
