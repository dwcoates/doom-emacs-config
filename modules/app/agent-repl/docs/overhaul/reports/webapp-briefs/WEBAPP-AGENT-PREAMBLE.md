# WEBAPP OVERHAUL — implementer preamble (binding for every webapp subagent)

You are an implementation subagent under the WEBAPP TEAMLEAD of the agent-repl
overhaul. The webapp (TypeScript, `modules/app/agent-repl/webapp`) is being
ported from a hand-decoded WebSocket transport onto generated Connect clients
and rebuilt as a STATELESS renderer of server-resolved views. This file is the
shared contract every subagent follows; your dispatch prompt adds your scope.

## 0. Worktree hygiene (do this first, verbatim)

- `cd <your worktree>` (the absolute path in your prompt). Run
  `git rev-parse --show-toplevel` and `git branch --show-current`; both must
  match your prompt. If either does not, STOP and report.
- Work ONLY inside that worktree. Never touch `~/.config/doom`,
  `~/.config/doom-overhaul/webapp`, or any other worktree. Never touch `master`.
- Commit atomically on your branch: one self-contained change per commit; the
  unit tests covering a change ride in the same commit. Many small commits.
- Never edit anything under `proto/` (the contract is frozen; generated
  bindings are committed under `proto/gen/ts`). A schema gap is REPORTED, never
  patched locally.
- Never run `bin/build-frontend.sh`, `bin/deploy-all.sh`, never touch the
  running daemon or Emacs. Never make network calls to any vendor.
- Paths below are relative to `modules/app/agent-repl/` inside your worktree.

## 0b. Offloading mechanical work (user ruling, 2026-08-29)

You MAY offload mechanical, fully-specified writes — boilerplate, tests from a
settled table, rote conversions, doc sections — to a `sonnet-medium` subagent
(the Agent tool with `subagent_type: "sonnet-medium"`; if that type is not
offered, `subagent_type: "claude"` with `model: "sonnet"` and state "medium
effort" in the brief) at your own judgment, in YOUR worktree, one offload at a
time. You stay accountable: you specify the work exactly, review every line it
wrote, run the suites yourself, and list each offload (what, outcome) in your
report. Never offload design, validation logic, or anything not fully
specified. The session-wide subagent cap is shared: at most ONE offload agent
live at a time per implementer.

## 1. What to read before coding

- `docs/overhaul/webapp.md` — the planning document (transport approach, dead
  code, merge bubble, topbar, visual-quality directives, contract context,
  rulings). Read it whole.
- The protos your scope names, under `proto/src/` — the proto COMMENTS are the
  authoritative per-field documentation (producer sets / consumer draws /
  gotchas). `frontend/v1/*.proto` is what you DRAW; `agentrepl/v1/*.proto` is
  what you CALL; `workspace/v1/workspace.proto` is identity; `conversation/v1`
  only where agentrepl embeds it (`UserSaid`, `TurnId`, `AgentModel`,
  `ModelOption`, `SessionCompactScope`, `SessionCommand`).
- The generated TS under `proto/gen/ts/<pkg>/v1/*_pb.ts` — protobuf-es v2
  (`@bufbuild/protobuf` 2.x, `codegenv2`): `create(XSchema, init)`, oneofs are
  `{ case: "lowerCamelArm", value }`, message fields are `T | undefined`,
  `optional` scalars are `T | undefined`, `int64` is `bigint`.
- NEVER read `docs/protobuf-design/` (project-lead-only material).
- `webapp/legacy/` (once it exists) holds the OLD renderers as read-only
  reference for the existing look and feel. Port the look; never import from it.

## 2. Architecture rules (binding)

- SERVER-DRIVEN, STATELESS. The webapp renders views verbatim and derives
  nothing: no phase→word tables, no state→color mapping beyond a CSS class per
  arm, no counting rows to label chips, no token arithmetic, no ANSI parsing,
  no per-tool knowledge. Where the daemon composed a sentence, draw the
  sentence. Whole-view pushes replace their unit whole; feed rows upsert by
  `FeedId`; the client accumulates nothing across pushes.
- TYPED ARMS, NO FALLBACKS. Every oneof is switched exhaustively; an unset
  oneof, an unset non-optional message field, or an arm you do not know is a
  MALFORMED VIEW: throw `MalformedView` (from `src/rpc/malformed.ts`) — never
  default, never draw something else. `optional` fields absent = draw nothing.
  Presence, never sentinels.
- CLOCKS TICK CLIENT-SIDE. The wire ships instants (`*_at_ms`, `bigint`);
  the client ticks via the shared `Ticker` (`src/clock.ts`) and formats with
  `src/duration.ts`. A countdown ships its deadline. Never a `setInterval`
  of your own; subscribe to the ticker.
- PROTO→CODE MAPPING. Every message you render gets ONE base function
  `draw<MessageName>(msg, ctx)` (validation + drawing live there, once);
  every non-primitive use site (message-typed field, oneof arm) gets its own
  dedicated, exported, testable function delegating to the child's base;
  primitives get no wrappers. Requests are built symmetrically:
  `build<RequestName>(...)` returning a `create(XSchema, …)` value.
- LOGGING. Use ONLY the canonical API in `src/log.ts` (`log(level, message,
  {operation, context})` and `logVerbose`). Every nontrivial function logs its
  entry at debug; every branch that selects a materially different outcome
  logs its selection; warnings at `warn`, errors at `error`, each error logged
  exactly once by its owning layer with resolved inputs and cause. No direct
  `console.*`.
- THE FOUR IDENTIFIER SPACES are never interchangeable: `FeedId` names a row
  (and a bubble's sub-feed), `TurnId` names a turn, `WorkspaceRef.id` names a
  workspace, `FeedWatchToken` names an opened tail. Echo them verbatim; never
  parse or construct one.
- EVERY CLICK IS AN agentrepl RPC with plain fields; refusals (`<Method>Error`
  arms — TYPED since landing 4: a `cause` oneof with the cross-cutting four on every
  per-workspace rpc — unknown_workspace, workspace_ref_mismatch{registry_dir},
  transferring_away{address}, not_yet_adopted — plus per-rpc arms; render EVERY arm as
  `.refusal[data-arm="<case>"]` with a short per-arm sentence carrying the arm's fact,
  an unset cause = MalformedView, arms enumerated from `<Method>ErrorSchema` in tests)
  render AT THE CALL SITE (the clicked control, the composer, the tray
  row), never as pushed state. Domain outcomes (deny, nothing-running, empty)
  are SUCCESS arms. A response whose `result` oneof is unset is malformed.
- STREAMS are standing (never conclude on their own); the daemon flushes response headers on accept, so a watch's acceptance is observable before its first frame; a client ENDS a watch only by aborting its request (`AbortController`, which `watchStream.cancel()` does) — never by waiting for the stream to end; a stream ending without
  the client cancelling it is a TRANSPORT FAILURE (report via
  `ctx.failures`, reopen). Stopping anything is always an RPC (`Interrupt`),
  never a stream close.
- NO BACKWARDS COMPATIBILITY: nothing old is preserved for its own sake, and
  the OLD TESTS ARE NOT A SOURCE OF TRUTH (many were deleted or mechanically
  adapted at reconciliation). Coverage is rebuilt from the contract.
- WORKFLOW IS KICKED: build no workflow surface of any kind.

## 3. The rpc core (owned by the `rpc-core` agent; everyone else imports it)

```
src/rpc/malformed.ts   class MalformedView extends Error { path: string; detail: string }
src/rpc/strict.ts      assertNoUnknownFields(schema: DescMessage, msg): void   // deep; throws MalformedView
                       requireMessage<T>(value: T | undefined, path: string): T
                       requireCase<T extends {case?: string}>(oneof: T, path: string): T & {case: string}
                       unreachableArm(path: string, arm: never | string): never   // throws MalformedView
                       msOf(v: bigint, path: string): number                     // int64 instant → number
src/rpc/transport.ts   createDaemonTransport(baseUrl: string): Transport         // @connectrpc/connect-web, binary
src/rpc/client.ts      type AgentReplClient = Client<typeof AgentRepl>; createAgentReplClient(t: Transport): AgentReplClient
src/rpc/streams.ts     type StreamEnd = {kind:"cancelled"} | {kind:"transport_failure"; error: unknown} | {kind:"producer_ended"}
                       watchStream<Res>(ctx, opts: { name: string; schema: DescMessage;
                         open: (client: AgentReplClient, signal: AbortSignal) => AsyncIterable<Res>;
                         onPush: (res: Res) => void; onEnd?: (end: StreamEnd) => void }): StreamHandle { cancel(): void }
                       // applies assertNoUnknownFields to every push; a MalformedView thrown by strict
                       // or by onPush is logged at error, reported as frame_undecodable via ctx.failures, and the
                       // frame is skipped; a transport failure reports daemon_unreachable and reopens with backoff
                       // (retracted on the first successful push); every handle is registered so
                       // ctx.replaceClient(newClient) cancels and reopens all of them on the new client.
src/rpc/unary.ts       callUnary<Req,Res>(ctx, name, fn: (client) => Promise<Res>, schema): Promise<Res>
                       // strict-checks the response; logs; rethrows transport errors as ConnectError
src/rpc/context.ts     interface AppContext { readonly client: AgentReplClient; readonly workspace: WorkspaceRef;
                         readonly ticker: Ticker; readonly failures: FailureSink; readonly composerEnabled: boolean;
                         replaceClient(next: AgentReplClient): void; onClientReplaced(fn: () => void): () => void }
                       createAppContext(init: {...}): AppContext
src/rpc/page-address.ts pageAddress(search: string): { workspaceId: string; workspaceDir: string; composer: boolean }  // ?workspace=<id>&dir=<dir> both REQUIRED (ruled); &composer=1 optional
src/rpc/workspace-ref.ts workspaceRef(id: string, dir: string): WorkspaceRef                    // the ONE place the full ref is built; every request echoes it
src/failure/sink.ts    interface FailureSink { report(kind: FailureKind): void; retract(arm: ClientFailureArm): void }
                       type ClientFailureArm = "daemonUnreachable"|"workspaceGone"|"bootFailed"|"controlPlaneFailed"|"frameUndecodable"|"staleBundle"
src/clock.ts           interface Ticker { subscribe(fn: (nowMs: number) => void): () => void; now(): number }
                       createTicker(intervalMs?: number): Ticker
src/log.ts             log(level, message, {operation, context?}); logVerbose(...); bindLogContext(...); setLogger(...)
src/link.ts            renderExternalLink(ctx, {text, url}): HTMLAnchorElement      // http(s) → OpenExternal on click
                       renderEditorLink(ctx, {text, path, line?}): HTMLAnchorElement // → OpenInEditor{workspace, path, line?} on click (ruled): plan edit, findings locations, worktree paths — ONE shared component
src/vocab.ts           typed accessors over proto/vocab/render-colors.json + paint-classes.json (see §6)
```

Until the rpc core is merged into your worktree you may code against these
signatures; your dispatch prompt says whether it is already present.

## 4. Component mount API (every component follows this; main.ts wires them)

```
interface Handle { dispose(): void }
src/feed/feed.ts          mountFeed(host, ctx, deps: { renderers: RowRenderers; composerFactory?: ComposerFactory }): FeedHandle
                          FeedHandle extends Handle { revealRow(id: FeedId): Promise<boolean> }
src/footer/footer.ts      mountFooter(host, ctx, deps: { revealRow: (id: FeedId) => Promise<boolean> }): FooterHandle
                          FooterHandle extends Handle { onStatus(fn: (statusCase: string) => void): () => void }
src/topbar/topbar.ts      mountTopbar(host, ctx, deps: { openLogin: () => void }): Handle
src/sidebar/sidebar.ts    mountSidebar(host, ctx): Handle
src/tray/tray.ts          mountHoldTray(host, ctx): Handle
src/composer/composer.ts  interface ComposerGate { current(): "open" | "closed"; set(g): void; subscribe(fn: (g) => void): () => void }
                          createComposerGate(): ComposerGate
                          mountComposer(host, ctx, opts: { feed?: FeedId; gate: ComposerGate; onPanel: (p: SubmitPromptCommandPanel) => void }): Handle
                          type ComposerFactory = (host: HTMLElement, feed: FeedId) => Handle
src/panels/panels.ts      drawCommandPanel(panel: SubmitPromptCommandPanel, ctx): HTMLElement
src/login/login.ts        mountLoginOverlay(host, ctx): LoginHandle   // LoginHandle extends Handle { open(): void }
src/lifecycle/lifecycle.ts startLifecycle(ctx, deps: { drainBannerHost: HTMLElement }): Handle
src/failure/overlay.ts    mountFailureOverlay(host): FailureSink & Handle
```

Each mount opens its own `Watch*` stream(s) through `watchStream`, draws the
whole view on every push into `host`, and cleans up on `dispose()`.

### 4b. The feed's seams (feed-core defines these in `src/feed/renderers.ts`; card agents implement against them; main.ts assembles the registry)

```
interface RowContext { ctx: AppContext; feed: FeedId | "root"; row: FeedRow;
                       revealRow(id: FeedId): Promise<boolean> }
interface RowRenderers {                       // each returns the row's BODY element (feed-core wraps it in the row chrome)
  response(u: FeedResponse, rc: RowContext): HTMLElement            // src/feed/cards/response.ts        drawFeedResponse
  simpleToolCall(u: FeedSimpleToolCall, rc): HTMLElement            // src/feed/cards/tool-call.ts       drawFeedSimpleToolCall
  hook(u: FeedHook, rc): HTMLElement                                // src/feed/cards/hook.ts            drawFeedHook
  skill(u: FeedSkill, rc): HTMLElement                              // src/feed/cards/skill.ts           drawFeedSkill
  artifact(u: FeedArtifact, rc): HTMLElement                        // src/feed/cards/artifact.ts        drawFeedArtifact
  plan(u: FeedPlan, rc): HTMLElement                                // src/feed/cards/plan.ts            drawFeedPlan
  findings(u: FeedFindings, rc): HTMLElement                        // src/feed/cards/findings.ts        drawFeedFindings
  shell(u: FeedShell, rc): HTMLElement                              // src/feed/cards/shell.ts           drawFeedShell (detached_shell body)
  permission(u: FeedPermission, rc): HTMLElement                    // src/feed/asks/permission.ts       drawFeedPermission
  question(u: FeedQuestion, rc): HTMLElement                        // src/feed/asks/question.ts         drawFeedQuestion
  coldGate(u: FeedColdGate, rc): HTMLElement                        // src/feed/asks/cold-gate.ts        drawFeedColdGate
  mergeHead(u: FeedMerge, rc): HTMLElement                          // src/feed/merge/merge.ts           drawFeedMerge (the collapsed head line)
  commandPanel(u: FeedCommandPanel, rc): HTMLElement                // src/panels/panels.ts              drawFeedCommandPanel (row 15; synthesized, non-durable)
  commandRefused(u: FeedCommandRefused, rc): HTMLElement            // src/panels/refused.ts             drawFeedCommandRefused (row 16; add-support → RequestCommandSupport)
  mergeBody: BubbleBodyRenderer                                     // src/feed/merge/merge-body.ts      mergeBubbleBody
}
// A bubble (subagent or merge) IS a sub-feed. feed-core owns the bubble chrome: the head slot,
// the ▸/▾ toggle (initial fold from the wire per R2, the user's toggle wins after), the
// expand → OpenFeed(row.id) → WatchFeed / collapse → cancel plumbing, and a SubfeedView the
// body renderer draws from. The default body (subagents) lists the rows in order with
// `parent` nesting; the merge body draws the tab strip. ONE plumbing path — parity invariant.
interface SubfeedView { rows(): readonly FeedRow[]; onChange(fn: () => void): () => void;
                        drawRow(row: FeedRow): HTMLElement;          // the ordinary row path, chrome included
                        breadcrumbs(): readonly FeedBreadcrumb[]; composerSlot?: HTMLElement }
type BubbleBodyRenderer = (mount: HTMLElement, view: SubfeedView, rc: RowContext) => Handle
```

Rows feed-core draws itself: `userPrompt`, `agentPrompt`, `turnEnded`, `separation` (the ONE
renderer), the subagent bubble head (`FeedSubagent`, sync and detached alike), `mergeTab`
rows are NOT drawn as rows on their own — the merge body consumes them from `SubfeedView.rows()`.
Every renderer is a pure function of (message, RowContext) → DOM; re-pushes re-draw the row
whole (feed-core replaces the body element), so a renderer must not keep state outside the DOM
it returns except through the ticker (clocks) and local UI toggles it re-applies from `data-*`
attributes on the previous element when feed-core hands it as `rc.previous?: HTMLElement`.

## 5. DOM hooks contract (stable attributes the integration suite targets)

Use exactly these attribute names; values are the generated oneof CASE names
(lowerCamel, e.g. `simpleToolCall`, `loggedOut`) unless stated.

- Feed: the container `[data-feed="root"]` or `[data-feed="<FeedId.value>"]`;
  each row element `[data-feed-row="<FeedId.value>"][data-row-kind="<row case>"]`;
  an activity row also carries `[data-unit="<unit case>"]`; a bubble row carries
  `[data-expanded="true|false"]` and hosts its sub-feed in `[data-subfeed]`;
  a merge tab `[data-merge-tab="<kind case>"][data-tab-state="<state case>"]`;
  cards carry `[data-state="<state/outcome case>"]` where the message has one;
  the load-more control `[data-load-more]`; breadcrumbs `[data-breadcrumbs]`.
- Footer: host `[data-component="footer"]`; `.footer-status[data-arm]`,
  `.footer-substatus[data-arm]`, `.footer-activity[data-arm]`, `.footer-clock`,
  `.footer-tokens`, `.footer-chip[data-chip="agents|tasks|shells|monitors|crons"]`,
  `.footer-expanded[data-panel="tokens|agents|tasks|shells|monitors|crons"]`,
  jump rows `[data-jump="<FeedId.value>"]`.
- Topbar: host `[data-component="topbar"]`; `.topbar-account[data-arm]`,
  `.topbar-connectivity[data-tone]`, `.topbar-title`, `.topbar-model`,
  `.topbar-context`, `.topbar-warnings`, `.topbar-warning-row[data-arm]`,
  `.topbar-reveal[data-reveal="session|model|context|warnings|warning-detail"]`.
- Sidebar: host `[data-component="sidebar"]`; `[data-grouping="repository|task"]`;
  `[data-roster-row="<WorkspaceRef.id>"][data-arm="<status case>"]`;
  `[data-current="true"]`, `[data-attention]`, `[data-closed="true"]`.
- Hold tray: host `[data-component="hold-tray"]`;
  `[data-held-turn="<TurnId.value>"][data-arm="<classification case>"][data-hold="<hold case or none>"]`;
  `[data-offer="<offer case>"]`.
- Overlays/banners: `[data-component="failure-overlay"] [data-arm]`,
  `[data-component="drain-banner"]`, `[data-component="login-overlay"]`.
- Composer: host `[data-component="composer"]`; `.composer-refusal[data-arm]`.
- Refusal surfaces anywhere: `.refusal[data-arm="<error case>"]` placed as the
  NEXT SIBLING of (or inside) the control that made the call.

### 5b. Consolidated hook vocabulary (the integration suite targets exactly these; add nothing else, rename nothing)

Feed and rows:
- `[data-expand]` the bubble's expand/collapse toggle; `[data-subfeed]` the sub-feed mount;
  `[data-breadcrumbs]`; `[data-load-more]`; `[data-page-error="<FeedPageError kind case>"]`.
- `[data-turn-error="<FeedTurnEndedErrored error case>"]` on the turn_ended row's error
  element; `[data-retry-countdown]` its ticking countdown.
- `[data-interrupt]` every stop control (a live bubble/shell head, the footer's turn stop, the
  agents panel's stop-all); `[data-interrupt-confirm]` the confirm step after `confirm_required`.
- `[data-editor-link]` on every anchor `renderEditorLink` returns (the rpc core's link.ts must
  set it — the wiring agent verifies); `[data-diff-line="<FeedDiffLine kind case>"]` per diff line.
- `[data-finding]` per findings row, with `[data-verdict="<case>"]` and `[data-outcome="<case>"]`.
- Permission card: `[data-permission="allowOnce|allowStanding|deny"]` buttons. Question card:
  `[data-question-option="<option label text>"]` per option input, `[data-question-other]` the
  free-text field per question, `[data-question-submit]`. Cold gate: `[data-cold-gate="pay|clear|compact"]`
  buttons, `[data-compact-model="<AgentModel.name>"]` and `[data-compact-scope="<SessionCompactScope enum name>"]` radios.
- Command rows: `[data-command]` the command text, `[data-add-support]` the offer button,
  `[data-support-note]` the success note.

Footer:
- `[data-topbar-strip]`-style marker is the topbar's; the footer strip is `.footer-*` per §5.
- `.footer-activity [data-datum="sha|attempt|count|position|percent"]` on colored typed datums;
  `.footer-tokens [data-alarm]` the ⚠ glyph, `.footer-tokens [data-verdict="<case>"]` the badge.
- Panels: `[data-panel="…"] [data-row]` per panel row; `[data-empty]` the empty line;
  `[data-glyph="<glyph name>"]` on glyph elements (task status glyphs `pending|running|completed`).

Topbar:
- `[data-topbar-strip]` the strip element; `.topbar-mode` the permission-mode picker with
  `[data-mode-option="<mode>"]` per option; `.topbar-model [data-model-option="<AgentModel.name>"]`;
  `.topbar-context [data-share]` a share cell, `[data-depth="N"]`, `[data-emphasized="true"]`;
  `[data-detail]` the warning detail overlay; login overlay `[data-login-term]` (the terminal mount)
  and `[data-login-close]`.

Sidebar:
- `[data-grouping-pick="repository|task"]` the grouping toggle; `[data-section-fold]` a section's
  fold toggle; `[data-roster-row] [data-select]` the row's SelectWorkspace click target;
  `[data-glyph="<merge_glyphs name | dot | inactive | none>"]` the status glyph element;
  `[data-when="lastSelected|merged"]` the when column; `[data-priority="<badge label>"]` the badge.
- Verbs: `[data-verb="open|close|kill|nuke|merge|restart|restartForce|priority|assign"]` controls;
  `[data-priority="p05|p1|p2|p3|clear"]` inside the priority menu; `[data-assign-task="<task id>"]`
  (`""` = unassign); `[data-task-change="setTitle|setDone|setOpen"]`, `[data-task-rename="<task id>"]`,
  `[data-task-create]`, `[data-task-title]` the title input, `[data-task-status]` the done check.
- Create form: `[data-create-form]` on the form (fields by `name=`), `[data-create-submit]` its submit.

Hold tray:
- `[data-held-turn] [data-held-action="release|drop|accept"]` buttons; `[data-queued]` the ticking
  queued-at age; `[data-offer] [data-offer-decision="keep|release"]`; `[data-empty]` the empty tray line.

Composer and panels:
- `[data-composer-send]` the send button; `.composer-refusal[data-arm]`.
- `[data-panel="status|todos|mcp|context|agents|help"]` the panel element (feed row and dev area alike);
  `[data-panel] [data-row]` per row; `[data-todo-status="<case>"]`, `[data-mcp-status="<case>"]`;
  context: `[data-fold="toolCalls|planes|…"][data-folded="true|false"]` foldable sections.

Lifecycle:
- `[data-component="drain-banner"] [data-restarting]` present while a shutdown/restart notice stands;
  `[data-shutdown-cause="<cause case>"]`.


## 6. Visual and UX directives (binding)

- THE EXISTING LOOK AND FEEL DOES NOT CHANGE except where the new feature set
  or a prescription requires it. Reuse the existing CSS classes and structure
  from `src/styles.css` (and `legacy/` as reference). Add CSS only in a
  clearly delimited section headed `/* ---- <component> (<file>) ---- */`
  appended to `styles.css`; never rename or restyle existing classes.
- GLYPHS, NEVER EMOJIS. Where the schema comments suggest glyphs (⚙ ☑ $ 👁 ⏱
  in the footer, ⇄ merge, ▸/▾ folds), use simple graphical glyphs or CSS
  shapes; 👁 and ⏱ become glyph-like characters or CSS icons, not emoji.
  Hollowed-out for done, blinking/breathing for live, minimalist over noisy.
- SEMANTIC COLOR, FROM THE VOCABULARY FILES. `proto/vocab/render-colors.json`
  and `proto/vocab/paint-classes.json` are on your branch and AUTHORITATIVE
  (the topbar.proto comment listing "teal" is stale — there is no teal; five
  colors: blue, purple, red, yellow, green, plus "none"). The rpc core exposes
  them through `src/vocab.ts` (typed accessors over the JSON: `rosterStatusColor(arm)`,
  `footerStatusColor(arm)`, `topbarTone(tone)` validating against `topbar_tones`,
  `mergeGlyph(arm)`, `feedMergeHeadGlyph`, `failureSideColor(side)`,
  `paintClass(name)` validating against `syntax` + `ansi` with "" = plain and an
  unknown name = unstyled with a warning, never an error). CONSUMERS: the sidebar
  paints `roster_status` (the merge arms take `merge_glyphs` names → your glyphs),
  the footer paints `footer_status`, the topbar validates `tone` against
  `topbar_tones`, the failure overlay paints `failure_sides` (client_local = blue),
  the tool card and the merge tests tab paint `paint-<class>` spans. Every
  consumer's table is asserted ROW FOR ROW against the file in its tests
  (every arm of the proto oneof appears in the file; every key in the file is an
  arm) so a new arm without a color fails loudly. Beyond the vocabulary:
  orange = warnings, purple = the agentic/vendor bubbles, yellow = the context
  figure; CSS classes are `.tone-<color>` (one rule set, defined once by the
  chassis) and `.arm-<case>` for arm-specific treatment.
- DROPDOWNS AND REVEALS open DOWNWARD below their strip and CLAMP inside the
  viewport; never clip off any edge.
- LISTS everywhere (dropdowns, expanded footer, panels, tray) get subtle thin
  grey delimiter lines between rows via ONE shared class `.list-rows > * + *`
  (defined once by rpc-core's chassis; use it, do not re-declare).
- Professional grade: slick, consistent, pleasing.
- Surface every UI/UX question the prescription leaves open in your REPORT
  (with options); do not improvise a UX for an unmodeled situation.

## 7. Tests

- vitest + jsdom (`test/setup.ts` installs the logger). ONE test file per
  source module: `test/<module>.test.ts` for `src/<dir>/<module>.ts` (mirror
  the directory: `test/feed/feed.test.ts`). Table-driven, Arrange/Act/Assert,
  ONE edge case per test.
- Build fixtures with `create(XSchema, {...})` from the generated code. For
  verbs, use `createRouterTransport` from `@connectrpc/connect` with a
  scripted `AgentRepl` implementation and `createAgentReplClient(transport)`.
- No real timers: use `vi.useFakeTimers()`; never `await sleep(...)`.
- Every branch of your production code has a test: every arm rendered, every
  malformed input rejected (unset oneof, unset required field, unknown arm),
  every refusal arm drawn at its call site, every tick/format.
- Run `npx vitest run test/<your files>` and `npm run typecheck` from
  `webapp/` before finishing; both must be green. If typecheck fails only in
  files outside your scope, say exactly which in your report.
- The ADVERSARIAL SUITE and the integration suite are not yours to run.

## 8. Rulings landed 2026-08-29 (the contract on your branch already carries them)

- OpenInEditor{workspace, path, line?} is the host-raised click: `renderEditorLink`
  (src/link.ts) is the ONE shared component for the plan edit button, findings
  locations and worktree divider paths; the webapp draws nothing on success.
- FeedRow `command_panel` (oneof status|todos|mcp|context) and `command_refused`
  (command text, composed reason, optional add_support marker) are SYNTHESIZED,
  NON-DURABLE rows on the root feed — never expect them in a paged history; the
  add-support button calls RequestCommandSupport{workspace, command} (success
  carries a WorkspaceRef; draw nothing — the roster shows the new workspace).
- WatchLoginTerminal(WatchLoginTerminalRequest{workspace}) is a SERVER stream of
  LoginTerminalOutput{bytes|closed}; SendLoginInput{workspace, keystrokes{data}|
  resize{rows, cols}} is the unary input direction.
- TopbarView.permission_mode_picker {current{mode, display_name}, options[]} —
  the picker is the model selector's sibling; SetPermissionMode echoes an
  option's `mode` verbatim.
- UpdateHeldPrompt.accept: draw [accept] ONLY on hold_for_turn_end entries.
- SubmitPromptRequest.workspace (WorkspaceRef, field 5) is REQUIRED on every submission (landing 2); a `feed` when set must belong to it.
- SubmitPromptRequest.origin is REQUIRED: the dev-mode composer and bubble
  composers send PROMPT_ORIGIN_WEBAPP_USER_SENT; card actions that submit a
  prompt (none exist today) would use PROMPT_ORIGIN_WEBAPP_CARD_ACTION.
  SubmitPromptSuccess.command_refused{command} answers the dev composer for a
  recognized-but-unsupported command; the card itself is the feed row.
- /agents and /help are recognized-but-unsupported this wave (they arrive as
  command_refused); /status, /todos, /mcp, /context are panels.
- The webview URL is `?workspace=<id>&dir=<dir>` (+`&composer=1` in dev mode).
- R1 momentary footer statuses are the daemon's; the client times nothing.
- R2 fold fields are the INITIAL fold on first draw; the user's toggle wins
  after (a re-push never un-toggles).
- R4 the six client-local FailureKind arms are minted only by the webapp's
  own failure overlay; machinery failures reach the user via the footer's
  disconnected/blocked families and the topbar warning strip.
- R5 a merge tab's badge draws label (+round) and state glyph only, no counts.
- R6 sub-feed expansion is INLINE inside the bubble; breadcrumbs draw only
  when non-empty as a small header line; no drill-in navigation.
- R7 the root composer is host-native (Emacs): production runs composer-less;
  per-bubble composers stay and disable while the footer status is merging,
  closing or disconnected.
- R8 cross-workspace clicks call SelectWorkspace; nothing else.
- A footer jump into a collapsed SHELL bubble degrades to scroll-if-rendered.
- R14 nothing is persisted client-side except webview-local preferences
  (grouping, folds, panel selection) in `localStorage` behind try/catch.

## 9. Your report (final message; terse bullets; no time estimates; no all-caps)

- What landed: files and commit range on your branch.
- Suites run and results (collect every failure; never stop at the first).
- Overrides of prescribed implementation details, with reasons.
- Concerns surfaced: missing fields, unsupported messages, undefined UX, with
  options — never improvised away.
- Seams left for wiring (what main.ts or another agent must connect).
