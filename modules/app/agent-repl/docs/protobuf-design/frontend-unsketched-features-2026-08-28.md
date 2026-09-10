# Frontend `frontend.v1` — UNSKETCHED / AMBIGUOUS IMPLIED FEATURES

Audit date: 2026-08-28. Read-only pass; nothing but this file was written.

## Method

1. `docs/protobuf-design/digests/webapp.md` read in full (876 lines) as the
   agreed-treatment baseline.
2. Every `proto/src/frontend/v1/*.proto` read in full — feed (1841), footer
   (1219), failure (533), sidebar (399), daemon_hold (269), topbar (255), and
   the six panel files.
3. Cross-checked against `proto/src/agentrepl/v1/*.proto` (36 endpoint files
   + `service.proto`) to find proto-implied affordances with no verb, and
   against `webapp/src/` for the mechanisms that exist today.
4. `figma-to-idl-redesign.md` grepped and section-read at every point where
   the digest was silent and a treatment might still have been agreed
   (editor-popup raise, composer ownership, failure card, fold ownership,
   palette).

## Evidence tier

- **Tier A (documented absence)** — the surface is named by a proto comment,
  and a targeted grep of the canonical record plus the digest finds no
  treatment, no verb, and no carrier. Cited as `high` confidence.
- **Tier B (materially ambiguous)** — a treatment exists but leaves an
  implementer a real choice about layout, interaction or state. Cited as
  `medium`.
- Absence from MY reading is never on its own the verdict: every finding
  below names the proto line that implies the surface AND the specific place
  the treatment would have lived.

---

## FINDINGS

### 1. The webapp→host raise channel does not exist, and three affordances depend on it

- **Surface**: plan bubble ✎ edit, every findings location, both worktree
  separation paths.
- **Proto evidence**: `feed.proto:274-288` (`FeedPlanEditTarget`),
  `feed.proto:322-354` (`FeedFindingsLocation.path/line`),
  `feed.proto:1494-1533` (`FeedWorktreePath`, "the click is HOST-raised").
- **Unspecified**: the digest (§9.4) mandates ONE shared webapp link
  component and ONE shared Emacs subroutine, but no channel carries the
  click. The 30-rpc surface has no open-in-editor verb, the record states
  flatly that the daemon never calls Emacs and `ReportHostAction` never
  exists, and today's only bridge (`webapp/src/host.ts`) is Emacs→webapp
  script evaluation exclusively. An implementer cannot make these three
  clicks do anything.
- **Confidence**: high.

### 2. `failure.proto`'s entire vocabulary has no drawing surface and no carrier

- **Surface**: none — that is the finding.
- **Proto evidence**: `failure.proto:93-132` (`FailureKind`, 18 arms),
  `failure.proto:528-533` (`FailureCardRef`).
- **Unspecified**: `FailureKind` is referenced by NO message in any package
  (verified by grep across `proto/src`). The record's `FeedFailure` row —
  the message that used to embed it (record §"feed.proto kind ⑤") — is not
  an arm of today's `FeedRow`. So the six client-local arms
  (`daemon_unreachable`, `workspace_gone`, `boot_failed`,
  `control_plane_failed`, `frame_undecodable`, `stale_bundle`) that the file
  says "a frontend mints itself" have no component to be drawn in, and the
  twelve machinery arms have no push that carries them. `FailureCardRef`
  additionally addresses by `Message.uuid`, a dead identity space (rows are
  keyed by `FeedId`).
- **Confidence**: high.

### 3. Command panels have no placement, no lifetime, and no path to the webapp

- **Surface**: `StatusPanelView`, `TodosPanelView`, `AgentsPanelView`,
  `McpPanelView`, `ContextPanelView`, `HelpPanelView`.
- **Proto evidence**: `agentrepl/v1/endpoint_submit_prompt.proto:56-90`
  (panels ride `SubmitPromptSuccess` only); `status_panel.proto:20-29`,
  `mcp_panel.proto:9-24`, etc. ("the WEBAPP owns the rendering").
- **Unspecified**: a panel is a unary response payload, not a row and not a
  push — so where it is drawn (modal, feed-tail card, footer expansion),
  what dismisses it, whether it survives a reload or a workspace switch, and
  what a SECOND `/status` does to a standing one are all undecided. Worse,
  the record states the composer is HOST-native (Emacs runs the webview with
  `composer=0`), which makes Emacs the caller of `SubmitPrompt` and the
  recipient of the panel — yet all six files assign the rendering to the
  webapp, with no relay defined.
- **Confidence**: high.

### 4. The merge tab BADGE's counts have no field to draw from

- **Surface**: `FeedMergeTab` strip.
- **Proto evidence**: `feed.proto:1553` (the file's own sketch:
  `tests (2) ● 8/12`, `conflicts ✓ 3`), `feed.proto:1647-1667`.
- **Unspecified**: the digest states the badge "draws from exactly two
  fields — its label and its state arm", but the drawn sketch shows progress
  counts and conflict counts on the badge. `FeedMergeTabTests` carries only
  `repeated FeedMergeTestSuite`, and `FeedMergeTabConflicts` carries nothing
  countable at all. Either the badge loses its counts, or the client derives
  them from the suites list — a derivation the contract forbids. An
  implementer has no non-violating reading.
- **Confidence**: high.

### 5. Merge-tab selection, defaulting and append behavior is undrawn

- **Surface**: the merge bubble's sub-feed.
- **Proto evidence**: `feed.proto:125-130` (`merge_tab` as a top-level row of
  the merge sub-feed), `feed.proto:1624-1658`.
- **Unspecified**: which tab is selected on expand (newest? first live?),
  whether selection is sticky when a new tab appends beneath the user, what
  happens to selection when the selected tab settles, and — critically — how
  the client separates an agentic tab's parented content rows from the tab
  rows themselves, since both arrive interleaved on the same sub-feed with
  no ordering guarantee. Tab strips overflow; no wrap/scroll treatment is
  agreed either.
- **Confidence**: high.

### 6. `render-colors.json` is stale against the new status vocabulary

- **Surface**: roster dots, footer status colors, topbar `tone`.
- **Proto evidence**: `topbar.proto:75-82` (`tone` = "none|blue|teal|green"),
  `sidebar.proto:201-259` (24 status arms), `footer.proto:104-106` ("the
  daemon resolves the CSS class").
- **Unspecified**: the shared vocabulary file still keys on the deleted
  `RENDER_STATE_*` enum names, still contains `RENDER_STATE_HIBERNATED` and
  teal, and the record says teal DIES and the palette contracts to five.
  Meanwhile `TopbarConnectivity.tone` still names teal as legal. Fourteen of
  the roster's arms (`start_failed`, `none`, `inactive`, `idle_async`,
  merge arms…) have no row in the file at all, and the "recycle glyph"
  treatment for the six merge arms is named but never drawn.
- **Confidence**: high.

### 7. `HeldPromptAccepted` is drawable but unsettable — no verb exists

- **Surface**: daemon-hold tray, the `[accept]` button in the file's own
  sketch.
- **Proto evidence**: `daemon_hold.proto:8` (sketch shows `[accept]`),
  `daemon_hold.proto:148-164` (`HeldPromptAccepted.accepted`);
  `agentrepl/v1/endpoint_update_held_prompt.proto:24-33` — the action oneof
  is `release | drop` only.
- **Unspecified**: nothing can set the acceptance. Either the button does not
  exist (and the field is dead) or a verb is missing; the record decides
  neither.
- **Confidence**: high.

### 8. The tray's per-entry button legality is a matrix nobody drew

- **Surface**: daemon-hold tray rows.
- **Proto evidence**: `daemon_hold.proto:182` ("THERE IS NO FORCE-THROUGH"),
  `:199-237` (keep-alive, session-starting, build-refresh holds each state
  their own exits), `:86-104` (five classification arms).
- **Unspecified**: release is illegal on `uninterruptible_turn`, on
  `keep_alive`, and on `session_starting`, legal on `shutdown` — so the
  client must gate the release button on a 5×5 classification/hold
  combination. Nothing states whether the button is hidden, disabled with a
  reason, or shown-and-refused. The drawing of `classifying`,
  `interject.rationale`, and `classification_error.detail` (three visually
  different states) is likewise unsketched.
- **Confidence**: high.

### 9. Fold state is on the wire with no verb to change it

- **Surface**: merge bubble fold, compaction summary fold.
- **Proto evidence**: `feed.proto:1619-1622` (`FeedMergeFold.folded`, "UI
  preference the daemon holds"), `feed.proto:1489-1492`
  (`FeedContextCutFold.folded`).
- **Unspecified**: the stage-3 ruling moved fold state OUT of the roster as
  webview-local, and digest §9.9 says "fold and cap are client
  presentation" — yet these two carry server-held fold state, with no verb
  to toggle it and no statement of what a user click does when the daemon
  will re-push the old value on the next frame.
- **Confidence**: high.

### 10. Cold-gate wording is the client's, and no copy was ever agreed

- **Surface**: `FeedColdGate` standing row.
- **Proto evidence**: `feed.proto:1335-1392` — the one recorded departure:
  raw `context_tokens`, raw `last_request` instant, a typed model, and a
  menu of scope enums.
- **Unspecified**: the client owns "wording, formatting and ticking" for the
  ONLY component that works this way. The digest names a headline, a cost,
  a parenthetical and three buttons but fixes no strings, no number
  formatting rule, no tick cadence for "lapsed 2h ago", and no submenu
  interaction (does picking a model auto-submit, is scope a radio group
  inside the same menu, what is the default scope). Every other component's
  copy is daemon-resolved, so there is no house style to fall back on.
- **Confidence**: high.

### 11. Sub-feed expansion: inline vs drill-in was never decided, and breadcrumbs assume drill-in

- **Surface**: subagent bubble, merge bubble, `FeedBreadcrumbs`.
- **Proto evidence**: `feed.proto:155-200` (`breadcrumbs`, "what a cold open
  deep inside nesting draws"), `feed.proto:886-892` ("OpenFeed on expand,
  cancel the watch on collapse").
- **Unspecified**: expansion reads as inline (the head stays on the parent
  feed, the child's rows render beneath it), but breadcrumbs are navigation
  chrome for a view that REPLACED the parent. When breadcrumbs are drawn,
  where, and whether a crumb click collapses or navigates is undecided; so
  is whether a nested bubble inside an expanded bubble expands in place
  (unbounded indentation) or pushes a level.
- **Confidence**: high.

### 12. Jump targets do not say what a jump does

- **Surface**: footer agent/shell rows, hook `gated_call` link, breadcrumbs,
  merge queue entries.
- **Proto evidence**: `footer.proto:1098` and `:1202` (`FeedId target`),
  `feed.proto:726-729` (`FeedHookGatedCall`), `feed.proto:1823-1826`
  (`FeedMergeQueueWorkspace` — "a jump target ACROSS WORKSPACES").
- **Unspecified**: whether a jump scrolls, highlights, expands a collapsed
  ancestor, or pages backwards to find a row not yet loaded; what happens
  when the target lives inside an unexpanded sub-feed (its rows are on a
  connection that is not open); and what a cross-workspace jump does at all,
  given one webview is bound to one workspace for life.
- **Confidence**: high.

### 13. Momentary statuses have no client-side expiry

- **Surface**: footer status cell.
- **Proto evidence**: `footer.proto:136` (`interrupted`, MOMENTARY),
  `footer.proto:149-151` (`loading`, MOMENTARY).
- **Unspecified**: both are documented as "cleared by the next status push",
  but the push cadence convention (`footer.proto:55-62`) explicitly forbids
  periodic re-pushes. On a quiet session the momentary status stands
  forever. No client-side timeout, fade, or fallback treatment is agreed.
- **Confidence**: high.

### 14. `SubmitPrompt`'s sub-feed targeting has no UI

- **Surface**: composer targeting, merge-parked guidance.
- **Proto evidence**: `agentrepl/v1/endpoint_submit_prompt.proto:38`
  (`optional frontend.v1.FeedId feed`), `feed.proto:1672-1683`
  (`FeedMergeTabParked` — "everything the user types while parked is
  delivered to this tab's agent").
- **Unspecified**: the webapp has no composer (host-native), so nothing in
  the webapp's contract tells it — or shows the user — that typing is
  currently routed to a merge tab's agent rather than the session. There is
  no webapp-visible signal of the parked composer mode at all (the
  `merge_parked` arm is on the HOST stream, which the webapp does not watch).
- **Confidence**: high.

### 15. Presentation nesting has no drawing treatment

- **Surface**: `FeedRow.parent`.
- **Proto evidence**: `feed.proto:83-87`, `feed.proto:645-649` (work "MAY
  nest under this row by `parent` — a presentation choice").
- **Unspecified**: what nesting looks like (indent, container box, rail,
  fold), whether a nested group is collapsible, what happens to a nested row
  whose parent has not arrived or has been paged out, and whether the skill
  heading closes (nothing at the source delimits a skill's scope, so the
  last-nested-row rule is undefined).
- **Confidence**: high.

### 16. `FeedPageError` introduces a second `tone` vocabulary and an undrawn state

- **Surface**: page-load failure.
- **Proto evidence**: `feed.proto:166-177` (`FeedPageErrorHeadline{text,
  tone}`).
- **Unspecified**: `tone` is a bare string with no vocabulary reference
  (unlike `TopbarConnectivity.tone`, which cites `render-colors.json`). Nor
  is it stated whether a page error replaces the feed, appears as a banner
  at the scroll edge, or offers a retry — and `GetFeedPage`'s "a `next` with
  no walk standing is a refusal" produces a distinct error the client must
  also render somewhere.
- **Confidence**: high.

### 17. The queue tab for the FRONT workspace is required but has nothing to say

- **Surface**: `FeedMergeTabQueue`.
- **Proto evidence**: `feed.proto:1702-1708` (`FeedMergeQueue queue = 3`,
  non-optional), `feed.proto:1804-1805` ("The head workspace is shown
  NOTHING about the queue").
- **Unspecified**: the field is required, so the front workspace must ship
  one — with `current` set and `ahead` empty. Whether its own bubble draws an
  empty queue tab, hides the tab, or draws a different treatment is
  undecided, and presence-not-sentinel forbids the obvious "unset it".
- **Confidence**: medium.

### 18. Question card interaction is underspecified

- **Surface**: `FeedQuestion`.
- **Proto evidence**: `feed.proto:1144-1236`; `:1184` ("where an 'at most N'
  cap lands later"); digest: "THE FREE-TEXT ESCAPE IS ALWAYS DRAWN".
- **Unspecified**: whether the always-drawn Other field is per question or
  per batch; whether one submit answers the whole batch (the verb takes
  `repeated`, implying yes) and whether a partially-answered batch may be
  submitted; whether free text plus a selection is legal on a single-select;
  and how `expired` and `answered` states differ visually beyond a timestamp.
- **Confidence**: medium.

### 19. Roster detail, children and closed rows have no interaction model

- **Surface**: `RosterRow`.
- **Proto evidence**: `sidebar.proto:267-280` (`children`, `detail`,
  `closed`), `sidebar.proto:26-32` (the sketch shows detail as "expanded").
- **Unspecified**: `RosterRowDetail` is always present but drawn "expanded" —
  what expands it (hover? selection? a local disclosure?) is webview-local
  and undecided; nesting depth for `children` has no cap or indent rule; and
  a `closed` row's greyed treatment interacting with the merge/attention
  markers is unstated.
- **Confidence**: medium.

### 20. The topbar warning strip's own chrome is undrawn

- **Surface**: `TopbarWarningStrip`.
- **Proto evidence**: `topbar.proto:84-119`.
- **Unspecified**: only "empty draws nothing" is fixed. The indicator itself
  (glyph? count? color from which vocabulary?), the two-level interaction
  (strip → dropdown list → per-entry detail overlay), overlay dismissal, and
  what happens when the standing overlay's warning is retracted by the next
  push are all open.
- **Confidence**: medium.

### 21. Footer panel behavior around unset chips is ambiguous

- **Surface**: `FooterExpanded`.
- **Proto evidence**: `footer.proto:880-897` ("a panel whose subject is empty
  still arrives … its chip is then unset and nothing can select it").
- **Unspecified**: what happens to an OPEN panel when its chip goes unset
  mid-view (the last shell exits while the shells panel is open) — close,
  keep showing an empty list, or keep the last content. Panel height,
  scroll, and whether two panels can be open at once are also undecided.
- **Confidence**: medium.

### 22. Response type-out pacing and the concluded-answer border are hand-waved

- **Surface**: `FeedResponse`, `FeedTurnEndedConcluded`.
- **Proto evidence**: `feed.proto:432-435` ("type-out pacing is the
  client's"), `feed.proto:770-775` (`optional FeedId answer`).
- **Unspecified**: the pacing algorithm is handed to the client with no
  agreed rate or catch-up rule, while the daemon re-pushes whole prose (so a
  naive renderer flickers). And the final-answer border's target arrives on
  a DIFFERENT row than the one it decorates — nothing states what happens
  when the terminal row arrives before that response row has been paged in,
  or when a later push replaces the answer row.
- **Confidence**: medium.

### 23. Tool-card presentation caps and paint classes are unfixed

- **Surface**: `FeedSimpleToolCall`.
- **Proto evidence**: `feed.proto:619-627` (`paint_class` is an open string,
  "a closed arm set is owed"), `feed.proto:550-554` (diagnostics
  "client-capped"), `feed.proto:567-570` ("the client caps visible height").
- **Unspecified**: the cap values and the expand affordance (fold? scroll?
  "show more"?) are named nowhere, and the paint-class inventory the
  stylesheet must cover is explicitly still owed — so highlighting cannot be
  implemented to completion, only defensively.
- **Confidence**: medium.

### 24. `FeedRow.turn` self-highlight has no treatment

- **Surface**: user prompt rows.
- **Proto evidence**: `feed.proto:88-91` ("what a client matches its
  `SubmitPromptSuccess.turn` against to find its own prompt").
- **Unspecified**: what the highlight IS, how long it lasts, and — since the
  webapp does not own the composer — whether the webapp ever holds a
  `TurnId` to match against in the first place.
- **Confidence**: medium.

---

## CHECKED AND CLEAR

Components read in full and judged fully specified for an implementer:

- **`FooterStrip` status/substatus/activity tree** — every arm's legality,
  the lowercase-with-spaces rule, the merge-when-no-substatus rule, activity
  absorbing free width, and the daemon-side precedence ladder are all fixed
  (`footer.proto:97-643`). Only the momentary-expiry gap (finding 13) is open.
- **`FooterTokensCell` + `FooterExpandedTokens`** — one figure, two glyphs,
  the always-set stable-shape line list, verdict arms mirrored between cell
  and panel.
- **`FooterCronRow` / `FooterMonitorRow` / `FooterTaskRow` /
  `FooterAgentRow` / `FooterShellRow`** — each row's elements, its markers,
  its clock direction, and its jump-target status (including the deliberate
  non-targets) are stated per row.
- **`FooterLiveWorkChips`** — presence gating, the at-least-1 invariant, the
  tasks fraction's explicit non-oneof, and the deliberately absent unmodeled
  chip.
- **`FeedTurnEndedErrored`** — the full vendor taxonomy, per-arm evidence,
  the countdown-bearing `retry_after_ms`, and the envelope message.
- **`FeedSubagent` and `FeedShell` heads** — label/description/tokens/clock,
  live-vs-settled, snapshot spool semantics, `lost` as its own word, the
  no-`failed`-arm shell ruling.
- **`FeedPermission`** — headline/subtitle/trigger/arguments, the
  `standing_offered` presence marker, denial-is-an-answer, policy wording.
- **`FeedSessionSeparation`** — the one-renderer structural invariant, arm
  selects accent + payload only, tokens on context arms only. (Its worktree
  path CLICK is finding 1; the divider drawing itself is clear.)
- **`FeedArtifact`, `FeedPlan`, `FeedFindings` bubble bodies** — heading,
  state arms, badge/chip decoration as the client's, purple response
  styling, read-only ruling. (Their jump/edit clicks are finding 1.)
- **`FeedHook`** — failures-only ruling, blocked-is-loud vs failed-ordinary,
  exit chip.
- **`TopbarTitle` / `TopbarSessionLine` / `TopbarModelSelector` /
  `TokenBreakdownView`** — composed strings, whole-option selection with the
  typed echo token, always-populated menu needing no round-trip,
  `emphasized`/`depth`/`share_permille` layout facts resolved daemon-side.
- **The six panel row shapes themselves** (`status_panel`, `todos_panel`,
  `agents_panel`, `mcp_panel`, `context_panel`, `help_panel`) — label/value,
  arm-is-the-badge, depth indentation, omit-don't-blank. Only their
  PLACEMENT is unspecified (finding 3).
- **`WorkspaceRoster` structure** — both groupings resolved, section keys vs
  labels, the task-only done check, the when-column's daemon-chosen arm, the
  attention marker's canonical two-blink cadence.
- **`DaemonHoldTray` shell** — heading composed daemon-side, whole-list
  replace, arm-is-the-kind routing, `HeldOfferMergeDequeue`'s composed
  headline with answers as verb arms.
