# Catalogue: errors and warnings that surface as dedicated feed items

Date: 2026-10-06. Research only; no code changed. Master a9a309305.

Purpose: before removing "feed failure cards" in favor of salient errors in the footer, list every error or warning that is drawn as its own item in (or inside the host of) a feed.

Paths below are relative to `modules/app/agent-repl/`. Abbreviations: `feed.proto` = `proto/src/frontend/v1/feed.proto`; `res/` = `daemon/internal/resolve/feed/`; `wa/` = `webapp/src/`.

## 0. Findings that change the framing

1. The generic "failure card" no longer exists as a contract concept.
   - `failure.proto` (header, lines 1-50) says every `FailureKind` arm is "NEVER A ROW": entry-less failures are "footer/topbar/gate state", and entry-correlated failures are the owning feed entry's own `error` arm.
   - `FailureCardRef` (`failure.proto:528`) has no user anywhere in the module (orphan message).
   - `render-colors.json#failure_sides` (machinery blue, vendor purple, client_local blue) still says "a failure card takes the color of the side it lands on", but its only consumers are the vocab tables (`wa/vocab.ts`, `daemon/internal/vocab/api.go`) and their tests; no feed renderer draws a card from it. It is vestigial for this purpose, and it also disagrees with the status-color table (vendor is turquoise there, purple here).
   - So "feed failure cards" in practice means the per-entry error arms catalogued below, led by the turn-ended errored bubble.

2. An API error draws no feed row at all.
   - `system:api_error` store rows reach the feed via `OnApiError` (`res/sink.go:332`) and history replay (`res/history.go:276`) and become EVIDENCE text appended to the turn's terminal headline ("a vendor request failed mid-turn and the turn went on: ..."), `sink.go:393`, `sink.go:~375`.
   - They surface in the feed only through the turn-ended bubble (item 1) and are withdrawn by `res/retired.go:101`.
   - The footer already has a counterpart: `vendor_fault` status and the salient `retrying` line (`footer.proto:253-260`, `:499`).

3. Two contract/renderer arms have no daemon producer (dead on the drawing side only):
   - `FeedPageError` / `history_replay_truncated`: no non-test Go code builds `FeedPage_Error` or `FeedPageError` (grep over `daemon/`); the webapp draws it (`wa/feed/feed-view.ts:555`).
   - `FeedContextCutCompacted.cold_read` (`feed.proto:2179`): no producer in `daemon/`; the webapp draws it (`wa/feed/rows/separation.ts:207`).

4. The footer's `turn_failed` status deliberately has NO substatus: "the feed's turn-end row carries the account" (`footer.proto:408-416`). Removing item 1 from the feed removes the only place the cause is stated, so the footer must take over the account, not just the color.

## 1. The catalogue

Class key: FACT = part of what happened in the conversation; SYS = agent-repl, vendor or client machinery fault; BOTH = a fact with a system cause or a system fault told as a feed event.

Rows 1.x are the 22 arms of one row kind, FeedTurnEnded.errored; the shared producer and renderer are given once.

### 1. FeedTurnEnded.errored (the "ended-turn bubble")

- Produced: `res/turnended.go:19-53` (`drawTerminal`) -> `erroredOutcome` (`:304`), `apiErrorArm` (`:387`), `producerErrorArm` (`:471`), `turnFailedArm` (`:555`), `drawQueryDied` (`:577`); recorded-close path `res/turnclosed.go` (`closedEnding`, ~`:92-140`). Contract `feed.proto:1249-1380`.
- Drawn: `wa/feed/rows/turn-ended.ts:100` (`drawFeedTurnEnded`), `:246` (`drawFeedTurnEndedErrored`): purple response bubble with red border (`turn-ended-bubble`), the daemon's headline verbatim, vendor message below, retry countdown for rate_limited/overloaded, query-death cause line.
- Footer overlap today: `turn_failed` status (turquoise) and salient `query_died` line ("vendor query died - the next prompt restarts it"); `vendor_fault` for auth/billing/usage/retry. No account of the cause.

| # | Arm (headline source) | Trigger | Class | Proposed footer line |
|---|---|---|---|---|
| 1.1 | `rate_limited` | vendor 429; may carry wait | SYS (vendor) | "rate limited - retry in 42s" (countdown stays footer-ticked) |
| 1.2 | `overloaded` | vendor 529; may carry wait | SYS (vendor) | "vendor overloaded - retry in 42s" |
| 1.3 | `authentication_failed` | vendor 401 | SYS (vendor/account) | "vendor rejected credentials - sign in again" |
| 1.4 | `permission_denied` | vendor 403 | SYS (vendor/account) | "vendor denied this account access" |
| 1.5 | `invalid_request` | vendor 400 | SYS (vendor) | "vendor refused the request as malformed" |
| 1.6 | `request_too_large` | vendor 413 | SYS (vendor) | "request too large - cut the context" |
| 1.7 | `not_found` | vendor 404 | SYS (vendor) | "vendor resource not found" |
| 1.8 | `internal` | vendor 500 | SYS (vendor) | "vendor internal error" |
| 1.9 | `vendor_unmodeled` (carries vendor type) | unknown API error class | SYS (vendor) | "vendor error: <type>" |
| 1.10 | `max_tokens` | response cut at token ceiling | BOTH | "turn cut short at the token ceiling" |
| 1.11 | `max_output_tokens` | request refused at output ceiling | SYS (vendor) | "output ceiling refused the request" |
| 1.12 | `refusal` | model refused to continue (witnessed only by the response frame, `res/response.go:187-196`) | BOTH (model act, ruled as turn failure) | "model refused to continue - no answer" |
| 1.13 | `query_died` (unexpected_eof / iterator_failure) | agent stream closed or SDK iterator threw (`res/turnended.go:577-667`) | SYS (agent-repl/vendor SDK) | already footer: keep "vendor query died - the next prompt restarts it" |
| 1.14 | `billing_error` | vendor billing refusal | SYS (account) | "billing error - check the account" |
| 1.15 | `model_not_found` | model id unknown | SYS (config) | "model not found" |
| 1.16 | `oauth_org_not_allowed` | org not permitted | SYS (account) | "this organization is not allowed" |
| 1.17 | `max_turns` (`FailureVendorMaxTurns`) | user-set round-trip ceiling hit | FACT (ran to a limit) | "stopped at the turn limit" |
| 1.18 | `max_budget` (`FailureVendorMaxBudget`) | spend ceiling hit | FACT | "stopped at the budget" |
| 1.19 | `execution_error` | run broke while executing | SYS (vendor) | "the run broke while executing" |
| 1.20 | `turn_failed` (stop_reason names: `prompt_too_long`, `blocking_limit`, `rapid_refill_breaker`, `image_error`, `model_error`, `malformed_tool_use_exhausted`, `hook_stopped`, `tool_deferred*`, `structured_output_retry_exhausted`, `turn_setup_failed`, `continuation_prevented`, `lost:*`, `closed:orphaned`, `closed:failed`, `unset`) | every other abnormal end (`res/turnended.go:471-560`, `turnclosed.go`) | mostly SYS; `prompt_too_long`, `image_error`, `malformed_tool_use_exhausted` are BOTH | "turn failed: <sentence>" (one wording per stop_reason, already composed daemon-side) |
| 1.21 | `stop_hook_prevented` | a Stop hook forbade ending the run | FACT (a hook acted) | "a Stop hook ended the run" |
| 1.22 | `agent_process_died` | agent process died with the turn (`turnclosed.go` `CloseAgentDied`) | SYS | "agent process died - the turn ended with it" |

Evidence riders on the same bubble headline (not separate rows): mid-turn api failure that was retried (`res/sink.go:350`), "a compaction failed and nothing was cut" (`res/separation.go:237`), both drawn inside the headline parentheses (`turnended.go:355-358`).

Related, same bubble, not an error: `interrupted` (`wa/feed/rows/turn-ended.ts:117-120`, `INTERRUPTED_SENTENCE = "the turn was interrupted"`) draws the same red-bordered bubble except for an interjection. It is a user act and a FACT; if error bubbles go, this one needs an explicit decision (neutral line or footer transient). The footer already has `interrupted` status.

### 2. Other per-entry error arms

| # | Name | Produced (file:line; proto) | Trigger | Shows (text/color/controls) | Class | Proposed footer line if moved |
|---|---|---|---|---|---|---|
| 2 | Response cut short (`FeedResponse.error`) | `res/response.go:187`, `res/thinking.go:134`; `feed.proto:822` `FeedResponseError` | a response or thinking block died mid-arrival | partial prose stays, `response-cut-short-marker` "cut short" (`wa/feed/cards/response.ts:593`); the WHY is item 1 | FACT (what landed is conversation content) | none: footer carries the cause via item 1; the marker is a fact about the bubble |
| 3 | Vendor-synthesized notice (`FeedResponse.notice`) | `res/response.go:144-162`, heading `:377`; `feed.proto:~765` | vendor injects usage_limit, usage_transition, usage_warning (or unclassified) notice | notice-register bubble with heading "Notice - your allowance is exhausted / window changed / approaching a limit. This is the vendor's own message, not the agent's." | BOTH (vendor machinery text that the vendor placed in the conversation) | "allowance exhausted" / "approaching allowance limit" (footer already has `vendor_fault` usage-limit and an enduring usage line) |
| 4 | Tool call failed (`FeedToolCallReturned.failed`) | `res/toolcall.go:95`; `feed.proto:1032`; includes a shim-ruled wedge (stalled call arrives as `returned.failed` with the shim's wedge text, `feed.proto:938`) | tool returned error, or shim ruled a call wedged | red "error" badge, output is the error text (`wa/feed/cards/tool-call.ts:572-579`, stderr class `:606`) | FACT; wedge variant is SYS | for wedge only: "tool call stalled: <tool>" |
| 5 | Tool call denied (`FeedSimpleToolCall.denied`) | `res/toolcall.go:453` (`deniedOutcome`), `:93` guard; `feed.proto:950` | permission gate refused; call never ran | "denied" badge (`tool-call.ts:411-417`) | FACT (gate decision in the conversation) | none |
| 6 | Hook blocked (`FeedHook.blocked`) | `res/bubbles.go:380`; `feed.proto:1201` | hook refused the gated action | `tool-card tool-hook tool-hook-blocked` loud card, hook's own reason verbatim (`wa/feed/cards/hook.ts:51`, `:167`) | FACT | none |
| 7 | Hook failed (`FeedHook.failed`) | `res/bubbles.go:386-391`; `feed.proto:1205` | hook itself errored (non-blocking error) | ordinary card, exit chip (red when non-zero), capped output (`hook.ts:195-205`) | BOTH (hook machinery failing, shown in the conversation) | "hook failed: <name> (exit N)" |
| 8 | Skill failed | `res/skill.go:88`; `feed.proto:1164` | skill invocation failed | `badge err "failed"` + composed text (`wa/feed/cards/skill.ts:51`, `:175`) | FACT | none |
| 9 | Skill denied | `res/skill.go:33`, `res/permission.go:333`; `feed.proto:1170` | gate refused the skill | `badge err "denied"` (`skill.ts:52`, `:116`) | FACT | none |
| 10 | Plan failed | `res/bubbles.go:104` (vendor-stated failure), `:148` (episode broken: "the turn ended while plan mode was still open" / "the query died while plan mode was still open", `res/turnended.go:79`, `:605`); `feed.proto:615` | plan-mode call failed or the turn ended inside plan mode | `badge err "failed"`, `plan-failed` text (`wa/feed/cards/plan.ts:49`, `:147`) | `:104` FACT; `:148` is a DUPLICATE of item 1 re-stated per episode | none for `:104`; `:148` duplicate disappears when item 1 moves |
| 11 | Artifact publish failed | `res/bubbles.go:319`; `feed.proto:728` | artifact publish failed | `badge err "failed"`, `artifact-failed` text (`wa/feed/cards/artifact.ts:45`, `:129`) | FACT | none |
| 12 | Subagent failed | `res/subagent.go:550`; `feed.proto:1523` | subagent ended on its own failure | "failed" word, red badge, red filled dot on the bubble head (`wa/feed/rows/subagent.ts:66-69`) | FACT | none |
| 13 | Subagent lost sight of | `res/subagent.go:535-541`, causes `res/seams.go:163-188`; `feed.proto:1517-1535` | sidecar can no longer see the run: transcript vanished, went silent, boot sweep found no producer | muted "lost sight of" badge (`subagent.ts:70`); not a failure by design | SYS (sidecar staleness ruling) | "lost sight of subagent <name>: <cause>" |
| 14 | Detached shell failed | `res/subagent.go:1398-1400`; `feed.proto:1643` | non-zero exit or signal | red filled dot on the shell head, exit chip with code | FACT | none |
| 15 | Detached shell lost | `feed.proto:~1617` (`FeedShellLost`) | spool gone or silent past the shim's ruling | muted "lost" treatment, same family as 13 | SYS | "lost sight of shell <cmd>: <cause>" |
| 16 | Permission denied by policy (`FeedPermissionDeniedByPolicy`) | `res/permission.go:274`; `feed.proto:1751` | a rule refused with nobody asked | `badge err`, daemon text verbatim (`wa/feed/asks/permission.ts:249-253`) | FACT (a decision) | none |
| 17 | Permission denied undecidable | `res/permission.go:286`; `feed.proto:1743` | classifier reached no verdict, no rule applied, call denied "for want of a decider" | `badge err` + class `arm-deniedUndecidable`, daemon text verbatim (`permission.ts:254-260`) | BOTH (the gate machinery failed to decide) | "permission undecidable - call denied for want of a decider" |
| 18 | Permission denied by user | `res/permission.go:264`; `feed.proto:1749` | user clicked deny | `badge err` "denied by user" (`permission.ts:83`, `:246`) | FACT; the proto itself says "a DENIAL IS AN ANSWER, not an error" (`feed.proto:1725`) | none; consider restyling away from error red |
| 19 | Agent prompt refused (`FeedAgentPrompt.refused`) | `res/sendmessage.go:207-212`; `feed.proto:1969-1997` | an agent's send to another agent was refused | prompt-delivery marker wearing `.refused`, plus the producer's reason (`wa/feed/rows/agent-prompt.ts:119-153`) | FACT | none |
| 20 | Command refused card (`FeedCommandRefused`) | `res/resolver.go:1293-1301` (`UpsertCommandRefused`, non-durable, root feed); `feed.proto:371-396` | user typed a recognized-but-unsupported command (`/agents`, `/help`, ...) | `command-refused` card: command (monospace), daemon sentence, optional "Engineer support for it" button (`wa/panels/refused.ts:41-110`); button failure draws an inline refusal | SYS (agent-repl feature gap) and not part of the vendor conversation (synthesized, never stored) | transient footer line "/agents is not supported here" (the support offer needs a new home; see section 4) |
| 21 | Compaction failed divider | `res/separation.go:233-243`; `feed.proto:2139`, `:2329` | a compaction was attempted and failed | `sep-compaction-failed` line in the divider slot, producer's error verbatim, `log.warn` (`wa/feed/rows/separation.ts:229-238`) | BOTH | already in footer as `context_budget` "compaction failed - ..." (`footer.proto:393-399`); also an evidence rider in item 1 |
| 22 | Compaction cold-read warning | no daemon producer; `feed.proto:2179-2185` | compaction re-read at the uncached rate | `sep-cold-read`: "read cold: N uncached input tokens", `log.warn` (`separation.ts:207-220`) | SYS (cost waste) | "compaction re-read N uncached tokens" |
| 23 | Merge failed / abandoned terminal | `daemon/internal/merge/terminal.go:96` (failed), `:702` (abandoned); `feed.proto:2308-2326` | merge gave up, or left the queue unrun (user drop, dequeue, workspace closed, daemon shutdown) | `merge-error`: clock, badge "failed" / "abandoned", daemon summary (`wa/feed/merge/merge.ts:143-165`) | FACT (the merge is the work) and SYS | already footer `merge_failed` (conflicts / tests / other, turquoise); keep the summary sentence there |
| 24 | Merge tab failed; merge test suite failed | `daemon/internal/merge/tabs.go:106`, `testgate.go:105`; `feed.proto:2413`, `:2530` | a merge phase tab settled failed; a test suite in the tests tab failed | failed glyph in the tab / suite row (`wa/feed/merge/tests-tab.ts:96-98`, `tab-row.ts`) | FACT (detail of item 23) | none; sub-detail of 23 |
| 25 | Page error (`FeedPageError`) | contract `feed.proto:430-445`, `failure.proto:224`; NO daemon producer; drawn `wa/feed/feed-view.ts:555-580` into `.feed-page-error-slot` (`:347`) | history replay ended short of the live window, leaving a gap | headline (daemon sentence, tone class) + evidence reason; `log.error` (`feed-view.ts:578`) | SYS | "history replay truncated: delivered N of M records (<reason>)" |
| 26 | Malformed row placeholder | `wa/feed/feed-view.ts:1552-1568`, called `:1348` | the client cannot draw a row (unknown arm, missing required field) | compact `.row-malformed` "unreadable row at <path>", `log.error`, AND reports `frameUndecodable` to the topbar warning chip | SYS (client/contract skew) | already partly in chip ("frame undecodable"); a footer line "cannot read a feed row at <path> - reload" |

### 3. Inline refusals drawn at controls inside the feed host

These are answers to a click, drawn as `span.refusal[data-arm]` beside the control (`wa/rpc/refuse.ts:51-58`, `wa/feed/feed-view.ts:1894-1901`). They are not rows, but they are dedicated error items in the feed's DOM.

| # | Where | Produced | Trigger | Text | Class | Footer? |
|---|---|---|---|---|---|---|
| 27 | "older" control, transport | `wa/feed/feed-view.ts:1598` | `GetFeedPage next` call never landed | "the daemon could not be reached" | SYS | already covered by `unary_transport` client failure; duplicate |
| 28 | "older" control, daemon error | `wa/feed/feed-view.ts:1609` | `GetFeedPage` returned `error` (no walk standing, feed not in workspace) | "older pages are not available" | SYS | footer line "older history unavailable" |
| 29 | Bubble head, open sub-feed | `wa/feed/bubble.ts:286`, `:308` | `OpenFeed` transport failure / daemon refusal | "the daemon could not be reached" / "this bubble's feed could not be opened" | SYS | footer line; the tail-died reopen failure ALREADY goes to footer as `feed_not_tailing` (`bubble.ts:419-430`) |
| 30 | Subagent stop button | `wa/feed/rows/subagent.ts:305-362` | `Interrupt` refused or transport | shared interrupt sentences (`interrupt-error.ts`) | SYS/answer | footer transient or keep local |
| 31 | Shell stop button | `wa/feed/cards/shell.ts:350`, `:413` | same | same | SYS/answer | same |
| 32 | Permission card answer | `wa/feed/asks/permission.ts:360-365`, `:403-411` | `AnswerPermission` refused (ask no longer standing, no standing offer, no session) | per-cause sentences | answer to a click | keep at control or footer transient |
| 33 | Question answer | `wa/feed/asks/question.ts:602`, `:634` | `AnswerQuestion` refused | per-cause sentences | answer to a click | same |
| 34 | Cold gate answer | `wa/feed/asks/cold-gate.ts:536-545`, `:583-591`, `OWN_CAUSES.reopenFailed` | `AnswerColdGate` refused or re-open failed | e.g. "the session did not come back from the re-open: <detail>" | SYS | the re-open failure ALREADY has the footer fault `cold_gate_reopen_failed`; inline copy is a duplicate |
| 35 | Select feed row | `wa/feed/select-feed-row.ts:72` | `SelectFeedRow` refused | per-cause sentences | answer | keep or footer |
| 36 | Open link | `wa/link.ts:360`, `:415` | open-external / open-in-editor unreadable or refused | refusal span near the link | answer | keep or footer |
| 37 | Command refused "support" request | `wa/panels/refused.ts:139-180` | `RequestCommandSupport` refused | cross-cutting sentence at the button | answer | keep with item 20 |
| 38 | Malformed refusal | `wa/rpc/refuse.ts` `drawMalformedRefusal` (used by all of the above) | an error this build cannot read | "malformed" arm text | SYS | already reports `frameUndecodable` |

Held-prompt tray (`daemon_hold.proto:318` `HeldPromptClassificationError`, `wa/tray/held-prompt.ts:1051`) is drawn beside the feed, not in it. Out of scope, noted for completeness.

## 2. Errors that ALREADY go to the footer (for contrast)

Daemon status arms and salient lines (`footer.proto`):

| Surface | Source | Condition |
|---|---|---|
| `agent_repl_fault` (blue, composer closed) | `daemon/internal/health/footer.go`, `kinds.go` | shim start failed, resume failed, relaunch resume failed, adoption window expired, cold-gate reopen failed (`KindColdGateReopenFailed`), shim died, bounce died, session absent, link severed, daemon impaired |
| `vendor_fault` (turquoise, usable) | `health/footer.go:106-110` | vendor start retrying / rejected / failed; auth prompt, usage limit, billing, vendor error, retried API call |
| `network_fault` (blue) | `KindNetworkUnreachable` | machine offline |
| `turn_failed` (turquoise) | `footer.proto:408` | last turn failed, NO cause text (the feed row is the account) |
| `degraded` (turquoise) | `footer.proto:418` | shim observation degraded; state unreported |
| `merge_failed` (turquoise), conflicts/tests/other | `footer.proto:808-828` | merge gave up |
| salient `query_died` | `footer.proto:372-380` | "vendor query died - the next prompt restarts it" |
| salient `retrying` | `footer.proto:499-507` | vendor call being retried |
| salient `context_budget` | `footer.proto:393`, `:520` | vendor budget warning, or "compaction failed - ..." |
| salient `notification` | `footer.proto:388`, `:512` | agent push notification |
| fault `final_answer_unresolved` | `res/finalanswer.go:112-250`, `health/kinds.go:80` | turn concluded but its answer did not land (no answer named, answer row unresolved, stalled) |
| LoadFeedThrough target error | `wa/feed/feed-view.ts:1668-1676` | "THE DAEMON PUBLISHES THE FOOTER LINE for a target-scoped error" |

Client-side:

| Surface | Source | Condition |
|---|---|---|
| Disconnected status substatus words (8 kinds) | `wa/rpc/link.ts:40-48`, `:102` | `unary_transport`, `stream_ended`, `subscription_source_ended`, `unsubscribe_failed`, `client_log_failed`, `daemon_unreachable_card`, `frame_undecodable_card`, `feed_not_tailing` |
| Topbar warning chip (6 client-local `FailureKind` arms) | `wa/failure/sink.ts:30-37`, `local.ts` | `daemon_unreachable`, `workspace_gone`, `boot_failed`, `control_plane_failed`, `frame_undecodable`, `stale_bundle` |
| Daemon `FailureKind` arms 1-11 | `failure.proto:93-120` | shim version mismatch, seq regression, shim degraded, store write rejected, session deleted / superseded / shim died / start failed / resume failed / ended unclassified, internal unclassified; "reach the user through the footer's disconnected and blocked families and the topbar's pushed warnings" (`wa/failure/local.ts:4-9`) |

## 3. Counts

Feed items catalogued: 38 numbered rows, plus the 22 arms inside item 1 (one drawing).

- Per-entry error arms (items 1-26): 26 rows; item 1 expands to 22 arms.
  - FACT, clearly: 2, 4 (non-wedge), 5, 6, 8, 9, 11, 12, 14, 16, 18, 19, 24, and item 1 arms 1.17, 1.18, 1.21 (13 rows plus 3 arms).
  - SYS, clearly: 13, 15, 20, 22, 25, 26, and item 1 arms 1.1-1.9, 1.11, 1.13-1.16, 1.19, 1.22 (6 rows plus 17 arms).
  - BOTH or ambiguous: 3, 7, 10, 17, 21, 23, and 4 (wedge), and item 1 arms 1.10, 1.12, 1.20 (7 rows plus 3 arms).
- Inline refusals at controls (items 27-38): 12 rows, all answers to a click or a transport failure; 3 are duplicates of footer reports that already exist (27, 29 reopen, 34).

## 4. Notes for the owner

1. Clearly footer-bound (system faults, no conversation content):
   - page error `history_replay_truncated` (25), malformed row placeholder (26), compaction cold-read (22), subagent/shell "lost sight of" (13, 15), and the transport-class refusals at "older" and bubble heads (27-29, which already file `unary_transport`/`feed_not_tailing`).
   - Turn-ended arms that are plainly vendor, account or agent-repl machinery: 1.1-1.9, 1.11, 1.13-1.16, 1.19, 1.22.
   - The command-refused card (20) is synthesized, never stored and not part of the vendor conversation; it is a product limitation, so it fits the footer better than a feed row.

2. Clearly stays in the feed (it is what happened in the conversation):
   - tool failed and denied (4, 5), hook blocked (6), skill failed and denied (8, 9), artifact failed (11), subagent failed (12), shell failed (14), permission verdicts (16, 18), agent prompt refused (19), response cut short (2), merge outcome bubble (23, 24).

3. Ambiguous, owner decision needed:
   - The turn-ended bubble as a whole (item 1). Removing it loses the cause because the footer `turn_failed` status has no substatus. Decide whether the footer gets per-cause lines (the daemon already composes one headline sentence per arm, so the wording exists) and whether `interrupted` keeps any feed line.
   - `max_turns`, `max_budget`, `stop_hook_prevented` (1.17, 1.18, 1.21): turn endings the user or their config caused; arguably facts.
   - `refusal` and `max_tokens` (1.10, 1.12): the model's own act or limit, but ruled a turn failure.
   - Hook failed (7) and permission denied undecidable (17): a fact in the conversation caused by machinery failing.
   - Vendor notices (3): vendor text placed in the conversation, but about allowance (account state the footer already shows).
   - Compaction failed (21): already triple-surfaced (divider, turn headline rider, footer `context_budget`); at least two of the three are redundant.
   - Plan failed at `res/bubbles.go:148` (10): a derived duplicate of the turn terminal; it disappears for free if item 1 moves.
   - Merge failed (23): the footer already has `merge_failed`, but the summary sentence ("rebase conflicts in ...") lives only in the bubble.
   - Command-refused (20): its "Engineer support for it" button has no footer equivalent; the footer would need a control or the offer must be dropped.

4. Cleanup implied (separate decisions, not done here):
   - `FailureCardRef` (orphan), `FeedPageError` (no producer), `FeedContextCutColdRead` (no producer), and the `failure_sides` color table's "card" vocabulary are dead or vestigial for feed cards.
   - `FeedTurnEnded.errored` is also the data source for the roster/footer `turn_failed` classification; the row must keep existing as the liveness anchor (`feed.proto:1213-1227`) even if it stops drawing.

## 5. Method

Read: `proto/src/frontend/v1/{feed,failure,footer,daemon_hold}.proto`, `proto/vocab/render-colors.json`; `daemon/internal/resolve/feed/*.go` (producers via grep of every `Failed`/`Denied`/`Refused`/`Error` arm constructor), `daemon/internal/merge/{terminal,tabs,testgate}.go`, `daemon/internal/health/*.go`; `webapp/src/feed/**`, `webapp/src/panels/refused.ts`, `webapp/src/rpc/{refuse,link}.ts`, `webapp/src/failure/*`. No builds, tests or services were run. Line numbers are as of master a9a309305 and approximate within a few lines where marked with `~`.
