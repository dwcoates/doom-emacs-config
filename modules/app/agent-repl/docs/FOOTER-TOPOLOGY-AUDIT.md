# Footer topology audit

The owner's ruling of 2026-09-13 (`REALTEST-JUDGEMENT-CALLS.md`, "Owner ruling:
every daemon update reaches the footer"): every update the daemon produces for a
workspace must reach the user through the footer or the feed. This is the
catalogue of every response vector from the daemon to the webapp, and of the
ones that reach neither.

The incident behind it: a failed `AnswerColdGate` at 17:37:03 and 17:55:21. The
owner answered a cold gate with "clear and start fresh", the shim's
`StartSession` failed, the daemon logged it four times at ERROR, the webapp
logged `rpc.unary-transport-failure`, and nothing durable was drawn. The full
trace is §4; the log records are §7.

Nothing here is decided. Where a remediation needs a proto change it is
described, not chosen.

## 1. Four structural facts

The first three decide every row below; the fourth ties the roster's verdict to
the footer's. They are stated once.

| # | fact | evidence |
|---|---|---|
| F1 | **The footer is written by the daemon, and — since the owner's ruling of 2026-09-13 — by the CLIENT'S OWN LINK VERDICT.** `FooterView` arrives on `WatchFooter` and the webapp replaces the whole dock from it; there was no client-side write path into the footer at all, so nothing the CLIENT observed — a transport failure, a stream ending, an unreadable frame — could reach it. That is now the one carved exception: every failing client site reports through `reportClientFailure`, and while a verdict stands the footer composes its three status cells locally (`disconnected`, the kind's substatus, the site's ad-hoc line) over the daemon's last clock, tokens and chips. Every N2 and N3 row below is therefore **drawn: footer (client verdict)**. | `webapp/src/rpc/link.ts`; `webapp/src/footer/footer.ts` (`draw`'s verdict branch); `webapp/src/footer/strip.ts` (`drawClientDisconnectedStrip`) |
| F2 | **`FooterStatusDisconnected` is about the daemon→shim link, not the webapp→daemon link.** Its five substatuses (`starting`, `degraded`, `severed`, `dead`, `start_failed`) all describe the shim. A webapp that cannot reach the daemon has no arm to be drawn in, and could not be pushed one if it had — which is why the client's own substatus is a locally composed WORD ("daemon unreachable", "feed not tailing", "frame unreadable") and not a sixth arm. **No proto change was made or needed.** | `proto/src/frontend/v1/footer.proto:536-564` |
| F3 | **The daemon's fault table reaches Emacs, not the webapp.** Open faults are rendered as `HostFault` onto `WatchHostWorkspace`, which only the elisp host subscribes to. `WatchWebWorkspace` carries exactly two arms, `transferred` and `session_identity`. `frontend.v1.FailureKind`'s eleven daemon-minted arms — `session_start_failed` among them — are imported by the webapp only in its own client-local failure sink. | `daemon/internal/server/host.go:418-446`; `proto/src/agentrepl/v1/endpoint_watch_web_workspace.proto:27`; `webapp/src/failure/sink.ts:24`, and `sessionStartFailed` appears nowhere under `webapp/src` |
| F4 | **The roster's `vendor_blocked` and `turn_failed` and the footer's `blocked` are ONE classifier (owner rulings 2026-09-14 and 2026-09-28).** A turn-ending failure decides all of them through `ladder.ClassifyFailure`, called by the footer in `OnAgentTerminal` and by the roster in its own `OnAgentTerminal`, so the strip and the rail dot cannot disagree about the same failure. `vendor_blocked`/`blocked` is ONLY the vendor or the account (an api request failure, a blocking limit, a rapid-refill breaker, a model error, or a rejected rate limit via `ladder.RateLimitBlocks`); every other failure is the turn's own `turn_failed` (footer `idle · turn_failed`), and a Stop hook or a deferred tool is an expected stop drawn as `done`. The bug this first closed was the drift — the roster kept a private allowlist that omitted `authentication_failed`, so a session the footer painted `blocked` read `done`/`ready` (green) on the sidebar. | `daemon/internal/resolve/ladder/failure.go` (`ClassifyFailure`, `RateLimitBlocks`); `daemon/internal/resolve/footer/chips.go` (`blockFor`); `daemon/internal/resolve/sidebar/resolver.go`; `proto/vocab/render-colors.json` |

## 2. Vector table

`footer/feed?` answers the owner's ruling directly: whether the fact reaches the
footer or the feed. A `.refusal` span drawn beside the clicked control is
recorded as **control (ephemeral)** — it is a real draw, but it is not the
footer or the feed, and a feed row upsert replaces the row's DOM and destroys it
(`webapp/src/feed/feed-view.ts:307-308`, "replace in place if seen").

### 2a. Standing streams

All eight webapp subscriptions are multiplexed onto the page's one `WatchPage`
stream (`webapp/src/rpc/page-streams.ts`).

| stream | daemon-side event | wire carrier | drawn | footer/feed? | evidence |
|---|---|---|---|---|---|
| `WatchFooter` | the whole footer view | `FooterView` | footer strip + expanded section | yes | `webapp/src/footer/footer.ts:88,129` |
| `WatchFeed` | feed rows, incl. 22 `FeedTurnEndedErrored` arms | `FeedRow` | feed rows and cards | yes | `webapp/src/feed/feed.ts:193`; `proto/src/frontend/v1/feed.proto:918-970` |
| `WatchTopbar` | 5 warning kinds (`accounting`, `unmodeled_tool`, `detached_unmodeled`, `session_fault`, `degraded_window`) | `TopbarWarning.detail` | topbar warning cells | no (topbar only) | `webapp/src/topbar/warnings.ts:165-173`; `proto/src/frontend/v1/topbar.proto:267-282` |
| `WatchWorkspaceRoster` | 8 error/fault row statuses (`vendor_blocked`, `init`, `severed`, `start_failed`, `degraded`, `dead`, `merge_conflict`, `merge_failed`) | `RosterRow.status` | sidebar rows | no (sidebar only) | `webapp/src/sidebar/sidebar.ts:182`; `proto/src/frontend/v1/sidebar.proto:222-274` |
| `WatchDaemonHolds` | the hold tray | `DaemonHoldTray` | hold tray | no (tray only) | `webapp/src/tray/tray.ts:59` |
| `WatchWebWorkspace` | `transferred`, `session_identity` | 2 arms | move banner, login chip | no | `webapp/src/lifecycle/lifecycle.ts:165` |
| `WatchDaemon` | `shutdown_announced`, `drain_scheduled`, `drain_cancelled` | 3 arms | lifecycle banner | no | `webapp/src/lifecycle/lifecycle.ts:186` |
| `WatchLoginTerminal` | `bytes`, `closed` | 2 arms | login overlay | no | `webapp/src/login/link.ts:46` |
| `WatchHostWorkspace` | `host`, `notification`, `transferred`, `reload_webapp`, `open_in_editor`, **and every open `HostFault`** | `HostFault[]` | **NOWHERE** — the webapp never subscribes | no | `daemon/internal/server/host.go:418-446`; no `watchHostWorkspace` under `webapp/src` |

### 2b. Stream endings and frame faults

| condition | observed by | wire carrier | drawn | footer/feed? | evidence |
|---|---|---|---|---|---|
| the page's `WatchPage` stream ends uncancelled | client | none (the link died) | `daemon_unreachable` overlay card, window-shaped, retracted on the next push | no | `webapp/src/rpc/streams.ts:197-205`, `:172-183` |
| a subscription ends `failed` | daemon | `PageSubscriptionFailed{code,message}` | rebuilt into a `ConnectError`, surfaces as the same `daemon_unreachable` card | no | `webapp/src/rpc/page-streams.ts:306-336` |
| a subscription ends `source_ended` | daemon | `PageSubscriptionEnded.source_ended` | **NOWHERE** — the queue is closed, `log.info` only | no | `webapp/src/rpc/page-streams.ts:295-305` |
| a subscription ends `unsubscribed` | this page | `PageSubscriptionEnded.unsubscribed` | nothing (correct: the page asked) | n/a | `webapp/src/rpc/page-streams.ts:286-294` |
| a push cannot be decoded | client | none | `frame_undecodable` overlay card | no | `webapp/src/rpc/streams.ts:187-195` |
| a `WatchPage` frame has no arm set | client | none | `MalformedView` thrown out of the mux | see below | `webapp/src/rpc/page-streams.ts:345-349` |

### 2c. Unary rpcs the webapp calls

Thirty-three rpcs, all through `callUnary` except `ClientLog`. Every one of them
answers `oneof result { success | error }`, and every workspace-scoped `error`
carries the cross-cutting four (`unknown_workspace`, `workspace_ref_mismatch`,
`transferring_away`, `not_yet_adopted`) plus the endpoint's own arms.

**The refusal path is uniform and correct at the control**: `refusalOf` words the
arm, the call site appends a `.refusal` span, `transferring_away` additionally
raises the page-wide move notice (`webapp/src/rpc/refuse.ts:108-115`). None of it
reaches the footer or the feed.

**The transport path is uniform too, and that is the problem**: `callUnary` logs
`rpc.unary-transport-failure` at error and throws; the call site appends
`refusal("transport", "the daemon could not be reached")` and files nothing.

| rpc | call site | refusal drawn | transport failure drawn | footer/feed? |
|---|---|---|---|---|
| `SubmitPrompt` | `webapp/src/composer/composer.ts:204` | control | control (`:290`) | no |
| `AnswerColdGate` | `webapp/src/feed/asks/cold-gate.ts:485` | control (`:526`) | control (`:495`) | **yes, since 2026-09-14** — the answer is a footer ACT from the click to the outcome: `thinking · <the chosen remediation>` with the daemon's own progress line, published before the shim is dialed and updated for every compaction phase the shim relays. The card draws the SAME sentence in its own slot. `daemon/internal/workspace/answers.go`; `daemon/internal/resolve/footer/compaction.go`; `webapp/src/footer/progress.ts` |
| `AnswerPermission` | `webapp/src/feed/asks/permission.ts:349` | control | control (`:362`) | no |
| `AnswerQuestion` | `webapp/src/feed/asks/question.ts:395` | control | control (`:405`) | no |
| `AnswerHeldOffer` | `webapp/src/tray/held-offer.ts:140` | control (`:160`) | control (`:170`) | no |
| `UpdateHeldPrompt` | `webapp/src/tray/held-prompt.ts:540` | control | control (`:576`) | no |
| `Interrupt` (footer stop) | `webapp/src/footer/stop.ts:188` | control | control (`:213`); a malformed answer also files `frame_undecodable` (`:209`) | no |
| `Interrupt` (shell card) | `webapp/src/feed/cards/shell.ts:319` | control | control (`:329`) | no |
| `Interrupt` (subagent row) | `webapp/src/feed/rows/subagent.ts:269` | control | control (`:278`) | no |
| `SetModel` | `webapp/src/topbar/model.ts:368` | control | control (`:383`) | no |
| `SetPermissionMode` | `webapp/src/topbar/permission-mode.ts:125` | control | control (`:140`) | no |
| `RequestCommandSupport` | `webapp/src/panels/refused.ts:147` | control (`:174`) | control (`:183`) | no |
| `GetFeedPage` | `webapp/src/feed/feed-view.ts:578` | control (`:599`) | control (`:588`) | no |
| `SelectWorkspace` (merge queue) | `webapp/src/feed/merge/queue.ts:144` | control (`:164`) | control (`:152`) | no |
| `OpenExternal`, `OpenInEditor` | `webapp/src/link.ts:154,190` | control | control (`:179,:218`) | no |
| `OpenLogin`, `CloseLogin` | `webapp/src/login/login.ts:208,246` | control | control (`:215,:255,:370`) | no |
| `SendLoginInput` | `webapp/src/login/link.ts:51,68` | control | control | no |
| `CreateWorkspace`, `OpenWorkspace`, `CloseWorkspace`, `KillWorkspace`, `NukeWorkspace`, `MergeWorkspace`, `RestartWorkspace`, `SetWorkspacePriority`, `AssignWorkspaceTask`, `CreateTask`, `UpdateTask`, `SelectWorkspace` (row) | all via `runVerb`, `webapp/src/sidebar/verbs.ts:502` | control (`:519`) | control (`:532`), logged `sidebar.verbs.failed` (`:529`) | no |
| `AdoptWebWorkspace` | `webapp/src/lifecycle/lifecycle.ts:554` | `control_plane_failed` overlay card (`:591,:598`) | same card | no |
| `SubscribePage` | `webapp/src/rpc/page-streams.ts:383` | rethrown into `watchStream` → `daemon_unreachable` card | same | no |
| `OpenFeed` (root tail) | `webapp/src/feed/feed.ts:181` | **NOWHERE** — `log.error` at `:197`, the attempt ends | the retry loop files `daemon_unreachable`, which misnames a daemon that answered | no |
| `OpenFeed` (bubble expand) | `webapp/src/feed/bubble.ts:232` | control | control (`:239`) | no |
| `OpenFeed` (bubble reopen) | `webapp/src/feed/bubble.ts:345` | **NOWHERE** — `log.error` at `:360`, returns null | rethrown into the sub-feed's own retry loop | no |
| `OpenFeed` (reveal probe) | `webapp/src/feed/feed.ts:301` | **NOWHERE** — `log.warn` at `:315` | **NOWHERE** — bare `catch { return false; }` at `:307` | no |
| `UnsubscribePage` | `webapp/src/rpc/page-streams.ts:414` | **NOWHERE** — `log.debug` at `:421` | same | no |
| `ClientLog` | `webapp/src/main.ts:81-93`, deliberately outside `callUnary` | **NOWHERE** — counted in `sinkFailureCount`, console only | same | no |

### 2d. Daemon-side events, by whether any wire field carries them

| daemon event class | count | wire carrier | drawn | evidence |
|---|---|---|---|---|
| open fault kinds | 19 | `HostFault` on `WatchHostWorkspace` (Emacs only) | **NOWHERE for the webapp**, with the two exceptions below | `daemon/internal/health/kinds.go:19-78`; `daemon/internal/server/host.go:418` |
| — `shim_start_failed` | 1 | its own bespoke path: `Footer.SetStartFailed` + `Sinks.Footer.OnLink(LinkDead)` beside the fault, not derived from it | footer `disconnected/start_failed` + `FooterStatusActivityStartFailed` | `daemon/internal/workspace/sessions.go:1043-1069`; `proto/src/frontend/v1/footer.proto:523-533,548,575` |
| — `link_severed`, `shim_died` | 2 | the same `OnLink` sinks | footer `disconnected/severed` \| `dead` | `daemon/internal/health/health.go:148-151`; `daemon/internal/workspace/sessions.go:1074` |
| fault kinds with a render arm and no raise site | 4 (`log_sink_poisoned`, `deploy_script_failed`, `successor_spawn_failed`, `wsm_read_only`) | `DaemonFault` arms exist | n/a — nothing raises them | `daemon/internal/health/kinds.go:119-135` |
| `frontend.v1.FailureKind` daemon-minted arms | 11 (`shim_version_mismatch` … `internal_unclassified`) | **NONE** — no daemon code sets a `FailureKind` on any message the webapp reads | **NOWHERE** | `proto/src/frontend/v1/failure.proto:100-120`; F3 above |
| `FeedTurnEndedErrored` arms | 22 | `FeedRow` | feed cards | `daemon/internal/resolve/feed/turnended.go:245-408`; `proto/src/frontend/v1/feed.proto:918-970` |
| `FeedPageError.history_replay_truncated` | 1 | `FeedPage` | feed | `daemon/internal/resolve/feed/history.go:65` |
| typed unary refusal arms | ~60 across 25 rpcs | the rpc's own `error` arm | control (ephemeral) | `daemon/internal/workspace/refusal.go`; `daemon/internal/server/refuse.go:494` |
| non-refusal errors on a unary rpc | every `fail()` path | `connect.CodeInternal` — no arm | control (ephemeral) via `rpc.unary-transport-failure` | `daemon/internal/server/refuse.go:423-434` |
| unlanded refusal arms (an arm raised under one rpc's vocabulary and answered on another's response) | see §4 | none — `setResponseError` cannot place it | **NOWHERE**; daemon warns `daemon.refusal.unlanded_arm` and returns a Connect error | `daemon/internal/server/refuse.go:165` |
| `log.Info` progress records carrying a workspace | 209 call sites, 193 distinct messages | mostly none | the small subset the footer/feed resolvers publish independently; the rest **NOWHERE** | `daemon/internal/workspace/*.go`, `daemon/internal/rollout/*.go`, `daemon/internal/promptqueue/*.go` |

## 3. The NOWHERE and log-only rows

**Fifteen NOWHERE rows** — nothing is drawn on any surface (N1, N2, N3). A
further five (N4) are drawn somewhere, but not at the footer or the feed, and
are listed so the owner can rule on whether their surface suffices.

### N1 — daemon facts with no webapp carrier (6) — 1, 4 and 5 CLOSED

Rows 1, 4 and 5 closed on 2026-09-13 under the owner's ruling that EVERY daemon
fault kind reaches the footer. The carrier is the footer's own status tree: the
mapping is decided once in `daemon/internal/health/footer.go` (THE FAULT
PARTITION, restated normatively in `footer.proto`'s `FooterStatus` header and in
the module `AGENTS.md`), and the footer is told from the ONE place faults are
written — `health.ObserveFaults` decorates the state client every raise site
shares — rather than from plumbing beside each raise.

| # | row | drawn | evidence |
|---|---|---|---|
| 1 | every open fault kind except the three link kinds — 16 of 19 | **drawn: footer**. `disconnected · start_failed`: `shim_start_failed`, `resume_failed`, `relaunch_resume_failed`, session-scope `adoption_window_expired`, `cold_gate_reopen_failed`. `disconnected · dead`: `shim_died`, `bounce_died`, `session_absent`. `disconnected · severed`: `link_severed`, `watch_open_refused`. `blocked · daemon_impaired`: `prompts_dir_missing`, `wsm_read_only`, `log_sink_poisoned`, `deploy_script_failed`, `successor_spawn_failed`, `daemon_state_unreadable`, daemon-scope `adoption_window_expired`. NON-ESCALATING, activity line only: `shim_reported`, `classifier_failed`, `bounce_unknown`, `conversation_abandoned` — the shim answered in each, and `disconnected` closes the composer. `bounce_disposition` alone draws nothing: it is opened and closed in one breath and never stands | `daemon/internal/health/footer.go`; `daemon/internal/resolve/footer/faults.go`; `proto/src/frontend/v1/footer.proto` (`FooterStatusActivityFault`, `FooterSubStatusBlockedDaemonImpaired`) |
| 2 | `HostFault` as a whole: the webapp has no equivalent of `WatchHostWorkspace`'s fault list | still NOWHERE as a LIST — and now deliberately so. The strip draws ONE standing fault, the strongest, and the ruling's three-cell shape has no room for a list. A fault ROSTER, if one is ever wanted, is a panel question, not a strip one | `daemon/internal/server/host.go:418` |
| 3 | the eleven daemon-minted `frontend.v1.FailureKind` arms, `session_start_failed` included | **NOWHERE** — untouched. They are a client-local failure vocabulary the daemon still sets on nothing | `proto/src/frontend/v1/failure.proto:100-120` |
| 4 | an `AnswerColdGate` whose re-open failed — no arm existed (§4) | **drawn: control + footer**. `AnswerColdGateError.reopen_failed{detail}` is the answer at the buttons, and the `cold_gate_reopen_failed` fault is the same sentence on the strip under `disconnected · start_failed` | `proto/src/agentrepl/v1/endpoint_answer_cold_gate.proto`; `daemon/internal/workspace/answers.go`; `webapp/src/feed/asks/cold-gate.ts` |
| 5 | every unlanded refusal arm raised under `OpenWorkspace`'s vocabulary from inside the cold-gate re-open | **drawn for THIS path**: those refusals reach `AnswerColdGate`'s own `reopen_failed` arm, because the branch that swallowed them now answers rather than falling through to a Connect internal. The GENERAL unlanded-arm path (any rpc, any vocabulary) is untouched and is still priority 3 | `daemon/internal/workspace/answers.go`; `daemon/internal/server/refuse.go:165` |
| 6 | ~190 of the 193 distinct workspace-scoped `log.Info` progress messages | **NOWHERE** — untouched | §2d |

### N2 — client-observed conditions with nowhere to go (5) — CLOSED

**All five are now drawn: footer (client verdict).** The row states what was
missing and what reports it today.

| # | row | drawn | evidence |
|---|---|---|---|
| 7 | a unary transport failure: never reached the footer, never filed a card, and the `.refusal` span was destroyed by the next upsert of its container | **drawn: footer (client verdict)** — `unary_transport`, "`<Rpc>`: `<the daemon's own account>`" | `webapp/src/rpc/unary.ts` (the transport catch) |
| 8 | a subscription that ends `source_ended`: queue closed, `log.info` only | **drawn: footer (client verdict)** — `subscription_source_ended` | `webapp/src/rpc/page-streams.ts` (the `sourceEnded` arm) |
| 9 | `UnsubscribePage` failing: `log.debug` only | **drawn: footer (client verdict)** — `unsubscribe_failed` | `webapp/src/rpc/page-streams.ts` (`unsubscribe`'s catch) |
| 10 | `ClientLog` failing: console only, counted in `sinkFailureCount`, forwarding stops after the first failure | **drawn: footer (client verdict)** — `client_log_failed`, on a fixed line so the report→log→forward→fail loop terminates | `webapp/src/main.ts` (`clientLogSink`) |
| 11 | the `daemon_unreachable` and `frame_undecodable` cards reached the overlay but never the footer, so a footer left showing `thinking` under a dead link kept saying `thinking` | **drawn: footer (client verdict)** — `daemon_unreachable_card`, and `frame_undecodable_card` under the substatus "frame unreadable" (judgement row) | `webapp/src/failure/overlay.ts` (`report`, after the card is filed) |

### N3 — silently swallowed at a call site (4) — CLOSED

Every one reports now. A REFUSED `OpenFeed` draws the substatus "feed not
tailing" rather than "daemon unreachable", because the daemon answered — the
judgement row argues it, and row 14 below is the audit's own account of why.

| # | row | drawn | evidence |
|---|---|---|---|
| 12 | `OpenFeed` reveal probe, transport failure: was a bare `catch { return false; }` | **drawn: footer (client verdict)** — `unary_transport`, naming the probe | `webapp/src/feed/feed.ts` (`revealRow`'s catch) |
| 13 | `OpenFeed` reveal probe, refusal: `log.warn`, returns false | **drawn: footer (client verdict)** — `feed_not_tailing` | `webapp/src/feed/feed.ts` (`revealRow`'s error arm) |
| 14 | `OpenFeed` root tail, refusal: `log.error`, the attempt ends and the loop retries behind a card that says the daemon is unreachable when it is not | **drawn: footer (client verdict)** — `feed_not_tailing`. The tail's own ending verdict still replaces the LINE in the same tick; teaching the retry loop to tell a refused open from a dead link is priority 6, not this wiring | `webapp/src/feed/feed.ts` (`openAndTail`'s error arm) |
| 15 | `OpenFeed` bubble reopen, refusal: `log.error`, returns null; the sub-feed silently stops tailing | **drawn: footer (client verdict)** — `feed_not_tailing` | `webapp/src/feed/bubble.ts` (`reopen`'s error arm) |

### N4 — drawn, but not at the footer or the feed (5)

Not defects against the letter of the ruling's "or the feed" only if the surface
they land on is judged sufficient. Listed so the owner can rule.

| # | row | drawn at | evidence |
|---|---|---|---|
| 16 | five topbar warning kinds | topbar cell | `webapp/src/topbar/warnings.ts:165-173` |
| 17 | eight sidebar roster error statuses | sidebar row | `proto/src/frontend/v1/sidebar.proto:222-274` |
| 18 | six client-local failure arms | the failure overlay | `webapp/src/failure/overlay.ts:76-83` |
| 19 | ~60 typed unary refusal arms | the clicked control, ephemerally | `webapp/src/rpc/refuse.ts:117-144` |
| 20 | the workspace-moved notice | the page-wide banner | `webapp/src/rpc/moved.ts` |

## 4. The cold-gate answer failure, traced

The headline case, end to end.

| step | what happens | evidence |
|---|---|---|
| 1 | the user answers the gate; `AnswerColdGate` is sent | `webapp/src/feed/asks/cold-gate.ts:485-491` |
| 2 | the verb re-opens the session through `Fleet.ResumeCold` | `daemon/internal/workspace/answers.go:205-208` |
| 3 | `startSession` interprets the shim's `StartSession` answer | `daemon/internal/workspace/sessions.go:1195-1309` |
| 4a | **cold again** — the only clean path. A fresh gate is raised (feed row + footer + topbar), and `ResumeCold` answers the landed `AnswerColdGateError.no_session` arm on a normal 200 | `daemon/internal/workspace/sessions.go:1246-1253`, `:863-870` |
| 4b | **the shim's `StartSession` errors** — a plain `error`, logged `log.Error(opColdGate, "the re-open with the remediation failed")`, then `fail()` logs `"the rpc failed"` at ERROR and returns `connect.CodeInternal`. **This is the branch both incidents took** (§7) | `daemon/internal/workspace/answers.go:218`; `daemon/internal/server/refuse.go:423-434` |
| 4c | **`conversation_owned` / `unknown_session` / `vendor_start_failed` / `already_started` / unset cause** — a typed `*Refusal` raised under the **`OpenWorkspace`** arm vocabulary. `AnswerColdGateError` has no such arm, so `setResponseError` cannot place it and it falls to the unlanded-arm path: one WARN and a Connect error | `daemon/internal/workspace/sessions.go:1265,1276,1288,1299,1306`; `daemon/internal/server/refuse.go:165` |
| 5 | `ResumeCold` never calls `noteStartFailed`, so unlike an ordinary bring-up death this raises **no fault, no `SetStartFailed`, no dead link** | `daemon/internal/workspace/sessions.go:812-873` vs `:1043-1069` |
| 6 | `ClearColdGate`, the resolved feed row and `Footer.SetColdGate(false)` are all past the early return, so the gate stays standing and the footer keeps saying the session is parked | `daemon/internal/workspace/answers.go:224-233` |
| 7 | the webapp sees a Connect error, logs `rpc.unary-transport-failure`, and appends `refusal("transport", "the daemon could not be reached")` to the gate row's `actions` | `webapp/src/rpc/unary.ts:69-80`; `webapp/src/feed/asks/cold-gate.ts:493-496` |
| 8 | that span lives only until the next push of the same `FeedId`, which replaces the row's DOM in place | `webapp/src/feed/feed-view.ts:307-308` |

So the honest account of "nothing was drawn" is: for 4c and 4b nothing durable
exists, the one span that is drawn says the wrong thing (the daemon was reached
and answered), and the footer states the pre-answer truth throughout.

## 5. Proposed topology

One rule per class. Proto additions are **described, not decided**; the arms that
already suffice are named.

| class | rule | needs | already sufficient |
|---|---|---|---|
| **a. unary refusal** | drawn at the control that asked, AND a footer activity line naming the refused verb and its arm | a footer activity kind that can stand under any status, carrying a verb name and a composed sentence. The activity oneofs are per-status and closed (`footer.proto:170-661`), so a cross-status kind means adding one arm to each of the ten — or a single status-independent slot beside them | the refusal arms themselves: every endpoint already types its causes, and `refusalOf` already composes the sentence |
| **b. transport failure** — **SETTLED AND LANDED** (owner ruling, 2026-09-13): the client composes the three status cells itself, out of `webapp/src/rpc/link.ts`'s verdict; the daemon's clock, tokens and chips still come from its last push, and its pushed view returns when a unary is answered or a stream reads again | a footer activity saying the link to the daemon is down, plus the retractable window card that already exists | F1 and F2 blocked this outright: the footer cannot be pushed over a link that is down, and `FooterStatusDisconnected` means the shim. Either a client-composed footer cell carved explicitly out of the stateless-renderer rule, or a client-side "last known footer, stale" treatment. **This is the one class that cannot be solved by a proto addition alone** | `FailureControlPlaneFailed` (`failure.proto:128`) already carries `{what, cause}` and is client-mintable — it is the right card for a failed verb, and today only `AdoptWebWorkspace` and the login terminal use it |
| **c. stream ending** | the link's own status: `source_ended` and a `failed` subscription both draw the same window card the page stream's death draws | nothing — `PageSubscriptionEnded` already carries `how`; the gap is that `source_ended` files nothing | `FailureDaemonUnreachable` (`failure.proto:125`) |
| **d. daemon fault** | footer status/substatus for the link kinds it already covers, plus a feed failure card for every other kind | a web-side carrier for the fault list. The `HostFault` shape already exists on `WatchHostWorkspace`; the described addition is a mirror on `WatchWebWorkspace`, or a feed row arm carrying `FailureKind` so the eleven daemon-minted arms have a place to be drawn | the eleven `FailureKind` arms are already defined and already have a producer split documented in `webapp/src/failure/sink.ts:5-11` — only the carrier is missing |
| **e. progress** | a footer activity line for the operations a user waits on (bring-up, merge, adopt, revive, drain), not for all 193 | a selection: which `log.Info` operations are progress a user waits on. The footer's activity precedence is already specified (`footer.proto:118-123`), so a new kind slots into it | `FooterStatusActivityRetrying`, `FooterStatusActivityStartFailed`, `FooterStatusActivityMergingCommit` show the shape a progress kind takes |

Two contract questions the remediation cannot settle on its own, for the owner:

1. ~~Rule (b) requires the footer to draw something the daemon did not compose.~~
   **RULED, 2026-09-13**: the owner granted the exception. It is written into
   `webapp/AGENTS.md`'s standing rules and implemented in
   `webapp/src/rpc/link.ts`; the drawing is `drawClientDisconnectedStrip`.

2. Rule (a) asks whether every refused click deserves a footer line, or only
   those whose refusal the control cannot survive to show. A control inside a
   feed row cannot survive one; a topbar or sidebar control can.

## 6. Priority

| # | item | why first |
|---|---|---|
| 1 | ~~**`AnswerColdGate`'s re-open failure has no arm.**~~ **DONE** — `AnswerColdGateError.reopen_failed{detail}`, raised under this verb's own vocabulary. Add the arm(s) `AnswerColdGateError` needs so a failed re-open is an ANSWER rather than a `CodeInternal`, and stop raising `OpenWorkspace`'s vocabulary from a cold-gate call path | the ruling's own incident; the user's answer to a gate silently does nothing, twice on one day |
| 2 | ~~**`ResumeCold`'s failure raises no fault and no footer line.**~~ **DONE** — the answer path opens `cold_gate_reopen_failed`, which the partition puts at `disconnected · start_failed` with the failure's own line. A failed re-open is a bring-up death and should take `noteStartFailed`'s path — `shim_start_failed` + `SetStartFailed` + a dead link — exactly as a failed spawn does | it is the same failure the owner already ruled on for held prompts (`REALTEST-JUDGEMENT-CALLS.md` row 65, 2026-09-12), applied to the one bring-up path that was missed |
| 3 | **The unlanded-arm path draws nothing.** Any refusal the response cannot carry becomes a Connect error the user reads as "the daemon could not be reached" | it is silent by construction and affects every rpc, not just this one |
| 4 | ~~**A unary transport failure leaves nothing durable.**~~ **DONE** — it files a footer verdict; the `control_plane_failed` card is still open as described. File `control_plane_failed` from `callUnary`'s own catch, so every failed verb has a card, and settle rule (b)'s footer question | the failure sink already has the arm; this is the cheapest of the five rules |
| 5 | ~~**`source_ended` files nothing.**~~ **DONE** — it files a footer verdict. A subscription whose source finished leaves its component permanently unfed with no notice | a whole surface can go stale silently |
| 6 | **The four `OpenFeed` swallows** (each REPORTS now; what is left is the retry loop naming a refused open as an unreachable daemon) (`feed.ts:307`, `feed.ts:315`, `feed.ts:196`, `bubble.ts:359`) | a feed that stops tailing is the failure most likely to be read as "the agent is idle" |
| 7 | ~~**Sixteen fault kinds have no webapp carrier.**~~ **DONE** — the carrier is the footer's own status tree; see N1 row 1 | large but not acute: Emacs sees them today |
| 8 | **Progress selection.** Pick the operations a user waits on and give them footer activity kinds | the widest and the least urgent |

## 7. Log evidence, 2026-09-13

### The two cold-gate incidents

Workspace `0100059cb65649bc` (`explanation-engine`, dir hash `99808d49`). The
sequence at 17:37:03 repeats verbatim at 17:55:21, same producer id.

```
17:36:38.109 info  daemon.topbar.set_cold_gate  the topbar published the cold-gate view  context_tokens=101578 standing=true
17:37:03.138 error daemon.shimclient.start_session  shim call failed  connect_code=internal
             error: shim.v1.StartSession: the producer "claude-shim:90a1151f-..." has already written rows and cannot be un-named
17:37:03.138 error daemon.workspace.bring_up        the StartSession call failed
17:37:03.138 error daemon.workspace.answer_cold_gate the re-open with the remediation failed
17:37:03.138 error AnswerColdGate                   the rpc failed  code=internal
```

The webapp's mirror, the only error-level record in either per-workspace
`webapp.log` today:

```
{"timestamp":"2026-09-13T17:55:21.253-04:00","runtime":"webapp","level":"error",
 "operation":"rpc.unary-transport-failure",
 "message":"AnswerColdGate failed at the transport: [internal] shim.v1.StartSession: the producer \"claude-shim:90a1151f-...\" has already written rows and cannot be un-named",
 "context":{"code":13,"rpc":"AnswerColdGate","workspace_dir_hash":"99808d49"}}
```

Four daemon ERROR records and one webapp ERROR record per incident. Nothing was
pushed on `WatchFooter`, nothing was upserted on `WatchFeed`, and the topbar's
own cold-gate view from 17:36:38 stood unchanged. The root cause was the shim's
`SessionStart:startup` hook failing (`Executable not found in $PATH:
"powershell"`) and the store then refusing to un-name a producer that had
already written rows — neither of which any surface named.

### Other daemon ERROR/WARN of the last day, and where each landed

`~/.claude-emacs/logs/daemon.run.log`, 2026-09-12 22:56 → 2026-09-13 17:52.
Rotated siblings are all 2026-09-10 or earlier.

| level | operation | count | drawn anywhere? |
|---|---|---|---|
| error | `daemon.boot.bring_up` — `start session for "2b81f45a..."` | 10 (16:11:29 → 17:52:06) | **NOWHERE** — a boot-path bring-up death; the workspace has no page open to push to |
| warn | `daemon.rollout.reconcile` — "a session's bounce disposition needs a human" (`disposition=UNKNOWN`) | 4 (15:07:35 → 15:34:49) | **NOWHERE** — raises `bounce_unknown`, which has no webapp carrier (N1 row 1) |
| warn | `daemon.rollout.reconcile` — "sessions survived a bounce that wrote no intent manifest" | 4 | **NOWHERE** — same |
| warn | `daemon.health.open_fault` — `kind=link_severed` | 1 (16:03:25) | footer `disconnected/severed` — one of the three kinds that does reach it |
| error | `daemon.server.accept` / `daemon.server.assets` — `too many open files in system` | 3 | **NOWHERE** — no fault kind, no carrier |
| error | `daemon.cmd.boot` — boot reconciliation exceeded its 90000ms bound, exiting | 1 (14:38:29) | **NOWHERE** on the web side; Emacs sees the daemon exit |
| error | `daemon.cmd.serve` — "a background loop outlived its serving context" | 1 (15:19:07) | **NOWHERE** |
| warn | `daemon.server.await_quiet` — "the exit's grace expired with calls still being answered" | 1 (15:19:03) | **NOWHERE** |

`sidebar.verbs.failed` appears three times in the webapp logs, all on
2026-09-11, none today. No stream-ending record appears in either webapp log
today.

### Adjacent runtimes with error volume in the same window

Not daemon→webapp vectors, listed so they are not mistaken for one: the sidecar
logged errors under `cancel-terminal` (31), `discover-meta` (30),
`storeclient-write-batch` (17), `store-write` (12) and `storeclient-cursors`
(7); the store logged `store.db.write-batch` errors (9, `SQLITE_BUSY`) and
`store.db.slow-query` warnings (23). None of these has any path to a webapp
surface either, and none names `AnswerColdGate`, `StartSession` or `cold_gate`.
