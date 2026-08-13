# `frontend.v1/shared.proto` — message inventory

Every one of the 142 declarations in `shared.proto`, with what it is for, whether
it re-spells `conversation.v1`, and which of the four canonical UI files reach
it.

**How this was produced.** Twenty independent agents, each given a coherent
family of messages and the whole proto tree, working only from the schema and
the daemon/webapp source. No agent saw another's answer.

**"Used by" is TRANSITIVE.** A message that is an arm of a container one of the
four embeds counts as used by that file. Direct-reference-only would report
"nothing" for 140 of 142, because only `SessionCommand` and `FailureKind` are
named outright by a canonical file.

**"Raised by" is a different question and is reported separately for commands.**
Which surface's affordance *sends* a command is not the same as which file
*imports* it. Every command below is imported only by `frame.proto`; the
surface that raises it is often one of the four.

**One correction applied throughout.** Three agents wrote `SessionSnapshot.death`.
That message does not exist. The real field is `frontend.v1.SessionView.death`
at `footer.proto:422`, and it is normalized to that everywhere here.

---

## Summary — what reaches a canonical file, and what does not

Everything that reaches one of the four does so through exactly **three doors**:

| door | messages | surfaces reached |
|---|---|---|
| `FailureCardView.kind` | the 71 failure messages | `feed`, `footer` |
| `WorkspaceState.merge_status` | `MergeStatus` + 8 phase arms | `footer` |
| `WorkspaceState.merge_dequeue_offer` | `MergeDequeueOffer`/`Waiting`/`Running` | `footer` |
| `SessionView.hibernation` | `HibernationDetail` + 3 cause arms | `footer` |
| direct | `SessionCommand` | `feed`, `footer` |

**Nothing reaches `topbar.proto`. Nothing reaches `sidebar.proto`.** The sidebar
imports no file at all and declares its own empty status markers rather than
embedding any shared vocabulary.

### Implied destination map

| destination | count | why |
|---|---|---|
| → `feed.proto` | 73 | `FailureKind` + 71 arms + `SessionCommand`; `FailureCardView` already lives there, and `footer`/`frame` already import `feed` |
| → `footer.proto` | 16 | 12 merge + 4 hibernation; `WorkspaceState` and `SessionView` both live there now |
| stays in `shared.proto` | 53 | reachable only via `frame.proto` — the daemon-control surface |

`SessionCommandSpec` is counted with `feed` but is a special case: it is an
`extend google.protobuf.EnumValueOptions` extension read only by reflection, and
travels on no frame at all.

### What stays, and why it is coherent

The 53 that remain are the daemon-control surface: shutdown scheduling, session
lifecycle, workspace provisioning, queue administration, host actions, and the
revival commands. None is any component's props. This is the residue the
figma→idl rule explicitly allows — transport, host-driver, and control verbs.

---

## Findings worth acting on, independent of file placement

1. **The failure vocabulary has no per-arm variation whatsoever.** All 71 arms
   reach exactly `feed` and `footer`, by the same chain. There is no arm that
   serves one surface and not the other, so it cannot be split by consumer.

2. **`FooterFailureRow` deliberately drops the kind**, carrying a pre-resolved
   `tone` string plus a `FailureCardRef`. So the footer's *typed* dependency on
   `FailureKind` runs solely through `SessionView.death` — that one field is the
   entire reason the footer needs the vocabulary.

3. **The sidebar re-spells the merge phases.** `RosterRowStatusMerging`,
   `MergeConflict`, `MergeFailed`, `Merged`, `MergeQueued`, `MergeEnqueuing` are
   empty glyph markers duplicating `MergeStatus`'s arms. The sidebar's conflict
   arm cannot say *which* commit conflicted; only `MergeStatusConflict` holds
   that.

4. **`HostSetSidebarView` and `HostSetRepositoryFold` do not reach the sidebar**,
   despite their names. The sidebar carries its own fold state inline
   (`RosterRepositorySection.folded`) and its own view state as the typed
   `WorkspaceRoster.view` oneof. The host actions are untyped `string` requests
   to Emacs for the same facts — a second, weaker spelling.

5. **`HostLegacyCommand` is the one hole in the closed vocabulary.** Its `type`
   is constrained to eight verbs by comment and daemon check, but its body is a
   `google.protobuf.Struct` — no generated type, no required-field check, no wire
   compatibility guarantee. The guarantee is prose plus a runtime check, not
   structure.

6. **`ShutdownHoldTasks` carries a COUNT, not identities.** The drain panel can
   say "3 tasks still running" but cannot name, link, or offer to cancel any of
   them. This is the same count-instead-of-identity defect already tracked
   elsewhere as `DrainHold.LiveTasks`.

7. **`RENDER_STATE_HIBERNATED` with no `hibernation` detail is representable.**
   Nothing in the schema forbids publishing a workspace stood down by a backend
   bounce as hibernated with no `HibernationDetail` set — so the teal
   "nothing is wrong" reading is available for a workspace that never slept.
   This is the schema making the known bounce-vs-hibernation confusion possible.

8. **Every workspace-lifecycle command is Emacs-only.** All six were verified as
   never constructed anywhere in `webapp/src/`. `ClientLogCmd` is the inverse:
   raised only by webapp instrumentation, with no user affordance at all.

9. **The two enums both survive the no-state-enums rule, for the same reason.**
   `ResumeMode` is a caller-supplied *intent*, and `CompactionScope` is a
   user-chosen *scope* — neither is a phase, status, or condition of anything.
   The states they border on are already oneofs (`HibernationDetail.cause`,
   `ReviveSessionCmd.mode`).

10. **Respelling verdicts are overwhelmingly `NO`.** Of 142, the partial
    re-spellings are: `FailureShimRejected`, `FailureShimAckTimeout`,
    `FailureShimStoreWriteRejected`, `FailureShimHandshakeIncomplete`,
    `FailureShimUnhealthy`, `FailureConversationUnresumable`,
    `SessionResumeFailure`, `SessionResumeFailureIdentityMismatch`,
    `FailureCompactionColdRead`, `FailureQueueEntryUnwired`,
    `FailureQueueEntryUninterruptibleTurn`, `FailureCommandUnsent`,
    `FailureCommandRejectionUnclassified`, `MergeQueueEntry`,
    `AnswerMergeDequeueCmd`, `ShutdownHold`, `HostBootSweepSessionUnwired`,
    `HostWorkspaceCreateFailed` — 18 in total, and nearly all of them are the
    *same* defect: a bare `string *_id` where a typed identity belongs, or a
    verbatim prose field paralleling `conversation.v1.FailureRaised.detail`.

    The one substantive near-miss is
    `FailureQueueEntryUninterruptibleTurn.command`, which narrows a 30-value
    `SessionCommand` to exactly the clear/compact pair that
    `conversation.v1.ContextCut` already models as two typed arms.

---

## The failure vocabulary — 71 messages

**Shared reachability, stated once.** Every message in this section reaches
exactly two canonical files, by the same chain:

- `frontend.v1.FailureCardView` — in `feed.proto`, as
  `Message.payload.failure_card` → `FailureCardView.kind` → `FailureKind` → the arm
- `frontend.v1.SessionView` — in `footer.proto`, as `SessionView.death`
  (itself a `FailureCardView`) → `FailureKind` → the arm

Per-message entries below give the *consequence* rather than restating that
chain 71 times.

### `frontend.v1.VendorFailureContext`
- **Purpose / UX:** The vendor-side coordinates (Claude session id, API request id, API message id) attached to any vendor failure, so a card can show the exact identifiers to quote in a support report.
- **Respelling:** `NO` — no `conversation.v1` message carries vendor request/message correlation ids; `FailureRaised` holds only prose.
- **Used by:** `feed` (the "raw account" region of the card), `footer` (a session that died on billing or auth needs these ids to be reportable rather than merely red).

### `frontend.v1.FailureKind`
- **Purpose / UX:** The single closed ~62-arm oneof naming every way work can fail, split by field number into daemon-minted (1–54) and frontend-minted (55–62), and by side into machinery (BLUE) versus vendor (PURPLE). It decides the workspace's failure color and which typed evidence renders.
- **Respelling:** `NO` — `conversation.v1.FailureRaised` is explicitly the *other* thing (the vendor's own durable transcript error), and its comment states that classification is deliberately absent and belongs to `FailureCardView`.
- **Used by:**
  - `frontend.v1.FailureCardView` — in `feed.proto`
    - embedded directly as `FailureKind kind = 1`; one of only two `shared.proto` symbols a canonical file names outright
    - the card's color comes from which side of the vocabulary the arm sits on
    - the arm selects which typed evidence block renders, so an unmatched arm fails loudly instead of degrading to generic text
  - `frontend.v1.SessionView` — in `footer.proto`
    - via `death`, the sole reader-facing account of why a session is terminal
    - note `FooterFailureRow` does *not* carry the kind, so this is the footer's only typed dependency on it

### Shim connectivity — 7

- **`FailureShimNotConnected`** — no live connection, so a command had nowhere to go. `NO` respelling (transport state; `conversation.v1` models none). Feed: distinguishes "prompt vanished" from a generic error. Footer: the typed account when a session is terminal purely because nothing connected.
- **`FailureShimRejected`** — the shim received a request and actively refused it, carrying `request_id` and verbatim `reason`. `PARTIAL of conversation.v1.FailureRaised` — the identifier+reason pairing overlaps, but `request_id` is a bare string for an untyped request identity. Feed: tells the user the agent *chose* to refuse, changing the remedy from retry to inspect. Footer: names the refused request.
- **`FailureShimAckTimeout`** — a request went unacknowledged, carrying `request_id` and `waited_ms`. `PARTIAL of conversation.v1.FailureRaised` — `retry_in_ms` and `waited_ms` are both millisecond waits, but one is vendor advice and the other an elapsed measurement. Feed: `waited_ms` is what separates a stall from an abandoned request; the card is deliberately ambiguous about whether the work ran, so the user does not blindly resend.
- **`FailureShimVersionMismatch`** — carries `shim_version` and `daemon_version`. `NO` (no versioning concept in `conversation.v1`). Feed: both strings must survive or the user cannot tell which binary is stale. Footer: a workspace permanently unusable from skew shows that as its typed death.
- **`FailureShimSeqRegression`** — the event stream went backwards, carrying the arriving `seq` and `last_seen_seq`. `NO` (`conversation.v1` carries no sequence fields; ordering there is `MessageParent` lineage). Feed: the one arm that tells the reader to distrust the transcript in front of them, so it must appear inline at the point of corruption.
- **`FailureShimDegraded`** — window-shaped silence from the shim, naming the `component`. `NO`. Feed: exercises `FailureCardView.lifecycle`'s re-send-under-one-uuid reconciliation, so alarm and all-clear collapse into a single row. Footer: a degradation that never closed is what is reported at teardown.
- **`FailureShimStoreWriteRejected`** — persistence failed, carrying `component`, verbatim `reason`, `dropped_count`. `PARTIAL of conversation.v1.FailureRaised`. Feed: the card sits in the very feed whose durability is in question, so it is the only place the user learns surrounding rows are missing; severity is read off the count.

### Shim lifecycle and health — 5

- **`FailureQueryTermination`** — the SDK query driving the session ended unexpectedly. `NO`. Wraps `QueryTerminationFailure`. Feed/footer: explains a session that stopped responding while still appearing alive.
- **`FailureShimNotSpawned`** — empty; no agent process was ever started. `NO`. Feed: distinguishes "never spawned" from "spawned and broken" without parsing prose.
- **`FailureShimHandshakeIncomplete`** — connected but never finished wiring, with `request_id` and verbatim `cause`. `PARTIAL of conversation.v1.FailureRaised`. Feed: supplies the `request_id` a user quotes for a half-wired workspace.
- **`FailureShimUnhealthy`** — the shim's own self-diagnosis: request, `component`, verbatim `reason`. `PARTIAL of conversation.v1.FailureRaised`. Feed: distinguishes "the process says it is broken" from "we inferred it from silence" (`FailureShimDegraded`); the named component is what makes it actionable.
- **`QueryTerminationFailure`** — the machine-readable evidence record: query invocation id, vendor conversation identity (or an explicit statement it was never discovered), observation time, typed reason oneof. `NO`. **It is not owned by `FailureQueryTermination`** — it is referenced from two places, as `FailureQueryTermination.detail` *and* as the `query_termination` cause arm of `SessionResumeFailure`, so the same card can explain a failed resume with exact termination evidence.

### Session existence and supersession — 6

- **`FailureSessionNotEstablished`** — bring-up never finished inside its window, with verbatim `cause`. `NO` — `FailureRaised` is a vendor-reported *turn* failure; this is a daemon-observed pre-conversation timeout with no vendor and no retry hint.
- **`FailureWorkspaceNotLive`** — empty; a command addressed a session this workspace no longer runs. `NO`. Feed: a refused command still leaves a visible account instead of vanishing.
- **`FailureSessionDeleted`** — the session was deliberately deleted; an account, not a fault. `NO` — `conversation.v1`'s `StopReason` arms describe why a *turn* ended, never why a session ceased to exist. Footer: the UI offers no retry affordance.
- **`FailureSessionSuperseded`** — a NEW session took over the workspace, enforcing one-live-session-per-workspace. `NO`.
- **`FailureReconnectSuperseded`** — the *viewer* is behind: the connection changed generation, so the requested replay would come from a generation this client never saw. Carries a daemon-composed `remedy` rendered verbatim. `NO`.
  - **The distinction from `FailureSessionSuperseded`:** that one is about the SESSION — a successor replaced it and it is genuinely dead, no fields, no remedy. This one is about the VIEWER — the session may be perfectly alive, and it is the only one of the pair carrying a `remedy`, because the fix is to reload rather than to accept a death.
- **`FailureSessionShimDied`** — the agent process exited; the bluntest machinery death. `NO` — a dead process has no notion of the retry hint `FailureRaised` carries.

### Session start and resume — 11

- **`FailureSessionStartFailed`** — the session could never be brought up, with verbatim bring-up `cause`. `NO`.
- **`FailureSessionResumeFailed`** — an existing vendor conversation could not be resumed without breaking continuity; wraps `SessionResumeFailure`. `NO`.
- **`FailureConversationUnresumable`** — this workspace owns a vendor conversation that could not be reached, and no blank one will be started in its place. Carries `claude_session_id`, `cwd`, `config_dir`. `PARTIAL of conversation.v1.MessageEntry` — the session id is a bare string standing in for a vendor-conversation identity. Feed: without the id and cwd the user cannot locate the conversation the refusal is protecting.
- **`FailureResumeModeRetired`** — empty; the client asked for a resume mode the daemon no longer supports. `NO`. Separates "you must update" from "something broke", which the footer's color class depends on.
- **`FailureSessionEndedUnclassified`** — carries `raw_reason` when the daemon could not classify why a session ended. `NO`. It guarantees `SessionView.death` is always settable, so the footer never invents prose.
- **`SessionResumeFailure`** — the continuity evidence: authoritative `claude_session_id`, `cwd` and config roots searched, an `attempt` oneof, a `cause` oneof. `PARTIAL of conversation.v1.MessageEntry` (bare id string). The `cause` arm is what distinguishes "no transcript" from "wrong identity".
- **`SessionResumeFailureCreate`** — empty `attempt` arm: the failure happened while a frontend was creating a session that continues a durable conversation. `NO`. Makes the card's remedy addressable to an action the user just took.
- **`SessionResumeFailureAutomaticRestore`** — empty `attempt` arm: the daemon was reconstructing a shim for an already-allocated session. `NO`. Lets the feed say the failure was unprompted.
- **`SessionResumeFailureTranscriptUnavailable`** — `cause` arm listing every absolute `searched_paths` location examined. `NO`. The path list is the only thing that makes a "transcript missing" card verifiable, and it marks a recoverable-by-hand death.
- **`SessionResumeFailureIdentityMismatch`** — `cause` arm recording that recovery proposed a different vendor UUID, or an empty `replacement_claude_session_id` meaning it would have started fresh. `PARTIAL of conversation.v1.MessageEntry`. Names the wrong conversation that was almost resumed — the concrete thing the refusal protected against.
- **`SessionResumeFailureBringUpFailure`** — `cause` arm retaining the verbatim driveability-gate string when exact resume failed with no typed query-lifecycle record. `NO`. The fallback that keeps the `cause` oneof always settable.

### History and replay — 4

These bear directly on the pagination surface; each is what a client sees
*instead of* a page.

- **`FailureHistoryRepullInFlight`** — empty; a second re-pull was refused because one is running. `NO`. Shown in place of the `ConversationHistoryPage` a `FirstPageCmd`/`NextPageCmd` asked for.
- **`FailureHistoryReplayTruncated`** — a re-pull stopped before reaching the live window: `from_seq`, `stop_at_seq`, `delivered`, `reason`. `NO` (seq extents are not a `conversation.v1` concept). Its `stop_at_seq` against `ConversationHistoryPage.live_join_seq` is how a client knows the gap-free splice did not happen.
- **`FailureReplayMarkRetired`** — refuses a replay from a mark in a retired seq space, carrying `from_seq` and `live_last_seq`. `NO`. It is the only signal distinguishing "REPLACE the feed from a fresh tail page" from "append a page" — which the `PageScope` echo and `live_join_seq` alone cannot express.
- **`FailureCompactionColdRead`** — a compaction re-read the whole conversation at the uncached rate, carrying `uncached_input_tokens`. `PARTIAL of conversation.v1.TokenUsage` — one projected counter corresponding to the `TokenCacheMisses` pair (written + unwritten), collapsed to the single number the card states. Does *not* substitute for a page; it reports cost waste next to the `ContextCut` it belongs to.

### Prompt queue, keep-alive, hibernation — 7

- **`FailureInterruptUndelivered`** — empty; the stop the user pressed never reached the agent, so the turn is still running. `NO` — `conversation.v1.StopInterrupted` records an interrupt that *succeeded* durably. Backs an honest negative acknowledgement instead of a stop button that silently did nothing.
- **`FailureQueueEntryUnwired`** — a held prompt has no agent process attached, naming `entry_id` and `reason`. `PARTIAL of conversation.v1.FailureRaised`.
- **`FailureQueueEntryKeepAliveHeld`** — a queued prompt is blocked behind an in-flight cache keep-alive turn. `NO`. It is the failure twin of `QueueEntry.hold.keep_alive` (`QueueEntryKeepAliveHold`), and its `keep_alive_turn_id` matches that arm's `turn_id`, so card and queue row name the same releasing turn. It is the only surface stating why `QueueForceCmd` was refused.
- **`FailureSessionHibernated`** — the workspace is asleep and needs an explicit revival decision, with `since_ms`. `NO`. The typed sibling of `SessionView.hibernated`/`hibernation`: the bool projects the state, this classifies the refusal it caused.
- **`FailureKeepAliveWindowUnclosed`** — a keep-alive window could not be closed, so new conversation is withheld. `NO`. Withheld conversation is invisible in the feed unless this card states the withholding.
- **`FailureKeepAliveWindowInverted`** — a window ended before it began, so the daemon's own keep-alive turn may be visible in the user's conversation. `NO`. Explains a stray turn the user did not send; the card must sit next to the leaked turn.
- **`FailureQueueEntryUninterruptibleTurn`** — a queued prompt is stuck behind a context cut, naming the running `SessionCommand`. `PARTIAL of conversation.v1.ContextCut` — it narrows to exactly the clear/compact pair that `ContextCut.cleared`/`.compacted` spell as typed arms, but carries a 30-value command enum and names a cut *in flight* rather than a durable record. Failure counterpart of `QueueClassificationUninterruptibleTurn`, carrying the same field.

### API authentication, billing, ceilings — 7

- **`FailureApiAuthenticationFailed`** — credentials rejected; PURPLE, remedy is re-authenticate, not retry. `NO`.
- **`FailureApiBillingError`** — a billing problem, not a fault; remedy is a human fixing the account. `NO`. `attempts` says the daemon did not silently burn retries against an unpayable account.
- **`FailureApiRateLimit`** — the account is rate limited. `NO`. Pairs with the footer's `RateLimitWindow` cells (`ProgressView.rate_limited`, `rate_limited_weekly`); `http_status` and `attempts` distinguish an exhausted retry budget from a single 429.
- **`FailureApiOAuthOrgNotAllowed`** — the org behind the credential is not permitted; remedy is an administrator. `NO`. A distinct arm is what stops it rendering as a generic auth failure with the wrong remedy.
- **`FailureApiMaxOutputTokens`** — the response hit the output ceiling. `PARTIAL of conversation.v1.StopMaxTokens` — same event, but `StopMaxTokens` is the empty durable stop reason on `AgentSaid.stop_reason` while this adds `VendorFailureContext` with bare id strings. Pairs with `FailureCardTerminal` so the card does not invite waiting on an answer that will never continue.
- **`FailureApiMaxTurns`** — a user-chosen turn ceiling was reached. `NO`. Its own arm is what stops a deliberate limit being colored as a breakage.
- **`FailureApiMaxBudget`** — the turn hit its configured budget. `NO`. The terminal counterpart to `ContextCostAlert`, which only warns about an expensive turn that still completed.

### API transport and server faults — 7

- **`FailureApiInvalidRequest`** — the vendor rejected the request as malformed. `NO`.
- **`FailureApiServerError`** — a 5xx; the default arm for any 5xx-family status, so most vendor outages surface here. `NO`. `attempts` lets the card say the SDK's retries were already spent.
- **`FailureApiOverloaded`** — vendor over capacity. `NO`. Window-shaped, so the same card is re-sent under one `Message.uuid` with a `FailureCardResolved` lifecycle.
- **`FailureApiModelNotFound`** — the requested model does not exist; the only API arm carrying a `model` string, so the card can name it beside the topbar's model selection. `NO` — `AgentSaid.model` names the model of a *successful* response and shares no other field.
- **`FailureApiNetworkDown`** — the request never reached the vendor. `NO`. Classed PURPLE rather than BLUE because none of agent-repl's machinery failed; carries only `VendorFailureContext`, so the card renders the story from the arm identity alone.
- **`FailureApiRequestFailed`** — the HTTP-level catch-all: a real error *response* whose status matched no known status, was not 5xx-family, and was not a network-down link. `NO`. Its `http_status` and `attempts` are the only evidence the reader gets.
- **`FailureApiUnknown`** — the SDK-level catch-all: selected when the SDK's own classification is the literal member "unknown", i.e. **the vendor itself declined to say what went wrong**. `NO`.
  - **The distinction:** `FailureApiRequestFailed` is selected only from a real error response the daemon could not place; `FailureApiUnknown` is selected when there is no vendor account at all. It is also what guarantees an unset `FailureKind` is never needed, so the feed can reject an empty kind as malformed.

### API execution and client faults — 7

- **`FailureApiExecutionError`** — a turn aborted mid-execution. `NO` — `FailureRaised` is the *producer-observed* transcript error; this is daemon-synthesized classification whose only field is correlation ids.
- **`FailureApiRefusal`** — the model refused the request. `NO`. Terminal, so the card renders settled rather than as an open alarm inviting waiting.
- **`FailureApiTurnFailed`** — the residual vendor arm, preserving the raw `stop_reason` verbatim. `NO` — it overlaps `StopReason`/`StopUnsupported` only thematically. It keeps an unrecognized vendor stop reason renderable instead of dropped.
- **`FailurePromptRefusedByMergeState`** — the daemon refused a prompt because merge machinery holds the session, carrying the composite render-state name. `NO` — `conversation.v1.DetachedMerge` describes merge as *work the agent did*, not as a refusal.
  - **The merge↔prompt join, concretely:** `SubmitPromptCmd` in `footer.proto` is the send site, `FooterMergeChip` is what the user sees holding the session ("merge queued 2/3"), and this arm's `state` field carries the same `RENDER_STATE_MERGE_*` name the chip was resolved from — so the refusal explains itself in the chip's own vocabulary.
- **`FailureTurnUndriven`** — a turn stood bound with nothing driving it and the daemon closed it. `NO` — exactly the class `FailureRaised`'s comment excludes, since nothing observed it and no transcript holds it. Stops the workspace showing "thinking" forever.
- **`FailureClientLogIdentityStale`** — a browser log record arrived against a superseded workspace state and was not recorded. `NO`. BLUE, so dropped telemetry reads as machinery rather than a vendor problem.
- **`FailureInternalUnclassified`** — the honest residual for agent-repl's own machinery, carrying `cause` verbatim. `NO`. Without it the oneof would have unrepresentable cases, and an unset `FailureKind` is defined as a malformed frame.

### Daemon reachability and frontend transport — 8

These are the failures where the transport itself broke, which raises the
question of how the card reaches the client at all. **All eight are
client-minted** — arms 55–62, above the daemon's 1–54 band — built by the
frontend into its own feed model rather than delivered over the wire.

- **`FailureDaemonUnreachable`** — the socket dropped and the frontend is reconnecting. `NO`. Window-shaped and *retracted outright* on reconnect rather than settling into a resolved notice.
- **`FailureWorkspaceGone`** — the addressed workspace no longer exists on the daemon. `NO`. The only kind that tells the feed to render terminal rather than open, so the user stops waiting for a reconnect with nothing to come back to.
- **`FailureBootFailed`** — the frontend could not start at all, carrying the caught `cause`. `NO`. **Uniquely, no delivery path exists** — the machinery that would carry it is the machinery that failed to build, so a frontend renders it from whatever it has before any state exists.
- **`FailureControlPlaneFailed`** — a frontend request outside the command stream (a login, an account switch) failed, with a `what` discriminator precisely so repeats of *different* control-plane requests do not reconcile onto one card. `NO`.
- **`FailureFrameUndecodable`** — a frame could not be read and was skipped, carrying `frame_head`. `NO` — `conversation.v1.UnsupportedBlock` is the nearest shape but preserves an unrecognized *content block* inside a decoded message. A card rather than a log line, because conversation may be missing as a result.
- **`FailureStaleBundle`** — this page cannot read the daemon's state and reloading did not fix it. `NO`. Deliberately unresolvable, so the page cannot quietly go on being wrong.
- **`FailureCommandUnsent`** — a command never reached the wire because the connection was down, as distinct from being refused. `PARTIAL of conversation.v1.MessageEntry` — `string command` is untyped prose where the `SessionCommand` enum would belong. No `CommandAck` can exist for a command never sent, which is exactly why it is client-minted. Tells the user retrying is meaningful because nothing was decided.
- **`FailureCommandRejectionUnclassified`** — the daemon refused a command with no classified kind on its `CommandAck`. `PARTIAL of conversation.v1.MessageEntry`. Here the transport *worked*: an ack with `ok=false` and `failure` unset did arrive, and the frontend converts it locally rather than picking a kind on the daemon's behalf.

---

## Merge — 24 messages

### Merge run status and phases — 9 → `footer.proto`

Reached as `WorkspaceState.merge_status`, and a second way as
`WorkspaceState.merge_dequeue_offer` → `MergeDequeueRunning.status`, so the
dequeue card names the phase from the vocabulary it is already rendering.

- **`MergeStatus`** — the live whole state of a workspace's merge run, keyed by daemon-minted `run_id`, phase carried as which arm is set. `NO` — `conversation.v1.DetachedMerge` is an empty marker arm of `DetachedWorkKind` tagging a feed row's provenance, with no run id, phase, or progress. **The sidebar's status arms and `FooterMergeChip` are independent lossy re-spellings** (empty glyph markers and pre-resolved display strings), not embeddings.
- **`MergeStatusEnqueued`** — 1-based `position` and queue `depth`. `NO`. What makes the chip read "merge queued 2/3".
- **`MergeStatusBeforeAction`** — the pre-merge action is running, with its `prompt`. `NO` — `UserSaid` content is typed `UserContent` blocks, not a bare display string.
- **`MergeStatusCherryPicking`** — commits replaying onto the target: landed/total counts, current sha and subject. `NO` (`conversation.v1` has no git vocabulary). Supplies the within-phase ticks that prove the run is alive.
- **`MergeStatusTesting`** — the landed-but-ungated commit is under test, same counts plus sha and subject. `NO`. The only way to distinguish testing from cherry-picking at identical counts.
- **`MergeStatusConflict`** — stopped on a conflicted commit, naming that sha and subject. `NO`. **The sole carrier of WHICH commit conflicted** — the sidebar's `RosterRowStatusMergeConflict` is empty.
- **`MergeStatusAfterAction`** — the post-merge action is running, with its `prompt`. `NO`. Keeps the chip live during work after the commits landed.
- **`MergeStatusMerged`** — terminal success: `commits_total` and any `after_action_error`. `NO` — `DetachedSucceeded` carries only free-text `summary` and no commit count. **The only place a merge that succeeded but whose after-action failed is representable**, which no glyph arm can express.
- **`MergeStatusFailed`** — terminal failure: cause, counts, failing sha and subject, plus `failed_json` — this message protojson-serialized without that field. `NO`. `failed_json` is the only field letting the footer surface the complete record without a hand-written formatter that would drift from the schema.

### Merge queue roster — 6 → stays in `shared.proto`

Reachable only via `Frame.merge_queue_roster` and
`StateSnapshot.merge_queue_roster`. **The footer's chip is drawn from
`WorkspaceState.merge_status`, a per-run projection — not from the roster.**

- **`MergeQueueRoster`** — the whole queue as the daemon will drain it: global pause bit, change timestamp, every repo's entries. `NO`.
- **`MergeRepoQueue`** — one repository's slice, its opaque repo key plus entries in delivery order. `NO`.
- **`MergeQueueEntry`** — run id, workspace key and display name, source branch, plus the head-state oneof set only on `entries[0]`. `PARTIAL of conversation.v1.MessageEntry`, weakly — bare `string` identifiers where typed identities would stand, with no other overlap.
- **`MergeQueueHeadRunning`** — empty; the drain goroutine holds this entry. `NO`. Why the UI refuses an evict on it.
- **`MergeQueueHeadPausedWaiting`** — empty; delivered but parked at the pause gate with nothing started. `NO`. Distinguishes "paused, nothing running" from "running".
- **`MergeQueueHeadTerminalOwed`** — empty; a durably marked terminal word awaits re-publication and will be answered rather than re-run. `NO`. Tells a user their merge is already decided and will not execute again after a bounce.

### Merge commands and the dequeue offer — 9

- **`PauseMergeQueueCmd`** — empty, daemon-global, durable, idempotent; stops new merges dequeuing while letting the in-flight run finish. `NO`. **Raised by `sidebar`** (the roster is the only surface rendering the merge pipeline). **Used by:** nothing — `SessionCommand.pause_merge_queue = 29` in `frame.proto` only.
- **`ResumeMergeQueueCmd`** — the idempotent inverse. `NO`. **Raised by `sidebar`.** **Used by:** nothing.
- **`EvictMergeCmd`** — removes ONE waiting entry by `run_id`, refused against the running head, giving that run a terminal failed status so its workspace's merge axis resolves. `NO`. **Raised by `sidebar`** (the row whose status is `RosterRowStatusMergeQueued`). **Used by:** nothing.
- **`MergeDequeueOffer`** — the at-most-one-per-workspace QUESTION about taking an interrupted workspace's merge off the queue. `NO`. **Used by `WorkspaceState.merge_dequeue_offer` in `footer.proto`** — the only place the question is published, so without it an interrupt would silently destroy a queued merge; its presence is the footer's entire signal to draw the card.
- **`MergeDequeueWaiting`** — the merge is behind others and nothing has run: `ahead`, `position`, `depth`. `NO`. Distinguishes the cheap "drop the entry" confirmation from an abort.
- **`MergeDequeueRunning`** — the merge IS the head and in flight, carrying the whole `MergeStatus`. `NO`. Supports the harsher "this will ABORT the run" wording and lets the footer reuse the exact `MergeStatus` it already renders.
- **`AnswerMergeDequeueCmd`** — the answer to a specific offer: `offer_id` plus a mandatory confirm/decline arm. `PARTIAL of conversation.v1.MessageEntry` (bare id string). **Raised by `footer`.** **Used by:** nothing.
- **`MergeDequeueConfirm`** — empty; evict while waiting, abort while running. `NO`. **Raised by `footer`.**
- **`MergeDequeueDecline`** — empty; clears the offer and lets the merge proceed, so declining is a real answer rather than an unanswered card. `NO`. **Raised by `footer`.**

---

## Session lifecycle — 7 → stays in `shared.proto`

- **`ResumeMode`** *(enum)* — the caller's *intent* about which vendor conversation a new session lands on. `NO`.
  - **Enum legitimacy:** a genuine non-state closed set. It is a caller-supplied intent verb on an inbound command, never an observed condition any surface renders; the states it borders on are already oneofs (`HibernationDetail.cause`, `FailureConversationUnresumable`, `FailureSessionResumeFailed`). `FailureResumeModeRetired` is evidence *for* this reading — it exists so a stale client naming a retired *intent* is refused, which is the failure shape of a vocabulary, not of a state.
- **`CreateSessionCmd`** — starts a session under a given cwd, account (`config_dir`), permission posture, model, and resume intent. `NO`. **Raised by `sidebar`** (opening a workspace row). *`topbar.proto` mentions it only in a comment contrasting `SetModelCmd` — not a reference.*
- **`DeleteSessionCmd`** — tears down one named session; the deliberate teardown that later surfaces as `FailureSessionDeleted`. `NO`. **Raised by `sidebar`.**
- **`RestartSessionCmd`** — empty; hard-restarts the shim while leaving the session record untouched, so the transcript is preserved. `NO`. **Raised by `footer`** — where a restart is observable, via the queue hold that parks prompts "because the session's shim is being restarted onto the current build".
- **`HibernateWorkspaceCmd`** — empty; immediately hibernates a workspace to stop paying keep-alive cost. `NO`. **Raised by `sidebar`**; its effect surfaces as `HibernationForced`.
- **`ClientLogLevel`** *(enum)* — info/warn/error for a `ClientLogCmd`. `NO`. No user affordance.
- **`ClientLogCmd`** — mirrors a frontend diagnostic (level, message, schemaless `Struct` context) into the daemon's on-disk log. `NO` — evidence about the *client*, never the conversation; the daemon writes it and never acts on it. **Raised by no user affordance at all** — webapp instrumentation only (`clientlog-throttle.ts`, `connect-resync.ts`). It exists because the webapp runs inside an Emacs xwidget whose JS console nobody can see and nothing persists.

---

## Shutdown scheduling — 10 → stays in `shared.proto`

All reachable only via `frame.proto`.

- **`ShutdownCmd`** — graceful shutdown now, with `stop_shims` deciding whether live shims are SIGTERMed. `NO`. Issued by Emacs and `deploy-all.sh` when a rebuilt shim bundle must not survive the bounce.
- **`ShutdownScheduleView`** — the daemon-global drain-lease broadcast, idle or draining, pushed on change and carried in `StateSnapshot` so a client connecting mid-bounce sees the lease without waiting for an edge. `NO`.
- **`ShutdownScheduleIdle`** — empty; no shutdown scheduled. `NO`. Makes lease clearing representable, distinguishing "cancelled/completed" from "no information yet".
- **`ShutdownScheduleDraining`** — `schedule_id`, `scheduled_at_ms`, free-text `cause`, `stop_shims`, non-empty holds. `NO`. The whole source for the drain panel.
- **`ShutdownHold`** — one workspace still blocking quiescence plus the reasons, keyed by absolute CWD with a host-surface `session_id`. `PARTIAL of conversation.v1.MessageEntry` (bare session id).
  - **`footer.proto`'s `QueueEntryShutdownHold` is a DISTINCT fact, not a re-spelling.** It carries only `schedule_id` to say which schedule is holding a queue entry; `ShutdownHold` says which workspace is holding the schedule. They point in opposite directions and share no field.
- **`ShutdownHoldTurn`** — a turn is in flight, named by `turn_id`. `NO`. Points the user at the exact turn to wait on or interrupt.
- **`ShutdownHoldTasks`** — **carries only an `int32 count`, not task identities.** `NO`. The drain panel can say "3 tasks still running" but cannot link, name, or offer to cancel any; a user wanting that must go to the task catalog, which remains the authority.
- **`ScheduleShutdownCmd`** — schedules a shutdown behind a drain, taking the lease immediately and blocking new turns. `NO`. Backs "bounce when everyone is done".
- **`CancelScheduledShutdownCmd`** — cancels by `schedule_id`, releasing the lease, nacking loudly on a stale id. `NO`.
- **`RestartPendingView`** — the one-shot edge announcing a deliberate shutdown: cause, clamped `expected_outage_seconds`, `stop_shims`, `announced_at_ms`. `NO`. **What stops a deliberate deploy bounce from painting the webapp's severed banner and Emacs's degraded-link segment.** Deliberately excluded from snapshot state — it is an edge, never state.

---

## Workspace lifecycle — 6 → stays in `shared.proto`

**Every one was verified as never constructed in `webapp/src/`.** These are
purely Emacs-raised; the webapp only observes their effects.

- **`CreateWorkspaceCmd`** — the sole creation ingress: name resolution, worktree, session, initial prompt (`git_root`, `base_commit`, `fork_from`, `permission_mode`, `allow_ungated`). `NO`. Backs `SPC TAB n/N` and skill-produced JSON dispatches.
- **`WorkspaceAvailable`** — the daemon's retained account of a workspace whose worktree and shim are healthy and ready for host materialization (`worktree_path`, `branch`, `git_root`, `source_workspace`, `config_dir`). `NO`. Host-only and stripped from every GUI client; the webapp decodes it only because it shares the frame decoder.
- **`WorkspaceMaterializedCmd`** — Emacs's durable acknowledgement that it built the perspective, which **releases the initial prompt the daemon held until a visible workspace existed**. `NO`. Only Emacs can honestly send it, since only Emacs creates the perspective it attests to.
- **`OpenWorkspaceCmd`** — reattaches or starts a session under editor-owned run preferences, deliberately carrying no session identity. `NO`.
- **`CloseWorkspaceCmd`** — empty; tears a workspace down without merging. Its entire meaning lives in the `FrontendCommand.workspace` envelope key. `NO`.
- **`MergeWorkspaceCmd`** — a geometry-free request to merge back into the source recorded at creation, backing `SPC TAB M` and the resolve-and-continue handoff. `NO` — `DetachedMerge` names a merge only as a *kind of detached work already running* and cannot express `conflict_resolved_continue`.

---

## Host actions — 10 → stays in `shared.proto`

- **`HostAction`** — the retained-until-completed envelope handing one daemon-sourced, Emacs-only UI action to the host with its `action_id` ack key, so an inbox request survives reconnects instead of being dropped. `NO`.
- **`HostBootSweepSessionUnwired`** — the boot sweep left a surviving session unwired without tearing it down, with one display-ready `reason` rendered verbatim. `PARTIAL of conversation.v1.FailureRaised`.
- **`HostWorkspaceCreateFailed`** — a durably-failed creation job, keyed by `job_id`, with `requested_name`. `PARTIAL of conversation.v1.FailureRaised`.
- **`HostSwitchWorkspace`** — asks Emacs to make a project `dir` current. `NO`.
- **`HostSetRepositoryFold`** — asks the host to collapse/expand a sidebar repo section by `repo_key`. `NO`. **Despite the name, `sidebar.proto` does NOT reach it** — the sidebar carries fold state inline as `RosterRepositorySection.folded`, keyed by the same `repo_key`, so the rendered fold arrives in `WorkspaceRoster`.
- **`HostSetSidebarView`** — asks the host to switch the sidebar's grouping to a named `view`. `NO`. **`sidebar.proto` does NOT reach it either** — the sidebar's view state is the typed `WorkspaceRoster.view` oneof (`RosterRepositoryView`/`RosterTaskView`), whereas this is an untyped `string view` request.
- **`HostTaskCreate`** — empty; open the host's new-task flow. `NO`.
- **`HostTaskById`** — names one task by `id`, serving three arms (toggle-done, open, add-workspace) behind one shape. `NO`. `sidebar.proto` independently carries `RosterTaskSection.task_id` as the same identity but never embeds this message.
- **`HostLegacyCommand`** — the established non-create verbs whose bodies are not yet stable enough to promise a schema. `NO` — a schema-deferral envelope, with a `google.protobuf.Struct` payload nothing in `conversation.v1` uses.
  - "Legacy" means the pre-cutover JSON inbox verbs — the file-based command channel Emacs and the daemon once both consumed — carried forward verbatim rather than modeled.
  - It does NOT fully break the closed vocabulary: `type` is constrained by comment to eight verbs, the daemon validates it, and `Struct` keeps every supplied field inspectable.
  - It DOES weaken it at the payload level: the eight bodies are unschematized, so no generated type, no required-field check, and no wire compatibility protects them. **The guarantee is prose plus a runtime check, not structure** — the one place in the file where an arm's contents are not closed.
- **`HostActionCompletedCmd`** — closes out a `HostAction` by `action_id`; `ok=false` **retains** the action and records the error rather than silently dropping a UI request Emacs could not perform. `NO`.

---

## Hibernation and the workspace gate — 12

### Hibernation detail — 4 → `footer.proto`

Reached as `SessionView.hibernation`.

- **`HibernationDetail`** — when the session slept and the typed cause. `NO`. The typed account behind the coarse `SessionView.hibernated` bool; **the only per-session surface distinguishing a deliberate stand-down from an idle-sweeper reclaim**, which `RENDER_STATE_HIBERNATED` cannot do alone.
  - **Schema hazard:** nothing forbids publishing a workspace as `RENDER_STATE_HIBERNATED` with no `hibernation` detail set, so the teal "nothing is wrong" reading is representable for a workspace stood down by a backend bounce that never slept.
- **`HibernationIdleCutoff`** — automatic hibernation at the keep-alive loop's configured cutoff, carrying that cutoff. `NO`. Lets the card name the threshold without the frontend knowing daemon config.
- **`HibernationForced`** — empty; the user hibernated it via `HibernateWorkspaceCmd`. `NO`. Stops the card blaming an automatic sweeper for a deliberate act.
- **`HibernationCacheExpired`** — the prompt cache went cold before a keep-alive ping fired, carrying actual elapsed idle time and the expected TTL exceeded. `NO` — `TokenCacheHits`/`TokenCacheMisses` are per-turn accounting, not a TTL judgement. Explains a lid-close or daemon-downtime hibernation, where an idle-cutoff explanation would be a lie about what the daemon did.

### The gate — 3 → stays in `shared.proto`

Reachable only via `frame.proto` (`workspace_gate = 22`,
`StateSnapshot.workspace_gates = 14`). `footer.proto` names `WorkspaceGateView`
only inside a prose comment, which is not a field reference.

- **`WorkspaceGateView`** — whether prompts may be sent right now and, if not, what decision is pending; fenced and workspace-addressed. `NO`. Drives the full-pane revival card that **displaces the feed** and blocks the composer.
- **`WorkspaceGateOpen`** — empty; the composer is live. `NO`. Takes the overlay down and restores the feed.
- **`WorkspaceGateHibernated`** — the gate is closed because the session is asleep, carrying the `HibernationDetail` the card explains itself from, so a closed gate always arrives with its own account. `NO`.
  - **`RENDER_STATE_HIBERNATED` and `QueueEntryRevivalHold` are distinct facts, not re-spellings.** The render state is a color/precedence ruling carrying no cause and no fence; `QueueEntryRevivalHold` is empty and describes one queue entry waiting behind an *already-chosen* revival. Neither can answer "may I prompt, and what must I decide first", which is all this arm exists to answer.

### Revival commands — 5 → stays in `shared.proto`

Reachable only as `FrontendCommand.revive_session` (field 28). The four canonical
files carry only the *gate*, never the command answering it.

- **`ReviveSessionCmd`** — the one-shot revival decision as a oneof of three mode arms. `NO` — every `conversation.v1` payload records something that already happened; this is forward-looking intent. Backs the card's Compact / Resume as-is / Clear buttons.
- **`ReviveCompactFirst`** — the compact-first arm plus a required `CompactionScope`. `NO` — the *request* to compact, whereas `ContextCompacted` is the *record* of one that landed; opposite sides of the event, no shared field.
- **`CompactionScope`** *(enum)* — what a revival compaction may summarize away (all / responses / prompts / prompts-and-responses), each carried to the CLI as `/compact <instructions>`. `NO`.
  - **Enum legitimacy:** a genuine non-state closed set — a choice about *scope*, not a phase or condition. The surrounding design still uses a oneof where a state is at stake: `ReviveSessionCmd.mode` is a oneof of empty messages precisely so "no decision" is unrepresentable.
  - **No overlap with `ContextCut`/`ContextCompacted`:** they are the two halves of a request/record pair and would coexist for the same event — the command goes out on `FrontendCommand`, the resulting `ContextCut` lands in the conversation.
- **`ReviveDirect`** — empty; resume as-is with full accumulated context. `NO`. The deliberate "I know it's big" button.
- **`ReviveClear`** — empty; discard the conversation outright on wake. `NO` — `ContextCleared` is the *record*; this is the *request*, produced by a different actor. Deliberately scope-less, because a clear keeps nothing.

---

## Slash commands — 2

- **`SessionCommandSpec`** — per-command facts that are schema rather than traffic: the literal spelling (`/compact`) and whether trailing text is an argument. `NO`.
  - **Not a field on anything.** Attached by `extend google.protobuf.EnumValueOptions { SessionCommandSpec session_command_spec = 60002; }` and read back only by reflection (`ts/schema-literals.ts`'s `sessionCommandSpecs()`). It travels on no frame.
  - **Why an extension:** it annotates each enum value in place, so identity and spelling live in one definition. It replaced three hand-written copies that had drifted — the daemon's recognition table, the webapp's `SESSION_COMMANDS` list, and `SESSION_COMMAND_LABELS`.
  - `takes_args` defaults to false as the safe side: a no-arg command is recognized only as an entire prompt, so `/status of the build` stays a prompt and keeps its user message.
- **`SessionCommand`** *(enum)* — the closed set of slash commands the daemon/CLI answers itself rather than forwarding to the agent. `NO` — a command *identity*; `ContextCut` names the outcome of two of these but carries no command identity and cannot express the other twenty-eight.
  - **The only symbol in `shared.proto` reached directly by two canonical files.**
  - **Used by:**
    - `frontend.v1.DaemonInterceptedCommandItem` — in `feed.proto`
      - it is the message's *only* field and the whole record of "the user invoked a slash command"; the item is `Message.payload.daemon_intercepted_command`
      - the absence of any text field makes the enum load-bearing: an enum is the only shape that can carry the identity without also being able to carry the user's typed argument (`/model opus`) onto a surface that must not show it
    - `frontend.v1.QueueClassificationUninterruptibleTurn` — in `footer.proto`
      - names *which* cut (`/compact` or `/clear`) is running in front of a held prompt; an arm that could not say which would explain nothing
      - reaches the footer as `QueueEntry.classification.uninterruptible_turn` inside `QueueView.entries`
    - `frontend.v1.FailureCardView` — in `feed.proto`
      - transitively via `FailureKind` → `FailureQueueEntryUninterruptibleTurn.command`
