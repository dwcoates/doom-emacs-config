# RE-SPELLINGS — where a namespace re-declares another's types

Every entry is a type that exists in one namespace and was written out again in
another instead of imported. See the intra-namespace carve-out rule in
`DESIGN-protobuf-surfaces.md`.

**How this was found.** The message inventory of each namespace, compared
against `conversation.v1`'s 56 messages and against each other, plus every bare
`string *_id` field in `frontend.v1`.

**What is NOT here.** `frontend.v1` types that embed a `conversation.v1` message
whole and add native fields are correct and stay. `agent-response.proto`,
`tool-call.proto` and `agent-emission.proto` all do this — they are the working
example of the shape everything else should take.

---

## 1. The detached-work vocabulary exists in THREE sizes

The clearest case, and the one that already diverged.

| | outcome arms | count |
|---|---|---|
| `conversation.v1.DetachedWorkEnded` | succeeded, failed, cancelled, **lost** | 4 |
| `frontend.v1.DetachedWorkSettled` | done, error, killed | 3 |
| `frontend.v1.TaskEntry` status | done, error, killed, lost, running, stopped | 6 |

The KIND vocabulary is duplicated the same way, and its names have already
drifted apart:

| | arms |
|---|---|
| `conversation.v1.DetachedWorkKind` | agent, shell, **workflow**, unclassified, skill, merge |
| `frontend.v1.DetachedWork.kind` | agent, shell, **journal**, unclassified, skill, merge |
| `frontend.v1.TaskEntry.kind` | agent, shell, workflow, unclassified |

Three spellings of one idea, at three different sizes, with one name already
different. Nothing failed to compile at any point.

Re-spelled messages: `DetachedWorkAgent`, `DetachedWorkShell`,
`DetachedWorkJournal`, `DetachedWorkUnclassified`, `DetachedWorkSkill`,
`DetachedWorkMerge`, `DetachedWorkOutcomeDone`, `DetachedWorkOutcomeError`,
`DetachedWorkOutcomeKilled`, `TaskKindAgent`, `TaskKindShell`,
`TaskKindWorkflow`, `TaskKindUnclassified`, `TaskStatusDone`, `TaskStatusError`,
`TaskStatusKilled`, `TaskStatusLost`, `TaskStatusRunning`, `TaskStatusStopped`.

## 2. The message model itself

| `frontend.v1` | `conversation.v1` |
|---|---|
| `Message.uuid` | `MessageEntry.message_id` |
| `MessageLineage` | `MessageParent` |
| `AgentEmission.response` / `.tool_call` / `.tool_result` | `AgentSaid`, `ToolReturned` |
| `Message.user_message` (`UserContent`) | `UserSaid` |
| `Message.detached_work` | `DetachedWorkStarted`/`Progressed`/`Ended` |

`Message` should be a thin wrapper: a `MessageEntry` arm plus arms for what the
daemon synthesizes and no conversation record exists for (`FailureCardView`,
`DaemonInterceptedCommandItem`, `CompactionSummaryItem`). `durability` and
`source` legitimately stay on the wrapper — they are facts about the FEED, not
about the record.

Also re-spelled: `frontend.v1.TypingDelta` against
`conversation.v1.ContentArriving`, and `frontend.v1.SkillBodyItem` /
`DetachedWorkSkillBodyResolved` against `conversation.v1.SkillBodyResolved`.

## 3. Five names declared in BOTH `protocol.v1` and `frontend.v1`

Not conversation re-spellings — a second axis of the same defect, and these are
identical names in two packages:

- `DetachedAgentsCancelled`
- `DetachedCancelOutcome`
- `DetachedCancelUnsupported`
- `NoDetachedAgentsRunning`
- `ModelOption`

Each is a shim-wire fact the daemon forwards. One of the two declarations is
redundant.

## 4. Typed identities held as bare strings in `frontend.v1`

A `string message_id` is a `conversation.v1` identity with its type removed. It
accepts any string at all, so nothing structural stops a turn id, a request id
or an empty string from being assigned to it.

The message-identity ones, which are `conversation.v1`'s to mint:

- `feed.proto:95` `top_level_message_id`
- `feed.proto:103` `parent_message_id`
- `feed.proto:421`, `feed.proto:443` `parent_message_id`
- `detached-work.proto:363` `message_id`
- `agent-response.proto:46` `api_message_id`
- `errors.proto:47` `api_message_id`

The identity is already modeled: `conversation.v1.MessageParent` is a oneof
precisely so "feed row" and "unresolved parent" cannot wear the same value —
and every bare `parent_message_id` above re-admits exactly the ambiguity that
oneof exists to forbid.

Lower priority, same class: `entry_id`, `run_id`, `offer_id`,
`permission_request_id`, `query_instance_id`, `turn_id`, `session_id`,
`claude_session_id` — 40+ bare `string *_id` fields across `frontend.v1`.

## 5. `state.v1`'s token vocabulary — flagged, NOT judged

`state.v1` declares `TokenUsageTotals`, `TokenCacheCreation`, `VendorTokenUsage`
and `TokenOutputDetails` against `conversation.v1`'s `TokenUsage`,
`TokenCacheHits` and `TokenCacheMisses`.

This one may be legitimate. `state.v1` is the durable persistence layer and
carries its own compatibility constraints — it is explicitly BENEATH the UI
mapping, and `TokenUtilization`/`TurnAccounting` moved there field-for-field
identical so replay stays byte-safe. A vendor-faithful durable spelling and a
vendor-agnostic conversation spelling are arguably two different things rather
than one thing written twice.

Needs a decision rather than a fix.

---

## Ranking

1. **The detached-work vocabulary** — three sizes, already drifted, and the
   `workflow`/`journal` split means a consumer reading one cannot switch on the
   other.
2. **`Message`** — the largest structural change, and the one that makes
   passthrough literal (gap 11).
3. **The five protocol/frontend duplicate names** — smallest, purely mechanical.
4. **Bare message-identity strings** — worth doing with (2), since `Message`
   embedding `MessageEntry` removes most of them for free.
5. **`state.v1` tokens** — decide before touching.
