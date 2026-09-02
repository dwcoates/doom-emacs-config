# agent-shim/claude/shim/

The per-session Claude shim: one TypeScript/Node process per workspace session,
spawned by the daemon, driving the vendor's agent binary through the Claude
Agent SDK.

## The six surfaces, and which direction each faces

| Surface | Direction | Where the contract lives |
| --- | --- | --- |
| `shim.v1` | the shim **SERVES** it to the daemon | `proto/src/shim/v1/` |
| `store.v1` | the shim **WRITES** and **READS** it | `proto/src/store/v1/` |
| `conversation.v1` | the shim **PRODUCES** it into the store | `proto/src/conversation/v1/` |
| the Claude Agent SDK | the shim **DRIVES** it | `node_modules/@anthropic-ai/claude-agent-sdk/sdk.d.ts` |
| the vendor's files | the shim's transcript backup **READS** them; `--fake` **WRITES** them | `docs/overhaul/shim.md`, mock section |
| the kernel locks | the shim **HOLDS** them; the daemon **PROBES** them | `~/.cache/agent-repl/run/` (`$AGENT_REPL_LOCK_DIR`) |

Everything the shim says on the wire is one of the first three. Nothing
vendor-shaped leaves this process: the vendor's uuids, message ids and record
shapes stay inside it, and what crosses the boundary is `conversation.v1`.

## What the shim is

- **The vendor adapter.** One shim per session. It converts the SDK's flat
  message log into `conversation.v1` units with identity, upserted whole.
- **Stateless.** It accumulates nothing of variable size. History is served
  FROM THE STORE, never from memory; the joins it keeps are constant-size (the
  pending permission callbacks, the live task table, spawn provenance).
- **Longer-lived than its daemon.** A daemon disconnect does not end a turn, and
  a daemon death does not end the process. That is why the log sink is an
  inherited descriptor rather than a pipe to the daemon's stderr, and why
  SIGTERM exists as a teardown path at all.

## Module map

```
src/
  main.ts              argv, env, the log fd, signals, --version, wiring (NO locks)
  build-identity.ts    SHIM_BUILD_SHA + sdk/agent-binary versions (SessionRuntime)
  log.ts               THE canonical JSONL logging API
  locks.ts             the two kernel flocks (both taken inside StartSession)
  vendor-guard.ts      the ONLY dynamic import of the SDK; the FORBID_VENDOR_CALLS gate
  metaprompt.ts        the canonical metaprompt append
  proto.ts             THE single import site: shimv1 / storev1 / conversationv1 namespaces
  sdk/
    types.ts           the SDK boundary, aliased off sdk.d.ts; the upgrade canary's surface
    real-query.ts      the real query() factory (preset prompt, setting sources, pre-mint)
  service/
    server.ts          the UDS listener (h2c + HTTP/1.1 on one socket)
    routes.ts          the shim.v1 implementation: one handler per rpc
    failures.ts        one constructor per failure message + arm
    validate/          fields.ts (per non-primitive field), requests.ts (per request message)
  engine/
    engine.ts          the Engine seam + NotImplementedEngine
    session.ts turn.ts identity.ts cold.ts keepalive.ts compaction.ts
    backup.ts detached.ts pushes.ts permission-gate.ts
  convert/
    fold.ts            the fold seam and FoldOutput
    ids.ts             the four identifier spaces, minted
  store/
    keys.ts            upsert_key + write_id (THE one place)
    client.ts          the store.v1 client over the store UDS
  fake/
    index.ts           createFakeQuery(): the scenario engine behind --fake
test/
  one test file per src module, mirroring its path
  fakes/store-server.ts   an in-process store.v1 server, shared by the store and integration suites
scripts/
  dist-smoke.ts        the built bundle, spawned and dialed for real
  capture/             the capture harness
```

## The spawn contract

```
node dist/main.js --listen <uds> --store-socket <uds> --log-fd 3 [--fake]
node dist/main.js --version
```

Nothing else. An unrecognized flag is a startup **failure**, not a warning: it
means the daemon and this build disagree about the contract.

- **cwd** is the workspace directory, set by the spawner. It is not a flag —
  two sources for one fact can disagree.
- **Session facts travel only in `StartSession`.** The model, the permission
  mode and the vendor binding (fresh with a pre-minted id, or a resume handle)
  are rpc arguments, so `--session-id`, `--model`, `--permission-mode` and
  `--resume` do not exist. Neither does `--claude-bin`: the SDK's own bundled,
  pinned binary is the engine (R12).
- **Environment**, all refusals rather than defaults:
  - `CLAUDE_CONFIG_DIR` (required) — which ACCOUNT the session runs as.
  - `AGENT_REPL_OWNED=1` (required) — the daemon's mark; a shim refuses to run
    unowned.
  - `SHIM_BUILD_SHA` (required) — reported on `SessionStarted`; the daemon
    compares it against the deploy stamp and bounces a stale survivor.
  - `AGENT_REPL_STATE_DIR` (default `~/.claude-emacs`) — the one state root.
  - `AGENT_REPL_STORE_SOCKET` — the store socket when `--store-socket` is
    absent. **The flag beats the env.**
  - `AGENT_REPL_LOCK_DIR` (default `~/.cache/agent-repl/run`) — the kernel-lock
    directory. Both the shim and the daemon's probe read it, which is what makes
    relocation safe.
  - `AGENT_REPL_FORBID_VENDOR_CALLS` — the guard (see below).
  - `AGENT_REPL_FAKE_TURN_GATE`, `AGENT_REPL_FAKE_TURN_GATE_TEXT`,
    `AGENT_REPL_FAKE_SPOOL_ROOT` — `--fake` only.
- **Startup order**: parse argv → resolve env → configure the log on fd 3 →
  bind the UDS → serve. **NO LOCK IS TAKEN AT STARTUP.** A shim that has served
  but has no session is **INERT** and holds neither kernel lock, which is what
  lets the daemon prelaunch a replacement beside the live shim instead of
  wedging it behind a lock the live shim holds for its lifetime.
- **Both kernel locks are taken inside `StartSession`**, before the SDK is
  touched and held for the process lifetime: the SESSION lock first (keyed by
  the vendor session id), then the WORKSPACE lock (keyed by the cwd), in that
  fixed order so two racing shims cannot take them in opposite orders. Either
  conflict answers `StartSession` `conversation_owned` — one arm, because from
  the daemon's side "someone else owns this conversation" is one fact — with
  the contended lock path in the detail. The daemon's probe is unchanged: a
  held lock still means a live shim owns the conversation.
- **Signals**: SIGTERM is the one authorized shutdown and takes the
  `KillSession{force:true}` path, then exits 0 (nonzero if the stand-down
  failed). SIGINT is REFUSED and logged at error — an attached terminal's Ctrl-C
  must not end a live turn.
- **`--version`** prints `claude-shim <version>` and exits before any socket,
  lock, log fd or SDK import. It is a dependency-free smoke of the bundle.

## Mocked vendor: prompt → scenario table

GENERATED from `src/fake/registry.ts` by `scripts/scenario-table.ts`, and
asserted against it by `test/fake/registry.test.ts` in BOTH directions: a
scenario missing from this table fails the suite, and a row with no scenario
behind it does too. Regenerate rather than edit:

```
npx esbuild scripts/scenario-table.ts --bundle --platform=node --format=esm \
  --outfile=/tmp/scenario-table.mjs && node /tmp/scenario-table.mjs
```

A prompt selects a scenario by an exact `!name` prefix followed by whitespace or
end-of-string; the longest matching name wins. Anything else is plain prose,
EXCEPT a prompt containing the literal `e2e-fail-this-turn`, which fails the
turn (the daemon's merge-pipeline gate spells it identically). Env:
`AGENT_REPL_FAKE_TURN_GATE` + `_TEXT` park a matching turn until the named path
appears; `AGENT_REPL_FAKE_SPOOL_ROOT` roots the spool tree.

### Which rows are capture-grounded, and which are only declared

Every row below is one of two things, and the difference matters when a row and
production disagree.

**CAPTURE-GROUNDED.** A real recording of the actual agent binary stands behind
it, under `testdata/captures/`, and `test/fake/golden-conformance.test.ts` drives
the mock's scenario and that capture through the SAME fold and compares the unit
kinds they produce. Twenty-seven of the fifty-nine mapped rows reproduce their capture's shape EXACTLY;
the rest are pinned there too, each with a stated reason for the difference —
almost always MODEL CHOICE (the recorded run read a file before editing it, or
globbed with `Bash`), which is one model's habits rather than vendor shape.

**DECLARED, NOT CAPTURE-GROUNDED.** No capture exercises the arm, so the row is
built from `sdk.d.ts` and the corpus fixtures alone. These stay — a declared type
is still a contract — but nothing has confirmed the vendor spells them this way:

- the typed arms the recorded runs never reached, because the model chose `Bash`
  or `Skill` instead: `!glob`, `!grep-content`, `!grep-files`, `!grep-count`,
  `!artifact-publish`, `!artifact-list`, `!wakeup-schedule`, `!wakeup-stop`,
  `!worktree-keep`, `!worktree-remove`, `!memory`, `!skills-injected`;
- `!send-message-resumed` / `!send-message-refused` — no capture addresses a
  subagent;
- `!subagent-failed` — no capture has a failed subagent;
- every `!api-*` row, `!refusal-fallback`, `!refusal-no-fallback` and
  `!context-window`: the capture harness quarantines an API error, so an API
  failure can never be a golden;
- `!query-eof`, `!query-fail`, `!fail-marker`, `!fault-converter`,
  `!fault-recover` — producer-side failures no vendor run produces;
- `!cold-seed` — the cold gate is tripped on a LATER resume, which no single
  capture spans;
- `!compact`, `!compact-auto`, `!compact-failed` — the `compaction-directed`
  capture answered "Not enough messages to compact", so no
  `compact_boundary` / `isCompactSummary` record exists anywhere in the corpus.
  The compaction writer stays graded against the corpus fixture and MARKED
  SYNTHETIC until a longer-history capture is approved.

| Prompt | What the vendor emits | What it writes on disk | conversation.v1 arms exercised |
| --- | --- | --- | --- |
| `(any text with no `!scenario` prefix)` | one API response of four blocks — withheld thinking, visible thinking, an opening text block, and the concluding text block — then a success `result` whose `result` is the conclusion verbatim | four assistant lines sharing one `message.id`, then the user prompt line and the turn record | AgentThinking (withheld + text), AgentResponse.from_model, AgentSuccess.completed |
| `!md` | one text block carrying the markdown showcase, then a success `result` | one assistant line, the prompt line and the turn record | AgentResponse.from_model, AgentSuccess.completed |
| `!read` | a `Read` tool_use with only `file_path`, then a text tool_result whose `toolUseResult.file` spans the whole file | the tool_use assistant line, the tool_result user line with `toolUseResult`, the closing text line | AgentRead.start + AgentReadSuccess.extent=whole |
| `!read-head` | a `Read` with `limit` and no `offset`, answered with the first lines and a total that exceeds them | the tool_use line, the tool_result line, the closing text line | AgentRead.start + AgentReadSuccess.extent=head |
| `!read-range` | a `Read` with both `offset` and `limit`, answered with a window whose `startLine` is not 1 | the tool_use line, the tool_result line, the closing text line | AgentRead.start + AgentReadSuccess.extent=range |
| `!read-truncated` | a `Read` cut at the token cap, plus the vendor's `read_truncation_notice` attachment naming the call | the tool_use line, the tool_result line, a `read_truncation_notice` attachment line, the closing text line | AgentReadSuccess.cut=token_cap |
| `!read-image` | a `Read` of a png, answered with an image content block and an image `toolUseResult` carrying dimensions | the tool_use line, the image tool_result line, the closing text line | AgentReadSuccess with an ImageBlock |
| `!write-create` | a `Write` answered with `toolUseResult.type: "create"` and an empty `structuredPatch` | the tool_use line, the tool_result line, the closing text line | AgentWrite.start + AgentWriteSuccess.outcome=created |
| `!write-update` | a `Write` over an existing file, answered with `type: "update"`, the prior body and a structuredPatch | the tool_use line, the tool_result line, the closing text line | AgentWrite.start + AgentWriteSuccess.outcome=updated |
| `!edit` | an `Edit` answered with the corpus edit shape — filePath, oldString, newString, structuredPatch, replaceAll | the tool_use line, the tool_result line, the closing text line | AgentEdit.start + AgentEditSuccess |
| `!ide-diagnostics` | an `Edit`, then the vendor's `diagnostics` attachment reporting a typescript error in the edited file | the tool_use line, the tool_result line, a `diagnostics` attachment line, the closing text line | AgentEdit.diagnostics (AgentDiagnosticsReport joined to the last edit by adjacency) |
| `!grep-content` | a `Grep` in content mode answered with matching lines and a total that exceeds them | the tool_use line, the tool_result line, the closing text line | AgentGrep.start + AgentGrepSuccess.matches=content (extent=partial) |
| `!grep-files` | a `Grep` in files_with_matches mode answered with paths only | the tool_use line, the tool_result line, the closing text line | AgentGrepSuccess.matches=files (extent=all) |
| `!grep-count` | a `Grep` in count mode answered with per-file counts | the tool_use line, the tool_result line, the closing text line | AgentGrepSuccess.matches=count |
| `!glob` | a `Glob` answered with a truncated path list and a total larger than the list | the tool_use line, the tool_result line, the closing text line | AgentGlob.start + AgentGlobSuccess.extent=partial with an omitted count |
| `!bash [command]` | a foreground `Bash` tool_use, then its result — nothing in between, because foreground output is unobservable while running | the tool_use line, the tool_result line with a `BashOutput`-shaped `toolUseResult`, the closing text line | AgentBash.start + AgentBashSuccess.outcome=completed how=exited(0) |
| `!bash-hold` | a FOREGROUND `Bash` that never returns: the tool_use lands and the turn parks until an interrupt, so the unit stays live and foreground for as long as a caller needs it to | the tool_use line, the prompt line and (at the interrupt) the turn record | no terminal at all while it holds — the lever for DetachForeground's `unsupported` refusal, which needs a GENUINELY LIVE foreground unit to refuse (`!bash` settles before the call can be made, so it answered `already_concluded` instead and the refusal under test was never reached) |
| `!bash-fail` | a foreground `Bash` whose result is an ERROR carrying stderr and a non-zero interpretation | the tool_use line, the error tool_result line, the closing text line | AgentBashSuccess.outcome=completed how=exited(non-zero) — a non-zero exit is a completed run, not a failure |
| `!bash-timeout` | a foreground `Bash` that hits its timeout: `task_started`, then a result carrying `timedOutAfterMs` and `backgroundTaskId` — the vendor auto-backgrounds rather than killing | the tool_use line, the tool_result line, an incremental spool with NO `EXIT=` line, the closing text line | AgentBashInterrupted.cause=timed_out; the run stays live as detached work |
| `!bash-spill` | a foreground `Bash` whose output was too large for the message and spilled to a file on disk | the tool_use line, the tool_result line carrying `persistedOutputPath`/`persistedOutputSize`, the closing text line | AgentBashOutputPartial — the partial extent with the omitted byte count |
| `!bash-image` | a foreground `Bash` whose stdout IS image data (`isImage: true`), answered with an image content block | the tool_use line, the image tool_result line, the closing text line | AgentBashOutput.form=image |
| `!bash-detach` | a `Bash` with `run_in_background`, `task_started`, `background_tasks_changed`, a result carrying only `backgroundTaskId`, then — after the turn — `task_updated` and a completed `task_notification` | the tool_use and tool_result lines, and `<spool-root>/<slug>/<session>/tasks/b<hex>.output` written INCREMENTALLY and terminated by `EXIT=0` | AgentBash detached_work + AgentBashUpdate deltas fed by the sidecar tailing the spool |
| `!bash-detach-fail` | a detached `Bash` that ends non-zero: `task_updated{status:"failed"}` and a failed `task_notification` | the tool_use and tool_result lines, and a spool terminated by `EXIT=3` | AgentBash detached_work terminating in a non-zero exit |
| `!bash-detach-live` | a detached `Bash` that NEVER finishes: no terminal notification, and the task stays in the live set | an unterminated spool with no `EXIT=` line — the corpus's `bash-midoutput.output` shape | AgentBash detached_work still live; what a fan-wide cancel and a StopBash act on |
| `!ctrl-b` | a FOREGROUND `Bash` the user detaches mid-flight: the scenario parks, `backgroundTasks(toolUseId)` marks it `is_backgrounded`, and the foreground result then reports `backgroundedByUser: true` | the tool_use line, the tool_result line carrying `backgroundedByUser`, an unterminated spool | AgentBackgrounded — the user-requested detach of foreground work |
| `!web-fetch` | a `WebFetch` answered with the corpus shape: bytes, code, codeText, result, durationMs, url | the tool_use line, the tool_result line, the closing text line | AgentWebFetch.start + AgentWebFetchSuccess |
| `!web-fetch-redirect` | a `WebFetch` answered with a 302 and the vendor's redirect instruction as the result body | the tool_use line, the tool_result line, the closing text line | AgentWebFetchSuccess carrying a non-2xx status |
| `!web-search` | a `WebSearch` answered with BOTH result kinds — a hit list keyed by a server tool_use id, and a bare commentary string | the tool_use line, the tool_result line, the closing text line | AgentWebSearch.start + AgentWebSearchSuccess with entry=link AND entry=note |
| `!skill` | a `Skill` tool_use, its `{success, commandName, allowedTools}` acknowledgement, then the skill DOCUMENT as an `isMeta` user record joined by `sourceToolUseID` | the tool_use line, the acknowledgement line, the isMeta document line, the closing text line | AgentSkillUse.start + AgentSkillUseSuccess settled on the document, with the allowances |
| `!skill-fail` | a `Skill` for a name that does not resolve, answered with an error result and no document | the tool_use line, the error tool_result line, the closing text line | AgentSkillUse.start + AgentSkillUseFailure |
| `!memory` | prose only; the injected memory is a FILE-PLANE fact the vendor never streams | a `nested_memory` attachment line and a `file` attachment line carrying a memory file's body | AgentContextInjected.injected=memory |
| `!skills-injected` | prose only; the injected skills are attachment records | `invoked_skills`, `dynamic_skill` and `skill_listing` attachment lines | AgentContextInjected.injected=skills |
| `!task-create` | two `TaskCreate` calls and a `TaskUpdate` that links the second as blocked by the first | the tool_use and tool_result lines for all three calls, the closing text line | AgentTaskAct.act=created (twice) with the DAG edge |
| `!task-change` | a `TaskUpdate` answered with the corpus's `statusChange` shape (`from`/`to`) | the tool_use line, the tool_result line, the closing text line | AgentTaskAct.act=changed with status pending→running |
| `!task-reject` | a `TaskUpdate` the board REFUSES, answered with `success: false` and an `error` | the tool_use line, the error tool_result line, the closing text line | AgentTaskAct.act=rejected |
| `!send-message` | a `SendMessage` to a LIVE agent, answered WITHOUT `resumedAgentId` — the message queues for its next tool round | the tool_use line, the tool_result line, the closing text line | AgentSendMessage.delivery=queued_to_live |
| `!send-message-resumed` | a `SendMessage` to an IDLE agent, answered WITH `resumedAgentId` and an output-file path — the vendor resumed it from its transcript in the background | the tool_use line, the tool_result line, the closing text line | AgentSendMessage.delivery=resumed_recipient |
| `!send-message-refused` | a `SendMessage` to an agent the user stopped, answered with `success: false` and the vendor's refusal prose | the tool_use line, the tool_result line, the closing text line | AgentSendMessageFailure |
| `!subagent` | an `Agent` tool_use, then the SUBAGENT's own assistant and user messages carrying `agent_id`, `parent_tool_use_id`, `subagent_type` and `task_description`, then the completed `AgentOutput` | `<session>/subagents/agent-<id>.meta.json` and `agent-<id>.jsonl` (the subagent's own chained sidechain transcript), plus the main transcript's tool_use and tool_result lines | AgentSubagent.start + AgentSubagentUpdate (nested activity) + AgentSubagentSuccess with full usage |
| `!subagent-detached` | an `Agent` with `run_in_background`: `task_started`, `background_tasks_changed`, an `async_launched` `AgentOutput` naming the output file, then a completed `task_notification` carrying usage | the agent's `.meta.json` and `agent-<id>.jsonl`, the spool `<spool-root>/<slug>/<session>/tasks/a<hex>.output` written as AGENT JSONL, and the main transcript's lines | AgentSubagent detached_work + AgentSubagentSuccess.usage=total_only from the notification |
| `!subagent-failed` | a detached `Agent` that ends in failure: `task_updated{status:"failed"}` and a failed `task_notification` | the agent's `.meta.json` and transcript, its spool, and the main transcript's lines | AgentSubagentFailure |
| `!cancel-all` | THREE detached items launched in one turn — two agents and a shell — left LIVE. The cancel is the caller's `stopTask` per item; emptying the live set makes the engine write the vendor's `agents_killed` record | both agents' `.meta.json` and transcripts, the shell's spool, and the main transcript's lines | the fan-wide cancel: AgentSubagentFailure.cause=stopped_by_user per item, plus the agents_killed record |
| `!plan` | an `EnterPlanMode` call, prose written under plan mode, then an `ExitPlanMode` answered with the plan and the path it was saved to, plus the vendor's `plan_mode_exit` attachment | the tool_use and tool_result lines for both calls, a `plan_mode_exit` attachment line, the closing text line | AgentPlanMode.act=enter/exit with AgentPlanModeEntered and AgentPlanModeExited |
| `!findings` | a `ReportFindings` carrying THREE findings — one confirmed, one plausible, and one re-reported with an `outcome` — so every verdict and every outcome arm is reachable from one call | the tool_use line, the tool_result line, the closing text line | AgentReportFindings.start + Success with verdict=confirmed/plausible and outcome=fixed/skipped/no_change_needed |
| `!worktree-keep` | an `EnterWorktree` then an `ExitWorktree` with `action: "keep"` — the worktree and branch stay on disk | the tool_use and tool_result lines for both calls, the closing text line | AgentWorktree.act=enter/exit with outcome=kept |
| `!worktree-remove` | an `ExitWorktree` with `action: "remove"` reporting the discarded file and commit counts | the tool_use and tool_result lines, the closing text line | AgentWorktree.act=exit with outcome=removed |
| `!cron` | a `CronCreate`, a `CronList` and a `CronDelete` — all three acts in one turn | the tool_use and tool_result lines for all three calls, the closing text line | AgentCron.act=create/list/delete with created/listed/deleted |
| `!push-sent` | a `PushNotification` answered with `pushSent: true` | the tool_use line, the tool_result line, the closing text line | AgentPushNotification.outcome=sent |
| `!push-config-off` | a `PushNotification` answered with `disabledReason: "config_off"` | the tool_use line, the tool_result line, the closing text line | AgentPushNotification.outcome=not_sent reason=config_off |
| `!push-user-present` | a `PushNotification` answered with `disabledReason: "user_present"` | the tool_use line, the tool_result line, the closing text line | AgentPushNotification.outcome=not_sent reason=user_present |
| `!push-no-transport` | a `PushNotification` answered with `disabledReason: "no_transport"` | the tool_use line, the tool_result line, the closing text line | AgentPushNotification.outcome=not_sent reason=no_transport |
| `!monitor-deadline` | a `Monitor` with a finite `timeoutMs` and `persistent: false` (corpus: tool-results/monitor.jsonl) | the tool_use line, the tool_result line, the closing text line; the monitor stays in the live set | AgentMonitor.lifetime=deadline |
| `!monitor-persistent` | a `Monitor` with `timeoutMs: 0` and `persistent: true` — it runs until TaskStop or session end | the tool_use line, the tool_result line, the closing text line; the monitor stays in the live set | AgentMonitor.lifetime=persistent |
| `!wakeup-schedule` | a `ScheduleWakeup` answered with the corpus shape — scheduledFor, clampedDelaySeconds, wasClamped | the tool_use line, the tool_result line, the closing text line | AgentScheduleWakeup.act=schedule outcome=scheduled |
| `!wakeup-stop` | a `ScheduleWakeup` with `stop: true`, answered with `stopped: true` and the cancelled count | the tool_use line, the tool_result line, the closing text line | AgentScheduleWakeup.act=stop outcome=stopped |
| `!artifact-publish` | an `Artifact` publish answered with the url, the source path, a title and a contract version | the tool_use line, the tool_result line, and a `frame-link` metadata line, the closing text line | AgentArtifact.act=publish outcome=published |
| `!artifact-list` | an `Artifact` list answered with two rows, one owned and one shared, and `truncated: false` | the tool_use line, the tool_result line, the closing text line | AgentArtifact.act=list outcome=listed |
| `!unmodeled` | an `mcp__echo__echo` call — a tool NO converter owns — answered with an opaque payload | the tool_use line, the tool_result line, the closing text line | AgentUnmodeled, with `mcp_server` stated from the vendor's own field rather than parsed out of the name |
| `!hook-success` | `hook_started` and `hook_response{outcome:"success"}` around a `Read` | the tool_use line, a `hook_success` attachment line carrying `toolUseID`, the tool_result line | AgentHook.result=succeeded |
| `!hook-blocked` | `hook_started` and `hook_response{outcome:"error"}` around an `Edit` the hook BLOCKS | the tool_use line, a `hook_blocking_error` attachment line, the error tool_result line | AgentHook.result=blocking_error; the TURN still succeeds, because a blocked tool is not a stopped turn |
| `!hook-failed` | a `SessionStart` hook that FAILS without blocking anything: exit 1 on stderr | a `hook_non_blocking_error` attachment line carrying stderr, exitCode, command and durationMs | AgentHook.result=non_blocking_error |
| `!hook-cancelled` | `hook_started` and `hook_response{outcome:"cancelled"}` around an `Edit` | the tool_use line, a `hook_cancelled` attachment line — four fields and nothing else — the tool_result line | AgentHook.result=cancelled |
| `!perm-allow-once` | a gated `Bash`, one `canUseTool` ask, and the run that follows an ALLOW with no `updatedPermissions` | the tool_use line, the tool_result line, the closing text line | AgentPermission.start + AgentPermissionAllowed.scope=once |
| `!perm-allow-standing` | a gated `Bash` whose ask carries `suggestions`; a standing allow comes back with those rules echoed as `updatedPermissions`, which the scenario reports verbatim in its conclusion | the tool_use line, the tool_result line, the closing text line | AgentPermissionAllowed.scope=standing with the AgentPermissionChange rules |
| `!perm-deny-user` | a gated `Bash` the user DENIES: the deny message becomes the tool_result the model sees, the record carries `toolDenialKind`, and the turn's `result` lists the call under `permission_denials` | the tool_use line, the denied tool_result line, the closing text line | AgentPermissionDenied.by=user |
| `!perm-deny-policy` | a gated `Bash` refused by a RULE — no ask reaches `canUseTool` at all. The vendor emits `system:permission_denied` with `decision_reason_type: "rule"`, and the result lists the denial | the tool_use line, the denied tool_result line carrying `toolDenialKind: "permission-rule"`, the closing text line | AgentPermissionDenied.by=policy, reached without any AgentPermission ask |
| `!perm-undecidable` | a gated `Bash` in `auto` mode whose classifier reaches no verdict: `system:permission_denied` with `decision_reason_type: "classifier"` and no ask | the tool_use line, the denied tool_result line, the closing text line | AgentPermissionDenied.by=undecidable — a KNOWN-OPEN arm: `sdk.d.ts` declares no discriminator that separates 'nobody could decide' from an ordinary policy deny, so this scenario is the closest producer |
| `!ask-single` | one single-select `AskUserQuestion` with four options, asked through the shim's own gate | the tool_use line, the tool_result line carrying `questions` and the `answers` map, the closing text line | AgentQuestion.start choices=single_select + AgentQuestionSuccess.outcome=answered |
| `!ask-multi` | a TWO-question batch: one multi-select and one single-select, so the answer map has to be keyed by the question's own text rather than by position | the tool_use line, the tool_result line, the closing text line | AgentQuestion.choices=multi_select alongside single_select in one batch |
| `!ask-free` | a single-select question answered with FREE TEXT rather than a listed label — the vendor's automatic "Other" option. NO corpus sample exists for this shape; the answer map simply carries prose no option matches | the tool_use line, the tool_result line, the closing text line | AgentQuestionAnswers carrying free text — the residue rule's subject |
| `!ask-unanswered` | a question the user never answers: the gate's DENY becomes an error tool_result and the batch ends unanswered. `sdk.d.ts` declares NO question timeout, so an expiry is modeled as this same denial and the gap is recorded rather than invented | the tool_use line, the error tool_result line, the closing text line | AgentQuestionSuccess.outcome=unanswered |
| `!rotate` | a `/clear` in the OBSERVED shape: ONE `conversation_reset` carrying the OLD `session_id` and a `new_conversation_id` nothing later uses, then a SECOND `system:init` whose `session_id` is the REAL new id (a third uuid), and the REST of the turn — its result included — belongs to that identity | a NEW `<new-session>.jsonl` carrying everything after the reset; the OLD file simply STOPS, with no closing record of any kind | SessionIdentityRotated + AgentUpdate.context_cut(ContextCleared) |
| `!slash` | a slash command the VENDOR answers itself: a `local_command_output` message, and the transcript's `system:local_command` record wrapping the output in `<local-command-stdout>` | a `system:local_command` line and a `command_permissions` attachment line | the vendor-answered slash-command family — no agent activity beyond the answer, and NO reasoning |
| `!context-usage-drift` | prose only. It switches `getContextUsage()` to a GROWING answer, so the `context_usage` the shim pushes at this turn's end differs from the one it pushed at session start — total tokens, percentage, the message category and the whole `messageBreakdown` all move, and the answer stays a full `SDKControlGetContextUsageResponse`. CADENCE IS THE ENGINE'S: context_usage is pushed at session start and at EVERY turn end regardless of scenario, so this one changes what is sampled and never when | the assistant line, the prompt line and the turn record | SessionContextUsage — the same arm twice with DIFFERENT figures, which is what a re-render tests |
| `!model-fallback` | an UNSOLICITED model change: the declared `model_refusal_fallback` message with `direction: "retry"`, the `session_state_changed` beat, and then the answer from the FALLBACK model — whose `message.model` is the only evidence the swap happened. The swap STICKS, so a following turn answers on the fallback model too. Nothing called SetSessionModel, so no confirmation exists anywhere | a `system:model_refusal_fallback` line, the fallback-model assistant line, the prompt line and the turn record | SessionModelChanged with no SetSessionModel behind it — the vendor's own decision, not a confirmed request |
| `!fast-on` | a turn whose `result` reports `fast_mode_state: "on"`. The state STICKS: every later result reports it, and so does the `init` a rotation emits — the two places `sdk.d.ts` carries fast mode at all | the assistant line, the prompt line and the turn record | SessionFastMode.state=on |
| `!fast-off` | a turn whose `result` reports `fast_mode_state: "off"` with `fast_mode_disabled_reason: "preference"`. The state STICKS: every later result reports it, and so does the `init` a rotation emits — the two places `sdk.d.ts` carries fast mode at all | the assistant line, the prompt line and the turn record | SessionFastMode.state=off |
| `!fast-cooldown` | a turn whose `result` reports `fast_mode_state: "cooldown"` with `fast_mode_disabled_reason: "extra_usage_disabled"`. The state STICKS: every later result reports it, and so does the `init` a rotation emits — the two places `sdk.d.ts` carries fast mode at all | the assistant line, the prompt line and the turn record | SessionFastMode.state=cooldown |
| `!mcp-all` | prose only. It switches `mcpServerStatus()` to the FIVE-server catalog — one per declared health: connected, failed, needs-auth, pending, disabled — and the shim's own cadence discovers the change | the assistant line, the prompt line and the turn record | SessionMcpServer.health=connected/failed/needs_auth/pending/disabled |
| `!mcp-healthy` | prose only. It narrows `mcpServerStatus()` to the single connected server, so the arms CHANGE rather than merely existing | the assistant line, the prompt line and the turn record | SessionMcpServer.health=connected only — the change is what a push tests |
| `!usage-available` | prose only; it switches the account-usage answer to the `available` shape | the assistant line, the prompt line and the turn record | SessionAccountUsage.outcome=available with five_hour, seven_day, seven_day_oauth_apps, seven_day_opus, seven_day_sonnet, model_scoped and extra_usage |
| `!usage-full` | prose only; it switches the account-usage answer to the `available` shape | the assistant line, the prompt line and the turn record | SessionAccountUsage.outcome=available with EVERY window populated — five_hour, seven_day, seven_day_oauth_apps, seven_day_opus, seven_day_sonnet, model_scoped and extra_usage, each with utilization and resets_at — beside subscription_type |
| `!usage-opus-absent` | prose only; it switches the account-usage answer to the `opus_absent` shape | the assistant line, the prompt line and the turn record | SessionAccountUsage.outcome=available with seven_day_opus UNSET — an absent optional window, which is not an unavailability |
| `!usage-service-unavailable` | prose only; it switches the account-usage answer to the `service_unavailable` shape | the assistant line, the prompt line and the turn record | SessionAccountUsage.outcome=unavailable reason=service_unavailable |
| `!usage-window-unavailable` | prose only; it switches the account-usage answer to the `window_unavailable` shape | the assistant line, the prompt line and the turn record | SessionAccountUsage.outcome=unavailable reason=window_unavailable — the FIVE-HOUR window is null, which is what that reason means |
| `!usage-utilization-unavailable` | prose only; it switches the account-usage answer to the `utilization_unavailable` shape | the assistant line, the prompt line and the turn record | SessionAccountUsage.outcome=unavailable reason=utilization_unavailable |
| `!usage-sampling-failure` | prose only; it switches the account-usage answer to the `sampling_failure` shape | the assistant line, the prompt line and the turn record | SessionAccountUsage.outcome=unavailable reason=sampling_failure |
| `!rate-limit` | a `rate_limit_event` in the corpus's shape — allowed_warning on the overage window with a threshold | the assistant line, the prompt line and the turn record | SessionAccountUsage from the rate-limit event, plus the usage-warning synthesized notice |
| `!rate-limit-five-hour` | a `rate_limit_event` naming the `five_hour` window — `allowed_warning` with a utilization and a reset instant, the shape the FOOTER's window row joins against | the assistant line, the prompt line and the turn record | SessionRateLimitStatus.rate_limit_type=five_hour |
| `!rate-limit-seven-day` | a `rate_limit_event` naming the `seven_day` window — `allowed_warning` with a utilization and a reset instant, the shape the FOOTER's window row joins against | the assistant line, the prompt line and the turn record | SessionRateLimitStatus.rate_limit_type=seven_day |
| `!context-tip` | prose only, plus the vendor's `context_tip` ATTACHMENT — a GENERIC CLI TIP, which is what the one real capture of this record actually is. IT IS NOT THE CONTEXT-BUDGET WARNING (ruling, landing 5): which attachment carries that warning is on the capture run's checklist, and mapping the tip to it would draw an unrelated tip as "your context is filling" | a `context_tip` attachment line | residue `attachment/context_tip` — the tip is recorded as itself, unconverted, and reaches no arm |
| `!tokens-reminder` | prose only, plus the vendor's `total_tokens_reminder` ATTACHMENT — the ONE token-budget carrier any real capture holds (`artifact-publish-and-list`, once): a bare `text` field spelling `<total_tokens>N tokens left</total_tokens>` and nothing else. IT IS NOT the context-budget warning either — no capture carries a `context_budget_warning` record of any spelling, so that producer stays ungrounded rather than guessed | a `total_tokens_reminder` attachment line | residue `attachment/total_tokens_reminder` — recorded as itself, unconverted, and reaching no arm |
| `!compact` | a compaction: `status{compacting}`, a `compact_boundary` carrying the full corpus `compact_metadata` (trigger, pre/post tokens, duration, the preserved segment AND the preserved-messages uuid list), then `status{compact_result:"success"}` | a `system:compact_boundary` line whose `logicalParentUuid` names the preserved head, plus a summary user line | SessionCompacting + AgentUpdate.context_cut(ContextCompacted) with trigger=requested |
| `!compact-auto` | an AUTOMATIC compaction — the same shapes with `trigger: "auto"`, which is the only discriminator | a `system:compact_boundary` line with `compactMetadata.trigger: "auto"` | AgentUpdate.context_cut(ContextCompacted) with trigger=automatic |
| `!compact-failed` | a compaction that FAILS: `status{compacting}` then `status{compact_result:"failed", compact_error}` and NO boundary | nothing but the prompt line and the turn record — a failed compaction cut nothing | AgentUpdate.context_cut(ContextCompactionFailed) |
| `!away-summary` | prose only; the vendor's recap is a `system:away_summary` transcript record | a `system:away_summary` line | vendor_specific residue — `system/away_summary`, which no conversation.v1 arm models |
| `!residue` | prose only; it writes the two attachment records BOTH planes agree are vendor bookkeeping, not context | `deferred_tools_delta` and `agent_listing_delta` attachment lines | NONE — these are `StoreUnservedItem.vendor_specific{kind:"attachment/deferred_tools_delta"}` and `attachment/agent_listing_delta`, dropped from every page by both planes |
| `!cold-seed` | an ordinary turn whose TRANSCRIPT RECORDS are stamped TWO HOURS IN THE PAST, so the next resume of this session trips the shim's own cold-context detection | the ASSISTANT line (with its usage) and the turn_duration line, both carrying a two-hour-old `timestamp` | SessionColdLapsed on the NEXT resume — this scenario only seeds the condition |
| `!fail-execution` | a reasoning block and a partial answer, then an error `result` with subtype `error_during_execution` and `terminal_reason: "api_error"` | the assistant lines for the work it did reach, the prompt line and the turn record | AgentThinking + AgentResponse, then AgentFailure.execution_error |
| `!fail-max-turns` | a reasoning block and a partial answer, then an error `result` with subtype `error_max_turns` and `terminal_reason: "max_turns"` | the assistant lines for the work it did reach, the prompt line and the turn record | AgentThinking + AgentResponse, then AgentFailure.max_turns |
| `!fail-budget` | a reasoning block and a partial answer, then an error `result` with subtype `error_max_budget_usd` and `terminal_reason: "budget_exhausted"` | the assistant lines for the work it did reach, the prompt line and the turn record | AgentThinking + AgentResponse, then AgentFailure.budget_exhausted |
| `!fail-structured-output` | a reasoning block and a partial answer, then an error `result` with subtype `error_max_structured_output_retries` and `terminal_reason: "structured_output_retry_exhausted"` | the assistant lines for the work it did reach, the prompt line and the turn record | AgentThinking + AgentResponse, then AgentFailure.structured_output_retry_exhausted |
| `!fail-blocking-limit` | a reasoning block and a partial answer, then an error `result` with subtype `error_during_execution` and `terminal_reason: "blocking_limit"` | the assistant lines for the work it did reach, the prompt line and the turn record | AgentThinking + AgentResponse, then AgentFailure.blocking_limit |
| `!fail-rapid-refill` | a reasoning block and a partial answer, then an error `result` with subtype `error_during_execution` and `terminal_reason: "rapid_refill_breaker"` | the assistant lines for the work it did reach, the prompt line and the turn record | AgentThinking + AgentResponse, then AgentFailure.rapid_refill_breaker |
| `!fail-prompt-too-long` | a reasoning block and a partial answer, then an error `result` with subtype `error_during_execution` and `terminal_reason: "prompt_too_long"` | the assistant lines for the work it did reach, the prompt line and the turn record | AgentThinking + AgentResponse, then AgentFailure.prompt_too_long |
| `!fail-image` | a reasoning block and a partial answer, then an error `result` with subtype `error_during_execution` and `terminal_reason: "image_error"` | the assistant lines for the work it did reach, the prompt line and the turn record | AgentThinking + AgentResponse, then AgentFailure.image_error |
| `!fail-model` | a reasoning block and a partial answer, then an error `result` with subtype `error_during_execution` and `terminal_reason: "model_error"` | the assistant lines for the work it did reach, the prompt line and the turn record | AgentThinking + AgentResponse, then AgentFailure.model_error |
| `!fail-malformed-tool-use` | a reasoning block and a partial answer, then an error `result` with subtype `error_during_execution` and `terminal_reason: "malformed_tool_use_exhausted"` | the assistant lines for the work it did reach, the prompt line and the turn record | AgentThinking + AgentResponse, then AgentFailure.malformed_tool_use_exhausted |
| `!fail-tool-deferred` | a reasoning block and a partial answer, then an error `result` with subtype `error_during_execution` and `terminal_reason: "tool_deferred"` | the assistant lines for the work it did reach, the prompt line and the turn record | AgentThinking + AgentResponse, then AgentFailure.tool_deferred |
| `!fail-tool-deferred-unavailable` | a reasoning block and a partial answer, then an error `result` with subtype `error_during_execution` and `terminal_reason: "tool_deferred_unavailable"` | the assistant lines for the work it did reach, the prompt line and the turn record | AgentThinking + AgentResponse, then AgentFailure.tool_deferred_unavailable |
| `!fail-turn-setup` | a reasoning block and a partial answer, then an error `result` with subtype `error_during_execution` and `terminal_reason: "turn_setup_failed"` | the assistant lines for the work it did reach, the prompt line and the turn record | AgentThinking + AgentResponse, then AgentFailure.turn_setup_failed |
| `!fail-aborted-tools` | a reasoning block and a partial answer, then an error `result` with subtype `error_during_execution` and `terminal_reason: "aborted_tools"` | the assistant lines for the work it did reach, the prompt line and the turn record | AgentThinking + AgentResponse, then AgentInterrupted.by_user, reached through the tools rather than the stream |
| `!fail-stop-hook` | a reasoning block and a partial answer, then an error `result` with subtype `error_during_execution` and `terminal_reason: "stop_hook_prevented"` | the assistant lines for the work it did reach, the prompt line and the turn record | AgentThinking + AgentResponse, then AgentFailure.stop_hook_prevented |
| `!fail-hook-stopped` | a reasoning block and a partial answer, then an error `result` with subtype `error_during_execution` and `terminal_reason: "hook_stopped"` | the assistant lines for the work it did reach, the prompt line and the turn record | AgentThinking + AgentResponse, then AgentFailure.hook_stopped |
| `!fail-continuation-prevented` | an `informational` message with `prevent_continuation: true`, a `stop_hook_summary` record whose `preventedContinuation` is true, then a `stop_hook_prevented` terminal | the `system:stop_hook_summary` line, the prompt line and the turn record | AgentFailure.continuation_prevented — UNSETTLED: no `TerminalReason` names it, so this pairs the two declared prevent-continuation signals with the nearest terminal |
| `!api-429` | a `system:api_error` record and an `api_retry` message, then an `error_during_execution` result with `terminal_reason: "api_error"` and status 429 | a `system:api_error` line, the prompt line and the turn record | AgentUpdate.api_error mid-turn, then AgentFailure.api_request_failed → ApiRateLimited (with the retry-after) |
| `!api-529` | a `system:api_error` record and an `api_retry` message, then an `error_during_execution` result with `terminal_reason: "api_error"` and status 529 | a `system:api_error` line, the prompt line and the turn record | AgentUpdate.api_error mid-turn, then AgentFailure.api_request_failed → ApiOverloaded |
| `!api-401` | a `system:api_error` record, then an `error_during_execution` result with `terminal_reason: "api_error"` and status 401 | a `system:api_error` line, the prompt line and the turn record | AgentUpdate.api_error mid-turn, then AgentFailure.api_request_failed → ApiAuthenticationFailed |
| `!api-403` | a `system:api_error` record, then an `error_during_execution` result with `terminal_reason: "api_error"` and status 403 | a `system:api_error` line, the prompt line and the turn record | AgentUpdate.api_error mid-turn, then AgentFailure.api_request_failed → ApiPermissionDenied |
| `!api-400` | a `system:api_error` record, then an `error_during_execution` result with `terminal_reason: "api_error"` and status 400 | a `system:api_error` line, the prompt line and the turn record | AgentUpdate.api_error mid-turn, then AgentFailure.api_request_failed → ApiInvalidRequest |
| `!api-413` | a `system:api_error` record, then an `error_during_execution` result with `terminal_reason: "api_error"` and status 413 | a `system:api_error` line, the prompt line and the turn record | AgentUpdate.api_error mid-turn, then AgentFailure.api_request_failed → ApiRequestTooLarge |
| `!api-404` | a `system:api_error` record, then an `error_during_execution` result with `terminal_reason: "api_error"` and status 404 | a `system:api_error` line, the prompt line and the turn record | AgentUpdate.api_error mid-turn, then AgentFailure.api_request_failed → ApiNotFound |
| `!api-500` | a `system:api_error` record and an `api_retry` message, then an `error_during_execution` result with `terminal_reason: "api_error"` and status 500 | a `system:api_error` line, the prompt line and the turn record | AgentUpdate.api_error mid-turn, then AgentFailure.api_request_failed → ApiInternal |
| `!api-billing` | a `system:api_error` record, then an `error_during_execution` result with `terminal_reason: "api_error"` and status 402 | a `system:api_error` line, the prompt line and the turn record | AgentUpdate.api_error mid-turn, then AgentFailure.api_request_failed → ApiBillingError |
| `!api-oauth-org` | a `system:api_error` record, then an `error_during_execution` result with `terminal_reason: "api_error"` and status 403 | a `system:api_error` line, the prompt line and the turn record | AgentUpdate.api_error mid-turn, then AgentFailure.api_request_failed → ApiOauthOrgNotAllowed |
| `!api-max-output` | a `system:api_error` record, then an `error_during_execution` result with `terminal_reason: "api_error"` and status null | a `system:api_error` line, the prompt line and the turn record | AgentUpdate.api_error mid-turn, then AgentFailure.api_request_failed → ApiMaxOutputTokens |
| `!api-unmodeled` | a `system:api_error` record, then an `error_during_execution` result with `terminal_reason: "api_error"` and status 418 | a `system:api_error` line, the prompt line and the turn record | AgentUpdate.api_error mid-turn, then AgentFailure.api_request_failed → ApiUnmodeledError |
| `!max-tokens` | an assistant message truncated at the output-token ceiling: `stop_reason: "max_tokens"` on both the message and the result, with the partial text kept | the truncated assistant line, the prompt line and the turn record | AgentResponseFailure.reason=max_tokens — the text is kept, the answer is incomplete |
| `!refusal-fallback` | a refusal that FELL BACK to another model: a `fallback` content block naming both models, a `model_refusal_fallback` message carrying the category and the retracted uuids, and the answer from the fallback leg | the fallback assistant line, a `system:model_refusal_fallback` line, the answer line, the turn record | AgentResponseFailure.reason=refused, then a fresh AgentResponse from the fallback model |
| `!refusal-no-fallback` | a refusal with NO fallback configured: a `model_refusal_no_fallback` message whose `content` is empty and whose explanation points the integrator at the fallback docs, then an error terminal | a `system:model_refusal_no_fallback` line, the prompt line and the turn record | AgentResponseFailure.reason=refused with no recovery |
| `!context-window` | a `prompt_too_long` terminal preceded by the vendor's informational notice naming the window | the prompt line and the turn record | AgentResponseFailure.reason=context_window_exceeded and AgentFailure.prompt_too_long |
| `!fault-converter` | ONE MALFORMED VENDOR MESSAGE and then an ordinary turn: a `hook_started` whose `hook_id` is the EMPTY STRING — an identity the converter requires and refuses to invent — followed by prose and a success result. The malformed message produces NO frame at all: the fold refuses it, logs a converter defect and records it as residue, which is the contract's answer to a record missing a required field | the assistant line, the prompt line and the turn record; the malformed message is stream-only | SessionFault.converter_defect with an OPEN SessionDegradedWindow — the diagnostics arm, reached without any rpc failing. The malformed message itself reaches NO conversation.v1 arm, which is the point |
| `!fault-recover` | the SAME hook announcement, WELL-FORMED: a `hook_started`/`hook_response` pair carrying a real `hook_id`, so the fold converts it and the frame the malformed turn could not produce appears. Nothing else changes — the recovery is that an ordinary turn converted cleanly | the assistant line, the prompt line and the turn record | AgentHook.result=succeeded, and the diagnostics returning to HEALTHY with the degraded window CLOSED carrying the dropped count the fault left behind |
| `(any prompt containing `e2e-fail-this-turn`)` | an `error_during_execution` result and no assistant content — the daemon's merge-pipeline failure gate | the prompt line and the turn record | AgentFailure.execution_error |
| `!hold` | an assistant message frame and then NOTHING: the turn stays in flight until an interrupt lands, and ends the way an interrupted turn does — no content, an error result | the opening assistant line, the prompt line and (at the interrupt) the turn record | AgentInterrupted.by_user, reached without any permission question |
| `!interrupt` | a tool call the interrupt lands INSIDE: the assistant message is marked `aborted`, the tool result reports `interrupted: true`, and the turn ends `error_during_execution` / `aborted_streaming` | the aborted assistant line, the interrupted tool_result line, the prompt line and the turn record | AgentBashInterrupted.cause=by_user and AgentInterrupted.by_user |
| `!query-eof` | NOTHING, and then the iterable ENDS — the turn never terminates. The CLI going away cleanly mid-turn | the prompt line only; there is no turn record because there was no turn end | SessionQueryDied.cause=unexpected_eof |
| `!query-fail` | NOTHING, and then the iterable REJECTS — the producer died rather than finished | the prompt line only | SessionQueryDied.cause=iterator_failure |
| `!keepalive` | an ordinary short turn. It exists so a test can drive a keep-alive-shaped turn deterministically; the `<!--agent-repl:keepalive-->` marker is the SHIM's, and the mock never adds or removes it | the assistant line, the prompt line (marker and all) and the turn record | AgentResponse.from_model, AgentSuccess.completed — classified keep-alive by the marker on the PROMPT |

### What the mock writes, and where

```
$CLAUDE_CONFIG_DIR/projects/<cwd-slug>/<vendor-session-id>.jsonl
$CLAUDE_CONFIG_DIR/projects/<cwd-slug>/<vendor-session-id>/subagents/agent-<agent-id>.jsonl
$CLAUDE_CONFIG_DIR/projects/<cwd-slug>/<vendor-session-id>/subagents/agent-<agent-id>.meta.json
<spool-root>/<cwd-slug>/<vendor-session-id>/tasks/<b|a><hex>.output
```

`<cwd-slug>` replaces EVERY byte of the absolute cwd that is not `[A-Za-z0-9]`
with `-` (underscore included; case preserved). `agent-<id>.meta.json` carries
exactly four camelCase fields — `agentType`, `description`, `toolUseId`,
`spawnDepth`. An assistant message with several blocks is written as ONE LINE
PER BLOCK sharing `message.id`, in block order, which is what makes
`<message.id>:<block_index>` address a block. Shell spools are written
incrementally and terminated by `EXIT=<code>`; agent spools are the agent's own
JSONL and carry no terminator.

## No real SDK calls from tests

`src/vendor-guard.ts` is the ONLY place that may dynamically import
`@anthropic-ai/claude-agent-sdk`; every call site goes through `importRealSDK`.
When `AGENT_REPL_FORBID_VENDOR_CALLS` is set to any non-empty value the guard
throws and the shim exits nonzero — never a silent no-op, never a fake fallback.
`test/setup.ts` sets it for the whole vitest suite, so a test needing offline
behavior must pass `--fake`. Production must never set it.

`test/vendor-guard.test.ts` enforces the chokepoint STRUCTURALLY: it walks
`src/` and fails if any file other than the guard contains a dynamic import of
the SDK. `src/sdk/types.ts` imports SDK types with `import type`, which is erased
at build time and is not a vendor import site.

## Logging

- `src/log.ts` is the one canonical JSON logging API, split between normal and
  verbose emission. New or changed shim code uses that API only.
- The durable sink is the **inherited fd 3**, never a pipe to the daemon's
  stderr: a shim must survive its daemon's death without dying on its own log
  line (EPIPE incident, 2026-08-10). A poisoned sink is surfaced, never
  silently swallowed; the stderr mirror is a convenience that retires itself
  once, durably recorded.
- Every record carries the shim `pid`, the workspace dir and its id, and every
  known agent-repl and Claude session identifier. Before a session exists the
  `agent_repl_session_id` is the process's own `shim-<workspace-key>-<pid>`,
  which correlates a log line with its lock file.
- **Every logical branch logs** — warnings at `warn`, errors at `error`. This
  instrumentation exists for the remediation loop: the integration suite is run
  to read the production code's logs.
- Each error is logged exactly once by its owning layer, with session, store
  key, socket, request, operation, resolved inputs, branch outcome and cause.
- Frequent or hot diagnostics use the verbose helper. Direct `console`,
  `process.stderr` or ad hoc logger aliases are forbidden except the documented
  pre-logger bootstrap failure and logger-sink emergency paths.
- The full contract is `modules/app/agent-repl/logging-contract.md`.

### Standing streams

Two rules exist because a Go client cannot tell a QUIET stream from a REFUSED
one: connect-go surfaces a server-stream refusal only at the first `Receive`,
so a stream that accepted and has not spoken yet blocks the caller exactly the
way a refusal does.

1. **The response head is flushed ON ACCEPT.** connect-node writes a stream's
   head lazily — for a stream that has pushed nothing it fires only when the
   stream ENDS — so `service/server.ts` writes it first: a streaming request
   content type (`application/connect+proto|json`,
   `application/grpc-web+proto|json`, `application/grpc+proto`,
   `application/grpc`) gets `200` with its own content type echoed back,
   `flushHeaders()`, and then `writeHead` rebound to a no-op so the adapter's
   later call cannot raise `ERR_HTTP_HEADERS_SENT`. It is applied on BOTH
   dialects (the HTTP/1.1 server and the h2 server behind the preface sniffer).
   Unary requests are untouched — they have a real status to report. A refused
   streaming verb still reports its refusal, because the Connect protocol
   carries a stream's error in its END-OF-STREAM frame, not in the head.
2. **`WatchSession` pushes `diagnostics` immediately, on every open.** The
   frame is seeded into the subscriber's queue synchronously by
   `SessionPushes.subscribe()`, before the iterable is returned, so the first
   `next()` resolves without waiting on anything. That push IS the daemon's
   readiness signal, and there is no other. A late joiner is then caught up on
   the current `context_usage`, `model_changed` and `permission_mode_changed`;
   after that, arms are pushed on CHANGE only.
3. **The store client ends `WatchAgentSession` by CANCELLING its context**
   (an `AbortSignal`), never by a bare close: a bare close leaves the store
   holding a reading session nobody will ever pull, and the store has no other
   signal that the reader is gone.

## Validation and errors

- **One base validate function per request message** (`service/validate/
  requests.ts`), **one per non-primitive field** (`service/validate/fields.ts`).
  An unset non-optional field or oneof is answered `InvalidArgument`
  IMMEDIATELY; refusals name the field path from the request root.
- **One constructor per failure message and arm** (`service/failures.ts`), so a
  refusal's `kind`/`cause` oneof can never be left unset.
- Three refusal channels: a TYPED failure in the response's own oneof (a fact
  about the session), a CONNECT ERROR (`InvalidArgument` for a malformed
  request, `NotFound` for a refused stream open, `Unimplemented` for the
  workflow trio), and a SESSION FAULT on `WatchSession` (the shim reporting its
  own degradation).
- **Workflow is kicked** (ruled 2026-08-29): `GetWorkflow`, `WatchWorkflow` and
  `StopWorkflow` answer `Code.Unimplemented` and have no `Engine` method.

## Verification

```bash
npm run typecheck     # tsc over src/, test/, scripts/ and the generated stubs
npm test              # vitest
npm run coverage      # vitest with v8 coverage over authored src/**/*.ts
npm run build         # esbuild -> dist/main.js (the entry the daemon spawns)
npm run smoke         # spawn and dial dist/main.js for real (needs a build first)
```

- `AGENT_REPL_FORBID_VENDOR_CALLS=1` in every shell you run tests in.
- `modules/app/agent-repl/bin/test-all.sh` (from the repository root) runs every
  tracked suite across the module.
- Maintain at least 90% statement coverage. Never reduce the measured baseline,
  and add focused tests for every critical branch and every error path changed.
- `modules/app/agent-repl/bin/report-logging-density.sh shim` is a rough review
  aid, not semantic coverage: audit critical branches and errors directly even
  when the ratio rises.
