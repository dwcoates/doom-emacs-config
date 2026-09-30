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
  locks.ts             the two kernel flocks (both taken inside StartSession,
                       each held by a spawned agent-shim/shim-lock child)
  vendor-guard.ts      the ONLY dynamic import of the SDK; the FORBID_VENDOR_CALLS gate
  metaprompt.ts        the canonical metaprompt append
  trust.ts             the vendor's folder-trust entry, granted before any spawn
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
    persistence.ts     THE seam the engine writes and reads through; retry + batch policies
    writer.ts          the one ordered store writer: never drops, bounded batches, backpressure
    keys.ts            upsert_key + write_id (THE one place)
    retry.ts           the READ half's retry schedule (the writer's own policy)
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
- **THE UNSTATED PERMISSION MODE IS `auto`, NOT THE VENDOR'S `default`** (owner
  ruling 2026-09-14: "the default permission mode should be auto for the
  SDK/shim"). `DEFAULT_PERMISSION_MODE` in `src/engine/permission-gate.ts` is
  the one place it is written; the session's `permissionMode` starts there, so
  a `StartSession{fresh}` that names no mode and a resume whose transcript
  states none both reach `createQuery` as `permissionMode: "auto"`. NOTHING IS
  DROPPED BY THE CHOICE: `auto` keeps the gate, with a classifier deciding each
  ask instead of the user, which is why it is not in the daemon's
  `UngatedPermissionModes` and needs no creation consent. `default` stays a
  mode the vendor can REPORT and both conversion tables carry it in full — the
  shim simply never PICKS it for anyone. Every mode change is still one pushed
  `permission_mode_changed` update, logged as before.
- **Environment**, all refusals rather than defaults:
  - `CLAUDE_CONFIG_DIR` (required) — which ACCOUNT the session runs as.
  - `AGENT_REPL_OWNED=1` (required) — the daemon's mark; a shim refuses to run
    unowned.
  - `SHIM_BUILD_SHA` (required) — the content hash (lowercase hex SHA-256) of
    the `dist/main.js` bundle the daemon spawned this process from, stated by
    the daemon at spawn time. Reported on `SessionStarted` and on every
    `SessionDiagnostics` frame (including a session-less shim's opening
    WatchSession frame); the daemon's deploy compares it against a freshly
    built bundle's own hash and bounces a shim whose reported hash differs.
  - `AGENT_REPL_STATE_DIR` (default `~/.claude-emacs`) — the one state root.
  - `AGENT_REPL_STORE_SOCKET` — the store socket when `--store-socket` is
    absent. **The flag beats the env.**
  - `AGENT_REPL_LOCK_DIR` (default `~/.cache/agent-repl/run`) — the kernel-lock
    directory. Both the shim and the daemon's probe read it, which is what makes
    relocation safe.
  - `AGENT_REPL_SHIM_LOCK_BIN` (default `~/.cache/agent-repl/bin/shim-lock`) —
    the lock-holder binary. Node cannot take a `flock`, so each kernel claim is
    a `shim-lock` CHILD PROCESS this shim keeps the stdin pipe of; see
    `agent-shim/shim-lock/AGENTS.md`. Every suite that spawns a real shim
    overrides it at the binary that suite built.
  - `AGENT_REPL_FORBID_VENDOR_CALLS` — the guard (see below).
  - `AGENT_REPL_FAKE_TURN_GATE`, `AGENT_REPL_FAKE_TURN_GATE_TEXT`,
    `AGENT_REPL_FAKE_SPOOL_ROOT`, `AGENT_REPL_FAKE_REFUSE`,
    `AGENT_REPL_FAKE_INIT_TIMING` — `--fake` only.
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
- **A claim is a child process, not an fd.** `locks.ts` spawns
  `shim-lock <path>`, waits for its `locked` line, and releases by closing its
  stdin; the holder takes the real `flock(2)` the daemon probes. The
  predecessor used `open(2)`'s `O_EXLOCK`, which is macOS/BSD only and made the
  shim refuse every session on Linux. `shim-lock` exit 3 is the distinct
  "another process holds it" answer (`LockHeldError`), the ONLY path to
  `conversation_owned`.
- **Every other holder failure is `lock_holder_unavailable`, saying how.**
  `LockHolderUnavailableError` carries `conversation.v1.LockHolderFailure`'s
  `how`: `spawnFailed` (os error), `exited` (code, stderr), `signaled`
  (signal, stderr), `misanswered` (line) or `silent` (the bound), recorded
  once at ERROR by `holderFailed`. Nobody owns the conversation then; the
  daemon relays it as `OpenWorkspaceError.lock_holder_unavailable`. Anything
  else a claim throws is raised from `StartSession`, never answered as an
  owner.
- **`StartSession` ALWAYS ANSWERS.** The verb is unsettled from the moment the
  query is created, and SIX things settle it: the PROVEN-LIVE SIGNAL (below);
  `init`, for a vendor that still announces one first; a hook that comes back
  BLOCKING before either (the only hooks that can fire that early are the
  vendor's `SessionStart` ones, and a blocked one gets no further answer, so its
  blocking text IS the start's failure reason); a `result` carrying `is_error`
  (the vendor refusing the opening, whose own text is the reason); the QUERY
  ENDING before either (an exited child, an ended stream, a throwing iterator);
  or `INIT_TIMEOUT_MS`, the shim's own last-resort bound, sized UNDER the
  daemon's bring-up bound so the shim — which knows why — answers before the
  daemon, which does not. The first two are successes; the other four end as
  `StartSession{vendor_start_failed}` with the reason in `detail`.
- **A START SETTLES ON A PROVEN-LIVE SIGNAL, NOT ON `init`.**
  **THE VENDOR ANNOUNCES `system:init` ONLY ONCE A FIRST TURN REACHES IT** —
  grounded 2026-09-13 against claude 2.1.220 AND 2.1.270, driven exactly as this
  shim drives them (`--input-format stream-json`): the child answers control
  requests (`supportedModels`, `supportedCommands`, `setPermissionMode`) within
  300ms and emits no `init` at all until an input message arrives, while feeding
  one turn produces `init` in ~600ms in the same directory. Reproduced with the
  full option set and with a bare one, fresh and resume, in three directories and
  three account roots (a pristine one with no plugins or hooks included). A start
  that waited for `init` before prompting therefore deadlocked on every real
  session and failed at the bound.
  The signal is ONE CONTROL ROUND-TRIP answered inside `LIVE_SIGNAL_TIMEOUT_MS`
  (3s, ten times the observed ~300ms, stated at the constant). A child that
  answers one is spawned, connected and taking work, which is exactly what
  `endpoint_start_session.proto` says this verb resolves on: a prompt can be
  ACCEPTED. `supportedModels()` is the call, because the opening owes it anyway —
  its answer IS `SessionStarted.model_catalog`, so the proof costs nothing extra
  and the catalog is in hand before the opening returns. A round-trip that is
  REFUSED or that misses its bound refuses the start, naming the call and the
  bound; once the start has settled, the same failure is the catalog component's
  own `SessionFault`, which a later re-read lifts.
- **THE INIT FACTS ARE LEARNED WHEN INIT ARRIVES, WITH THE FIRST TURN.** Session
  id rotation, model, permission mode, agent binary version and the fast-mode
  facts are applied to the live session then, each pushed as the `SessionUpdate`
  it already maps to (`identity_rotated`, `model_changed`,
  `permission_mode_changed`, `fast_mode`) — and a fact init merely RESTATES
  pushes nothing. Until then the fixed schema's absence is what a surface draws:
  `effective_model` is unstated on a fresh start that named no model, and the
  topbar's cells show their no-session dashes. `SessionRuntime` is still whole at
  the opening because `agent_binary_version` also comes from the SDK's bundled
  manifest; init's `claude_code_version` OVERWRITES it, since the running binary
  outranks the packaged declaration. It has no `SessionUpdate` arm and rides only
  `SessionStarted`, so a late correction is recorded and logged, not pushed.
  **THE LAST-RESORT BOUND IS FOR SILENCE ONLY**, and after the live signal it is
  silence of a kind the round-trip's own 3s bound already catches — it survives
  for the one case with no bound of its own: a child that answers neither its
  control channel nor its stream. Every conclusive answer settles the start
  at once; waiting the bound out on an answer already in hand is what lets the
  daemon's bound fire first and blame the shim. The vendor child's stderr is
  captured (`Options.stderr`) and appended to the refusal's `detail`, because a
  CLI that will not honour a resume prints its reason there and nowhere else.
  A failed start also CLOSES the query it opened: two attempts on one
  conversation are two writers on one transcript, and a released query's loop
  ending is not the live session dying.
  Blocking-versus-merely-failing has ONE reading, `convert/hooks.ts`
  `hookBlockingText`, shared by the gate and the drawn hook row so they cannot
  drift.
- **Signals**: SIGTERM is the one authorized shutdown and takes the
  `KillSession{force:true}` path, then exits 0 (nonzero if the stand-down
  failed). SIGINT is REFUSED and logged at error — an attached terminal's Ctrl-C
  must not end a live turn.
- **THE WORKSPACE IS TRUSTED BEFORE THE VENDOR IS CONSTRUCTED.** The vendor
  keeps a per-directory trust decision in `<config_root>/.claude.json` under
  `projects.<dir>.hasTrustDialogAccepted`, and a directory it was never told to
  trust is run with the workspace's `permissions.allow` entries DROPPED — said
  once on the child's stderr and nowhere else. Neither the SDK (`Options` has no
  such field) nor the CLI (no `--trust` flag) offers a supported switch, so the
  key the vendor's own warning prescribes is what `trust.ts` writes:
  read-modify-write of the whole file, its own indentation and mode kept, one
  key touched, replaced by rename; a file that will not parse is left standing
  and the failure raised. **The key is the REPOSITORY, not the worktree** —
  grounded against claude 2.1.220: trusting a linked worktree's own path leaves
  the warning standing, trusting its main repository silences it — and the main
  repository is read out of the worktree's `.git` FILE, never by running git.
  It happens in `queryFactory`, the one place any query is constructed, mocked
  vendor included, so no spawn path can forget it.
- **`--version`** prints `claude-shim <version>` and exits before any socket,
  lock, log fd or SDK import. It is a dependency-free smoke of the bundle.

## Mock levers that are NOT prompts

Some shim.v1 refusals are the shim relaying a vendor that said no to a CONTROL
CALL, so no prompt can reach them. `AGENT_REPL_FAKE_REFUSE` names those verbs,
comma-separated:

| Value | What the mocked vendor does | The arm it makes reachable |
| --- | --- | --- |
| `start` | `createFakeQuery` throws before any message, every time | `StartSession{vendor_start_failed}` |
| `start-once` | the FIRST start under the account root throws; every later one succeeds. The one refusal is marked by a FILE in the config root, not a process counter, because the daemon STOPS the shim of a failed start and the retry therefore reaches a NEW process | a retry after `vendor_start_failed` succeeding — on the same warm shim, or on the replacement the daemon spawns |
| `start-eof` | the query is created and its stream ENDS with no `init` at all | `StartSession{vendor_start_failed}` settled by the query's end, not by the init bound |
| `start-error-result` | the query answers the opening with an error `result` and ends | `StartSession{vendor_start_failed}` carrying the vendor's own refusal text |
| `set_model` | `setModel()` rejects | `SetSessionModel{vendor_refused}` |
| `set_permission_mode` | `setPermissionMode()` rejects | `SetSessionPermissionMode{vendor_refused}` |

An unrecognized verb is a refusal to start, never a silently ignored knob.

`AGENT_REPL_FAKE_INIT_TIMING` is the other whole-process lever, and it is not a
refusal at all: it says WHEN the mocked vendor announces its `system:init`.

| Value | What the mocked vendor does | What it reaches |
| --- | --- | --- |
| `at-start` (default) | init leads, and the rest of the session follows it | the OLDER vendor's shape, and the shape every captured corpus was recorded under — an init that settles a start before the live signal |
| `after-first-turn` | no init until a first user message arrives; control requests are answered throughout | the REAL vendor's shape (claude 2.1.220 and 2.1.270, grounded 2026-09-13) — a start settling on the live signal alone, and the init facts landing on an already-started session |

An unrecognized value is a refusal to start, exactly as an unrecognized refuse
verb is. The default is `at-start` deliberately: the corpus and every scenario
were captured with init leading, so flipping the default would re-record the
suite rather than test the contract.

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
appears; `AGENT_REPL_FAKE_DETACH_GATE` parks `!bash-detach`'s detached work,
after its first spool line, until the named path appears — no `_TEXT`
companion, since the gate applies to that one scenario rather than to a
matched prompt; `AGENT_REPL_FAKE_SPOOL_ROOT` roots the spool tree.

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
- `!send-message-resumed` / `!send-message-refused` / `!subagent-resumed` — no
  capture addresses a subagent; `!subagent-resumed`'s resume `task_started`
  from the send is the shape a live session's log showed (2026-09-27);
- `!subagent-failed` — no capture has a failed subagent;
- `!subagent-network-failed` / `!network-resume` — the failure's records are
  the 2026-09-27 outage's own (production records, not a capture), and the
  resume answer is the declared `!send-message-resumed` shape per agent;
- `!subagent-interleaved` — no capture streams a subagent's response INTO an
  open main block; the shape is the one a live session's logs showed, where a
  background subagent's `message_start` landed between two deltas of the main
  agent's open block;
- `!subagent-detached-hold` — no capture interrupts a turn beside a live
  background agent under the per-task stop declaration;
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
| `!ide-diagnostics-write` | a `Write` that CREATES a file, then the vendor's `diagnostics` attachment reporting a typescript error in the written file | the tool_use line, the tool_result line, a `diagnostics` attachment line, the closing text line | AgentWrite.diagnostics (AgentDiagnosticsReport joined to the last WRITE by adjacency) |
| `!grep-content` | a `Grep` in content mode answered with matching lines and a total that exceeds them | the tool_use line, the tool_result line, the closing text line | AgentGrep.start + AgentGrepSuccess.matches=content (extent=partial) |
| `!grep-files` | a `Grep` in files_with_matches mode answered with paths only | the tool_use line, the tool_result line, the closing text line | AgentGrepSuccess.matches=files (extent=all) |
| `!grep-count` | a `Grep` in count mode answered with per-file counts | the tool_use line, the tool_result line, the closing text line | AgentGrepSuccess.matches=count |
| `!glob` | a `Glob` answered with a truncated path list and a total larger than the list | the tool_use line, the tool_result line, the closing text line | AgentGlob.start + AgentGlobSuccess.extent=partial with an omitted count |
| `!bash [command]` | a foreground `Bash` tool_use, its FOREGROUND task (`task_started` with `is_backgrounded: false`, the 0.3.280 shape), then its result and the task's completed `task_notification` — no output in between, because foreground output is unobservable while running | the tool_use line, the tool_result line with a `BashOutput`-shaped `toolUseResult`, the closing text line | AgentBash.start + AgentBashSuccess.outcome=completed how=exited(0); the foreground task is NEVER detached work — no detachment, no live-set entry |
| `!bash-hold` | a FOREGROUND `Bash` that never returns: the tool_use lands and the turn parks until an interrupt, so the unit stays live and foreground for as long as a caller needs it to | the tool_use line, the prompt line and (at the interrupt) the turn record | no terminal at all while it holds — the lever for DetachForeground's `unsupported` refusal, which needs a GENUINELY LIVE foreground unit to refuse (`!bash` settles before the call can be made, so it answered `already_concluded` instead and the refusal under test was never reached). AT THE STOP the unit settles AgentBashInterrupted.cause=by_user, minted by the converter's own `cut` rather than by any vendor result: the vendor returns none for a call a stop landed inside, and a unit left on its running arm draws a live shell inside a turn that ended |
| `!bash-fail` | a foreground `Bash` whose result is an ERROR carrying stderr and a non-zero interpretation | the tool_use line, the error tool_result line, the closing text line | AgentBashSuccess.outcome=completed how=exited(non-zero) — a non-zero exit is a completed run, not a failure |
| `!bash-timeout` | a foreground `Bash` that hits its timeout: `task_started`, then a result carrying `timedOutAfterMs` and `backgroundTaskId` — the vendor auto-backgrounds rather than killing | the tool_use line, the tool_result line, an incremental spool with NO `EXIT=` line, the closing text line | AgentBashInterrupted.cause=timed_out; the run stays live as detached work |
| `!bash-spill` | a foreground `Bash` whose output was too large for the message and spilled to a file on disk | the tool_use line, the tool_result line carrying `persistedOutputPath`/`persistedOutputSize`, the closing text line | AgentBashOutputPartial — the partial extent with the omitted byte count |
| `!bash-image` | a foreground `Bash` whose stdout IS image data (`isImage: true`), answered with an image content block | the tool_use line, the image tool_result line, the closing text line | AgentBashOutput.form=image |
| `!bash-detach` | a `Bash` with `run_in_background`, `task_started`, `background_tasks_changed`, a result carrying only `backgroundTaskId`, then — after the turn — `task_updated` and a completed `task_notification`. When `AGENT_REPL_FAKE_DETACH_GATE` names a path, the run PARKS after its first spool line until that path exists, so a test can observe the turn concluded and the detached work still going | the tool_use and tool_result lines, and `<spool-root>/<slug>/<session>/tasks/b<hex>.output` written INCREMENTALLY (the first line before any detach gate, the rest after it) and terminated by `EXIT=0` | AgentBash detached_work + the AgentBashTail snapshot fed by the sidecar tailing the spool |
| `!bash-detach-poll [command]` | a `Bash` with `run_in_background`, then — in the SAME turn — explicit `TaskOutput` poll tool_use/tool_result pairs: two reporting RUNNING with growing output, then one reporting a terminal exit code and status. UNGROUNDED, INVENTED: no capture ever calls `TaskOutput`, only lists it in `init.tools` | the tool_use/tool_result lines for the background and for each poll, and the spool terminated by `EXIT=0` | AgentBash detached_work; the polls themselves reach no converter arm — `TaskOutput` is in `EXEMPT_TOOLS`, so each poll is dropped SILENTLY: no unit, no `AgentUnmodeled`, no unmodeled warning |
| `!bash-detach-fail` | a detached `Bash` that ends non-zero: `task_updated{status:"failed"}` and a failed `task_notification` | the tool_use and tool_result lines, and a spool terminated by `EXIT=3` | AgentBash detached_work terminating in a non-zero exit |
| `!bash-detach-live` | a detached `Bash` that NEVER finishes: no terminal notification, and the task stays in the live set | an unterminated spool with no `EXIT=` line — the corpus's `bash-midoutput.output` shape | AgentBash detached_work still live; what a fan-wide cancel and a StopBash act on |
| `!vendor-backgrounded` | a FOREGROUND `Bash` the vendor detaches mid-flight: the scenario parks, `backgroundTasks(toolUseId)` marks it `is_backgrounded`, and the foreground result then reports `backgroundedByUser: true` | the tool_use line, the tool_result line carrying `backgroundedByUser`, an unterminated spool | AgentBackgrounded — a vendor-backgrounded foreground unit |
| `!web-fetch` | a `WebFetch` answered with the corpus shape: bytes, code, codeText, result, durationMs, url | the tool_use line, the tool_result line, the closing text line | AgentWebFetch.start + AgentWebFetchSuccess |
| `!web-fetch-redirect` | a `WebFetch` answered with a 302 and the vendor's redirect instruction as the result body | the tool_use line, the tool_result line, the closing text line | AgentWebFetchSuccess carrying a non-2xx status |
| `!web-search` | a `WebSearch` answered with BOTH result kinds — a hit list keyed by a server tool_use id, and a bare commentary string | the tool_use line, the tool_result line, the closing text line | AgentWebSearch.start + AgentWebSearchSuccess with entry=link AND entry=note |
| `!skill [skill-name] [args]` | a `Skill` tool_use, its `{success, commandName, allowedTools}` acknowledgement, then the skill DOCUMENT as an `isMeta` user record joined by `sourceToolUseID`. The skill name and args are parameterized by the prompt (first token = name, rest = args), and the document body is derived from the name — a caller can name e.g. `create-or-update-workspace` with args `merge` | the tool_use line, the acknowledgement line, the isMeta document line, the closing text line | AgentSkillUse.start + AgentSkillUseSuccess settled on the document, with the allowances |
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
| `!subagent-interleaved` | a detached `Agent` whose own responses stream INTO the main agent's open blocks: one whole subagent response (its `message_start` included) between the two deltas of the main thinking block, another between the two deltas of the main text block, then the completed `task_notification` | the agent's `.meta.json` and `agent-<id>.jsonl`, its spool as agent JSONL, and the main transcript's lines | AgentThinking + AgentResponse.from_model on the main book, each ONE unit, beside the subagent's own AgentResponse units on its book |
| `!subagent-detached-live` | a detached `Agent` that is left LIVE after the turn ends and then raises its OWN gated call: the `canUseTool` ask carries the subagent's `agentID`. Nothing here ever finishes the agent — only a `stopTask` does, which is what makes a stop targeted at a subagent's AgentId observable | the agent's `.meta.json` and `agent-<id>.jsonl`, its spool, and the main transcript's lines | AgentSubagent detached_work left live, an AgentPermission raised UNDER the subagent, and AgentSubagentFailure.cause=stopped_by_user when the stop lands |
| `!subagent-resumed` | a `SendMessage` that RESUMES an idle background agent — its task id is the prompt's argument, else a minted one: `task_started` naming the agent's task id and the SEND's tool_use_id, the send's `resumedAgentId` result, then a completed `task_notification` | the main transcript's lines | AgentDetachedWork(kind=subagent) detached from the send, naming the agent its spawn created + AgentSendMessage.delivery=resumed_recipient |
| `!subagent-detached-hold` | a detached `Agent` left LIVE, and then the turn that spawned it HOLDS: nothing further until an interrupt lands, which ends the turn the way an interrupted turn ends. Whether the agent survives that interrupt is the vendor's `perTaskStopAffordance` posture, never the scenario's | the agent's `.meta.json` and `agent-<id>.jsonl`, its spool, and the main transcript's lines | AgentSubagent detached_work live UNDER AN OPEN TURN, and AgentInterrupted.by_user for the turn while the agent stays live |
| `!subagent-detached-utterance` | a detached `Agent` left LIVE after the turn ends, whose only post-turn activity is ONE ordinary sidechain assistant text line — a mid-flight utterance with `IsSidechain`/`AgentId`/`SourceToolUseId` set and NO completion. Nothing here ever finishes the agent | the agent's `.meta.json` and `agent-<id>.jsonl` (the utterance lands there too), its spool, and the main transcript's lines | AgentSubagent detached_work left live; the utterance itself proves the router keeps a live subagent's prose OUT of the top-level feed rather than adding a new arm |
| `!subagent-failed` | a detached `Agent` that ends in failure: `task_updated{status:"failed"}` and a failed `task_notification` | the agent's `.meta.json` and transcript, its spool, and the main transcript's lines | AgentSubagentFailure |
| `!subagent-network-failed` | a detached `Agent` the vendor ENDS because the API was unreachable, in the 2026-09-27 incident's shape: after the turn, the agent's own SYNTHETIC error message (`model: "<synthetic>"`, `error: "server_error"`, the vendor's ENOTFOUND notice) and then a failed `task_notification` whose summary carries the same notice and `(error type server_error)` | the agent's `.meta.json` and transcript (the synthetic error record lands there, as it did in the incident's transcript), its spool, and the main transcript's lines | AgentSubagentFailure — and the shim's network resume: the agent waits for the API and is then continued |
| `<!--agent-repl:network-resume-->` | the MAIN agent answering the shim's own network-resume prompt: one `SendMessage` per agent the prompt names, each answered WITH `resumedAgentId` (the vendor resuming that SAME agent from its transcript) after that agent's `task_started` under the `SendMessage` call, and — after the turn — one model-authored message of the resumed agent and its completed `task_notification` | the tool_use and tool_result lines, the closing text line, and each resumed agent's reply in its own transcript | AgentSendMessage.delivery=resumed_recipient per agent, then AgentSubagentSuccess for the SAME agent the outage failed |
| `!cancel-all` | THREE detached items launched in one turn — two agents and a shell — left LIVE. The cancel is the caller's `stopTask` per item; emptying the live set makes the engine write the vendor's `agents_killed` record | both agents' `.meta.json` and transcripts, the shell's spool, and the main transcript's lines | the fan-wide cancel: AgentSubagentFailure.cause=stopped_by_user per item, plus the agents_killed record |
| `!usage-historical` | prose only on the main stream. The historical usage record itself is written ONLY to a NESTED subagent's own transcript file (spawnDepth 2), as a FILE-plane assistant record with NO paired STREAM-plane `message_start` — the historical case that must retain usage without inventing a generation duration. UNGROUNDED, INVENTED: no capture carries a file-plane-only historical usage record with nested-subagent attribution and this sub-field set | the nested subagent's `agent-<id>.meta.json` and `agent-<id>.jsonl` carrying one untimed assistant record, plus the main turn's ordinary lines | ungrounded — see MANIFEST.md; the usage sub-fields (cache_creation split, server_tool_use, service_tier, speed, inference_geo) are the ones a session-usage aggregation would need to attribute to an untimed nested actor |
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
| `!mcp-tool` | an `mcp__echo__echo` call — an MCP server's tool — answered with its text | the tool_use line, the tool_result line, the closing text line | AgentMcpToolCall, an ordinary tool call, its address resolved by lookup against the session's `echo` server |
| `!unmodeled` | a `StructuredOutput` call — an SDK tool NO converter owns and no MCP server serves — answered with an opaque payload | the tool_use line, the tool_result line, the closing text line | AgentUnmodeled |
| `!hook-success` | `hook_started` and `hook_response{outcome:"success"}` around a `Read` | the tool_use line, a `hook_success` attachment line carrying `toolUseID`, the tool_result line | AgentHook.result=succeeded |
| `!hook-blocked` | `hook_started` and `hook_response{outcome:"error"}` around an `Edit` the hook BLOCKS | the tool_use line, a `hook_blocking_error` attachment line, the error tool_result line | AgentHook.result=blocking_error; the TURN still succeeds, because a blocked tool is not a stopped turn |
| `!hook-failed` | a `SessionStart` hook that FAILS without blocking anything: exit 1 on stderr | a `hook_non_blocking_error` attachment line carrying stderr, exitCode, command and durationMs | AgentHook.result=non_blocking_error |
| `!hook-cancelled` | `hook_started` and `hook_response{outcome:"cancelled"}` around an `Edit` | the tool_use line, a `hook_cancelled` attachment line — four fields and nothing else — the tool_result line | AgentHook.result=cancelled |
| `!perm-allow-once` | a gated `Bash`, one `canUseTool` ask, and the run that follows an ALLOW with no `updatedPermissions` | the tool_use line, the tool_result line, the closing text line | AgentPermission.start + AgentPermissionAllowed.scope=once |
| `!perm-allow-standing` | a gated `Bash` whose ask carries `suggestions`; a standing allow comes back with those rules echoed as `updatedPermissions`, which the scenario reports verbatim in its conclusion | the tool_use line, the tool_result line, the closing text line | AgentPermissionAllowed.scope=standing with the AgentPermissionChange rules |
| `!perm-allow-standing-mode` | a gated `Bash` whose ask OFFERS a standing that changes the session's permission mode: the suggestions carry an `addRules` and a `setMode` to `acceptEdits` on the SESSION destination, so a grant echoing the offered standing legitimately moves the session's mode | the tool_use line, the tool_result line, the closing text line | AgentPermissionAllowed.scope=standing whose changes include set_mode — the ONE grounded producer of a mode-changing grant, and the negative for a set_mode the ask never offered |
| `!perm-no-standing` | a gated `Bash` whose ask offers NO `suggestions` at all — the shape the vendor sends when no standing rule could be written for the call. The ask can only ever produce a once-allow | the tool_use line, the tool_result line, the closing text line | AgentPermission.start with offered_standing UNSET — the negative for an unoffered standing grant |
| `!perm-hold` | a gated `Bash` ask, and then a turn that PARKS however the ask resolves: the scenario never concludes on its own, so the only terminal it can reach is an interrupt's. It exists so a teardown during an open ask has both obligations observable at once — the callback resolved, and the turn interrupted | the tool_use line, the prompt line and (at the interrupt) the turn record | AgentPermissionDenied by a teardown, then AgentInterrupted.by_user |
| `!perm-deny-user` | a gated `Bash` the user DENIES: the deny message becomes the tool_result the model sees, the record carries `toolDenialKind`, and the turn's `result` lists the call under `permission_denials` | the tool_use line, the denied tool_result line, the closing text line | AgentPermissionDenied.by=user |
| `!perm-deny-policy` | a gated `Bash` refused by a RULE — no ask reaches `canUseTool` at all. The vendor emits `system:permission_denied` with `decision_reason_type: "rule"`, and the result lists the denial | the tool_use line, the denied tool_result line carrying `toolDenialKind: "permission-rule"`, the closing text line | AgentPermissionDenied.by=policy, reached without any AgentPermission ask |
| `!perm-undecidable` | a gated `Bash` in `auto` mode whose classifier reaches no verdict: `system:permission_denied` with `decision_reason_type: "classifier"` and no ask | the tool_use line, the denied tool_result line, the closing text line | AgentPermissionDenied.by=undecidable — a KNOWN-OPEN arm: `sdk.d.ts` declares no discriminator that separates 'nobody could decide' from an ordinary policy deny, so this scenario is the closest producer |
| `!ask-single` | one single-select `AskUserQuestion` with four options, asked through the shim's own gate | the tool_use line, the tool_result line carrying `questions` and the `answers` map, the closing text line | AgentQuestion.start choices=single_select + AgentQuestionSuccess.outcome=answered |
| `!ask-multi` | a TWO-question batch: one multi-select and one single-select, so the answer map has to be keyed by the question's own text rather than by position | the tool_use line, the tool_result line, the closing text line | AgentQuestion.choices=multi_select alongside single_select in one batch |
| `!ask-free` | a single-select question answered with FREE TEXT rather than a listed label — the vendor's automatic "Other" option. NO corpus sample exists for this shape; the answer map simply carries prose no option matches | the tool_use line, the tool_result line, the closing text line | AgentQuestionAnswers carrying free text — the residue rule's subject |
| `!ask-unanswered` | a question the user never answers: the gate's DENY becomes an error tool_result and the batch ends unanswered. `sdk.d.ts` declares NO question timeout, so an expiry is modeled as this same denial and the gap is recorded rather than invented | the tool_use line, the error tool_result line, the closing text line | AgentQuestionSuccess.outcome=unanswered |
| `!rotate` | a `/clear` in the OBSERVED shape: ONE `conversation_reset` carrying the OLD `session_id` and a `new_conversation_id` nothing later uses, then a SECOND `system:init` whose `session_id` is the REAL new id (a third uuid), and the REST of the turn — its result included — belongs to that identity | a NEW `<new-session>.jsonl` opening with the harness's local-command trio — the `<local-command-caveat>` isMeta record, the `<command-name>/clear</command-name>` ENVELOPE (the clear's only file-plane record), and an empty `system:local_command` — and carrying everything after the reset; the OLD file simply STOPS, with no closing record of any kind | SessionIdentityRotated + AgentUpdate.context_cut(ContextCleared) |
| `!slash` | a slash command the VENDOR answers itself: a `local_command_output` message, and the transcript's `system:local_command` record wrapping the output in `<local-command-stdout>` | a `system:local_command` line and a `command_permissions` attachment line | the vendor-answered slash-command family — no agent activity beyond the answer, and NO reasoning |
| `!slash-shape-a [command]` | NOTHING on the stream — this is a FILE-PLANE-ONLY shape. It writes a "user"-typed `TranscriptLine` whose content is the CLI's own slash-command bookkeeping (`<command-message>{name}</command-message>\n` `<command-name>/{name}</command-name>\n<command-args></command-args>`), parameterized by command name | one `user` transcript line carrying a fresh `promptId`, then the ordinary prompt line and the turn record | none in this converter (nothing reaches the stream); the SIDECAR classifies the record on the file plane — a command the CLI answers itself is `user/slash_command` residue, never a prompt (shim-sidecar internal/convert/bookkeeping.go) |
| `!slash-shape-a-unnamed` | NOTHING on the stream — the same FILE-PLANE-ONLY "user"-typed record, but the WITHHELD-UNNAMED shape: only `<local-command-stdout>...</local-command-stdout>`, with no `<command-name>` element at all | one `user` transcript line carrying a fresh `promptId`, then the ordinary prompt line and the turn record | none in this converter (nothing reaches the stream); the SIDECAR classifies the record on the file plane as `user/local_command_output` residue, never a prompt (shim-sidecar internal/convert/bookkeeping.go) |
| `!context-usage-drift` | prose only. It switches `getContextUsage()` to a GROWING answer, so the `context_usage` the shim pushes at this turn's end differs from the one it pushed at session start — total tokens, percentage, the message category and the whole `messageBreakdown` all move, and the answer stays a full `SDKControlGetContextUsageResponse`. CADENCE IS THE ENGINE'S: context_usage is pushed at session start, after every main-agent API response that carries usage, and at EVERY turn end regardless of scenario, so this one changes what is sampled and never when | the assistant line, the prompt line and the turn record | SessionContextUsage — the same arm twice with DIFFERENT figures, which is what a re-render tests |
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
| `!tokens-reminder` | prose only, plus the vendor's `total_tokens_reminder` ATTACHMENT — the ONE token-budget carrier any real capture holds (`artifact-publish-and-list`, once): a bare `text` field spelling `<total_tokens>N tokens left</total_tokens>` and nothing else. IT IS NOT the context-budget warning either — no capture carries a `context_budget_warning` record of any spelling, so that producer stays ungrounded rather than guessed | a `total_tokens_reminder` attachment line | NOTHING IS STORED for it: the sidecar reads and classifies the line and then drops it — `attachment/total_tokens_reminder` is on the never-persisted list (owner ruling 2026-09-13, shim-sidecar/internal/convert/neverpersist.go) |
| `!context-budget-warning` | prose only, plus a `context_budget_warning` ATTACHMENT (`{type: "context_budget_warning", content}`), the shape `convertAttachment` already recognizes (`test/convert/attachments.test.ts`). UNGROUNDED, INVENTED: no capture — not even the one literally NAMED `context-budget-warning` (MANIFEST evidence gap; excluded from golden-conformance) — carries a record of this spelling. `!context-tip` and `!tokens-reminder` stay exactly as landing 5 ruled them (a generic CLI tip and the one observed token-count reminder, neither the budget warning); this is a SEPARATE, separately-named producer added only so the converter's arm has a fake-SDK path to drive it from, pending a grounding capture (orchestrator ruling, pending the project lead's) | a `context_budget_warning` attachment line | AgentUpdate.update=contextBudgetWarning(ContextBudgetWarning) — UNGROUNDED, invented; see MANIFEST.md |
| `!compact [summary]` | a compaction: `status{compacting}`, a `compact_boundary` carrying the full corpus `compact_metadata` (trigger, pre/post tokens, duration, the preserved segment AND the preserved-messages uuid list), then `status{compact_result:"success"}`. The summary is stated as the record right after the boundary — a synthetic main-stream `user` record whose uuid is the boundary's `anchor_uuid` — and `ContextCompacted.Summary` is read off it (`settleCompaction`), so a caller names its own distinctive summary as the prompt's argument instead of the fixed default | a `system:compact_boundary` line whose `logicalParentUuid` names the preserved TAIL, plus the `isCompactSummary` user line under the anchor's uuid | SessionCompacting + AgentUpdate.context_cut(ContextCompacted) with trigger=requested |
| `!compact-auto` | an AUTOMATIC compaction — the same shapes with `trigger: "auto"`, which is the only discriminator | a `system:compact_boundary` line with `compactMetadata.trigger: "auto"` | AgentUpdate.context_cut(ContextCompacted) with trigger=automatic |
| `!compact-failed` | a compaction that FAILS: `status{compacting}` then `status{compact_result:"failed", compact_error}` and NO boundary | nothing but the prompt line and the turn record — a failed compaction cut nothing | AgentUpdate.context_cut(ContextCompactionFailed) |
| `!away-summary` | prose only; the vendor's recap is a `system:away_summary` transcript record | a `system:away_summary` line | vendor_specific residue — `system/away_summary`, which no conversation.v1 arm models |
| `!residue` | prose only; it writes the two attachment records BOTH planes agree are vendor bookkeeping, not context | `deferred_tools_delta` and `agent_listing_delta` attachment lines | NONE — these are `StoreUnservedItem.vendor_specific{kind:"attachment/deferred_tools_delta"}` and `attachment/agent_listing_delta`, dropped from every page by both planes |
| `!cold-seed` | an ordinary turn whose TRANSCRIPT RECORDS are stamped TWO HOURS IN THE PAST, so the next resume of this session trips the shim's own cold-context detection | the ASSISTANT line (with its usage) and the turn_duration line, both carrying a two-hour-old `timestamp` | SessionColdLapsed on the NEXT resume — this scenario only seeds the condition |
| `!fail-execution` | a reasoning block and a partial answer, then an error `result` with subtype `error_during_execution` and NO `terminal_reason` — the subtype alone is the vendor's whole account | the assistant lines for the work it did reach, the prompt line and the turn record | AgentThinking + AgentResponse, then AgentFailure.execution_error — DECLARED-ONLY: no capture grounds this terminal (`turn-stop-error-during-execution` ended `success.interrupted` after an `aborted_streaming`), so the mock keeps the declared arm and the evidence gap is listed in testdata/captures/MANIFEST.md |
| `!fail-max-turns` | a reasoning block and a partial answer, then an error `result` with subtype `error_max_turns` and `terminal_reason: "max_turns"` | the assistant lines for the work it did reach, the prompt line and the turn record | AgentThinking + AgentResponse, then AgentFailure.max_turns |
| `!fail-budget` | a reasoning block and a partial answer, then an error `result` with subtype `error_max_budget_usd` and `terminal_reason: "budget_exhausted"` | the assistant lines for the work it did reach, the prompt line and the turn record | AgentThinking + AgentResponse, then AgentFailure.budget_exhausted |
| `!fail-structured-output` | a reasoning block and a partial answer, then an error `result` with subtype `error_max_structured_output_retries` and `terminal_reason: "structured_output_retry_exhausted"` | the assistant lines for the work it did reach, the prompt line and the turn record | AgentThinking + AgentResponse, then AgentFailure.structured_output_retry_exhausted — DECLARED-ONLY: no capture grounds this terminal (`turn-stop-max-structured-output-retries` ended `success.completed`), so the mock keeps the declared arm and the evidence gap is listed in testdata/captures/MANIFEST.md |
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
| `!fail-stop-hook` | a reasoning block and a partial answer, then an error `result` with subtype `error_during_execution` and `terminal_reason: "stop_hook_prevented"` | the assistant lines for the work it did reach, the prompt line and the turn record | AgentThinking + AgentResponse, then AgentFailure.stop_hook_prevented — DECLARED-ONLY: no capture grounds this terminal (`turn-stop-hook-stop` ended `success.completed`), so the mock keeps the declared arm and the evidence gap is listed in testdata/captures/MANIFEST.md |
| `!fail-hook-stopped` | a reasoning block and a partial answer, then an error `result` with subtype `error_during_execution` and `terminal_reason: "hook_stopped"` | the assistant lines for the work it did reach, the prompt line and the turn record | AgentThinking + AgentResponse, then AgentFailure.hook_stopped |
| `!fail-continuation-prevented` | an `informational` message with `prevent_continuation: true`, a `stop_hook_summary` record whose `preventedContinuation` is true, then a `stop_hook_prevented` terminal | the `system:stop_hook_summary` line, the prompt line and the turn record | AgentFailure.stop_hook_prevented — the arm this pairing ACTUALLY reaches. AgentFailure.continuation_prevented is UNSETTLED and UNGROUNDED: no `TerminalReason` names it, so the two declared prevent-continuation signals ride the nearest terminal and nothing produces the continuation_prevented arm |
| `!api-429` | a `system:api_error` record and an `api_retry` message, a failed assistant message carrying the vendor's `error` class, then an `error_during_execution` result with `terminal_reason: "api_error"` and status 429 | a `system:api_error` line, the prompt line and the turn record | AgentUpdate.api_error mid-turn, then AgentFailure.api_request_failed → ApiRateLimited (with the retry-after) |
| `!api-529` | a `system:api_error` record and an `api_retry` message, a failed assistant message carrying the vendor's `error` class, then an `error_during_execution` result with `terminal_reason: "api_error"` and status 529 | a `system:api_error` line, the prompt line and the turn record | AgentUpdate.api_error mid-turn, then AgentFailure.api_request_failed → ApiOverloaded |
| `!api-401` | a `system:api_error` record, a failed assistant message carrying the vendor's `error` class, then an `error_during_execution` result with `terminal_reason: "api_error"` and status 401 | a `system:api_error` line, the prompt line and the turn record | AgentUpdate.api_error mid-turn, then AgentFailure.api_request_failed → ApiAuthenticationFailed |
| `!api-403` | a `system:api_error` record, a failed assistant message carrying the vendor's `error` class, then an `error_during_execution` result with `terminal_reason: "api_error"` and status 403 | a `system:api_error` line, the prompt line and the turn record | AgentUpdate.api_error mid-turn, then AgentFailure.api_request_failed → ApiPermissionDenied |
| `!api-400` | a `system:api_error` record, a failed assistant message carrying the vendor's `error` class, then an `error_during_execution` result with `terminal_reason: "api_error"` and status 400 | a `system:api_error` line, the prompt line and the turn record | AgentUpdate.api_error mid-turn, then AgentFailure.api_request_failed → ApiInvalidRequest |
| `!api-413` | a `system:api_error` record, a failed assistant message carrying the vendor's `error` class, then an `error_during_execution` result with `terminal_reason: "api_error"` and status 413 | a `system:api_error` line, the prompt line and the turn record | AgentUpdate.api_error mid-turn, then AgentFailure.api_request_failed → ApiRequestTooLarge |
| `!api-404` | a `system:api_error` record, a failed assistant message carrying the vendor's `error` class, then an `error_during_execution` result with `terminal_reason: "api_error"` and status 404 | a `system:api_error` line, the prompt line and the turn record | AgentUpdate.api_error mid-turn, then AgentFailure.api_request_failed → ApiNotFound |
| `!api-500` | a `system:api_error` record and an `api_retry` message, a failed assistant message carrying the vendor's `error` class, then an `error_during_execution` result with `terminal_reason: "api_error"` and status 500 | a `system:api_error` line, the prompt line and the turn record | AgentUpdate.api_error mid-turn, then AgentFailure.api_request_failed → ApiInternal |
| `!api-billing` | a `system:api_error` record, a failed assistant message carrying the vendor's `error` class, then an `error_during_execution` result with `terminal_reason: "api_error"` and status 402 | a `system:api_error` line, the prompt line and the turn record | AgentUpdate.api_error mid-turn, then AgentFailure.api_request_failed → ApiBillingError |
| `!api-oauth-org` | a `system:api_error` record, a failed assistant message carrying the vendor's `error` class, then an `error_during_execution` result with `terminal_reason: "api_error"` and status 403 | a `system:api_error` line, the prompt line and the turn record | AgentUpdate.api_error mid-turn, then AgentFailure.api_request_failed → ApiOauthOrgNotAllowed |
| `!api-max-output` | a `system:api_error` record, a failed assistant message carrying the vendor's `error` class, then an `error_during_execution` result with `terminal_reason: "api_error"` and status null | a `system:api_error` line, the prompt line and the turn record | AgentUpdate.api_error mid-turn, then AgentFailure.api_request_failed → ApiMaxOutputTokens |
| `!api-unmodeled` | a `system:api_error` record, a failed assistant message carrying the vendor's `error` class, then an `error_during_execution` result with `terminal_reason: "api_error"` and status 418 | a `system:api_error` line, the prompt line and the turn record | AgentUpdate.api_error mid-turn, then AgentFailure.api_request_failed → ApiUnmodeledError |
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
| `!query-eof-mid-ask` | a gated `Bash` whose `canUseTool` ask is opened and then NEVER answered by the vendor: the iterable ENDS with the callback still pending. THE ASK IS OPENED BEFORE THE DEATH, which is the whole point — an unresolved `canUseTool` promise wedges the vendor process, so the query-death path owes every pending callback a denial | the tool_use line and the prompt line; there is no turn record because there was no turn end | SessionQueryDied.cause=unexpected_eof with an AgentPermission settling denied |
| `!query-fail` | NOTHING, and then the iterable REJECTS — the producer died rather than finished | the prompt line only | SessionQueryDied.cause=iterator_failure |
| `!keepalive` | an ordinary short turn. It exists so a test can drive a keep-alive-shaped turn deterministically; the `<!--agent-repl:keepalive-->` marker is the SHIM's, and the mock never adds or removes it | the assistant line, the prompt line (marker and all) and the turn record | AgentResponse.from_model, AgentSuccess.completed — classified keep-alive by the marker on the PROMPT |
| `!queue-vendor-turn` | an ordinary short turn, and then — ahead of the NEXT send's own turn — a turn the vendor runs ON ITS OWN, the way a background task's notification starts one: an assistant answer and a result with `origin: {kind: "task-notification"}`, and NO `user_message_uuid` anywhere, because no send asked for it | the assistant line, the prompt line and the turn record, then the vendor turn's assistant line and record | AgentResponse.from_model, AgentSuccess.completed — twice, the second answering nobody. Grounded in the 2026-09-23 keep-alive leak, where such a turn's result closed the shim's keep-alive early |

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

THE DAEMON GUARANTEES THE `--fake` HALF. A daemon that is itself under the
guard spawns every shim with `--fake` rather than refusing the spawn
(`daemon/internal/shimclient/supervisor.go`, `fakeMode`), so a guarded run
creates, forks and prompts workspaces entirely against the mocked vendor. The
guard is still stated on the child, so the throw at `createRealQuery` remains
the backstop if the shim ever reaches for the SDK in fake mode.

`test/vendor-guard.test.ts` enforces the chokepoint STRUCTURALLY: it walks
`src/` and fails if any file other than the guard contains a dynamic import of
the SDK. `src/sdk/types.ts` imports SDK types with `import type`, which is erased
at build time and is not a vendor import site.

## Logging

- `src/log.ts` is the one canonical JSON logging API. A logger has exactly one
  normal-emission method per level: `debug`, `info`, `warn`, and `error`.
  `logVerbose` is the debug-level verbose-class variant. Every call site names
  its level in the method, and every method uses the same record builder and
  inherited-fd sink.
- The durable sink is the **inherited fd 3**, never a pipe to the daemon's
  stderr: a shim must survive its daemon's death without dying on its own log
  line (EPIPE incident, 2026-08-10). A poisoned sink is surfaced, never
  silently swallowed; the stderr mirror is a convenience that retires itself
  once, durably recorded.
- Every record carries the shim `pid`, the workspace dir and its id, and every
  known agent-repl and Claude session identifier. Before a session exists the
  `agent_repl_session_id` is the process's own `shim-<workspace id>-<pid>`,
  which joins the daemon's own records for the same workspace.
- **`workspace_id` IS THE DAEMON'S 16-HEX WORKSPACE ID, and nothing else.**
  `bin/logs.sh --workspace` and the realtest harvest group records by it, so a
  shim that answered with an id of its own devising filed its records under a
  workspace nothing else in the fleet ever wrote to. No spawn argument or
  environment variable carries it; what the daemon does hand over is the listen
  socket, which it names `<state>/sock/<workspace id>.sock` (plus the rollout's
  `.n<generation>`), so `workspaceIdFromListenSocket` reads it back off the
  basename. A socket that does not spell one is a STARTUP REFUSAL, exactly like
  an unrecognized flag: it means this build and the daemon disagree about the
  socket layout, and a record filed under an unknown workspace is worse than a
  shim that says why it will not start.
- The shim's own md5 prefix of the workspace directory travels beside it as the
  `shim_workspace_hash` CONTEXT key. It is what the workspace lock FILE is
  named after, so it is the only thing joining a record to that file on disk —
  it is simply not the fleet's workspace identity.
- `AGENT_REPL_LOG_LEVEL` is the process-startup threshold for both durable
  persistence and stderr mirroring. It accepts exactly `debug`, `info`, `warn`,
  or `error` and defaults to `info`. An invalid value aborts logger setup.
  `AGENT_REPL_LOG_VERBOSE=1` only permits verbose-class records to mirror to
  stderr after the level threshold admits them. It never changes persistence.
- **Every logical branch logs.** Request boundaries, store round-trips, and
  ordinary state transitions are `debug`. Session and turn start/end, kill,
  hibernate, compaction, and detach edges are `info`. A defect or explicitly
  named refusal/decision is `warn`. Only an actual error is `error`, with its
  text carried by the message or structured cause.
- Each error is logged exactly once by its owning layer, with session, store
  key, socket, request, operation, resolved inputs, branch outcome and cause.
- Frequent or hot diagnostics use `logVerbose`. ESLint rejects direct
  `console`, direct `process.stderr` outside `src/log.ts`, and every generic
  `.log(...)` call in production source. The only stderr exceptions are the
  documented pre-logger bootstrap/sink emergency and sink-mirror paths owned by
  `src/log.ts`.
- Every `warn` call is immediately preceded by `// warn: a defect because …`
  or `// warn: a decision because …`; ESLint rejects a warning without one.
- The full contract is `modules/app/agent-repl/logging-contract.md`.

Read shim records and harvest run windows through `../../../bin/logs.sh`; the
full path, rotation, attribution, and level-switch table is in
`../../../AGENTS.md`.

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
   the current `context_usage`, `model_changed`, `permission_mode_changed`,
   `fast_mode`, `account_usage`, `title` and `network_resume_waits`;
   after that, arms are pushed on CHANGE only. A level is replayed only once
   it has been stated: a session that never waited on the network states no
   `network_resume_waits` at open, and one whose last wait ended replays the
   empty set it stated then.
3. **The store client ends `WatchAgentSession` by CANCELLING its context**
   (an `AbortSignal`), never by a bare close: a bare close leaves the store
   holding a reading session nobody will ever pull, and the store has no other
   signal that the reader is gone.
4. **A READ THAT STANDS NO TAIL GOES THROUGH `readFirstPage`, NEVER
   `openAgentPage`.** The two differ in what they stand, not in how they are
   called: `openAgentPage` opens a session the caller will watch, while
   `readFirstPage` opens `page_only` so the store mints no watch token at all.
   The store cannot tell the difference on its own — `OpenAgentSession` is
   unary and its service has no close — so a page opened and then closed here
   leaves a token nothing will ever spend, for the store's whole process
   lifetime. Every one-shot site uses the read verb: StartTurn's opening page,
   the teardown's book head, the StartSession reconciliation read, the
   live-work re-announcement, and `ReadHistory`. `e2e`'s
   `TestACompletedTurnAndTeardownLeaveNoWatchTokenOutstanding` is what holds
   this: it drives a real turn and stop, then reads the store's own shutdown
   record for `outstanding_tokens=0`.
5. **A STANDING STREAM MAY END ONLY BECAUSE SOMEBODY ASKED IT TO, AND EVERY
   ENDING IS NAMED.** The store's `WatchAgentSession` has no failure arm and no
   natural end, and its handler returns a CLEAN end of stream when the store is
   shutting down. `store/reader.ts` used to read that as "the store closed, so
   stop": the engine's `for await` fell out, `WatchAgent` returned normally, and
   `service/routes.ts` recorded `completed` at DEBUG — so the daemon opened a
   `link_fault` for a standing stream that ended while the session lived, and
   the shim's log held nothing at any level it runs at (workspace
   2b81f45a724642ef, 2026-09-13).
   - An unasked end now takes the refused token's own recovery: re-open from
     the LAST SERVED POINTER, on the read half's retry schedule, so a store
     restart under a live shim does not sever every consumer's tail. It is
     bounded by `UNASKED_END_BUDGET` consecutive ends that delivered nothing,
     after which the tail throws `store_unavailable` rather than spinning.
   - `engine/turn.ts` names how EVERY `WatchAgent` ended: `concluded` (the
     teardown's, at info), the consumer's own departure or a throw the route
     already recorded (debug), and a tail that simply ran out — the daemon's
     severed link, seen from this side — at ERROR. The handler learns of a
     conclusion by observing it on the `AgentPageSession` it registers with the
     session, which is what the teardown concludes through.

## A retired line is relayed, never dropped

The store can RETIRE a page line (store.v1 `StoreRetirement`: the sidecar's
conversion changed and the record behind the row no longer converts to it).
A standing `WatchAgentSession` sends it ONCE on the `retired` arm, as the line
last served, at its own pointer; no page serves it again.

- `store/reader.ts` switches on `WatchAgentSessionResponse.frame.case`:
  `line` is served as before, `retired` is converted by the same
  `toHistoryEntryAt` and yielded as an `AgentTailFrame` on the `retired` arm,
  and an UNSET arm is still a loud `store_unavailable`, never skipped.
- `AgentPageSession.tail` yields `AgentTailFrame` (a discriminated union whose
  arms are named after `WatchAgentResponse.frame`'s), and `engine/turn.ts`
  relays `retired` as `WatchAgentResponse.retired` so the daemon removes what
  it drew.
- A retirement COUNTS AS SERVED for the conclusion: its pointer joins `served`
  and becomes `servedThrough`, so a teardown concluding through a retired head
  ends instead of standing for a line that can never come. It is recorded
  under a content marker no fingerprint equals, so a row later taken back at
  the same position is always served again on a re-open.
- Each relayed retirement is one `logVerbose` record with `agent` and
  `pointer`; nothing on this path logs at warn.

## The keep-alive rewind: what may anchor it, and what happens when it fails

The shim submits its own keep-alive prompt every four minutes to keep the
vendor's five-minute prompt cache warm (`src/engine/keepalive.ts`). A real
prompt must never build on that housekeeping, so before one is delivered the
vendor context is ROLLED BACK: the query is closed and reopened with `resume`
plus `resumeSessionAt: <uuid>`, the SDK's one declared surface for truncating a
conversation without rewriting the vendor's file.

**THE ANCHOR IS AN ASSISTANT RECORD OF A REAL TURN, AND NOTHING ELSE.** The SDK
says so at the option itself — "The message ID should be from
`SDKAssistantMessage.uuid`" — and every other SDK message carries a `uuid`
anyway: `system:init` and `result` both DECLARE one as required, and those uuids
name no transcript record. `KeepaliveRewind.noteRecord` therefore takes the
MESSAGE and the turn it arrived under, not a uuid, and refuses everything that
is not an `assistant` message of an open, non-keep-alive turn. The call site
never extracts a uuid at all, so the filter cannot be got wrong by a caller.

This was paid for on the owner's workspace on 2026-09-14: twice, the second real
prompt after a resume died at once with the vendor exiting 1 on `No message
found with message.uuid of: 19e047a0-…` (and earlier `b64f2741-…`) — uuids in no
transcript, taken from the init and control messages that were the only
non-keep-alive traffic a cold-gate resume had produced.

**AND THE ANCHOR DOES NOT CROSS A BOUNDARY.** A uuid from before a compaction,
a conversation reset, or a query rebinding may no longer be resumable, so each
CLEARS it: `startQuery` drops it on every binding that is not itself the rewind
(the opening, a cold-gate answer, a rotation, a restart, a plain replacement),
the vendor's `compact_boundary` and `conversation_reset` drop it as they arrive,
and the shim's own compaction drops it as it lands. After a clear the next real
prompt CARRIES the keep-alive turns rather than rewinding — the existing "no
anchor" branch — and says so at INFO.

**THE REWIND IS VISIBLE.** The "REWINDING…" record names the anchor's uuid, the
turn it came from, and the keep-alive turns being discarded; a rewind that lands
says so; a cleared anchor says why. All INFO. When a rewind goes wrong the only
evidence anyone has is which uuid the shim chose, so none of it is debug.

**AND IT IS SURVIVABLE. THE PROMPT IS NEVER LOST.** If the replaced query fails
to start, or the vendor refuses the anchor afterwards — an error result naming
the uuid, or the child ending its stream having printed
`No message found with message.uuid` — the shim does NOT let the session die. It
records one ERROR with the anchor and the vendor's own words, forgets the
anchor, reopens the query WITHOUT `resumeSessionAt` (a plain resume), delivers
the SAME prompt onto it, and raises a `keepaliveFailed` fault on its own
component (`shim-engine-keepalive-rewind`, cleared by the next rewind that
lands) so the footer says what happened. The refusal never reaches the fold, so
the feed shows the answer rather than "the run broke while executing".

## The keep-alive turn scope: what the keep-alive produced, and who may see it

A keep-alive turn is NEVER served to anyone: not its prompt, its reply, its
thinking, its usage, its terminal or its end (`src/engine/keepalive.ts`,
`KeepaliveScope`).

- **THE VENDOR ATTRIBUTES, NOT ARRIVAL ORDER.** The keep-alive send carries a
  client `uuid` the shim mints; the vendor echoes it (`user_message_uuid` /
  `user_message_uuids`) on the first reply frames and the `result` of the turn
  that answers it. A vendor turn is the keep-alive's from its first frame
  naming that uuid to its result. Paid for on 2026-09-23: the vendor ran a
  background task's notification turn between the keep-alive's send and its
  answer, that turn's `result` closed the keep-alive, and the keep-alive's `.`
  arrived untagged and was drawn as a green final answer.
- **WORK IS THE KEEP-ALIVE'S ONLY WHEN THE KEEP-ALIVE OPENED IT.** A frame that
  names its work — a subagent's frame (`parent_tool_use_id`), tool progress, a
  task's start, progress, notification or update — is tagged only when that
  work descends from a call the keep-alive's own frames opened
  (`KeepaliveScope.spawned`, forgotten when the scope closes); it never inherits
  the running vendor turn. A permission or question ask is attributed the same
  way, by the book it lands on. Paid for on 2026-09-15 and 2026-09-23: a
  BACKGROUNDED subagent's frames arriving during a keep-alive turn were stored
  as keep-alive rows under real subagent upsert_keys, and the sidecar's page
  line for each was refused as an identity change, parking the transcript.
- **ONE TAG, TAKEN ONCE.** `onSdkMessage` asks the scope once per message; the
  answer is the fold context's `keepalive`, so every row the fold produces
  carries it. The store writer DROPS a tagged entry at its door (`write` and
  `writeDurable`, one DEBUG record each, naming no upsert key), and
  `toStoreEntry` refuses one that got past it; `serveSessionUpdates` never
  pushes a tagged session fact. Only the keep-alive's own result closes it, and
  that close pushes nothing (no context usage, title or account re-probe).
- **THE DAEMON NEVER SEES ONE.** No verb names a keep-alive as the turn in
  flight (`servedOpenTurn`): not `SessionStarted`, not `SessionLive`, not
  `KillSession`, not `Hibernate`, and no refusal either: the retired
  `turn_already_open.keepalive` flag (proto tag reserved) is gone.
- **A REAL PROMPT WAITS BEHIND IT, INSIDE THE SHIM (2026-09-23).** A
  `StartTurn` that lands while a keep-alive holds the slot waits for the
  keep-alive to leave it — its own result, an abandoned beat, the query's
  death, a teardown — and then opens its turn as on an idle session
  (`TurnEngine.waitOutShimTurn`). Nothing of the turn exists during the wait,
  so its two exits lose and double nothing: the bound
  (`KEEPALIVE_YIELD_BUDGET_MS`) refuses `vendor_refused` at ERROR, and the
  caller's abort signal refuses it undelivered at INFO. THE WAITING PROMPT
  INTERRUPTS THE KEEP-ALIVE FIRST (2026-09-29, `TurnEngine.interruptKeepalive`):
  the prompt rolls the vendor context back past it anyway, so letting it run
  only makes the user wait, and a keep-alive whose API call hangs (an outage on
  2026-09-28) held every prompt behind it for whole budgets. The slot is NOT
  released by the interrupt: the keep-alive leaves on its own result, which the
  send ledger attributes to the keep-alive's own send by id, so nothing of it
  can land on the prompt's turn. A refused interrupt is ERROR and the prompt
  waits within the bound as before. `setOpen` is the one writer of the slot, so every way a
  keep-alive leaves releases the wait, ends the keep-alive's rewind watch (so a
  later anchor refusal cannot re-deliver the keep-alive under the real turn),
  and the keep-alive's rewind debt is counted before `closeTurn` first awaits.
  A second `StartTurn` while one is being started is refused
  `turn_already_open`, a beat is skipped while a start is in flight, and a
  `KillTurn` naming the waiting turn waits for the start and then interrupts
  the opened turn, as it would for any start. A `KillTurn` or main-agent
  stop that finds ONLY a keep-alive open answers `no_turn_open` /
  `nothing_running` and never interrupts it (`TurnEngine.servedTurn`).
- **NOTHING IS STORED, ON EITHER PLANE (2026-09-23).** No purpose needs a
  row. The send, the answer and the scope's close are this process's memory;
  the rewind anchor is taken from the SDK messages as they pass
  (`KeepaliveRewind.noteRecord`), `resumeSessionAt` reads the vendor's own
  transcript, and a refused rewind re-delivers the prompt it still holds. The
  "accounting" the rows were kept for had no reader: the store serves only page
  lines and bash runs, and the daemon has no store client. The sidecar skips the
  same turn's transcript records by the marker plus the transcript's promptId
  and parentUuid links (its AGENTS.md, "Keep-alive"), so no plane writes a key
  the other must be guarded against. Rows written before stand as they are.
- **WHAT IT DOES NOT CLAIM.** A vendor turn's preamble (`init`, a
  `UserPromptSubmit` hook, a status line) carries no stamp and stays untagged.
- **THE FILE PLANE is the marker, not the stamp**: the title digest and the
  transcript listing skip a prompt that begins with the keep-alive marker.

## A reply is matched to the send that caused it, by id (ruled 2026-09-28)

`src/engine/sends.ts` (`SendLedger`) is the ONE attribution of vendor output
to turns. Arrival order attributes nothing.

- **EVERY SEND IS STAMPED.** `submit` (every StartTurn prompt: user, queued,
  held, session acts, merge-repair, guidance), `deliverNetworkResume` and the
  keep-alive beat all push through `pushSend`, which registers a client uuid
  in the ledger BEFORE the push. A refused rewind re-delivers the same send
  under the same uuid. `newUuid` names sends only; turn ids never use it.
- **THE VENDOR'S ECHO ATTRIBUTES EACH VENDOR TURN.** `user_message_uuid` /
  `user_message_uuids` (sdk.d.ts) on a turn's first reply frames and its
  `result` name the send it answers; the SDK's binding rule lets a sender find
  its uuid anywhere in the list. Unstamped frames after the first reply belong
  to the vendor turn already running.
- **AN UNSTAMPED FIRST REPLY IS A VENDOR-STARTED TURN** and takes the
  adoption path (`VENDOR_STARTED` prompt row, shim-minted id). It is adopted
  BESIDE the send slot, never into it, so a StartTurn is never refused because
  of it: the prompt is delivered, and the vendor runs it after its own turn or
  FOLDS it in. A fold (a vendor-started turn whose echo moves onto one of our
  sends) concludes the adopted turn as absorbed (`Fold.concludeAbsorbedTurn`,
  COMPLETED naming its last prose) before the send's rows.
- **AN ECHO NAMING A UUID WE NEVER SENT** is an invariant violation: ERROR
  (`shim-engine-sends`), attributed to no turn, never guessed.
- **WHAT NO ID SPEAKS TO.** A turn's preamble (init, a UserPromptSubmit
  hook) and detached work between turns carry no stamp; they are charged by
  slot (`unstatedTurn`), which is not a reply's attribution.
- **A STARTTURN WAITS ONLY BEHIND THE SHIM'S OWN TURN**: the keep-alive, or
  the network-resume prompt (`shimTurnEnded`), bounded by
  `KEEPALIVE_YIELD_BUDGET_MS`. `turn_already_open` is only ever the daemon's
  own open turn; a send slot found taken after StartTurn's awaits is an
  invariant break at ERROR, refused undelivered.
- **THE DAEMON STANDS THE WAITING TURN BEHIND THE ADOPTED ONE**
  (`daemon/internal/sessionwatcher`, `waiting`): the adopted turn is served as
  the turn in flight (`servedOpenTurn`), and the daemon's turn stands in flight
  again when it ends.
- **NOTHING VENDOR-SHAPED CROSSES THE WIRE.** The client uuids stay in this
  process; the daemon correlates by turn id, so no proto field carries them.

## A turn id is started once

`TurnEngine.repeatedStart` (`src/engine/turn.ts`); the contract is the comment on
`StartTurnRequest.turn`.

- **A REPEAT STARTS NOTHING.** A `StartTurn` repeating a turn id this shim
  accepted delivers nothing, writes no prompt row, opens no turn, and is
  answered success: the prompt under that id with the ORIGINAL start's origin,
  and the opening page the repeat asks for. It is judged first, before the
  double-submit and open-turn refusals (those are about a DIFFERENT turn) and
  before any keep-alive wait.
- **IT IS AN INVARIANT VIOLATION, RECORDED AT ERROR** with the turn, the verb,
  the original's state (`open`, `ended`, `starting`), both origins, and the
  session's agent and vendor session ids. Only a daemon that lost its record of
  an acceptance (a death between the shim's answer and its stamp) re-drives one.
- **A REPEAT RACING ITS STILL-STARTING ORIGINAL WAITS FOR IT**: an accepted
  original answers it as above; a refused one leaves it a first start. A
  refused start is never remembered, so its retry is a first start.
- **THE MEMORY IS CONSTANT-SIZE**: the last `STARTED_TURNS_REMEMBERED` accepted
  ids and their origins, never the prompt. It is process memory, so a shim that
  died forgets; an id pushed out of the window is started as a first start.

## The hibernate contract: at most one compaction per idle period

`Hibernate` is the daemon's pre-hibernation directive, and the shim's answer to
it obeys two invariants. Both were paid for on the owner's workspace on
2026-09-14, where the idle sweep bought thirteen vendor summary turns for one
conversation between 09:02 and 10:03 and never stood the shim down once.

**THE ANSWER IS BOUNDED BY THE CALLER'S DEADLINE.** A compaction is a real
vendor turn and takes as long as one; the daemon's `StandBound` is a sum of
TEARDOWN bounds and is seconds. So the answer is not the work's completion:

- `compacting` — the compaction has STARTED and outlives this rpc. The
  compaction runs to completion regardless of the rpc's cancellation, which it
  always did; what is new is that the caller is TOLD so, defers the pass
  without calling it a failure, and asks again.
- `success` — there is nothing left to do. The daemon may stand the shim down.
- `error` — a refusal, with the arm as the reason. A compaction that FAILED is
  reported here, on the ask after the failure, because the rpc that started it
  was answered long before it failed. It is reported once and cleared, so the
  ask after that retries rather than repeating a stale failure forever.

**A TRANSCRIPT THAT IS ALREADY COMPACTED IS NEVER COMPACTED AGAIN.** The ask
that follows a landed compaction is acked WITHOUT a vendor turn. Two
independent readings settle it, and either one is enough, because they fail in
different ways:

- the persisted mark (`engine/compaction-mark.ts`), one file per vendor session
  under `$AGENT_REPL_STATE_DIR/shim/<workspace-key>/compaction/`, stating the
  transcript's BYTE LENGTH at the instant the compaction finished. A transcript
  is append-only, so "as long as the mark says" and "nothing has been said
  since" are one statement — and it is a statement a FRESH PROCESS can make,
  which is the point: a daemon bounce, or a shim restarted under one, must not
  buy the same summary twice.
- the transcript's own tail (`transcriptTailIsCompaction`), which is a
  `compact_boundary` and its `isCompactSummary` partner as the LAST two
  records. It survives the mark being lost with its state directory. The TAIL
  and nothing else: a boundary anywhere earlier is a compaction the
  conversation has since outgrown, which is exactly the case owed another one.

A mark that cannot be read answers "no mark" and is recorded. The cost of a
lost mark is one extra compaction; the cost of treating an unreadable mark as
present would be a hibernation that never compacts at all.

**THE SUITE AWAITS THE WORK THE DAEMON DOES NOT.** The daemon has a next sweep
pass; a test has no clock to wait on, so `EngineDeps.onHibernationCompactionSettled`
is injected the way the scheduler and the identity store are. Production leaves
it absent and reads the compaction's own records instead.

## A settle stands alone

A unit's start and its settle upsert one store row, so a replay serves the
settle alone, and every settle restates what its start carried. Two structural
doors keep that from depending on each converter remembering:

- `convert/entries.ts` `agentActivity` stamps every activity
  `AgentActivity.contract = SETTLES_STAND_ALONE`. It is the ONE activity
  constructor (the boot sweep's closings build through it too); the daemon grades
  a bare settle by this stamp, so it must never move into per-kind code.
- `settledAt(atMs, startedAtMs)` and `toolFailure(content, atMs, startedAtMs)`
  take the start as a REQUIRED argument; `failureOf(call, outcome)` and
  `settle(call, outcome)` read it off the call. Prose and reasoning blocks
  announce no instant and pass `undefined`.

A detached spawn's notification carries neither prompt nor start, so the task
table (`TaskKindRegistry.rememberCall`) keeps the spawning call from the moment
it is seen open; its terminals and running beats restate the prompt from it and
the created agent by the minting rule. The boot sweep's spawn closing restates
what the agent's book records for the unit.

## The in-flight call registry: it holds only what this stream can settle

The fold remembers each announced tool call (`convert/tool-calls.ts`
`CallRegistry`) so the call's terminal can restate the call's own input. It is
a constant-size join, and these are its invariants:

- **A CALL IS HELD ONLY ON A STREAM THIS PLANE CAN SETTLE.** Each call records
  the stream it rode (`PendingCall.spawningCall`, the message's
  `parent_tool_use_id` read through `spawningCallOf`, the same rule as the book
  and the block state). The main stream is always held. A subagent's stream is
  held while its spawning call is held and not handed off. A BACKGROUNDED
  agent's calls are announced and never held: the SDK forwards its `tool_use`
  blocks onto the stream but not its `tool_result` records (the
  `subagent-detached` capture), so the file plane settles those units from the
  sidechain transcript.
- **A RESULT ALWAYS RELEASES ITS CALL.** It stays held only when the converter's
  `retain` says the unit awaits a later record on this stream (the skill's
  document). A result that writes no terminal still releases the call. That
  covers a subagent's result, which carries no `tool_use_result` on the stream,
  a backgrounded shell's receipt, an async spawn's launch receipt and a
  monitor's arming receipt.
- **A HANDOFF IS NEVER CUT.** `task_started{is_backgrounded: true}` and a
  `task_updated` backgrounding patch mark the call detached and release its
  stream's calls. A stop never cuts a detached call.
- **AN AGENT'S END RELEASES ITS STREAM.** A spawning call's settle releases every
  call its stream still held, recursively.
- **NO CALL OUTLIVES ITS TURN.** Every turn terminal drains the registry
  (`endTurnCalls`). A stop cuts only the genuinely open calls whose kind states
  a `cut`, and each cut frame gets its own block ordinal. Everything else is
  released at INFO. A query's death or replacement drains it too
  (`EngineFold.endQuery`), with no frame written.
- **THE BOUND IS AN INVARIANT, NOT A LIMIT.** `CALL_REGISTRY_CAPACITY` (512) is
  never reached in normal operation. Reaching it is logged at ERROR, because the
  eviction loses a card's settle.
- **REGISTRATION AND SETTLEMENT SHARE ONE IDENTITY.** A result that rides another
  stream than its call is logged at ERROR and still settled by the vendor's call
  id. A result on a stream this plane does not hold is logged at DEBUG, because
  the file plane settles it. An unannounced result on a held stream stays a WARN.
- `Fold.inFlightCalls()` is the snapshot the suites assert against. After every
  capture's terminals, and after a mixed fake-SDK workload, it is empty
  (`test/convert/fold.test.ts`).

## A detached shell's lifecycle is the shim's to write (2026-09-27)

`convert/detached.ts` (`shellRunStartEntry`, `shellRunTerminalEntry`),
`engine/detached.ts` (`ShellRunStarts`), `engine/turn.ts` (`watchBash`).
Paid for by task `bfa5s1wjd`: a hand-backgrounded shell that concluded in
eleven seconds got no store row at all (its rows came only from the sidecar
tailing the spool, and none were written), so `WatchBash` stood open, SILENT,
forever, and the run's announcement stayed open in the record.

- **THE SHIM WRITES A RUN'S START AND ITS TERMINAL; THE SIDECAR WRITES ITS
  OUTPUT.** The start (`bash:<run>:start`) rides AHEAD of every announcement of
  the run in the one ordered buffer — the `Bash` result that says it moved, and
  a by-hand backgrounding's `task_updated` — and is restated at the conclusion.
  The terminal (`bash:<run>:terminal`) is written from the task's
  `task_notification`, the moment the engine retires the run: `completed` and
  `failed` settle `completed` (a non-zero exit is the command's own verdict),
  `stopped` settles `interrupted.by_user` (no cause for `worker_restart`), and
  the output is `not_observed`, because every byte of it is the spool's.
- **ONE WRITE IDENTITY PER RUN AND ARM.** The source coordinate is the run
  itself (`shell-run:<run>`), so every restatement of a start mints the same
  write id and the store's ledger absorbs all but the first.
- **THE KEYS ARE THE CROSS-PLANE ONES**, the sidecar's `BashStartKey` /
  `BashTerminalKey` spellings, so a sidecar row for the same fact lands on the
  same row and a run has ONE start and ONE terminal. A sidecar terminal written
  later supersedes the shim's whole and carries the spool's evidence (the exit
  status, the output). Nothing written after a terminal reopens the run: the
  store only ever ENDS a `detached_work` row, and a late tail is an upsert of
  its own row, served on the next replay and never re-listed as live.
- **`WatchBash` NEVER WAITS.** The stream is opened only after the run's
  remembered start is made DURABLE (`writeDurable` of the same entry — absorbed
  if it already landed, landed behind the original if not), so its first frame
  is `start` at once; a concluded run replays and closes on its terminal; a run
  THIS SHIM HOLDS LIVE with no stored row yet is opened with
  `store.v1.WatchBashRunRequest.await_first_row`, waiting on its first row for
  as long as the run lives (landed change 4, 2026-09-30); any other run the
  store holds no row for is refused `NotFound` (`unknown_work`), which is the
  contract's "not a live shell: refused". There is no recheck cadence and
  no standing predicate any more. A start that cannot be made durable refuses
  the watch rather than opening it.
- **WHAT IT DOES NOT COVER.** A run whose call this fold never saw open has no
  start to write (the command is unknown); its terminal is still written, with
  the command unset. Precedence between the planes is last-writer-wins on the
  terminal key, exactly as `StopBash`'s own terminal already was: a shim
  terminal landing after a sidecar `EXIT=` row replaces the spool's evidence on
  that row (the tail row keeps the output).

## A shell run's detachment is CLAIMED for the sidecar (2026-09-30)

`convert/detached.ts` (`shellRunClaimEntry`); design record
`docs/protobuf-design/footer-activity-tiers.md`, landed change 4.

- **EVERY DETACHMENT FACT THE TASK STREAM STATES FOR A SHELL WRITES A CLAIM**
  (`store.v1.EntryBatch.shell_run_claims`): `task_started` in the background,
  and a `task_updated` patch that moves it. The sidecar reads the claims once
  per rescan and attributes the run's spool to the book that owns it, so a
  shell the vendor moved on its own is watched like one it announced.
- **A PATCH STATES THAT WORK MOVED, NEVER WHY**, so a shell it moves is
  announced `conversation.v1.DetachedCauseVendorMoved`; its own tool result
  restates the row with the real cause when it reaches the shim. An agent
  moved by a patch is announced `vendor_moved` too (2026-09-30), and nothing
  restates it: the agent's own tool result is the `async_launched` launch
  receipt, whose declared shape (`AgentOutput` in the SDK's `sdk-tools.d.ts`)
  has no cause field, and the vendor also moves agents on its own
  (`CLAUDE_AUTO_BACKGROUND_TASKS`). No capture holds a moved agent's frames
  (`ctrl-b-detach-of-foreground-subagent` asked too early and moved nothing).
- **A MOVE THE SHIM ASKED FOR IS THE USER'S** (ruled 2026-09-30).
  `DetachForeground` (the user's Ctrl-B) notes its unit with the fold
  (`EngineFold.noteUserDetach`, kept in `TaskKindRegistry` by spawning call)
  BEFORE calling `query.backgroundTasks(unit)`, so the patch that moves that
  unit, shell or agent, is announced `by_user`. The request is retired by
  that patch, by the unit's `task_notification`, by a `backgroundTasks`
  answer of `false` or a throw, and by the query's end; it is bounded by
  `TASK_KIND_CAPACITY` and keyed by the unit, so it can never be read by
  another unit's patch. Every other patch-moved unit stays `vendor_moved`.

## A prompt may JOIN the running turn (2026-09-30)

`TurnEngine.joinRunningTurn` (`src/engine/turn.ts`), the session's `joining`
slot (`src/engine/session.ts`); the contract is the comment on
`StartTurnRequest.join_running_turn`.

- **THE PROMPT IS PUSHED AT ONCE, WITH NO INTERRUPT**, as its own stamped
  send, and its row waits: the check of the running turn and the push are one
  synchronous step, so the join is registered before that turn can leave the
  slot or never made. With no daemon turn running the flag changes nothing.
- **THE VENDOR'S ECHO DECIDES ITS FATE.** A frame of the running turn naming
  the join among the sends it consumed means the vendor FOLDED it in at a tool
  boundary: its row is written there with `folded_into`, and it opens no turn.
  The running turn leaving the send slot first, however it leaves (its result,
  a kill, whose queued input the SDK still runs), makes the join the NEXT
  turn: it takes the slot in the same step and its row is written as that
  turn's first.
- **A DEATH OR TEARDOWN ENDS IT WITH THE REST** (`turnsOwedAnEnd`): after the
  running turn's terminal, the join is announced as its own turn and ended.
- **ONE JOIN AT A TIME.** A second is refused `turn_already_open`; the daemon
  holds the prompt instead.
- **A `vendor_note` RIDES ONLY TO THE VENDOR** (`vendorSaid`): a closing
  `<system-reminder>` block after what was said, never on the prompt row. The
  sidecar withholds agent-repl's own prompts from the transcript plane, so the
  feed never draws it.

## A resumed subagent is named by the store when this process never saw its spawn (2026-09-27)

A subagent's vendor task id (`task_started.task_id`, the `<id>` of
`agent-<id>.jsonl`) is its LOCATOR and comes back when `SendMessage` resumes
it, but the resume's call is the send, never the spawn whose `tool_use_id` IS
the agent. Two sources name the agent, in this order:

- **The fold's own join** (`TaskKindRegistry.rememberAgent`), recorded when this
  process sees the task start from its spawn. KEPT, not deleted: it is written
  synchronously by this process at the spawn, so it covers the window before the
  sidecar has discovered the transcript and its meta and booked the pairing,
  and a store outage at the resume. The store alone would refuse a resume that
  lands in that window.
- **The store** (`store.v1.GetAgentByVendorTask`, `src/store/locator.ts`), the
  sidecar's pairing, for what memory structurally cannot know: a process that
  restarted since the spawn. The shim never reads a vendor file itself.

The engine asks the store at three sites through ONE helper
(`agentFromStore` in `engine/session.ts`), which hands every answer to the fold
(`EngineFold.learnTaskAgent`) and writes the one record of the lookup: INFO
found, ERROR not-found or failed.

- **The announcement.** `EngineFold.taskAwaitingAgent` names a task whose call is
  known and is not its spawn, with no join; the engine awaits the store inside
  the serial message loop BEFORE folding, so the fold stays synchronous and no
  later message is folded ahead. A miss stays the fold's ERROR refusal, with
  `store_answer` in the record.
- **The restore.** A live handle whose recorded unit is a send is re-announced
  `created`, named by the store from the send's settled recipient (the vendor's
  id for the agent) and described by the agent's own spawn unit
  (`resumedRecipient` / `resumedAgentAnnouncement` in `store/reconcile.ts`).
- **The permission gate.** `agentFor` takes the fold's `taskAgent` first, so a
  resumed agent's ask is never credited to the send; anything nothing on the
  stream names is looked up by its id. `agentFor` answers a promise only then,
  and an ask whose lookup spans a stand-down is denied with that stand-down.

## A background subagent a network outage killed is resumed (2026-09-27)

Owner ruling after the 2026-09-27 DNS outage. `src/engine/network-resume.ts`
owns the rule, `src/engine/api-reachability.ts` the probe, and
`deliverNetworkResume` in `src/engine/session.ts` the delivery.

- **Only a network failure is resumed.** `classifyAgentFailure` reads the
  vendor's structured error class first, then a connection code, then an HTTP
  status (an answer means reachable), and the vendor's prose last. Auth,
  billing, quota, overload, rate limits, invalid requests and model errors are
  never resumed.
- **One probe loop per process**, shared by every waiting agent, on a FIXED
  five-second beat with no backoff. It exists only while something waits, and
  stand-down or the vendor query dying cancels it.
- **The probe spends no tokens.** It is a `HEAD` of the host the vendor is
  configured for: `ANTHROPIC_BASE_URL` or the default, through
  `HTTPS_PROXY`/`HTTP_PROXY` unless `NO_PROXY` exempts the host.
- **The wait is bounded.** Thirty minutes from the failure, then the wait
  gives up at ERROR and the agent keeps its failure terminal.
- **One resume per failure event.** A resumed run that fails again restarts
  the window only if the model answered for it since the resume, so a
  resume-then-fail loop cannot outlive the first window.
- **The resume is the vendor's own `SendMessage`.** The shim asks the MAIN
  agent, with a marked prompt (`src/engine/network-resume-prompt.ts`), to
  continue each named agent. The vendor resumes the same agent from its
  transcript, and the fold names it through the one resumed-agent identity
  path above (the fold's join, else the store).
- **The resume's turn is ADOPTED before the push** (`adoptTurn`, the path a
  vendor-started turn takes): a shim-minted id and a `VENDOR_STARTED` prompt
  row. A `StartTurn` arriving before the vendor's first reply is refused
  `turnAlreadyOpen` and waits behind it. It is delivered only on an idle main
  agent; an open turn or a `StartTurn` in flight makes the beat wait.
- **Every transition is one record** under `shim.engine.network_resume`, with
  `outcome` = `waiting`, `resumed`, `not_resumed`, `abandoned` or `gave_up`
  (ERROR).
- **Visible on the session stream, and only there** (visibility only,
  `docs/protobuf-design/footer-activity-tiers.md`, landed change 2).
  `NetworkResume`'s injected `emit` states the WHOLE waiting set as
  `SessionUpdate.network_resume_waits` on every change, and one
  `network_resume_outcome` (resumed, gave_up, abandoned with its reason) per
  wait that ends, at the `resumed`, `gave_up` and `abandoned` log sites;
  `not_resumed` states nothing. AN OUTCOME IS STATED ONLY FOR A WAIT THAT
  STANDS, EXACTLY ONCE: `endWait` marks the wait ended, so a delivery that
  completes after `expire()` gave its wait up is still delivered and logged
  but states nothing on the wire (an INFO record names the suppressed and
  the standing outcome), and a refail past its window, which opens no wait,
  states no `gave_up`. **A FINISHED DELIVERY ENDS ONLY THE WAIT IT WAS
  STARTED FOR** (ruled 2026-09-30): each wait has its own identity (`wait_id`,
  a per-process generation, in every record), and a wait leaves the standing
  set only by that identity (`removeWait`, which throws at ERROR on any other).
  A delivery finishing after its wait ended (expired, or superseded by a newer
  failure of the same agent while it stood) is logged `resumed` with
  `late: true` plus one INFO naming the ended and the standing wait, and it
  moves no history, removes no wait and gives nothing up, so a newer wait of
  the same agent keeps its own lifecycle and its own outcome. A superseded
  wait is ended on the wire by the new waiting edge for the same agent, which
  the restated set already expresses (ruled 2026-09-30: no outcome arm, no
  proto change). A wait's `work` is the FAILED RUN's
  `DetachedWorkId`: the notification's `tool_use_id`, else the agent's latest
  run, so after a resume it is the resuming `SendMessage`'s handle, the one the
  fold's failure terminal retired. A seam failure is recorded at ERROR and the
  wait carries on. The feed row keeps its failure.

`--fake`-only levers (a real session never honors them):

| Env | Meaning |
| --- | --- |
| `AGENT_REPL_FAKE_API_REACHABLE_GATE` | a path whose existence is the API being reachable; unset means always reachable |
| `AGENT_REPL_FAKE_NETWORK_RESUME_INTERVAL_MS` | the probe beat |
| `AGENT_REPL_FAKE_NETWORK_RESUME_WINDOW_MS` | the give-up window |

## The store writer: it never drops a row

`src/store/writer.ts` is the ONE ordered writer every row the shim produces goes
through (`write`, `writeDurable`). Its invariants:

- **NO ROW IS EVER DROPPED FOR AN OUTAGE OR A BACKLOG.** The store is the
  durable record every consumer reads from, the daemon's turn endings
  included. A failed batch is HELD in its place and replayed: on the retry
  schedule, then — once `maxAttempts` failures declare it PERSISTENT — every
  `heldRetryMs` (1s; a `--fake` backoff override's last step) for as long as
  the process lives, which bounds how long a store that came back waits to be
  noticed. The first failure
  opens the degraded window and raises `store_unreachable` (WARN); the
  persistent declaration is an ERROR naming the held keys; the window closes,
  with nothing dropped, when a write lands again (INFO). The only row that is
  ever not written is one the store REFUSES as malformed: it is named at ERROR
  and raised as `converter_defect`, and a refused multi-row batch is re-sent one
  row at a time so its well-formed neighbours still land.
- **THE BUFFER IS BOUNDED BY BACKPRESSURE, NOT EVICTION.** At either high-water
  mark (`DEFAULT_BATCH_POLICY`: 1,024 rows or 16 MiB queued) the writer opens a
  backlog episode (one WARN) and `Persistence.whenWritable()` holds the vendor
  message loop (`engine/session.ts` `runLoop`) before it reads the next
  message; the episode closes (one INFO) at the low-water marks (256 rows and
  4 MiB). A durable producer-side spill was rejected: it would make the shim a
  second durable copy of the record. The SDK keeps reading its child's stdout
  while the loop is held (it must, to answer control requests), so a pause
  moves the backlog into the SDK's own queue of raw vendor messages rather
  than into the writer.
- **BATCHES ARE BOUNDED IN ROWS, BYTES AND TIME.** An interactive batch is one
  store transaction, so its size is the store's hold on its one writer. A
  batch carries at most `maxBatchRows` (64) rows and `maxBatchBytes` (1 MiB) of
  payload — at least one row, always — and a batch that overran
  `batchTimeBudgetMs` (500ms) halves the next one's row bound, which doubles
  back once a batch lands inside a quarter of the budget. A backlog of one-row
  writes is MERGED into bounded batches; one huge write (an interrupt's cut
  calls) is SPLIT.
- **THE STORE RECEIVES EVERY ROW IN EXACTLY THE ORDER IT WAS PRODUCED, across
  every book.** A turn's terminal is the turn's last word — the fold puts even
  the calls a stop cut ahead of it — and consumers read "the terminal landed"
  as "the whole turn is recorded", subagent books included (the integration
  suite's subagent and meta-sidecar scenarios wait on exactly that). So no row
  overtakes another; the latency lever for a TURN EDGE (a prompt, an agent
  terminal) is that its batch ENDS at it, so its ack never waits on a row
  produced after it. `writeDurable` joins the same buffer: its caller is
  released at once when the store is known to be down, and the rows stay held
  — a caller must never re-queue them.
- **`flush()` never waits forever.** It resolves when the buffer is empty, or
  as soon as an attempt declares or confirms a persistent failure, counting the
  rows still held; the stand-down's exit code is decided by that count.
- Batch timing is logged per batch (`logVerbose`, or `debug` while a backlog
  episode is open) with rows, bytes, attempts, `duration_ms` and the backlog.

## Every row states its conversation place, and every entry serves it

The daemon orders every feed row by `HistoryEntryAt.place` and keys a durable
row by it across restarts, so the shim stamps a place on every row it writes and
serves one on every entry it hands out.

- **STAMPED ONCE, AT THE WRITER'S DOOR.** `store/writer.ts` places each row as
  it is enqueued (`PlaceClock`): `at_ms` is the instant the shim first held the
  fact (`PersistenceOptions.nowMs`, wall-clock milliseconds in production), and
  `ordinal` counts the rows stamped within that millisecond. The place rides the
  queued row (`PlacedEntry`), so a retried batch carries the ORIGINAL instant
  and `toStoreEntry` never reads a clock. The clock never runs backwards: a
  wall-clock step back holds the last instant and keeps counting ordinals. A
  reading that is not a positive instant is refused as a writer defect, never
  stamped. The store keeps a row's FIRST stated place, so a unit's later
  upserts (every streamed delta) move nothing.
- **SERVED BY ONE MAPPING.** `toHistoryEntryAt` (`store/reader.ts`) is the one
  conversion every serving path uses — a WatchAgent page and its live and
  retired frames, ReadHistory, StartTurn's page — and it passes the store's
  `recorded_place` / `received_place` arm through unchanged. A line a store
  served with no place (one that predates places) is served unplaced and
  traced at debug; the proto defines that as "the serving side states no
  places", which the consumer orders by its own receipt.
- **`ReadHistory.through`** is validated (a positive `at_ms`,
  `read_history.through`) and forwarded to the store's `ReadAgentPage.through`
  through `Persistence.readPageThrough`. It never vouches for the book: any book
  the store holds may be named, and one it never heard of is the typed
  `unknown_agent` refusal.
- The fake store (`test/fakes/store-server.ts`) reproduces the same contract:
  first stated place kept, an unplaced row served at its receipt instant,
  pages in descending place, `after` below the named line's current place,
  `through` with its `unknown_agent` refusal, and catch-up by write order.

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

## What landing 7 settled (2026-09-02)

- **`WatchSession` serves a oneof.** `WatchSessionResponse.frame` is
  `update | session_started`. The opening diagnostics stays FIRST AND ALONE —
  it is the readiness signal connect surfaces at the first Receive, so nothing
  may be computed ahead of it — and the session's own `SessionStarted` follows
  it, ONCE PER WATCH, on every new watch rather than only the first. That is
  what lets a daemon adopting an already-started shim (crash boot, handover)
  attach purely.
  - Identity, runtime, model, mode and catalog are the ORIGINAL facts; they are
    fixed for the session, which is why an opening states them at all.
  - `turn_in_flight` and `live_work` are recomputed to NOW. `live_work` comes
    from the STORE's live set, never the in-memory table: work re-adopted at
    `StartSession` was never seen to START in this process, so the table does
    not hold it.
  - Re-announcing is A READ AND NOTHING ELSE. The `StartSession` reconciliation
    writes terminals for work the vendor no longer holds; a daemon attaching is
    not a reason to close anybody's run, so the two share only the pure
    description step (`announceLiveWork`). A record plane the shim cannot reach
    goes out as a session fault, never as a quietly empty membership.
  - **EVERY LIVE-WORK READ IS SCOPED TO THIS SESSION.** One store serves every
    session on the host, and the `StartSession` reconciliation closes whatever
    the read answers that this vendor does not hold — so on 2026-09-23 an
    unscoped read let one workspace's start reap five running subagents of
    another. `Persistence.liveWork(session)` takes this conversation's main
    agent id and the store answers only its lineage; an empty session is
    refused as `invalid_request` before the store is asked, and a store's
    `invalid_request` is surfaced as that kind — never read as "no book yet",
    which would serve an empty set in silence.
- **`StartSessionFresh.model` is optional.** UNSET = pass no model to the SDK
  and let its own default take effect; `SessionStarted.effective_model` states
  what did. A model that IS set still has to name something — saying nothing
  and meaning to say something are different requests.
- **`UpdateAgentFailure.agent_busy`.** A prompt to a subagent whose OWN TURN is
  already running is refused with this arm, and the daemon relays it as
  `SubmitPromptError.bubble_refused{agent_busy}` — this refusal is that relay's
  ONE producer. The addressee's state is answered before the route question:
  an IDLE subagent still gets `not_deliverable` (the pinned SDK declares no
  route to a named agent), and a `local_bash` run under the same handle is not
  an agent at all.
- **`OpenAgentSessionFailure.unknown_agent`** maps to `unknown_agent`, which
  `WatchAgent` closes at the transport as `Code.NotFound` — but ONLY for a
  target the producer does not vouch for.
  - **A FRESH AGENT IS NOT AN UNKNOWN ONE.** The store's `agent` row is created
    by the agent's FIRST WRITE (`db.ensureAgent`, and `db.createSpawnedAgent`
    for a subagent's own book), while the endpoint contract has the daemon open
    the main agent's `WatchAgent` with an UNSET target the moment the session
    starts — "a fresh agent simply yields an empty page". The open therefore
    races the first write and loses on every fresh bring-up, and refusing there
    reached the daemon as `link_fault` → `open_fault{link_severed}`.
  - **THE PRODUCER IS THE ARBITER** (`SessionContext.knowsAgent`). It rides down
    into the record plane as `openAgentPage`'s `known` predicate (a shell
    run needs no such predicate: its start is written before it is announced):
    - vouched for → the refusal is WAITED OUT: an empty opening page now, and
      the tail stood on the book's first row (woken by the write that lands it,
      `Reader.noteAgentRows`). `ReadHistory` answers the same empty page rather
      than refusing, which is what a keep-alive-only session's book is.
    - not vouched for → the store's refusal stands, as `Code.NotFound`.
    - any other refusal (`storage_failure`) is untouched.
  - The empty-book refusal in `watchAgent` also stays, for a store that serves
    an empty page instead of refusing. Retire both — and `knowsAgent` with them
    — only once an id's existence is answerable without the producer.
- **A TAIL'S CONCLUSION ASKS "HAVE I SERVED THIS POINTER", AND THE ANSWER IS A
  SET, NEVER THE LAST POINTER.** The teardown concludes every open `WatchAgent`
  tail through its book's HEAD and waits, bounded by
  `WATCHER_CONCLUSION_BUDGET_MS`. The store streams an UPSERT of an old row at
  its ORIGINAL pointer, so the newest pointer a tail has handed over walks
  BACKWARD whenever a line already read past is updated — and a conclusion
  through the head then named a row that went out earlier and will never be sent
  again. The tail stood on it and `KillSession` spent the whole budget. Both the
  real session and the deferred wrapper keep the set of pointers they served;
  a pointer names a POSITION and an upsert reuses it, so the set is bounded by
  the book's LINES, not by the frames written to them. The pointers stay
  OPAQUE — the shim compares them, never parses them, so there is no "newer
  than" to lean on.

## Verification

```bash
npm run lint          # eslint, type-aware, over src/, test/, scripts/ and the root configs
npm run typecheck     # tsc over src/, test/, scripts/ and the generated stubs
npm test              # vitest
npm run coverage      # vitest with istanbul coverage over authored src/**/*.ts
npm run coverage:verify  # prove the per-file numbers are still a measurement
npm run build         # esbuild -> dist/main.js (the entry the daemon spawns)
npm run smoke         # spawn and dial dist/main.js for real (needs a build first)
# test, coverage and smoke (and their pre-hooks) run through ../../../bin/background.sh;
# the vitest configs refuse a run without it, so prefix any bare `npx vitest` with it
```

Dependencies come from the SELF-HEALING shared node store, exactly as the
webapp's do: `node_modules` links `~/.cache/agent-repl/node-store/shim-<lockhash>`,
every `pre*` hook runs `../../../bin/ensure-deps.sh`, and an entry that exists
but fails `npm ls --depth=0` is repaired in place under its own lock and swapped
in atomically, never reinstalled through the link. `webapp/AGENTS.md`
("Dependencies come from a SELF-HEALING shared store") is the full account.

- `npm run lint` is TYPE-AWARE and is not a style pass: it reads the same
  program `tsc` does, and the rules it adds on top are the ones that catch what
  `tsc` cannot see — a floating promise, a `switch` with neither a missing arm's
  case nor a default, an `any` that spreads through an object literal. Its rule
  set and every deliberate omission are argued inline in `eslint.config.js`;
  disagree with a rule there, in one place, rather than with an inline disable.
  An inline disable is legitimate when it carries a `--` reason a reviewer would
  accept, and unused ones fail the run.
- `AGENT_REPL_FORBID_VENDOR_CALLS=1` in every shell you run tests in.
- `modules/app/agent-repl/bin/test-all.sh` (from the repository root) runs every
  tracked suite across the module.
- Maintain at least 90% statement coverage. Never reduce the measured baseline,
  and add focused tests for every critical branch and every error path changed.
  The baseline is **98.61% of statements**; it reads lower than the 99.34% this
  package used to report because the provider changed, not because the tests
  did.
- COVERAGE IS ISTANBUL, and `npm run coverage:verify` is what keeps it honest.
  `@vitest/coverage-v8@2.1.9` merges each test-file window's raw V8 coverage
  with `mergeProcessCovs` before remapping it through the source maps, so a
  module compiled in more than one window loses one window's counts and which
  window survives depends on which test files shared a process: two runs of this
  suite differing only in test-FILE order reported different branch counts for
  `src/convert/hooks.ts`, `src/convert/stream-events.ts`,
  `src/convert/tools/cron.ts`, `src/convert/tools/unmodeled.ts` and
  `src/engine/turn.ts`. v8 was also scoring five type-only modules
  (`src/proto.ts`, `src/proto-conversation.ts`, `src/proto-shim.ts`,
  `src/proto-store.ts`, `src/fake/scenario.ts`) at 100% of nothing; istanbul
  drops them, because a file with no executable statement has no coverage.
  `npm run coverage:verify` runs this package's own coverage command three times
  — once naturally ordered, twice with the test files shuffled under fixed seeds
  — and fails if any file's counts move. Run it after any change to the
  provider, its version, or this config's isolation, which the istanbul provider
  needs and vitest gives by default.
- `modules/app/agent-repl/bin/report-logging-density.sh shim` is a rough review
  aid, not semantic coverage: audit critical branches and errors directly even
  when the ratio rises.

### Timeout bounds, and why they are this tight

Every wait/timeout bound in these two suites is a small multiple (about 3x)
of the observed healthy max for that suite, never a round "safe" guess. A
suite hitting its bound is HUNG, not merely slow, and should be diagnosed as
a hang — never fixed by raising the number back up.

| Bound | Where | Value | Observed healthy max it is sized from |
| --- | --- | --- | --- |
| `testTimeout` | `vitest.config.ts` (unit) | 2,500ms | ~640ms (`test/log.test.ts`, bootstrap-stderr logging) |
| `hookTimeout` | `vitest.config.ts` (unit) | 2,500ms | same — hooks here are `setupFiles` only |
| `teardownTimeout` | `vitest.config.ts` (unit) | 2,500ms | same |
| `testTimeout` | `vitest.integration.config.ts` | 5,000ms | ~1.55s, the CONTENDED max across 310 tests (~645ms on a quiet machine: `test/integration/session.test.ts`, a forced `KillSession` spending the whole scaled watcher-conclusion budget). Sized from the contended figure because this suite always runs its seven files in parallel, each spawning a real node process — contention is its normal condition, not an anomaly to size below and flake on |
| `hookTimeout` | `vitest.integration.config.ts` | 5,000ms | shares the test budget — the only hook is `afterEach(cleanupShims)` (SIGKILL + temp-dir removal), far cheaper than any test body |
| `teardownTimeout` | `vitest.integration.config.ts` | 10,000ms | same reasoning, vitest's own default |
| the fs.watch re-drain interval | `test/integration-support/redrain.ts` | 20ms | deliberate level-then-edge guard against a dropped FSEvents notification, not a success path — keep as-is, do not tighten further |
| the "stops promptly" hang guards | `test/store/reader.test.ts` (2 sites) | 300ms | the real settle is sub-millisecond; this only bounds how long a genuine hang costs before failing with a clear "hung" value |

### The production windows a test would otherwise RIDE

Three constants are LAST-RESORT bounds that only a test arranging the
pathological case ever actually spends. Riding them cost the suite 35 of its 80
seconds of test time and made those eight scenarios the slowest here, while
proving nothing the same scenario at a smaller bound does not prove — what they
assert is an ordering and an attempt count, neither of which is a function of
how long the process idles.

Each therefore has a `--fake`-ONLY environment override, in the shape
`AGENT_REPL_FAKE_KEEPALIVE_INTERVAL_MS` already established: REFUSED for a real
session, refused when malformed, every refusal reported. **The production
defaults are unchanged, and a real session cannot reach any of them.**

| Constant | Where | Production default | Override | Harness value |
| --- | --- | --- | --- | --- |
| `WATCHER_CONCLUSION_BUDGET_MS` | `src/engine/session.ts` | 5,000ms | `AGENT_REPL_FAKE_WATCHER_CONCLUSION_BUDGET_MS` | 500ms |
| `EXIT_QUIET_BUDGET_MS` | `src/main.ts` | 5,000ms | `AGENT_REPL_FAKE_EXIT_QUIET_BUDGET_MS` | 500ms |
| `DEFAULT_RETRY_POLICY.backoffMs` | `src/store/persistence.ts` | `[50, 200, 800, 3000]` | `AGENT_REPL_FAKE_STORE_BACKOFF_MS` | `5,20,80,300` |

`test/integration-support/harness.ts` sets all three in every spawn's standard
env; a test that wants a production window back overrides it per-spawn through
`SpawnShimOptions.env`, which layers over that standard env.

ONLY THE WAITING IS OVERRIDABLE. The retry override reaches `backoffMs` alone —
`maxAttempts` stays pinned to `DEFAULT_RETRY_POLICY`, and the batch and backlog
bounds to `DEFAULT_BATCH_POLICY`; none is reachable from the environment at all,
precisely so this cannot become a way to weaken the attempt-count and
never-drop assertions it exists to keep fast. Keep it that way.

The forced-kill scenarios fell from ~5.14s to ~0.64s and the six store-outage
scenarios from ~4.2s to ~0.55s each; the suite went from 30.9s to 11.6s, and its
slowest test is now 645ms. **No test in either suite is above a second.** A new
test that takes longer than that is riding a window — find it and scale it here,
never by weakening what the test asserts.
