/**
 * fake/index.ts — the mocked vendor behind `--fake`.
 *
 * # What this is, and what it deliberately is not
 *
 * `createFakeQuery` builds a `QueryLike`: the same surface the real `query()`
 * offers, answered from a script. The REAL shim runs over it unchanged — no
 * branch anywhere in `src/` asks whether it is talking to the mock — so every
 * offline test exercises production code and only the vendor is substituted.
 *
 * It is NOT a simulator. It decides nothing, retries nothing and infers
 * nothing: each turn's prompt selects a `Scenario`, and the scenario replays a
 * sequence of shapes harvested from `testdata/corpus` and the pinned
 * `sdk.d.ts`. That is what makes it usable as an oracle — a simulator's output
 * is an opinion, a replay's output is evidence.
 *
 * # It writes files, because half the system reads files
 *
 * Every scenario writes what the real binary would have written, where it would
 * have written it, so the REAL sidecar ingests a `--fake` run: the session
 * transcript, subagent transcripts with their `.meta.json`, and detached-work
 * spools terminated by `EXIT=<code>`. See `vendor-files.ts`.
 *
 * # The one type cast, and why it lives here
 *
 * `emit` casts its argument to `SdkMessage` exactly once. The alternative is to
 * construct `BetaMessage` and `BetaRawMessageStreamEvent` values literally,
 * which drags the Anthropic SDK's whole content-block union into every scenario
 * and — worse — makes the corpus unusable as the source of truth, because real
 * records carry fields (`stop_details`, `inference_geo`, `iterations`,
 * `caller`) the declarations do not name. Observed shapes beat declared types
 * (the fanout plan's evidence ranking), so the shapes come from the corpus and
 * this is the single documented place where that is admitted.
 */
import { randomBytes } from "node:crypto";
import { existsSync, mkdirSync, watch, writeFileSync, type FSWatcher } from "node:fs";
import { dirname } from "node:path";

import { bindLog } from "../log.js";
import type {
  AccountInfoLike,
  AccountUsageLike,
  AgentInfoLike,
  CanUseToolLike,
  ContextUsageLike,
  InitializationResultLike,
  InterruptReceipt,
  McpServerStatusLike,
  ModelInfoLike,
  PermissionModeLike,
  QueryLike,
  SdkMessage,
  SdkUserMessage,
  SlashCommandLike,
} from "../sdk/types.js";
import {
  FAKE_ACCOUNT_INFO,
  FAKE_AGENTS,
  FAKE_COMMANDS,
  FAKE_DEFAULT_MODEL,
  FAKE_INITIALIZATION_RESULT,
  FAKE_MCP_SERVERS,
  FAKE_MCP_SERVERS_HEALTHY,
  FAKE_MODELS,
  fakeAccountUsage,
  fakeContextUsage,
} from "./catalogs.js";
import { AsyncQueue } from "./queue.js";
import { selectScenario } from "./registry.js";
import type {
  AccountUsageArm,
  AssistantEmission,
  AssistantOptions,
  FakeBlock,
  LiveTask,
  ResultSpec,
  ScenarioContext,
  ToolCall,
  ToolResultOptions,
} from "./scenario.js";
import { FAKE_CLI_VERSION, FAKE_REASONING_SIGNATURE, VendorFiles } from "./vendor-files.js";

/**
 * IS THIS TASK A SHELL RUN? The vendor's own distinction, read off the id.
 *
 * `b<hex>` is a background shell run and `a<hex>` a local agent — the shapes
 * the sidecar already names spool and transcript paths from. It matters here
 * because the two spools are DIFFERENT FILES IN KIND: a shell spool is
 * incremental bytes terminated by `EXIT=<code>`, and an agent spool is the
 * agent's own JSONL with no terminator ever.
 */
function isShellTaskId(taskId: string): boolean {
  return taskId.startsWith("b");
}

const LOGGER = bindLog({ component: "shim-fake", operation: "shim.fake.query" });

export {
  FAKE_ACCOUNT_INFO,
  FAKE_AGENTS,
  FAKE_COMMANDS,
  FAKE_MCP_SERVERS,
  FAKE_MODELS,
} from "./catalogs.js";
export { FAIL_TURN_MARKER } from "./registry.js";

/**
 * THE TURN GATE. A test that must arrange "this turn is STILL RUNNING while
 * something else happens" cannot do it by winning a race with the mock, which
 * answers in microseconds. `AGENT_REPL_FAKE_TURN_GATE` names a path and
 * `AGENT_REPL_FAKE_TURN_GATE_TEXT` the prompt that parks on it: a turn carrying
 * exactly that prompt does not begin emitting until the path exists, so the
 * test decides when the turn ends AND the turn still ends the ORDINARY way,
 * rather than through an interrupt that would change what it is.
 *
 * The wait is LEVEL-then-EDGE: the file is checked first and the watch is only
 * a wakeup, so a gate released before the turn started is not a missed edge.
 *
 * The edge alone is NOT ENOUGH, though. On macOS `fs.watch` rides FSEvents,
 * which coalesces and can drop a notification outright; when it does, the wait
 * does not fail, it HANGS, and the park shows up as an unrelated test timeout
 * (the same flake bucket `test/integration-support/redrain.ts` exists for). So
 * a coarse, unref'd re-check runs alongside the watcher and re-tests the same
 * LEVEL condition. The watcher stays the fast path and settles in microseconds;
 * the re-check is only there so a lost event costs milliseconds instead of the
 * caller's whole budget.
 *
 * THE DETACHED-WORK GATE is the same idea, one turn later. `bash-detach`
 * concludes its turn and then writes its whole spool across four scheduler
 * ticks, so detached work lives for microseconds after the foreground frees
 * up — no consumer can arrange "detached work is still running while the
 * foreground is free" against that. `AGENT_REPL_FAKE_DETACH_GATE` names a
 * path: once set, `bash-detach` appends its first spool line and then PARKS
 * until that path exists before appending the rest and terminating the spool,
 * using the identical level-then-edge wait `awaitTurnGate` uses (both share
 * {@link awaitGateFile}). There is no interrupt exit here — the turn that
 * started the detached work has already concluded, so there is nothing left
 * to interrupt.
 */
export const TURN_GATE_PATH_ENV = "AGENT_REPL_FAKE_TURN_GATE";
export const TURN_GATE_TEXT_ENV = "AGENT_REPL_FAKE_TURN_GATE_TEXT";
export const DETACH_GATE_PATH_ENV = "AGENT_REPL_FAKE_DETACH_GATE";
/** Roots the spool tree; the default matches the vendor's `/tmp/claude-<uid>`. */
export const SPOOL_ROOT_ENV = "AGENT_REPL_FAKE_SPOOL_ROOT";

/**
 * The control verbs the mocked vendor REFUSES, comma-separated.
 *
 * Several shim.v1 refusals are the shim faithfully relaying a vendor that said
 * no — `SetSessionModel{vendor_refused}`, `SetSessionPermissionMode
 * {vendor_refused}`, `StartSession{vendor_start_failed}` — and a mock whose
 * control verbs always succeed leaves every one of those branches asserted
 * nowhere. There is no scenario prompt that could reach them: they are answers
 * to CONTROL CALLS, not to a turn, so the lever has to be an environment knob
 * the whole process reads rather than a prompt.
 *
 * Recognized verbs: `start` (createFakeQuery itself throws), `start-once`,
 * `start-eof`, `start-error-result`, `start-auth-error`, `set_model`,
 * `set_permission_mode`. Anything else is a refusal to start rather than a
 * silently ignored knob.
 *
 * `start-auth-error` is `start-error-result`'s AUTHENTICATION shape: the vendor
 * refuses the opening with a credential rejection carrying `api_error_status`
 * 401, which is what the shim's start-path auth diagnostic reads. It exists so
 * the diagnostic record can be driven at start without a real credential.
 *
 * `start-eof` and `start-error-result` are the two ways a query that WAS
 * created still never opens a session — the child exits, or the vendor answers
 * the opening with an error result — and they exist because the grounded
 * failure (2026-09-13) was neither a refused `query()` nor a blocking hook: the
 * vendor simply stopped, and the shim sat out its whole init bound.
 */
const REFUSE_ENV = "AGENT_REPL_FAKE_REFUSE";

/** The control verbs {@link REFUSE_ENV} may name. */
const REFUSABLE = new Set([
  "start",
  "start-once",
  "start-eof",
  "start-error-result",
  "start-auth-error",
  "set_model",
  "set_permission_mode",
]);

/**
 * Where `start-once` records that it has spent its one refusal.
 *
 * `start` refuses forever, which proves the arm but can never show what happens
 * AFTER the condition clears — and "a failed start leaves the engine as it
 * found it" is only observable when the retry actually succeeds. `start-once`
 * refuses the FIRST start under an account root and allows every later one,
 * which is exactly the daemon's story: something was wrong, it was fixed, and
 * the conversation is served.
 *
 * THE MARK IS A FILE, NOT A PROCESS COUNTER, and it has to be: the daemon STOPS
 * the shim of a failed start, so the retry reaches a NEW shim process. A
 * counter in module state would be zero again in that process, refuse a second
 * time, and the recovery half of the arm could never be reached. The account
 * root is the scope because it is what one world owns: a suite gives each world
 * its own `CLAUDE_CONFIG_DIR`, so one world's spent refusal cannot reach
 * another's, and every shim of THAT world shares the one mark.
 */
const START_ONCE_MARK = ".agent-repl-fake-start-once";

/**
 * Spend `start-once`'s single refusal, or report it already spent.
 *
 * A mark that cannot be written is a HARD failure rather than a second
 * refusal: a lever that silently degrades into `start` would make the recovery
 * half of the arm look broken and say nothing about why.
 */
function spendStartOnce(configDir: string): boolean {
  const mark = `${configDir}/${START_ONCE_MARK}`;
  if (existsSync(mark)) return false;
  mkdirSync(configDir, { recursive: true });
  writeFileSync(mark, "spent\n", "utf8");
  return true;
}

/** Which control verbs this process was told to refuse. */
function refusedVerbs(env: NodeJS.ProcessEnv = process.env): ReadonlySet<string> {
  const raw = env[REFUSE_ENV] ?? "";
  const named = raw
    .split(",")
    .map((verb) => verb.trim())
    .filter((verb) => verb !== "");
  for (const verb of named) {
    if (!REFUSABLE.has(verb)) {
      throw new Error(
        `${REFUSE_ENV}: ${JSON.stringify(verb)} is not a refusable control verb (expected one of ${[...REFUSABLE].join(", ")})`,
      );
    }
  }
  return new Set(named);
}

/**
 * WHEN THE MOCKED VENDOR ANNOUNCES ITS `system:init`.
 *
 * `at-start` (the default) is the OLDER vendor's shape and the shape every
 * captured corpus was recorded under: init leads, and the rest of the session
 * follows it. `after-first-turn` is what claude 2.1.220 and 2.1.270 ACTUALLY
 * do when driven as this shim drives them (grounded 2026-09-13): the child
 * answers control requests within ~300ms and emits no init at all until a
 * first user message reaches it.
 *
 * The lever exists because the two shapes exercise different halves of the
 * start contract — the live signal settling a start with no init in sight, and
 * the init facts being applied to an ALREADY-STARTED session — and neither can
 * be reached from a prompt, because the timing is decided before any prompt
 * exists.
 */
const INIT_TIMING_ENV = "AGENT_REPL_FAKE_INIT_TIMING";

/** The values {@link INIT_TIMING_ENV} may take. */
const INIT_TIMINGS = new Set(["at-start", "after-first-turn"]);

/** When this process's mocked vendor announces its init. */
function initTiming(env: NodeJS.ProcessEnv = process.env): string {
  const raw = (env[INIT_TIMING_ENV] ?? "at-start").trim();
  const named = raw === "" ? "at-start" : raw;
  if (!INIT_TIMINGS.has(named)) {
    throw new Error(
      `${INIT_TIMING_ENV}: ${JSON.stringify(named)} is not a recognized init timing (expected one of ${[...INIT_TIMINGS].join(", ")})`,
    );
  }
  return named;
}

/** How often a parked gate re-checks when no edge has arrived. */
const GATE_REDRAIN_INTERVAL_MS = 20;

/** One outstanding wait on {@link awaitGateFile}, cancellable before it settles. */
interface GateFileWait {
  /** Resolves once `path` exists. */
  readonly promise: Promise<void>;
  /** Tear down the watcher and re-check without resolving. A no-op once settled. */
  cancel(): void;
}

/**
 * THE SHARED FILE-WAIT CORE behind both gates. Resolves once `path` exists,
 * LEVEL-then-EDGE: the file is checked first and the watch is only a wakeup,
 * so a gate released before the wait started is not a missed edge.
 *
 * The edge alone is NOT ENOUGH, though. On macOS `fs.watch` rides FSEvents,
 * which coalesces and can drop a notification outright; when it does, the wait
 * does not fail, it HANGS, and the park shows up as an unrelated test timeout
 * (the same flake bucket `test/integration-support/redrain.ts` exists for). So
 * a coarse, unref'd re-check runs alongside the watcher and re-tests the same
 * LEVEL condition. The watcher stays the fast path and settles in microseconds;
 * the re-check is only there so a lost event costs milliseconds instead of the
 * caller's whole budget. Both the watcher and the re-check are unref'd, so a
 * wait parked here can never by itself hold the process open.
 *
 * ONE HELPER, TWO CALLERS, so the turn gate and the detach gate cannot drift:
 * `awaitTurnGate` races this against an interrupt; the detach gate has no
 * interrupt exit and awaits it directly.
 */
function awaitGateFile(path: string): GateFileWait {
  if (existsSync(path)) return { promise: Promise.resolve(), cancel: () => {} };
  let settled = false;
  let watcher: FSWatcher | null = null;
  let redrain: NodeJS.Timeout | null = null;
  const teardown = (): void => {
    watcher?.close();
    if (redrain !== null) clearInterval(redrain);
  };
  const promise = new Promise<void>((resolve) => {
    const settle = (): void => {
      if (settled) return;
      settled = true;
      teardown();
      resolve();
    };
    const release = (): void => {
      if (settled || !existsSync(path)) return;
      settle();
    };
    watcher = watch(dirname(path), release);
    // Unref'd on every platform that offers it, so the watcher can never by
    // itself hold the process open.
    watcher.unref?.();
    // The backstop for an FSEvents notification that never arrives. Unref'd, so
    // it can never by itself hold the process open.
    redrain = setInterval(release, GATE_REDRAIN_INTERVAL_MS);
    redrain.unref();
    // The gate can also be released between the check above and the watch's
    // installation, which no edge would then report.
    release();
  });
  return {
    promise,
    cancel: () => {
      if (settled) return;
      settled = true;
      teardown();
    },
  };
}

/**
 * Park a turn on its gate until the gate appears OR the turn is interrupted.
 *
 * TWO WAYS OUT, NOT ONE. A parked turn is a live turn: `interrupt()` is exactly
 * the input a consumer sends to a turn that is taking too long, and a gate that
 * only ever settled on the file appearing swallowed the kill outright — the
 * mocked vendor logged "fake turn PARKED on its gate", the shim reported a turn
 * stopped, and no terminal was ever emitted for it. `awaitInterrupt` is the
 * same rendezvous the scenarios use, so an interrupt during the park is
 * observed by the very promise the interrupt resolves.
 */
function awaitTurnGate(text: string, awaitInterrupt: () => Promise<void>): Promise<void> {
  const path = process.env[TURN_GATE_PATH_ENV] ?? "";
  const gateText = process.env[TURN_GATE_TEXT_ENV] ?? "";
  if (path === "" || gateText === "" || text.trim() !== gateText.trim()) return Promise.resolve();
  if (existsSync(path)) return Promise.resolve();
  LOGGER.debug({ gate_path: path }, "fake turn PARKED on its gate");
  const gate = awaitGateFile(path);
  return new Promise<void>((resolve) => {
    let settled = false;
    const settle = (why: string): void => {
      if (settled) return;
      settled = true;
      gate.cancel();
      LOGGER.debug({ gate_path: path, released_by: why }, "fake turn RELEASED by its gate");
      resolve();
    };
    // THE KILL PATH. Resolved by `interrupt()`, whatever the gate does; the
    // caller then reads `interrupted` and emits the interrupt terminal, which
    // is the whole point of settling here at all.
    void awaitInterrupt().then(() => {
      settle("interrupt");
    });
    void gate.promise.then(() => {
      settle("gate");
    });
  });
}

/**
 * Park detached work on its gate until the gate appears.
 *
 * NO INTERRUPT EXIT. The turn that started this detached work has already
 * concluded — there is nothing live for an `interrupt()` to stop — so unlike
 * {@link awaitTurnGate} this is a single wait, not a race.
 */
function awaitDetachGate(): Promise<void> {
  const path = process.env[DETACH_GATE_PATH_ENV] ?? "";
  if (path === "" || existsSync(path)) return Promise.resolve();
  LOGGER.debug({ gate_path: path }, "fake detached work PARKED on its gate");
  return awaitGateFile(path).promise.then(() => {
    LOGGER.debug({ gate_path: path, released_by: "gate" }, "fake detached work RELEASED by its gate");
  });
}

/** The default spool root when the env override is absent. */
export function defaultSpoolRoot(): string {
  const uid = typeof process.getuid === "function" ? process.getuid() : 0;
  return `/tmp/claude-${uid}`;
}

/** What the entrypoint knows about a fake session when it builds one. */
export interface FakeQueryOpts {
  /** The workspace directory; every record's `cwd` and the slug's source. */
  readonly cwd?: string;
  /** `CLAUDE_CONFIG_DIR` — the account root the transcript tree hangs under. */
  readonly configDir?: string;
  /** The vendor session id the mock reports, pre-minted on a fresh start. */
  readonly sessionId: string;
  /** The vendor session being continued, when this is a resume. */
  readonly resume?: string;
  /**
   * The transcript record the resume is REWOUND TO, when the shim named one.
   *
   * The keep-alive rewind's whole claim is that the resumed session stops at a
   * particular record, and until now nothing on the vendor side recorded which
   * uuid was named — the shim's own log said what it INTENDED, which is not
   * evidence that the value reached the vendor at all. The mock records it (see
   * the `resume_session_at` field of the mock's session-start record) so that claim is assertable.
   */
  readonly resumeSessionAt?: string;
  /** Mint a uuid. Injectable so goldens are stable. */
  readonly newUuid: () => string;
  /** Milliseconds since the epoch. Injectable so goldens are stable. */
  readonly nowMs?: () => number;
  /** The model the session starts on. */
  readonly model?: string;
  /** The gate's mode at start. */
  readonly permissionMode?: PermissionModeLike;
  /** Where the spools go; defaults to the env override, then `/tmp/claude-<uid>`. */
  readonly spoolRoot?: string;
  /** The branch every transcript record reports. */
  readonly gitBranch?: string;
  /** Ends the fake stream when the session stands down. */
  readonly abortSignal?: AbortSignal;
}

/**
 * The usage object every fake API response reports, shaped like the corpus's.
 *
 * `contextTokens`, when a scenario states one, is the total the cold gate
 * reads back (`cache_read + cache_creation + input`); the cache READ is the
 * part that absorbs the difference, because that is what a re-read pays for.
 */
function fakeUsage(contextTokens?: number): Record<string, unknown> {
  const cacheRead = contextTokens === undefined ? 21_755 : contextTokens - 10 - 3_224;
  return {
    input_tokens: 10,
    cache_creation_input_tokens: 3_224,
    cache_read_input_tokens: cacheRead,
    cache_creation: { ephemeral_5m_input_tokens: 0, ephemeral_1h_input_tokens: 3_224 },
    output_tokens: 36,
    output_tokens_details: { thinking_tokens: 30 },
    server_tool_use: { web_search_requests: 0, web_fetch_requests: 0 },
    service_tier: "standard",
    inference_geo: "not_available",
    speed: "standard",
    iterations: [
      {
        input_tokens: 10,
        output_tokens: 36,
        cache_read_input_tokens: cacheRead,
        cache_creation_input_tokens: 3_224,
        cache_creation: { ephemeral_5m_input_tokens: 0, ephemeral_1h_input_tokens: 3_224 },
        type: "message",
      },
    ],
  };
}

/** The `modelUsage` row a result reports for one model. */
function fakeModelUsage(model: string): Record<string, unknown> {
  return {
    inputTokens: 10,
    outputTokens: 36,
    cacheReadInputTokens: 21_755,
    cacheCreationInputTokens: 3_224,
    webSearchRequests: 0,
    costUSD: 0.0088135,
    contextWindow: 200_000,
    maxOutputTokens: 32_000,
    canonicalModel: model,
    provider: "firstParty",
  };
}

export function createFakeQuery(
  prompt: AsyncIterable<SdkUserMessage>,
  canUseTool: CanUseToolLike,
  opts: FakeQueryOpts,
): QueryLike {
  const refuse = refusedVerbs();
  // READ AND VALIDATED HERE, beside the refusals, so an unrecognized value is a
  // refusal to start rather than a knob that turns out to have done nothing
  // several seconds into a session.
  const initAfterFirstTurn = initTiming() === "after-first-turn";
  const cwd = opts.cwd ?? process.cwd();
  const configDir =
    opts.configDir ?? process.env.CLAUDE_CONFIG_DIR ?? `${process.env.HOME ?? ""}/.claude`;
  if (refuse.has("start")) {
    // The vendor could not be started at all. StartSession turns this into
    // `vendor_start_failed`, which is otherwise unreachable behind `--fake`.
    throw new Error("the mocked vendor was told to refuse to start");
  }
  if (refuse.has("start-once") && spendStartOnce(configDir)) {
    throw new Error("the mocked vendor was told to refuse the FIRST start only");
  }
  const spoolRoot = opts.spoolRoot ?? process.env[SPOOL_ROOT_ENV] ?? defaultSpoolRoot();
  const nowMs = opts.nowMs ?? ((): number => Date.now());

  // The session uuid every message currently reports. MUTABLE because the
  // vendor mutates it: a `/clear` retires the transcript identity mid-stream
  // and mints a new one, and every message after that point carries the new
  // uuid. On resume it starts at the RESUMED uuid, which the real CLI reports
  // on every message of a continued session — not only on its init.
  let sessionUuid = opts.resume ?? opts.sessionId;
  let model = opts.model ?? FAKE_DEFAULT_MODEL;
  let permissionMode: PermissionModeLike = opts.permissionMode ?? "default";
  let accountUsageArm: AccountUsageArm = "available";
  let mcpArm: "all" | "healthy" = "all";
  // Whether `getContextUsage()` answers a growing occupancy. OFF by default: the
  // ordinary session's answer moves only with the turn counter, and a mock that
  // always drifted would make a consumer's change detection untestable in the
  // steady case.
  let contextUsageDrift = false;
  // The session's fast-mode state, which `init` and every `result` report.
  let fastModeState: "on" | "off" | "cooldown" = "off";
  let fastModeDisabledReason: string | undefined = "preference";
  let turn = 0;
  let interrupted = false;
  let resultEmitted = false;
  let promptId = opts.newUuid();
  let releaseInterrupt: (() => void) | null = null;

  const out = new AsyncQueue<SdkMessage>();
  opts.abortSignal?.addEventListener("abort", () => out.end(), { once: true });

  const files = new VendorFiles({
    configDir,
    cwd,
    spoolRoot,
    sessionId: sessionUuid,
    gitBranch: opts.gitBranch ?? "HEAD",
  });

  // THE SPAWN TAG THAT MAKES A FAKE API MESSAGE ID GLOBALLY UNIQUE. The id used
  // to be `msg_fake_<turn>` off a per-query counter, which made it unique per
  // QUERY and nothing more — but a session outlives its shim, and a respawn's
  // first turn re-minted the same id under the SAME session, which the store
  // correctly refuses as a divergent duplicate. Per CREATED QUERY is the right
  // scope: two queries in one process must not share it either.
  const spawnTag = randomBytes(4).toString("hex");
  let messageCounter = 0;
  let toolCounter = 0;

  const nowIso = (): string => new Date(nowMs()).toISOString();

  const emitWithUuid = (uuid: string, message: Record<string, unknown>): void => {
    if (out.isEnded) return;
    out.push({ session_id: sessionUuid, ...message, uuid } as unknown as SdkMessage);
  };
  const emit = (message: Record<string, unknown>): void => emitWithUuid(opts.newUuid(), message);

  /**
   * The call a subagent's stream events belong to, while one is being emitted.
   *
   * A SUBAGENT'S STREAM DELTAS CARRY THE SAME ATTRIBUTION AS ITS MESSAGE. The
   * fold books a frame by `parent_tool_use_id`, and emitting the deltas with a
   * null one put a subagent's OPEN block on the main agent's book and its
   * SETTLED block on the subagent's — the same upsert key landing in two books,
   * which is what a page then served back to the wrong reader.
   */
  let streamParentToolUseId: string | null = null;

  const emitStream = (event: Record<string, unknown>): void =>
    emit({
      type: "stream_event",
      event,
      parent_tool_use_id: streamParentToolUseId,
      // A fake message_start models the same SDK timing contract as a live one,
      // so the real ephemeral-correlation path stays exercised.
      ...(event.type === "message_start" ? { ttft_ms: 1 } : {}),
    });

  const mintMessageId = (): string => `msg_fake_${spawnTag}_${++messageCounter}`;
  const mintToolUseId = (): string => `toolu_fake_${spawnTag}_${++toolCounter}`;
  // The vendor's shell task ids are 9-character base36 (`b86pl7ir1`) and its
  // agent ids 17-character hex (`a0cbd94e5da2d662d`). The shapes matter: the
  // sidecar names a spool from them and an agent transcript file from the agent
  // id, so a mock that minted uuids would produce paths no real tree contains.
  const mintShellTaskId = (): string =>
    `b${opts.newUuid().replace(/[^a-z0-9]/g, "").slice(0, 8).padEnd(8, "0")}`;
  const mintAgentTaskId = (): string =>
    `a${opts.newUuid().replace(/[^a-f0-9]/g, "").slice(0, 16).padEnd(16, "0")}`;

  const liveTasks = new Map<string, LiveTask>();

  // A RESUMED VENDOR STILL HOLDS WHAT IT WAS RUNNING. Backgrounding a shell is
  // exactly the request that it outlive the turn, and a shim that died without
  // stopping it did not end it — so the run is adopted back from its own
  // evidence on disk: an unterminated spool plus the transcript line naming the
  // call that launched it. Without this the mock says "gone" for every run a
  // resume inherits, and the shim's re-adoption path could never be exercised.
  //
  // SILENTLY, with no `task_started`: the shim's reconciliation ASKS
  // (`backgroundTasks`) precisely because a revived process re-announces on its
  // own schedule, and a start replayed here would be a second announcement of
  // work that never restarted.
  if (opts.resume !== undefined) {
    for (const run of files.survivingShellRuns()) {
      liveTasks.set(run.taskId, {
        taskId: run.taskId,
        toolUseId: run.toolUseId,
        kind: "local_bash",
        description: run.description,
        backgrounded: true,
      });
      LOGGER.debug(
        { claude_session_id: sessionUuid, task_id: run.taskId, tool_use_id: run.toolUseId },
        "fake vendor resumed a session that still holds a running shell; re-adopted from its unterminated spool",
      );
    }
  }

  const blockPayload = (block: FakeBlock): Record<string, unknown> => {
    switch (block.type) {
      case "text":
        return { type: "text", text: block.text };
      case "thinking":
        return { type: "thinking", thinking: block.thinking, signature: block.signature };
      case "tool_use":
        // `caller` rides every real tool_use block; the corpus has no sample
        // without it, so omitting it would be the invented shape.
        return {
          type: "tool_use",
          id: block.id,
          name: block.name,
          input: block.input,
          caller: { type: "direct" },
        };
      case "fallback":
        return { type: "fallback", from: block.from, to: block.to };
    }
  };

  /**
   * Run another agent's emission in the middle of an open block, then restore
   * THIS response's stream attribution: the nested emission set its own and
   * cleared it at its `message_stop`.
   */
  const interleaved = (midBlock: (() => void) | undefined): void => {
    if (midBlock === undefined) return;
    const own = streamParentToolUseId;
    midBlock();
    streamParentToolUseId = own;
  };

  const emitBlockStream = (block: FakeBlock, index: number, midBlock?: () => void): void => {
    switch (block.type) {
      case "text": {
        emitStream({ type: "content_block_start", index, content_block: { type: "text", text: "" } });
        // Two deltas, never one: a consumer that concatenated wrongly would
        // still pass against a single-delta block.
        const mid = Math.ceil(block.text.length / 2);
        const [head, tail] = [block.text.slice(0, mid), block.text.slice(mid)];
        if (head !== "") {
          emitStream({ type: "content_block_delta", index, delta: { type: "text_delta", text: head } });
        }
        interleaved(midBlock);
        if (tail !== "") {
          emitStream({ type: "content_block_delta", index, delta: { type: "text_delta", text: tail } });
        }
        break;
      }
      case "thinking": {
        emitStream({
          type: "content_block_start",
          index,
          content_block: { type: "thinking", thinking: "", signature: "" },
        });
        // Two deltas, never one, for the reason prose has two.
        const mid = Math.ceil(block.thinking.length / 2);
        const [head, tail] = [block.thinking.slice(0, mid), block.thinking.slice(mid)];
        if (head !== "") {
          emitStream({
            type: "content_block_delta",
            index,
            delta: { type: "thinking_delta", thinking: head, estimated_tokens: null },
          });
        }
        interleaved(midBlock);
        if (tail !== "") {
          emitStream({
            type: "content_block_delta",
            index,
            delta: { type: "thinking_delta", thinking: tail, estimated_tokens: null },
          });
        }
        // The signature delta arrives even when the reasoning itself is
        // WITHHELD — that is exactly what a withheld thinking block is on the
        // wire (corpus: `content-blocks/thinking.jsonl` has `thinking: ""` and
        // a signature), and the arm the converter picks depends on it.
        emitStream({
          type: "content_block_delta",
          index,
          delta: { type: "signature_delta", signature: block.signature },
        });
        break;
      }
      case "tool_use": {
        emitStream({
          type: "content_block_start",
          index,
          content_block: { type: "tool_use", id: block.id, name: block.name, input: {} },
        });
        interleaved(midBlock);
        emitStream({
          type: "content_block_delta",
          index,
          delta: { type: "input_json_delta", partial_json: JSON.stringify(block.input) },
        });
        break;
      }
      case "fallback": {
        emitStream({
          type: "content_block_start",
          index,
          content_block: { type: "fallback", from: block.from, to: block.to },
        });
        interleaved(midBlock);
        break;
      }
    }
  };

  /**
   * Close one block.
   *
   * SEPARATE FROM {@link emitBlockStream} because of the ORDER the real binary
   * uses: with `includePartialMessages` the vendor emits the block's `assistant`
   * line BEFORE that block's `content_block_stop` (observed in every streamed
   * capture; `prose-streamed` is the smallest). The line RESTATES the block the
   * stream is still streaming, and the fold reads it that way — so the mock
   * cannot close the block first without making every streamed response's
   * terminal land on a different unit than its start.
   */
  const emitBlockStop = (index: number): void => {
    emitStream({ type: "content_block_stop", index });
  };

  /**
   * Announce one API response, block by block.
   *
   * ONE TRANSCRIPT LINE AND ONE SDK `assistant` MESSAGE PER BLOCK, all sharing
   * `message.id`, in block order. That is the real binary's split — the SDK
   * declares that several assistant messages may share a message id, and the
   * corpus shows one message's thinking block and tool_use block on two chained
   * transcript lines — and it is why `<message.id>:<block_index>` addresses a
   * BLOCK rather than a line. Every split carries the SAME usage, so the fold's
   * "usage rides block 0" rule has a value to pick on the first one — and each
   * line is emitted BEFORE its block's `content_block_stop`, which is the order
   * the real binary uses and the order the fold's block identity depends on.
   */
  const assistant = (
    blocks: readonly FakeBlock[],
    options: AssistantOptions = {},
  ): AssistantEmission => {
    const messageId = options.messageId ?? mintMessageId();
    streamParentToolUseId = options.agent?.parentToolUseId ?? null;
    const reportedModel = options.model ?? model;
    const usage = fakeUsage(options.contextTokens);
    const requestId = `req_fake_${spawnTag}_${messageCounter}`;
    const uuids: string[] = [];
    emitStream({
      type: "message_start",
      message: {
        model: reportedModel,
        id: messageId,
        type: "message",
        role: "assistant",
        content: [],
        stop_reason: null,
        stop_sequence: null,
        stop_details: null,
        usage,
        diagnostics: null,
      },
    });
    blocks.forEach((block, index) => {
      emitBlockStream(block, index, options.interleave?.get(index));
      const uuid = opts.newUuid();
      uuids.push(uuid);
      // A SCENARIO MAY LIE ABOUT WHEN, and about nothing else: see
      // AssistantOptions.timestamp.
      const timestamp = options.timestamp ?? nowIso();
      const message = {
        model: reportedModel,
        id: messageId,
        type: "message",
        role: "assistant",
        content: [blockPayload(block)],
        stop_reason: options.stopReason ?? null,
        stop_sequence: null,
        stop_details: options.stopDetails ?? null,
        usage,
        diagnostics: null,
      };
      emitWithUuid(uuid, {
        type: "assistant",
        message,
        parent_tool_use_id: options.agent?.parentToolUseId ?? null,
        request_id: requestId,
        timestamp,
        ...(options.error === undefined ? {} : { error: options.error }),
        ...(options.aborted === true ? { aborted: true } : {}),
        ...(options.agent === undefined
          ? {}
          : {
              agent_id: options.agent.agentId,
              subagent_type: options.agent.subagentType,
              task_description: options.agent.taskDescription,
            }),
      });
      if (options.skipTranscript !== true) {
        const record = {
          message,
          requestId,
          type: "assistant",
          uuid,
          timestamp,
          ...(options.effort === undefined ? {} : { effort: options.effort }),
        };
        if (options.agent === undefined) files.transcript.append(record);
        else files.subagent(options.agent.agentId).append(record);
      }
      // AFTER the line, never before: see emitBlockStop.
      emitBlockStop(index);
    });
    emitStream({
      type: "message_delta",
      delta: {
        stop_reason: options.stopReason ?? "end_turn",
        stop_sequence: null,
        stop_details: options.stopDetails ?? null,
      },
      usage,
      context_management: { applied_edits: [] },
    });
    emitStream({ type: "message_stop" });
    streamParentToolUseId = null;
    return { messageId, uuids };
  };

  /**
   * The reasoning block that precedes a tool call and a turn's conclusion.
   *
   * WITHHELD, which on the wire is a `thinking` block with an empty `thinking`
   * string and a PRESENT signature (corpus: `content-blocks/thinking.jsonl`).
   * The signature is what tells a consumer the reasoning existed and was not
   * surfaced; a block without one would be a different fact.
   */
  const withheldReasoning = (): FakeBlock => ({
    type: "thinking",
    thinking: "",
    signature: FAKE_REASONING_SIGNATURE,
  });

  const toolUse = (
    name: string,
    input: Record<string, unknown>,
    options: AssistantOptions = {},
  ): ToolCall => {
    const toolUseId = mintToolUseId();
    // THE OBSERVED SHAPE OF A TOOL TURN'S FIRST API RESPONSE: `[thinking,
    // tool_use]` on ONE message id, each block its own assistant line. Every
    // capture has it (`bash-foreground-completed` is the smallest), and a mock
    // that announced a bare tool_use produced a turn with no reasoning unit at
    // all — a shape the vendor never emits.
    const blocks: FakeBlock[] =
      options.noReasoning === true
        ? [{ type: "tool_use", id: toolUseId, name, input }]
        : [withheldReasoning(), { type: "tool_use", id: toolUseId, name, input }];
    const emission = assistant(blocks, options);
    return {
      toolUseId,
      name,
      input,
      // The tool_use block's OWN record uuid — the one the answering user record
      // names in `sourceToolAssistantUUID`. It is the LAST line of the emission
      // because the reasoning block precedes it.
      assistantUuid: emission.uuids.at(-1) ?? "",
      messageId: emission.messageId,
    };
  };

  const toolResult = (
    call: ToolCall,
    content: string,
    toolUseResult: unknown,
    options: ToolResultOptions = {},
  ): void => {
    const uuid = opts.newUuid();
    const timestamp = nowIso();
    const resultBlock = {
      tool_use_id: call.toolUseId,
      type: "tool_result",
      content: options.blocks ?? content,
      is_error: options.isError === true,
    };
    const message = { role: "user", content: [resultBlock] };
    emitWithUuid(uuid, {
      type: "user",
      message,
      parent_tool_use_id: options.agent?.parentToolUseId ?? null,
      // The SDK's structured half of a tool result. The transcript's field is
      // `toolUseResult`; the stream's is `tool_use_result`. They carry the same
      // object and both planes must see it.
      tool_use_result: toolUseResult,
      timestamp,
      ...(options.agent === undefined
        ? {}
        : {
            agent_id: options.agent.agentId,
            subagent_type: options.agent.subagentType,
            task_description: options.agent.taskDescription,
          }),
    });
    if (options.skipTranscript === true) return;
    const record = {
      promptId,
      type: "user",
      message,
      uuid,
      timestamp,
      ...(options.toolDenialKind === undefined ? {} : { toolDenialKind: options.toolDenialKind }),
      toolUseResult,
      sourceToolAssistantUUID: call.assistantUuid,
    };
    if (options.agent === undefined) files.transcript.append(record);
    else files.subagent(options.agent.agentId).append(record);
  };

  const attachment = (payload: Record<string, unknown>): void => {
    // BOTH PLANES, ONE UUID. The vendor records an attachment in the transcript
    // AND puts it on the stream, so the sidecar and the shim each convert the
    // same record. Sharing the uuid is what makes that safe rather than
    // duplicating: residue is keyed `residue:<vendor record uuid>` (landing 5),
    // so the two planes' rows collide on one key and the store absorbs the
    // second. Writing only the file would leave the shim's whole attachment
    // converter unreachable and its residue rows unwritten.
    const uuid = opts.newUuid();
    emitWithUuid(uuid, { attachment: payload, type: "attachment" });
    files.transcript.append({
      attachment: payload,
      type: "attachment",
      uuid,
      timestamp: nowIso(),
    });
  };

  const systemMessage = (subtype: string, fields: Record<string, unknown>): void =>
    emit({ type: "system", subtype, ...fields });

  const systemRecord = (
    subtype: string,
    fields: Record<string, unknown>,
    file?: Record<string, unknown>,
  ): string => {
    // BOTH PLANES, ONE UUID, exactly as `attachment` and `result` do it — and
    // `file` is here because some records are not spelled identically on the
    // two planes. The vendor puts `compact_metadata` on the stream and
    // `compactMetadata` in the transcript for ONE `compact_boundary`, and the
    // grounded capture shows the two carrying the SAME uuid. A scenario that
    // hand-rolled the transcript line minted a second uuid, which is a shape no
    // real session has: the two planes' rows could then never collide on one
    // upsert key, and one compaction reached the feed as two dividers.
    const uuid = opts.newUuid();
    emitWithUuid(uuid, { type: "system", subtype, ...fields });
    files.transcript.append({
      type: "system",
      subtype,
      isMeta: false,
      ...(file ?? fields),
      uuid,
      timestamp: nowIso(),
    });
    return uuid;
  };

  const result = (spec: ResultSpec): void => {
    resultEmitted = true;
    // BOTH PLANES, ONE UUID, exactly as `attachment` does it. The turn's
    // terminal row is keyed `terminal:<AgentId>:<vendor record uuid>`, and that
    // key only collides with the sidecar's row for the SAME turn if the uuid on
    // the stream's `result` and the uuid on the transcript's turn record are one
    // value. Two uuids would put one turn's ending in the book twice.
    const uuid = opts.newUuid();
    emitWithUuid(uuid, {
      type: "result",
      subtype: spec.subtype,
      is_error: spec.subtype !== "success",
      duration_ms: 1_236,
      duration_api_ms: 1_071,
      ttft_ms: 1_186,
      ttft_stream_ms: 996,
      time_to_request_ms: 132,
      num_turns: turn,
      stop_reason: spec.stopReason ?? (spec.subtype === "success" ? "end_turn" : null),
      total_cost_usd: 0.0088135,
      usage: fakeUsage(),
      modelUsage: { [model]: fakeModelUsage(model), ...(spec.extraModelUsage ?? {}) },
      permission_denials: spec.permissionDenials ?? [],
      // THE SIXTEEN FAILURE ARMS LIVE HERE, not in `subtype`. `sdk.d.ts`
      // declares only four error subtypes; every finer stop — blocking_limit,
      // prompt_too_long, hook_stopped, tool_deferred and the rest — is a
      // `TerminalReason`. A mock that only varied the subtype could reach four
      // of the sixteen conversation.v1 failure arms.
      // AN ERROR RESULT WITH NO REASON HAS NO REASON. Defaulting one to
      // `api_error` made every unclassified stop claim the API had failed, and
      // `execution_error` -- the arm that exists precisely for a stop the
      // producer did not classify -- became unreachable.
      ...(spec.terminalReason === undefined
        ? spec.subtype === "success"
          ? { terminal_reason: "completed" }
          : {}
        : { terminal_reason: spec.terminalReason }),
      // THE SESSION'S STATE IS THE DEFAULT, not a constant: fast mode is a
      // session fact the vendor restates on every result, so a turn that says
      // nothing about it reports what the session is actually in.
      fast_mode_state: spec.fastModeState ?? fastModeState,
      ...(() => {
        const reason =
          spec.fastModeState === undefined
            ? fastModeDisabledReason
            : spec.fastModeDisabledReason;
        return reason === undefined ? {} : { fast_mode_disabled_reason: reason };
      })(),
      // `api_error_status` RIDES THE ERROR RESULT TOO. It is the only field
      // that says WHICH api failure a `terminal_reason: "api_error"` was, and
      // emitting it on success results alone left every `!api-*` row reaching
      // the `unmodeled` kind -- the twelve statuses were indistinguishable.
      api_error_status: spec.apiErrorStatus ?? null,
      ...(spec.subtype === "success"
        ? {
            result: spec.result ?? "",
            ...(spec.structuredOutput === undefined ? {} : { structured_output: spec.structuredOutput }),
          }
        : { errors: spec.errors ?? [] }),
    });
    files.transcript.append({
      type: "system",
      subtype: "turn_duration",
      durationMs: 1_236,
      messageCount: messageCounter,
      isMeta: false,
      uuid,
      timestamp: nowIso(),
    });
  };

  const emitInit = (): void =>
    emit({
      type: "system",
      subtype: "init",
      cwd,
      tools: [
        "Task", "Bash", "CronCreate", "CronDelete", "CronList", "Edit", "EnterWorktree",
        "ExitWorktree", "Glob", "Grep", "Monitor", "NotebookEdit", "PushNotification", "Read",
        "ReportFindings", "ScheduleWakeup", "SendMessage", "Skill", "TaskCreate", "TaskGet",
        "TaskList", "TaskOutput", "TaskStop", "TaskUpdate", "ToolSearch", "WebFetch", "WebSearch",
        "Workflow", "Write", "AskUserQuestion", "Artifact", "mcp__echo__echo",
      ],
      mcp_servers: FAKE_MCP_SERVERS.map((s) => ({ name: s.name, status: s.status })),
      model,
      permissionMode,
      slash_commands: FAKE_COMMANDS.map((c) => c.name),
      apiKeySource: "none",
      claude_code_version: FAKE_CLI_VERSION,
      output_style: "default",
      agents: FAKE_AGENTS.map((a) => a.name),
      skills: ["fake-skill"],
      plugins: [{ name: "fake-plugin", path: "/fake/plugins/fake-plugin", version: "1.0.0" }],
      capabilities: ["interrupt_receipt_v1", "msg_lifecycle_v1"],
      betas: [],
      fast_mode_state: fastModeState,
      ...(fastModeDisabledReason === undefined
        ? {}
        : { fast_mode_disabled_reason: fastModeDisabledReason }),
    });

  /**
   * Retire the vendor session identity and mint a new one, as `/clear` does.
   *
   * THE CORPUS HAS NO `/clear` RECORD. It carries a `compact_boundary` for
   * compaction and nothing for a clear, so the retirement record written here
   * is the closest DECLARED shape (`SDKConversationResetMessage` on the stream,
   * a `compact_boundary`-shaped system record on the retired file) rather than
   * an observed one — flagged in `docs/overhaul/shim.md`'s mock section.
   */
  /**
   * A `/clear`, in THE SHAPE THE REAL BINARY USES.
   *
   * Observed in the `identity-rotation-clear` capture (2026-09-01), and it is
   * not what the mock used to do. THREE uuids are involved:
   *
   *   1. the OLD session id, which `conversation_reset.session_id` carries;
   *   2. `new_conversation_id` — a uuid NOTHING LATER EVER USES, on that one
   *      message and nowhere else. It is never adopted as an identity;
   *   3. the REAL new id, which is the `session_id` of the SECOND `system:init`
   *      that follows, and which every later turn's init repeats.
   *
   * On disk a new transcript file appears under the init id and THE OLD FILE
   * SIMPLY STOPS. There is no closing record of any kind — the mock's invented
   * `compact_boundary` "Conversation cleared" line was declared-not-observed
   * and is gone; it also said the wrong thing, since compaction is IN PLACE and
   * never rotates an id.
   */
  const rotate = (): string => {
    // Announced under the OLD identity: `emitWithUuid` stamps `session_id` from
    // `sessionUuid`, so the reset must be pushed BEFORE the swap.
    const announcedButUnused = opts.newUuid();
    emit({ type: "conversation_reset", new_conversation_id: announcedButUnused });
    const next = opts.newUuid();
    sessionUuid = next;
    files.rotate(next);
    // THE CLEAR'S ONLY FILE-PLANE RECORD LANDS IN THE **NEW** TRANSCRIPT.
    //
    // Observed in the `identity-rotation-clear` capture: the retired file stops
    // with no closing record, and the new file opens with the harness's
    // local-command trio — the caveat (isMeta), the `/clear` COMMAND ENVELOPE,
    // and the empty `system:local_command` stdout. The envelope is the ONLY
    // place a clear exists on disk, so a mock that rotated without writing it
    // produced a rotation no file-plane reader could recognize as a clear.
    files.transcript.append({
      type: "user",
      message: {
        role: "user",
        content:
          "<local-command-caveat>Caveat: The messages below were generated by the user while running local " +
          "commands. DO NOT respond to these messages or otherwise consider them in your response unless the " +
          "user explicitly asks you to.</local-command-caveat>",
      },
      isMeta: true,
      uuid: opts.newUuid(),
      timestamp: nowIso(),
    });
    // The capture's own whitespace, verbatim: the envelope is UNWRAPPED by its
    // reader, and reflowing it here would be a shape the vendor never wrote.
    files.transcript.append({
      type: "user",
      message: {
        role: "user",
        content:
          "<command-name>/clear</command-name>\n            <command-message>clear</command-message>\n" +
          "            <command-args></command-args>",
      },
      uuid: opts.newUuid(),
      timestamp: nowIso(),
    });
    files.transcript.append({
      type: "system",
      subtype: "local_command",
      content: "<local-command-stdout></local-command-stdout>",
      level: "info",
      isMeta: false,
      uuid: opts.newUuid(),
      timestamp: nowIso(),
    });
    // THE SECOND INIT is where the real new id is stated.
    emitInit();
    LOGGER.info(
      { claude_session_id: next, announced_conversation_id: announcedButUnused },
      "fake vendor session identity ROTATED; new_conversation_id is announced and never used again",
    );
    return next;
  };

  /** Scenarios parked on `awaitBackgrounded`, keyed by the call they own. */
  const backgroundWaiters = new Map<string, (ack: () => void) => void>();

  const startTask = (task: Omit<LiveTask, "backgrounded">): LiveTask => {
    const live: LiveTask = { ...task, backgrounded: false };
    liveTasks.set(task.taskId, live);
    // THE SPOOL EXISTS FROM THE MOMENT THE TASK DOES. A detached run that ends
    // by TIMING OUT or by being CANCELLED never gets a scenario line that opens
    // one, so whatever tails the run had no file to open at all — the spool has
    // to be created by the task's own start, exactly as the vendor's does.
    //
    // ONLY FOR THE KINDS THAT OWN A SPOOL. A shell run's spool is its output and
    // an agent's is its own transcript; a monitor has neither, and inventing an
    // empty file for one puts bytes on disk the vendor never writes.
    const spooled = task.kind === "local_bash" || task.kind === "local_agent";
    const outputFile = spooled ? files.spool(task.taskId).path : undefined;
    systemMessage("task_started", {
      task_id: task.taskId,
      tool_use_id: task.toolUseId,
      description: task.description,
      task_type: task.kind,
      ...(outputFile === undefined ? {} : { output_file: outputFile }),
    });
    return live;
  };

  const announceLiveTasks = (): void =>
    // REPLACE semantics: the payload is the whole live set after the change,
    // which is why every mutation re-announces instead of sending a delta.
    systemMessage("background_tasks_changed", {
      tasks: [...liveTasks.values()].map((t) => ({
        task_id: t.taskId,
        task_type: t.kind,
        description: t.description,
      })),
    });

  /** Resolve when this turn is interrupted, now or later. */
  const awaitInterrupt = (): Promise<void> =>
    interrupted
      ? Promise.resolve()
      : new Promise<void>((resolve) => {
          releaseInterrupt = resolve;
        });

  const context: ScenarioContext = {
    get turn() {
      return turn;
    },
    prompt: "",
    args: "",
    get model() {
      return model;
    },
    get permissionMode() {
      return permissionMode;
    },
    newUuid: opts.newUuid,
    nowMs,
    nowIso,
    files,
    cwd,
    configDir,
    spoolRoot,
    emit,
    emitStream,
    endStream: () => out.end(),
    failStream: (error) => out.fail(error),
    assistant,
    toolUse,
    toolResult,
    attachment,
    systemRecord,
    systemMessage,
    result,
    canUseTool,
    awaitInterrupt,
    awaitDetachGate,
    awaitBackgrounded: (toolUseId) =>
      new Promise<() => void>((resolve) => {
        backgroundWaiters.set(toolUseId, resolve);
      }),
    tick: () => new Promise<void>((resolve) => setImmediate(resolve)),
    rotate,
    mintToolUseId,
    mintShellTaskId,
    mintAgentTaskId,
    mintMessageId,
    startTask,
    announceLiveTasks,
    endTask: (taskId: string) => {
      liveTasks.delete(taskId);
    },
    setAccountUsageArm: (arm) => {
      accountUsageArm = arm;
    },
    setMcpArm: (arm) => {
      mcpArm = arm;
    },
    setContextUsageDrift: (drifting) => {
      contextUsageDrift = drifting;
    },
    setFastMode: (state, reason) => {
      fastModeState = state;
      // A reason is meaningless while fast mode is ON — `sdk.d.ts` documents the
      // field as absent when nothing blocks fast mode — so the ON state clears
      // it rather than carrying a stale explanation.
      fastModeDisabledReason = state === "on" ? undefined : reason;
    },
    fallbackTo: (next) => {
      LOGGER.debug(
        { claude_session_id: sessionUuid, previous_model: model, model: next },
        "fake vendor fell back to another model ON ITS OWN; nothing asked it to",
      );
      model = next;
    },
    log: LOGGER.with({ claude_session_id: sessionUuid }),
  };

  const promptTextOf = (message: SdkUserMessage): string => {
    const content = message.message.content;
    if (typeof content === "string") return content;
    return content.map((block) => ("type" in block && block.type === "text" ? block.text : "")).join("");
  };

  /**
   * The vendor's interrupt terminal: no assistant content, an error result, and
   * the abort spelled out in `terminal_reason`.
   *
   * ONE SPELLING, TWO PATHS: a turn aborted mid-scenario and a turn killed
   * while parked on its gate terminate identically, because from the
   * consumer's side they are the same fact.
   */
  const emitInterruptTerminal = (): void => {
    result({
      subtype: "error_during_execution",
      terminalReason: "aborted_streaming",
      errors: ["Interrupted by user"],
    });
  };

  const main = async (): Promise<void> => {
    if (refuse.has("start-eof")) {
      // THE CHILD IS GONE BEFORE IT SAID ANYTHING. No init, no result, no
      // error: the stream simply ends, which is what a vendor binary that died
      // during its own bring-up looks like from here.
      LOGGER.info(
        { claude_session_id: sessionUuid },
        "the mocked vendor was told to END ITS STREAM before the session's init",
      );
      out.end();
      return;
    }
    if (refuse.has("start-error-result")) {
      // THE VENDOR REFUSED THE OPENING IN ITS OWN WORDS. A resume the binary
      // will not honour is answered with an error result and nothing else.
      LOGGER.info(
        { claude_session_id: sessionUuid },
        "the mocked vendor was told to REFUSE the session's opening with an error result",
      );
      result({
        subtype: "error_during_execution",
        errors: [`No conversation found with session ID: ${sessionUuid}`],
      });
      out.end();
      return;
    }
    if (refuse.has("start-auth-error")) {
      // THE VENDOR REFUSED THE OPENING ON A CREDENTIAL. This is
      // `start-error-result`'s authentication shape: an error result with a
      // 401 and the vendor's own credential sentence, which is what the shim's
      // start-path auth diagnostic reads. No real credential is involved.
      LOGGER.info(
        { claude_session_id: sessionUuid },
        "the mocked vendor was told to REFUSE the session's opening with a credential rejection",
      );
      result({
        subtype: "error_during_execution",
        errors: ["the credential was rejected — sign in again"],
        apiErrorStatus: 401,
      });
      out.end();
      return;
    }
    // THE INIT MAY BE OWED TO THE FIRST TURN RATHER THAN TO THE OPENING. The
    // real vendor withholds it until an input message arrives; the lever
    // reproduces that, and `at-start` keeps the corpus's own shape.
    if (!initAfterFirstTurn) emitInit();
    LOGGER.info(
      {
        claude_session_id: sessionUuid,
        init_timing: initAfterFirstTurn ? "after-first-turn" : "at-start",
        resumed: opts.resume !== undefined,
        workspace_dir: cwd,
        // THE REWIND TARGET, as the vendor received it. PRESENT ONLY WHEN THE
        // SHIM NAMED ONE — a sentinel would make every plain start look like a
        // rewind to a reader keying on the field. A spawned shim's only
        // observable is its log, so this is where "the rewind actually reached
        // the vendor" is asserted from.
        ...(opts.resumeSessionAt === undefined
          ? {}
          : { vendor_resume_session_at: opts.resumeSessionAt }),
      },
      opts.resume === undefined
        ? "fake vendor session STARTED"
        : "fake vendor session RESUMED; init reports the resumed id and re-emits NO history",
    );
    let initOwed = initAfterFirstTurn;
    for await (const userMessage of prompt) {
      // THE FIRST TURN IS WHAT BRINGS THE INIT, and it arrives BEFORE the turn
      // it rode in on produces anything — which is the order the real vendor
      // emits them in, and the order that makes the init facts land on a
      // session the shim has already announced as started.
      if (initOwed) {
        initOwed = false;
        LOGGER.info(
          { claude_session_id: sessionUuid },
          "the mocked vendor announces its init NOW, with the first user message, as the real vendor does",
        );
        emitInit();
      }
      turn++;
      interrupted = false;
      resultEmitted = false;
      promptId = opts.newUuid();
      const text = promptTextOf(userMessage);
      await awaitTurnGate(text, awaitInterrupt);
      if (out.isEnded) return;
      // A TURN KILLED ON ITS GATE STILL TERMINATES. The scenario never ran, so
      // there is nothing to abort inside it -- but the turn was live and the
      // consumer is waiting on its terminal like any other.
      if (interrupted) {
        emitInterruptTerminal();
        continue;
      }
      const scenario = selectScenario(text);
      Object.assign(context, {
        prompt: text,
        args: scenario.name === "" ? text : text.slice(`!${scenario.name}`.length).trim(),
      });
      // The vendor records the enqueue before the prompt itself; both lines
      // exist in the corpus and a consumer keyed on the queue operation would
      // otherwise never see one.
      files.transcript.appendUnchained({
        type: "queue-operation",
        operation: "enqueue",
        timestamp: nowIso(),
      });
      files.transcript.append({
        promptId,
        type: "user",
        message: { role: "user", content: [{ type: "text", text }] },
        uuid: opts.newUuid(),
        timestamp: nowIso(),
        permissionMode,
        promptSource: "sdk",
      });
      LOGGER.debug(
        { claude_session_id: sessionUuid, turn, scenario: scenario.name === "" ? "prose" : scenario.name },
        "fake vendor selected a scenario for the turn",
      );
      await scenario.run(context);
      if (out.isEnded) return;
      if (resultEmitted) continue;
      if (interrupted) {
        emitInterruptTerminal();
        continue;
      }
      // A scenario that returned without ending its turn is a DEFECT in the
      // mock, not a vendor behavior. Failing the stream surfaces it at the
      // consumer instead of leaving a turn that never terminates.
      const message = `fake scenario "${scenario.name}" returned without emitting a result`;
      LOGGER.error(
        { claude_session_id: sessionUuid, turn, scenario: scenario.name, detail: message },
        message,
      );
      throw new Error(message);
    }
    out.end();
    LOGGER.info({ claude_session_id: sessionUuid, turns: turn }, "fake vendor input ended");
  };

  void main().catch((err: unknown) => {
    // The mock is the producer boundary, so it owns the one causal error record
    // and FAILS the iterable. Ending cleanly here would make callers see
    // ordinary SDK EOF and silently erase the producer failure.
    LOGGER.error(
      { claude_session_id: sessionUuid, cause: err },
      `fake vendor producer failed: ${err instanceof Error ? err.message : String(err)}`,
    );
    out.fail(err);
  });

  const iterator = out[Symbol.asyncIterator]();
  return {
    [Symbol.asyncIterator]: () => iterator,

    /**
     * Resolves a REPRESENTATIVE receipt, never `undefined`. The shim pins SDK
     * 0.3.220, whose `interrupt()` always answers with one — probing a real
     * session returns exactly `{"still_queued":[]}` — so `undefined` would model
     * a CLI we no longer ship against. Empty is the honest value: the offline
     * query has no CLI-side queue, so nothing can survive an interrupt here.
     */
    interrupt: async (): Promise<InterruptReceipt | undefined> => {
      interrupted = true;
      const release = releaseInterrupt;
      releaseInterrupt = null;
      release?.();
      LOGGER.info({ claude_session_id: sessionUuid, turn }, "fake vendor interrupt accepted");
      return { still_queued: [] };
    },

    setPermissionMode: async (mode: PermissionModeLike): Promise<void> => {
      if (refuse.has("set_permission_mode")) {
        throw new Error("the mocked vendor refused the permission mode change");
      }
      LOGGER.debug(
        { claude_session_id: sessionUuid, previous_permission_mode: permissionMode, permission_mode: mode },
        "fake vendor permission mode changed",
      );
      permissionMode = mode;
      // The vendor announces the new mode on its `status` message, the only
      // place outside `init` where `sdk.d.ts` declares a permission mode at all.
      systemMessage("status", { status: null, permissionMode: mode });
    },

    setModel: async (next?: string): Promise<void> => {
      if (refuse.has("set_model")) {
        throw new Error("the mocked vendor refused the model change");
      }
      const resolved = next ?? FAKE_DEFAULT_MODEL;
      LOGGER.debug(
        { claude_session_id: sessionUuid, previous_model: model, model: resolved },
        "fake vendor model changed",
      );
      model = resolved;
      // `sdk.d.ts` declares NO `model_changed` system message. The declared
      // signal a model switch produces is a `session_state_changed` beat, and
      // the REAL evidence is the next assistant message reporting the new model
      // — which every scenario does, because `assistant` reads `model` live.
      systemMessage("session_state_changed", { state: "idle" });
    },

    supportedModels: async (): Promise<ModelInfoLike[]> => FAKE_MODELS,
    supportedCommands: async (): Promise<SlashCommandLike[]> => FAKE_COMMANDS,
    supportedAgents: async (): Promise<AgentInfoLike[]> => FAKE_AGENTS,
    mcpServerStatus: async (): Promise<McpServerStatusLike[]> =>
      mcpArm === "all" ? FAKE_MCP_SERVERS : FAKE_MCP_SERVERS_HEALTHY,
    getContextUsage: async (): Promise<ContextUsageLike> =>
      // A DRIFTING answer grows by twenty thousand tokens a turn and scales the
      // occupancy-derived tables with it; the steady one moves only by the
      // thousand-token-per-turn the ordinary session accrues.
      contextUsageDrift
        ? fakeContextUsage(model, 30_000 + turn * 20_000, turn)
        : fakeContextUsage(model, 30_000 + turn * 1_000),
    usage_EXPERIMENTAL_MAY_CHANGE_DO_NOT_RELY_ON_THIS_API_YET: async (): Promise<AccountUsageLike> =>
      fakeAccountUsage(accountUsageArm, nowMs()),
    accountInfo: async (): Promise<AccountInfoLike> => FAKE_ACCOUNT_INFO,
    initializationResult: async (): Promise<InitializationResultLike> => FAKE_INITIALIZATION_RESULT,

    /**
     * The vendor's per-task stop, offline.
     *
     * It answers the way the CLI does: the stopped task emits its own
     * `task_notification{status:"stopped"}` — the ordinary terminal fact the
     * whole stack settles on — and the live set is re-announced, because
     * `background_tasks_changed` is REPLACE semantics and a consumer that never
     * saw the shrunk set would keep the task forever. A task this engine never
     * started is ACCEPTED and says so: the real stop is idempotent, and a task
     * that already ended is not an error to stop again.
     */
    stopTask: async (taskId: string): Promise<void> => {
      const live = liveTasks.get(taskId);
      if (live === undefined) {
        LOGGER.debug(
          { claude_session_id: sessionUuid, task_id: taskId },
          "fake vendor stop_task named no live task; accepted as a no-op",
        );
        return;
      }
      liveTasks.delete(taskId);
      // A stopped SHELL's spool gets its terminator: the tailer's only way to
      // learn the run ended is the EXIT line, and 143 is SIGTERM's exit code.
      //
      // AN AGENT'S SPOOL NEVER DOES. An agent spool is the agent's own JSONL
      // and carries no terminator at all, so an `EXIT=` line written into one
      // is a line no real tree contains — and a reader that learned to parse it
      // would be built against a shape the vendor never produces. The kind is
      // read off the task id, which is the vendor's own distinction (`b<hex>`
      // shell runs, `a<hex>` agents).
      if (isShellTaskId(taskId) && files.unfinishedSpools().includes(taskId)) {
        files.spool(taskId).finish(143);
      } else if (files.unfinishedSpools().includes(taskId)) {
        LOGGER.info(
          { claude_session_id: sessionUuid, task_id: taskId },
          "fake vendor stopped an AGENT task; its spool is left unterminated, as an agent spool always is",
        );
      }
      systemMessage("task_notification", {
        task_id: taskId,
        tool_use_id: live.toolUseId,
        status: "stopped",
        output_file: files.spoolPathFor(taskId),
        summary: "Task stopped by request",
      });
      announceLiveTasks();
      // THE FAN-WIDE CANCEL'S OWN RECORD. `agents_killed` states that the live
      // set EMPTIED, which no individual stop can know, so it is written here
      // rather than by a scenario. The corpus carries it as a bare system line
      // with no payload beyond the envelope.
      if (liveTasks.size === 0) {
        files.transcript.append({
          type: "system",
          subtype: "agents_killed",
          isMeta: false,
          uuid: opts.newUuid(),
          timestamp: nowIso(),
        });
      }
      LOGGER.info(
        { claude_session_id: sessionUuid, task_id: taskId, tool_use_id: live.toolUseId },
        "fake vendor stop_task stopped a live task",
      );
    },

    /**
     * Ctrl-B: move foreground work to the background.
     *
     * With no argument it reports whether ANY task is live. With a
     * `tool_use_id` it detaches that call: the task is marked
     * `is_backgrounded` through `task_updated` and the live set is
     * re-announced. The FOREGROUND tool result that follows is the scenario's
     * job — a shell's backgrounding cause is harvested from the bash tool
     * result (`backgroundedByUser`), never from the task stream.
     */
    backgroundTasks: async (toolUseId?: string): Promise<boolean> => {
      if (toolUseId === undefined) return liveTasks.size > 0;
      const live = [...liveTasks.values()].find((t) => t.toolUseId === toolUseId);
      if (live === undefined) {
        LOGGER.debug(
          { claude_session_id: sessionUuid, tool_use_id: toolUseId },
          "fake vendor background_tasks named no live task for that tool call",
        );
        return liveTasks.size > 0;
      }
      // STATE FIRST, THEN THE ANNOUNCEMENTS. Every emit below must describe a
      // world that is already true: a consumer that read the level while the
      // flag was still unset would see the task listed as foreground in the
      // very message that announces it left.
      live.backgrounded = true;
      systemMessage("task_updated", { task_id: live.taskId, patch: { is_backgrounded: true } });
      announceLiveTasks();
      // THEN THE VENDOR'S OWN DETACHMENT RECORD, BEFORE THIS VERB ANSWERS. A
      // real Ctrl-B has already put the backgrounded foreground result on the
      // stream by the time the binary reports the detach; answering first would
      // let a caller observe DetachForeground succeeding against a conversation
      // that still shows the work in the foreground. The parked scenario
      // acknowledges once it has emitted, so this is a happens-before.
      const waiter = backgroundWaiters.get(toolUseId);
      if (waiter !== undefined) {
        backgroundWaiters.delete(toolUseId);
        await new Promise<void>((resolve) => waiter(resolve));
      }
      LOGGER.info(
        { claude_session_id: sessionUuid, task_id: live.taskId, tool_use_id: toolUseId },
        "fake vendor moved a foreground task to the background",
      );
      return true;
    },

    /**
     * More user messages into a running query.
     *
     * The mock consumes ONE input iterable — the one it was built with — and
     * the shim's own input queue is what callers push into, so a second stream
     * has nothing to add. Refusing would be wrong (the verb exists and the real
     * query accepts it) and silently ignoring would hide a caller's mistake, so
     * it logs the fact and resolves.
     */
    streamInput: async (stream: AsyncIterable<SdkUserMessage>): Promise<void> => {
      LOGGER.debug(
        { claude_session_id: sessionUuid },
        "fake vendor stream_input accepted; the mock drains its original prompt iterable",
      );
      void stream;
    },

    close: (): void => {
      LOGGER.info({ claude_session_id: sessionUuid, turns: turn }, "fake vendor query closed");
      out.end();
    },
  };
}
