/**
 * The session engine, end to end over a scripted vendor.
 *
 * WHAT THIS GUARDS: the acts that are irreversible or invisible. A cold resume
 * is REFUSED with its cost before a token is spent; the session lock is taken
 * BEFORE the SDK is touched, so two shims can never write one transcript; the
 * teardown resolves every pending callback as denied before anything else,
 * because an unresolved `canUseTool` wedges the vendor process outright.
 */
import { existsSync, mkdirSync, mkdtempSync, writeFileSync } from "node:fs";
import { nextPush } from "../next-push.js";
import os from "node:os";
import path from "node:path";
import { beforeEach, describe, expect, it, vi } from "vitest";
import { logRecordsDuring, logRecordsSince, logSinkMark } from "../log-records.js";
import { create } from "@bufbuild/protobuf";
import { Code } from "@connectrpc/connect";
import { conversationv1, shimv1, storev1 } from "../../src/proto.js";
import { recordAgentBinaryVersion, resetAgentBinaryVersionForTest } from "../../src/build-identity.js";
import { cwdSlug } from "../../src/engine/cold.js";
import { bindLog, clearRequestId } from "../../src/log.js";
import { createEngine, type QuerySpec, type SessionEngine } from "../../src/engine/session.js";
import { agentIdPath } from "../../src/engine/identity.js";
import { LockHeldError, LockHolderUnavailableError, workspaceLockKey } from "../../src/locks.js";
import { saidText, textSaid } from "../../src/engine/turn.js";
import { KEEPALIVE_INTERVAL_MS, KEEPALIVE_PROMPT_MARKER } from "../../src/engine/keepalive.js";
import { toStanding } from "../../src/engine/permission-gate.js";
import { SYNTHETIC_MODEL } from "../../src/model.js";
import type {
  AccountUsageLike,
  ContextUsageLike,
  McpServerStatusLike,
  ModelInfoLike,
} from "../../src/sdk/types.js";
import { mainAgentId } from "../../src/convert/ids.js";
// STATICALLY, NOT `await import(...)` AT THE CALL SITE: the engine recognizes
// a store outage by `instanceof PersistenceError`, so a copy of the class
// from a second module graph would sail past every one of those arms. The
// static binding is the file's one copy and cannot drift from the engine's.
import { PersistenceError, type PersistEntry } from "../../src/store/persistence.js";
import type { SdkMessage, SdkUserMessage } from "../../src/sdk/types.js";
import {
  NETWORK_RESUME_PROBE_INTERVAL_MS,
} from "../../src/engine/network-resume.js";
import { isNetworkResumePrompt, resumePromptTargets } from "../../src/engine/network-resume-prompt.js";
import { ManualScheduler, RecordingFold, RecordingPersistence, ScriptedProbe, ScriptedQuery, errorResultMessage, hookResponse, initMessage, resultMessage } from "./fakes.js";

interface Harness {
  readonly engine: SessionEngine;
  readonly persistence: RecordingPersistence;
  readonly fold: RecordingFold;
  readonly scheduler: ManualScheduler;
  /** The network-resume loop's own scheduler, fired by hand. */
  readonly networkScheduler: ManualScheduler;
  /** The reachability probe the network-resume loop asks. */
  readonly probe: ScriptedProbe;
  readonly queries: { spec: QuerySpec; query: ScriptedQuery }[];
  readonly stateDir: string;
  readonly configDir: string;
  readonly cwd: string;
  readonly locks: string[];
  /** Every workspace directory the engine claimed a kernel lock on. */
  readonly workspaceLocks: string[];
  readonly released: string[];
  /** Every exit code the engine asked `main.ts` to end the process with. */
  readonly exits: number[];
  /**
   * Every client uuid the engine minted, in order: one per keep-alive send.
   * A scripted reply stamps the latest to answer that send, as the vendor does.
   */
  readonly minted: string[];
}

function scratch(): string {
  return mkdtempSync(path.join(os.tmpdir(), "shim-session-"));
}

/** Write a transcript for `sessionId` where the vendor would have written it. */
function writeTranscript(configDir: string, cwd: string, sessionId: string, lines: unknown[]): void {
  const dir = path.join(configDir, "projects", cwdSlug(cwd));
  mkdirSync(dir, { recursive: true });
  writeFileSync(
    path.join(dir, `${sessionId}.jsonl`),
    `${lines.map((line) => JSON.stringify(line)).join("\n")}\n`,
    "utf8",
  );
}

/** The context this fixture reports, ABOVE the cold gate's 70,000-token floor. */
const FIXTURE_CONTEXT_TOKENS = 100_000;

/**
 * One assistant line, reporting a context the cold gate WILL ask about.
 *
 * The floor (owner ruling, `engine/cold.ts`) means a fixture under 70,000
 * tokens resumes warm no matter how long the lapse, so a cold-gate test built
 * on one would pass for the wrong reason. `smallAssistantLine` is the
 * deliberately-under-the-floor counterpart.
 */
function assistantLine(overrides: Record<string, unknown> = {}): Record<string, unknown> {
  return {
    type: "assistant",
    uuid: "u-1",
    timestamp: new Date(1_000_000).toISOString(),
    cwd: "/ws",
    sessionId: "resume-1",
    message: {
      model: "claude-opus-5",
      usage: {
        input_tokens: 0,
        cache_creation_input_tokens: 0,
        cache_read_input_tokens: FIXTURE_CONTEXT_TOKENS,
        cache_creation: { ephemeral_1h_input_tokens: 0, ephemeral_5m_input_tokens: 1 },
      },
    },
    ...overrides,
  };
}

/** One assistant line whose context is UNDER the cold-gate floor. */
function smallAssistantLine(): Record<string, unknown> {
  return assistantLine({
    message: {
      model: "claude-opus-5",
      usage: {
        input_tokens: 0,
        cache_creation_input_tokens: 0,
        cache_read_input_tokens: 500,
        cache_creation: { ephemeral_1h_input_tokens: 0, ephemeral_5m_input_tokens: 1 },
      },
    },
  });
}

function harness(
  options: {
    nowMs?: number;
    lockThrows?: boolean;
    /** Make the WORKSPACE claim refuse, so its own conversation_owned arm shows. */
    workspaceLockThrows?: boolean;
    keepaliveIntervalMs?: number;
    /** What every scripted query answers `backgroundTasks` with. */
    backgroundTasks?: boolean;
    /** Make `backgroundTasks` reject, so the fail-open path is exercised. */
    backgroundTasksThrows?: boolean;
    /** What the scripted query answers `mcpServerStatus` with. */
    mcp?: McpServerStatusLike[];
    /** What the scripted query answers the account-usage control verb with. */
    accountUsage?: AccountUsageLike;
    /** The teardown's per-wait budget, so a hang suite does not sit out five seconds. */
    watcherConclusionBudgetMs?: number;
    /** Leave every scripted query's stream standing after `close()`. */
    closeLeavesStreamOpen?: boolean;
    /** The LAST-RESORT bound on a child that answers nothing at all. */
    initTimeoutMs?: number;
    /** How long StartSession waits for its one control round-trip to answer. */
    liveSignalTimeoutMs?: number;
    /**
     * HOLD THE PROVEN-LIVE SIGNAL on every scripted query.
     *
     * `supportedModels()` is what a start settles on, and a scripted query
     * answers it in a microtask — so a suite whose subject settles a start
     * BEFORE the live signal (a blocking hook, an error result, a stream that
     * ends, the silence bound) never reaches its subject without this.
     */
    holdLiveSignal?: boolean;
    /** Refuse to create any query after this many have been created. */
    createQueryFailsFrom?: number;
    /**
     * Refuse EXACTLY the nth query creation (0-based), letting the rest through.
     *
     * The keep-alive rewind's recovery opens a SECOND query after the one it
     * was refused, so a suite that has to fail only the rewind cannot use
     * `createQueryFailsFrom`, which would take the recovery down with it.
     */
    createQueryFailsAt?: number;
    /**
     * Reject EVERY `createQuery` with this exact value.
     *
     * Distinct from `createQueryFailsFrom`, which always rejects with an
     * `Error`: the SDK is a foreign boundary and a rejection that is not an
     * `Error` is exactly what the engine's `String(err)` arms exist for.
     */
    createQueryRejection?: unknown;
    /** Throw this exact value out of the SESSION claim, Error or not. */
    lockRefusal?: unknown;
    /** Throw this exact value out of the WORKSPACE claim, Error or not. */
    workspaceLockRefusal?: unknown;
    /** Build the engine with NO scheduler, the way a real session does. */
    withoutScheduler?: boolean;
    /** Build the engine with NEITHER lock injected, the way `main.ts` does. */
    withoutLockInjection?: boolean;
    /** Build the engine with NO `endProcess`, the way an in-process build does. */
    withoutEndProcess?: boolean;
    /**
     * Pull the prompt stream the engine hands the vendor, as the real SDK does.
     *
     * `ScriptedQuery` never touches `spec.prompt`, so nothing normally exercises
     * the push-to-pull bridge the engine submits every prompt through.
     */
    drainPrompts?: string[];
    /** Like `drainPrompts`, but keeps every send WHOLE, client uuid and all. */
    drainSends?: SdkUserMessage[];
    /** Reach each scripted query the moment it is created, before it is returned. */
    onQueryCreated?: (query: ScriptedQuery, spec: QuerySpec, index: number) => void;
    /**
     * Build the engine with a DIFFERENT module graph's `createEngine`.
     *
     * The one caller is the durable-log poisoning scenario: the shim's logger
     * is a process-wide singleton and a poisoning is one-way, so that test
     * takes its own `log.ts` and needs the engine that registers on it to come
     * from the same graph. Everything else gets this file's own engine.
     */
    engineFactory?: typeof createEngine;
    /**
     * Build over an EXISTING pair of directories, the way a restarted shim
     * comes back over the state its predecessor left.
     */
    over?: { stateDir: string; configDir: string };
  } = {},
): Harness {
  const stateDir = options.over?.stateDir ?? scratch();
  const configDir = options.over?.configDir ?? scratch();
  /** Creations that were REFUSED, which `queries` does not count. */
  let refusedQueries = 0;
  const cwd = "/ws";
  const persistence = new RecordingPersistence();
  const fold = new RecordingFold();
  const scheduler = new ManualScheduler();
  const networkScheduler = new ManualScheduler();
  const probe = new ScriptedProbe();
  const queries: { spec: QuerySpec; query: ScriptedQuery }[] = [];
  const locks: string[] = [];
  const released: string[] = [];
  const workspaceLocks: string[] = [];
  const exits: number[] = [];
  const minted: string[] = [];
  const engine = (options.engineFactory ?? createEngine)({
    ...(options.withoutEndProcess === true ? {} : { endProcess: (code: number) => exits.push(code) }),
    persistence,
    fold,
    createQuery: (spec) => {
      if (options.createQueryRejection !== undefined) return Promise.reject(options.createQueryRejection);
      if (options.createQueryFailsFrom !== undefined && queries.length >= options.createQueryFailsFrom) {
        return Promise.reject(new Error("the vendor refused another query"));
      }
      if (options.createQueryFailsAt === queries.length + refusedQueries) {
        refusedQueries++;
        return Promise.reject(new Error("No message found with message.uuid of: assistant-uuid"));
      }
      const query = new ScriptedQuery();
      if (options.backgroundTasks === true) query.backgroundTaskAnswer = true;
      if (options.backgroundTasksThrows === true) {
        query.backgroundTasks = () => Promise.reject(new Error("the vendor cannot answer"));
      }
      if (options.mcp !== undefined) query.mcp = options.mcp;
      if (options.accountUsage !== undefined) query.accountUsage = options.accountUsage;
      if (options.closeLeavesStreamOpen === true) query.closeLeavesStreamOpen = true;
      if (options.holdLiveSignal === true) query.holdModels();
      const index = queries.length;
      queries.push({ spec, query });
      options.onQueryCreated?.(query, spec, index);
      const sends = options.drainSends;
      if (sends !== undefined) {
        void (async (): Promise<void> => {
          for await (const message of spec.prompt) sends.push(message);
        })();
      }
      const drained = options.drainPrompts;
      if (drained !== undefined) {
        void (async (): Promise<void> => {
          for await (const message of spec.prompt) {
            drained.push(typeof message.message.content === "string" ? message.message.content : "");
          }
          drained.push("<end>");
        })();
      }
      return Promise.resolve(query);
    },
    runtime: { shimBuildSha: "sha", sdkVersion: "0.3.220" },
    newUuid: () => {
      const uuid = `00000000-0000-4000-8000-${String(minted.length + 1).padStart(12, "0")}`;
      minted.push(uuid);
      return uuid;
    },
    env: { stateDir, configDir, cwd },
    nowMs: () => options.nowMs ?? 1_000_100,
    probeApiReachable: probe.probe,
    networkResumeScheduler: networkScheduler,
    ...(options.withoutScheduler === true ? {} : { scheduler }),
    ...(options.initTimeoutMs === undefined ? {} : { initTimeoutMs: options.initTimeoutMs }),
    ...(options.liveSignalTimeoutMs === undefined
      ? {}
      : { liveSignalTimeoutMs: options.liveSignalTimeoutMs }),
    ...(options.keepaliveIntervalMs === undefined
      ? {}
      : { keepaliveIntervalMs: options.keepaliveIntervalMs }),
    ...(options.watcherConclusionBudgetMs === undefined
      ? {}
      : { watcherConclusionBudgetMs: options.watcherConclusionBudgetMs }),
    ...(options.withoutLockInjection === true
      ? {}
      : {
          acquireLock: (sessionId: string): (() => void) => {
            if (options.lockThrows === true) throw new LockHeldError("locked by another shim");
            if (options.lockRefusal !== undefined) throw options.lockRefusal;
            locks.push(sessionId);
            return () => {
              released.push(sessionId);
            };
          },
        }),
    // STUBBED LIKE THE SESSION CLAIM. The workspace lock moved into
    // StartSession, so a unit test that left it real would take a kernel lock
    // on whatever directory the harness names.
    ...(options.withoutLockInjection === true
      ? {}
      : {
          acquireWorkspaceLock: (dir: string): (() => void) => {
            if (options.workspaceLockThrows === true) throw new LockHeldError("locked by another shim");
            if (options.workspaceLockRefusal !== undefined) throw options.workspaceLockRefusal;
            workspaceLocks.push(dir);
            return () => {
              released.push(`workspace:${dir}`);
            };
          },
        }),
  });
  return {
    engine,
    persistence,
    fold,
    scheduler,
    networkScheduler,
    probe,
    queries,
    stateDir,
    configDir,
    cwd,
    locks,
    workspaceLocks,
    released,
    exits,
    minted,
  };
}

function freshRequest(): shimv1.StartSessionRequest {
  return create(shimv1.StartSessionRequestSchema, {
    source: {
      case: "fresh",
      value: create(shimv1.StartSessionFreshSchema, {
        model: create(conversationv1.AgentModelSchema, { name: "claude-opus-5" }),
        permissionMode: create(conversationv1.AgentPermissionModeSchema, {
          mode: { case: "default", value: create(conversationv1.AgentPermissionModeDefaultSchema, {}) },
        }),
      }),
    },
  });
}

/** A fresh start naming NEITHER model nor permission mode. */
function freshRequestNoFacts(): shimv1.StartSessionRequest {
  return create(shimv1.StartSessionRequestSchema, {
    source: { case: "fresh", value: create(shimv1.StartSessionFreshSchema, {}) },
  });
}

/** A fresh start naming NO model: the SDK's own default takes effect. */
function freshRequestNoModel(): shimv1.StartSessionRequest {
  return create(shimv1.StartSessionRequestSchema, {
    source: {
      case: "fresh",
      value: create(shimv1.StartSessionFreshSchema, {
        permissionMode: create(conversationv1.AgentPermissionModeSchema, {
          mode: { case: "default", value: create(conversationv1.AgentPermissionModeDefaultSchema, {}) },
        }),
      }),
    },
  });
}

function resumeRequest(
  vendorSessionId: string,
  remediation?: conversationv1.SessionColdRemediation,
  options: { rebind?: boolean } = {},
): shimv1.StartSessionRequest {
  return create(shimv1.StartSessionRequestSchema, {
    source: {
      case: "resume",
      value: create(shimv1.StartSessionResumeSchema, {
        vendorSessionId,
        ...(remediation === undefined ? {} : { coldRemediation: remediation }),
        ...(options.rebind === true
          ? { rebind: create(shimv1.StartSessionRebindSchema, {}) }
          : {}),
      }),
    },
  });
}

/** Persist a main AgentId for the harness's workspace, as an earlier session would. */
function persistIdentity(h: Harness, originalVendorSessionId: string): void {
  const file = agentIdPath(h.stateDir, workspaceLockKey(h.cwd));
  mkdirSync(path.dirname(file), { recursive: true });
  writeFileSync(
    file,
    JSON.stringify({
      original_vendor_session_id: originalVendorSessionId,
      workspace_key: workspaceLockKey(h.cwd),
      minted_at_ms: 1,
    }),
    "utf8",
  );
}

/**
 * Wait for the engine to have asked for its nth query.
 *
 * StartSession awaits several things before it creates one (the identity file,
 * the lock), so a single microtask tick is not enough — and polling a condition
 * is the only honest way to wait for an async step with no completion signal of
 * its own.
 */
async function untilQuery(h: Harness, index: number): Promise<{ spec: QuerySpec; query: ScriptedQuery }> {
  for (let attempt = 0; attempt < 200; attempt++) {
    const created = h.queries[index];
    if (created !== undefined) return created;
    await new Promise((resolve) => setImmediate(resolve));
  }
  throw new Error(`no query was created at index ${index}`);
}

/** The pre-minted id a fresh binding carries. */
function freshSessionId(spec: QuerySpec): string {
  return spec.binding.kind === "fresh" ? spec.binding.sessionId : "";
}

/**
 * A started session with a transcript on disk — the state `Hibernate` acts on.
 *
 * The vendor session id comes back with it because every hibernation assertion
 * is about THAT conversation's transcript and THAT conversation's mark.
 */
async function hibernatable(): Promise<Harness & { vendorSessionId: string }> {
  const h = harness();
  await started(h);
  const vendorSessionId = freshSessionId((await untilQuery(h, 0)).spec);
  writeTranscript(h.configDir, h.cwd, vendorSessionId, [assistantLine({ sessionId: vendorSessionId })]);
  return { ...h, vendorSessionId };
}

/** Bring a fresh session up: start it, and answer the vendor's init. */
async function started(h: Harness): Promise<shimv1.StartSessionResponse> {
  const pending = h.engine.startSession(freshRequest());
  const first = await untilQuery(h, 0);
  const sessionId = freshSessionId(first.spec);
  first.query.emit(initMessage({ sessionId }));
  return pending;
}

/** The refusal locks.ts raises when this shim's own lock holder would not spawn. */
function unspawnableHolder(): LockHolderUnavailableError {
  return new LockHolderUnavailableError(
    "/missing/shim-lock",
    { kind: "spawnFailed", osError: "spawn /missing/shim-lock ENOENT" },
    "shim-session-lock: the lock holder /missing/shim-lock could not be spawned: spawn /missing/shim-lock ENOENT",
  );
}

/** The refusal locks.ts raises when this shim's own lock holder exited 1 before holding the lock. */
function exitedHolder(): LockHolderUnavailableError {
  return new LockHolderUnavailableError(
    "/bin/shim-lock",
    { kind: "exited", code: 1, stderr: "EACCES" },
    "shim-session-lock: the lock holder /bin/shim-lock exited with code 1 before taking the lock (EACCES)",
  );
}

function failureCause(response: shimv1.StartSessionResponse): string | undefined {
  return response.result.case === "failure" ? response.result.value.cause.case : undefined;
}

beforeEach(() => {
  resetAgentBinaryVersionForTest();
  recordAgentBinaryVersion("2.1.999");
});

describe("StartSession, fresh", () => {
  it("pre-mints the vendor session id and binds fresh", async () => {
    const h = harness();
    await started(h);

    expect(h.queries[0]?.spec.binding.kind).toBe("fresh");
  });

  it("declares the pre-minted AgentId absent from the store, so no book is asked for", async () => {
    // Arrange.
    const h = harness();

    // Act.
    await started(h);

    // Assert. The id is a uuid minted moments ago; the store cannot hold a book
    // for it until this session's first write lands.
    expect(h.persistence.mintedAgents).toEqual([h.persistence.producer]);
  });

  it("adopts the pre-minted id as the main AgentId (R9)", async () => {
    const h = harness();
    const response = await started(h);
    const minted = h.queries[0]?.spec.binding.kind === "fresh" ? h.queries[0].spec.binding.sessionId : "";

    expect(
      response.result.case === "success" ? response.result.value.session?.vendorSessionId : undefined,
    ).toBe(minted);
  });

  it("takes the SESSION LOCK before the SDK is touched", async () => {
    // Two shims that both started a query would already be two writers on one
    // transcript by the time either discovered the other.
    const h = harness();
    await started(h);

    expect(h.locks).toHaveLength(1);
  });

  it("refuses with conversation_owned when another shim holds the lock", async () => {
    const h = harness({ lockThrows: true });

    expect(failureCause(await h.engine.startSession(freshRequest()))).toBe("conversationOwned");
  });

  it("does not create a query when the lock is refused", async () => {
    const h = harness({ lockThrows: true });
    await h.engine.startSession(freshRequest());

    expect(h.queries).toHaveLength(0);
  });

  it("reports the build identity the daemon compares against its deploy stamp", async () => {
    const h = harness();
    const response = await started(h);

    expect(
      response.result.case === "success" ? response.result.value.session?.runtime?.shimBuildSha : undefined,
    ).toBe("sha");
  });

  it("reports the agent binary version the vendor's own init stated", async () => {
    const h = harness();
    const response = await started(h);

    expect(
      response.result.case === "success"
        ? response.result.value.session?.runtime?.agentBinaryVersion
        : undefined,
    ).toBe("2.1.999");
  });

  it("passes NO model to the SDK when the fresh start named none", async () => {
    // Optional since landing 7: UNSET means the SDK's own default, and naming
    // an empty model would override that default with nothing.
    const h = harness();
    const pending = h.engine.startSession(freshRequestNoModel());
    const first = await untilQuery(h, 0);
    first.query.emit(
      initMessage({
        sessionId: first.spec.binding.kind === "fresh" ? first.spec.binding.sessionId : "",
        model: "claude-sonnet-5",
      }),
    );
    await pending;

    expect(h.queries[0]?.spec.model).toBeUndefined();
  });

  it("passes auto to the SDK when the fresh start named NO permission mode", async () => {
    // Owner ruling 2026-09-14: the unstated mode is `auto`, never the vendor's
    // `default`.
    const h = harness();
    const pending = h.engine.startSession(freshRequestNoFacts());
    const first = await untilQuery(h, 0);
    first.query.emit(
      initMessage({
        sessionId: first.spec.binding.kind === "fresh" ? first.spec.binding.sessionId : "",
      }),
    );
    await pending;

    expect(h.queries[0]?.spec.permissionMode).toBe("auto");
  });

  it("leaves effective_model UNSTATED when none was named and no init has landed", async () => {
    // THE MODEL IS AN INIT FACT, AND INIT COMES WITH THE FIRST TURN. A start
    // that named no model has not been told which one took effect, and the
    // fixed schema draws absence — inventing a name here would put a model in
    // every surface that nothing has actually chosen.
    const h = harness();

    const response = await h.engine.startSession(freshRequestNoModel());

    expect(
      response.result.case === "success"
        ? response.result.value.session?.effectiveModel?.name
        : undefined,
    ).toBe("");
  });

  it("pushes the model the SDK chose when the first turn's init names it", async () => {
    // AND THEN IT IS TOLD. The init that rides the first turn carries the model
    // the SDK settled on, and it reaches every surface as the same
    // `model_changed` a mid-session switch does.
    const h = harness();

    const seen = await pushedUpdates(
      h,
      (update) => (update.case === "modelChanged" ? update.value.effectiveModel?.name : undefined),
      async () => {
        await h.engine.startSession(freshRequestNoModel());
        h.queries[0]?.query.emit(
          initMessage({ sessionId: freshSessionId(h.queries[0].spec), model: "claude-sonnet-5" }),
        );
        await vi.waitFor(() => {
          expect(h.queries[0]?.query.calls).toContain("supportedModels");
        });
      },
    );

    expect(seen).toContain("claude-sonnet-5");
  });

  it("reports the model catalog from the vendor", async () => {
    // The catalog is set BEFORE the query is handed over: the start's own
    // live-signal round-trip reads it, so a suite that set it afterwards would
    // be asserting against the read that already happened.
    const h = harness({
      onQueryCreated: (query) => {
        query.models = [
          { value: "claude-opus-5", displayName: "Opus 5", description: "the big one", supportsEffort: true, supportedEffortLevels: ["low", "high"] },
        ];
      },
    });
    const pending = h.engine.startSession(freshRequest());
    const first = await untilQuery(h, 0);
    first.query.emit(
      initMessage({ sessionId: first.spec.binding.kind === "fresh" ? first.spec.binding.sessionId : "" }),
    );
    const response = await pending;

    expect(
      response.result.case === "success"
        ? response.result.value.session?.modelCatalog.map((option) => option.model?.name)
        : undefined,
    ).toEqual(["claude-opus-5"]);
  });

  it("states declared model capabilities and leaves undeclared ones unstated", async () => {
    const h = harness({
      onQueryCreated: (query) => {
        query.models = [{ value: "m", displayName: "M", description: "d" }];
      },
    });
    const pending = h.engine.startSession(freshRequest());
    const first = await untilQuery(h, 0);
    first.query.emit(
      initMessage({ sessionId: first.spec.binding.kind === "fresh" ? first.spec.binding.sessionId : "" }),
    );
    const response = await pending;

    expect(
      response.result.case === "success"
        ? response.result.value.session?.modelCatalog[0]?.capabilities
        : undefined,
    ).toBeUndefined();
  });

  it("STARTS the keep-alive cadence before success returns", async () => {
    const h = harness();
    await started(h);

    expect(h.scheduler.handlers.length).toBeGreaterThan(0);
  });

  it("refuses a SECOND StartSession — one shim serves exactly one session", async () => {
    const h = harness();
    await started(h);

    expect(failureCause(await h.engine.startSession(freshRequest()))).toBe("alreadyStarted");
  });

  it("refuses with vendor_start_failed when the query cannot be created", async () => {
    const h = harness();
    const engine = createEngine({
      probeApiReachable: new ScriptedProbe().probe,
      persistence: h.persistence,
      fold: h.fold,
      createQuery: () => Promise.reject(new Error("the mocked vendor is not implemented")),
      runtime: { shimBuildSha: "sha", sdkVersion: "0.3.220" },
      env: { stateDir: h.stateDir, configDir: h.configDir, cwd: h.cwd },
      nowMs: () => 1,
      scheduler: h.scheduler,
      acquireLock: () => () => undefined,
      acquireWorkspaceLock: () => () => undefined,
    });

    expect(failureCause(await engine.startSession(freshRequest()))).toBe("vendorStartFailed");
  });

  it("releases the session lock when the vendor start fails", async () => {
    const h = harness();
    const engine = createEngine({
      probeApiReachable: new ScriptedProbe().probe,
      persistence: h.persistence,
      fold: h.fold,
      createQuery: () => Promise.reject(new Error("no")),
      runtime: { shimBuildSha: "sha", sdkVersion: "0.3.220" },
      env: { stateDir: h.stateDir, configDir: h.configDir, cwd: h.cwd },
      nowMs: () => 1,
      scheduler: h.scheduler,
      acquireLock: (id) => {
        h.locks.push(id);
        return () => {
          h.released.push(id);
        };
      },
      acquireWorkspaceLock: (dir) => {
        h.workspaceLocks.push(dir);
        return () => {
          h.released.push(`workspace:${dir}`);
        };
      },
    });
    await engine.startSession(freshRequest());

    expect(h.released).toEqual(expect.arrayContaining([expect.stringMatching(/^workspace:/)]));
  });

  it("UN-NAMES the record plane's writer when the vendor start fails", async () => {
    // A failed start settled an identity and named the writer from it, then
    // abandoned the attempt. Leaving the name behind made the NEXT StartSession
    // — which settles a different identity — hit setProducer's re-key guard and
    // escape as an unhandled Internal, on a verb that has a typed refusal for
    // every real condition.
    const h = harness();
    const engine = createEngine({
      probeApiReachable: new ScriptedProbe().probe,
      persistence: h.persistence,
      fold: h.fold,
      createQuery: () => Promise.reject(new Error("no")),
      runtime: { shimBuildSha: "sha", sdkVersion: "0.3.220" },
      env: { stateDir: h.stateDir, configDir: h.configDir, cwd: h.cwd },
      nowMs: () => 1,
      scheduler: h.scheduler,
      acquireLock: () => () => undefined,
      acquireWorkspaceLock: () => () => undefined,
    });

    await engine.startSession(freshRequest());

    expect(h.persistence.producer).toBeUndefined();
  });

  it("FORGETS the identity a failed start minted", async () => {
    // The file is written before the query is created so a crash between the
    // mint and the first record stays recoverable — but a start that never
    // reached a query left no conversation for that identity to name, and
    // keeping it would hand the next reader an AgentId for a conversation the
    // vendor never opened.
    const h = harness();
    const engine = createEngine({
      probeApiReachable: new ScriptedProbe().probe,
      persistence: h.persistence,
      fold: h.fold,
      createQuery: () => Promise.reject(new Error("no")),
      runtime: { shimBuildSha: "sha", sdkVersion: "0.3.220" },
      env: { stateDir: h.stateDir, configDir: h.configDir, cwd: h.cwd },
      nowMs: () => 1,
      scheduler: h.scheduler,
      acquireLock: () => () => undefined,
      acquireWorkspaceLock: () => () => undefined,
    });

    await engine.startSession(freshRequest());

    expect(existsSync(agentIdPath(h.stateDir, workspaceLockKey(h.cwd)))).toBe(false);
  });

  it("keeps an identity an EARLIER session established when a later start fails", async () => {
    // The abandonment path may only discard what THIS attempt minted. An
    // identity a previous session persisted names a real conversation, and
    // discarding it would split that conversation's book at the failed start.
    const h = harness();
    const persistedBefore = {
      original_vendor_session_id: "established-by-an-earlier-session",
      workspace_key: workspaceLockKey(h.cwd),
      minted_at_ms: 1,
    };
    mkdirSync(path.dirname(agentIdPath(h.stateDir, workspaceLockKey(h.cwd))), {
      recursive: true,
    });
    writeFileSync(
      agentIdPath(h.stateDir, workspaceLockKey(h.cwd)),
      JSON.stringify(persistedBefore),
      "utf8",
    );
    const engine = createEngine({
      probeApiReachable: new ScriptedProbe().probe,
      persistence: h.persistence,
      fold: h.fold,
      createQuery: () => Promise.reject(new Error("no")),
      runtime: { shimBuildSha: "sha", sdkVersion: "0.3.220" },
      env: { stateDir: h.stateDir, configDir: h.configDir, cwd: h.cwd },
      nowMs: () => 1,
      scheduler: h.scheduler,
      acquireLock: () => () => undefined,
      acquireWorkspaceLock: () => () => undefined,
    });

    await engine.startSession(freshRequest());

    expect(existsSync(agentIdPath(h.stateDir, workspaceLockKey(h.cwd)))).toBe(true);
  });

  it("a retry after a failed start succeeds, settling its own identity", async () => {
    // THE WHOLE POINT of leaving the engine as it was found: the shim the
    // daemon already has can serve the conversation once the condition clears.
    const h = harness();
    let starts = 0;
    const engine = createEngine({
      probeApiReachable: new ScriptedProbe().probe,
      persistence: h.persistence,
      fold: h.fold,
      createQuery: (spec) => {
        starts += 1;
        if (starts === 1) return Promise.reject(new Error("the vendor was not startable yet"));
        const query = new ScriptedQuery();
        h.queries.push({ spec, query });
        return Promise.resolve(query);
      },
      runtime: { shimBuildSha: "sha", sdkVersion: "0.3.220" },
      env: { stateDir: h.stateDir, configDir: h.configDir, cwd: h.cwd },
      nowMs: () => 1,
      scheduler: h.scheduler,
      acquireLock: () => () => undefined,
      acquireWorkspaceLock: () => () => undefined,
    });
    expect(failureCause(await engine.startSession(freshRequest()))).toBe("vendorStartFailed");

    const pending = engine.startSession(freshRequest());
    const created = await untilQuery(h, 0);
    const sessionId =
      created.spec.binding.kind === "fresh" ? created.spec.binding.sessionId : "";
    created.query.emit(initMessage({ sessionId }));
    const retried = await pending;

    expect(retried.result.case).toBe("success");
    expect(h.persistence.producer).toBe(sessionId);
  });

  it("refuses conversation_owned when another shim holds the WORKSPACE lock", async () => {
    // Both claims answer the SAME arm: from the daemon's side "someone else
    // owns this conversation" is one fact, whichever kernel lock proved it.
    const h = harness({ workspaceLockThrows: true });

    expect(failureCause(await h.engine.startSession(freshRequest()))).toBe("conversationOwned");
  });

  it("a refused WORKSPACE claim releases the session lock it had already taken", async () => {
    // The session claim is taken first, so a workspace refusal must hand it
    // back; a shim that kept it would own a conversation it refused to serve.
    const h = harness({ workspaceLockThrows: true });

    await h.engine.startSession(freshRequest());

    expect(h.released).toEqual(h.locks);
  });

  it("refuses lock_holder_unavailable when the SESSION lock holder cannot be spawned", async () => {
    // Arrange: nobody owns the conversation; the shim's own helper is missing.
    const h = harness({ lockRefusal: unspawnableHolder() });

    // Act
    const response = await h.engine.startSession(freshRequest());

    // Assert
    expect(failureCause(response)).toBe("lockHolderUnavailable");
  });

  it("refuses lock_holder_unavailable when the WORKSPACE lock holder cannot be spawned", async () => {
    // Arrange
    const h = harness({ workspaceLockRefusal: unspawnableHolder() });

    // Act
    const response = await h.engine.startSession(freshRequest());

    // Assert
    expect(failureCause(response)).toBe("lockHolderUnavailable");
  });

  it("carries the holder binary and how it failed on lock_holder_unavailable", async () => {
    // Arrange
    const h = harness({ lockRefusal: unspawnableHolder() });

    // Act
    const response = await h.engine.startSession(freshRequest());

    // Assert
    const cause = response.result.case === "failure" ? response.result.value.cause : undefined;
    const failure = cause?.case === "lockHolderUnavailable" ? cause.value.failure : undefined;
    expect({ binary: failure?.binary, how: failure?.how.case }).toEqual({
      binary: "/missing/shim-lock",
      how: "spawnFailed",
    });
  });

  it("refuses a holder that EXITED before holding the lock as lock_holder_unavailable, never conversation_owned", async () => {
    // Arrange: exit 1 is the helper failing, not a genuine owner.
    const h = harness({ lockRefusal: exitedHolder() });

    // Act
    const response = await h.engine.startSession(freshRequest());

    // Assert
    expect(failureCause(response)).toBe("lockHolderUnavailable");
  });

  it("words the lock_holder_unavailable detail as the helper failing and nobody owning the conversation", async () => {
    // Arrange
    const h = harness({ lockRefusal: exitedHolder() });

    // Act
    const response = await h.engine.startSession(freshRequest());

    // Assert
    expect(response.result.case === "failure" ? response.result.value.detail : undefined).toBe(
      "this shim's lock helper /bin/shim-lock exited with code 1 before taking the lock (EACCES); " +
        "no other process is known to own this conversation",
    );
  });

  it("an unspawnable WORKSPACE holder still releases the session lock it had already taken", async () => {
    // Arrange
    const h = harness({ workspaceLockRefusal: unspawnableHolder() });

    // Act
    await h.engine.startSession(freshRequest());

    // Assert
    expect(h.released).toEqual(h.locks);
  });

  it("no lock of either kind is taken before StartSession", async () => {
    // AN INERT SHIM HOLDS NOTHING. A prelaunched shim must be able to sit
    // beside the live one it will replace, which it cannot do while holding
    // the live shim's workspace lock.
    const h = harness();

    expect(h.locks).toEqual([]);
    expect(h.workspaceLocks).toEqual([]);
  });
});

/**
 * A START THE VENDOR OPENED AND THEN REFUSED.
 *
 * GROUNDED, 2026-09-13: a `SessionStart:resume` hook that cannot run
 * (`powershell` is not on this box's PATH) blocked the opening of every boot in
 * one workspace. The query was already live, so the hook message went through
 * the converter and landed rows under the producer this attempt had just named
 * — and the abandonment path then tried to UN-NAME it, which the writer refuses
 * outright. The refusal escaped `StartSession` as an unhandled `Internal`, on
 * both the boot's start and the one a cold-gate answer re-opens with.
 */
describe("a start REFUSED after the vendor already wrote rows", () => {
  /**
   * HOLD THE LIVE SIGNAL ON THE FIRST QUERY ONLY.
   *
   * A start settles on one answered control round-trip, so a scripted query
   * that answers it would settle these starts before the blocking hook could —
   * and the subject here is what happens AFTER a hook refuses one. The RETRY's
   * query must still answer, or there is no successful retry to assert.
   */
  const holdFirstStart = {
    onQueryCreated: (query: ScriptedQuery, _spec: QuerySpec, index: number): void => {
      if (index === 0) query.holdModels();
    },
  };

  /** A fold that records one row for the very message that blocks the start. */
  function writesOnTheBlockingHook(h: Harness): void {
    h.fold.entriesFor = (message) =>
      message.type === "system" ? [foldEntry({ kind: "session_update", update: create(conversationv1.SessionUpdateSchema, {}) }, "hook")] : [];
  }

  /** Start fresh, let the vendor open, then have a hook block the opening. */
  async function refusedAfterWriting(h: Harness): Promise<shimv1.StartSessionResponse> {
    writesOnTheBlockingHook(h);
    const pending = h.engine.startSession(freshRequest());
    (await untilQuery(h, 0)).query.emit(hookResponse({ outcome: "error", output: "not today" }));
    return pending;
  }

  it("answers the typed refusal instead of throwing the writer's un-naming refusal", async () => {
    const h = harness(holdFirstStart);

    expect(failureCause(await refusedAfterWriting(h))).toBe("vendorStartFailed");
  });

  it("KEEPS the writer named, because rows already carry the name", async () => {
    const h = harness(holdFirstStart);

    await refusedAfterWriting(h);

    expect(h.persistence.producer).toBe(freshSessionId(h.queries[0].spec));
  });

  it("KEEPS the identity file, because the rows are on the book it names", async () => {
    const h = harness(holdFirstStart);

    await refusedAfterWriting(h);

    expect(existsSync(agentIdPath(h.stateDir, workspaceLockKey(h.cwd)))).toBe(true);
  });

  it("a retry reuses the identity the refused start recorded under", async () => {
    // Minting a second id would key the retry's rows to a book the first
    // attempt's rows are not on, splitting one conversation in two.
    const h = harness(holdFirstStart);
    await refusedAfterWriting(h);
    const recorded = freshSessionId(h.queries[0].spec);

    const pending = h.engine.startSession(freshRequest());
    const retry = await untilQuery(h, 1);
    retry.query.emit(initMessage({ sessionId: freshSessionId(retry.spec) }));
    await pending;

    expect(freshSessionId(retry.spec)).toBe(recorded);
  });

  it("the retry succeeds, rather than hitting the re-key guard", async () => {
    const h = harness(holdFirstStart);
    await refusedAfterWriting(h);

    const pending = h.engine.startSession(freshRequest());
    const retry = await untilQuery(h, 1);
    retry.query.emit(initMessage({ sessionId: freshSessionId(retry.spec) }));

    expect((await pending).result.case).toBe("success");
  });

  it("answers the typed refusal on a RESUME whose hook blocks the opening", async () => {
    // The live case: a resumed workspace, blocked on every boot.
    const h = harness({ nowMs: 1_000_100, ...holdFirstStart });
    writeTranscript(h.configDir, h.cwd, "resume-1", [assistantLine()]);
    writesOnTheBlockingHook(h);

    const pending = h.engine.startSession(resumeRequest("resume-1"));
    (await untilQuery(h, 0)).query.emit(hookResponse({ outcome: "error", output: "not today" }));

    expect(failureCause(await pending)).toBe("vendorStartFailed");
  });

  it("answers the typed refusal on the start a COLD-GATE answer re-opens with", async () => {
    // The second live case: the owner answered the gate with `clear`, and the
    // un-naming refusal came back through AnswerColdGate's transport.
    const h = harness({ nowMs: 1_000_000 + 10 * 60 * 1000, ...holdFirstStart });
    writeTranscript(h.configDir, h.cwd, "resume-1", [assistantLine()]);
    writesOnTheBlockingHook(h);
    const clear = create(conversationv1.SessionColdRemediationSchema, {
      remediation: { case: "clear", value: create(conversationv1.SessionColdClearSchema, {}) },
    });

    const pending = h.engine.startSession(resumeRequest("resume-1", clear));
    (await untilQuery(h, 0)).query.emit(hookResponse({ outcome: "error", output: "not today" }));

    expect(failureCause(await pending)).toBe("vendorStartFailed");
  });
});

describe("StartSession, resume", () => {
  it("refuses an id with no transcript in this workspace", async () => {
    const h = harness();

    expect(failureCause(await h.engine.startSession(resumeRequest("nope")))).toBe("unknownSession");
  });

  it("REFUSES a lapsed resume with its cost, before a token is spent", async () => {
    const h = harness({ nowMs: 1_000_000 + 10 * 60 * 1000 });
    writeTranscript(h.configDir, h.cwd, "resume-1", [assistantLine()]);

    expect(failureCause(await h.engine.startSession(resumeRequest("resume-1")))).toBe("cold");
  });

  it("states the context the refusal is protecting", async () => {
    const h = harness({ nowMs: 1_000_000 + 10 * 60 * 1000 });
    writeTranscript(h.configDir, h.cwd, "resume-1", [assistantLine()]);

    const response = await h.engine.startSession(resumeRequest("resume-1"));
    const failure = response.result.case === "failure" ? response.result.value : undefined;
    expect(failure?.cause.case === "cold" ? failure.cause.value.contextTokens : undefined).toBe(
      BigInt(FIXTURE_CONTEXT_TOKENS),
    );
  });

  it("CONTINUES a lapsed resume whose context is under the cold-gate floor, without asking", async () => {
    // OWNER RULING, 2026-09-13: under 70,000 tokens the cold read is not worth
    // a gate, so the session continues automatically however long the lapse.
    const h = harness({ nowMs: 1_000_000 + 10 * 60 * 1000 });
    writeTranscript(h.configDir, h.cwd, "resume-1", [smallAssistantLine()]);

    const pending = h.engine.startSession(resumeRequest("resume-1"));
    (await untilQuery(h, 0)).query.emit(initMessage({ sessionId: "resume-1" }));

    expect((await pending).result.case).toBe("success");
  });

  it("does not create a query for a refused cold resume", async () => {
    const h = harness({ nowMs: 1_000_000 + 10 * 60 * 1000 });
    writeTranscript(h.configDir, h.cwd, "resume-1", [assistantLine()]);

    await h.engine.startSession(resumeRequest("resume-1"));

    expect(h.queries).toHaveLength(0);
  });

  it("PROCEEDS on a warm resume", async () => {
    const h = harness({ nowMs: 1_000_100 });
    writeTranscript(h.configDir, h.cwd, "resume-1", [assistantLine()]);
    const pending = h.engine.startSession(resumeRequest("resume-1"));
    (await untilQuery(h, 0)).query.emit(initMessage({ sessionId: "resume-1" }));

    expect((await pending).result.case).toBe("success");
  });

  it("does NOT declare the AgentId minted here, because an earlier session may have written under it", async () => {
    // Arrange.
    const h = harness({ nowMs: 1_000_100 });
    writeTranscript(h.configDir, h.cwd, "resume-1", [assistantLine()]);

    // Act.
    const pending = h.engine.startSession(resumeRequest("resume-1"));
    (await untilQuery(h, 0)).query.emit(initMessage({ sessionId: "resume-1" }));
    await pending;

    // Assert. Claiming absence here would serve an empty opening page over a
    // book that holds the whole conversation.
    expect(h.persistence.mintedAgents).toEqual([]);
  });

  it("binds resume, not fresh", async () => {
    const h = harness({ nowMs: 1_000_100 });
    writeTranscript(h.configDir, h.cwd, "resume-1", [assistantLine()]);
    const pending = h.engine.startSession(resumeRequest("resume-1"));
    (await untilQuery(h, 0)).query.emit(initMessage({ sessionId: "resume-1" }));
    await pending;

    expect(h.queries[0]?.spec.binding).toEqual({ kind: "resume", resumeSessionId: "resume-1" });
  });

  it("a resume MARKED as a rebind files rows under the RESUMED conversation's book", async () => {
    // Arrange. The workspace's persisted book is another conversation's; the
    // user chose `resume-1` through BindWorkspaceSession.
    const h = harness({ nowMs: 1_000_100 });
    persistIdentity(h, "a-previous-conversation");
    writeTranscript(h.configDir, h.cwd, "resume-1", [assistantLine()]);

    // Act.
    const pending = h.engine.startSession(resumeRequest("resume-1", undefined, { rebind: true }));
    (await untilQuery(h, 0)).query.emit(initMessage({ sessionId: "resume-1" }));
    await pending;

    // Assert. The producer IS the book every row of this session lands on,
    // and the daemon reads history under exactly that name.
    expect(h.persistence.producer).toBe("resume-1");
  });

  it("an UNMARKED resume keeps the persisted book, so a restart cannot orphan its rows", async () => {
    // Arrange. Identical to the rebind case but for the missing marker.
    const h = harness({ nowMs: 1_000_100 });
    persistIdentity(h, "a-previous-conversation");
    writeTranscript(h.configDir, h.cwd, "resume-1", [assistantLine()]);

    // Act.
    const pending = h.engine.startSession(resumeRequest("resume-1"));
    (await untilQuery(h, 0)).query.emit(initMessage({ sessionId: "resume-1" }));
    await pending;

    // Assert.
    expect(h.persistence.producer).toBe("a-previous-conversation");
  });

  it("RECOVERS the model the conversation was last running under", async () => {
    const h = harness({ nowMs: 1_000_100 });
    writeTranscript(h.configDir, h.cwd, "resume-1", [assistantLine()]);
    const pending = h.engine.startSession(resumeRequest("resume-1"));
    (await untilQuery(h, 0)).query.emit(initMessage({ sessionId: "resume-1", model: "claude-opus-5" }));
    await pending;

    expect(h.queries[0]?.spec.model).toBe("claude-opus-5");
  });

  it("never resumes on the synthetic marker the CLI wrote for a refused request", async () => {
    // Arrange: a real answer, then the CLI's own notice for a refused request.
    const h = harness({ nowMs: 1_000_100 });
    writeTranscript(h.configDir, h.cwd, "resume-1", [
      assistantLine(),
      assistantLine({
        uuid: "u-2",
        isApiErrorMessage: true,
        message: { model: "<synthetic>", usage: { input_tokens: 0, cache_read_input_tokens: 0 } },
      }),
    ]);

    // Act
    const pending = h.engine.startSession(resumeRequest("resume-1"));
    (await untilQuery(h, 0)).query.emit(initMessage({ sessionId: "resume-1", model: "claude-opus-5" }));
    await pending;

    // Assert: the conversation's last REAL model, never the marker.
    expect(h.queries[0]?.spec.model).toBe("claude-opus-5");
  });

  it("RECOVERS the permission mode the last user record ran under", async () => {
    const h = harness({ nowMs: 1_000_100 });
    writeTranscript(h.configDir, h.cwd, "resume-1", [
      { type: "user", permissionMode: "acceptEdits", uuid: "u-0" },
      assistantLine(),
    ]);
    const pending = h.engine.startSession(resumeRequest("resume-1"));
    (await untilQuery(h, 0)).query.emit(initMessage({ sessionId: "resume-1" }));
    await pending;

    expect(h.queries[0]?.spec.permissionMode).toBe("acceptEdits");
  });

  it("pays for the read when the caller says pay", async () => {
    const h = harness({ nowMs: 1_000_000 + 10 * 60 * 1000 });
    writeTranscript(h.configDir, h.cwd, "resume-1", [assistantLine()]);
    const pay = create(conversationv1.SessionColdRemediationSchema, {
      remediation: { case: "pay", value: create(conversationv1.SessionColdPaySchema, {}) },
    });
    const pending = h.engine.startSession(resumeRequest("resume-1", pay));
    (await untilQuery(h, 0)).query.emit(initMessage({ sessionId: "resume-1" }));

    expect((await pending).result.case).toBe("success");
  });

  it("CLEAR binds a newly minted vendor session id — no API call, context discarded", async () => {
    const h = harness({ nowMs: 1_000_000 + 10 * 60 * 1000 });
    writeTranscript(h.configDir, h.cwd, "resume-1", [assistantLine()]);
    const clear = create(conversationv1.SessionColdRemediationSchema, {
      remediation: { case: "clear", value: create(conversationv1.SessionColdClearSchema, {}) },
    });
    const pending = h.engine.startSession(resumeRequest("resume-1", clear));
    const first = await untilQuery(h, 0);
    const binding = first.spec.binding;
    first.query.emit(initMessage({ sessionId: binding.kind === "fresh" ? binding.sessionId : "" }));
    await pending;

    expect(binding.kind).toBe("fresh");
  });

  it("CLEAR does not resume the old transcript", async () => {
    const h = harness({ nowMs: 1_000_000 + 10 * 60 * 1000 });
    writeTranscript(h.configDir, h.cwd, "resume-1", [assistantLine()]);
    const clear = create(conversationv1.SessionColdRemediationSchema, {
      remediation: { case: "clear", value: create(conversationv1.SessionColdClearSchema, {}) },
    });
    const pending = h.engine.startSession(resumeRequest("resume-1", clear));
    const first = await untilQuery(h, 0);
    const binding = first.spec.binding;
    first.query.emit(initMessage({ sessionId: binding.kind === "fresh" ? binding.sessionId : "" }));
    await pending;

    expect(binding.kind === "fresh" ? binding.sessionId : "").not.toBe("resume-1");
  });
});

describe("the vendor's own facts", () => {
  it("pushes the model the vendor reported", async () => {
    const h = harness();
    const stream = h.engine.pushes.subscribe()[Symbol.asyncIterator]();
    await started(h);
    const seen: string[] = [];
    for (let index = 0; index < 4; index++) {
      const step = await stream.next();
      if (step.done === true) break;
      seen.push(step.value.update.case ?? "");
    }

    expect(seen).toContain("modelChanged");
  });

  it("does NOT rotate to conversation_reset's new_conversation_id", async () => {
    // EVIDENCE, from the real /clear capture: `new_conversation_id` is a uuid
    // NOTHING later uses -- no transcript is written under it, no init
    // announces it, no resume takes it. Publishing it as the new identity would
    // name an id that does not exist and hand the daemon a dead resume handle.
    const h = harness();
    const response = await started(h);
    const original =
      response.result.case === "success" ? (response.result.value.session?.vendorSessionId ?? "") : "";
    const stream = h.engine.pushes.subscribe()[Symbol.asyncIterator]();

    await h.engine.onSdkMessage({
      type: "conversation_reset",
      new_conversation_id: "an-id-nothing-uses",
      uuid: "00000000-0000-4000-8000-000000000009",
      session_id: original,
    } as never);
    // A rotation would be pushed synchronously with the message; the init that
    // follows is what carries the real one.
    await h.engine.onSdkMessage(initMessage({ sessionId: "the-id-the-session-moved-to" }));

    const seen: string[] = [];
    for (let index = 0; index < 6; index++) {
      const step = await stream.next();
      if (step.done === true) break;
      const update = step.value.update;
      if (update.case !== "identityRotated") continue;
      seen.push(update.value.vendorSessionId);
      break;
    }
    expect(seen).not.toContain("an-id-nothing-uses");
  });

  it("rotates to the id the post-reset init announces", async () => {
    const h = harness();
    const response = await started(h);
    const original =
      response.result.case === "success" ? (response.result.value.session?.vendorSessionId ?? "") : "";
    const stream = h.engine.pushes.subscribe()[Symbol.asyncIterator]();

    await h.engine.onSdkMessage({
      type: "conversation_reset",
      new_conversation_id: "an-id-nothing-uses",
      uuid: "00000000-0000-4000-8000-000000000009",
      session_id: original,
    } as never);
    await h.engine.onSdkMessage(initMessage({ sessionId: "the-id-the-session-moved-to" }));
    // The rotation writes the link files, so the push lands a tick later.
    await new Promise((resolve) => setImmediate(resolve));

    let rotatedTo = "";
    for (let index = 0; index < 16; index++) {
      const step = await stream.next();
      if (step.done === true) break;
      const update = step.value.update;
      if (update.case !== "identityRotated") continue;
      rotatedTo = update.value.vendorSessionId;
      break;
    }
    expect(rotatedTo).toBe("the-id-the-session-moved-to");
  });

  it("ROTATES the vendor id on a conversation reset while keeping the AgentId", async () => {
    const h = harness();
    const response = await started(h);
    const original =
      response.result.case === "success" ? (response.result.value.session?.vendorSessionId ?? "") : "";

    await h.engine.onSdkMessage({
      type: "conversation_reset",
      new_conversation_id: "rotated-2",
      uuid: "00000000-0000-4000-8000-000000000009",
      session_id: original,
    } as never);

    const history = await h.engine.readHistory(
      create(shimv1.ReadHistoryRequestSchema, {
        pageSize: 1,
        position: { case: "first", value: create(shimv1.ReadHistoryFirstSchema, {}) },
      }),
    );
    expect(history.result.case).toBe("success");
  });

  it("CONCLUDES the open turn with a failure terminal when the query dies", async () => {
    // query_died is a SESSION fact; a consumer watching the AGENT -- the one
    // actually waiting on the turn -- would otherwise see its stream simply
    // stop producing, unable to tell a dead query from a slow one.
    const h = harness();
    await started(h);
    await h.engine.startTurn(
      create(shimv1.StartTurnRequestSchema, {
        turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }),
        said: textSaid("go"),
        origin: conversationv1.PromptOrigin.USER_SENT,
        pageSize: 5,
      }),
    );

    h.queries[0]?.query.end();
    await new Promise((resolve) => setImmediate(resolve));

    const terminal = h.persistence.buffered.find(
      (entry) => entry.item.kind === "frame" && entry.item.frame.result.case === "failure",
    );
    expect(terminal).toBeDefined();
  });

  it("writes NO terminal when the query dies between turns", async () => {
    // A session that lost its query with nothing open has no turn to conclude.
    const h = harness();
    await started(h);

    h.queries[0]?.query.end();
    await new Promise((resolve) => setImmediate(resolve));

    const terminal = h.persistence.buffered.find(
      (entry) => entry.item.kind === "frame" && entry.item.frame.result.case === "failure",
    );
    expect(terminal).toBeUndefined();
  });

  it("reports the query's death as an unexpected EOF", async () => {
    const h = harness();
    await started(h);
    const stream = h.engine.pushes.subscribe()[Symbol.asyncIterator]();
    h.queries[0]?.query.end();

    const seen: string[] = [];
    for (let index = 0; index < 12 && !seen.includes("queryDied"); index++) {
      const step = await stream.next();
      if (step.done === true) break;
      seen.push(step.value.update.case ?? "");
    }
    expect(seen).toContain("queryDied");
  });
});

describe("the fold's calls at a query's end", () => {
  it("lets the fold go of everything when the query dies", async () => {
    // Arrange: nothing the dead query announced can settle any more.
    const h = harness();
    await started(h);

    // Act
    h.queries[0]?.query.end();
    await new Promise((resolve) => setImmediate(resolve));

    // Assert
    expect(h.fold.queryEnds).toEqual(["the vendor query died: the vendor query ended without being asked to"]);
  });

  it("lets the fold go of everything when the query is replaced", async () => {
    // Arrange: the keep-alive rewind replaces the query before the next prompt.
    const h = harness();
    await started(h);
    await realTurn(h, "turn-0", [assistantMessage("real-assistant-uuid")]);
    await keepaliveTurn(h, []);

    // Act
    await realPrompt(h, "turn-1");

    // Assert
    expect(h.fold.queryEnds).toEqual(["the query was replaced"]);
  });
});

/** Let pending microtasks and I/O settle, up to a bound, until `done` holds. */
async function settledUntil(done: () => boolean): Promise<void> {
  for (let index = 0; index < 1_000 && !done(); index++) {
    await new Promise((resolve) => setImmediate(resolve));
  }
}

describe("the store writer's backpressure", () => {
  it("does not read the next vendor message while the writer's backlog is past its mark", async () => {
    // Arrange. The writer reports a backlog episode that has not drained.
    const h = harness();
    await started(h);
    let drained: () => void = () => undefined;
    h.persistence.backlog = new Promise<void>((resolve) => {
      drained = resolve;
    });
    const query = (await untilQuery(h, 0)).query;

    // Act.
    query.emit(hookResponse({ uuid: "00000000-0000-4000-8000-00000000000a" }));
    query.emit(hookResponse({ uuid: "00000000-0000-4000-8000-00000000000b" }));
    await settledUntil(() => h.fold.seen.length >= 2);
    await settledUntil(() => false);

    // Assert. The first was folded; the second waits for the backlog.
    expect(h.fold.seen).toHaveLength(2);
    drained();
  });

  it("reads the next vendor message once the backlog drains", async () => {
    // Arrange.
    const h = harness();
    await started(h);
    let drained: () => void = () => undefined;
    h.persistence.backlog = new Promise<void>((resolve) => {
      drained = resolve;
    });
    const query = (await untilQuery(h, 0)).query;
    query.emit(hookResponse({ uuid: "00000000-0000-4000-8000-00000000000a" }));
    query.emit(hookResponse({ uuid: "00000000-0000-4000-8000-00000000000b" }));
    await settledUntil(() => h.fold.seen.length >= 2);

    // Act.
    drained();
    await settledUntil(() => h.fold.seen.length >= 3);

    // Assert.
    expect(h.fold.seen).toHaveLength(3);
  });
});

describe("the turn loop", () => {
  it("hands the fold every SDK message", async () => {
    const h = harness();
    await started(h);

    expect(h.fold.seen.map((message) => message.type)).toEqual(["system"]);
  });

  it("tells the fold which turn is open", async () => {
    const h = harness();
    await started(h);
    await h.engine.startTurn(
      create(shimv1.StartTurnRequestSchema, {
        turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }),
        said: textSaid("go"),
        origin: conversationv1.PromptOrigin.USER_SENT,
        pageSize: 5,
      }),
    );
    h.queries[0]?.query.emit(answering(h, resultMessage()));
    await new Promise((resolve) => setImmediate(resolve));

    expect(h.fold.contexts.at(-1)?.turnId?.value).toBe("turn-1");
  });

  it("CLOSES the turn on the fold's turnEnded", async () => {
    const h = harness();
    await started(h);
    await h.engine.startTurn(
      create(shimv1.StartTurnRequestSchema, {
        turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }),
        said: textSaid("go"),
        origin: conversationv1.PromptOrigin.USER_SENT,
        pageSize: 5,
      }),
    );
    h.queries[0]?.query.emit(answering(h, resultMessage()));
    await new Promise((resolve) => setImmediate(resolve));

    // A second StartTurn now succeeds, which is only true if the first closed.
    const second = await h.engine.startTurn(
      create(shimv1.StartTurnRequestSchema, {
        turn: create(conversationv1.TurnIdSchema, { value: "turn-2" }),
        said: textSaid("again"),
        origin: conversationv1.PromptOrigin.USER_SENT,
        pageSize: 5,
      }),
    );
    expect(second.result.case).toBe("success");
  });

  it("pushes context usage at the turn's end", async () => {
    const h = harness();
    await started(h);
    const before = h.queries[0]?.query.calls.filter((call) => call === "getContextUsage").length ?? 0;
    await h.engine.startTurn(
      create(shimv1.StartTurnRequestSchema, {
        turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }),
        said: textSaid("go"),
        origin: conversationv1.PromptOrigin.USER_SENT,
        pageSize: 5,
      }),
    );
    h.queries[0]?.query.emit(answering(h, resultMessage()));
    await new Promise((resolve) => setImmediate(resolve));

    const after = h.queries[0]?.query.calls.filter((call) => call === "getContextUsage").length ?? 0;
    expect(after).toBeGreaterThan(before);
  });

  it("re-probes account usage at the turn's end", async () => {
    // Account usage is PULLED from the vendor, so a session that probed once at
    // StartSession would never notice a limit being approached.
    const h = harness();
    await started(h);
    const usageCall = "usage";
    const before = h.queries[0]?.query.calls.filter((call) => call === usageCall).length ?? 0;
    await h.engine.startTurn(
      create(shimv1.StartTurnRequestSchema, {
        turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }),
        said: textSaid("go"),
        origin: conversationv1.PromptOrigin.USER_SENT,
        pageSize: 5,
      }),
    );
    h.queries[0]?.query.emit(answering(h, resultMessage()));
    await new Promise((resolve) => setImmediate(resolve));

    const after = h.queries[0]?.query.calls.filter((call) => call === usageCall).length ?? 0;
    expect(after).toBeGreaterThan(before);
  });

  it("re-probes mcp server health at the turn's end", async () => {
    // Same reason: a server going down between turns is invisible otherwise.
    const h = harness();
    await started(h);
    const before = h.queries[0]?.query.calls.filter((call) => call === "mcpServerStatus").length ?? 0;
    await h.engine.startTurn(
      create(shimv1.StartTurnRequestSchema, {
        turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }),
        said: textSaid("go"),
        origin: conversationv1.PromptOrigin.USER_SENT,
        pageSize: 5,
      }),
    );
    h.queries[0]?.query.emit(answering(h, resultMessage()));
    await new Promise((resolve) => setImmediate(resolve));

    const after = h.queries[0]?.query.calls.filter((call) => call === "mcpServerStatus").length ?? 0;
    expect(after).toBeGreaterThan(before);
  });
});

/**
 * A TURN THE VENDOR STARTED ON ITS OWN IS ADOPTED (owner ruling 2026-09-27).
 *
 * The vendor runs turns no StartTurn asked for -- a background subagent's
 * hand-back arriving makes the main agent reply. Those turns used to run with
 * no turn open, so their frames and terminal carried no turn id and nothing
 * downstream learned they ran. Each is now opened as a real turn under a
 * shim-minted id, announced by a `VENDOR_STARTED` prompt row.
 */
describe("a turn the vendor started on its own", () => {
  /** Every adoption row the engine wrote, in order. */
  const adoptions = (h: Harness): conversationv1.AgentPrompt[] =>
    h.persistence.buffered.flatMap((entry) =>
      entry.item.kind === "prompt" && entry.item.prompt.origin === conversationv1.PromptOrigin.VENDOR_STARTED
        ? [entry.item.prompt]
        : [],
    );

  /** A top-level stream event: a reply frame of the running turn. */
  const streamEvent = (uuid: string): SdkMessage =>
    ({ type: "stream_event", uuid, session_id: "s", parent_tool_use_id: null, event: { type: "message_start" } }) as never;

  /** An assistant frame of a subagent: it names the work it belongs to. */
  const subagentReply = (uuid: string): SdkMessage =>
    ({ ...assistantMessage(uuid), parent_tool_use_id: "toolu_spawn" }) as never;

  /** A background task starting: detached work, which starts no turn. */
  const taskStarted = (uuid: string): SdkMessage =>
    ({
      type: "system",
      subtype: "task_started",
      task_id: "task-1",
      tool_use_id: "toolu_bg",
      description: "background",
      uuid,
      session_id: "s",
    }) as never;

  it.each([
    { name: "a top-level assistant reply", message: assistantMessage("m-1"), adopts: 1 },
    { name: "a top-level stream event", message: streamEvent("m-1"), adopts: 1 },
    { name: "a result with no reply before it", message: resultMessage("m-1"), adopts: 1 },
    { name: "a subagent's reply", message: subagentReply("m-1"), adopts: 0 },
    { name: "a background task's start", message: taskStarted("m-1"), adopts: 0 },
    { name: "the vendor's init", message: initMessage(), adopts: 0 },
  ])("adopts a turn on $name with no turn open: $adopts", async ({ message, adopts }) => {
    // Arrange
    const h = harness();
    await started(h);

    // Act
    await h.engine.onSdkMessage(message);

    // Assert
    expect(adoptions(h)).toHaveLength(adopts);
  });

  it("writes the adoption row ahead of the rows the turn's first message produced", async () => {
    // Arrange
    const h = harness();
    await started(h);
    h.fold.entriesFor = (message) => [foldEntry({ kind: "frame", frame: create(conversationv1.AgentFrameSchema, {}) }, `row-${message.uuid}`)];

    // Act
    await h.engine.onSdkMessage(assistantMessage("first-reply"));

    // Assert
    expect(h.persistence.buffered.map((entry) => entry.item.kind)).toEqual(["prompt", "frame"]);
  });

  it("writes the adoption row with no words said", async () => {
    // Arrange
    const h = harness();
    await started(h);

    // Act
    await h.engine.onSdkMessage(assistantMessage("first-reply"));

    // Assert
    expect(adoptions(h)[0]?.said?.content?.blocks).toEqual([]);
  });

  it("names the adopted turn to the fold for the turn's terminal", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await h.engine.onSdkMessage(assistantMessage("reply"));

    // Act
    await h.engine.onSdkMessage(resultMessage("result"));

    // Assert
    const adopted = adoptions(h)[0]?.id?.value;
    expect([adopted?.startsWith("adopted-"), h.fold.contexts.at(-1)?.turnId?.value === adopted]).toEqual([true, true]);
  });

  it("leaves a StartTurn's own turn unadopted and named to the fold", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");

    // Act
    await h.engine.onSdkMessage(answering(h, assistantMessage("reply")));

    // Assert
    expect([adoptions(h).length, h.fold.contexts.at(-1)?.turnId?.value]).toEqual([0, "turn-1"]);
  });

  it("gives two consecutive vendor-started turns distinct ids", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await h.engine.onSdkMessage(assistantMessage("first"));
    await h.engine.onSdkMessage(resultMessage("first-result"));

    // Act
    await h.engine.onSdkMessage(assistantMessage("second"));

    // Assert
    const [first, second] = adoptions(h).map((prompt) => prompt.id?.value);
    expect([adoptions(h).length, first === second]).toEqual([2, false]);
  });

  it("adopts nothing further on the running turn's later replies", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await h.engine.onSdkMessage(assistantMessage("first"));

    // Act
    await h.engine.onSdkMessage(assistantMessage("second"));

    // Assert
    expect(adoptions(h)).toHaveLength(1);
  });

  it("accepts a StartTurn while the adopted turn runs (ruled 2026-09-28)", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await h.engine.onSdkMessage(assistantMessage("reply"));

    // Act
    const response = await startDuring(h, "turn-1");

    // Assert
    expect(response.result.case).toBe("success");
  });

  it("delivers a StartTurn's prompt while the adopted turn runs, under a client uuid of its own", async () => {
    // Arrange
    const sends: SdkUserMessage[] = [];
    const h = harness({ drainSends: sends });
    await started(h);
    await h.engine.onSdkMessage(assistantMessage("reply"));

    // Act
    await startDuring(h, "turn-1");
    await new Promise((resolve) => setImmediate(resolve));

    // Assert
    expect(sends.map((send) => send.uuid)).toEqual([h.minted.at(-1)]);
  });

  it("frees the slot on the adopted turn's result", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await h.engine.onSdkMessage(assistantMessage("reply"));
    await h.engine.onSdkMessage(resultMessage("result"));

    // Act
    const response = await startDuring(h, "turn-1");

    // Assert
    expect(response.result.case).toBe("success");
  });

  it("names the adopted turn as the turn in flight", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await h.engine.onSdkMessage(assistantMessage("reply"));

    // Act
    const response = await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    // Assert
    const failure = response.result.case === "failure" ? response.result.value : undefined;
    const adopted = adoptions(h)[0]?.id?.value;
    const inFlight = failure?.cause.case === "live" ? failure.cause.value.turnInFlight?.value : undefined;
    expect([adopted?.startsWith("adopted-"), inFlight === adopted]).toEqual([true, true]);
  });

  /** The SessionStarted a new WatchSession re-announces. */
  async function reannounced(h: Harness): Promise<conversationv1.SessionStarted | undefined> {
    const iterator = h.engine.watchSession(create(shimv1.WatchSessionRequestSchema, {}))[Symbol.asyncIterator]();
    await iterator.next();
    const second = await nextPush(iterator);
    await iterator.return?.();
    return second.frame.case === "sessionStarted" ? second.frame.value : undefined;
  }

  it.each([
    {
      name: "a StartTurn's send waits behind the adopted turn",
      arrange: async (h: Harness) => {
        await h.engine.onSdkMessage(assistantMessage("reply"));
        await startDuring(h, "turn-1");
      },
      inFlight: (h: Harness) => adoptions(h)[0]?.id?.value,
      waiting: ["turn-1"],
    },
    {
      name: "the adopted turn runs alone",
      arrange: async (h: Harness) => {
        await h.engine.onSdkMessage(assistantMessage("reply"));
      },
      inFlight: (h: Harness) => adoptions(h)[0]?.id?.value,
      waiting: [],
    },
    {
      name: "the keep-alive holds the send slot beside the adopted turn",
      arrange: async (h: Harness) => {
        h.scheduler.fire(0);
        await new Promise((resolve) => setImmediate(resolve));
        await h.engine.onSdkMessage(assistantMessage("vendor-reply"));
      },
      inFlight: (h: Harness) => adoptions(h)[0]?.id?.value,
      waiting: [],
    },
    {
      name: "a StartTurn runs with no adopted turn",
      arrange: async (h: Harness) => {
        await startDuring(h, "turn-1");
      },
      inFlight: () => "turn-1",
      waiting: [],
    },
  ])("re-announces the turns waiting behind the turn in flight when $name", async ({ arrange, inFlight, waiting }) => {
    // Arrange
    const h = harness();
    await started(h);
    await arrange(h);

    // Act
    const start = await reannounced(h);

    // Assert
    expect({
      inFlight: start?.turnInFlight?.value,
      waiting: start?.turnsWaiting.map((turn) => turn.value),
    }).toEqual({ inFlight: inFlight(h), waiting });
  });

  it("charges a killed turn's stop result to the killed turn and adopts nothing", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");
    await h.engine.killTurn(
      create(shimv1.KillTurnRequestSchema, { turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }) }),
    );

    // Act
    await h.engine.onSdkMessage(resultMessage("stopped-result"));

    // Assert
    expect([adoptions(h).length, h.fold.contexts.at(-1)?.turnId?.value]).toEqual([0, "turn-1"]);
  });

  it("records the adoption at INFO with the turn id, the cause and the vendor session", async () => {
    // Arrange
    const h = harness();
    await started(h);
    const mark = logSinkMark();

    // Act
    await h.engine.onSdkMessage(assistantMessage("reply"));

    // Assert
    const record = logRecordsSince(mark).find((entry) => entry.message.startsWith("adopted a turn the vendor started"));
    expect({
      level: record?.level,
      turn: record?.context.turn_id,
      cause: typeof record?.context.cause,
      session: typeof record?.context.vendor_session_id,
    }).toEqual({ level: "info", turn: adoptions(h)[0]?.id?.value, cause: "string", session: "string" });
  });

  it("adopts a vendor turn beside the keep-alive as a served, untagged turn", async () => {
    // Arrange
    const h = harness();
    await started(h);
    h.scheduler.fire(0);
    await new Promise((resolve) => setImmediate(resolve));

    // Act
    await h.engine.onSdkMessage(assistantMessage("vendor-reply"));

    // Assert
    const row = h.persistence.buffered.find(
      (entry) => entry.item.kind === "prompt" && entry.item.prompt.origin === conversationv1.PromptOrigin.VENDOR_STARTED,
    );
    expect(row?.keepalive).toBe(false);
  });

  it("names the turn adopted beside the keep-alive to the fold for that turn's terminal", async () => {
    // Arrange
    const h = harness();
    await started(h);
    h.scheduler.fire(0);
    await new Promise((resolve) => setImmediate(resolve));
    await h.engine.onSdkMessage(assistantMessage("vendor-reply"));

    // Act
    await h.engine.onSdkMessage(resultMessage("vendor-result"));

    // Assert
    const adopted = adoptions(h)[0]?.id?.value;
    expect([adopted?.startsWith("adopted-"), h.fold.contexts.at(-1)?.turnId?.value === adopted]).toEqual([true, true]);
  });

  it("adopts the next vendor turn beside the keep-alive afresh once the first closed", async () => {
    // Arrange
    const h = harness();
    await started(h);
    h.scheduler.fire(0);
    await new Promise((resolve) => setImmediate(resolve));
    await h.engine.onSdkMessage(assistantMessage("vendor-reply"));
    await h.engine.onSdkMessage(resultMessage("vendor-result"));

    // Act
    await h.engine.onSdkMessage(assistantMessage("vendor-reply-2"));

    // Assert
    expect(adoptions(h)).toHaveLength(2);
  });
});

/**
 * A PROMPT SENT TO JOIN THE RUNNING TURN (`join_running_turn`, 2026-09-30).
 *
 * The prompt is pushed while the daemon's turn runs, and its row waits for the
 * vendor to decide its fate: folded into the running turn at a tool boundary,
 * or run as the vendor's next turn once the running turn leaves the slot.
 */
describe("a prompt joining the running turn", () => {
  /** Send `turnId` to join the running turn, and answer its client uuid. */
  async function join(h: Harness, turnId: string): Promise<string> {
    const response = await h.engine.startTurn(
      create(shimv1.StartTurnRequestSchema, {
        turn: create(conversationv1.TurnIdSchema, { value: turnId }),
        said: textSaid("also this"),
        origin: conversationv1.PromptOrigin.USER_SENT,
        pageSize: 5,
        joinRunningTurn: true,
      }),
    );
    if (response.result.case !== "success") throw new Error(`the join was refused: ${response.result.case}`);
    return h.minted.at(-1) ?? "";
  }

  /** A frame answering `answers` in a turn that consumed every send in `all`. */
  const consumed = (message: SdkMessage, answers: string, all: string[]): SdkMessage =>
    ({ ...message, user_message_uuid: answers, user_message_uuids: all }) as SdkMessage;

  /** Every prompt row written, as [turn, folded_into], in order. */
  const promptRows = (h: Harness): [string, string | undefined][] =>
    [...h.persistence.durable, ...h.persistence.buffered].flatMap((entry) =>
      entry.item.kind === "prompt"
        ? [[entry.item.prompt.id?.value ?? "", entry.item.prompt.foldedInto?.value] as [string, string | undefined]]
        : [],
    );

  /** Every row written after the first, as its turn and kind, in order. */
  const rowsAfterStart = (h: Harness): string[] =>
    h.persistence.buffered.flatMap((entry) => {
      if (entry.item.kind === "prompt") return [`prompt:${entry.item.prompt.id?.value ?? ""}`];
      if (entry.item.kind === "frame" && entry.item.frame.result.case !== undefined) {
        return [`terminal:${entry.turn?.value ?? ""}`];
      }
      return [];
    });

  it("writes the folded prompt's row naming the turn the vendor folded it into", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");
    const running = h.minted.at(-1) ?? "";
    const joined = await join(h, "turn-2");

    // Act
    await h.engine.onSdkMessage(consumed(assistantMessage("after-the-tool"), running, [running, joined]));

    // Assert
    expect(promptRows(h)).toEqual([
      ["turn-1", undefined],
      ["turn-2", "turn-1"],
    ]);
  });

  it("charges the running turn's frames after a fold to the running turn", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");
    const running = h.minted.at(-1) ?? "";
    const joined = await join(h, "turn-2");

    // Act
    await h.engine.onSdkMessage(consumed(assistantMessage("after-the-tool"), running, [running, joined]));

    // Assert
    expect(h.fold.contexts.at(-1)?.turnId?.value).toBe("turn-1");
  });

  it("opens no turn for a folded prompt: the slot is free once the running turn ends", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");
    const running = h.minted.at(-1) ?? "";
    const joined = await join(h, "turn-2");
    await h.engine.onSdkMessage(consumed(assistantMessage("after-the-tool"), running, [running, joined]));

    // Act
    await h.engine.onSdkMessage(consumed(resultMessage("turn-1-result"), running, [running, joined]));

    // Assert
    expect((await startDuring(h, "turn-3")).result.case).toBe("success");
  });

  it("writes the unfolded prompt's row as its own turn right after the running turn's end", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");
    const running = h.minted.at(-1) ?? "";
    await join(h, "turn-2");

    // Act
    await h.engine.onSdkMessage(consumed(resultMessage("turn-1-result"), running, [running]));

    // Assert
    expect(promptRows(h)).toEqual([
      ["turn-1", undefined],
      ["turn-2", undefined],
    ]);
  });

  it("holds the send slot for the unfolded prompt's turn", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");
    const running = h.minted.at(-1) ?? "";
    await join(h, "turn-2");

    // Act
    await h.engine.onSdkMessage(consumed(resultMessage("turn-1-result"), running, [running]));

    // Assert
    const next = await startDuring(h, "turn-3");
    expect(next.result.case === "failure" ? next.result.value.kind.case : "accepted").toBe("turnAlreadyOpen");
  });

  it("charges the vendor's next turn to the unfolded prompt and closes it on its own result", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");
    const running = h.minted.at(-1) ?? "";
    const joined = await join(h, "turn-2");
    await h.engine.onSdkMessage(consumed(resultMessage("turn-1-result"), running, [running]));
    await h.engine.onSdkMessage(consumed(assistantMessage("turn-2-reply"), joined, [joined]));
    const charged = h.fold.contexts.at(-1)?.turnId?.value;

    // Act
    await h.engine.onSdkMessage(consumed(resultMessage("turn-2-result"), joined, [joined]));

    // Assert
    expect([charged, (await startDuring(h, "turn-3")).result.case]).toEqual(["turn-2", "success"]);
  });

  it("opens the waiting prompt as its own turn when the running turn is killed", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");
    await join(h, "turn-2");

    // Act
    await h.engine.killTurn(
      create(shimv1.KillTurnRequestSchema, { turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }) }),
    );

    // Assert
    expect(promptRows(h).at(-1)).toEqual(["turn-2", undefined]);
  });

  it("ends the waiting prompt's turn too when the query dies, after the running turn's terminal", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");
    await join(h, "turn-2");

    // Act
    h.queries[0]?.query.end();
    await new Promise((resolve) => setImmediate(resolve));

    // Assert
    expect(rowsAfterStart(h).slice(-3)).toEqual(["terminal:turn-1", "prompt:turn-2", "terminal:turn-2"]);
  });

  it("ends the waiting prompt's turn too when the session is torn down, after the running turn's terminal", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");
    await join(h, "turn-2");

    // Act
    await h.engine.standDown("SIGTERM");

    // Assert
    expect(rowsAfterStart(h).slice(-3)).toEqual(["terminal:turn-1", "prompt:turn-2", "terminal:turn-2"]);
  });

  it("interrupts nothing to send a prompt to join the running turn", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");

    // Act
    await join(h, "turn-2");

    // Assert
    expect(h.queries[0]?.query.calls ?? []).not.toContain("interrupt");
  });
});

/**
 * A REPLY IS MATCHED TO THE SEND THAT CAUSED IT BY ID (owner ruling 2026-09-28).
 *
 * Every send carries a client uuid the vendor echoes on the reply, and the
 * shim attributes each vendor turn by that echo alone. The failure mode
 * excluded is the race the adoption left open: a vendor-started turn landing
 * the instant before a StartTurn's own answer was charged to the StartTurn.
 */
describe("replies matched to their send by id", () => {
  /** The turn id the fold was handed for each message, in order, from `mark`. */
  const turnsFrom = (h: Harness, mark: number): (string | undefined)[] =>
    h.fold.contexts.slice(mark).map((context) => context.turnId?.value);

  /** Every adoption row the engine wrote, in order. */
  const adoptions = (h: Harness): string[] =>
    h.persistence.buffered.flatMap((entry) =>
      entry.item.kind === "prompt" && entry.item.prompt.origin === conversationv1.PromptOrigin.VENDOR_STARTED
        ? [entry.item.prompt.id?.value ?? ""]
        : [],
    );

  /** A frame stamped as answering `uuid`. */
  const stampedWith = (message: SdkMessage, uuid: string): SdkMessage =>
    ({ ...message, user_message_uuid: uuid, user_message_uuids: [uuid] }) as SdkMessage;

  it("attributes each turn's output to its own turn when a StartTurn races a vendor-started turn", async () => {
    // Arrange: the StartTurn's send is in the vendor's queue, and the vendor
    // runs a turn of its own first.
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");
    const send = h.minted.at(-1) ?? "";
    const mark = h.fold.contexts.length;

    // Act
    await h.engine.onSdkMessage(assistantMessage("vendor-reply"));
    await h.engine.onSdkMessage(resultMessage("vendor-result"));
    await h.engine.onSdkMessage(stampedWith(assistantMessage("turn-1-reply"), send));
    await h.engine.onSdkMessage(stampedWith(resultMessage("turn-1-result"), send));

    // Assert
    const vendorTurn = adoptions(h)[0];
    expect(turnsFrom(h, mark)).toEqual([vendorTurn, vendorTurn, "turn-1", "turn-1"]);
  });

  it("keeps the StartTurn's turn open across the vendor-started turn's result", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");
    await h.engine.onSdkMessage(assistantMessage("vendor-reply"));

    // Act
    await h.engine.onSdkMessage(resultMessage("vendor-result"));

    // Assert
    const second = await startDuring(h, "turn-2");
    expect(second.result.case === "failure" ? second.result.value.kind.case : "accepted").toBe("turnAlreadyOpen");
  });

  it("closes the StartTurn's turn on its own stamped result", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");
    const send = h.minted.at(-1) ?? "";
    await h.engine.onSdkMessage(assistantMessage("vendor-reply"));
    await h.engine.onSdkMessage(resultMessage("vendor-result"));

    // Act
    await h.engine.onSdkMessage(stampedWith(resultMessage("turn-1-result"), send));

    // Assert
    expect((await startDuring(h, "turn-2")).result.case).toBe("success");
  });

  it("matches two quick sends each by its own echo", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");
    const first = h.minted.at(-1) ?? "";
    const mark = h.fold.contexts.length;
    await h.engine.onSdkMessage(stampedWith(assistantMessage("turn-1-reply"), first));
    await h.engine.onSdkMessage(stampedWith(resultMessage("turn-1-result"), first));
    await realPrompt(h, "turn-2");
    const second = h.minted.at(-1) ?? "";

    // Act
    await h.engine.onSdkMessage(stampedWith(assistantMessage("turn-2-reply"), second));
    await h.engine.onSdkMessage(stampedWith(resultMessage("turn-2-result"), second));

    // Assert
    expect([first !== second, turnsFrom(h, mark)]).toEqual([true, ["turn-1", "turn-1", "turn-2", "turn-2"]]);
  });

  it("concludes a vendor-started turn that folds the StartTurn's send in, before the send's own rows", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");
    const send = h.minted.at(-1) ?? "";
    await h.engine.onSdkMessage(assistantMessage("vendor-reply"));

    // Act
    await h.engine.onSdkMessage(stampedWith(assistantMessage("folded-reply"), send));

    // Assert
    expect([h.fold.absorbed.map((entry) => entry.turn), h.fold.contexts.at(-1)?.turnId?.value]).toEqual([
      [adoptions(h)[0]],
      "turn-1",
    ]);
  });

  it("frees the adopted turn a fold absorbed, so the next vendor-started turn is adopted afresh", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");
    const send = h.minted.at(-1) ?? "";
    await h.engine.onSdkMessage(assistantMessage("vendor-reply"));
    await h.engine.onSdkMessage(stampedWith(assistantMessage("folded-reply"), send));
    await h.engine.onSdkMessage(stampedWith(resultMessage("turn-1-result"), send));

    // Act
    await h.engine.onSdkMessage(assistantMessage("next-vendor-reply"));

    // Assert
    expect(adoptions(h)).toHaveLength(2);
  });

  it("attributes a reply echoing an unknown uuid to no turn, never guessing the open one", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");

    // Act
    await h.engine.onSdkMessage(stampedWith(assistantMessage("stray-reply"), "00000000-0000-4000-8000-00000000dead"));

    // Assert
    expect([h.fold.contexts.at(-1)?.turnId, adoptions(h)]).toEqual([undefined, []]);
  });

  it("records a reply echoing an unknown uuid at ERROR", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");
    const mark = logSinkMark();

    // Act
    await h.engine.onSdkMessage(stampedWith(assistantMessage("stray-reply"), "00000000-0000-4000-8000-00000000dead"));

    // Assert
    expect(logRecordsSince(mark).map((record) => [record.level, record.message])).toContainEqual([
      "error",
      "a vendor frame echoed a client uuid the shim never sent; it is attributed to no send",
    ]);
  });

  it("charges a killed turn's stamped stop result to the killed turn", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");
    const send = h.minted.at(-1) ?? "";
    await h.engine.killTurn(
      create(shimv1.KillTurnRequestSchema, { turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }) }),
    );

    // Act
    await h.engine.onSdkMessage(stampedWith(resultMessage("stopped-result"), send));

    // Assert
    expect([h.fold.contexts.at(-1)?.turnId?.value, adoptions(h)]).toEqual(["turn-1", []]);
  });

  it("serves the adopted turn as the turn in flight while a StartTurn waits behind it in the vendor", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");
    await h.engine.onSdkMessage(assistantMessage("vendor-reply"));

    // Act
    const response = await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    // Assert
    const failure = response.result.case === "failure" ? response.result.value : undefined;
    const inFlight = failure?.cause.case === "live" ? failure.cause.value.turnInFlight?.value : undefined;
    expect(inFlight).toBe(adoptions(h)[0]);
  });
});

describe("the keep-alive turn", () => {
  it("submits a marker-prefixed prompt when the session is idle", async () => {
    const h = harness();
    await started(h);

    h.scheduler.fire(0);
    await new Promise((resolve) => setImmediate(resolve));

    const prompt = h.persistence.buffered.find((entry) => entry.item.kind === "prompt");
    expect(prompt?.keepalive).toBe(true);
  });

  it("does NOT beat while a turn is in flight", async () => {
    const h = harness();
    await started(h);
    await h.engine.startTurn(
      create(shimv1.StartTurnRequestSchema, {
        turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }),
        said: textSaid("go"),
        origin: conversationv1.PromptOrigin.USER_SENT,
        pageSize: 5,
      }),
    );
    h.persistence.buffered.length = 0;

    h.scheduler.fire(0);
    await new Promise((resolve) => setImmediate(resolve));

    expect(h.persistence.buffered.filter((entry) => entry.item.kind === "prompt")).toEqual([]);
  });

  it("REWINDS the vendor context before the next real prompt", async () => {
    const h = harness();
    await started(h);
    // A real turn that leaves an ASSISTANT record, then a keep-alive turn.
    await realTurn(h, "turn-0", [assistantMessage("real-assistant-uuid")]);
    await keepaliveTurn(h, []);

    await realPrompt(h, "turn-1");

    expect(h.queries.at(-1)?.spec.resumeSessionAt).toBe("real-assistant-uuid");
  });
});

/**
 * THE ROLLBACK RUNS BETWEEN BEATS.
 *
 * The bug of 2026-09-17: a keep-alive degenerated into a ~64,000-token block,
 * and because keep-alives were rewound only on the next REAL prompt, an idle
 * night with no real prompt let block after block pile up until the context
 * blew the client-side length guard. The fix rewinds the outstanding keep-alive
 * before sending the next, so the transcript never holds more than one.
 */
/**
 * THE KEEP-ALIVE TURN SERVES NOTHING (the leak of 2026-09-23).
 *
 * WHAT THIS GUARDS: that every row and push a keep-alive turn gives rise to —
 * its reply, its thinking, its usage, its terminal, its turn end — stays off
 * every served plane, and that the tag comes from the send the vendor says a
 * message answers. The failure mode being excluded is the owner's live one: a
 * turn the vendor ran on its own closed the keep-alive, and the keep-alive's
 * "." then arrived untagged and was drawn as a green final answer.
 */
describe("the keep-alive turn serves nothing", () => {
  /** Beat the cadence once and let the send land. */
  async function beat(h: Harness): Promise<void> {
    h.scheduler.fire(0);
    await new Promise((resolve) => setImmediate(resolve));
  }

  /**
   * A fold that behaves like the real converter in the one respect under test:
   * every row it produces carries the context's tag, and an assistant message
   * also produces a usage fact.
   */
  function taggingFold(h: Harness): void {
    h.fold.entriesFor = (message) => {
      const keepalive = h.fold.contexts.at(-1)?.keepalive ?? false;
      const rows: PersistEntry[] = [
        { ...activityEntry(`unit-${message.uuid}`, { case: undefined }, `frame-${message.uuid}`), keepalive },
      ];
      if (message.type === "assistant") {
        rows.push({
          ...foldEntry(
            {
              kind: "session_update",
              update: create(conversationv1.SessionUpdateSchema, {
                update: { case: "rateLimitStatus", value: create(conversationv1.SessionRateLimitStatusSchema, {}) },
              }),
            },
            `usage-${message.uuid}`,
          ),
          keepalive,
        });
      }
      return rows;
    };
  }

  /** The persisted rows the fold produced for one vendor message. */
  const rowsFor = (h: Harness, uuid: string): PersistEntry[] =>
    h.persistence.buffered.filter((entry) => entry.source.vendorUuid.endsWith(uuid));

  /** A vendor message answering nobody: the vendor's own task-notification turn. */
  const vendorReply = (uuid: string): SdkMessage => assistantMessage(uuid);

  /** A thinking-progress frame of the running turn. */
  const thinking = (uuid: string): SdkMessage =>
    ({
      type: "system",
      subtype: "thinking_tokens",
      estimated_tokens: 10,
      estimated_tokens_delta: 10,
      uuid,
      session_id: "s",
    }) as never;


  it("sends the keep-alive with a client uuid of its own", async () => {
    // Arrange
    const sends: SdkUserMessage[] = [];
    const h = harness({ drainSends: sends });
    await started(h);

    // Act
    await beat(h);
    await new Promise((resolve) => setImmediate(resolve));

    // Assert
    expect(sends.at(-1)?.uuid).toBe(h.minted.at(-1));
  });

  it("sends a real prompt under a client uuid of its own (ruled 2026-09-28)", async () => {
    // Arrange
    const sends: SdkUserMessage[] = [];
    const h = harness({ drainSends: sends });
    await started(h);

    // Act
    await realPrompt(h, "turn-1");
    await new Promise((resolve) => setImmediate(resolve));

    // Assert
    expect(sends.at(-1)?.uuid).toBe(h.minted.at(-1));
  });

  it("keeps the keep-alive's reply off every page", async () => {
    // Arrange
    const h = harness();
    await started(h);
    taggingFold(h);
    await beat(h);

    // Act
    await h.engine.onSdkMessage(answering(h, assistantMessage("ka-reply")));

    // Assert
    expect(rowsFor(h, "ka-reply").map((row) => row.keepalive)).toEqual([true, true]);
  });

  it("keeps the keep-alive's terminal off every page", async () => {
    // Arrange
    const h = harness();
    await started(h);
    taggingFold(h);
    await beat(h);
    await h.engine.onSdkMessage(answering(h, assistantMessage("ka-reply")));

    // Act
    await h.engine.onSdkMessage(answering(h, resultMessage("ka-result")));

    // Assert
    expect(rowsFor(h, "ka-result").map((row) => row.keepalive)).toEqual([true]);
  });

  it("keeps the keep-alive's thinking off every page", async () => {
    // Arrange
    const h = harness();
    await started(h);
    taggingFold(h);
    await beat(h);

    // Act
    await h.engine.onSdkMessage(answering(h, thinking("ka-thinking")));

    // Assert
    expect(rowsFor(h, "ka-thinking").map((row) => row.keepalive)).toEqual([true]);
  });

  it("pushes no usage fact the keep-alive's reply produced", async () => {
    // Arrange
    const h = harness();
    await started(h);
    taggingFold(h);
    await beat(h);

    // Act
    const arms = await pushedUpdates(
      h,
      (update) => (update.case === "rateLimitStatus" ? update.case : undefined),
      async () => {
        await h.engine.onSdkMessage(answering(h, assistantMessage("ka-reply")));
      },
    );

    // Assert
    expect(arms).toEqual([]);
  });

  it("pushes no model the keep-alive was answered on", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await beat(h);
    const reply = {
      ...assistantMessage("ka-reply"),
      message: { id: "msg_ka", role: "assistant", content: [], model: "some-fallback-model" },
    } as never;

    // Act
    const models = await pushedUpdates(
      h,
      // The session's own model is restated to a new subscriber; only the
      // keep-alive's would be news.
      (update) =>
        update.case === "modelChanged" && update.value.effectiveModel?.name === "some-fallback-model"
          ? update.value.effectiveModel.name
          : undefined,
      async () => {
        await h.engine.onSdkMessage(answering(h, reply));
      },
    );

    // Assert
    expect(models).toEqual([]);
  });

  it("pushes no fast-mode state the keep-alive's result restates", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await beat(h);
    const result = { ...resultMessage("ka-result"), fast_mode_state: "on" } as never;

    // Act
    const states = await pushedUpdates(
      h,
      (update) => (update.case === "fastMode" ? update.case : undefined),
      async () => {
        await h.engine.onSdkMessage(answering(h, result));
      },
    );

    // Assert
    expect(states).toEqual([]);
  });

  it("samples no context usage at the keep-alive's end", async () => {
    // Arrange
    const h = harness();
    await started(h);
    const probes = (): number => h.queries[0]?.query.calls.filter((call) => call === "getContextUsage").length ?? 0;
    await beat(h);
    const before = probes();

    // Act
    await h.engine.onSdkMessage(answering(h, resultMessage("ka-result")));

    // Assert
    expect(probes()).toBe(before);
  });

  it("re-probes no account usage at the keep-alive's end", async () => {
    // Arrange
    const h = harness();
    await started(h);
    const probes = (): number => h.queries[0]?.query.calls.filter((call) => call === "usage").length ?? 0;
    await beat(h);
    const before = probes();

    // Act
    await h.engine.onSdkMessage(answering(h, resultMessage("ka-result")));

    // Assert
    expect(probes()).toBe(before);
  });

  it("serves the rows of a turn the vendor ran while the keep-alive waited", async () => {
    // Arrange
    const h = harness();
    await started(h);
    taggingFold(h);
    await beat(h);

    // Act
    await h.engine.onSdkMessage(vendorReply("vendor-reply"));

    // Assert
    expect(rowsFor(h, "vendor-reply").map((row) => row.keepalive)).toEqual([false, false]);
  });

  it("names the adopted turn, never the keep-alive's, to the fold for a turn the vendor ran while the keep-alive waited", async () => {
    // Arrange: the keep-alive's turn id must reach no served row; the vendor's
    // own turn is adopted beside it (owner ruling 2026-09-27: every
    // vendor-started turn is a real turn with an id).
    const h = harness();
    await started(h);
    await beat(h);

    // Act
    await h.engine.onSdkMessage(vendorReply("vendor-reply"));

    // Assert
    expect(h.fold.contexts.at(-1)?.turnId?.value).toMatch(/^adopted-/);
  });

  it("keeps the keep-alive open across the result of a turn the vendor ran first", async () => {
    // Arrange: THE 2026-09-23 SEQUENCE.
    const h = harness();
    await started(h);
    await beat(h);
    await h.engine.onSdkMessage(vendorReply("vendor-reply"));

    // Act
    await h.engine.onSdkMessage(resultMessage("vendor-result"));

    // Assert: a real prompt still waits behind the keep-alive.
    expect(await settledSoon(startDuring(h, "turn-1"))).toBe(false);
  });

  it("keeps the keep-alive's answer off every page when it arrives after a vendor turn", async () => {
    // Arrange
    const h = harness();
    await started(h);
    taggingFold(h);
    await beat(h);
    await h.engine.onSdkMessage(vendorReply("vendor-reply"));
    await h.engine.onSdkMessage(resultMessage("vendor-result"));

    // Act
    await h.engine.onSdkMessage(answering(h, assistantMessage("ka-dot")));

    // Assert
    expect(rowsFor(h, "ka-dot").map((row) => row.keepalive)).toEqual([true, true]);
  });

  it("closes the keep-alive on its own result", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await beat(h);

    // Act
    await h.engine.onSdkMessage(answering(h, resultMessage("ka-result")));

    // Assert: the next real prompt is accepted at once.
    expect((await startDuring(h, "turn-1")).result.case).toBe("success");
  });

  it("interrupts a keep-alive in flight when a real prompt arrives, and opens the prompt's turn on the keep-alive's result", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await beat(h);
    const starting = startDuring(h, "turn-1");
    await settledSoon(starting);
    const interrupted = h.queries.at(-1)?.query.calls.includes("interrupt");

    // Act
    await h.engine.onSdkMessage(answering(h, resultMessage("ka-result")));
    const response = await starting;

    // Assert
    expect([interrupted, response.result.case]).toEqual([true, "success"]);
  });

  it("leaves a real turn right after a keep-alive tagged as served", async () => {
    // Arrange
    const h = harness();
    await started(h);
    taggingFold(h);
    await keepaliveTurn(h, []);
    await realPrompt(h, "turn-1");

    // Act
    await h.engine.onSdkMessage(answering(h, assistantMessage("real-reply")));

    // Assert
    expect([h.fold.contexts.at(-1)?.turnId?.value, ...rowsFor(h, "real-reply").map((row) => row.keepalive)]).toEqual([
      "turn-1",
      false,
      false,
    ]);
  });

  it("still samples context usage at a real turn's end after a keep-alive", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await keepaliveTurn(h, []);
    await realPrompt(h, "turn-1");
    const probes = (): number =>
      h.queries.reduce((sum, entry) => sum + entry.query.calls.filter((call) => call === "getContextUsage").length, 0);
    const before = probes();

    // Act
    await h.engine.onSdkMessage(answering(h, resultMessage("real-result")));

    // Assert
    expect(probes()).toBeGreaterThan(before);
  });

  it("names no keep-alive as the turn in flight to a new watch", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await beat(h);
    const iterator = h.engine.watchSession(create(shimv1.WatchSessionRequestSchema, {}))[Symbol.asyncIterator]();
    await iterator.next();

    // Act
    const second = await nextPush(iterator);
    await iterator.return?.();

    // Assert
    expect(second.frame.case === "sessionStarted" ? second.frame.value.turnInFlight : "not-started").toBeUndefined();
  });

  it("does not refuse KillSession over a keep-alive", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await beat(h);

    // Act
    const response = await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    // Assert
    expect(response.result.case).not.toBe("failure");
  });

  it("does not refuse Hibernate over a keep-alive", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await beat(h);

    // Act
    const response = await h.engine.hibernate(create(shimv1.HibernateRequestSchema, {}));

    // Assert
    expect(response.result.case).not.toBe("failure");
  });

  it("re-delivers a keep-alive under the SAME client uuid when its rewind is refused", async () => {
    // Arrange: an anchor, one keep-alive of debt, and the next beat's rewind.
    const sends: SdkUserMessage[] = [];
    const h = harness({ drainSends: sends });
    await started(h);
    await realTurn(h, "turn-0", [assistantMessage("assistant-uuid")]);
    await keepaliveTurn(h, []);
    await beat(h);
    await new Promise((resolve) => setImmediate(resolve));

    // Act
    await h.engine.onSdkMessage(
      errorResultMessage({ errors: ["No message found with message.uuid of: assistant-uuid"] }),
    );
    for (let attempt = 0; attempt < 50 && sends.filter((send) => send.uuid === h.minted.at(-1)).length < 2; attempt++) {
      await new Promise((resolve) => setImmediate(resolve));
    }

    // Assert
    expect(sends.filter((send) => send.uuid === h.minted.at(-1)).length).toBe(2);
  });

  it("closes the keep-alive's scope, loudly, when it cannot be submitted", async () => {
    // Arrange: both the rewind and its plain-resume recovery fail to open.
    const h = harness({ createQueryFailsFrom: 1 });
    await started(h);
    await realTurn(h, "turn-0", [assistantMessage("assistant-uuid")]);
    await keepaliveTurn(h, []);
    const before = logSinkMark();

    // Act
    await beat(h);
    for (let attempt = 0; attempt < 50; attempt++) await new Promise((resolve) => setImmediate(resolve));

    // Assert
    expect(logLevelFor(before, "a keep-alive's turn scope closed WITHOUT its answer")).toBe("info");
  });

  it("closes the keep-alive's scope when the vendor query dies under it", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await beat(h);
    const before = logSinkMark();

    // Act
    h.queries[0]?.query.end();
    for (let attempt = 0; attempt < 50; attempt++) await new Promise((resolve) => setImmediate(resolve));

    // Assert
    expect(logContextFor(before, "a keep-alive's turn scope closed WITHOUT its answer")?.reason).toMatch(
      /^the vendor query died/,
    );
  });
});

/**
 * A REAL PROMPT THAT ARRIVES DURING A KEEP-ALIVE (2026-09-23).
 *
 * WHAT THIS GUARDS: the keep-alive is invisible outside the shim, so a
 * StartTurn that lands while one runs — before its send, mid-stream, or as
 * its result arrives — is never refused: it waits inside the shim and opens its
 * turn once the keep-alive leaves the slot, delivering its prompt exactly once
 * and after the keep-alive's send.
 */
describe("a real prompt that arrives during a keep-alive", () => {
  it("before the keep-alive is sent: the start is accepted once the keep-alive answers", async () => {
    // Arrange: the beat has claimed the slot but not yet pushed its send.
    const h = harness({ drainSends: [] });
    await started(h);
    h.scheduler.fire(0);
    const starting = startDuring(h, "turn-1");
    await drainTurns();

    // Act
    await h.engine.onSdkMessage(answering(h, resultMessage("ka-result")));

    // Assert
    expect((await starting).result.case).toBe("success");
  });

  it("before the keep-alive is sent: the keep-alive goes first and the prompt exactly once after", async () => {
    // Arrange
    const sends: SdkUserMessage[] = [];
    const h = harness({ drainSends: sends });
    await started(h);
    h.scheduler.fire(0);
    const starting = startDuring(h, "turn-1");
    await drainTurns();

    // Act
    await h.engine.onSdkMessage(answering(h, resultMessage("ka-result")));
    await starting;
    await drainTurns();

    // Assert
    expect(sendKinds(sends)).toEqual(["keepalive", "real"]);
  });

  it("mid-stream: the start waits while the keep-alive is still answering", async () => {
    // Arrange
    const h = harness();
    await started(h);
    h.scheduler.fire(0);
    await drainTurns();
    await h.engine.onSdkMessage(answering(h, assistantMessage("ka-reply")));

    // Act
    const starting = startDuring(h, "turn-1");

    // Assert
    expect(await settledSoon(starting)).toBe(false);
  });

  it("mid-stream: the keep-alive's result releases the start, which is accepted", async () => {
    // Arrange
    const h = harness();
    await started(h);
    h.scheduler.fire(0);
    await drainTurns();
    await h.engine.onSdkMessage(answering(h, assistantMessage("ka-reply")));
    const starting = startDuring(h, "turn-1");

    // Act
    await h.engine.onSdkMessage(answering(h, resultMessage("ka-result")));

    // Assert
    expect((await starting).result.case).toBe("success");
  });

  it("mid-stream: the prompt is sent exactly once, after the keep-alive", async () => {
    // Arrange
    const sends: SdkUserMessage[] = [];
    const h = harness({ drainSends: sends });
    await started(h);
    h.scheduler.fire(0);
    await drainTurns();
    await h.engine.onSdkMessage(answering(h, assistantMessage("ka-reply")));
    const starting = startDuring(h, "turn-1");

    // Act
    await h.engine.onSdkMessage(answering(h, resultMessage("ka-result")));
    await starting;
    await drainTurns();

    // Assert
    expect(sendKinds(sends)).toEqual(["keepalive", "real"]);
  });

  it("at the keep-alive's result: a start arriving with it is accepted", async () => {
    // Arrange
    const h = harness();
    await started(h);
    h.scheduler.fire(0);
    await drainTurns();

    // Act: the result and the start race.
    const closing = h.engine.onSdkMessage(answering(h, resultMessage("ka-result")));
    const starting = startDuring(h, "turn-1");
    await closing;

    // Assert
    expect((await starting).result.case).toBe("success");
  });

  it("at the keep-alive's result: the prompt is sent exactly once, after the keep-alive", async () => {
    // Arrange
    const sends: SdkUserMessage[] = [];
    const h = harness({ drainSends: sends });
    await started(h);
    h.scheduler.fire(0);
    await drainTurns();

    // Act
    const closing = h.engine.onSdkMessage(answering(h, resultMessage("ka-result")));
    const starting = startDuring(h, "turn-1");
    await Promise.all([closing, starting]);
    await drainTurns();

    // Assert
    expect(sendKinds(sends)).toEqual(["keepalive", "real"]);
  });

  it("at the keep-alive's result: the prompt still rides the rewind to the last real record", async () => {
    // Arrange: a real turn leaves the anchor; the keep-alive after it is the debt.
    const h = harness();
    await started(h);
    await realTurn(h, "turn-0", [assistantMessage("real-assistant-uuid")]);
    h.scheduler.fire(0);
    await drainTurns();

    // Act
    const closing = h.engine.onSdkMessage(answering(h, resultMessage("ka-result")));
    const starting = startDuring(h, "turn-1");
    await Promise.all([closing, starting]);

    // Assert
    expect(h.queries.at(-1)?.spec.resumeSessionAt).toBe("real-assistant-uuid");
  });

  it("mid-stream: the prompt rides the rewind to the last real record", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await realTurn(h, "turn-0", [assistantMessage("real-assistant-uuid")]);
    h.scheduler.fire(0);
    await drainTurns();
    const starting = startDuring(h, "turn-1");

    // Act
    await h.engine.onSdkMessage(answering(h, resultMessage("ka-result")));
    await starting;

    // Assert
    expect(h.queries.at(-1)?.spec.resumeSessionAt).toBe("real-assistant-uuid");
  });

  it("names the real turn, never the keep-alive's, as the turn in flight once it opens", async () => {
    // Arrange
    const h = harness();
    await started(h);
    h.scheduler.fire(0);
    await drainTurns();
    const starting = startDuring(h, "turn-1");
    await h.engine.onSdkMessage(answering(h, resultMessage("ka-result")));
    await starting;
    const iterator = h.engine.watchSession(create(shimv1.WatchSessionRequestSchema, {}))[Symbol.asyncIterator]();
    await iterator.next();

    // Act
    const second = await nextPush(iterator);
    await iterator.return?.();

    // Assert
    expect(second.frame.case === "sessionStarted" ? second.frame.value.turnInFlight?.value : "not-started").toBe(
      "turn-1",
    );
  });

  it("does not beat while a StartTurn is still being processed", async () => {
    // Arrange: the start is held inside its durable prompt write.
    const h = harness();
    await started(h);
    h.persistence.writeDurable = () => new Promise<void>(() => {});
    void startDuring(h, "turn-1");

    // Act
    h.scheduler.fire(0);
    await drainTurns();

    // Assert
    expect(h.minted).toEqual([]);
  });

  it("answers query_dead to a start waiting when the query dies under the keep-alive", async () => {
    // Arrange
    const h = harness();
    await started(h);
    h.scheduler.fire(0);
    await drainTurns();
    const starting = startDuring(h, "turn-1");

    // Act
    h.queries[0]?.query.end();

    // Assert
    const response = await starting;
    expect(response.result.case === "failure" ? response.result.value.kind.case : "accepted").toBe("queryDead");
  });

  it("delivers nothing for a start the caller abandons while it waits", async () => {
    // Arrange
    const sends: SdkUserMessage[] = [];
    const h = harness({ drainSends: sends });
    await started(h);
    h.scheduler.fire(0);
    await drainTurns();
    const caller = new AbortController();
    const starting = startDuring(h, "turn-1", caller.signal);

    // Act
    caller.abort();
    await starting;
    await h.engine.onSdkMessage(answering(h, resultMessage("ka-result")));
    await drainTurns();

    // Assert
    expect(sendKinds(sends)).toEqual(["keepalive"]);
  });

  it("re-delivers a refused-rewind keep-alive once and the waiting prompt once", async () => {
    // Arrange: an anchor, one keep-alive of debt, and the next beat's rewind,
    // with a real prompt already waiting behind that beat.
    const sends: SdkUserMessage[] = [];
    const h = harness({ drainSends: sends });
    await started(h);
    await realTurn(h, "turn-0", [assistantMessage("assistant-uuid")]);
    await keepaliveTurn(h, []);
    h.scheduler.fire(0);
    await drainTurns();
    const starting = startDuring(h, "turn-1");

    // Act: the vendor refuses the anchor, then answers the re-delivered keep-alive.
    await h.engine.onSdkMessage(
      errorResultMessage({ errors: ["No message found with message.uuid of: assistant-uuid"] }),
    );
    await drainTurns();
    await h.engine.onSdkMessage(answering(h, resultMessage("ka-result")));
    await starting;
    await drainTurns();

    // Assert: the first keep-alive, the refused beat and its re-delivery, then the prompt.
    expect(sendKinds(sends.slice(1))).toEqual(["keepalive", "keepalive", "keepalive", "real"]);
  });

  it("ends the keep-alive's rewind watch when the keep-alive leaves the slot", async () => {
    // Arrange: the beat rewound, and the keep-alive then ends on an error that
    // is not about the anchor.
    const h = harness();
    await started(h);
    await realTurn(h, "turn-0", [assistantMessage("assistant-uuid")]);
    await keepaliveTurn(h, []);
    h.scheduler.fire(0);
    await drainTurns();
    const before = logSinkMark();

    // Act
    await h.engine.onSdkMessage(answering(h, errorResultMessage({ errors: ["overloaded"] })));

    // Assert
    expect(logLevelFor(before, "the keep-alive left the turn slot; its rewind watch ends with it")).toBe("debug");
  });

  it("serves a resumed session's prompt that arrived during a keep-alive", async () => {
    // Arrange: a warm resume, then a keep-alive with a prompt waiting behind it.
    const h = harness({ nowMs: 1_000_100 });
    writeTranscript(h.configDir, h.cwd, "resume-1", [assistantLine()]);
    const pending = h.engine.startSession(resumeRequest("resume-1"));
    (await untilQuery(h, 0)).query.emit(initMessage({ sessionId: "resume-1" }));
    await pending;
    h.scheduler.fire(0);
    await drainTurns();
    const starting = startDuring(h, "turn-1");

    // Act
    await h.engine.onSdkMessage(answering(h, resultMessage("ka-result")));

    // Assert
    expect((await starting).result.case).toBe("success");
  });
});

describe("the keep-alive rewind between beats", () => {
  it("rewinds the prior keep-alive before the next, to the last REAL record", async () => {
    // Arrange. A real turn leaves the anchor, then one keep-alive rides it.
    const h = harness();
    await started(h);
    await realTurn(h, "turn-0", [assistantMessage("real-uuid")]);
    await keepaliveTurn(h, []);
    const afterFirst = h.queries.length;

    // Act. The SECOND beat is the first that owes a rollback.
    await keepaliveTurn(h, []);

    // Assert. Exactly one new query, resuming at the real record — the first
    // keep-alive is rewound out, so at most one is ever in the transcript.
    expect(h.queries.length).toBe(afterFirst + 1);
    expect(h.queries.at(-1)?.spec.resumeSessionAt).toBe("real-uuid");
  });

  it("does NOT rewind on the first keep-alive after a real turn", async () => {
    // The first beat owes nothing: no keep-alive has run since the anchor.
    const h = harness();
    await started(h);
    await realTurn(h, "turn-0", [assistantMessage("real-uuid")]);
    const before = h.queries.length;

    await keepaliveTurn(h, []);

    expect(h.queries.length).toBe(before);
  });

  it("rewinds even a degenerate huge keep-alive response before the next cycle", async () => {
    // Arrange. The keep-alive leaves its OWN assistant record, as the runaway
    // 64k-token block did; the anchor must not move onto it.
    const h = harness();
    await started(h);
    await realTurn(h, "turn-0", [assistantMessage("real-uuid")]);
    await keepaliveTurn(h, [assistantMessage("keepalive-degenerate-uuid")]);

    // Act.
    await keepaliveTurn(h, []);

    // Assert. The next beat rewinds to the real record, discarding the huge one.
    expect(h.queries.at(-1)?.spec.resumeSessionAt).toBe("real-uuid");
  });

  it("still rewinds the one outstanding keep-alive when a real prompt finally arrives", async () => {
    // Many idle cycles, then a real prompt: the existing real-prompt rewind
    // still removes the single keep-alive that is outstanding.
    const h = harness();
    await started(h);
    await realTurn(h, "turn-0", [assistantMessage("real-uuid")]);
    await keepaliveTurn(h, []);
    await keepaliveTurn(h, []);
    await keepaliveTurn(h, []);

    await realPrompt(h, "turn-1");

    expect(h.queries.at(-1)?.spec.resumeSessionAt).toBe("real-uuid");
  });

  it("numbers successive keep-alive prompts so no two beats are identical", async () => {
    const h = harness();
    await started(h);
    await realTurn(h, "turn-0", [assistantMessage("real-uuid")]);

    await keepaliveTurn(h, []);
    await keepaliveTurn(h, []);

    const texts: string[] = [];
    for (const entry of h.persistence.buffered) {
      if (entry.item.kind !== "prompt" || !entry.keepalive) continue;
      const said = entry.item.prompt.said;
      if (said !== undefined) texts.push(saidText(said));
    }
    expect(texts.length).toBeGreaterThanOrEqual(2);
    expect(new Set(texts).size).toBe(texts.length);
  });
});

/**
 * THE MANUAL RESET (SIGUSR2 backdoor; ruled 2026-09-17).
 *
 * `resetKeepalives` is the same collapse the per-cycle rewind performs, run on
 * demand rather than before a beat or a real prompt. It shares the exact
 * rollback: a replacement query bound with `resumeSessionAt` at the last real
 * record, never a hand-edit of the vendor's transcript file.
 */
describe("resetKeepalives (the manual keep-alive reset)", () => {
  it("collapses an outstanding keep-alive turn back to the last real record", async () => {
    // Arrange. The one keep-alive beat after a real turn owes nothing YET (see
    // "does NOT rewind on the first keep-alive after a real turn" above) but
    // leaves the debt outstanding once it ends — exactly the state the manual
    // reset exists to collapse on demand, instead of waiting for the next beat
    // or real prompt to do it.
    const h = harness();
    await started(h);
    await realTurn(h, "turn-0", [assistantMessage("real-uuid")]);
    await keepaliveTurn(h, []);
    const beforeReset = h.queries.length;

    // Act.
    await h.engine.resetKeepalives();

    // Assert. A new query was opened, resuming at the last real record.
    expect(h.queries.length).toBe(beforeReset + 1);
    expect(h.queries.at(-1)?.spec.resumeSessionAt).toBe("real-uuid");
  });

  it("is a safe no-op when no keep-alive turns are outstanding", async () => {
    // Arrange. A real turn with no keep-alive since it: nothing is owed.
    const h = harness();
    await started(h);
    await realTurn(h, "turn-0", [assistantMessage("real-uuid")]);
    const before = h.queries.length;

    // Act.
    await h.engine.resetKeepalives();

    // Assert. No query was replaced.
    expect(h.queries.length).toBe(before);
  });

  it("is a safe no-op when no session has been bound yet", async () => {
    // Arrange: a fresh engine, never started.
    const h = harness();

    // Act, Assert.
    await expect(h.engine.resetKeepalives()).resolves.toBeUndefined();
    expect(h.queries.length).toBe(0);
  });

  it("goes through the SAME rollback the per-cycle rewind uses: an identical resumeSessionAt", async () => {
    // Arrange: two sessions brought to an identical outstanding-keep-alive
    // state, one settled by the ordinary per-cycle rewind (a second beat), the
    // other by the manual reset.
    const viaCadence = harness();
    await started(viaCadence);
    await realTurn(viaCadence, "turn-0", [assistantMessage("real-uuid")]);
    await keepaliveTurn(viaCadence, []);
    await keepaliveTurn(viaCadence, []);

    const viaReset = harness();
    await started(viaReset);
    await realTurn(viaReset, "turn-0", [assistantMessage("real-uuid")]);
    await keepaliveTurn(viaReset, []);
    await viaReset.engine.resetKeepalives();

    // Assert: both landed a replacement query resuming at the same anchor.
    expect(viaCadence.queries.at(-1)?.spec.resumeSessionAt).toBe("real-uuid");
    expect(viaReset.queries.at(-1)?.spec.resumeSessionAt).toBe("real-uuid");
  });
});

/**
 * THE ANCHOR, AND THE TWO DEAD UUIDS OF 2026-09-14.
 *
 * `resumeSessionAt` is declared to take an `SDKAssistantMessage.uuid`. The
 * vendor's `system:init` and `result` messages carry required uuids too, and a
 * query opened at one of those is refused — `No message found with message.uuid
 * of: 19e047a0-…` — which killed the owner's session and lost the prompt it was
 * carrying. Every case below is one shape that must never reach the vendor as a
 * rewind target, or one boundary the anchor must not cross.
 */
describe("the keep-alive rewind anchor", () => {
  it("anchors on the assistant record of a real turn", async () => {
    // Arrange.
    const h = harness();
    await started(h);

    // Act.
    await realTurn(h, "turn-0", [assistantMessage("assistant-uuid")]);
    await keepaliveTurn(h, []);
    await realPrompt(h, "turn-1");

    // Assert.
    expect(h.queries.at(-1)?.spec.resumeSessionAt).toBe("assistant-uuid");
  });

  it("never anchors on the `system:init` uuid", async () => {
    // Arrange.
    const h = harness();
    await started(h);

    // Act. An init arriving mid-turn, exactly as a resumed vendor emits one.
    await realTurn(h, "turn-0", [
      assistantMessage("assistant-uuid"),
      initMessage({ sessionId: "session-1" }),
    ]);
    await keepaliveTurn(h, []);
    await realPrompt(h, "turn-1");

    // Assert. `initMessage`'s uuid is 11111111-…; the assistant's is the anchor.
    expect(h.queries.at(-1)?.spec.resumeSessionAt).toBe("assistant-uuid");
  });

  it("never anchors on a `result` uuid", async () => {
    // Arrange.
    const h = harness();
    await started(h);

    // Act. The result is what ENDS the real turn, so it is the last uuid seen.
    await realTurn(h, "turn-0", [assistantMessage("assistant-uuid")], "result-uuid");
    await keepaliveTurn(h, []);
    await realPrompt(h, "turn-1");

    // Assert.
    expect(h.queries.at(-1)?.spec.resumeSessionAt).toBe("assistant-uuid");
  });

  it("never anchors on a keep-alive turn's OWN assistant record", async () => {
    // Arrange.
    const h = harness();
    await started(h);

    // Act.
    await realTurn(h, "turn-0", [assistantMessage("assistant-uuid")]);
    await keepaliveTurn(h, [assistantMessage("keepalive-assistant-uuid")]);
    await realPrompt(h, "turn-1");

    // Assert.
    expect(h.queries.at(-1)?.spec.resumeSessionAt).toBe("assistant-uuid");
  });

  it("proceeds WITHOUT a rewind when the vendor compacted since the anchor", async () => {
    // Arrange.
    const h = harness();
    await started(h);
    await realTurn(h, "turn-0", [assistantMessage("assistant-uuid")]);

    // Act.
    await h.engine.onSdkMessage(compactBoundaryMessage("boundary-uuid"));
    await keepaliveTurn(h, []);
    await realPrompt(h, "turn-1");

    // Assert. A uuid from before the cut may name no record the resume can find.
    expect(h.queries.at(-1)?.spec.resumeSessionAt).toBeUndefined();
  });

  it("proceeds WITHOUT a rewind when the vendor reset the conversation since the anchor", async () => {
    // Arrange.
    const h = harness();
    await started(h);
    await realTurn(h, "turn-0", [assistantMessage("assistant-uuid")]);

    // Act.
    await h.engine.onSdkMessage(conversationResetMessage("reset-uuid"));
    await keepaliveTurn(h, []);
    await realPrompt(h, "turn-1");

    // Assert.
    expect(h.queries.at(-1)?.spec.resumeSessionAt).toBeUndefined();
  });

  it("does not open a second query at all when the anchor was cleared", async () => {
    // Arrange.
    const h = harness();
    await started(h);
    await realTurn(h, "turn-0", [assistantMessage("assistant-uuid")]);
    await h.engine.onSdkMessage(compactBoundaryMessage("boundary-uuid"));
    const before = h.queries.length;

    // Act.
    await keepaliveTurn(h, []);
    await realPrompt(h, "turn-1");

    // Assert.
    expect(h.queries.length).toBe(before);
  });
});

/**
 * THE REWIND IS SURVIVABLE.
 *
 * When the vendor refuses the anchor, the prompt the rewind was performed FOR
 * must still be delivered. It reaches the shim two ways: as an error result
 * naming the uuid, and as the child simply ending its stream having said so on
 * stderr.
 */
describe("a keep-alive rewind the vendor refuses", () => {
  /** Bring a session to the moment a rewind has just been performed. */
  async function rewound(delivered: string[]): Promise<Harness> {
    const h = harness({ drainPrompts: delivered });
    await started(h);
    await realTurn(h, "turn-0", [assistantMessage("assistant-uuid")]);
    await keepaliveTurn(h, []);
    await realPrompt(h, "turn-1");
    return h;
  }

  /** The vendor's refusal, in its own words. */
  const refusal = (): SdkMessage =>
    errorResultMessage({ errors: ["No message found with message.uuid of: assistant-uuid"] });

  /** The faults standing on the session right now. */
  function standingFaults(h: Harness): conversationv1.SessionFault[] {
    const update = h.engine.pushes.diagnostics().update;
    if (update.case !== "diagnostics") throw new Error("the engine stated no diagnostics");
    const health = update.value.health;
    return health.case === "unhealthy" ? health.value.faults : [];
  }

  it("reopens the query WITHOUT a rewind", async () => {
    // Arrange.
    const delivered: string[] = [];
    const h = await rewound(delivered);

    // Act.
    await h.engine.onSdkMessage(refusal());

    // Assert.
    expect(h.queries.at(-1)?.spec.resumeSessionAt).toBeUndefined();
  });

  it("reopens it as a plain RESUME of the same conversation", async () => {
    // Arrange.
    const delivered: string[] = [];
    const h = await rewound(delivered);

    // Act.
    await h.engine.onSdkMessage(refusal());

    // Assert.
    expect(h.queries.at(-1)?.spec.binding.kind).toBe("resume");
  });

  it("RE-DELIVERS the prompt the rewind was carrying", async () => {
    // Arrange.
    const delivered: string[] = [];
    const h = await rewound(delivered);

    // Act.
    await h.engine.onSdkMessage(refusal());
    for (let attempt = 0; attempt < 50 && !delivered.slice(1).includes("go"); attempt++) {
      await new Promise((resolve) => setImmediate(resolve));
    }

    // Assert. The prompt reaches the REPLACEMENT query, not only the dead one.
    expect(delivered.filter((prompt) => prompt === "go").length).toBeGreaterThanOrEqual(2);
  });

  it("raises a session fault so the footer says what happened", async () => {
    // Arrange.
    const delivered: string[] = [];
    const h = await rewound(delivered);

    // Act.
    await h.engine.onSdkMessage(refusal());

    // Assert.
    expect(standingFaults(h).map((fault) => fault.kind.case)).toContain("keepaliveFailed");
  });

  it("names the refused anchor in the fault's detail", async () => {
    // Arrange.
    const delivered: string[] = [];
    const h = await rewound(delivered);

    // Act.
    await h.engine.onSdkMessage(refusal());

    // Assert.
    expect(standingFaults(h).some((fault) => fault.detail.includes("assistant-uuid"))).toBe(true);
  });

  it("does NOT report the session's query as dead", async () => {
    // Arrange.
    const delivered: string[] = [];
    const h = await rewound(delivered);

    // Act.
    await h.engine.onSdkMessage(refusal());

    // Assert.
    expect(standingFaults(h).map((fault) => fault.kind.case)).not.toContain("vendorQueryFailed");
  });

  it("DROPS the anchor, so the prompt after it rewinds nowhere", async () => {
    // Arrange.
    const delivered: string[] = [];
    const h = await rewound(delivered);
    await h.engine.onSdkMessage(refusal());

    // Act.
    await keepaliveTurn(h, []);
    await realPrompt(h, "turn-2");

    // Assert.
    expect(h.queries.at(-1)?.spec.resumeSessionAt).toBeUndefined();
  });

  it("falls back to a plain resume when the rewind's query never starts", async () => {
    // Arrange. Creation 1 is the rewind's; creation 0 opened the session.
    const delivered: string[] = [];
    const h = harness({ drainPrompts: delivered, createQueryFailsAt: 1 });
    await started(h);
    await realTurn(h, "turn-0", [assistantMessage("assistant-uuid")]);
    await keepaliveTurn(h, []);

    // Act.
    await realPrompt(h, "turn-1");

    // Assert.
    expect(h.queries.at(-1)?.spec.resumeSessionAt).toBeUndefined();
  });

  it("DELIVERS the prompt on that plain resume", async () => {
    // Arrange.
    const delivered: string[] = [];
    const h = harness({ drainPrompts: delivered, createQueryFailsAt: 1 });
    await started(h);
    await realTurn(h, "turn-0", [assistantMessage("assistant-uuid")]);
    await keepaliveTurn(h, []);

    // Act.
    await realPrompt(h, "turn-1");
    for (let attempt = 0; attempt < 50 && !delivered.includes("go"); attempt++) {
      await new Promise((resolve) => setImmediate(resolve));
    }

    // Assert.
    expect(delivered).toContain("go");
  });
});

describe("SetSessionModel", () => {
  it("refuses when no session has been started", async () => {
    const h = harness();

    const response = await h.engine.setSessionModel(
      create(shimv1.SetSessionModelRequestSchema, {
        model: create(conversationv1.AgentModelSchema, { name: "m" }),
      }),
    );

    expect(response.result.case === "failure" ? response.result.value.cause.case : undefined).toBe(
      "noSession",
    );
  });

  it("refuses a model the catalog does not carry", async () => {
    const h = harness({
      onQueryCreated: (query) => {
        query.models = [{ value: "claude-opus-5", displayName: "O", description: "d" }];
      },
    });
    const pending = h.engine.startSession(freshRequest());
    const first = await untilQuery(h, 0);
    first.query.emit(
      initMessage({ sessionId: first.spec.binding.kind === "fresh" ? first.spec.binding.sessionId : "" }),
    );
    await pending;

    const response = await h.engine.setSessionModel(
      create(shimv1.SetSessionModelRequestSchema, {
        model: create(conversationv1.AgentModelSchema, { name: "not-a-model" }),
      }),
    );

    expect(response.result.case === "failure" ? response.result.value.cause.case : undefined).toBe(
      "modelNotInCatalog",
    );
  });

  it("applies the model immediately when no turn is open", async () => {
    const h = harness();
    await started(h);

    await h.engine.setSessionModel(
      create(shimv1.SetSessionModelRequestSchema, {
        model: create(conversationv1.AgentModelSchema, { name: "claude-sonnet-5" }),
      }),
    );

    expect(h.queries[0]?.query.calls).toContain("setModel:claude-sonnet-5");
  });

  it("DEFERS the model to the turn boundary while a turn is open", async () => {
    const h = harness();
    await started(h);
    await h.engine.startTurn(
      create(shimv1.StartTurnRequestSchema, {
        turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }),
        said: textSaid("go"),
        origin: conversationv1.PromptOrigin.USER_SENT,
        pageSize: 5,
      }),
    );

    // NOT AWAITED: the call itself does not resolve until the turn boundary
    // (B4), because an ack while the running turn still answers on the old
    // model would be contradicted by that turn's own context_usage.
    const pending = h.engine.setSessionModel(
      create(shimv1.SetSessionModelRequestSchema, {
        model: create(conversationv1.AgentModelSchema, { name: "claude-sonnet-5" }),
      }),
    );
    await new Promise((resolve) => setImmediate(resolve));

    expect(h.queries[0]?.query.calls).not.toContain("setModel:claude-sonnet-5");
    h.queries[0]?.query.emit(resultMessage());
    await pending;
  });

  it("does not RESOLVE the call until the turn ends (B4)", async () => {
    const h = harness();
    await started(h);
    await h.engine.startTurn(
      create(shimv1.StartTurnRequestSchema, {
        turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }),
        said: textSaid("go"),
        origin: conversationv1.PromptOrigin.USER_SENT,
        pageSize: 5,
      }),
    );

    let settled = false;
    const pending = h.engine
      .setSessionModel(
        create(shimv1.SetSessionModelRequestSchema, {
          model: create(conversationv1.AgentModelSchema, { name: "claude-sonnet-5" }),
        }),
      )
      .then((response) => {
        settled = true;
        return response;
      });
    await new Promise((resolve) => setImmediate(resolve));

    expect(settled).toBe(false);
    h.queries[0]?.query.emit(resultMessage());
    await pending;
  });

  it("answers a call still waiting on a turn boundary when the session stands down", async () => {
    // Leaving the daemon holding a promise nothing can settle is worse than
    // telling it the change did not land.
    const h = harness();
    await started(h);
    await h.engine.startTurn(
      create(shimv1.StartTurnRequestSchema, {
        turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }),
        said: textSaid("go"),
        origin: conversationv1.PromptOrigin.USER_SENT,
        pageSize: 5,
      }),
    );
    const pending = h.engine.setSessionModel(
      create(shimv1.SetSessionModelRequestSchema, {
        model: create(conversationv1.AgentModelSchema, { name: "claude-sonnet-5" }),
      }),
    );

    await h.engine.standDown("KillSession");

    expect((await pending).result.case).toBe("failure");
  });

  it("applies the deferred model once the turn ends", async () => {
    const h = harness();
    await started(h);
    await h.engine.startTurn(
      create(shimv1.StartTurnRequestSchema, {
        turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }),
        said: textSaid("go"),
        origin: conversationv1.PromptOrigin.USER_SENT,
        pageSize: 5,
      }),
    );
    const pending = h.engine.setSessionModel(
      create(shimv1.SetSessionModelRequestSchema, {
        model: create(conversationv1.AgentModelSchema, { name: "claude-sonnet-5" }),
      }),
    );

    h.queries[0]?.query.emit(resultMessage());
    await pending;

    expect(h.queries[0]?.query.calls).toContain("setModel:claude-sonnet-5");
  });

  it("REFUSES a switch above the caller's cold threshold", async () => {
    const h = harness({ nowMs: 1_000_100 });
    writeTranscript(h.configDir, h.cwd, "resume-1", [assistantLine()]);
    const pending = h.engine.startSession(resumeRequest("resume-1"));
    (await untilQuery(h, 0)).query.emit(initMessage({ sessionId: "resume-1" }));
    await pending;

    const response = await h.engine.setSessionModel(
      create(shimv1.SetSessionModelRequestSchema, {
        model: create(conversationv1.AgentModelSchema, { name: "claude-sonnet-5" }),
        coldThresholdTokens: 100n,
      }),
    );

    expect(response.result.case === "failure" ? response.result.value.cause.case : undefined).toBe("cold");
  });

  it("ALLOWS a switch under the cold-gate floor, even at a threshold of zero", async () => {
    // The floor is the owner's, not the caller's: a threshold of 0 asks for a
    // gate on every switch, and below 70,000 tokens there is no gate to give.
    const h = harness({ nowMs: 1_000_100 });
    writeTranscript(h.configDir, h.cwd, "resume-1", [smallAssistantLine()]);
    const pending = h.engine.startSession(resumeRequest("resume-1"));
    (await untilQuery(h, 0)).query.emit(initMessage({ sessionId: "resume-1" }));
    await pending;

    const response = await h.engine.setSessionModel(
      create(shimv1.SetSessionModelRequestSchema, {
        model: create(conversationv1.AgentModelSchema, { name: "claude-sonnet-5" }),
        coldThresholdTokens: 0n,
      }),
    );

    expect(response.result.case).toBe("success");
  });
});

describe("SetSessionPermissionMode", () => {
  it("refuses when no session has been started", async () => {
    const h = harness();

    const response = await h.engine.setSessionPermissionMode(
      create(shimv1.SetSessionPermissionModeRequestSchema, {
        permissionMode: create(conversationv1.AgentPermissionModeSchema, {
          mode: { case: "plan", value: create(conversationv1.AgentPermissionModePlanSchema, {}) },
        }),
      }),
    );

    expect(response.result.case === "failure" ? response.result.value.kind.case : undefined).toBe(
      "noSession",
    );
  });

  it("sets the vendor's mode", async () => {
    const h = harness();
    await started(h);

    await h.engine.setSessionPermissionMode(
      create(shimv1.SetSessionPermissionModeRequestSchema, {
        permissionMode: create(conversationv1.AgentPermissionModeSchema, {
          mode: { case: "plan", value: create(conversationv1.AgentPermissionModePlanSchema, {}) },
        }),
      }),
    );

    expect(h.queries[0]?.query.calls).toContain("setPermissionMode:plan");
  });

  it("surfaces a vendor refusal rather than reporting a mode that is not in force", async () => {
    const h = harness();
    await started(h);
    const query = h.queries[0]?.query;
    if (query === undefined) throw new Error("no query");
    query.setPermissionModeRejects = new Error("the binary said no");

    const response = await h.engine.setSessionPermissionMode(
      create(shimv1.SetSessionPermissionModeRequestSchema, {
        permissionMode: create(conversationv1.AgentPermissionModeSchema, {
          mode: { case: "plan", value: create(conversationv1.AgentPermissionModePlanSchema, {}) },
        }),
      }),
    );

    expect(response.result.case === "failure" ? response.result.value.kind.case : undefined).toBe(
      "vendorRefused",
    );
  });
});

describe("Hibernate", () => {
  it("refuses when no session has been started", async () => {
    const h = harness();

    const response = await h.engine.hibernate(create(shimv1.HibernateRequestSchema, {}));

    expect(response.result.case === "error" ? response.result.value.kind.case : undefined).toBe(
      "noSession",
    );
  });

  it("REFUSES while a turn is in flight", async () => {
    const h = harness();
    await started(h);
    await h.engine.startTurn(
      create(shimv1.StartTurnRequestSchema, {
        turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }),
        said: textSaid("go"),
        origin: conversationv1.PromptOrigin.USER_SENT,
        pageSize: 5,
      }),
    );

    const response = await h.engine.hibernate(create(shimv1.HibernateRequestSchema, {}));

    expect(response.result.case === "error" ? response.result.value.kind.case : undefined).toBe(
      "turnInFlight",
    );
  });

  it("acks even when the session has no transcript on disk", async () => {
    // NOTHING IS READ ANY MORE. While the directive compacted, a session with
    // no transcript was a refusal, because there was nothing to summarize.
    // Stopping a shim needs no transcript at all.
    const h = harness();
    await started(h);

    const response = await h.engine.hibernate(create(shimv1.HibernateRequestSchema, {}));

    expect(response.result.case).toBe("success");
  });

  it("acks WITHOUT asking the vendor for anything", async () => {
    // THE INVARIANT OF THE OWNER'S RULING (2026-09-20). Hibernation stops the
    // shim to free its memory and does nothing else: no summary turn, no
    // throwaway query, no rewritten transcript. The query count is the whole
    // assertion -- a compaction cannot happen without one.
    const h = await hibernatable();
    const queriesBefore = h.queries.length;

    const response = await h.engine.hibernate(create(shimv1.HibernateRequestSchema, {}));

    expect([response.result.case, h.queries.length]).toEqual(["success", queriesBefore]);
  });

  it("acks a second directive the same way, still without a vendor turn", async () => {
    // THE LOOP THAT IS NOW IMPOSSIBLE. A sweep pass that did not stand the
    // shim down asks again five minutes later; while the directive compacted,
    // that ask could buy a second summary turn. It can no longer buy anything.
    const h = await hibernatable();
    await h.engine.hibernate(create(shimv1.HibernateRequestSchema, {}));
    const queriesAfterTheFirst = h.queries.length;

    const response = await h.engine.hibernate(create(shimv1.HibernateRequestSchema, {}));

    expect([response.result.case, h.queries.length]).toEqual(["success", queriesAfterTheFirst]);
  });

  it("lets KillSession finish after a hibernation", async () => {
    // THE HANG THIS GUARDS. The teardown awaits `loop`, and a hibernation that
    // left a query of its own behind made that the WRONG loop -- a promise
    // nothing the teardown does can settle, so KillSession never returned.
    const h = await hibernatable();
    await h.engine.hibernate(create(shimv1.HibernateRequestSchema, {}));

    const response = await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    expect(response.result.case === "success" ? response.result.value.closed?.how.case : undefined).toBe(
      "idle",
    );
  });
});

describe("the teardown's waits are bounded", () => {
  it("finishes KillSession when the vendor's message loop never ends after the close", async () => {
    // THE HANG THIS GUARDS. `close()` is the vendor's end-of-stream signal, not
    // a guarantee: a loop still parked in the iterator afterwards must not be
    // the reason a stand-down never returns.
    const h = harness({ closeLeavesStreamOpen: true, watcherConclusionBudgetMs: 5 });
    await started(h);

    const response = await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    expect(response.result.case === "success" ? response.result.value.closed?.how.case : undefined).toBe(
      "idle",
    );
  });

  it("finishes KillSession when the store never answers the head read a watcher's conclusion needs", async () => {
    const h = harness({ watcherConclusionBudgetMs: 5 });
    await started(h);
    h.persistence.standingTail = true;
    const watching = h.engine.watchAgent(
      create(shimv1.WatchAgentRequestSchema, { pageSize: 5 }),
    )[Symbol.asyncIterator]();
    await watching.next();
    h.persistence.openHangs = true;

    const response = await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    expect(response.result.case === "success" ? response.result.value.closed?.how.case : undefined).toBe(
      "idle",
    );
  });
});

describe("the record plane's faults", () => {
  it("restates the diagnostics as unhealthy when the store raises a fault", async () => {
    // The record plane's faults are the SESSION's: nothing subscribing to them
    // meant a store outage was visible only in the shim's own log while the
    // daemon's diagnostics stayed healthy.
    const h = harness();
    await started(h);
    const before = h.engine.pushes.faultCount;

    h.persistence.raiseFault("the store is down");

    expect(h.engine.pushes.faultCount).toBe(before + 1);
  });

  it("carries a degraded window the record plane opened into the diagnostics", async () => {
    const h = harness();
    await started(h);

    h.persistence.raiseDegradedWindow("the store is down");

    const update = h.engine.pushes.diagnostics().update;
    expect(update.case === "diagnostics" ? update.value.degradedWindows.length : 0).toBe(1);
  });
});

describe("KillSession", () => {
  it("refuses when no session has been started", async () => {
    const h = harness();

    const response = await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    expect(response.result.case === "failure" ? response.result.value.cause.case : undefined).toBe(
      "noSession",
    );
  });

  it("closes an idle session as idle", async () => {
    const h = harness();
    await started(h);

    const response = await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    expect(response.result.case === "success" ? response.result.value.closed?.how.case : undefined).toBe(
      "idle",
    );
  });

  it("ends the process with 0 once an idle session is closed", async () => {
    // KillSession is a PROCESS-level verb: the session it ends is the only one
    // this shim will serve, so a shim that kept serving would hold its socket
    // and its workspace lock against the next spawn.
    const h = harness();
    await started(h);

    await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    expect(h.exits).toEqual([0]);
  });

  it("ends the process with 1 when the store never acked some rows", async () => {
    // A23: reporting 0 would tell the daemon the session ended in good order
    // when part of the conversation never landed.
    const h = harness();
    await started(h);
    h.persistence.lostRows = 3;

    await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    expect(h.exits).toEqual([1]);
  });

  it("does NOT end the process on a refused kill", async () => {
    const h = harness();

    await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    expect(h.exits).toEqual([]);
  });

  it("REFUSES while a turn is in flight and force was not set", async () => {
    const h = harness();
    await started(h);
    await h.engine.startTurn(
      create(shimv1.StartTurnRequestSchema, {
        turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }),
        said: textSaid("go"),
        origin: conversationv1.PromptOrigin.USER_SENT,
        pageSize: 5,
      }),
    );

    const response = await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    expect(response.result.case === "failure" ? response.result.value.cause.case : undefined).toBe("live");
  });

  it("NAMES the turn it refused over", async () => {
    const h = harness();
    await started(h);
    await h.engine.startTurn(
      create(shimv1.StartTurnRequestSchema, {
        turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }),
        said: textSaid("go"),
        origin: conversationv1.PromptOrigin.USER_SENT,
        pageSize: 5,
      }),
    );

    const response = await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));
    const failure = response.result.case === "failure" ? response.result.value : undefined;
    expect(
      failure?.cause.case === "live" ? failure.cause.value.turnInFlight?.value : undefined,
    ).toBe("turn-1");
  });

  it("names the interrupted turn when forced", async () => {
    const h = harness();
    await started(h);
    await h.engine.startTurn(
      create(shimv1.StartTurnRequestSchema, {
        turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }),
        said: textSaid("go"),
        origin: conversationv1.PromptOrigin.USER_SENT,
        pageSize: 5,
      }),
    );

    const response = await h.engine.killSession(
      create(shimv1.KillSessionRequestSchema, { force: true }),
    );
    const killed = response.result.case === "success" ? response.result.value.closed : undefined;
    expect(
      killed?.how.case === "forced" ? killed.how.value.interruptedTurn?.value : undefined,
    ).toBe("turn-1");
  });

  it("FLUSHES every buffered write before answering", async () => {
    const h = harness();
    await started(h);

    await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    expect(h.persistence.flushes).toBe(1);
  });

  it("releases BOTH kernel claims", async () => {
    // The session claim and the workspace claim are both taken by StartSession
    // and both belong to the session, so a kill that kept either would leave a
    // dead conversation owning a lock the next shim probes.
    const h = harness();
    await started(h);

    await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    expect(h.released).toEqual([...h.locks, `workspace:${h.cwd}`]);
  });
});

describe("standing down", () => {
  it("resolves every pending permission callback as DENIED first", async () => {
    // An unresolved canUseTool promise wedges the vendor process outright.
    const h = harness();
    await started(h);
    const spec = h.queries[0]?.spec;
    if (spec === undefined) throw new Error("no query");
    const pending = spec.canUseTool("Bash", {}, {
      signal: new AbortController().signal,
      toolUseID: "toolu_1",
      requestId: "r",
    });
    await Promise.resolve();

    await h.engine.standDown("SIGTERM");

    expect(await pending).toEqual({ behavior: "deny", message: "SIGTERM" });
  });

  it("closes the query", async () => {
    const h = harness();
    await started(h);

    await h.engine.standDown("SIGTERM");

    expect(h.queries[0]?.query.calls).toContain("close");
  });

  it("ends every WatchSession stream", async () => {
    const h = harness();
    await started(h);
    const iterator = h.engine.watchSession(create(shimv1.WatchSessionRequestSchema, {}))[
      Symbol.asyncIterator
    ]();
    await iterator.next();

    await h.engine.standDown("SIGTERM");

    // The stream TERMINATES; the frames already queued for this consumer are
    // still delivered, because a fact the shim stated is not un-stated by the
    // shim going away.
    for (let index = 0; index < 32; index++) {
      const step = await iterator.next();
      if (step.done === true) {
        expect(step.done).toBe(true);
        return;
      }
    }
    throw new Error("the WatchSession stream did not terminate after the stand-down");
  });

  it("is idempotent, so a second SIGTERM cannot double-release either lock", async () => {
    const h = harness();
    await started(h);

    await h.engine.standDown("SIGTERM");
    await h.engine.standDown("SIGTERM");

    expect(h.released).toEqual([...h.locks, `workspace:${h.cwd}`]);
  });
});

describe("WatchSession", () => {
  /** The arm names of the first `count` frames of a fresh watch. */
  async function frames(h: Harness, count: number): Promise<string[]> {
    const iterator = h.engine.watchSession(create(shimv1.WatchSessionRequestSchema, {}))[
      Symbol.asyncIterator
    ]();
    const seen: string[] = [];
    for (let taken = 0; taken < count; taken++) {
      const next = await iterator.next();
      if (next.done === true) break;
      const frame = next.value.frame;
      seen.push(frame.case === "update" ? `update.${frame.value.update.case ?? "unset"}` : (frame.case ?? "unset"));
    }
    await iterator.return?.();
    return seen;
  }

  it("delivers diagnostics as its FIRST frame", async () => {
    const h = harness();

    expect((await frames(h, 1))[0]).toBe("update.diagnostics");
  });

  it("re-announces nothing before a session has started", async () => {
    // There is no opening to re-state, and the diagnostics already said so.
    // A second frame is pushed so the assertion reads a real frame rather than
    // waiting out a stream that would correctly never produce one.
    const h = harness();
    const iterator = h.engine.watchSession(create(shimv1.WatchSessionRequestSchema, {}))[
      Symbol.asyncIterator
    ]();
    await iterator.next();
    h.engine.pushes.push(
      create(conversationv1.SessionUpdateSchema, {
        update: {
          case: "queryDied",
          value: create(conversationv1.SessionQueryDiedSchema, {}),
        },
      }),
    );

    const second = await nextPush(iterator);
    await iterator.return?.();

    expect(second.frame.case).toBe("update");
  });

  it("re-announces the session's opening right AFTER the diagnostics", async () => {
    // Landing 7: a daemon adopting an already-started shim attaches purely.
    const h = harness();
    await started(h);

    expect((await frames(h, 2))[1]).toBe("sessionStarted");
  });

  it("re-announces on EVERY new watch, not only the first", async () => {
    const h = harness();
    await started(h);
    await frames(h, 2);

    expect((await frames(h, 2))[1]).toBe("sessionStarted");
  });

  it("re-states the ORIGINAL identity, which is fixed for the session", async () => {
    const h = harness();
    const opening = await started(h);
    const announced =
      opening.result.case === "success" ? opening.result.value.session?.vendorSessionId : undefined;

    const iterator = h.engine.watchSession(create(shimv1.WatchSessionRequestSchema, {}))[
      Symbol.asyncIterator
    ]();
    await iterator.next();
    const second = await nextPush(iterator);
    await iterator.return?.();

    expect(
      second.frame.case === "sessionStarted"
        ? second.frame.value.vendorSessionId
        : undefined,
    ).toBe(announced);
  });

  it("re-states the turn in flight as it is NOW, not as the opening found it", async () => {
    // The opening's live membership is the one thing that is not a fact at
    // start: an adopting daemon needs what is live now.
    const h = harness();
    await started(h);
    await h.engine.startTurn(
      create(shimv1.StartTurnRequestSchema, {
        turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }),
        said: textSaid("go"),
        origin: conversationv1.PromptOrigin.USER_SENT,
        pageSize: 5,
      }),
    );

    const iterator = h.engine.watchSession(create(shimv1.WatchSessionRequestSchema, {}))[
      Symbol.asyncIterator
    ]();
    await iterator.next();
    const second = await nextPush(iterator);
    await iterator.return?.();

    expect(
      second.frame.case === "sessionStarted"
        ? second.frame.value.turnInFlight?.value !== undefined
        : undefined,
    ).toBe(true);
  });
});

describe("GetLiveWork reconciliation", () => {
  /** One recorded shell run in the main agent's book, as the store holds it. */
  function recordedBashRun(unit: string): conversationv1.HistoryEntryAt {
    return create(conversationv1.HistoryEntryAtSchema, {
      at: create(conversationv1.HistoryPointerSchema, { value: "1" }),
      entry: create(conversationv1.HistoryEntrySchema, {
        entry: {
          case: "agentFrame",
          value: create(conversationv1.AgentFrameSchema, {
            result: {
              case: "update",
              value: create(conversationv1.AgentUpdateSchema, {
                update: {
                  case: "activity",
                  value: create(conversationv1.AgentActivitySchema, {
                    activityId: create(conversationv1.AgentActivityIdSchema, { value: unit }),
                    item: {
                      case: "bash",
                      value: create(conversationv1.AgentBashSchema, {
                        result: {
                          case: "start",
                          value: create(conversationv1.AgentBashStartSchema, {
                            command: create(conversationv1.AgentBashCommandSchema, {
                              line: "sleep 100",
                            }),
                            startedAt: create(conversationv1.AgentActivityStartedAtSchema, {
                              atMs: 5n,
                            }),
                          }),
                        },
                      }),
                    },
                  }),
                },
              }),
            },
          }),
        },
      }),
    });
  }

  /** One recorded unit of the main book, keyed `unit`, holding `item`. */
  function recordedUnit(unit: string, item: conversationv1.AgentActivity["item"]): conversationv1.HistoryEntryAt {
    return create(conversationv1.HistoryEntryAtSchema, {
      at: create(conversationv1.HistoryPointerSchema, { value: unit }),
      entry: create(conversationv1.HistoryEntrySchema, {
        entry: {
          case: "agentFrame",
          value: create(conversationv1.AgentFrameSchema, {
            result: {
              case: "update",
              value: create(conversationv1.AgentUpdateSchema, {
                update: {
                  case: "activity",
                  value: create(conversationv1.AgentActivitySchema, {
                    activityId: create(conversationv1.AgentActivityIdSchema, { value: unit }),
                    item,
                  }),
                },
              }),
            },
          }),
        },
      }),
    });
  }

  /** An announcement of `work` in the main book, stating `kind` and no unit row. */
  function recordedAnnouncement(work: string, kind: conversationv1.DetachedWorkKind["kind"]): conversationv1.HistoryEntryAt {
    return create(conversationv1.HistoryEntryAtSchema, {
      at: create(conversationv1.HistoryPointerSchema, { value: `announce-${work}` }),
      entry: create(conversationv1.HistoryEntrySchema, {
        entry: {
          case: "agentFrame",
          value: create(conversationv1.AgentFrameSchema, {
            result: {
              case: "detachedWork",
              value: create(conversationv1.AgentDetachedWorkSchema, {
                work: create(conversationv1.DetachedWorkIdSchema, { value: work }),
                kind: create(conversationv1.DetachedWorkKindSchema, { kind }),
              }),
            },
          }),
        },
      }),
    });
  }

  /** A session revived over a store holding `work` live and a book of `entries`. */
  function revivedOver(work: string, entries: conversationv1.HistoryEntryAt[]): Harness {
    const h = harness();
    h.persistence.live = create(storev1.GetLiveWorkSuccessSchema, {
      liveDetached: [create(conversationv1.DetachedWorkIdSchema, { value: work })],
    });
    h.persistence.page = create(conversationv1.HistoryPageSchema, {
      entries,
      boundary: { case: "floor", value: create(conversationv1.HistoryFloorSchema, {}) },
    });
    return h;
  }

  const RUNNING_SPAWN: conversationv1.AgentActivity["item"] = {
    case: "subagent",
    value: create(conversationv1.AgentSubagentSchema, {
      result: {
        case: "update",
        value: create(conversationv1.AgentSubagentUpdateSchema, {
          prompt: create(conversationv1.AgentSubagentPromptSchema, { text: "sweep" }),
        }),
      },
    }),
  };

  const ARMED_MONITOR: conversationv1.AgentActivity["item"] = {
    case: "monitor",
    value: create(conversationv1.AgentMonitorSchema, {
      result: { case: "start", value: create(conversationv1.AgentMonitorStartSchema, { description: "watch" }) },
    }),
  };

  it("leaves a recorded shell run live, writing no terminal: its spool is the sidecar's", async () => {
    // Arrange.
    const h = revivedOver("b01", [recordedBashRun("b01")]);

    // Act.
    await started(h);

    // Assert.
    expect(h.persistence.buffered.some((entry) => entry.upsertKey === "bash:b01:terminal")).toBe(false);
  });

  it("re-adopts the recorded shell run as live work at StartSession", async () => {
    // Arrange.
    const h = revivedOver("b01", [recordedBashRun("b01")]);

    // Act.
    const response = await started(h);

    // Assert.
    const live = response.result.case === "success" ? (response.result.value.session?.liveWork ?? []) : [];
    expect(live.map((work) => work.work?.value)).toEqual(["b01"]);
  });

  it("records the shell left for the sidecar at debug", async () => {
    // Arrange.
    const h = revivedOver("b01", [recordedBashRun("b01")]);
    const before = logSinkMark();

    // Act.
    await started(h);

    // Assert.
    const message = "a spool-backed run outlives the CLI process; it stays live and its terminal is the sidecar's";
    expect([logLevelFor(before, message), logContextFor(before, message)?.work_id]).toEqual(["debug", "b01"]);
  });

  it("never asks the vendor about survival at StartSession", async () => {
    // Arrange.
    const h = revivedOver("toolu_spawn", [recordedUnit("toolu_spawn", RUNNING_SPAWN)]);

    // Act.
    await started(h);

    // Assert.
    expect(h.queries[0]?.query.calls.filter((call) => call.startsWith("backgroundTasks"))).toEqual([]);
  });

  it("closes a recorded subagent run with its lost terminal: it ran inside the replaced CLI process", async () => {
    // Arrange.
    const h = revivedOver("toolu_spawn", [recordedUnit("toolu_spawn", RUNNING_SPAWN)]);

    // Act.
    await started(h);

    // Assert.
    expect(
      h.persistence.buffered
        .filter((entry) => entry.upsertKey === "activity:toolu_spawn")
        .map((entry) => entry.source.discriminator),
    ).toEqual(["activity.subagent.failure.lost.swept_up"]);
  });

  it("records the in-process closing at INFO, naming the reason", async () => {
    // Arrange.
    const h = revivedOver("toolu_spawn", [recordedUnit("toolu_spawn", RUNNING_SPAWN)]);
    const before = logSinkMark();

    // Act.
    await started(h);

    // Assert.
    const message = "closed live work that ran inside the CLI process: the CLI process that ran it was replaced";
    expect([logLevelFor(before, message), logContextFor(before, message)?.reason]).toEqual([
      "info",
      "the CLI process that ran it was replaced",
    ]);
  });

  it("does not re-adopt the closed subagent run", async () => {
    // Arrange.
    const h = revivedOver("toolu_spawn", [recordedUnit("toolu_spawn", RUNNING_SPAWN)]);

    // Act.
    const response = await started(h);

    // Assert.
    expect(response.result.case === "success" ? response.result.value.session?.liveWork : undefined).toEqual([]);
  });

  it("closes a recorded monitor with its ended arm: its watch was the CLI's", async () => {
    // Arrange.
    const h = revivedOver("toolu_mon", [recordedUnit("toolu_mon", ARMED_MONITOR)]);

    // Act.
    await started(h);

    // Assert.
    const closing = h.persistence.buffered.find((entry) => entry.upsertKey === "activity:toolu_mon");
    const frame = closing?.item.kind === "frame" ? closing.item.frame : undefined;
    const activity = frame?.result.case === "update" && frame.result.value.update.case === "activity" ? frame.result.value.update.value : undefined;
    expect(activity?.item.case === "monitor" ? activity.item.value.result.case : undefined).toBe("ended");
  });

  it("closes an item its announcement states is a subagent, with no unit row in the book", async () => {
    // Arrange.
    const h = revivedOver("toolu_sub", [
      recordedAnnouncement("toolu_sub", {
        case: "subagent",
        value: create(conversationv1.DetachedWorkKindSubagentSchema, { agentId: create(conversationv1.AgentIdSchema, { value: "toolu_sub" }) }),
      }),
    ]);

    // Act.
    await started(h);

    // Assert.
    expect(h.persistence.buffered.some((entry) => entry.upsertKey === "activity:toolu_sub")).toBe(true);
  });

  it("leaves live an item its announcement states is a shell, with no unit row in the book", async () => {
    // Arrange.
    const h = revivedOver("b09", [
      recordedAnnouncement("b09", { case: "bash", value: create(conversationv1.DetachedWorkKindBashSchema, {}) }),
    ]);

    // Act.
    await started(h);

    // Assert.
    expect(h.persistence.buffered.filter((entry) => entry.upsertKey.includes("b09"))).toEqual([]);
  });

  it("leaves live an item the record states no kind for, writing nothing for it", async () => {
    // Arrange.
    const h = revivedOver("b01", []);

    // Act.
    await started(h);

    // Assert.
    expect(h.persistence.buffered.filter((entry) => entry.upsertKey.includes("b01"))).toEqual([]);
  });

  it("records an item the record states no kind for at WARN, as the decision to leave it live", async () => {
    // Arrange.
    const h = revivedOver("b01", []);
    const before = logSinkMark();

    // Act.
    await started(h);

    // Assert.
    const message =
      "the record states no kind for this live work; it is left live, since only work that ran in the CLI process may be closed at revival";
    expect([logLevelFor(before, message), logContextFor(before, message)?.work_id]).toEqual(["warn", "b01"]);
  });

  it("re-announces the live membership as it is NOW on a new watch", async () => {
    // Landing 7: everything else on the opening is a fact at start, but a
    // daemon adopting a running shim needs the membership that is live now.
    const h = harness({ backgroundTasks: true });
    h.persistence.live = create(storev1.GetLiveWorkSuccessSchema, {
      liveDetached: [create(conversationv1.DetachedWorkIdSchema, { value: "b01" })],
    });
    h.persistence.page = create(conversationv1.HistoryPageSchema, {
      entries: [recordedBashRun("b01")],
      boundary: { case: "floor", value: create(conversationv1.HistoryFloorSchema, {}) },
    });
    await started(h);

    const iterator = h.engine.watchSession(create(shimv1.WatchSessionRequestSchema, {}))[
      Symbol.asyncIterator
    ]();
    await iterator.next();
    const second = await nextPush(iterator);
    await iterator.return?.();

    expect(
      second.frame.case === "sessionStarted"
        ? second.frame.value.liveWork.map(
            (work: conversationv1.AgentDetachedWork) => work.work?.value,
          )
        : undefined,
    ).toEqual(["b01"]);
  });

  it("REPORTS a book it could not read for a re-announcement as a session fault", async () => {
    // A watch that opens is better than one that fails, but a record plane the
    // shim cannot reach is a session-level fact, never a quiet empty list.
    const h = harness({ backgroundTasks: true });
    h.persistence.live = create(storev1.GetLiveWorkSuccessSchema, {
      liveDetached: [create(conversationv1.DetachedWorkIdSchema, { value: "b01" })],
    });
    h.persistence.page = create(conversationv1.HistoryPageSchema, {
      entries: [recordedBashRun("b01")],
      boundary: { case: "floor", value: create(conversationv1.HistoryFloorSchema, {}) },
    });
    await started(h);
    h.persistence.openError = new PersistenceError(
      "store_unavailable",
      "the store is down",
    );

    const iterator = h.engine.watchSession(create(shimv1.WatchSessionRequestSchema, {}))[
      Symbol.asyncIterator
    ]();
    await iterator.next();
    const unhealthy: boolean[] = [];
    // The re-announcement and the replayed current view sit ahead of the
    // restated diagnostics the fault produces; this drains past them and stops
    // as soon as one is seen, so it never waits on a frame that will not come.
    for (let taken = 0; taken < 8 && !unhealthy.includes(true); taken++) {
      const next = await iterator.next();
      if (next.done === true) break;
      const frame = next.value.frame;
      if (frame.case !== "update") continue;
      const update = frame.value.update;
      if (update.case === "diagnostics") unhealthy.push(update.value.health.case === "unhealthy");
    }
    await iterator.return?.();

    // A fault restates the diagnostics, which is how every consumer learns it.
    expect(unhealthy).toContain(true);
  });

  it("writes a closing terminal for a subagent that did not survive", async () => {
    const h = harness();
    h.persistence.live = create(storev1.GetLiveWorkSuccessSchema, {
      liveAgents: [create(conversationv1.AgentIdSchema, { value: "sub-1" })],
    });

    await started(h);

    expect(
      h.persistence.buffered.some((entry) => entry.agentId?.value === "sub-1"),
    ).toBe(true);
  });

  it("does not close the main agent's own book", async () => {
    const h = harness();
    const response = await started(h);
    const agent =
      response.result.case === "success" ? (response.result.value.session?.vendorSessionId ?? "") : "";

    expect(h.persistence.buffered.some((entry) => entry.agentId?.value === agent && entry.item.kind === "frame")).toBe(
      false,
    );
  });

  it("scopes the StartSession live-work read to its own main agent", async () => {
    // The store is shared by every session on the host; an unscoped read
    // handed this session other sessions' running work to close.
    const h = harness();
    const response = await started(h);
    const agent =
      response.result.case === "success" ? (response.result.value.session?.vendorSessionId ?? "") : "";

    expect(h.persistence.liveWorkSessions).toEqual([agent]);
  });

  it("scopes a joining watch's re-announcement read to its own main agent", async () => {
    const h = harness({ backgroundTasks: true });
    const response = await started(h);
    const agent =
      response.result.case === "success" ? (response.result.value.session?.vendorSessionId ?? "") : "";

    const iterator = h.engine.watchSession(create(shimv1.WatchSessionRequestSchema, {}))[
      Symbol.asyncIterator
    ]();
    await iterator.next();
    await nextPush(iterator);
    await iterator.return?.();

    expect(h.persistence.liveWorkSessions).toEqual([agent, agent]);
  });

  it("reports a refused re-announcement read as a session fault, never an empty membership", async () => {
    // `invalid_request` is this process's defect; reading it as "no book yet"
    // would re-announce nothing and say nothing.
    const h = harness({ backgroundTasks: true });
    await started(h);
    h.persistence.liveWorkError = new PersistenceError("invalid_request", "session: refused");

    const iterator = h.engine.watchSession(create(shimv1.WatchSessionRequestSchema, {}))[
      Symbol.asyncIterator
    ]();
    await iterator.next();
    const unhealthy: boolean[] = [];
    for (let taken = 0; taken < 8 && !unhealthy.includes(true); taken++) {
      const next = await iterator.next();
      if (next.done === true) break;
      const frame = next.value.frame;
      if (frame.case !== "update") continue;
      const update = frame.value.update;
      if (update.case === "diagnostics") unhealthy.push(update.value.health.case === "unhealthy");
    }
    await iterator.return?.();

    expect(unhealthy).toContain(true);
  });

  it("reports a fault rather than failing the start when the store is unreachable", async () => {
    const h = harness();
    h.persistence.liveWorkError = new PersistenceError(
      "store_unavailable",
      "down",
    );

    expect((await started(h)).result.case).toBe("success");
  });
});

describe("the converter's own health", () => {
  /** The diagnostics the engine would state right now. */
  function diagnostics(h: Harness): conversationv1.SessionDiagnostics {
    const update = h.engine.pushes.diagnostics().update;
    if (update.case !== "diagnostics") throw new Error("the engine stated no diagnostics");
    return update.value;
  }

  const prose = (uuid: string): never =>
    ({
      type: "assistant",
      uuid,
      session_id: "s",
      parent_tool_use_id: null,
      message: { model: "claude-opus-5", content: [] },
    }) as never;

  it("reports a refused message as a converter_defect fault", async () => {
    const h = harness();
    await started(h);
    h.fold.faultFor = (message) => (message.type === "assistant" ? "the hook firing id is empty" : undefined);

    await h.engine.onSdkMessage(prose("u-defect"));

    const health = diagnostics(h).health;
    expect(health.case === "unhealthy" ? health.value.faults[0]?.kind.case : "").toBe("converterDefect");
  });

  it("names the converter as the faulting component", async () => {
    const h = harness();
    await started(h);
    h.fold.faultFor = (message) => (message.type === "assistant" ? "boom" : undefined);

    await h.engine.onSdkMessage(prose("u-defect"));

    const health = diagnostics(h).health;
    expect(health.case === "unhealthy" ? health.value.faults[0]?.component : "").toBe("converter");
  });

  it("opens a degraded window for the converter", async () => {
    const h = harness();
    await started(h);
    h.fold.faultFor = (message) => (message.type === "assistant" ? "boom" : undefined);

    await h.engine.onSdkMessage(prose("u-defect"));

    expect(diagnostics(h).degradedWindows[0]?.extent.case).toBe("open");
  });

  // THE TURN IS THE UNIT OF RECOVERY, not the message: the messages after a
  // refusal are the same turn's own remainder, and the turn that lost a record
  // stays degraded for its whole length. The `!fault-converter` /
  // `!fault-recover` pair states exactly that — the defective turn leaves an
  // OPEN window, and the clean turn after it is what closes it.
  it("stays degraded for the rest of the turn a message was refused in", async () => {
    const h = harness();
    await started(h);
    h.fold.faultFor = (message) =>
      (message as { uuid?: string }).uuid === "u-defect" ? "boom" : undefined;
    await h.engine.onSdkMessage(prose("u-defect"));

    await h.engine.onSdkMessage(prose("u-good"));

    expect(diagnostics(h).health.case).toBe("unhealthy");
  });

  it("returns to healthy at the end of a turn that refused nothing", async () => {
    const h = harness();
    await started(h);
    h.fold.faultFor = (message) =>
      (message as { uuid?: string }).uuid === "u-defect" ? "boom" : undefined;
    await h.engine.onSdkMessage(prose("u-defect"));
    await h.engine.onSdkMessage(resultMessage("u-result-1"));

    await h.engine.onSdkMessage(resultMessage("u-result-2"));

    expect(diagnostics(h).health.case).toBe("healthy");
  });

  it("closes the window with the number of messages it refused", async () => {
    const h = harness();
    await started(h);
    h.fold.faultFor = (message) =>
      (message as { uuid?: string }).uuid?.startsWith("u-defect") === true ? "boom" : undefined;
    await h.engine.onSdkMessage(prose("u-defect-1"));
    await h.engine.onSdkMessage(prose("u-defect-2"));
    await h.engine.onSdkMessage(resultMessage("u-result-1"));

    await h.engine.onSdkMessage(resultMessage("u-result-2"));

    const window = diagnostics(h).degradedWindows[0];
    expect(window?.extent.case === "closed" ? window.extent.value.droppedCount : -1n).toBe(2n);
  });

  it("opens ONE window across consecutive refusals", async () => {
    const h = harness();
    await started(h);
    h.fold.faultFor = () => "boom";

    await h.engine.onSdkMessage(prose("u-defect-1"));
    await h.engine.onSdkMessage(prose("u-defect-2"));

    expect(diagnostics(h).degradedWindows.length).toBe(1);
  });

  it("leaves the window OPEN at the end of the turn that refused a message", async () => {
    const h = harness();
    await started(h);
    h.fold.faultFor = (message) => (message.type === "assistant" ? "boom" : undefined);
    await h.engine.onSdkMessage(prose("u-defect"));

    await h.engine.onSdkMessage(resultMessage("u-result"));

    expect(diagnostics(h).degradedWindows[0]?.extent.case).toBe("open");
  });

  it("closes the window at the end of the next clean turn", async () => {
    const h = harness();
    await started(h);
    h.fold.faultFor = (message) => (message.type === "assistant" ? "boom" : undefined);
    await h.engine.onSdkMessage(prose("u-defect"));
    await h.engine.onSdkMessage(resultMessage("u-result-1"));
    h.fold.faultFor = () => undefined;

    await h.engine.onSdkMessage(resultMessage("u-result-2"));

    expect(diagnostics(h).degradedWindows[0]?.extent.case).toBe("closed");
  });
});

describe("the model the vendor answers on", () => {
  /** An assistant message reporting the model that produced it. */
  const answeredOn = (model: string): never =>
    ({
      type: "assistant",
      uuid: `u-${model}`,
      session_id: "s",
      parent_tool_use_id: null,
      message: { model, content: [] },
    }) as never;

  /** Every model name the engine pushed, in order. */
  async function pushedModels(h: Harness, act: () => Promise<void>): Promise<string[]> {
    const stream = h.engine.pushes.subscribe()[Symbol.asyncIterator]();
    const names: string[] = [];
    const reading = (async () => {
      for (;;) {
        const step = await stream.next();
        if (step.done === true) return;
        const update = step.value.update;
        if (update.case === "modelChanged") names.push(update.value.effectiveModel?.name ?? "");
      }
    })();
    await act();
    await h.engine.killSession(create(shimv1.KillSessionRequestSchema, { force: true }));
    await reading;
    return names;
  }

  it("pushes model_changed for a model nothing asked for", async () => {
    const h = harness();
    await started(h);

    const names = await pushedModels(h, async () => {
      await h.engine.onSdkMessage(answeredOn("claude-haiku-4-5"));
    });

    expect(names).toContain("claude-haiku-4-5");
  });

  it("pushes nothing when the reported model is the one in effect", async () => {
    const h = harness();
    await started(h);

    const names = await pushedModels(h, async () => {
      await h.engine.onSdkMessage(answeredOn("claude-opus-5"));
    });

    expect(names.filter((name) => name === "claude-opus-5").length).toBe(1);
  });

  it("never adopts the synthetic marker as a model", async () => {
    const h = harness();
    await started(h);

    const names = await pushedModels(h, async () => {
      await h.engine.onSdkMessage(answeredOn(SYNTHETIC_MODEL));
    });

    expect(names).not.toContain(SYNTHETIC_MODEL);
  });

  /** The vendor's own announcement that it retried on another model. */
  const refusalFallback = (original: string, fallback: string): never =>
    ({
      type: "system",
      subtype: "model_refusal_fallback",
      uuid: `u-fallback-${fallback}`,
      session_id: "s",
      trigger: "refusal",
      direction: "retry",
      original_model: original,
      fallback_model: fallback,
      request_id: "req_fallback",
      api_refusal_category: "cyber",
      api_refusal_explanation: null,
      retracted_message_uuids: [],
      refused_user_message_uuid: null,
      content: `Switched to ${fallback}.`,
    }) as never;

  it("folds the vendor's refusal fallback into model_changed", async () => {
    const h = harness();
    await started(h);

    const names = await pushedModels(h, async () => {
      await h.engine.onSdkMessage(refusalFallback("claude-opus-5", "claude-haiku-4-5"));
    });

    expect(names).toContain("claude-haiku-4-5");
  });

  it("states nothing when the fallback names the model already in effect", async () => {
    const h = harness();
    await started(h);

    const names = await pushedModels(h, async () => {
      await h.engine.onSdkMessage(refusalFallback("claude-opus-5", "claude-opus-5"));
    });

    expect(names.filter((name) => name === "claude-opus-5").length).toBe(1);
  });

  it("states the fallback once when the fallback leg's answer agrees with it", async () => {
    const h = harness();
    await started(h);

    const names = await pushedModels(h, async () => {
      await h.engine.onSdkMessage(refusalFallback("claude-opus-5", "claude-haiku-4-5"));
      await h.engine.onSdkMessage(answeredOn("claude-haiku-4-5"));
    });

    expect(names.filter((name) => name === "claude-haiku-4-5").length).toBe(1);
  });

  it("never adopts the synthetic marker from a fallback record", async () => {
    const h = harness();
    await started(h);

    const names = await pushedModels(h, async () => {
      await h.engine.onSdkMessage(refusalFallback("claude-opus-5", SYNTHETIC_MODEL));
    });

    expect(names).not.toContain(SYNTHETIC_MODEL);
  });

  it("keeps the adopted model for the next message that agrees with it", async () => {
    const h = harness();
    await started(h);

    const names = await pushedModels(h, async () => {
      await h.engine.onSdkMessage(answeredOn("claude-haiku-4-5"));
      await h.engine.onSdkMessage(answeredOn("claude-haiku-4-5"));
    });

    expect(names.filter((name) => name === "claude-haiku-4-5").length).toBe(1);
  });
});

describe("fast mode", () => {
  /** A turn terminal restating the session's fast-mode state. */
  const resultWithFastMode = (state: string, reason?: string): never =>
    ({
      ...(resultMessage("u-fast") as unknown as Record<string, unknown>),
      fast_mode_state: state,
      ...(reason === undefined ? {} : { fast_mode_disabled_reason: reason }),
    }) as never;

  /** Every fast-mode arm the engine pushed, in order. */
  async function pushedFastMode(h: Harness, act: () => Promise<void>): Promise<string[]> {
    const stream = h.engine.pushes.subscribe()[Symbol.asyncIterator]();
    const arms: string[] = [];
    const reading = (async () => {
      for (;;) {
        const step = await stream.next();
        if (step.done === true) return;
        const update = step.value.update;
        if (update.case === "fastMode") arms.push(update.value.state.case ?? "");
      }
    })();
    await act();
    await h.engine.killSession(create(shimv1.KillSessionRequestSchema, { force: true }));
    await reading;
    return arms;
  }

  it("pushes the state a result reports", async () => {
    const h = harness();
    await started(h);

    const arms = await pushedFastMode(h, async () => {
      await h.engine.onSdkMessage(resultWithFastMode("on"));
    });

    expect(arms).toContain("on");
  });

  it("carries the vendor's own reason on the off arm", async () => {
    const h = harness();
    await started(h);
    const stream = h.engine.pushes.subscribe()[Symbol.asyncIterator]();
    const reasons: string[] = [];
    const reading = (async () => {
      for (;;) {
        const step = await stream.next();
        if (step.done === true) return;
        const update = step.value.update;
        if (update.case === "fastMode" && update.value.state.case === "off") {
          reasons.push(update.value.state.value.reason);
        }
      }
    })();

    await h.engine.onSdkMessage(resultWithFastMode("off", "preference"));
    await h.engine.killSession(create(shimv1.KillSessionRequestSchema, { force: true }));
    await reading;

    expect(reasons).toContain("preference");
  });

  it("distinguishes a cooldown from off", async () => {
    const h = harness();
    await started(h);

    const arms = await pushedFastMode(h, async () => {
      await h.engine.onSdkMessage(resultWithFastMode("cooldown"));
    });

    expect(arms).toContain("cooldown");
  });

  it("pushes an unchanged state only once", async () => {
    const h = harness();
    await started(h);

    const arms = await pushedFastMode(h, async () => {
      await h.engine.onSdkMessage(resultWithFastMode("on"));
      await h.engine.onSdkMessage(resultWithFastMode("on"));
    });

    expect(arms.filter((arm) => arm === "on").length).toBe(1);
  });

  it("pushes nothing for a result that states no fast-mode state", async () => {
    const h = harness();
    await started(h);

    const arms = await pushedFastMode(h, async () => {
      await h.engine.onSdkMessage(resultMessage("u-silent"));
    });

    expect(arms).toEqual([]);
  });

  it("replays the current state to a consumer that joins late", async () => {
    const h = harness();
    await started(h);
    await h.engine.onSdkMessage(resultWithFastMode("on"));

    const arms = await pushedFastMode(h, async () => undefined);

    expect(arms).toEqual(["on"]);
  });
});

describe("the session-started record", () => {
  it("states how long each awaited start step took", async () => {
    // Arrange.
    const h = harness();
    const mark = logSinkMark();

    // Act.
    await started(h);

    // Assert.
    const record = logRecordsSince(mark).find((entry) => entry.message === "session started");
    expect(record?.context).toMatchObject({
      mcp_status_ms: expect.any(Number),
      live_work_ms: expect.any(Number),
      context_usage_ms: expect.any(Number),
    });
  });
});

describe("the keep-alive interval", () => {
  it("beats on the module constant when nothing overrode it", async () => {
    const h = harness();

    await started(h);

    expect(h.scheduler.intervals[0]).toBe(KEEPALIVE_INTERVAL_MS);
  });

  it("beats on the interval the caller supplied", async () => {
    const h = harness({ keepaliveIntervalMs: 200 });

    await started(h);

    expect(h.scheduler.intervals[0]).toBe(200);
  });
});

/**
 * `engine`'s per-turn verbs are one-line delegations to `turns.*`
 * (`watchAgent: (request) => turns.watchAgent(request)`, and so on) — the
 * dispatch surface `service/server.ts` actually calls. `engine/turn.test.ts`
 * covers `turns.*` directly and exhaustively; nothing calls them THROUGH
 * `engine` in any unit test, which is why coverage saw these arrows as
 * zero-hit. This pins that the delegation itself works, with the cheapest
 * refusal each verb answers before a session exists.
 */
describe("the per-turn verbs, through the engine's own dispatch surface", () => {
  it("watchAgent refuses (at iteration) with no session started", async () => {
    const h = harness();

    await expect(
      (async () => {
        for await (const _ of h.engine.watchAgent(
          create(shimv1.WatchAgentRequestSchema, { pageSize: 10 }),
        )) {
          // refused before anything is yielded
        }
      })(),
    ).rejects.toThrow();
  });

  it("updateAgent refuses noSession with no session started", async () => {
    const h = harness();

    const response = await h.engine.updateAgent(create(shimv1.UpdateAgentRequestSchema, {}));

    expect(
      response.result.case === "failure" ? response.result.value.kind.case : undefined,
    ).toBe("noSession");
  });

  it("killTurn refuses noSession with no session started", async () => {
    const h = harness();

    const response = await h.engine.killTurn(create(shimv1.KillTurnRequestSchema, {}));

    expect(
      response.result.case === "failure" ? response.result.value.cause.case : undefined,
    ).toBe("noSession");
  });

  it("watchBash refuses (at iteration) with no work id named", async () => {
    const h = harness();

    await expect(
      (async () => {
        for await (const _ of h.engine.watchBash(create(shimv1.WatchBashRequestSchema, {}))) {
          // refused before anything is yielded
        }
      })(),
    ).rejects.toThrow();
  });

  it("stopBash refuses unknownWork for a work id nothing announced", async () => {
    const h = harness();

    const response = await h.engine.stopBash(
      create(shimv1.StopBashRequestSchema, {
        work: create(conversationv1.DetachedWorkIdSchema, { value: "b-nope" }),
      }),
    );

    expect(
      response.result.case === "failure" ? response.result.value.kind.case : undefined,
    ).toBe("unknownWork");
  });

  it("detachForeground refuses noSession with no session started", async () => {
    const h = harness();

    const response = await h.engine.detachForeground(
      create(shimv1.DetachForegroundRequestSchema, {
        unit: create(conversationv1.AgentActivityIdSchema, { value: "toolu_1" }),
      }),
    );

    expect(
      response.result.case === "failure" ? response.result.value.kind.case : undefined,
    ).toBe("noSession");
  });
});

/**
 * A standing grant's mode change: engine/turn.ts's `answer`/`answerOutcome`
 * (UpdateAgent's "answer" input arm, driven for both a question answer and a
 * permission decision) and engine/session.ts's `onPermissionModeSet` gate
 * callback (`permissionMode = mode; pushPermissionMode();`) were all zero-hit
 * -- nothing in this suite ever resolves a canUseTool ask THROUGH the engine's
 * own UpdateAgent RPC (permission-gate.test.ts exercises the gate directly, in
 * isolation, and its own onPermissionModeSet is a throwaway test callback).
 */
describe("a standing grant's mode change, delivered through UpdateAgent", () => {
  it("updates the session's own permission mode and pushes the change", async () => {
    const h = harness();
    await started(h);
    const spec = h.queries[0]?.spec;
    if (spec === undefined) throw new Error("no query");
    const stream = h.engine.pushes.subscribe()[Symbol.asyncIterator]();
    const pushed: string[] = [];
    const reading = (async () => {
      for (;;) {
        const step = await stream.next();
        if (step.done === true) return;
        const update = step.value.update;
        if (update.case === "permissionModeChanged") {
          pushed.push(update.value.permissionMode?.mode.case ?? "");
        }
      }
    })();

    const pending = spec.canUseTool("Bash", {}, {
      signal: new AbortController().signal,
      toolUseID: "toolu_1",
      requestId: "req_1",
      suggestions: [{ type: "setMode", destination: "session", mode: "acceptEdits" }],
    } as never);
    await Promise.resolve();

    const response = await h.engine.updateAgent(
      create(shimv1.UpdateAgentRequestSchema, {
        input: create(conversationv1.AgentInputSchema, {
          input: {
            case: "answer",
            value: create(conversationv1.AgentAnswerSchema, {
              answer: {
                case: "permissionDecision",
                value: create(conversationv1.AgentPermissionDecisionSchema, {
                  ask: create(conversationv1.AgentPermissionIdSchema, { value: "toolu_1" }),
                  decision: {
                    case: "allowed",
                    value: create(conversationv1.AgentPermissionAllowedSchema, {
                      scope: {
                        case: "standing",
                        value: create(conversationv1.AgentPermissionAllowedStandingSchema, {
                          standing: toStanding([
                            { type: "setMode", destination: "session", mode: "acceptEdits" },
                          ]),
                        }),
                      },
                    }),
                  },
                }),
              },
            }),
          },
        }),
      }),
    );
    await pending;
    await h.engine.killSession(create(shimv1.KillSessionRequestSchema, { force: true }));
    await reading;

    expect(response.result.case).toBe("success");
    expect(pushed).toEqual(["default", "acceptEdits"]);
  });

  it("answerOutcome refuses noOpenAsk when the ask id names nothing open", async () => {
    const h = harness();
    await started(h);

    const response = await h.engine.updateAgent(
      create(shimv1.UpdateAgentRequestSchema, {
        input: create(conversationv1.AgentInputSchema, {
          input: {
            case: "answer",
            value: create(conversationv1.AgentAnswerSchema, {
              answer: {
                case: "permissionDecision",
                value: create(conversationv1.AgentPermissionDecisionSchema, {
                  ask: create(conversationv1.AgentPermissionIdSchema, { value: "nope" }),
                  decision: {
                    case: "denied",
                    value: create(conversationv1.AgentPermissionDeniedByUserSchema, { message: "no" }),
                  },
                }),
              },
            }),
          },
        }),
      }),
    );

    expect(
      response.result.case === "failure" ? response.result.value.kind.case : undefined,
    ).toBe("noOpenAsk");
  });
});

/**
 * The account facts a scheduled beat pushes: mcpUpdate (pushMcpServerStatus,
 * called once during StartSession) and usageWindow/optional
 * (accountUsageUpdate, called on the account-usage interval's own beat).
 * Neither had ever run: no test in this suite configures the scripted
 * query's mcpServerStatus()/usage_EXPERIMENTAL... answers, or fires the
 * account-usage interval ManualScheduler registers second (after the
 * keepalive cadence).
 */
describe("mcp server status, pushed at StartSession", () => {
  it("pushes one mcpServer update per declared server, each its own health arm", async () => {
    const h = harness({
      mcp: [
        { name: "docs", status: "connected" },
        { name: "search", status: "failed", error: "auth expired" },
      ] as McpServerStatusLike[],
    });
    const stream = h.engine.pushes.subscribe()[Symbol.asyncIterator]();
    const seen: { name: string; health: string }[] = [];
    const reading = (async () => {
      for (;;) {
        const step = await stream.next();
        if (step.done === true) return;
        const update = step.value.update;
        if (update.case === "mcpServer") {
          seen.push({ name: update.value.name, health: update.value.health.case ?? "" });
        }
      }
    })();

    await started(h);
    await h.engine.killSession(create(shimv1.KillSessionRequestSchema, { force: true }));
    await reading;

    expect(seen).toEqual([
      { name: "docs", health: "connected" },
      { name: "search", health: "failed" },
    ]);
  });
});

describe("account usage, pushed on the account-usage interval", () => {
  it("reports the five-hour window and echoes the offered seven-day window", async () => {
    const h = harness({
      accountUsage: {
        session: {
          total_cost_usd: 0,
          total_api_duration_ms: 0,
          total_duration_ms: 0,
          total_lines_added: 0,
          total_lines_removed: 0,
          model_usage: {},
        },
        subscription_type: "max",
        rate_limits_available: true,
        rate_limits: {
          five_hour: { utilization: 10, resets_at: "2026-01-01T00:00:00.000Z" },
          seven_day: { utilization: 20, resets_at: "2026-01-02T00:00:00.000Z" },
          seven_day_oauth_apps: null,
          seven_day_opus: null,
          seven_day_sonnet: null,
          model_scoped: [],
        },
        behaviors: null,
      },
    });
    const stream = h.engine.pushes.subscribe()[Symbol.asyncIterator]();
    let seen: conversationv1.SessionAccountUsage | undefined;
    const reading = (async () => {
      for (;;) {
        const step = await stream.next();
        if (step.done === true) return;
        const update = step.value.update;
        if (update.case === "accountUsage") {
          seen = update.value;
          return;
        }
      }
    })();

    await started(h);
    h.scheduler.fire(1);
    await reading;

    expect(seen?.outcome.case).toBe("available");
    const available = seen?.outcome.value as conversationv1.SessionAccountUsageAvailable | undefined;
    expect(available?.fiveHour?.utilizationPercent).toBe(10);
    expect(available?.sevenDay?.utilizationPercent).toBe(20);
    expect(available?.sevenDayOpus).toBeUndefined();
  });
});

/**
 * reportStoreUnreachable (ReadHistory's store_unavailable path) and
 * concludeStoppedRuns (StopBash's write of the interrupted terminal).
 */
describe("ReadHistory reports a store outage", () => {
  it("calls reportStoreUnreachable when the store answers store_unavailable", async () => {
    const h = harness();
    await started(h);
    h.persistence.readError = new PersistenceError(
      "store_unavailable",
      "the store is down",
    );
    const before = h.engine.pushes.faultCount;

    const response = await h.engine.readHistory(
      create(shimv1.ReadHistoryRequestSchema, {
        pageSize: 5,
        position: {
          case: "after",
          value: create(conversationv1.HistoryPointerSchema, { value: "p-1" }),
        },
      }),
    );

    expect(
      response.result.case === "failure" ? response.result.value.kind.case : undefined,
    ).toBe("storeUnavailable");
    expect(h.engine.pushes.faultCount).toBe(before + 1);
  });
});

describe("StopBash writes the interrupted terminal (concludeStoppedRuns)", () => {
  it("closes a live shell run the shim itself stopped", async () => {
    const h = harness();
    await started(h);
    await h.engine.onSdkMessage({
      type: "system",
      subtype: "task_started",
      task_id: "b01",
      tool_use_id: "toolu_1",
      task_type: "local_bash",
      description: "sleep 600",
      uuid: "00000000-0000-4000-8000-000000000000",
      session_id: h.queries[0]?.spec.binding.kind === "fresh" ? h.queries[0].spec.binding.sessionId : "",
    } as never);

    const response = await h.engine.stopBash(
      create(shimv1.StopBashRequestSchema, {
        work: create(conversationv1.DetachedWorkIdSchema, { value: "toolu_1" }),
      }),
    );

    expect(response.result.case).toBe("success");
    const wrote = h.persistence.buffered.some((entry) => {
      if (entry.item.kind !== "bash_run") return false;
      const result = entry.item.frame.result;
      if (result.case !== "success") return false;
      return result.value.outcome.case === "interrupted";
    });
    expect(wrote).toBe(true);
  });
});

/**
 * A gated call raised INSIDE a subagent (engine/session.ts's `agentFor`).
 *
 * `canUseTool`'s `agentID` is the agent TASK id, verbatim -- the same string
 * `task_started.task_id` states for the `local_agent` task (the capture corpus
 * settles it: testdata/captures/ctrl-b-detach-of-foreground-subagent). The
 * session ANNOUNCES the subagent under the spawning call's `tool_use_id`
 * instead, so before this every permission and question raised under a
 * subagent landed on the main agent's book with "the vendor raised an ask
 * under an agent this session never announced".
 */
describe("an ask raised under a subagent's vendor agent id", () => {
  /** Announce a live detached agent task spawned by `toolu_spawn`. */
  const spawnSubagent = async (h: Harness): Promise<void> => {
    await h.engine.onSdkMessage({
      type: "system",
      subtype: "task_started",
      task_id: "a01",
      tool_use_id: "toolu_spawn",
      task_type: "agent",
      subagent_type: "general-purpose",
      description: "look something up",
      uuid: "00000000-0000-4000-8000-00000000000a",
      session_id: "s",
    } as never);
  };

  /** The book the gate's permission frame landed on. */
  const permissionBook = (h: Harness): string | undefined => {
    for (const entry of h.persistence.buffered) {
      if (entry.item.kind !== "frame") continue;
      const result = entry.item.frame.result;
      if (result.case !== "update") continue;
      if (result.value.update.case !== "permission") continue;
      return entry.agentId?.value;
    }
    return undefined;
  };

  it("books an ask named by the vendor task id under the live subagent", async () => {
    const h = harness();
    await started(h);
    await spawnSubagent(h);
    const spec = h.queries[0]?.spec;
    if (spec === undefined) throw new Error("no query");

    const pending = spec.canUseTool("Bash", {}, {
      signal: new AbortController().signal,
      toolUseID: "toolu_inner",
      agentID: "a01",
      requestId: "req_1",
    });
    await Promise.resolve();
    await h.engine.standDown("SIGTERM");
    await pending;

    expect(permissionBook(h)).toBe("toolu_spawn");
  });

  it("books an ask named by the spawning call under the live subagent", async () => {
    const h = harness();
    await started(h);
    await spawnSubagent(h);
    const spec = h.queries[0]?.spec;
    if (spec === undefined) throw new Error("no query");

    const pending = spec.canUseTool("Bash", {}, {
      signal: new AbortController().signal,
      toolUseID: "toolu_inner",
      agentID: "toolu_spawn",
      requestId: "req_1",
    });
    await Promise.resolve();
    await h.engine.standDown("SIGTERM");
    await pending;

    expect(permissionBook(h)).toBe("toolu_spawn");
  });

  it("falls back to the main agent once the subagent has concluded", async () => {
    const h = harness();
    await started(h);
    await spawnSubagent(h);
    await h.engine.onSdkMessage({
      type: "system",
      subtype: "task_notification",
      task_id: "a01",
      status: "completed",
      output_file: "",
      summary: "",
      uuid: "00000000-0000-4000-8000-00000000000b",
      session_id: "s",
    } as never);
    const spec = h.queries[0]?.spec;
    if (spec === undefined) throw new Error("no query");

    const pending = spec.canUseTool("Bash", {}, {
      signal: new AbortController().signal,
      toolUseID: "toolu_inner",
      agentID: "a01",
      requestId: "req_1",
    });
    await Promise.resolve();
    await h.engine.standDown("SIGTERM");
    await pending;

    const sessionId =
      h.queries[0]?.spec.binding.kind === "fresh" ? h.queries[0].spec.binding.sessionId : "";
    expect(permissionBook(h)).toBe(mainAgentId(sessionId).value);
  });
});

/**
 * Settledness for an item with NO `result` oneof (engine/session.ts's
 * `noteForegroundUnits`).
 *
 * `AgentTaskAct` and its siblings carry an `act` rather than a lifecycle: the
 * act IS the whole unit. Deriving settledness from a `result` they do not have
 * left them in flight forever, and `DetachForeground` then answered
 * `not_detachable` for a unit that had plainly concluded.
 */
describe("a foreground unit whose item has no lifecycle", () => {
  it("is settled the moment its act is recorded", async () => {
    const h = harness();
    await started(h);
    h.fold.entriesFor = (message) =>
      message.type === "assistant"
        ? [
            {
              agentId: mainAgentId("vendor-session"),
              upsertKey: "k",
              source: { producer: "p", vendorUuid: "u", arm: "task_act" } as never,
              keepalive: false,
              turn: undefined,
              item: {
                kind: "frame",
                frame: create(conversationv1.AgentFrameSchema, {
                  result: {
                    case: "update",
                    value: create(conversationv1.AgentUpdateSchema, {
                      update: {
                        case: "activity",
                        value: create(conversationv1.AgentActivitySchema, {
                          activityId: create(conversationv1.AgentActivityIdSchema, {
                            value: "toolu_task",
                          }),
                          item: {
                            case: "taskAct",
                            value: create(conversationv1.AgentTaskActSchema, {
                              act: { case: "created", value: create(conversationv1.AgentTaskCreatedSchema, {}) },
                            }),
                          },
                        }),
                      },
                    }),
                  },
                }),
              },
            },
          ]
        : [];
    await h.engine.onSdkMessage({
      type: "assistant",
      uuid: "00000000-0000-4000-8000-00000000000c",
      session_id: "s",
      message: { id: "msg_1", role: "assistant", content: [] },
    } as never);

    const response = await h.engine.detachForeground(
      create(shimv1.DetachForegroundRequestSchema, {
        unit: create(conversationv1.AgentActivityIdSchema, { value: "toolu_task" }),
      }),
    );

    expect(
      response.result.case === "failure" ? response.result.value.kind.case : undefined,
    ).toBe("alreadyConcluded");
  });
});


/**
 * The IDE-diagnostics adjacency join (engine/session.ts's `lastChange`).
 *
 * The vendor's `diagnostics` attachment carries no tool id, so
 * convert/attachments.ts joins it to the change it concerns by the one
 * remembered write-or-edit unit -- and nothing assigned it, so every
 * diagnostics record fell to "IDE diagnostics arrived with no preceding write
 * or edit" and landed as residue.
 *
 * The remembered value carries the KIND as well as the id, because the report
 * lands on that unit's own arm and nothing else states which arm that is.
 */
describe("the last change the fold context carries", () => {
  /** Fold one activity of the given kind, then a later message that reads the context. */
  async function foldOneChange(
    kind: "write" | "edit",
    activityId: string,
  ): Promise<ReturnType<typeof harness>> {
    const h = harness();
    await started(h);
    h.fold.entriesFor = (message) =>
      message.type === "assistant"
        ? [
            {
              agentId: mainAgentId("vendor-session"),
              upsertKey: "k",
              source: { producer: "p", vendorUuid: "u", arm: kind } as never,
              keepalive: false,
              turn: undefined,
              item: {
                kind: "frame",
                frame: create(conversationv1.AgentFrameSchema, {
                  result: {
                    case: "update",
                    value: create(conversationv1.AgentUpdateSchema, {
                      update: {
                        case: "activity",
                        value: create(conversationv1.AgentActivitySchema, {
                          activityId: create(conversationv1.AgentActivityIdSchema, {
                            value: activityId,
                          }),
                          item:
                            kind === "edit"
                              ? { case: "edit", value: create(conversationv1.AgentEditSchema, {}) }
                              : { case: "write", value: create(conversationv1.AgentWriteSchema, {}) },
                        }),
                      },
                    }),
                  },
                }),
              },
            },
          ]
        : [];
    await h.engine.onSdkMessage({
      type: "assistant",
      uuid: "00000000-0000-4000-8000-00000000000d",
      session_id: "s",
      message: { id: "msg_1", role: "assistant", content: [] },
    } as never);

    // The attachment is a LATER message, which is the whole point of the join.
    await h.engine.onSdkMessage({
      type: "user",
      uuid: "00000000-0000-4000-8000-00000000000e",
      session_id: "s",
      message: { role: "user", content: [] },
    } as never);
    return h;
  }

  it("names the edit once one has been folded", async () => {
    const h = await foldOneChange("edit", "toolu_edit");

    expect(h.fold.contexts.at(-1)?.lastChange?.unit.value).toBe("toolu_edit");
  });

  it("states that an edit was an edit, so the report rides the edit arm", async () => {
    const h = await foldOneChange("edit", "toolu_edit");

    expect(h.fold.contexts.at(-1)?.lastChange?.kind).toBe("edit");
  });

  it("names the write once one has been folded", async () => {
    const h = await foldOneChange("write", "toolu_write");

    expect(h.fold.contexts.at(-1)?.lastChange?.unit.value).toBe("toolu_write");
  });

  it("states that a write was a write, so the report rides the write arm", async () => {
    const h = await foldOneChange("write", "toolu_write");

    expect(h.fold.contexts.at(-1)?.lastChange?.kind).toBe("write");
  });
});

describe("the vendor request the turn runs under", () => {
  /** An assistant message, with or without the vendor's request identity. */
  const assistant = (requestId?: string): never =>
    ({
      type: "assistant",
      uuid: "u-request-id",
      session_id: "s",
      parent_tool_use_id: null,
      message: { model: "claude-opus-5", content: [] },
      ...(requestId === undefined ? {} : { request_id: requestId }),
    }) as never;

  /** The request_id the logger stamps right now, undefined when it stamps none. */
  function stampedRequestId(): string | undefined {
    const [record] = logRecordsDuring(() => bindLog({ operation: "shim.test.request-id" }).debug({}, "probe"));
    return record.request_id as string | undefined;
  }

  beforeEach(() => {
    // The logger is one process-wide singleton, so a previous turn's stamp is
    // dropped the way the end of that turn drops it.
    clearRequestId();
  });

  it("stamps the request the assistant message revealed", async () => {
    const h = harness();
    await started(h);

    await h.engine.onSdkMessage(assistant("req_revealed"));

    expect(stampedRequestId()).toBe("req_revealed");
  });

  it("stamps nothing when the assistant message names no request", async () => {
    const h = harness();
    await started(h);

    await h.engine.onSdkMessage(assistant());

    expect(stampedRequestId()).toBeUndefined();
  });

  it("drops the request id at the end of the turn that revealed it", async () => {
    const h = harness();
    await started(h);
    await h.engine.onSdkMessage(assistant("req_ends_with_the_turn"));

    await h.engine.onSdkMessage(resultMessage());

    expect(stampedRequestId()).toBeUndefined();
  });
});

/**
 * Every push the engine produced while `act` ran, narrowed by `pick`.
 *
 * The same shape the model and fast-mode suites above use: subscribe first, do
 * the thing, then stand the session down so the stream ends and the reader can
 * be awaited rather than raced.
 */
async function pushedUpdates<T>(
  h: Harness,
  pick: (update: conversationv1.SessionUpdate["update"]) => T | undefined,
  act: () => Promise<void>,
): Promise<T[]> {
  const stream = h.engine.pushes.subscribe()[Symbol.asyncIterator]();
  const seen: T[] = [];
  const reading = (async () => {
    for (;;) {
      const step = await stream.next();
      if (step.done === true) return;
      const got = pick(step.value.update);
      if (got !== undefined) seen.push(got);
    }
  })();
  await act();
  await h.engine.standDown("collected");
  await reading;
  return seen;
}

describe("the vendor's own title for the conversation", () => {
  /** Every title the engine states while starting a resume over LINES. */
  async function titlesPushed(lines: unknown[]): Promise<string[]> {
    const h = harness({ nowMs: 1_000_100 });
    writeTranscript(h.configDir, h.cwd, "resume-1", lines);
    return pushedUpdates(
      h,
      (update) => (update.case === "title" ? update.value.text : undefined),
      async () => {
        const pending = h.engine.startSession(resumeRequest("resume-1"));
        (await untilQuery(h, 0)).query.emit(initMessage({ sessionId: "resume-1" }));
        await pending;
      },
    );
  }

  it("states the title the transcript already holds", async () => {
    const titles = await titlesPushed([
      assistantLine(),
      { type: "ai-title", aiTitle: "Add SPC j keybinding support", sessionId: "resume-1" },
    ]);

    expect(titles).toEqual(["Add SPC j keybinding support"]);
  });

  it("states nothing when the vendor has written no title", async () => {
    const titles = await titlesPushed([assistantLine()]);

    expect(titles).toEqual([]);
  });
});

describe("the context usage the vendor states, mapped field by field", () => {
  /** Every field of the vendor's answer populated, so each mapping is testable. */
  function fullUsage(overrides: Partial<ContextUsageLike> = {}): ContextUsageLike {
    return {
      categories: [
        { name: "messages", tokens: 10, color: "#111", isDeferred: true, kind: "deferred" },
        { name: "tools", tokens: 20, color: "#222", kind: "used" },
      ],
      totalTokens: 100,
      maxTokens: 200,
      rawMaxTokens: 300,
      percentage: 50,
      gridRows: [],
      model: "claude-opus-5",
      memoryFiles: [{ path: "/ws/CLAUDE.md", type: "project", tokens: 7 }],
      mcpTools: [{ name: "search", serverName: "docs", tokens: 9, isLoaded: true }],
      deferredBuiltinTools: [{ name: "WebFetch", tokens: 3, isLoaded: false }],
      systemTools: [{ name: "Bash", tokens: 4 }],
      systemPromptSections: [{ name: "identity", tokens: 5 }],
      agents: [{ agentType: "Explore", source: "builtin", tokens: 6 }],
      slashCommands: { totalCommands: 12, includedCommands: 8, tokens: 40 },
      skills: {
        totalSkills: 3,
        includedSkills: 2,
        tokens: 30,
        skillFrontmatter: [{ name: "graphify", source: "user", tokens: 11 }],
      },
      autoCompactThreshold: 190,
      isAutoCompactEnabled: true,
      messageBreakdown: {
        toolCallTokens: 1,
        toolResultTokens: 2,
        attachmentTokens: 3,
        assistantMessageTokens: 4,
        userMessageTokens: 5,
        redirectedContextTokens: 6,
        unattributedTokens: 7,
        toolCallsByType: [{ name: "Bash", callTokens: 8, resultTokens: 9 }],
        attachmentsByType: [{ name: "image", tokens: 10 }],
      },
      apiUsage: {
        input_tokens: 21,
        output_tokens: 22,
        cache_creation_input_tokens: 23,
        cache_read_input_tokens: 24,
      },
      ...overrides,
    };
  }

  /** Start a session whose vendor answers `getContextUsage` with `usage`. */
  async function usagePushedFor(usage: ContextUsageLike): Promise<conversationv1.SessionContextUsage> {
    const h = harness({ onQueryCreated: (query) => (query.contextUsage = usage) });
    const seen = await pushedUpdates(
      h,
      (update) => (update.case === "contextUsage" ? update.value : undefined),
      async () => {
        await started(h);
      },
    );
    const first = seen[0];
    if (first === undefined) throw new Error("the engine pushed no context usage");
    return first;
  }

  it("carries each category the vendor named", async () => {
    const usage = await usagePushedFor(fullUsage());

    expect(usage.categories.map((category) => category.label)).toEqual(["messages", "tools"]);
  });

  it("states a category the vendor marked deferred", async () => {
    const usage = await usagePushedFor(fullUsage());

    expect(usage.categories[0]?.isDeferred).toBe(true);
  });

  it("leaves a category the vendor said nothing about UNSTATED, not false", async () => {
    const usage = await usagePushedFor(fullUsage());

    expect(usage.categories[1]?.isDeferred).toBeUndefined();
  });

  it("ROUNDS a fractional figure rather than losing the whole push", async () => {
    // `BigInt()` throws outright on a non-integer, which would turn one
    // fractional vendor field into a lost context-usage push and a fault.
    const usage = await usagePushedFor(fullUsage({ percentage: 84.6 }));

    expect(usage.percentage).toBe(85n);
  });

  it("carries the memory files the context holds", async () => {
    const usage = await usagePushedFor(fullUsage());

    expect(usage.memoryFiles.map((file) => [file.path, file.tokens])).toEqual([["/ws/CLAUDE.md", 7n]]);
  });

  it("carries the mcp tools and whether each is loaded", async () => {
    const usage = await usagePushedFor(fullUsage());

    expect(usage.mcpTools.map((tool) => [tool.name, tool.serverName, tool.isLoaded])).toEqual([
      ["search", "docs", true],
    ]);
  });

  it("carries the deferred builtin tools", async () => {
    const usage = await usagePushedFor(fullUsage());

    expect(usage.deferredBuiltinTools.map((tool) => [tool.name, tool.isLoaded])).toEqual([
      ["WebFetch", false],
    ]);
  });

  it("carries the system tools", async () => {
    const usage = await usagePushedFor(fullUsage());

    expect(usage.systemTools.map((tool) => [tool.name, tool.tokens])).toEqual([["Bash", 4n]]);
  });

  it("carries the system prompt sections", async () => {
    const usage = await usagePushedFor(fullUsage());

    expect(usage.systemPromptSections.map((section) => section.name)).toEqual(["identity"]);
  });

  it("carries the agent definitions in context", async () => {
    const usage = await usagePushedFor(fullUsage());

    expect(usage.agents.map((agent) => [agent.agentType, agent.source])).toEqual([
      ["Explore", "builtin"],
    ]);
  });

  it("states the slash-command block when the vendor declared one", async () => {
    const usage = await usagePushedFor(fullUsage());

    expect(usage.slashCommands?.includedCommands).toBe(8n);
  });

  it("states the skills block, frontmatter included", async () => {
    const usage = await usagePushedFor(fullUsage());

    expect(usage.skills?.skillFrontmatter.map((skill) => skill.name)).toEqual(["graphify"]);
  });

  it("states the auto-compact threshold the vendor declared", async () => {
    const usage = await usagePushedFor(fullUsage());

    expect(usage.autoCompactThreshold).toBe(190n);
  });

  it("states the per-message breakdown the vendor declared", async () => {
    const usage = await usagePushedFor(fullUsage());

    expect(usage.messageBreakdown?.unattributedTokens).toBe(7n);
  });

  it("carries the breakdown's per-tool call and result figures", async () => {
    const usage = await usagePushedFor(fullUsage());

    expect(
      usage.messageBreakdown?.toolCallsByType.map((entry) => [entry.name, entry.callTokens, entry.resultTokens]),
    ).toEqual([["Bash", 8n, 9n]]);
  });

  it("carries the breakdown's per-attachment-type figures", async () => {
    const usage = await usagePushedFor(fullUsage());

    expect(usage.messageBreakdown?.attachmentsByType.map((entry) => entry.name)).toEqual(["image"]);
  });

  it("states the API usage the vendor reported for the last request", async () => {
    const usage = await usagePushedFor(fullUsage());

    expect(usage.apiUsage?.cacheReadInputTokens).toBe(24n);
  });

  it("states NO api usage when the vendor reported none", async () => {
    const usage = await usagePushedFor(fullUsage({ apiUsage: null }));

    expect(usage.apiUsage).toBeUndefined();
  });

  it("reports a getContextUsage failure as a vendor_query_failed fault", async () => {
    const h = harness({
      onQueryCreated: (query) => {
        query.getContextUsage = () => Promise.reject(new Error("the vendor will not answer"));
      },
    });
    const faults = await pushedUpdates(
      h,
      (update) =>
        update.case === "diagnostics" ? update.value.health.case : undefined,
      async () => {
        await started(h);
      },
    );

    expect(faults).toContain("unhealthy");
  });
});

describe("the account's rate-limit windows", () => {
  /** The vendor's usage answer, with only the rate-limit half varied. */
  function usage(overrides: Partial<AccountUsageLike>): AccountUsageLike {
    return {
      session: {
        total_cost_usd: 0,
        total_api_duration_ms: 0,
        total_duration_ms: 0,
        total_lines_added: 0,
        total_lines_removed: 0,
        model_usage: {},
      },
      subscription_type: "max",
      rate_limits_available: true,
      rate_limits: null,
      behaviors: null,
      ...overrides,
    };
  }

  /** The account-usage push a session with this vendor answer produces. */
  async function accountUsagePushed(answer: AccountUsageLike): Promise<conversationv1.SessionAccountUsage> {
    const h = harness({ accountUsage: answer });
    const seen = await pushedUpdates(
      h,
      (update) => (update.case === "accountUsage" ? update.value : undefined),
      async () => {
        await started(h);
      },
    );
    const first = seen[0];
    if (first === undefined) throw new Error("the engine pushed no account usage");
    return first;
  }

  /** The unavailable reason on a push that carries one. */
  function unavailableReason(pushed: conversationv1.SessionAccountUsage): string | undefined {
    return pushed.outcome.case === "unavailable" ? (pushed.outcome.value.reason.case ?? "") : undefined;
  }

  it("reports service_unavailable when limits are claimed and none are given", async () => {
    // The service said it had limits and produced none: still the service
    // failing to answer, not a shape with a window missing from it.
    const pushed = await accountUsagePushed(usage({ rate_limits: null }));

    expect(unavailableReason(pushed)).toBe("serviceUnavailable");
  });

  it("reports window_unavailable when the answer carries no five-hour window", async () => {
    const pushed = await accountUsagePushed(
      usage({ rate_limits: { five_hour: null } }),
    );

    expect(unavailableReason(pushed)).toBe("windowUnavailable");
  });

  it("reports utilization_unavailable when the five-hour window states no utilization", async () => {
    const pushed = await accountUsagePushed(
      usage({
        rate_limits: { five_hour: { utilization: null, resets_at: "2026-01-01T00:00:00.000Z" } },
      }),
    );

    expect(unavailableReason(pushed)).toBe("utilizationUnavailable");
  });

  it("reports utilization_unavailable when the window's reset time cannot be read", async () => {
    const pushed = await accountUsagePushed(
      usage({
        rate_limits: { five_hour: { utilization: 10, resets_at: "not a timestamp" } },
      }),
    );

    expect(unavailableReason(pushed)).toBe("utilizationUnavailable");
  });

  it("echoes each per-model window the vendor offered", async () => {
    const pushed = await accountUsagePushed(
      usage({
        rate_limits: {
          five_hour: { utilization: 10, resets_at: "2026-01-01T00:00:00.000Z" },
          model_scoped: [
            { display_name: "Fable", utilization: 30, resets_at: "2026-01-03T00:00:00.000Z" },
          ],
        },
      }),
    );
    const available =
      pushed.outcome.case === "available" ? pushed.outcome.value : undefined;

    expect(available?.modelScoped.map((scoped) => [scoped.model?.name, scoped.window?.utilizationPercent])).toEqual(
      [["Fable", 30]],
    );
  });

  it("DROPS a per-model window the vendor could not state", async () => {
    const pushed = await accountUsagePushed(
      usage({
        rate_limits: {
          five_hour: { utilization: 10, resets_at: "2026-01-01T00:00:00.000Z" },
          model_scoped: [{ display_name: "Fable", utilization: null, resets_at: null }],
        },
      }),
    );
    const available =
      pushed.outcome.case === "available" ? pushed.outcome.value : undefined;

    expect(available?.modelScoped).toEqual([]);
  });

  it("reports a sampling failure when the vendor's usage verb throws", async () => {
    // "We could not ask" is a different fact to a consumer than "the service is
    // down", so it gets its own reason rather than an absent field.
    const h = harness({
      onQueryCreated: (query) => {
        query.usage_EXPERIMENTAL_MAY_CHANGE_DO_NOT_RELY_ON_THIS_API_YET = () =>
          Promise.reject(new Error("the usage endpoint is down"));
      },
    });
    const seen = await pushedUpdates(
      h,
      (update) => (update.case === "accountUsage" ? update.value : undefined),
      async () => {
        await started(h);
      },
    );
    const outcome = seen[0]?.outcome;

    expect(
      outcome?.case === "unavailable" && outcome.value.reason.case === "samplingFailure"
        ? outcome.value.reason.value.cause
        : undefined,
    ).toBe("the usage endpoint is down");
  });
});

describe("mcp server health, arm by arm", () => {
  /** The health arm the engine pushed for one declared server. */
  async function healthFor(status: McpServerStatusLike["status"]): Promise<string | undefined> {
    const h = harness({ mcp: [{ name: "docs", status }] as McpServerStatusLike[] });
    const seen = await pushedUpdates(
      h,
      (update) => (update.case === "mcpServer" ? (update.value.health.case ?? "") : undefined),
      async () => {
        await started(h);
      },
    );
    return seen[0];
  }

  it("states a server awaiting authorization as needs_auth", async () => {
    expect(await healthFor("needs-auth")).toBe("needsAuth");
  });

  it("states a server still connecting as pending", async () => {
    expect(await healthFor("pending")).toBe("pending");
  });

  it("states a server the user turned off as disabled", async () => {
    expect(await healthFor("disabled")).toBe("disabled");
  });

  it("reports an mcpServerStatus failure as a session fault", async () => {
    const h = harness({
      onQueryCreated: (query) => {
        query.mcpServerStatus = () => Promise.reject(new Error("the vendor cannot probe its servers"));
      },
    });
    const before = h.engine.pushes.faultCount;
    await started(h);

    expect(h.engine.pushes.faultCount).toBeGreaterThan(before);
  });
});

describe("the model catalog's capabilities", () => {
  /** The catalog a session reports when the vendor declares `models`. */
  async function catalogFor(models: ModelInfoLike[]): Promise<conversationv1.ModelOption[]> {
    const h = harness({ onQueryCreated: (query) => (query.models = models) });
    const response = await started(h);
    return response.result.case === "success" ? (response.result.value.session?.modelCatalog ?? []) : [];
  }

  it("states the wire model an alias row resolves to", async () => {
    const catalog = await catalogFor([
      { value: "sonnet", displayName: "Sonnet", description: "alias", resolvedModel: "claude-sonnet-5" },
    ]);

    expect(catalog[0]?.capabilities?.resolvedModel?.name).toBe("claude-sonnet-5");
  });

  it("states effort UNSUPPORTED for a model that declares other capabilities but no effort", async () => {
    const catalog = await catalogFor([
      { value: "m", displayName: "M", description: "d", supportsAdaptiveThinking: true },
    ]);

    expect(catalog[0]?.capabilities?.effortSupport.case).toBe("effortUnsupported");
  });

  it("maps an effort level the shim does not know to UNSPECIFIED", async () => {
    const catalog = await catalogFor([
      {
        value: "m",
        displayName: "M",
        description: "d",
        supportsEffort: true,
        supportedEffortLevels: ["ludicrous"] as unknown as ModelInfoLike["supportedEffortLevels"],
      },
    ]);
    const support = catalog[0]?.capabilities?.effortSupport;

    expect(support?.case === "effortSupported" ? support.value.levels : undefined).toEqual([
      conversationv1.AgentEffortLevel.UNSPECIFIED,
    ]);
  });

  it("omits a catalog row whose value is the synthetic marker", async () => {
    const catalog = await catalogFor([
      { value: SYNTHETIC_MODEL, displayName: "Default", description: "the CLI's own pick" },
      { value: "opus", displayName: "Opus", description: "a real model" },
    ]);

    expect(catalog.map((option) => option.model?.name)).toEqual(["opus"]);
  });

  it("omits a catalog row whose value is empty", async () => {
    const catalog = await catalogFor([
      { value: "", displayName: "Default", description: "names no model" },
      { value: "opus", displayName: "Opus", description: "a real model" },
    ]);

    expect(catalog.map((option) => option.model?.name)).toEqual(["opus"]);
  });
});

/**
 * The fold's own rows, and what the engine does with them.
 *
 * A `PersistEntry` the fold produced, wrapped so each test states only the row
 * it cares about.
 */
function foldEntry(item: PersistEntry["item"], arm: string): PersistEntry {
  return {
    agentId: mainAgentId("vendor-session"),
    upsertKey: `k-${arm}`,
    source: { vendorUuid: `u-${arm}`, discriminator: arm },
    keepalive: false,
    turn: undefined,
    item,
  };
}

/** One assistant message, so a fold has something to answer. */
function assistantMessage(uuid: string): SdkMessage {
  return {
    type: "assistant",
    uuid,
    session_id: "s",
    parent_tool_use_id: null,
    message: { id: "msg_1", role: "assistant", content: [] },
  } as never;
}

/** The vendor's own compaction divider, which the anchor must not cross. */
function compactBoundaryMessage(uuid: string): SdkMessage {
  return {
    type: "system",
    subtype: "compact_boundary",
    uuid,
    session_id: "s",
    compact_metadata: { trigger: "auto", pre_tokens: 100 },
  } as never;
}

/** The vendor retiring the conversation, which the anchor must not cross. */
function conversationResetMessage(uuid: string): SdkMessage {
  return {
    type: "conversation_reset",
    uuid,
    session_id: "s",
    new_conversation_id: "00000000-0000-4000-8000-000000000abc",
  } as never;
}

/** Open a real turn, without waiting on anything it produces. */
async function realPrompt(h: Harness, turnId: string): Promise<void> {
  await h.engine.startTurn(
    create(shimv1.StartTurnRequestSchema, {
      turn: create(conversationv1.TurnIdSchema, { value: turnId }),
      said: textSaid("go"),
      origin: conversationv1.PromptOrigin.USER_SENT,
      pageSize: 5,
    }),
  );
}

/** A whole real turn: the prompt, the records it produced, and its result. */
async function realTurn(
  h: Harness,
  turnId: string,
  records: SdkMessage[],
  resultUuid = `${turnId}-result-uuid`,
): Promise<void> {
  await realPrompt(h, turnId);
  for (const record of records) await h.engine.onSdkMessage(answering(h, record));
  await h.engine.onSdkMessage(answering(h, resultMessage(resultUuid)));
}

/** A whole keep-alive turn, beaten by the suite's own scheduler. */
async function keepaliveTurn(h: Harness, records: SdkMessage[]): Promise<void> {
  h.scheduler.fire(0);
  await new Promise((resolve) => setImmediate(resolve));
  for (const record of records) await h.engine.onSdkMessage(answering(h, record));
  await h.engine.onSdkMessage(answering(h, resultMessage("keepalive-result-uuid")));
}

/**
 * A reply frame as the vendor stamps it: naming the send it answers — the
 * LATEST send the engine minted a client uuid for, real or keep-alive
 * (sdk.d.ts, `user_message_uuid` / `user_message_uuids`). Every send is
 * stamped (ruled 2026-09-28), so every reply a test means as a send's answer
 * goes through here.
 */
function answering(h: Harness, message: SdkMessage): SdkMessage {
  const send = h.minted.at(-1);
  if (send === undefined) throw new Error("no send was minted a client uuid");
  return { ...message, user_message_uuid: send, user_message_uuids: [send] } as SdkMessage;
}

/** A real StartTurn, left pending: the caller decides when to await it. */
function startDuring(h: Harness, turnId: string, signal?: AbortSignal): Promise<shimv1.StartTurnResponse> {
  return h.engine.startTurn(
    create(shimv1.StartTurnRequestSchema, {
      turn: create(conversationv1.TurnIdSchema, { value: turnId }),
      said: textSaid("go"),
      origin: conversationv1.PromptOrigin.USER_SENT,
      pageSize: 5,
    }),
    signal,
  );
}

/**
 * Whether `promise` settles within a few event-loop turns — never a clock.
 *
 * Every step the engine takes before a StartTurn answers is a microtask or an
 * immediate (the scripted store and query answer in-process), so a start that
 * has not settled after these turns is waiting on something that has not
 * happened.
 */
async function settledSoon(promise: Promise<unknown>): Promise<boolean> {
  let done = false;
  void promise.then(
    () => {
      done = true;
    },
    () => {
      done = true;
    },
  );
  for (let turn = 0; turn < 20 && !done; turn++) await new Promise((resolve) => setImmediate(resolve));
  return done;
}

/** Each send the vendor received, as `keepalive` (it carries a client uuid) or `real`. */
function sendKinds(sends: readonly SdkUserMessage[]): string[] {
  // EVERY send carries a client uuid now (ruled 2026-09-28), so a keep-alive is
  // told by its marker, as the store and sidecar tell it.
  return sends.map((send) =>
    typeof send.message.content === "string" && send.message.content.startsWith(KEEPALIVE_PROMPT_MARKER)
      ? "keepalive"
      : "real",
  );
}

/** Let every queued send reach the drained prompt stream. */
async function drainTurns(): Promise<void> {
  for (let turn = 0; turn < 20; turn++) await new Promise((resolve) => setImmediate(resolve));
}

/** One activity frame row, as the fold produces them. */
function activityEntry(
  activityId: string,
  item: conversationv1.AgentActivity["item"],
  arm: string,
): PersistEntry {
  return foldEntry(
    {
      kind: "frame",
      frame: create(conversationv1.AgentFrameSchema, {
        result: {
          case: "update",
          value: create(conversationv1.AgentUpdateSchema, {
            update: {
              case: "activity",
              value: create(conversationv1.AgentActivitySchema, {
                activityId: create(conversationv1.AgentActivityIdSchema, { value: activityId }),
                item,
              }),
            },
          }),
        },
      }),
    },
    arm,
  );
}

describe("the session updates the fold itself produced", () => {
  it("fans out an arm the engine does not state itself", async () => {
    const h = harness();
    await started(h);
    h.fold.entriesFor = (message) =>
      message.type === "assistant"
        ? [
            foldEntry(
              {
                kind: "session_update",
                update: create(conversationv1.SessionUpdateSchema, {
                  update: {
                    case: "rateLimitStatus",
                    value: create(conversationv1.SessionRateLimitStatusSchema, {}),
                  },
                }),
              },
              "rate_limit_status",
            ),
          ]
        : [];

    const arms = await pushedUpdates(
      h,
      (update) => (update.case === "rateLimitStatus" ? "rateLimitStatus" : undefined),
      async () => {
        await h.engine.onSdkMessage(assistantMessage("00000000-0000-4000-8000-0000000000a1"));
      },
    );

    expect(arms).toEqual(["rateLimitStatus"]);
  });

  it("DROPS a fold duplicate of an arm the engine owns", async () => {
    // One producer per arm, or a consumer sees the same flip twice.
    const h = harness();
    await started(h);
    h.fold.entriesFor = (message) =>
      message.type === "assistant"
        ? [
            foldEntry(
              {
                kind: "session_update",
                update: create(conversationv1.SessionUpdateSchema, {
                  update: {
                    case: "modelChanged",
                    value: create(conversationv1.SessionModelChangedSchema, {
                      effectiveModel: create(conversationv1.AgentModelSchema, {
                        name: "the-fold-said-this",
                      }),
                    }),
                  },
                }),
              },
              "model_changed",
            ),
          ]
        : [];

    const names = await pushedUpdates(
      h,
      (update) => (update.case === "modelChanged" ? update.value.effectiveModel?.name : undefined),
      async () => {
        await h.engine.onSdkMessage(assistantMessage("00000000-0000-4000-8000-0000000000a2"));
      },
    );

    expect(names).not.toContain("the-fold-said-this");
  });
});

describe("the vendor's init when it names no real model", () => {
  it("keeps the model already in effect rather than adopting the synthetic marker", async () => {
    // `<synthetic>` is the CLI's stand-in for "no nameable model"; adopting it
    // would put an unspawnable id in the picker and every later field.
    const h = harness();
    const pending = h.engine.startSession(freshRequest());
    const first = await untilQuery(h, 0);
    first.query.emit(
      initMessage({
        sessionId: first.spec.binding.kind === "fresh" ? first.spec.binding.sessionId : "",
        model: SYNTHETIC_MODEL,
      }),
    );
    const response = await pending;

    expect(
      response.result.case === "success" ? response.result.value.session?.effectiveModel?.name : undefined,
    ).toBe("claude-opus-5");
  });
});

describe("the vendor's own compaction, announced through system:status", () => {
  /** Every context cut this session wrote, newest last. */
  function contextCuts(h: Harness): conversationv1.ContextCut[] {
    return h.persistence.buffered.flatMap((entry) => {
      if (entry.item.kind !== "frame") return [];
      const result = entry.item.frame.result;
      if (result.case !== "update") return [];
      const update = result.value.update;
      return update.case === "contextCut" ? [update.value] : [];
    });
  }

  it("pushes `compacting` when the vendor says it started", async () => {
    const h = harness();
    await started(h);

    const arms = await pushedUpdates(
      h,
      (update) => (update.case === "compacting" ? "compacting" : undefined),
      async () => {
        await h.engine.onSdkMessage({
          type: "system",
          subtype: "status",
          status: "compacting",
          uuid: "00000000-0000-4000-8000-0000000000b1",
          session_id: "s",
        } as never);
      },
    );

    expect(arms).toEqual(["compacting"]);
  });

  it("writes a compaction_failed page line carrying the vendor's own wording", async () => {
    // A failure has no `compact_boundary` record at all, so this is its ONLY
    // producer.
    const h = harness();
    await started(h);

    await h.engine.onSdkMessage({
      type: "system",
      subtype: "status",
      compact_result: "failed",
      compact_error: "the model refused to summarize",
      uuid: "00000000-0000-4000-8000-0000000000b2",
      session_id: "s",
    } as never);

    const cut = contextCuts(h).at(-1);
    expect(cut?.cut.case === "compactionFailed" ? cut.cut.value.error : undefined).toBe(
      "the model refused to summarize",
    );
  });

  it("states its own wording when the vendor named no error", async () => {
    const h = harness();
    await started(h);

    await h.engine.onSdkMessage({
      type: "system",
      subtype: "status",
      compact_result: "failed",
      uuid: "00000000-0000-4000-8000-0000000000b3",
      session_id: "s",
    } as never);

    const cut = contextCuts(h).at(-1);
    expect(cut?.cut.case === "compactionFailed" ? cut.cut.value.error : undefined).toBe(
      "the vendor's compaction failed",
    );
  });
});

describe("what the engine remembers from the fold's own frames", () => {
  it("RELAYS a denial the fold produced into the one memory of denied calls", async () => {
    // The policy and undecidable arms never reach the gate's ask, so the
    // `tool_result` that follows them must still be recognised as a relayed
    // deny rather than a call that ran.
    const h = harness();
    await started(h);
    h.fold.entriesFor = (message) =>
      message.type === "assistant"
        ? [
            foldEntry(
              {
                kind: "frame",
                frame: create(conversationv1.AgentFrameSchema, {
                  result: {
                    case: "update",
                    value: create(conversationv1.AgentUpdateSchema, {
                      update: {
                        case: "permission",
                        value: create(conversationv1.AgentPermissionSchema, {
                          gatedCall: create(conversationv1.AgentActivityIdSchema, {
                            value: "toolu_denied",
                          }),
                          result: {
                            case: "success",
                            value: create(conversationv1.AgentPermissionSuccessSchema, {
                              decision: {
                                case: "denied",
                                value: create(conversationv1.AgentPermissionDeniedSchema, {
                                  by: {
                                    case: "policy",
                                    value: create(
                                      conversationv1.AgentPermissionDeniedByPolicySchema,
                                      {},
                                    ),
                                  },
                                }),
                              },
                            }),
                          },
                        }),
                      },
                    }),
                  },
                }),
              },
              "permission_denied",
            ),
          ]
        : [];

    await h.engine.onSdkMessage(assistantMessage("00000000-0000-4000-8000-0000000000c1"));
    await h.engine.onSdkMessage(assistantMessage("00000000-0000-4000-8000-0000000000c2"));

    expect(h.fold.contexts.at(-1)?.deniedCall("toolu_denied")).toBe(true);
  });

  it("ANNOUNCES the agent a spawn created, so a watch on it can be answered", async () => {
    // The daemon opens its WatchAgent the instant it sees the spawn — before a
    // single row of the child's exists.
    const h = harness();
    await started(h);
    h.fold.entriesFor = (message) =>
      message.type === "assistant"
        ? [
            activityEntry(
              "toolu_spawn",
              {
                case: "subagent",
                value: create(conversationv1.AgentSubagentSchema, {
                  result: {
                    case: "start",
                    value: create(conversationv1.AgentSubagentStartSchema, {
                      createdAgentId: create(conversationv1.AgentIdSchema, { value: "agent-child" }),
                    }),
                  },
                }),
              },
              "subagent_start",
            ),
          ]
        : [];
    await h.engine.onSdkMessage(assistantMessage("00000000-0000-4000-8000-0000000000c3"));

    const iterator = h.engine
      .watchAgent(
        create(shimv1.WatchAgentRequestSchema, {
          target: create(conversationv1.AgentIdSchema, { value: "agent-child" }),
          pageSize: 5,
        }),
      )[Symbol.asyncIterator]();
    const first = await nextPush(iterator);
    await iterator.return?.();

    expect(first.frame.case).toBe("page");
  });

  it("REFUSES a watch on an agent this session never announced", async () => {
    const h = harness();
    await started(h);

    const iterator = h.engine
      .watchAgent(
        create(shimv1.WatchAgentRequestSchema, {
          target: create(conversationv1.AgentIdSchema, { value: "agent-nobody-minted" }),
          pageSize: 5,
        }),
      )[Symbol.asyncIterator]();

    await expect(iterator.next()).rejects.toThrow(/never been announced|no agent by that id/);
  });
});

describe("the live detached table, driven by the vendor's own messages", () => {
  /** `task_started` for one shell run. */
  const taskStarted = (overrides: Record<string, unknown> = {}): SdkMessage =>
    ({
      type: "system",
      subtype: "task_started",
      task_id: "t01",
      tool_use_id: "toolu_run",
      task_type: "local_bash",
      description: "sleep 600",
      uuid: "00000000-0000-4000-8000-0000000000d0",
      session_id: "s",
      ...overrides,
    }) as never;

  /** The command lines of every interrupted shell terminal this session wrote. */
  function interruptedCommands(h: Harness): string[] {
    return h.persistence.buffered.flatMap((entry) => {
      if (entry.item.kind !== "bash_run") return [];
      const result = entry.item.frame.result;
      if (result.case !== "success" || result.value.outcome.case !== "interrupted") return [];
      return [result.value.command?.line ?? ""];
    });
  }

  it("PATCHES a live item from the vendor's task_updated", async () => {
    const h = harness();
    await started(h);
    await h.engine.onSdkMessage(taskStarted());
    await h.engine.onSdkMessage({
      type: "system",
      subtype: "task_updated",
      task_id: "t01",
      patch: { description: "sleep 900" },
      uuid: "00000000-0000-4000-8000-0000000000d1",
      session_id: "s",
    } as never);

    await h.engine.killSession(create(shimv1.KillSessionRequestSchema, { force: true }));

    expect(interruptedCommands(h)).toEqual(["sleep 900"]);
  });

  it("applies the vendor's LEVEL by replacement, retiring what it omits", async () => {
    const h = harness();
    await started(h);
    await h.engine.onSdkMessage(taskStarted());
    await h.engine.onSdkMessage({
      type: "system",
      subtype: "background_tasks_changed",
      tasks: [],
      uuid: "00000000-0000-4000-8000-0000000000d2",
      session_id: "s",
    } as never);

    const response = await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    expect(response.result.case === "success" ? response.result.value.closed?.how.case : undefined).toBe(
      "idle",
    );
  });

  it("tells the fold which call a live task belongs to", async () => {
    const h = harness();
    await started(h);
    await h.engine.onSdkMessage(taskStarted());

    expect(h.fold.contexts.at(-1)?.liveTask("t01")).toEqual({ toolUseId: "toolu_run", taskType: "local_bash" });
  });

  it("tells the fold no kind for a live task whose start stated none", async () => {
    const h = harness();
    await started(h);
    await h.engine.onSdkMessage(taskStarted({ task_type: undefined }));

    expect(h.fold.contexts.at(-1)?.liveTask("t01")).toEqual({ toolUseId: "toolu_run" });
  });

  it("tells the fold nothing for a task id it never saw start", async () => {
    const h = harness();
    await started(h);
    await h.engine.onSdkMessage(taskStarted());

    expect(h.fold.contexts.at(-1)?.liveTask("t99")).toBeUndefined();
  });

  it("writes NO shell terminal for a stopped item that names no originating call", async () => {
    // Tracked for liveness, addressable by nobody: there is no unit to settle,
    // and inventing one would put work on a stream that never announced it.
    const h = harness();
    await started(h);
    await h.engine.onSdkMessage(taskStarted({ tool_use_id: undefined }));

    await h.engine.killSession(create(shimv1.KillSessionRequestSchema, { force: true }));

    expect(interruptedCommands(h)).toEqual([]);
  });

  it("writes NO shell terminal for a stopped SUBAGENT: its terminal is the spawn unit's", async () => {
    const h = harness();
    await started(h);
    await h.engine.onSdkMessage(taskStarted({ task_type: "local_agent" }));

    await h.engine.killSession(create(shimv1.KillSessionRequestSchema, { force: true }));

    expect(interruptedCommands(h)).toEqual([]);
  });
});

describe("a model name the vendor could not really state", () => {
  it("adopts nothing from a reported model that is only whitespace", async () => {
    const h = harness();
    await started(h);
    const stream = h.engine.pushes.subscribe()[Symbol.asyncIterator]();
    const names: string[] = [];
    const reading = (async () => {
      for (;;) {
        const step = await stream.next();
        if (step.done === true) return;
        const update = step.value.update;
        if (update.case === "modelChanged") names.push(update.value.effectiveModel?.name ?? "");
      }
    })();

    await h.engine.onSdkMessage({
      type: "assistant",
      uuid: "00000000-0000-4000-8000-0000000000e1",
      session_id: "s",
      parent_tool_use_id: null,
      message: { model: "   ", content: [] },
    } as never);
    await h.engine.standDown("done");
    await reading;

    expect(names.filter((name) => name.trim() === "")).toEqual([]);
  });
});

describe("folding before the session has an identity", () => {
  it("REFUSES to fold a vendor message, rather than keying rows to nothing", async () => {
    const h = harness();

    await expect(
      h.engine.onSdkMessage(assistantMessage("00000000-0000-4000-8000-0000000000e2")),
    ).rejects.toThrow(/identity is not established/);
  });
});

describe("the vendor query dying under the loop", () => {
  it("reports an iterator failure as query_died.iterator_failure", async () => {
    const h = harness();
    await started(h);
    const stream = h.engine.pushes.subscribe()[Symbol.asyncIterator]();
    const causes: string[] = [];
    const reading = (async () => {
      for (;;) {
        const step = await stream.next();
        if (step.done === true) return;
        const update = step.value.update;
        if (update.case === "queryDied") causes.push(update.value.cause.case ?? "");
      }
    })();

    // The loop is PARKED in the iterator; a stream that merely ends under a
    // parked reader is an EOF. A message first unparks it, so the failure is
    // raised on the next pull, which is how a real iterator throws.
    h.queries[0]?.query.emit(assistantMessage("00000000-0000-4000-8000-0000000000e3"));
    h.queries[0]?.query.fail(new Error("the vendor stream broke"));
    for (let attempt = 0; attempt < 50 && causes.length === 0; attempt++) {
      await new Promise((resolve) => setImmediate(resolve));
    }
    await h.engine.standDown("done");
    await reading;

    expect(causes).toContain("iteratorFailure");
  });
});

describe("a vendor that ENDS the opening instead of announcing it", () => {
  /**
   * WHAT THIS GUARDS: that `INIT_TIMEOUT_MS` is a bound on SILENCE and nothing
   * else. The grounded failure (2026-09-13, workspace 2b81f45a724642ef) had the
   * vendor emit a `SessionStart:resume` hook and then stop, and every one of
   * these arms used to sit out the full 45s before answering — long enough for
   * the daemon's own bound to fire first and blame the shim.
   *
   * EVERY HARNESS HERE HOLDS THE PROVEN-LIVE SIGNAL. A start now settles on one
   * answered control round-trip, and a scripted query answers it in a
   * microtask — so without the hold the start would succeed before any of these
   * subjects (a stream that ends, an error result, a blocking hook, the
   * silence bound) could land, and every one of these tests would be asserting
   * a race rather than the arm it names.
   *
   * Every bound here is generous ON PURPOSE: a test that passed because the
   * bound fired would prove the opposite of what it claims.
   */
  const AMPLE = 60_000;

  it("settles the start when the vendor's stream ENDS before its init", async () => {
    const h = harness({ initTimeoutMs: AMPLE, liveSignalTimeoutMs: AMPLE, holdLiveSignal: true });
    const pending = h.engine.startSession(freshRequest());

    (await untilQuery(h, 0)).query.end();

    expect(failureCause(await pending)).toBe("vendorStartFailed");
  });

  it("names the query's end as the reason when the stream ends before the init", async () => {
    const h = harness({ initTimeoutMs: AMPLE, liveSignalTimeoutMs: AMPLE, holdLiveSignal: true });
    const pending = h.engine.startSession(freshRequest());

    (await untilQuery(h, 0)).query.end();

    const response = await pending;
    expect(response.result.case === "failure" ? response.result.value.detail : "").toContain(
      "the vendor query ended before its init message",
    );
  });

  it("settles the start when the vendor's stream THROWS before its init", async () => {
    const h = harness({ initTimeoutMs: AMPLE, liveSignalTimeoutMs: AMPLE, holdLiveSignal: true });
    const pending = h.engine.startSession(freshRequest());
    const first = await untilQuery(h, 0);

    // The loop is PARKED in the iterator, and a stream that merely ends under a
    // parked reader is an EOF. One message unparks it, so the failure is raised
    // on the next pull — which is how a real iterator throws.
    first.query.emit(hookResponse({ outcome: "success", output: "" }));
    first.query.fail(new Error("spawn ENOENT"));

    const response = await pending;
    expect(response.result.case === "failure" ? response.result.value.detail : "").toContain(
      "spawn ENOENT",
    );
  });

  it("settles the start when the vendor answers the opening with an error result", async () => {
    const h = harness({ initTimeoutMs: AMPLE, liveSignalTimeoutMs: AMPLE, holdLiveSignal: true });
    const pending = h.engine.startSession(freshRequest());

    (await untilQuery(h, 0)).query.emit(errorResultMessage());

    expect(failureCause(await pending)).toBe("vendorStartFailed");
  });

  it("relays the vendor's own text when an error result refuses the opening", async () => {
    // THE VENDOR'S WORDS, NOT THE SHIM'S. "No conversation found with session
    // ID ..." is an answer a reader can act on; "did not send its init message"
    // is not.
    const h = harness({ initTimeoutMs: AMPLE, liveSignalTimeoutMs: AMPLE, holdLiveSignal: true });
    const pending = h.engine.startSession(freshRequest());

    (await untilQuery(h, 0)).query.emit(
      errorResultMessage({ errors: ["No conversation found with session ID: bf5fcae1"] }),
    );

    const response = await pending;
    expect(response.result.case === "failure" ? response.result.value.detail : "").toContain(
      "No conversation found with session ID: bf5fcae1",
    );
  });

  it("leaves a SUCCESSFUL result before the init to the turn engine", async () => {
    // Only an ERROR result ends an opening. A success-shaped result is an
    // ordinary terminal and must not refuse a session that then opens fine.
    const h = harness({ initTimeoutMs: AMPLE, liveSignalTimeoutMs: AMPLE, holdLiveSignal: true });
    const pending = h.engine.startSession(freshRequest());
    const first = await untilQuery(h, 0);
    first.query.emit(resultMessage());
    first.query.emit(initMessage({ sessionId: freshSessionId(first.spec) }));
    // The opening still wants its catalog, which the hold was keeping back so
    // the result could land first.
    first.query.releaseModels();

    expect((await pending).result.case).toBe("success");
  });

  it("carries the vendor child's stderr into a start that timed out on silence", async () => {
    // THE BOUND STILL FIRES FOR TRUE SILENCE, and when the child explained
    // itself on stderr the refusal says so rather than reporting the quiet.
    const h = harness({ initTimeoutMs: 5, liveSignalTimeoutMs: AMPLE, holdLiveSignal: true });
    const pending = h.engine.startSession(freshRequest());
    (await untilQuery(h, 0)).spec.onStderr?.("Error: the resume handle is not recognized");

    const response = await pending;
    expect(response.result.case === "failure" ? response.result.value.detail : "").toContain(
      "the vendor said: Error: the resume handle is not recognized",
    );
  });

  it("closes the vendor query a failed start had opened", async () => {
    // NO ORPHANED CHILD. The retry must be the only writer on the
    // conversation; the failed attempt's query is not left running beside it.
    const h = harness({ initTimeoutMs: 5, liveSignalTimeoutMs: AMPLE, holdLiveSignal: true });

    await h.engine.startSession(freshRequest());

    expect(h.queries[0]?.query.calls).toContain("close");
  });

  it("raises no query_died fault for the query a failed start closed", async () => {
    // A RELEASED QUERY'S END IS NOT THE SESSION'S DEATH. Reporting one left a
    // shim that had merely refused a start carrying a permanent vendor fault.
    const h = harness({ initTimeoutMs: 5, liveSignalTimeoutMs: AMPLE, holdLiveSignal: true });
    const pushed: string[] = [];
    const stream = h.engine.pushes.subscribe()[Symbol.asyncIterator]();
    const reading = (async () => {
      for (;;) {
        const step = await stream.next();
        if (step.done === true) return;
        pushed.push(step.value.update.case ?? "");
      }
    })();

    await h.engine.startSession(freshRequest());
    for (let attempt = 0; attempt < 20; attempt++) await new Promise((r) => setImmediate(r));
    await h.engine.standDown("done");
    await reading;

    expect(pushed).not.toContain("queryDied");
  });

  it("records what the vendor DID emit before the start failed", async () => {
    // THE NEXT OCCURRENCE EXPLAINS ITSELF. The grounded failure's only evidence
    // was a store frame decoded by hand, because nothing logged the kinds.
    const h = harness({ initTimeoutMs: 5, liveSignalTimeoutMs: AMPLE, holdLiveSignal: true });
    const before = logSinkMark();
    const pending = h.engine.startSession(freshRequest());
    (await untilQuery(h, 0)).query.emit(hookResponse({ outcome: "success", output: "" }));
    await pending;

    expect(logContextFor(before, "what the vendor emitted before the start failed")?.["pre_init_kinds"]).toBe(
      "system:hook_response",
    );
  });

  it("records the account root the failed start was spawned under", async () => {
    // A SILENT START IS DIAGNOSED FROM THE SHIM'S SIDE OR NOT AT ALL. Which
    // account root and which trust key govern this directory is half of that,
    // and it was on neither side's record.
    const h = harness({ initTimeoutMs: 5, liveSignalTimeoutMs: AMPLE, holdLiveSignal: true });
    const before = logSinkMark();

    await h.engine.startSession(freshRequest());

    expect(logContextFor(before, "what the vendor emitted before the start failed")?.["trust_root"]).toBe(
      "/ws",
    );
  });

  it("names the control round-trip the start settles on when only hook events arrived", async () => {
    // THE GROUNDED SILENCE HAS A CAUSE, AND THE REFUSAL SHOULD SAY IT. The
    // start settles on one control round-trip, so "hook events then nothing,
    // and no control answer either" is the shape of a child that is not
    // serving at all rather than of a slow one.
    const h = harness({ initTimeoutMs: 5, liveSignalTimeoutMs: AMPLE, holdLiveSignal: true });
    const pending = h.engine.startSession(freshRequest());
    (await untilQuery(h, 0)).query.emit(hookResponse({ outcome: "success", output: "" }));

    const response = await pending;
    expect(response.result.case === "failure" ? response.result.value.detail : "").toContain(
      "The start settles on one control round-trip, so a child this quiet is not serving its control channel at all",
    );
  });

  it("names the trust entry to confirm, and the file it lives in", async () => {
    // THE COMPANION FAULT. An untrusted workspace does not hang, but it runs
    // with its permission allowlists dropped and says so only on a stderr line
    // nobody reads, so the one refusal a reader does see names the key.
    const h = harness({ initTimeoutMs: 5, liveSignalTimeoutMs: AMPLE, holdLiveSignal: true });
    const pending = h.engine.startSession(freshRequest());
    (await untilQuery(h, 0)).query.emit(hookResponse({ outcome: "success", output: "" }));

    const response = await pending;
    expect(response.result.case === "failure" ? response.result.value.detail : "").toContain(
      'projects["/ws"].hasTrustDialogAccepted is true in',
    );
  });

  it("keeps the bare bound when the vendor emitted something other than hooks", async () => {
    // A VENDOR THAT WAS TALKING HAS A MORE SPECIFIC STORY. The init-on-first-
    // turn reading would talk over it, so it is claimed only for the shape it
    // was grounded on.
    const h = harness({ initTimeoutMs: 5, liveSignalTimeoutMs: AMPLE, holdLiveSignal: true });
    const pending = h.engine.startSession(freshRequest());
    (await untilQuery(h, 0)).query.emit(assistantMessage("uuid-pre-init"));

    const response = await pending;
    expect(response.result.case === "failure" ? response.result.value.detail : "").toBe(
      "the vendor neither answered a control request nor said anything within 5ms",
    );
  });
});

/**
 * THE PROVEN-LIVE SIGNAL A START SETTLES ON.
 *
 * WHAT THIS GUARDS: that `StartSession` never again waits for a message the
 * vendor does not send until it is prompted. Grounded 2026-09-13 against
 * claude 2.1.220 AND 2.1.270, driven exactly as this shim drives them: the
 * child answers control requests in ~300ms and emits no `system:init` at all
 * until a first user message arrives. A start that waited for `init` before it
 * would accept a prompt therefore deadlocked on every real session.
 */
describe("the proven-live signal a start settles on", () => {
  it("settles the start on the control round-trip, with no init at all", async () => {
    const h = harness();

    const response = await h.engine.startSession(freshRequest());

    expect(response.result.case).toBe("success");
  });

  it("proves the child live by ASKING it something, not by waiting", async () => {
    // The round-trip is the whole proof, and it has to have actually happened:
    // a start that answered success without asking would be asserting nothing.
    const h = harness();

    await h.engine.startSession(freshRequest());

    expect(h.queries[0]?.query.calls).toContain("supportedModels");
  });

  it("spends ONE control call on the catalog, not two", async () => {
    // The round-trip's answer IS `SessionStarted.model_catalog`, so the opening
    // reads it once. A second read here would spend a vendor call to learn what
    // the start already knows.
    const h = harness();

    await h.engine.startSession(freshRequest());

    expect(h.queries[0]?.query.calls.filter((call) => call === "supportedModels")).toHaveLength(1);
  });

  it("carries the catalog the round-trip answered into the opening", async () => {
    const h = harness({
      onQueryCreated: (query) => {
        query.models = [{ value: "claude-opus-5", displayName: "O", description: "d" }];
      },
    });

    const response = await h.engine.startSession(freshRequest());

    expect(
      response.result.case === "success"
        ? response.result.value.session?.modelCatalog.map((option) => option.model?.name)
        : undefined,
    ).toEqual(["claude-opus-5"]);
  });

  it("states the agent binary version from the SDK's manifest, with no init", async () => {
    // `SessionRuntime.agent_binary_version` is non-optional and init is what
    // used to state it. The SDK's own bundled manifest states it too, which is
    // why an opening that never sees an init is still a legal message.
    const h = harness();

    const response = await h.engine.startSession(freshRequest());

    expect(
      response.result.case === "success"
        ? response.result.value.session?.runtime?.agentBinaryVersion
        : undefined,
    ).toBe("2.1.999");
  });

  it("still settles on `init` when a vendor announces one FIRST", async () => {
    // THE OLDER VENDOR'S SHAPE, unchanged. The live signal is held so init is
    // provably what settles this start rather than racing it.
    const h = harness({ holdLiveSignal: true, liveSignalTimeoutMs: 60_000 });
    const pending = h.engine.startSession(freshRequest());
    const first = await untilQuery(h, 0);
    first.query.emit(initMessage({ sessionId: freshSessionId(first.spec) }));
    first.query.releaseModels();

    expect((await pending).result.case).toBe("success");
  });

  it("applies the permission mode the first turn's init reports", async () => {
    // AN INIT FACT LEARNED LATE IS STILL LEARNED. The start asked for `default`
    // and the vendor answered on `plan`, which nothing else would ever say.
    const h = harness();
    await h.engine.startSession(freshRequest());

    const seen = await pushedUpdates(
      h,
      (update) =>
        update.case === "permissionModeChanged"
          ? update.value.permissionMode?.mode.case
          : undefined,
      async () => {
        h.queries[0]?.query.emit({
          ...initMessage({ sessionId: freshSessionId(h.queries[0].spec) }),
          permissionMode: "plan",
        } as SdkMessage);
        await vi.waitFor(() => {
          expect(h.fold.seen.length).toBeGreaterThan(0);
        });
      },
    );

    expect(seen).toContain("plan");
  });

  it("says nothing when the first turn's init merely RESTATES the start's facts", async () => {
    // The ordinary case: init agrees with what the opening already announced,
    // and a push for a fact that did not change is noise on every surface. The
    // one entry here is the fan-out REPLAYING the mode to a joining consumer,
    // which is the start's own push and not a second statement of it.
    const h = harness();
    await h.engine.startSession(freshRequest());

    const seen = await pushedUpdates(
      h,
      (update) =>
        update.case === "permissionModeChanged"
          ? update.value.permissionMode?.mode.case
          : undefined,
      async () => {
        h.queries[0]?.query.emit(initMessage({ sessionId: freshSessionId(h.queries[0].spec) }));
        await vi.waitFor(() => {
          expect(h.fold.seen.length).toBeGreaterThan(0);
        });
      },
    );

    expect(seen).toEqual(["default"]);
  });

  it("rotates the identity when the first turn's init names another session id", async () => {
    // The vendor's own id wins, whenever it states one — and it now states it
    // for the first time AFTER the start already announced the minted id.
    const h = harness();
    await h.engine.startSession(freshRequest());

    const seen = await pushedUpdates(
      h,
      (update) => (update.case === "identityRotated" ? update.value.vendorSessionId : undefined),
      async () => {
        h.queries[0]?.query.emit(initMessage({ sessionId: "22222222-2222-4222-8222-222222222222" }));
        await vi.waitFor(() => {
          expect(h.fold.seen.length).toBeGreaterThan(0);
        });
      },
    );

    expect(seen).toContain("22222222-2222-4222-8222-222222222222");
  });
});
describe("StartSession's remaining refusals", () => {
  it("refuses vendor_start_failed when the vendor answers nothing at all", async () => {
    // A CHILD THAT ANSWERS NEITHER ITS CONTROL CHANNEL NOR ITS STREAM. Without
    // a last-resort bound it would hold the verb open forever.
    const h = harness({ initTimeoutMs: 5, liveSignalTimeoutMs: 60_000, holdLiveSignal: true });

    expect(failureCause(await h.engine.startSession(freshRequest()))).toBe("vendorStartFailed");
  });

  it("names the bound it waited out when the vendor answers nothing at all", async () => {
    // A HANG THAT ANSWERS SAYS WHY. The daemon relays this detail verbatim, so
    // the bound that fired has to be in it.
    const h = harness({ initTimeoutMs: 5, liveSignalTimeoutMs: 60_000, holdLiveSignal: true });

    const response = await h.engine.startSession(freshRequest());

    expect(response.result.case === "failure" ? response.result.value.detail : "").toContain(
      "neither answered a control request nor said anything within 5ms",
    );
  });

  it("refuses at once when a hook blocks before the vendor's init", async () => {
    // THE GROUNDED CASE: a blocking `SessionStart:resume` hook, after which the
    // vendor says nothing more. Waiting out a bound for a refusal already in
    // hand is what left every boot bring-up hanging. The live signal is HELD so
    // the hook is what settles this start rather than a race with it.
    const h = harness({ holdLiveSignal: true });
    const pending = h.engine.startSession(freshRequest());
    (await untilQuery(h, 0)).query.emit(hookResponse({ outcome: "error", output: "not today" }));

    expect(failureCause(await pending)).toBe("vendorStartFailed");
  });

  it("names the hook and its own reason when a hook blocks before the init", async () => {
    const h = harness({ holdLiveSignal: true });
    const pending = h.engine.startSession(freshRequest());
    (await untilQuery(h, 0)).query.emit(hookResponse({ outcome: "error", output: "not today" }));

    const response = await pending;

    const detail = response.result.case === "failure" ? response.result.value.detail : "";
    expect(detail).toContain("SessionStart:resume");
    expect(detail).toContain("not today");
  });

  it("proceeds when a hook merely FAILS before the vendor's init", async () => {
    // A hook that gates nothing — no interpreter on PATH is the grounded one —
    // writes stderr and blocks nothing. The session opens as it always did.
    const h = harness();
    const pending = h.engine.startSession(freshRequest());
    const first = await untilQuery(h, 0);
    first.query.emit(hookResponse({ outcome: "error", output: "", stderr: "powershell: not found" }));
    first.query.emit(initMessage({ sessionId: freshSessionId(first.spec) }));

    expect((await pending).result.case).toBe("success");
  });

  it("proceeds when a hook SUCCEEDS with output before the vendor's init", async () => {
    // A `SessionStart` hook's additional context is output, and output alone is
    // not a refusal.
    const h = harness();
    const pending = h.engine.startSession(freshRequest());
    const first = await untilQuery(h, 0);
    first.query.emit(hookResponse({ outcome: "success", output: "extra context" }));
    first.query.emit(initMessage({ sessionId: freshSessionId(first.spec) }));

    expect((await pending).result.case).toBe("success");
  });

  it("leaves a hook that blocks AFTER the session opened to the turn it gates", async () => {
    // The start gate is closed once `init` landed: a `PreToolUse` refusal later
    // in the session is the turn's business, not the start's.
    const h = harness();
    await started(h);

    h.queries[0]?.query.emit(hookResponse({ hook_name: "PreToolUse:one", outcome: "error", output: "no" }));

    expect(h.exits).toEqual([]);
  });

  it("throws when StartSession reaches the engine with no source at all", async () => {
    const h = harness();

    await expect(
      h.engine.startSession(create(shimv1.StartSessionRequestSchema, {})),
    ).rejects.toThrow(/no source/);
  });

  it("proceeds without remediation when the caller named an arm the shim does not know", async () => {
    const h = harness({ nowMs: 1_000_000 + 10 * 60 * 1000 });
    writeTranscript(h.configDir, h.cwd, "resume-1", [assistantLine()]);
    const unset = create(conversationv1.SessionColdRemediationSchema, {});
    const pending = h.engine.startSession(resumeRequest("resume-1", unset));
    (await untilQuery(h, 0)).query.emit(initMessage({ sessionId: "resume-1" }));

    expect((await pending).result.case).toBe("success");
  });

  it("REFUSES the start when the vendor will not answer supportedModels", async () => {
    // THAT CALL IS THE START'S PROOF OF LIFE. A child that refuses it has not
    // shown it can take work, so the opening is a refusal that names the
    // vendor's own reason rather than a session with an empty picker.
    const h = harness({
      onQueryCreated: (query) => {
        query.supportedModels = () => Promise.reject(new Error("the vendor cannot list its models"));
      },
    });

    const response = await h.engine.startSession(freshRequest());

    expect(failureCause(response)).toBe("vendorStartFailed");
  });

  it("names the vendor's own reason when the live signal is refused", async () => {
    const h = harness({
      onQueryCreated: (query) => {
        query.supportedModels = () => Promise.reject(new Error("the vendor cannot list its models"));
      },
    });

    const response = await h.engine.startSession(freshRequest());

    expect(response.result.case === "failure" ? response.result.value.detail : "").toContain(
      "the vendor did not prove itself live: the vendor cannot list its models",
    );
  });

  it("names the round-trip's bound when the vendor never answers supportedModels", async () => {
    // A BOUND THAT FIRES SAYS WHICH CALL AND HOW LONG. The daemon relays this
    // detail verbatim, and "the start failed" tells a reader nothing.
    const h = harness({ liveSignalTimeoutMs: 5, holdLiveSignal: true });

    const response = await h.engine.startSession(freshRequest());

    expect(response.result.case === "failure" ? response.result.value.detail : "").toContain(
      "the vendor did not answer supportedModels within 5ms",
    );
  });

  it("reports a supportedModels failure AFTER the start as a session fault", async () => {
    // ONCE THE START HAS SETTLED the catalog is an ordinary pulled fact again:
    // a read that fails raises the catalog's own component fault, which a
    // later read can lift, and takes nothing else down with it.
    let reads = 0;
    const h = harness({
      onQueryCreated: (query) => {
        query.supportedModels = async (): Promise<ModelInfoLike[]> => {
          reads += 1;
          if (reads === 1) return query.models;
          throw new Error("the vendor cannot list its models");
        };
      },
    });
    await started(h);
    const before = h.engine.pushes.faultCount;

    await h.engine.startTurn(
      create(shimv1.StartTurnRequestSchema, {
        turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }),
        said: textSaid("go"),
        origin: conversationv1.PromptOrigin.USER_SENT,
        pageSize: 5,
      }),
    );
    h.queries[0]?.query.emit(resultMessage());
    await vi.waitFor(() => {
      expect(h.engine.pushes.faultCount).toBeGreaterThan(before);
    });
  });
});

describe("the cold gate's COMPACT remediation", () => {
  /** A `compact` remediation over the whole conversation. */
  const compactRemediation = (): conversationv1.SessionColdRemediation =>
    create(conversationv1.SessionColdRemediationSchema, {
      remediation: {
        case: "compact",
        value: create(conversationv1.SessionColdCompactSchema, {
          scope: conversationv1.SessionCompactScope.ALL,
        }),
      },
    });

  /** Every context cut this session wrote, in the order it wrote them. */
  function contextCuts(h: Harness): conversationv1.ContextCut[] {
    return h.persistence.buffered.flatMap((entry) => {
      if (entry.item.kind !== "frame") return [];
      const result = entry.item.frame.result;
      if (result.case !== "update") return [];
      const update = result.value.update;
      return update.case === "contextCut" ? [update.value] : [];
    });
  }

  it("WRITES the cut it held until the session had an identity", async () => {
    // The remediation runs before the identity is settled, so its page line
    // cannot be keyed yet — it waits rather than vanishing.
    const h = harness({
      nowMs: 1_000_000 + 10 * 60 * 1000,
      onQueryCreated: (query, _spec, index) => {
        if (index === 0) query.emit(resultMessage("77777777-7777-4777-8777-777777777777"));
      },
    });
    writeTranscript(h.configDir, h.cwd, "resume-1", [assistantLine()]);
    const pending = h.engine.startSession(resumeRequest("resume-1", compactRemediation()));
    (await untilQuery(h, 1)).query.emit(initMessage({ sessionId: "resume-1" }));
    await pending;

    expect(contextCuts(h).map((cut) => cut.cut.case)).toEqual(["compacted"]);
  });

  it("refuses vendor_start_failed when the summarizing session ends in a failure subtype", async () => {
    const h = harness({
      nowMs: 1_000_000 + 10 * 60 * 1000,
      onQueryCreated: (query, _spec, index) => {
        if (index === 0) {
          query.emit({
            ...(resultMessage("88888888-8888-4888-8888-888888888888") as unknown as Record<string, unknown>),
            subtype: "error_max_turns",
          } as never);
        }
      },
    });
    writeTranscript(h.configDir, h.cwd, "resume-1", [assistantLine()]);

    const response = await h.engine.startSession(resumeRequest("resume-1", compactRemediation()));

    expect(
      response.result.case === "failure" ? response.result.value.detail : undefined,
    ).toBe("the summarizing session ended as error_max_turns");
  });

  it("refuses when the summarizing session produced no summary", async () => {
    const h = harness({
      nowMs: 1_000_000 + 10 * 60 * 1000,
      onQueryCreated: (query, _spec, index) => {
        if (index === 0) {
          query.emit({
            ...(resultMessage("99999999-9999-4999-8999-999999999999") as unknown as Record<string, unknown>),
            result: "",
          } as never);
        }
      },
    });
    writeTranscript(h.configDir, h.cwd, "resume-1", [assistantLine()]);

    const response = await h.engine.startSession(resumeRequest("resume-1", compactRemediation()));

    expect(
      response.result.case === "failure" ? response.result.value.detail : undefined,
    ).toBe("the summarizing session produced no summary");
  });

  it("refuses with the vendor's own words when the throwaway query cannot be created", async () => {
    const h = harness({ nowMs: 1_000_000 + 10 * 60 * 1000, createQueryFailsFrom: 0 });
    writeTranscript(h.configDir, h.cwd, "resume-1", [assistantLine()]);

    const response = await h.engine.startSession(resumeRequest("resume-1", compactRemediation()));

    expect(
      response.result.case === "failure" ? response.result.value.detail : undefined,
    ).toBe("the vendor refused another query");
  });

  /** A `pay` remediation: the caller elects to pay for the cold read. */
  const payRemediation = (): conversationv1.SessionColdRemediation =>
    create(conversationv1.SessionColdRemediationSchema, {
      remediation: { case: "pay", value: create(conversationv1.SessionColdPaySchema, {}) },
    });

  /**
   * Watch every `compaction_progress` frame the engine pushes, IN ORDER, and
   * interleave it with whatever else the caller records in `timeline`.
   *
   * The fan-out's own subscriber is an async iterator, so a frame it delivers
   * cannot be ordered against a synchronous event like "the summarizing query
   * was created". The push itself can: it happens on the engine's own stack.
   */
  function watchPhases(
    h: Harness,
    timeline: string[],
  ): { frames: conversationv1.SessionCompactionProgress[] } {
    const frames: conversationv1.SessionCompactionProgress[] = [];
    const real = h.engine.pushes.push.bind(h.engine.pushes);
    vi.spyOn(h.engine.pushes, "push").mockImplementation((update) => {
      if (update.update.case === "compactionProgress") {
        frames.push(update.update.value);
        timeline.push(conversationv1.SessionCompactionPhase[update.update.value.phase]);
      }
      return real(update);
    });
    return { frames };
  }

  /**
   * A cold resume, driven to its own answer.
   *
   * `queryIndex` is which created query is the REAL session's: a `compact`
   * answer spends query 0 on the throwaway summarizer, a `pay` answer spends
   * none, so the session's own query is the next one either way.
   */
  async function coldResume(
    h: Harness,
    remediation: conversationv1.SessionColdRemediation,
    queryIndex: number,
  ): Promise<shimv1.StartSessionResponse> {
    writeTranscript(h.configDir, h.cwd, "resume-1", [assistantLine()]);
    const pending = h.engine.startSession(resumeRequest("resume-1", remediation));
    (await untilQuery(h, queryIndex)).query.emit(initMessage({ sessionId: "resume-1" }));
    return pending;
  }

  /** The same, for the `compact` answer this suite is about. */
  const compactedResume = (h: Harness): Promise<shimv1.StartSessionResponse> =>
    coldResume(h, compactRemediation(), 1);

  /** A harness whose throwaway summarizing query answers with a summary. */
  const summarizing = (timeline?: string[]): Harness =>
    harness({
      nowMs: 1_000_000 + 10 * 60 * 1000,
      onQueryCreated: (query, _spec, index) => {
        if (index !== 0) return;
        timeline?.push("summarizing-query");
        query.emit(resultMessage("77777777-7777-4777-8777-777777777777"));
      },
    });

  it("pushes SUMMARIZING before the summarizing query is created", async () => {
    // Arrange: the compaction happens inside StartSession, so the frame that
    // says it started must precede the query that does the work — otherwise
    // the opening of the wait is unnarrated.
    const timeline: string[] = [];
    const h = summarizing(timeline);
    watchPhases(h, timeline);

    // Act
    await compactedResume(h);

    // Assert
    expect(timeline.slice(0, 2)).toEqual(["SUMMARIZING", "summarizing-query"]);
  });

  it("carries the transcript's own before figure on the SUMMARIZED frame", async () => {
    // Arrange
    const timeline: string[] = [];
    const h = summarizing();
    const watched = watchPhases(h, timeline);

    // Act
    await compactedResume(h);

    // Assert
    const summarized = watched.frames.find(
      (frame) => frame.phase === conversationv1.SessionCompactionPhase.SUMMARIZED,
    );
    expect(summarized?.tokensBefore).toBe(BigInt(FIXTURE_CONTEXT_TOKENS));
  });

  it("carries the summary's own output tokens as the SUMMARIZED after figure", async () => {
    // Arrange: what remains in context after the cut IS the summary, so its
    // output tokens are the only honest "after" figure the shim holds.
    const timeline: string[] = [];
    const h = summarizing();
    const watched = watchPhases(h, timeline);

    // Act
    await compactedResume(h);

    // Assert
    const summarized = watched.frames.find(
      (frame) => frame.phase === conversationv1.SessionCompactionPhase.SUMMARIZED,
    );
    expect(summarized?.tokensAfter).toBe(BigInt(2));
  });

  it("pushes FAILED carrying the failure's own wording", async () => {
    // Arrange
    const timeline: string[] = [];
    const h = harness({
      nowMs: 1_000_000 + 10 * 60 * 1000,
      onQueryCreated: (query, _spec, index) => {
        if (index !== 0) return;
        query.emit({
          ...(resultMessage("88888888-8888-4888-8888-888888888888") as unknown as Record<string, unknown>),
          subtype: "error_max_turns",
        } as never);
      },
    });
    const watched = watchPhases(h, timeline);
    writeTranscript(h.configDir, h.cwd, "resume-1", [assistantLine()]);

    // Act
    await h.engine.startSession(resumeRequest("resume-1", compactRemediation()));

    // Assert
    const failed = watched.frames.find(
      (frame) => frame.phase === conversationv1.SessionCompactionPhase.FAILED,
    );
    expect(failed?.error).toBe("the summarizing session ended as error_max_turns");
  });

  it("pushes RESUMING then STARTED, in that order, once the compaction landed", async () => {
    // Arrange
    const timeline: string[] = [];
    const h = summarizing();
    watchPhases(h, timeline);

    // Act
    await compactedResume(h);

    // Assert
    expect(timeline).toEqual(["SUMMARIZING", "SUMMARIZED", "RESUMING", "STARTED"]);
  });

  it("restates the compaction's figures on the STARTED frame", async () => {
    // Arrange: `started` is pushed from StartSession, which never read the
    // transcript's size — it restates what the compaction measured.
    const timeline: string[] = [];
    const h = summarizing();
    const watched = watchPhases(h, timeline);

    // Act
    await compactedResume(h);

    // Assert
    const started_ = watched.frames.find(
      (frame) => frame.phase === conversationv1.SessionCompactionPhase.STARTED,
    );
    expect([started_?.tokensBefore, started_?.tokensAfter]).toEqual([
      BigInt(FIXTURE_CONTEXT_TOKENS),
      BigInt(2),
    ]);
  });

  it("pushes no compaction phase for a gate answered with PAY", async () => {
    // Arrange: `pay` cuts nothing, so a progress frame would narrate a
    // compaction that never happened.
    const timeline: string[] = [];
    const h = harness({ nowMs: 1_000_000 + 10 * 60 * 1000 });
    watchPhases(h, timeline);

    // Act
    await coldResume(h, payRemediation(), 0);

    // Assert
    expect(timeline).toEqual([]);
  });
});

describe("reconciliation when the record cannot describe the work", () => {
  it("leaves a live run live when the book cannot be read: nothing states it ran in the CLI process", async () => {
    // Arrange.
    const h = harness();
    h.persistence.live = create(storev1.GetLiveWorkSuccessSchema, {
      liveDetached: [create(conversationv1.DetachedWorkIdSchema, { value: "b01" })],
    });
    h.persistence.openError = new PersistenceError("store_unavailable", "the store is down");

    // Act.
    await started(h);

    // Assert.
    expect(h.persistence.buffered.filter((entry) => entry.upsertKey.includes("b01"))).toEqual([]);
  });

  it("closes a SPAWN the book describes as a subagent, not as a shell run", async () => {
    // The kind comes from what the book says the unit was; closing a spawn as a
    // shell would put a bash terminal on a subagent's own unit.
    const h = harness();
    h.persistence.live = create(storev1.GetLiveWorkSuccessSchema, {
      liveDetached: [create(conversationv1.DetachedWorkIdSchema, { value: "toolu_spawn" })],
    });
    h.persistence.page = create(conversationv1.HistoryPageSchema, {
      entries: [
        create(conversationv1.HistoryEntryAtSchema, {
          at: create(conversationv1.HistoryPointerSchema, { value: "1" }),
          entry: create(conversationv1.HistoryEntrySchema, {
            entry: {
              case: "agentFrame",
              value: create(conversationv1.AgentFrameSchema, {
                result: {
                  case: "update",
                  value: create(conversationv1.AgentUpdateSchema, {
                    update: {
                      case: "activity",
                      value: create(conversationv1.AgentActivitySchema, {
                        activityId: create(conversationv1.AgentActivityIdSchema, {
                          value: "toolu_spawn",
                        }),
                        item: {
                          case: "subagent",
                          value: create(conversationv1.AgentSubagentSchema, {
                            result: {
                              case: "start",
                              value: create(conversationv1.AgentSubagentStartSchema, {}),
                            },
                          }),
                        },
                      }),
                    },
                  }),
                },
              }),
            },
          }),
        }),
      ],
      boundary: { case: "floor", value: create(conversationv1.HistoryFloorSchema, {}) },
    });

    await started(h);

    expect(
      h.persistence.buffered.some(
        (entry) => entry.source.discriminator === "activity.subagent.failure.lost.swept_up",
      ),
    ).toBe(true);
  });

  /** One book entry stating a unit's activity item, at pointer `at`. */
  function unitEntry(
    at: string,
    unit: string,
    item: conversationv1.AgentActivity["item"],
  ): conversationv1.HistoryEntryAt {
    return create(conversationv1.HistoryEntryAtSchema, {
      at: create(conversationv1.HistoryPointerSchema, { value: at }),
      entry: create(conversationv1.HistoryEntrySchema, {
        entry: {
          case: "agentFrame",
          value: create(conversationv1.AgentFrameSchema, {
            result: {
              case: "update",
              value: create(conversationv1.AgentUpdateSchema, {
                update: {
                  case: "activity",
                  value: create(conversationv1.AgentActivitySchema, {
                    activityId: create(conversationv1.AgentActivityIdSchema, { value: unit }),
                    item,
                  }),
                },
              }),
            },
          }),
        },
      }),
    });
  }
  const spawnStart: conversationv1.AgentActivity["item"] = {
    case: "subagent",
    value: create(conversationv1.AgentSubagentSchema, {
      result: { case: "start", value: create(conversationv1.AgentSubagentStartSchema, {}) },
    }),
  };
  const monitorStart: conversationv1.AgentActivity["item"] = {
    case: "monitor",
    value: create(conversationv1.AgentMonitorSchema, {
      result: { case: "start", value: create(conversationv1.AgentMonitorStartSchema, {}) },
    }),
  };

  it("closes a spawn that started before the newest page as a subagent", async () => {
    // A long session's live agent started pages ago; reading only the newest
    // page closed it as a shell run and the store relabelled it `bash`.
    const h = harness();
    h.persistence.live = create(storev1.GetLiveWorkSuccessSchema, {
      liveDetached: [create(conversationv1.DetachedWorkIdSchema, { value: "toolu_old_spawn" })],
    });
    h.persistence.page = create(conversationv1.HistoryPageSchema, {
      entries: [unitEntry("9", "toolu_recent", monitorStart)],
      boundary: { case: "more", value: create(conversationv1.HistoryMoreSchema, {}) },
    });
    h.persistence.olderPages = [
      create(conversationv1.HistoryPageSchema, {
        entries: [unitEntry("1", "toolu_old_spawn", spawnStart)],
        boundary: { case: "floor", value: create(conversationv1.HistoryFloorSchema, {}) },
      }),
    ];

    await started(h);

    const discriminators = h.persistence.buffered.map((entry) => entry.source.discriminator);
    expect(discriminators).toContain("activity.subagent.failure.lost.swept_up");
    expect(discriminators).not.toContain("agent_bash.success.interrupted.lost.swept_up");
  });

  it("walks down from the oldest entry it already holds", async () => {
    const h = harness();
    h.persistence.live = create(storev1.GetLiveWorkSuccessSchema, {
      liveDetached: [create(conversationv1.DetachedWorkIdSchema, { value: "toolu_old_spawn" })],
    });
    h.persistence.page = create(conversationv1.HistoryPageSchema, {
      entries: [unitEntry("9", "toolu_a", monitorStart), unitEntry("8", "toolu_b", monitorStart)],
      boundary: { case: "more", value: create(conversationv1.HistoryMoreSchema, {}) },
    });
    h.persistence.olderPages = [
      create(conversationv1.HistoryPageSchema, {
        entries: [unitEntry("1", "toolu_old_spawn", spawnStart)],
        boundary: { case: "floor", value: create(conversationv1.HistoryFloorSchema, {}) },
      }),
    ];

    await started(h);

    expect(h.persistence.olderPageAfter[0]).toBe("8");
  });

  it("reads no older page when the newest one describes every live unit", async () => {
    const h = harness();
    h.persistence.live = create(storev1.GetLiveWorkSuccessSchema, {
      liveDetached: [create(conversationv1.DetachedWorkIdSchema, { value: "toolu_spawn" })],
    });
    h.persistence.page = create(conversationv1.HistoryPageSchema, {
      entries: [unitEntry("1", "toolu_spawn", spawnStart)],
      boundary: { case: "more", value: create(conversationv1.HistoryMoreSchema, {}) },
    });

    await started(h);

    expect(h.persistence.olderPageAfter).toEqual([]);
  });

  it("closes a MONITOR the book describes with the monitor's ended arm, not as a shell run", async () => {
    const h = harness();
    h.persistence.live = create(storev1.GetLiveWorkSuccessSchema, {
      liveDetached: [create(conversationv1.DetachedWorkIdSchema, { value: "toolu_watch" })],
    });
    h.persistence.page = create(conversationv1.HistoryPageSchema, {
      entries: [unitEntry("1", "toolu_watch", monitorStart)],
      boundary: { case: "floor", value: create(conversationv1.HistoryFloorSchema, {}) },
    });

    await started(h);

    const discriminators = h.persistence.buffered.map((entry) => entry.source.discriminator);
    expect(discriminators).toContain("activity.monitor.ended.swept_up");
    expect(discriminators).not.toContain("agent_bash.success.interrupted.lost.swept_up");
  });

  it("restates the recorded start on the monitor it closes, so its card still draws", async () => {
    const h = harness();
    h.persistence.live = create(storev1.GetLiveWorkSuccessSchema, {
      liveDetached: [create(conversationv1.DetachedWorkIdSchema, { value: "toolu_watch" })],
    });
    const armed = create(conversationv1.AgentMonitorStartSchema, { description: "watch the log" });
    h.persistence.page = create(conversationv1.HistoryPageSchema, {
      entries: [
        unitEntry("1", "toolu_watch", {
          case: "monitor",
          value: create(conversationv1.AgentMonitorSchema, { result: { case: "start", value: armed } }),
        }),
      ],
      boundary: { case: "floor", value: create(conversationv1.HistoryFloorSchema, {}) },
    });

    await started(h);

    const closed = h.persistence.buffered.find(
      (entry) => entry.source.discriminator === "activity.monitor.ended.swept_up",
    );
    const frame = closed?.item.kind === "frame" ? closed.item.frame : undefined;
    const update = (frame?.result.value as conversationv1.AgentUpdate | undefined)?.update;
    const monitor = (update?.value as conversationv1.AgentActivity | undefined)?.item.value as
      | conversationv1.AgentMonitor
      | undefined;
    expect((monitor?.result.value as conversationv1.AgentMonitorEnded | undefined)?.call?.description).toBe(
      "watch the log",
    );
  });

  it("neither re-adopts nor closes a live WORKFLOW run", async () => {
    // WORKFLOW IS KICKED this wave: a terminal written for one would close an
    // obligation nothing in this build owns.
    const h = harness();
    h.persistence.live = create(storev1.GetLiveWorkSuccessSchema, {
      liveWorkflows: [create(conversationv1.DetachedWorkIdSchema, { value: "w01" })],
    });

    const response = await started(h);

    expect(h.persistence.buffered).toEqual([]);
    expect(
      response.result.case === "success" ? response.result.value.session?.liveWork : undefined,
    ).toEqual([]);
  });
});

describe("the keep-alive beat that could not be recorded", () => {
  it("reports the failure as a keepalive_failed fault rather than losing the beat", async () => {
    const h = harness();
    await started(h);
    h.persistence.write = (): void => {
      throw new Error("the record plane refused the keep-alive prompt row");
    };
    const before = h.engine.pushes.faultCount;

    h.scheduler.fire(0);
    for (let attempt = 0; attempt < 50 && h.engine.pushes.faultCount === before; attempt++) {
      await new Promise((resolve) => setImmediate(resolve));
    }

    expect(h.engine.pushes.faultCount).toBe(before + 1);
  });
});

describe("what this session will answer a watch about", () => {
  it("refuses a watch on the empty agent id", async () => {
    const h = harness();
    await started(h);

    const iterator = h.engine
      .watchAgent(
        create(shimv1.WatchAgentRequestSchema, {
          target: create(conversationv1.AgentIdSchema, { value: "" }),
          pageSize: 5,
        }),
      )[Symbol.asyncIterator]();

    await expect(iterator.next()).rejects.toThrow(/no agent by that id/);
  });

  it("answers a watch on a subagent the live table still holds", async () => {
    const h = harness();
    await started(h);
    await h.engine.onSdkMessage({
      type: "system",
      subtype: "task_started",
      task_id: "a01",
      tool_use_id: "toolu_live_agent",
      task_type: "local_agent",
      description: "explore",
      uuid: "00000000-0000-4000-8000-0000000000f1",
      session_id: "s",
    } as never);

    const iterator = h.engine
      .watchAgent(
        create(shimv1.WatchAgentRequestSchema, {
          target: create(conversationv1.AgentIdSchema, { value: "toolu_live_agent" }),
          pageSize: 5,
        }),
      )[Symbol.asyncIterator]();
    const first = await nextPush(iterator);
    await iterator.return?.();

    expect(first.frame.case).toBe("page");
  });

  it("answers a watch on a subagent that has since RETIRED", async () => {
    // A retired handle was still announced; refusing it would deny a consumer
    // the book of work that plainly happened.
    const h = harness();
    await started(h);
    const spawn = {
      type: "system",
      subtype: "task_started",
      task_id: "a02",
      tool_use_id: "toolu_retired_agent",
      task_type: "local_agent",
      description: "explore",
      uuid: "00000000-0000-4000-8000-0000000000f2",
      session_id: "s",
    } as never;
    await h.engine.onSdkMessage(spawn);
    await h.engine.onSdkMessage({
      type: "system",
      subtype: "task_notification",
      task_id: "a02",
      status: "completed",
      uuid: "00000000-0000-4000-8000-0000000000f3",
      session_id: "s",
    } as never);

    const iterator = h.engine
      .watchAgent(
        create(shimv1.WatchAgentRequestSchema, {
          target: create(conversationv1.AgentIdSchema, { value: "toolu_retired_agent" }),
          pageSize: 5,
        }),
      )[Symbol.asyncIterator]();
    const first = await nextPush(iterator);
    await iterator.return?.();

    expect(first.frame.case).toBe("page");
  });
});

describe("the teardown's tails", () => {
  /** A one-entry page whose head pointer a conclusion can name. */
  function pageWithHead(pointer: string): conversationv1.HistoryPage {
    return create(conversationv1.HistoryPageSchema, {
      entries: [
        create(conversationv1.HistoryEntryAtSchema, {
          at: create(conversationv1.HistoryPointerSchema, { value: pointer }),
          entry: create(conversationv1.HistoryEntrySchema, {}),
        }),
      ],
      boundary: { case: "floor", value: create(conversationv1.HistoryFloorSchema, {}) },
    });
  }

  it("CONCLUDES an open tail through the book's head before the process ends", async () => {
    // The terminals are already durable, so the head names the last row the
    // consumer is owed; cutting the tail instead reads as a transport failure.
    const h = harness({ watcherConclusionBudgetMs: 25 });
    await started(h);
    h.persistence.page = pageWithHead("p-9");
    h.persistence.standingTail = true;
    const watching = h.engine
      .watchAgent(create(shimv1.WatchAgentRequestSchema, { pageSize: 5 }))[Symbol.asyncIterator]();
    await watching.next();

    await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    expect(h.persistence.concludedThrough).toContain("p-9");
  });

  it("VOUCHES for the agent when reading the head, so a book never written is not asked for", async () => {
    // Arrange. A session killed before its first turn has an agent with no book,
    // and asking the store for one earns an `unknown_agent` refusal on every
    // such teardown.
    const h = harness({ watcherConclusionBudgetMs: 25 });
    await started(h);
    h.persistence.standingTail = true;
    const watching = h.engine
      .watchAgent(create(shimv1.WatchAgentRequestSchema, { pageSize: 5 }))[Symbol.asyncIterator]();
    await watching.next();

    // Act.
    await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    // Assert. The head read carried the producer's own answer, and it holds.
    expect(h.persistence.lastKnownAgent?.()).toBe(true);
  });

  it("reads the book's head with the ONE-SHOT verb, so no watch token is minted for it", async () => {
    // Arrange. The head read stands no tail, and the store cannot learn that a
    // page was abandoned — OpenAgentSession is unary and there is no close — so
    // a reading session opened here would leave a token nothing ever spends.
    const h = harness({ watcherConclusionBudgetMs: 25 });
    await started(h);
    h.persistence.page = pageWithHead("p-9");
    h.persistence.standingTail = true;
    const watching = h.engine
      .watchAgent(create(shimv1.WatchAgentRequestSchema, { pageSize: 5 }))[Symbol.asyncIterator]();
    await watching.next();
    const openedBeforeTeardown = h.persistence.pagesOpened;

    // Act.
    await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    // Assert. The head arrived as a one-shot read, and the teardown opened no
    // further reading session.
    expect([h.persistence.firstPageReads, h.persistence.pagesOpened]).toEqual([
      1,
      openedBeforeTeardown,
    ]);
  });

  it("concludes NOTHING for a tail that already ended on its own", async () => {
    const h = harness({ watcherConclusionBudgetMs: 25 });
    await started(h);
    h.persistence.page = pageWithHead("p-9");
    const watching = h.engine.watchAgent(
      create(shimv1.WatchAgentRequestSchema, { pageSize: 5 }),
    );
    for await (const _frame of watching) {
      // drained to completion, which is what disposes the watcher
    }

    await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    expect(h.persistence.concludedThrough).toEqual([]);
  });

  it("finishes KillSession when a WatchBash stream never ends", async () => {
    // A LAST RESORT bound: a consumer that stopped pulling must not keep a
    // killed shim alive forever.
    const h = harness({ watcherConclusionBudgetMs: 25 });
    await started(h);
    h.persistence.openBashRun = () =>
      Promise.resolve({
        async *[Symbol.asyncIterator](): AsyncIterator<conversationv1.AgentBash> {
          await new Promise<void>(() => undefined);
        },
      });
    const watching = h.engine
      .watchBash(
        create(shimv1.WatchBashRequestSchema, {
          work: create(conversationv1.DetachedWorkIdSchema, { value: "toolu_never_ends" }),
        }),
      )[Symbol.asyncIterator]();
    void watching.next().catch(() => undefined);
    await new Promise((resolve) => setImmediate(resolve));

    const response = await h.engine.killSession(create(shimv1.KillSessionRequestSchema, { force: true }));

    expect(response.result.case === "success" ? response.result.value.closed?.how.case : undefined).toBe(
      "idle",
    );
  });

  it("continues the teardown when the vendor refuses the interrupt", async () => {
    const h = harness({
      onQueryCreated: (query) => {
        query.interrupt = () => Promise.reject(new Error("the vendor will not be interrupted"));
      },
    });
    await started(h);

    await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    expect(h.queries[0]?.query.calls).toContain("close");
  });

  it("continues the teardown when a detached item cannot be stopped", async () => {
    const h = harness({
      onQueryCreated: (query) => {
        query.stopTask = () => Promise.reject(new Error("the vendor cannot stop this task"));
      },
    });
    await started(h);
    await h.engine.onSdkMessage({
      type: "system",
      subtype: "task_started",
      task_id: "t02",
      tool_use_id: "toolu_unstoppable",
      task_type: "local_bash",
      description: "sleep 600",
      uuid: "00000000-0000-4000-8000-0000000000f4",
      session_id: "s",
    } as never);

    await h.engine.killSession(create(shimv1.KillSessionRequestSchema, { force: true }));

    expect(
      h.persistence.buffered.some((entry) => entry.upsertKey === "bash:toolu_unstoppable:terminal"),
    ).toBe(true);
  });
});

describe("ending the process the session was serving", () => {
  it("says so rather than exiting when this build has no process to end", async () => {
    const h = harness({ withoutEndProcess: true });
    await started(h);

    const response = await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    expect(response.result.case).toBe("success");
    expect(h.exits).toEqual([]);
  });

  it("stands down NONZERO when the store never acked some rows", async () => {
    const h = harness();
    await started(h);
    h.persistence.lostRows = 3;

    expect(await h.engine.standDown("SIGTERM")).toBe(1);
  });
});

describe("SetSessionModel and SetSessionPermissionMode, refused by the vendor", () => {
  it("answers a deferred model change with the vendor's refusal at the turn boundary", async () => {
    // The caller has been waiting for exactly this moment; swallowing the
    // refusal would leave the daemon believing a change landed that never did.
    const h = harness();
    await started(h);
    await h.engine.startTurn(
      create(shimv1.StartTurnRequestSchema, {
        turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }),
        said: textSaid("go"),
        origin: conversationv1.PromptOrigin.USER_SENT,
        pageSize: 5,
      }),
    );
    const query = h.queries[0]?.query;
    if (query !== undefined) query.setModelRejects = new Error("the vendor refuses that model");
    const deferred = h.engine.setSessionModel(
      create(shimv1.SetSessionModelRequestSchema, {
        model: create(conversationv1.AgentModelSchema, { name: "claude-haiku-4-5" }),
      }),
    );

    query?.emit(resultMessage("bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb"));
    const response = await deferred;

    expect(
      response.result.case === "failure" ? response.result.value.detail : undefined,
    ).toBe("the vendor refuses that model");
  });

  it("surfaces an immediate vendor refusal rather than reporting a model that is not in force", async () => {
    const h = harness();
    await started(h);
    const query = h.queries[0]?.query;
    if (query !== undefined) query.setModelRejects = new Error("the vendor refuses that model");

    const response = await h.engine.setSessionModel(
      create(shimv1.SetSessionModelRequestSchema, {
        model: create(conversationv1.AgentModelSchema, { name: "claude-haiku-4-5" }),
      }),
    );

    expect(
      response.result.case === "failure" ? response.result.value.cause.case : undefined,
    ).toBe("vendorRefused");
  });

  it("throws when SetSessionPermissionMode reaches the engine with no mode", async () => {
    const h = harness();
    await started(h);

    await expect(
      h.engine.setSessionPermissionMode(create(shimv1.SetSessionPermissionModeRequestSchema, {})),
    ).rejects.toThrow(/no mode/);
  });
});

describe("the prompt stream the engine hands the vendor", () => {
  it("DELIVERS a submitted prompt through the push-to-pull bridge", async () => {
    const delivered: string[] = [];
    const h = harness({ drainPrompts: delivered });
    await started(h);

    await h.engine.startTurn(
      create(shimv1.StartTurnRequestSchema, {
        turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }),
        said: textSaid("go"),
        origin: conversationv1.PromptOrigin.USER_SENT,
        pageSize: 5,
      }),
    );
    for (let attempt = 0; attempt < 50 && delivered.length === 0; attempt++) {
      await new Promise((resolve) => setImmediate(resolve));
    }

    expect(delivered).toEqual(["go"]);
  });

  it("ENDS the vendor's prompt stream when the session stands down", async () => {
    const delivered: string[] = [];
    const h = harness({ drainPrompts: delivered });
    await started(h);

    await h.engine.standDown("SIGTERM");
    for (let attempt = 0; attempt < 50 && !delivered.includes("<end>"); attempt++) {
      await new Promise((resolve) => setImmediate(resolve));
    }

    expect(delivered).toEqual(["<end>"]);
  });
});

describe("compaction when no model is in effect", () => {
  it("passes NO model to the throwaway query, leaving the SDK's own default", async () => {
    const h = harness({
      nowMs: 1_000_000 + 10 * 60 * 1000,
      onQueryCreated: (query, _spec, index) => {
        if (index === 0) query.emit(resultMessage("cccccccc-cccc-4ccc-8ccc-cccccccccccc"));
      },
    });
    // A transcript whose records never named a model: nothing to recover, so
    // nothing to ask the summarizing query for either.
    writeTranscript(h.configDir, h.cwd, "resume-1", [
      assistantLine({
        message: {
          usage: {
            input_tokens: 0,
            cache_creation_input_tokens: 0,
            cache_read_input_tokens: 500,
            cache_creation: { ephemeral_1h_input_tokens: 0, ephemeral_5m_input_tokens: 1 },
          },
        },
      }),
    ]);
    const remediation = create(conversationv1.SessionColdRemediationSchema, {
      remediation: {
        case: "compact",
        value: create(conversationv1.SessionColdCompactSchema, {
          scope: conversationv1.SessionCompactScope.ALL,
        }),
      },
    });
    const pending = h.engine.startSession(resumeRequest("resume-1", remediation));
    (await untilQuery(h, 1)).query.emit(initMessage({ sessionId: "resume-1" }));
    await pending;

    expect(h.queries[0]?.spec.model).toBeUndefined();
  });
});

describe("reconciliation against work the shim has already SEEN start", () => {
  it("re-adopts it without asking the vendor at all", async () => {
    // The live table already holds it, and a vendor round trip for something
    // this process watched begin would be asking about its own knowledge.
    const h = harness({
      onQueryCreated: (query, spec) => {
        query.emit(initMessage({ sessionId: spec.binding.kind === "fresh" ? spec.binding.sessionId : "" }));
        query.emit({
          type: "system",
          subtype: "task_started",
          task_id: "t10",
          tool_use_id: "toolu_seen",
          task_type: "local_bash",
          description: "sleep 600",
          uuid: "00000000-0000-4000-8000-000000000101",
          session_id: "s",
        } as never);
      },
    });
    h.persistence.live = create(storev1.GetLiveWorkSuccessSchema, {
      liveDetached: [create(conversationv1.DetachedWorkIdSchema, { value: "toolu_seen" })],
    });

    await h.engine.startSession(freshRequest());

    expect(h.queries[0]?.query.calls).not.toContain("backgroundTasks:toolu_seen");
    expect(
      h.persistence.buffered.some((entry) => entry.upsertKey === "bash:toolu_seen:terminal"),
    ).toBe(false);
  });
});

// ---------------------------------------------------------------------------
// The arms a first pass left open.
// ---------------------------------------------------------------------------

/** The context of the first record since `from` whose message carries `needle`. */
function logContextFor(from: number, needle: string): Record<string, unknown> | undefined {
  return logRecordsSince(from).find((record) => record.message.includes(needle))?.context;
}

/** The LEVEL of the first record since `from` whose message carries `needle`. */
function logLevelFor(from: number, needle: string): string | undefined {
  return logRecordsSince(from).find((record) => record.message.includes(needle))?.level;
}

/** Every fault detail the session's diagnostics carried while `act` ran. */
async function faultDetailsWhile(h: Harness, act: () => Promise<void>): Promise<string[]> {
  const seen = await pushedUpdates(
    h,
    (update) => (update.case === "diagnostics" ? update.value : undefined),
    act,
  );
  const details: string[] = [];
  for (const diagnostics of seen) {
    if (diagnostics.health.case !== "unhealthy") continue;
    for (const fault of diagnostics.health.value.faults) details.push(fault.detail);
  }
  return details;
}

/**
 * A vendor failure that is not an `Error`.
 *
 * THE SDK IS A FOREIGN BOUNDARY. Nothing makes a rejected vendor promise carry
 * an `Error`, and every place the engine states a cause reads
 * `err instanceof Error ? err.message : String(err)` for exactly that reason.
 * A rejection that is a bare string must reach the record and the wire whole,
 * not as "[object Object]" and not as an empty detail.
 */
describe("a vendor failure that is not an Error", () => {
  it("carries a bare-string getContextUsage rejection into the fault", async () => {
    const h = harness({
      onQueryCreated: (query) => {
        query.getContextUsage = () => Promise.reject("the context socket went away");
      },
    });

    const details = await faultDetailsWhile(h, async () => {
      await started(h);
    });

    expect(details).toContain("getContextUsage failed: the context socket went away");
  });

  it("carries a bare-string mcpServerStatus rejection into the fault", async () => {
    const h = harness({
      onQueryCreated: (query) => {
        query.mcpServerStatus = () => Promise.reject("the mcp probe went away");
      },
    });

    const details = await faultDetailsWhile(h, async () => {
      await started(h);
    });

    expect(details).toContain("mcpServerStatus failed: the mcp probe went away");
  });

  it("carries a bare-string usage rejection into the sampling failure", async () => {
    const h = harness({
      onQueryCreated: (query) => {
        query.usage_EXPERIMENTAL_MAY_CHANGE_DO_NOT_RELY_ON_THIS_API_YET = () =>
          Promise.reject("the usage endpoint went away");
      },
    });
    const seen = await pushedUpdates(
      h,
      (update) => (update.case === "accountUsage" ? update.value : undefined),
      async () => {
        await started(h);
      },
    );
    const outcome = seen[0]?.outcome;

    expect(
      outcome?.case === "unavailable" && outcome.value.reason.case === "samplingFailure"
        ? outcome.value.reason.value.cause
        : undefined,
    ).toBe("the usage endpoint went away");
  });

  it("carries a bare-string supportedModels rejection into the fault", async () => {
    // THE SECOND READ, not the first: the first IS the start's live signal, and
    // a bare-string rejection there is carried by the START's refusal instead
    // (its own test). This is the pulled-fact path, where the same string has
    // to reach the fault whole.
    let reads = 0;
    const h = harness({
      onQueryCreated: (query) => {
        query.supportedModels = async (): Promise<ModelInfoLike[]> => {
          reads += 1;
          if (reads === 1) return query.models;
          throw "the catalog endpoint went away";
        };
      },
    });
    await started(h);

    const details = await faultDetailsWhile(h, async () => {
      await h.engine.startTurn(
        create(shimv1.StartTurnRequestSchema, {
          turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }),
          said: textSaid("go"),
          origin: conversationv1.PromptOrigin.USER_SENT,
          pageSize: 5,
        }),
      );
      h.queries[0]?.query.emit(resultMessage());
      // The re-probe rides the turn's end asynchronously; the collector must
      // still be reading when its fault lands.
      await vi.waitFor(() => {
        expect(reads).toBe(2);
      });
    });

    expect(details).toContain("supportedModels failed: the catalog endpoint went away");
  });

  it("carries a bare-string query-creation rejection into the StartSession refusal", async () => {
    const h = harness({ createQueryRejection: "the vendor binary is not on this box" });

    const response = await h.engine.startSession(freshRequest());

    expect(
      response.result.case === "failure" ? response.result.value.detail : undefined,
    ).toBe("the vendor binary is not on this box");
  });

  it("raises a SESSION claim failure that names no owner rather than answering conversation_owned", async () => {
    // Only a holder that found the lock held (LockHeldError) is a genuine
    // owner; anything else the claim throws is not a refusal the contract names.
    const h = harness({ lockRefusal: "the lock directory is gone" });

    await expect(h.engine.startSession(freshRequest())).rejects.toBe("the lock directory is gone");
  });

  it("raises a WORKSPACE claim failure that names no owner rather than answering conversation_owned", async () => {
    const h = harness({ workspaceLockRefusal: "the lock directory is gone" });

    await expect(h.engine.startSession(freshRequest())).rejects.toBe("the lock directory is gone");
  });

  it("carries a bare-string setModel rejection into the immediate refusal", async () => {
    const h = harness();
    await started(h);
    const query = h.queries[0]?.query;
    if (query !== undefined) query.setModel = () => Promise.reject("the model service went away");

    const response = await h.engine.setSessionModel(
      create(shimv1.SetSessionModelRequestSchema, {
        model: create(conversationv1.AgentModelSchema, { name: "claude-haiku-4-5" }),
      }),
    );

    expect(
      response.result.case === "failure" ? response.result.value.detail : undefined,
    ).toBe("the model service went away");
  });

  it("carries a bare-string setModel rejection into the deferred refusal", async () => {
    const h = harness();
    await started(h);
    await h.engine.startTurn(
      create(shimv1.StartTurnRequestSchema, {
        turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }),
        said: textSaid("go"),
        origin: conversationv1.PromptOrigin.USER_SENT,
        pageSize: 5,
      }),
    );
    const query = h.queries[0]?.query;
    if (query !== undefined) query.setModel = () => Promise.reject("the model service went away");
    const deferred = h.engine.setSessionModel(
      create(shimv1.SetSessionModelRequestSchema, {
        model: create(conversationv1.AgentModelSchema, { name: "claude-haiku-4-5" }),
      }),
    );

    query?.emit(resultMessage("cccccccc-cccc-4ccc-8ccc-cccccccccccc"));
    const response = await deferred;

    expect(
      response.result.case === "failure" ? response.result.value.detail : undefined,
    ).toBe("the model service went away");
  });

  it("carries a bare-string setPermissionMode rejection into the refusal", async () => {
    const h = harness();
    await started(h);
    const query = h.queries[0]?.query;
    if (query !== undefined) {
      query.setPermissionMode = () => Promise.reject("the permission service went away");
    }

    const response = await h.engine.setSessionPermissionMode(
      create(shimv1.SetSessionPermissionModeRequestSchema, {
        permissionMode: create(conversationv1.AgentPermissionModeSchema, {
          mode: { case: "plan", value: create(conversationv1.AgentPermissionModePlanSchema, {}) },
        }),
      }),
    );

    expect(
      response.result.case === "failure" ? response.result.value.detail : undefined,
    ).toBe("the permission service went away");
  });

  it("carries a bare-string iterator failure into query_died", async () => {
    const h = harness();
    const seen = await pushedUpdates(
      h,
      (update) => (update.case === "queryDied" ? update.value : undefined),
      async () => {
        await started(h);
        // The loop is PARKED in the iterator; a stream that merely ends under
        // a parked reader is an EOF, so a message unparks it first.
        h.queries[0]?.query.emit(assistantMessage("00000000-0000-4000-8000-0000000000f1"));
        h.queries[0]?.query.fail("the vendor stream went away" as unknown as Error);
        for (let attempt = 0; attempt < 50; attempt++) {
          await new Promise((resolve) => setImmediate(resolve));
        }
      },
    );
    const cause = seen[0]?.cause;

    expect(cause?.case === "iteratorFailure" ? cause.value.cause : undefined).toBe(
      "the vendor stream went away",
    );
  });

  it("carries a bare-string GetLiveWork rejection into the fault", async () => {
    const h = harness();
    h.persistence.liveWork = () => Promise.reject("the store socket went away");

    const details = await faultDetailsWhile(h, async () => {
      await started(h);
    });

    expect(details).toContain("GetLiveWork failed: the store socket went away");
  });

  it("logs a bare-string book-read rejection while reconciling", async () => {
    const h = harness();
    h.persistence.live = create(storev1.GetLiveWorkSuccessSchema, {
      liveDetached: [create(conversationv1.DetachedWorkIdSchema, { value: "b01" })],
    });
    h.persistence.readFirstPage = () => Promise.reject("the store socket went away");
    const before = logSinkMark();

    await started(h);

    expect(logContextFor(before, "could not be read for reconciliation")?.cause).toBe(
      "the store socket went away",
    );
  });

  it("carries a bare-string keep-alive row failure into the fault", async () => {
    const h = harness();
    const details = await faultDetailsWhile(h, async () => {
      await started(h);
      const recorded = h.persistence.write.bind(h.persistence);
      h.persistence.write = (): void => {
        throw "the record plane went away";
      };
      const before = h.engine.pushes.faultCount;
      h.scheduler.fire(0);
      for (let attempt = 0; attempt < 50 && h.engine.pushes.faultCount === before; attempt++) {
        await new Promise((resolve) => setImmediate(resolve));
      }
      // Restored before the stand-down, which writes the shutdown terminal
      // through this same channel.
      h.persistence.write = recorded;
    });

    expect(details).toContain("the record plane went away");
  });

  it("carries a bare-string book-read rejection into the re-announcement fault", async () => {
    const h = harness();
    h.persistence.live = create(storev1.GetLiveWorkSuccessSchema, {
      liveDetached: [create(conversationv1.DetachedWorkIdSchema, { value: "b01" })],
    });
    const details = await faultDetailsWhile(h, async () => {
      await started(h);
      h.persistence.readFirstPage = () => Promise.reject("the store socket went away");
      const watching = h.engine
        .watchSession(create(shimv1.WatchSessionRequestSchema, {}))[Symbol.asyncIterator]();
      await watching.next();
      await watching.next();
      await watching.return?.();
    });

    expect(details).toContain(
      "re-announcing live work for a new WatchSession failed: the store socket went away",
    );
  });

  it("logs a bare-string interrupt rejection during the teardown", async () => {
    const h = harness({
      onQueryCreated: (query) => {
        query.interrupt = () => Promise.reject("the vendor went away");
      },
    });
    await started(h);
    const before = logSinkMark();

    await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    expect(logContextFor(before, "refused the interrupt during teardown")?.cause).toBe(
      "the vendor went away",
    );
  });

  it("logs a bare-string stopTask rejection during the teardown", async () => {
    const h = harness({
      onQueryCreated: (query) => {
        query.stopTask = () => Promise.reject("the vendor went away");
      },
    });
    await started(h);
    await h.engine.onSdkMessage({
      type: "system",
      subtype: "task_started",
      task_id: "t09",
      tool_use_id: "toolu_unstoppable",
      task_type: "local_bash",
      description: "sleep 600",
      uuid: "00000000-0000-4000-8000-0000000000f9",
      session_id: "s",
    } as never);
    const before = logSinkMark();

    await h.engine.killSession(create(shimv1.KillSessionRequestSchema, { force: true }));

    expect(logContextFor(before, "could not stop a detached item during teardown")?.cause).toBe(
      "the vendor went away",
    );
  });

  it("logs a bare-string head-read rejection while concluding a watcher", async () => {
    const h = harness({ watcherConclusionBudgetMs: 25 });
    await started(h);
    h.persistence.standingTail = true;
    const watching = h.engine
      .watchAgent(create(shimv1.WatchAgentRequestSchema, { pageSize: 5 }))[Symbol.asyncIterator]();
    await watching.next();
    h.persistence.readFirstPage = () => Promise.reject("the store socket went away");
    const before = logSinkMark();

    await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    expect(
      logContextFor(before, "could not read a book's head while concluding its watcher")?.cause,
    ).toBe("the store socket went away");
  });

  it("carries a bare-string throwaway-query rejection into the cold refusal", async () => {
    const h = harness({
      nowMs: 1_000_000 + 10 * 60 * 1000,
      createQueryRejection: "the vendor binary is not on this box",
    });
    writeTranscript(h.configDir, h.cwd, "resume-1", [assistantLine()]);

    const response = await h.engine.startSession(
      resumeRequest(
        "resume-1",
        create(conversationv1.SessionColdRemediationSchema, {
          remediation: {
            case: "compact",
            value: create(conversationv1.SessionColdCompactSchema, {
              scope: conversationv1.SessionCompactScope.ALL,
            }),
          },
        }),
      ),
    );

    expect(
      response.result.case === "failure" ? response.result.value.detail : undefined,
    ).toBe("the vendor binary is not on this box");
  });
});

/**
 * The push-to-pull bridge when the vendor is not already parked on it.
 *
 * `drainPrompts` pulls continuously, so the queue's own buffer is never used;
 * a real SDK pulls when it is ready, and what it is handed then is what these
 * cover.
 */
describe("the prompt queue's own buffer", () => {
  it("hands over a prompt submitted before the vendor pulled", async () => {
    const h = harness();
    await started(h);
    const spec = h.queries[0]?.spec;
    if (spec === undefined) throw new Error("no query");
    const prompts = spec.prompt[Symbol.asyncIterator]();

    await h.engine.startTurn(
      create(shimv1.StartTurnRequestSchema, {
        turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }),
        said: textSaid("go"),
        origin: conversationv1.PromptOrigin.USER_SENT,
        pageSize: 5,
      }),
    );
    const first = await nextPush(prompts);

    expect(first.message.content).toBe("go");
  });

  it("completes for a vendor that pulls only after the stand-down closed it", async () => {
    const h = harness();
    await started(h);
    const spec = h.queries[0]?.spec;
    if (spec === undefined) throw new Error("no query");
    const prompts = spec.prompt[Symbol.asyncIterator]();

    await h.engine.standDown("SIGTERM");

    expect((await prompts.next()).done).toBe(true);
  });
});

describe("the claims a build that injects none falls back to", () => {
  it("serves shim.v1 without taking either kernel claim before a session exists", async () => {
    // An INERT shim holds neither lock, which is what lets a prelaunched
    // replacement sit beside the live shim it is about to replace.
    const h = harness({ withoutLockInjection: true });

    const response = await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    expect(
      response.result.case === "failure" ? response.result.value.cause.case : undefined,
    ).toBe("noSession");
  });
});

describe("the cadence a build that injects no scheduler beats on", () => {
  it("starts and stops a session on the real scheduler", async () => {
    const h = harness({ withoutScheduler: true });
    await started(h);

    const response = await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    expect(
      response.result.case === "success" ? response.result.value.closed?.how.case : undefined,
    ).toBe("idle");
  });
});

describe("whose book a gated ask lands on", () => {
  /** Raise one ask and let the stand-down resolve it as denied. */
  const askUnder = async (h: Harness, agentID: string): Promise<void> => {
    const spec = h.queries[0]?.spec;
    if (spec === undefined) throw new Error("no query");
    const pending = spec.canUseTool("Bash", {}, {
      signal: new AbortController().signal,
      toolUseID: "toolu_asked",
      agentID,
      requestId: "req_1",
    });
    await Promise.resolve();
    await h.engine.standDown("SIGTERM");
    await pending;
  };

  /** The book the gate's permission frame landed on. */
  const bookOf = (h: Harness): string | undefined => {
    for (const entry of h.persistence.buffered) {
      if (entry.item.kind !== "frame") continue;
      const result = entry.item.frame.result;
      if (result.case !== "update") continue;
      if (result.value.update.case !== "permission") continue;
      return entry.agentId?.value;
    }
    return undefined;
  };

  it("takes the vendor at its word when it names the MAIN agent", async () => {
    const h = harness();
    await started(h);
    const sessionId =
      h.queries[0]?.spec.binding.kind === "fresh" ? h.queries[0].spec.binding.sessionId : "";
    const before = logSinkMark();

    await askUnder(h, mainAgentId(sessionId).value);

    expect(logContextFor(before, "an agent this session never announced")).toBeUndefined();
  });

  it("books an ask under a subagent this session ANNOUNCED, task table or not", async () => {
    // A SYNCHRONOUS subagent runs inside the turn and is never a task, so the
    // live table never holds it -- the announcement is the only record.
    const h = harness();
    await started(h);
    h.fold.entriesFor = (message) =>
      message.type === "assistant"
        ? [
            activityEntry(
              "toolu_spawn",
              {
                case: "subagent",
                value: create(conversationv1.AgentSubagentSchema, {
                  result: {
                    case: "start",
                    value: create(conversationv1.AgentSubagentStartSchema, {
                      createdAgentId: create(conversationv1.AgentIdSchema, { value: "agent-child" }),
                    }),
                  },
                }),
              },
              "subagent_start",
            ),
          ]
        : [];
    await h.engine.onSdkMessage(assistantMessage("00000000-0000-4000-8000-000000000101"));
    h.persistence.buffered.length = 0;

    await askUnder(h, "agent-child");

    expect(bookOf(h)).toBe("agent-child");
  });

  it("flags an ask raised inside a KEEP-ALIVE turn as keep-alive", async () => {
    // A keep-alive turn's rows are recorded and never served, and the gate's
    // own rows are no exception.
    const h = harness();
    await started(h);
    h.scheduler.fire(0);
    await new Promise((resolve) => setImmediate(resolve));
    // THE ASK FOLLOWS THE ASSISTANT MESSAGE THAT CALLED THE TOOL, and that
    // message is the keep-alive turn's first reply — stamped with its send.
    await h.engine.onSdkMessage(answering(h, assistantMessage("keepalive-tool-call-uuid")));
    h.persistence.buffered.length = 0;

    await askUnder(h, "");

    expect(
      h.persistence.buffered.find((entry) => entry.source.discriminator.includes("permission"))
        ?.keepalive,
    ).toBe(true);
  });
});

describe("what the fold is told about a live task", () => {
  it("answers an empty originating call for a task that named none", async () => {
    // Tracked for liveness, addressable by nobody: absence would read as "no
    // such task", which is a different fact.
    const h = harness();
    await started(h);
    await h.engine.onSdkMessage({
      type: "system",
      subtype: "task_started",
      task_id: "t20",
      task_type: "local_bash",
      description: "sleep 600",
      uuid: "00000000-0000-4000-8000-000000000102",
      session_id: "s",
    } as never);

    await h.engine.onSdkMessage(assistantMessage("00000000-0000-4000-8000-000000000103"));

    expect(h.fold.contexts.at(-1)?.liveTask("t20")).toEqual({ toolUseId: "", taskType: "local_bash" });
  });

  it("names the turn a task started inside", async () => {
    const h = harness();
    await started(h);
    await h.engine.startTurn(
      create(shimv1.StartTurnRequestSchema, {
        turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }),
        said: textSaid("go"),
        origin: conversationv1.PromptOrigin.USER_SENT,
        pageSize: 5,
      }),
    );
    const before = logSinkMark();

    await h.engine.onSdkMessage({
      type: "system",
      subtype: "task_started",
      task_id: "t21",
      tool_use_id: "toolu_bg",
      task_type: "local_bash",
      description: "sleep 600",
      uuid: "00000000-0000-4000-8000-000000000104",
      session_id: "s",
    } as never);

    expect(logContextFor(before, "recorded a live detached-work item")?.turn_id).toBe("turn-1");
  });
});

describe("the vendor answers the engine maps around an absent field", () => {
  it("leaves an mcp tool the vendor said nothing about UNSTATED, not false", async () => {
    const h = harness({
      onQueryCreated: (query) => {
        query.contextUsage = {
          ...query.contextUsage,
          mcpTools: [{ name: "search", serverName: "docs", tokens: 9 }],
        };
      },
    });
    const seen = await pushedUpdates(
      h,
      (update) => (update.case === "contextUsage" ? update.value : undefined),
      async () => {
        await started(h);
      },
    );

    expect(seen[0]?.mcpTools[0]?.isLoaded).toBeUndefined();
  });

  it("states no per-model windows when the account named none", async () => {
    const h = harness({
      accountUsage: {
        session: {
          total_cost_usd: 0,
          total_api_duration_ms: 0,
          total_duration_ms: 0,
          total_lines_added: 0,
          total_lines_removed: 0,
          model_usage: {},
        },
        subscription_type: "max",
        rate_limits_available: true,
        rate_limits: { five_hour: { utilization: 10, resets_at: "2026-01-01T00:00:00.000Z" } },
        behaviors: null,
      } as unknown as AccountUsageLike,
    });
    const seen = await pushedUpdates(
      h,
      (update) => (update.case === "accountUsage" ? update.value : undefined),
      async () => {
        await started(h);
      },
    );
    const outcome = seen[0]?.outcome;

    expect(outcome?.case === "available" ? outcome.value.modelScoped : undefined).toEqual([]);
  });

  it("states an empty error for a failed server the vendor gave no words for", async () => {
    const h = harness({ mcp: [{ name: "docs", status: "failed" }] as McpServerStatusLike[] });
    const seen = await pushedUpdates(
      h,
      (update) => (update.case === "mcpServer" ? update.value.health : undefined),
      async () => {
        await started(h);
      },
    );
    const health = seen[0];

    expect(health?.case === "failed" ? health.value.error : undefined).toBe("");
  });

  it("states NO effort levels for a model that supports effort and lists none", async () => {
    const h = harness({
      onQueryCreated: (query) => {
        query.models = [{ value: "m", displayName: "M", description: "d", supportsEffort: true }];
      },
    });
    const response = await started(h);
    const support =
      response.result.case === "success"
        ? response.result.value.session?.modelCatalog[0]?.capabilities?.effortSupport
        : undefined;

    expect(support?.case === "effortSupported" ? support.value.levels : undefined).toEqual([]);
  });

  it("fans out a fold update whose arm is not set at all", async () => {
    const h = harness();
    const arms = await pushedUpdates(
      h,
      (update) => (update.case === undefined ? "unset" : undefined),
      async () => {
        await started(h);
        h.fold.entriesFor = (message) =>
          message.type === "assistant"
            ? [foldEntry({ kind: "session_update", update: create(conversationv1.SessionUpdateSchema, {}) }, "unset_arm")]
            : [];
        await h.engine.onSdkMessage(assistantMessage("00000000-0000-4000-8000-000000000105"));
      },
    );

    expect(arms).toContain("unset");
  });

  it("names no previous id when a reset arrives before the session has one", async () => {
    const h = harness();
    const before = logSinkMark();

    // The fold refuses to key rows before an identity exists, so the message
    // does not survive the call -- but the reset is noted before that point.
    await expect(
      h.engine.onSdkMessage({
        type: "conversation_reset",
        new_conversation_id: "33333333-3333-4333-8333-333333333333",
        uuid: "00000000-0000-4000-8000-000000000106",
        session_id: "s",
      } as never),
    ).rejects.toThrow(/identity is not established/);

    expect(logContextFor(before, "the vendor reset the conversation")?.previous).toBe("");
  });
});

describe("the fold rows the engine walks past", () => {
  it("records a terminal frame without tracking it as a unit in flight", async () => {
    const h = harness();
    await started(h);
    h.fold.entriesFor = (message) =>
      message.type === "assistant"
        ? [
            foldEntry(
              {
                kind: "frame",
                frame: create(conversationv1.AgentFrameSchema, {
                  result: {
                    case: "success",
                    value: create(conversationv1.AgentSuccessSchema, {}),
                  },
                }),
              },
              "agent_success",
            ),
          ]
        : [];

    await h.engine.onSdkMessage(assistantMessage("00000000-0000-4000-8000-000000000107"));

    expect(h.persistence.buffered.some((entry) => entry.upsertKey === "k-agent_success")).toBe(true);
  });

  it("records a frame update that is neither a permission nor an activity", async () => {
    const h = harness();
    await started(h);
    h.fold.entriesFor = (message) =>
      message.type === "assistant"
        ? [
            foldEntry(
              {
                kind: "frame",
                frame: create(conversationv1.AgentFrameSchema, {
                  result: {
                    case: "update",
                    value: create(conversationv1.AgentUpdateSchema, {
                      update: {
                        case: "contextCut",
                        value: create(conversationv1.ContextCutSchema, {}),
                      },
                    }),
                  },
                }),
              },
              "context_cut",
            ),
          ]
        : [];

    await h.engine.onSdkMessage(assistantMessage("00000000-0000-4000-8000-000000000108"));

    expect(h.persistence.buffered.some((entry) => entry.upsertKey === "k-context_cut")).toBe(true);
  });

  it("remembers no denial for a fold denial that named no gated call", async () => {
    const h = harness();
    await started(h);
    h.fold.entriesFor = (message) =>
      message.type === "assistant"
        ? [
            foldEntry(
              {
                kind: "frame",
                frame: create(conversationv1.AgentFrameSchema, {
                  result: {
                    case: "update",
                    value: create(conversationv1.AgentUpdateSchema, {
                      update: {
                        case: "permission",
                        value: create(conversationv1.AgentPermissionSchema, {
                          result: {
                            case: "success",
                            value: create(conversationv1.AgentPermissionSuccessSchema, {
                              decision: {
                                case: "denied",
                                value: create(conversationv1.AgentPermissionDeniedSchema, {
                                  by: {
                                    case: "policy",
                                    value: create(
                                      conversationv1.AgentPermissionDeniedByPolicySchema,
                                      {},
                                    ),
                                  },
                                }),
                              },
                            }),
                          },
                        }),
                      },
                    }),
                  },
                }),
              },
              "permission_denied_unnamed",
            ),
          ]
        : [];

    await h.engine.onSdkMessage(assistantMessage("00000000-0000-4000-8000-000000000109"));
    await h.engine.onSdkMessage(assistantMessage("00000000-0000-4000-8000-00000000010a"));

    // The empty handle addresses nothing, and inventing one would make the
    // next tool_result read as a relayed deny of a call that never happened.
    expect(h.fold.contexts.at(-1)?.deniedCall("")).toBe(false);
  });

  it("ANNOUNCES nothing for a spawn that named no created agent", async () => {
    const h = harness();
    await started(h);
    h.fold.entriesFor = (message) =>
      message.type === "assistant"
        ? [
            activityEntry(
              "toolu_spawn",
              {
                case: "subagent",
                value: create(conversationv1.AgentSubagentSchema, {
                  result: {
                    case: "start",
                    value: create(conversationv1.AgentSubagentStartSchema, {}),
                  },
                }),
              },
              "subagent_start_unnamed",
            ),
          ]
        : [];
    await h.engine.onSdkMessage(assistantMessage("00000000-0000-4000-8000-00000000010b"));

    const iterator = h.engine
      .watchAgent(
        create(shimv1.WatchAgentRequestSchema, {
          target: create(conversationv1.AgentIdSchema, { value: "agent-child" }),
          pageSize: 5,
        }),
      )[Symbol.asyncIterator]();

    await expect(iterator.next()).rejects.toThrow(/no agent by that id/);
  });

  it("an activity naming no unit leaves nothing to detach: a detach naming none is invalid input", async () => {
    const h = harness();
    await started(h);
    h.fold.entriesFor = (message) =>
      message.type === "assistant"
        ? [
            foldEntry(
              {
                kind: "frame",
                frame: create(conversationv1.AgentFrameSchema, {
                  result: {
                    case: "update",
                    value: create(conversationv1.AgentUpdateSchema, {
                      update: {
                        case: "activity",
                        value: create(conversationv1.AgentActivitySchema, {
                          item: {
                            case: "bash",
                            value: create(conversationv1.AgentBashSchema, {
                              result: {
                                case: "start",
                                value: create(conversationv1.AgentBashStartSchema, {}),
                              },
                            }),
                          },
                        }),
                      },
                    }),
                  },
                }),
              },
              "unnamed_activity",
            ),
          ]
        : [];
    await h.engine.onSdkMessage(assistantMessage("00000000-0000-4000-8000-00000000010c"));

    // An empty unit is invalid input, refused before the vendor is asked (the
    // table's own refusal to track "" is foreground.test.ts's subject).
    const refused = h.engine.detachForeground(
      create(shimv1.DetachForegroundRequestSchema, {
        unit: create(conversationv1.AgentActivityIdSchema, {}),
      }),
    );

    await expect(refused).rejects.toMatchObject({ code: Code.InvalidArgument });
    expect(h.queries[0]?.query.calls.filter((call) => call.startsWith("backgroundTasks"))).toEqual([]);
  });

  it("settles an activity that names a unit but no kind", async () => {
    const h = harness();
    await started(h);
    h.fold.entriesFor = (message) =>
      message.type === "assistant"
        ? [
            foldEntry(
              {
                kind: "frame",
                frame: create(conversationv1.AgentFrameSchema, {
                  result: {
                    case: "update",
                    value: create(conversationv1.AgentUpdateSchema, {
                      update: {
                        case: "activity",
                        value: create(conversationv1.AgentActivitySchema, {
                          activityId: create(conversationv1.AgentActivityIdSchema, {
                            value: "toolu_kindless",
                          }),
                        }),
                      },
                    }),
                  },
                }),
              },
              "kindless_activity",
            ),
          ]
        : [];
    await h.engine.onSdkMessage(assistantMessage("00000000-0000-4000-8000-00000000010d"));

    const response = await h.engine.detachForeground(
      create(shimv1.DetachForegroundRequestSchema, {
        unit: create(conversationv1.AgentActivityIdSchema, { value: "toolu_kindless" }),
      }),
    );

    expect(
      response.result.case === "failure" ? response.result.value.kind.case : undefined,
    ).toBe("alreadyConcluded");
  });
});

describe("a DetachForeground reaches the fold as the user's request", () => {
  const detachUnit = (unit: string): shimv1.DetachForegroundRequest =>
    create(shimv1.DetachForegroundRequestSchema, {
      unit: create(conversationv1.AgentActivityIdSchema, { value: unit }),
    });

  it("notes the unit with the fold when the vendor moved it", async () => {
    // Arrange
    const h = harness({ backgroundTasks: true });
    await started(h);

    // Act
    await h.engine.detachForeground(detachUnit("toolu_moved"));

    // Assert
    expect([h.fold.userDetaches, h.fold.retiredUserDetaches]).toEqual([["toolu_moved"], []]);
  });

  it("retires the unit with the fold when the vendor moved nothing", async () => {
    // Arrange
    const h = harness();
    await started(h);

    // Act
    await h.engine.detachForeground(detachUnit("toolu_unmoved"));

    // Assert
    expect(h.fold.retiredUserDetaches).toEqual([
      { toolUseId: "toolu_unmoved", why: "the vendor moved nothing for the request" },
    ]);
  });
});

describe("the session's own beats once the vendor query is gone", () => {
  /** Bring a session up and then lose its query the way a broken stream does. */
  const withDeadQuery = async (): Promise<Harness> => {
    const h = harness();
    await started(h);
    h.queries[0]?.query.emit(assistantMessage("00000000-0000-4000-8000-00000000010e"));
    h.queries[0]?.query.fail(new Error("the vendor stream broke"));
    for (let attempt = 0; attempt < 50; attempt++) {
      await new Promise((resolve) => setImmediate(resolve));
    }
    h.persistence.buffered.length = 0;
    return h;
  };

  it("submits no keep-alive prompt once there is nothing to submit to", async () => {
    const h = await withDeadQuery();

    h.scheduler.fire(0);
    await new Promise((resolve) => setImmediate(resolve));

    expect(h.persistence.buffered.filter((entry) => entry.item.kind === "prompt")).toEqual([]);
  });

  it("samples no account usage once there is nothing to sample", async () => {
    const h = await withDeadQuery();
    const before = h.queries[0]?.query.calls.filter((call) => call === "usage").length ?? 0;

    h.scheduler.fire(1);
    await new Promise((resolve) => setImmediate(resolve));

    expect(h.queries[0]?.query.calls.filter((call) => call === "usage").length).toBe(before);
  });

  it("accepts a model change without asking a vendor that is gone", async () => {
    const h = await withDeadQuery();
    const before = h.queries[0]?.query.calls.filter((call) => call.startsWith("setModel")).length ?? 0;
    await h.engine.setSessionModel(
      create(shimv1.SetSessionModelRequestSchema, {
        model: create(conversationv1.AgentModelSchema, { name: "claude-haiku-4-5" }),
      }),
    );

    expect(h.queries[0]?.query.calls.filter((call) => call.startsWith("setModel")).length).toBe(before);
  });
});

describe("the record a dead vendor leaves", () => {
  /**
   * Bring a session up, let its child say something and end, then break the
   * stream — the order a real death arrives in.
   */
  async function died(options: { said: string; code: number | null; signal: string | null }): Promise<void> {
    const h = harness();
    await started(h);
    const child = h.queries[0];
    child?.spec.onStderr?.(options.said);
    child?.spec.onChildExit?.({ code: options.code, signal: options.signal });
    child?.query.fail(new Error("ProcessTransport is not ready for writing"));
    for (let attempt = 0; attempt < 50; attempt++) {
      await new Promise((resolve) => setImmediate(resolve));
    }
  }

  it("names the cause the stream reported", async () => {
    const before = logSinkMark();

    await died({ said: "", code: 1, signal: null });

    expect(logContextFor(before, "the vendor query is gone")?.cause).toBe(
      "ProcessTransport is not ready for writing",
    );
  });

  it("carries the child's exit code", async () => {
    // THE FACT THAT WAS MISSING. A vendor died on 2026-09-14 and the immediate
    // cause could not be recovered from any log afterwards.
    const before = logSinkMark();

    await died({ said: "", code: 137, signal: null });

    expect(logContextFor(before, "the vendor query is gone")?.vendor_exit_code).toBe(137);
  });

  it("carries the signal that killed the child", async () => {
    const before = logSinkMark();

    await died({ said: "", code: null, signal: "SIGKILL" });

    expect(logContextFor(before, "the vendor query is gone")?.vendor_exit_signal).toBe("SIGKILL");
  });

  it("carries the last words the child wrote to stderr", async () => {
    const before = logSinkMark();

    await died({ said: "out of memory\n", code: 137, signal: null });

    expect(logContextFor(before, "the vendor query is gone")?.vendor_stderr).toBe("out of memory");
  });

  it("distinguishes a child that never ended from one that exited zero", async () => {
    // -1 AND NOT AN OMITTED FIELD. A field that disappears makes "the child
    // exited 0" and "no child ever ended" the same record, and those are
    // opposite diagnoses.
    const h = harness();
    await started(h);
    const before = logSinkMark();
    h.queries[0]?.query.fail(new Error("the vendor stream broke"));
    for (let attempt = 0; attempt < 50; attempt++) {
      await new Promise((resolve) => setImmediate(resolve));
    }

    expect(logContextFor(before, "the vendor query is gone")?.vendor_exit_code).toBe(-1);
  });

  it("records the death at ERROR", async () => {
    const before = logSinkMark();

    await died({ said: "", code: 1, signal: null });

    expect(logLevelFor(before, "the vendor query is gone")).toBe("error");
  });
});

describe("SetSessionModel's remaining arms", () => {
  it("throws when it reaches the engine with no model at all", async () => {
    const h = harness();
    await started(h);

    await expect(
      h.engine.setSessionModel(create(shimv1.SetSessionModelRequestSchema, {})),
    ).rejects.toThrow(/no model/);
  });

  it("tells the first caller when a later change replaced it mid-turn", async () => {
    // Leaving the first caller holding a promise nothing will settle is worse
    // than telling it the change did not land.
    const h = harness();
    await started(h);
    await h.engine.startTurn(
      create(shimv1.StartTurnRequestSchema, {
        turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }),
        said: textSaid("go"),
        origin: conversationv1.PromptOrigin.USER_SENT,
        pageSize: 5,
      }),
    );
    const first = h.engine.setSessionModel(
      create(shimv1.SetSessionModelRequestSchema, {
        model: create(conversationv1.AgentModelSchema, { name: "claude-haiku-4-5" }),
      }),
    );
    await Promise.resolve();
    const second = h.engine.setSessionModel(
      create(shimv1.SetSessionModelRequestSchema, {
        model: create(conversationv1.AgentModelSchema, { name: "claude-sonnet-5" }),
      }),
    );
    h.queries[0]?.query.emit(resultMessage("dddddddd-dddd-4ddd-8ddd-dddddddddddd"));
    const response = await first;
    await second;

    expect(
      response.result.case === "failure" ? response.result.value.detail : undefined,
    ).toBe("a later SetSessionModel replaced this one before the turn ended");
  });
});

describe("KillSession refused over live work alone", () => {
  it("says there is NO turn in flight when only detached work is live", async () => {
    const h = harness();
    await started(h);
    await h.engine.onSdkMessage({
      type: "system",
      subtype: "task_started",
      task_id: "t30",
      tool_use_id: "toolu_live",
      task_type: "local_bash",
      description: "sleep 600",
      uuid: "00000000-0000-4000-8000-00000000010f",
      session_id: "s",
    } as never);

    const response = await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    expect(
      response.result.case === "failure" ? response.result.value.detail : undefined,
    ).toBe("the session has no turn in flight and 1 live item(s)");
  });
});

describe("the cold gate's compact remediation, in detail", () => {
  /** A cold resume, remediated by compaction under `remediation`. */
  const compactWith = (
    h: Harness,
    remediation: conversationv1.SessionColdRemediation,
  ): Promise<shimv1.StartSessionResponse> => {
    writeTranscript(h.configDir, h.cwd, "resume-1", [assistantLine()]);
    return h.engine.startSession(resumeRequest("resume-1", remediation));
  };

  it("summarizes on the model the remediation named", async () => {
    const h = harness({
      nowMs: 1_000_000 + 10 * 60 * 1000,
      onQueryCreated: (query, _spec, index) => {
        if (index === 0) query.emit(resultMessage("eeeeeeee-eeee-4eee-8eee-eeeeeeeeeeee"));
      },
    });
    const pending = compactWith(
      h,
      create(conversationv1.SessionColdRemediationSchema, {
        remediation: {
          case: "compact",
          value: create(conversationv1.SessionColdCompactSchema, {
            scope: conversationv1.SessionCompactScope.ALL,
            model: create(conversationv1.AgentModelSchema, { name: "claude-haiku-4-5" }),
          }),
        },
      }),
    );
    (await untilQuery(h, 1)).query.emit(initMessage({ sessionId: "resume-1" }));
    await pending;

    expect(h.queries[0]?.spec.model).toBe("claude-haiku-4-5");
  });

  it("ignores everything the summarizing session says before its result", async () => {
    const h = harness({
      nowMs: 1_000_000 + 10 * 60 * 1000,
      onQueryCreated: (query, _spec, index) => {
        if (index !== 0) return;
        query.emit(assistantMessage("00000000-0000-4000-8000-000000000110"));
        query.emit(resultMessage("ffffffff-ffff-4fff-8fff-ffffffffffff"));
      },
    });
    const pending = compactWith(
      h,
      create(conversationv1.SessionColdRemediationSchema, {
        remediation: {
          case: "compact",
          value: create(conversationv1.SessionColdCompactSchema, {
            scope: conversationv1.SessionCompactScope.ALL,
          }),
        },
      }),
    );
    (await untilQuery(h, 1)).query.emit(initMessage({ sessionId: "resume-1" }));

    expect((await pending).result.case).toBe("success");
  });
});

describe("reconciliation's remaining descriptions", () => {
  it("re-adopts a live shell whose row holds no arm, announcing it with an empty command", async () => {
    const h = harness();
    h.persistence.live = create(storev1.GetLiveWorkSuccessSchema, {
      liveDetached: [create(conversationv1.DetachedWorkIdSchema, { value: "b02" })],
    });
    h.persistence.page = create(conversationv1.HistoryPageSchema, {
      entries: [
        create(conversationv1.HistoryEntryAtSchema, {
          at: create(conversationv1.HistoryPointerSchema, { value: "1" }),
          entry: create(conversationv1.HistoryEntrySchema, {
            entry: {
              case: "agentFrame",
              value: create(conversationv1.AgentFrameSchema, {
                result: {
                  case: "update",
                  value: create(conversationv1.AgentUpdateSchema, {
                    update: {
                      case: "activity",
                      value: create(conversationv1.AgentActivitySchema, {
                        activityId: create(conversationv1.AgentActivityIdSchema, { value: "b02" }),
                        item: {
                          case: "bash",
                          value: create(conversationv1.AgentBashSchema, {}),
                        },
                      }),
                    },
                  }),
                },
              }),
            },
          }),
        }),
      ],
      boundary: { case: "floor", value: create(conversationv1.HistoryFloorSchema, {}) },
    });

    const response = await started(h);

    const live = response.result.case === "success" ? (response.result.value.session?.liveWork ?? []) : [];
    const origin = live[0]?.origin;
    const work = origin?.case === "created" ? origin.value.workCreated?.work : undefined;
    expect(work?.case === "bash" && work.value.result.case === "start" ? work.value.result.value.command?.line : undefined).toBe("");
  });

  it("does not close the MAIN agent's own book when the store lists it as live", async () => {
    const h = harness();
    h.persistence.live = create(storev1.GetLiveWorkSuccessSchema, {
      liveAgents: [mainAgentId("resume-1")],
    });
    writeTranscript(h.configDir, h.cwd, "resume-1", [assistantLine()]);
    const pending = h.engine.startSession(resumeRequest("resume-1"));
    (await untilQuery(h, 0)).query.emit(initMessage({ sessionId: "resume-1" }));
    await pending;

    expect(
      h.persistence.buffered.some((entry) => entry.agentId?.value === mainAgentId("resume-1").value
        && entry.source.discriminator.includes("swept_up")),
    ).toBe(false);
  });
});

describe("a WatchBash stream that ends on its own", () => {
  it("leaves the teardown nothing to wait for", async () => {
    const h = harness({ watcherConclusionBudgetMs: 25 });
    await started(h);
    await h.engine.onSdkMessage({
      type: "system",
      subtype: "task_started",
      task_id: "t40",
      tool_use_id: "toolu_watched",
      task_type: "local_bash",
      description: "sleep 600",
      uuid: "00000000-0000-4000-8000-000000000111",
      session_id: "s",
    } as never);
    for await (const _frame of h.engine.watchBash(
      create(shimv1.WatchBashRequestSchema, {
        work: create(conversationv1.DetachedWorkIdSchema, { value: "toolu_watched" }),
      }),
    )) {
      // drained to completion, which is what disposes the watcher
    }
    const before = logSinkMark();

    await h.engine.killSession(create(shimv1.KillSessionRequestSchema, { force: true }));

    expect(logContextFor(before, "did not end within its conclusion budget")).toBeUndefined();
  });
});

describe("a turn that ends while the session is standing down", () => {
  it("does NOT re-probe the vendor it is about to close", async () => {
    // The stand-down is already tearing the query down; a probe raced against
    // it can only answer for a session that no longer exists.
    let releaseInterrupt = (): void => undefined;
    const interrupted = new Promise<void>((resolve) => {
      releaseInterrupt = resolve;
    });
    const h = harness({
      onQueryCreated: (query) => {
        query.interrupt = async () => {
          query.calls.push("interrupt");
          await interrupted;
          return { still_queued: [] };
        };
      },
    });
    await started(h);
    h.scheduler.fire(0);
    await new Promise((resolve) => setImmediate(resolve));
    const standing = h.engine.standDown("SIGTERM");
    for (let attempt = 0; attempt < 20 && !(h.queries[0]?.query.calls.includes("interrupt") ?? false); attempt++) {
      await new Promise((resolve) => setImmediate(resolve));
    }
    const before = h.queries[0]?.query.calls.filter((call) => call === "usage").length ?? 0;

    await h.engine.onSdkMessage(resultMessage("11111111-2222-4333-8444-555555555555"));
    releaseInterrupt();
    await standing;

    expect(h.queries[0]?.query.calls.filter((call) => call === "usage").length).toBe(before);
  });
});

/**
 * StartSession finishing after the vendor query died under it.
 *
 * The opening probes and the reconciliation run AFTER the query is up, and
 * nothing makes the vendor survive them: a query that dies while the catalog
 * call is outstanding leaves the rest of StartSession with no vendor to ask.
 * Each remaining step has to answer for a session with no query rather than
 * raise on one.
 */
describe("StartSession's remaining steps when the query died under them", () => {
  /**
   * Start a session, lose the query while `supportedModels` is outstanding,
   * then let StartSession run the rest of its opening.
   */
  async function startedWithQueryLostMidOpening(): Promise<Harness> {
    let release = (): void => undefined;
    const catalog = new Promise<void>((resolve) => {
      release = resolve;
    });
    const h = harness({
      onQueryCreated: (query) => {
        query.supportedModels = async (): Promise<ModelInfoLike[]> => {
          query.calls.push("supportedModels");
          await catalog;
          return [];
        };
      },
    });
    h.persistence.live = create(storev1.GetLiveWorkSuccessSchema, {
      liveDetached: [create(conversationv1.DetachedWorkIdSchema, { value: "b01" })],
    });
    const pending = h.engine.startSession(freshRequest());
    const first = await untilQuery(h, 0);
    const sessionId = first.spec.binding.kind === "fresh" ? first.spec.binding.sessionId : "";
    first.query.emit(initMessage({ sessionId }));
    for (let attempt = 0; attempt < 50 && !first.query.calls.includes("supportedModels"); attempt++) {
      await new Promise((resolve) => setImmediate(resolve));
    }
    // The loop is PARKED in the iterator, so a message unparks it and the
    // failure is raised on the next pull, exactly as a real iterator throws.
    first.query.emit(assistantMessage("00000000-0000-4000-8000-000000000120"));
    first.query.fail(new Error("the vendor stream broke"));
    for (let attempt = 0; attempt < 50 && h.engine.pushes.faultCount === 0; attempt++) {
      await new Promise((resolve) => setImmediate(resolve));
    }
    release();
    await pending;
    return h;
  }

  it("probes no mcp server health without a vendor to probe", async () => {
    const h = await startedWithQueryLostMidOpening();

    expect(h.queries[0]?.query.calls).not.toContain("mcpServerStatus");
  });

  it("asks no context usage without a vendor to ask", async () => {
    const h = await startedWithQueryLostMidOpening();

    expect(h.queries[0]?.query.calls).not.toContain("getContextUsage");
  });

  it("writes no terminal for live work the record states no kind for, even with the query gone", async () => {
    // Survival is never the vendor's to answer: a shell's spool may still be
    // growing whether or not any query is alive, so only its sidecar ends it.
    const h = await startedWithQueryLostMidOpening();

    expect(h.persistence.buffered.some((entry) => entry.upsertKey === "bash:b01:terminal")).toBe(false);
  });
});

/**
 * The shim's own durable log dying.
 *
 * IT OWNS ITS OWN LOGGER. The shim's log sink is ONE process-wide singleton
 * and a poisoning is one-way -- a poisoned sink drops every later record --
 * so poisoning the one this file shares would decide the verdict of every
 * test that reads a log line, depending only on which ran first. Instead the
 * scenario takes a FRESH module graph: its own `log.ts`, its own `node:fs`
 * mock, and the `createEngine` bound to them, so the listener that must hear
 * the poisoning is registered on the sink that was poisoned. The file's own
 * logger is never touched, and this describe may run in any position.
 */
describe("the durable log sink being poisoned", () => {
  it("states the loss on WatchSession, the only channel left once fd 3 is gone", async () => {
    vi.resetModules();
    const freshFs = await import("node:fs");
    const freshLog = await import("../../src/log.js");
    const { createEngine: freshCreateEngine } = await import("../../src/engine/session.js");
    freshLog.configureLog({ fd: 3, cwd: "/ws", workspaceId: "000000000000ab01", agentReplSessionId: "poison-suite" });
    vi.mocked(freshFs.writeSync).mockImplementationOnce(() => {
      throw new Error("fd 3 is gone");
    });
    freshLog.bindLog({ operation: "shim.test.poison" }).debug({}, "the record this sink cannot take");

    const h = harness({ engineFactory: freshCreateEngine });

    expect(h.engine.pushes.faultCount).toBeGreaterThan(0);
  });
});

/**
 * A TRANSIENT FAULT LIFTS WHEN THE SAME OPERATION SUCCEEDS.
 *
 * WHAT THIS GUARDS, and why it exists: a single `SQLITE_BUSY` on one store read
 * made the owner's shim unhealthy for twenty-two hours. Every new WatchSession
 * was answered with that one fault still standing, so the daemon refused to
 * adopt the shim on every boot, forever. A transient condition that raises a
 * permanent verdict is the defect — so every fault-raising operation here has
 * its own component, and every transient one clears its component when it next
 * succeeds.
 */
describe("a component that recovers", () => {
  /** The faults standing right now, by component. */
  function standingFaults(h: Harness): { component: string; detail: string }[] {
    const update = h.engine.pushes.diagnostics().update;
    if (update.case !== "diagnostics") throw new Error("diagnostics is the only arm here");
    const health = update.value.health;
    if (health.case !== "unhealthy") return [];
    return health.value.faults.map((fault) => ({
      component: fault.component,
      detail: fault.detail,
    }));
  }

  /** The components with a fault standing right now. */
  function faultyComponents(h: Harness): string[] {
    return standingFaults(h).map((fault) => fault.component);
  }

  /** Drive one prompt to its `result`, which is what re-probes the pulled facts. */
  async function closeATurn(h: Harness, turn: string): Promise<void> {
    await h.engine.startTurn(
      create(shimv1.StartTurnRequestSchema, {
        turn: create(conversationv1.TurnIdSchema, { value: turn }),
        said: textSaid("go"),
        origin: conversationv1.PromptOrigin.USER_SENT,
        pageSize: 5,
      }),
    );
    h.queries[0]?.query.emit(resultMessage());
  }

  it("clears the record plane's fault when its degraded window closes", async () => {
    // Arrange: the store stopped answering the writer, then answered again.
    const h = harness();
    await started(h);
    h.persistence.raiseDegradedWindow("the store stopped answering");
    h.persistence.raiseFault("the store stopped answering");
    expect(faultyComponents(h)).toContain("store-writer");

    // Act.
    h.persistence.closeDegradedWindow("the store stopped answering", 3);

    // Assert.
    expect(faultyComponents(h)).toEqual([]);
  });

  // ONE OUTAGE IS ONE WINDOW. The writer announces its window twice, and
  // relaying both as fresh windows left a consumer reading two holes where
  // there was one.
  it("closes the window it already holds instead of recording a second one", async () => {
    // Arrange.
    const h = harness();
    await started(h);
    h.persistence.raiseDegradedWindow("the store stopped answering");

    // Act.
    h.persistence.closeDegradedWindow("the store stopped answering", 3);

    // Assert.
    const update = h.engine.pushes.diagnostics().update;
    if (update.case !== "diagnostics") throw new Error("diagnostics is the only arm here");
    const windows = update.value.degradedWindows.filter(
      (window) => window.component === "store-writer",
    );
    expect(windows.map((window) => window.extent.case)).toEqual(["closed"]);
  });

  // A CLOSED WINDOW WITH NOTHING TO CLOSE IS STILL A FACT. If the open
  // announcement never reached this session -- a watch that attached after it,
  // or a plane that only ever announced the closing -- the hole still happened
  // and a consumer is still entitled to it.
  it("records a closing window that matches nothing standing", async () => {
    // Arrange.
    const h = harness();
    await started(h);

    // Act.
    h.persistence.closeDegradedWindow("a hole this session never saw open", 2);

    // Assert.
    const update = h.engine.pushes.diagnostics().update;
    if (update.case !== "diagnostics") throw new Error("diagnostics is the only arm here");
    expect(
      update.value.degradedWindows.map((window) => [window.reason, window.extent.case]),
    ).toEqual([["a hole this session never saw open", "closed"]]);
  });

  it("clears the context-usage probe's fault when the next sample answers", async () => {
    // Arrange: the vendor refuses the first probe only.
    let probes = 0;
    const h = harness({
      onQueryCreated: (query) => {
        query.getContextUsage = async (): Promise<ContextUsageLike> => {
          probes += 1;
          if (probes === 1) throw new Error("the vendor is busy");
          return query.contextUsage;
        };
      },
    });
    await started(h);
    expect(faultyComponents(h)).toContain("vendor-context-usage");

    // Act.
    await closeATurn(h, "turn-1");

    // Assert.
    await vi.waitFor(() => {
      expect(faultyComponents(h)).not.toContain("vendor-context-usage");
    });
  });

  it("clears the mcp probe's fault when the next probe answers", async () => {
    // Arrange.
    let probes = 0;
    const h = harness({
      onQueryCreated: (query) => {
        query.mcpServerStatus = async (): Promise<McpServerStatusLike[]> => {
          probes += 1;
          if (probes === 1) throw new Error("the vendor is busy");
          return query.mcp;
        };
      },
    });
    await started(h);
    expect(faultyComponents(h)).toContain("vendor-mcp-status");

    // Act.
    await closeATurn(h, "turn-1");

    // Assert.
    await vi.waitFor(() => {
      expect(faultyComponents(h)).not.toContain("vendor-mcp-status");
    });
  });

  it("clears the model catalog's fault when the next read answers", async () => {
    // Arrange.
    // The FIRST read is the start's own live signal, which has to answer or
    // there is no session to fault; the fault under test is the read after it.
    let reads = 0;
    const h = harness({
      onQueryCreated: (query) => {
        query.supportedModels = async (): Promise<ModelInfoLike[]> => {
          reads += 1;
          if (reads === 2) throw new Error("the vendor is busy");
          return query.models;
        };
      },
    });
    await started(h);
    await closeATurn(h, "turn-1");
    await vi.waitFor(() => {
      expect(faultyComponents(h)).toContain("vendor-model-catalog");
    });

    // Act.
    await closeATurn(h, "turn-2");

    // Assert.
    await vi.waitFor(() => {
      expect(faultyComponents(h)).not.toContain("vendor-model-catalog");
    });
  });

  // THE OWNER'S OWN FAULT, EXACTLY. The re-announcement's read of the open
  // obligations met a locked database once and the verdict never lifted.
  it("clears the open-obligations fault when a later read answers", async () => {
    // Arrange: StartSession's own reconciliation met a locked database.
    const h = harness();
    h.persistence.liveWorkError = new PersistenceError(
      "store_unavailable",
      "storage failure: begin read transaction: database is locked (5) (SQLITE_BUSY)",
    );
    await started(h);
    expect(faultyComponents(h)).toContain("store-live-work");

    // Act: the store answers, and a new WatchSession re-announces from it.
    h.persistence.liveWorkError = undefined;
    const stream = h.engine.watchSession(create(shimv1.WatchSessionRequestSchema, {}));
    const watch = stream[Symbol.asyncIterator]();
    await watch.next();
    await watch.next();

    // Assert.
    await vi.waitFor(() => {
      expect(faultyComponents(h)).not.toContain("store-live-work");
    });
    await watch.return?.();
  });

  it("clears the keep-alive's fault when the next beat submits", async () => {
    // Arrange: the first beat cannot record its prompt.
    const h = harness();
    await started(h);
    h.persistence.writeThrows = new Error("the row could not be enveloped");
    h.scheduler.fire(0);
    await vi.waitFor(() => {
      expect(faultyComponents(h)).toContain("shim-engine-keepalive");
    });

    // Act.
    h.persistence.writeThrows = undefined;
    h.scheduler.fire(0);

    // Assert.
    await vi.waitFor(() => {
      expect(faultyComponents(h)).not.toContain("shim-engine-keepalive");
    });
  });

  it("clears the history reader's fault when the next read is served", async () => {
    // Arrange.
    const h = harness();
    await started(h);
    h.persistence.readError = new PersistenceError("store_unavailable", "the store is down");
    await h.engine.readHistory(
      create(shimv1.ReadHistoryRequestSchema, {
        pageSize: 5,
        position: {
          case: "after",
          value: create(conversationv1.HistoryPointerSchema, { value: "p-1" }),
        },
      }),
    );
    expect(faultyComponents(h)).toContain("shim-store-reader");

    // Act.
    h.persistence.readError = undefined;
    await h.engine.readHistory(
      create(shimv1.ReadHistoryRequestSchema, {
        pageSize: 5,
        position: {
          case: "after",
          value: create(conversationv1.HistoryPointerSchema, { value: "p-1" }),
        },
      }),
    );

    // Assert.
    expect(faultyComponents(h)).not.toContain("shim-store-reader");
  });

  // THE ONE FAULT MEANT TO STAND. Nothing in this process restarts a query it
  // lost, and the per-operation components are what keep a probe that still
  // answers from clearing it.
  it("keeps the lost query's fault standing while another component recovers", async () => {
    // Arrange: the context probe refuses once, and then the query dies.
    let probes = 0;
    const h = harness({
      onQueryCreated: (query) => {
        query.getContextUsage = async (): Promise<ContextUsageLike> => {
          probes += 1;
          if (probes === 1) throw new Error("the vendor is busy");
          return query.contextUsage;
        };
      },
    });
    await started(h);
    h.queries[0]?.query.fail(new Error("the vendor process is gone"));
    await vi.waitFor(() => {
      expect(faultyComponents(h)).toContain("vendor-query");
    });

    // Act: the context probe is resolved directly, the way a later sample does.
    h.engine.pushes.resolveComponent("vendor-context-usage", 0);

    // Assert.
    expect(faultyComponents(h)).toEqual(["vendor-query"]);
  });
});

/**
 * LIVE WORK NO ROW OF THE MAIN BOOK DESCRIBES.
 *
 * `GetLiveWork` is scoped to this session's lineage, so such a handle IS this
 * conversation's work, and its absence from the announcement is a record-plane
 * loss: stated at ERROR, never judged by asking the vendor (`backgroundTasks`
 * MOVES foreground work, and answers `false` for work already in the
 * background, so it is no observation).
 */
describe("re-announcing live work the record cannot describe", () => {
  /** A session whose store holds one live handle with no row in this book. */
  async function sessionWithForeignHandle(): Promise<Harness> {
    const h = harness();
    await started(h);
    h.persistence.live = create(storev1.GetLiveWorkSuccessSchema, {
      liveDetached: [create(conversationv1.DetachedWorkIdSchema, { value: "toolu_foreign" })],
    });
    return h;
  }

  /** Open one WatchSession far enough to take its re-announcement. */
  async function reannounce(h: Harness): Promise<void> {
    const stream = h.engine.watchSession(create(shimv1.WatchSessionRequestSchema, {}));
    const watch = stream[Symbol.asyncIterator]();
    await watch.next();
    await watch.next();
    await watch.return?.();
  }

  it("records the undescribable handle at ERROR", async () => {
    // Arrange.
    const h = await sessionWithForeignHandle();
    const before = logSinkMark();

    // Act.
    await reannounce(h);

    // Assert.
    const message = "live work of this session has no describable record, so its kind is unknown and it cannot be announced";
    expect([logLevelFor(before, message), logContextFor(before, message)?.work_id]).toEqual(["error", "toolu_foreign"]);
  });

  it("never asks the vendor about it", async () => {
    // Arrange.
    const h = await sessionWithForeignHandle();

    // Act.
    await reannounce(h);

    // Assert.
    expect(h.queries[0]?.query.calls.filter((call) => call.startsWith("backgroundTasks"))).toEqual([]);
  });

  it("announces nothing for a handle it cannot describe", async () => {
    // Arrange.
    const h = await sessionWithForeignHandle();

    // Act.
    const stream = h.engine.watchSession(create(shimv1.WatchSessionRequestSchema, {}));
    const watch = stream[Symbol.asyncIterator]();
    await watch.next();
    const second = await watch.next();
    await watch.return?.();

    // Assert.
    const response = second.value as shimv1.WatchSessionResponse | undefined;
    const frame = response?.frame;
    expect(frame?.case === "sessionStarted" ? frame.value.liveWork : undefined).toEqual([]);
  });
});

/**
 * THE 2026-09-30 ADOPTION: a deploy handed the workspace to a new daemon, which
 * adopted the running shim, and the re-announced `live_work` was EMPTY while
 * the store held two live background subagents. Each unit's row had moved past
 * its start (a background unit's call returns at once, and its beats restate
 * the row), and only a row still at `start` was described.
 */
describe("re-announcing live background work whose row moved past its start", () => {
  /** One unit row of the main book, keyed `id`, holding `item`. */
  function unitRow(id: string, item: conversationv1.AgentActivity["item"]): conversationv1.HistoryEntryAt {
    return create(conversationv1.HistoryEntryAtSchema, {
      at: create(conversationv1.HistoryPointerSchema, { value: id }),
      place: { case: "recordedPlace", value: create(conversationv1.ConversationPlaceSchema, { atMs: 1_000n }) },
      entry: create(conversationv1.HistoryEntrySchema, {
        entry: {
          case: "agentFrame",
          value: create(conversationv1.AgentFrameSchema, {
            result: {
              case: "update",
              value: create(conversationv1.AgentUpdateSchema, {
                update: {
                  case: "activity",
                  value: create(conversationv1.AgentActivitySchema, {
                    activityId: create(conversationv1.AgentActivityIdSchema, { value: id }),
                    item,
                  }),
                },
              }),
            },
          }),
        },
      }),
    });
  }

  /** A started session whose store holds `id` live and whose book holds `row`. */
  async function adoptedWith(id: string, row: conversationv1.HistoryEntryAt): Promise<Harness> {
    const h = harness();
    await started(h);
    h.persistence.live = create(storev1.GetLiveWorkSuccessSchema, {
      liveDetached: [create(conversationv1.DetachedWorkIdSchema, { value: id })],
    });
    h.persistence.page = create(conversationv1.HistoryPageSchema, {
      entries: [row],
      boundary: { case: "floor", value: create(conversationv1.HistoryFloorSchema, {}) },
    });
    return h;
  }

  /** The `live_work` an adopting daemon's WatchSession is told. */
  async function reannounced(h: Harness): Promise<conversationv1.AgentDetachedWork[]> {
    const stream = h.engine.watchSession(create(shimv1.WatchSessionRequestSchema, {}));
    const watch = stream[Symbol.asyncIterator]();
    await watch.next();
    const second = await watch.next();
    await watch.return?.();
    const frame = (second.value as shimv1.WatchSessionResponse | undefined)?.frame;
    return frame?.case === "sessionStarted" ? frame.value.liveWork : [];
  }

  const BACKGROUND_AGENT = unitRow("toolu_01G8D89ityjVCxRTfqUFCzPR", {
    case: "subagent",
    value: create(conversationv1.AgentSubagentSchema, {
      result: {
        case: "update",
        value: create(conversationv1.AgentSubagentUpdateSchema, {
          prompt: create(conversationv1.AgentSubagentPromptSchema, { text: "audit the footer" }),
        }),
      },
    }),
  });

  const BACKGROUND_SHELL = unitRow("toolu_bg_shell", {
    case: "bash",
    value: create(conversationv1.AgentBashSchema, {
      result: { case: "progress", value: create(conversationv1.AgentToolCallProgressSchema, { lastProgressAtMs: 2_000n }) },
    }),
  });

  const SHELL_START = create(conversationv1.AgentBashSchema, {
    result: {
      case: "start",
      value: create(conversationv1.AgentBashStartSchema, {
        command: create(conversationv1.AgentBashCommandSchema, { line: "npm run test:integration" }),
      }),
    },
  });

  it("re-announces a live background agent whose row carries its launch-time beat", async () => {
    // Arrange.
    const h = await adoptedWith("toolu_01G8D89ityjVCxRTfqUFCzPR", BACKGROUND_AGENT);

    // Act.
    const live = await reannounced(h);

    // Assert.
    expect(live.map((work) => [work.work?.value, work.kind?.kind.case === "subagent" ? work.kind.kind.value.agentId?.value : ""])).toEqual([
      ["toolu_01G8D89ityjVCxRTfqUFCzPR", "toolu_01G8D89ityjVCxRTfqUFCzPR"],
    ]);
  });

  it("re-announces a live background shell described from its run's own start row", async () => {
    // Arrange.
    const h = await adoptedWith("toolu_bg_shell", BACKGROUND_SHELL);
    h.persistence.bashFrames = [SHELL_START];

    // Act.
    const live = await reannounced(h);

    // Assert.
    const origin = live[0]?.origin;
    const work = origin?.case === "created" ? origin.value.workCreated?.work : undefined;
    expect(work?.case === "bash" && work.value.result.case === "start" ? work.value.result.value.command?.line : undefined).toBe(
      "npm run test:integration",
    );
  });

  it("reads the shell's start row without waiting for a first row", async () => {
    // Arrange.
    const h = await adoptedWith("toolu_bg_shell", BACKGROUND_SHELL);
    h.persistence.bashFrames = [SHELL_START];

    // Act.
    await reannounced(h);

    // Assert.
    expect([h.persistence.bashRunCalls, h.persistence.bashRunAwaits]).toEqual([["open:toolu_bg_shell"], [false]]);
  });

  it("still re-announces the shell when its start row cannot be read, recording the failure at ERROR", async () => {
    // Arrange.
    const h = await adoptedWith("toolu_bg_shell", BACKGROUND_SHELL);
    h.persistence.openBashRun = () => Promise.reject(new Error("the store is restarting"));
    const before = logSinkMark();

    // Act.
    const live = await reannounced(h);

    // Assert.
    const message = "the live shell run's own start row could not be read; its announcement states no command";
    expect([live.map((work) => work.kind?.kind.case), logLevelFor(before, message), logContextFor(before, message)?.cause]).toEqual([
      ["bash"],
      "error",
      "the store is restarting",
    ]);
  });

  it("states a shell whose store holds no rows at debug, and announces it by handle and kind", async () => {
    // Arrange.
    const h = await adoptedWith("toolu_bg_shell", BACKGROUND_SHELL);
    h.persistence.openBashRun = () => Promise.reject(new PersistenceError("unknown_work", "no rows"));
    const before = logSinkMark();

    // Act.
    const live = await reannounced(h);

    // Assert.
    const message = "the store holds no rows for this live shell run; its announcement states no command";
    expect([live.map((work) => work.kind?.kind.case), logLevelFor(before, message)]).toEqual([["bash"], "debug"]);
  });

  it("never asks the vendor while re-announcing", async () => {
    // Arrange.
    const h = await adoptedWith("toolu_01G8D89ityjVCxRTfqUFCzPR", BACKGROUND_AGENT);

    // Act.
    await reannounced(h);

    // Assert.
    expect(h.queries[0]?.query.calls.filter((call) => call.startsWith("backgroundTasks"))).toEqual([]);
  });
});

// ---------------------------------------------------------------------------
// A book with no rows yet is an ANSWER, not an unreachable store.
// ---------------------------------------------------------------------------

/**
 * `unknown_agent` on the re-announcement's read of this session's own book.
 *
 * A workspace created seconds ago has written no row, so the store answers
 * `unknown_agent` — its ordinary answer, which it records at info. Reading that
 * as "the record plane could not be reached" opened a `storeUnreachable`
 * session fault and made the daemon open a health fault over a store that was
 * reachable and answered correctly. The class separates the two: this one is an
 * empty live membership, every other one is the failure it always was.
 */
describe("re-announcing when the store holds no rows for this agent yet", () => {
  /** A session whose store reports live work but refuses this session's book. */
  async function sessionWithNoBookYet(
    error: PersistenceError,
  ): Promise<Harness> {
    const h = harness({ backgroundTasks: true });
    h.persistence.live = create(storev1.GetLiveWorkSuccessSchema, {
      liveDetached: [create(conversationv1.DetachedWorkIdSchema, { value: "b01" })],
    });
    await started(h);
    h.persistence.openError = error;
    return h;
  }

  /** The unknown-agent refusal the store answers a book it holds no rows for. */
  function unknownAgent(): PersistenceError {
    return new PersistenceError(
      "unknown_agent",
      'unknown agent: agent "82d48acc" names no book of this store',
    );
  }

  /** The re-announcement frame of one new watch. */
  async function reannouncedLiveWork(
    h: Harness,
  ): Promise<conversationv1.AgentDetachedWork[] | undefined> {
    const watch = h.engine.watchSession(create(shimv1.WatchSessionRequestSchema, {}))[
      Symbol.asyncIterator
    ]();
    await watch.next();
    const second = await nextPush(watch);
    await watch.return?.();
    return second.frame.case === "sessionStarted" ? second.frame.value.liveWork : undefined;
  }

  it("serves an empty live membership", async () => {
    // Arrange.
    const h = await sessionWithNoBookYet(unknownAgent());

    // Act.
    const announced = await reannouncedLiveWork(h);

    // Assert.
    expect(announced).toEqual([]);
  });

  it("opens no session fault", async () => {
    // Arrange.
    const h = await sessionWithNoBookYet(unknownAgent());
    const before = h.engine.pushes.faultCount;

    // Act.
    await reannouncedLiveWork(h);

    // Assert.
    expect(h.engine.pushes.faultCount).toBe(before);
  });

  it("states the empty membership below warning level", async () => {
    // Arrange.
    const h = await sessionWithNoBookYet(unknownAgent());
    const before = logSinkMark();

    // Act.
    await reannouncedLiveWork(h);

    // Assert.
    expect(logLevelFor(before, "re-announcing an empty live membership")).toBe("debug");
  });

  it("still records storeUnreachable when the store cannot be reached", async () => {
    // Arrange.
    const h = await sessionWithNoBookYet(
      new PersistenceError("store_unavailable", "connect ECONNREFUSED"),
    );

    // Act.
    const kinds = await pushedUpdates(
      h,
      (update) =>
        update.case === "diagnostics" && update.value.health.case === "unhealthy"
          ? update.value.health.value.faults.map((fault) => fault.kind.case)
          : undefined,
      async () => {
        await reannouncedLiveWork(h);
      },
    );

    // Assert.
    expect(kinds.flat()).toContain("storeUnreachable");
  });
});

/**
 * The same class, on the StartSession reconciliation's read of the same book.
 *
 * Reconciliation opens no fault, but it warned "the book could not be read" on
 * every first watch of a brand-new workspace. It is not a lost book: it is a
 * book with nothing in it yet.
 */
describe("reconciling when the store holds no rows for this agent yet", () => {
  /** A session started against a store that refuses this agent's book. */
  async function startWithBookRefused(error: PersistenceError): Promise<number> {
    const h = harness({ backgroundTasks: true });
    h.persistence.live = create(storev1.GetLiveWorkSuccessSchema, {
      liveDetached: [create(conversationv1.DetachedWorkIdSchema, { value: "b01" })],
    });
    h.persistence.openError = error;
    const before = logSinkMark();
    await started(h);
    return before;
  }

  it("states the empty book below warning level", async () => {
    // Arrange & Act.
    const before = await startWithBookRefused(
      new PersistenceError("unknown_agent", 'agent "82d48acc" names no book of this store'),
    );

    // Assert.
    expect(logLevelFor(before, "reconciliation describes live work from an empty book")).toBe(
      "debug",
    );
  });

  it("still warns when the book could not be read at all", async () => {
    // Arrange & Act.
    const before = await startWithBookRefused(
      new PersistenceError("store_unavailable", "connect ECONNREFUSED"),
    );

    // Assert.
    expect(logLevelFor(before, "the book could not be read for reconciliation")).toBe("warn");
  });
});

describe("foreground work on the 0.3.280 vendor, which starts a task for every call", () => {
  /** A FOREGROUND shell task, its spawning call still blocking on it. */
  const foregroundShell = async (h: Harness): Promise<void> => {
    await h.engine.onSdkMessage({
      type: "system",
      subtype: "task_started",
      task_id: "b01",
      tool_use_id: "toolu_fg",
      task_type: "local_bash",
      is_backgrounded: false,
      description: "ls",
      uuid: "00000000-0000-4000-8000-0000000000e1",
      session_id: "s",
    } as never);
  };

  it("answers KillSession idle while only foreground work has a task", async () => {
    // Arrange.
    const h = harness();
    await started(h);
    await foregroundShell(h);

    // Act.
    const response = await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    // Assert.
    expect(response.result.case === "success" ? response.result.value.closed?.how.case : undefined).toBe(
      "idle",
    );
  });

  it("refuses StopBash on a foreground shell, which is no detached run", async () => {
    // Arrange.
    const h = harness();
    await started(h);
    await foregroundShell(h);

    // Act.
    const response = await h.engine.stopBash(
      create(shimv1.StopBashRequestSchema, {
        work: create(conversationv1.DetachedWorkIdSchema, { value: "toolu_fg" }),
      }),
    );

    // Assert.
    expect(response.result.case === "failure" ? response.result.value.kind.case : undefined).toBe("unknownWork");
  });

  it("admits a foreground shell to the live set once its own tool result says it moved", async () => {
    // Arrange.
    const h = harness();
    await started(h);
    await foregroundShell(h);

    // Act.
    await h.engine.onSdkMessage({
      type: "user",
      message: { role: "user", content: [] },
      parent_tool_use_id: null,
      tool_use_result: { stdout: "", backgroundTaskId: "b01", timedOutAfterMs: 120_000 },
      uuid: "00000000-0000-4000-8000-0000000000e2",
      session_id: "s",
    } as never);
    const response = await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    // Assert.
    expect(response.result.case).toBe("failure");
  });
});

describe("refreshing the context reading after a main-agent API response", () => {
  /** An API response as the vendor emits it, carrying usage unless told not to. */
  function apiResponse(opts: { parent?: string | null; usage?: boolean } = {}): SdkMessage {
    const message: Record<string, unknown> = { id: "msg_ctx", role: "assistant", content: [] };
    if (opts.usage !== false) {
      message.usage = {
        input_tokens: 10,
        output_tokens: 5,
        cache_creation_input_tokens: 0,
        cache_read_input_tokens: 0,
      };
    }
    return {
      type: "assistant",
      uuid: "ctx-assistant-uuid",
      session_id: "s",
      parent_tool_use_id: opts.parent ?? null,
      message,
    } as never;
  }

  /** How many context probes the vendor has been asked for. */
  function probes(h: Harness): number {
    return h.queries[0]?.query.calls.filter((call) => call === "getContextUsage").length ?? 0;
  }

  /** Let every queued probe run to its push. */
  async function drained(): Promise<void> {
    await new Promise((resolve) => setImmediate(resolve));
  }

  /** A vendor whose probes each wait for the test to answer them. */
  function heldProbes(h: Harness): { answer: (total: number) => void; asked: () => number } {
    const pending: ((usage: ContextUsageLike) => void)[] = [];
    const query = h.queries[0]?.query;
    if (query === undefined) throw new Error("no query to hold");
    const base = query.contextUsage;
    query.getContextUsage = (): Promise<ContextUsageLike> =>
      new Promise((resolve) => {
        pending.push(resolve);
      });
    return {
      answer: (total) => {
        const next = pending.shift();
        if (next === undefined) throw new Error("no probe is waiting");
        next({ ...base, totalTokens: total });
      },
      asked: () => pending.length,
    };
  }

  it.each([
    { name: "a main-agent response carrying usage asks for one probe", message: () => apiResponse(), want: 1 },
    { name: "a subagent's response asks for none", message: () => apiResponse({ parent: "toolu_sub" }), want: 0 },
    { name: "a main-agent message with no usage asks for none", message: () => apiResponse({ usage: false }), want: 0 },
  ])("$name", async ({ message, want }) => {
    // Arrange.
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");
    const before = probes(h);

    // Act.
    await h.engine.onSdkMessage(answering(h, message()));
    await drained();

    // Assert.
    expect(probes(h) - before).toBe(want);
  });

  it("asks for no probe on a keep-alive's response", async () => {
    // Arrange: a keep-alive turn is open.
    const h = harness();
    await started(h);
    await realTurn(h, "turn-0", [assistantMessage("real-uuid")]);
    h.scheduler.fire(0);
    await drained();
    const before = probes(h);

    // Act.
    await h.engine.onSdkMessage(answering(h, apiResponse()));
    await drained();

    // Assert.
    expect(probes(h) - before).toBe(0);
  });

  it("pushes the refreshed reading while the turn is still open", async () => {
    // Arrange: the context has grown since the session's opening reading.
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");
    const query = h.queries[0]?.query;
    if (query === undefined) throw new Error("no query");
    query.contextUsage = { ...query.contextUsage, totalTokens: 4242 };

    // Act.
    const seen = await pushedUpdates(
      h,
      (update) =>
        update.case === "contextUsage" && update.value.totalTokens === 4242n ? update.value : undefined,
      async () => {
        await h.engine.onSdkMessage(apiResponse());
        await drained();
      },
    );

    // Assert.
    expect(seen).toHaveLength(1);
  });

  it("coalesces the responses that land while a refresh is queued", async () => {
    // Arrange: the first refresh is in flight and held by the vendor.
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");
    const held = heldProbes(h);
    await h.engine.onSdkMessage(apiResponse());
    await drained();

    // Act: three more responses land while it is held.
    await h.engine.onSdkMessage(apiResponse());
    await h.engine.onSdkMessage(apiResponse());
    await h.engine.onSdkMessage(apiResponse());
    held.answer(101);
    await drained();

    // Assert: they rode ONE queued probe.
    expect(held.asked()).toBe(1);
  });

  it("pushes a turn-end reading after the mid-turn reading it queued behind", async () => {
    // Arrange: a mid-turn refresh is held by the vendor.
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");
    const held = heldProbes(h);

    // Act: the turn ends while the refresh is still held, then the vendor
    // answers both probes in order.
    const seen = await pushedUpdates(
      h,
      (update) => (update.case === "contextUsage" ? update.value.totalTokens : undefined),
      async () => {
        await h.engine.onSdkMessage(apiResponse());
        await drained();
        const ending = h.engine.onSdkMessage(resultMessage());
        await drained();
        held.answer(110);
        await drained();
        held.answer(120);
        await ending;
        await drained();
      },
    );

    // Assert: the subscription opens on the session's standing reading (0),
    // then the two probes' readings arrive in the order they were asked for.
    expect(seen.filter((total) => total !== 0n)).toEqual([110n, 120n]);
  });

  /** A session whose next probe cannot even report its own failure. */
  async function unreportableProbe(): Promise<Harness> {
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");
    const query = h.queries[0]?.query;
    if (query === undefined) throw new Error("no query");
    query.getContextUsage = () => Promise.reject(new Error("the vendor is busy"));
    vi.spyOn(h.engine.pushes, "fault").mockImplementationOnce(() => {
      throw new Error("the fault could not be recorded");
    });
    return h;
  }

  it("records a refresh that failed before it could push, at ERROR with its cause", async () => {
    // Arrange.
    const h = await unreportableProbe();
    const from = logSinkMark();

    // Act.
    await h.engine.onSdkMessage(apiResponse());
    await drained();

    // Assert.
    expect(logLevelFor(from, "the mid-turn context refresh failed")).toBe("error");
    expect(logContextFor(from, "the mid-turn context refresh failed")?.cause).toBe("the fault could not be recorded");
  });

  it("still runs the next probe after one failed outright", async () => {
    // Arrange: a refresh has failed before it could push.
    const h = await unreportableProbe();
    await h.engine.onSdkMessage(apiResponse());
    await drained();
    const query = h.queries[0]?.query;
    if (query === undefined) throw new Error("no query");
    let asked = 0;
    query.getContextUsage = (): Promise<ContextUsageLike> => {
      asked += 1;
      return Promise.resolve(query.contextUsage);
    };

    // Act.
    await h.engine.onSdkMessage(apiResponse());
    await drained();

    // Assert.
    expect(asked).toBe(1);
  });

  it("raises the context-usage fault when a mid-turn probe fails", async () => {
    // Arrange: the vendor answers the session's opening probe, then refuses.
    let answered = 0;
    const h = harness({
      onQueryCreated: (query) => {
        query.getContextUsage = async (): Promise<ContextUsageLike> => {
          answered += 1;
          if (answered > 1) throw new Error("the vendor is busy mid-turn");
          return query.contextUsage;
        };
      },
    });
    await started(h);
    await realPrompt(h, "turn-1");

    // Act.
    await h.engine.onSdkMessage(apiResponse());
    await drained();

    // Assert.
    const update = h.engine.pushes.diagnostics().update;
    if (update.case !== "diagnostics") throw new Error("diagnostics is the only arm here");
    const health = update.value.health;
    const components = health.case === "unhealthy" ? health.value.faults.map((fault) => fault.component) : [];
    expect(components).toContain("vendor-context-usage");
  });
});

describe("the held stop command (KillTurn.commanded_by)", () => {
  const interjection = (): conversationv1.AgentInterruptedByUser =>
    create(conversationv1.AgentInterruptedByUserSchema, {
      command: { case: "interjection", value: create(conversationv1.AgentInterruptedByUserInterjectionSchema, {}) },
    });

  /** Kill the open real turn, stating `commandedBy` when given. */
  async function kill(h: Harness, turnId: string, commandedBy?: conversationv1.AgentInterruptedByUser): Promise<void> {
    await h.engine.killTurn(
      create(shimv1.KillTurnRequestSchema, {
        turn: create(conversationv1.TurnIdSchema, { value: turnId }),
        force: false,
        ...(commandedBy === undefined ? {} : { commandedBy }),
      }),
    );
  }

  it("hands the stated command to the fold of the stopped turn's result", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");
    await kill(h, "turn-1", interjection());

    // Act
    await h.engine.onSdkMessage(resultMessage("stopped-result"));

    // Assert
    expect(h.fold.contexts.at(-1)?.stopCommand?.command.case).toBe("interjection");
  });

  it("hands no command to the fold when the kill stated none", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");
    await kill(h, "turn-1");

    // Act
    await h.engine.onSdkMessage(resultMessage("stopped-result"));

    // Assert
    expect(h.fold.contexts.at(-1)?.stopCommand).toBeUndefined();
  });

  it("is consumed by the stopped turn's result, so the next message folds without it", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");
    await kill(h, "turn-1", interjection());
    await h.engine.onSdkMessage(resultMessage("stopped-result"));

    // Act
    await h.engine.onSdkMessage(assistantMessage("after-the-stop"));

    // Assert
    expect(h.fold.contexts.at(-1)?.stopCommand).toBeUndefined();
  });

  it("is retired unconsumed when a real turn opens, so it never reaches a later turn", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await realPrompt(h, "turn-1");
    await kill(h, "turn-1", interjection());

    // Act
    await realPrompt(h, "turn-2");
    await h.engine.onSdkMessage(resultMessage("turn-2-result"));

    // Assert
    expect(h.fold.contexts.at(-1)?.stopCommand).toBeUndefined();
  });
});

/**
 * THE NETWORK RESUME, WIRED INTO THE SESSION (engine/network-resume.ts).
 *
 * The module's own suite owns classification, the loop and the window; this
 * block owns what only the session can answer: that its messages reach the
 * loop, that the resume is delivered to the vendor as the main agent's own
 * turn only when the main agent is idle, and that stand-down cancels the loop.
 */
describe("a background agent a network outage cut off", () => {
  const OUTAGE = "API Error: Can't reach the API server — check your internet or DNS (ENOTFOUND)";

  /** The vendor's account of agent a1 dying on an unreachable API. */
  async function cutOff(h: Harness): Promise<void> {
    await h.engine.onSdkMessage({
      type: "system",
      subtype: "task_started",
      task_id: "a1",
      tool_use_id: "toolu_spawn",
      description: "a sweep",
      task_type: "local_agent",
      uuid: "00000000-0000-4000-8000-0000000000a1",
      session_id: "s",
    } as unknown as SdkMessage);
    await h.engine.onSdkMessage({
      type: "assistant",
      message: { model: "<synthetic>", content: [{ type: "text", text: OUTAGE }] },
      parent_tool_use_id: "toolu_spawn",
      error: "server_error",
      uuid: "00000000-0000-4000-8000-0000000000a2",
      session_id: "s",
    } as unknown as SdkMessage);
    await h.engine.onSdkMessage({
      type: "system",
      subtype: "task_notification",
      task_id: "a1",
      tool_use_id: "toolu_spawn",
      status: "failed",
      output_file: "/tmp/a1.output",
      summary: `Agent "a sweep" failed: Agent terminated early due to an API error: ${OUTAGE} (error type server_error)`,
      uuid: "00000000-0000-4000-8000-0000000000a3",
      session_id: "s",
    } as unknown as SdkMessage);
  }

  /** One beat of the loop, and every send it caused let through. */
  async function beat(h: Harness): Promise<void> {
    h.networkScheduler.fire(0);
    await drainTurns();
  }

  it("waits on the session's one probe loop, beating every five seconds", async () => {
    // Arrange
    const h = harness();
    await started(h);

    // Act
    await cutOff(h);

    // Assert
    expect(h.networkScheduler.intervals).toEqual([NETWORK_RESUME_PROBE_INTERVAL_MS]);
  });

  it("is continued by the main agent once the API is reachable", async () => {
    // Arrange
    const delivered: string[] = [];
    const h = harness({ drainPrompts: delivered });
    await started(h);
    await cutOff(h);

    // Act
    await beat(h);

    // Assert
    expect(delivered.filter(isNetworkResumePrompt).map(resumePromptTargets)).toEqual([["a1"]]);
  });

  it("is not delivered while the API is unreachable", async () => {
    // Arrange
    const delivered: string[] = [];
    const h = harness({ drainPrompts: delivered });
    await started(h);
    await cutOff(h);
    h.probe.reachable = false;

    // Act
    await beat(h);

    // Assert
    expect(delivered.filter(isNetworkResumePrompt)).toEqual([]);
  });

  it("waits while a real turn is open", async () => {
    // Arrange
    const delivered: string[] = [];
    const h = harness({ drainPrompts: delivered });
    await started(h);
    await realPrompt(h, "turn-1");
    await cutOff(h);

    // Act
    await beat(h);

    // Assert
    expect(delivered.filter(isNetworkResumePrompt)).toEqual([]);
  });

  it("is delivered on the first beat after the open turn ends", async () => {
    // Arrange
    const delivered: string[] = [];
    const h = harness({ drainPrompts: delivered });
    await started(h);
    await realPrompt(h, "turn-1");
    await cutOff(h);
    await beat(h);
    await h.engine.onSdkMessage(answering(h, resultMessage("turn-1-result")));

    // Act
    await beat(h);

    // Assert
    expect(delivered.filter(isNetworkResumePrompt)).toHaveLength(1);
  });

  /** Every adoption row the engine wrote, in order. */
  const adoptions = (h: Harness): conversationv1.AgentPrompt[] =>
    h.persistence.buffered.flatMap((entry) =>
      entry.item.kind === "prompt" && entry.item.prompt.origin === conversationv1.PromptOrigin.VENDOR_STARTED
        ? [entry.item.prompt]
        : [],
    );

  it("holds a StartTurn right after it behind the resume's turn rather than refusing it", async () => {
    // Arrange
    const h = harness({ drainPrompts: [] });
    await started(h);
    await cutOff(h);
    await beat(h);

    // Act
    const starting = startDuring(h, "turn-2");

    // Assert
    expect(await settledSoon(starting)).toBe(false);
  });

  it("opens the held StartTurn once the resume's own result ends its turn", async () => {
    // Arrange
    const h = harness({ drainPrompts: [] });
    await started(h);
    await cutOff(h);
    await beat(h);
    const resumeSend = h.minted.at(-1);
    const starting = startDuring(h, "turn-2");

    // Act
    await h.engine.onSdkMessage({
      ...resultMessage("resume-result"),
      user_message_uuid: resumeSend,
      user_message_uuids: [resumeSend],
    } as SdkMessage);

    // Assert
    expect((await starting).result.case).toBe("success");
  });

  it("sends the resume prompt under a client uuid of its own", async () => {
    // Arrange
    const sends: SdkUserMessage[] = [];
    const h = harness({ drainSends: sends });
    await started(h);
    await cutOff(h);

    // Act
    await beat(h);

    // Assert
    expect(sends.map((send) => send.uuid)).toEqual([h.minted.at(-1)]);
  });

  it("matches the resume's reply by its echo, not by arriving next", async () => {
    // Arrange: the vendor runs a turn of its own ahead of the resume's answer.
    const h = harness({ drainPrompts: [] });
    await started(h);
    await cutOff(h);
    await beat(h);
    const resumeTurn = adoptions(h)[0]?.id?.value;
    await h.engine.onSdkMessage(assistantMessage("vendor-reply"));
    await h.engine.onSdkMessage(resultMessage("vendor-result"));

    // Act
    await h.engine.onSdkMessage(answering(h, assistantMessage("resume-reply")));

    // Assert
    const vendorTurn = adoptions(h)[1]?.id?.value;
    expect([h.fold.contexts.at(-1)?.turnId?.value === resumeTurn, vendorTurn !== resumeTurn]).toEqual([true, true]);
  });

  it("writes one VENDOR_STARTED prompt row for the resume's turn", async () => {
    // Arrange
    const h = harness({ drainPrompts: [] });
    await started(h);
    await cutOff(h);

    // Act
    await beat(h);

    // Assert
    expect(adoptions(h)).toHaveLength(1);
  });

  it("the resume turn's first reply adopts no second turn", async () => {
    // Arrange
    const h = harness({ drainPrompts: [] });
    await started(h);
    await cutOff(h);
    await beat(h);

    // Act
    await h.engine.onSdkMessage(answering(h, assistantMessage("resume-reply")));

    // Assert
    expect(adoptions(h)).toHaveLength(1);
  });

  it("the resume turn's result frees the slot for a StartTurn", async () => {
    // Arrange
    const h = harness({ drainPrompts: [] });
    await started(h);
    await cutOff(h);
    await beat(h);
    await h.engine.onSdkMessage(answering(h, resultMessage("resume-result")));

    // Act
    const response = await startDuring(h, "turn-2");

    // Assert
    expect(response.result.case).toBe("success");
  });

  it("stand-down cancels the probe loop", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await cutOff(h);

    // Act
    await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    // Assert
    expect(h.networkScheduler.cleared).toBe(1);
  });

  /** Open a WatchSession on the engine. */
  function watch(h: Harness): AsyncIterator<shimv1.WatchSessionResponse> {
    return h.engine.watchSession(create(shimv1.WatchSessionRequestSchema, {}))[Symbol.asyncIterator]();
  }

  /**
   * Every session update a watch yields up to a `queryDied` sentinel pushed
   * NOW, so the read is bounded by a frame the test itself put last.
   */
  async function updatesUntilSentinel(
    h: Harness,
    iterator: AsyncIterator<shimv1.WatchSessionResponse>,
  ): Promise<conversationv1.SessionUpdate[]> {
    h.engine.pushes.push(
      create(conversationv1.SessionUpdateSchema, {
        update: { case: "queryDied", value: create(conversationv1.SessionQueryDiedSchema, {}) },
      }),
    );
    const updates: conversationv1.SessionUpdate[] = [];
    for (;;) {
      const frame = await nextPush(iterator);
      if (frame.frame.case !== "update") continue;
      if (frame.frame.value.update.case === "queryDied") return updates;
      updates.push(frame.frame.value);
    }
  }

  /** The works of the last waiting set among `updates`. */
  function lastWaits(updates: conversationv1.SessionUpdate[]): string[] | undefined {
    const sets = updates.flatMap((update) =>
      update.update.case === "networkResumeWaits" ? [update.update.value.waits.map((wait) => wait.work?.value)] : [],
    );
    return sets.at(-1) as string[] | undefined;
  }

  it("states the standing wait to a consumer that opens after it", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await cutOff(h);

    // Act
    const iterator = watch(h);
    const updates = await updatesUntilSentinel(h, iterator);
    await iterator.return?.();

    // Assert
    expect(lastWaits(updates)).toEqual(["toolu_spawn"]);
  });

  it("states the wait to a consumer already watching when it opens", async () => {
    // Arrange
    const h = harness();
    await started(h);
    const iterator = watch(h);

    // Act
    await cutOff(h);
    const updates = await updatesUntilSentinel(h, iterator);
    await iterator.return?.();

    // Assert
    expect(lastWaits(updates)).toEqual(["toolu_spawn"]);
  });

  it("states the resume's outcome on the session stream", async () => {
    // Arrange
    const h = harness({ drainPrompts: [] });
    await started(h);
    await cutOff(h);
    const iterator = watch(h);

    // Act
    await beat(h);
    const updates = await updatesUntilSentinel(h, iterator);
    await iterator.return?.();

    // Assert
    const outcomes = updates.flatMap((update) =>
      update.update.case === "networkResumeOutcome"
        ? [[update.update.value.work?.value, update.update.value.outcome.case]]
        : [],
    );
    expect(outcomes).toEqual([["toolu_spawn", "resumed"]]);
  });

  it("stand-down states the wait abandoned before the stream ends", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await cutOff(h);
    const iterator = watch(h);

    // Act
    await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    // Assert
    const ends: (string | undefined)[] = [];
    for (let frame = await iterator.next(); frame.done !== true; frame = await iterator.next()) {
      const update = frame.value.frame.case === "update" ? frame.value.frame.value.update : undefined;
      if (update?.case === "networkResumeOutcome") ends.push(update.value.outcome.case);
    }
    expect(ends).toEqual(["abandoned"]);
  });

  it("the vendor query dying cancels the probe loop", async () => {
    // Arrange
    const h = harness();
    await started(h);
    await cutOff(h);

    // Act
    (await untilQuery(h, 0)).query.end();
    await settledUntil(() => h.networkScheduler.cleared > 0);

    // Assert
    expect(h.networkScheduler.cleared).toBe(1);
  });
});

/**
 * A SUBAGENT RESUMED BY `SendMessage` AFTER THE SHIM RESTARTED (2026-09-27).
 *
 * The resume's `task_started` names the send, not the spawn, and a restarted
 * shim never saw the spawn — so its memory holds no pairing of the vendor task
 * locator with the agent. The store holds it (the sidecar wrote it with the
 * agent's rows), and the engine asks the store before the fold, at the
 * restore, and for the book an ask raised by the agent lands on.
 */
describe("a subagent resumed by a send whose spawn this process never saw", () => {
  const LOCATOR = "a5583";
  const SPAWN = create(conversationv1.AgentIdSchema, { value: "toolu_spawn" });
  const resumeStarted = {
    type: "system",
    subtype: "task_started",
    task_id: LOCATOR,
    tool_use_id: "toolu_send",
    task_type: "local_agent",
    description: "resumed",
    uuid: "00000000-0000-4000-8000-0000000000d1",
    session_id: "s",
  } as never as SdkMessage;

  /** A harness whose fold says the resume awaits the store, as the real fold would. */
  async function resumeHarness(options: Parameters<typeof harness>[0] = {}): Promise<Harness> {
    const h = harness(options);
    h.fold.awaitingFor = (message) => (message === resumeStarted ? LOCATOR : undefined);
    await started(h);
    return h;
  }

  it("hands the fold the store's agent BEFORE folding the resume", async () => {
    // Arrange.
    const h = await resumeHarness();
    h.persistence.vendorTasks.set(LOCATOR, { kind: "found", agent: SPAWN, commission: undefined });
    let learnedWhenFolded = -1;
    h.fold.entriesFor = (message) => {
      if (message === resumeStarted) learnedWhenFolded = h.fold.learned.length;
      return [];
    };

    // Act.
    await h.engine.onSdkMessage(resumeStarted);

    // Assert.
    expect(learnedWhenFolded).toBe(1);
  });

  it("asks the store scoped to this session's main agent, naming the locator", async () => {
    // Arrange.
    const h = await resumeHarness();
    const sessionId =
      h.queries[0]?.spec.binding.kind === "fresh" ? h.queries[0].spec.binding.sessionId : "";

    // Act.
    await h.engine.onSdkMessage(resumeStarted);

    // Assert.
    expect(h.persistence.vendorTaskLookups).toEqual([`${mainAgentId(sessionId).value}/${LOCATOR}`]);
  });

  it("records the store naming the agent at INFO", async () => {
    // Arrange.
    const h = await resumeHarness();
    h.persistence.vendorTasks.set(LOCATOR, { kind: "found", agent: SPAWN, commission: undefined });
    const before = logSinkMark();

    // Act.
    await h.engine.onSdkMessage(resumeStarted);

    // Assert.
    const named = logRecordsSince(before).filter(
      (record) => record.message === "the store named the agent a vendor task is running",
    );
    expect(named.map((record) => [record.level, record.context.agent, record.context.site])).toEqual([
      ["info", "toolu_spawn", "announcement"],
    ]);
  });

  it("records a store that names no agent at ERROR, with its answer", async () => {
    // Arrange.
    const h = await resumeHarness();
    const before = logSinkMark();

    // Act.
    await h.engine.onSdkMessage(resumeStarted);

    // Assert.
    const missed = logRecordsSince(before).filter(
      (record) => record.message === "the store named no agent for a vendor task this process cannot name itself",
    );
    expect(missed.map((record) => [record.level, record.context.detail])).toEqual([
      ["error", "not_found: no agent of this session's lineage is paired with the locator"],
    ]);
  });

  it("asks the store nothing for a message no task awaits", async () => {
    // Arrange.
    const h = await resumeHarness();

    // Act.
    await h.engine.onSdkMessage(assistantMessage("00000000-0000-4000-8000-0000000000d2"));

    // Assert.
    expect(h.persistence.vendorTaskLookups).toEqual([]);
  });

  /** The ask a subagent raises under its vendor task id; its book is returned. */
  async function askBook(h: Harness): Promise<string | undefined> {
    const spec = h.queries[0]?.spec;
    if (spec === undefined) throw new Error("no query");
    const pending = spec.canUseTool("Bash", {}, {
      signal: new AbortController().signal,
      toolUseID: "toolu_asked",
      agentID: LOCATOR,
      requestId: "req_1",
    });
    await vi.waitFor(() => {
      expect(h.persistence.buffered.some((entry) => entry.source.discriminator.includes("permission"))).toBe(true);
    });
    await h.engine.standDown("SIGTERM");
    await pending;
    const entry = h.persistence.buffered.find((buffered) => buffered.source.discriminator.includes("permission"));
    return entry?.agentId?.value;
  }

  it("credits a resumed agent's ask to the agent the fold named", async () => {
    // Arrange: the fold holds the pairing — its spawn, or the store's answer at the resume.
    const h = await resumeHarness();
    h.fold.knowledge.set(LOCATOR, { kind: "named", agent: SPAWN });

    // Act, Assert.
    expect(await askBook(h)).toBe("toolu_spawn");
  });

  it("credits a resumed agent's ask to the agent the store names, never to the send", async () => {
    // Arrange: the live table holds the task under the SEND that resumed it.
    const h = await resumeHarness();
    await h.engine.onSdkMessage(resumeStarted);
    h.fold.knowledge.set(LOCATOR, { kind: "not_its_spawn" });
    h.persistence.vendorTasks.set(LOCATOR, { kind: "found", agent: SPAWN, commission: undefined });

    // Act, Assert.
    expect(await askBook(h)).toBe("toolu_spawn");
  });

  it("lands an ask the store cannot name on the main agent, recorded at ERROR", async () => {
    // Arrange.
    const h = await resumeHarness();
    h.fold.knowledge.set(LOCATOR, { kind: "not_its_spawn" });
    const sessionId =
      h.queries[0]?.spec.binding.kind === "fresh" ? h.queries[0].spec.binding.sessionId : "";
    const before = logSinkMark();

    // Act.
    const book = await askBook(h);

    // Assert.
    expect(book).toBe(mainAgentId(sessionId).value);
    expect(
      logRecordsSince(before)
        .filter((record) => record.level === "error")
        .map((record) => record.context.site),
    ).toEqual(["permission"]);
  });

  /** One unit in the main book, as the store holds it. */
  function bookUnit(at: string, unit: string, item: conversationv1.AgentActivity["item"]): conversationv1.HistoryEntryAt {
    return create(conversationv1.HistoryEntryAtSchema, {
      at: create(conversationv1.HistoryPointerSchema, { value: at }),
      entry: create(conversationv1.HistoryEntrySchema, {
        entry: {
          case: "agentFrame",
          value: create(conversationv1.AgentFrameSchema, {
            result: {
              case: "update",
              value: create(conversationv1.AgentUpdateSchema, {
                update: {
                  case: "activity",
                  value: create(conversationv1.AgentActivitySchema, {
                    activityId: create(conversationv1.AgentActivityIdSchema, { value: unit }),
                    item,
                  }),
                },
              }),
            },
          }),
        },
      }),
    });
  }

  /** The main book after a resume: the send that reached `a5583`, and the spawn of `toolu_spawn`. */
  function resumedBook(): conversationv1.HistoryPage {
    return create(conversationv1.HistoryPageSchema, {
      entries: [
        bookUnit("2", "toolu_send", {
          case: "sendMessage",
          value: create(conversationv1.AgentSendMessageSchema, {
            result: {
              case: "success",
              value: create(conversationv1.AgentSendMessageSuccessSchema, {
                recipientAgentId: create(conversationv1.AgentIdSchema, { value: LOCATOR }),
              }),
            },
          }),
        }),
        bookUnit("1", "toolu_spawn", {
          case: "subagent",
          value: create(conversationv1.AgentSubagentSchema, {
            result: {
              case: "start",
              value: create(conversationv1.AgentSubagentStartSchema, { createdAgentId: SPAWN }),
            },
          }),
        }),
      ],
      boundary: { case: "floor", value: create(conversationv1.HistoryFloorSchema, {}) },
    });
  }

  /** A restarted session whose store holds the resumed agent's send as live work. */
  function restoredHarness(): Harness {
    const h = harness({ backgroundTasks: true });
    h.persistence.live = create(storev1.GetLiveWorkSuccessSchema, {
      liveDetached: [create(conversationv1.DetachedWorkIdSchema, { value: "toolu_send" })],
    });
    h.persistence.page = resumedBook();
    return h;
  }

  it("closes a send-resumed run at StartSession under the agent the store names: it ran in the replaced CLI process", async () => {
    // Arrange.
    const h = restoredHarness();
    h.persistence.vendorTasks.set(LOCATOR, { kind: "found", agent: SPAWN, commission: undefined });

    // Act.
    await started(h);

    // Assert.
    const closing = h.persistence.buffered.find((entry) => entry.upsertKey === "activity:toolu_send");
    const frame = closing?.item.kind === "frame" ? closing.item.frame : undefined;
    const activity = frame?.result.case === "update" && frame.result.value.update.case === "activity" ? frame.result.value.update.value : undefined;
    const failure =
      activity?.item.case === "subagent" && activity.item.value.result.case === "failure" ? activity.item.value.result.value : undefined;
    expect([failure?.cause.case, failure?.createdAgentId?.value]).toEqual(["lost", "toolu_spawn"]);
  });

  it("does not re-adopt the send-resumed run at StartSession", async () => {
    // Arrange.
    const h = restoredHarness();
    h.persistence.vendorTasks.set(LOCATOR, { kind: "found", agent: SPAWN, commission: undefined });

    // Act.
    const response = await started(h);

    // Assert.
    expect(response.result.case === "success" ? response.result.value.session?.liveWork : undefined).toEqual([]);
  });

  it("re-announces a resumed agent to a new watch", async () => {
    // Arrange.
    const h = restoredHarness();
    h.persistence.vendorTasks.set(LOCATOR, { kind: "found", agent: SPAWN, commission: undefined });
    await started(h);
    const watch = h.engine.watchSession(create(shimv1.WatchSessionRequestSchema, {}))[Symbol.asyncIterator]();
    await watch.next();

    // Act.
    const second = await nextPush(watch);
    await watch.return?.();

    // Assert.
    const live = second.frame.case === "sessionStarted" ? second.frame.value.liveWork : [];
    expect(live.map((work) => work.work?.value)).toEqual(["toolu_send"]);
  });

  it("closes a send-resumed run the store names no agent for under the send's own id, at ERROR", async () => {
    // Arrange.
    const h = restoredHarness();
    const before = logSinkMark();

    // Act.
    await started(h);

    // Assert.
    const message = "the agent a send resumed could not be named; its run is closed under the send's own id";
    expect([
      logLevelFor(before, message),
      h.persistence.buffered.some((entry) => entry.upsertKey === "activity:toolu_send"),
    ]).toEqual(["error", true]);
  });
});
