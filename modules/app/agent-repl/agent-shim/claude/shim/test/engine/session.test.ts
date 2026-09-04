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
import os from "node:os";
import path from "node:path";
import { beforeEach, describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { conversationv1, shimv1, storev1 } from "../../src/proto.js";
import { recordAgentBinaryVersion, resetAgentBinaryVersionForTest } from "../../src/build-identity.js";
import { cwdSlug } from "../../src/engine/cold.js";
import { createEngine, type QuerySpec, type SessionEngine } from "../../src/engine/session.js";
import { agentIdPath } from "../../src/engine/identity.js";
import { workspaceLockKey } from "../../src/locks.js";
import { textSaid } from "../../src/engine/turn.js";
import { KEEPALIVE_INTERVAL_MS } from "../../src/engine/keepalive.js";
import { toStanding } from "../../src/engine/permission-gate.js";
import { SYNTHETIC_MODEL } from "../../src/model.js";
import type { AccountUsageLike, McpServerStatusLike } from "../../src/sdk/types.js";
import { mainAgentId } from "../../src/convert/ids.js";
import { ManualScheduler, RecordingFold, RecordingPersistence, ScriptedQuery, initMessage, resultMessage } from "./fakes.js";

interface Harness {
  readonly engine: SessionEngine;
  readonly persistence: RecordingPersistence;
  readonly fold: RecordingFold;
  readonly scheduler: ManualScheduler;
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
        cache_read_input_tokens: 500,
        cache_creation: { ephemeral_1h_input_tokens: 0, ephemeral_5m_input_tokens: 1 },
      },
    },
    ...overrides,
  };
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
  } = {},
): Harness {
  const stateDir = scratch();
  const configDir = scratch();
  const cwd = "/ws";
  const persistence = new RecordingPersistence();
  const fold = new RecordingFold();
  const scheduler = new ManualScheduler();
  const queries: { spec: QuerySpec; query: ScriptedQuery }[] = [];
  const locks: string[] = [];
  const released: string[] = [];
  const workspaceLocks: string[] = [];
  const exits: number[] = [];
  const engine = createEngine({
    endProcess: (code) => exits.push(code),
    persistence,
    fold,
    createQuery: (spec) => {
      const query = new ScriptedQuery();
      if (options.backgroundTasks === true) query.backgroundTaskAnswer = true;
      if (options.backgroundTasksThrows === true) {
        query.backgroundTasks = () => Promise.reject(new Error("the vendor cannot answer"));
      }
      if (options.mcp !== undefined) query.mcp = options.mcp;
      if (options.accountUsage !== undefined) query.accountUsage = options.accountUsage;
      queries.push({ spec, query });
      return Promise.resolve(query);
    },
    runtime: { shimBuildSha: "sha", sdkVersion: "0.3.220" },
    env: { stateDir, configDir, cwd },
    nowMs: () => options.nowMs ?? 1_000_100,
    scheduler,
    ...(options.keepaliveIntervalMs === undefined
      ? {}
      : { keepaliveIntervalMs: options.keepaliveIntervalMs }),
    acquireLock: (sessionId) => {
      if (options.lockThrows === true) throw new Error("locked by another shim");
      locks.push(sessionId);
      return () => {
        released.push(sessionId);
      };
    },
    // STUBBED LIKE THE SESSION CLAIM. The workspace lock moved into
    // StartSession, so a unit test that left it real would take a kernel lock
    // on whatever directory the harness names.
    acquireWorkspaceLock: (dir) => {
      if (options.workspaceLockThrows === true) throw new Error("locked by another shim");
      workspaceLocks.push(dir);
      return () => {
        released.push(`workspace:${dir}`);
      };
    },
  });
  return {
    engine,
    persistence,
    fold,
    scheduler,
    queries,
    stateDir,
    configDir,
    cwd,
    locks,
    workspaceLocks,
    released,
    exits,
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
): shimv1.StartSessionRequest {
  return create(shimv1.StartSessionRequestSchema, {
    source: {
      case: "resume",
      value: create(shimv1.StartSessionResumeSchema, {
        vendorSessionId,
        ...(remediation === undefined ? {} : { coldRemediation: remediation }),
      }),
    },
  });
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

/** Bring a fresh session up: start it, and answer the vendor's init. */
async function started(h: Harness): Promise<shimv1.StartSessionResponse> {
  const pending = h.engine.startSession(freshRequest());
  const first = await untilQuery(h, 0);
  const sessionId = first.spec.binding.kind === "fresh" ? first.spec.binding.sessionId : "";
  first.query.emit(initMessage({ sessionId }));
  return pending;
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

  it("reports the model the SDK chose as effective_model when none was named", async () => {
    const h = harness();
    const pending = h.engine.startSession(freshRequestNoModel());
    const first = await untilQuery(h, 0);
    first.query.emit(
      initMessage({
        sessionId: first.spec.binding.kind === "fresh" ? first.spec.binding.sessionId : "",
        model: "claude-sonnet-5",
      }),
    );
    const response = await pending;

    expect(
      response.result.case === "success"
        ? response.result.value.session?.effectiveModel?.name
        : undefined,
    ).toBe("claude-sonnet-5");
  });

  it("reports the model catalog from the vendor", async () => {
    const h = harness();
    const pending = h.engine.startSession(freshRequest());
    const first = await untilQuery(h, 0);
    first.query.models = [
      { value: "claude-opus-5", displayName: "Opus 5", description: "the big one", supportsEffort: true, supportedEffortLevels: ["low", "high"] },
    ];
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
    const h = harness();
    const pending = h.engine.startSession(freshRequest());
    const first = await untilQuery(h, 0);
    first.query.models = [{ value: "m", displayName: "M", description: "d" }];
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

  it("no lock of either kind is taken before StartSession", async () => {
    // AN INERT SHIM HOLDS NOTHING. A prelaunched shim must be able to sit
    // beside the live one it will replace, which it cannot do while holding
    // the live shim's workspace lock.
    const h = harness();

    expect(h.locks).toEqual([]);
    expect(h.workspaceLocks).toEqual([]);
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
    expect(failure?.cause.case === "cold" ? failure.cause.value.contextTokens : undefined).toBe(500n);
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

  it("binds resume, not fresh", async () => {
    const h = harness({ nowMs: 1_000_100 });
    writeTranscript(h.configDir, h.cwd, "resume-1", [assistantLine()]);
    const pending = h.engine.startSession(resumeRequest("resume-1"));
    (await untilQuery(h, 0)).query.emit(initMessage({ sessionId: "resume-1" }));
    await pending;

    expect(h.queries[0]?.spec.binding).toEqual({ kind: "resume", resumeSessionId: "resume-1" });
  });

  it("RECOVERS the model the conversation was last running under", async () => {
    const h = harness({ nowMs: 1_000_100 });
    writeTranscript(h.configDir, h.cwd, "resume-1", [assistantLine()]);
    const pending = h.engine.startSession(resumeRequest("resume-1"));
    (await untilQuery(h, 0)).query.emit(initMessage({ sessionId: "resume-1", model: "claude-opus-5" }));
    await pending;

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
    h.queries[0]?.query.emit(resultMessage());
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
    h.queries[0]?.query.emit(resultMessage());
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
    h.queries[0]?.query.emit(resultMessage());
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
    h.queries[0]?.query.emit(resultMessage());
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
    h.queries[0]?.query.emit(resultMessage());
    await new Promise((resolve) => setImmediate(resolve));

    const after = h.queries[0]?.query.calls.filter((call) => call === "mcpServerStatus").length ?? 0;
    expect(after).toBeGreaterThan(before);
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
    // A real record, then a keep-alive turn, then a real prompt.
    await h.engine.onSdkMessage(resultMessage("real-uuid"));
    h.scheduler.fire(0);
    await new Promise((resolve) => setImmediate(resolve));
    h.queries.at(-1)?.query.emit(resultMessage("keepalive-uuid"));
    await new Promise((resolve) => setImmediate(resolve));

    await h.engine.startTurn(
      create(shimv1.StartTurnRequestSchema, {
        turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }),
        said: textSaid("go"),
        origin: conversationv1.PromptOrigin.USER_SENT,
        pageSize: 5,
      }),
    );

    expect(h.queries.at(-1)?.spec.resumeSessionAt).toBe("real-uuid");
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
    const h = harness();
    const pending = h.engine.startSession(freshRequest());
    const first = await untilQuery(h, 0);
    first.query.models = [{ value: "claude-opus-5", displayName: "O", description: "d" }];
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

  it("carries the failure's own wording when there is nothing to compact", async () => {
    const h = harness();
    await started(h);

    const response = await h.engine.hibernate(create(shimv1.HibernateRequestSchema, {}));
    const error = response.result.case === "error" ? response.result.value : undefined;
    expect(error?.kind.case === "compactionFailed" ? error.kind.value.error : undefined).toBe(
      "there is no transcript to compact",
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
    } as never);
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

    const second = await iterator.next();
    await iterator.return?.();

    expect(second.value?.frame.case).toBe("update");
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
    const second = await iterator.next();
    await iterator.return?.();

    expect(
      second.value?.frame.case === "sessionStarted"
        ? second.value.frame.value.vendorSessionId
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
    const second = await iterator.next();
    await iterator.return?.();

    expect(
      second.value?.frame.case === "sessionStarted"
        ? second.value.frame.value.turnInFlight?.value !== undefined
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

  it("writes a closing terminal for detached work the vendor no longer has", async () => {
    const h = harness();
    h.persistence.live = create(storev1.GetLiveWorkSuccessSchema, {
      liveDetached: [create(conversationv1.DetachedWorkIdSchema, { value: "b01" })],
    });
    h.persistence.page = create(conversationv1.HistoryPageSchema, {
      entries: [recordedBashRun("b01")],
      boundary: { case: "floor", value: create(conversationv1.HistoryFloorSchema, {}) },
    });

    await started(h);

    expect(h.persistence.buffered.some((entry) => entry.upsertKey === "bash:b01:terminal")).toBe(true);
  });

  it("RE-ADOPTS work the vendor still holds, asked directly rather than waited for", async () => {
    // The live table is built from messages the shim has already seen, and at
    // StartSession it has seen almost none: a revived process announces its
    // surviving tasks on its own schedule, after the init this reconciliation
    // follows. Judging survival off that table alone swept up work the vendor
    // still had.
    const h = harness({ backgroundTasks: true });
    h.persistence.live = create(storev1.GetLiveWorkSuccessSchema, {
      liveDetached: [create(conversationv1.DetachedWorkIdSchema, { value: "b01" })],
    });
    h.persistence.page = create(conversationv1.HistoryPageSchema, {
      entries: [recordedBashRun("b01")],
      boundary: { case: "floor", value: create(conversationv1.HistoryFloorSchema, {}) },
    });
    await started(h);

    expect(h.persistence.buffered.some((entry) => entry.upsertKey === "bash:b01:terminal")).toBe(false);
  });

  it("treats work the vendor could not be ASKED about as surviving", async () => {
    // A vendor that cannot answer is not a vendor that said "gone": closing the
    // run would write a terminal over something that may still be producing.
    const h = harness({ backgroundTasksThrows: true });
    h.persistence.live = create(storev1.GetLiveWorkSuccessSchema, {
      liveDetached: [create(conversationv1.DetachedWorkIdSchema, { value: "b01" })],
    });
    h.persistence.page = create(conversationv1.HistoryPageSchema, {
      entries: [recordedBashRun("b01")],
      boundary: { case: "floor", value: create(conversationv1.HistoryFloorSchema, {}) },
    });
    await started(h);

    expect(h.persistence.buffered.some((entry) => entry.upsertKey === "bash:b01:terminal")).toBe(false);
  });

  it("closes work the record cannot describe rather than leaving it open", async () => {
    // RULING (landing 5): `live_detached` is the store's shell table, so a row
    // there IS a shell run and its kind is known from where it was found. An
    // obligation the shim declines to close never gets a terminal at all.
    const h = harness();
    h.persistence.live = create(storev1.GetLiveWorkSuccessSchema, {
      liveDetached: [create(conversationv1.DetachedWorkIdSchema, { value: "b01" })],
    });

    await started(h);

    expect(h.persistence.buffered.some((entry) => entry.upsertKey === "bash:b01:terminal")).toBe(true);
  });

  it("closes an undescribable run with lost.swept_up", async () => {
    const h = harness();
    h.persistence.live = create(storev1.GetLiveWorkSuccessSchema, {
      liveDetached: [create(conversationv1.DetachedWorkIdSchema, { value: "b01" })],
    });

    await started(h);

    const entry = h.persistence.buffered.find((buffered) => buffered.upsertKey === "bash:b01:terminal");
    const outcome =
      entry?.item.kind === "bash_run" && entry.item.frame.result.case === "success"
        ? entry.item.frame.result.value.outcome
        : undefined;
    expect(
      outcome?.case === "interrupted" && outcome.value.cause.case === "lost"
        ? outcome.value.cause.value.how.case
        : "",
    ).toBe("sweptUp");
  });

  it("states no command for a run whose start the record never held", async () => {
    const h = harness();
    h.persistence.live = create(storev1.GetLiveWorkSuccessSchema, {
      liveDetached: [create(conversationv1.DetachedWorkIdSchema, { value: "b01" })],
    });

    await started(h);

    const entry = h.persistence.buffered.find((buffered) => buffered.upsertKey === "bash:b01:terminal");
    const command =
      entry?.item.kind === "bash_run" && entry.item.frame.result.case === "success"
        ? entry.item.frame.result.value.command
        : undefined;
    expect(command?.line).toBe("");
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
    const second = await iterator.next();
    await iterator.return?.();

    expect(
      second.value?.frame.case === "sessionStarted"
        ? second.value.frame.value.liveWork.map(
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
    h.persistence.openError = new (await import("../../src/store/persistence.js")).PersistenceError(
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
      h.persistence.buffered.some((entry) => entry.agentId.value === "sub-1"),
    ).toBe(true);
  });

  it("does not close the main agent's own book", async () => {
    const h = harness();
    const response = await started(h);
    const agent =
      response.result.case === "success" ? (response.result.value.session?.vendorSessionId ?? "") : "";

    expect(h.persistence.buffered.some((entry) => entry.agentId.value === agent && entry.item.kind === "frame")).toBe(
      false,
    );
  });

  it("reports a fault rather than failing the start when the store is unreachable", async () => {
    const h = harness();
    h.persistence.liveWorkError = new (await import("../../src/store/persistence.js")).PersistenceError(
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

  it("returns to healthy once a message converts", async () => {
    const h = harness();
    await started(h);
    h.fold.faultFor = (message) =>
      (message as { uuid?: string }).uuid === "u-defect" ? "boom" : undefined;
    await h.engine.onSdkMessage(prose("u-defect"));

    await h.engine.onSdkMessage(prose("u-good"));

    expect(diagnostics(h).health.case).toBe("healthy");
  });

  it("closes the window with the number of messages it refused", async () => {
    const h = harness();
    await started(h);
    h.fold.faultFor = (message) =>
      (message as { uuid?: string }).uuid?.startsWith("u-defect") === true ? "boom" : undefined;
    await h.engine.onSdkMessage(prose("u-defect-1"));
    await h.engine.onSdkMessage(prose("u-defect-2"));

    await h.engine.onSdkMessage(prose("u-good"));

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

  it("closes the window at the turn's end", async () => {
    const h = harness();
    await started(h);
    h.fold.faultFor = (message) => (message.type === "assistant" ? "boom" : undefined);
    await h.engine.onSdkMessage(prose("u-defect"));

    await h.engine.onSdkMessage(resultMessage("u-result"));

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
      create(shimv1.DetachForegroundRequestSchema, {}),
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
      } as AccountUsageLike,
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
    h.persistence.readError = new (await import("../../src/store/persistence.js")).PersistenceError(
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
      return entry.agentId.value;
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
    } as never);
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
    } as never);
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
    } as never);
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
 * The IDE-diagnostics adjacency join (engine/session.ts's `lastWriteOrEdit`).
 *
 * The vendor's `diagnostics` attachment carries no tool id, so
 * convert/attachments.ts joins it to the change it concerns by the one
 * remembered write-or-edit unit -- and nothing assigned it, so every
 * diagnostics record fell to "IDE diagnostics arrived with no preceding write
 * or edit" and landed as residue.
 */
describe("the last write or edit unit the fold context carries", () => {
  it("names the edit once one has been folded", async () => {
    const h = harness();
    await started(h);
    h.fold.entriesFor = (message) =>
      message.type === "assistant"
        ? [
            {
              agentId: mainAgentId("vendor-session"),
              upsertKey: "k",
              source: { producer: "p", vendorUuid: "u", arm: "edit" } as never,
              keepalive: false,
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
                            value: "toolu_edit",
                          }),
                          item: {
                            case: "edit",
                            value: create(conversationv1.AgentEditSchema, {}),
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

    expect(h.fold.contexts.at(-1)?.lastWriteOrEditUnit?.value).toBe("toolu_edit");
  });
});
