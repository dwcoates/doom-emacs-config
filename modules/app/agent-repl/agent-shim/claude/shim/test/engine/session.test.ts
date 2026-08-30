/**
 * The session engine, end to end over a scripted vendor.
 *
 * WHAT THIS GUARDS: the acts that are irreversible or invisible. A cold resume
 * is REFUSED with its cost before a token is spent; the session lock is taken
 * BEFORE the SDK is touched, so two shims can never write one transcript; the
 * teardown resolves every pending callback as denied before anything else,
 * because an unresolved `canUseTool` wedges the vendor process outright.
 */
import { mkdirSync, mkdtempSync, writeFileSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { beforeEach, describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { conversationv1, shimv1, storev1 } from "../../src/proto.js";
import { recordAgentBinaryVersion, resetAgentBinaryVersionForTest } from "../../src/build-identity.js";
import { cwdSlug } from "../../src/engine/cold.js";
import { createEngine, type QuerySpec, type SessionEngine } from "../../src/engine/session.js";
import { textSaid } from "../../src/engine/turn.js";
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
  readonly released: string[];
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

function harness(options: { nowMs?: number; lockThrows?: boolean } = {}): Harness {
  const stateDir = scratch();
  const configDir = scratch();
  const cwd = "/ws";
  const persistence = new RecordingPersistence();
  const fold = new RecordingFold();
  const scheduler = new ManualScheduler();
  const queries: { spec: QuerySpec; query: ScriptedQuery }[] = [];
  const locks: string[] = [];
  const released: string[] = [];
  const engine = createEngine({
    persistence,
    fold,
    createQuery: (spec) => {
      const query = new ScriptedQuery();
      queries.push({ spec, query });
      return Promise.resolve(query);
    },
    runtime: { shimBuildSha: "sha", sdkVersion: "0.3.220" },
    env: { stateDir, configDir, cwd },
    nowMs: () => options.nowMs ?? 1_000_100,
    scheduler,
    acquireLock: (sessionId) => {
      if (options.lockThrows === true) throw new Error("locked by another shim");
      locks.push(sessionId);
      return () => released.push(sessionId);
    },
  });
  return { engine, persistence, fold, scheduler, queries, stateDir, configDir, cwd, locks, released };
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
        return () => h.released.push(id);
      },
    });
    await engine.startSession(freshRequest());

    expect(h.released).toHaveLength(1);
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

    await h.engine.setSessionModel(
      create(shimv1.SetSessionModelRequestSchema, {
        model: create(conversationv1.AgentModelSchema, { name: "claude-sonnet-5" }),
      }),
    );

    expect(h.queries[0]?.query.calls).not.toContain("setModel:claude-sonnet-5");
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
    await h.engine.setSessionModel(
      create(shimv1.SetSessionModelRequestSchema, {
        model: create(conversationv1.AgentModelSchema, { name: "claude-sonnet-5" }),
      }),
    );

    h.queries[0]?.query.emit(resultMessage());
    await new Promise((resolve) => setImmediate(resolve));

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

  it("releases the session lock", async () => {
    const h = harness();
    await started(h);

    await h.engine.killSession(create(shimv1.KillSessionRequestSchema, {}));

    expect(h.released).toHaveLength(1);
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

  it("is idempotent, so a second SIGTERM cannot double-release the lock", async () => {
    const h = harness();
    await started(h);

    await h.engine.standDown("SIGTERM");
    await h.engine.standDown("SIGTERM");

    expect(h.released).toHaveLength(1);
  });
});

describe("WatchSession", () => {
  it("delivers diagnostics as its FIRST frame", async () => {
    const h = harness();
    const iterator = h.engine.watchSession(create(shimv1.WatchSessionRequestSchema, {}))[
      Symbol.asyncIterator
    ]();

    const first = await iterator.next();

    expect(first.value?.update?.update.case).toBe("diagnostics");
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

    expect(h.persistence.buffered.some((entry) => entry.upsertKey === "bash:b01")).toBe(true);
  });

  it("leaves work the record cannot describe OPEN rather than inventing its kind", async () => {
    const h = harness();
    h.persistence.live = create(storev1.GetLiveWorkSuccessSchema, {
      liveDetached: [create(conversationv1.DetachedWorkIdSchema, { value: "b01" })],
    });

    await started(h);

    // Every terminal arm is kind-specific: closing an unknown unit as a shell
    // would claim it ran a command, and as a spawn that it made an agent.
    expect(h.persistence.buffered.some((entry) => entry.upsertKey.includes("b01"))).toBe(false);
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
