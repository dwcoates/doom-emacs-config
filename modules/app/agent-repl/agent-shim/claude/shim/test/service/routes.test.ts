/**
 * The routing layer, driven in-process through a router transport.
 *
 * WHAT THESE GUARD: that each of the seventeen verbs reaches the RIGHT engine
 * method with the request unchanged, that the workflow trio is refused WITHOUT
 * an engine ever being consulted, and that validation runs BEFORE the engine so
 * an illegal request can never reach a session.
 */
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError, createClient, createRouterTransport } from "@connectrpc/connect";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { logRecordsSince, logSinkMark } from "../log-records.js";
import type { Engine } from "../../src/engine/engine.js";
import { conversationv1, shimv1 } from "../../src/proto.js";
import * as failures from "../../src/service/failures.js";
import { shimRoutes } from "../../src/service/routes.js";
import * as requests from "./requests.js";

function requestBoundariesSince(before: number, rpc: string): Array<{ level: unknown; boundary: unknown }> {
  return logRecordsSince(before)
    .filter((record) => record.context !== undefined && record.context.rpc === rpc)
    .map((record) => ({ level: record.level, boundary: record.context.boundary }))
    .filter((record) => record.boundary !== undefined);
}

/** An engine that records what it was asked and answers the emptiest legal thing. */
function recordingEngine(): { engine: Engine; calls: Array<{ verb: string; request: unknown }> } {
  const calls: Array<{ verb: string; request: unknown }> = [];
  const record = (verb: string) => (request: unknown): void => {
    calls.push({ verb, request });
  };
  const engine: Engine = {
    async startSession(request) {
      record("startSession")(request);
      return create(shimv1.StartSessionResponseSchema, {});
    },
    async *watchSession(request) {
      record("watchSession")(request);
      yield create(shimv1.WatchSessionResponseSchema, {});
    },
    async setSessionModel(request) {
      record("setSessionModel")(request);
      return create(shimv1.SetSessionModelResponseSchema, {});
    },
    async setSessionPermissionMode(request) {
      record("setSessionPermissionMode")(request);
      return create(shimv1.SetSessionPermissionModeResponseSchema, {});
    },
    async hibernate(request) {
      record("hibernate")(request);
      return create(shimv1.HibernateResponseSchema, {});
    },
    async killSession(request) {
      record("killSession")(request);
      return create(shimv1.KillSessionResponseSchema, {});
    },
    async startTurn(request) {
      record("startTurn")(request);
      return create(shimv1.StartTurnResponseSchema, {});
    },
    async *watchAgent(request) {
      record("watchAgent")(request);
      yield create(shimv1.WatchAgentResponseSchema, {});
    },
    async updateAgent(request) {
      record("updateAgent")(request);
      return create(shimv1.UpdateAgentResponseSchema, {});
    },
    async killTurn(request) {
      record("killTurn")(request);
      return create(shimv1.KillTurnResponseSchema, {});
    },
    async rollBackSession(request) {
      record("rollBackSession")(request);
      return create(shimv1.RollBackSessionResponseSchema, {});
    },
    async *watchBash(request) {
      record("watchBash")(request);
      yield create(shimv1.WatchBashResponseSchema, {});
    },
    async stopBash(request) {
      record("stopBash")(request);
      return create(shimv1.StopBashResponseSchema, {});
    },
    async detachForeground(request) {
      record("detachForeground")(request);
      return create(shimv1.DetachForegroundResponseSchema, {});
    },
    async readHistory(request) {
      record("readHistory")(request);
      return create(shimv1.ReadHistoryResponseSchema, {});
    },
    async readTranscripts(request) {
      record("readTranscripts")(request);
      return create(shimv1.ReadTranscriptsResponseSchema, {});
    },
    async gatherTitleDigest(request) {
      record("gatherTitleDigest")(request);
      return create(shimv1.GatherTitleDigestResponseSchema, {});
    },
    async standDown() {
      record("standDown")(undefined);
      return 0;
    },
  };
  return { engine, calls };
}

function clientFor(engine: Engine) {
  return createClient(shimv1.Shim, createRouterTransport(shimRoutes(engine)));
}

async function drain(stream: AsyncIterable<unknown>): Promise<void> {
  for await (const _frame of stream) break;
}

async function drainToCompletion(stream: AsyncIterable<unknown>): Promise<void> {
  for await (const _frame of stream) {
    // The completion boundary is emitted only after the producer ends.
  }
}

describe("shimRoutes unary delegation", () => {
  it.each([
    ["startSession", requests.startSessionRequest],
    ["setSessionModel", requests.setSessionModelRequest],
    ["setSessionPermissionMode", requests.setSessionPermissionModeRequest],
    ["hibernate", requests.hibernateRequest],
    ["killSession", requests.killSessionRequest],
    ["startTurn", requests.startTurnRequest],
    ["updateAgent", requests.updateAgentRequest],
    ["killTurn", requests.killTurnRequest],
    ["rollBackSession", requests.rollBackSessionRequest],
    ["stopBash", requests.stopBashRequest],
    ["detachForeground", requests.detachForegroundRequest],
    ["readHistory", requests.readHistoryRequest],
    ["gatherTitleDigest", requests.gatherTitleDigestRequest],
  ] as const)("routes %s to its own engine method", async (verb, build) => {
    // Arrange.
    const { engine, calls } = recordingEngine();
    const client = clientFor(engine);

    // Act.
    await (client[verb] as (r: unknown) => Promise<unknown>)(build());

    // Assert.
    expect(calls.map((call) => call.verb)).toEqual([verb]);
  });
});

/**
 * RollBackSession's arms reach the caller exactly as the engine answered them:
 * the arm IS the outcome, so a handler that reshaped one would change what
 * happened.
 */
describe("shimRoutes RollBackSession outcomes", () => {
  it.each([
    ["success keeping the files", failures.rollBackSessionSucceeded(undefined)],
    ["success restoring the files", failures.rollBackSessionSucceeded(["/ws/a.ts"])],
    ["no_session", failures.rollBackSessionRefused({ kind: "noSession" }, "d")],
    ["prompt_not_recorded", failures.rollBackSessionRefused({ kind: "promptNotRecorded" }, "d")],
    ["first_prompt", failures.rollBackSessionRefused({ kind: "firstPrompt" }, "d")],
    ["unseen_prompt", failures.rollBackSessionRefused({ kind: "unseenPrompt", vendorPromptUuid: "u-9" }, "d")],
    ["vendor_refused", failures.rollBackSessionRefused({ kind: "vendorRefused", vendorMessage: "no" }, "d")],
    ["files_not_restorable", failures.rollBackSessionRefused({ kind: "filesNotRestorable", vendorMessage: "no" }, "d")],
  ] as const)("answers %s unchanged", async (_arm, answer) => {
    // Arrange.
    const { engine } = recordingEngine();
    engine.rollBackSession = () => Promise.resolve(answer);
    const client = clientFor(engine);

    // Act.
    const response = await client.rollBackSession(requests.rollBackSessionRequest());

    // Assert.
    expect(response).toEqual(answer);
  });
});

describe("shimRoutes stream delegation", () => {
  it.each([
    ["watchSession", requests.watchSessionRequest],
    ["watchAgent", requests.watchAgentRequest],
    ["watchBash", requests.watchBashRequest],
  ] as const)("routes %s to its own engine method", async (verb, build) => {
    // Arrange.
    const { engine, calls } = recordingEngine();
    const client = clientFor(engine);

    // Act.
    await drain((client[verb] as (r: unknown) => AsyncIterable<unknown>)(build()));

    // Assert.
    expect(calls.map((call) => call.verb)).toEqual([verb]);
  });
});

describe("shimRoutes request fidelity", () => {
  it("hands the engine the request unchanged", async () => {
    // Arrange.
    const { engine, calls } = recordingEngine();
    const request = requests.startTurnRequest();

    // Act.
    await clientFor(engine).startTurn(request);

    // Assert.
    expect(calls[0]?.request).toEqual(request);
  });
});

describe("shimRoutes request-boundary records", () => {
  it("records a successful unary request's entry and completion at debug", async () => {
    // Arrange.
    const { engine } = recordingEngine();
    const before = logSinkMark();

    // Act.
    await clientFor(engine).startSession(requests.startSessionRequest());

    // Assert.
    expect(requestBoundariesSince(before, "StartSession")).toEqual([
      { level: "debug", boundary: "entered" },
      { level: "debug", boundary: "completed" },
    ]);
  });

  it("records a successful streaming request's entry and completion at debug", async () => {
    // Arrange.
    const { engine } = recordingEngine();
    const before = logSinkMark();

    // Act.
    await drainToCompletion(clientFor(engine).watchSession(requests.watchSessionRequest()));

    // Assert.
    expect(requestBoundariesSince(before, "WatchSession")).toEqual([
      { level: "debug", boundary: "entered" },
      { level: "debug", boundary: "completed" },
    ]);
  });
});

describe("shimRoutes workflow verbs", () => {
  it.each([
    ["getWorkflow", requests.getWorkflowRequest],
    ["stopWorkflow", requests.stopWorkflowRequest],
  ] as const)("refuses %s with Unimplemented", async (verb, build) => {
    // Arrange.
    const { engine } = recordingEngine();

    // Act.
    const rejection = await (clientFor(engine)[verb] as (r: unknown) => Promise<unknown>)(build())
      .then(() => null, (err: unknown) => ConnectError.from(err));

    // Assert.
    expect(rejection?.code).toBe(Code.Unimplemented);
  });

  it("refuses the WatchWorkflow stream with Unimplemented", async () => {
    // Arrange.
    const { engine } = recordingEngine();

    // Act.
    const rejection = await drain(
      clientFor(engine).watchWorkflow(requests.watchWorkflowRequest()),
    ).then(() => null, (err: unknown) => ConnectError.from(err));

    // Assert.
    expect(rejection?.code).toBe(Code.Unimplemented);
  });

  it("never consults an engine for a kicked verb", async () => {
    // Arrange.
    const { engine, calls } = recordingEngine();

    // Act.
    await clientFor(engine)
      .getWorkflow(requests.getWorkflowRequest())
      .catch(() => undefined);

    // Assert.
    expect(calls).toEqual([]);
  });
});

describe("shimRoutes validation ordering", () => {
  it("refuses an unset request oneof with InvalidArgument", async () => {
    // Arrange.
    const { engine } = recordingEngine();
    const request = create(shimv1.StartSessionRequestSchema, {});

    // Act.
    const rejection = await clientFor(engine)
      .startSession(request)
      .then(() => null, (err: unknown) => ConnectError.from(err));

    // Assert.
    expect(rejection?.code).toBe(Code.InvalidArgument);
  });

  it("never reaches the engine with an illegal request", async () => {
    // Arrange.
    const { engine, calls } = recordingEngine();

    // Act.
    await clientFor(engine)
      .startSession(create(shimv1.StartSessionRequestSchema, {}))
      .catch(() => undefined);

    // Assert.
    expect(calls).toEqual([]);
  });

  it("refuses an unset non-optional message field with InvalidArgument", async () => {
    // Arrange.
    const { engine } = recordingEngine();
    const request = create(shimv1.KillTurnRequestSchema, { force: true });

    // Act.
    const rejection = await clientFor(engine)
      .killTurn(request)
      .then(() => null, (err: unknown) => ConnectError.from(err));

    // Assert.
    expect(rejection?.code).toBe(Code.InvalidArgument);
  });

  it("refuses an illegal STREAM open with InvalidArgument too", async () => {
    // Arrange.
    const { engine } = recordingEngine();
    const request = create(shimv1.WatchBashRequestSchema, {});

    // Act.
    const rejection = await drain(clientFor(engine).watchBash(request)).then(
      () => null,
      (err: unknown) => ConnectError.from(err),
    );

    // Assert.
    expect(rejection?.code).toBe(Code.InvalidArgument);
  });

  it("names the offending field path so a refusal is actionable", async () => {
    // Arrange.
    const { engine } = recordingEngine();
    const request = create(shimv1.StartTurnRequestSchema, {
      turn: create(conversationv1.TurnIdSchema, { value: "t" }),
      said: requests.said(),
      origin: conversationv1.PromptOrigin.USER_SENT,
      pageSize: 0,
    });

    // Act.
    const rejection = await clientFor(engine)
      .startTurn(request)
      .then(() => null, (err: unknown) => ConnectError.from(err));

    // Assert.
    expect(rejection?.message).toContain("start_turn.page_size");
  });
});

describe("shimRoutes stream completion", () => {
  it.each([
    ["watchSession", requests.watchSessionRequest],
    ["watchAgent", requests.watchAgentRequest],
    ["watchBash", requests.watchBashRequest],
  ] as const)("yields %s's frames through and ends when the engine's stream ends", async (verb, build) => {
    // Arrange.
    const { engine } = recordingEngine();
    const client = clientFor(engine);

    // Act: drained to completion, not broken out of, so the pass-through's own
    // end is what closes the stream.
    const frames: unknown[] = [];
    for await (const frame of (client[verb] as (r: unknown) => AsyncIterable<unknown>)(build())) {
      frames.push(frame);
    }

    // Assert.
    expect(frames).toHaveLength(1);
  });
});

describe("shimRoutes debug request boundaries", () => {
  let written: string[] = [];

  beforeEach(() => {
    written = [];
    process.env.AGENT_REPL_LOG_VERBOSE = "1";
    vi.spyOn(process.stderr, "write").mockImplementation((chunk) => {
      written.push(String(chunk));
      return true;
    });
  });

  afterEach(() => {
    delete process.env.AGENT_REPL_LOG_VERBOSE;
  });

  function boundaries(rpc: string): string[] {
    return written
      .map((line) => JSON.parse(line) as { level: string; context: Record<string, unknown> })
      .filter((record) => record.context.rpc === rpc)
      .map((record) => `${record.level}:${String(record.context.boundary)}`);
  }

  it("records both boundaries of a successful unary request at debug", async () => {
    // Arrange.
    const { engine } = recordingEngine();

    // Act.
    await clientFor(engine).startSession(requests.startSessionRequest());

    // Assert.
    expect(boundaries("StartSession")).toEqual(["debug:entered", "debug:completed"]);
  });

  it("records stream completion only after the engine stream ends", async () => {
    // Arrange.
    const { engine } = recordingEngine();

    // Act.
    const stream = clientFor(engine).watchSession(requests.watchSessionRequest());
    for await (const _frame of stream) {
      // Drain the stream so its completion boundary is reached.
    }

    // Assert.
    expect(boundaries("WatchSession")).toEqual(["debug:entered", "debug:completed"]);
  });
});

/**
 * An exception the handler never anticipated must reach the caller WITH its
 * detail: a bare `[internal] internal error` buries the evidence inside the
 * process, and on a stream it would be a silent close.
 */
describe("shimRoutes unanticipated exceptions", () => {
  /** An engine whose WatchSession throws `thrown` instead of yielding. */
  function throwingStreamEngine(thrown: unknown): Engine {
    const { engine } = recordingEngine();
    return {
      ...engine,
      async *watchSession(): AsyncIterable<shimv1.WatchSessionResponse> {
        throw thrown;
      },
    };
  }

  /** The stderr mirror, which every `error` record reaches. */
  function records(written: string[]): Array<{ level: string; context: Record<string, unknown> }> {
    return written.flatMap((line) => {
      try {
        return [JSON.parse(line) as { level: string; context: Record<string, unknown> }];
      } catch {
        return [];
      }
    });
  }

  let written: string[] = [];
  beforeEach(() => {
    written = [];
    vi.spyOn(process.stderr, "write").mockImplementation((chunk) => {
      written.push(String(chunk));
      return true;
    });
  });

  it("turns a plain exception thrown mid-stream into Internal", async () => {
    // Arrange.
    const client = clientFor(throwingStreamEngine(new Error("the fold came apart")));

    // Act.
    const rejection = await drain(client.watchSession(requests.watchSessionRequest())).then(
      () => null,
      (err: unknown) => ConnectError.from(err),
    );

    // Assert.
    expect(rejection?.code).toBe(Code.Internal);
  });

  it("carries the exception's own detail out to the caller", async () => {
    // Arrange.
    const client = clientFor(throwingStreamEngine(new Error("the fold came apart")));

    // Act.
    const rejection = await drain(client.watchSession(requests.watchSessionRequest())).then(
      () => null,
      (err: unknown) => ConnectError.from(err),
    );

    // Assert.
    expect(rejection?.message).toContain("the fold came apart");
  });

  it("names the rpc that threw, so the record points somewhere", async () => {
    // Arrange.
    const client = clientFor(throwingStreamEngine(new Error("the fold came apart")));

    // Act.
    await drain(client.watchSession(requests.watchSessionRequest())).catch(() => undefined);

    // Assert.
    expect(
      records(written).some(
        (record) => record.level === "error" && record.context.rpc === "WatchSession",
      ),
    ).toBe(true);
  });

  it("records the throwing stack, which is the only copy of it", async () => {
    // Arrange.
    const client = clientFor(throwingStreamEngine(new Error("the fold came apart")));

    // Act.
    await drain(client.watchSession(requests.watchSessionRequest())).catch(() => undefined);

    // Assert.
    expect(
      records(written).some(
        (record) => typeof record.context.stack === "string" && record.context.stack.includes("Error: the fold came apart"),
      ),
    ).toBe(true);
  });

  it("records NO stack when what was thrown was not an Error and has none", async () => {
    // Arrange: a bare string thrown mid-stream carries no stack to record.
    const client = clientFor(throwingStreamEngine("the fold came apart"));

    // Act.
    await drain(client.watchSession(requests.watchSessionRequest())).catch(() => undefined);

    // Assert. The rpc is still named — the record stays useful without a stack.
    const unhandled = records(written).filter(
      (record) => record.level === "error" && record.context.rpc === "WatchSession",
    );
    expect(unhandled.map((record) => record.context.stack)).toEqual([undefined]);
  });

  it("passes a ConnectError the shim MEANT to throw straight through", async () => {
    // Arrange.
    const client = clientFor(
      throwingStreamEngine(new ConnectError("that session is gone", Code.NotFound)),
    );

    // Act.
    const rejection = await drain(client.watchSession(requests.watchSessionRequest())).then(
      () => null,
      (err: unknown) => ConnectError.from(err),
    );

    // Assert.
    expect(rejection?.code).toBe(Code.NotFound);
  });

  it("says NOTHING about an unanticipated exception when the refusal was deliberate", async () => {
    // A record per deliberate refusal would bury the one that names a defect.
    // Arrange.
    const client = clientFor(
      throwingStreamEngine(new ConnectError("that session is gone", Code.NotFound)),
    );

    // Act.
    await drain(client.watchSession(requests.watchSessionRequest())).catch(() => undefined);

    // Assert.
    expect(records(written).some((record) => record.level === "error")).toBe(false);
  });

  /** The exception Node raises for a write onto a stream the peer already reset. */
  function streamDestroyed(): Error {
    return Object.assign(new Error("The stream has been destroyed"), {
      code: "ERR_HTTP2_INVALID_STREAM",
    });
  }

  it("does NOT call a write onto a peer-destroyed stream an unanticipated exception", async () => {
    // A cancel is the ordinary end of a standing watch; recorded as a defect it
    // buries the exceptions that are one.
    // Arrange.
    const client = clientFor(throwingStreamEngine(streamDestroyed()));

    // Act.
    await drain(client.watchSession(requests.watchSessionRequest())).catch(() => undefined);

    // Assert.
    expect(records(written).some((record) => record.level === "error")).toBe(false);
  });

  it("still RECORDS the peer's departure, which the log is the only place to learn", async () => {
    // Arrange.
    const client = clientFor(throwingStreamEngine(streamDestroyed()));

    // Act.
    await drain(client.watchSession(requests.watchSessionRequest())).catch(() => undefined);

    // Assert.
    expect(
      records(written).some(
        (record) => record.level === "info" && record.context.rpc === "WatchSession",
      ),
    ).toBe(true);
  });

  it("reads the departure off the message when the code did not survive a wrapper", async () => {
    // Arrange: the same failure re-thrown with its text but without its code.
    const client = clientFor(throwingStreamEngine(new Error("The stream has been destroyed")));

    // Act.
    await drain(client.watchSession(requests.watchSessionRequest())).catch(() => undefined);

    // Assert.
    expect(records(written).some((record) => record.level === "error")).toBe(false);
  });

  it("still answers the caller Internal, because a listener is owed the failure", async () => {
    // Arrange.
    const client = clientFor(throwingStreamEngine(streamDestroyed()));

    // Act.
    const rejection = await drain(client.watchSession(requests.watchSessionRequest())).then(
      () => null,
      (err: unknown) => ConnectError.from(err),
    );

    // Assert.
    expect(rejection?.code).toBe(Code.Internal);
  });
});
