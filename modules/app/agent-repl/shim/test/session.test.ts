import { describe, expect, it } from "vitest";
import { AsyncQueue } from "../src/input-queue.js";
import { ModelInfo, PermissionMode, ShimEvent, SlashCommand } from "../src/protocol.js";
import {
  CanUseToolLike,
  PermissionResultLike,
  QueryLike,
  SdkMessageLike,
  SdkUserMessageLike,
  ShimSession,
} from "../src/session.js";

/** Wait until `pred` holds, advancing the event loop without timers. */
async function until(pred: () => boolean): Promise<void> {
  for (let i = 0; i < 1000; i++) {
    if (pred()) return;
    await new Promise<void>((resolve) => setImmediate(resolve));
  }
  throw new Error("until(): predicate never became true");
}

interface Harness {
  session: ShimSession;
  emitted: ShimEvent[];
  sdkOut: AsyncQueue<SdkMessageLike>;
  userMessages: SdkUserMessageLike[];
  canUseTool: () => CanUseToolLike;
  interruptCalls: () => number;
  modeCalls: () => PermissionMode[];
  modelCalls: () => string[];
  probeCalls: () => number;
  exitCode: () => number | null;
  pump: Promise<void>;
  send: (frame: Record<string, unknown>) => void;
  eventsOfType: <T extends ShimEvent["type"]>(t: T) => Array<Extract<ShimEvent, { type: T }>>;
}

interface HarnessOpts {
  setPermissionModeError?: Error;
  setModelError?: Error;
  supportedModels?: ModelInfo[];
  supportedModelsError?: Error;
  supportedCommands?: SlashCommand[];
  supportedCommandsError?: Error;
  /** What a `refresh-commands` probe resolves to (defaults to REFRESHED_COMMANDS). */
  probeCommands?: SlashCommand[];
  probeCommandsError?: Error;
}

const FAKE_MODELS: ModelInfo[] = [
  { value: "claude-opus-4-5", displayName: "Opus 4.5", description: "smartest" },
  { value: "claude-haiku-4-5", displayName: "Haiku 4.5", description: "fastest" },
];

const FAKE_COMMANDS: SlashCommand[] = [
  { name: "compact", description: "summarize the context", argumentHint: "<instructions>" },
  { name: "debug-logs", description: "read the debug log", argumentHint: "" },
];

/** The list a re-probe resolves: a skill added since the session started. */
const REFRESHED_COMMANDS: SlashCommand[] = [
  ...FAKE_COMMANDS,
  { name: "brand-new-skill", description: "added mid-session", argumentHint: "" },
];

function makeHarness(opts?: HarnessOpts): Harness {
  const emitted: ShimEvent[] = [];
  const sdkOut = new AsyncQueue<SdkMessageLike>();
  const userMessages: SdkUserMessageLike[] = [];
  let capturedCanUseTool: CanUseToolLike | null = null;
  let interruptCount = 0;
  const modes: PermissionMode[] = [];
  const models: string[] = [];
  let probeCount = 0;
  let exit: number | null = null;
  let requestCounter = 0;

  const query: QueryLike = {
    [Symbol.asyncIterator]: () => sdkOut[Symbol.asyncIterator](),
    interrupt: async () => {
      interruptCount++;
    },
    setPermissionMode: async (mode) => {
      if (opts?.setPermissionModeError) throw opts.setPermissionModeError;
      modes.push(mode);
    },
    setModel: async (model) => {
      if (opts?.setModelError) throw opts.setModelError;
      models.push(model);
    },
    supportedModels: async () => {
      if (opts?.supportedModelsError) throw opts.supportedModelsError;
      return opts?.supportedModels ?? FAKE_MODELS;
    },
    supportedCommands: async () => {
      if (opts?.supportedCommandsError) throw opts.supportedCommandsError;
      return opts?.supportedCommands ?? FAKE_COMMANDS;
    },
  };

  const session = new ShimSession({
    sessionId: "sess-1",
    shimVersion: "0.0.0-test",
    sdkVersion: "0.0.0-sdk",
    initialPermissionMode: "default",
    createQuery: (prompt, canUseTool) => {
      capturedCanUseTool = canUseTool;
      void (async () => {
        for await (const m of prompt) userMessages.push(m);
      })();
      return query;
    },
    probeCommands: async () => {
      probeCount++;
      if (opts?.probeCommandsError) throw opts.probeCommandsError;
      return opts?.probeCommands ?? REFRESHED_COMMANDS;
    },
    emit: (evt) => emitted.push(evt),
    exit: (code) => {
      exit = code;
    },
    newRequestId: () => `perm-${++requestCounter}`,
  });
  const pump = session.start();

  return {
    session,
    emitted,
    sdkOut,
    userMessages,
    canUseTool: () => capturedCanUseTool!,
    interruptCalls: () => interruptCount,
    modeCalls: () => modes,
    modelCalls: () => models,
    probeCalls: () => probeCount,
    exitCode: () => exit,
    pump,
    send: (frame) => session.handleLine(JSON.stringify(frame)),
    eventsOfType: (t) => emitted.filter((e) => e.type === t) as never,
  };
}

describe("ShimSession lifecycle", () => {
  it("emits ready with the handshake fields on start", () => {
    // Arrange + Act
    const h = makeHarness();
    // Assert
    expect(h.emitted[0]).toEqual({
      type: "ready",
      session_id: "sess-1",
      shim_version: "0.0.0-test",
      sdk_version: "0.0.0-sdk",
      permission_mode: "default",
    });
  });

  it("emits closed with reason sdk_end when the query ends without shutdown", async () => {
    // Arrange
    const h = makeHarness();
    // Act
    h.sdkOut.end();
    await h.pump;
    // Assert
    expect(h.eventsOfType("closed")).toEqual([
      { type: "closed", session_id: "sess-1", exit_code: 0, reason: "sdk_end" },
    ]);
    expect(h.exitCode()).toBe(0);
  });

  it("acks shutdown and closes with reason shutdown and the request_id", async () => {
    // Arrange
    const h = makeHarness();
    // Act
    h.send({ type: "shutdown", request_id: "r-shut" });
    h.sdkOut.end();
    await h.pump;
    // Assert
    expect(h.eventsOfType("ack")).toEqual([
      { type: "ack", session_id: "sess-1", request_id: "r-shut" },
    ]);
    expect(h.eventsOfType("closed")).toEqual([
      {
        type: "closed",
        session_id: "sess-1",
        request_id: "r-shut",
        exit_code: 0,
        reason: "shutdown",
      },
    ]);
  });

  it("rejects commands received after shutdown with shutdown_in_progress", async () => {
    // Arrange
    const h = makeHarness();
    h.send({ type: "shutdown", request_id: "r-shut" });
    // Act
    h.send({ type: "user-message", request_id: "r-late", content: "too late" });
    // Assert
    const errs = h.eventsOfType("error");
    expect(errs).toHaveLength(1);
    expect(errs[0]).toMatchObject({ code: "shutdown_in_progress", request_id: "r-late" });
  });

  it("treats stdin end as a shutdown without a request_id", async () => {
    // Arrange
    const h = makeHarness();
    // Act
    h.session.handleStdinEnd();
    h.sdkOut.end();
    await h.pump;
    // Assert
    expect(h.eventsOfType("closed")).toEqual([
      { type: "closed", session_id: "sess-1", exit_code: 0, reason: "shutdown" },
    ]);
  });

  it("emits error sdk_throw then closed fatal_error when the query throws", async () => {
    // Arrange
    const emitted: ShimEvent[] = [];
    let exit: number | null = null;
    const throwingQuery: QueryLike = {
      // eslint-disable-next-line require-yield
      [Symbol.asyncIterator]: () => ({
        next: async (): Promise<IteratorResult<SdkMessageLike>> => {
          throw new Error("boom");
        },
      }),
      interrupt: async () => {},
      setPermissionMode: async () => {},
      setModel: async () => {},
      supportedModels: async () => [],
      supportedCommands: async () => [],
    };
    const session = new ShimSession({
      sessionId: "sess-1",
      shimVersion: "v",
      sdkVersion: "v",
      initialPermissionMode: "default",
      createQuery: () => throwingQuery,
      probeCommands: async () => [],
      emit: (e) => emitted.push(e),
      exit: (c) => {
        exit = c;
      },
      newRequestId: () => "p",
    });
    // Act
    await session.start();
    // Assert
    expect(emitted.some((e) => e.type === "error" && e.code === "sdk_throw" && /boom/.test(e.message))).toBe(true);
    const closed = emitted.find((e) => e.type === "closed");
    expect(closed).toMatchObject({ reason: "fatal_error", exit_code: 1 });
    expect(exit).toBe(1);
  });
});

describe("ShimSession command handling", () => {
  it("normalizes string user-message content into one text block", async () => {
    // Arrange
    const h = makeHarness();
    // Act
    h.send({ type: "user-message", request_id: "r1", content: "hi" });
    await until(() => h.userMessages.length === 1);
    // Assert
    expect(h.userMessages[0]).toEqual({
      type: "user",
      message: { role: "user", content: [{ type: "text", text: "hi" }] },
      parent_tool_use_id: null,
      session_id: "sess-1",
    });
  });

  it("passes parent_tool_use_id through on user-message", async () => {
    // Arrange
    const h = makeHarness();
    // Act
    h.send({
      type: "user-message",
      request_id: "r1",
      content: [{ type: "text", text: "hi" }],
      parent_tool_use_id: "toolu_1",
    });
    await until(() => h.userMessages.length === 1);
    // Assert
    expect(h.userMessages[0].parent_tool_use_id).toBe("toolu_1");
  });

  it("ignores frames with unknown types without emitting anything", () => {
    // Arrange
    const h = makeHarness();
    const before = h.emitted.length;
    // Act
    h.send({ type: "novel-command", request_id: "r1" });
    // Assert
    expect(h.emitted.length).toBe(before);
  });

  it("emits error bad_command for malformed JSON lines", () => {
    // Arrange
    const h = makeHarness();
    // Act
    h.session.handleLine("{not json");
    // Assert
    expect(h.eventsOfType("error")[0]).toMatchObject({ code: "bad_command" });
  });

  it("calls query.interrupt on the interrupt command", async () => {
    // Arrange
    const h = makeHarness();
    // Act
    h.send({ type: "interrupt", request_id: "r1" });
    await until(() => h.interruptCalls() === 1);
    // Assert
    expect(h.interruptCalls()).toBe(1);
  });

  it("acks set-permission-mode after applying it to the query", async () => {
    // Arrange
    const h = makeHarness();
    // Act
    h.send({ type: "set-permission-mode", request_id: "r1", mode: "plan" });
    await until(() => h.eventsOfType("ack").length === 1);
    // Assert
    expect(h.modeCalls()).toEqual(["plan"]);
    expect(h.eventsOfType("ack")[0]).toEqual({
      type: "ack",
      session_id: "sess-1",
      request_id: "r1",
    });
  });

  it("emits error sdk_throw with the request_id when set-permission-mode fails", async () => {
    // Arrange
    const h = makeHarness({ setPermissionModeError: new Error("nope") });
    // Act
    h.send({ type: "set-permission-mode", request_id: "r1", mode: "plan" });
    await until(() => h.eventsOfType("error").length === 1);
    // Assert
    expect(h.eventsOfType("error")[0]).toMatchObject({
      code: "sdk_throw",
      request_id: "r1",
    });
  });

  it("applies set-model to the query", async () => {
    // Arrange
    const h = makeHarness();
    // Act
    h.send({ type: "set-model", request_id: "r1", model: "claude-haiku-4-5" });
    await until(() => h.modelCalls().length === 1);
    // Assert
    expect(h.modelCalls()).toEqual(["claude-haiku-4-5"]);
  });

  it("acks set-model once the query has applied it", async () => {
    // Arrange
    const h = makeHarness();
    // Act
    h.send({ type: "set-model", request_id: "r1", model: "claude-haiku-4-5" });
    await until(() => h.eventsOfType("ack").length === 1);
    // Assert — the ack is what licenses the daemon to emit model-changed.
    expect(h.eventsOfType("ack")[0]).toEqual({
      type: "ack",
      session_id: "sess-1",
      request_id: "r1",
    });
  });

  it("emits error sdk_throw with the request_id when set-model fails", async () => {
    // Arrange
    const h = makeHarness({ setModelError: new Error("nope") });
    // Act
    h.send({ type: "set-model", request_id: "r1", model: "claude-haiku-4-5" });
    await until(() => h.eventsOfType("error").length === 1);
    // Assert
    expect(h.eventsOfType("error")[0]).toMatchObject({
      code: "sdk_throw",
      request_id: "r1",
    });
  });

  it("does not ack a set-model the query rejected", async () => {
    // Arrange — a failed switch must not look like a successful one, or
    // the daemon would broadcast a model the session is not actually on.
    const h = makeHarness({ setModelError: new Error("nope") });
    // Act
    h.send({ type: "set-model", request_id: "r1", model: "claude-haiku-4-5" });
    await until(() => h.eventsOfType("error").length === 1);
    // Assert
    expect(h.eventsOfType("ack")).toEqual([]);
  });
});

describe("ShimSession model menu", () => {
  it("publishes the SDK's supported models on start", async () => {
    // Arrange
    const h = makeHarness();
    // Act
    await until(() => h.eventsOfType("models").length === 1);
    // Assert
    expect(h.eventsOfType("models")[0]).toEqual({
      type: "models",
      session_id: "sess-1",
      models: FAKE_MODELS,
    });
  });

  it("publishes the model menu without being asked for it", async () => {
    // Arrange — no command is sent at all.
    const h = makeHarness();
    // Act
    await until(() => h.eventsOfType("models").length === 1);
    // Assert — the list rides on startup, not on a request.
    expect(h.eventsOfType("models")).toHaveLength(1);
  });

  it("surfaces a supportedModels failure as a recoverable sdk_throw", async () => {
    // Arrange — an unpopulated picker is a degraded session, not a dead
    // one, so the error carries a request_id (which is what marks it
    // recoverable to the daemon) rather than reading as a shim death.
    const h = makeHarness({ supportedModelsError: new Error("no models") });
    // Act
    await until(() => h.eventsOfType("error").length === 1);
    // Assert
    const err = h.eventsOfType("error")[0];
    expect(err).toMatchObject({ code: "sdk_throw" });
    expect(err.request_id).toBeTruthy();
  });

  it("emits no models event when supportedModels fails", async () => {
    // Arrange
    const h = makeHarness({ supportedModelsError: new Error("no models") });
    // Act
    await until(() => h.eventsOfType("error").length === 1);
    // Assert — an empty menu is never asserted as if it were the truth.
    expect(h.eventsOfType("models")).toEqual([]);
  });
});

describe("ShimSession command menu", () => {
  it("publishes the SDK's supported commands on start", async () => {
    // Arrange
    const h = makeHarness();
    // Act
    await until(() => h.eventsOfType("commands").length === 1);
    // Assert
    expect(h.eventsOfType("commands")[0]).toEqual({
      type: "commands",
      session_id: "sess-1",
      commands: FAKE_COMMANDS,
    });
  });

  it("publishes the command menu without being asked for it", async () => {
    // Arrange — no command is sent at all.
    const h = makeHarness();
    // Act
    await until(() => h.eventsOfType("commands").length === 1);
    // Assert — the list rides on startup, not on a request.
    expect(h.eventsOfType("commands")).toHaveLength(1);
  });

  it("carries the argument hint of a command that takes an argument", async () => {
    // Arrange — the hint is what the completion UI renders as an annotation.
    const h = makeHarness();
    // Act
    await until(() => h.eventsOfType("commands").length === 1);
    // Assert
    const compact = h.eventsOfType("commands")[0].commands.find((c) => c.name === "compact");
    expect(compact?.argumentHint).toBe("<instructions>");
  });

  it("surfaces a supportedCommands failure as a recoverable sdk_throw", async () => {
    // Arrange — an unpopulated completion menu is a degraded session, not a
    // dead one, so the error carries a request_id (which is what marks it
    // recoverable to the daemon) rather than reading as a shim death.
    const h = makeHarness({ supportedCommandsError: new Error("no commands") });
    // Act
    await until(() => h.eventsOfType("error").length === 1);
    // Assert
    const err = h.eventsOfType("error")[0];
    expect(err).toMatchObject({ code: "sdk_throw" });
    expect(err.request_id).toBeTruthy();
  });

  it("emits no commands event when supportedCommands fails", async () => {
    // Arrange
    const h = makeHarness({ supportedCommandsError: new Error("no commands") });
    // Act
    await until(() => h.eventsOfType("error").length === 1);
    // Assert — an empty menu is never asserted as if it were the truth.
    expect(h.eventsOfType("commands")).toEqual([]);
  });
});

describe("ShimSession refresh-commands", () => {
  it("republishes the menu from a fresh probe rather than the live query", async () => {
    // Arrange — the live query's supportedCommands() is memoized against the
    // init handshake, so only the probe can see a skill added since.
    const h = makeHarness();
    await until(() => h.eventsOfType("commands").length === 1);
    // Act
    h.send({ type: "refresh-commands", request_id: "r1" });
    await until(() => h.eventsOfType("commands").length === 2);
    // Assert
    expect(h.eventsOfType("commands")[1].commands).toEqual(REFRESHED_COMMANDS);
  });

  it("probes exactly once per refresh-commands", async () => {
    // Arrange
    const h = makeHarness();
    await until(() => h.eventsOfType("commands").length === 1);
    // Act
    h.send({ type: "refresh-commands", request_id: "r1" });
    await until(() => h.eventsOfType("commands").length === 2);
    // Assert — the startup publish rides the live query, so the probe (which
    // costs a process spawn) must not have run for it.
    expect(h.probeCalls()).toBe(1);
  });

  it("acks the refresh-commands once the fresh menu is published", async () => {
    // Arrange
    const h = makeHarness();
    // Act
    h.send({ type: "refresh-commands", request_id: "r1" });
    await until(() => h.eventsOfType("ack").length === 1);
    // Assert
    expect(h.eventsOfType("ack")[0]).toEqual({
      type: "ack",
      session_id: "sess-1",
      request_id: "r1",
    });
  });

  it("surfaces a failed probe as an sdk_throw carrying the request_id", async () => {
    // Arrange — a refresh that cannot spawn its probe leaves the session
    // perfectly usable on the menu it already has.
    const h = makeHarness({ probeCommandsError: new Error("spawn failed") });
    // Act
    h.send({ type: "refresh-commands", request_id: "r1" });
    await until(() => h.eventsOfType("error").length === 1);
    // Assert
    expect(h.eventsOfType("error")[0]).toMatchObject({
      code: "sdk_throw",
      request_id: "r1",
    });
  });

  it("does not ack a refresh-commands whose probe failed", async () => {
    // Arrange
    const h = makeHarness({ probeCommandsError: new Error("spawn failed") });
    // Act
    h.send({ type: "refresh-commands", request_id: "r1" });
    await until(() => h.eventsOfType("error").length === 1);
    // Assert
    expect(h.eventsOfType("ack")).toEqual([]);
  });

  it("republishes nothing when the probe failed", async () => {
    // Arrange
    const h = makeHarness({ probeCommandsError: new Error("spawn failed") });
    await until(() => h.eventsOfType("commands").length === 1);
    // Act
    h.send({ type: "refresh-commands", request_id: "r1" });
    await until(() => h.eventsOfType("error").length === 1);
    // Assert — the startup menu stands; a failed refresh never blanks it.
    expect(h.eventsOfType("commands")).toHaveLength(1);
  });
});

describe("ShimSession permission flow", () => {
  function startPermission(h: Harness): Promise<PermissionResultLike> {
    return h.canUseTool()("Bash", { command: "ls" }, {
      signal: new AbortController().signal,
      toolUseID: "toolu_9",
      suggestions: [{ hint: 1 }],
    });
  }

  it("emits permission-request when the SDK invokes canUseTool", () => {
    // Arrange
    const h = makeHarness();
    // Act
    void startPermission(h);
    // Assert
    expect(h.eventsOfType("permission-request")).toEqual([
      {
        type: "permission-request",
        session_id: "sess-1",
        request_id: "perm-1",
        tool_use_id: "toolu_9",
        tool_name: "Bash",
        input: { command: "ls" },
        suggestions: [{ hint: 1 }],
      },
    ]);
  });

  it("resolves allow decisions with the original input when no updated_input given", async () => {
    // Arrange
    const h = makeHarness();
    const resultP = startPermission(h);
    // Act
    h.send({
      type: "permission-decision",
      request_id: "perm-1",
      decision: { behavior: "allow" },
    });
    // Assert
    await expect(resultP).resolves.toEqual({
      behavior: "allow",
      updatedInput: { command: "ls" },
      updatedPermissions: undefined,
    });
  });

  it("resolves allow decisions with updated_input when provided", async () => {
    // Arrange
    const h = makeHarness();
    const resultP = startPermission(h);
    // Act
    h.send({
      type: "permission-decision",
      request_id: "perm-1",
      decision: { behavior: "allow", updated_input: { command: "ls -la" } },
    });
    // Assert
    await expect(resultP).resolves.toMatchObject({
      behavior: "allow",
      updatedInput: { command: "ls -la" },
    });
  });

  it("resolves deny decisions with the message and interrupt flag", async () => {
    // Arrange
    const h = makeHarness();
    const resultP = startPermission(h);
    // Act
    h.send({
      type: "permission-decision",
      request_id: "perm-1",
      decision: { behavior: "deny", message: "no", interrupt: true },
    });
    // Assert
    await expect(resultP).resolves.toEqual({
      behavior: "deny",
      message: "no",
      interrupt: true,
    });
  });

  it("emits error bad_command for a decision with an unknown request_id", () => {
    // Arrange
    const h = makeHarness();
    // Act
    h.send({
      type: "permission-decision",
      request_id: "perm-404",
      decision: { behavior: "allow" },
    });
    // Assert
    expect(h.eventsOfType("error")[0]).toMatchObject({
      code: "bad_command",
      request_id: "perm-404",
    });
  });

  it("resolves a pending permission as deny when the SDK aborts it", async () => {
    // Arrange
    const h = makeHarness();
    const controller = new AbortController();
    const resultP = h.canUseTool()("Bash", { command: "ls" }, {
      signal: controller.signal,
      toolUseID: "toolu_9",
    });
    // Act
    controller.abort();
    // Assert
    await expect(resultP).resolves.toMatchObject({ behavior: "deny" });
  });

  it("cancels pending permissions when an interrupt command arrives", async () => {
    // Arrange
    const h = makeHarness();
    const resultP = startPermission(h);
    // Act
    h.send({ type: "interrupt", request_id: "r1" });
    // Assert
    await expect(resultP).resolves.toMatchObject({
      behavior: "deny",
      message: expect.stringContaining("cancelled"),
    });
  });

  it("cancels pending permissions when shutdown arrives", async () => {
    // Arrange
    const h = makeHarness();
    const resultP = startPermission(h);
    // Act
    h.send({ type: "shutdown", request_id: "r-shut" });
    // Assert
    await expect(resultP).resolves.toMatchObject({ behavior: "deny" });
  });
});

describe("ShimSession SDK message mapping", () => {
  async function drive(h: Harness, ...msgs: SdkMessageLike[]): Promise<void> {
    for (const m of msgs) h.sdkOut.push(m);
    h.sdkOut.end();
    await h.pump;
  }

  it("maps stream_event messages to stream-event events", async () => {
    // Arrange
    const h = makeHarness();
    const event = { type: "message_stop" };
    // Act
    await drive(h, { type: "stream_event", uuid: "u1", parent_tool_use_id: null, event });
    // Assert
    expect(h.eventsOfType("stream-event")).toEqual([
      { type: "stream-event", session_id: "sess-1", uuid: "u1", event },
    ]);
  });

  it("preserves parent_tool_use_id on stream-event when present", async () => {
    // Arrange
    const h = makeHarness();
    // Act
    await drive(h, {
      type: "stream_event",
      uuid: "u1",
      parent_tool_use_id: "toolu_p",
      event: { type: "ping" },
    });
    // Assert
    expect(h.eventsOfType("stream-event")[0].parent_tool_use_id).toBe("toolu_p");
  });

  it("maps assistant messages onto the assistant-message shape", async () => {
    // Arrange
    const h = makeHarness();
    const message = {
      id: "msg_1",
      model: "m",
      stop_reason: "end_turn",
      content: [{ type: "text", text: "yo" }],
      usage: { input_tokens: 1, output_tokens: 2 },
    };
    // Act
    await drive(h, { type: "assistant", uuid: "u2", parent_tool_use_id: null, message });
    // Assert
    expect(h.eventsOfType("assistant-message")).toEqual([
      {
        type: "assistant-message",
        session_id: "sess-1",
        uuid: "u2",
        message: { ...message, role: "assistant" },
      },
    ]);
  });

  it("decomposes user messages into one tool-result event per tool_result block", async () => {
    // Arrange
    const h = makeHarness();
    // Act
    await drive(h, {
      type: "user",
      uuid: "u3",
      parent_tool_use_id: null,
      message: {
        role: "user",
        content: [
          { type: "tool_result", tool_use_id: "t1", content: "ok", is_error: false },
          { type: "tool_result", tool_use_id: "t2", content: "bad", is_error: true },
          { type: "text", text: "not a tool result" },
        ],
      },
    });
    // Assert
    expect(h.eventsOfType("tool-result")).toEqual([
      {
        type: "tool-result",
        session_id: "sess-1",
        uuid: "u3",
        tool_use_id: "t1",
        is_error: false,
        content: "ok",
      },
      {
        type: "tool-result",
        session_id: "sess-1",
        uuid: "u3",
        tool_use_id: "t2",
        is_error: true,
        content: "bad",
      },
    ]);
  });

  it("emits nothing for user messages with plain string content", async () => {
    // Arrange
    const h = makeHarness();
    // Act
    await drive(h, {
      type: "user",
      uuid: "u4",
      parent_tool_use_id: null,
      message: { role: "user", content: "typed by a human" },
    });
    // Assert
    expect(h.eventsOfType("tool-result")).toHaveLength(0);
  });

  it("maps a success result including result text and permission denials", async () => {
    // Arrange
    const h = makeHarness();
    // Act
    await drive(h, {
      type: "result",
      uuid: "u5",
      subtype: "success",
      duration_ms: 10,
      duration_api_ms: 8,
      num_turns: 1,
      total_cost_usd: 0.01,
      usage: { input_tokens: 3, output_tokens: 4, cache_read_input_tokens: 5 },
      is_error: false,
      result: "done",
      permission_denials: [{ tool_use_id: "t1", tool_name: "Bash", tool_input: {} }],
    });
    // Assert
    expect(h.eventsOfType("result")).toEqual([
      {
        type: "result",
        session_id: "sess-1",
        uuid: "u5",
        subtype: "success",
        duration_ms: 10,
        duration_api_ms: 8,
        num_turns: 1,
        total_cost_usd: 0.01,
        usage: { input_tokens: 3, output_tokens: 4, cache_read_input_tokens: 5 },
        is_error: false,
        result: "done",
        permission_denials: [{ tool_use_id: "t1", tool_name: "Bash" }],
      },
    ]);
  });

  it("collapses unknown SDK error subtypes to error_during_execution", async () => {
    // Arrange
    const h = makeHarness();
    // Act
    await drive(h, {
      type: "result",
      uuid: "u6",
      subtype: "error_max_budget_usd",
      duration_ms: 1,
      duration_api_ms: 1,
      num_turns: 1,
      total_cost_usd: 0,
      usage: { input_tokens: 0, output_tokens: 0 },
      is_error: true,
    });
    // Assert
    expect(h.eventsOfType("result")[0].subtype).toBe("error_during_execution");
  });

  it("marks the first result after an in-flight interrupt as aborted", async () => {
    // Arrange — a turn must be in flight for an interrupt to abort it.
    const h = makeHarness();
    h.send({ type: "user-message", request_id: "r0", content: "hi" });
    h.send({ type: "interrupt", request_id: "r1" });
    // Act
    await drive(h, {
      type: "result",
      uuid: "u7",
      subtype: "error_during_execution",
      duration_ms: 1,
      duration_api_ms: 1,
      num_turns: 1,
      total_cost_usd: 0,
      usage: { input_tokens: 0, output_tokens: 0 },
      is_error: true,
    });
    // Assert
    expect(h.eventsOfType("result")[0].subtype).toBe("aborted");
  });

  it("does not mark the next turn aborted after an idle interrupt", async () => {
    // Arrange — interrupt with NO turn in flight, then start a turn.
    const h = makeHarness();
    h.send({ type: "interrupt", request_id: "r1" });
    h.send({ type: "user-message", request_id: "r2", content: "hi" });
    // Act
    await drive(h, {
      type: "result",
      uuid: "u7",
      subtype: "success",
      duration_ms: 1,
      duration_api_ms: 1,
      num_turns: 1,
      total_cost_usd: 0,
      usage: { input_tokens: 0, output_tokens: 0 },
      is_error: false,
      result: "ok",
    });
    // Assert — the idle interrupt must not poison the fresh turn.
    expect(h.eventsOfType("result")[0].subtype).toBe("success");
  });

  it("marks a queued second turn's result aborted when interrupted mid-queue", async () => {
    // Arrange — two turns queued; turn A completes, turn B is still
    // outstanding when the interrupt lands. A boolean in-flight flag
    // would misread this as idle (turn A's result cleared it).
    const h = makeHarness();
    h.send({ type: "user-message", request_id: "rA", content: "turn A" });
    h.send({ type: "user-message", request_id: "rB", content: "turn B" });
    h.sdkOut.push({
      type: "result",
      uuid: "uA",
      subtype: "success",
      duration_ms: 1,
      duration_api_ms: 1,
      num_turns: 1,
      total_cost_usd: 0,
      usage: { input_tokens: 0, output_tokens: 0 },
      is_error: false,
      result: "A done",
    });
    await until(() => h.eventsOfType("result").length === 1);
    // Act — interrupt while turn B is outstanding, then B's result.
    h.send({ type: "interrupt", request_id: "r1" });
    h.sdkOut.push({
      type: "result",
      uuid: "uB",
      subtype: "error_during_execution",
      duration_ms: 1,
      duration_api_ms: 1,
      num_turns: 1,
      total_cost_usd: 0,
      usage: { input_tokens: 0, output_tokens: 0 },
      is_error: true,
    });
    h.sdkOut.end();
    await h.pump;
    // Assert — the interrupt aborted the OUTSTANDING queued turn.
    expect(h.eventsOfType("result")[1].subtype).toBe("aborted");
  });

  it("treats an interrupt after a completed turn as idle", async () => {
    // Arrange — first turn completes, so in-flight resets before the interrupt.
    const h = makeHarness();
    const result = (uuid: string): SdkMessageLike => ({
      type: "result",
      uuid,
      subtype: "success",
      duration_ms: 1,
      duration_api_ms: 1,
      num_turns: 1,
      total_cost_usd: 0,
      usage: { input_tokens: 0, output_tokens: 0 },
      is_error: false,
      result: "ok",
    });
    h.send({ type: "user-message", request_id: "r0", content: "hi" });
    h.sdkOut.push(result("u1"));
    await until(() => h.eventsOfType("result").length === 1);
    h.send({ type: "interrupt", request_id: "r1" });
    h.send({ type: "user-message", request_id: "r2", content: "again" });
    // Act
    h.sdkOut.push(result("u2"));
    h.sdkOut.end();
    await h.pump;
    // Assert — the post-turn interrupt was idle, so turn two is untouched.
    expect(h.eventsOfType("result")[1].subtype).toBe("success");
  });

  it("maps system init messages to system events with subtype init", async () => {
    // Arrange
    const h = makeHarness();
    // Act
    await drive(h, { type: "system", subtype: "init", uuid: "u8", model: "m" });
    // Assert
    expect(h.eventsOfType("system")[0]).toMatchObject({
      subtype: "init",
      uuid: "u8",
      data: expect.objectContaining({ model: "m" }),
    });
  });

  it("maps compact_boundary system messages through", async () => {
    // Arrange
    const h = makeHarness();
    // Act
    await drive(h, {
      type: "system",
      subtype: "compact_boundary",
      uuid: "u9",
      compact_metadata: { trigger: "auto", pre_tokens: 111 },
    });
    // Assert
    expect(h.eventsOfType("system")[0]).toMatchObject({
      subtype: "compact_boundary",
      data: expect.objectContaining({
        compact_metadata: { trigger: "auto", pre_tokens: 111 },
      }),
    });
  });

  it("forwards status:compacting system messages so the GUI can show progress", async () => {
    // Arrange
    const h = makeHarness();
    // Act
    await drive(h, {
      type: "system",
      subtype: "status",
      uuid: "u9c",
      status: "compacting",
    });
    // Assert
    expect(h.eventsOfType("system")[0]).toMatchObject({
      subtype: "status",
      data: expect.objectContaining({ status: "compacting" }),
    });
  });

  it("drops status system messages that are not compacting", async () => {
    // Arrange — land the startup `models` event first, so the sampled window
    // holds only what the driven message produced.
    const h = makeHarness();
    await until(() => h.eventsOfType("models").length === 1);
    const before = h.emitted.length;
    // Act
    await drive(h, {
      type: "system",
      subtype: "status",
      uuid: "u9d",
      status: "requesting",
    });
    // Assert — no system event added, only the closed event
    const added = h.emitted.slice(before).map((e) => e.type);
    expect(added).toEqual(["closed"]);
  });

  it("maps tool_progress messages to system events with subtype tool_use_progress", async () => {
    // Arrange
    const h = makeHarness();
    // Act
    await drive(h, {
      type: "tool_progress",
      uuid: "u10",
      tool_use_id: "t1",
      tool_name: "Bash",
      elapsed_time_seconds: 3,
    });
    // Assert
    expect(h.eventsOfType("system")[0]).toMatchObject({
      subtype: "tool_use_progress",
      data: expect.objectContaining({ tool_use_id: "t1" }),
    });
  });

  it("drops SDK message types with no Layer-1 representation", async () => {
    // Arrange — let the startup `models` event land first, so the window
    // sampled below holds only what the driven SDK message produced.
    const h = makeHarness();
    await until(() => h.eventsOfType("models").length === 1);
    const before = h.emitted.length;
    // Act
    await drive(h, { type: "auth_status", uuid: "u11", isAuthenticating: true, output: [] });
    // Assert — only the closed event was added
    const added = h.emitted.slice(before).map((e) => e.type);
    expect(added).toEqual(["closed"]);
  });
});
