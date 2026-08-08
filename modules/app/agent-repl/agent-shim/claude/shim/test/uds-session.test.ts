/**
 * UdsSession entry-wiring integration (stitch task S5).
 *
 * A fake SDK query (records submitted prompts, emits SDK messages on demand)
 * is driven through a real UdsSession over TEMP sockets, with a fake store peer
 * and a fake daemon peer. These lock the S5 wiring contract:
 *   - a SubmitPrompt lands in the SDK input queue (prompt in → SDK);
 *   - a persistent SDK message is WRITTEN to the store, never sent to the
 *     daemon directly; an ephemeral stream_event is sent STRAIGHT to the
 *     daemon, never to the store (SDK out → store-write vs ephemeral routing);
 *   - a merged store Event is forwarded to the daemon (onMerged round-trip);
 *   - a store outage becomes an Event(DegradedState) to the daemon;
 *   - a daemon disconnect ends neither the session nor the turn (reattach);
 *   - canUseTool round-trips a PermissionRequest and resolves on the response;
 *   - an Interrupt cancels the pending permission and forwards to the SDK.
 */
import { afterEach, describe, expect, it, vi } from "vitest";
import net from "node:net";
import { once } from "node:events";
import { create } from "@bufbuild/protobuf";
import { anyPack } from "@bufbuild/protobuf/wkt";
import {
  ClaudeStreamMessageSchema,
  UserMessageSchema,
  type ClaudeStreamMessage,
} from "../../../../proto/gen/ts/agentshim/data/v1/stream_pb.js";
import {
  AssistantLineSchema,
  QueueOperationLineSchema,
  QueueOp,
  TranscriptLineSchema,
  UserLineSchema,
  type TranscriptLine,
} from "../../../../proto/gen/ts/agentshim/data/v1/transcript_pb.js";
import {
  ApiContentBlocksSchema,
  ApiUserMessageSchema,
  ContentBlockSchema,
  TextBlockSchema,
  ToolResultBlockSchema,
  type ApiUserMessage,
} from "../../../../proto/gen/ts/agentshim/data/v1/tools_pb.js";
import { AsyncQueue } from "../src/input-queue.js";
import {
  QueryTerminationCleanupError,
  QueryTerminationPersistenceError,
  ResumeIdentityMismatchError,
  UdsSession,
  isQueryTerminationCleanupError,
  isQueryTerminationPersistenceError,
  sessionRewoundDedupKey,
} from "../src/uds/uds-session.js";
import type { UdsQuery } from "../src/uds/uds-session.js";
import type {
  CanUseToolLike,
  InterruptReceipt,
  PermissionResultLike,
  QueryLike,
  SdkMessageLike,
  SdkUserMessageLike,
} from "../src/session.js";
import type { ModelInfo, PermissionMode, SlashCommand } from "../src/protocol.js";
import type { SubscriptionUsageResponse } from "../src/subscription-usage.js";
import type { Event, ShimHello, ShimReady, StoreWrite, Subscribe } from "../src/uds/proto.js";
import {
  AckSchema,
  DaemonHelloSchema,
  EventClass,
  DiagnosticSourceRuntime,
  EventSchema,
  HeartbeatSchema,
  HealthCheckSchema,
  HealthStatusSchema,
  FilePlaneDiagnosticSchema,
  InterruptSchema,
  NackSchema,
  PermissionDecision,
  InterruptOutcome,
  PermissionRequestSchema,
  PermissionResponseSchema,
  Plane,
  PromptOrigin,
  SessionSource,
  ReplayDoneSchema,
  ReplayEventSchema,
  ReplayRequestSchema,
  ShimHelloSchema,
  ShimReadySchema,
  StoreWriteAckSchema,
  StoreWriteSchema,
  SubmitPromptSchema,
  SubscribeSchema,
} from "../src/uds/proto.js";
import { FramedPeer, acceptShim, tmpSocketPath, until } from "./uds-harness.js";
import { unpackAs } from "../src/uds/framing.js";

const tick = (): Promise<void> => new Promise<void>((r) => setImmediate(r));

/** A scripted SDK query: records prompts pushed at it, emits messages on cue. */
class FakeQuery implements QueryLike {
  readonly prompts: SdkUserMessageLike[] = [];
  readonly canUseTool: CanUseToolLike;
  interruptCalls = 0;
  abortCalls = 0;
  /** The receipt `interrupt()` resolves; undefined models a pre-0.3.205 CLI. */
  interruptReceipt: InterruptReceipt | undefined = undefined;
  private readonly outbox = new AsyncQueue<SdkMessageLike>();
  subscriptionUsageResponse: SubscriptionUsageResponse = {
    subscription_type: null,
    rate_limits_available: false,
    rate_limits: null,
  };
  subscriptionUsageCalls = 0;
  subscriptionUsageImpl: () => Promise<SubscriptionUsageResponse> = () =>
    Promise.resolve(this.subscriptionUsageResponse);

  constructor(prompt: AsyncIterable<SdkUserMessageLike>, canUseTool: CanUseToolLike) {
    this.canUseTool = canUseTool;
    void (async (): Promise<void> => {
      for await (const p of prompt) this.prompts.push(p);
    })();
  }

  /** Push one SDK message onto the stream the UdsSession pump consumes. */
  emit(msg: SdkMessageLike): void {
    this.outbox.push(msg);
  }

  /** End the SDK stream (the pump loop exits, session shuts down). */
  endStream(): void {
    this.outbox.end();
  }

  /** Fail the SDK iterator without making any session shutdown request. */
  failStream(cause: Error): void {
    this.outbox.fail(cause);
  }

  /** The session-owned query cleanup capability. */
  abort(): void {
    this.abortCalls++;
    this.outbox.end();
  }

  [Symbol.asyncIterator](): AsyncIterator<SdkMessageLike> {
    return this.outbox[Symbol.asyncIterator]();
  }
  interrupt(): Promise<InterruptReceipt | undefined> {
    this.interruptCalls++;
    return Promise.resolve(this.interruptReceipt);
  }
  /** Modes the session actually applied, and an optional forced rejection. */
  readonly permissionModes: string[] = [];
  setPermissionModeError: Error | undefined = undefined;
  setPermissionMode(mode: string): Promise<void> {
    if (this.setPermissionModeError) return Promise.reject(this.setPermissionModeError);
    this.permissionModes.push(mode);
    return Promise.resolve();
  }
  setModel(): Promise<void> {
    return Promise.resolve();
  }
  supportedModels(): Promise<ModelInfo[]> {
    return Promise.resolve([]);
  }
  supportedCommands(): Promise<SlashCommand[]> {
    return Promise.resolve([]);
  }
  subscriptionUsage(): Promise<SubscriptionUsageResponse> {
    this.subscriptionUsageCalls++;
    return this.subscriptionUsageImpl();
  }
}

interface FakeStore {
  socketPath: string;
  /** The first accepted connection (the producer conn). */
  peer: () => FramedPeer;
  /** The most recently accepted connection (the newest subscription conn). */
  latest: () => FramedPeer;
  count: () => number;
  close: () => void;
}

function fakeStore(): Promise<FakeStore> {
  const socketPath = tmpSocketPath();
  const accepted: FramedPeer[] = [];
  return new Promise((resolve, reject) => {
    const server = net.createServer((socket) => {
      const peer = new FramedPeer(socket);
      peer.onReceive((frame) => {
        if (unpackAs(frame, SubscribeSchema) === undefined) return false;
        peer.send(HeartbeatSchema, create(HeartbeatSchema, { sentAtMs: 1n }));
        return false;
      });
      accepted.push(peer);
    });
    server.once("error", reject);
    server.listen(socketPath, () =>
      resolve({
        socketPath,
        peer: () => {
          if (!accepted[0]) throw new Error("store: no connection accepted");
          return accepted[0];
        },
        latest: () => {
          const last = accepted[accepted.length - 1];
          if (!last) throw new Error("store: no connection accepted");
          return last;
        },
        count: () => accepted.length,
        close: () => {
          accepted.forEach((p) => p.destroy());
          server.close();
        },
      }),
    );
  });
}

interface Rig {
  session: UdsSession;
  /** The owned SDK pump completion. */
  done: Promise<void>;
  query: FakeQuery;
  queryFactoryCalls: () => number;
  store: FakeStore;
  daemon: FramedPeer;
  hello: ShimHello;
  udsSocketPath: string;
  /** Accepts the shim's RECONNECT after a daemon drop. */
  daemonListener: { next: () => Promise<FramedPeer>; close: () => void };
  /**
   * The shim-asserted readiness Event, already drained off `daemon`.
   *
   * The shim announces readiness inside the bring-up gate, so it is ALWAYS
   * the first Event a gated daemon sees. rig() consumes it so every other
   * test still reads its own event as the next one; the readiness tests
   * assert on this value instead.
   */
  readiness: Event;
  /** The ShimReady that closed the gate, already drained off `daemon`. */
  ready: ShimReady;
  /** The Subscribe the gate opened the standing store tail with. */
  standingSubscribe: Subscribe;
  /**
   * The session's own QueryCreated write, already drained off the store.
   *
   * It is written BEFORE the handshake supplies a from_seq, so the standing
   * subscription cannot yet exist and the store serves this row back during
   * catch-up on every later replay. That makes it the exact record whose
   * ORIGIN must not be confused with its DELIVERY.
   */
  createdLifecycle: StoreWrite;
}

const cleanups: Array<() => void | Promise<void>> = [];
afterEach(async () => {
  for (const c of cleanups.splice(0)) await c();
});

async function shutdownFixture(session: UdsSession, query: FakeQuery, done: Promise<void>): Promise<void> {
  try {
    await session.shutdown("test-cleanup");
  } catch (cause) {
    // Tests that deliberately sever the store cannot obtain a cleanup-only
    // termination receipt. Production surfaces this exact error fatally; the
    // dedicated violation test asserts that contract.
    if (!(cause instanceof QueryTerminationPersistenceError)) throw cause;
  }
  query.endStream();
  await done.catch(() => {});
}

type CapturedLogRecord = {
  level?: string;
  operation?: string;
  message?: string;
  context?: Record<string, unknown>;
  agent_repl_session_id?: string;
  claude_session_id?: string;
  request_id?: string;
};

/**
 * Capture the canonical JSON records written by the shim logging API.
 *
 * The public logger writes straight to stderr, so a spy on the stream is what
 * "did it log?" means here. Restored via the shared cleanups so a failing
 * assertion cannot leave the spy installed for the next test.
 */
function captureLog(): {
  find: (needle: string) => string;
  record: (needle: string) => CapturedLogRecord;
  count: (needle: string) => number;
} {
  const lines: string[] = [];
  const spy = vi.spyOn(process.stderr, "write").mockImplementation(((chunk: unknown): boolean => {
    lines.push(String(chunk));
    return true;
  }) as typeof process.stderr.write);
  cleanups.push(() => spy.mockRestore());
  const find = (needle: string): string => {
    const line = lines.find((candidate) => candidate.includes(needle));
    // Loud on a miss: an assertion on `undefined` would report the wrong
    // failure and hide what WAS logged.
    if (line === undefined) throw new Error(`no shim log record contains ${JSON.stringify(needle)}; captured: ${lines.join("")}`);
    return line;
  };
  return {
    find,
    record: (needle) => JSON.parse(find(needle)) as CapturedLogRecord,
    count: (needle) => lines.filter((l) => l.includes(needle)).length,
  };
}

/** An ApiUserMessage whose content is a single text block. */
function textMessage(text: string): ApiUserMessage {
  return create(ApiUserMessageSchema, {
    content: {
      case: "contentBlocks",
      value: create(ApiContentBlocksSchema, {
        blocks: [create(ContentBlockSchema, { block: { case: "text", value: create(TextBlockSchema, { text }) } })],
      }),
    },
  });
}

/** A file-plane transcript line for a user turn carrying `message`. */
function userLine(message: ApiUserMessage): TranscriptLine {
  return create(TranscriptLineSchema, { line: { case: "user", value: create(UserLineSchema, { message }) } });
}

/** A merged store Event wrapping a data.v1.TranscriptLine as its vendor payload. */
/**
 * A merged store Event wrapping a data.v1.TranscriptLine (file plane).
 *
 * `storeKey` names the seq space the event is filed under, and it matters: the
 * session positions file-plane evidence against its own QueryCreated store
 * coordinate, so a rig that resumed under `vendor-uuid` must send merged events
 * under that key rather than under the shim's `--session-id`.
 */
function transcriptEvent(seq: bigint, line: TranscriptLine, storeKey = "sess-1"): Event {
  return create(EventSchema, {
    sessionId: storeKey,
    seq,
    payload: { case: "vendor", value: anyPack(TranscriptLineSchema, line) },
  });
}

/** A merged store Event wrapping a data.v1.ClaudeStreamMessage (stream plane). */
function streamEvent(seq: bigint, msg: ClaudeStreamMessage): Event {
  return create(EventSchema, {
    sessionId: "sess-1",
    seq,
    payload: { case: "vendor", value: anyPack(ClaudeStreamMessageSchema, msg) },
  });
}

/**
 * Stand up store + session and run the WHOLE bring-up gate: ShimHello,
 * DaemonHello carrying `fromSeq`, the standing store subscription it drives,
 * and the ShimReady that closes it. There is no un-subscribed variant any
 * more — a gated session is subscribed by the time it is ready, which is the
 * property the gate exists to establish.
 */
async function rig(
  opts: {
    fromSeq?: bigint;
    sessionSource?: SessionSource;
    storeSessionId?: string;
    replayIdleMs?: number;
    queryInstanceId?: string;
    /** The `--permission-mode` argv the session is constructed with. */
    permissionMode?: PermissionMode;
    /** DaemonHello.permission_mode; omitted models a daemon predating it. */
    handshakeMode?: string;
    /** Keep the ordered start batch unacked so a test can assert its barrier. */
    holdTurnStartAck?: boolean;
  } = {},
): Promise<Rig> {
  const store = await fakeStore();
  cleanups.push(() => store.close());

  let query!: FakeQuery;
  let queryFactoryCalls = 0;
  const createQuery = (
    prompt: AsyncIterable<SdkUserMessageLike>,
    canUseTool: CanUseToolLike,
  ): UdsQuery => {
    queryFactoryCalls++;
    const live = new FakeQuery(prompt, canUseTool);
    query = live;
    return { query: live, subscriptionUsage: () => live.subscriptionUsage(), abort: () => live.abort() };
  };

  // Be the daemon: the shim dials us. Listening BEFORE start() mirrors
  // production, where the daemon is up long before it spawns a shim.
  const udsSocketPath = tmpSocketPath();
  const daemonListener = acceptShim(udsSocketPath);
  cleanups.push(() => daemonListener.close());

  const session = new UdsSession({
    sessionId: "sess-1",
    shimVersion: "9.9",
    protocolVersion: "1",
    udsSocketPath,
    storeSocketPath: store.socketPath,
    sessionSource: opts.sessionSource ?? SessionSource.FRESH,
    ...(opts.queryInstanceId !== undefined ? { queryInstanceId: opts.queryInstanceId } : {}),
    ...(opts.permissionMode !== undefined ? { permissionMode: opts.permissionMode } : {}),
    ...(opts.storeSessionId !== undefined ? { storeSessionId: opts.storeSessionId } : {}),
    createQuery,
    heartbeatIntervalMs: 0,
    // A dropped store link is reported only once it fails to come back within
    // its relink retry budget (store-client.ts RELINK_REPORT_AFTER_MS). Every
    // test below that provokes an outage kills the store FOR GOOD, so it wants
    // that report without waiting out the real budget.
    storeRelinkReportAfterMs: 0,
    ...(opts.replayIdleMs !== undefined ? { replayIdleMs: opts.replayIdleMs } : {}),
    newRequestId: (() => {
      let n = 0;
      return () => `req-${++n}`;
    })(),
  });
  let outstandingFixtureWrites = 0;
  let fixtureTearingDown = false;
  // start() resolves only when the SDK stream ends; kick it off and clean up.
  const done = session.start();
  let runSettled = false;
  void done.then(
    () => { runSettled = true; },
    () => { runSettled = true; },
  );
  cleanups.unshift(async () => {
    fixtureTearingDown = true;
    while (outstandingFixtureWrites > 0) {
      outstandingFixtureWrites--;
      store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
        accepted: 1n,
        lastSeq: 1n,
      }));
    }
    await tick();
    if (!runSettled) {
      await shutdownFixture(session, query, done);
    }
  });

  await until(() => store.count() >= 1);
  const created = await store.peer().next(StoreWriteSchema);
  expect(created.batch!.events).toHaveLength(1);
  expect(created.batch!.events[0]!.payload.case).toBe("queryLifecycle");
  store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 1n, lastSeq: 1n }));
  store.peer().onReceive((frame) => {
    const write = unpackAs(frame, StoreWriteSchema);
    if (write === undefined) return false;
    const internalTelemetry = write.batch?.events.every((event) =>
      event.payload.case === "accountUsageObservation" || event.payload.case === "queryLifecycle",
    ) ?? false;
    const orderedTurnStartBoundary = write.batch?.events.length === 2
      && write.batch.events[0]?.payload.case === "turnStarted"
      && write.batch.events[1]?.payload.case === "accountUsageObservation";
    if (orderedTurnStartBoundary) {
      if (opts.holdTurnStartAck !== true) {
        queueMicrotask(() => store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
          accepted: 2n,
          lastSeq: 1n,
        })));
      }
      return false;
    }
    if (!internalTelemetry) {
      if (fixtureTearingDown) {
        queueMicrotask(() => store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
          accepted: BigInt(write.batch?.events.length ?? 0),
          lastSeq: 1n,
        })));
        return true;
      }
      outstandingFixtureWrites++;
      return false;
    }
    queueMicrotask(() => store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: BigInt(write.batch?.events.length ?? 0),
      lastSeq: 1n,
    })));
    return true;
  });
  const daemon = await daemonListener.next();
  const hello = await daemon.next(ShimHelloSchema);
  daemon.send(DaemonHelloSchema, create(DaemonHelloSchema, {
    daemonVersion: "d1", protocolVersion: "1", fromSeq: opts.fromSeq ?? 0n,
    ...(opts.handshakeMode !== undefined ? { permissionMode: opts.handshakeMode } : {}),
  }));
  // The gate opens the standing subscription on its OWN store connection
  // (single-role store) before it acks.
  await until(() => store.count() >= 2);
  const standingSubscribe = await store.latest().next(SubscribeSchema);
  const readiness = await daemon.next(EventSchema);
  const ready = await daemon.next(ShimReadySchema);
  return {
    session,
    done,
    query,
    queryFactoryCalls: () => queryFactoryCalls,
    store,
    daemon,
    hello,
    udsSocketPath,
    daemonListener,
    readiness,
    ready,
    standingSubscribe,
    createdLifecycle: created,
  };
}

/** Acknowledge the mandatory initial QueryCreated receipt in custom session rigs. */
async function acknowledgeInitialQueryLifecycle(store: FakeStore): Promise<void> {
  await until(() => store.count() >= 1);
  const write = await store.peer().next(StoreWriteSchema);
  expect(write.batch!.events).toHaveLength(1);
  expect(write.batch!.events[0]!.payload.case).toBe("queryLifecycle");
  store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 1n, lastSeq: 1n }));
}

/** Drive one fully observed usage turn so assertions only describe the outcome under test. */
async function completedUsageTurn(
  requestId: string,
  responses: SubscriptionUsageResponse[],
): Promise<ReturnType<typeof captureLog>> {
  const { query, daemon } = await rig({ storeSessionId: "vendor-uuid" });
  const log = captureLog();
  query.subscriptionUsageImpl = async () => {
    const response = responses.shift();
    if (response === undefined) throw new Error("usage-turn fixture omitted a boundary response");
    return response;
  };

  daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId, text: "go", promptOrigin: PromptOrigin.USER_SENT }));
  await daemon.next(AckSchema);
  query.emit({ type: "result", uuid: `r-${requestId}`, session_id: "vendor-uuid", subtype: "success" } as unknown as SdkMessageLike);
  await until(() => query.subscriptionUsageCalls === 2);
  await tick();
  return log;
}

describe("UdsSession control: prompt in → SDK", () => {
  it("announces QueryCreated's exact durable sequence in the handshake", async () => {
    const { hello } = await rig({ storeSessionId: "vendor-session", sessionSource: SessionSource.RESUME });

    expect(hello.vendorSessionId).toBe("vendor-session");
    expect(hello.queryCreatedSeq).toBe(1n);
  });

  it("pushes a SubmitPrompt into the SDK input queue and Acks", async () => {
    // Arrange
    const { query, daemon, session } = await rig();
    // Act
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "hello there", promptOrigin: PromptOrigin.USER_SENT }));
    const ack = await daemon.next(AckSchema);
    await until(() => query.prompts.length === 1);
    // Assert
    expect(ack.requestId).toBe("p1");
    const content = query.prompts[0]!.message.content;
    expect(content).toEqual([{ type: "text", text: "hello there" }]);
    expect(session.turnCount()).toBe(1);
  });

  it("rejects a whitespace-only request id before creating any durable or SDK turn state", async () => {
    // Arrange
    const { query, daemon, session, store } = await rig();
    // Act
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: " \t", text: "must not run" }));
    const nack = await daemon.next(NackSchema);
    await tick();
    // Assert
    expect(nack.reason).toBe("SubmitPrompt requires a non-empty request_id");
    expect(query.subscriptionUsageCalls).toBe(0);
    expect(query.prompts).toEqual([]);
    expect(session.turnCount()).toBe(0);
    expect(store.peer().count(StoreWriteSchema)).toBe(0);
  });

  it("captures five-hour utilization before admitting the prompt to the SDK", async () => {
    // Arrange
    const { query, daemon } = await rig();
    let release!: (response: SubscriptionUsageResponse) => void;
    query.subscriptionUsageImpl = () => new Promise((resolve) => { release = resolve; });

    // Act: leave the start observation pending.
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p-quota", text: "measure me", promptOrigin: PromptOrigin.USER_SENT }));
    await until(() => query.subscriptionUsageCalls === 1);
    // Assert: neither admission nor its receipt can overtake the observation.
    expect(query.prompts).toEqual([]);

    release({
      subscription_type: "max",
      rate_limits_available: true,
      rate_limits: { five_hour: { utilization: 12.5, resets_at: "2026-08-03T22:00:00Z" } },
    });
    expect((await daemon.next(AckSchema)).requestId).toBe("p-quota");
    await until(() => query.prompts.length === 1);
  });

  it("logs raw turn-boundary utilization and a same-window delta", async () => {
    // Arrange
    const { query, daemon } = await rig({ storeSessionId: "vendor-uuid" });
    const log = captureLog();
    const responses: SubscriptionUsageResponse[] = [
      {
        subscription_type: "max",
        rate_limits_available: true,
        rate_limits: { five_hour: { utilization: 20.25, resets_at: "2026-08-03T22:00:00Z" } },
      },
      {
        subscription_type: "max",
        rate_limits_available: true,
        rate_limits: { five_hour: { utilization: 21.75, resets_at: "2026-08-03T22:00:00Z" } },
      },
    ];
    query.subscriptionUsageImpl = async () => {
      const response = responses.shift();
      if (response === undefined) throw new Error("unexpected usage sample");
      return response;
    };

    // Act
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p-delta", text: "go", promptOrigin: PromptOrigin.USER_SENT }));
    await daemon.next(AckSchema);
    query.emit({ type: "result", uuid: "r1", session_id: "vendor-uuid", subtype: "success" } as unknown as SdkMessageLike);
    await until(() => query.subscriptionUsageCalls === 2);
    await tick();

    // Assert
    expect(log.record("captured five-hour utilization at turn start").context).toMatchObject({
      phase: "turn_start",
      turn_id: "p-delta",
      measurement_available: true,
      five_hour_utilization: 20.25,
      five_hour_resets_at: "2026-08-03T22:00:00Z",
      five_hour_resets_at_ms: Date.parse("2026-08-03T22:00:00Z"),
      five_hour_reset_contract_version: "anthropic-five-hour-cadence-v1",
      subscription_type: "max",
      rate_limits_available: true,
    });
    expect(log.record("captured five-hour utilization at turn end").context).toMatchObject({
      phase: "turn_end",
      turn_id: "p-delta",
      start_five_hour_utilization: 20.25,
      end_five_hour_utilization: 21.75,
      start_five_hour_resets_at: "2026-08-03T22:00:00Z",
      end_five_hour_resets_at: "2026-08-03T22:00:00Z",
      start_five_hour_resets_at_ms: Date.parse("2026-08-03T22:00:00Z"),
      end_five_hour_resets_at_ms: Date.parse("2026-08-03T22:00:00Z"),
      five_hour_reset_raw_delta_ms: 0,
      five_hour_reset_cycle_displacement: 0,
      five_hour_reset_residual_jitter_ms: 0,
      five_hour_reset_comparison_outcome: "same_window",
      five_hour_reset_contract_version: "anthropic-five-hour-cadence-v1",
      same_five_hour_window: true,
      five_hour_utilization_delta_available: true,
      five_hour_utilization_delta: 1.5,
    });
  });

  it("does not compute a utilization delta across five-hour windows", async () => {
    const log = await completedUsageTurn("p-reset", [
      {
        subscription_type: "max",
        rate_limits_available: true,
        rate_limits: { five_hour: { utilization: 99, resets_at: "2026-08-03T22:00:00.557120Z" } },
      },
      {
        subscription_type: "max",
        rate_limits_available: true,
        rate_limits: { five_hour: { utilization: 1, resets_at: "2026-08-04T03:00:00.939811Z" } },
      },
    ]);

    // Assert
    expect(log.record("captured five-hour utilization at turn end").context).toMatchObject({
      same_five_hour_window: false,
      five_hour_utilization_delta_available: false,
      five_hour_utilization_delta: null,
      five_hour_reset_cycle_displacement: 1,
      five_hour_reset_residual_jitter_ms: 382,
      five_hour_reset_comparison_outcome: "different_window",
      delta_unavailable_reason: "five_hour_window_changed_or_unknown",
    });
  });

  it.each([
    ["first observed straddling-minute pair", "2026-08-03T19:19:59.908698Z", "2026-08-03T19:20:00.502647Z"],
    ["second observed fractional pair", "2026-08-03T19:20:00.557120Z", "2026-08-03T19:20:00.939811Z"],
  ])("computes a delta for %s", async (_case, startReset, endReset) => {
    const log = await completedUsageTurn("p-jitter", [
      { subscription_type: "max", rate_limits_available: true, rate_limits: { five_hour: { utilization: 17, resets_at: startReset } } },
      { subscription_type: "max", rate_limits_available: true, rate_limits: { five_hour: { utilization: 18, resets_at: endReset } } },
    ]);

    expect(log.record("captured five-hour utilization at turn end").context).toMatchObject({
      same_five_hour_window: true,
      five_hour_utilization_delta_available: true,
      five_hour_utilization_delta: 1,
      five_hour_reset_cycle_displacement: 0,
      five_hour_reset_comparison_outcome: "same_window",
    });
  });

  it("keeps an unavailable end observation distinct from a window crossing", async () => {
    const log = await completedUsageTurn("p-missing-end", [
      { subscription_type: "max", rate_limits_available: true, rate_limits: { five_hour: { utilization: 17, resets_at: "2026-08-03T19:20:00Z" } } },
      { subscription_type: "max", rate_limits_available: true, rate_limits: { five_hour: { utilization: 18, resets_at: null } } },
    ]);

    expect(log.record("captured five-hour utilization at turn end").context).toMatchObject({
      measurement_available: false,
      measurement_unavailable_reason: "five_hour_window_unavailable",
      five_hour_reset_comparison_outcome: "unavailable",
      same_five_hour_window: false,
      five_hour_utilization_delta_available: false,
      delta_unavailable_reason: "turn_end_measurement_unavailable",
    });
  });

  it("logs a malformed reset timestamp as a failed start observation with its cause", async () => {
    const { query, daemon } = await rig();
    const log = captureLog();
    query.subscriptionUsageImpl = async () => ({
      subscription_type: "max",
      rate_limits_available: true,
      rate_limits: { five_hour: { utilization: 17, resets_at: "not-a-timestamp" } },
    });

    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p-malformed-reset", text: "go", promptOrigin: PromptOrigin.USER_SENT }));
    await daemon.next(AckSchema);
    await until(() => query.prompts.length === 1);

    const record = log.record("failed to capture five-hour utilization at turn start");
    expect(record.level).toBe("error");
    expect(record.context).toMatchObject({
      phase: "turn_start",
      turn_id: "p-malformed-reset",
      measurement_available: false,
      measurement_unavailable_reason: "sample_failed",
      five_hour_resets_at: null,
      five_hour_resets_at_ms: null,
      five_hour_reset_contract_version: "anthropic-five-hour-cadence-v1",
      cause: expect.objectContaining({ message: expect.stringContaining("valid ISO 8601") }),
    });
  });

  it("logs a failed start sample and admits the prompt without silent telemetry fallback", async () => {
    // Arrange
    const { query, daemon } = await rig();
    const log = captureLog();
    query.subscriptionUsageImpl = () => Promise.reject(new Error("usage endpoint unavailable"));

    // Act
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p-failed-sample", text: "go", promptOrigin: PromptOrigin.USER_SENT }));
    expect((await daemon.next(AckSchema)).requestId).toBe("p-failed-sample");
    await until(() => query.prompts.length === 1);

    // Assert
    const record = log.record("failed to capture five-hour utilization at turn start");
    expect(record.context).toMatchObject({
      phase: "turn_start",
      measurement_available: false,
      five_hour_utilization: null,
      five_hour_resets_at: null,
      rate_limits_available: false,
      measurement_unavailable_reason: "sample_failed",
    });
  });
});

describe("UdsSession lifecycle: shim-authoritative TurnStarted", () => {
  it("writes a TurnStarted to the store when it accepts a SubmitPrompt", async () => {
    // Arrange: a resumed session already knows the vendor session id.
    const { store, daemon } = await rig({ storeSessionId: "vendor-uuid" });
    // Act
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go", promptOrigin: PromptOrigin.USER_SENT }));
    const sw = await store.peer().next(StoreWriteSchema);
    // Assert
    const payload = sw.batch!.events[0]!.payload;
    expect(payload.case).toBe("turnStarted");
    if (payload.case !== "turnStarted") throw new Error("case");
    expect(payload.value.promptOrigin).toBe(PromptOrigin.USER_SENT);
  });

  it("bounds the TurnStarted preview to the prompt's first line", async () => {
    // Arrange
    const { store, daemon } = await rig({ storeSessionId: "vendor-uuid" });
    // Act
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "line one\nline two", promptOrigin: PromptOrigin.USER_SENT }));
    const sw = await store.peer().next(StoreWriteSchema);
    // Assert
    const payload = sw.batch!.events[0]!.payload;
    if (payload.case !== "turnStarted") throw new Error("case");
    expect(payload.value.promptPreview).toBe("line one");
  });

  it("correlates the TurnStarted to the SubmitPrompt's request id", async () => {
    // Arrange
    const { store, daemon } = await rig({ storeSessionId: "vendor-uuid" });
    // Act
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go", promptOrigin: PromptOrigin.USER_SENT }));
    const sw = await store.peer().next(StoreWriteSchema);
    // Assert
    const event = sw.batch!.events[0]!;
    expect(event.requestId).toBe("p1");
    if (event.payload.case !== "turnStarted") throw new Error("case");
    expect(event.payload.value.turnId).toBe("p1");
  });

  it("correlates the SDK result to the accepted turn it closes", async () => {
    // Arrange: consume the start write for p1.
    const { query, store, daemon } = await rig({ storeSessionId: "vendor-uuid" });
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go", promptOrigin: PromptOrigin.USER_SENT }));
    const startBatch = await store.peer().next(StoreWriteSchema);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: BigInt(startBatch.batch!.events.length),
      lastSeq: 8n,
    }));
    await daemon.next(AckSchema);
    // Act.
    query.emit({
      type: "result",
      uuid: "r1",
      session_id: "vendor-uuid",
      subtype: "success",
    } as unknown as SdkMessageLike);
    const sw = await store.peer().next(StoreWriteSchema);
    // Assert: all response and usage evidence precedes the terminal lifecycle
    // fact, so reducer resolution cannot overtake accounting.
    expect(sw.batch!.events.map((event) => event.payload.case)).toEqual([
      "vendor",
      "accountUsageObservation",
      "turnEnded",
    ]);
    const ended = sw.batch!.events.find((event) => event.payload.case === "turnEnded");
    expect(ended?.requestId).toBe("p1");
    if (ended?.payload.case !== "turnEnded") throw new Error("case");
    expect(ended.payload.value.turnId).toBe("p1");
  });

  it("correlates an assistant response to the active accepted root turn", async () => {
    const { query, store, daemon, session } = await rig({ storeSessionId: "vendor-uuid" });
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go", promptOrigin: PromptOrigin.USER_SENT }));
    await store.peer().next(StoreWriteSchema);
    await daemon.next(AckSchema);
    expect(session.turnCount()).toBe(1);

    query.emit({
      type: "assistant",
      uuid: "u1",
      session_id: "vendor-uuid",
      request_id: "api-request-9",
      message: {
        id: "m1",
        model: "claude",
        content: [{ type: "text", text: "hi" }],
        stop_reason: "end_turn",
        usage: {
          input_tokens: 10,
          output_tokens: 2,
          cache_read_input_tokens: 0,
          cache_creation_input_tokens: 0,
        },
      },
    } as unknown as SdkMessageLike);
    const write = await store.peer().next(StoreWriteSchema);

    expect(write.batch!.events).toHaveLength(1);
    expect(write.batch!.events[0]!.requestId).toBe("p1");
    expect(session.turnCount()).toBe(1);
  });

  it("keeps one root turn live through a result and late background-task usage", async () => {
    // Arrange: one accepted prompt launches one SDK task.
    const { query, store, daemon, session, queryFactoryCalls } = await rig({ storeSessionId: "vendor-uuid" });
    const log = captureLog();
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, {
      requestId: "p1",
      text: "fan out",
      promptOrigin: PromptOrigin.USER_SENT,
    }));
    const start = await store.peer().next(StoreWriteSchema);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: BigInt(start.batch!.events.length),
      lastSeq: 8n,
    }));
    await daemon.next(AckSchema);

    query.emit({
      type: "system",
      subtype: "task_started",
      uuid: "task-start-1",
      session_id: "vendor-uuid",
      task_id: "agent-python",
      task_type: "local_agent",
      tool_use_id: "tool-agent-python",
      description: "Python hello world",
    } as unknown as SdkMessageLike);
    const taskStart = await store.peer().next(StoreWriteSchema);
    expect(taskStart.batch!.events.map((event) => event.payload.case)).toEqual(["vendor", "taskStarted"]);
    expect(taskStart.batch!.events.every((event) => event.requestId === "p1")).toBe(true);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: BigInt(taskStart.batch!.events.length),
      lastSeq: 10n,
    }));
    query.emit({
      type: "system",
      subtype: "task_started",
      uuid: "task-start-2",
      session_id: "vendor-uuid",
      task_id: "agent-go",
      task_type: "local_agent",
      tool_use_id: "tool-agent-go",
      description: "Go hello world",
    } as unknown as SdkMessageLike);
    const secondTaskStart = await store.peer().next(StoreWriteSchema);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: BigInt(secondTaskStart.batch!.events.length),
      lastSeq: 12n,
    }));

    // Act: the parent result arrives while the SDK task is live. It is stored,
    // but it cannot terminate the accepted root turn.
    query.emit({
      type: "result",
      uuid: "parent-result",
      session_id: "vendor-uuid",
      subtype: "success",
      duration_ms: 100,
    } as unknown as SdkMessageLike);
    const parentResult = await store.peer().next(StoreWriteSchema);
    expect(parentResult.batch!.events.map((event) => event.payload.case)).toEqual(["vendor"]);
    expect(parentResult.batch!.events[0]!.requestId).toBe("p1");
    expect(session.turnCount()).toBe(1);
    expect(query.subscriptionUsageCalls).toBe(1);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: 1n,
      lastSeq: 13n,
    }));

    query.emit({
      type: "assistant",
      uuid: "agent-response",
      session_id: "vendor-uuid",
      request_id: "raw-api-request",
      parent_tool_use_id: "tool-agent-python",
      message: {
        id: "agent-message",
        model: "claude-sonnet",
        content: [{ type: "text", text: "implemented" }],
        stop_reason: "end_turn",
        usage: {
          input_tokens: 2,
          output_tokens: 60,
          cache_read_input_tokens: 26004,
          cache_creation_input_tokens: 192,
        },
      },
    } as unknown as SdkMessageLike);
    const lateAssistant = await store.peer().next(StoreWriteSchema);
    expect(lateAssistant.batch!.events).toHaveLength(1);
    expect(lateAssistant.batch!.events[0]!.requestId).toBe("p1");
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: 1n,
      lastSeq: 14n,
    }));

    query.emit({
      type: "system",
      subtype: "task_notification",
      uuid: "task-end-1",
      session_id: "vendor-uuid",
      task_id: "agent-python",
      tool_use_id: "tool-agent-python",
      status: "completed",
      output_file: "/tmp/agent-python.output",
      summary: "done",
    } as unknown as SdkMessageLike);
    const firstTaskEnd = await store.peer().next(StoreWriteSchema);
    expect(firstTaskEnd.batch!.events.map((event) => event.payload.case)).toEqual(["vendor", "taskEnded"]);
    expect(firstTaskEnd.batch!.events.every((event) => event.requestId === "p1")).toBe(true);
    expect(session.turnCount()).toBe(1);
    expect(query.subscriptionUsageCalls).toBe(1);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: BigInt(firstTaskEnd.batch!.events.length),
      lastSeq: 16n,
    }));

    // Claude emits both task_updated and task_notification as terminal facts
    // for one background agent. Preserve both raw vendor messages, but expose
    // exactly one lifecycle end and keep the root turn live for the other task.
    query.emit({
      type: "system",
      subtype: "task_updated",
      uuid: "task-end-1-duplicate",
      session_id: "vendor-uuid",
      task_id: "agent-python",
      patch: { status: "completed" },
    } as unknown as SdkMessageLike);
    const duplicateTaskEnd = await store.peer().next(StoreWriteSchema);
    expect(duplicateTaskEnd.batch!.events.map((event) => event.payload.case)).toEqual(["vendor"]);
    expect(duplicateTaskEnd.batch!.events[0]!.requestId).toBe("p1");
    expect(session.turnCount()).toBe(1);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: 1n,
      lastSeq: 17n,
    }));

    const queuedNotification = "<task-notification>agent-go</task-notification>";
    store.latest().send(EventSchema, transcriptEvent(18n, create(TranscriptLineSchema, {
      line: {
        case: "queueOperation",
        value: create(QueueOperationLineSchema, {
          operation: QueueOp.ENQUEUE,
          content: queuedNotification,
        }),
      },
    }), "vendor-uuid"));
    await daemon.next(EventSchema);

    query.emit({
      type: "result",
      uuid: "first-task-notification-result",
      session_id: "vendor-uuid",
      subtype: "success",
      duration_ms: 120,
      origin: { kind: "task_notification" },
    } as unknown as SdkMessageLike);
    const firstTaskNotificationResult = await store.peer().next(StoreWriteSchema);
    expect(firstTaskNotificationResult.batch!.events.map((event) => event.payload.case)).toEqual(["vendor"]);
    expect(firstTaskNotificationResult.batch!.events[0]!.requestId).toBe("p1");
    expect(session.turnCount()).toBe(1);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: 1n,
      lastSeq: 19n,
    }));

    query.emit({
      type: "system",
      subtype: "task_notification",
      uuid: "task-end-2",
      session_id: "vendor-uuid",
      task_id: "agent-go",
      tool_use_id: "tool-agent-go",
      status: "completed",
      output_file: "/tmp/agent-go.output",
      summary: "done",
    } as unknown as SdkMessageLike);
    const finalTaskEnd = await store.peer().next(StoreWriteSchema);

    // The last task fact does not end the root turn. Claude emits another
    // result after the task notification has driven its final internal cycle.
    expect(finalTaskEnd.batch!.events.map((event) => event.payload.case)).toEqual(["vendor", "taskEnded"]);
    expect(finalTaskEnd.batch!.events.every((event) => event.requestId === "p1")).toBe(true);
    expect(session.turnCount()).toBe(1);
    expect(query.subscriptionUsageCalls).toBe(1);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: BigInt(finalTaskEnd.batch!.events.length),
      lastSeq: 20n,
    }));

    query.emit({
      type: "result",
      uuid: "queued-task-notification-result",
      session_id: "vendor-uuid",
      subtype: "success",
      duration_ms: 130,
      origin: { kind: "task_notification" },
    } as unknown as SdkMessageLike);
    const queuedTaskNotificationResult = await store.peer().next(StoreWriteSchema);
    expect(queuedTaskNotificationResult.batch!.events.map((event) => event.payload.case)).toEqual(["vendor"]);
    expect(session.turnCount()).toBe(1);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: 1n,
      lastSeq: 21n,
    }));

    store.latest().send(EventSchema, transcriptEvent(22n, create(TranscriptLineSchema, {
      line: {
        case: "queueOperation",
        value: create(QueueOperationLineSchema, {
          operation: QueueOp.REMOVE,
          content: queuedNotification,
        }),
      },
    }), "vendor-uuid"));
    await daemon.next(EventSchema);

    query.emit({
      type: "result",
      uuid: "final-parent-result",
      session_id: "vendor-uuid",
      subtype: "success",
      duration_ms: 140,
      origin: { kind: "task_notification" },
    } as unknown as SdkMessageLike);
    const terminal = await store.peer().next(StoreWriteSchema);
    expect(terminal.batch!.events.map((event) => event.payload.case)).toEqual([
      "vendor",
      "accountUsageObservation",
      "turnEnded",
    ]);
    expect(terminal.batch!.events.every((event) => event.requestId === "p1")).toBe(true);
    expect(query.subscriptionUsageCalls).toBe(2);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: BigInt(terminal.batch!.events.length),
      lastSeq: 25n,
    }));
    await until(() => session.turnCount() === 0);
    expect(queryFactoryCalls()).toBe(1);
    expect(query.abortCalls).toBe(0);
    expect(log.record("SDK result retained while background-task result cycles remain outstanding")).toMatchObject({
      request_id: "p1",
      context: {
        live_sdk_task_count: 2,
        live_sdk_task_ids: ["agent-go", "agent-python"],
        sdk_task_count: 2,
        task_notification_result: false,
        pending_task_notification_count: 0,
        decision: "retain_turn_for_sdk_task_cycles",
      },
    });
    expect(log.record("SDK result correlated to accepted turn")).toMatchObject({
      request_id: "p1",
      context: {
        turn_id: "p1",
        decision: "turn_ended",
      },
    });
    expect(log.record("SDK repeated a stored terminal task fact; retaining vendor evidence without duplicating lifecycle")).toMatchObject({
      request_id: "p1",
      context: {
        duplicate_terminal_sdk_task_ids: ["agent-python"],
        live_sdk_task_ids: ["agent-go"],
        decision: "retain_vendor_message_without_duplicate_task_end",
      },
    });
  });

  it("ignores a task-notification queue transition that predates its own QueryCreated", async () => {
    // Arrange: a revived conversation replays its whole transcript, so a
    // `<task-notification>` enqueue from a retired process epoch arrives on the
    // merged feed BELOW this query's QueryCreated coordinate (seq 1).
    const { query, store, daemon, session } = await rig({ storeSessionId: "vendor-uuid" });
    const log = captureLog();
    store.latest().send(EventSchema, transcriptEvent(1n, create(TranscriptLineSchema, {
      line: {
        case: "queueOperation",
        value: create(QueueOperationLineSchema, {
          operation: QueueOp.ENQUEUE,
          content: "<task-notification>stale-epoch</task-notification>",
        }),
      },
    }), "vendor-uuid"));
    await daemon.next(EventSchema);
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, {
      requestId: "p1",
      text: "/compact",
      promptOrigin: PromptOrigin.USER_SENT,
    }));
    const start = await store.peer().next(StoreWriteSchema);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: BigInt(start.batch!.events.length),
      lastSeq: 8n,
    }));
    await daemon.next(AckSchema);

    // Act
    query.emit({
      type: "result",
      uuid: "compaction-result",
      session_id: "vendor-uuid",
      subtype: "success",
      duration_ms: 100,
    } as unknown as SdkMessageLike);

    // Assert: the pre-epoch entry retained nothing, so the result closes the turn.
    const terminal = await store.peer().next(StoreWriteSchema);
    expect(terminal.batch!.events.map((event) => event.payload.case)).toEqual([
      "vendor",
      "accountUsageObservation",
      "turnEnded",
    ]);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: BigInt(terminal.batch!.events.length),
      lastSeq: 12n,
    }));
    await until(() => session.turnCount() === 0);
  });

  it("releases a turn whose only retention is task notifications no SDK task of this query backs", async () => {
    // Arrange: an in-epoch enqueue with no task lifecycle behind it at all —
    // nothing this query instance ran can ever drive its result cycle.
    const { query, store, daemon, session } = await rig({ storeSessionId: "vendor-uuid" });
    const log = captureLog();
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, {
      requestId: "p1",
      text: "/compact",
      promptOrigin: PromptOrigin.USER_SENT,
    }));
    const start = await store.peer().next(StoreWriteSchema);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: BigInt(start.batch!.events.length),
      lastSeq: 8n,
    }));
    await daemon.next(AckSchema);
    store.latest().send(EventSchema, transcriptEvent(9n, create(TranscriptLineSchema, {
      line: {
        case: "queueOperation",
        value: create(QueueOperationLineSchema, {
          operation: QueueOp.ENQUEUE,
          content: "<task-notification>unbacked</task-notification>",
        }),
      },
    }), "vendor-uuid"));
    await daemon.next(EventSchema);

    // Act
    query.emit({
      type: "result",
      uuid: "compaction-result",
      session_id: "vendor-uuid",
      subtype: "success",
      duration_ms: 100,
    } as unknown as SdkMessageLike);

    // Assert: released rather than retained forever, and said out loud.
    const terminal = await store.peer().next(StoreWriteSchema);
    expect(terminal.batch!.events.map((event) => event.payload.case)).toEqual([
      "vendor",
      "accountUsageObservation",
      "turnEnded",
    ]);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: BigInt(terminal.batch!.events.length),
      lastSeq: 12n,
    }));
    await until(() => session.turnCount() === 0);
    expect(log.record("pending task notifications are backed by no SDK task this query instance ever ran")).toMatchObject({
      level: "warn",
      request_id: "p1",
      context: {
        live_sdk_task_count: 0,
        sdk_task_count: 0,
        completed_sdk_task_count: 0,
        pending_task_notification_count: 1,
        decision: "release_turn_over_unresolvable_task_notifications",
      },
    });
  });

  it("retains a turn for an in-epoch task notification a completed SDK task backs", async () => {
    // Arrange: one task of THIS query ran to completion, so its queued
    // notification can still drive another internal result cycle.
    const { query, store, daemon, session } = await rig({ storeSessionId: "vendor-uuid" });
    const log = captureLog();
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, {
      requestId: "p1",
      text: "fan out",
      promptOrigin: PromptOrigin.USER_SENT,
    }));
    const start = await store.peer().next(StoreWriteSchema);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: BigInt(start.batch!.events.length),
      lastSeq: 8n,
    }));
    await daemon.next(AckSchema);
    query.emit({
      type: "system",
      subtype: "task_started",
      uuid: "task-start-1",
      session_id: "vendor-uuid",
      task_id: "agent-go",
      task_type: "local_agent",
      tool_use_id: "tool-agent-go",
      description: "Go hello world",
    } as unknown as SdkMessageLike);
    const taskStart = await store.peer().next(StoreWriteSchema);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: BigInt(taskStart.batch!.events.length),
      lastSeq: 10n,
    }));
    query.emit({
      type: "system",
      subtype: "task_notification",
      uuid: "task-end-1",
      session_id: "vendor-uuid",
      task_id: "agent-go",
      tool_use_id: "tool-agent-go",
      status: "completed",
      output_file: "/tmp/agent-go.output",
      summary: "done",
    } as unknown as SdkMessageLike);
    const taskEnd = await store.peer().next(StoreWriteSchema);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: BigInt(taskEnd.batch!.events.length),
      lastSeq: 12n,
    }));
    store.latest().send(EventSchema, transcriptEvent(13n, create(TranscriptLineSchema, {
      line: {
        case: "queueOperation",
        value: create(QueueOperationLineSchema, {
          operation: QueueOp.ENQUEUE,
          content: "<task-notification>agent-go</task-notification>",
        }),
      },
    }), "vendor-uuid"));
    await daemon.next(EventSchema);

    // Act
    query.emit({
      type: "result",
      uuid: "parent-result",
      session_id: "vendor-uuid",
      subtype: "success",
      duration_ms: 100,
      origin: { kind: "task_notification" },
    } as unknown as SdkMessageLike);

    // Assert: no turn end, the turn is still the shim's claim.
    const retained = await store.peer().next(StoreWriteSchema);
    expect(retained.batch!.events.map((event) => event.payload.case)).toEqual(["vendor"]);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: 1n,
      lastSeq: 14n,
    }));
    expect(session.turnCount()).toBe(1);
    expect(log.record("SDK result retained while background-task result cycles remain outstanding")).toMatchObject({
      request_id: "p1",
      context: {
        pending_task_notification_count: 1,
        sdk_task_count: 1,
        decision: "retain_turn_for_sdk_task_cycles",
      },
    });
  });

  it("keeps the query alive when the SDK ends a task this instance never saw start", async () => {
    // Arrange: a resumed shim attaches to a vendor conversation whose
    // background task was started under the PREVIOUS shim process, so the
    // in-memory task table has no record of it.
    const { query, store, daemon, session, done } = await rig({ storeSessionId: "vendor-uuid" });
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, {
      requestId: "p1",
      text: "carry on",
      promptOrigin: PromptOrigin.USER_SENT,
    }));
    const start = await store.peer().next(StoreWriteSchema);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: BigInt(start.batch!.events.length),
      lastSeq: 8n,
    }));
    await daemon.next(AckSchema);

    // Act: the orphaned task reports its terminal fact.
    query.emit({
      type: "system",
      subtype: "task_notification",
      uuid: "orphan-end",
      session_id: "vendor-uuid",
      task_id: "a4699ecc217adfa70",
      status: "completed",
      output_file: "/tmp/orphan.output",
      summary: "done",
    } as unknown as SdkMessageLike);

    // Assert: the durable end still reaches the store, the turn is intact, and
    // the stream keeps flowing instead of dying as an iterator failure.
    const orphanEnd = await store.peer().next(StoreWriteSchema);
    expect(orphanEnd.batch!.events.map((event) => event.payload.case)).toEqual(["vendor", "taskEnded"]);
    expect(orphanEnd.batch!.events.every((event) => event.requestId === "p1")).toBe(true);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: BigInt(orphanEnd.batch!.events.length),
      lastSeq: 10n,
    }));
    expect(session.turnCount()).toBe(1);
    expect(query.abortCalls).toBe(0);
    await expect(Promise.race([done, Promise.resolve("alive")])).resolves.toBe("alive");
  });

  it("warns with the known-task census when it contains an unknown task end", async () => {
    // Arrange: one live task, so the census in the record is non-trivial.
    const { query, store, daemon } = await rig({ storeSessionId: "vendor-uuid" });
    const log = captureLog();
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, {
      requestId: "p1",
      text: "fan out",
      promptOrigin: PromptOrigin.USER_SENT,
    }));
    const start = await store.peer().next(StoreWriteSchema);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: BigInt(start.batch!.events.length),
      lastSeq: 8n,
    }));
    await daemon.next(AckSchema);
    query.emit({
      type: "system",
      subtype: "task_started",
      uuid: "task-start-1",
      session_id: "vendor-uuid",
      task_id: "agent-go",
      task_type: "local_agent",
      tool_use_id: "tool-agent-go",
      description: "Go hello world",
    } as unknown as SdkMessageLike);
    const taskStart = await store.peer().next(StoreWriteSchema);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: BigInt(taskStart.batch!.events.length),
      lastSeq: 10n,
    }));

    // Act
    query.emit({
      type: "system",
      subtype: "task_notification",
      uuid: "orphan-end",
      session_id: "vendor-uuid",
      task_id: "a4699ecc217adfa70",
      status: "completed",
    } as unknown as SdkMessageLike);
    const orphanEnd = await store.peer().next(StoreWriteSchema);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: BigInt(orphanEnd.batch!.events.length),
      lastSeq: 12n,
    }));

    // Assert
    expect(log.record("SDK emitted TaskEnded for a task this query instance never saw start")).toMatchObject({
      level: "warn",
      request_id: "p1",
      context: {
        unknown_ended_sdk_task_ids: ["a4699ecc217adfa70"],
        live_sdk_task_ids: ["agent-go"],
        sdk_subtype: "task_notification",
        decision: "contain_unknown_task_end",
      },
    });
  });

  it("reports an unknown task end to the daemon as a recovered DegradedState", async () => {
    // Arrange
    const { query, store, daemon } = await rig({ storeSessionId: "vendor-uuid" });
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, {
      requestId: "p1",
      text: "carry on",
      promptOrigin: PromptOrigin.USER_SENT,
    }));
    const start = await store.peer().next(StoreWriteSchema);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: BigInt(start.batch!.events.length),
      lastSeq: 8n,
    }));
    await daemon.next(AckSchema);

    // Act
    query.emit({
      type: "system",
      subtype: "task_notification",
      uuid: "orphan-end",
      session_id: "vendor-uuid",
      task_id: "a4699ecc217adfa70",
      status: "completed",
    } as unknown as SdkMessageLike);

    // Assert: a contained notice, not ongoing downtime.
    const evt = await daemon.next(EventSchema);
    expect(evt.payload.case).toBe("degradedState");
    if (evt.payload.case !== "degradedState") throw new Error("case");
    expect(evt.payload.value.component).toBe("claude-shim-task-lifecycle");
    expect(evt.payload.value.recovered).toBe(true);
    expect(evt.payload.value.reason).toContain("a4699ecc217adfa70");
  });

  it("still ends a known task without any containment notice", async () => {
    // Arrange: the task is started under THIS instance.
    const { query, store, daemon } = await rig({ storeSessionId: "vendor-uuid" });
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, {
      requestId: "p1",
      text: "fan out",
      promptOrigin: PromptOrigin.USER_SENT,
    }));
    const start = await store.peer().next(StoreWriteSchema);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: BigInt(start.batch!.events.length),
      lastSeq: 8n,
    }));
    await daemon.next(AckSchema);
    query.emit({
      type: "system",
      subtype: "task_started",
      uuid: "task-start-1",
      session_id: "vendor-uuid",
      task_id: "agent-go",
      task_type: "local_agent",
      tool_use_id: "tool-agent-go",
      description: "Go hello world",
    } as unknown as SdkMessageLike);
    const taskStart = await store.peer().next(StoreWriteSchema);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: BigInt(taskStart.batch!.events.length),
      lastSeq: 10n,
    }));
    const log = captureLog();

    // Act
    query.emit({
      type: "system",
      subtype: "task_notification",
      uuid: "task-end-1",
      session_id: "vendor-uuid",
      task_id: "agent-go",
      tool_use_id: "tool-agent-go",
      status: "completed",
    } as unknown as SdkMessageLike);
    const taskEnd = await store.peer().next(StoreWriteSchema);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: BigInt(taskEnd.batch!.events.length),
      lastSeq: 12n,
    }));

    // Assert
    expect(taskEnd.batch!.events.map((event) => event.payload.case)).toEqual(["vendor", "taskEnded"]);
    expect(log.count("SDK emitted TaskEnded for a task this query instance never saw start")).toBe(0);
    // Needle on the end's own commit record: the START commits under the same
    // message, so matching on the message alone would find the wrong line.
    await until(() => log.count(`"ended_sdk_task_ids":["agent-go"]`) === 1);
    expect(log.record(`"ended_sdk_task_ids":["agent-go"]`).context).toMatchObject({
      live_sdk_task_ids: [],
      decision: "commit_sdk_task_lifecycle",
    });
  });

  it("writes a non-lifecycle turn claim bridge into a rotated vendor seq space before its end", async () => {
    // Arrange: the old key has delivered data, so a different result UUID is
    // a real rotation. The prompt start is first written under that old key.
    const { query, store, daemon, daemonListener } = await rig({ storeSessionId: "uuid-old" });
    store.latest().send(EventSchema, create(EventSchema, { sessionId: "uuid-old", seq: 7n }));
    await daemon.next(EventSchema);
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go", promptOrigin: PromptOrigin.USER_SENT }));
    const oldStart = await store.peer().next(StoreWriteSchema);
    await daemon.next(AckSchema);
    expect(oldStart.batch!.events[0]!.sessionId).toBe("uuid-old");

    // Act: the matching result belongs to the newly minted vendor UUID.
    query.emit({
      type: "result",
      uuid: "r1",
      session_id: "uuid-new",
      subtype: "success",
    } as unknown as SdkMessageLike);
    const rotatedBatch = await store.peer().next(StoreWriteSchema);
    const daemon2 = await daemonListener.next();
    cleanups.push(() => daemon2.destroy());
    const hello2 = await daemon2.next(ShimHelloSchema);

    // Assert: the new space contains its own proof before completion, and the
    // handshake keeps the identity until this batch is durably acked.
    const kinds = rotatedBatch.batch!.events.map((event) => event.payload.case);
    const bridgeIndex = kinds.indexOf("turnClaimBridge");
    const usageIndex = kinds.indexOf("accountUsageObservation");
    const endIndex = kinds.indexOf("turnEnded");
    expect(kinds).not.toContain("turnStarted");
    expect(bridgeIndex).toBeGreaterThanOrEqual(0);
    expect(usageIndex).toBeGreaterThan(bridgeIndex);
    expect(endIndex).toBeGreaterThan(usageIndex);
    expect(endIndex).toBe(kinds.length - 1);
    expect(rotatedBatch.batch!.events[bridgeIndex]!.sessionId).toBe("uuid-new");
    expect(rotatedBatch.batch!.events[endIndex]!.sessionId).toBe("uuid-new");
    const bridged = rotatedBatch.batch!.events[bridgeIndex]!.payload;
    if (bridged.case !== "turnClaimBridge") throw new Error("case");
    expect(bridged.value).toMatchObject({
      turnId: "p1",
      previousSessionId: "uuid-old",
    });
    expect(hello2.activeTurnIds).toEqual(["p1"]);
    expect(hello2.turnInFlight).toBe(true);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: BigInt(rotatedBatch.batch!.events.length),
      lastSeq: 8n,
    }));
  });

  it("writes a rotated turn claim bridge before the first assistant usage", async () => {
    const { query, store, daemon } = await rig({ storeSessionId: "uuid-old" });
    store.latest().send(EventSchema, create(EventSchema, { sessionId: "uuid-old", seq: 7n }));
    await daemon.next(EventSchema);
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go", promptOrigin: PromptOrigin.USER_SENT }));
    await store.peer().next(StoreWriteSchema);
    await daemon.next(AckSchema);

    query.emit({
      type: "system",
      subtype: "init",
      session_id: "uuid-new",
      uuid: "i1",
      model: "claude",
      cwd: "/tmp",
    } as unknown as SdkMessageLike);
    const rotatedInitBatch = await store.peer().next(StoreWriteSchema);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: BigInt(rotatedInitBatch.batch!.events.length),
      lastSeq: 8n,
    }));

    query.emit({
      type: "assistant",
      uuid: "u1",
      session_id: "uuid-new",
      request_id: "api-request-9",
      parent_tool_use_id: null,
      message: {
        id: "m1",
        role: "assistant",
        model: "claude",
        content: [{ type: "text", text: "hi" }],
        stop_reason: "end_turn",
        usage: {
          input_tokens: 10,
          output_tokens: 2,
          cache_read_input_tokens: 0,
          cache_creation_input_tokens: 0,
        },
      },
    } as unknown as SdkMessageLike);
    const assistantBatch = await store.peer().next(StoreWriteSchema);

    if (assistantBatch.batch!.events[1]!.payload.case === "unparsed") {
      throw new Error(assistantBatch.batch!.events[1]!.payload.value.error);
    }

    expect(assistantBatch.batch!.events.map((event) => event.payload.case)).toEqual([
      "turnClaimBridge",
      "vendor",
    ]);
    expect(assistantBatch.batch!.events[0]!.requestId).toBe("p1");
    expect(assistantBatch.batch!.events[1]!.requestId).toBe("p1");
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: BigInt(assistantBatch.batch!.events.length),
      lastSeq: 10n,
    }));

    query.emit({
      type: "result",
      uuid: "r1",
      session_id: "uuid-new",
      subtype: "success",
    } as unknown as SdkMessageLike);
    const terminalBatch = await store.peer().next(StoreWriteSchema);
    expect(terminalBatch.batch!.events.map((event) => event.payload.case)).not.toContain("turnClaimBridge");
  });

  it("files the TurnStarted under the vendor session id, not the shim's own id", async () => {
    // Arrange: the store's seq space is keyed by the vendor uuid; writing
    // under `sess-1` would land in a space nothing subscribes to.
    const { store, daemon } = await rig({ storeSessionId: "vendor-uuid" });
    // Act
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go", promptOrigin: PromptOrigin.USER_SENT }));
    const sw = await store.peer().next(StoreWriteSchema);
    // Assert
    expect(sw.batch!.events[0]!.sessionId).toBe("vendor-uuid");
  });

  it("durably orders TurnStarted before start usage before admitting a fresh prompt", async () => {
    // Arrange: a fresh session has no vendor id, so both facts use the
    // placeholder seq space and any later rotation receives a claim bridge.
    const { query, store, daemon } = await rig({ holdTurnStartAck: true });

    // Act: hold the store acknowledgement at the boundary.
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go", promptOrigin: PromptOrigin.USER_SENT }));
    const start = await store.peer().next(StoreWriteSchema);

    // Assert: reducer-safe order is structural, and SDK admission cannot
    // overtake durability of either boundary fact.
    expect(start.batch!.events.map((event) => event.payload.case)).toEqual([
      "turnStarted",
      "accountUsageObservation",
    ]);
    expect(start.batch!.events.every((event) => event.sessionId === "sess-1")).toBe(true);
    const started = start.batch!.events[0]!.payload;
    if (started.case !== "turnStarted") throw new Error("case");
    expect(started.value.promptOrigin).toBe(PromptOrigin.USER_SENT);
    expect(query.prompts).toEqual([]);

    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: 2n,
      lastSeq: 2n,
    }));
    expect((await daemon.next(AckSchema)).requestId).toBe("p1");
    await until(() => query.prompts.length === 1);
  });

  it("does not duplicate a durable start when the SDK later reveals the vendor key", async () => {
    // Arrange: the start boundary is already durable in the initial seq space.
    const { query, store, daemon } = await rig();
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go", promptOrigin: PromptOrigin.USER_SENT }));
    await daemon.next(AckSchema);
    const start = await store.peer().next(StoreWriteSchema);
    expect(start.batch!.events.map((event) => event.payload.case)).toEqual([
      "turnStarted",
      "accountUsageObservation",
    ]);

    // Act: the SDK reveals the vendor session id.
    query.emit({
      type: "assistant",
      uuid: "u1",
      session_id: "vendor-uuid",
      message: {
        id: "m1",
        model: "claude",
        content: [{ type: "text", text: "hi" }],
        stop_reason: "end_turn",
        usage: {
          input_tokens: 10,
          output_tokens: 2,
          cache_read_input_tokens: 80,
          cache_creation_input_tokens: 10,
        },
      },
    } as unknown as SdkMessageLike);
    const vendor = await store.peer().next(StoreWriteSchema);

    // Assert: vendor evidence is written once under the adopted key; start
    // correlation crosses spaces only through the terminal claim bridge.
    expect(vendor.batch!.events.map((event) => event.payload.case)).not.toContain("turnStarted");
    expect(vendor.batch!.events[0]!.sessionId).toBe("vendor-uuid");
  });

  it("no system:init emits a SessionStarted twin, first or otherwise", async () => {
    // Arrange: readiness is asserted at start() now, so an init's twin would
    // re-announce a session already announced — and, arriving on the FIRST
    // PROMPT, would land after the fact.
    const { query, store } = await rig({ storeSessionId: "vendor-uuid" });
    const init = {
      type: "system",
      subtype: "init",
      session_id: "vendor-uuid",
      uuid: "i1",
      model: "claude",
      cwd: "/tmp",
    } as unknown as SdkMessageLike;
    // Act
    query.emit(init);
    const firstWrite = await store.peer().next(StoreWriteSchema);
    // Ack the first batch as production's store always does. StoreClient
    // pipelines sends (a batch issues without waiting for the prior ack), so
    // this ack is not needed to release the second emit — it keeps the fake
    // store's ack bookkeeping faithful to the real one.
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 2n, lastSeq: 2n }));
    query.emit(init);
    const secondWrite = await store.peer().next(StoreWriteSchema);
    // Assert
    expect(firstWrite.batch!.events.map((e) => e.payload.case)).not.toContain("sessionStarted");
    expect(secondWrite.batch!.events.map((e) => e.payload.case)).not.toContain("sessionStarted");
  });
});

describe("UdsSession lifecycle: shim-asserted readiness", () => {
  it("reports session health only after a live subscription and correlated store probe", async () => {
    // Arrange: the daemon's Subscribe establishes the live merged-event path
    // that a restored frontend would depend on.
    const { store, daemon } = await rig();

    // Act: the daemon requests session health; the shim opens a separate
    // store probe without moving its standing subscription.
    daemon.send(HealthCheckSchema, create(HealthCheckSchema, { requestId: "session-health-1" }));
    await until(() => store.count() === 3);
    const probe = store.latest();
    const check = await probe.next(HealthCheckSchema);
    expect(check.requestId).toBe("session-health-1");
    probe.send(HealthStatusSchema, create(HealthStatusSchema, {
      requestId: "session-health-1",
      healthy: true,
      component: "shim-store",
    }));

    // Assert: this is a response from the shim's dependency gate, not merely
    // a proof that a daemon socket exists.
    const status = await daemon.next(HealthStatusSchema);
    expect(status).toMatchObject({
      requestId: "session-health-1",
      healthy: true,
      component: "claude-shim",
    });
  });

  it("reports session health as unhealthy once the standing subscription is lost", async () => {
    // Arrange: the gate leaves this session subscribed, so the only way to
    // reach an unsubscribed shim is to LOSE the tail. The DegradedState the
    // loss produces is what tells this test the shim has noticed.
    const { store, daemon } = await rig();
    store.latest().destroy();
    const degraded = await daemon.next(EventSchema);
    expect(degraded.payload.case).toBe("degradedState");

    // Act
    daemon.send(HealthCheckSchema, create(HealthCheckSchema, { requestId: "session-health-no-sub" }));
    const status = await daemon.next(HealthStatusSchema);

    // Assert: rendering would otherwise race a missing store tail.
    expect(status.healthy).toBe(false);
    expect(status.reason).toContain("standing store subscription is not live");
  });

  it("acks readiness only after the standing store subscription has settled", async () => {
    // Arrange + Act: rig() runs the gate and drains its two frames.
    const { store, ready } = await rig({ fromSeq: 11n });
    // Assert: the ack names the position it subscribed at, and a store
    // subscription connection genuinely exists to carry it.
    expect(ready.fromSeq).toBe(11n);
    expect(store.count()).toBeGreaterThanOrEqual(2);
  });

  it("drives the store subscription from the from_seq the DaemonHello carried", async () => {
    // Arrange + Act: a resumed session whose daemon is 42 events in.
    const { standingSubscribe } = await rig({ fromSeq: 42n });
    // Assert: the hello is the ONLY place that number comes from now.
    expect(standingSubscribe.fromSeq).toBe(42n);
  });

  // THE POINT OF THE WHOLE MECHANISM. The SDK emits system:init only on the
  // first prompt, so a session nobody has typed into never announced itself
  // and its workspace sat in bring-up waiting for a message the user had no
  // reason to send. Readiness cannot be conditional on being used.
  it("announces readiness without any prompt ever being submitted", async () => {
    // Arrange + Act: rig() only starts the session and handshakes.
    const { readiness } = await rig();
    // Assert
    expect(readiness.payload.case).toBe("sessionStarted");
  });

  it("announces readiness without the SDK emitting anything at all", async () => {
    // Arrange + Act: no query.emit() in this test, deliberately.
    const { readiness } = await rig();
    // Assert: the assertion is the shim's own, not a twin of a vendor message.
    expect(readiness.sessionId).toBe("sess-1");
  });

  it("sends readiness DIRECT to the daemon, never through the store", async () => {
    // Arrange + Act
    const { store } = await rig();
    // Assert: the store keys by the VENDOR session id, unknown on a fresh
    // session until the first prompt — a store write here would be deferred
    // straight back into the trap this escapes.
    expect(store.peer().count(StoreWriteSchema)).toBe(0);
  });

  it("carries the session source on the readiness assertion", async () => {
    // Arrange + Act
    const { readiness } = await rig({ sessionSource: SessionSource.RESUME, storeSessionId: "vendor-uuid" });
    // Assert
    expect(readiness.payload.case).toBe("sessionStarted");
    if (readiness.payload.case === "sessionStarted") {
      expect(readiness.payload.value.source).toBe(SessionSource.RESUME);
    }
  });

  it("announces readiness exactly once per handshake", async () => {
    // Arrange: rig() already drained the one readiness event.
    const { daemon, query } = await rig({ storeSessionId: "vendor-uuid" });
    // Act: an init afterwards must not re-announce.
    query.emit({
      type: "system",
      subtype: "init",
      session_id: "vendor-uuid",
      uuid: "i1",
      model: "claude",
      cwd: "/tmp",
    } as unknown as SdkMessageLike);
    await tick();
    // Assert: nothing further reached the daemon directly.
    expect(daemon.count(EventSchema)).toBe(0);
  });
});

describe("UdsSession events: store-write vs ephemeral routing", () => {
  it("writes a persistent SDK message to the store, not to the daemon directly", async () => {
    // Arrange
    const { query, store, daemon } = await rig();
    // Act: a persistent assistant message.
    query.emit({
      type: "assistant",
      uuid: "u1",
      session_id: "sess-1",
      message: {
        id: "m1",
        model: "claude",
        content: [{ type: "text", text: "hi" }],
        stop_reason: "end_turn",
        usage: {
          input_tokens: 10,
          output_tokens: 2,
          cache_read_input_tokens: 80,
          cache_creation_input_tokens: 10,
        },
      },
    } as unknown as SdkMessageLike);
    const sw = await store.peer().next(StoreWriteSchema);
    await tick();
    // Assert: it went to the store as a vendor event, and NOT to the daemon.
    expect(sw.batch!.events).toHaveLength(1);
    if (sw.batch!.events[0]!.payload.case === "unparsed") {
      throw new Error(sw.batch!.events[0]!.payload.value.error);
    }
    expect(sw.batch!.events[0]!.payload.case).toBe("vendor");
    expect(daemon.count(EventSchema)).toBe(0);
  });

  it("sends an ephemeral stream_event straight to the daemon, never to the store", async () => {
    // Arrange
    const { query, store, daemon } = await rig();
    // Act: a live-typing delta (EPHEMERAL).
    query.emit({
      type: "stream_event",
      uuid: "u2",
      session_id: "sess-1",
      event: { type: "content_block_delta", index: 0, delta: { type: "text_delta", text: "x" } },
    } as unknown as SdkMessageLike);
    const evt = await daemon.next(EventSchema);
    await tick();
    // Assert: delivered directly as a ContentDelta; store saw no StoreWrite.
    expect(evt.payload.case).toBe("contentDelta");
    expect(store.peer().count(StoreWriteSchema)).toBe(0);
  });

  it("forwards input_json with its content-block tool identity directly to the daemon", async () => {
    const { query, store, daemon } = await rig();
    query.emit({
      type: "stream_event",
      uuid: "start-envelope",
      session_id: "sess-1",
      event: { type: "message_start", message: { id: "msg_tools" } },
    } as unknown as SdkMessageLike);
    query.emit({
      type: "stream_event",
      uuid: "tool-start-envelope",
      session_id: "sess-1",
      event: { type: "content_block_start", index: 3, content_block: { type: "tool_use", id: "toolu_three", name: "Bash", input: {} } },
    } as unknown as SdkMessageLike);
    query.emit({
      type: "stream_event",
      uuid: "input-envelope",
      session_id: "sess-1",
      event: { type: "content_block_delta", index: 3, delta: { type: "input_json_delta", partial_json: "{\"command\":\"pwd\"}" } },
    } as unknown as SdkMessageLike);

    const evt = await daemon.next(EventSchema);
    if (evt.payload.case !== "contentDelta" || evt.payload.value.delta.case !== "inputJson") throw new Error("expected input-json delta");
    expect(evt.payload.value).toMatchObject({
      uuid: "msg_tools",
      blockIndex: 3,
      toolUseId: "toolu_three",
    });
    expect(evt.payload.value.delta.value).toBe("{\"command\":\"pwd\"}");
    await tick();
    expect(store.peer().count(StoreWriteSchema)).toBe(0);
  });

  it("writes stamped message-start latency through the store for replay", async () => {
    // Arrange
    const { query, store, daemon } = await rig();
    // Act: first-token timing is durable analysis evidence, not live-only UI
    // state. The stream tracker must stamp the MessageLatency with m1.
    query.emit({
      type: "stream_event",
      uuid: "u3",
      session_id: "sess-1",
      ttft_ms: 865,
      event: { type: "message_start", message: { id: "m1", type: "message", role: "assistant" } },
    } as unknown as SdkMessageLike);
    const sw = await store.peer().next(StoreWriteSchema);
    await tick();
    // Assert: direct delivery is forbidden because a detached daemon must be
    // able to replay this response-timing evidence from the store.
    expect(sw.batch!.events).toHaveLength(1);
    const persisted = sw.batch!.events[0]!;
    expect([persisted.class, persisted.payload.case]).toEqual([
      EventClass.PERSISTENT,
      "messageLatency",
    ]);
    if (persisted.payload.case !== "messageLatency") throw new Error("case");
    expect(persisted.payload.value).toMatchObject({ uuid: "m1", ttftMs: 865n });
    expect(daemon.count(EventSchema)).toBe(0);
  });
});

describe("UdsSession events: store round-trip and sad path", () => {
  it("fails the SDK pump when durable message latency is rejected", async () => {
    const { daemon, done, query, store } = await rig({
      storeSessionId: "sess-1",
      queryInstanceId: "query-latency-write-failure",
    });
    const log = captureLog();

    query.emit({
      type: "stream_event",
      uuid: "stream-latency-failure",
      session_id: "sess-1",
      ttft_ms: 321,
      event: { type: "message_start", message: { id: "message-latency-failure", type: "message", role: "assistant" } },
    } as unknown as SdkMessageLike);
    const latencyWrite = await store.peer().next(StoreWriteSchema);
    expect(latencyWrite.batch!.events.map((event) => event.payload.case)).toEqual(["messageLatency"]);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { error: "latency disk full" }));
    expect((await daemon.next(EventSchema)).payload.case).toBe("degradedState");

    const terminalWrite = await store.peer().next(StoreWriteSchema);
    expect(terminalWrite.batch!.events.map((event) => event.payload.case)).toEqual(["queryLifecycle", "degradedState"]);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 2n, lastSeq: 3n }));
    const lifecycle = create(EventSchema, { ...terminalWrite.batch!.events[0]!, seq: 3n });
    store.latest().send(EventSchema, lifecycle);
    let delivered = await daemon.next(EventSchema);
    if (delivered.payload.case === "degradedState") delivered = await daemon.next(EventSchema);
    expect(delivered).toEqual(lifecycle);

    await expect(done).rejects.toMatchObject({
      name: "UnexpectedSdkStreamTerminationError",
      terminationKind: "iterator_throw",
      cause: expect.objectContaining({ message: expect.stringContaining("latency disk full") }),
    });
    expect(log.record("persistent SDK message latency did not receive a durable store receipt")).toMatchObject({
      operation: "shim.uds-session.persistent-evidence",
      agent_repl_session_id: "test-agent-session",
      claude_session_id: "sess-1",
      context: expect.objectContaining({
        query_instance_id: "query-latency-write-failure",
        api_message_id: "message-latency-failure",
        evidence_kind: "message_latency",
        failed_operation: "store.write.message_latency",
        outcome: "fatal_missing_persistent_evidence_receipt",
      }),
    });
  });

  it("fails the SDK pump when a terminal turn batch is rejected", async () => {
    const { daemon, done, query, store } = await rig({
      storeSessionId: "sess-1",
      queryInstanceId: "query-terminal-write-failure",
    });
    const log = captureLog();
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, {
      requestId: "turn-terminal-failure",
      text: "go",
      promptOrigin: PromptOrigin.USER_SENT,
    }));
    await store.peer().next(StoreWriteSchema);
    await daemon.next(AckSchema);

    query.emit({
      type: "result",
      uuid: "result-terminal-failure",
      session_id: "sess-1",
      subtype: "success",
    } as unknown as SdkMessageLike);
    const resultWrite = await store.peer().next(StoreWriteSchema);
    expect(resultWrite.batch!.events.map((event) => event.payload.case)).toContain("turnEnded");
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { error: "terminal disk full" }));
    expect((await daemon.next(EventSchema)).payload.case).toBe("degradedState");

    const terminalWrite = await store.peer().next(StoreWriteSchema);
    expect(terminalWrite.batch!.events.map((event) => event.payload.case)).toEqual(["queryLifecycle", "degradedState"]);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 2n, lastSeq: 4n }));
    const lifecycle = create(EventSchema, { ...terminalWrite.batch!.events[0]!, seq: 4n });
    store.latest().send(EventSchema, lifecycle);
    let delivered = await daemon.next(EventSchema);
    if (delivered.payload.case === "degradedState") delivered = await daemon.next(EventSchema);
    expect(delivered).toEqual(lifecycle);

    await expect(done).rejects.toMatchObject({
      name: "UnexpectedSdkStreamTerminationError",
      terminationKind: "iterator_throw",
      cause: expect.objectContaining({ message: expect.stringContaining("terminal disk full") }),
    });
    expect(log.record("persistent SDK event batch did not receive a durable store receipt")).toMatchObject({
      operation: "shim.uds-session.persistent-evidence",
      agent_repl_session_id: "test-agent-session",
      claude_session_id: "sess-1",
      request_id: "turn-terminal-failure",
      context: expect.objectContaining({
        query_instance_id: "query-terminal-write-failure",
        turn_id: "turn-terminal-failure",
        evidence_kind: "terminal_turn_batch",
        failed_operation: "store.write.terminal_turn_batch",
        outcome: "fatal_missing_persistent_evidence_receipt",
      }),
    });
  });

  it("forwards a merged store Event to the daemon (onMerged)", async () => {
    // Arrange
    const { store, daemon } = await rig();
    // Act: the store emits a merged, seq-stamped event on the subscription conn.
    store.latest().send(EventSchema, create(EventSchema, { sessionId: "sess-1", seq: 5n }));
    const evt = await daemon.next(EventSchema);
    // Assert
    expect(evt.seq).toBe(5n);
  });

  it("forwards a live persistent file-plane diagnostic field-faithfully without conversation conversion", async () => {
    // Arrange: this diagnostic has already been persisted by shim-store. The
    // shim is intentionally an opaque transport for this event family.
    const { store, daemon } = await rig();
    const diagnostic = create(FilePlaneDiagnosticSchema, {
      sourceRuntime: DiagnosticSourceRuntime.SIDECAR,
      level: "error",
      verbosity: "normal",
      operation: "sidecar.store.write",
      message: "store write failed",
      context: { table: "events", retryable: false, attempt: 3 },
      sourcePid: 4123n,
      sourcePath: "/tmp/sidecar.sock",
    });
    const persisted = create(EventSchema, {
      sessionId: "sess-1",
      seq: 5n,
      producedAtMs: 1720000000000n,
      requestId: "request-live-1",
      payload: { case: "filePlaneDiagnostic", value: diagnostic },
    });

    // Act
    store.latest().send(EventSchema, persisted);
    const forwarded = await daemon.next(EventSchema);

    // Assert: it remains the diagnostic payload and every durable field
    // reaches the daemon unchanged, rather than becoming conversation data.
    expect(forwarded).toEqual(persisted);
    expect(forwarded.payload.case).toBe("filePlaneDiagnostic");
    expect(forwarded.payload.case === "filePlaneDiagnostic" && forwarded.payload.value).toEqual(diagnostic);
  });

  it("forwards a store outage to the daemon as an Event(DegradedState)", async () => {
    // Arrange
    const { store, daemon } = await rig();
    // Act: the store connection drops.
    store.close();
    // Assert: a DegradedState event reaches the daemon.
    const evt = await daemon.next(EventSchema);
    expect(evt.payload.case).toBe("degradedState");
    if (evt.payload.case !== "degradedState") throw new Error("case");
    expect(evt.payload.value.component).toBe("shim-store-client");
  });
});

describe("UdsSession instrumentation: prompt round-trip receipts", () => {
  it("logs an acceptance receipt when it takes a SubmitPrompt", async () => {
    // Arrange
    const { daemon } = await rig();
    const log = captureLog();
    // Act
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "hello", promptOrigin: PromptOrigin.USER_SENT }));
    await daemon.next(AckSchema);
    // Assert
    expect(log.record("prompt accepted -> SDK input")).toMatchObject({
      operation: "shim.uds-session.lifecycle",
      claude_session_id: "sess-1",
      request_id: "p1",
      context: {
        plane: "stream",
        turn_id: "p1",
        len: 5,
        turns_in_flight: 1,
        decision: "turn_started",
      },
    });
  });

  it("logs a forward receipt for a merged TranscriptLine carrying the user's prompt", async () => {
    // Arrange: the file-plane echo — the carrier that becomes the GUI bubble.
    const { store, daemon } = await rig();
    const log = captureLog();
    // Act
    store.latest().send(EventSchema, transcriptEvent(9n, userLine(textMessage("hi there"))));
    await daemon.next(EventSchema);
    // Assert
    expect(log.record("user prompt event forwarded to daemon")).toMatchObject({
      operation: "shim.uds-session.lifecycle",
      claude_session_id: "sess-1",
      context: { seq: "9", len: 8, arm: "transcript_user_line" },
    });
  });

  it("logs a forward receipt for a merged stream-plane UserMessage", async () => {
    // Arrange: how the same turn arrives on a resume replay.
    const { store, daemon } = await rig();
    const log = captureLog();
    // Act
    store.latest().send(EventSchema, streamEvent(4n, create(ClaudeStreamMessageSchema, {
      msg: { case: "user", value: create(UserMessageSchema, { message: textMessage("hey") }) },
    })));
    await daemon.next(EventSchema);
    // Assert
    expect(log.record("user prompt event forwarded to daemon")).toMatchObject({
      operation: "shim.uds-session.lifecycle",
      claude_session_id: "sess-1",
      context: { seq: "4", len: 3, arm: "user_message" },
    });
  });

  it("stays silent for a user message carrying only tool_result blocks", async () => {
    // Arrange: pure tool feedback rides the user role too. A receipt per tool
    // call would bury the one receipt per prompt this instrumentation is for.
    const { store, daemon } = await rig();
    const log = captureLog();
    // Act
    store.latest().send(EventSchema, transcriptEvent(9n, userLine(create(ApiUserMessageSchema, {
      content: {
        case: "contentBlocks",
        value: create(ApiContentBlocksSchema, {
          blocks: [create(ContentBlockSchema, {
            block: { case: "toolResult", value: create(ToolResultBlockSchema, { toolUseId: "t1" }) },
          })],
        }),
      },
    }))));
    await daemon.next(EventSchema);
    // Assert
    expect(log.count("user prompt event forwarded to daemon")).toBe(0);
  });

  it("stays silent for a merged event that is not a user message at all", async () => {
    // Arrange
    const { store, daemon } = await rig();
    const log = captureLog();
    // Act: an assistant transcript line on the same vendor carrier.
    store.latest().send(EventSchema, transcriptEvent(9n, create(TranscriptLineSchema, {
      line: { case: "assistant", value: create(AssistantLineSchema, {}) },
    })));
    await daemon.next(EventSchema);
    // Assert
    expect(log.count("user prompt event forwarded to daemon")).toBe(0);
  });
});

describe("UdsSession lifetime: reattach", () => {
  it("a daemon disconnect ends neither the session nor the in-flight turn", async () => {
    // Arrange: a turn is in flight.
    const { session, query, queryFactoryCalls, store, daemon, daemonListener } = await rig();
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go", promptOrigin: PromptOrigin.USER_SENT }));
    await daemon.next(AckSchema);
    await until(() => query.prompts.length === 1);
    // Act: the daemon vanishes mid-turn.
    daemon.destroy();
    await until(() => !session.isConnected());
    // Assert: the turn survives and the store connection is untouched.
    expect(session.turnCount()).toBe(1);
    expect(queryFactoryCalls()).toBe(1);
    expect(query.abortCalls).toBe(0);
    expect(store.peer().closed).toBe(false);
    // The shim redials on its own; the replacement daemon accepts and
    // resubscribes on the SAME live session.
    const daemon2 = await daemonListener.next();
    cleanups.push(() => daemon2.destroy());
    const hello = await daemon2.next(ShimHelloSchema);
    expect(hello.turnInFlight).toBe(true);
    expect(hello.activeTurnIds).toEqual(["p1"]);
    daemon2.send(DaemonHelloSchema, create(DaemonHelloSchema, { daemonVersion: "d2", fromSeq: 2n }));
    // The replacement daemon re-runs the WHOLE gate, from_seq and all. That is
    // the point, not a duplicate: a restarted daemon has no memory of this
    // session and must be wired to it exactly as the first one was.
    await until(() => store.count() >= 3);
    const sub2 = await store.latest().next(SubscribeSchema);
    expect(sub2.fromSeq).toBe(2n);
    const readiness2 = await daemon2.next(EventSchema);
    expect(readiness2.payload.case).toBe("sessionStarted");
    expect((await daemon2.next(ShimReadySchema)).fromSeq).toBe(2n);
    store.latest().send(EventSchema, create(EventSchema, { sessionId: "sess-1", seq: 3n }));
    const e3 = await daemon2.next(EventSchema);
    expect(e3.seq).toBe(3n);
  });
});

describe("UdsSession lifetime: SDK stream termination", () => {
  it("persists startup failure and its cursor-releasing degradation in one batch", async () => {
    // Arrange: the query exists, but its initial lifecycle receipt is rejected
    // before a daemon can finish the bring-up handshake.
    const store = await fakeStore();
    cleanups.push(() => store.close());
    let query!: FakeQuery;
    const session = new UdsSession({
      sessionId: "sess-startup-failure",
      shimVersion: "9.9",
      protocolVersion: "1",
      udsSocketPath: tmpSocketPath(),
      storeSocketPath: store.socketPath,
      sessionSource: SessionSource.FRESH,
      queryInstanceId: "startup-query",
      createQuery: (prompt, canUseTool): UdsQuery => {
        const live = new FakeQuery(prompt, canUseTool);
        query = live;
        return { query: live, subscriptionUsage: () => live.subscriptionUsage(), abort: () => live.abort() };
      },
      heartbeatIntervalMs: 0,
    });

    // Act: reject QueryCreated, which moves start() through its startup-failure
    // terminal path after the SDK query has been constructed.
    const done = session.start();
    await until(() => store.count() >= 1);
    const created = await store.peer().next(StoreWriteSchema);
    expect(created.batch!.events).toHaveLength(1);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { error: "created lifecycle refused" }));
    const terminal = await store.peer().next(StoreWriteSchema);

    // Assert: the lifecycle and companion are one durable, ordered unit.  A
    // restored daemon therefore sees both sides of its replay-cursor contract.
    expect(terminal.batch!.events.map((event) => event.payload.case)).toEqual(["queryLifecycle", "degradedState"]);
    const lifecycle = terminal.batch!.events[0]!;
    const companion = terminal.batch!.events[1]!;
    if (lifecycle.payload.case !== "queryLifecycle" || lifecycle.payload.value.event.case !== "terminated") throw new Error("case");
    expect(lifecycle.payload.value.event.value.reason).toEqual({
      case: "startupFailure",
      value: expect.objectContaining({ cause: expect.stringContaining("created lifecycle refused") }),
    });
    expect(lifecycle.payload.value.event.value.vendorIdentity.case).toBe("vendorSessionIdentityUnavailable");
    expect(companion.payload).toEqual(expect.objectContaining({
      case: "degradedState",
      value: expect.objectContaining({
        component: "claude-shim-sdk",
        reason: "unexpected_query_termination",
        queryInstanceId: "startup-query",
      }),
    }));
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 2n, lastSeq: 2n }));
    await expect(done).rejects.toThrow("created lifecycle refused");
    expect(query.abortCalls).toBe(1);
  });

  it("settles SIGTERM before daemon readiness without leaving the owned query alive", async () => {
    // Arrange: the store starts, but no daemon listener exists, so this is the
    // startup window where SessionServer.connect() retries indefinitely.
    const store = await fakeStore();
    cleanups.push(() => store.close());
    let query!: FakeQuery;
    const session = new UdsSession({
      sessionId: "sess-starting",
      shimVersion: "9.9",
      protocolVersion: "1",
      udsSocketPath: tmpSocketPath(),
      storeSocketPath: store.socketPath,
      sessionSource: SessionSource.FRESH,
      createQuery: (prompt, canUseTool): UdsQuery => {
        const live = new FakeQuery(prompt, canUseTool);
        query = live;
        return { query: live, subscriptionUsage: () => live.subscriptionUsage(), abort: () => live.abort() };
      },
      heartbeatIntervalMs: 0,
    });
    const done = session.start();
    await acknowledgeInitialQueryLifecycle(store);
    // Act: no readiness wait or polling may be needed to make SIGTERM win.
    const shuttingDown = session.shutdown("SIGTERM");
    const terminated = await store.peer().next(StoreWriteSchema);
    expect(terminated.batch!.events).toHaveLength(1);
    expect(terminated.batch!.events[0]!.payload.case).toBe("queryLifecycle");
    if (terminated.batch!.events[0]!.payload.case !== "queryLifecycle") throw new Error("case");
    expect(terminated.batch!.events[0]!.payload.value.event.case).toBe("terminated");
    let settled = false;
    void shuttingDown.then(() => { settled = true; });
    await tick();
    expect(settled).toBe(false);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 1n, lastSeq: 2n }));
    await shuttingDown;
    // Assert
    await expect(done).resolves.toBeUndefined();
    expect(query.abortCalls).toBe(1);
  });

  it("fails intentional shutdown after cleanup when its termination receipt is rejected", async () => {
    const store = await fakeStore();
    cleanups.push(() => store.close());
    const log = captureLog();
    let query!: FakeQuery;
    const session = new UdsSession({
      sessionId: "sess-rejected-stop",
      shimVersion: "9.9",
      protocolVersion: "1",
      udsSocketPath: tmpSocketPath(),
      storeSocketPath: store.socketPath,
      sessionSource: SessionSource.FRESH,
      queryInstanceId: "query-rejected-stop",
      createQuery: (prompt, canUseTool): UdsQuery => {
        const live = new FakeQuery(prompt, canUseTool);
        query = live;
        return { query: live, subscriptionUsage: () => live.subscriptionUsage(), abort: () => live.abort() };
      },
      heartbeatIntervalMs: 0,
    });
    const done = session.start();
    await acknowledgeInitialQueryLifecycle(store);
    const storeClosed = once(store.peer().socket, "close");

    const shuttingDown = session.shutdown("SIGTERM");
    const terminated = await store.peer().next(StoreWriteSchema);
    expect(terminated.batch!.events).toHaveLength(1);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { error: "disk full" }));

    const failure = await shuttingDown.catch((cause: unknown) => cause);
    expect(failure).toBeInstanceOf(QueryTerminationPersistenceError);
    expect(isQueryTerminationPersistenceError(failure)).toBe(true);
    expect(isQueryTerminationPersistenceError(new Error("different failure"))).toBe(false);
    expect(failure).toEqual(expect.objectContaining({
      name: "QueryTerminationPersistenceError",
      terminationKind: "intentional",
      queryInstanceId: "query-rejected-stop",
      cause: expect.objectContaining({ message: expect.stringContaining("disk full") }),
    }));
    await expect(done).resolves.toBeUndefined();
    await storeClosed;
    expect(query.abortCalls).toBe(1);
    expect(store.peer().closed).toBe(true);
    expect(log.record("query termination could not receive a durable store receipt").context).toMatchObject({
      query_instance_id: "query-rejected-stop",
      store_key: "sess-rejected-stop",
      termination_kind: "intentional",
      termination_cause: "SIGTERM",
      outcome: "fatal_missing_termination_receipt",
      cause: expect.objectContaining({ message: expect.stringContaining("disk full") }),
    });
    expect(log.count("intentional shim shutdown complete")).toBe(0);
  });

  it("fails the shim when the SDK iterator ends without intentional shutdown", async () => {
    // Arrange
    const { daemon, done, query, store } = await rig({
      storeSessionId: "vendor-uuid",
      queryInstanceId: "query-eof",
    });
    const log = captureLog();
    const storeClosed = once(store.peer().socket, "close");
    const daemonClosed = once(daemon.socket, "close");
    // Act
    query.endStream();
    const persisted = await store.peer().next(StoreWriteSchema);
    expect(persisted.batch!.events.map((event) => event.payload.case)).toEqual(["queryLifecycle", "degradedState"]);
    const report = persisted.batch!.events[1]!;
    expect([report.class, report.payload.case]).toEqual([
      EventClass.PERSISTENT,
      "degradedState",
    ]);
    if (report.payload.case !== "degradedState") throw new Error("case");
    expect(report.payload.value).toMatchObject({
      component: "claude-shim-sdk",
      reason: "unexpected_query_termination",
      queryInstanceId: "query-eof",
    });
    const lifecycleEvent = persisted.batch!.events[0]!.payload;
    if (lifecycleEvent.case !== "queryLifecycle" || lifecycleEvent.value.event.case !== "terminated") throw new Error("case");
    expect(lifecycleEvent.value.event.value.vendorIdentity).toEqual({ case: "vendorSessionId", value: "vendor-uuid" });
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 2n, lastSeq: 3n }));
    const lifecycle = create(EventSchema, {
      ...persisted.batch!.events[0]!,
      seq: 3n,
    });
    store.latest().send(EventSchema, lifecycle);
    const delivered = await daemon.next(EventSchema);
    expect(delivered).toEqual(lifecycle);
    // Assert: EOF is not a normal session completion and cannot leave the
    // process alive with a dead query it might try to replace. Shutdown waits
    // for the durable lifecycle fact to reach the connected daemon.
    await expect(done).rejects.toMatchObject({
      name: "UnexpectedSdkStreamTerminationError",
      terminationKind: "iterator_eof",
    });
    await Promise.all([storeClosed, daemonClosed]);
    expect(query.abortCalls).toBe(0);
    expect(store.peer().closed).toBe(true);
    expect(log.count("SDK stream terminated outside intentional shim shutdown; exiting nonzero")).toBe(1);
    expect(log.record("SDK stream terminated outside intentional shim shutdown").context).toMatchObject({
      query_instance_id: "query-eof",
      vendor_session_id: "vendor-uuid",
      termination_kind: "iterator_eof",
      termination_arm: "unexpectedEof",
      termination_cause: null,
      intentional: false,
      active_turn_ids: [],
      input_ended: false,
      query_aborted: false,
      resume_requested: false,
      store_key: "vendor-uuid",
    });
  });

  it("persists the exact unexpected-termination report while the daemon is detached", async () => {
    // Arrange: no live daemon socket can carry a direct ephemeral report.
    const { daemon, daemonListener, done, query, session, store } = await rig({ queryInstanceId: "query-before-vendor" });
    daemon.destroy();
    await until(() => !session.isConnected());

    // Act
    query.endStream();
    const persisted = await store.peer().next(StoreWriteSchema);

    // Assert: this StoreWrite is the replayable notification a restored daemon
    // receives after it reconnects; it is not contingent on the dead socket.
    expect(persisted.batch!.events.map((event) => event.payload.case)).toEqual(["queryLifecycle", "degradedState"]);
    const report = persisted.batch!.events[1]!;
    expect([report.class, report.payload.case]).toEqual([
      EventClass.PERSISTENT,
      "degradedState",
    ]);
    if (report.payload.case !== "degradedState") throw new Error("case");
    expect(report.payload.value).toMatchObject({
      component: "claude-shim-sdk",
      reason: "unexpected_query_termination",
      queryInstanceId: "query-before-vendor",
    });
    const preVendorLifecycle = persisted.batch!.events[0]!.payload;
    if (preVendorLifecycle.case !== "queryLifecycle" || preVendorLifecycle.value.event.case !== "terminated") throw new Error("case");
    expect(preVendorLifecycle.value.event.value.vendorIdentity.case).toBe("vendorSessionIdentityUnavailable");
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 2n, lastSeq: 3n }));

    let settled = false;
    void done.finally(() => { settled = true; }).catch(() => undefined);
    await tick();
    expect(settled).toBe(false);

    // The shim remains available until a replacement daemon subscribes and
    // receives the exact persisted termination lifecycle through store replay.
    const daemon2 = await daemonListener.next();
    cleanups.push(() => daemon2.destroy());
    await daemon2.next(ShimHelloSchema);
    daemon2.send(DaemonHelloSchema, create(DaemonHelloSchema, {
      daemonVersion: "d2",
      protocolVersion: "1",
      fromSeq: 2n,
    }));
    await until(() => store.count() >= 3);
    expect((await store.latest().next(SubscribeSchema)).fromSeq).toBe(2n);
    expect((await daemon2.next(EventSchema)).payload.case).toBe("sessionStarted");
    expect((await daemon2.next(ShimReadySchema)).fromSeq).toBe(2n);

    const lifecycle = create(EventSchema, {
      ...persisted.batch!.events[0]!,
      seq: 3n,
    });
    store.latest().send(EventSchema, lifecycle);
    expect(await daemon2.next(EventSchema)).toEqual(lifecycle);
    await expect(done).rejects.toMatchObject({
      name: "UnexpectedSdkStreamTerminationError",
      terminationKind: "iterator_eof",
    });
  });

  it("closes resources before surfacing a rejected unexpected-termination receipt", async () => {
    // Arrange
    const { daemon, done, query, session, store } = await rig({
      storeSessionId: "vendor-uuid",
      queryInstanceId: "query-rejected-eof",
    });
    const log = captureLog();
    const storeClosed = once(store.peer().socket, "close");
    const daemonClosed = once(daemon.socket, "close");

    // Act: the query is dead, but the store refuses the failure record that a
    // restored daemon would need in order to explain that death.
    query.endStream();
    const persisted = await store.peer().next(StoreWriteSchema);
    expect(persisted.batch!.events.map((event) => event.payload.case)).toEqual(["queryLifecycle", "degradedState"]);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { error: "disk full" }));

    // Assert: cleanup is scope-bound to the fatal path, while the exact store
    // rejection remains attached to the typed propagated failure.
    const failure = await done.catch((cause: unknown) => cause);
    expect(failure).toEqual(expect.objectContaining({
      name: "QueryTerminationPersistenceError",
      terminationKind: "iterator_eof",
      queryInstanceId: "query-rejected-eof",
      cause: expect.objectContaining({ message: expect.stringContaining("disk full") }),
    }));
    await Promise.all([storeClosed, daemonClosed]);
    expect(store.peer().closed).toBe(true);
    expect(session.isConnected()).toBe(false);
    expect(query.abortCalls).toBe(0);

    const record = log.record("unexpected SDK termination degradation could not receive a durable store receipt");
    expect(record).toMatchObject({
      operation: "shim.uds-session.unexpected-termination-delivery",
      agent_repl_session_id: "test-agent-session",
      claude_session_id: "vendor-uuid",
    });
    expect(record.context).toMatchObject({
      query_instance_id: "query-rejected-eof",
      vendor_session_id: "vendor-uuid",
      store_key: "vendor-uuid",
      termination_kind: "iterator_eof",
      termination_arm: "unexpectedEof",
      termination_cause: null,
      failed_operation: "store.write.unexpected_query_termination",
      outcome: "fatal_missing_termination_receipt",
      cause: expect.objectContaining({ message: expect.stringContaining("disk full") }),
    });
  });

  it("aggregates a rejected unexpected-termination receipt with cleanup failure", async () => {
    const { daemon, done, query, session, store } = await rig({
      storeSessionId: "vendor-uuid",
      queryInstanceId: "query-double-failure",
    });
    const log = captureLog();
    const cleanupFailure = new Error("resource close exploded");
    const closeSpy = vi.spyOn(
      session as unknown as { closeResources(): Promise<void> },
      "closeResources",
    ).mockRejectedValue(cleanupFailure);
    cleanups.push(() => {
      closeSpy.mockRestore();
      daemon.destroy();
    });

    query.endStream();
    const persisted = await store.peer().next(StoreWriteSchema);
    expect(persisted.batch!.events.map((event) => event.payload.case)).toEqual(["queryLifecycle", "degradedState"]);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { error: "disk full" }));

    const failure = await done.catch((cause: unknown) => cause);
    expect(failure).toBeInstanceOf(QueryTerminationCleanupError);
    expect(isQueryTerminationCleanupError(failure)).toBe(true);
    expect(isQueryTerminationCleanupError(new Error("different failure"))).toBe(false);
    expect((failure as QueryTerminationCleanupError).errors).toEqual([
      expect.objectContaining({
        name: "QueryTerminationPersistenceError",
        cause: expect.objectContaining({ message: expect.stringContaining("disk full") }),
      }),
      cleanupFailure,
    ]);
    expect(log.record("unexpected SDK termination persistence and resource cleanup both failed")).toMatchObject({
      operation: "shim.uds-session.unexpected-termination-cleanup",
      context: expect.objectContaining({
        query_instance_id: "query-double-failure",
        termination_arm: "unexpectedEof",
        outcome: "fatal_persistence_and_cleanup_failure",
        cleanup_failure: expect.objectContaining({ message: "resource close exploded" }),
      }),
    });
  });

  it("preserves the typed unexpected termination when cleanup alone fails", async () => {
    const { daemon, done, query, session, store } = await rig({
      storeSessionId: "vendor-uuid",
      queryInstanceId: "query-cleanup-failure",
    });
    const log = captureLog();
    const cleanupFailure = new Error("resource close exploded");
    const closeSpy = vi.spyOn(
      session as unknown as { closeResources(): Promise<void> },
      "closeResources",
    ).mockRejectedValue(cleanupFailure);
    cleanups.push(() => {
      closeSpy.mockRestore();
      daemon.destroy();
    });

    query.endStream();
    const persisted = await store.peer().next(StoreWriteSchema);
    expect(persisted.batch!.events.map((event) => event.payload.case)).toEqual(["queryLifecycle", "degradedState"]);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 2n, lastSeq: 3n }));
    const lifecycle = create(EventSchema, { ...persisted.batch!.events[0]!, seq: 3n });
    store.latest().send(EventSchema, lifecycle);
    expect(await daemon.next(EventSchema)).toEqual(lifecycle);

    const failure = await done.catch((cause: unknown) => cause);
    expect(failure).toBeInstanceOf(QueryTerminationCleanupError);
    expect(isQueryTerminationCleanupError(failure)).toBe(true);
    expect((failure as QueryTerminationCleanupError).errors).toEqual([
      expect.objectContaining({
        name: "UnexpectedSdkStreamTerminationError",
        terminationKind: "iterator_eof",
      }),
      cleanupFailure,
    ]);
    expect(log.record("unexpected SDK termination resource cleanup failed")).toMatchObject({
      operation: "shim.uds-session.unexpected-termination-cleanup",
      context: expect.objectContaining({
        query_instance_id: "query-cleanup-failure",
        termination_arm: "unexpectedEof",
        outcome: "fatal_cleanup_failure",
        cleanup_failure: expect.objectContaining({ message: "resource close exploded" }),
      }),
    });
  });

  it("fails the shim with the original cause when the SDK iterator throws", async () => {
    // Arrange
    const { daemon, done, query, store } = await rig({ storeSessionId: "vendor-uuid" });
    const log = captureLog();
    const storeClosed = once(store.peer().socket, "close");
    const daemonClosed = once(daemon.socket, "close");
    // Act
    query.failStream(new Error("sdk transport exploded"));
    const persisted = await store.peer().next(StoreWriteSchema);
    expect(persisted.batch!.events.map((event) => event.payload.case)).toEqual(["queryLifecycle", "degradedState"]);
    if (persisted.batch!.events[0]!.payload.case !== "queryLifecycle") throw new Error("case");
    const lifecycle = persisted.batch!.events[0]!.payload.value.event;
    if (lifecycle.case !== "terminated") throw new Error("case");
    expect(lifecycle.value.reason.case).toBe("iteratorFailure");
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 2n, lastSeq: 3n }));
    const deliveredLifecycle = create(EventSchema, {
      ...persisted.batch!.events[0]!,
      seq: 3n,
    });
    store.latest().send(EventSchema, deliveredLifecycle);
    expect(await daemon.next(EventSchema)).toEqual(deliveredLifecycle);
    // Assert
    await expect(done).rejects.toMatchObject({
      name: "UnexpectedSdkStreamTerminationError",
      terminationKind: "iterator_throw",
      cause: expect.objectContaining({ message: "sdk transport exploded" }),
    });
    await Promise.all([storeClosed, daemonClosed]);
    expect(query.abortCalls).toBe(0);
    expect(store.peer().closed).toBe(true);
    expect(log.record("SDK stream terminated outside intentional shim shutdown").context).toMatchObject({
      termination_kind: "iterator_throw",
      intentional: false,
      cause: expect.objectContaining({ message: "sdk transport exploded" }),
    });
  });

  it("ends the owned query exactly once during intentional shutdown", async () => {
    // Arrange
    const { session, done, query } = await rig();
    const log = captureLog();
    // Act
    const stop = session.shutdown("SIGTERM");
    // Assert
    await stop;
    await expect(done).resolves.toBeUndefined();
    expect(query.abortCalls).toBe(1);
    expect(log.count("SDK stream terminated outside intentional shim shutdown; exiting nonzero")).toBe(0);
  });
});

describe("UdsSession permission round-trip", () => {
  it("emits a PermissionRequest and resolves canUseTool on the ALLOW response", async () => {
    // Arrange
    const { query, daemon } = await rig();
    const ac = new AbortController();
    // Act: the SDK asks to use a tool.
    const decision: Promise<PermissionResultLike> = query.canUseTool("Bash", { command: "ls" }, { signal: ac.signal });
    const req = await daemon.next(PermissionRequestSchema);
    expect(req.toolName).toBe("Bash");
    daemon.send(PermissionResponseSchema, create(PermissionResponseSchema, {
      requestId: req.requestId,
      decision: PermissionDecision.ALLOW,
      updatedInput: { command: "ls -la" },
    }));
    // Assert
    const result = await decision;
    expect(result.behavior).toBe("allow");
    if (result.behavior !== "allow") throw new Error("behavior");
    expect(result.updatedInput).toEqual({ command: "ls -la" });
  });

  it("an Interrupt cancels the pending permission and forwards to the SDK", async () => {
    // Arrange: a permission wait is outstanding.
    const { query, daemon } = await rig();
    const ac = new AbortController();
    const decision: Promise<PermissionResultLike> = query.canUseTool("Bash", { command: "rm" }, { signal: ac.signal });
    await daemon.next(PermissionRequestSchema);
    // Act
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1" }));
    await daemon.next(AckSchema);
    // Assert: the SDK was interrupted and the blocked callback resolved as deny.
    await until(() => query.interruptCalls >= 1);
    const result = await decision;
    expect(result.behavior).toBe("deny");
  });

  it("an interrupt receipt reporting survivors reaches the daemon as DegradedState", async () => {
    // Arrange: the SDK will answer interrupt() with work that outlived it.
    const { query, daemon } = await rig();
    query.interruptReceipt = { still_queued: ["u-1", "u-2"] };
    // Act
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1" }));
    await daemon.next(AckSchema);
    // Assert: the survivors are surfaced, not swallowed into a log line.
    const evt = await daemon.next(EventSchema);
    expect(evt.payload.case).toBe("degradedState");
    if (evt.payload.case !== "degradedState") throw new Error("case");
    expect(evt.payload.value.component).toBe("claude-shim-interrupt");
    expect(evt.payload.value.reason).toContain("still_queued=[u-1 u-2]");
  });

  it("an interrupt receipt with no survivors emits no DegradedState", async () => {
    // Arrange: the ordinary case — nothing outlived the interrupt.
    const { query, store, daemon } = await rig();
    query.interruptReceipt = { still_queued: [] };
    // Act
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1" }));
    await daemon.next(AckSchema);
    await until(() => query.interruptCalls >= 1);
    // Assert: the FIRST Event to arrive is the store outage deliberately
    // provoked afterwards, proving the quiet interrupt emitted none of its own.
    store.close();
    const evt = await daemon.next(EventSchema);
    if (evt.payload.case !== "degradedState") throw new Error("case");
    expect(evt.payload.value.component).toBe("shim-store-client");
  });
});

// ---------------------------------------------------------------------------
// The three-valued interrupt outcome, decided against the REAL turn counter.
// ---------------------------------------------------------------------------

describe("UdsSession interrupt outcome", () => {
  it("reports INTERRUPTED when a live turn was signalled", async () => {
    // Arrange: a turn is genuinely in flight.
    const { daemon, session } = await rig();
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go", promptOrigin: PromptOrigin.USER_SENT }));
    await daemon.next(AckSchema);
    await until(() => session.turnCount() === 1);
    // Act
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1" }));
    const ack = await daemon.next(AckSchema);
    // Assert
    expect(ack.interruptOutcome).toBe(InterruptOutcome.INTERRUPTED);
  });

  it("reports ALREADY_COMPLETE when no turn was in flight", async () => {
    // Arrange: nothing has ever been submitted.
    const { daemon, session } = await rig();
    expect(session.turnCount()).toBe(0);
    // Act
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1" }));
    const ack = await daemon.next(AckSchema);
    // Assert — a SUCCESS: the user asked for the turn to be over and it is.
    // The old two-valued reading painted precisely this as a failed stop.
    expect(ack.interruptOutcome).toBe(InterruptOutcome.ALREADY_COMPLETE);
  });

  it("reports ALREADY_COMPLETE once the turn has ended", async () => {
    // Arrange: submit, then let the turn's result close it.
    const { daemon, query, session } = await rig();
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go", promptOrigin: PromptOrigin.USER_SENT }));
    await daemon.next(AckSchema);
    await until(() => session.turnCount() === 1);
    query.emit({
      type: "result",
      uuid: "r1",
      session_id: "vendor-uuid",
      subtype: "success",
    } as unknown as SdkMessageLike);
    await until(() => session.turnCount() === 0);
    // Act
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1" }));
    const ack = await daemon.next(AckSchema);
    // Assert
    expect(ack.interruptOutcome).toBe(InterruptOutcome.ALREADY_COMPLETE);
  });

  // THE RACE PIN. The liveness read happens synchronously, before any await
  // and before any promise is created, so a turn that ends AFTER the stop
  // landed cannot retroactively rewrite the verdict. Deciding this
  // downstream is what made it ambiguous: an observer watching for the
  // turn's `aborted` result sees the two as unordered events.
  it("freezes the outcome at the instant the stop landed", async () => {
    // Arrange: a live turn, interrupted while it is genuinely running.
    const { daemon, query, session } = await rig();
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go", promptOrigin: PromptOrigin.USER_SENT }));
    await daemon.next(AckSchema);
    await until(() => session.turnCount() === 1);
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1" }));
    const ack = await daemon.next(AckSchema);
    // Act: the turn ends AFTERWARDS.
    query.emit({
      type: "result",
      uuid: "r1",
      session_id: "vendor-uuid",
      subtype: "success",
    } as unknown as SdkMessageLike);
    await until(() => session.turnCount() === 0);
    // Assert: the verdict already written stays INTERRUPTED. It is a
    // statement about the turn AT THE MOMENT THE STOP LANDED, and nothing
    // later may rewrite it — which is the whole reason it is decided inside
    // the handler rather than derived from what the result turns out to say.
    expect(ack.interruptOutcome).toBe(InterruptOutcome.INTERRUPTED);
  });

  // The counterpart, and the one my first draft of the pin got wrong: when
  // the turn really did end before the stop arrived, ALREADY_COMPLETE is the
  // TRUTHFUL answer, not a missed interrupt. The outcome reports what was
  // true at delivery, and delivery is when the frame lands — not when the
  // caller decided to send it.
  it("reports ALREADY_COMPLETE when the turn ended in transit", async () => {
    // Arrange: a live turn.
    const { daemon, query, session } = await rig();
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go", promptOrigin: PromptOrigin.USER_SENT }));
    await daemon.next(AckSchema);
    await until(() => session.turnCount() === 1);
    // Act: the stop is sent over the socket, but the result — emitted
    // synchronously into the SDK stream — closes the turn before it lands.
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1" }));
    query.emit({
      type: "result",
      uuid: "r1",
      session_id: "vendor-uuid",
      subtype: "success",
    } as unknown as SdkMessageLike);
    const ack = await daemon.next(AckSchema);
    // Assert
    expect(ack.interruptOutcome).toBe(InterruptOutcome.ALREADY_COMPLETE);
  });

  it("still cancels blocked permission waits while reporting the outcome", async () => {
    // Arrange: a tool blocked on a permission decision.
    const { daemon, query, session } = await rig();
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go", promptOrigin: PromptOrigin.USER_SENT }));
    await daemon.next(AckSchema);
    await until(() => session.turnCount() === 1);
    const ac = new AbortController();
    const decision: Promise<PermissionResultLike> =
      query.canUseTool("Bash", { command: "ls" }, { signal: ac.signal });
    await daemon.next(PermissionRequestSchema);
    // Act
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1" }));
    const ack = await daemon.next(AckSchema);
    // Assert: the outcome rides along without disturbing the cancel path.
    expect(ack.interruptOutcome).toBe(InterruptOutcome.INTERRUPTED);
    await expect(decision).resolves.toMatchObject({ behavior: "deny" });
  });

  it("leaves the still-queued receipt reporting untouched", async () => {
    // Arrange: survivors AND a live turn, so both paths run together.
    const { daemon, query, session } = await rig();
    query.interruptReceipt = { still_queued: ["u-1"] };
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go", promptOrigin: PromptOrigin.USER_SENT }));
    await daemon.next(AckSchema);
    await until(() => session.turnCount() === 1);
    // Act
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1" }));
    const ack = await daemon.next(AckSchema);
    // Assert: the outcome is on the ack, and the survivors still reach the
    // daemon on their own DegradedState channel.
    expect(ack.interruptOutcome).toBe(InterruptOutcome.INTERRUPTED);
    const evt = await daemon.next(EventSchema);
    if (evt.payload.case !== "degradedState") throw new Error("case");
    expect(evt.payload.value.reason).toContain("still_queued=[u-1]");
  });
});

describe("UdsSession permission-mode overrides", () => {
  it("a launch-only mode is refused loudly instead of silently doing nothing", async () => {
    // Arrange: bypassPermissions is valid at launch but the CLI refuses to
    // switch INTO it, so the shim must not pretend it applied.
    const { daemon } = await rig();
    // Act
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, {
      requestId: "p1", text: "go", promptOrigin: PromptOrigin.USER_SENT, permissionMode: "bypassPermissions",
    }));
    await daemon.next(AckSchema);
    // Assert
    const evt = await daemon.next(EventSchema);
    if (evt.payload.case !== "degradedState") throw new Error("case");
    expect(evt.payload.value.component).toBe("claude-shim-permission-mode");
    expect(evt.payload.value.reason).toContain("bypassPermissions");
  });

  it("a mode the CLI rejects reaches the user as DegradedState", async () => {
    // Arrange: the SDK rejects the switch (e.g. an unknown mode reaching a
    // newer CLI). The user picked it, so they must be told it did not take.
    const { query, daemon } = await rig();
    query.setPermissionModeError = new Error("Cannot set permission mode");
    // Act
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, {
      requestId: "p1", text: "go", promptOrigin: PromptOrigin.USER_SENT, permissionMode: "plan",
    }));
    await daemon.next(AckSchema);
    // Assert
    const evt = await daemon.next(EventSchema);
    if (evt.payload.case !== "degradedState") throw new Error("case");
    expect(evt.payload.value.reason).toContain("still in the previous mode");
  });

  it("an accepted mode reports no degradation", async () => {
    // Arrange
    const { query, store, daemon } = await rig();
    // Act
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, {
      requestId: "p1", text: "go", promptOrigin: PromptOrigin.USER_SENT, permissionMode: "plan",
    }));
    await daemon.next(AckSchema);
    await until(() => query.permissionModes.length === 1);
    // Assert: the first Event is the store outage provoked afterwards, so the
    // accepted switch emitted none of its own.
    store.close();
    const evt = await daemon.next(EventSchema);
    if (evt.payload.case !== "degradedState") throw new Error("case");
    expect(evt.payload.value.component).toBe("shim-store-client");
  });
});

// ---------------------------------------------------------------------------
// The three-valued interrupt outcome, decided against the REAL turn counter.
// ---------------------------------------------------------------------------

describe("UdsSession interrupt outcome", () => {
  it("reports INTERRUPTED when a live turn was signalled", async () => {
    // Arrange: a turn is genuinely in flight.
    const { daemon, session } = await rig();
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go", promptOrigin: PromptOrigin.USER_SENT }));
    await daemon.next(AckSchema);
    await until(() => session.turnCount() === 1);
    // Act
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1" }));
    const ack = await daemon.next(AckSchema);
    // Assert
    expect(ack.interruptOutcome).toBe(InterruptOutcome.INTERRUPTED);
  });

  it("reports ALREADY_COMPLETE when no turn was in flight", async () => {
    // Arrange: nothing has ever been submitted.
    const { daemon, session } = await rig();
    expect(session.turnCount()).toBe(0);
    // Act
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1" }));
    const ack = await daemon.next(AckSchema);
    // Assert — a SUCCESS: the user asked for the turn to be over and it is.
    // The old two-valued reading painted precisely this as a failed stop.
    expect(ack.interruptOutcome).toBe(InterruptOutcome.ALREADY_COMPLETE);
  });

  it("reports ALREADY_COMPLETE once the turn has ended", async () => {
    // Arrange: submit, then let the turn's result close it.
    const { daemon, query, session } = await rig();
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go", promptOrigin: PromptOrigin.USER_SENT }));
    await daemon.next(AckSchema);
    await until(() => session.turnCount() === 1);
    query.emit({
      type: "result",
      uuid: "r1",
      session_id: "vendor-uuid",
      subtype: "success",
    } as unknown as SdkMessageLike);
    await until(() => session.turnCount() === 0);
    // Act
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1" }));
    const ack = await daemon.next(AckSchema);
    // Assert
    expect(ack.interruptOutcome).toBe(InterruptOutcome.ALREADY_COMPLETE);
  });

  // THE RACE PIN. The liveness read happens synchronously, before any await
  // and before any promise is created, so a turn that ends AFTER the stop
  // landed cannot retroactively rewrite the verdict. Deciding this
  // downstream is what made it ambiguous: an observer watching for the
  // turn's `aborted` result sees the two as unordered events.
  it("freezes the outcome at the instant the stop landed", async () => {
    // Arrange: a live turn, interrupted while it is genuinely running.
    const { daemon, query, session } = await rig();
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go", promptOrigin: PromptOrigin.USER_SENT }));
    await daemon.next(AckSchema);
    await until(() => session.turnCount() === 1);
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1" }));
    const ack = await daemon.next(AckSchema);
    // Act: the turn ends AFTERWARDS.
    query.emit({
      type: "result",
      uuid: "r1",
      session_id: "vendor-uuid",
      subtype: "success",
    } as unknown as SdkMessageLike);
    await until(() => session.turnCount() === 0);
    // Assert: the verdict already written stays INTERRUPTED. It is a
    // statement about the turn AT THE MOMENT THE STOP LANDED, and nothing
    // later may rewrite it — which is the whole reason it is decided inside
    // the handler rather than derived from what the result turns out to say.
    expect(ack.interruptOutcome).toBe(InterruptOutcome.INTERRUPTED);
  });

  // The counterpart, and the one my first draft of the pin got wrong: when
  // the turn really did end before the stop arrived, ALREADY_COMPLETE is the
  // TRUTHFUL answer, not a missed interrupt. The outcome reports what was
  // true at delivery, and delivery is when the frame lands — not when the
  // caller decided to send it.
  it("reports ALREADY_COMPLETE when the turn ended in transit", async () => {
    // Arrange: a live turn.
    const { daemon, query, session } = await rig();
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go", promptOrigin: PromptOrigin.USER_SENT }));
    await daemon.next(AckSchema);
    await until(() => session.turnCount() === 1);
    // Act: the stop is sent over the socket, but the result — emitted
    // synchronously into the SDK stream — closes the turn before it lands.
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1" }));
    query.emit({
      type: "result",
      uuid: "r1",
      session_id: "vendor-uuid",
      subtype: "success",
    } as unknown as SdkMessageLike);
    const ack = await daemon.next(AckSchema);
    // Assert
    expect(ack.interruptOutcome).toBe(InterruptOutcome.ALREADY_COMPLETE);
  });

  it("still cancels blocked permission waits while reporting the outcome", async () => {
    // Arrange: a tool blocked on a permission decision.
    const { daemon, query, session } = await rig();
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go", promptOrigin: PromptOrigin.USER_SENT }));
    await daemon.next(AckSchema);
    await until(() => session.turnCount() === 1);
    const ac = new AbortController();
    const decision: Promise<PermissionResultLike> =
      query.canUseTool("Bash", { command: "ls" }, { signal: ac.signal });
    await daemon.next(PermissionRequestSchema);
    // Act
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1" }));
    const ack = await daemon.next(AckSchema);
    // Assert: the outcome rides along without disturbing the cancel path.
    expect(ack.interruptOutcome).toBe(InterruptOutcome.INTERRUPTED);
    await expect(decision).resolves.toMatchObject({ behavior: "deny" });
  });

  it("leaves the still-queued receipt reporting untouched", async () => {
    // Arrange: survivors AND a live turn, so both paths run together.
    const { daemon, query, session } = await rig();
    query.interruptReceipt = { still_queued: ["u-1"] };
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go", promptOrigin: PromptOrigin.USER_SENT }));
    await daemon.next(AckSchema);
    await until(() => session.turnCount() === 1);
    // Act
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1" }));
    const ack = await daemon.next(AckSchema);
    // Assert: the outcome is on the ack, and the survivors still reach the
    // daemon on their own DegradedState channel.
    expect(ack.interruptOutcome).toBe(InterruptOutcome.INTERRUPTED);
    const evt = await daemon.next(EventSchema);
    if (evt.payload.case !== "degradedState") throw new Error("case");
    expect(evt.payload.value.reason).toContain("still_queued=[u-1]");
  });
});

describe("UdsSession store subscription key", () => {
  it("rejects a resumed query's first replacement identity before rekeying the store", async () => {
    // Arrange: the exact resume requested uuid-old and no SDK message has yet
    // confirmed that the backend continued that conversation.
    const { daemon, done, query, store } = await rig({
      sessionSource: SessionSource.RESUME,
      storeSessionId: "uuid-old",
      queryInstanceId: "query-resume-mismatch",
    });
    const log = captureLog();

    // Act: the first authoritative SDK identity contradicts the resume target.
    query.emit({
      type: "system",
      subtype: "init",
      session_id: "uuid-replacement",
      uuid: "i1",
      model: "claude-opus-4-1",
      cwd: "/tmp",
    } as unknown as SdkMessageLike);
    const persisted = await store.peer().next(StoreWriteSchema);

    // Assert: the mismatch terminates under the requested key. No connection
    // or write adopts the replacement conversation.
    expect(persisted.batch!.events.map((event) => event.payload.case)).toEqual(["queryLifecycle", "degradedState"]);
    expect(persisted.batch!.events.every((event) => event.sessionId === "uuid-old")).toBe(true);
    expect(store.count()).toBe(2);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 2n, lastSeq: 4n }));
    const deliveredLifecycle = create(EventSchema, { ...persisted.batch!.events[0]!, seq: 4n });
    store.latest().send(EventSchema, deliveredLifecycle);
    expect(await daemon.next(EventSchema)).toEqual(deliveredLifecycle);
    await expect(done).rejects.toMatchObject({
      name: "UnexpectedSdkStreamTerminationError",
      terminationKind: "iterator_throw",
      cause: expect.objectContaining({
        name: "ResumeIdentityMismatchError",
        requestedVendorSessionId: "uuid-old",
        observedVendorSessionId: "uuid-replacement",
      }),
    });
    expect(new ResumeIdentityMismatchError("old", "new").message).toContain("instead of requested session");
    expect(log.record("refusing replacement conversation")).toMatchObject({
      operation: "shim.uds-session.resume-identity-confirmation",
      context: {
        outcome: "fatal_identity_mismatch",
        query_instance_id: "query-resume-mismatch",
        requested_vendor_session_id: "uuid-old",
        observed_vendor_session_id: "uuid-replacement",
      },
    });
  });

  it("re-subscribes under the vendor session id the SDK reports", async () => {
    // Arrange: the shim is `sess-1`, but the SDK reports the conversation's
    // own uuid — which is the id the store files these events under (and the
    // id the sidecar keys the same conversation by, from its transcript
    // filename). Subscribing under `sess-1` listens to nothing.
    const { query, store } = await rig();
    const initialConnections = store.count();

    // Act: a persistent message carrying the vendor uuid.
    query.emit({
      type: "assistant",
      uuid: "u1",
      session_id: "96a0baaf-652a-4bb1-9450-e8292c595d33",
      message: { id: "m1", model: "claude", content: [{ type: "text", text: "hi" }], stop_reason: "end_turn", usage: {} },
    } as unknown as SdkMessageLike);

    // Assert: the subscription is reopened under the vendor uuid.
    await until(() => store.count() > initialConnections, "resubscribed under the vendor uuid");
    const resub = await store.latest().next(SubscribeSchema);
    expect(resub.sessionId).toBe("96a0baaf-652a-4bb1-9450-e8292c595d33");
  });
});

describe("UdsSession bounded historical replay (core.proto ReplayRequest)", () => {
  it("serves a ReplayRequest as tagged ReplayEvents", async () => {
    // Arrange: a daemon whose frontend needs history the daemon no longer holds.
    const { store, daemon } = await rig({ storeSessionId: "vendor-uuid", replayIdleMs: 200 });
    // Act
    daemon.send(ReplayRequestSchema, create(ReplayRequestSchema, { requestId: "r1", fromSeq: 0n, toSeq: 3n }));
    await until(() => store.count() >= 3);
    const replayConn = store.latest();
    await replayConn.next(SubscribeSchema);
    replayConn.send(EventSchema, create(EventSchema, { sessionId: "vendor-uuid", seq: 1n }));
    const replayed = await daemon.next(ReplayEventSchema);
    // Assert
    expect(replayed.requestId).toBe("r1");
    expect(replayed.event?.seq).toBe(1n);
  });

  it("forwards a replayed persistent file-plane diagnostic field-faithfully without conversation conversion", async () => {
    // Arrange
    const { store, daemon } = await rig({ storeSessionId: "vendor-uuid", replayIdleMs: 200 });
    const diagnostic = create(FilePlaneDiagnosticSchema, {
      sourceRuntime: DiagnosticSourceRuntime.SIDECAR,
      level: "warn",
      verbosity: "verbose",
      operation: "sidecar.replay.read",
      message: "replayed diagnostic",
      context: { cursor: "42", retained: true },
      sourcePid: 9988n,
      sourcePath: "/tmp/sidecar-replay.sock",
    });
    const persisted = create(EventSchema, {
      sessionId: "vendor-uuid",
      seq: 1n,
      producedAtMs: 1720000001000n,
      requestId: "request-replay-1",
      payload: { case: "filePlaneDiagnostic", value: diagnostic },
    });

    // Act
    daemon.send(ReplayRequestSchema, create(ReplayRequestSchema, { requestId: "r-diagnostic", fromSeq: 0n, toSeq: 3n }));
    await until(() => store.count() >= 3);
    const replayConn = store.latest();
    await replayConn.next(SubscribeSchema);
    replayConn.send(EventSchema, persisted);
    const replayed = await daemon.next(ReplayEventSchema);

    // Assert
    expect(replayed.requestId).toBe("r-diagnostic");
    expect(replayed.event).toEqual(persisted);
    expect(replayed.event?.payload.case).toBe("filePlaneDiagnostic");
    expect(replayed.event?.payload.case === "filePlaneDiagnostic" && replayed.event.payload.value).toEqual(diagnostic);
  });

  it("closes a served replay with exactly one ReplayDone", async () => {
    // Arrange
    const { store, daemon } = await rig({ storeSessionId: "vendor-uuid", replayIdleMs: 200 });
    // Act
    daemon.send(ReplayRequestSchema, create(ReplayRequestSchema, { requestId: "r1", fromSeq: 0n, toSeq: 3n }));
    await until(() => store.count() >= 3);
    const replayConn = store.latest();
    await replayConn.next(SubscribeSchema);
    replayConn.send(EventSchema, create(EventSchema, { sessionId: "vendor-uuid", seq: 3n }));
    const done = await daemon.next(ReplayDoneSchema);
    // Assert
    expect(done.requestId).toBe("r1");
    expect(done.truncated).toBe(false);
  });

  it("reports a truncated replay honestly", async () => {
    // Arrange: the store never reaches to_seq, so the range is never closed.
    const { store, daemon } = await rig({ storeSessionId: "vendor-uuid", replayIdleMs: 60 });
    // Act
    daemon.send(ReplayRequestSchema, create(ReplayRequestSchema, { requestId: "r1", fromSeq: 0n, toSeq: 999n }));
    await until(() => store.count() >= 3);
    await store.latest().next(SubscribeSchema);
    const done = await daemon.next(ReplayDoneSchema);
    // Assert
    expect(done.truncated).toBe(true);
    expect(done.reason).not.toBe("");
  });

  it("serves a replay WITHOUT re-subscribing the standing tail", async () => {
    // Arrange: the standing subscription's position belongs to the daemon;
    // moving it would drag the daemon's own consumption backwards.
    const { store, daemon } = await rig({ storeSessionId: "vendor-uuid", replayIdleMs: 200 });
    const standing = store.latest();
    const standingFrames: unknown[] = [];
    standing.socket.on("data", (chunk) => standingFrames.push(chunk));
    // Act
    daemon.send(ReplayRequestSchema, create(ReplayRequestSchema, { requestId: "r1", fromSeq: 0n, toSeq: 3n }));
    await until(() => store.count() >= 3);
    const replayConn = store.latest();
    await replayConn.next(SubscribeSchema);
    replayConn.send(EventSchema, create(EventSchema, { sessionId: "vendor-uuid", seq: 3n }));
    await daemon.next(ReplayDoneSchema);
    // Assert
    expect(standingFrames).toEqual([]);
  });
});

// ---------------------------------------------------------------------------
// THE BRING-UP GATE. Its whole value is that the ack is the LAST step, so the
// two cases worth pinning are the one where the wiring cannot be done at all,
// and the one where it has to be done again.
// ---------------------------------------------------------------------------

describe("UdsSession bring-up gate", () => {
  it("withholds the ack, loudly, when the store subscription cannot be opened", async () => {
    // Arrange: the producer link is up (start() needed it), and then the store
    // goes away — so the gate's subscribe dial is the thing that fails.
    const store = await fakeStore();
    cleanups.push(() => store.close());
    let query!: FakeQuery;
    const udsSocketPath = tmpSocketPath();
    const daemonListener = acceptShim(udsSocketPath);
    cleanups.push(() => daemonListener.close());
    const session = new UdsSession({
      sessionId: "sess-1",
      shimVersion: "9.9",
      protocolVersion: "1",
      udsSocketPath,
      storeSocketPath: store.socketPath,
      sessionSource: SessionSource.FRESH,
      createQuery: (prompt, canUseTool): UdsQuery => {
        const live = new FakeQuery(prompt, canUseTool);
        query = live;
        return { query: live, subscriptionUsage: () => live.subscriptionUsage(), abort: () => live.abort() };
      },
      heartbeatIntervalMs: 0,
    });
    const done = session.start();
    cleanups.push(async () => {
      await shutdownFixture(session, query, done);
    });
    await acknowledgeInitialQueryLifecycle(store);
    const daemon = await daemonListener.next();
    await daemon.next(ShimHelloSchema);
    const log = captureLog();
    store.close();

    // Act: the daemon completes its half of the gate.
    daemon.send(DaemonHelloSchema, create(DaemonHelloSchema, { daemonVersion: "d1", protocolVersion: "1" }));
    await until(() => log.count("bring-up gate REFUSED") === 1, "the refusal was logged");

    // Assert: no ack, ever — the daemon's readiness wait must fail on its own
    // deadline rather than be told this session is usable.
    expect(daemon.count(ShimReadySchema)).toBe(0);
  });

  it("re-runs the whole gate on the rotation bounce", async () => {
    // Arrange: a resumed session with one merged event forwarded, so the next
    // vendor uuid is a ROTATION rather than a first adoption.
    const { query, store, daemon, daemonListener } = await rig({
      sessionSource: SessionSource.RESUME,
      storeSessionId: "uuid-old",
    });
    store.latest().send(EventSchema, create(EventSchema, { sessionId: "uuid-old", seq: 7n }));
    await daemon.next(EventSchema);
    query.emit({
      type: "system",
      subtype: "init",
      session_id: "uuid-old",
      uuid: "i1",
      model: "claude",
      cwd: "/tmp",
    } as unknown as SdkMessageLike);
    const initialIdentityBatch = await store.peer().next(StoreWriteSchema);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, {
      accepted: BigInt(initialIdentityBatch.batch!.events.length),
      lastSeq: 8n,
    }));

    // Act: the vendor mints a new transcript identity mid-stream.
    query.emit({
      type: "assistant",
      uuid: "u1",
      session_id: "uuid-new",
      message: { id: "m1", model: "claude", content: [{ type: "text", text: "hi" }], stop_reason: "end_turn", usage: {} },
    } as unknown as SdkMessageLike);

    // Assert: the bounce opens a new connection that runs the gate in full —
    // hello announcing the rotated uuid, the daemon's reset from_seq, the
    // store subscription in the NEW seq space, and the ack.
    const daemon2 = await daemonListener.next();
    cleanups.push(() => daemon2.destroy());
    const hello2 = await daemon2.next(ShimHelloSchema);
    expect(hello2.vendorSessionId).toBe("uuid-new");
    expect(hello2.queryCreatedSeq).toBe(0n);
    daemon2.send(DaemonHelloSchema, create(DaemonHelloSchema, { daemonVersion: "d2", protocolVersion: "1", fromSeq: 0n }));
    await until(() => store.count() >= 3, "the new seq space was subscribed");
    const resub = await store.latest().next(SubscribeSchema);
    expect(resub.sessionId).toBe("uuid-new");
    expect(resub.fromSeq).toBe(0n);
    expect((await daemon2.next(ShimReadySchema)).vendorSessionId).toBe("uuid-new");
  });
});

// ---------------------------------------------------------------------------
// HANDSHAKE-BORNE PERMISSION MODE (core.proto DaemonHello.permission_mode).
// The daemon's session record decides the posture; argv is only a spawn-time
// snapshot. These pin precedence, the refusal, and the readiness report.
// ---------------------------------------------------------------------------

describe("UdsSession handshake permission mode", () => {
  it("applies the mode the DaemonHello carried to the SDK query", async () => {
    // Arrange + Act: the gate runs with a handshake mode differing from argv.
    const { query } = await rig({ permissionMode: "default", handshakeMode: "auto" });
    // Assert
    expect(query.permissionModes).toEqual(["auto"]);
  });

  it("logs the override when the handshake mode differs from argv", async () => {
    // Arrange
    const log = captureLog();
    // Act
    await rig({ permissionMode: "default", handshakeMode: "auto" });
    // Assert
    expect(log.record("handshake permission mode OVERRIDES")).toMatchObject({
      operation: "shim.uds-session.lifecycle",
      context: { permission_mode: "auto", argv_permission_mode: "default" },
    });
  });

  it("applies the mode BEFORE the readiness ack, so no turn can run in the old one", async () => {
    // Arrange + Act
    const { query, ready } = await rig({ permissionMode: "default", handshakeMode: "plan" });
    // Assert: the ack exists, and the switch had already happened when it was
    // written (rig drains the ack, so a mode applied after it would be absent).
    expect(ready.sessionId).toBe("sess-1");
    expect(query.permissionModes).toEqual(["plan"]);
  });

  it("falls back to argv when the field is absent (a daemon predating it)", async () => {
    // Arrange + Act: no handshakeMode at all — the rollout-compatibility case.
    const { query } = await rig({ permissionMode: "acceptEdits" });
    // Assert: nothing switched; the query stays as argv constructed it.
    expect(query.permissionModes).toEqual([]);
  });

  it("asserts the mode on the query even when the handshake echoes argv", async () => {
    // Arrange + Act
    const { query } = await rig({ permissionMode: "auto", handshakeMode: "auto" });
    // Assert: the gate STATES the posture rather than trusting that argv took
    // — the offline fake query ignores argv entirely, and "the flag must have
    // applied" is the assumption this whole change exists to stop making.
    expect(query.permissionModes).toEqual(["auto"]);
  });

  it("accepts a launch-only mode that argv already launched the session in", async () => {
    // Arrange + Act: bypassPermissions cannot be switched INTO, so matching
    // argv is the only way a session can legitimately be in it.
    const { query, ready } = await rig({
      permissionMode: "bypassPermissions",
      handshakeMode: "bypassPermissions",
    });
    // Assert: acked, and no doomed switch attempted.
    expect(ready.sessionId).toBe("sess-1");
    expect(query.permissionModes).toEqual([]);
  });

  it("refuses a launch-only mode the session was NOT launched in", async () => {
    // Arrange
    const log = captureLog();
    const store = await fakeStore();
    cleanups.push(() => store.close());
    let query!: FakeQuery;
    const udsSocketPath = tmpSocketPath();
    const daemonListener = acceptShim(udsSocketPath);
    cleanups.push(() => daemonListener.close());
    const session = new UdsSession({
      sessionId: "sess-1",
      shimVersion: "9.9",
      protocolVersion: "1",
      udsSocketPath,
      storeSocketPath: store.socketPath,
      sessionSource: SessionSource.FRESH,
      permissionMode: "default",
      createQuery: (prompt, canUseTool): UdsQuery => {
        const live = new FakeQuery(prompt, canUseTool);
        query = live;
        return { query: live, subscriptionUsage: () => live.subscriptionUsage(), abort: () => live.abort() };
      },
      heartbeatIntervalMs: 0,
    });
    const done = session.start();
    cleanups.push(async () => {
      await shutdownFixture(session, query, done);
    });
    await acknowledgeInitialQueryLifecycle(store);
    const daemon = await daemonListener.next();
    await daemon.next(ShimHelloSchema);

    // Act
    daemon.send(DaemonHelloSchema, create(DaemonHelloSchema, {
      daemonVersion: "d1", protocolVersion: "1", permissionMode: "bypassPermissions",
    }));
    await until(() => log.count("bring-up gate REFUSED") === 1, "the refusal was logged");

    // Assert: no ack, and no doomed switch attempted either.
    expect(daemon.count(ShimReadySchema)).toBe(0);
    expect(query.permissionModes).toEqual([]);
  });

  it("reports the EFFECTIVE mode on the readiness announcement", async () => {
    // Arrange
    const log = captureLog();
    // Act
    await rig({ permissionMode: "default", handshakeMode: "auto" });
    // Assert
    expect(log.record("readiness asserted")).toMatchObject({
      operation: "shim.uds-session.lifecycle",
      context: { permission_mode: "auto" },
    });
  });

  it("refuses loudly and withholds the ack for a mode outside the vocabulary", async () => {
    // Arrange: a full session whose store is healthy, so the ONLY thing that
    // can refuse the gate is the mode.
    const store = await fakeStore();
    cleanups.push(() => store.close());
    let query!: FakeQuery;
    const udsSocketPath = tmpSocketPath();
    const daemonListener = acceptShim(udsSocketPath);
    cleanups.push(() => daemonListener.close());
    const session = new UdsSession({
      sessionId: "sess-1",
      shimVersion: "9.9",
      protocolVersion: "1",
      udsSocketPath,
      storeSocketPath: store.socketPath,
      sessionSource: SessionSource.FRESH,
      permissionMode: "default",
      createQuery: (prompt, canUseTool): UdsQuery => {
        const live = new FakeQuery(prompt, canUseTool);
        query = live;
        return { query: live, subscriptionUsage: () => live.subscriptionUsage(), abort: () => live.abort() };
      },
      heartbeatIntervalMs: 0,
    });
    const done = session.start();
    cleanups.push(async () => {
      await shutdownFixture(session, query, done);
    });
    await acknowledgeInitialQueryLifecycle(store);
    const daemon = await daemonListener.next();
    await daemon.next(ShimHelloSchema);
    const log = captureLog();

    // Act
    daemon.send(DaemonHelloSchema, create(DaemonHelloSchema, {
      daemonVersion: "d1", protocolVersion: "1", permissionMode: "yolo",
    }));
    await until(() => log.count("bring-up gate REFUSED") === 1, "the refusal was logged");

    // Assert: no ack, and no silent fallback to "default" either.
    expect(daemon.count(ShimReadySchema)).toBe(0);
    expect(query.permissionModes).toEqual([]);
  });
});

describe("UdsSession rewind lineage: SessionRewound", () => {
  const LINEAGE = {
    previousVendorSessionId: "old-vendor",
    retainedLeafUuid: "leaf-uuid",
    droppedTurnIds: ["ka-1", "ka-2"],
  };

  /**
   * Stand up a session resumed onto a truncated transcript and hand back the
   * store with NOTHING drained: the rewind is the first persistent event in the
   * new seq space, ahead of QueryCreated, so the first store write is what each
   * test is here to inspect. Deliberately stops short of the daemon handshake:
   * the rewind lands before the daemon connection exists.
   */
  async function rewindRig(
    opts: { rewindLineage?: typeof LINEAGE; storeSessionId?: string } = {},
  ): Promise<{ store: FakeStore; session: UdsSession; done: Promise<void> }> {
    const store = await fakeStore();
    cleanups.push(() => store.close());
    const session = new UdsSession({
      sessionId: "sess-rewound",
      shimVersion: "9.9",
      protocolVersion: "1",
      // No listener on this path: start() parks on the daemon dial, which keeps
      // the test focused on the store writes that precede it.
      udsSocketPath: tmpSocketPath(),
      storeSocketPath: store.socketPath,
      sessionSource: SessionSource.RESUME,
      storeSessionId: opts.storeSessionId ?? "new-vendor",
      queryInstanceId: "rewound-query",
      ...(opts.rewindLineage !== undefined ? { rewindLineage: opts.rewindLineage } : {}),
      createQuery: (prompt, canUseTool): UdsQuery => {
        const live = new FakeQuery(prompt, canUseTool);
        return { query: live, subscriptionUsage: () => live.subscriptionUsage(), abort: () => live.abort() };
      },
      heartbeatIntervalMs: 0,
    });
    const done = session.start();
    void done.catch(() => {});
    // The store connection is only observable once bring-up has produced its
    // first write; peer() is unavailable before that.
    await until(() => store.count() >= 1);
    return { store, session, done };
  }

  it("emits SessionRewound as the first persistent event, ahead of QueryCreated", async () => {
    // Arrange
    const { store } = await rewindRig({ rewindLineage: LINEAGE });
    // Act
    const write = await store.peer().next(StoreWriteSchema);
    // Assert — the explanation must precede the query it explains.
    expect(write.batch!.events.map((e) => e.payload.case)).toEqual(["sessionRewound"]);
  });

  it("emits QueryCreated immediately after the rewind that explains it", async () => {
    // Arrange
    const { store } = await rewindRig({ rewindLineage: LINEAGE });
    await store.peer().next(StoreWriteSchema);
    // Act
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 1n, deduped: 0n, lastSeq: 1n }));
    const write = await store.peer().next(StoreWriteSchema);
    // Assert
    expect(write.batch!.events.map((e) => e.payload.case)).toEqual(["queryLifecycle"]);
  });

  it("names both sides of the rewind and the retained leaf", async () => {
    // Arrange
    const { store } = await rewindRig({ rewindLineage: LINEAGE });
    // Act
    const write = await store.peer().next(StoreWriteSchema);
    const event = write.batch!.events[0]!;
    if (event.payload.case !== "sessionRewound") throw new Error("case");
    // Assert
    expect(event.payload.value.previousVendorSessionId).toBe("old-vendor");
    expect(event.payload.value.newVendorSessionId).toBe("new-vendor");
    expect(event.payload.value.retainedLeafUuid).toBe("leaf-uuid");
  });

  it("carries the dropped keep-alive turn ids in submission order", async () => {
    // Arrange
    const { store } = await rewindRig({ rewindLineage: LINEAGE });
    // Act
    const write = await store.peer().next(StoreWriteSchema);
    const event = write.batch!.events[0]!;
    if (event.payload.case !== "sessionRewound") throw new Error("case");
    if (event.payload.value.reason.case !== "keepAliveDiscard") throw new Error("reason");
    // Assert
    expect(event.payload.value.reason.value.droppedTurnIds).toEqual(["ka-1", "ka-2"]);
  });

  it("files the rewind into the NEW vendor session's seq space", async () => {
    // Arrange
    const { store } = await rewindRig({ rewindLineage: LINEAGE });
    // Act
    const write = await store.peer().next(StoreWriteSchema);
    // Assert — the retired seq space must never receive it.
    expect(write.batch!.events[0]!.sessionId).toBe("new-vendor");
  });

  it("writes the rewind as a PERSISTENT stream-plane event", async () => {
    // Arrange
    const { store } = await rewindRig({ rewindLineage: LINEAGE });
    // Act
    const event = (await store.peer().next(StoreWriteSchema)).batch!.events[0]!;
    // Assert
    expect(event.plane).toBe(Plane.STREAM);
    expect(event.class).toBe(EventClass.PERSISTENT);
  });

  it("stamps a dedup key keyed on the retired vendor session id", async () => {
    // Arrange
    const { store } = await rewindRig({ rewindLineage: LINEAGE });
    // Act
    const event = (await store.peer().next(StoreWriteSchema)).batch!.events[0]!;
    // Assert — a shim restart re-emits this exact key and the store collapses it.
    expect(event.dedupKey).toBe(sessionRewoundDedupKey("old-vendor"));
    expect(event.dedupKey).toBe("rewind:old-vendor");
  });

  it("treats a single-dedup receipt as success on a shim restart", async () => {
    // Arrange: the store has already persisted this exact lineage.
    const { store, done } = await rewindRig({ rewindLineage: LINEAGE });
    await store.peer().next(StoreWriteSchema);
    let failed = false;
    void done.catch(() => { failed = true; });
    // Act: a duplicate consumes no seq, so last_seq is 0.
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 0n, deduped: 1n, lastSeq: 0n }));
    // Assert: bring-up continued into QueryCreated — no terminal write, no rejection.
    const next = await store.peer().next(StoreWriteSchema);
    expect(next.batch!.events[0]!.payload.case).toBe("queryLifecycle");
    if (next.batch!.events[0]!.payload.case !== "queryLifecycle") throw new Error("case");
    expect(next.batch!.events[0]!.payload.value.event.case).toBe("created");
    expect(failed).toBe(false);
  });

  it("fails bring-up when the rewind receipt accepts nothing at all", async () => {
    // Arrange
    const { store, done } = await rewindRig({ rewindLineage: LINEAGE });
    await store.peer().next(StoreWriteSchema);
    // Act: neither an acceptance nor a dedup.
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 0n, deduped: 0n, lastSeq: 0n }));
    // Assert
    const terminal = await store.peer().next(StoreWriteSchema);
    expect(terminal.batch!.events[0]!.payload.case).toBe("queryLifecycle");
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 2n, lastSeq: 3n }));
    await expect(done).rejects.toThrow(/SessionRewound persistence returned an invalid receipt/);
  });

  it("fails bring-up when the rewind is accepted without an assigned seq", async () => {
    // Arrange
    const { store, done } = await rewindRig({ rewindLineage: LINEAGE });
    await store.peer().next(StoreWriteSchema);
    // Act: an acceptance always assigns a seq, so seq 0 is a broken receipt.
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 1n, deduped: 0n, lastSeq: 0n }));
    // Assert
    const terminal = await store.peer().next(StoreWriteSchema);
    expect(terminal.batch!.events[0]!.payload.case).toBe("queryLifecycle");
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 2n, lastSeq: 3n }));
    await expect(done).rejects.toThrow(/SessionRewound persistence returned an invalid receipt/);
  });

  it("logs the invalid rewind receipt once with its causal context", async () => {
    // Arrange
    const log = captureLog();
    const { store, done } = await rewindRig({ rewindLineage: LINEAGE });
    await store.peer().next(StoreWriteSchema);
    // Act: a batch of one cannot have accepted two.
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 2n, deduped: 0n, lastSeq: 4n }));
    await store.peer().next(StoreWriteSchema);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 2n, lastSeq: 5n }));
    await expect(done).rejects.toThrow(/invalid receipt/);
    // Assert
    const record = log.record("fatal_invalid_session_rewound_receipt");
    expect(record.operation).toBe("shim.uds-session.session-rewound");
    expect(record.context).toMatchObject({
      outcome: "fatal_invalid_session_rewound_receipt",
      previous_vendor_session_id: "old-vendor",
      store_key: "new-vendor",
      query_instance_id: "rewound-query",
      accepted: "2",
      deduped: "0",
      last_seq: "4",
    });
    expect(log.count("fatal_invalid_session_rewound_receipt")).toBe(1);
  });

  it("refuses a lineage naming the resumed session as its own predecessor", async () => {
    // Arrange + Act: argv validation catches this first in production; the
    // session refuses it too so no caller can write a self-referential rewind.
    const { store, done } = await rewindRig({
      rewindLineage: { ...LINEAGE, previousVendorSessionId: "new-vendor" },
      storeSessionId: "new-vendor",
    });
    const terminal = await store.peer().next(StoreWriteSchema);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 2n, lastSeq: 3n }));
    // Assert
    expect(terminal.batch!.events[0]!.payload.case).toBe("queryLifecycle");
    await expect(done).rejects.toThrow(/both sides of the rewind/);
  });

  it("writes no SessionRewound for an ordinary resume with no lineage", async () => {
    // Arrange: the same resumed session, without rewind flags.
    const { store } = await rewindRig();
    // Act: the only write is QueryCreated, which no rewind precedes.
    await acknowledgeInitialQueryLifecycle(store);
    await tick();
    await tick();
    // Assert
    expect(store.peer().count(StoreWriteSchema)).toBe(0);
  });
});

describe("UdsSession keep-alive turns: prompt_origin passthrough", () => {
  it("carries PROMPT_ORIGIN_CACHE_KEEP_ALIVE onto TurnStarted unchanged", async () => {
    // Arrange
    const { daemon, store } = await rig({ holdTurnStartAck: true });
    // Act: a keep-alive ping is an ORDINARY SubmitPrompt.
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, {
      requestId: "ka-1",
      text: "respond with only '.', no tool calls or changes",
      promptOrigin: PromptOrigin.CACHE_KEEP_ALIVE,
    }));
    const write = await store.peer().next(StoreWriteSchema);
    const started = write.batch!.events[0]!;
    if (started.payload.case !== "turnStarted") throw new Error("case");
    // Assert: the durable copy is the submitted origin, with no remapping.
    expect(started.payload.value.promptOrigin).toBe(PromptOrigin.CACHE_KEEP_ALIVE);
    expect(started.payload.value.turnId).toBe("ka-1");
    // Release the held boundary so the fixture can tear the session down.
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 2n, lastSeq: 2n }));
    expect((await daemon.next(AckSchema)).requestId).toBe("ka-1");
  });

  it("accepts a keep-alive prompt into the SDK like any other turn", async () => {
    // Arrange
    const { daemon, query } = await rig();
    // Act
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, {
      requestId: "ka-2",
      text: "respond with only '.'",
      promptOrigin: PromptOrigin.CACHE_KEEP_ALIVE,
    }));
    const ack = await daemon.next(AckSchema);
    await until(() => query.prompts.length === 1);
    // Assert: no special-casing — the ping reaches the vendor as a real prompt.
    expect(ack.requestId).toBe("ka-2");
    expect(query.prompts[0]!.message.content).toEqual([
      { type: "text", text: "respond with only '.'" },
    ]);
  });
});

// ---------------------------------------------------------------------------
// Producer provenance: every envelope the session builds names the query it is
// running, INCLUDING the one written before the subscription is open.
// ---------------------------------------------------------------------------

describe("UdsSession stamps the query it is running", () => {
  it("names the running query on the QueryCreated written before the subscription opens", async () => {
    // Arrange: a session whose first act is persisting its own QueryCreated --
    // written to the store BEFORE the handshake supplies a from_seq, so the
    // store serves it back during catch-up on every later replay.
    const { createdLifecycle, hello } = await rig({ queryInstanceId: "startup-query" });

    // Act: read the envelope the shim actually wrote.
    const event = createdLifecycle.batch!.events[0]!;

    // Assert: it names the LIVE query -- the same id the handshake announces.
    // A consumer therefore classifies it with one comparison and gets "live",
    // which is the whole point: delivery order says nothing about origin.
    expect(event.queryInstanceId).toBe("startup-query");
    expect(event.queryInstanceId).toBe(hello.queryInstanceId);
  });

  it("names the running query on a persisted turn start", async () => {
    // Arrange: a resumed session bound to a known query.
    const { store, daemon } = await rig({ storeSessionId: "vendor-uuid", queryInstanceId: "turn-query" });

    // Act.
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go", promptOrigin: PromptOrigin.USER_SENT }));
    const sw = await store.peer().next(StoreWriteSchema);

    // Assert: the turn boundary carries the same provenance as the lifecycle.
    const event = sw.batch!.events[0]!;
    expect(event.payload.case).toBe("turnStarted");
    expect(event.queryInstanceId).toBe("turn-query");
  });
});
