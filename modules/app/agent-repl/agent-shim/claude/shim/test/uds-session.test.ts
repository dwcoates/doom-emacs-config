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
import { afterEach, describe, expect, it } from "vitest";
import net from "node:net";
import { create } from "@bufbuild/protobuf";
import { AsyncQueue } from "../src/input-queue.js";
import { UdsSession } from "../src/uds/uds-session.js";
import type {
  CanUseToolLike,
  InterruptReceipt,
  PermissionResultLike,
  QueryLike,
  SdkMessageLike,
  SdkUserMessageLike,
} from "../src/session.js";
import type { ModelInfo, SlashCommand } from "../src/protocol.js";
import type { Event } from "../src/uds/proto.js";
import {
  AckSchema,
  DaemonHelloSchema,
  EventSchema,
  InterruptSchema,
  PermissionDecision,
  InterruptOutcome,
  PermissionRequestSchema,
  PermissionResponseSchema,
  SessionSource,
  ReplayDoneSchema,
  ReplayEventSchema,
  ReplayRequestSchema,
  ShimHelloSchema,
  StoreWriteAckSchema,
  StoreWriteSchema,
  SubmitPromptSchema,
  SubscribeSchema,
} from "../src/uds/proto.js";
import { FramedPeer, acceptShim, tmpSocketPath, until } from "./uds-harness.js";

const tick = (): Promise<void> => new Promise<void>((r) => setImmediate(r));

/** A scripted SDK query: records prompts pushed at it, emits messages on cue. */
class FakeQuery implements QueryLike {
  readonly prompts: SdkUserMessageLike[] = [];
  readonly canUseTool: CanUseToolLike;
  interruptCalls = 0;
  /** The receipt `interrupt()` resolves; undefined models a pre-0.3.205 CLI. */
  interruptReceipt: InterruptReceipt | undefined = undefined;
  private readonly outbox = new AsyncQueue<SdkMessageLike>();

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
    const server = net.createServer((s) => accepted.push(new FramedPeer(s)));
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
  query: FakeQuery;
  store: FakeStore;
  daemon: FramedPeer;
  udsSocketPath: string;
  /** Accepts the shim's RECONNECT after a daemon drop. */
  daemonListener: { next: () => Promise<FramedPeer>; close: () => void };
  /**
   * The shim-asserted readiness Event, already drained off `daemon`.
   *
   * The shim announces readiness on the handshake edge, so it is ALWAYS the
   * first Event a handshaked daemon sees. rig() consumes it so every other
   * test still reads its own event as the next one; the readiness tests
   * assert on this value instead.
   */
  readiness: Event;
}

const cleanups: Array<() => void | Promise<void>> = [];
afterEach(async () => {
  for (const c of cleanups.splice(0)) await c();
});

/** Stand up store + session, connect a daemon, and (optionally) subscribe. */
async function rig(
  opts: { subscribe?: boolean; sessionSource?: SessionSource; storeSessionId?: string; replayIdleMs?: number } = {},
): Promise<Rig> {
  const store = await fakeStore();
  cleanups.push(() => store.close());

  let query!: FakeQuery;
  const createQuery = (
    prompt: AsyncIterable<SdkUserMessageLike>,
    canUseTool: CanUseToolLike,
  ): QueryLike => (query = new FakeQuery(prompt, canUseTool));

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
    ...(opts.storeSessionId !== undefined ? { storeSessionId: opts.storeSessionId } : {}),
    createQuery,
    heartbeatIntervalMs: 0,
    ...(opts.replayIdleMs !== undefined ? { replayIdleMs: opts.replayIdleMs } : {}),
    newRequestId: (() => {
      let n = 0;
      return () => `req-${++n}`;
    })(),
  });
  // start() resolves only when the SDK stream ends; kick it off and clean up.
  const done = session.start();
  cleanups.push(async () => {
    await session.shutdown("test-cleanup");
    query.endStream();
    await done.catch(() => {});
  });

  const daemon = await daemonListener.next();
  await daemon.next(ShimHelloSchema);
  daemon.send(DaemonHelloSchema, create(DaemonHelloSchema, { daemonVersion: "d1", protocolVersion: "1" }));
  await until(() => session.isConnected());

  if (opts.subscribe) {
    daemon.send(SubscribeSchema, create(SubscribeSchema, { sessionId: "sess-1", fromSeq: 0n }));
    // The subscription rides its own store connection (single-role store).
    await until(() => store.count() >= 2);
    await store.latest().next(SubscribeSchema);
  }
  // Readiness rides the handshake edge, so it is already in flight by now.
  const readiness = await daemon.next(EventSchema);
  return { session, query, store, daemon, udsSocketPath, daemonListener, readiness };
}

describe("UdsSession control: prompt in → SDK", () => {
  it("pushes a SubmitPrompt into the SDK input queue and Acks", async () => {
    // Arrange
    const { query, daemon, session } = await rig();
    // Act
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "hello there" }));
    const ack = await daemon.next(AckSchema);
    await until(() => query.prompts.length === 1);
    // Assert
    expect(ack.requestId).toBe("p1");
    const content = query.prompts[0]!.message.content;
    expect(content).toEqual([{ type: "text", text: "hello there" }]);
    expect(session.turnCount()).toBe(1);
  });
});

describe("UdsSession lifecycle: shim-authoritative TurnStarted", () => {
  it("writes a TurnStarted to the store when it accepts a SubmitPrompt", async () => {
    // Arrange: a resumed session already knows the vendor session id.
    const { store, daemon } = await rig({ subscribe: true, storeSessionId: "vendor-uuid" });
    // Act
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go" }));
    const sw = await store.peer().next(StoreWriteSchema);
    // Assert
    expect(sw.batch!.events[0]!.payload.case).toBe("turnStarted");
  });

  it("bounds the TurnStarted preview to the prompt's first line", async () => {
    // Arrange
    const { store, daemon } = await rig({ subscribe: true, storeSessionId: "vendor-uuid" });
    // Act
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "line one\nline two" }));
    const sw = await store.peer().next(StoreWriteSchema);
    // Assert
    const payload = sw.batch!.events[0]!.payload;
    if (payload.case !== "turnStarted") throw new Error("case");
    expect(payload.value.promptPreview).toBe("line one");
  });

  it("correlates the TurnStarted to the SubmitPrompt's request id", async () => {
    // Arrange
    const { store, daemon } = await rig({ subscribe: true, storeSessionId: "vendor-uuid" });
    // Act
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go" }));
    const sw = await store.peer().next(StoreWriteSchema);
    // Assert
    expect(sw.batch!.events[0]!.requestId).toBe("p1");
  });

  it("files the TurnStarted under the vendor session id, not the shim's own id", async () => {
    // Arrange: the store's seq space is keyed by the vendor uuid; writing
    // under `sess-1` would land in a space nothing subscribes to.
    const { store, daemon } = await rig({ subscribe: true, storeSessionId: "vendor-uuid" });
    // Act
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go" }));
    const sw = await store.peer().next(StoreWriteSchema);
    // Assert
    expect(sw.batch!.events[0]!.sessionId).toBe("vendor-uuid");
  });

  it("holds a TurnStarted accepted before the vendor session id is known", async () => {
    // Arrange: a FRESH session has no --resume id, so the store key is still
    // the placeholder when the first prompt arrives.
    const { store, daemon } = await rig({ subscribe: true });
    // Act
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go" }));
    await daemon.next(AckSchema);
    await tick();
    // Assert: nothing written yet — it is held, not filed under `sess-1`.
    expect(store.peer().count(StoreWriteSchema)).toBe(0);
  });

  it("flushes a held TurnStarted under the adopted key once the SDK reveals it", async () => {
    // Arrange: a held turn from a fresh session.
    const { query, store, daemon } = await rig({ subscribe: true });
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go" }));
    await daemon.next(AckSchema);
    // Act: the SDK reveals the vendor session id.
    query.emit({
      type: "assistant",
      uuid: "u1",
      session_id: "vendor-uuid",
      message: { id: "m1", model: "claude", content: [{ type: "text", text: "hi" }], stop_reason: "end_turn", usage: {} },
    } as unknown as SdkMessageLike);
    const first = await store.peer().next(StoreWriteSchema);
    // Assert: the held turn is flushed, restamped with the adopted key.
    expect(first.batch!.events[0]!.payload.case).toBe("turnStarted");
    expect(first.batch!.events[0]!.sessionId).toBe("vendor-uuid");
  });

  it("no system:init emits a SessionStarted twin, first or otherwise", async () => {
    // Arrange: readiness is asserted at start() now, so an init's twin would
    // re-announce a session already announced — and, arriving on the FIRST
    // PROMPT, would land after the fact.
    const { query, store } = await rig({ subscribe: true, storeSessionId: "vendor-uuid" });
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
    const { readiness } = await rig({ sessionSource: SessionSource.RESUME });
    // Assert
    expect(readiness.payload.case).toBe("sessionStarted");
    if (readiness.payload.case === "sessionStarted") {
      expect(readiness.payload.value.source).toBe(SessionSource.RESUME);
    }
  });

  it("announces readiness exactly once per handshake", async () => {
    // Arrange: rig() already drained the one readiness event.
    const { daemon, query } = await rig();
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
    const { query, store, daemon } = await rig({ subscribe: true });
    // Act: a persistent assistant message.
    query.emit({
      type: "assistant",
      uuid: "u1",
      session_id: "sess-1",
      message: { id: "m1", model: "claude", content: [{ type: "text", text: "hi" }], stop_reason: "end_turn", usage: {} },
    } as unknown as SdkMessageLike);
    const sw = await store.peer().next(StoreWriteSchema);
    await tick();
    // Assert: it went to the store as a vendor event, and NOT to the daemon.
    expect(sw.batch!.events).toHaveLength(1);
    expect(sw.batch!.events[0]!.payload.case).toBe("vendor");
    expect(daemon.count(EventSchema)).toBe(0);
  });

  it("sends an ephemeral stream_event straight to the daemon, never to the store", async () => {
    // Arrange
    const { query, store, daemon } = await rig({ subscribe: true });
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
});

describe("UdsSession events: store round-trip and sad path", () => {
  it("forwards a merged store Event to the daemon (onMerged)", async () => {
    // Arrange
    const { store, daemon } = await rig({ subscribe: true });
    // Act: the store emits a merged, seq-stamped event on the subscription conn.
    store.latest().send(EventSchema, create(EventSchema, { sessionId: "sess-1", seq: 5n }));
    const evt = await daemon.next(EventSchema);
    // Assert
    expect(evt.seq).toBe(5n);
  });

  it("forwards a store outage to the daemon as an Event(DegradedState)", async () => {
    // Arrange
    const { store, daemon } = await rig({ subscribe: true });
    // Act: the store connection drops.
    store.close();
    // Assert: a DegradedState event reaches the daemon.
    const evt = await daemon.next(EventSchema);
    expect(evt.payload.case).toBe("degradedState");
    if (evt.payload.case !== "degradedState") throw new Error("case");
    expect(evt.payload.value.component).toBe("shim-store-client");
  });
});

describe("UdsSession lifetime: reattach", () => {
  it("a daemon disconnect ends neither the session nor the in-flight turn", async () => {
    // Arrange: a turn is in flight.
    const { session, query, store, daemon, daemonListener } = await rig({ subscribe: true });
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go" }));
    await daemon.next(AckSchema);
    await until(() => query.prompts.length === 1);
    // Act: the daemon vanishes mid-turn.
    daemon.destroy();
    await until(() => !session.isConnected());
    // Assert: the turn survives and the store connection is untouched.
    expect(session.turnCount()).toBe(1);
    expect(store.peer().closed).toBe(false);
    // The shim redials on its own; the replacement daemon accepts and
    // resubscribes on the SAME live session.
    const daemon2 = await daemonListener.next();
    cleanups.push(() => daemon2.destroy());
    const hello = await daemon2.next(ShimHelloSchema);
    expect(hello.turnInFlight).toBe(true);
    daemon2.send(DaemonHelloSchema, create(DaemonHelloSchema, { daemonVersion: "d2" }));
    await until(() => session.isConnected());
    // Readiness is re-asserted to the REPLACEMENT daemon. That is the point,
    // not a duplicate: a restarted daemon has no memory of this session and
    // must be told it is usable exactly as the first one was.
    const readiness2 = await daemon2.next(EventSchema);
    expect(readiness2.payload.case).toBe("sessionStarted");
    daemon2.send(SubscribeSchema, create(SubscribeSchema, { sessionId: "sess-1", fromSeq: 2n }));
    await until(() => store.count() >= 3);
    const sub2 = await store.latest().next(SubscribeSchema);
    expect(sub2.fromSeq).toBe(2n);
    store.latest().send(EventSchema, create(EventSchema, { sessionId: "sess-1", seq: 3n }));
    const e3 = await daemon2.next(EventSchema);
    expect(e3.seq).toBe(3n);
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
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1", hard: true }));
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
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1", hard: true }));
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
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1", hard: true }));
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
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go" }));
    await daemon.next(AckSchema);
    await until(() => session.turnCount() === 1);
    // Act
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1", hard: true }));
    const ack = await daemon.next(AckSchema);
    // Assert
    expect(ack.interruptOutcome).toBe(InterruptOutcome.INTERRUPTED);
  });

  it("reports ALREADY_COMPLETE when no turn was in flight", async () => {
    // Arrange: nothing has ever been submitted.
    const { daemon, session } = await rig();
    expect(session.turnCount()).toBe(0);
    // Act
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1", hard: true }));
    const ack = await daemon.next(AckSchema);
    // Assert — a SUCCESS: the user asked for the turn to be over and it is.
    // The old two-valued reading painted precisely this as a failed stop.
    expect(ack.interruptOutcome).toBe(InterruptOutcome.ALREADY_COMPLETE);
  });

  it("reports ALREADY_COMPLETE once the turn has ended", async () => {
    // Arrange: submit, then let the turn's result close it.
    const { daemon, query, session } = await rig();
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go" }));
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
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1", hard: true }));
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
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go" }));
    await daemon.next(AckSchema);
    await until(() => session.turnCount() === 1);
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1", hard: true }));
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
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go" }));
    await daemon.next(AckSchema);
    await until(() => session.turnCount() === 1);
    // Act: the stop is sent over the socket, but the result — emitted
    // synchronously into the SDK stream — closes the turn before it lands.
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1", hard: true }));
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
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go" }));
    await daemon.next(AckSchema);
    await until(() => session.turnCount() === 1);
    const ac = new AbortController();
    const decision: Promise<PermissionResultLike> =
      query.canUseTool("Bash", { command: "ls" }, { signal: ac.signal });
    await daemon.next(PermissionRequestSchema);
    // Act
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1", hard: true }));
    const ack = await daemon.next(AckSchema);
    // Assert: the outcome rides along without disturbing the cancel path.
    expect(ack.interruptOutcome).toBe(InterruptOutcome.INTERRUPTED);
    await expect(decision).resolves.toMatchObject({ behavior: "deny" });
  });

  it("leaves the still-queued receipt reporting untouched", async () => {
    // Arrange: survivors AND a live turn, so both paths run together.
    const { daemon, query, session } = await rig();
    query.interruptReceipt = { still_queued: ["u-1"] };
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go" }));
    await daemon.next(AckSchema);
    await until(() => session.turnCount() === 1);
    // Act
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1", hard: true }));
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
      requestId: "p1", text: "go", permissionMode: "bypassPermissions",
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
      requestId: "p1", text: "go", permissionMode: "plan",
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
      requestId: "p1", text: "go", permissionMode: "plan",
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
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go" }));
    await daemon.next(AckSchema);
    await until(() => session.turnCount() === 1);
    // Act
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1", hard: true }));
    const ack = await daemon.next(AckSchema);
    // Assert
    expect(ack.interruptOutcome).toBe(InterruptOutcome.INTERRUPTED);
  });

  it("reports ALREADY_COMPLETE when no turn was in flight", async () => {
    // Arrange: nothing has ever been submitted.
    const { daemon, session } = await rig();
    expect(session.turnCount()).toBe(0);
    // Act
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1", hard: true }));
    const ack = await daemon.next(AckSchema);
    // Assert — a SUCCESS: the user asked for the turn to be over and it is.
    // The old two-valued reading painted precisely this as a failed stop.
    expect(ack.interruptOutcome).toBe(InterruptOutcome.ALREADY_COMPLETE);
  });

  it("reports ALREADY_COMPLETE once the turn has ended", async () => {
    // Arrange: submit, then let the turn's result close it.
    const { daemon, query, session } = await rig();
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go" }));
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
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1", hard: true }));
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
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go" }));
    await daemon.next(AckSchema);
    await until(() => session.turnCount() === 1);
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1", hard: true }));
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
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go" }));
    await daemon.next(AckSchema);
    await until(() => session.turnCount() === 1);
    // Act: the stop is sent over the socket, but the result — emitted
    // synchronously into the SDK stream — closes the turn before it lands.
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1", hard: true }));
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
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go" }));
    await daemon.next(AckSchema);
    await until(() => session.turnCount() === 1);
    const ac = new AbortController();
    const decision: Promise<PermissionResultLike> =
      query.canUseTool("Bash", { command: "ls" }, { signal: ac.signal });
    await daemon.next(PermissionRequestSchema);
    // Act
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1", hard: true }));
    const ack = await daemon.next(AckSchema);
    // Assert: the outcome rides along without disturbing the cancel path.
    expect(ack.interruptOutcome).toBe(InterruptOutcome.INTERRUPTED);
    await expect(decision).resolves.toMatchObject({ behavior: "deny" });
  });

  it("leaves the still-queued receipt reporting untouched", async () => {
    // Arrange: survivors AND a live turn, so both paths run together.
    const { daemon, query, session } = await rig();
    query.interruptReceipt = { still_queued: ["u-1"] };
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go" }));
    await daemon.next(AckSchema);
    await until(() => session.turnCount() === 1);
    // Act
    daemon.send(InterruptSchema, create(InterruptSchema, { requestId: "i1", hard: true }));
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
  it("re-subscribes under the vendor session id the SDK reports", async () => {
    // Arrange: the shim is `sess-1`, but the SDK reports the conversation's
    // own uuid — which is the id the store files these events under (and the
    // id the sidecar keys the same conversation by, from its transcript
    // filename). Subscribing under `sess-1` listens to nothing.
    const { query, store } = await rig({ subscribe: true });
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
    await until(() => store.count() >= 2);
    const replayConn = store.latest();
    await replayConn.next(SubscribeSchema);
    replayConn.send(EventSchema, create(EventSchema, { sessionId: "vendor-uuid", seq: 1n }));
    const replayed = await daemon.next(ReplayEventSchema);
    // Assert
    expect(replayed.requestId).toBe("r1");
    expect(replayed.event?.seq).toBe(1n);
  });

  it("closes a served replay with exactly one ReplayDone", async () => {
    // Arrange
    const { store, daemon } = await rig({ storeSessionId: "vendor-uuid", replayIdleMs: 200 });
    // Act
    daemon.send(ReplayRequestSchema, create(ReplayRequestSchema, { requestId: "r1", fromSeq: 0n, toSeq: 3n }));
    await until(() => store.count() >= 2);
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
    await until(() => store.count() >= 2);
    await store.latest().next(SubscribeSchema);
    const done = await daemon.next(ReplayDoneSchema);
    // Assert
    expect(done.truncated).toBe(true);
    expect(done.reason).not.toBe("");
  });

  it("serves a replay WITHOUT re-subscribing the standing tail", async () => {
    // Arrange: the standing subscription's position belongs to the daemon;
    // moving it would drag the daemon's own consumption backwards.
    const { store, daemon } = await rig({ subscribe: true, storeSessionId: "vendor-uuid", replayIdleMs: 200 });
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
