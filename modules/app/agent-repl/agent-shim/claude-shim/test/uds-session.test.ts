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
  PermissionResultLike,
  QueryLike,
  SdkMessageLike,
  SdkUserMessageLike,
} from "../src/session.js";
import type { ModelInfo, SlashCommand } from "../src/protocol.js";
import {
  AckSchema,
  DaemonHelloSchema,
  EventSchema,
  InterruptSchema,
  PermissionDecision,
  PermissionRequestSchema,
  PermissionResponseSchema,
  SessionSource,
  ShimHelloSchema,
  StoreWriteSchema,
  SubmitPromptSchema,
  SubscribeSchema,
} from "../src/uds/proto.js";
import { FramedPeer, connectPeer, tmpSocketPath, until } from "./uds-harness.js";

const tick = (): Promise<void> => new Promise<void>((r) => setImmediate(r));

/** A scripted SDK query: records prompts pushed at it, emits messages on cue. */
class FakeQuery implements QueryLike {
  readonly prompts: SdkUserMessageLike[] = [];
  readonly canUseTool: CanUseToolLike;
  interruptCalls = 0;
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
  interrupt(): Promise<void> {
    this.interruptCalls++;
    return Promise.resolve();
  }
  setPermissionMode(): Promise<void> {
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
  peer: () => FramedPeer;
  close: () => void;
}

function fakeStore(): Promise<FakeStore> {
  const socketPath = tmpSocketPath();
  let accepted: FramedPeer | null = null;
  return new Promise((resolve, reject) => {
    const server = net.createServer((s) => (accepted = new FramedPeer(s)));
    server.once("error", reject);
    server.listen(socketPath, () =>
      resolve({
        socketPath,
        peer: () => {
          if (!accepted) throw new Error("store: no connection accepted");
          return accepted;
        },
        close: () => {
          accepted?.destroy();
          server.close();
        },
      }),
    );
  });
}

/** Connect a daemon peer once the shim's listener is up (start() is async). */
async function connectWhenReady(socketPath: string): Promise<FramedPeer> {
  for (let i = 0; i < 2000; i++) {
    try {
      return await connectPeer(socketPath);
    } catch {
      await tick();
    }
  }
  throw new Error(`connectWhenReady: ${socketPath} never became connectable`);
}

interface Rig {
  session: UdsSession;
  query: FakeQuery;
  store: FakeStore;
  daemon: FramedPeer;
  udsSocketPath: string;
}

const cleanups: Array<() => void | Promise<void>> = [];
afterEach(async () => {
  for (const c of cleanups.splice(0)) await c();
});

/** Stand up store + session, connect a daemon, and (optionally) subscribe. */
async function rig(opts: { subscribe?: boolean; sessionSource?: SessionSource } = {}): Promise<Rig> {
  const store = await fakeStore();
  cleanups.push(() => store.close());

  let query!: FakeQuery;
  const createQuery = (
    prompt: AsyncIterable<SdkUserMessageLike>,
    canUseTool: CanUseToolLike,
  ): QueryLike => (query = new FakeQuery(prompt, canUseTool));

  const udsSocketPath = tmpSocketPath();
  const session = new UdsSession({
    sessionId: "sess-1",
    shimVersion: "9.9",
    protocolVersion: "1",
    udsSocketPath,
    storeSocketPath: store.socketPath,
    sessionSource: opts.sessionSource ?? SessionSource.FRESH,
    createQuery,
    heartbeatIntervalMs: 0,
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

  const daemon = await connectWhenReady(udsSocketPath);
  await daemon.next(ShimHelloSchema);
  daemon.send(DaemonHelloSchema, create(DaemonHelloSchema, { daemonVersion: "d1", protocolVersion: "1" }));
  await until(() => session.isConnected());

  if (opts.subscribe) {
    daemon.send(SubscribeSchema, create(SubscribeSchema, { sessionId: "sess-1", fromSeq: 0n }));
    await store.peer().next(SubscribeSchema);
  }
  return { session, query, store, daemon, udsSocketPath };
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
    // Act: the store emits a merged, seq-stamped event.
    store.peer().send(EventSchema, create(EventSchema, { sessionId: "sess-1", seq: 5n }));
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
    const { session, query, store, daemon, udsSocketPath } = await rig({ subscribe: true });
    daemon.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "go" }));
    await daemon.next(AckSchema);
    await until(() => query.prompts.length === 1);
    // Act: the daemon vanishes mid-turn.
    daemon.destroy();
    await until(() => !session.isConnected());
    // Assert: the turn survives and the store connection is untouched.
    expect(session.turnCount()).toBe(1);
    expect(store.peer().closed).toBe(false);
    // A new daemon reattaches on the SAME live shim and resubscribes.
    const daemon2 = await connectWhenReady(udsSocketPath);
    cleanups.push(() => daemon2.destroy());
    const hello = await daemon2.next(ShimHelloSchema);
    expect(hello.turnInFlight).toBe(true);
    daemon2.send(DaemonHelloSchema, create(DaemonHelloSchema, { daemonVersion: "d2" }));
    await until(() => session.isConnected());
    daemon2.send(SubscribeSchema, create(SubscribeSchema, { sessionId: "sess-1", fromSeq: 2n }));
    const sub2 = await store.peer().next(SubscribeSchema);
    expect(sub2.fromSeq).toBe(2n);
    store.peer().send(EventSchema, create(EventSchema, { sessionId: "sess-1", seq: 3n }));
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
});
