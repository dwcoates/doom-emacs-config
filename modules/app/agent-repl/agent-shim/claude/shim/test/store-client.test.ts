import { afterEach, describe, expect, it } from "vitest";
import fs from "node:fs";
import net from "node:net";
import { create } from "@bufbuild/protobuf";
import { StoreClient } from "../src/uds/store-client.js";
import {
  DegradedState,
  Event,
  EventSchema,
  HealthCheckSchema,
  HealthStatusSchema,
  StoreWriteAckSchema,
  StoreWriteSchema,
  SubscribeSchema,
} from "../src/uds/proto.js";
import { FramedPeer, tmpSocketPath, until } from "./uds-harness.js";

/** A controllable fake shim-store: framed, one accepted connection per role. */
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
  return fakeStoreAt(tmpSocketPath());
}

/**
 * A fake store bound to an EXACT path, so a test can kill one and stand a
 * replacement up on the same socket — the launchd restart the shim must
 * survive. Any stale socket file is removed first (a closed server may leave
 * one behind, which would fail the rebind with EADDRINUSE).
 */
function fakeStoreAt(socketPath: string): Promise<FakeStore> {
  try {
    fs.unlinkSync(socketPath);
  } catch {
    // No stale file: the normal case for a fresh path.
  }
  const accepted: FramedPeer[] = [];
  return new Promise((resolve, reject) => {
    const server = net.createServer((socket) => {
      accepted.push(new FramedPeer(socket));
    });
    server.once("error", reject);
    server.listen(socketPath, () => {
      resolve({
        socketPath,
        peer: () => {
          if (!accepted[0]) throw new Error("no store connection accepted yet");
          return accepted[0];
        },
        latest: () => {
          const last = accepted[accepted.length - 1];
          if (!last) throw new Error("no store connection accepted yet");
          return last;
        },
        count: () => accepted.length,
        close: () => {
          accepted.forEach((p) => p.destroy());
          server.close();
        },
      });
    });
  });
}

const clients: StoreClient[] = [];
const stores: FakeStore[] = [];
afterEach(() => {
  clients.splice(0).forEach((c) => c.close());
  stores.splice(0).forEach((s) => s.close());
});

async function connectedClient(store: FakeStore, sink?: (e: Event) => void, degraded?: (d: DegradedState) => void): Promise<StoreClient> {
  const client = new StoreClient({ socketPath: store.socketPath, sessionId: "sess-1", producer: "claude-shim:sess-1", heartbeatIntervalMs: 0 });
  clients.push(client);
  if (sink) client.onMerged(sink);
  if (degraded) client.onDegraded(degraded);
  await client.connect();
  await until(() => {
    try {
      store.peer();
      return true;
    } catch {
      return false;
    }
  });
  return client;
}

describe("StoreClient subscribe/write happy path", () => {
  it("requires a live standing subscription before asserting store health", async () => {
    // Arrange: a producer socket by itself cannot render merged state.
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);

    // Act / Assert
    await expect(client.health("health-without-subscription")).resolves.toEqual({
      healthy: false,
      reason: "standing store subscription is not live",
    });
  });

  it("proves store health through a correlated probe without moving the standing subscription", async () => {
    // Arrange: producer + daemon-owned merged-event subscription are live.
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);
    client.subscribe(0n);
    await until(() => store.count() === 2);
    await store.latest().next(SubscribeSchema);

    // Act: health opens one throwaway probe and requires its matching reply.
    const health = client.health("store-health-1");
    await until(() => store.count() === 3);
    const probe = store.latest();
    const check = await probe.next(HealthCheckSchema);
    expect(check.requestId).toBe("store-health-1");
    probe.send(HealthStatusSchema, create(HealthStatusSchema, {
      requestId: "store-health-1",
      healthy: true,
      component: "shim-store",
    }));

    // Assert
    await expect(health).resolves.toEqual({ healthy: true, reason: "" });
    expect(client.isSubscribed()).toBe(true);
  });

  it("rejects a mismatched store-health response rather than accepting another probe's verdict", async () => {
    // Arrange
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);
    client.subscribe(0n);
    await until(() => store.count() === 2);
    await store.latest().next(SubscribeSchema);

    // Act
    const health = client.health("store-health-expected");
    await until(() => store.count() === 3);
    await store.latest().next(HealthCheckSchema);
    store.latest().send(HealthStatusSchema, create(HealthStatusSchema, {
      requestId: "another-probe",
      healthy: true,
      component: "shim-store",
    }));

    // Assert
    await expect(health).resolves.toMatchObject({
      healthy: false,
      reason: expect.stringContaining("store-health-expected"),
    });
  });

  it("sends Subscribe{session_id, from_seq} first on a dedicated connection", async () => {
    // Arrange
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);
    // Act
    client.subscribe(5n);
    await until(() => store.count() === 2);
    const sub = await store.latest().next(SubscribeSchema);
    // Assert
    expect(sub.sessionId).toBe("sess-1");
    expect(sub.fromSeq).toBe(5n);
  });

  it("resolves write() on the StoreWriteAck", async () => {
    // Arrange
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);
    // Act
    const ackP = client.write([create(EventSchema, { sessionId: "sess-1" })]);
    const write = await store.peer().next(StoreWriteSchema);
    expect(write.producer).toBe("claude-shim:sess-1");
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 1n, lastSeq: 42n }));
    const ack = await ackP;
    // Assert
    expect(ack.accepted).toBe(1n);
    expect(ack.lastSeq).toBe(42n);
  });

  it("issues the second write without waiting for the first write's ack", async () => {
    // Arrange: sends chain on ISSUE, never on acks — chaining on acks would
    // serialize every batch behind a store round-trip and throttle the stream.
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);
    // Act: two batches back to back, with NOTHING acked in between.
    const first = client.write([create(EventSchema, { sessionId: "sess-1", seq: 1n })]);
    const second = client.write([create(EventSchema, { sessionId: "sess-1", seq: 2n })]);
    const w1 = await store.peer().next(StoreWriteSchema, "first batch");
    const w2 = await store.peer().next(StoreWriteSchema, "second batch (pipelined)");
    // Assert: both reached the wire un-acked, in call order.
    expect([w1, w2].map((w) => w.batch!.events[0]!.seq)).toEqual([1n, 2n]);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { lastSeq: 1n }));
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { lastSeq: 2n }));
    await Promise.all([first, second]);
  });

  it("matches acks to pipelined batches positionally", async () => {
    // Arrange: pendingWrites is matched to acks POSITIONALLY, so pipelining
    // must not hand a caller another batch's ack.
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);
    // Act: three un-acked batches on the wire, then acks in arrival order.
    const acks = [1n, 2n, 3n].map((seq) => client.write([create(EventSchema, { sessionId: "sess-1", seq })]));
    for (let i = 0; i < 3; i++) await store.peer().next(StoreWriteSchema, `batch ${i + 1}`);
    for (const lastSeq of [1n, 2n, 3n]) {
      store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { lastSeq }));
    }
    // Assert: each caller got ITS batch's ack.
    expect((await Promise.all(acks)).map((a) => a.lastSeq)).toEqual([1n, 2n, 3n]);
  });

  it("hands merged store events to the sink", async () => {
    // Arrange
    const store = await fakeStore();
    stores.push(store);
    const received: Event[] = [];
    const client = await connectedClient(store, (e) => received.push(e));
    client.subscribe(0n);
    await until(() => store.count() === 2);
    await store.latest().next(SubscribeSchema);
    // Act
    store.latest().send(EventSchema, create(EventSchema, { sessionId: "sess-1", seq: 1n }));
    store.latest().send(EventSchema, create(EventSchema, { sessionId: "sess-1", seq: 2n }));
    await until(() => received.length === 2);
    // Assert
    expect(received.map((e) => e.seq)).toEqual([1n, 2n]);
  });
});

describe("StoreClient subscription connection (single-role store)", () => {
  it("keeps writes on the producer connection after subscribing", async () => {
    // Arrange
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);
    client.subscribe(0n);
    await until(() => store.count() === 2);
    await store.latest().next(SubscribeSchema);
    // Act
    const ackP = client.write([create(EventSchema, { sessionId: "sess-1" })]);
    const write = await store.peer().next(StoreWriteSchema);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 1n }));
    await ackP;
    // Assert
    expect(write.producer).toBe("claude-shim:sess-1");
  });

  it("reports DegradedState but stays write-connected when the subscription drops", async () => {
    // Arrange
    const store = await fakeStore();
    stores.push(store);
    const degradations: DegradedState[] = [];
    const client = await connectedClient(store, undefined, (d) => degradations.push(d));
    client.subscribe(0n);
    await until(() => store.count() === 2);
    await store.latest().next(SubscribeSchema);
    // Act
    store.latest().destroy();
    await until(() => degradations.length >= 1);
    // Assert
    expect(degradations[0]!.reason).toContain("subscription");
    expect(client.isConnected()).toBe(true);
  });

  it("re-subscribing replaces the subscription connection without a degrade", async () => {
    // Arrange
    const store = await fakeStore();
    stores.push(store);
    const degradations: DegradedState[] = [];
    const client = await connectedClient(store, undefined, (d) => degradations.push(d));
    client.subscribe(0n);
    await until(() => store.count() === 2);
    await store.latest().next(SubscribeSchema);
    // Act
    client.subscribe(7n);
    await until(() => store.count() === 3);
    const sub2 = await store.latest().next(SubscribeSchema);
    // Assert
    expect(sub2.fromSeq).toBe(7n);
    expect(degradations).toHaveLength(0);
  });
});

describe("StoreClient sad path (no spill, no retry)", () => {
  it("rejects write() and reports DegradedState when the connection is down", async () => {
    // Arrange
    const store = await fakeStore();
    stores.push(store);
    const degradations: DegradedState[] = [];
    const client = await connectedClient(store, undefined, (d) => degradations.push(d));
    client.close();
    // Act / Assert
    await expect(client.write([create(EventSchema, { sessionId: "sess-1", seq: 9n })])).rejects.toThrow();
    expect(degradations).toHaveLength(1);
    expect(degradations[0]!.component).toBe("shim-store-client");
    expect(degradations[0]!.droppedCount).toBe(1n);
    expect(degradations[0]!.recovered).toBe(false);
  });

  it("rejects the batch and drops events when the store Acks an error", async () => {
    // Arrange
    const store = await fakeStore();
    stores.push(store);
    const degradations: DegradedState[] = [];
    const client = await connectedClient(store, undefined, (d) => degradations.push(d));
    // Act
    const writeP = client.write([
      create(EventSchema, { sessionId: "sess-1", seq: 1n }),
      create(EventSchema, { sessionId: "sess-1", seq: 2n }),
    ]);
    await store.peer().next(StoreWriteSchema);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { error: "disk full" }));
    // Assert
    await expect(writeP).rejects.toThrow(/disk full/);
    expect(degradations[0]!.droppedCount).toBe(2n);
  });

  it("rejects an in-flight write and reports degraded when the store drops", async () => {
    // Arrange
    const store = await fakeStore();
    stores.push(store);
    const degradations: DegradedState[] = [];
    const client = await connectedClient(store, undefined, (d) => degradations.push(d));
    // Act: issue a write, then kill the store before it acks
    const writeP = client.write([create(EventSchema, { sessionId: "sess-1", seq: 1n })]);
    await store.peer().next(StoreWriteSchema);
    store.close();
    // Assert
    await expect(writeP).rejects.toThrow();
    await until(() => degradations.length >= 1);
    expect(degradations.some((d) => d.droppedCount === 1n)).toBe(true);
    expect(client.isConnected()).toBe(false);
  });

  it("rejects every pipelined in-flight write when the store drops", async () => {
    // Arrange: pipelining puts several batches in flight at once, and a drop
    // must leave none of them hanging on an ack that will never come.
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);
    // Act: two un-acked batches on the wire, then the store dies.
    const writes = [1n, 2n].map((seq) => client.write([create(EventSchema, { sessionId: "sess-1", seq })]));
    for (let i = 0; i < 2; i++) await store.peer().next(StoreWriteSchema, `batch ${i + 1}`);
    store.close();
    // Assert
    const settled = await Promise.allSettled(writes);
    expect(settled.map((s) => s.status)).toEqual(["rejected", "rejected"]);
  });
});

describe("StoreClient producer redial", () => {
  it("redials and delivers the batch after the store restarts", async () => {
    // Arrange: connect, then kill the store the way a launchd restart does —
    // the shim survives it and would otherwise write to a corpse forever.
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);
    const socketPath = store.socketPath;
    store.close();
    await until(() => !client.isConnected(), "producer conn observed down");
    const restarted = await fakeStoreAt(socketPath);
    stores.push(restarted);

    // Act
    const ackP = client.write([create(EventSchema, { sessionId: "sess-1", seq: 1n })]);
    await until(() => restarted.count() >= 1, "redialed connection accepted");
    const write = await restarted.peer().next(StoreWriteSchema);
    restarted.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 1n, lastSeq: 1n }));

    // Assert: the batch reached the NEW store rather than being dropped.
    expect(write.batch?.events).toHaveLength(1);
    await expect(ackP).resolves.toMatchObject({ accepted: 1n });
  });

  it("drops the batch when the redial fails", async () => {
    // Arrange: store gone for good — nothing is listening on the path.
    const store = await fakeStore();
    stores.push(store);
    const degradations: DegradedState[] = [];
    const client = await connectedClient(store, undefined, (d) => degradations.push(d));
    store.close();
    await until(() => !client.isConnected(), "producer conn observed down");

    // Act / Assert: the honest sad path is unchanged — drop, degrade, reject.
    await expect(client.write([create(EventSchema, { sessionId: "sess-1", seq: 1n })])).rejects.toThrow();
    expect(degradations.some((d) => d.droppedCount === 1n && d.recovered === false)).toBe(true);
  });

  it("dials once for a burst of writes arriving during one outage", async () => {
    // Arrange: writes are fire-and-forget, so a burst must share one dial.
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);
    const socketPath = store.socketPath;
    store.close();
    await until(() => !client.isConnected(), "producer conn observed down");
    const restarted = await fakeStoreAt(socketPath);
    stores.push(restarted);

    // Act: three writes issued before any dial can complete.
    const writes = [1n, 2n, 3n].map((seq) =>
      client.write([create(EventSchema, { sessionId: "sess-1", seq })]).catch(() => undefined),
    );
    await until(() => restarted.count() >= 1, "redialed connection accepted");
    for (let i = 0; i < 3; i++) {
      await restarted.peer().next(StoreWriteSchema);
      restarted.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 1n }));
    }
    await Promise.all(writes);

    // Assert: one socket for the outage, not one per queued write.
    expect(restarted.count()).toBe(1);
  });

  it("keeps acks matched to batches across a redial", async () => {
    // Arrange: pendingWrites is matched to acks POSITIONALLY, so a reorder
    // would resolve the wrong caller with the wrong ack.
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);
    const socketPath = store.socketPath;
    store.close();
    await until(() => !client.isConnected(), "producer conn observed down");
    const restarted = await fakeStoreAt(socketPath);
    stores.push(restarted);

    // Act: three writes, acked with distinguishable lastSeq in arrival order.
    const acks = [
      client.write([create(EventSchema, { sessionId: "sess-1", seq: 1n })]),
      client.write([create(EventSchema, { sessionId: "sess-1", seq: 2n })]),
      client.write([create(EventSchema, { sessionId: "sess-1", seq: 3n })]),
    ];
    await until(() => restarted.count() >= 1, "redialed connection accepted");
    const seen: bigint[] = [];
    for (let i = 0; i < 3; i++) {
      const w = await restarted.peer().next(StoreWriteSchema);
      seen.push(w.batch!.events[0]!.seq);
      restarted.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { lastSeq: w.batch!.events[0]!.seq }));
    }

    // Assert: FIFO on the wire, and each caller got ITS batch's ack.
    expect(seen).toEqual([1n, 2n, 3n]);
    expect((await Promise.all(acks)).map((a) => a.lastSeq)).toEqual([1n, 2n, 3n]);
  });

  it("keeps sending after a batch fails to reach the store", async () => {
    // Arrange: the send chain carries ORDER only, so one failed batch must not
    // wedge every write queued behind it.
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);
    const socketPath = store.socketPath;
    store.close();
    await until(() => !client.isConnected(), "producer conn observed down");

    // Act: the first write fails its redial (nothing listening), then the
    // store comes back and a second write follows it.
    await expect(client.write([create(EventSchema, { sessionId: "sess-1", seq: 1n })])).rejects.toThrow();
    const restarted = await fakeStoreAt(socketPath);
    stores.push(restarted);
    const ackP = client.write([create(EventSchema, { sessionId: "sess-1", seq: 2n })]);
    await until(() => restarted.count() >= 1, "redialed connection accepted");
    const write = await restarted.peer().next(StoreWriteSchema, "post-failure batch");
    restarted.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 1n }));

    // Assert
    expect(write.batch!.events[0]!.seq).toBe(2n);
    await expect(ackP).resolves.toMatchObject({ accepted: 1n });
  });

  it("never redials after a deliberate close", async () => {
    // Arrange: close() is teardown; a write racing shutdown must not
    // resurrect the connection.
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);
    const before = store.count();
    client.close();

    // Act / Assert: dropped without dialing, store untouched.
    await expect(client.write([create(EventSchema, { sessionId: "sess-1", seq: 1n })])).rejects.toThrow();
    expect(store.count()).toBe(before);
  });
});

describe("StoreClient store session key", () => {
  // The store keys events by the VENDOR session id carried on the Event
  // envelope (the Claude uuid, which is the transcript filename the sidecar
  // also keys by). Subscribing under the shim's own `--session-id` registered
  // on a channel nothing publishes to: writes landed, replay and live-tail
  // silently returned nothing, and only EPHEMERAL events reached the daemon.

  async function clientWith(store: FakeStore, storeSessionId?: string): Promise<StoreClient> {
    const client = new StoreClient({
      socketPath: store.socketPath,
      sessionId: "sess-1",
      producer: "claude-shim:sess-1",
      heartbeatIntervalMs: 0,
      ...(storeSessionId !== undefined ? { storeSessionId } : {}),
    });
    clients.push(client);
    await client.connect();
    await until(() => store.count() >= 1, "producer connection accepted");
    return client;
  }

  it("subscribes under the seeded vendor uuid, not the shim session id", async () => {
    // Arrange: the --resume path, where the uuid is known at spawn.
    const store = await fakeStore();
    stores.push(store);
    const client = await clientWith(store, "96a0baaf-uuid");

    // Act
    client.subscribe(0n);
    await until(() => store.count() === 2, "subscription connection accepted");

    // Assert
    const sub = await store.latest().next(SubscribeSchema);
    expect(sub.sessionId).toBe("96a0baaf-uuid");
  });

  it("adopts the vendor uuid and reopens the subscription at the same from_seq", async () => {
    // Arrange: a FRESH session subscribes before the uuid is known.
    const store = await fakeStore();
    stores.push(store);
    const client = await clientWith(store);
    client.subscribe(7n);
    await until(() => store.count() === 2, "first subscription accepted");
    expect((await store.latest().next(SubscribeSchema)).sessionId).toBe("sess-1");

    // Act: the first converted event reveals the real uuid.
    client.adoptStoreKey("96a0baaf-uuid");
    await until(() => store.count() === 3, "resubscribed under the uuid");

    // Assert: same position, corrected key — the store replays seq > 7 from
    // disk, so subscribing late loses nothing.
    const resub = await store.latest().next(SubscribeSchema);
    expect(resub.sessionId).toBe("96a0baaf-uuid");
    expect(resub.fromSeq).toBe(7n);
  });

  it("does not reopen the subscription when the key is unchanged", async () => {
    // Arrange: adoptStoreKey runs on EVERY converted event, so an unchanged
    // key must not churn the connection.
    const store = await fakeStore();
    stores.push(store);
    const client = await clientWith(store, "96a0baaf-uuid");
    client.subscribe(0n);
    await until(() => store.count() === 2, "subscription accepted");

    // Act
    client.adoptStoreKey("96a0baaf-uuid");
    client.adoptStoreKey("96a0baaf-uuid");

    // Assert
    expect(store.count()).toBe(2);
  });

  it("records the key without subscribing when the daemon has not subscribed", async () => {
    // Arrange: events can be converted before any daemon Subscribe arrives.
    const store = await fakeStore();
    stores.push(store);
    const client = await clientWith(store);

    // Act
    client.adoptStoreKey("96a0baaf-uuid");

    // Assert: key recorded, but no subscription invented on the shim's behalf.
    expect(client.storeSessionId()).toBe("96a0baaf-uuid");
    expect(store.count()).toBe(1);
  });

  it("ignores an empty vendor session id", async () => {
    // Arrange: an unparsed/malformed SDK message can carry no session_id, and
    // adopting "" would subscribe to nothing at all.
    const store = await fakeStore();
    stores.push(store);
    const client = await clientWith(store, "96a0baaf-uuid");

    // Act
    client.adoptStoreKey("");

    // Assert
    expect(client.storeSessionId()).toBe("96a0baaf-uuid");
  });
});

describe("StoreClient vendor session rotation", () => {
  // The vendor mints a NEW transcript identity mid-stream (a `/clear` does
  // exactly this), which retires one store seq space and starts another at 1.
  // Each session id is its own seq space and the daemon tracks last_seen_seq
  // per CONNECTION, so re-keying underneath a live daemon connection would
  // replay the new key from seq=1 against a much higher last_seen — the
  // terminal ErrSeqRegression a mid-session `conversation_reset` produced on
  // 2026-07-25. The rotation is therefore a controlled RE-HANDSHAKE: bounce
  // the daemon link first, then re-key and resubscribe from zero.

  /** A rotating client with one merged event already forwarded under `uuid-old`. */
  async function rotatingClient(store: FakeStore, bounces: string[][]): Promise<StoreClient> {
    const received: Event[] = [];
    const client = new StoreClient({
      socketPath: store.socketPath, sessionId: "sess-1",
      producer: "claude-shim:sess-1", heartbeatIntervalMs: 0,
      storeSessionId: "uuid-old",
    });
    clients.push(client);
    client.onMerged((e) => received.push(e));
    client.onRotation((previous, next) => bounces.push([previous, next, client.storeSessionId()]));
    await client.connect();
    await until(() => store.count() >= 1, "producer accepted");
    client.subscribe(0n);
    await until(() => store.count() === 2, "subscription accepted");
    await store.latest().next(SubscribeSchema);
    store.latest().send(EventSchema, create(EventSchema, { sessionId: "uuid-old", seq: 5990n }));
    await until(() => received.length === 1, "event forwarded under the old key");
    return client;
  }

  it("re-keys the standing subscription to the rotated uuid at from_seq=0", async () => {
    // Arrange
    const store = await fakeStore();
    stores.push(store);
    const client = await rotatingClient(store, []);

    // Act: the CLI reports a NEW conversation uuid.
    client.adoptStoreKey("uuid-new");
    await until(() => store.count() === 3, "resubscribed under the rotated uuid");

    // Assert: the new seq space starts fresh, so the daemon's old position
    // (5990, counted in the RETIRED space) means nothing here.
    const resub = await store.latest().next(SubscribeSchema);
    expect(resub.sessionId).toBe("uuid-new");
    expect(resub.fromSeq).toBe(0n);
    expect(client.storeSessionId()).toBe("uuid-new");
  });

  it("bounces the daemon link BEFORE the store key moves", async () => {
    // Arrange: the bounce records the key as it stood when it fired, because
    // ordering is the whole mechanism — a re-key ahead of the bounce would
    // push the new space's seq=1 down the old connection.
    const store = await fakeStore();
    stores.push(store);
    const bounces: string[][] = [];
    const client = await rotatingClient(store, bounces);

    // Act
    client.adoptStoreKey("uuid-new");
    await until(() => bounces.length === 1, "daemon link bounced");

    // Assert
    expect(bounces[0]).toEqual(["uuid-old", "uuid-new", "uuid-old"]);
  });

  it("announces the rotated uuid as the vendor session id for the next hello", async () => {
    // Arrange
    const store = await fakeStore();
    stores.push(store);
    const client = await rotatingClient(store, []);

    // Act
    client.adoptStoreKey("uuid-new");
    await until(() => client.vendorSessionId() === "uuid-new", "vendor id re-announced");

    // Assert
    expect(client.vendorSessionId()).toBe("uuid-new");
  });

  it("delivers a write issued before the rotation under the OLD key", async () => {
    // Arrange: a batch already on the wire when the rotation starts. Its acks
    // are matched positionally on the producer connection, so it must settle
    // rather than be dropped or re-ordered by the re-key.
    const store = await fakeStore();
    stores.push(store);
    const client = await rotatingClient(store, []);
    const acked = client.write([create(EventSchema, { sessionId: "uuid-old", seq: 5991n })]);

    // Act
    client.adoptStoreKey("uuid-new");
    const write = await store.peer().next(StoreWriteSchema, "the pre-rotation batch");
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 1n, lastSeq: 5991n }));

    // Assert
    expect(write.batch!.events[0]!.sessionId).toBe("uuid-old");
    expect((await acked).lastSeq).toBe(5991n);
  });

  it("neither rotates nor reopens for a re-announcement of the CURRENT uuid", async () => {
    // Arrange: adoptStoreKey runs on EVERY converted event, so the same uuid
    // arrives thousands of times. Nothing may move a key to itself — that is
    // what keeps one uuid's seq space monotonic.
    const store = await fakeStore();
    stores.push(store);
    const bounces: string[][] = [];
    const client = await rotatingClient(store, bounces);

    // Act
    client.adoptStoreKey("uuid-old");
    client.adoptStoreKey("uuid-old");
    await until(() => true, "event loop turned");

    // Assert
    expect(bounces).toEqual([]);
    expect(store.count()).toBe(2);
    expect(client.storeSessionId()).toBe("uuid-old");
  });

  it("starts exactly one rotation for a burst of events carrying the new uuid", async () => {
    // Arrange: the SDK emits many messages under the new identity, and each
    // one calls adoptStoreKey while the first rotation is still draining its
    // in-flight writes.
    const store = await fakeStore();
    stores.push(store);
    const bounces: string[][] = [];
    const client = await rotatingClient(store, bounces);

    // Act
    client.adoptStoreKey("uuid-new");
    client.adoptStoreKey("uuid-new");
    client.adoptStoreKey("uuid-new");
    await until(() => store.count() === 3, "resubscribed once");

    // Assert
    expect(bounces.length).toBe(1);
    expect(store.count()).toBe(3);
  });

  it("still adopts the key silently before anything has been forwarded", async () => {
    // Arrange: a fresh session learns its uuid before any merged event. That
    // is a FIRST ADOPTION replacing the `--session-id` placeholder, not a
    // rotation: no bounce, and the daemon's position is kept.
    const store = await fakeStore();
    stores.push(store);
    const bounces: string[][] = [];
    const client = new StoreClient({
      socketPath: store.socketPath, sessionId: "sess-1",
      producer: "claude-shim:sess-1", heartbeatIntervalMs: 0,
    });
    clients.push(client);
    client.onRotation((previous, next) => bounces.push([previous, next]));
    await client.connect();
    await until(() => store.count() >= 1, "producer accepted");
    client.subscribe(9n);
    await until(() => store.count() === 2, "subscription accepted");
    await store.latest().next(SubscribeSchema);

    // Act
    client.adoptStoreKey("uuid-new");
    await until(() => store.count() === 3, "resubscribed");

    // Assert
    const resub = await store.latest().next(SubscribeSchema);
    expect(resub.sessionId).toBe("uuid-new");
    expect(resub.fromSeq).toBe(9n);
    expect(bounces).toEqual([]);
  });

  it("reports no vendor session id while the key is still the placeholder", async () => {
    // Arrange: a fresh session's hello must announce "" rather than the shim's
    // own `--session-id` — the daemon would read that placeholder as a
    // rotation away from the real conversation.
    const store = await fakeStore();
    stores.push(store);
    const client = new StoreClient({
      socketPath: store.socketPath, sessionId: "sess-1",
      producer: "claude-shim:sess-1", heartbeatIntervalMs: 0,
    });
    clients.push(client);
    await client.connect();

    // Act / Assert
    expect(client.storeSessionId()).toBe("sess-1");
    expect(client.vendorSessionId()).toBe("");
  });
});
