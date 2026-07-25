import { afterEach, describe, expect, it } from "vitest";
import fs from "node:fs";
import net from "node:net";
import { create } from "@bufbuild/protobuf";
import { StoreClient } from "../src/uds/store-client.js";
import {
  DegradedState,
  Event,
  EventSchema,
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
