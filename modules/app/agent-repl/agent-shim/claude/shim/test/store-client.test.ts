import { afterEach, describe, expect, it } from "vitest";
import fs from "node:fs";
import net from "node:net";
import { create } from "@bufbuild/protobuf";
import { StoreClient } from "../src/uds/store-client.js";
import {
  DegradedState,
  Event,
  EventSchema,
  HeartbeatSchema,
  HealthCheckSchema,
  HealthStatusSchema,
  StoreWriteAckSchema,
  StoreWriteSchema,
  SubscribeSchema,
} from "../src/uds/proto.js";
import { join } from "node:path";
import { SpillJournal } from "../src/uds/store-spill.js";
import { FramedPeer, tmpSocketPath, tmpSpillDir, until } from "./uds-harness.js";
import { unpackAs } from "../src/uds/framing.js";

/** A controllable fake shim-store: framed, one accepted connection per role. */
interface FakeStore {
  socketPath: string;
  /** The first accepted connection (the producer conn). */
  peer: () => FramedPeer;
  /** The most recently accepted connection (the newest subscription conn). */
  latest: () => FramedPeer;
  /**
   * Resolves with successive accepted connections, in accept order, whether
   * they arrived before or after the call. It is how a test watches the client
   * RELINK after an outage without polling for it: the connection itself is
   * the signal.
   */
  nextConn: () => Promise<FramedPeer>;
  count: () => number;
  close: () => void;
}

function fakeStore(acknowledgeSubscriptions = true): Promise<FakeStore> {
  return fakeStoreAt(tmpSocketPath(), acknowledgeSubscriptions);
}

/**
 * A fake store bound to an EXACT path, so a test can kill one and stand a
 * replacement up on the same socket — the launchd restart the shim must
 * survive. Any stale socket file is removed first (a closed server may leave
 * one behind, which would fail the rebind with EADDRINUSE).
 */
function fakeStoreAt(socketPath: string, acknowledgeSubscriptions = true): Promise<FakeStore> {
  try {
    fs.unlinkSync(socketPath);
  } catch {
    // No stale file: the normal case for a fresh path.
  }
  const accepted: FramedPeer[] = [];
  const unclaimed: FramedPeer[] = [];
  const waiting: Array<(p: FramedPeer) => void> = [];
  return new Promise((resolve, reject) => {
    const server = net.createServer((socket) => {
      const peer = new FramedPeer(socket);
      if (acknowledgeSubscriptions) {
        peer.onReceive((frame) => {
          if (unpackAs(frame, SubscribeSchema) === undefined) return false;
          peer.send(HeartbeatSchema, create(HeartbeatSchema, { sentAtMs: 1n }));
          return false;
        });
      }
      accepted.push(peer);
      const claim = waiting.shift();
      if (claim) claim(peer);
      else unclaimed.push(peer);
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
        nextConn: () =>
          new Promise<FramedPeer>((claim) => {
            const ready = unclaimed.shift();
            if (ready) claim(ready);
            else waiting.push(claim);
          }),
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

/**
 * `relinkReportAfterMs` collapses the relink retry budget for a test that
 * asserts the OUTAGE behaviour: a link-loss report then lands on the next tick
 * instead of after the real budget. Deterministic even against a store that is
 * still up, because the budget's expiry is armed BEFORE the relink attempt is.
 */
async function connectedClient(
  store: FakeStore,
  sink?: (e: Event) => void,
  degraded?: (d: DegradedState) => void,
  relinkReportAfterMs?: number,
): Promise<StoreClient> {
  const client = new StoreClient({
    spillDir: tmpSpillDir(),
    socketPath: store.socketPath,
    sessionId: "sess-1",
    producer: "claude-shim:sess-1",
    heartbeatIntervalMs: 0,
    ...(relinkReportAfterMs !== undefined ? { relinkReportAfterMs } : {}),
  });
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

/** The store's post-replay Heartbeat is the standing-tail readiness barrier. */
async function awaitSubscriptionReady(client: StoreClient): Promise<void> {
  await until(() => client.isSubscribed(), "standing subscription readiness");
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
    await awaitSubscriptionReady(client);

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
    await awaitSubscriptionReady(client);

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
    await awaitSubscriptionReady(client);
    // Assert
    expect(sub.sessionId).toBe("sess-1");
    expect(sub.fromSeq).toBe(5n);
  });

  it("asserts a standing subscription only after the store readiness heartbeat", async () => {
    // Arrange: this fake intentionally withholds the post-replay barrier.
    const store = await fakeStore(false);
    stores.push(store);
    const client = await connectedClient(store);

    // Act: Subscribe reaches the wire, but the tail is not live yet.
    const subscribed = client.subscribe(5n);
    await until(() => store.count() === 2);
    await store.latest().next(SubscribeSchema);
    expect(client.isSubscribed()).toBe(false);

    // Assert: the readiness heartbeat is the structural state transition.
    store.latest().send(HeartbeatSchema, create(HeartbeatSchema, { sentAtMs: 1n }));
    await expect(subscribed).resolves.toBeUndefined();
    expect(client.isSubscribed()).toBe(true);
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
    await awaitSubscriptionReady(client);
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
    await awaitSubscriptionReady(client);
    // Act
    const ackP = client.write([create(EventSchema, { sessionId: "sess-1" })]);
    const write = await store.peer().next(StoreWriteSchema);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 1n }));
    await ackP;
    // Assert
    expect(write.producer).toBe("claude-shim:sess-1");
  });

  it("reports DegradedState but stays write-connected when the subscription drops for longer than the relink budget", async () => {
    // Arrange: budget 0, i.e. a subscription that is given no time at all to
    // come back — the outage behaviour, unchanged.
    const store = await fakeStore();
    stores.push(store);
    const degradations: DegradedState[] = [];
    const client = await connectedClient(store, undefined, (d) => degradations.push(d), 0);
    client.subscribe(0n);
    await until(() => store.count() === 2);
    await store.latest().next(SubscribeSchema);
    await awaitSubscriptionReady(client);
    // Act
    store.latest().destroy();
    await until(() => degradations.length >= 1);
    // Assert
    expect(degradations[0]!.reason).toContain("subscription");
    expect(client.isConnected()).toBe(true);
  });

  it("reports nothing when a dropped subscription is reopened inside the relink budget", async () => {
    // The fleet-wide storm this budget exists for: a store bounce drops every
    // live shim's subscription at once, the relink reopens each one at
    // resumeSeq with the gap replayed, and nothing was ever degraded — so a
    // DegradedState here would alarm about a self-healing event.
    const store = await fakeStore();
    stores.push(store);
    const degradations: DegradedState[] = [];
    const client = await connectedClient(store, undefined, (d) => degradations.push(d));
    client.subscribe(0n);
    await until(() => store.count() === 2);
    await store.latest().next(SubscribeSchema);
    await awaitSubscriptionReady(client);
    // Act: the tail drops, and the relink reopens it against the live store.
    store.latest().destroy();
    await until(() => store.count() === 3, "subscription reopened by the relink");
    await store.latest().next(SubscribeSchema);
    await awaitSubscriptionReady(client);
    // Assert
    expect(degradations).toHaveLength(0);
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
    await awaitSubscriptionReady(client);
    // Act
    client.subscribe(7n);
    await until(() => store.count() === 3);
    const sub2 = await store.latest().next(SubscribeSchema);
    await awaitSubscriptionReady(client);
    // Assert
    expect(sub2.fromSeq).toBe(7n);
    expect(degradations).toHaveLength(0);
  });
});

describe("StoreClient sad path (loud, and never a silent loss)", () => {
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

  it("has no open degraded window while the store link is healthy", async () => {
    // Arrange — a connected client that has never degraded.
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);
    // Act / Assert — a healthy session must never manufacture a fault to
    // re-announce on reattach.
    expect(client.openDegradedReport()).toBeNull();
  });

  it("retains the OPEN degraded window so a reattaching daemon can be told about it", async () => {
    // Arrange — a DegradedState is EPHEMERAL: one raised while no daemon is
    // attached is never written to the store and never replayed, so the state
    // itself has to remain askable.
    const store = await fakeStore();
    stores.push(store);
    const degradations: DegradedState[] = [];
    const client = await connectedClient(store, undefined, (d) => degradations.push(d));
    // Act
    const writeP = client.write([create(EventSchema, { sessionId: "sess-1", seq: 1n })]);
    await store.peer().next(StoreWriteSchema);
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { error: "disk full" }));
    await expect(writeP).rejects.toThrow(/disk full/);
    // Assert — the SAME assertion the reporter received, not a re-worded one.
    expect(client.openDegradedReport()).toBe(degradations[0]);
  });

  it("re-holds an in-flight write for replay when the store drops before acking", async () => {
    // Arrange: a batch on the wire when the store dies is the case that used to
    // be PERMANENT loss — it was declared dropped, degraded and rejected,
    // because re-sending it might have written it twice.
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);
    const writeP = client.write([create(EventSchema, { sessionId: "sess-1", seq: 1n })]);
    await store.peer().next(StoreWriteSchema);

    // Act
    store.close();

    // Assert: it is held for replay, not destroyed. The store's write identity
    // is what makes re-sending it safe whether or not it already landed.
    await until(() => client.heldWriteCount() === 1, "unacked batch re-held for replay");
    expect(client.isConnected()).toBe(false);
    void writeP.catch(() => undefined);
  });

  it("keeps every pipelined in-flight write in order when the store drops", async () => {
    // Arrange: pipelining puts several batches in flight at once, and a drop
    // must lose none of them and reorder none of them.
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);
    const socketPath = store.socketPath;
    const writes = [1n, 2n].map((seq) => client.write([create(EventSchema, { sessionId: "sess-1", seq })]));
    for (let i = 0; i < 2; i++) await store.peer().next(StoreWriteSchema, `batch ${i + 1}`);

    // Act: the store dies, then a replacement comes up on the same path.
    store.close();
    await until(() => client.heldWriteCount() === 2, "both unacked batches re-held");
    const restarted = await fakeStoreAt(socketPath);
    stores.push(restarted);
    await until(() => restarted.count() >= 1, "relinked connection accepted");
    const seen: bigint[] = [];
    for (let i = 0; i < 2; i++) {
      const w = await restarted.peer().next(StoreWriteSchema, `replayed batch ${i + 1}`);
      seen.push(w.batch!.events[0]!.seq);
      restarted.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { lastSeq: w.batch!.events[0]!.seq }));
    }

    // Assert: both replayed, in the order they were written.
    expect(seen).toEqual([1n, 2n]);
    expect((await Promise.all(writes)).map((a) => a.lastSeq)).toEqual([1n, 2n]);
  });

  it("replays an unacked batch under the write identity it was first sent with", async () => {
    // Arrange: a replay whose identity differed from the first delivery would be
    // a NEW event to the store, which is the duplicate this whole mechanism
    // exists to prevent.
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);
    const socketPath = store.socketPath;
    const writeP = client.write([create(EventSchema, { sessionId: "sess-1", seq: 1n })]);
    const first = await store.peer().next(StoreWriteSchema);

    // Act
    store.close();
    await until(() => client.heldWriteCount() === 1, "unacked batch re-held");
    const restarted = await fakeStoreAt(socketPath);
    stores.push(restarted);
    await until(() => restarted.count() >= 1, "relinked connection accepted");
    const replayed = await restarted.peer().next(StoreWriteSchema, "replayed batch");
    restarted.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { deduped: 1n }));
    await writeP;

    // Assert
    expect(replayed.batch!.events[0]!.writeId).toBe(first.batch!.events[0]!.writeId);
    expect(replayed.batch!.events[0]!.writeId).not.toBe("");
  });
});

describe("StoreClient stable write identity", () => {
  it("stamps a write identity on every event of a batch", async () => {
    // Arrange
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);

    // Act
    void client.write([
      create(EventSchema, { sessionId: "sess-1", seq: 1n }),
      create(EventSchema, { sessionId: "sess-1", seq: 2n }),
    ]).catch(() => undefined);
    const sent = await store.peer().next(StoreWriteSchema);

    // Assert
    const ids = sent.batch!.events.map((e) => e.writeId);
    expect(ids.every((id) => id !== "")).toBe(true);
    expect(new Set(ids).size).toBe(2);
  });

  it("keeps a write identity the caller already supplied", async () => {
    // Arrange: a batch recovered from a previous shim's spill journal is
    // re-offered with its ORIGINAL identity, and re-minting it would make the
    // store treat it as a second event.
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);

    // Act
    void client.write([create(EventSchema, { sessionId: "sess-1", seq: 1n, writeId: "already-minted" })])
      .catch(() => undefined);
    const sent = await store.peer().next(StoreWriteSchema);

    // Assert
    expect(sent.batch!.events[0]!.writeId).toBe("already-minted");
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

  it("holds the batch for as long as the redial keeps failing, with no deadline", async () => {
    // Arrange: store gone — nothing is listening on the path. The batch used to
    // be destroyed once a wall-clock budget expired, which made durability a
    // question about how fast the store came back.
    const store = await fakeStore();
    stores.push(store);
    const degradations: DegradedState[] = [];
    const client = await connectedClient(store, undefined, (d) => degradations.push(d), 5);
    store.close();
    await until(() => !client.isConnected(), "producer conn observed down");

    // Act
    const writeP = client.write([create(EventSchema, { sessionId: "sess-1", seq: 1n })]);
    await until(() => client.heldWriteCount() === 1, "batch held for the relink");

    // Assert: still held well past the old budget, and never counted as lost.
    await until(() => degradations.length >= 1, "the link outage itself is reported");
    expect(degradations.every((d) => d.droppedCount === 0n)).toBe(true);
    expect(client.heldWriteCount()).toBe(1);
    void writeP.catch(() => undefined);
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

  it("keeps sending after a batch the store rejects", async () => {
    // Arrange: the send chain carries ORDER only, so one failed batch must not
    // wedge every write queued behind it. A store REJECTION is the failure that
    // is genuinely final — unlike a down link, replaying it would only be
    // rejected again.
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);

    // Act
    const rejected = client.write([create(EventSchema, { sessionId: "sess-1", seq: 1n })]);
    await store.peer().next(StoreWriteSchema, "rejected batch");
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { error: "disk full" }));
    await expect(rejected).rejects.toThrow(/disk full/);
    const ackP = client.write([create(EventSchema, { sessionId: "sess-1", seq: 2n })]);
    const next = await store.peer().next(StoreWriteSchema, "post-failure batch");
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 1n }));

    // Assert
    expect(next.batch!.events[0]!.seq).toBe(2n);
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

describe("StoreClient durable write hold across a store bounce", () => {
  /**
   * A client with an explicit write-hold configuration.
   *
   * There is deliberately no time knob to pass: the hold has no deadline any
   * more, so no test here can be decided by a timer. The bounds that remain are
   * the BACKPRESSURE threshold, and `spillDir` lets a test share one journal
   * between two clients to exercise recovery.
   */
  async function holdingClient(
    store: FakeStore,
    hold: { writeHoldMaxBatches?: number; writeHoldMaxBytes?: number; spillDir?: string },
    degraded?: (d: DegradedState) => void,
  ): Promise<StoreClient> {
    const client = new StoreClient({
      spillDir: hold.spillDir ?? tmpSpillDir(),
      socketPath: store.socketPath,
      sessionId: "sess-1",
      producer: "claude-shim:sess-1",
      heartbeatIntervalMs: 0,
      ...hold,
    });
    clients.push(client);
    if (degraded) client.onDegraded(degraded);
    await client.connect();
    await until(() => store.count() >= 1, "producer connection accepted");
    return client;
  }

  it("flushes a write held through the outage once the store relinks", async () => {
    // Arrange: a store bounce, the deploy's launchd kickstart, with a durable
    // write landing while nothing is listening.
    const store = await fakeStore();
    stores.push(store);
    const client = await holdingClient(store, {});
    const socketPath = store.socketPath;
    store.close();
    await until(() => !client.isConnected(), "producer conn observed down");

    // Act: the write is HELD, not failed, and the replacement store gets it.
    const ackP = client.write([create(EventSchema, { sessionId: "sess-1", seq: 7n })]);
    await until(() => client.heldWriteCount() === 1, "batch held for the relink");
    const restarted = await fakeStoreAt(socketPath);
    stores.push(restarted);
    await until(() => restarted.count() >= 1, "relinked connection accepted");
    const write = await restarted.peer().next(StoreWriteSchema, "flushed held batch");
    restarted.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 1n, lastSeq: 7n }));

    // Assert: the event reached the store and its own receipt came back.
    expect(write.batch!.events[0]!.seq).toBe(7n);
    await expect(ackP).resolves.toMatchObject({ lastSeq: 7n });
    expect(client.heldWriteCount()).toBe(0);
  });

  it("keeps held and post-relink writes in one write order", async () => {
    // Arrange: two batches held during the outage, one issued as the store
    // returns — a flush that raced the live path would reorder them.
    const store = await fakeStore();
    stores.push(store);
    const client = await holdingClient(store, {});
    const socketPath = store.socketPath;
    store.close();
    await until(() => !client.isConnected(), "producer conn observed down");

    // Act
    const acks = [
      client.write([create(EventSchema, { sessionId: "sess-1", seq: 1n })]),
      client.write([create(EventSchema, { sessionId: "sess-1", seq: 2n })]),
    ];
    await until(() => client.heldWriteCount() === 2, "both batches held");
    const restarted = await fakeStoreAt(socketPath);
    stores.push(restarted);
    acks.push(client.write([create(EventSchema, { sessionId: "sess-1", seq: 3n })]));
    await until(() => restarted.count() >= 1, "relinked connection accepted");
    const seen: bigint[] = [];
    for (let i = 0; i < 3; i++) {
      const w = await restarted.peer().next(StoreWriteSchema, `batch ${i + 1}`);
      seen.push(w.batch!.events[0]!.seq);
      restarted.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { lastSeq: w.batch!.events[0]!.seq }));
    }

    // Assert: call order on the wire, and each caller got ITS batch's receipt.
    expect(seen).toEqual([1n, 2n, 3n]);
    expect((await Promise.all(acks)).map((a) => a.lastSeq)).toEqual([1n, 2n, 3n]);
  });

  it("stops ACCEPTING new batches once the hold reaches its bound", async () => {
    // Arrange: room for one held batch. The bound used to DESTROY the queue
    // when it was crossed; now it is a flow-control threshold, so the correct
    // outcome is that the second batch is not taken on at all.
    const store = await fakeStore();
    stores.push(store);
    const client = await holdingClient(store, { writeHoldMaxBatches: 1 });
    store.close();
    await until(() => !client.isConnected(), "producer conn observed down");

    // Act
    const first = client.write([create(EventSchema, { sessionId: "sess-1", seq: 1n })]);
    await until(() => client.heldWriteCount() === 1, "first batch held");
    const second = client.write([create(EventSchema, { sessionId: "sess-1", seq: 2n })]);
    await until(() => client.heldWriteCount() === 1, "second batch is NOT admitted");

    // Assert: the first is intact and the second is neither held nor failed —
    // it is waiting, which is what backpressure looks like from here.
    expect(client.heldWriteCount()).toBe(1);
    let secondSettled = false;
    void second.then(() => { secondSettled = true; }, () => { secondSettled = true; });
    await new Promise((r) => setImmediate(r));
    expect(secondSettled).toBe(false);
    void first.catch(() => undefined);
    void second.catch(() => undefined);
  });

  it("accepts the backpressured batch once the hold drains", async () => {
    // Arrange: a full hold, a batch waiting on capacity, then the store returns.
    const store = await fakeStore();
    stores.push(store);
    const socketPath = store.socketPath;
    const client = await holdingClient(store, { writeHoldMaxBatches: 1 });
    store.close();
    await until(() => !client.isConnected(), "producer conn observed down");
    const first = client.write([create(EventSchema, { sessionId: "sess-1", seq: 1n })]);
    await until(() => client.heldWriteCount() === 1, "first batch held");
    const second = client.write([create(EventSchema, { sessionId: "sess-1", seq: 2n })]);

    // Act
    const restarted = await fakeStoreAt(socketPath);
    stores.push(restarted);
    await until(() => restarted.count() >= 1, "relinked connection accepted");
    const seen: bigint[] = [];
    for (let i = 0; i < 2; i++) {
      const w = await restarted.peer().next(StoreWriteSchema, `batch ${i + 1}`);
      seen.push(w.batch!.events[0]!.seq);
      restarted.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { lastSeq: w.batch!.events[0]!.seq }));
    }

    // Assert: both delivered, in call order, neither dropped.
    expect(seen).toEqual([1n, 2n]);
    expect((await Promise.all([first, second])).map((a) => a.lastSeq)).toEqual([1n, 2n]);
  });

  it("admits a batch larger than the byte bound rather than deadlocking on it", async () => {
    // Arrange: a byte bound no single batch can get under. Waiting for room
    // that draining can never create would park the producer forever.
    const store = await fakeStore();
    stores.push(store);
    const socketPath = store.socketPath;
    const client = await holdingClient(store, { writeHoldMaxBytes: 1 });
    store.close();
    await until(() => !client.isConnected(), "producer conn observed down");

    // Act
    const writeP = client.write([create(EventSchema, { sessionId: "sess-1", seq: 1n })]);
    await until(() => client.heldWriteCount() === 1, "oversized batch held anyway");
    const restarted = await fakeStoreAt(socketPath);
    stores.push(restarted);
    await until(() => restarted.count() >= 1, "relinked connection accepted");
    const flushed = await restarted.peer().next(StoreWriteSchema, "flushed oversized batch");
    restarted.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 1n, lastSeq: 1n }));

    // Assert
    expect(flushed.batch!.events[0]!.seq).toBe(1n);
    await expect(writeP).resolves.toMatchObject({ lastSeq: 1n });
  });

  it("recovers a previous shim's held batch from the spill journal", async () => {
    // Arrange: a shim accepts a durable write during a store outage and then
    // DIES holding it. An in-memory hold takes that write to the grave; the
    // journal is what a replacement shim reads it back out of.
    const store = await fakeStore();
    stores.push(store);
    const socketPath = store.socketPath;
    const spillDir = tmpSpillDir();
    const dying = await holdingClient(store, { spillDir });
    store.close();
    await until(() => !dying.isConnected(), "producer conn observed down");
    const lost = dying.write([create(EventSchema, { sessionId: "sess-1", seq: 42n })]);
    await until(() => dying.heldWriteCount() === 1, "batch held and spilled");
    void lost.catch(() => undefined);
    dying.close();

    // Act: the store comes back and a replacement shim opens the same journal.
    const restarted = await fakeStoreAt(socketPath);
    stores.push(restarted);
    const replacement = await holdingClient(restarted, { spillDir });
    const replayed = await restarted.peer().next(StoreWriteSchema, "recovered spill batch");
    restarted.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 1n, lastSeq: 42n }));

    // Assert: the event the first shim accepted reached the store anyway.
    expect(replayed.batch!.events[0]!.seq).toBe(42n);
    expect(replacement.heldWriteCount()).toBe(0);
  });

  it("replays a recovered batch under its original write identity", async () => {
    // Arrange: recovery is only safe because the store can recognize a repeat,
    // and it can only do that if the identity survives the crash with the event.
    const store = await fakeStore();
    stores.push(store);
    const socketPath = store.socketPath;
    const spillDir = tmpSpillDir();
    const dying = await holdingClient(store, { spillDir });
    store.close();
    await until(() => !dying.isConnected(), "producer conn observed down");
    const event = create(EventSchema, { sessionId: "sess-1", seq: 42n });
    void dying.write([event]).catch(() => undefined);
    await until(() => dying.heldWriteCount() === 1, "batch held and spilled");
    const mintedId = event.writeId;
    dying.close();

    // Act
    const restarted = await fakeStoreAt(socketPath);
    stores.push(restarted);
    await holdingClient(restarted, { spillDir });
    const replayed = await restarted.peer().next(StoreWriteSchema, "recovered spill batch");
    restarted.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { deduped: 1n }));

    // Assert
    expect(mintedId).not.toBe("");
    expect(replayed.batch!.events[0]!.writeId).toBe(mintedId);
  });

  it("clears the spill journal once the store has acknowledged everything", async () => {
    // Arrange: a journal that is never truncated would replay the whole session
    // on every restart, so a settled batch must retire its record.
    const store = await fakeStore();
    stores.push(store);
    const socketPath = store.socketPath;
    const spillDir = tmpSpillDir();
    const client = await holdingClient(store, { spillDir });
    store.close();
    await until(() => !client.isConnected(), "producer conn observed down");
    const ackP = client.write([create(EventSchema, { sessionId: "sess-1", seq: 1n })]);
    await until(() => client.heldWriteCount() === 1, "batch held and spilled");
    const restarted = await fakeStoreAt(socketPath);
    stores.push(restarted);
    await until(() => restarted.count() >= 1, "relinked connection accepted");
    await restarted.peer().next(StoreWriteSchema, "flushed held batch");

    // Act
    restarted.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 1n, lastSeq: 1n }));
    await ackP;

    // Assert: a fresh client on the same directory finds nothing owed.
    const journal = new SpillJournal({ path: join(spillDir, "store-write-spill.bin"), sessionId: "sess-1" });
    const remaining = journal.read();
    journal.close();
    expect(remaining).toEqual([]);
  });

  it("fails a held write when the client is SEALED for shutdown", async () => {
    // Arrange: the hold's premise is that the link comes back and we deliver.
    // That is false for a process on its way out, and waiting anyway is how an
    // orderly shutdown hangs on an unreachable store.
    const store = await fakeStore();
    stores.push(store);
    const degradations: DegradedState[] = [];
    const client = await holdingClient(store, {}, (d) => degradations.push(d));
    store.close();
    await until(() => !client.isConnected(), "producer conn observed down");
    const writeP = client.write([create(EventSchema, { sessionId: "sess-1", seq: 1n })]);
    await until(() => client.heldWriteCount() === 1, "batch held for the relink");

    // Act
    client.seal();

    // Assert: the sad path, unchanged, and nothing left waiting.
    await expect(writeP).rejects.toThrow(/write on a down connection/);
    expect(degradations.some((d) => d.droppedCount === 1n && d.recovered === false)).toBe(true);
    expect(client.heldWriteCount()).toBe(0);
  });

  it("still delivers over a LIVE link after the client is sealed", async () => {
    // Arrange: sealing must not amputate a link that works — the termination
    // receipt an orderly shutdown writes goes out over exactly this path.
    const store = await fakeStore();
    stores.push(store);
    const client = await holdingClient(store, {});

    // Act
    client.seal();
    const ackP = client.write([create(EventSchema, { sessionId: "sess-1", seq: 1n })]);
    await store.peer().next(StoreWriteSchema, "post-seal batch");
    store.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 1n, lastSeq: 1n }));

    // Assert
    await expect(ackP).resolves.toMatchObject({ lastSeq: 1n });
  });

  it("fails a held write on a deliberate close rather than leaving it unsettled", async () => {
    // Arrange: teardown is final, so a held batch will never reach the store.
    const store = await fakeStore();
    stores.push(store);
    const client = await holdingClient(store, {});
    store.close();
    await until(() => !client.isConnected(), "producer conn observed down");
    const writeP = client.write([create(EventSchema, { sessionId: "sess-1", seq: 1n })]);
    await until(() => client.heldWriteCount() === 1, "batch held for the relink");

    // Act / Assert
    client.close();
    await expect(writeP).rejects.toThrow(/write on a down connection/);
  });
});

describe("StoreClient store link recovery", () => {
  // A shim-store restart (a deploy step) kills BOTH connections under a live
  // shim. Before the link recovered as a link, the producer was redialed only
  // by the next write and the standing subscription was never reopened at all
  // — so the connectivity fault the daemon opened for the outage never closed
  // and every workspace stayed painted degraded until something was bounced.

  interface LinkRig {
    client: StoreClient;
    /** Every report the client made, in order. */
    reports: DegradedState[];
    /** Resolves on the FIRST `recovered: true` report. */
    recovered: Promise<DegradedState>;
    /** Events handed to the sink. */
    forwarded: Event[];
    /** Resolves once `n` events have been forwarded. */
    awaitForwarded: (n: number) => Promise<void>;
  }

  /**
   * A client whose relink backoff is compressed to milliseconds. The backoff
   * BOUNDS are the only thing a test needs to shrink: every wait below is on a
   * connection or a report, never on a duration.
   */
  async function linkRig(store: FakeStore, relinkReportAfterMs?: number): Promise<LinkRig> {
    const reports: DegradedState[] = [];
    const forwarded: Event[] = [];
    const forwardWaiters: Array<{ n: number; resolve: () => void }> = [];
    let announceRecovery!: (d: DegradedState) => void;
    const recovered = new Promise<DegradedState>((resolve) => {
      announceRecovery = resolve;
    });
    const client = new StoreClient({
    spillDir: tmpSpillDir(),
      socketPath: store.socketPath,
      sessionId: "sess-1",
      producer: "claude-shim:sess-1",
      heartbeatIntervalMs: 0,
      relinkBackoffMinMs: 2,
      relinkBackoffMaxMs: 10,
      // Left at the client's real default unless a test asks otherwise: a rig
      // with the default budget is how the SELF-HEALING bounce is observed,
      // and `0` is how the genuine outage's degraded/recovered pair is.
      ...(relinkReportAfterMs !== undefined ? { relinkReportAfterMs } : {}),
    });
    clients.push(client);
    client.onDegraded((d) => {
      reports.push(d);
      if (d.recovered) announceRecovery(d);
    });
    client.onMerged((e) => {
      forwarded.push(e);
      for (const w of forwardWaiters.splice(0)) {
        if (forwarded.length >= w.n) w.resolve();
        else forwardWaiters.push(w);
      }
    });
    await client.connect();
    await store.nextConn();
    return {
      client,
      reports,
      recovered,
      forwarded,
      awaitForwarded: (n) =>
        new Promise<void>((resolve) => {
          if (forwarded.length >= n) resolve();
          else forwardWaiters.push({ n, resolve });
        }),
    };
  }

  /** Kill the store and stand its replacement up on the same socket path. */
  async function restart(store: FakeStore): Promise<FakeStore> {
    const socketPath = store.socketPath;
    store.close();
    const restarted = await fakeStoreAt(socketPath);
    stores.push(restarted);
    return restarted;
  }

  it("reports recovery once both connections are back after an outage that outlasted the relink budget", async () => {
    // Arrange: a fully wired link — producer connection plus the daemon's
    // standing subscription — whose retry budget is collapsed to zero, so the
    // drop is reported and a fault window is genuinely opened.
    const store = await fakeStore();
    stores.push(store);
    const rig = await linkRig(store, 0);
    await rig.client.subscribe(0n);
    await (await store.nextConn()).next(SubscribeSchema);

    // Act: the deploy restarts the store under the live client.
    await restart(store);

    // Assert: the close edge of the window the outage opened, which is what
    // the daemon's fault machinery closes the session_fault row on.
    const report = await rig.recovered;
    expect(report.component).toBe("shim-store-client");
    expect(report.recovered).toBe(true);
  });

  it("closes the retained degraded window on recovery, so nothing stale is re-announced", async () => {
    // Arrange — an outage that outlasted the budget, so a window is genuinely
    // open and retained.
    const store = await fakeStore();
    stores.push(store);
    const rig = await linkRig(store, 0);
    await rig.client.subscribe(0n);
    await (await store.nextConn()).next(SubscribeSchema);
    await restart(store);

    // Act
    await rig.recovered;

    // Assert — a reattach after this must announce nothing.
    expect(rig.client.openDegradedReport()).toBeNull();
  });

  it("reports neither degradation nor recovery for a restart the relink absorbs", async () => {
    // THE DEPLOY'S OWN STORE KICKSTART. Both connections drop and both come
    // back, nothing is lost, and the daemon hears about none of it — the pair
    // of DegradedState reports this used to emit per shim (plus the recovery
    // that settled them) was a fleet-wide warn storm over a self-healing event.
    const store = await fakeStore();
    stores.push(store);
    const rig = await linkRig(store);
    await rig.client.subscribe(0n);
    await (await store.nextConn()).next(SubscribeSchema);

    // Act
    const restarted = await restart(store);
    await restarted.nextConn(); // the relinked producer connection
    await (await restarted.nextConn()).next(SubscribeSchema);
    // A full write round-trip on the recovered link, so the assertion stands
    // after the client has gone on working rather than mid-relink.
    const ackP = rig.client.write([create(EventSchema, { sessionId: "sess-1", seq: 1n })]);
    await restarted.peer().next(StoreWriteSchema);
    restarted.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 1n }));
    await ackP;

    // Assert
    expect(rig.reports).toHaveLength(0);
  });

  it("reports the degradation when the store never comes back within the budget", async () => {
    // The genuine outage, unchanged: the budget expires with the link still
    // down, and the report is exactly the one the first drop used to send.
    const store = await fakeStore();
    stores.push(store);
    const rig = await linkRig(store, 1);
    await rig.client.subscribe(0n);
    await (await store.nextConn()).next(SubscribeSchema);

    // Act: gone for good — nothing rebinds the socket.
    store.close();

    // Assert
    await until(() => rig.reports.length >= 1, "the budget expired with the link down");
    expect(rig.reports[0]!.component).toBe("shim-store-client");
    expect(rig.reports[0]!.recovered).toBe(false);
    expect(rig.reports[0]!.reason).toMatch(/store (connection|subscription) closed/);
  });

  it("reports one degradation for an outage that dropped both connections", async () => {
    // Both edges fire within a tick of each other and describe the SAME outage
    // of the SAME component, for which the daemon holds one fault row and one
    // card — so the second report was only ever a duplicate warn.
    const store = await fakeStore();
    stores.push(store);
    const rig = await linkRig(store, 1);
    await rig.client.subscribe(0n);
    await (await store.nextConn()).next(SubscribeSchema);

    // Act: both edges drop, and both are observed down before anything is
    // asserted — so the single report below is a merge of two losses rather
    // than a race that only one of them reached.
    store.close();
    await until(() => !rig.client.isConnected() && !rig.client.isSubscribed(),
      "both store connections observed down");
    await until(() => rig.reports.length >= 1, "the budget expired with the link down");

    // Assert
    expect(rig.reports.filter((r) => !r.recovered)).toHaveLength(1);
  });

  it("reopens the subscription after the last forwarded seq so the gap replays", async () => {
    // Arrange: two merged events reach the daemon before the store dies, so
    // the daemon's handshake position is already stale.
    const store = await fakeStore();
    stores.push(store);
    const rig = await linkRig(store);
    await rig.client.subscribe(0n);
    const sub = await store.nextConn();
    await sub.next(SubscribeSchema);
    sub.send(EventSchema, create(EventSchema, { sessionId: "sess-1", seq: 1n }));
    sub.send(EventSchema, create(EventSchema, { sessionId: "sess-1", seq: 2n }));
    await rig.awaitForwarded(2);

    // Act
    const restarted = await restart(store);
    await restarted.nextConn(); // the relinked producer connection
    const resubscribed = await restarted.nextConn();

    // Assert: EXCLUSIVE from_seq at the last forwarded event — everything the
    // store accepted during the gap replays, and nothing already seen repeats.
    expect((await resubscribed.next(SubscribeSchema)).fromSeq).toBe(2n);
  });

  it("reopens at the daemon's own position when no event was forwarded yet", async () => {
    // Arrange: a subscription that never delivered anything has no position of
    // its own, so the daemon's handshake from_seq is still the truth.
    const store = await fakeStore();
    stores.push(store);
    const rig = await linkRig(store);
    await rig.client.subscribe(7n);
    await (await store.nextConn()).next(SubscribeSchema);

    // Act
    const restarted = await restart(store);
    await restarted.nextConn(); // the relinked producer connection
    const resubscribed = await restarted.nextConn();

    // Assert
    expect((await resubscribed.next(SubscribeSchema)).fromSeq).toBe(7n);
  });

  it("recovers on the producer connection alone when the daemon never subscribed", async () => {
    // Arrange: a session whose bring-up gate has not run yet still loses its
    // producer connection to a store restart, and still owes the daemon the
    // recovery that closes the fault. Budget 0, so the loss is reported and
    // there is a fault window for the recovery to close.
    const store = await fakeStore();
    stores.push(store);
    const rig = await linkRig(store, 0);

    // Act
    const restarted = await restart(store);
    await rig.recovered;

    // Assert: exactly the producer connection, and no subscription invented
    // for a daemon that never asked for one.
    expect(restarted.count()).toBe(1);
    expect(rig.client.isSubscribed()).toBe(false);
  });

  it("reports one recovery for an outage that dropped both connections", async () => {
    // Arrange: a restart drops the producer AND the subscription, so both
    // report the outage — but the daemon holds ONE fault row per component and
    // rejects a second close of a window that is no longer open. Budget 0, so
    // the outage is reported and a window is genuinely open.
    const store = await fakeStore();
    stores.push(store);
    const rig = await linkRig(store, 0);
    await rig.client.subscribe(0n);
    await (await store.nextConn()).next(SubscribeSchema);

    // Act
    const restarted = await restart(store);
    await rig.recovered;
    // A full write round-trip on the recovered link, so the assertion below
    // stands after the client has gone on working rather than at the instant
    // the first recovery landed.
    const ackP = rig.client.write([create(EventSchema, { sessionId: "sess-1", seq: 1n })]);
    await restarted.peer().next(StoreWriteSchema);
    restarted.peer().send(StoreWriteAckSchema, create(StoreWriteAckSchema, { accepted: 1n }));
    await ackP;

    // Assert
    expect(rig.reports.filter((r) => r.recovered)).toHaveLength(1);
  });

  it("reports no recovery for a subscription the daemon deliberately replaced", async () => {
    // Arrange: a reopen at a new from_seq is not an outage, so it must neither
    // degrade nor claim a recovery — an unpaired close is an error in the
    // daemon's fault machinery, not a no-op.
    const store = await fakeStore();
    stores.push(store);
    const rig = await linkRig(store);
    await rig.client.subscribe(0n);
    await (await store.nextConn()).next(SubscribeSchema);

    // Act
    await rig.client.subscribe(9n);
    await (await store.nextConn()).next(SubscribeSchema);

    // Assert
    expect(rig.reports).toHaveLength(0);
  });

  it("never relinks after a deliberate close", async () => {
    // Arrange: an outage arms recovery, then the shim shuts down. A relink
    // firing after teardown would redial the store out from under it.
    const store = await fakeStore();
    stores.push(store);
    const rig = await linkRig(store);
    await rig.client.subscribe(0n);
    await (await store.nextConn()).next(SubscribeSchema);
    const socketPath = store.socketPath;
    store.close();
    await until(() => !rig.client.isConnected(), "producer conn observed down");

    // Act: close, then stand the store back up.
    rig.client.close();
    const restarted = await fakeStoreAt(socketPath);
    stores.push(restarted);
    // The awaited rejection carries the send chain through the exact place a
    // redial would happen, so the assertion below is about a path that ran.
    await expect(rig.client.write([create(EventSchema, { sessionId: "sess-1", seq: 1n })])).rejects.toThrow();

    // Assert
    expect(restarted.count()).toBe(0);
    expect(rig.reports.some((r) => r.recovered)).toBe(false);
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
    spillDir: tmpSpillDir(),
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
    await awaitSubscriptionReady(client);

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
    await awaitSubscriptionReady(client);

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
    await awaitSubscriptionReady(client);

    // Act
    client.adoptStoreKey("96a0baaf-uuid");
    client.adoptStoreKey("96a0baaf-uuid");

    // Assert
    expect(store.count()).toBe(2);
  });

  it("marks an SDK-reported key as vendor identity when it equals the placeholder", async () => {
    // Arrange: a fresh session begins with the daemon id as an explicitly
    // untrusted storage placeholder.
    const store = await fakeStore();
    stores.push(store);
    const client = await clientWith(store);
    expect(client.storeSessionId()).toBe("sess-1");
    expect(client.vendorSessionId()).toBe("");

    // Act: the SDK reports that exact string as its vendor session id.
    client.adoptStoreKey("sess-1");

    // Assert: the value is authoritative without inventing a re-key or a
    // subscription the daemon did not request.
    expect(client.vendorSessionId()).toBe("sess-1");
    expect(store.count()).toBe(1);
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
  // the daemon link first, then re-key and RETIRE the retired space's
  // subscription. The new one is opened by the bounce's re-handshake gate, at
  // the from_seq that hello carries.

  /** A rotating client with one merged event already forwarded under `uuid-old`. */
  async function rotatingClient(store: FakeStore, bounces: string[][]): Promise<StoreClient> {
    const received: Event[] = [];
    const client = new StoreClient({
    spillDir: tmpSpillDir(),
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
    await awaitSubscriptionReady(client);
    store.latest().send(EventSchema, create(EventSchema, { sessionId: "uuid-old", seq: 5990n }));
    await until(() => received.length === 1, "event forwarded under the old key");
    return client;
  }

  it("re-keys to the rotated uuid and RETIRES the retired space's subscription", async () => {
    // Arrange
    const store = await fakeStore();
    stores.push(store);
    const client = await rotatingClient(store, []);
    const retired = store.latest();

    // Act: the CLI reports a NEW conversation uuid.
    client.adoptStoreKey("uuid-new");
    await until(() => client.storeSessionId() === "uuid-new", "store re-keyed");
    await until(() => retired.closed, "retired subscription closed");

    // Assert: no connection to the retired seq space outlives the rotation,
    // and no replacement is opened here — the bounce's re-handshake opens it
    // at the from_seq that hello carries (which the daemon resets to zero).
    expect(client.isSubscribed()).toBe(false);
    expect(store.count()).toBe(2);
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
    await until(() => client.storeSessionId() === "uuid-new", "rotation settled");

    // Assert: one bounce, and no extra store connection opened by the burst.
    expect(bounces.length).toBe(1);
    expect(store.count()).toBe(2);
  });

  it("still adopts the key silently before anything has been forwarded", async () => {
    // Arrange: a fresh session learns its uuid before any merged event. That
    // is a FIRST ADOPTION replacing the `--session-id` placeholder, not a
    // rotation: no bounce, and the daemon's position is kept.
    const store = await fakeStore();
    stores.push(store);
    const bounces: string[][] = [];
    const client = new StoreClient({
    spillDir: tmpSpillDir(),
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
    await awaitSubscriptionReady(client);

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
    spillDir: tmpSpillDir(),
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
