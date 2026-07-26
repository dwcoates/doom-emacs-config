/**
 * StoreClient.replay — the shim's half of the daemon's bounded historical
 * replay (core.proto ReplayRequest, design §5.4.1).
 *
 * The property that matters most here is NEGATIVE and has its own test: the
 * standing subscription must be untouched. It is the daemon's live tail and
 * its position belongs to the daemon; serving history by reopening it would
 * drag that position backwards and re-deliver history down the live path.
 */
import { afterEach, describe, expect, it } from "vitest";
import fs from "node:fs";
import net from "node:net";
import { create } from "@bufbuild/protobuf";
import { StoreClient } from "../src/uds/store-client.js";
import { Event, EventSchema, SubscribeSchema } from "../src/uds/proto.js";
import { FramedPeer, tmpSocketPath, until } from "./uds-harness.js";

interface FakeStore {
  socketPath: string;
  /** Every accepted connection, in accept order. */
  conns: FramedPeer[];
  close: () => void;
}

function fakeStore(): Promise<FakeStore> {
  const socketPath = tmpSocketPath();
  try {
    fs.unlinkSync(socketPath);
  } catch {
    // No stale file: the normal case for a fresh path.
  }
  const conns: FramedPeer[] = [];
  return new Promise((resolve, reject) => {
    const server = net.createServer((socket) => conns.push(new FramedPeer(socket)));
    server.once("error", reject);
    server.listen(socketPath, () =>
      resolve({
        socketPath,
        conns,
        close: () => {
          conns.forEach((c) => c.destroy());
          server.close();
        },
      }),
    );
  });
}

const clients: StoreClient[] = [];
const stores: FakeStore[] = [];
afterEach(() => {
  clients.splice(0).forEach((c) => c.close());
  stores.splice(0).forEach((s) => s.close());
});

async function connectedClient(store: FakeStore): Promise<StoreClient> {
  const client = new StoreClient({
    socketPath: store.socketPath,
    sessionId: "sess-1",
    producer: "claude-shim:sess-1",
    heartbeatIntervalMs: 0,
  });
  clients.push(client);
  await client.connect();
  await until(() => store.conns.length >= 1);
  return client;
}

/** An event as the store would deliver it, seq-stamped. */
function storeEvent(seq: number): Event {
  return create(EventSchema, { sessionId: "sess-1", seq: BigInt(seq) });
}

/** Await the replay subscription's Subscribe frame, and return its peer. */
async function replayPeer(store: FakeStore, index: number): Promise<FramedPeer> {
  await until(() => store.conns.length > index);
  const peer = store.conns[index]!;
  await peer.next(SubscribeSchema);
  return peer;
}

describe("StoreClient.replay", () => {
  it("opens its own subscription rather than reusing the producer connection", async () => {
    // Arrange
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);
    // Act
    const done = client.replay({ fromSeq: 0n, toSeq: 3n, maxEvents: 0, idleMs: 200, onEvent: () => {} });
    const peer = await replayPeer(store, 1);
    peer.send(EventSchema, storeEvent(3));
    await done;
    // Assert
    expect(store.conns.length).toBe(2);
  });

  it("subscribes from the requested seq", async () => {
    // Arrange
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);
    // Act
    const done = client.replay({ fromSeq: 42n, toSeq: 99n, maxEvents: 0, idleMs: 200, onEvent: () => {} });
    await until(() => store.conns.length > 1);
    const sub = await store.conns[1]!.next(SubscribeSchema);
    store.conns[1]!.send(EventSchema, storeEvent(99));
    await done;
    // Assert
    expect(sub.fromSeq).toBe(42n);
  });

  it("LEAVES THE STANDING SUBSCRIPTION UNTOUCHED", async () => {
    // Arrange: a live daemon tail from seq 500, then a replay of old history.
    // Reopening the standing subscription is exactly what this design exists
    // to avoid, so the proof is that its connection sees no second Subscribe.
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);
    client.subscribe(500n);
    const standing = await replayPeer(store, 1);
    const standingFrames: unknown[] = [];
    standing.socket.on("data", (chunk) => standingFrames.push(chunk));

    // Act
    const done = client.replay({ fromSeq: 0n, toSeq: 10n, maxEvents: 0, idleMs: 200, onEvent: () => {} });
    const replay = await replayPeer(store, 2);
    replay.send(EventSchema, storeEvent(10));
    await done;

    // Assert: nothing was ever written back down the standing connection.
    expect(standingFrames).toEqual([]);
  });

  it("keeps the standing subscription's live tail flowing during a replay", async () => {
    // Arrange
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);
    const live: bigint[] = [];
    client.onMerged((evt) => live.push(evt.seq));
    client.subscribe(500n);
    const standing = await replayPeer(store, 1);

    // Act: a replay runs while the live tail delivers.
    const done = client.replay({ fromSeq: 0n, toSeq: 10n, maxEvents: 0, idleMs: 200, onEvent: () => {} });
    const replay = await replayPeer(store, 2);
    standing.send(EventSchema, storeEvent(501));
    replay.send(EventSchema, storeEvent(10));
    await done;
    await until(() => live.length === 1);

    // Assert
    expect(live).toEqual([501n]);
  });

  it("never routes replayed events into the live sink", async () => {
    // Arrange: the live sink forwards to the daemon as Event; a replayed event
    // reaching it would be indistinguishable from live state.
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);
    const live: bigint[] = [];
    client.onMerged((evt) => live.push(evt.seq));

    // Act
    const done = client.replay({ fromSeq: 0n, toSeq: 5n, maxEvents: 0, idleMs: 200, onEvent: () => {} });
    const peer = await replayPeer(store, 1);
    peer.send(EventSchema, storeEvent(1));
    peer.send(EventSchema, storeEvent(5));
    await done;

    // Assert
    expect(live).toEqual([]);
  });

  it("streams the events below to_seq to the caller", async () => {
    // Arrange
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);
    const got: bigint[] = [];

    // Act
    const done = client.replay({ fromSeq: 0n, toSeq: 3n, maxEvents: 0, idleMs: 200, onEvent: (e) => got.push(e.seq) });
    const peer = await replayPeer(store, 1);
    peer.send(EventSchema, storeEvent(1));
    peer.send(EventSchema, storeEvent(2));
    peer.send(EventSchema, storeEvent(3));
    await done;

    // Assert
    expect(got).toEqual([1n, 2n]);
  });

  it("stops at to_seq without truncating", async () => {
    // Arrange
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);

    // Act
    const done = client.replay({ fromSeq: 0n, toSeq: 3n, maxEvents: 0, idleMs: 200, onEvent: () => {} });
    const peer = await replayPeer(store, 1);
    peer.send(EventSchema, storeEvent(3));
    const outcome = await done;

    // Assert
    expect(outcome).toEqual({ delivered: 0, truncated: false, reason: "" });
  });

  it("closes its subscription when the range completes", async () => {
    // Arrange: a replay that outlived its range would keep live-tailing the
    // store forever, one leaked connection per frontend mount.
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);
    let closed = false;

    // Act
    const done = client.replay({ fromSeq: 0n, toSeq: 3n, maxEvents: 0, idleMs: 200, onEvent: () => {} });
    const peer = await replayPeer(store, 1);
    peer.socket.on("close", () => {
      closed = true;
    });
    peer.send(EventSchema, storeEvent(3));
    await done;
    await until(() => closed);

    // Assert
    expect(closed).toBe(true);
  });

  it("truncates at the event cap", async () => {
    // Arrange
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);

    // Act
    const done = client.replay({ fromSeq: 0n, toSeq: 999n, maxEvents: 2, idleMs: 5000, onEvent: () => {} });
    const peer = await replayPeer(store, 1);
    peer.send(EventSchema, storeEvent(1));
    peer.send(EventSchema, storeEvent(2));
    peer.send(EventSchema, storeEvent(3));
    const outcome = await done;

    // Assert
    expect(outcome.truncated).toBe(true);
    expect(outcome.delivered).toBe(2);
  });

  it("truncates when the subscription goes idle before to_seq", async () => {
    // Arrange
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);

    // Act
    const done = client.replay({ fromSeq: 0n, toSeq: 999n, maxEvents: 0, idleMs: 60, onEvent: () => {} });
    const peer = await replayPeer(store, 1);
    peer.send(EventSchema, storeEvent(1));
    const outcome = await done;

    // Assert
    expect(outcome.truncated).toBe(true);
    expect(outcome.delivered).toBe(1);
  });

  it("completes cleanly when an unbounded replay drains", async () => {
    // Arrange: to_seq 0 means "stream until the replay drains", so a quiet
    // subscription IS the whole answer rather than a shortfall.
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);

    // Act
    const done = client.replay({ fromSeq: 0n, toSeq: 0n, maxEvents: 0, idleMs: 60, onEvent: () => {} });
    const peer = await replayPeer(store, 1);
    peer.send(EventSchema, storeEvent(1));
    const outcome = await done;

    // Assert
    expect(outcome).toEqual({ delivered: 1, truncated: false, reason: "" });
  });

  it("truncates when the store drops the replay subscription", async () => {
    // Arrange
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);

    // Act
    const done = client.replay({ fromSeq: 0n, toSeq: 999n, maxEvents: 0, idleMs: 5000, onEvent: () => {} });
    const peer = await replayPeer(store, 1);
    peer.destroy();
    const outcome = await done;

    // Assert
    expect(outcome.truncated).toBe(true);
  });

  it("truncates rather than throwing when the store socket is gone", async () => {
    // Arrange: a dial failure is a failed replay, reported — never a retry
    // loop, and never an unhandled rejection the caller cannot close on.
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);
    store.close();
    stores.splice(stores.indexOf(store), 1);

    // Act
    const outcome = await client.replay({
      fromSeq: 0n,
      toSeq: 9n,
      maxEvents: 0,
      idleMs: 200,
      onEvent: () => {},
    });

    // Assert
    expect(outcome.truncated).toBe(true);
  });

  it("subscribes under the adopted vendor store key", async () => {
    // Arrange: the store keys events by the vendor uuid, so a replay under any
    // other id would subscribe to a channel nothing publishes to.
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);
    client.adoptStoreKey("vendor-uuid");

    // Act
    const done = client.replay({ fromSeq: 0n, toSeq: 3n, maxEvents: 0, idleMs: 200, onEvent: () => {} });
    await until(() => store.conns.length > 1);
    const sub = await store.conns[1]!.next(SubscribeSchema);
    store.conns[1]!.send(EventSchema, storeEvent(3));
    await done;

    // Assert
    expect(sub.sessionId).toBe("vendor-uuid");
  });
});
