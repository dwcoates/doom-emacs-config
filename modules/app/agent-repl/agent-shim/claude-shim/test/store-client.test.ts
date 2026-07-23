import { afterEach, describe, expect, it } from "vitest";
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

/** A controllable fake shim-store: one accepted connection, framed. */
interface FakeStore {
  socketPath: string;
  peer: () => FramedPeer;
  close: () => void;
}

function fakeStore(): Promise<FakeStore> {
  const socketPath = tmpSocketPath();
  let accepted: FramedPeer | null = null;
  return new Promise((resolve, reject) => {
    const server = net.createServer((socket) => {
      accepted = new FramedPeer(socket);
    });
    server.once("error", reject);
    server.listen(socketPath, () => {
      resolve({
        socketPath,
        peer: () => {
          if (!accepted) throw new Error("no store connection accepted yet");
          return accepted;
        },
        close: () => {
          accepted?.destroy();
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
  it("sends Subscribe{session_id, from_seq}", async () => {
    // Arrange
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);
    // Act
    client.subscribe(5n);
    const sub = await store.peer().next(SubscribeSchema);
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
    await store.peer().next(SubscribeSchema);
    // Act
    store.peer().send(EventSchema, create(EventSchema, { sessionId: "sess-1", seq: 1n }));
    store.peer().send(EventSchema, create(EventSchema, { sessionId: "sess-1", seq: 2n }));
    await until(() => received.length === 2);
    // Assert
    expect(received.map((e) => e.seq)).toEqual([1n, 2n]);
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
