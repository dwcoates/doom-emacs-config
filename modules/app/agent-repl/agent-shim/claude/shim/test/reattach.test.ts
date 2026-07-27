/**
 * Reattach integration: a fake store peer feeds the StoreClient, whose merged
 * events the SessionServer forwards to a fake daemon peer. The daemon
 * subscribes, receives a prefix, DROPS mid-stream, reconnects, re-subscribes
 * from the last seq it saw, and receives the remainder with no loss and no
 * duplication. This exercises the whole G5 transport wired the way the stitch
 * phase will wire it (store→shim→daemon), and the §4.4 REATTACH contract: the
 * shim outlives the daemon and re-serves from `Subscribe{from_seq}`.
 */
import { afterEach, describe, expect, it } from "vitest";
import net from "node:net";
import { create } from "@bufbuild/protobuf";
import { SessionServer, Receipt } from "../src/uds/server.js";
import { StoreClient } from "../src/uds/store-client.js";
import {
  AckSchema,
  DaemonHelloSchema,
  EventSchema,
  HealthStatusSchema,
  ShimHelloSchema,
  SubscribeSchema,
} from "../src/uds/proto.js";
import { FramedPeer, acceptShim, tmpSocketPath, until } from "./uds-harness.js";

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

const cleanups: Array<() => void | Promise<void>> = [];
afterEach(async () => {
  for (const c of cleanups.splice(0)) await c();
});

describe("daemon reattach with from_seq continuation", () => {
  it("continues the stream after a mid-stream drop with no loss or duplication", async () => {
    // ---- Arrange: store + shim server, wired store→shim→daemon ----
    const store = await fakeStore();
    cleanups.push(() => store.close());
    const storeClient = new StoreClient({
      socketPath: store.socketPath,
      sessionId: "sess-1",
      producer: "claude-shim:sess-1",
      heartbeatIntervalMs: 0,
    });
    cleanups.push(() => storeClient.close());
    await storeClient.connect();

    // Be the daemon: the shim dials us, and redials itself after a drop.
    const socketPath = tmpSocketPath();
    const daemonListener = acceptShim(socketPath);
    cleanups.push(() => daemonListener.close());
    const server = new SessionServer(
      { socketPath, sessionId: "sess-1", shimVersion: "1", protocolVersion: "1", heartbeatIntervalMs: 0 },
      {
        onSubmitPrompt: (m): Receipt => create(AckSchema, { requestId: m.requestId }),
        onInterrupt: (m): Receipt => create(AckSchema, { requestId: m.requestId }),
        onPermissionResponse: () => {},
        // The reattach hinge: a daemon Subscribe drives a store re-subscribe.
        onSubscribe: (m) => storeClient.subscribe(m.fromSeq),
        onReplayRequest: () => {},
        onHealthCheck: (m) => create(HealthStatusSchema, {
          requestId: m.requestId,
          healthy: true,
          component: "test-shim",
        }),
      },
    );
    cleanups.push(() => server.close());
    // The forward loop: every merged store event goes to the daemon.
    storeClient.onMerged((evt) => server.sendEvent(evt));
    await server.connect();
    await until(() => {
      try {
        store.peer();
        return true;
      } catch {
        return false;
      }
    });

    // ---- Act 1: daemon connects, handshakes, subscribes from 0 ----
    const daemon1 = await daemonListener.next();
    await daemon1.next(ShimHelloSchema);
    daemon1.send(DaemonHelloSchema, create(DaemonHelloSchema, { daemonVersion: "d1" }));
    await until(() => server.isConnected());
    daemon1.send(SubscribeSchema, create(SubscribeSchema, { sessionId: "sess-1", fromSeq: 0n }));

    // The store sees the subscribe arrive on its own subscriber connection
    // (single-role store) and streams the first two events there.
    await until(() => store.count() >= 2);
    const sub1 = await store.latest().next(SubscribeSchema);
    expect(sub1.fromSeq).toBe(0n);
    store.latest().send(EventSchema, create(EventSchema, { sessionId: "sess-1", seq: 1n }));
    store.latest().send(EventSchema, create(EventSchema, { sessionId: "sess-1", seq: 2n }));

    const e1 = await daemon1.next(EventSchema);
    const e2 = await daemon1.next(EventSchema);
    expect([e1.seq, e2.seq]).toEqual([1n, 2n]);

    // ---- Act 2: daemon drops mid-stream ----
    daemon1.destroy();
    await until(() => !server.isConnected());
    // The shim (and its store connection) are untouched.
    expect(storeClient.isConnected()).toBe(true);

    // ---- Act 3: daemon reconnects and re-subscribes from the last seq ----
    const daemon2 = await daemonListener.next();
    await daemon2.next(ShimHelloSchema);
    daemon2.send(DaemonHelloSchema, create(DaemonHelloSchema, { daemonVersion: "d2" }));
    await until(() => server.isConnected());
    daemon2.send(SubscribeSchema, create(SubscribeSchema, { sessionId: "sess-1", fromSeq: 2n }));

    // The store re-serves from seq>2 (from_seq is exclusive) on a FRESH
    // subscriber connection (the client deliberately replaced the old one).
    await until(() => store.count() >= 3);
    const sub2 = await store.latest().next(SubscribeSchema);
    expect(sub2.fromSeq).toBe(2n);
    store.latest().send(EventSchema, create(EventSchema, { sessionId: "sess-1", seq: 3n }));
    store.latest().send(EventSchema, create(EventSchema, { sessionId: "sess-1", seq: 4n }));
    store.latest().send(EventSchema, create(EventSchema, { sessionId: "sess-1", seq: 5n }));

    const e3 = await daemon2.next(EventSchema);
    const e4 = await daemon2.next(EventSchema);
    const e5 = await daemon2.next(EventSchema);

    // ---- Assert: continuation, no loss, no duplication ----
    expect([e3.seq, e4.seq, e5.seq]).toEqual([3n, 4n, 5n]);
    // The reconnected daemon never re-received 1 or 2.
    expect(daemon2.count(EventSchema)).toBe(0);

    cleanups.push(() => {
      daemon2.destroy();
    });
  });
});
