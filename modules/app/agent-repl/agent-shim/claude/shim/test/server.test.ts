import { afterEach, describe, expect, it, vi } from "vitest";
import { writeSync } from "node:fs";
import { create } from "@bufbuild/protobuf";
import { SessionServer, SessionServerHandlers, Receipt } from "../src/uds/server.js";
import type { AsyncReceipt } from "../src/uds/server.js";
import type { SessionServerOptions } from "../src/uds/server.js";
import {
  AckSchema,
  CancelDetachedAgentsSchema,
  DaemonHelloSchema,
  EventClass,
  EventSchema,
  HealthCheckSchema,
  HealthStatusSchema,
  InterruptSchema,
  NackSchema,
  PermissionRequestSchema,
  PermissionResponseSchema,
  ReplayDoneSchema,
  ReplayEventSchema,
  ReplayRequestSchema,
  PromptOrigin,
  QueryRuntimeIdentitySchema,
  ShimHelloSchema,
  ShimReadySchema,
  SubmitPromptSchema,
} from "../src/uds/proto.js";
import type {
  CancelDetachedAgents,
  DaemonHello,
  Interrupt,
  PermissionResponse,
  ReplayRequest,
  SubmitPrompt,
} from "../src/uds/proto.js";
import { FramedPeer, acceptShim, tmpSocketPath, until } from "./uds-harness.js";

interface Calls {
  prompts: SubmitPrompt[];
  interrupts: Interrupt[];
  cancelDetached: CancelDetachedAgents[];
  perms: PermissionResponse[];
  /** Every DaemonHello the bring-up gate handed to onDaemonConnected. */
  hellos: DaemonHello[];
  replays: ReplayRequest[];
  connected: number;
  disconnected: number;
}

function harness(
  overrides: Partial<SessionServerHandlers> = {},
  options: Partial<SessionServerOptions> = {},
): {
  server: SessionServer;
  socketPath: string;
  calls: Calls;
} {
  const socketPath = tmpSocketPath();
  const calls: Calls = { prompts: [], interrupts: [], cancelDetached: [], perms: [], hellos: [], replays: [], connected: 0, disconnected: 0 };
  const handlers: SessionServerHandlers = {
    onSubmitPrompt: (m): Receipt => {
      calls.prompts.push(m);
      return create(AckSchema, { requestId: m.requestId });
    },
    onInterrupt: (m): Receipt => {
      calls.interrupts.push(m);
      return create(AckSchema, { requestId: m.requestId });
    },
    onCancelDetachedAgents: (m): AsyncReceipt => {
      calls.cancelDetached.push(m);
      return create(AckSchema, { requestId: m.requestId });
    },
    onSetModel: async (m) => create(AckSchema, { requestId: m.requestId, selectedModel: m.model }),
        onQuerySelectedModel: (m) => create(AckSchema, { requestId: m.requestId, selectedModel: "claude-sonnet-5" }),
    onPermissionResponse: (m) => calls.perms.push(m),
    onReplayRequest: (m) => calls.replays.push(m),
    onHealthCheck: (m) => create(HealthStatusSchema, {
      requestId: m.requestId,
      healthy: true,
      component: "test-shim",
    }),
    onDaemonConnected: (hello) => {
      calls.hellos.push(hello);
      calls.connected++;
    },
    onDaemonDisconnected: () => calls.disconnected++,
    ...overrides,
  };
  const server = new SessionServer(
    { socketPath, sessionId: "sess-1", queryInstanceId: "query-1", queryCreatedSeq: () => 41n, shimVersion: "9.9", protocolVersion: "1", heartbeatIntervalMs: 0, ...options },
    handlers,
  );
  return { server, socketPath, calls };
}

/**
 * Read back the JSONL records the shim persisted to inherited fd 3 from this
 * point on. `test/log-setup.ts` mocks `node:fs`'s `writeSync`, which is the
 * canonical sink's only write, so the mock's calls ARE the durable records.
 */
function persistedRecords(): () => Record<string, unknown>[] {
  const mocked = vi.mocked(writeSync);
  mocked.mockClear();
  return () =>
    (mocked.mock.calls as unknown as Array<[number, Buffer, number, number]>).map(
      ([, bytes, offset, length]) =>
        JSON.parse(bytes.subarray(offset, offset + length).toString("utf8")) as Record<string, unknown>,
    );
}

const listeners: Array<() => void> = [];
const servers: SessionServer[] = [];
function track(s: SessionServer): SessionServer {
  servers.push(s);
  return s;
}
afterEach(async () => {
  await Promise.all(servers.splice(0).map((s) => s.close()));
  listeners.splice(0).forEach((close) => close());
});

/**
 * Stand up a fake daemon, let the shim dial in, and complete the
 * ShimHello/DaemonHello handshake. The shim speaks first, as the dialer.
 */
async function handshake(server: SessionServer, socketPath: string, fromSeq = 0n): Promise<FramedPeer> {
  const daemon = acceptShim(socketPath);
  listeners.push(daemon.close);
  await server.connect();
  const peer = await daemon.next();
  const hello = await peer.next(ShimHelloSchema);
  expect(hello.sessionId).toBe("sess-1");
  peer.send(DaemonHelloSchema, create(DaemonHelloSchema, { daemonVersion: "d1", protocolVersion: "1", fromSeq }));
  return peer;
}

describe("SessionServer handshake", () => {
  it("speaks ShimHello first on accept", async () => {
    // Arrange
    const { server, socketPath } = harness();
    track(server);
    const daemon = acceptShim(socketPath);
    listeners.push(daemon.close);
    // Act
    await server.connect();
    const peer = await daemon.next();
    const hello = await peer.next(ShimHelloSchema);
    // Assert
    expect(hello.sessionId).toBe("sess-1");
    expect(hello.vendor).toBe("claude");
    expect(hello.shimVersion).toBe("9.9");
    expect(hello.queryCreatedSeq).toBe(41n);
  });

  it("reannounces the stable query identity and runtime snapshot after a reconnect", async () => {
    const runtime = create(QueryRuntimeIdentitySchema, { vendorSessionId: "vendor-1", effectiveModel: "opus" });
    const { server, socketPath } = harness({}, {
      queryInstanceId: "query-running",
      queryRuntimeIdentity: () => runtime,
      reconnectMinMs: 1,
    });
    track(server);
    const daemon = acceptShim(socketPath);
    listeners.push(daemon.close);

    await server.connect();
    const first = await daemon.next();
    const firstHello = await first.next(ShimHelloSchema);
    first.send(DaemonHelloSchema, create(DaemonHelloSchema, { daemonVersion: "d1", protocolVersion: "1" }));
    first.destroy();

    const second = await daemon.next();
    const secondHello = await second.next(ShimHelloSchema);
    expect(firstHello.queryInstanceId).toBe("query-running");
    expect(secondHello.queryInstanceId).toBe("query-running");
    expect(firstHello.queryRuntimeIdentity).toEqual(runtime);
    expect(secondHello.queryRuntimeIdentity).toEqual(runtime);
  });

  it("marks connected after the DaemonHello reply", async () => {
    // Arrange
    const { server, socketPath, calls } = harness();
    track(server);
    // Act
    await handshake(server, socketPath);
    await until(() => calls.connected === 1);
    // Assert
    expect(server.isConnected()).toBe(true);
  });

  it("ignores control messages received before the DaemonHello", async () => {
    // Arrange
    const { server, socketPath, calls } = harness();
    track(server);
    const daemon = acceptShim(socketPath);
    listeners.push(daemon.close);
    await server.connect();
    const peer = await daemon.next();
    await peer.next(ShimHelloSchema);
    // Act: jump the gun with a prompt before saying hello
    peer.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "early", promptOrigin: PromptOrigin.USER_SENT }));
    await new Promise<void>((r) => setImmediate(r));
    // Assert
    expect(calls.prompts).toHaveLength(0);
    expect(server.isConnected()).toBe(false);
  });
});

describe("SessionServer control dispatch", () => {
  it("returns a correlated health assertion only after handshake", async () => {
    // Arrange
    const { server, socketPath } = harness();
    track(server);
    const peer = await handshake(server, socketPath);

    // Act
    peer.send(HealthCheckSchema, create(HealthCheckSchema, { requestId: "daemon-health-1" }));
    const status = await peer.next(HealthStatusSchema);

    // Assert
    expect(status.requestId).toBe("daemon-health-1");
    expect(status.healthy).toBe(true);
    expect(status.component).toBe("test-shim");
  });

  it("reports an unhealthy status when the health handler rejects", async () => {
    // Arrange
    const { server, socketPath } = harness({
      onHealthCheck: async () => { throw new Error("store subscription is down"); },
    });
    track(server);
    const peer = await handshake(server, socketPath);

    // Act
    peer.send(HealthCheckSchema, create(HealthCheckSchema, { requestId: "daemon-health-2" }));
    const status = await peer.next(HealthStatusSchema);

    // Assert: a failed assertion is explicit, never a caller-side timeout.
    expect(status.requestId).toBe("daemon-health-2");
    expect(status.healthy).toBe(false);
    expect(status.reason).toContain("store subscription is down");
  });

  it("dispatches SubmitPrompt and Acks", async () => {
    // Arrange
    const { server, socketPath, calls } = harness();
    track(server);
    const peer = await handshake(server, socketPath);
    // Act
    peer.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "hi", promptOrigin: PromptOrigin.USER_SENT }));
    const ack = await peer.next(AckSchema);
    // Assert
    expect(ack.requestId).toBe("p1");
    expect(calls.prompts.map((p) => p.requestId)).toEqual(["p1"]);
  });

  it("relays a Nack receipt from the handler", async () => {
    // Arrange
    const { server, socketPath } = harness({
      onSubmitPrompt: (m): Receipt => create(NackSchema, { requestId: m.requestId, reason: "busy" }),
    });
    track(server);
    const peer = await handshake(server, socketPath);
    // Act
    peer.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p2", promptOrigin: PromptOrigin.USER_SENT }));
    const nack = await peer.next(NackSchema);
    // Assert
    expect(nack.reason).toBe("busy");
  });

  it("dispatches Interrupt", async () => {
    // Arrange
    const { server, socketPath, calls } = harness();
    track(server);
    const peer = await handshake(server, socketPath);
    // Act
    peer.send(InterruptSchema, create(InterruptSchema, { requestId: "i1" }));
    await peer.next(AckSchema);
    // Assert
    expect(calls.interrupts[0]!.requestId).toBe("i1");
  });

  it("dispatches PermissionResponse", async () => {
    // Arrange
    const { server, socketPath, calls } = harness();
    track(server);
    const peer = await handshake(server, socketPath);
    // Act
    peer.send(PermissionResponseSchema, create(PermissionResponseSchema, { requestId: "perm-1" }));
    await until(() => calls.perms.length === 1);
    // Assert
    expect(calls.perms[0]!.requestId).toBe("perm-1");
  });

});

describe("SessionServer bring-up gate", () => {
  it("hands the DaemonHello's from_seq to the wiring stage", async () => {
    // Arrange: from_seq is the daemon's resume position, and the ONLY place
    // the shim learns it now that the separate Subscribe frame is gone.
    const { server, socketPath, calls } = harness();
    track(server);
    // Act
    await handshake(server, socketPath, 7n);
    await until(() => calls.hellos.length === 1);
    // Assert
    expect(calls.hellos[0]!.fromSeq).toBe(7n);
  });

  it("writes the ShimReady ack that closes the gate", async () => {
    // Arrange
    const { server, socketPath } = harness();
    track(server);
    const peer = await handshake(server, socketPath, 4n);
    await until(() => server.isConnected());
    // Act: the owner finished its wiring.
    server.sendReady(create(ShimReadySchema, { sessionId: "sess-1", fromSeq: 4n, vendorSessionId: "uuid-1" }));
    const ready = await peer.next(ShimReadySchema);
    // Assert
    expect(ready).toMatchObject({ sessionId: "sess-1", fromSeq: 4n, vendorSessionId: "uuid-1" });
  });

  it("abandons the ack when the daemon went away before the wiring finished", async () => {
    // Arrange: the connection drops mid-wiring, which is exactly when a
    // speculative ack would be worst — the next gate re-runs in full instead.
    const { server, socketPath } = harness();
    track(server);
    const peer = await handshake(server, socketPath, 0n);
    await until(() => server.isConnected());
    peer.destroy();
    await until(() => !server.isConnected());
    // Act + Assert: no throw, and nothing to send it on.
    server.sendReady(create(ShimReadySchema, { sessionId: "sess-1" }));
    expect(server.isConnected()).toBe(false);
  });
});

describe("SessionServer outbound", () => {
  it("forwards an Event to the connected daemon", async () => {
    // Arrange
    const { server, socketPath } = harness();
    track(server);
    const peer = await handshake(server, socketPath);
    await until(() => server.isConnected());
    // Act
    server.sendEvent(create(EventSchema, { sessionId: "sess-1", seq: 3n }));
    const evt = await peer.next(EventSchema);
    // Assert
    expect(evt.seq).toBe(3n);
  });

  it("drops an Event (no throw) when no daemon is attached", async () => {
    // Arrange
    const { server } = harness();
    track(server);
    // Act / Assert: durable in the store, replays on resubscribe
    expect(() => server.sendEvent(create(EventSchema, { sessionId: "sess-1", seq: 1n }))).not.toThrow();
    expect(server.isConnected()).toBe(false);
  });

  it("keeps a detached PERSISTENT event at info, since the store still replays it", async () => {
    // Arrange
    const { server } = harness();
    track(server);
    const records = persistedRecords();
    // Act
    server.sendEvent(create(EventSchema, { sessionId: "sess-1", seq: 1n, class: EventClass.PERSISTENT }));
    // Assert
    expect(records().filter((r) => r.message === "no daemon attached; event not forwarded (durable in store, replays on resubscribe)").map((r) => r.level)).toEqual(["info"]);
  });

  it("records a detached EPHEMERAL event as an error, since nothing replays it", async () => {
    // Arrange — the class a DegradedState rides: seq 0, never persisted.
    const { server } = harness();
    track(server);
    const records = persistedRecords();
    // Act
    server.sendEvent(create(EventSchema, { sessionId: "sess-1", seq: 0n, class: EventClass.EPHEMERAL }));
    // Assert
    const dropped = records().filter((r) => String(r.message).includes("EPHEMERAL event dropped permanently"));
    expect(dropped).toHaveLength(1);
    expect(dropped[0].level).toBe("error");
  });

  it("drops a replay event with no daemon attached, naming the daemon-side retirement", async () => {
    // Arrange — the daemon retires the request on its own teardown and
    // re-issues it after the next ShimReady, so this is expected, not a hole.
    const { server } = harness();
    track(server);
    const records = persistedRecords();
    // Act
    server.sendReplayEvent("req-1", create(EventSchema, { sessionId: "sess-1", seq: 7n }));
    // Assert
    const dropped = records().filter((r) => String(r.message).includes("re-issues it after the next ShimReady"));
    expect(dropped).toHaveLength(1);
  });

  it("drops a replay completion with no daemon attached rather than leaving it unwritten and unaccounted", async () => {
    // Arrange
    const { server } = harness();
    track(server);
    const records = persistedRecords();
    // Act
    server.sendReplayDone(create(ReplayDoneSchema, { requestId: "req-1" }));
    // Assert
    const dropped = records().filter((r) => String(r.message).includes("re-issues it after the next ShimReady"));
    expect(dropped).toHaveLength(1);
  });

  it("forwards a PermissionRequest to the daemon", async () => {
    // Arrange
    const { server, socketPath } = harness();
    track(server);
    const peer = await handshake(server, socketPath);
    await until(() => server.isConnected());
    // Act
    const delivered = server.sendPermissionRequest(create(PermissionRequestSchema, { requestId: "pr-1", toolName: "Bash" }));
    const req = await peer.next(PermissionRequestSchema);
    // Assert
    expect(req.toolName).toBe("Bash");
    expect(delivered).toBe(true);
  });

  it("reports a PermissionRequest undelivered when no daemon is attached", () => {
    // Arrange: never connected, so there is nobody to answer.
    const { server } = harness();
    track(server);
    // Act
    const delivered = server.sendPermissionRequest(create(PermissionRequestSchema, { requestId: "pr-2", toolName: "Bash" }));
    // Assert
    expect(delivered).toBe(false);
  });

  it("logs an undelivered PermissionRequest as an error naming the re-send", () => {
    // Arrange
    const { server } = harness();
    track(server);
    const records = persistedRecords();
    // Act
    server.sendPermissionRequest(create(PermissionRequestSchema, { requestId: "pr-3", toolName: "Bash" }));
    // Assert
    const dropped = records().filter((r) => String(r.message).includes("permission request cannot be delivered"));
    expect(dropped).toHaveLength(1);
    expect(dropped[0].level).toBe("error");
  });
});

describe("SessionServer disconnect tolerance", () => {
  it("survives a daemon drop and redials, re-handshaking on the same session", async () => {
    // Arrange: the shim is the DIALER now, so recovering the link is its job.
    const { server, socketPath, calls } = harness();
    track(server);
    const daemon = acceptShim(socketPath);
    listeners.push(daemon.close);
    await server.connect();
    const peer1 = await daemon.next();
    await peer1.next(ShimHelloSchema);
    peer1.send(DaemonHelloSchema, create(DaemonHelloSchema, { daemonVersion: "d1", protocolVersion: "1" }));
    await until(() => server.isConnected());

    // Act: the daemon vanishes.
    peer1.destroy();
    await until(() => calls.disconnected === 1);

    // Assert: nothing is torn down, and the shim comes back on its own.
    expect(server.isConnected()).toBe(false);
    const peer2 = await daemon.next();
    const hello = await peer2.next(ShimHelloSchema);
    expect(hello.sessionId).toBe("sess-1");
    peer2.send(DaemonHelloSchema, create(DaemonHelloSchema, { daemonVersion: "d2", protocolVersion: "1" }));
    await until(() => calls.connected === 2);
    expect(server.isConnected()).toBe(true);
  });

  it("completes an in-flight interrupt locally when the connection dies before its receipt", async () => {
    // Arrange: an interrupt whose handler runs to completion — the SDK abort —
    // against a connection that is gone by the time the Ack is due.
    const { server, socketPath, calls } = harness({
      onInterrupt: (m): Receipt => {
        calls.interrupts.push(m);
        void server.close();
        return create(AckSchema, { requestId: m.requestId });
      },
    });
    track(server);
    const peer = await handshake(server, socketPath);
    const records = persistedRecords();

    // Act.
    peer.send(InterruptSchema, create(InterruptSchema, { requestId: "int-1" }));
    await until(() => calls.interrupts.length === 1);

    // Assert: the interrupt happened; only its answer was lost.
    expect(calls.interrupts[0]?.requestId).toBe("int-1");
    await until(() => records().some((r) => String(r.message ?? "").includes("control receipt DROPPED")));
  });

  it("records a dropped receipt at error level so the lost outcome is not silent", async () => {
    // Arrange: the same window, asserted on the record's severity — a lost
    // outcome for work that already happened is a divergence, not a nicety.
    const { server, socketPath, calls } = harness({
      onInterrupt: (m): Receipt => {
        calls.interrupts.push(m);
        void server.close();
        return create(AckSchema, { requestId: m.requestId });
      },
    });
    track(server);
    const peer = await handshake(server, socketPath);
    const records = persistedRecords();

    // Act.
    peer.send(InterruptSchema, create(InterruptSchema, { requestId: "int-1" }));
    await until(() => records().some((r) => String(r.message ?? "").includes("control receipt DROPPED")));

    // Assert.
    const dropped = records().find((r) => String(r.message ?? "").includes("control receipt DROPPED"));
    expect(dropped?.level).toBe("error");
    expect(dropped?.request_id).toBe("int-1");
  });

  // The "two daemons connect, newest wins" case is gone from this side: the
  // shim owns ONE outbound connection, so it cannot be connected twice.
  // Superseding a stale connection is now the daemon listener's job and is
  // covered there (shimlisten: reconnect supersedes the parked connection).
});

describe("SessionServer replay lifetime is its connection's", () => {
  // A ReplayRequest is a question ONE daemon connection asked. When that
  // connection dies the question dies with it — the daemon retires it on
  // teardown and re-issues it after the next ShimReady — so the shim must not
  // finish answering it to whoever happens to be attached later.

  /** Handshake, deliver a ReplayRequest, and return that connection's peer. */
  async function requestReplay(
    server: SessionServer,
    socketPath: string,
    requestId: string,
  ): Promise<FramedPeer> {
    const peer = await handshake(server, socketPath);
    await until(() => server.isConnected());
    peer.send(ReplayRequestSchema, create(ReplayRequestSchema, { requestId, fromSeq: 0n }));
    return peer;
  }

  it("answers a replay on the connection that asked for it", async () => {
    // Arrange
    const { server, socketPath, calls } = harness();
    track(server);
    const peer = await requestReplay(server, socketPath, "req-live");
    await until(() => calls.replays.length === 1, "the replay request to arrive");

    // Act
    server.sendReplayEvent("req-live", create(EventSchema, { sessionId: "sess-1", seq: 7n }));

    // Assert
    expect((await peer.next(ReplayEventSchema)).requestId).toBe("req-live");
  });

  it("never writes a superseded connection's replay event to the new connection", async () => {
    // Arrange — the request arrives, its connection dies, a new one handshakes.
    const { server, socketPath, calls } = harness({}, { reconnectMinMs: 1 });
    track(server);
    const daemon = acceptShim(socketPath);
    listeners.push(daemon.close);
    await server.connect();
    const peer1 = await daemon.next();
    await peer1.next(ShimHelloSchema);
    peer1.send(DaemonHelloSchema, create(DaemonHelloSchema, { daemonVersion: "d1", protocolVersion: "1" }));
    await until(() => server.isConnected());
    peer1.send(ReplayRequestSchema, create(ReplayRequestSchema, { requestId: "req-stale", fromSeq: 0n }));
    await until(() => calls.replays.length === 1, "the replay request to arrive");
    peer1.destroy();
    const peer2 = await daemon.next();
    await peer2.next(ShimHelloSchema);
    peer2.send(DaemonHelloSchema, create(DaemonHelloSchema, { daemonVersion: "d1", protocolVersion: "1" }));
    await until(() => calls.connected === 2, "the reattach handshake");
    const records = persistedRecords();

    // Act — the shim's replay loop, still running, emits into the new link.
    server.sendReplayEvent("req-stale", create(EventSchema, { sessionId: "sess-1", seq: 7n }));

    // Assert — dropped as belonging to a superseded connection.
    const dropped = records().filter((r) => String(r.message).includes("SUPERSEDED daemon connection"));
    expect(dropped).toHaveLength(1);
  });

  it("records every replay stranded by a connection teardown", async () => {
    // Arrange
    const { server, socketPath, calls } = harness({}, { reconnectMinMs: 1 });
    track(server);
    const peer = await requestReplay(server, socketPath, "req-stranded");
    await until(() => calls.replays.length === 1, "the replay request to arrive");
    const records = persistedRecords();

    // Act
    peer.destroy();
    await until(() => calls.disconnected === 1, "the teardown");

    // Assert
    const stranded = records().filter((r) => String(r.message).includes("in-flight replay request(s)"));
    expect(stranded).toHaveLength(1);
    expect((stranded[0].context as Record<string, unknown>).request_ids).toEqual(["req-stranded"]);
  });
});

describe("SessionServer vendor session rotation bounce", () => {
  // The vendor rotates its session uuid mid-stream; the store client re-keys
  // and asks for a DELIBERATE bounce so the next handshake announces the new
  // identity. Unlike close(), the reconnect loop stays armed.

  /** A server whose hello reports whatever `vendor` currently holds. */
  function rotatingHarness(vendor: { id: string }): { server: SessionServer; socketPath: string; calls: Calls } {
    const built = harness();
    const server = new SessionServer(
      {
        socketPath: built.socketPath, sessionId: "sess-1", queryInstanceId: "query-1", shimVersion: "9.9",
        protocolVersion: "1", heartbeatIntervalMs: 0, reconnectMinMs: 1,
        queryCreatedSeq: () => 41n,
        vendorSessionId: () => vendor.id,
      },
      {
        onSubmitPrompt: (m): Receipt => create(AckSchema, { requestId: m.requestId }),
        onInterrupt: (m): Receipt => create(AckSchema, { requestId: m.requestId }),
        onCancelDetachedAgents: (m): Receipt => create(AckSchema, { requestId: m.requestId }),
        onSetModel: async (m) => create(AckSchema, { requestId: m.requestId, selectedModel: m.model }),
        onQuerySelectedModel: (m) => create(AckSchema, { requestId: m.requestId, selectedModel: "claude-sonnet-5" }),
        onPermissionResponse: () => {},
        onReplayRequest: () => {},
        onHealthCheck: (m) => create(HealthStatusSchema, { requestId: m.requestId, healthy: true, component: "test-shim" }),
        onDaemonConnected: () => built.calls.connected++,
        onDaemonDisconnected: () => built.calls.disconnected++,
      },
    );
    return { server, socketPath: built.socketPath, calls: built.calls };
  }

  it("announces the current vendor session id on every hello", async () => {
    // Arrange
    const vendor = { id: "uuid-old" };
    const { server, socketPath } = rotatingHarness(vendor);
    track(server);
    const daemon = acceptShim(socketPath);
    listeners.push(daemon.close);

    // Act
    await server.connect();
    const peer = await daemon.next();

    // Assert
    expect((await peer.next(ShimHelloSchema)).vendorSessionId).toBe("uuid-old");
  });

  it("re-handshakes with the ROTATED uuid after a bounce", async () => {
    // Arrange: a live, handshaked link under the old identity.
    const vendor = { id: "uuid-old" };
    const { server, socketPath, calls } = rotatingHarness(vendor);
    track(server);
    const daemon = acceptShim(socketPath);
    listeners.push(daemon.close);
    await server.connect();
    const peer1 = await daemon.next();
    await peer1.next(ShimHelloSchema);
    peer1.send(DaemonHelloSchema, create(DaemonHelloSchema, { daemonVersion: "d1", protocolVersion: "1" }));
    await until(() => server.isConnected());

    // Act: the store client re-keys, then asks for the bounce.
    vendor.id = "uuid-new";
    server.bounce("vendor session rotation uuid-old -> uuid-new");

    // Assert: a NEW connection whose hello carries the new identity.
    const peer2 = await daemon.next();
    expect((await peer2.next(ShimHelloSchema)).vendorSessionId).toBe("uuid-new");
    peer2.send(DaemonHelloSchema, create(DaemonHelloSchema, { daemonVersion: "d1", protocolVersion: "1" }));
    await until(() => calls.connected === 2, "re-handshaked");
    expect(server.isConnected()).toBe(true);
  });

  it("never resurrects a deliberately closed link", async () => {
    // Arrange: shutdown already ran; a late rotation must not undo it.
    const vendor = { id: "uuid-old" };
    const { server, socketPath, calls } = rotatingHarness(vendor);
    track(server);
    const daemon = acceptShim(socketPath);
    listeners.push(daemon.close);
    await server.connect();
    const peer1 = await daemon.next();
    await peer1.next(ShimHelloSchema);
    peer1.send(DaemonHelloSchema, create(DaemonHelloSchema, { daemonVersion: "d1", protocolVersion: "1" }));
    await until(() => server.isConnected());
    await server.close();

    // Act
    vendor.id = "uuid-new";
    server.bounce("vendor session rotation after shutdown");

    // Assert
    await until(() => true, "event loop turned");
    expect(server.isConnected()).toBe(false);
    expect(calls.connected).toBe(1);
  });
});
