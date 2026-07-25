import { afterEach, describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { SessionServer, SessionServerHandlers, Receipt } from "../src/uds/server.js";
import {
  AckSchema,
  DaemonHelloSchema,
  EventSchema,
  InterruptSchema,
  NackSchema,
  PermissionRequestSchema,
  PermissionResponseSchema,
  ShimHelloSchema,
  SubmitPromptSchema,
  SubscribeSchema,
} from "../src/uds/proto.js";
import type {
  Interrupt,
  PermissionResponse,
  SubmitPrompt,
  Subscribe,
} from "../src/uds/proto.js";
import { FramedPeer, acceptShim, tmpSocketPath, until } from "./uds-harness.js";

interface Calls {
  prompts: SubmitPrompt[];
  interrupts: Interrupt[];
  perms: PermissionResponse[];
  subs: Subscribe[];
  connected: number;
  disconnected: number;
}

function harness(overrides: Partial<SessionServerHandlers> = {}): {
  server: SessionServer;
  socketPath: string;
  calls: Calls;
} {
  const socketPath = tmpSocketPath();
  const calls: Calls = { prompts: [], interrupts: [], perms: [], subs: [], connected: 0, disconnected: 0 };
  const handlers: SessionServerHandlers = {
    onSubmitPrompt: (m): Receipt => {
      calls.prompts.push(m);
      return create(AckSchema, { requestId: m.requestId });
    },
    onInterrupt: (m): Receipt => {
      calls.interrupts.push(m);
      return create(AckSchema, { requestId: m.requestId });
    },
    onPermissionResponse: (m) => calls.perms.push(m),
    onSubscribe: (m) => calls.subs.push(m),
    onDaemonConnected: () => calls.connected++,
    onDaemonDisconnected: () => calls.disconnected++,
    ...overrides,
  };
  const server = new SessionServer(
    { socketPath, sessionId: "sess-1", shimVersion: "9.9", protocolVersion: "1", heartbeatIntervalMs: 0 },
    handlers,
  );
  return { server, socketPath, calls };
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
async function handshake(server: SessionServer, socketPath: string): Promise<FramedPeer> {
  const daemon = acceptShim(socketPath);
  listeners.push(daemon.close);
  await server.connect();
  const peer = await daemon.next();
  const hello = await peer.next(ShimHelloSchema);
  expect(hello.sessionId).toBe("sess-1");
  peer.send(DaemonHelloSchema, create(DaemonHelloSchema, { daemonVersion: "d1", protocolVersion: "1" }));
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
    peer.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "early" }));
    await new Promise<void>((r) => setImmediate(r));
    // Assert
    expect(calls.prompts).toHaveLength(0);
    expect(server.isConnected()).toBe(false);
  });
});

describe("SessionServer control dispatch", () => {
  it("dispatches SubmitPrompt and Acks", async () => {
    // Arrange
    const { server, socketPath, calls } = harness();
    track(server);
    const peer = await handshake(server, socketPath);
    // Act
    peer.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p1", text: "hi" }));
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
    peer.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "p2" }));
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
    peer.send(InterruptSchema, create(InterruptSchema, { requestId: "i1", hard: true }));
    await peer.next(AckSchema);
    // Assert
    expect(calls.interrupts[0]!.hard).toBe(true);
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

  it("dispatches Subscribe", async () => {
    // Arrange
    const { server, socketPath, calls } = harness();
    track(server);
    const peer = await handshake(server, socketPath);
    // Act
    peer.send(SubscribeSchema, create(SubscribeSchema, { sessionId: "sess-1", fromSeq: 7n }));
    await until(() => calls.subs.length === 1);
    // Assert
    expect(calls.subs[0]!.fromSeq).toBe(7n);
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

  it("forwards a PermissionRequest to the daemon", async () => {
    // Arrange
    const { server, socketPath } = harness();
    track(server);
    const peer = await handshake(server, socketPath);
    await until(() => server.isConnected());
    // Act
    server.sendPermissionRequest(create(PermissionRequestSchema, { requestId: "pr-1", toolName: "Bash" }));
    const req = await peer.next(PermissionRequestSchema);
    // Assert
    expect(req.toolName).toBe("Bash");
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

  // The "two daemons connect, newest wins" case is gone from this side: the
  // shim owns ONE outbound connection, so it cannot be connected twice.
  // Superseding a stale connection is now the daemon listener's job and is
  // covered there (shimlisten: reconnect supersedes the parked connection).
});
