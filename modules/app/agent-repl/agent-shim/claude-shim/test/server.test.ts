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
import { FramedPeer, connectPeer, tmpSocketPath, until } from "./uds-harness.js";

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

const servers: SessionServer[] = [];
function track(s: SessionServer): SessionServer {
  servers.push(s);
  return s;
}
afterEach(async () => {
  await Promise.all(servers.splice(0).map((s) => s.close()));
});

/** Connect a daemon peer and complete the ShimHello/DaemonHello handshake. */
async function handshake(server: SessionServer, socketPath: string): Promise<FramedPeer> {
  await server.listen();
  const peer = await connectPeer(socketPath);
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
    await server.listen();
    // Act
    const peer = await connectPeer(socketPath);
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
    await server.listen();
    const peer = await connectPeer(socketPath);
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
    await server.listen();
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
  it("survives a daemon drop without tearing down, and re-handshakes on reconnect", async () => {
    // Arrange
    const { server, socketPath, calls } = harness();
    track(server);
    const peer1 = await handshake(server, socketPath);
    await until(() => server.isConnected());
    // Act: daemon vanishes
    peer1.destroy();
    await until(() => calls.disconnected === 1);
    // Assert: nothing torn down, still listening
    expect(server.isConnected()).toBe(false);
    // A new daemon connection re-handshakes on the SAME live server
    const peer2 = await connectPeer(socketPath);
    const hello = await peer2.next(ShimHelloSchema);
    expect(hello.sessionId).toBe("sess-1");
    peer2.send(DaemonHelloSchema, create(DaemonHelloSchema, { daemonVersion: "d2" }));
    await until(() => calls.connected === 2);
    expect(server.isConnected()).toBe(true);
  });

  it("supersedes an older connection when a new one arrives", async () => {
    // Arrange
    const { server, socketPath, calls } = harness();
    track(server);
    const peer1 = await handshake(server, socketPath);
    await until(() => calls.connected === 1);
    // Act: a second daemon connects without the first dropping
    const peer2 = await connectPeer(socketPath);
    await peer2.next(ShimHelloSchema);
    peer2.send(DaemonHelloSchema, create(DaemonHelloSchema, { daemonVersion: "d2" }));
    await until(() => calls.connected === 2);
    // Assert: the newest connection is the live one
    peer2.send(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "on-2" }));
    const ack = await peer2.next(AckSchema);
    expect(ack.requestId).toBe("on-2");
    peer1.destroy();
  });
});
