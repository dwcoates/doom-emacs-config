import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { CommandDispatcher } from "../src/command-dispatch.js";
import type { CommandStruct } from "../src/frontend-command.js";
import { decodeFrontendFrame } from "../src/frontend-proto.js";
import { WsClient } from "../src/ws.js";
import {
  ForwardingLogger,
  bindLogContext,
  log,
  resetLoggingForTests,
  setLogger,
} from "../src/wslog.js";

class GraphWebSocket {
  static readonly OPEN = 1;
  static instances: GraphWebSocket[] = [];

  readonly sent: string[] = [];
  readyState = 0;
  onopen: (() => void) | null = null;
  onmessage: ((event: { data: string }) => void) | null = null;
  onclose: ((event: { code: number; reason: string }) => void) | null = null;
  onerror: (() => void) | null = null;

  constructor(readonly url: string) {
    GraphWebSocket.instances.push(this);
  }

  send(raw: string): void {
    this.sent.push(raw);
  }

  open(): void {
    this.readyState = GraphWebSocket.OPEN;
    this.onopen?.();
  }

  receive(raw: string): void {
    this.onmessage?.({ data: raw });
  }

  drop(code = 1006, reason = "transport dropped"): void {
    this.readyState = 3;
    this.onclose?.({ code, reason });
  }

  close(): void {
    this.readyState = 3;
    this.onclose?.({ code: 1000, reason: "closed by test" });
  }
}

interface ProductionGraph {
  client: WsClient;
  dispatcher: CommandDispatcher;
  logger: ForwardingLogger;
  localOnly: string[];
}

function requestId(raw: string): string {
  return String((JSON.parse(raw) as { requestId: string }).requestId);
}

function operation(raw: string): string {
  const parsed = JSON.parse(raw) as { clientLog?: { context?: { operation?: string } } };
  return parsed.clientLog?.context?.operation ?? "";
}

function acknowledge(socket: GraphWebSocket, dispatcher: CommandDispatcher, raw: string): void {
  dispatcher.observe(decodeFrontendFrame(JSON.stringify({
    commandAck: { requestId: requestId(raw), ok: true },
  })));
}

function newProductionGraph(): ProductionGraph {
  let dispatcher: CommandDispatcher | null = null;
  let nextRequest = 0;
  const localOnly: string[] = [];
  const logger = new ForwardingLogger(
    (cmd) => dispatcher?.clientLog(
      "/workspace",
      cmd.level,
      cmd.message,
      cmd.context as CommandStruct,
    ) ?? false,
    () => {},
  );
  setLogger(logger);
  bindLogContext({
    workspace_dir: "/workspace",
    workspace_id: "workspace-id",
    agent_repl_session_id: "agent-session",
    connection_id: "connection-1",
  });

  const client = new WsClient({
    url: "ws://daemon/sessions/agent-session/stream",
    onMessage: (raw) => dispatcher!.observe(decodeFrontendFrame(raw)),
    wsFactory: (url) => new GraphWebSocket(url) as unknown as WebSocket,
    backoffMs: [10],
  });
  dispatcher = new CommandDispatcher({
    send: (raw) => client.send(raw),
    newRequestId: () => `log-${++nextRequest}`,
    logLocal: (message) => localOnly.push(message),
  });
  return { client, dispatcher, logger, localOnly };
}

function openAndSettleStartup(graph: ProductionGraph): GraphWebSocket {
  graph.client.connect();
  const socket = GraphWebSocket.instances[0];
  socket.open();
  graph.client.adoptSnapshot({ revision_at_ms: 1 });
  graph.logger.flush();
  for (const raw of [...socket.sent]) acknowledge(socket, graph.dispatcher, raw);
  socket.sent.length = 0;
  return socket;
}

beforeEach(() => {
  GraphWebSocket.instances = [];
  vi.stubGlobal("WebSocket", GraphWebSocket);
  vi.useFakeTimers();
});

afterEach(() => {
  resetLoggingForTests();
  vi.unstubAllGlobals();
  vi.useRealTimers();
});

describe("the production webapp logging graph", () => {
  it("constructs before sink activation by retaining the constructor record", () => {
    const graph = newProductionGraph();

    expect(graph.logger.pendingCount()).toBe(1);
    expect(GraphWebSocket.instances).toHaveLength(0);
  });

  it("forwards one log as one frame and consumes its acknowledgement silently", () => {
    const graph = newProductionGraph();
    const socket = openAndSettleStartup(graph);

    log("warn", "one diagnostic", { operation: "test.production-graph.one" });

    expect(socket.sent).toHaveLength(1);
    expect(operation(socket.sent[0])).toBe("test.production-graph.one");
    acknowledge(socket, graph.dispatcher, socket.sent[0]);
    expect(socket.sent).toHaveLength(1);
  });

  it("bounds 257 unacknowledged logs with one local-only eviction and no recursion", () => {
    const graph = newProductionGraph();
    const socket = openAndSettleStartup(graph);

    for (let i = 0; i < 257; i++) {
      log("info", `diagnostic ${i}`, { operation: "test.production-graph.bounded" });
    }

    expect(socket.sent).toHaveLength(257);
    expect(graph.dispatcher.trackedClientLogCount()).toBe(256);
    expect(graph.localOnly).toHaveLength(1);
    expect(graph.localOnly[0]).toContain("evicted request");
    acknowledge(socket, graph.dispatcher, socket.sent.at(-1)!);
    expect(socket.sent).toHaveLength(257);
  });

  it("queues disconnected transitions without recursion and flushes them on reconnect", async () => {
    const graph = newProductionGraph();
    const first = openAndSettleStartup(graph);

    first.drop(1006, "daemon restart");
    expect(graph.logger.pendingCount()).toBe(3);
    await vi.advanceTimersByTimeAsync(10);
    expect(GraphWebSocket.instances).toHaveLength(2);
    expect(graph.logger.pendingCount()).toBe(5);

    const second = GraphWebSocket.instances[1];
    second.open();
    graph.client.adoptSnapshot({ revision_at_ms: 2 });
    graph.logger.flush();

    expect(graph.logger.pendingCount()).toBe(0);
    expect(second.sent.map(operation)).toEqual([
      "ws.freshness-changed",
      "ws.close-unexpected",
      "ws.reconnect-scheduled",
      "ws.freshness-changed",
      "ws.connect",
      "ws.open",
      "ws.freshness-changed",
      "ws.freshness-changed",
    ]);
    expect(second.sent.filter((raw) => operation(raw) === "ws.close-unexpected")).toHaveLength(1);
    expect(second.sent.filter((raw) => operation(raw) === "ws.open")).toHaveLength(1);
  });
});
