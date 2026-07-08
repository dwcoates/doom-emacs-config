import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { WsClient } from "../src/ws.js";

/** Minimal scripted WebSocket standing in for the real one. */
class FakeWebSocket {
  static instances: FakeWebSocket[] = [];
  static OPEN = 1;
  url: string;
  readyState = 0;
  sent: string[] = [];
  onopen: (() => void) | null = null;
  onmessage: ((event: { data: string }) => void) | null = null;
  onclose: (() => void) | null = null;
  onerror: (() => void) | null = null;

  constructor(url: string) {
    this.url = url;
    FakeWebSocket.instances.push(this);
  }

  open(): void {
    this.readyState = FakeWebSocket.OPEN;
    this.onopen?.();
  }

  receive(data: string): void {
    this.onmessage?.({ data });
  }

  send(data: string): void {
    this.sent.push(data);
  }

  close(): void {
    this.readyState = 3;
    this.onclose?.();
  }
}

beforeEach(() => {
  FakeWebSocket.instances = [];
  vi.stubGlobal("WebSocket", FakeWebSocket);
  vi.useFakeTimers();
});

afterEach(() => {
  vi.unstubAllGlobals();
  vi.useRealTimers();
});

function newClient(onMessage: (data: string) => any = () => undefined) {
  const statusChanges: boolean[] = [];
  const client = new WsClient({
    url: "ws://x/sessions/s1/stream",
    onMessage,
    onStatusChange: (c) => statusChanges.push(c),
    wsFactory: (url) => new FakeWebSocket(url) as unknown as WebSocket,
    backoffMs: [10, 20],
  });
  return { client, statusChanges };
}

describe("WsClient", () => {
  it("reports connected on open", () => {
    // Arrange
    const { client, statusChanges } = newClient();
    // Act
    client.connect();
    FakeWebSocket.instances[0].open();
    // Assert
    expect(statusChanges).toEqual([true]);
  });

  it("delivers inbound messages to onMessage", () => {
    // Arrange
    const seen: string[] = [];
    const { client } = newClient((d) => {
      seen.push(d);
      return undefined;
    });
    client.connect();
    FakeWebSocket.instances[0].open();
    // Act
    FakeWebSocket.instances[0].receive(`{"type":"x","seq":1}`);
    // Assert
    expect(seen).toEqual([`{"type":"x","seq":1}`]);
  });

  it("sends the command onMessage returns (replay-request path)", () => {
    // Arrange
    const { client } = newClient(() => ({ type: "replay-request", from_seq: 4 }));
    client.connect();
    const ws = FakeWebSocket.instances[0];
    ws.open();
    // Act
    ws.receive(`{"type":"x","seq":9}`);
    // Assert
    expect(ws.sent).toEqual([`{"type":"replay-request","from_seq":4}`]);
  });

  it("send() returns false when the socket is not open", () => {
    // Arrange
    const { client } = newClient();
    client.connect();
    // Act + Assert — never opened.
    expect(client.send({ type: "interrupt", request_id: "r1" })).toBe(false);
  });

  it("reconnects with backoff after an unexpected close", () => {
    // Arrange
    const { client } = newClient();
    client.connect();
    const first = FakeWebSocket.instances[0];
    first.open();
    // Act
    first.close();
    vi.advanceTimersByTime(10);
    // Assert
    expect(FakeWebSocket.instances).toHaveLength(2);
  });

  it("does not reconnect after a user-initiated close", () => {
    // Arrange
    const { client } = newClient();
    client.connect();
    FakeWebSocket.instances[0].open();
    // Act
    client.close();
    vi.advanceTimersByTime(1000);
    // Assert
    expect(FakeWebSocket.instances).toHaveLength(1);
  });
});
