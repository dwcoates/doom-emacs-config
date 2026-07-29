import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { WsClient, composerEnabled, makeSessionExistsProbe } from "../src/ws.js";
import { ForwardingLogger, setLogger } from "../src/wslog.js";

function captureCanonicalLogs(): Array<Record<string, unknown>> {
  const records: Array<Record<string, unknown>> = [];
  setLogger(new ForwardingLogger((cmd) => {
    records.push(cmd.context as Record<string, unknown>);
    return true;
  }, () => {}));
  return records;
}

/** Minimal scripted WebSocket standing in for the real one. */
class FakeWebSocket {
  static instances: FakeWebSocket[] = [];
  static OPEN = 1;
  url: string;
  readyState = 0;
  sent: string[] = [];
  onopen: (() => void) | null = null;
  onmessage: ((event: { data: string }) => void) | null = null;
  onclose: ((event: { code: number; reason: string }) => void) | null = null;
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
    this.onclose?.({ code: 1006, reason: "test close" });
  }

  error(): void {
    this.onerror?.();
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

function newClient(onMessage: (data: string) => void = () => {}) {
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
    });
    client.connect();
    FakeWebSocket.instances[0].open();
    // Act
    FakeWebSocket.instances[0].receive(`{"type":"x","seq":1}`);
    // Assert
    expect(seen).toEqual([`{"type":"x","seq":1}`]);
  });

  it("send() forwards a pre-encoded frame verbatim once open", () => {
    // Arrange
    const { client } = newClient();
    client.connect();
    const ws = FakeWebSocket.instances[0];
    ws.open();
    // Act
    const ok = client.send(`{"requestId":"r1","interrupt":{"confirmAgents":false}}`);
    // Assert
    expect(ok).toBe(true);
    expect(ws.sent).toEqual([`{"requestId":"r1","interrupt":{"confirmAgents":false}}`]);
  });

  it("send() returns false when the socket is not open", () => {
    // Arrange
    const { client } = newClient();
    client.connect();
    // Act + Assert — never opened.
    expect(client.send("{}")).toBe(false);
  });

  it("composerEnabled is on by default and off only for composer=0", () => {
    // Arrange + Act + Assert
    expect(composerEnabled(new URLSearchParams(""))).toBe(true);
    expect(composerEnabled(new URLSearchParams("composer=1"))).toBe(true);
    expect(composerEnabled(new URLSearchParams("composer=0"))).toBe(false);
  });

  it("makeSessionExistsProbe answers from the /sessions listing", async () => {
    // Arrange
    const fetchFn = (async () => ({
      ok: true,
      json: async () => ({ sessions: [{ session_id: "s1" }] }),
    })) as unknown as typeof fetch;
    // Act / Assert
    await expect(makeSessionExistsProbe("http://d", "s1", fetchFn)()).resolves.toBe(true);
    await expect(makeSessionExistsProbe("http://d", "ghost", fetchFn)()).resolves.toBe(false);
  });

  it("makeSessionExistsProbe throws on a non-2xx listing", async () => {
    // Arrange — transport failure must read as unknown, not gone.
    const fetchFn = (async () => ({ ok: false, status: 502 })) as unknown as typeof fetch;
    // Act / Assert
    await expect(makeSessionExistsProbe("http://d", "s1", fetchFn)()).rejects.toThrow("502");
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

  it("defers error ownership to close so a transport drop records once", () => {
    const { client } = newClient();
    client.connect();
    FakeWebSocket.instances[0].error();
    expect(FakeWebSocket.instances).toHaveLength(1);
  });

  it("stops reconnecting and fires onGone when the session is gone", async () => {
    // Arrange — the pre-reconnect probe reports the session vanished.
    let gone = 0;
    const records = captureCanonicalLogs();
    const client = new WsClient({
      url: "ws://x/sessions/s1/stream",
      onMessage: () => undefined,
      wsFactory: (url) => new FakeWebSocket(url) as unknown as WebSocket,
      backoffMs: [10],
      sessionExists: async () => false,
      onGone: () => {
        gone++;
      },
    });
    client.connect();
    FakeWebSocket.instances[0].open();
    // Act
    FakeWebSocket.instances[0].close();
    await vi.advanceTimersByTimeAsync(10);
    // Assert — no second socket, onGone fired exactly once, and the
    // terminal transition left a trace in the log.
    expect(FakeWebSocket.instances).toHaveLength(1);
    expect(gone).toBe(1);
    expect(records).toContainEqual(expect.objectContaining({ operation: "ws.session-gone" }));
  });

  it("ignores a stale in-flight probe after close() then connect()", async () => {
    // Arrange — a probe we resolve manually, long after the client has
    // been closed and reconnected by the user.
    let resolveProbe: (v: boolean) => void = () => {};
    const client = new WsClient({
      url: "ws://x/sessions/s1/stream",
      onMessage: () => undefined,
      wsFactory: (url) => new FakeWebSocket(url) as unknown as WebSocket,
      backoffMs: [10],
      sessionExists: () => new Promise<boolean>((resolve) => (resolveProbe = resolve)),
    });
    client.connect();
    FakeWebSocket.instances[0].open();
    FakeWebSocket.instances[0].close(); // schedules reconnect
    await vi.advanceTimersByTimeAsync(10); // probe now in flight
    // Act — user closes and reconnects while the probe dangles...
    client.close();
    client.connect(); // socket #2
    expect(FakeWebSocket.instances).toHaveLength(2);
    resolveProbe(true); // ...then the stale probe resolves
    await vi.advanceTimersByTimeAsync(0);
    // Assert — the stale probe must NOT open socket #3.
    expect(FakeWebSocket.instances).toHaveLength(2);
  });

  it("keeps reconnecting when the existence probe itself fails", async () => {
    // Arrange — an unreachable probe counts as unknown, not gone.
    let gone = 0;
    const records = captureCanonicalLogs();
    const client = new WsClient({
      url: "ws://x/sessions/s1/stream",
      onMessage: () => undefined,
      wsFactory: (url) => new FakeWebSocket(url) as unknown as WebSocket,
      backoffMs: [10],
      sessionExists: async () => {
        throw new Error("daemon briefly down");
      },
      onGone: () => {
        gone++;
      },
    });
    client.connect();
    FakeWebSocket.instances[0].open();
    // Act
    FakeWebSocket.instances[0].close();
    await vi.advanceTimersByTimeAsync(10);
    // Assert — reconnect proceeded despite the probe error, and the
    // failure was logged rather than swallowed silently.
    expect(FakeWebSocket.instances).toHaveLength(2);
    expect(gone).toBe(0);
    expect(records).toContainEqual(expect.objectContaining({
      operation: "ws.session-exists-probe-failed",
      context: expect.objectContaining({ cause: expect.objectContaining({ message: "daemon briefly down" }) }),
    }));
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
