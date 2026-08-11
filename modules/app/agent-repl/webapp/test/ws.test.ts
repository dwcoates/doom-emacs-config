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
  const freshnessChanges: string[] = [];
  const client = new WsClient({
    url: "ws://x/sessions/s1/stream",
    onMessage,
    onStatusChange: (c) => statusChanges.push(c),
    onFreshnessChange: (freshness) => freshnessChanges.push(freshness),
    wsFactory: (url) => new FakeWebSocket(url) as unknown as WebSocket,
    backoffMs: [10, 20],
  });
  return { client, statusChanges, freshnessChanges };
}

describe("WsClient", () => {
  it("does not report current until the first snapshot is adopted", () => {
    // Arrange
    const { client, statusChanges, freshnessChanges } = newClient();
    // Act
    client.connect();
    FakeWebSocket.instances[0].open();
    expect(statusChanges).toEqual([]);
    expect(freshnessChanges).toEqual(["connecting", "awaiting_snapshot"]);
    client.adoptSnapshot({ revision_at_ms: 10 });
    // Assert
    expect(statusChanges).toEqual([true]);
    expect(freshnessChanges).toEqual(["connecting", "awaiting_snapshot", "current"]);
  });

  // THE READ-ONLY FRESHNESS ACCESSOR. A caller deciding between dialling and
  // asking a live connection for something needs to know which it has, and the
  // transitions stay this class's own: a caller that could write it could claim
  // a currentness no snapshot had established.
  it("reports disconnected before any dial", () => {
    // Arrange
    const { client } = newClient();
    // Act, Assert
    expect(client.state).toBe("disconnected");
  });

  it("reports current once the first snapshot is adopted", () => {
    // Arrange
    const { client } = newClient();
    client.connect();
    FakeWebSocket.instances[0].open();
    // Act
    client.adoptSnapshot({ revision_at_ms: 10 });
    // Assert
    expect(client.state).toBe("current");
  });

  it("reports awaiting_snapshot on an open socket with no snapshot yet", () => {
    // Arrange
    const { client } = newClient();
    // Act
    client.connect();
    FakeWebSocket.instances[0].open();
    // Assert
    expect(client.state).toBe("awaiting_snapshot");
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
    client.adoptSnapshot();
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

  it("send() returns false while an open socket still awaits its snapshot", () => {
    const { client } = newClient();
    client.connect();
    FakeWebSocket.instances[0].open();
    expect(client.send("{}")).toBe(false);
  });

  it("rejects snapshot adoption before the connection opens", () => {
    const records = captureCanonicalLogs();
    const { client } = newClient();
    client.connect();

    expect(() => client.adoptSnapshot({ revision_at_ms: 10 })).toThrow(
      "cannot adopt snapshot while freshness=connecting",
    );
    expect(records).toContainEqual(expect.objectContaining({
      operation: "ws.snapshot-adoption-rejected",
      context: expect.objectContaining({ freshness: "connecting", revision_at_ms: 10 }),
    }));
  });

  it("rejects a non-positive freshness timeout with its configured value", () => {
    const records = captureCanonicalLogs();
    const client = new WsClient({
      url: "ws://x/sessions/s1/stream",
      onMessage: () => undefined,
      wsFactory: (url) => new FakeWebSocket(url) as unknown as WebSocket,
      snapshotTimeoutMs: 0,
    });
    client.connect();

    expect(() => FakeWebSocket.instances[0].open()).toThrow(
      "snapshotTimeoutMs must be positive, got 0",
    );
    expect(records).toContainEqual(expect.objectContaining({
      operation: "ws.snapshot-timeout-invalid",
      context: expect.objectContaining({ timeout_ms: 0 }),
    }));
  });

  it("expires a connection whose authoritative snapshot lease is not renewed", () => {
    const freshnessChanges: string[] = [];
    const statusChanges: boolean[] = [];
    const records = captureCanonicalLogs();
    const client = new WsClient({
      url: "ws://x/sessions/s1/stream",
      onMessage: () => undefined,
      onFreshnessChange: (freshness) => freshnessChanges.push(freshness),
      onStatusChange: (connected) => statusChanges.push(connected),
      wsFactory: (url) => new FakeWebSocket(url) as unknown as WebSocket,
      backoffMs: [10],
      snapshotTimeoutMs: 30,
    });
    client.connect();
    FakeWebSocket.instances[0].open();
    client.adoptSnapshot({ revision_at_ms: 10 });

    vi.advanceTimersByTime(30);

    expect(freshnessChanges).toEqual(["connecting", "awaiting_snapshot", "current", "expired"]);
    expect(statusChanges).toEqual([true, false]);
    expect(records).toContainEqual(expect.objectContaining({
      operation: "ws.freshness-changed",
      context: expect.objectContaining({ previous: "current", next: "expired", timeout_ms: 30 }),
    }));
  });

  it("a renewed snapshot extends the lease from its own adoption", () => {
    const { client, freshnessChanges } = newClient();
    client.connect();
    FakeWebSocket.instances[0].open();
    client.adoptSnapshot();
    vi.advanceTimersByTime(30_000);
    client.adoptSnapshot();
    vi.advanceTimersByTime(30_000);
    expect(freshnessChanges).toEqual(["connecting", "awaiting_snapshot", "current"]);
  });

  it("ignores messages from a retired socket generation", () => {
    const seen: string[] = [];
    const { client } = newClient((data) => seen.push(data));
    client.connect();
    const first = FakeWebSocket.instances[0];
    first.open();
    client.close();
    client.connect();
    first.receive("stale");
    expect(seen).toEqual([]);
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

  it("dials the FIRST reconnect immediately, without spending a backoff rung", () => {
    // Arrange — a live socket, so the close below is the daemon going away.
    const { client } = newClient();
    client.connect();
    const first = FakeWebSocket.instances[0];
    first.open();

    // Act — the socket drops and NO time passes.
    first.close();
    vi.advanceTimersByTime(0);

    // Assert — the redial is already out. A backoff has nothing to back off
    // from before the first attempt, and the 251ms every workspace spent on
    // this rung in production was pure page-side latency.
    expect(FakeWebSocket.instances).toHaveLength(2);
  });

  it("keeps the ladder for reconnects after the first one fails", () => {
    // Arrange — first redial goes out immediately and is refused.
    const { client } = newClient();
    client.connect();
    FakeWebSocket.instances[0].open();
    FakeWebSocket.instances[0].close();
    vi.advanceTimersByTime(0);
    expect(FakeWebSocket.instances).toHaveLength(2);

    // Act — that attempt fails; the ladder's first rung (10ms) now applies.
    FakeWebSocket.instances[1].close();
    vi.advanceTimersByTime(9);
    // Assert — still waiting: a failed attempt DOES owe a delay.
    expect(FakeWebSocket.instances).toHaveLength(2);
    vi.advanceTimersByTime(1);
    expect(FakeWebSocket.instances).toHaveLength(3);
  });

  it("returns to an immediate first redial once a connection has succeeded", () => {
    // Arrange — climb the ladder, then open successfully, which resets it.
    const { client } = newClient();
    client.connect();
    FakeWebSocket.instances[0].open();
    FakeWebSocket.instances[0].close();
    vi.advanceTimersByTime(0);
    FakeWebSocket.instances[1].close();
    vi.advanceTimersByTime(10);
    FakeWebSocket.instances[2].open();

    // Act — a later drop on that healthy socket.
    FakeWebSocket.instances[2].close();
    vi.advanceTimersByTime(0);

    // Assert — immediate again: an open socket is what the ladder backs off
    // FROM, so surviving one discharges the history it accumulated.
    expect(FakeWebSocket.instances).toHaveLength(4);
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

  it("ensureConnected dials immediately instead of waiting out the backoff", () => {
    // Arrange — a drop schedules a reconnect 10ms out that has not fired.
    const { client } = newClient();
    client.connect();
    FakeWebSocket.instances[0].open();
    FakeWebSocket.instances[0].close();
    expect(FakeWebSocket.instances).toHaveLength(1);
    // Act — the user clicks before the timer runs.
    client.ensureConnected();
    // Assert — dialed now, and the cancelled timer opens no third socket.
    expect(FakeWebSocket.instances).toHaveLength(2);
    vi.advanceTimersByTime(1000);
    expect(FakeWebSocket.instances).toHaveLength(2);
  });

  it("ensureConnected re-dials after a session-gone verdict stopped the loop", async () => {
    // Arrange — one probe reports the session gone, which is terminal for the
    // automatic loop.
    const client = new WsClient({
      url: "ws://x/sessions/s1/stream",
      onMessage: () => undefined,
      wsFactory: (url) => new FakeWebSocket(url) as unknown as WebSocket,
      backoffMs: [10],
      sessionExists: async () => false,
      onGone: () => undefined,
    });
    client.connect();
    FakeWebSocket.instances[0].open();
    FakeWebSocket.instances[0].close();
    await vi.advanceTimersByTimeAsync(10);
    expect(FakeWebSocket.instances).toHaveLength(1);
    // Act — an explicit user action after the terminal verdict.
    client.ensureConnected();
    // Assert — a stale "gone" reading is not permanent against a user action.
    expect(FakeWebSocket.instances).toHaveLength(2);
  });

  it("ensureConnected does nothing while state is already current", () => {
    // Arrange
    const { client } = newClient();
    client.connect();
    FakeWebSocket.instances[0].open();
    client.adoptSnapshot();
    // Act
    client.ensureConnected();
    // Assert — no second socket alongside the live one.
    expect(FakeWebSocket.instances).toHaveLength(1);
  });

  it("ensureConnected leaves an in-flight dial alone", () => {
    // Arrange — connecting, socket not yet open.
    const { client } = newClient();
    client.connect();
    // Act
    client.ensureConnected();
    // Assert — the in-flight attempt is not abandoned for a fresh one.
    expect(FakeWebSocket.instances).toHaveLength(1);
  });

  it("whenCurrent resolves true on the transition into current", async () => {
    // Arrange
    const { client } = newClient();
    client.connect();
    const waited = client.whenCurrent(1000);
    // Act
    FakeWebSocket.instances[0].open();
    client.adoptSnapshot();
    // Assert
    await expect(waited).resolves.toBe(true);
  });

  it("whenCurrent resolves true immediately when state is already current", async () => {
    const { client } = newClient();
    client.connect();
    FakeWebSocket.instances[0].open();
    client.adoptSnapshot();
    await expect(client.whenCurrent(1000)).resolves.toBe(true);
  });

  it("whenCurrent resolves false when currentness never arrives within the bound", async () => {
    // Arrange — socket opens but no snapshot is ever adopted.
    const records = captureCanonicalLogs();
    const { client } = newClient();
    client.connect();
    FakeWebSocket.instances[0].open();
    const waited = client.whenCurrent(50);
    // Act
    await vi.advanceTimersByTimeAsync(50);
    // Assert
    await expect(waited).resolves.toBe(false);
    expect(records).toContainEqual(expect.objectContaining({
      operation: "ws.when-current-timeout",
      context: expect.objectContaining({ timeout_ms: 50, freshness: "awaiting_snapshot" }),
    }));
  });

  it("whenCurrent resolves false at once when the user closes the connection", async () => {
    // Arrange
    const { client } = newClient();
    client.connect();
    FakeWebSocket.instances[0].open();
    const waited = client.whenCurrent(60_000);
    // Act
    client.close();
    // Assert — no waiting out the bound against a connection nobody reopens.
    await expect(waited).resolves.toBe(false);
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
