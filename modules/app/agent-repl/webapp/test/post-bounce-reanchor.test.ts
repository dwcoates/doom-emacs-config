/**
 * THE POST-BOUNCE CHAIN, ON A CLOCK.
 *
 * The defect this file pins down was measured on a live daemon bounce across
 * six webviews. Every one of them closed on `code=1001 server_shutdown` and
 * then, from the moment of the close:
 *
 *   +251ms   re-dialled            — the backoff ladder's first rung, to the
 *                                    millisecond, in all six workspaces
 *   +~660ms  socket open
 *   +~1840ms connect snapshot adopted, resync dispatched from the OLD mark
 *   +3088ms  the recovery SLO forced, its budget spent
 *   +3591ms  the daemon refused the mark as a retired seq space
 *   +3592ms  the conversation page finally requested
 *
 * Two of those segments were the page's own and neither had to exist. The
 * 251ms was a delay before an attempt that had no failure behind it. The
 * 1,751ms between the resync and its refusal was spent waiting to be told
 * something the adopted snapshot had ALREADY said: its head was below the mark
 * the page was asking from, which is the exact condition the daemon refuses
 * on. The page held both numbers and asked anyway.
 *
 * These tests compose the REAL WsClient, the REAL ConnectResync and the REAL
 * ConversationStore on a fake clock, so what they assert is elapsed time
 * through the actual chain rather than a restatement of any one module's unit
 * behaviour. Nothing in this file applies a recovery-SLO force: the point is
 * that the page no longer needs one.
 */
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { WsClient } from "../src/ws.js";
import { ConnectResync } from "../src/connect-resync.js";
import { ConversationStore } from "../src/store.js";
import type { AdapterEffect } from "../src/state-adapter.js";
import type { ConversationItem } from "../src/store.js";

/** A WebSocket whose open is scheduled rather than instantaneous. */
class ScriptedSocket {
  static instances: ScriptedSocket[] = [];
  static OPEN = 1;
  /** How long the daemon takes to accept a connection, once it is back. */
  static openDelayMs = 400;
  /** Dials before this time are refused; the daemon is still down. */
  static acceptingAtMs = 0;
  readyState = 0;
  sent: string[] = [];
  onopen: (() => void) | null = null;
  onmessage: ((event: { data: string }) => void) | null = null;
  onclose: ((event: { code: number; reason: string }) => void) | null = null;
  onerror: (() => void) | null = null;

  constructor(readonly url: string) {
    ScriptedSocket.instances.push(this);
    const dialledAt = Date.now();
    setTimeout(() => {
      if (dialledAt + ScriptedSocket.openDelayMs < ScriptedSocket.acceptingAtMs) {
        this.readyState = 3;
        this.onclose?.({ code: 1006, reason: "connection refused" });
        return;
      }
      this.readyState = ScriptedSocket.OPEN;
      this.onopen?.();
    }, ScriptedSocket.openDelayMs);
  }

  send(data: string): void {
    this.sent.push(data);
  }

  close(): void {
    this.readyState = 3;
    this.onclose?.({ code: 1001, reason: "server_shutdown" });
  }
}

/** Drain the microtask queue so a dispatched resync's promise settles. */
async function flush(): Promise<void> {
  for (let i = 0; i < 8; i++) await Promise.resolve();
}

function textItem(uuid: string): ConversationItem {
  return { kind: "text", uuid, role: "assistant", text: "x" } as unknown as ConversationItem;
}

function itemsEffect(throughSeq: number): AdapterEffect {
  return {
    kind: "conversation-items",
    workspace: "/ws",
    fence: "f1",
    throughSeq,
    items: [textItem(`t:${throughSeq}`)],
  } as unknown as AdapterEffect;
}

interface Chain {
  client: WsClient;
  /** Elapsed ms from the close to the conversation-page request, or null. */
  pageRequestedAtMs: number | null;
  resyncsSent: number;
  closedAtMs: number;
  /** Deliver the daemon's connect snapshot, carrying its head for this ws. */
  deliverSnapshot: (headSeq: number) => void;
}

/**
 * Wire a page whose applied mark is 1060 — the mark it carried into the bounce
 * — against a daemon whose conversation restarted and is now at head 12.
 */
function chain(): Chain {
  const store = new ConversationStore();
  store.ingest([itemsEffect(1060)]);
  const state = { pageRequestedAtMs: null as number | null, resyncsSent: 0, closedAtMs: 0 };

  const connectResync = new ConnectResync({
    // A resync goes to the daemon and is answered on the daemon's own time —
    // 1,751ms on the bounce above. What these tests count is whether one was
    // SENT at all, since sending it is what commits the page to that wait, so
    // the ack itself is modelled as ordinary and prompt.
    resync: () => {
      state.resyncsSent += 1;
      return Promise.resolve();
    },
    reanchor: () => {
      state.pageRequestedAtMs = Date.now() - state.closedAtMs;
      return true;
    },
  });

  const client = new WsClient({
    url: "ws://x/workspace-stream?workspace=/ws",
    onMessage: () => {},
    // The socket's own lifecycle arms and disarms the resync, exactly as
    // main.ts wires it: a fresh connection owes one resync, and a dead one
    // can never be attributed a snapshot.
    onFreshnessChange: (freshness) => {
      if (freshness === "awaiting_snapshot") connectResync.onConnect();
      if (freshness === "disconnected" || freshness === "expired") connectResync.onDisconnect();
    },
    wsFactory: (url) => new ScriptedSocket(url) as unknown as WebSocket,
    backoffMs: [250, 500, 1000, 2000, 5000],
  });

  return {
    client,
    get pageRequestedAtMs() {
      return state.pageRequestedAtMs;
    },
    get resyncsSent() {
      return state.resyncsSent;
    },
    get closedAtMs() {
      return state.closedAtMs;
    },
    set closedAtMs(v: number) {
      state.closedAtMs = v;
      // The connect that brought this page up sent its own ordinary resync.
      // What is under test is what the page spends AFTER the bounce, so the
      // count starts here.
      state.resyncsSent = 0;
    },
    deliverSnapshot: (headSeq: number) => {
      // The page ingests the connect snapshot exactly as main.ts does: adopt
      // it on the transport, then rule on what it said.
      client.adoptSnapshot();
      const result = store.ingest([itemsEffect(headSeq)]);
      const request = { workspace: "/ws", fromSeq: store.state.lastSeq, fence: "f1" };
      const reanchored =
        result.seqSpaceRetired === true && connectResync.observeRetiredSeqSpace(request);
      if (!reanchored) connectResync.observe(true, request);
    },
  };
}

beforeEach(() => {
  ScriptedSocket.instances = [];
  ScriptedSocket.openDelayMs = 400;
  ScriptedSocket.acceptingAtMs = 0;
  vi.stubGlobal("WebSocket", ScriptedSocket);
  vi.useFakeTimers();
  vi.setSystemTime(0);
});

afterEach(() => {
  vi.unstubAllGlobals();
  vi.useRealTimers();
});

describe("a page whose daemon bounced under it", () => {
  it("re-anchors within a few hundred ms of the link returning", async () => {
    // Arrange — a live page, connected and current.
    const h = chain();
    h.client.connect();
    vi.advanceTimersByTime(400);
    h.deliverSnapshot(1060);
    await flush();
    const live = ScriptedSocket.instances[0];

    // Act — the daemon shuts down; nothing else drives this page. No recovery
    // heartbeat is started, no visibility change fires and NO SLO FORCE is
    // applied anywhere below.
    h.closedAtMs = Date.now();
    live.close();
    // The redial is immediate; the socket takes 400ms to come up.
    vi.advanceTimersByTime(400);
    // The daemon's connect snapshot lands, announcing a head of 12 for a page
    // holding 1060.
    h.deliverSnapshot(12);
    await flush();

    // Assert — the conversation page is already requested, and the page never
    // spent a resync to learn what the snapshot told it.
    expect(h.pageRequestedAtMs).not.toBeNull();
    expect(h.pageRequestedAtMs!).toBeLessThan(500);
    expect(h.resyncsSent).toBe(0);
  });

  it("would have spent the whole budget on the round trip it now skips", async () => {
    // Arrange — the same page, with the local proof unavailable because the
    // daemon's head came back ABOVE the mark (an ordinary bounce that retired
    // nothing). This is the control: the resync path is still the path.
    const h = chain();
    h.client.connect();
    vi.advanceTimersByTime(400);
    h.deliverSnapshot(1060);
    await flush();

    // Act
    h.closedAtMs = Date.now();
    ScriptedSocket.instances[0].close();
    vi.advanceTimersByTime(400);
    h.deliverSnapshot(1200);
    await flush();

    // Assert — a delta is asked for, and no conversation is replaced. The
    // optimization must not fire where the mark is still good: a re-anchor
    // REPLACES history rather than extending it.
    expect(h.resyncsSent).toBe(1);
    expect(h.pageRequestedAtMs).toBeNull();
  });

  it("keeps re-dialling on the ladder while the daemon is still down", async () => {
    // Arrange — the daemon will not accept a connection for a full second, so
    // the immediate first dial genuinely fails.
    const h = chain();
    h.client.connect();
    vi.advanceTimersByTime(400);
    h.deliverSnapshot(1060);
    await flush();
    ScriptedSocket.acceptingAtMs = Date.now() + 1_000;
    const dialsBefore = ScriptedSocket.instances.length;

    // Act
    h.closedAtMs = Date.now();
    ScriptedSocket.instances[0].close();
    vi.advanceTimersByTime(3_000);

    // Assert — an immediate first attempt did not cost the page its retries:
    // the ladder is still there behind it.
    expect(ScriptedSocket.instances.length).toBeGreaterThan(dialsBefore + 1);
  });
});
