/**
 * The shim's connections to the shim-store.
 *
 * Two SINGLE-ROLE connections to the same socket — the store classifies every
 * connection by its FIRST frame, so the two jobs cannot share one conn:
 * 1. The PRODUCER connection (connect()): WRITE persistent event batches
 *    (`StoreWrite`) and reconcile each `StoreWriteAck` (accepted / deduped /
 *    error accounting).
 * 2. The SUBSCRIPTION connection (subscribe(), reopened per call): first
 *    frame `Subscribe{session_id, from_seq}`, then a continuous read loop
 *    handing every store-merged `Event` to an injected sink (wired to
 *    `SessionServer.sendEvent`, forwarding to the daemon verbatim).
 *
 * THE HONEST SAD PATH (design §4.4, metaprompt no-fallbacks rule): if the
 * store is unreachable or rejects a batch, every event in that batch is
 * loud-logged as dropped and a `DegradedState` is reported to the injected
 * reporter. There is NO spill buffer, NO retry of a rejected batch, NO
 * fallback — store downtime is honest downtime and the display goes stale
 * until it returns.
 *
 * "Until it returns" is load-bearing, and one thing is needed to make it
 * true: the producer connection is REDIALED, once, by the next write that
 * finds it down. The store is launchd-managed and restarts under a live shim,
 * which kills that connection for good; without the redial every later write
 * would drop against a corpse and downtime would be permanent, not honest.
 * The redial is a connection lifecycle (the daemon<->shim link already works
 * this way, §4.4), not a fallback absorbing a failure: one attempt, no timer,
 * no background loop, and a redial that fails drops the batch exactly as a
 * down connection always did. A deliberate close() is final and never
 * redials.
 *
 * The SUBSCRIPTION connection has no such recovery: its `from_seq` belongs to
 * the daemon (§4.4), which re-sends `Subscribe` on daemon<->shim reconnect,
 * so the merged tail resumes there rather than here.
 *
 * THE SUBSCRIPTION KEY is the VENDOR session id (Claude's uuid), not this
 * shim's `--session-id` — see `storeKey`. Writes were always keyed that way
 * (the envelope is read off the SDK message); the subscription was not, so it
 * listened on a channel nothing published to.
 */
import net from "node:net";
import { create } from "@bufbuild/protobuf";
import { MessageConn, envelopeType, unpackAs } from "./framing.js";
import type { Any } from "./framing.js";
import { shimLog } from "./log.js";
import {
  DegradedState,
  DegradedStateSchema,
  Event,
  EventBatchSchema,
  EventSchema,
  Heartbeat,
  HeartbeatSchema,
  StoreWriteAck,
  StoreWriteAckSchema,
  StoreWriteSchema,
  SubscribeSchema,
} from "./proto.js";

/** Receives every store-merged, seq-stamped Event for forwarding. */
export type StoreSink = (evt: Event) => void;

/** Receives an honest degraded report when the store cannot be served. */
export type DegradedReporter = (report: DegradedState) => void;

export interface StoreClientOptions {
  socketPath: string;
  sessionId: string;
  /** Producer identity in StoreWrite; e.g. `claude-shim:<session>`. */
  producer: string;
  /** Heartbeat cadence on the connection; 0 disables. Default 5000ms. */
  heartbeatIntervalMs?: number;
  /**
   * The VENDOR session id (the Claude CLI's uuid, which is its transcript
   * filename) to subscribe under, when it is already known — i.e. on
   * `--resume`. Omit for a fresh session, whose uuid only the SDK can reveal;
   * `adoptStoreKey` sets it on the first converted event. See storeKey.
   */
  storeSessionId?: string;
}

const COMPONENT = "shim-store-client";

interface PendingWrite {
  resolve: (ack: StoreWriteAck) => void;
  reject: (err: Error) => void;
  events: Event[];
}

export class StoreClient {
  private conn: MessageConn | null = null;
  private subConn: MessageConn | null = null;
  private connected = false;
  private sink: StoreSink | null = null;
  private reporter: DegradedReporter | null = null;
  private readonly pendingWrites: PendingWrite[] = [];
  /**
   * The in-flight producer redial, shared by every write that finds the
   * connection down. Writes are fire-and-forget from routeSdkMessage, so
   * without this a burst arriving during an outage would open one socket per
   * message instead of one per outage.
   */
  private redialing: Promise<void> | null = null;
  /**
   * Set by close(). A deliberate teardown must STAY torn down: without this a
   * write racing shutdown would redial the store and resurrect a connection
   * the shim is in the middle of closing.
   */
  private closed = false;
  /**
   * Tail of the send queue. `pendingWrites` is matched to acks POSITIONALLY
   * (onAck shifts), so sends must be issued in call order. write() used to be
   * synchronous up to conn.send(), which guaranteed that for free; awaiting a
   * redial breaks it, so sends chain here instead. This resolves once a send
   * is ISSUED, never once its ack lands — chaining on acks would serialize
   * round-trips and throttle the stream.
   */
  private sendChain: Promise<unknown> = Promise.resolve();
  private heartbeatTimer: NodeJS.Timeout | null = null;
  private readonly heartbeatIntervalMs: number;
  /**
   * The session id events are actually STORED under, which is the vendor
   * (Claude CLI) uuid — NOT this shim's `--session-id`.
   *
   * The store keys everything by the `session_id` on the Event envelope, and
   * that envelope is filled from the SDK message (`readEnvelope`), so writes
   * land under the vendor uuid. It has to be the uuid: the shim-sidecar
   * writes the SAME conversation from the transcript file, and its only
   * available identity is the filename (`<uuid>.jsonl`) — it never talks to
   * the daemon and cannot know an `s_…` id. Both planes must agree or the
   * `(session_id, dedup_key)` dedup that merges them cannot fire.
   *
   * Subscribing under the daemon's `s_…` id therefore registered a subscriber
   * on a channel nothing is ever published to: writes worked, replay and
   * live-tail silently returned nothing, and only EPHEMERAL events (which
   * bypass the store) reached the daemon.
   *
   * Seeded from `--resume` when known, else from the shim's own session id —
   * which is the pre-fix behavior, and harmless as a starting point because
   * `adoptStoreKey` upgrades it (and reopens the subscription) the moment the
   * first converted event reveals the real uuid.
   */
  private storeKey: string;
  /**
   * The daemon's last requested `from_seq`, retained so adopting the store key
   * can REOPEN the subscription at the same position. Null when the daemon has
   * not subscribed yet, in which case adopting merely records the key.
   */
  private lastFromSeq: bigint | null = null;
  /**
   * Whether any merged event has been forwarded to the sink under the current
   * store key. Once true the key is settled: the daemon has begun tracking
   * that key's seq space, so switching would look like a seq regression.
   */
  private forwardedAny = false;

  constructor(private readonly opts: StoreClientOptions) {
    this.heartbeatIntervalMs = opts.heartbeatIntervalMs ?? 5000;
    this.storeKey = opts.storeSessionId ?? opts.sessionId;
  }

  /**
   * Adopt the vendor session id events are stored under, learned from a
   * converted event's envelope. Idempotent, and a no-op when unchanged.
   *
   * When this CHANGES an existing subscription's key, the subscription is
   * reopened at the daemon's last `from_seq`. Nothing is lost by subscribing
   * late: the store replays everything with `seq > from_seq` from disk, so
   * events written before the key was known arrive on that replay.
   */
  adoptStoreKey(sessionId: string): void {
    if (!sessionId || sessionId === this.storeKey) return;
    // Each session id is its own SEQ SPACE, and the daemon tracks last_seen_seq
    // per shim connection, not per key. Switching after events have already
    // been forwarded therefore hands the daemon a seq that goes BACKWARDS
    // (the new key's stream starts at 1), which it treats as
    // ErrSeqRegression — a TERMINAL protocol error it never reconnects from.
    // That is exactly what a mid-session `conversation_reset` did on
    // 2026-07-25: the shim adopted the CLI's new uuid, replayed it from seq=1
    // against last_seen=5990, and the daemon dropped the shim for good.
    //
    // So the key is settled by the FIRST forwarded event and never moves
    // again. A later change is surfaced loudly rather than acted on: the
    // conversation continues under the key the daemon is already tracking.
    if (this.forwardedAny) {
      shimLog(COMPONENT, { session: this.opts.sessionId, store_key: this.storeKey, ignored: sessionId },
        `REFUSING to switch store session key mid-stream — the daemon tracks one seq space per connection and would read the new key's seq as a regression; events for ${sessionId} will not be tailed until the shim respawns`);
      return;
    }
    const previous = this.storeKey;
    this.storeKey = sessionId;
    shimLog(COMPONENT, { session: this.opts.sessionId, store_key: sessionId },
      `adopted store session key (was ${previous})`);
    // Only reopen if the daemon has actually subscribed; otherwise recording
    // the key is enough and the next subscribe() picks it up.
    if (this.lastFromSeq !== null) this.subscribe(this.lastFromSeq);
  }

  /** The session id this client currently subscribes under. */
  storeSessionId(): string {
    return this.storeKey;
  }

  /** Route merged store events to `sink` (idempotent to set). */
  onMerged(sink: StoreSink): void {
    this.sink = sink;
  }

  /** Route degraded reports to `reporter`. */
  onDegraded(reporter: DegradedReporter): void {
    this.reporter = reporter;
  }

  /** Connect to the store socket. Rejects loudly if the connect fails. */
  connect(): Promise<void> {
    return new Promise((resolve, reject) => {
      const socket = net.connect(this.opts.socketPath);
      socket.once("error", reject);
      socket.once("connect", () => {
        socket.removeListener("error", reject);
        this.conn = new MessageConn(
          socket,
          {
            onMessage: (msg) => this.onMessage(msg),
            onClose: (err) => this.onConnClose(err),
          },
          COMPONENT,
        );
        this.connected = true;
        this.startHeartbeat();
        shimLog(COMPONENT, { session: this.opts.sessionId }, `connected to store at ${this.opts.socketPath}`);
        resolve();
      });
    });
  }

  /** True while the store connection is live. */
  isConnected(): boolean {
    return this.connected;
  }

  /**
   * Open (or reopen) the merged-event subscription at `fromSeq` (EXCLUSIVE).
   * The store replays persisted events with seq > fromSeq then live-tails;
   * this is exactly the reattach replay path (§4.4).
   *
   * The subscription rides its OWN connection whose first frame is the
   * Subscribe: the store's single-role protocol rejects a Subscribe sent down
   * the producer connection.
   */
  subscribe(fromSeq: bigint): void {
    // Retained so adoptStoreKey can reopen here once the vendor uuid is
    // known: a fresh session only learns it from the SDK, which can be AFTER
    // the daemon's first Subscribe. Nothing is lost by that late reopen — the
    // store replays every event with seq > from_seq from disk.
    this.lastFromSeq = fromSeq;
    if (this.subConn) {
      // Reopen: replace the old subscription deliberately. Nulling the field
      // first lets onSubClose tell replacement from a genuine drop.
      const old = this.subConn;
      this.subConn = null;
      old.close();
    }
    const socket = net.connect(this.opts.socketPath);
    const onDialError = (err: Error) => {
      this.degrade(`cannot subscribe: ${err.message}`, 0);
    };
    socket.once("error", onDialError);
    socket.once("connect", () => {
      socket.removeListener("error", onDialError);
      const conn: MessageConn = new MessageConn(
        socket,
        {
          onMessage: (msg) => this.onSubMessage(msg),
          onClose: (err) => this.onSubClose(conn, err),
        },
        COMPONENT,
      );
      this.subConn = conn;
      // storeKey, NOT opts.sessionId: the store keys events by the vendor
      // uuid on the Event envelope, so subscribing under this shim's
      // `--session-id` registers on a channel nothing publishes to.
      conn.send(SubscribeSchema, create(SubscribeSchema, {
        sessionId: this.storeKey,
        fromSeq,
      }));
      shimLog(COMPONENT, { session: this.opts.sessionId, store_key: this.storeKey, from_seq: fromSeq }, `subscribed to store`);
    });
  }

  /**
   * Write one PERSISTENT batch and resolve with its StoreWriteAck. On a batch
   * the store rejects, or a connection that cannot be re-established, every
   * event is loud-logged as dropped, a DegradedState is reported, and the
   * promise rejects.
   *
   * A DOWN connection is redialed ONCE first (see ensureProducerConn). The
   * store is launchd-managed and restarts under us; the connection it kills is
   * never re-established otherwise, so "the display goes stale until the store
   * returns" (design §4.4) would never end — every later write would drop
   * against a corpse. This does not soften the sad path: there is still no
   * spill buffer, a rejected batch is still never retried, and a redial that
   * fails drops the batch exactly as a down connection always did.
   */
  write(events: Event[]): Promise<StoreWriteAck> {
    // Chain the SEND so batches reach the wire in call order even when the
    // first of them is waiting on a redial (see sendChain).
    const sent = this.sendChain.then(
      () => this.sendBatch(events),
      () => this.sendBatch(events), // a prior send's failure never blocks this one
    );
    this.sendChain = sent.catch(() => {});
    return sent;
  }

  /** Redial-if-needed, then enqueue and send one batch. */
  private async sendBatch(events: Event[]): Promise<StoreWriteAck> {
    try {
      await this.ensureProducerConn();
    } catch (err) {
      const why = err instanceof Error ? err.message : String(err);
      this.dropBatch(events, `store connection is down (redial failed: ${why})`);
      throw new Error("store-client: write on a down connection");
    }
    return new Promise<StoreWriteAck>((resolve, reject) => {
      this.pendingWrites.push({ resolve, reject, events });
      this.conn!.send(StoreWriteSchema, create(StoreWriteSchema, {
        producer: this.opts.producer,
        batch: create(EventBatchSchema, { events }),
      }));
    });
  }

  /**
   * Resolve once the producer connection is live, redialing at most once per
   * outage. Concurrent writers share the one in-flight dial rather than each
   * opening a socket. NOT a retry loop: one attempt, and a failure propagates
   * to the caller, which drops the batch loudly.
   */
  private ensureProducerConn(): Promise<void> {
    if (this.connected && this.conn) return Promise.resolve();
    // A deliberate teardown is final: never redial out from under shutdown.
    // (An explicit connect() still works — only the IMPLICIT redial is gated.)
    if (this.closed) {
      return Promise.reject(new Error("store client is closed"));
    }
    if (!this.redialing) {
      shimLog(COMPONENT, { session: this.opts.sessionId }, `producer connection down — redialing ${this.opts.socketPath}`);
      this.redialing = this.connect().finally(() => {
        this.redialing = null;
      });
    }
    return this.redialing;
  }

  /** Close both connections deliberately (not an error path). */
  close(): void {
    this.closed = true;
    this.stopHeartbeat();
    this.connected = false;
    if (this.subConn) {
      const sub = this.subConn;
      this.subConn = null;
      sub.close();
    }
    if (this.conn) {
      this.conn.close();
      this.conn = null;
    }
  }

  private onMessage(msg: Any): void {
    const ack = unpackAs(msg, StoreWriteAckSchema);
    if (ack) {
      this.onAck(ack);
      return;
    }
    if (unpackAs(msg, HeartbeatSchema)) return; // liveness only
    shimLog(COMPONENT, { session: this.opts.sessionId }, `unhandled store message ${envelopeType(msg)}`);
  }

  private onSubMessage(msg: Any): void {
    const evt = unpackAs(msg, EventSchema);
    if (evt) {
      if (this.sink) {
        // Settles the store key: see adoptStoreKey.
        this.forwardedAny = true;
        this.sink(evt);
      } else {
        shimLog(COMPONENT, { session: this.opts.sessionId, seq: evt.seq }, `merged event dropped: no sink bound`);
      }
      return;
    }
    if (unpackAs(msg, HeartbeatSchema)) return; // liveness only
    shimLog(COMPONENT, { session: this.opts.sessionId }, `unhandled store subscription message ${envelopeType(msg)}`);
  }

  private onSubClose(conn: MessageConn, err: Error | null): void {
    if (this.subConn !== conn) return; // superseded or deliberately closed
    this.subConn = null;
    const reason = err ? `store subscription lost: ${err.message}` : `store subscription closed`;
    // The daemon's view goes stale without the tail; report honestly.
    this.degrade(reason, 0);
  }

  private onAck(ack: StoreWriteAck): void {
    const pending = this.pendingWrites.shift();
    if (!pending) {
      shimLog(COMPONENT, { session: this.opts.sessionId }, `received StoreWriteAck with no pending write`);
      return;
    }
    if (ack.error !== "") {
      // The store rejected the WHOLE batch (loudly, per StoreWriteAck.error).
      this.dropBatch(pending.events, `store rejected batch: ${ack.error}`);
      pending.reject(new Error(`store-client: batch rejected: ${ack.error}`));
      return;
    }
    if (ack.deduped > 0n) {
      // Expected overlap with the file plane; debug-level, not an anomaly.
      shimLog(COMPONENT, { session: this.opts.sessionId, accepted: ack.accepted, deduped: ack.deduped, last_seq: ack.lastSeq }, `batch acked`);
    }
    pending.resolve(ack);
  }

  private onConnClose(err: Error | null): void {
    this.stopHeartbeat();
    this.connected = false;
    this.conn = null;
    const reason = err ? `store connection lost: ${err.message}` : `store connection closed`;
    // Every in-flight write is now a dropped batch (no spill, no retry).
    const pending = this.pendingWrites.splice(0);
    for (const p of pending) {
      this.dropBatch(p.events, reason);
      p.reject(new Error(`store-client: ${reason}`));
    }
    // Report the outage even if nothing was in flight: the subscription is
    // dead, so the daemon's view is now stale until the store returns.
    this.degrade(reason, 0);
  }

  /** Loud-log each dropped event and report a DegradedState. */
  private dropBatch(events: Event[], reason: string): void {
    for (const evt of events) {
      shimLog(COMPONENT, { session: this.opts.sessionId, seq: evt.seq, kind: envelopeKind(evt) }, `DROPPED event: ${reason}`);
    }
    this.degrade(reason, events.length);
  }

  private degrade(reason: string, droppedCount: number): void {
    const report = create(DegradedStateSchema, {
      component: COMPONENT,
      reason,
      droppedCount: BigInt(droppedCount),
      recovered: false,
    });
    if (this.reporter) {
      this.reporter(report);
    } else {
      shimLog(COMPONENT, { session: this.opts.sessionId }, `DEGRADED (no reporter bound): ${reason}`);
    }
  }

  private startHeartbeat(): void {
    if (this.heartbeatIntervalMs <= 0) return;
    this.stopHeartbeat();
    this.heartbeatTimer = setInterval(() => {
      const hb = create(HeartbeatSchema, { sentAtMs: BigInt(Date.now()) } as Heartbeat);
      this.conn?.send(HeartbeatSchema, hb);
      this.subConn?.send(HeartbeatSchema, hb);
    }, this.heartbeatIntervalMs);
    this.heartbeatTimer.unref?.();
  }

  private stopHeartbeat(): void {
    if (this.heartbeatTimer) {
      clearInterval(this.heartbeatTimer);
      this.heartbeatTimer = null;
    }
  }
}

/** Best-effort event-kind label for drop logs (the payload oneof case). */
function envelopeKind(evt: Event): string {
  return evt.payload.case ?? "unknown";
}
