/**
 * The shim's connection to the shim-store.
 *
 * Two jobs over one UDS connection:
 * 1. WRITE PERSISTENT event batches (`StoreWrite`) and reconcile each
 *    `StoreWriteAck` (accepted / deduped / error accounting).
 * 2. SUBSCRIBE (`Subscribe{session_id, from_seq}`) and run a continuous read
 *    loop handing every store-merged `Event` to an injected sink (which the
 *    stitch phase wires to `SessionServer.sendEvent`, forwarding to the
 *    daemon verbatim).
 *
 * THE HONEST SAD PATH (design §4.4, metaprompt no-fallbacks rule): if the
 * store is unreachable or rejects a batch, every event in that batch is
 * loud-logged as dropped and a `DegradedState` is reported to the injected
 * reporter. There is NO spill buffer, NO retry-forever, NO fallback — store
 * downtime is honest downtime and the display goes stale until it returns.
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
}

const COMPONENT = "shim-store-client";

interface PendingWrite {
  resolve: (ack: StoreWriteAck) => void;
  reject: (err: Error) => void;
  events: Event[];
}

export class StoreClient {
  private conn: MessageConn | null = null;
  private connected = false;
  private sink: StoreSink | null = null;
  private reporter: DegradedReporter | null = null;
  private readonly pendingWrites: PendingWrite[] = [];
  private heartbeatTimer: NodeJS.Timeout | null = null;
  private readonly heartbeatIntervalMs: number;

  constructor(private readonly opts: StoreClientOptions) {
    this.heartbeatIntervalMs = opts.heartbeatIntervalMs ?? 5000;
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
   */
  subscribe(fromSeq: bigint): void {
    if (!this.connected || !this.conn) {
      this.degrade(`cannot subscribe: store connection is down`, 0);
      return;
    }
    this.conn.send(SubscribeSchema, create(SubscribeSchema, {
      sessionId: this.opts.sessionId,
      fromSeq,
    }));
    shimLog(COMPONENT, { session: this.opts.sessionId, from_seq: fromSeq }, `subscribed to store`);
  }

  /**
   * Write one PERSISTENT batch and resolve with its StoreWriteAck. On a down
   * connection or a batch the store rejects, every event is loud-logged as
   * dropped, a DegradedState is reported, and the promise rejects. No retry.
   */
  write(events: Event[]): Promise<StoreWriteAck> {
    if (!this.connected || !this.conn) {
      this.dropBatch(events, `store connection is down`);
      return Promise.reject(new Error("store-client: write on a down connection"));
    }
    return new Promise<StoreWriteAck>((resolve, reject) => {
      this.pendingWrites.push({ resolve, reject, events });
      this.conn!.send(StoreWriteSchema, create(StoreWriteSchema, {
        producer: this.opts.producer,
        batch: create(EventBatchSchema, { events }),
      }));
    });
  }

  /** Close the connection deliberately (not an error path). */
  close(): void {
    this.stopHeartbeat();
    this.connected = false;
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
    const evt = unpackAs(msg, EventSchema);
    if (evt) {
      if (this.sink) {
        this.sink(evt);
      } else {
        shimLog(COMPONENT, { session: this.opts.sessionId, seq: evt.seq }, `merged event dropped: no sink bound`);
      }
      return;
    }
    if (unpackAs(msg, HeartbeatSchema)) return; // liveness only
    shimLog(COMPONENT, { session: this.opts.sessionId }, `unhandled store message ${envelopeType(msg)}`);
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
      this.conn?.send(HeartbeatSchema, create(HeartbeatSchema, { sentAtMs: BigInt(Date.now()) } as Heartbeat));
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
