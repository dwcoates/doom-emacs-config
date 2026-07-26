/**
 * The shim's connection to the daemon.
 *
 * The shim OWNS the `session-<id>.sock` and OUTLIVES the daemon: a dropped
 * daemon connection tears nothing down (the SDK turn keeps running, the store
 * subscription keeps landing durable events). A restarted daemon simply
 * reconnects to the same live shim, re-handshakes, and re-subscribes
 * (design §4.4 REATTACH). This class therefore never ends a turn on
 * disconnect and never buffers events for an absent daemon — unsent events are
 * durable in the store and replayed on the next `Subscribe{from_seq}`.
 *
 * Handshake: the LISTENER speaks first (core.proto), sending `ShimHello`
 * immediately on accept; the daemon replies `DaemonHello`. Control messages
 * (`SubmitPrompt`, `Interrupt`, `PermissionResponse`, `Subscribe`) are
 * dispatched to injected handlers; `SubmitPrompt`/`Interrupt` handlers return
 * the synchronous `Ack`/`Nack` receipt this server writes back.
 */
import net from "node:net";
import {
  MessageConn,
  envelopeType,
  unpackAs,
} from "./framing.js";
import type { Any } from "./framing.js";
import { shimLog } from "./log.js";
import {
  Ack,
  AckSchema,
  DaemonHello,
  DaemonHelloSchema,
  Event,
  EventSchema,
  Heartbeat,
  HeartbeatSchema,
  Interrupt,
  InterruptSchema,
  Nack,
  NackSchema,
  PermissionRequest,
  PermissionRequestSchema,
  PermissionResponse,
  PermissionResponseSchema,
  ReplayDone,
  ReplayDoneSchema,
  ReplayEventSchema,
  ReplayRequest,
  ReplayRequestSchema,
  ShimHelloSchema,
  Subscribe,
  SubscribeSchema,
  SubmitPrompt,
  SubmitPromptSchema,
} from "./proto.js";
import { create } from "@bufbuild/protobuf";

/** A synchronous control receipt: either an Ack or a Nack. */
export type Receipt = Ack | Nack;

export interface SessionServerHandlers {
  /** Push a prompt into the SDK turn; return the sync receipt. */
  onSubmitPrompt(msg: SubmitPrompt): Receipt;
  /** Interrupt the SDK turn; return the sync receipt. */
  onInterrupt(msg: Interrupt): Receipt;
  /** Deliver a permission decision to the blocked canUseTool round-trip. */
  onPermissionResponse(msg: PermissionResponse): void;
  /** (Re)start the store→daemon forward from `from_seq` (reattach replay). */
  onSubscribe(msg: Subscribe): void;
  /**
   * Serve a BOUNDED historical replay (core.proto ReplayRequest). Distinct
   * from onSubscribe in the way that matters: this must NOT move the standing
   * subscription, and its events go back as ReplayEvent rather than Event.
   */
  onReplayRequest(msg: ReplayRequest): void;
  /** Daemon (re)connected and completed the handshake. */
  onDaemonConnected?(hello: DaemonHello): void;
  /** Daemon connection lost (reattach possible; nothing torn down). */
  onDaemonDisconnected?(): void;
}

export interface SessionServerOptions {
  /** The DAEMON socket this shim dials (every session shares one). */
  socketPath: string;
  sessionId: string;
  shimVersion: string;
  protocolVersion: string;
  vendor?: string;
  /** Reported in ShimHello.turn_in_flight so a reattaching daemon knows. */
  turnInFlight?: () => boolean;
  /** Heartbeat cadence on the live connection; 0 disables. Default 5000ms. */
  heartbeatIntervalMs?: number;
  /** First reconnect delay; doubles to reconnectMaxMs. Default 100ms. */
  reconnectMinMs?: number;
  /** Reconnect backoff ceiling. Default 5000ms. */
  reconnectMaxMs?: number;
}

const COMPONENT = "shim-server";

export class SessionServer {
  private conn: MessageConn | null = null;
  private handshaked = false;
  private heartbeatTimer: NodeJS.Timeout | null = null;
  private readonly heartbeatIntervalMs: number;
  /** Set by close(); stops the reconnect loop from resurrecting the link. */
  private closed = false;
  private reconnectTimer: NodeJS.Timeout | null = null;
  private readonly reconnectMinMs: number;
  private readonly reconnectMaxMs: number;
  private reconnectDelayMs: number;
  /** Resolves connect()'s promise on the first successful dial. */
  private resolveFirstConnect: (() => void) | null = null;

  constructor(
    private readonly opts: SessionServerOptions,
    private readonly handlers: SessionServerHandlers,
  ) {
    this.heartbeatIntervalMs = opts.heartbeatIntervalMs ?? 5000;
    this.reconnectMinMs = opts.reconnectMinMs ?? 100;
    this.reconnectMaxMs = opts.reconnectMaxMs ?? 5000;
    this.reconnectDelayMs = this.reconnectMinMs;
  }

  /**
   * Dial the daemon and keep the connection up until close().
   *
   * The shim DIALS; the daemon listens on one socket for every session
   * (design-shim-transport-inversion.md). It used to be the other way round,
   * which put the dialer ahead of the listener: the daemon dialled a
   * session-<id>.sock that this process had not created yet, so the first
   * dial always failed with ENOENT.
   *
   * Retrying belongs here, on the dialer, and it retries FOREVER: the daemon
   * may be down for an arbitrary period, and the SDK turn keeps running while
   * it is (§4.4 — a disconnect never ends the turn). Resolves on the first
   * successful connection.
   */
  connect(): Promise<void> {
    this.closed = false;
    return new Promise((resolve) => {
      this.resolveFirstConnect = resolve;
      this.dial();
    });
  }

  /** One dial attempt; schedules a retry on failure. */
  private dial(): void {
    if (this.closed) return;
    const socket = net.connect(this.opts.socketPath);
    const onError = (err: Error) => {
      socket.destroy();
      if (this.closed) return;
      shimLog(COMPONENT, { session: this.opts.sessionId },
        `daemon not reachable at ${this.opts.socketPath} (${err.message}); retrying in ${this.reconnectDelayMs}ms`);
      this.scheduleDial();
    };
    socket.once("error", onError);
    socket.once("connect", () => {
      socket.removeListener("error", onError);
      this.reconnectDelayMs = this.reconnectMinMs;
      this.onConnection(socket);
      const resolveFirst = this.resolveFirstConnect;
      this.resolveFirstConnect = null;
      resolveFirst?.();
    });
  }

  /** Arm the next dial with capped exponential backoff. */
  private scheduleDial(): void {
    if (this.closed || this.reconnectTimer) return;
    const delay = this.reconnectDelayMs;
    this.reconnectDelayMs = Math.min(this.reconnectDelayMs * 2, this.reconnectMaxMs);
    this.reconnectTimer = setTimeout(() => {
      this.reconnectTimer = null;
      this.dial();
    }, delay);
    this.reconnectTimer.unref?.();
  }

  /** True while a handshaked daemon connection is live. */
  isConnected(): boolean {
    return this.conn !== null && this.handshaked;
  }

  /**
   * Forward one store-merged Event to the daemon. Dropped (with a debug log)
   * when no daemon is attached — the event is durable in the store and
   * replayed on the next Subscribe, so buffering here would be a forbidden
   * spill path.
   */
  sendEvent(evt: Event): void {
    if (!this.isConnected()) {
      shimLog(COMPONENT, { session: this.opts.sessionId, seq: evt.seq }, `no daemon attached; event not forwarded (durable in store, replays on resubscribe)`);
      return;
    }
    this.conn!.send(EventSchema, evt);
  }

  /**
   * Forward one REPLAYED historical event, tagged with the request that asked
   * for it.
   *
   * A ReplayEvent is a different wire type from an Event on purpose: the
   * daemon's demux routes it to conversation translation only, and cannot
   * confuse replayed history for live state even by mistake (core.proto).
   */
  sendReplayEvent(requestId: string, evt: Event): void {
    if (!this.isConnected()) {
      shimLog(COMPONENT, { session: this.opts.sessionId, request: requestId, seq: evt.seq }, `no daemon attached; replay event not forwarded`);
      return;
    }
    this.conn!.send(ReplayEventSchema, create(ReplayEventSchema, { requestId, event: evt }));
  }

  /** Close a replay. Exactly one per ReplayRequest, whatever the outcome. */
  sendReplayDone(done: ReplayDone): void {
    if (!this.isConnected()) {
      shimLog(COMPONENT, { session: this.opts.sessionId, request: done.requestId }, `no daemon attached; replay completion not forwarded`);
      return;
    }
    this.conn!.send(ReplayDoneSchema, done);
  }

  /** Send a canUseTool PermissionRequest to the daemon. */
  sendPermissionRequest(req: PermissionRequest): void {
    if (!this.isConnected()) {
      shimLog(COMPONENT, { session: this.opts.sessionId, request: req.requestId }, `no daemon attached; permission request cannot be delivered`);
      return;
    }
    this.conn!.send(PermissionRequestSchema, req);
  }

  /** Drop the connection and stop reconnecting. Does not touch the SDK turn. */
  close(): Promise<void> {
    this.closed = true;
    this.stopHeartbeat();
    if (this.reconnectTimer) {
      clearTimeout(this.reconnectTimer);
      this.reconnectTimer = null;
    }
    this.resolveFirstConnect = null;
    if (this.conn) {
      this.conn.close();
      this.conn = null;
    }
    this.handshaked = false;
    return Promise.resolve();
  }

  private onConnection(socket: net.Socket): void {
    // Only ever one outbound connection now: a redial happens strictly after
    // the previous one closed, so there is no older peer to retire.
    const conn = new MessageConn(
      socket,
      {
        onMessage: (msg) => this.onMessage(msg),
        onClose: (err) => this.onConnClose(conn, err),
      },
      COMPONENT,
    );
    this.conn = conn;
    this.handshaked = false;
    // The DIALER speaks first, and this frame is also what identifies the
    // session to the daemon's listener, which routes the connection by it.
    conn.send(ShimHelloSchema, create(ShimHelloSchema, {
      sessionId: this.opts.sessionId,
      vendor: this.opts.vendor ?? "claude",
      shimVersion: this.opts.shimVersion,
      protocolVersion: this.opts.protocolVersion,
      turnInFlight: this.opts.turnInFlight ? this.opts.turnInFlight() : false,
    }));
    shimLog(COMPONENT, { session: this.opts.sessionId }, `connected to daemon at ${this.opts.socketPath}; ShimHello sent`);
  }

  private onMessage(msg: Any): void {
    if (!this.handshaked) {
      const hello = unpackAs(msg, DaemonHelloSchema);
      if (!hello) {
        // Anything before the DaemonHello is a protocol error; loud, ignored.
        shimLog(COMPONENT, { session: this.opts.sessionId }, `ignoring ${envelopeType(msg)} received before DaemonHello`);
        return;
      }
      this.handshaked = true;
      this.startHeartbeat();
      shimLog(COMPONENT, { session: this.opts.sessionId }, `handshake complete (daemon ${hello.daemonVersion || "?"})`);
      this.handlers.onDaemonConnected?.(hello);
      return;
    }
    this.dispatch(msg);
  }

  private dispatch(msg: Any): void {
    const submit = unpackAs(msg, SubmitPromptSchema);
    if (submit) {
      this.sendReceipt(this.handlers.onSubmitPrompt(submit));
      return;
    }
    const interrupt = unpackAs(msg, InterruptSchema);
    if (interrupt) {
      this.sendReceipt(this.handlers.onInterrupt(interrupt));
      return;
    }
    const perm = unpackAs(msg, PermissionResponseSchema);
    if (perm) {
      this.handlers.onPermissionResponse(perm);
      return;
    }
    const sub = unpackAs(msg, SubscribeSchema);
    if (sub) {
      shimLog(COMPONENT, { session: this.opts.sessionId, from_seq: sub.fromSeq }, `daemon subscribed`);
      this.handlers.onSubscribe(sub);
      return;
    }
    const replay = unpackAs(msg, ReplayRequestSchema);
    if (replay) {
      shimLog(COMPONENT, { session: this.opts.sessionId, request: replay.requestId, from_seq: replay.fromSeq, to_seq: replay.toSeq },
        `daemon requested a bounded history replay`);
      this.handlers.onReplayRequest(replay);
      return;
    }
    const hb = unpackAs(msg, HeartbeatSchema);
    if (hb) {
      return; // liveness only; nothing to do
    }
    shimLog(COMPONENT, { session: this.opts.sessionId }, `unhandled control message ${envelopeType(msg)}`);
  }

  private sendReceipt(receipt: Receipt): void {
    if (receipt.$typeName === AckSchema.typeName) {
      this.conn?.send(AckSchema, receipt as Ack);
    } else {
      this.conn?.send(NackSchema, receipt as Nack);
    }
  }

  private onConnClose(conn: MessageConn, err: Error | null): void {
    if (this.conn !== conn) return; // a superseded connection closing late
    this.conn = null;
    this.handshaked = false;
    this.stopHeartbeat();
    if (err) {
      shimLog(COMPONENT, { session: this.opts.sessionId }, `daemon connection lost: ${err.message} (turn survives; awaiting reattach)`);
    } else {
      shimLog(COMPONENT, { session: this.opts.sessionId }, `daemon disconnected cleanly (turn survives; awaiting reattach)`);
    }
    this.handlers.onDaemonDisconnected?.();
    // Get back to the daemon: the turn is still running and its events are
    // durable in the store, so reconnecting replays them from last_seen_seq.
    this.scheduleDial();
  }

  private startHeartbeat(): void {
    if (this.heartbeatIntervalMs <= 0) return;
    this.stopHeartbeat();
    this.heartbeatTimer = setInterval(() => {
      this.conn?.send(HeartbeatSchema, create(HeartbeatSchema, { sentAtMs: BigInt(Date.now()) }));
    }, this.heartbeatIntervalMs);
    // Do not keep the process alive solely for heartbeats.
    this.heartbeatTimer.unref?.();
  }

  private stopHeartbeat(): void {
    if (this.heartbeatTimer) {
      clearInterval(this.heartbeatTimer);
      this.heartbeatTimer = null;
    }
  }

}
