/**
 * The per-session UDS listener the daemon connects to.
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
import fs from "node:fs";
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
  /** Daemon (re)connected and completed the handshake. */
  onDaemonConnected?(hello: DaemonHello): void;
  /** Daemon connection lost (reattach possible; nothing torn down). */
  onDaemonDisconnected?(): void;
}

export interface SessionServerOptions {
  socketPath: string;
  sessionId: string;
  shimVersion: string;
  protocolVersion: string;
  vendor?: string;
  /** Reported in ShimHello.turn_in_flight so a reattaching daemon knows. */
  turnInFlight?: () => boolean;
  /** Heartbeat cadence on the live connection; 0 disables. Default 5000ms. */
  heartbeatIntervalMs?: number;
}

const COMPONENT = "shim-server";

export class SessionServer {
  private server: net.Server | null = null;
  private conn: MessageConn | null = null;
  private handshaked = false;
  private heartbeatTimer: NodeJS.Timeout | null = null;
  private readonly heartbeatIntervalMs: number;

  constructor(
    private readonly opts: SessionServerOptions,
    private readonly handlers: SessionServerHandlers,
  ) {
    this.heartbeatIntervalMs = opts.heartbeatIntervalMs ?? 5000;
  }

  /** Bind and listen on the session socket, replacing any stale file. */
  listen(): Promise<void> {
    return new Promise((resolve, reject) => {
      this.unlinkStaleSocket();
      const server = net.createServer((socket) => this.onConnection(socket));
      server.once("error", reject);
      server.listen(this.opts.socketPath, () => {
        server.removeListener("error", reject);
        this.server = server;
        shimLog(COMPONENT, { session: this.opts.sessionId }, `listening on ${this.opts.socketPath}`);
        resolve();
      });
    });
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

  /** Send a canUseTool PermissionRequest to the daemon. */
  sendPermissionRequest(req: PermissionRequest): void {
    if (!this.isConnected()) {
      shimLog(COMPONENT, { session: this.opts.sessionId, request: req.requestId }, `no daemon attached; permission request cannot be delivered`);
      return;
    }
    this.conn!.send(PermissionRequestSchema, req);
  }

  /** Stop listening and drop any connection. Does not touch the SDK turn. */
  close(): Promise<void> {
    this.stopHeartbeat();
    if (this.conn) {
      this.conn.close();
      this.conn = null;
    }
    this.handshaked = false;
    return new Promise((resolve) => {
      if (!this.server) {
        resolve();
        return;
      }
      const server = this.server;
      this.server = null;
      server.close(() => {
        this.unlinkStaleSocket();
        resolve();
      });
    });
  }

  private onConnection(socket: net.Socket): void {
    // A daemon restart may connect anew while the old socket is still
    // half-dead; the newest connection is the live daemon, so retire the old.
    if (this.conn) {
      shimLog(COMPONENT, { session: this.opts.sessionId }, `new daemon connection supersedes the previous one`);
      this.conn.close();
      this.conn = null;
      this.handshaked = false;
      this.stopHeartbeat();
    }
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
    // The listener speaks first.
    conn.send(ShimHelloSchema, create(ShimHelloSchema, {
      sessionId: this.opts.sessionId,
      vendor: this.opts.vendor ?? "claude",
      shimVersion: this.opts.shimVersion,
      protocolVersion: this.opts.protocolVersion,
      turnInFlight: this.opts.turnInFlight ? this.opts.turnInFlight() : false,
    }));
    shimLog(COMPONENT, { session: this.opts.sessionId }, `daemon connected; ShimHello sent`);
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

  private unlinkStaleSocket(): void {
    try {
      fs.unlinkSync(this.opts.socketPath);
    } catch (err) {
      // ENOENT is the normal case (no stale socket); anything else is loud.
      if ((err as NodeJS.ErrnoException).code !== "ENOENT") {
        shimLog(COMPONENT, { session: this.opts.sessionId }, `could not unlink stale socket ${this.opts.socketPath}: ${(err as Error).message}`);
      }
    }
  }
}
