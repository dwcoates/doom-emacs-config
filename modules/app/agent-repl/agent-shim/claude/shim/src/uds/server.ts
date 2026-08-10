/**
 * The shim's connection to the daemon.
 *
 * The shim OWNS the `session-<id>.sock` and OUTLIVES the daemon: a dropped
 * daemon connection tears nothing down (the SDK turn keeps running, the store
 * subscription keeps landing durable events). A restarted daemon simply
 * reconnects to the same live shim and re-runs the bring-up gate below
 * (design §4.4 REATTACH). This class therefore never ends a turn on
 * disconnect and never buffers events for an absent daemon — unsent events are
 * durable in the store and replayed from the from_seq the next hello carries.
 *
 * THE BRING-UP GATE (core.proto ShimHello). One exchange, three frames:
 *   1. `ShimHello`   — sent by this class the instant a connection is made
 *                      (the DIALER speaks first), and what identifies the
 *                      session to the daemon's listener.
 *   2. `DaemonHello` — the daemon's reply, carrying the from_seq the standing
 *                      store subscription is to be opened at. Handed to
 *                      `onDaemonConnected`, whose owner does that wiring.
 *   3. `ShimReady`   — written by {@link SessionServer.sendReady} once that
 *                      wiring is done. The daemon treats this session as
 *                      driveable only from this frame onwards.
 *
 * There is deliberately no daemon→shim `Subscribe` any more: it made the store
 * subscription a stage BEYOND the handshake, and every bring-up race lived in
 * the gap between the two.
 *
 * Control messages (`SubmitPrompt`, `Interrupt`, `PermissionResponse`,
 * `ReplayRequest`, `HealthCheck`) are dispatched to injected handlers;
 * `SubmitPrompt`/`Interrupt` handlers return the `Ack`/`Nack` receipt this
 * server writes back. Prompt admission is asynchronous because its turn-start
 * quota observation must finish before the prompt enters the SDK.
 */
import net from "node:net";
import {
  MessageConn,
  envelopeType,
  unpackAs,
} from "./framing.js";
import type { Any } from "./framing.js";
import { bindLog } from "./log.js";
import { shimBuildSha } from "../build-identity.js";
import {
  Ack,
  AckSchema,
  CancelDetachedAgents,
  CancelDetachedAgentsSchema,
  DaemonHello,
  DaemonHelloSchema,
  Event,
  EventClass,
  EventSchema,
  HealthCheck,
  HealthCheckSchema,
  HealthStatus,
  HealthStatusSchema,
  Heartbeat,
  HeartbeatSchema,
  Interrupt,
  InterruptSchema,
  ModelCatalog,
  ModelCatalogSchema,
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
  ShimReady,
  ShimReadySchema,
  SubmitPrompt,
  SubmitPromptSchema,
  SetModel,
  QuerySelectedModel,
  QuerySelectedModelSchema,
  SetModelSchema,
} from "./proto.js";
import type { QueryRuntimeIdentity } from "./proto.js";
import { create } from "@bufbuild/protobuf";

/** A synchronous control receipt: either an Ack or a Nack. */
export type Receipt = Ack | Nack;
export type AsyncReceipt = Receipt | Promise<Receipt>;

export interface SessionServerHandlers {
  /** Push a prompt into the SDK turn; return a receipt after admission settles. */
  onSubmitPrompt(msg: SubmitPrompt): AsyncReceipt;
  /** Interrupt the SDK turn; return the sync receipt. */
  onInterrupt(msg: Interrupt): Receipt;
  /**
   * Stop the session's detached background agents; return a receipt once the
   * stops have been delivered.
   *
   * ASYNCHRONOUS, unlike onInterrupt: the stop is a per-task SDK control
   * round-trip, and a receipt naming agents that had not been asked to stop
   * yet would be a receipt for work that had not happened.
   */
  onCancelDetachedAgents(msg: CancelDetachedAgents): AsyncReceipt;
  /** Set the live SDK model and return a receipt after the SDK settles. */
  onSetModel(msg: SetModel): AsyncReceipt;
  /**
   * Answer which model this session is currently running.
   *
   * SYNCHRONOUS, unlike onSetModel: it changes nothing and the answer is
   * already held, so there is no SDK round-trip to await.
   */
  onQuerySelectedModel(msg: QuerySelectedModel): Ack | Nack;
  /** Deliver a permission decision to the blocked canUseTool round-trip. */
  onPermissionResponse(msg: PermissionResponse): void;
  /**
   * Serve a BOUNDED historical replay (core.proto ReplayRequest). Its events
   * go back as ReplayEvent rather than Event, and it must NOT move the
   * standing store subscription.
   */
  onReplayRequest(msg: ReplayRequest): void;
  /** Return a correlated readiness assertion for this live shim session. */
  onHealthCheck(msg: HealthCheck): Promise<HealthStatus> | HealthStatus;
  /**
   * The daemon answered with its DaemonHello: STAGE 2 of the bring-up gate.
   *
   * The implementer completes this session's wiring — the standing store
   * subscription at `hello.fromSeq` above all — and then calls
   * {@link SessionServer.sendReady} to close the gate. It is deliberately not
   * this class's job: the store belongs to the session, and this class owns
   * only the daemon link.
   */
  onDaemonConnected?(hello: DaemonHello): void;
  /** Daemon connection lost (reattach possible; nothing torn down). */
  onDaemonDisconnected?(): void;
}

export interface SessionServerOptions {
  /** The DAEMON socket this shim dials (every session shares one). */
  socketPath: string;
  sessionId: string;
  /** Stable identity of the only SDK query() this shim owns. */
  queryInstanceId: string;
  /** Exact SDK runtime identity observed for queryInstanceId, when available. */
  queryRuntimeIdentity?: () => QueryRuntimeIdentity | undefined;
  /** QueryCreated's exact position in the announced vendor session stream. */
  queryCreatedSeq: () => bigint;
  shimVersion: string;
  protocolVersion: string;
  vendor?: string;
  /** Reported in ShimHello.turn_in_flight so a reattaching daemon knows. */
  turnInFlight?: () => boolean;
  /** Ordered turn identities reported on every reattach handshake. */
  activeTurnIds?: () => string[];
  /**
   * The VENDOR session id this shim is CURRENTLY filing events under, read
   * fresh for every hello (it rotates mid-stream) and reported in
   * ShimHello.vendor_session_id. "" while the shim has not learned it yet.
   */
  vendorSessionId?: () => string;
  /** Heartbeat cadence on the live connection; 0 disables. Default 5000ms. */
  heartbeatIntervalMs?: number;
  /** First reconnect delay; doubles to reconnectMaxMs. Default 100ms. */
  reconnectMinMs?: number;
  /** Reconnect backoff ceiling. Default 5000ms. */
  reconnectMaxMs?: number;
}

const COMPONENT = "shim-server";
const LOGGER = bindLog({ component: COMPONENT, operation: "shim.server.daemon-connection" });

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
  /**
   * The connection each in-flight ReplayRequest arrived on.
   *
   * A REPLAY'S LIFETIME IS ITS CONNECTION'S. The request is a question one
   * daemon connection asked; when that connection dies the question dies with
   * it, and the daemon already knows — it retires every in-flight replay on
   * teardown (shimclient replayRegistry.failAll -> ErrReplayLinkLost) and
   * re-issues the one it still wants the moment this shim's next ShimReady
   * lands (sessioncontroller rearmResyncAfterReattach / runPendingResync).
   *
   * Without this map the shim's own replay loop kept going and wrote its
   * remaining ReplayEvents — and its ReplayDone — onto whatever connection
   * happened to be up by then, where the daemon could only drop them as an
   * unknown request id. Those frames interleaved with a live bring-up for no
   * possible benefit. The entry is what makes "this connection's replay"
   * checkable rather than assumed.
   */
  private readonly replayConns = new Map<string, MessageConn>();

  constructor(
    private readonly opts: SessionServerOptions,
    private readonly handlers: SessionServerHandlers,
  ) {
    if (opts.queryInstanceId.trim() === "") {
      throw new Error("SessionServer requires a non-empty queryInstanceId");
    }
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
      LOGGER.log({ agent_repl_session_id: this.opts.sessionId },
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
   * Forward one store-merged Event to the daemon. Dropped when no daemon is
   * attached — a PERSISTENT event is durable in the store and replayed on the
   * next gate's from_seq, so buffering here would be a forbidden spill path.
   *
   * An EPHEMERAL event has no such second chance. The synthetic seq-0 frames
   * that ride this same call — a DegradedState above all — are never written
   * to the store, so dropping one is permanent: the user is never told the
   * session degraded, and the only trace was an info line. The two outcomes
   * are recorded at the severity each actually carries.
   */
  sendEvent(evt: Event): void {
    if (!this.isConnected()) {
      const durable = evt.class !== EventClass.EPHEMERAL;
      LOGGER.log({
        level: durable ? "info" : "error",
        agent_repl_session_id: this.opts.sessionId,
        seq: evt.seq,
        event_class: EventClass[evt.class],
      }, durable
        ? `no daemon attached; event not forwarded (durable in store, replays on resubscribe)`
        : `no daemon attached; EPHEMERAL event dropped permanently (never written to the store, never replayed)`);
      return;
    }
    this.conn!.send(EventSchema, evt);
  }

  /**
   * Forward a terminal durable event and report whether a handshaked daemon
   * accepted the complete frame into its socket. False leaves the caller's
   * durable-delivery latch armed for the next reconnect/replay.
   */
  async sendEventFlushed(evt: Event): Promise<boolean> {
    if (!this.isConnected()) {
      LOGGER.log({ agent_repl_session_id: this.opts.sessionId, seq: evt.seq },
        "no daemon attached; terminal event remains pending in durable replay");
      return false;
    }
    const conn = this.conn!;
    try {
      await conn.sendFlushed(EventSchema, evt);
      return true;
    } catch (cause) {
      LOGGER.log({ level: "error", agent_repl_session_id: this.opts.sessionId, seq: evt.seq, cause },
        "terminal event frame did not flush; durable replay remains authoritative");
      return false;
    }
  }

  /** Publish the SDK's selectable-model menu to the attached daemon. */
  sendModelCatalog(catalog: ModelCatalog): boolean {
    if (!this.isConnected()) {
      LOGGER.log({ level: "error", agent_repl_session_id: this.opts.sessionId, model_count: catalog.models.length },
        "no daemon attached; model catalog not forwarded");
      return false;
    }
    this.conn!.send(ModelCatalogSchema, catalog);
    LOGGER.log({ agent_repl_session_id: this.opts.sessionId, model_count: catalog.models.length },
      "model catalog forwarded to daemon");
    return true;
  }

  /**
   * STAGE 3 of the bring-up gate: tell the daemon this session is fully wired.
   *
   * Written by the session once its store subscription is open and settled at
   * the from_seq the DaemonHello carried. Nothing else may send it — the whole
   * value of the frame is that its ARRIVAL is proof of the wiring, so a
   * speculative one would be worse than none at all.
   *
   * A gone connection is loud and NOT retried here: the reconnect loop will
   * re-run the entire gate on the next connection, hello and all, which is the
   * correct way to re-establish readiness rather than replaying an ack for a
   * connection that no longer exists.
   */
  sendReady(ready: ShimReady): void {
    if (!this.isConnected()) {
      LOGGER.log({ agent_repl_session_id: this.opts.sessionId, from_seq: ready.fromSeq },
        `bring-up gate ack ABANDONED: the daemon connection went away before ShimReady could be written; the next connection re-runs the whole gate`);
      return;
    }
    this.conn!.send(ShimReadySchema, ready);
    LOGGER.log({
      agent_repl_session_id: this.opts.sessionId,
      from_seq: ready.fromSeq,
      store_key: ready.vendorSessionId,
    }, `bring-up gate CLOSED: ShimReady sent; this session is fully wired`);
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
    if (!this.replayIsLive(requestId, { seq: evt.seq })) return;
    this.conn!.send(ReplayEventSchema, create(ReplayEventSchema, { requestId, event: evt }));
  }

  /**
   * Close a replay. Exactly one per ReplayRequest ON ITS OWN CONNECTION.
   *
   * The old contract was "exactly one, whatever the outcome", which a dropped
   * frame broke: the daemon's request never terminated. It terminates
   * deterministically either way now — this frame when the connection is still
   * the one that asked, and the daemon's own connection-teardown retirement
   * when it is not. There is no outcome in which the request stays in flight.
   */
  sendReplayDone(done: ReplayDone): void {
    const live = this.replayIsLive(done.requestId, { delivered: done.delivered, truncated: done.truncated });
    this.replayConns.delete(done.requestId);
    if (!live) return;
    this.conn!.send(ReplayDoneSchema, done);
  }

  /**
   * Whether requestId's replay may still write to the CURRENT connection.
   *
   * False covers both ways a replay can be stranded, and says which: no daemon
   * at all, or a daemon that is not the one that asked. Neither is a warning
   * any more — the daemon terminates the request itself on the teardown and
   * re-issues it after the next ShimReady, so a stranded frame is an expected
   * consequence of a reattach rather than a hole nobody will notice.
   */
  private replayIsLive(requestId: string, detail: Record<string, unknown>): boolean {
    const origin = this.replayConns.get(requestId);
    if (!this.isConnected()) {
      LOGGER.log({ ...detail, agent_repl_session_id: this.opts.sessionId, request_id: requestId },
        `no daemon attached; replay frame dropped — the daemon retired this request when the connection died and re-issues it after the next ShimReady`);
      return false;
    }
    if (origin !== this.conn) {
      LOGGER.log({ ...detail, agent_repl_session_id: this.opts.sessionId, request_id: requestId },
        `replay frame belongs to a SUPERSEDED daemon connection; dropped rather than written to the current one, which never asked this question`);
      return false;
    }
    return true;
  }

  /**
   * Send a canUseTool PermissionRequest to the daemon, reporting whether it
   * reached a live connection.
   *
   * The caller (ControlDispatch) owns the open question and keeps it pending on
   * a `false`, re-sending on the next reattach. Reporting the miss rather than
   * only logging it is what lets that owner distinguish "asked" from "not yet
   * asked" instead of inferring delivery from silence.
   */
  sendPermissionRequest(req: PermissionRequest): boolean {
    if (!this.isConnected()) {
      LOGGER.log({ level: "error", agent_repl_session_id: this.opts.sessionId, request_id: req.requestId }, `no daemon attached; permission request cannot be delivered now and awaits re-send on reattach`);
      return false;
    }
    this.conn!.send(PermissionRequestSchema, req);
    return true;
  }

  /**
   * DELIBERATELY drop the live daemon connection and immediately dial a new
   * one. Unlike {@link close} this is not a teardown: the reconnect loop stays
   * armed and the SDK turn is untouched.
   *
   * The bounce is the shim-initiated half of a vendor session ROTATION
   * (store-client.ts rotateStoreKey). A daemon disconnect already tears
   * nothing down, and the connection it opens re-runs the WHOLE bring-up gate
   * — the new uuid announced on its ShimHello, the daemon's reset from_seq
   * returned on its DaemonHello, the store subscription reopened there, and
   * the ShimReady closing it. The rotation therefore needs no staging of its
   * own; it costs one round trip and re-establishes every link at once.
   *
   * The backoff is reset first — this drop is our own doing, not evidence that
   * the daemon is unwell, so the redial should not inherit a penalty earned by
   * earlier failures.
   */
  bounce(reason: string): void {
    if (this.closed) {
      LOGGER.log({ agent_repl_session_id: this.opts.sessionId },
        `daemon link bounce ignored (${reason}): the server is deliberately closed`);
      return;
    }
    this.reconnectDelayMs = this.reconnectMinMs;
    if (!this.conn) {
      // Nothing live to drop; the reconnect loop is already trying, and the
      // hello it eventually sends reads the CURRENT vendor id anyway.
      LOGGER.log({ agent_repl_session_id: this.opts.sessionId },
        `daemon link bounce (${reason}) found no live connection; the pending redial will carry the new hello`);
      this.scheduleDial();
      return;
    }
    LOGGER.log({ agent_repl_session_id: this.opts.sessionId },
      `BOUNCING the daemon link deliberately (${reason}); the re-handshake re-runs the whole bring-up gate under the current vendor session id`);
    // onConnClose fires from the socket's close event and re-arms the dial.
    this.conn.close();
  }

  /** Drop the connection and stop reconnecting. Does not touch the SDK turn. */
  close(): Promise<void> {
    LOGGER.log({ agent_repl_session_id: this.opts.sessionId, connected: this.conn !== null },
      "closing daemon UDS connection and disarming reconnect");
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
    this.replayConns.clear();
    LOGGER.log({ agent_repl_session_id: this.opts.sessionId }, "daemon UDS connection closed");
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
    // STAGE 1. The DIALER speaks first, and this frame is also what identifies
    // the session to the daemon's listener, which routes the connection by it.
    const runtimeIdentity = this.opts.queryRuntimeIdentity?.();
    const queryCreatedSeq = this.opts.queryCreatedSeq();
    conn.send(ShimHelloSchema, create(ShimHelloSchema, {
      sessionId: this.opts.sessionId,
      queryInstanceId: this.opts.queryInstanceId,
      queryCreatedSeq,
      ...(runtimeIdentity === undefined ? {} : { queryRuntimeIdentity: runtimeIdentity }),
      vendor: this.opts.vendor ?? "claude",
      shimVersion: this.opts.shimVersion,
      protocolVersion: this.opts.protocolVersion,
      turnInFlight: this.opts.turnInFlight ? this.opts.turnInFlight() : false,
      activeTurnIds: this.opts.activeTurnIds ? this.opts.activeTurnIds() : [],
      // Read fresh on every hello: the vendor rotates this uuid mid-stream,
      // and the whole point of a rotation bounce is that the NEXT hello
      // announces the NEW identity.
      vendorSessionId: this.opts.vendorSessionId ? this.opts.vendorSessionId() : "",
      // OUR OWN pid, so a daemon that never spawned this process can still
      // stop it (an explicit session restart, or a bounce onto a new build).
      // Trustworthy precisely because it arrives on the connection: a live
      // connection is proof the process that opened it is still that process.
      pid: process.pid,
      // The bundle this shim is running. The daemon compares it against the
      // current build stamp and bounces a superseded shim onto the new code.
      buildSha: shimBuildSha(),
    }));
    LOGGER.log({ agent_repl_session_id: this.opts.sessionId },
      `bring-up gate OPENED: connected to daemon at ${this.opts.socketPath}; ShimHello sent`);
  }

  private onMessage(msg: Any): void {
    if (!this.handshaked) {
      const hello = unpackAs(msg, DaemonHelloSchema);
      if (!hello) {
        // Anything before the DaemonHello is a protocol error; loud, ignored.
        LOGGER.log({ level: "warn", agent_repl_session_id: this.opts.sessionId }, `ignoring ${envelopeType(msg)} received before DaemonHello`);
        return;
      }
      // Handshaked BEFORE the wiring completes, deliberately: the wiring's own
      // sad path (a DegradedState) and its ShimReady both have to be writable
      // on this connection. What the daemon gates on is the ShimReady, not
      // this flag, so nothing is admitted early by it.
      this.handshaked = true;
      this.startHeartbeat();
      LOGGER.log({ agent_repl_session_id: this.opts.sessionId, from_seq: hello.fromSeq },
        `bring-up gate STAGE 2: DaemonHello received (daemon ${hello.daemonVersion || "?"}); wiring the session at the from_seq it carries`);
      this.handlers.onDaemonConnected?.(hello);
      return;
    }
    this.dispatch(msg);
  }

  private dispatch(msg: Any): void {
    const submit = unpackAs(msg, SubmitPromptSchema);
    if (submit) {
      this.sendAsyncReceipt(this.handlers.onSubmitPrompt(submit));
      return;
    }
    const interrupt = unpackAs(msg, InterruptSchema);
    if (interrupt) {
      this.sendReceipt(this.handlers.onInterrupt(interrupt));
      return;
    }
    const cancelDetached = unpackAs(msg, CancelDetachedAgentsSchema);
    if (cancelDetached) {
      this.sendAsyncReceipt(this.handlers.onCancelDetachedAgents(cancelDetached));
      return;
    }
    const setModel = unpackAs(msg, SetModelSchema);
    if (setModel) {
      this.sendAsyncReceipt(this.handlers.onSetModel(setModel));
      return;
    }
    const querySelectedModel = unpackAs(msg, QuerySelectedModelSchema);
    if (querySelectedModel) {
      this.sendReceipt(this.handlers.onQuerySelectedModel(querySelectedModel));
      return;
    }
    const perm = unpackAs(msg, PermissionResponseSchema);
    if (perm) {
      this.handlers.onPermissionResponse(perm);
      return;
    }
    const replay = unpackAs(msg, ReplayRequestSchema);
    if (replay) {
      // Bound to THIS connection before the handler can emit a single frame:
      // the replay may only ever answer the connection that asked it.
      this.replayConns.set(replay.requestId, this.conn!);
      LOGGER.log({ agent_repl_session_id: this.opts.sessionId, request_id: replay.requestId, from_seq: replay.fromSeq, to_seq: replay.toSeq },
        `daemon requested a bounded history replay`);
      this.handlers.onReplayRequest(replay);
      return;
    }
    const health = unpackAs(msg, HealthCheckSchema);
    if (health) {
      void this.answerHealthCheck(health);
      return;
    }
    const hb = unpackAs(msg, HeartbeatSchema);
    if (hb) {
      return; // liveness only; nothing to do
    }
    LOGGER.log({ level: "warn", agent_repl_session_id: this.opts.sessionId }, `unhandled control message ${envelopeType(msg)}`);
  }

  private sendAsyncReceipt(receipt: AsyncReceipt): void {
    // Synchronous controls retain their same-turn ordering with SDK events.
    // Prompt admission and set-model are asynchronous because their SDK work
    // must settle before a successful receipt can be truthful.
    if (!(receipt instanceof Promise)) {
      this.sendReceipt(receipt);
      return;
    }
    void receipt.then((resolved) => this.sendReceipt(resolved), (err: unknown) => {
      throw new Error(`shim control handler rejected without a receipt: ${String(err)}`);
    });
  }

  /**
   * Answer a daemon's correlated session-health probe.  Failure is expressed
   * as an unhealthy status rather than leaving the caller to time out: the
   * daemon must know precisely which session cannot be restored yet.
   */
  private async answerHealthCheck(check: HealthCheck): Promise<void> {
    let status: HealthStatus;
    try {
      if (check.requestId === "") throw new Error("health check requires request_id");
      status = await this.handlers.onHealthCheck(check);
      if (status.requestId !== check.requestId) {
        throw new Error(`health handler returned request_id=${JSON.stringify(status.requestId)}, want ${JSON.stringify(check.requestId)}`);
      }
    } catch (err) {
      const reason = err instanceof Error ? err.message : String(err);
      LOGGER.log({ level: "error", agent_repl_session_id: this.opts.sessionId, request_id: check.requestId, error: reason },
        `health FAIL: ${reason}`);
      status = create(HealthStatusSchema, {
        requestId: check.requestId,
        healthy: false,
        component: "claude-shim",
        reason,
      });
    }
    if (!this.isConnected()) {
      LOGGER.log({ agent_repl_session_id: this.opts.sessionId, request_id: check.requestId },
        `health response abandoned: daemon connection is no longer handshaked`);
      return;
    }
    this.conn!.send(HealthStatusSchema, status);
    LOGGER.log({
      agent_repl_session_id: this.opts.sessionId,
      request_id: check.requestId,
      healthy: status.healthy,
      component: status.component,
      reason: status.reason,
    }, status.healthy ? "health PASS" : "health FAIL");
  }

  /**
   * Send one control receipt back to the daemon.
   *
   * A RECEIPT FOR A REQUEST THIS SESSION ALREADY RAN IS NOT DISCARDABLE
   * SILENCE. The handler that produced it has already had its effect — an
   * interrupt's SDK abort above all — so a connection that died in between
   * leaves the daemon holding the request's fate without its outcome. That is a
   * real divergence and it is recorded here rather than absorbed by the
   * optional chain, because the daemon's own account of it
   * (`shimclient.ErrDeliveredUnacked`) is only legible beside this line.
   *
   * It is not retried or buffered. The effect is already in the store, and the
   * daemon resolves the outcome from the reattach handshake and the durable
   * record — never by asking for the same control a second time.
   */
  private sendReceipt(receipt: Receipt): void {
    if (this.conn === null) {
      LOGGER.log({
        level: "error",
        agent_repl_session_id: this.opts.sessionId,
        request_id: receipt.requestId,
        receipt_kind: receipt.$typeName === AckSchema.typeName ? "ack" : "nack",
      }, "no daemon attached; control receipt DROPPED for a request this session already ran (the daemon resolves its outcome from the reattach and the durable record, never by re-sending)");
      return;
    }
    if (receipt.$typeName === AckSchema.typeName) {
      this.conn.send(AckSchema, receipt as Ack);
    } else {
      this.conn.send(NackSchema, receipt as Nack);
    }
  }

  /**
   * DESIGN DEPENDENCE — THE SHIM MUST KEEP WORKING WHEN THIS CONNECTION CLOSES
   * OR IS LOST.
   *
   * A closed daemon connection is a routine condition, not a fault: it is what
   * every daemon restart, deploy and reload looks like from here. So the close
   * tears nothing down. The SDK query keeps running, an interrupt already
   * dispatched completes locally, pending permission waits stay open, and every
   * persistent event keeps being written to the store with nothing listening on
   * this socket.
   *
   * THAT IS LICENSED BY THE STORE BEING WHAT RESOLVES THE DIVERGENCE WHEN THE
   * CONNECTION IS REESTABLISHED. The two sides diverge for exactly as long as
   * the link is down, and the reconnect does not reconstruct the gap from
   * either side's memory: the daemon replays from its own durable cursor and
   * reads the durable record, which is what settles a turn that finished in the
   * gap and answers a control whose receipt was lost. A shim that stopped,
   * cancelled work, or buffered frames on close would destroy the very evidence
   * that reconciliation reads.
   */
  private onConnClose(conn: MessageConn, err: Error | null): void {
    if (this.conn !== conn) return; // a superseded connection closing late
    this.conn = null;
    this.handshaked = false;
    this.stopHeartbeat();
    // Every in-flight replay was asked by the connection that just died, and
    // the daemon has already retired them all on its own side. Forgetting them
    // here is what keeps a later frame from being matched against a stale
    // entry; replayIsLive then reports it as stranded rather than sending it.
    const stranded = [...this.replayConns.keys()];
    this.replayConns.clear();
    if (stranded.length > 0) {
      LOGGER.log({ agent_repl_session_id: this.opts.sessionId, request_ids: stranded },
        `daemon connection closed under ${stranded.length} in-flight replay request(s); each is retired with the connection and re-issued by the daemon after the next ShimReady`);
    }
    if (err) {
      // MessageConn owns and already recorded the causal transport failure.
      // This layer records only the semantic state transition.
      LOGGER.log({ agent_repl_session_id: this.opts.sessionId, close_outcome: "transport_error" }, "daemon transport closed; turn survives and awaits reattach");
    } else {
      LOGGER.log({ agent_repl_session_id: this.opts.sessionId }, `daemon disconnected cleanly (turn survives; awaiting reattach)`);
    }
    this.handlers.onDaemonDisconnected?.();
    // Get back to the daemon: the turn is still running and its events are
    // durable in the store, so the next gate replays them from its from_seq.
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
