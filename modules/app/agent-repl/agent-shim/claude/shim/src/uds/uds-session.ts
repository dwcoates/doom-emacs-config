/**
 * UdsSession — the UDS-mode session engine (design §8, stitch task S5).
 *
 * This is the UDS-mode twin of the stdio {@link import("../session.js").ShimSession}.
 * Where ShimSession maps SDK messages onto the Layer-1 NDJSON protocol, this
 * class binds the SAME SDK-driving machinery (an {@link AsyncQueue} streaming
 * input, a `query()` pump, a `canUseTool` round-trip) onto the merged G4/G5
 * layers:
 *
 *   - CONTROL: a {@link SessionServer} listens on `session-<id>.sock`; the
 *     daemon connects. Inbound SubmitPrompt/Interrupt/PermissionResponse are
 *     dispatched through a {@link ControlDispatch} bound to this session's live
 *     SDK query (the {@link SdkControlTarget} below). A daemon disconnect tears
 *     nothing down — the turn keeps running and a restarted daemon reattaches
 *     (§4.4).
 *   - EVENTS: every SDK message is classified. `stream_event`/`tool_progress`
 *     are EPHEMERAL — mapped by {@link toEphemeralEvent} and sent STRAIGHT to
 *     the daemon, NEVER written to the store (the §4.3 delta bypass). Every
 *     other message is {@link convert}ed to `{ vendor, lifecycle }` and written
 *     to the store; the store merges/seq-stamps and feeds the merged stream
 *     back via `onMerged`, which forwards it to the daemon (the §4.2 round-trip).
 *   - SAD PATH: a store outage surfaces as an `Event(DegradedState)` forwarded
 *     to the daemon (StoreClient already loud-logs each dropped event).
 *
 * LIFETIME (§4.4): the UDS server owns lifetime. A daemon disconnect does NOT
 * end the session or the in-flight turn. There is no stdin in UDS mode, so
 * stdin-EOF is not a stop path; the explicit stop path is {@link shutdown}
 * (wired to SIGTERM by main.ts). `ShimHello.turnInFlight` is wired to the live
 * turn count so a reattaching daemon learns whether a turn is running.
 */
import { AsyncQueue } from "../input-queue.js";
import { create } from "@bufbuild/protobuf";
import type { JsonObject } from "@bufbuild/protobuf";
import { isPermissionMode, type ContentBlock, type PermissionMode } from "../protocol.js";
import type {
  CanUseToolLike,
  PermissionResultLike,
  QueryLike,
  SdkMessageLike,
  SdkUserMessageLike,
} from "../session.js";
import { convert } from "../proto/convert.js";
import { isEphemeral, toEphemeralEvent, StreamMessageTracker } from "../proto/delta.js";
import { ControlDispatch, type SdkControlTarget, type ToolPermissionResult } from "./control.js";
import { SessionServer, type SessionServerHandlers } from "./server.js";
import { StoreClient } from "./store-client.js";
import { shimLog } from "./log.js";
import {
  DegradedState,
  DegradedStateSchema,
  Event,
  EventClass,
  EventSchema,
  Plane,
  SessionSource,
} from "./proto.js";

const COMPONENT = "uds-session";

export interface UdsSessionDeps {
  sessionId: string;
  shimVersion: string;
  protocolVersion: string;
  /** Where the shim listens for the daemon (session-<id>.sock). */
  udsSocketPath: string;
  /** Where the shim connects to the shim-store (store.sock). */
  storeSocketPath: string;
  /**
   * SESSION_SOURCE_RESUME when the shim was spawned with `--resume`, FRESH
   * otherwise. Threaded into every convert() so `system:init`'s SessionStarted
   * twin reports the true origin (design §5.2 SessionSource).
   */
  sessionSource: SessionSource;
  /**
   * The vendor session id (Claude's uuid) the store keys events by, when
   * already known — i.e. `--resume`. Omit for a fresh session; the uuid is
   * then adopted from the first converted event.
   */
  storeSessionId?: string;
  /** Construct the SDK query over the streaming input iterable. */
  createQuery: (
    prompt: AsyncIterable<SdkUserMessageLike>,
    canUseTool: CanUseToolLike,
  ) => QueryLike;
  /** Request-id minter for permission round-trips; defaults to randomUUID. */
  newRequestId?: () => string;
  /** Heartbeat cadence on both UDS connections; 0 disables. Test injects 0. */
  heartbeatIntervalMs?: number;
  /** Wall-clock injection for deterministic tests. */
  nowMs?: () => number;
}

export class UdsSession {
  private readonly input = new AsyncQueue<SdkUserMessageLike>();
  private readonly control: ControlDispatch;
  private readonly store: StoreClient;
  private readonly server: SessionServer;
  private query: QueryLike | null = null;
  /**
   * Outstanding-turn COUNTER (prompts submitted minus results seen). A counter
   * not a boolean for the same reason ShimSession keeps one: streaming input
   * queues turns, so a boolean would misreport a queued turn. Read by the
   * server's ShimHello.turn_in_flight so a reattaching daemon knows.
   */
  private turnsInFlight = 0;
  private closed = false;
  /**
   * Which assistant message the SDK is currently streaming. Deltas carry no
   * message identity of their own, so this supplies the one consumers
   * reconcile on — without it every chunk looked like a new message and the
   * frontend opened a bubble per chunk.
   */
  private readonly streamMessages = new StreamMessageTracker();

  constructor(private readonly deps: UdsSessionDeps) {
    const target: SdkControlTarget = {
      submitPrompt: ({ text, permissionMode }): void => {
        this.turnsInFlight++;
        const content: ContentBlock[] = [{ type: "text", text }];
        this.input.push({
          type: "user",
          message: { role: "user", content },
          parent_tool_use_id: null,
          session_id: this.deps.sessionId,
        });
        // A prompt-scoped permission-mode override rides on SubmitPrompt. Apply
        // it to the live query (fire-and-forget: the sync Ack does not wait on
        // the SDK). An unrecognized mode is loud-logged, never silently applied.
        if (permissionMode !== undefined && permissionMode !== "") {
          if (isPermissionMode(permissionMode)) {
            void this.query?.setPermissionMode(permissionMode as PermissionMode).catch((err: unknown) => {
              shimLog(COMPONENT, { session: this.deps.sessionId }, `setPermissionMode failed: ${errMsg(err)}`);
            });
          } else {
            shimLog(COMPONENT, { session: this.deps.sessionId, mode: permissionMode }, `ignoring unknown permission mode on SubmitPrompt`);
          }
        }
      },
      interrupt: (): void => {
        // Interrupt cancels every blocked permission wait so no SDK callback
        // hangs, then forwards to the SDK (a no-op when idle).
        this.control.cancelAll("interrupt");
        void this.query?.interrupt()
          .then((receipt) => {
            // SDK >= 0.3.205 answers with an interrupt receipt. `still_queued`
            // names async messages that SURVIVE the interrupt. Our daemon holds
            // its own queue rather than enqueuing into the CLI, so a non-empty
            // list means work outlived an interrupt the daemon believes it
            // cancelled — reported as honest downtime, never a swallowed log.
            const survivors = receipt?.still_queued ?? [];
            if (survivors.length === 0) return;
            this.reportDegraded(
              "claude-shim-interrupt",
              `interrupt left ${survivors.length} queued message(s) running: ${survivors.join(",")}`,
            );
          })
          .catch((err: unknown) => {
            this.reportDegraded("claude-shim-interrupt", `interrupt failed: ${errMsg(err)}`);
          });
      },
    };
    this.control = new ControlDispatch(
      target,
      (req) => this.server.sendPermissionRequest(req),
      this.deps.newRequestId !== undefined ? { newRequestId: this.deps.newRequestId } : {},
    );

    this.store = new StoreClient({
      socketPath: this.deps.storeSocketPath,
      sessionId: this.deps.sessionId,
      producer: `claude-shim:${this.deps.sessionId}`,
      ...(this.deps.heartbeatIntervalMs !== undefined ? { heartbeatIntervalMs: this.deps.heartbeatIntervalMs } : {}),
      ...(this.deps.storeSessionId !== undefined ? { storeSessionId: this.deps.storeSessionId } : {}),
    });

    const handlers: SessionServerHandlers = {
      onSubmitPrompt: (m) => this.control.handleSubmitPrompt(m),
      onInterrupt: (m) => this.control.handleInterrupt(m),
      onPermissionResponse: (m) => this.control.handlePermissionResponse(m),
      // The reattach hinge: a daemon Subscribe drives the store re-subscribe
      // that replays from `from_seq` (§4.4).
      onSubscribe: (m) => this.store.subscribe(m.fromSeq),
      onDaemonDisconnected: () => {
        // Reattach (§4.4): the turn keeps running and nothing is torn down.
        // Pending permission waits are NOT cancelled — a reattaching daemon can
        // still answer them; cancelAll fires only on interrupt/shutdown.
        shimLog(COMPONENT, { session: this.deps.sessionId, turn_in_flight: this.turnsInFlight > 0 }, `daemon detached; session and turn survive (awaiting reattach)`);
      },
    };
    this.server = new SessionServer(
      {
        socketPath: this.deps.udsSocketPath,
        sessionId: this.deps.sessionId,
        shimVersion: this.deps.shimVersion,
        protocolVersion: this.deps.protocolVersion,
        turnInFlight: () => this.turnsInFlight > 0,
        ...(this.deps.heartbeatIntervalMs !== undefined ? { heartbeatIntervalMs: this.deps.heartbeatIntervalMs } : {}),
      },
      handlers,
    );
  }

  /**
   * The SDK canUseTool callback: block the tool on a daemon decision via the
   * ControlDispatch PermissionRequest round-trip.
   */
  private readonly canUseTool: CanUseToolLike = (toolName, input) =>
    this.control.requestPermission(toolName, input as JsonObject).then(toPermissionResult);

  /**
   * Construct the query, connect the store, start listening, and pump the SDK
   * stream. Returns the pump promise (resolves when the SDK stream ends).
   */
  async start(): Promise<void> {
    this.query = this.deps.createQuery(this.input, this.canUseTool);
    // The store round-trip feed: merged, seq-stamped events go to the daemon.
    this.store.onMerged((evt) => this.server.sendEvent(evt));
    // Honest sad path: a store outage becomes an Event(DegradedState) forwarded
    // to the daemon (StoreClient has already loud-logged each dropped event).
    this.store.onDegraded((report) => this.server.sendEvent(this.degradedEvent(report)));
    await this.store.connect();
    await this.server.connect();
    return this.pump();
  }

  /** True while a handshaked daemon connection is live (test/diagnostics). */
  isConnected(): boolean {
    return this.server.isConnected();
  }

  /** Outstanding-turn count (test/diagnostics). */
  turnCount(): number {
    return this.turnsInFlight;
  }

  /**
   * The explicit stop path (§4.4): SIGTERM (wired by main.ts) or a deliberate
   * teardown. Cancels blocked permissions, ends the SDK input iterable (which
   * ends the pump), and closes both UDS connections. Idempotent.
   */
  async shutdown(reason = "shutdown"): Promise<void> {
    if (this.closed) return;
    this.closed = true;
    shimLog(COMPONENT, { session: this.deps.sessionId }, `shutdown (${reason})`);
    this.control.cancelAll(reason);
    this.input.end();
    await this.server.close();
    this.store.close();
  }

  private async pump(): Promise<void> {
    try {
      for await (const msg of this.query!) {
        this.routeSdkMessage(msg);
      }
    } catch (err) {
      shimLog(COMPONENT, { session: this.deps.sessionId }, `SDK stream failed: ${errMsg(err)}`);
      // A dead SDK turn is honest downtime: report it to the daemon, then tear
      // the session down (unlike a daemon disconnect, the turn itself is gone).
      this.server.sendEvent(this.degradedEvent(create(DegradedStateSchema, {
        component: "claude-shim-sdk",
        reason: `SDK stream failed: ${errMsg(err)}`,
        droppedCount: 0n,
        recovered: false,
      })));
      await this.shutdown("sdk_error");
      return;
    }
    shimLog(COMPONENT, { session: this.deps.sessionId }, `SDK stream ended`);
    await this.shutdown("sdk_end");
  }

  /**
   * Route one SDK message per the G4 wiring: EPHEMERAL deltas straight to the
   * daemon (never the store); everything else converted and written to the
   * store (whose merged echo returns via onMerged → the daemon).
   */
  private routeSdkMessage(msg: SdkMessageLike): void {
    if (isEphemeral(msg)) {
      // Observe BEFORE converting: a message_start must make its own id
      // current for the deltas that follow it.
      this.streamMessages.observe(msg);
      const evt = toEphemeralEvent(msg, { ...this.convertOpts(), messageId: this.streamMessages.current() });
      if (evt) this.server.sendEvent(evt);
      return;
    }
    // A result closes the turn it belongs to.
    if (msg.type === "result" && this.turnsInFlight > 0) this.turnsInFlight--;
    const { vendor, lifecycle } = convert(msg, { sessionSource: this.deps.sessionSource, ...this.convertOpts() });
    // The converted envelope carries the VENDOR session id (read off the SDK
    // message), which is the id the store files these events under. Adopt it
    // as the subscription key: a fresh session has no other way to learn it,
    // and subscribing under this shim's `--session-id` listens on a channel
    // nothing publishes to.
    this.store.adoptStoreKey(vendor.sessionId);
    void this.store.write([vendor, ...lifecycle]).catch(() => {
      // The honest sad path lives INSIDE StoreClient (loud-log per dropped
      // event + DegradedState to onDegraded). Nothing to add here; we only
      // keep the rejected promise from going unhandled.
    });
  }

  private convertOpts(): { nowMs?: number } {
    return this.deps.nowMs !== undefined ? { nowMs: this.deps.nowMs() } : {};
  }

  private now(): number {
    return this.deps.nowMs !== undefined ? this.deps.nowMs() : Date.now();
  }

  /**
   * Report an SDK-side failure to the daemon as honest downtime.
   *
   * The one channel this session has for "something the user asked for did
   * not happen": a loud log PLUS a DegradedState the daemon can surface. A
   * bare `shimLog` is not enough — nobody watching the UI ever sees it.
   */
  private reportDegraded(component: string, reason: string): void {
    shimLog(COMPONENT, { session: this.deps.sessionId }, reason);
    this.server.sendEvent(this.degradedEvent(create(DegradedStateSchema, {
      component,
      reason,
      droppedCount: 0n,
      recovered: false,
    })));
  }

  /** Wrap a DegradedState as a SYNTHETIC/EPHEMERAL Event for the daemon. */
  private degradedEvent(report: DegradedState): Event {
    return create(EventSchema, {
      sessionId: this.deps.sessionId,
      seq: 0n,
      plane: Plane.SYNTHETIC,
      class: EventClass.EPHEMERAL,
      producedAtMs: BigInt(this.now()),
      payload: { case: "degradedState", value: report },
    });
  }
}

/** Map a ControlDispatch resolution onto the SDK's PermissionResult shape. */
function toPermissionResult(r: ToolPermissionResult): PermissionResultLike {
  return r.behavior === "allow"
    ? { behavior: "allow", updatedInput: r.updatedInput as Record<string, unknown> }
    : { behavior: "deny", message: r.message };
}

function errMsg(err: unknown): string {
  return err instanceof Error ? err.message : String(err);
}
