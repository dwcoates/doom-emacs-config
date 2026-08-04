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
 *   - EVENTS: every SDK message is classified. Live typing and tool heartbeats
 *     are EPHEMERAL — mapped by {@link toEphemeralEvent} and sent STRAIGHT to
 *     the daemon. A stamped `stream_event.message_start` yields persistent
 *     MessageLatency through the store, as do all messages {@link convert}ed
 *     to `{ vendor, lifecycle }`; the store merges/seq-stamps and feeds the
 *     merged stream back via `onMerged`, which forwards it to the daemon.
 *   - SAD PATH: a store outage surfaces as an `Event(DegradedState)` forwarded
 *     to the daemon (StoreClient already loud-logs each dropped event).
 *
 * LIFETIME (§4.4): the UDS server owns lifetime. A daemon disconnect does NOT
 * end the session or the in-flight turn. There is no stdin in UDS mode, so
 * stdin-EOF is not a stop path; the explicit stop path is {@link shutdown}
 * (wired to SIGTERM by main.ts). `ShimHello.turnInFlight` is wired to the live
 * turn count so a reattaching daemon learns whether a turn is running.
 */
import { randomUUID } from "node:crypto";
import { AsyncQueue } from "../input-queue.js";
import { create } from "@bufbuild/protobuf";
import type { JsonObject } from "@bufbuild/protobuf";
import {
  PERMISSION_MODES,
  isPermissionMode,
  isSwitchablePermissionMode,
  type ContentBlock,
  type PermissionMode,
} from "../protocol.js";
import { describeInterruptSurvivors } from "../session.js";
import type {
  CanUseToolLike,
  PermissionResultLike,
  QueryLike,
  SdkMessageLike,
  SdkUserMessageLike,
} from "../session.js";
import { SessionStartGate, convert, promptPreview } from "../proto/convert.js";
import { isEphemeral, toEphemeralEvent, toPersistentEvent, StreamMessageTracker } from "../proto/delta.js";
import { ControlDispatch, ModelSelectionError, type SdkControlTarget, type ToolPermissionResult } from "./control.js";
import { SessionServer, type SessionServerHandlers } from "./server.js";
import { StoreClient, type ReplayOutcome } from "./store-client.js";
import { envelopeIs, unpackAs, type Any } from "./framing.js";
import { ClaudeStreamMessageSchema } from "../../../../../proto/gen/ts/agentshim/data/v1/stream_pb.js";
import { TranscriptLineSchema } from "../../../../../proto/gen/ts/agentshim/data/v1/transcript_pb.js";
import type { ApiUserMessage } from "../../../../../proto/gen/ts/agentshim/data/v1/tools_pb.js";
import { bindLog, setClaudeSessionId } from "./log.js";
import { normalizeModel } from "../model.js";
import { logAssistantApiResponseUsage } from "../usage-log.js";
import {
  fiveHourUsageSample,
  type FiveHourUsageSample,
  type SubscriptionUsageResponse,
} from "../subscription-usage.js";
import {
  DaemonHello,
  DegradedState,
  DegradedStateSchema,
  Event,
  EventClass,
  EventSchema,
  HealthCheck,
  HealthStatus,
  HealthStatusSchema,
  ModelCatalogSchema,
  ModelOptionSchema,
  Plane,
  ReplayDoneSchema,
  InterruptOutcome,
  ReplayRequest,
  SessionSource,
  SessionStartedSchema,
  ShimReadySchema,
  TurnClaimBridgeSchema,
  TurnStartedSchema,
  AccountUsageObservationSchema,
  AccountUsageAvailableSchema,
  AccountUsageUnavailableSchema,
  FiveHourWindowUnavailableSchema,
  QueryCreatedSchema,
  QueryIteratorFailureSchema,
  QueryLifecycleSchema,
  QueryRuntimeIdentitySchema,
  QueryRuntimeObservedSchema,
  QueryStartupFailureSchema,
  QueryTerminatedSchema,
  ResumedQuerySchema,
  TurnEndUsageBoundarySchema,
  TurnStartUsageBoundarySchema,
  UnexpectedQueryEofSchema,
  UsageSamplingFailureSchema,
  UsageServiceUnavailableSchema,
  UsageWindowSchema,
  UtilizationUnavailableSchema,
  FreshQuerySchema,
  IntentionalQueryTerminationSchema,
  EvidenceFingerprintSchema,
  FingerprintUnavailableSchema,
  VendorSessionIdentityUnavailableSchema,
} from "./proto.js";
import type { QueryLifecycle, QueryRuntimeIdentity } from "./proto.js";

const COMPONENT = "uds-session";
const CLAUDE_SHIM_SDK_COMPONENT = "claude-shim-sdk";
const UNEXPECTED_QUERY_TERMINATION_REASON = "unexpected_query_termination";
const LOGGER = bindLog({ component: COMPONENT, operation: "shim.uds-session.lifecycle" });

function completionLatch(): { promise: Promise<void>; resolve: () => void } {
  let resolve!: () => void;
  const promise = new Promise<void>((done) => {
    resolve = done;
  });
  return { promise, resolve };
}

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
  /**
   * The `--permission-mode` argv value the query was CONSTRUCTED with.
   *
   * It is a spawn-time snapshot and the WEAKER of the two channels: the
   * daemon's DaemonHello.permission_mode is read off the session record at
   * handshake time and overrides this (see completeWiring). Kept anyway so the
   * override is a comparison rather than a guess, and so a daemon too old to
   * send the field leaves the session exactly where argv put it.
   *
   * Defaults to "default", which is what the argv parser itself defaults to.
   */
  permissionMode?: PermissionMode;
  /** Construct the one SDK query that this UDS session owns. */
  createQuery: (
    prompt: AsyncIterable<SdkUserMessageLike>,
    canUseTool: CanUseToolLike,
  ) => UdsQuery;
  /** Request-id minter for permission round-trips; defaults to randomUUID. */
  newRequestId?: () => string;
  /** Heartbeat cadence on both UDS connections; 0 disables. Test injects 0. */
  heartbeatIntervalMs?: number;
  /** Wall-clock injection for deterministic tests. */
  nowMs?: () => number;
  /** Stable identity assigned once to the long-lived SDK query. */
  queryInstanceId?: string;
  /** Requested model captured before constructing the SDK query. */
  requestedModel?: string;
  /** Claude Agent SDK version loaded by this shim process. */
  sdkVersion?: string;
  /** Shim build identity embedded in the running bundle. */
  shimBuildSha?: string;
  /**
   * How long a bounded replay waits for the next store frame before deciding
   * its subscription drained. A FAILURE bound, not a pace: a store mid-replay
   * writes back to back. Default 5000ms; tests shorten it.
   */
  replayIdleMs?: number;
}

/**
 * The query and its sole process-cleanup capability belong to one UDS session.
 *
 * The Agent SDK exposes cancellation through an AbortController rather than a
 * Query.close() method. Keeping that controller behind this handle prevents a
 * daemon reconnect, a turn result, or any individual control request from
 * acquiring a second way to end or replace the streaming query.
 */
export interface UdsQuery {
  query: QueryLike;
  /** Read Claude subscription rate-limit state through the live query. */
  subscriptionUsage(): Promise<SubscriptionUsageResponse>;
  abort(): void;
  /**
   * Ends a fake SDK stream after the daemon handshake but before readiness.
   * Production SDK queries never expose this test seam.
   */
  failDuringBringUp?: () => void;
}

/** A live SDK stream ended even though this shim did not begin shutdown. */
export class UnexpectedSdkStreamTerminationError extends Error {
  constructor(
    readonly terminationKind: "iterator_eof" | "iterator_throw",
    cause?: unknown,
  ) {
    super(`SDK stream ${terminationKind === "iterator_eof" ? "ended" : "failed"} outside intentional shim shutdown`,
      cause === undefined ? undefined : { cause });
    this.name = "UnexpectedSdkStreamTerminationError";
  }
}

/** Errors logged by their owning lifecycle layer must not be logged again at process exit. */
export function isUnexpectedSdkStreamTerminationError(err: unknown): err is UnexpectedSdkStreamTerminationError {
  return err instanceof UnexpectedSdkStreamTerminationError;
}

/** A query termination lost its required durable lifecycle receipt. */
export class QueryTerminationPersistenceError extends Error {
  constructor(
    readonly terminationKind:
      | "intentional"
      | "startup_failure"
      | UnexpectedSdkStreamTerminationError["terminationKind"],
    readonly queryInstanceId: string,
    cause: unknown,
  ) {
    super(`query ${terminationKind} termination did not receive a durable store receipt`, { cause });
    this.name = "QueryTerminationPersistenceError";
  }
}

/** A termination-receipt failure is logged where the receipt is owned. */
export function isQueryTerminationPersistenceError(err: unknown): err is QueryTerminationPersistenceError {
  return err instanceof QueryTerminationPersistenceError;
}

/** Query termination and resource cleanup both failed after one owned diagnostic. */
export class QueryTerminationCleanupError extends AggregateError {
  constructor(errors: readonly unknown[], message: string) {
    super(errors, message);
    this.name = "QueryTerminationCleanupError";
  }
}

/** A cleanup aggregate is logged where the termination lifetime is owned. */
export function isQueryTerminationCleanupError(err: unknown): err is QueryTerminationCleanupError {
  return err instanceof QueryTerminationCleanupError;
}

type QueryIdentityState =
  | { case: "fresh-unconfirmed" }
  | { case: "resume-unconfirmed"; requestedVendorSessionId: string }
  | { case: "confirmed"; vendorSessionId: string };

/** A resumed query reported a conversation other than the one it was asked to resume. */
export class ResumeIdentityMismatchError extends Error {
  constructor(
    readonly requestedVendorSessionId: string,
    readonly observedVendorSessionId: string,
  ) {
    super(`resumed SDK query reported vendor session ${JSON.stringify(observedVendorSessionId)} instead of requested session ${JSON.stringify(requestedVendorSessionId)}`);
    this.name = "ResumeIdentityMismatchError";
  }
}

export class UdsSession {
  private readonly input = new AsyncQueue<SdkUserMessageLike>();
  private readonly control: ControlDispatch;
  private readonly store: StoreClient;
  private readonly server: SessionServer;
  private query: UdsQuery | null = null;
  /**
   * Ordered identities of accepted prompts awaiting SDK results.
   *
   * The queue replaces the old count because liveness alone cannot say WHICH
   * turn a result closes. Streaming input is FIFO, so the next result must
   * consume the first id. The same ids ride TurnStarted, TurnEnded, and every
   * reattach ShimHello, making an older result incapable of closing a newer
   * turn at the daemon.
  */
  private readonly activeTurnIds: string[] = [];
  /**
   * The persistent TurnStarted receipt for each active identity.
   *
   * Prompt acceptance can precede a vendor UUID rotation: the start is then
   * filed in the retiring seq space while the SDK result/end belongs to the
   * new one. Keeping the receipt lets routeSdkMessage bridge that same stable
   * turn into the new batch before its end, so a daemon that missed the old
   * tail never sees an unproved completion.
   */
  private readonly activeTurnStarts = new Map<string, Event>();
  /** Five-hour quota observations captured before each prompt reached the SDK. */
  private readonly turnStartUsage = new Map<string, FiveHourUsageSample>();
  /** Serialize quota reads so adjacent turn boundaries cannot be observed out of order. */
  private usageSampleTail: Promise<void> = Promise.resolve();
  /**
   * Result-correlated turns whose TurnEnded batch has not received its durable
   * StoreWriteAck yet. They remain handshake claims across a rotation bounce:
   * removing the id when the SDK merely emits `result` creates a window where
   * ShimHello says idle while the daemon has not observed any end.
   */
  private readonly pendingTurnEndIds: string[] = [];
  /** SDK background tasks whose terminal lifecycle fact has not been stored. */
  private readonly liveSdkTaskIds = new Set<string>();
  /** SDK task ids whose first terminal lifecycle fact has been stored. */
  private readonly completedSdkTaskIds = new Set<string>();
  /** SDK task ids launched by each accepted root turn. */
  private readonly turnSdkTaskIds = new Map<string, Set<string>>();
  /** Task-notification result cycles durably observed for each root turn. */
  private readonly turnTaskNotificationResultCounts = new Map<string, number>();
  private intentionalShutdownReason: string | null = null;
  private readonly intentionalShutdownStarted = completionLatch();
  private shutdownPromise: Promise<void> | null = null;
  private resourcesClosePromise: Promise<void> | null = null;
  private readonly runFinished = completionLatch();
  /** Keeps the dead-query shim alive until its durable typed failure reaches a daemon. */
  private unexpectedTerminationForwarded: ReturnType<typeof completionLatch> | null = null;
  private pumpStarted = false;
  /** Idle bound for a bounded replay's store subscription (see deps). */
  private readonly replayIdleMs: number;
  /**
   * Which assistant message the SDK is currently streaming. Deltas carry no
   * message identity of their own, so this supplies the one consumers
   * reconcile on — without it every chunk looked like a new message and the
   * frontend opened a bubble per chunk.
   */
  private readonly streamMessages = new StreamMessageTracker();
  /**
   * Admits only the FIRST `system:init` of this shim's lifetime to emit a
   * SessionStarted twin. Shim-lifetime state, so it is owned here and injected
   * into every convert() rather than living in the (otherwise stateless)
   * converter.
   */
  private readonly sessionGate = new SessionStartGate();
  /**
   * The mode this session is ACTUALLY running under: argv until the bring-up
   * gate applies the daemon's, then the daemon's. Reported on the readiness
   * announcement so "what posture is this session in?" is answerable from the
   * log rather than by inference over two channels.
   *
   * A per-prompt SubmitPrompt.permission_mode override is deliberately NOT
   * folded in: this is where the session STARTS, and that is a per-turn
   * override applied on top of it.
   */
  private effectivePermissionMode: PermissionMode;
  /** The only model this shim has observed or confirmed as selected. */
  private effectiveModel = "";
  /** One identifier for the only query() invocation the shim is permitted to own. */
  private readonly queryInstanceId: string;
  /** Durable position of QueryCreated and the stream key that owns it. */
  private queryCreatedPosition: { storeKey: string; seq: bigint } | null = null;
  /** Closed identity state for the one query invocation owned by this shim. */
  private queryIdentity: QueryIdentityState;
  /** The latest SDK-observed runtime identity for the query this shim owns. */
  private queryRuntimeIdentity: QueryRuntimeIdentity | null = null;

  constructor(private readonly deps: UdsSessionDeps) {
    this.queryInstanceId = deps.queryInstanceId ?? randomUUID();
    if (deps.sessionSource === SessionSource.RESUME) {
      if (deps.storeSessionId === undefined || deps.storeSessionId.trim() === "") {
        throw new Error("resumed UDS shim session requires a non-empty vendor session id");
      }
      this.queryIdentity = { case: "resume-unconfirmed", requestedVendorSessionId: deps.storeSessionId };
    } else {
      this.queryIdentity = { case: "fresh-unconfirmed" };
    }
    this.replayIdleMs = deps.replayIdleMs ?? 5000;
    this.effectivePermissionMode = deps.permissionMode ?? "default";
    LOGGER.log({ agent_repl_session_id: deps.sessionId, uds_socket: deps.udsSocketPath, store_socket: deps.storeSocketPath, session_source: deps.sessionSource, store_key_known: deps.storeSessionId !== undefined && deps.storeSessionId !== "", permission_mode: this.effectivePermissionMode }, "constructed UDS shim session");
    const target: SdkControlTarget = {
      submitPrompt: async ({ requestId, text, permissionMode }): Promise<void> => {
        const boundaryAtMs = this.now();
        const usage = await this.captureFiveHourUsage("turn_start", requestId);
        const turnStart = this.turnStartedEvent(requestId, text, boundaryAtMs);
        const usageObservation = this.accountUsageObservationEvent(
          "turn_start",
          requestId,
          boundaryAtMs,
          usage,
          turnStart.sessionId,
        );
        LOGGER.log({
          agent_repl_session_id: this.deps.sessionId,
          query_instance_id: this.queryInstanceId,
          turn_id: requestId,
          boundary_at_ms: boundaryAtMs,
          observed_at_ms: usage.observedAtMs,
          measurement_available: usage.measurementAvailable,
          subscription_type: usage.subscriptionType,
          store_key: turnStart.sessionId,
          event_order: ["turnStarted", "accountUsageObservation"],
        }, "persisting ordered turn-start boundary before SDK prompt admission");
        await this.store.write([turnStart, usageObservation]);
        this.activeTurnStarts.set(requestId, turnStart);
        this.turnStartUsage.set(requestId, usage);
        this.activeTurnIds.push(requestId);
        const content: ContentBlock[] = [{ type: "text", text }];
        this.input.push({
          type: "user",
          message: { role: "user", content },
          parent_tool_use_id: null,
          session_id: this.deps.sessionId,
        });
        // The ACCEPT half of the prompt round-trip receipt. Its other half is
        // the forward log below (`user prompt event forwarded to daemon`), so
        // a prompt that reached the SDK but never came back as a bubble shows
        // up as a receipt with no matching forward, rather than as silence.
        // Only the FAILURE path used to say anything (control.ts).
        LOGGER.log({
          agent_repl_session_id: this.deps.sessionId,
          request_id: requestId,
          plane: "stream",
          turn_id: requestId,
          len: text.length,
          turns_in_flight: this.activeTurnIds.length,
          decision: "turn_started",
        }, `prompt accepted -> SDK input`);
        // A prompt-scoped permission-mode override rides on SubmitPrompt. Apply
        // it to the live query. The receipt waits for quota sampling and prompt
        // admission, but not for this independent mode mutation.
        //
        // BOTH failure paths report DegradedState, not just a log line. The
        // user picked a mode; if it did not take, the session is running under
        // a DIFFERENT permission posture than the one they chose, and a log
        // nobody reads is not an acceptable way to tell them that. The gate is
        // `isSwitchablePermissionMode`, not `isPermissionMode`, because
        // bypassPermissions is launch-only and the CLI is guaranteed to reject
        // it here.
        if (permissionMode !== undefined && permissionMode !== "") {
          if (isSwitchablePermissionMode(permissionMode)) {
            void this.query?.query.setPermissionMode(permissionMode as PermissionMode).catch((err: unknown) => {
              this.reportDegraded(
                "claude-shim-permission-mode",
                `permission mode "${permissionMode}" was rejected, session still in the previous mode: ${errMsg(err)}`,
              );
            });
          } else {
            this.reportDegraded(
              "claude-shim-permission-mode",
              `permission mode "${permissionMode}" cannot be set on a running session, session still in the previous mode`,
            );
          }
        }
      },
      interrupt: (): InterruptOutcome => {
        // THE RACE, MADE STRUCTURALLY IMPOSSIBLE.
        //
        // The liveness read happens SYNCHRONOUSLY, here, before any await
        // and before any promise is created. This event loop is
        // single-threaded and this class owns the turn counter — it
        // increments on submit-accept and decrements when the result lands —
        // so no turn can start or end between this read and the outcome it
        // decides. A turn that ends a microsecond later cannot retroactively
        // turn INTERRUPTED into ALREADY_COMPLETE, and one that ended a
        // microsecond earlier is already counted out.
        //
        // Deciding it downstream is what made it ambiguous: an observer
        // watching for the turn's `aborted` result sees the stop and the
        // turn end as two unordered events.
        const wasLive = this.activeTurnIds.length > 0;
        LOGGER.log({
          agent_repl_session_id: this.deps.sessionId,
          active_turn_ids: this.activeTurnIds,
          turns_in_flight: this.activeTurnIds.length,
          interrupt_outcome: wasLive ? "interrupted" : "already_complete",
        }, "processing daemon interrupt request");
        // Interrupt cancels every blocked permission wait so no SDK callback
        // hangs, then forwards to the SDK (a no-op when idle).
        this.control.cancelAll("interrupt");
        if (!this.query) {
          // No SDK query to interrupt: the stop provably cannot be
          // delivered. Previously the optional-chain below swallowed this
          // whole — no forward, no report, and an Ack that read as success.
          this.reportDegraded("claude-shim-interrupt",
            "interrupt could not be delivered: no SDK query is constructed for this session");
          return InterruptOutcome.FAILED;
        }
        void this.query.query.interrupt()
          .then((receipt) => {
            // SDK >= 0.3.205 answers with an interrupt receipt. The anomaly
            // wording is shared with the stdio path (describeInterruptSurvivors)
            // so the two transports cannot describe the same broken assumption
            // two different ways. An empty receipt is the expected case and
            // says nothing; a non-empty one is honest downtime.
            const anomaly = describeInterruptSurvivors(receipt);
            if (anomaly === null) return;
            this.reportDegraded("claude-shim-interrupt", anomaly);
          })
          .catch((err: unknown) => {
            // An async rejection cannot revise an Ack already written, so it
            // keeps its DegradedState channel unchanged. That is the right
            // shape: the outcome is a statement about the TURN at the moment
            // the stop landed, while this is a later transport failure, and
            // collapsing the two would put a stale verdict on the wire.
            this.reportDegraded("claude-shim-interrupt", `interrupt failed: ${errMsg(err)}`);
          });
        return wasLive ? InterruptOutcome.INTERRUPTED : InterruptOutcome.ALREADY_COMPLETE;
      },
      setModel: async ({ requestId, model }): Promise<string> => {
        const normalized = normalizeModel(model);
        if (normalized === "") throw new Error("set-model cannot send an empty or <synthetic> model to the SDK");
        if (this.query === null) {
          throw new Error("set-model cannot run before the SDK query exists");
        }
        try {
          await this.query.query.setModel(normalized);
        } catch (err) {
          // The SDK rejected the mutation.  Return the selection this shim
          // actually knows, so the daemon can reset the UI from authority.
          throw new ModelSelectionError(errMsg(err), this.effectiveModel);
        }
        this.effectiveModel = normalized;
        LOGGER.log({ agent_repl_session_id: this.deps.sessionId, request_id: requestId, model: normalized },
          "model change confirmed by SDK");
        return this.effectiveModel;
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
      onSetModel: (m) => this.control.handleSetModel(m),
      onPermissionResponse: (m) => this.control.handlePermissionResponse(m),
      onReplayRequest: (m) => void this.serveReplay(m),
      onHealthCheck: (m) => this.health(m),
      // THE BRING-UP GATE'S WIRING STAGE. The DaemonHello carries the
      // from_seq this session's standing store subscription is to be opened
      // at; completeWiring opens it and only then acks with ShimReady.
      //
      // It runs on every REATTACH too, and that is the point rather than a
      // flaw: a restarted daemon has no memory of this session and must be
      // wired to it exactly as the first one was.
      onDaemonConnected: (hello) => void this.completeWiring(hello),
      onDaemonDisconnected: () => {
        // Reattach (§4.4): the turn keeps running and nothing is torn down.
        // Pending permission waits are NOT cancelled — a reattaching daemon can
        // still answer them; cancelAll fires only on interrupt/shutdown.
        LOGGER.log({
          agent_repl_session_id: this.deps.sessionId,
          turn_in_flight: this.handshakeTurnIds().length > 0,
          active_turn_ids: this.handshakeTurnIds(),
        }, `daemon detached; session and turn survive (awaiting reattach)`);
      },
    };
    this.server = new SessionServer(
      {
        socketPath: this.deps.udsSocketPath,
        sessionId: this.deps.sessionId,
        queryInstanceId: this.queryInstanceId,
        queryCreatedSeq: () => {
          const position = this.queryCreatedPosition;
          return position !== null && position.storeKey === this.store.storeSessionId()
            ? position.seq
            : 0n;
        },
        queryRuntimeIdentity: () => {
          const identity = this.queryRuntimeIdentity;
          return identity !== null && identity.vendorSessionId === this.store.vendorSessionId()
            ? identity
            : undefined;
        },
        shimVersion: this.deps.shimVersion,
        protocolVersion: this.deps.protocolVersion,
        turnInFlight: () => this.handshakeTurnIds().length > 0,
        activeTurnIds: () => this.handshakeTurnIds(),
        // The daemon resets its store cursor when this differs from the uuid
        // it has persisted, which is how a rotation's fresh seq space is
        // subscribed from zero instead of from a retired high-water mark.
        vendorSessionId: () => this.store.vendorSessionId(),
        ...(this.deps.heartbeatIntervalMs !== undefined ? { heartbeatIntervalMs: this.deps.heartbeatIntervalMs } : {}),
      },
      handlers,
    );
    // THE ROTATION'S OTHER HALF. The store client detects the vendor's new
    // transcript identity and owns the re-key; only this class can reach the
    // daemon link, so the bounce is injected here. It runs BEFORE the re-key
    // (store-client.ts rotateStoreKey), so no event of the new seq space can
    // reach a connection whose last_seen belongs to the retired one.
    this.store.onRotation((previous, next) => {
      LOGGER.log({ agent_repl_session_id: this.deps.sessionId, store_key: previous, rotating_to: next },
        `vendor session rotated; bouncing the daemon link so the re-handshake announces ${next}`);
      this.server.bounce(`vendor session rotation ${previous} -> ${next}`);
    });
  }

  /**
   * Answer only after the active shim has proved its required store paths.
   * Receiving this request itself proves the daemon's UDS connection completed
   * the SessionServer handshake; StoreClient proves the producer, standing
   * subscription, and an independent store protocol round-trip.
   */
  private async health(check: HealthCheck): Promise<HealthStatus> {
    const result = await this.store.health(check.requestId);
    LOGGER.log({
      agent_repl_session_id: this.deps.sessionId,
      request_id: check.requestId,
      healthy: result.healthy,
      store_connected: this.store.isConnected(),
      store_subscribed: this.store.isSubscribed(),
      reason: result.reason,
    }, result.healthy ? "session health PASS" : "session health FAIL");
    return create(HealthStatusSchema, {
      requestId: check.requestId,
      healthy: result.healthy,
      component: "claude-shim",
      reason: result.reason,
    });
  }

  /**
   * Serve one bounded historical replay for the daemon (core.proto
   * ReplayRequest, design §5.4.1).
   *
   * The events come off a THROWAWAY store subscription (StoreClient.replay)
   * and go back as ReplayEvent, which is a different wire type from the live
   * Event stream — so the daemon routes them to conversation translation and
   * nothing else, by frame type rather than by convention.
   *
   * Exactly one ReplayDone is sent, whatever happens. A replay that simply
   * stopped streaming would be indistinguishable from one still in flight, so
   * even an unexpected failure closes the request (loudly, as truncated).
   */
  private async serveReplay(req: ReplayRequest): Promise<void> {
    LOGGER.log({ agent_repl_session_id: this.deps.sessionId, request_id: req.requestId, from_seq: req.fromSeq, to_seq: req.toSeq, max_events: req.maxEvents, idle_ms: this.replayIdleMs }, "serving daemon replay request");
    let outcome: ReplayOutcome;
    try {
      outcome = await this.store.replay({
        fromSeq: req.fromSeq,
        toSeq: req.toSeq,
        maxEvents: req.maxEvents,
        idleMs: this.replayIdleMs,
        onEvent: (evt) => this.server.sendReplayEvent(req.requestId, evt),
      });
    } catch (err) {
      outcome = { delivered: 0, truncated: true, reason: `replay failed: ${errMsg(err)}` };
      LOGGER.log({ level: "error", agent_repl_session_id: this.deps.sessionId, request_id: req.requestId }, outcome.reason);
    }
    this.server.sendReplayDone(create(ReplayDoneSchema, {
      requestId: req.requestId,
      truncated: outcome.truncated,
      reason: outcome.reason,
      delivered: BigInt(outcome.delivered),
    }));
    LOGGER.log({ agent_repl_session_id: this.deps.sessionId, request_id: req.requestId, delivered: outcome.delivered, truncated: outcome.truncated, reason: outcome.reason }, "completed daemon replay request");
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
    try {
      LOGGER.log({ agent_repl_session_id: this.deps.sessionId, store_socket: this.deps.storeSocketPath, daemon_socket: this.deps.udsSocketPath }, "starting UDS shim session dependencies");
      if (this.query !== null) {
        throw new Error("UDS session cannot construct a second SDK query");
      }
      this.query = this.deps.createQuery(this.input, this.canUseTool);
      // The store round-trip feed: merged, seq-stamped events go to the daemon.
      this.store.onMerged((evt) => {
        this.logUserPromptForward(evt);
        if (this.unexpectedTerminationForwarded !== null && this.isUnexpectedTerminationLifecycle(evt)) {
          void this.server.sendEventFlushed(evt).then((forwarded) => {
            if (forwarded) this.unexpectedTerminationForwarded?.resolve();
          });
          return;
        }
        this.server.sendEvent(evt);
      });
      // Honest sad path: a store outage becomes an Event(DegradedState) forwarded
      // to the daemon (StoreClient has already loud-logged each dropped event).
      this.store.onDegraded((report) => this.server.sendEvent(this.degradedEvent(report)));
      await this.store.connect();
      LOGGER.log({ agent_repl_session_id: this.deps.sessionId, store_socket: this.deps.storeSocketPath }, "store producer connection established");
      await this.persistQueryCreated();
      // Readiness is asserted from the handshake hook wired in the
      // constructor, not here: connect() resolves on the DIAL, and an event
      // sent before the DaemonHello would be dropped.
      if (this.intentionalShutdownReason !== null) return;
      await Promise.race([
        this.server.connect(),
        this.intentionalShutdownStarted.promise,
      ]);
      LOGGER.log({ agent_repl_session_id: this.deps.sessionId, daemon_socket: this.deps.udsSocketPath }, "daemon connection established; awaiting bring-up gate");
      if (this.intentionalShutdownReason !== null) return;
      this.pumpStarted = true;
      await this.pump();
    } catch (err) {
      if (this.intentionalShutdownReason !== null) return;
      if (this.pumpStarted) throw err;
      let persistenceFailure: unknown;
      if (this.query !== null) {
        try {
          // Startup failure has the same durable terminal contract as an SDK
          // iterator failure.  The daemon pins its replay cursor at either
          // lifecycle until the immediately following degradation confirms the
          // stable failure class, so emitting only the lifecycle would strand
          // every restored daemon before this terminal fact.
          await this.persistUnexpectedSdkTermination(
            "startup_failure",
            this.queryTerminationEvent("startup_failure", err),
            false,
          );
        } catch (cause) {
          persistenceFailure = cause;
        }
      }
      this.control.cancelAll("startup_failure");
      this.input.end();
      this.query?.abort();
      let cleanupFailure: unknown;
      try {
        await this.closeResources();
      } catch (cause) {
        cleanupFailure = cause;
      }
      if (persistenceFailure !== undefined || cleanupFailure !== undefined) {
        throw new AggregateError(
          [err, persistenceFailure, cleanupFailure].filter((cause) => cause !== undefined),
          "SDK query startup failed with lifecycle evidence or cleanup failure",
        );
      }
      throw err;
    } finally {
      this.runFinished.resolve();
    }
  }

  /**
   * THE BRING-UP GATE, shim side (core.proto ShimHello). Finish wiring this
   * session at the from_seq the DaemonHello carried, then — and only then —
   * ack with ShimReady.
   *
   * WHAT IS ALREADY TRUE WHEN THIS RUNS, and why none of it needs doing here:
   *   - the SESSION LOCK is held, taken in main.ts before this object exists
   *     (a shim that could not claim its session refuses to start at all);
   *   - the SDK QUERY is constructed, and the STORE PRODUCER link is up, both
   *     by `start()` before it ever dials the daemon.
   * Each is therefore a structural precondition of reaching this code, not a
   * check performed by it.
   *
   * WHAT IS LEFT is the standing store subscription, which needs a from_seq
   * only the daemon knows. Awaiting it is the whole point: subscribe()
   * resolves when the Subscribe frame is on a live store connection, so the
   * ack that follows is a statement about a tail that EXISTS.
   *
   * A FAILURE WITHHOLDS THE ACK. No ShimReady is sent, the reason is logged
   * loudly, and a DegradedState carries it to the daemon (which the daemon can
   * surface even though this session is not ready). The daemon's readiness
   * wait then fails on its own deadline — the honest outcome, and far better
   * than acking a session whose store tail does not exist.
   */
  /**
   * Apply DaemonHello.permission_mode as this session's posture, INSIDE the
   * gate. Returns false when the gate must refuse (the caller then withholds
   * ShimReady exactly as it does for a failed store link).
   *
   * WHY IT IS A SWITCH AND NOT A CONSTRUCTION ARGUMENT. `start()` builds the
   * SDK query and only then dials the daemon, so the query already exists when
   * the hello lands — construction is strictly earlier, and there is no
   * ordering in which argv could be superseded at build time. Switching here,
   * before the ack, still gets the posture in place before the first prompt
   * can be delivered: nothing may be sent to a session that has not acked.
   *
   * PRECEDENCE: the handshake wins. argv is a spawn-time snapshot; this is the
   * record read at handshake time, and every reattach re-reads it. An override
   * of a DIFFERING argv value is logged, because a session silently running in
   * a mode other than the one its command line named is exactly the confusion
   * this whole change exists to end.
   *
   * REFUSALS ARE LOUD, NEVER FALLBACKS. An unknown mode, a missing query, a
   * launch-only mode the CLI cannot switch into, and a rejected switch all
   * withhold the ack and report a DegradedState. Falling back to "default"
   * would run the session under a posture nobody chose, which is the original
   * defect wearing a different hat.
   */
  private async applyHandshakePermissionMode(mode: string): Promise<boolean> {
    // Empty means a daemon too old to speak the field (core.proto): argv
    // stands, unchanged, for the rollout window.
    if (mode === "") {
      LOGGER.log({ agent_repl_session_id: this.deps.sessionId, argv_permission_mode: this.effectivePermissionMode }, "DaemonHello omitted permission mode; retaining spawn-time posture");
      return true;
    }
    if (!isPermissionMode(mode)) {
      this.refuseBringUp(`DaemonHello carried permission_mode "${mode}", which is not one of ${PERMISSION_MODES.join(", ")}`);
      return false;
    }
    const argv = this.deps.permissionMode ?? "default";
    if (!isSwitchablePermissionMode(mode)) {
      // LAUNCH-ONLY (bypassPermissions): the CLI refuses to switch into it, so
      // the only way a session can be in it is to have been LAUNCHED in it.
      // Equal-to-argv is therefore the one acceptable case; anything else is a
      // posture this shim cannot deliver, and saying so is the honest answer.
      if (mode !== argv) {
        this.refuseBringUp(`the handshake asked for launch-only permission_mode "${mode}", which the CLI cannot switch a running session into (argv launched it as "${argv}")`);
        return false;
      }
      this.effectivePermissionMode = mode;
      return true;
    }
    if (this.query === null) {
      this.refuseBringUp(`the handshake asked for permission_mode "${mode}" but no SDK query is constructed for this session`);
      return false;
    }
    // ASSERTED EVEN WHEN IT EQUALS ARGV. "the flag must already have taken" is
    // the assumption this whole change exists to stop making — the offline fake
    // query, for one, ignores argv entirely — so the gate STATES the posture on
    // the live query rather than inferring it from the command line.
    try {
      await this.query.query.setPermissionMode(mode);
    } catch (err) {
      this.refuseBringUp(`the handshake's permission_mode "${mode}" was rejected by the CLI: ${errMsg(err)}`);
      return false;
    }
    this.effectivePermissionMode = mode;
    if (mode !== argv) {
      LOGGER.log({ agent_repl_session_id: this.deps.sessionId, permission_mode: mode, argv_permission_mode: argv },
        `handshake permission mode OVERRIDES the --permission-mode argv; this session runs as ${mode}`);
    }
    return true;
  }

  /**
   * Refuse the bring-up gate: loud log, DegradedState to the daemon, and NO
   * ShimReady. The daemon's readiness wait then fails on its own deadline,
   * which is the honest outcome for a session that is not wired as asked.
   */
  private refuseBringUp(reason: string): void {
    this.reportDegraded("claude-shim-bringup", `bring-up gate REFUSED: ${reason}`);
  }

  private async completeWiring(hello: DaemonHello): Promise<void> {
    const fromSeq = hello.fromSeq;
    LOGGER.log({ agent_repl_session_id: this.deps.sessionId, from_seq: fromSeq, store_key: this.store.storeSessionId() },
      `bring-up gate: wiring the standing store subscription before acking readiness`);
    if (!(await this.applyHandshakePermissionMode(hello.permissionMode))) return;
    try {
      await this.store.subscribe(fromSeq);
    } catch (err) {
      const reason = `bring-up gate REFUSED: the standing store subscription could not be opened at from_seq=${fromSeq}: ${errMsg(err)}`;
      // reportDegraded loud-logs AND sends the DegradedState. Readiness is
      // withheld deliberately: there is no ack to send for a session that is
      // not wired, and the daemon must not be told otherwise.
      this.reportDegraded("claude-shim-bringup", reason);
      return;
    }
    if (this.query?.failDuringBringUp !== undefined) {
      this.query.failDuringBringUp();
      return;
    }
    // Readiness first, ack second, in frame order on one connection: the
    // daemon has this session's SessionStarted in hand before the gate that
    // releases its callers closes.
    this.emitSessionStarted();
    this.server.sendReady(create(ShimReadySchema, {
      sessionId: this.deps.sessionId,
      fromSeq,
      vendorSessionId: this.store.storeSessionId(),
    }));
    this.publishModelCatalog();
  }

  /** Publish the query-owned selectable-model menu after the daemon gate closes. */
  private publishModelCatalog(): void {
    if (this.query === null) {
      throw new Error("model catalog cannot publish before the SDK query exists");
    }
    void this.query.query.supportedModels()
      .then((models) => {
        const options = models.map((option) => ({ ...option, value: normalizeModel(option.value) }));
        const malformed = options.find((option) => option.value === "");
        if (malformed !== undefined) {
          throw new Error(`supportedModels returned an empty or <synthetic> option (display_name=${JSON.stringify(malformed.displayName)})`);
        }
        const forwarded = this.server.sendModelCatalog(create(ModelCatalogSchema, {
          sessionId: this.deps.sessionId,
          models: options.map((option) => create(ModelOptionSchema, option)),
        }));
        if (!forwarded) {
          this.reportDegraded("claude-shim-model-catalog", "model catalog publication failed because the daemon link was not live");
        }
      })
      .catch((err: unknown) => {
        const reason = `supportedModels failed: ${errMsg(err)}`;
        LOGGER.log({ level: "error", agent_repl_session_id: this.deps.sessionId }, reason);
        this.server.sendEvent(this.degradedEvent(create(DegradedStateSchema, {
          component: "claude-shim-model-catalog",
          reason,
          droppedCount: 0n,
          recovered: false,
        })));
      });
  }

  /**
   * Announce that this shim is ready, directly to the daemon.
   *
   * WHY NOT THE VENDOR'S system:init. SessionStarted used to ride as a twin
   * of the SDK's `system:init`, but the SDK does not emit that until the
   * FIRST PROMPT — so a session nobody had typed into yet never announced
   * itself, and the workspace sat in bring-up indefinitely waiting for a
   * message the user had no reason to send. Readiness cannot be conditional
   * on being used; that is the circularity this replaces.
   *
   * WHY DIRECT AND NOT THROUGH THE STORE. Every other lifecycle twin takes
   * the store round-trip so it lands seq-ordered against the conversation.
   * Readiness cannot: the store keys events by the VENDOR session id, which
   * on a fresh session is unknown until the SDK reveals it on the first
   * converted message — the first prompt again. Readiness is a fact about the
   * SHIM rather than about the vendor conversation, so it takes the same
   * SYNTHETIC/EPHEMERAL direct path DegradedState does.
   *
   * The gate is CLOSED afterwards so the vendor init's twin does not
   * re-announce a session already announced.
   */
  private emitSessionStarted(): void {
    this.sessionGate.close();
    this.server.sendEvent(create(EventSchema, {
      sessionId: this.deps.sessionId,
      seq: 0n,
      plane: Plane.SYNTHETIC,
      class: EventClass.EPHEMERAL,
      producedAtMs: BigInt(this.now()),
      payload: {
        case: "sessionStarted",
        value: create(SessionStartedSchema, { source: this.deps.sessionSource }),
      },
    }));
    // The EFFECTIVE mode, not the argv one: the readiness announcement is the
    // one line that says what posture this session actually came up in.
    LOGGER.log({
      agent_repl_session_id: this.deps.sessionId,
      permission_mode: this.effectivePermissionMode,
    },
      `readiness asserted (lock held, SDK query built, daemon handshaked)`);
  }

  /** True while a handshaked daemon connection is live (test/diagnostics). */
  isConnected(): boolean {
    return this.server.isConnected();
  }

  /** Outstanding-turn count (test/diagnostics). */
  turnCount(): number {
    return this.activeTurnIds.length;
  }

  /**
   * The explicit stop path (§4.4): SIGTERM (wired by main.ts) or a deliberate
   * teardown. Cancels blocked permissions, ends the SDK input iterable (which
   * ends the pump), and closes both UDS connections. Idempotent.
   */
  async shutdown(reason = "shutdown"): Promise<void> {
    if (this.shutdownPromise !== null) return this.shutdownPromise;
    this.intentionalShutdownReason = reason;
    this.intentionalShutdownStarted.resolve();
    this.shutdownPromise = this.finishIntentionalShutdown(reason);
    return this.shutdownPromise;
  }

  private async pump(): Promise<void> {
    LOGGER.log({ agent_repl_session_id: this.deps.sessionId }, "starting SDK stream pump");
    try {
      for await (const msg of this.query!.query) {
        await this.routeSdkMessage(msg);
      }
    } catch (err) {
      if (this.intentionalShutdownReason !== null) {
        LOGGER.log({ agent_repl_session_id: this.deps.sessionId, shutdown_reason: this.intentionalShutdownReason }, "SDK stream stopped during intentional shim shutdown");
        return;
      }
      await this.failUnexpectedSdkTermination("iterator_throw", err);
    }
    if (this.intentionalShutdownReason !== null) {
      LOGGER.log({ agent_repl_session_id: this.deps.sessionId, shutdown_reason: this.intentionalShutdownReason }, "SDK stream completed during intentional shim shutdown");
      return;
    }
    await this.failUnexpectedSdkTermination("iterator_eof");
  }

  /** End the one owned SDK query, then wait until its pump has stopped. */
  private async finishIntentionalShutdown(reason: string): Promise<void> {
    LOGGER.log({ agent_repl_session_id: this.deps.sessionId, shutdown_reason: reason, query_constructed: this.query !== null }, "beginning intentional shim shutdown");
    this.control.cancelAll(reason);
    this.input.end();
    this.query?.abort();
    let persistenceFailure: unknown;
    try {
      await this.persistQueryTerminationOrThrow("intentional", reason);
    } catch (err) {
      persistenceFailure = err;
    }
    let cleanupFailure: unknown;
    try {
      await this.closeResources();
    } catch (err) {
      cleanupFailure = err;
    }
    await this.runFinished.promise;
    if (persistenceFailure !== undefined && cleanupFailure !== undefined) {
      throw new AggregateError(
        [persistenceFailure, cleanupFailure],
        "intentional query termination lost its store receipt and resource cleanup failed",
      );
    }
    if (persistenceFailure !== undefined) throw persistenceFailure;
    if (cleanupFailure !== undefined) throw cleanupFailure;
    LOGGER.log({ agent_repl_session_id: this.deps.sessionId, shutdown_reason: reason }, "intentional shim shutdown complete");
  }

  /** Close non-SDK resources once regardless of which terminal path won. */
  private closeResources(): Promise<void> {
    if (this.resourcesClosePromise === null) {
      this.resourcesClosePromise = (async (): Promise<void> => {
        try {
          await this.server.close();
        } finally {
          this.store.close();
        }
      })();
    }
    return this.resourcesClosePromise;
  }

  /**
   * The SDK ended while this shim remained live. The query cannot be replaced:
   * continuing would sever the vendor conversation and destroy cache reuse.
   */
  private async failUnexpectedSdkTermination(
    terminationKind: UnexpectedSdkStreamTerminationError["terminationKind"],
    cause?: unknown,
  ): Promise<never> {
    const error = new UnexpectedSdkStreamTerminationError(terminationKind, cause);
    const termination = this.queryTerminationEvent(terminationKind, cause);
    const diagnostic = this.unexpectedTerminationDiagnostic(terminationKind, termination);
    LOGGER.log({
      level: "error",
      ...diagnostic.logFields,
      intentional: false,
      active_turn_ids: this.activeTurnIds,
      input_ended: this.input.isEnded,
      query_aborted: false,
      resume_requested: this.deps.sessionSource === SessionSource.RESUME,
      cause,
    }, "SDK stream terminated outside intentional shim shutdown; exiting nonzero");
    let persistenceFailure: unknown;
    let cleanupFailure: unknown;
    try {
      await this.persistUnexpectedSdkTermination(terminationKind, termination);
    } catch (err) {
      persistenceFailure = err;
    } finally {
      try {
        await this.closeResources();
      } catch (err) {
        cleanupFailure = err;
      }
    }
    if (persistenceFailure !== undefined && cleanupFailure !== undefined) {
      LOGGER.log({
        level: "error",
        operation: "shim.uds-session.unexpected-termination-cleanup",
        ...diagnostic.logFields,
        outcome: "fatal_persistence_and_cleanup_failure",
        persistence_failure: persistenceFailure,
        cleanup_failure: cleanupFailure,
      }, "unexpected SDK termination persistence and resource cleanup both failed");
      throw new QueryTerminationCleanupError(
        [persistenceFailure, cleanupFailure],
        "unexpected SDK termination persistence and resource cleanup both failed",
      );
    }
    if (persistenceFailure !== undefined) throw persistenceFailure;
    if (cleanupFailure !== undefined) {
      LOGGER.log({
        level: "error",
        operation: "shim.uds-session.unexpected-termination-cleanup",
        ...diagnostic.logFields,
        outcome: "fatal_cleanup_failure",
        cleanup_failure: cleanupFailure,
      }, "unexpected SDK termination resource cleanup failed");
      throw new QueryTerminationCleanupError(
        [error, cleanupFailure],
        "unexpected SDK termination and resource cleanup both failed",
      );
    }
    throw error;
  }

  /**
   * Persist the daemon's typed unexpected-query failure before the shim closes.
   *
   * The daemon may be detached when the SDK dies, so a direct SessionServer
   * send would be discarded and no reconnect could learn why the session
   * stopped. Store replay is the authoritative delivery path for this report.
   */
  private async persistUnexpectedSdkTermination(
    terminationKind: "startup_failure" | UnexpectedSdkStreamTerminationError["terminationKind"],
    termination: QueryLifecycle["event"],
    awaitLiveDelivery = true,
  ): Promise<void> {
    if (awaitLiveDelivery && this.unexpectedTerminationForwarded !== null) {
      throw new Error("unexpected SDK termination delivery latch already exists");
    }
    const forwarded = awaitLiveDelivery ? completionLatch() : null;
    if (forwarded !== null) this.unexpectedTerminationForwarded = forwarded;
    const report = create(DegradedStateSchema, {
      component: CLAUDE_SHIM_SDK_COMPONENT,
      // This discriminator is part of the daemon's stable failure contract.
      // The detailed cause remains durable in QueryLifecycle and in the
      // structured fatal log record because DegradedState has no detail field.
      reason: UNEXPECTED_QUERY_TERMINATION_REASON,
      droppedCount: 0n,
      recovered: false,
      queryInstanceId: this.queryInstanceId,
    });
    const degraded = create(EventSchema, {
      sessionId: this.store.storeSessionId(),
      seq: 0n,
      plane: Plane.STREAM,
      class: EventClass.PERSISTENT,
      producedAtMs: BigInt(this.now()),
      payload: { case: "degradedState", value: report },
    });
    const diagnostic = this.unexpectedTerminationDiagnostic(terminationKind, termination);
    const lifecycle = this.queryLifecycleEnvelope(termination);
    try {
      // One acknowledged batch makes the causal detail and the stable replay
      // discriminator atomic and ordered. A live daemon surfaces the lifecycle
      // immediately and deduplicates the following confirmation; a restored
      // daemon can surface either record from its replay floor.
      await this.store.write([lifecycle, degraded]);
      // A store receipt proves durability, not frontend delivery. A runtime
      // iterator failure keeps the shim alive until the merged lifecycle frame
      // flushes to a live daemon; startup failure cannot require that because
      // it can be the daemon connection that failed. Its replayable pair is
      // nevertheless complete and ordered in this same acknowledged batch.
      if (forwarded !== null) await forwarded.promise;
    } catch (cause) {
      LOGGER.log({
        level: "error",
        operation: "shim.uds-session.unexpected-termination-delivery",
        ...diagnostic.logFields,
        component: report.component,
        reason: report.reason,
        failed_operation: "store.write.unexpected_query_termination",
        outcome: "fatal_missing_termination_receipt",
        cause,
      }, "unexpected SDK termination degradation could not receive a durable store receipt");
      throw new QueryTerminationPersistenceError(
        terminationKind,
        this.queryInstanceId,
        cause,
      );
    }
  }

  /** Build the one diagnostic identity shared by every unexpected-terminal outcome. */
  private unexpectedTerminationDiagnostic(
    terminationKind: "startup_failure" | UnexpectedSdkStreamTerminationError["terminationKind"],
    termination: QueryLifecycle["event"],
  ): { logFields: Record<string, unknown> } {
    if (termination.case !== "terminated") {
      throw new Error("unexpected SDK termination requires a terminated lifecycle arm");
    }
    const terminationArm = termination.value.reason.case;
    if (terminationArm !== "unexpectedEof" && terminationArm !== "iteratorFailure" && terminationArm !== "startupFailure") {
      throw new Error(`unexpected SDK termination received invalid reason arm ${String(terminationArm)}`);
    }
    const vendorIdentity = termination.value.vendorIdentity;
    if (vendorIdentity.case === undefined) {
      throw new Error("unexpected SDK termination lacks vendor identity evidence");
    }
    const claudeSessionId = vendorIdentity.case === "vendorSessionId" ? vendorIdentity.value : "";
    const terminationCause = terminationArm === "iteratorFailure" || terminationArm === "startupFailure"
      ? termination.value.reason.value.cause
      : null;
    if (terminationArm === "startupFailure" && (terminationCause === null || terminationCause.trim() === "")) {
      throw new Error("startup failure termination lacks a nonblank cause");
    }
    return { logFields: {
      agent_repl_session_id: this.deps.sessionId,
      query_instance_id: this.queryInstanceId,
      ...(claudeSessionId !== "" ? { claude_session_id: claudeSessionId } : {}),
      vendor_session_id: claudeSessionId,
      store_key: this.store.storeSessionId(),
      termination_kind: terminationKind,
      termination_arm: terminationArm,
      termination_cause: terminationCause,
    } };
  }

  private isUnexpectedTerminationLifecycle(evt: Event): boolean {
    if (evt.payload.case !== "queryLifecycle") return false;
    const lifecycle = evt.payload.value;
    if (lifecycle.queryInstanceId !== this.queryInstanceId || lifecycle.event.case !== "terminated") return false;
    const reason = lifecycle.event.value.reason.case;
    return reason === "unexpectedEof" || reason === "iteratorFailure" || reason === "startupFailure";
  }

  /**
   * Read and log the account's five-hour quota at one turn boundary.
   *
   * Reads are serialized because the metric belongs to the account rather
   * than the request. A turn-end read queued before the next turn-start read
   * must execute first or the experiment would attribute the later state to
   * the wrong boundary.
   */
  private captureFiveHourUsage(
    phase: "turn_start" | "turn_end",
    turnId: string,
    startUsage?: FiveHourUsageSample,
  ): Promise<FiveHourUsageSample> {
    const capture = this.usageSampleTail.then(async (): Promise<FiveHourUsageSample> => {
      const startedAt = performance.now();
      try {
        if (this.query === null) throw new Error("five-hour usage cannot be sampled before the SDK query exists");
        const response = await this.query.subscriptionUsage();
        const sample = fiveHourUsageSample(response, performance.now() - startedAt, this.now());
        this.logFiveHourUsage(phase, turnId, sample, startUsage);
        return sample;
      } catch (cause) {
        const sample: FiveHourUsageSample = {
          observedAtMs: this.now(),
          measurementAvailable: false,
          utilization: null,
          resetsAt: null,
          subscriptionType: null,
          rateLimitsAvailable: false,
          sampleLatencyMs: performance.now() - startedAt,
          unavailableReason: "sample_failed",
          unavailableCause: errMsg(cause),
        };
        this.logFiveHourUsage(phase, turnId, sample, startUsage, cause);
        return sample;
      }
    });
    // capture handles and logs every failure, so this tail cannot reject and
    // poison later boundary reads.
    this.usageSampleTail = capture.then(() => undefined);
    return capture;
  }

  /** Build the durable wire event for one account-usage observation. */
  private accountUsageObservationEvent(
    phase: "turn_start" | "turn_end",
    turnId: string,
    boundaryAtMs: number,
    sample: FiveHourUsageSample,
    sessionId: string,
  ): Event {
    const resetsAtMs = (): bigint => {
      if (sample.resetsAt === null) {
        throw new Error("available five-hour usage observation omitted its reset timestamp");
      }
      const parsed = Date.parse(sample.resetsAt);
      if (!Number.isFinite(parsed)) {
        throw new Error(`available five-hour usage observation has an invalid reset timestamp ${JSON.stringify(sample.resetsAt)}`);
      }
      return BigInt(parsed);
    };
    const outcome = sample.measurementAvailable
      ? {
        case: "available" as const,
        value: create(AccountUsageAvailableSchema, {
          fiveHour: create(UsageWindowSchema, {
            utilizationPercent: sample.utilization!,
            resetsAtMs: resetsAtMs(),
          }),
        }),
      }
      : {
        case: "unavailable" as const,
        value: create(AccountUsageUnavailableSchema, {
          reason: this.usageUnavailableReason(sample),
        }),
      };
    return create(EventSchema, {
      sessionId,
      seq: 0n,
      plane: Plane.STREAM,
      class: EventClass.PERSISTENT,
      requestId: turnId,
      producedAtMs: BigInt(this.now()),
      payload: {
        case: "accountUsageObservation",
        value: create(AccountUsageObservationSchema, {
          queryInstanceId: this.queryInstanceId,
          turnId,
          boundaryAtMs: BigInt(boundaryAtMs),
          observedAtMs: BigInt(sample.observedAtMs),
          sampleLatencyMs: BigInt(Math.round(sample.sampleLatencyMs)),
          subscriptionType: sample.subscriptionType ?? "",
          boundary: phase === "turn_start"
            ? { case: "turnStart", value: create(TurnStartUsageBoundarySchema) }
            : { case: "turnEnd", value: create(TurnEndUsageBoundarySchema) },
          outcome,
        }),
      },
    });
  }

  /** Convert the SDK's explicitly validated unavailable outcome to the typed contract. */
  private usageUnavailableReason(sample: FiveHourUsageSample) {
    switch (sample.unavailableReason) {
      case "rate_limits_unavailable":
        return { case: "serviceUnavailable" as const, value: create(UsageServiceUnavailableSchema) };
      case "five_hour_window_unavailable":
        return { case: "windowUnavailable" as const, value: create(FiveHourWindowUnavailableSchema) };
      case "five_hour_utilization_unavailable":
        return { case: "utilizationUnavailable" as const, value: create(UtilizationUnavailableSchema) };
      case "sample_failed":
        if (sample.unavailableCause === undefined || sample.unavailableCause === "") {
          throw new Error("failed usage sample omitted its causal diagnostic");
        }
        return { case: "samplingFailure" as const, value: create(UsageSamplingFailureSchema, { cause: sample.unavailableCause }) };
      default:
        throw new Error(`unrecognized unavailable usage reason ${String(sample.unavailableReason)}`);
    }
  }

  /** Persist the construction of the only query() the shim owns. */
  private async persistQueryCreated(): Promise<void> {
    const storeKey = this.store.storeSessionId();
    const invocation = this.deps.sessionSource === SessionSource.RESUME
      ? { case: "resumed" as const, value: create(ResumedQuerySchema, { requestedVendorSessionId: this.deps.storeSessionId ?? "" }) }
      : { case: "fresh" as const, value: create(FreshQuerySchema) };
    const ack = await this.persistQueryLifecycle({
      case: "created",
      value: create(QueryCreatedSchema, { requestedModel: this.deps.requestedModel ?? "", invocation }),
    }, storeKey);
    if (ack.accepted !== 1n || ack.deduped !== 0n || ack.lastSeq === 0n) {
      throw new Error(`QueryCreated persistence returned an invalid receipt: accepted=${ack.accepted} deduped=${ack.deduped} last_seq=${ack.lastSeq}`);
    }
    this.queryCreatedPosition = { storeKey, seq: ack.lastSeq };
  }

  /** Persist effective query configuration after the SDK reports system initialization. */
  private async persistRuntimeObserved(
    message: SdkMessageLike,
    vendorSessionId: string,
    eventSessionId = vendorSessionId,
  ): Promise<void> {
    const raw = message as unknown as Record<string, unknown>;
    const text = (field: string): string => typeof raw[field] === "string" ? raw[field] : "";
    const unavailable = (cause: string) => create(EvidenceFingerprintSchema, {
      evidence: { case: "unavailable", value: create(FingerprintUnavailableSchema, { cause }) },
    });
    const identity = create(QueryRuntimeIdentitySchema, {
      vendorSessionId,
      effectiveModel: this.effectiveModel,
      sdkVersion: this.deps.sdkVersion ?? "",
      claudeCodeVersion: text("claude_code_version"),
      shimBuildSha: this.deps.shimBuildSha ?? "",
      authSource: text("api_key_source"),
      subscriptionType: "",
      fastModeState: text("fast_mode_state"),
      fastModeReason: text("fast_mode_reason"),
      effectiveOptions: unavailable("effective SDK options are not exposed by the Agent SDK initialization message"),
      settings: unavailable("effective settings are not exposed by the Agent SDK initialization message"),
      tools: unavailable("ordered tool definitions are not exposed by the Agent SDK initialization message"),
      mcp: unavailable("ordered MCP configuration is not exposed by the Agent SDK initialization message"),
      contextPrefix: unavailable("cacheable context prefix is not exposed by the Agent SDK initialization message"),
    });
    // This cache is assigned before the store receipt so a daemon reconnect
    // cannot observe a query whose durable lifecycle event has arrived while
    // its next ShimHello omits the same authoritative runtime identity.
    this.queryRuntimeIdentity = identity;
    await this.persistQueryLifecycle({
      case: "runtimeObserved",
      value: create(QueryRuntimeObservedSchema, {
        identity,
      }),
    }, eventSessionId);
  }

  /**
   * Confirm the first SDK-reported identity before any store rekey, registry
   * observation, or process-global identity mutation can occur.
   *
   * A later identity is an SDK rotation and is legal only after this query's
   * first identity has been confirmed. The dangerous state is specifically a
   * resumed query's first report: accepting a different id there silently
   * turns an exact resume into a fresh conversation.
   */
  private async confirmVendorIdentity(message: SdkMessageLike, observedVendorSessionId: string): Promise<void> {
    if (observedVendorSessionId.trim() === "") {
      throw new Error("SDK message omitted its vendor session identity");
    }
    switch (this.queryIdentity.case) {
      case "fresh-unconfirmed":
        this.queryIdentity = { case: "confirmed", vendorSessionId: observedVendorSessionId };
        return;
      case "confirmed":
        this.queryIdentity = { case: "confirmed", vendorSessionId: observedVendorSessionId };
        return;
      case "resume-unconfirmed": {
        const requestedVendorSessionId = this.queryIdentity.requestedVendorSessionId;
        if (observedVendorSessionId === requestedVendorSessionId) {
          this.queryIdentity = { case: "confirmed", vendorSessionId: observedVendorSessionId };
          return;
        }
        // Preserve both sides as structured lifecycle evidence under the
        // requested store key. Filing it under the replacement would mutate
        // identity before the mismatch reached the daemon.
        await this.persistRuntimeObserved(message, observedVendorSessionId, requestedVendorSessionId);
        LOGGER.log({
          level: "error",
          operation: "shim.uds-session.resume-identity-confirmation",
          outcome: "fatal_identity_mismatch",
          agent_repl_session_id: this.deps.sessionId,
          query_instance_id: this.queryInstanceId,
          requested_vendor_session_id: requestedVendorSessionId,
          observed_vendor_session_id: observedVendorSessionId,
          session_source: this.deps.sessionSource,
          shim_version: this.deps.shimVersion,
          sdk_version: this.deps.sdkVersion ?? "",
        }, "resumed SDK query reported a different vendor session identity; refusing replacement conversation");
        throw new ResumeIdentityMismatchError(requestedVendorSessionId, observedVendorSessionId);
      }
    }
  }

  /** Persist a query termination before resources are released. */
  private async persistQueryTermination(
    kind: "intentional" | "startup_failure" | UnexpectedSdkStreamTerminationError["terminationKind"],
    cause?: unknown,
  ): Promise<void> {
    await this.persistQueryLifecycle(this.queryTerminationEvent(kind, cause));
  }

  private queryTerminationEvent(
    kind: "intentional" | "startup_failure" | UnexpectedSdkStreamTerminationError["terminationKind"],
    cause?: unknown,
  ): QueryLifecycle["event"] {
    const reason = kind === "intentional"
      ? { case: "intentional" as const, value: create(IntentionalQueryTerminationSchema, { reason: String(cause ?? "shutdown") }) }
      : kind === "startup_failure"
        ? { case: "startupFailure" as const, value: create(QueryStartupFailureSchema, { cause: terminationCause(cause, "startup failure") }) }
      : kind === "iterator_eof"
        ? { case: "unexpectedEof" as const, value: create(UnexpectedQueryEofSchema) }
        : { case: "iteratorFailure" as const, value: create(QueryIteratorFailureSchema, { cause: terminationCause(cause, "iterator failure") }) };
    const vendorSessionId = this.store.vendorSessionId();
    return {
      case: "terminated",
      value: create(QueryTerminatedSchema, {
        vendorIdentity: vendorSessionId === ""
          ? { case: "vendorSessionIdentityUnavailable", value: create(VendorSessionIdentityUnavailableSchema) }
          : { case: "vendorSessionId", value: vendorSessionId },
        reason,
      }),
    };
  }

  /** Persist termination with one lifecycle-owned diagnostic and a fatal typed error. */
  private async persistQueryTerminationOrThrow(
    kind: "intentional" | "startup_failure",
    cause?: unknown,
  ): Promise<void> {
    try {
      await this.persistQueryTermination(kind, cause);
    } catch (err) {
      LOGGER.log({
        level: "error",
        agent_repl_session_id: this.deps.sessionId,
        query_instance_id: this.queryInstanceId,
        store_key: this.store.storeSessionId(),
        termination_kind: kind,
        termination_cause: cause,
        outcome: "fatal_missing_termination_receipt",
        cause: err,
      }, "query termination could not receive a durable store receipt");
      throw new QueryTerminationPersistenceError(kind, this.queryInstanceId, err);
    }
  }

  /** Write a query lifecycle event and await its store receipt. */
  private async persistQueryLifecycle(
    event: QueryLifecycle["event"],
    sessionId = this.store.storeSessionId(),
  ): ReturnType<StoreClient["write"]> {
    const envelope = this.queryLifecycleEnvelope(event, sessionId);
    LOGGER.log({ agent_repl_session_id: this.deps.sessionId, query_instance_id: this.queryInstanceId, lifecycle_event: event.case, store_key: sessionId }, "persisting query lifecycle event");
    return this.store.write([envelope]);
  }

  private queryLifecycleEnvelope(
    event: QueryLifecycle["event"],
    sessionId = this.store.storeSessionId(),
  ): Event {
    return create(EventSchema, {
      sessionId,
      seq: 0n,
      plane: Plane.STREAM,
      class: EventClass.PERSISTENT,
      producedAtMs: BigInt(this.now()),
      payload: { case: "queryLifecycle", value: create(QueryLifecycleSchema, {
        queryInstanceId: this.queryInstanceId,
        observedAtMs: BigInt(this.now()),
        event,
      }) },
    });
  }


  /** Emit one information-dense, machine-queryable turn-boundary record. */
  private logFiveHourUsage(
    phase: "turn_start" | "turn_end",
    turnId: string,
    sample: FiveHourUsageSample,
    startUsage?: FiveHourUsageSample,
    cause?: unknown,
  ): void {
    const claudeSessionId = this.store.vendorSessionId();
    const common = {
      agent_repl_session_id: this.deps.sessionId,
      ...(claudeSessionId === "" ? {} : { claude_session_id: claudeSessionId }),
      request_id: turnId,
      turn_id: turnId,
      phase,
      measurement_available: sample.measurementAvailable,
      five_hour_utilization: sample.utilization,
      five_hour_resets_at: sample.resetsAt,
      subscription_type: sample.subscriptionType,
      rate_limits_available: sample.rateLimitsAvailable,
      sample_latency_ms: sample.sampleLatencyMs,
      measurement_unavailable_reason: sample.unavailableReason,
    };
    if (phase === "turn_start") {
      LOGGER.log({
        ...common,
        ...(cause === undefined ? {} : { level: "error", cause }),
      }, cause === undefined ? "captured five-hour utilization at turn start" : "failed to capture five-hour utilization at turn start");
      return;
    }

    const sameWindow = startUsage?.resetsAt !== null
      && startUsage?.resetsAt !== undefined
      && sample.resetsAt !== null
      && startUsage.resetsAt === sample.resetsAt;
    const deltaAvailable = startUsage?.measurementAvailable === true
      && sample.measurementAvailable
      && sameWindow;
    let deltaUnavailableReason: string | undefined;
    if (!deltaAvailable) {
      if (startUsage === undefined) deltaUnavailableReason = "turn_start_sample_missing";
      else if (!startUsage.measurementAvailable) deltaUnavailableReason = "turn_start_measurement_unavailable";
      else if (!sample.measurementAvailable) deltaUnavailableReason = "turn_end_measurement_unavailable";
      else deltaUnavailableReason = "five_hour_window_changed_or_unknown";
    }
    LOGGER.log({
      ...common,
      ...(cause === undefined ? {} : { level: "error", cause }),
      start_measurement_available: startUsage?.measurementAvailable ?? false,
      start_five_hour_utilization: startUsage?.utilization ?? null,
      end_five_hour_utilization: sample.utilization,
      start_five_hour_resets_at: startUsage?.resetsAt ?? null,
      end_five_hour_resets_at: sample.resetsAt,
      same_five_hour_window: sameWindow,
      five_hour_utilization_delta_available: deltaAvailable,
      five_hour_utilization_delta: deltaAvailable
        ? sample.utilization! - startUsage!.utilization!
        : null,
      delta_unavailable_reason: deltaUnavailableReason,
    }, cause === undefined ? "captured five-hour utilization at turn end" : "failed to capture five-hour utilization at turn end");
  }

  /**
   * Route one SDK message per the G4 wiring: live typing and heartbeats go
   * straight to the daemon, durable MessageLatency and all other durable
   * events go through the store (whose merged echo returns via onMerged → the
   * daemon).
   */
  private async routeSdkMessage(msg: SdkMessageLike): Promise<void> {
    if (msg.type === "stream_event") {
      // Observe BEFORE converting: a message_start must make its own id
      // current for the deltas that follow it.
      this.streamMessages.observe(msg);
      const opts = { ...this.convertOpts(), messageId: this.streamMessages.current() };
      const persistent = toPersistentEvent(msg, opts);
      if (persistent !== null) {
        await this.confirmVendorIdentity(msg, persistent.sessionId);
        this.store.adoptStoreKey(persistent.sessionId);
        setClaudeSessionId(persistent.sessionId);
        LOGGER.logVerbose({ agent_repl_session_id: this.deps.sessionId, sdk_type: msg.type, payload_case: persistent.payload.case, claude_session_id: persistent.sessionId }, "writing persistent SDK structural event to store");
        // The SDK pump owns this durability boundary.  It may not consume the
        // next SDK message until the latency stamp has a store receipt: doing
        // so would allow the response and its usage to succeed without the
        // timing evidence needed to compute TTFT and generation throughput.
        try {
          await this.store.write([persistent]);
        } catch (cause) {
          const latency = persistent.payload.case === "messageLatency"
            ? persistent.payload.value
            : undefined;
          LOGGER.log({
            level: "error",
            operation: "shim.uds-session.persistent-evidence",
            agent_repl_session_id: this.deps.sessionId,
            claude_session_id: persistent.sessionId,
            query_instance_id: this.queryInstanceId,
            ...(this.activeTurnIds[0] === undefined ? {} : {
              request_id: this.activeTurnIds[0],
              turn_id: this.activeTurnIds[0],
            }),
            api_message_id: latency?.uuid ?? null,
            evidence_kind: "message_latency",
            failed_operation: "store.write.message_latency",
            outcome: "fatal_missing_persistent_evidence_receipt",
            cause,
          }, "persistent SDK message latency did not receive a durable store receipt");
          throw cause;
        }
        return;
      }
    }
    if (isEphemeral(msg)) {
      const evt = toEphemeralEvent(msg, { ...this.convertOpts(), messageId: this.streamMessages.current() });
      if (evt) {
        LOGGER.logVerbose({ agent_repl_session_id: this.deps.sessionId, sdk_type: msg.type, payload_case: evt.payload.case, claude_session_id: evt.sessionId }, "forwarding ephemeral SDK event directly to daemon");
        this.server.sendEvent(evt);
      }
      return;
    }
    // A result closes the oldest accepted input turn only when it has no live
    // SDK tasks. A result with live tasks is durable intermediate evidence,
    // but the turn remains authoritative until the SDK emits a result after
    // every task has terminated. Claude may emit multiple result messages as
    // background-task notifications drive additional internal agent cycles.
    let terminalTurnId: string | undefined;
    let terminalBoundaryAtMs: number | undefined;
    let turnStart: Event | undefined;
    let retainResultTurn = false;
    let taskNotificationResultCountAfterWrite: number | undefined;
    if (msg.type === "result") {
      const claimedTurnId = this.activeTurnIds[0];
      if (claimedTurnId === undefined) {
        this.reportDegraded(
          "claude-shim-turn-lifecycle",
          "SDK result has no accepted prompt turn to close",
        );
      } else {
        const origin = typeof msg.origin === "object" && msg.origin !== null
          ? msg.origin as Record<string, unknown>
          : undefined;
        const taskNotificationResult = origin?.kind === "task_notification" || origin?.kind === "task-notification";
        const taskCount = this.turnSdkTaskIds.get(claimedTurnId)?.size ?? 0;
        const priorNotificationResults = this.turnTaskNotificationResultCounts.get(claimedTurnId) ?? 0;
        taskNotificationResultCountAfterWrite = priorNotificationResults + (taskNotificationResult ? 1 : 0);
        retainResultTurn = this.liveSdkTaskIds.size > 0
          || (taskCount > 0 && !taskNotificationResult)
          || taskNotificationResultCountAfterWrite < taskCount;
      }
      if (claimedTurnId !== undefined && retainResultTurn) {
        retainResultTurn = true;
        turnStart = this.activeTurnStarts.get(claimedTurnId);
        const taskCount = this.turnSdkTaskIds.get(claimedTurnId)?.size ?? 0;
        LOGGER.log({
          agent_repl_session_id: this.deps.sessionId,
          request_id: claimedTurnId,
          turn_id: claimedTurnId,
          live_sdk_task_count: this.liveSdkTaskIds.size,
          live_sdk_task_ids: [...this.liveSdkTaskIds].sort(),
          sdk_task_count: taskCount,
          task_notification_result_count_after_write: taskNotificationResultCountAfterWrite,
          turns_in_flight: this.activeTurnIds.length,
          decision: "retain_turn_for_sdk_task_cycles",
        }, "SDK result retained while background-task result cycles remain outstanding");
      } else if (claimedTurnId !== undefined) {
        terminalTurnId = this.activeTurnIds.shift();
        if (terminalTurnId !== claimedTurnId) {
          throw new Error(`accepted turn FIFO changed while closing ${JSON.stringify(claimedTurnId)}`);
        }
        turnStart = this.activeTurnStarts.get(terminalTurnId);
        this.pendingTurnEndIds.push(terminalTurnId);
        terminalBoundaryAtMs = this.now();
        // The terminal vendor result cannot reach the daemon until this end
        // boundary is durably recorded in the same store batch below.
      }
    }
    // Assistant responses belong to the oldest accepted turn without closing
    // it. This is the same shim-owned FIFO authority used by result handling;
    // the SDK's request_id remains untouched inside AssistantMessage as raw
    // API evidence and is never repurposed as daemon turn correlation.
    const taskLifecycleMessage = msg.type === "system"
      && (msg.subtype === "task_started" || msg.subtype === "task_notification" || msg.subtype === "task_updated");
    const rootTurnId = msg.type === "assistant" || taskLifecycleMessage || retainResultTurn
      ? this.activeTurnIds[0]
      : terminalTurnId;
    if ((msg.type === "assistant" || taskLifecycleMessage || retainResultTurn) && rootTurnId !== undefined) {
      turnStart = this.activeTurnStarts.get(rootTurnId);
    }
    // The direct readiness SessionStarted closes the lifecycle gate before the
    // SDK emits its first system:init. Observe that raw SDK authority here so a
    // later rejected SetModel can name the currently selected model even though
    // the duplicate lifecycle twin is deliberately suppressed.
    const systemInit = msg.type === "system" && msg.subtype === "init";
    if (systemInit && typeof msg.model === "string") {
      const model = normalizeModel(msg.model);
      this.effectiveModel = model;
      LOGGER.log({ agent_repl_session_id: this.deps.sessionId, model, raw_model: msg.model },
        model === "" ? "system-init model normalized to empty" : "model observed from SDK system-init");
    }
    const { vendor, lifecycle, assistantApiUsage } = convert(msg, {
      sessionSource: this.deps.sessionSource,
      sessionGate: this.sessionGate,
      ...(rootTurnId !== undefined ? { rootTurnId } : {}),
      ...this.convertOpts(),
    });
    // Root-turn correlation belongs to this routing owner, not the raw SDK
    // converter. The Event envelope carries the daemon request identity while
    // AssistantMessage continues to preserve the SDK's API request_id.
    if (rootTurnId !== undefined) vendor.requestId = rootTurnId;
    if (msg.type === "assistant" && assistantApiUsage !== undefined) {
      logAssistantApiResponseUsage(msg, assistantApiUsage, this.deps.sessionId);
    }
    // The converted envelope carries the VENDOR session id (read off the SDK
    // message), which is the id the store files these events under. Adopt it
    // as the subscription key: a fresh session has no other way to learn it,
    // and subscribing under this shim's `--session-id` listens on a channel
    // nothing publishes to.
    await this.confirmVendorIdentity(msg, vendor.sessionId);
    this.store.adoptStoreKey(vendor.sessionId);
    setClaudeSessionId(vendor.sessionId);
    if (systemInit) await this.persistRuntimeObserved(msg, vendor.sessionId);
    let turnClaimBridge: Event | undefined;
    if (rootTurnId !== undefined && turnStart !== undefined && turnStart.sessionId !== vendor.sessionId) {
      if (turnStart.payload.case !== "turnStarted") {
        throw new Error(
          `active turn ${JSON.stringify(rootTurnId)} retained ${turnStart.payload.case || "empty"} instead of TurnStarted`,
        );
      }
      turnClaimBridge = create(EventSchema, {
        sessionId: vendor.sessionId,
        seq: 0n,
        plane: turnStart.plane,
        class: turnStart.class,
        requestId: rootTurnId,
        producedAtMs: turnStart.producedAtMs,
        payload: {
          case: "turnClaimBridge",
          value: create(TurnClaimBridgeSchema, {
            turnId: rootTurnId,
            previousSessionId: turnStart.sessionId,
          }),
        },
      });
    }
    if (turnClaimBridge !== undefined) {
      if (turnStart === undefined) {
        throw new Error(`turn-claim bridge ${JSON.stringify(rootTurnId)} lacks its retained TurnStarted carrier`);
      }
      LOGGER.log({
        agent_repl_session_id: this.deps.sessionId,
        request_id: rootTurnId,
        turn_id: rootTurnId,
        previous_session_id: turnStart.sessionId,
        claude_session_id: vendor.sessionId,
        decision: "emit_turn_claim_bridge",
      }, "writing non-lifecycle turn correlation proof in rotated vendor seq space");
      // This class owns the accepted turn's store-key history. Advancing the
      // retained carrier synchronously makes every later SDK message compare
      // against the key whose bridge is already ahead of it in StoreClient's
      // serialized write queue. A second rotation therefore emits a new
      // bridge, while the result following an assistant cannot duplicate the
      // first one.
      turnStart.sessionId = vendor.sessionId;
    }
    let authoritativeLifecycle = msg.type === "result" && terminalTurnId === undefined
      ? lifecycle.filter((event) => event.payload.case !== "turnEnded")
      : lifecycle;
    if (rootTurnId !== undefined && taskLifecycleMessage) {
      for (const event of authoritativeLifecycle) event.requestId = rootTurnId;
    }
    const startedTaskIds = authoritativeLifecycle.flatMap((event) =>
      event.payload.case === "taskStarted" ? [event.payload.value.taskId] : []);
    let endedTaskIds = authoritativeLifecycle.flatMap((event) =>
      event.payload.case === "taskEnded" ? [event.payload.value.taskId] : []);
    for (const taskId of [...startedTaskIds, ...endedTaskIds]) {
      if (taskId.trim() === "") throw new Error(`SDK ${msg.type} emitted a task lifecycle event without task_id`);
    }
    const liveAfterThisEvent = new Set(this.liveSdkTaskIds);
    for (const taskId of startedTaskIds) {
      if (liveAfterThisEvent.has(taskId)) {
        throw new Error(`SDK emitted duplicate TaskStarted for live task ${JSON.stringify(taskId)}`);
      }
      if (this.completedSdkTaskIds.has(taskId)) {
        throw new Error(`SDK emitted TaskStarted after terminal state for task ${JSON.stringify(taskId)}`);
      }
      liveAfterThisEvent.add(taskId);
    }
    const duplicateEndedTaskIds = endedTaskIds.filter((taskId) => this.completedSdkTaskIds.has(taskId));
    if (duplicateEndedTaskIds.length > 0) {
      const duplicates = new Set(duplicateEndedTaskIds);
      authoritativeLifecycle = authoritativeLifecycle.filter((event) =>
        event.payload.case !== "taskEnded" || !duplicates.has(event.payload.value.taskId));
      endedTaskIds = endedTaskIds.filter((taskId) => !duplicates.has(taskId));
      LOGGER.log({
        agent_repl_session_id: this.deps.sessionId,
        request_id: rootTurnId,
        turn_id: rootTurnId,
        duplicate_terminal_sdk_task_ids: [...duplicates].sort(),
        live_sdk_task_count: this.liveSdkTaskIds.size,
        live_sdk_task_ids: [...this.liveSdkTaskIds].sort(),
        decision: "retain_vendor_message_without_duplicate_task_end",
      }, "SDK repeated a stored terminal task fact; retaining vendor evidence without duplicating lifecycle");
    }
    for (const taskId of endedTaskIds) {
      if (!liveAfterThisEvent.delete(taskId)) {
        throw new Error(`SDK emitted TaskEnded for unknown task ${JSON.stringify(taskId)}`);
      }
    }
    if (msg.type === "result" && terminalTurnId !== undefined) {
      LOGGER.log({
        agent_repl_session_id: this.deps.sessionId,
        request_id: terminalTurnId,
        plane: "stream",
        turn_id: terminalTurnId,
        turns_in_flight: this.activeTurnIds.length,
        decision: "turn_ended",
      }, `SDK result correlated to accepted turn`);
    }
    LOGGER.logVerbose({
      agent_repl_session_id: this.deps.sessionId,
      sdk_type: msg.type,
      claude_session_id: vendor.sessionId,
      lifecycle_count: authoritativeLifecycle.length,
      store_key: this.store.storeSessionId(),
      turn_id: rootTurnId,
    }, "writing persistent SDK event batch to store");
    const terminalUsage = terminalTurnId === undefined
      ? undefined
      : await this.captureFiveHourUsage("turn_end", terminalTurnId, this.turnStartUsage.get(terminalTurnId));
    if (terminalTurnId !== undefined) this.turnStartUsage.delete(terminalTurnId);
    const terminalUsageEvent = terminalTurnId === undefined || terminalUsage === undefined
      ? undefined
      : this.accountUsageObservationEvent("turn_end", terminalTurnId, terminalBoundaryAtMs!, terminalUsage, vendor.sessionId);
    const nonTerminalLifecycle = authoritativeLifecycle.filter((event) => event.payload.case !== "turnEnded");
    const terminalLifecycle = authoritativeLifecycle.filter((event) => event.payload.case === "turnEnded");
    const persistentBatch = [
      ...(turnClaimBridge !== undefined ? [turnClaimBridge] : []),
      vendor,
      ...nonTerminalLifecycle,
      ...(terminalUsageEvent === undefined ? [] : [terminalUsageEvent]),
      ...terminalLifecycle,
    ];
    if (terminalTurnId !== undefined) this.activeTurnStarts.delete(terminalTurnId);
    try {
      const ack = await this.store.write(persistentBatch);
      for (const taskId of startedTaskIds) {
        this.liveSdkTaskIds.add(taskId);
        if (rootTurnId !== undefined) {
          const turnTaskIds = this.turnSdkTaskIds.get(rootTurnId) ?? new Set<string>();
          turnTaskIds.add(taskId);
          this.turnSdkTaskIds.set(rootTurnId, turnTaskIds);
        }
      }
      for (const taskId of endedTaskIds) {
        this.liveSdkTaskIds.delete(taskId);
        this.completedSdkTaskIds.add(taskId);
      }
      if (startedTaskIds.length > 0 || endedTaskIds.length > 0) {
        LOGGER.log({
          agent_repl_session_id: this.deps.sessionId,
          request_id: rootTurnId,
          turn_id: rootTurnId,
          started_sdk_task_ids: startedTaskIds,
          ended_sdk_task_ids: endedTaskIds,
          live_sdk_task_count: this.liveSdkTaskIds.size,
          live_sdk_task_ids: [...this.liveSdkTaskIds].sort(),
          store_last_seq: ack.lastSeq,
          decision: "commit_sdk_task_lifecycle",
        }, "SDK task lifecycle is durably reflected in root-turn liveness");
      }
      if (msg.type === "result" && rootTurnId !== undefined && taskNotificationResultCountAfterWrite !== undefined) {
        this.turnTaskNotificationResultCounts.set(rootTurnId, taskNotificationResultCountAfterWrite);
      }
      if (terminalTurnId !== undefined) {
        this.turnSdkTaskIds.delete(terminalTurnId);
        this.turnTaskNotificationResultCounts.delete(terminalTurnId);
        const index = this.pendingTurnEndIds.indexOf(terminalTurnId);
        if (index < 0) {
          LOGGER.log({
            level: "error",
            agent_repl_session_id: this.deps.sessionId,
            request_id: terminalTurnId,
            turn_id: terminalTurnId,
            store_last_seq: ack.lastSeq,
            pending_turn_end_ids: this.pendingTurnEndIds,
            decision: "reject_missing_pending_end_claim",
          }, "StoreWriteAck for TurnEnded had no pending handshake claim");
          return;
        }
        this.pendingTurnEndIds.splice(index, 1);
        LOGGER.log({
          agent_repl_session_id: this.deps.sessionId,
          request_id: terminalTurnId,
          turn_id: terminalTurnId,
          store_last_seq: ack.lastSeq,
          pending_turn_end_ids: this.pendingTurnEndIds,
          decision: "retire_durable_turn_end_claim",
        }, "TurnEnded is durably observable; retired its handshake claim");
      }
    } catch (cause) {
      LOGGER.log({
        level: "error",
        operation: "shim.uds-session.persistent-evidence",
        agent_repl_session_id: this.deps.sessionId,
        claude_session_id: vendor.sessionId,
        query_instance_id: this.queryInstanceId,
        ...(rootTurnId === undefined ? {} : {
          request_id: rootTurnId,
          turn_id: rootTurnId,
        }),
        sdk_type: msg.type,
        evidence_kind: terminalTurnId === undefined ? "vendor_response" : "terminal_turn_batch",
        persistent_event_count: persistentBatch.length,
        failed_operation: terminalTurnId === undefined
          ? "store.write.vendor_response"
          : "store.write.terminal_turn_batch",
        outcome: "fatal_missing_persistent_evidence_receipt",
        cause,
      }, "persistent SDK event batch did not receive a durable store receipt");
      throw cause;
    }
  }

  /** Build the lifecycle fact placed first in an acknowledged turn-start batch. */
  private turnStartedEvent(requestId: string, text: string, boundaryAtMs: number): Event {
    return create(EventSchema, {
      sessionId: this.store.storeSessionId(),
      seq: 0n,
      plane: Plane.STREAM,
      class: EventClass.PERSISTENT,
      requestId,
      producedAtMs: BigInt(boundaryAtMs),
      payload: {
        case: "turnStarted",
        value: create(TurnStartedSchema, {
          promptPreview: promptPreview(text),
          turnId: requestId,
        }),
      },
    });
  }

  private convertOpts(): { nowMs?: number } {
    return this.deps.nowMs !== undefined ? { nowMs: this.deps.nowMs() } : {};
  }

  private now(): number {
    return this.deps.nowMs !== undefined ? this.deps.nowMs() : Date.now();
  }

  /** Stable turn claims to announce on ShimHello, in acceptance order. */
  private handshakeTurnIds(): string[] {
    return [...this.pendingTurnEndIds, ...this.activeTurnIds];
  }

  /**
   * Report an SDK-side failure to the daemon as honest downtime.
   *
   * The one channel this session has for "something the user asked for did
   * not happen": a loud log PLUS a DegradedState the daemon can surface. A
   * bare `log` is not enough — nobody watching the UI ever sees it.
   */
  private reportDegraded(component: string, reason: string): void {
    LOGGER.log({ level: "error", agent_repl_session_id: this.deps.sessionId }, reason);
    this.server.sendEvent(this.degradedEvent(create(DegradedStateSchema, {
      component,
      reason,
      droppedCount: 0n,
      recovered: false,
    })));
  }

  /**
   * The RETURN half of the prompt round-trip receipt: one line when a merged
   * store event carrying the user's own prompt text goes back to the daemon.
   *
   * This is the leg nothing else covers. The live user-prompt echo does NOT
   * come back on the SDK stream (see submitPrompt) — the CLI writes a
   * transcript line, the sidecar files it, and it reaches the daemon only
   * through this forward. Without a line here, a prompt that was accepted but
   * never became a bubble looked identical to one that came back fine.
   */
  private logUserPromptForward(evt: Event): void {
    // Discriminator first: every lifecycle twin and every non-vendor payload
    // is out on a single string compare, before anything is unpacked.
    if (evt.payload.case !== "vendor") return;
    const prompt = userPromptText(evt.payload.value);
    if (prompt === null) return;
    LOGGER.log(
      { agent_repl_session_id: this.deps.sessionId, seq: evt.seq, len: prompt.len, arm: prompt.arm },
      `user prompt event forwarded to daemon`);
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

/**
 * The user-prompt text a vendor Any carries, or null when it carries none.
 *
 * TWO CARRIERS, deliberately both: a `data.v1.TranscriptLine` whose line is a
 * UserLine is the FILE plane — the live echo the CLI writes and the sidecar
 * republishes, which is what becomes the GUI bubble — while a
 * `data.v1.UserMessage` inside a ClaudeStreamMessage is the STREAM plane, how
 * the same turn arrives on a resume replay. Watching only one leaves the
 * other's prompts invisible.
 *
 * Neither type url matching means this is some other vendor payload, and the
 * Any is never parsed: the check is a string compare on the type url.
 */
function userPromptText(vendor: Any): { len: number; arm: string } | null {
  if (envelopeIs(vendor, TranscriptLineSchema)) {
    const line = unpackAs(vendor, TranscriptLineSchema);
    if (line?.line.case !== "user") return null;
    const len = userTextLen(line.line.value.message);
    return len > 0 ? { len, arm: "transcript_user_line" } : null;
  }
  if (envelopeIs(vendor, ClaudeStreamMessageSchema)) {
    const csm = unpackAs(vendor, ClaudeStreamMessageSchema);
    if (csm?.msg.case !== "user") return null;
    const len = userTextLen(csm.msg.value.message);
    return len > 0 ? { len, arm: "user_message" } : null;
  }
  return null;
}

/**
 * Prompt-text length of a user message, counted exactly as the daemon's
 * userTurnReceipt counts it: the content string, or the sum of its TEXT
 * blocks.
 *
 * Tool-result blocks contribute nothing on purpose. Pure tool feedback rides
 * the user role too, and counting it would put a "prompt forwarded" line on
 * every tool call, burying the one receipt per prompt this exists for. Such a
 * message totals 0 and the caller drops it.
 */
function userTextLen(message: ApiUserMessage | undefined): number {
  const content = message?.content;
  if (content?.case === "contentString") return content.value.length;
  if (content?.case === "contentBlocks") {
    let len = 0;
    for (const block of content.value.blocks) {
      if (block.block.case === "text") len += block.block.value.text.length;
    }
    return len;
  }
  return 0;
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

/** Preserve an SDK termination diagnostic, explicitly naming an empty one. */
function terminationCause(cause: unknown, kind: string): string {
  const message = errMsg(cause);
  return message.trim() === "" ? `SDK reported ${kind} with an empty diagnostic` : message;
}
