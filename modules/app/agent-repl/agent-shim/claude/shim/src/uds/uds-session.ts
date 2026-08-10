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
import { QueueOp, TranscriptLineSchema } from "../../../../../proto/gen/ts/agentshim/data/v1/transcript_pb.js";
import type { ApiUserMessage } from "../../../../../proto/gen/ts/agentshim/data/v1/tools_pb.js";
import { bindLog, setClaudeSessionId } from "./log.js";
import { normalizeModel } from "../model.js";
import { queryRuntimeIdentity } from "../runtime-identity.js";
import { logAssistantApiResponseUsage } from "../usage-log.js";
import {
  compareFiveHourResetWindows,
  FIVE_HOUR_RESET_WINDOW_CONTRACT_VERSION,
  fiveHourUsageSample,
  type FiveHourUsageSample,
  type SubscriptionUsageResponse,
} from "../subscription-usage.js";
import {
  CancelDetachedAgents,
  DaemonHello,
  DegradedState,
  DetachedAgentsCancelledSchema,
  DetachedCancelOutcome,
  DetachedCancelOutcomeSchema,
  DetachedCancelUnsupportedSchema,
  NoDetachedAgentsRunningSchema,
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
  PromptOrigin,
  ReplayDoneSchema,
  InterruptOutcome,
  ReplayRequest,
  SessionSource,
  SessionStartedSchema,
  ShimReadySchema,
  TurnClaimBridgeSchema,
  TurnEndedSchema,
  TurnStartedSchema,
  AccountUsageObservationSchema,
  AccountUsageAvailableSchema,
  AccountUsageUnavailableSchema,
  FiveHourWindowUnavailableSchema,
  QueryCreatedSchema,
  QueryIteratorFailureSchema,
  QueryLifecycleSchema,
  QueryRuntimeObservedSchema,
  QueryStartupFailureSchema,
  QueryTerminatedSchema,
  ResumedQuerySchema,
  SessionRewoundSchema,
  KeepAliveDiscardSchema,
  TurnEndUsageBoundarySchema,
  TurnStartUsageBoundarySchema,
  UnexpectedQueryEofSchema,
  UsageSamplingFailureSchema,
  UsageServiceUnavailableSchema,
  UsageWindowSchema,
  UtilizationUnavailableSchema,
  FreshQuerySchema,
  IntentionalQueryTerminationSchema,
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

/**
 * The daemon-supplied account of one conversation rewind, as spawned on argv.
 *
 * `previousVendorSessionId` is the retired seq space; the NEW id is not
 * carried here because it is already the session's store key — deriving it
 * rather than accepting a second copy makes the two disagreeing impossible.
 */
export interface RewindLineage {
  previousVendorSessionId: string;
  retainedLeafUuid: string;
  /** Dropped keep-alive turn ids, in submission order. Order is contractual. */
  droppedTurnIds: string[];
}

/**
 * Dedup key for a SessionRewound, per the store's §6.4 per-producer identity
 * convention (`<prefix>:<stable identity>`, cf. `clear:<uuid>`).
 *
 * Keyed on the RETIRED vendor session id: dedup is scoped
 * (session_id, dedup_key), the row lives under the new session id, and exactly
 * one rewind can ever have produced that pair. So a shim restart re-emitting
 * the event with the same argv collapses onto the existing row instead of
 * duplicating the lineage record.
 */
export function sessionRewoundDedupKey(previousVendorSessionId: string): string {
  return `rewind:${previousVendorSessionId}`;
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
   * Rewind lineage from argv, when the daemon respawned this shim onto a
   * TRUNCATED copy of a previous vendor transcript. Present => the session
   * emits exactly one SessionRewound into the new seq space at bring-up.
   *
   * The shim does not perform the truncation and owns no timers for it: the
   * daemon did the surgery before spawning us, and the shim still owns exactly
   * one query for its whole process life. This is durable EXPLANATION of an
   * identity change that already happened, nothing more.
   */
  rewindLineage?: RewindLineage;
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
  /**
   * How long a dropped store link may stay down before the store client reports
   * it as a degradation (its relink retry budget). Production leaves this at
   * the client's own default; a test that wants the report without waiting out
   * the real budget compresses it.
   */
  storeRelinkReportAfterMs?: number;
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
   * The exact options object this shim passed to the SDK's query().
   *
   * It is carried rather than re-derived because the runtime identity's
   * `effective_options` fingerprint is only evidence if it hashes what the
   * query actually ran under; a second derivation would drift from the first
   * the moment an option is added in one place and not the other.
   */
  effectiveQueryOptions?: Record<string, unknown>;
  /**
   * The system prompt this shim passed to the SDK's query() — the cacheable
   * instruction prefix every request re-sends, and so the thing the runtime
   * identity's `context_prefix` fingerprint is about.
   */
  contextPrefix?: unknown;
  /**
   * How long a bounded replay waits for the next store frame before deciding
   * its subscription drained. A FAILURE bound, not a pace: a store mid-replay
   * writes back to back. Default 5000ms; tests shorten it.
   */
  replayIdleMs?: number;
  /**
   * How long a turn this session ACKED as interrupted may stay open waiting for
   * the SDK's own terminal result before the session closes it itself.
   *
   * A FAILURE bound, not a pace: the CLI answers an interrupt by aborting the
   * turn and emitting its result promptly, so anything past this bound is the
   * terminal never coming. Default 15000ms; tests compress it.
   */
  interruptTerminalGraceMs?: number;
  /**
   * How long an OPEN turn may go with NO SDK activity of any kind before this
   * session decides the query behind it died without producing a terminal.
   *
   * IT IS THE INTERRUPT GRACE, GENERALIZED, and it exists because the interrupt
   * grace can only ever fire for a turn somebody thought to interrupt. A query
   * that dies quietly — the iterator neither yields another message nor throws —
   * leaves its turn latched open for the life of the process, and the daemon
   * carries the phantom `turn_active` behind it for as long as the shim lives.
   * That was observed as a turn whose "live" start was two days old.
   *
   * A FAILURE bound, not a pace, and deliberately generous: a working agent
   * emits stream deltas, tool calls and task lifecycle continuously, so a turn
   * that has produced NOTHING for this long is not a slow turn — it is a dead
   * query. Default 600000ms (ten minutes); tests compress it.
   */
  turnQuietGraceMs?: number;
}

/**
 * WHY a turn's terminal is being synthesized rather than observed.
 *
 * It exists so the two watchdogs share one writer without sharing one story:
 * both produce the identical durable boundary, and the reader of a log line or
 * a DegradedState can still tell an interrupt this session acked from a stream
 * that simply stopped speaking. A close that could not name its observation is
 * the shape this type exists to make unrepresentable.
 */
interface UnterminatedTurnEvidence {
  /** The `decision` token the canonical log line carries. */
  decision: string;
  /** The canonical log line's human sentence. */
  logMessage: string;
  /** The DegradedState reason, built around the turn being closed. */
  degradedReason: (turnId: string) => string;
  /** The bound that expired. */
  graceMs: number;
  /** How long the SDK stream had actually been silent, on the quiet arm only. */
  quietMs?: number;
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
  /** File-plane task notifications queued for an internal SDK result cycle. */
  private readonly pendingTaskNotificationQueue = new Set<string>();
  /**
   * The SESSION-OWNED degraded windows that are still open, keyed by component.
   *
   * A DegradedState is a STATE, not an instant, and it travels to the daemon on
   * the EPHEMERAL path — never stored, never replayed. Without this map the
   * only degradation a reattaching daemon could be re-told about was the store
   * client's own (StoreClient.openDegradedReport), because that is the only
   * producer that kept its current state; every degradation the session itself
   * raised — a permission mode a running session could not adopt, a model
   * catalog that never published, an interrupt anomaly — was announced once and
   * then existed nowhere. One daemon bounce and the session came back looking
   * healthy while still running under the posture the user was warned about.
   *
   * Keyed by component because that is the daemon's own granularity: it holds
   * one fault row per component and closes it on the `recovered: true` edge of
   * the same component's report, so this mirrors that ledger rather than
   * inventing a second one.
   */
  private readonly openDegradedWindows = new Map<string, DegradedState>();
  /**
   * Deadlines for the turns this session ACKED as interrupted.
   *
   * An `INTERRUPT_OUTCOME_INTERRUPTED` ack is a claim that a turn was stopped,
   * and a turn that was stopped must reach its terminal. The SDK normally
   * supplies that terminal itself — the CLI aborts and emits a `result`, which
   * closes the turn through the ordinary path. When it does not (a turn the
   * shim was retaining for background-task cycles is not running as far as the
   * CLI is concerned, so the interrupt has nothing to abort and no result ever
   * follows), the turn would stay latched open for the life of the process and
   * every consumer of `turn_active` — hibernation, merge resolution, the
   * restart turn-guard — waits on a turn that will never end.
   *
   * So the claim is BACKED: whichever terminal arrives first wins, and if none
   * arrives within the grace this session writes it (`closeUnterminatedTurn`).
   */
  private readonly displacedTurnDeadlines = new Map<string, ReturnType<typeof setTimeout>>();
  /**
   * The turns this session ACKED as interrupted, as a fact about the OUTCOME
   * rather than about the deadline.
   *
   * It is deliberately not the deadline map above, which is disarmed the
   * instant the terminal arrives — and the terminal is precisely the message
   * that needs to know. So the fact outlives the watchdog by a hair: it is
   * added when the ack claims the turn displaced, and spent by whichever
   * terminal closes that turn (the SDK's result, or the one this session
   * synthesizes).
   */
  private readonly interruptedTurnIds = new Set<string>();
  /**
   * When the SDK last produced ANYTHING for this session.
   *
   * It is the evidence the quiet watchdog reads, and it is stamped at the ONE
   * funnel every SDK message passes through (`routeSdkMessage`), so no message
   * kind has to remember to refresh it. A turn open while this has not moved
   * for `turnQuietGraceMs` is a turn whose query is not running.
   */
  private lastSdkActivityMs: number;
  /**
   * The single re-arming watchdog over open turns that have gone quiet.
   *
   * ONE TIMER, NOT ONE PER TURN, because the evidence it reads is
   * session-wide: the SDK stream is one stream, and a session with several
   * turns open has them all behind the same iterator. It is armed when a turn
   * opens and disarmed the moment no turn is open, so an idle session holds no
   * timer at all.
   */
  private turnQuietTimer: ReturnType<typeof setTimeout> | null = null;
  /**
   * Turns whose terminal this session synthesized, oldest first.
   *
   * A late SDK result for one of them is still durable vendor evidence and is
   * stored as such; the entry exists only so that result is reported as the
   * known late terminal it is rather than as an uncorrelated result.
   */
  private readonly synthesizedTurnEndIds: string[] = [];
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
  /** Grace an acked-interrupted turn gets to receive its SDK terminal (see deps). */
  private readonly interruptTerminalGraceMs: number;
  /** Quiet window an open turn gets before its query is judged dead (see deps). */
  private readonly turnQuietGraceMs: number;
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
    this.interruptTerminalGraceMs = deps.interruptTerminalGraceMs ?? 15000;
    this.turnQuietGraceMs = deps.turnQuietGraceMs ?? 600000;
    this.lastSdkActivityMs = this.now();
    this.effectivePermissionMode = deps.permissionMode ?? "default";
    LOGGER.log({ agent_repl_session_id: deps.sessionId, uds_socket: deps.udsSocketPath, store_socket: deps.storeSocketPath, session_source: deps.sessionSource, store_key_known: deps.storeSessionId !== undefined && deps.storeSessionId !== "", permission_mode: this.effectivePermissionMode }, "constructed UDS shim session");
    const target: SdkControlTarget = {
      submitPrompt: async ({ requestId, text, permissionMode, promptOrigin }): Promise<void> => {
        const boundaryAtMs = this.now();
        const usage = await this.captureFiveHourUsage("turn_start", requestId);
        const turnStart = this.turnStartedEvent(requestId, text, boundaryAtMs, promptOrigin);
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
        // THE TURN IS WATCHED FROM THE INSTANT IT OPENS, not from the instant
        // somebody interrupts it. The prompt about to be pushed onto the input
        // queue is the last thing this session does for this turn; everything
        // after it has to come back off the SDK stream, so a turn that opens
        // and then produces nothing is precisely the failure the watchdog
        // exists to notice — unless a background task from an earlier turn is
        // still running, in which case the reconcile leaves the watch down
        // because this session's stream is entitled to stay silent.
        this.reconcileTurnQuietWatch();
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
          prompt_origin: promptOrigin,
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
        // Read in the same synchronous breath as the verdict: these are the
        // turns the INTERRUPTED ack is a claim ABOUT, so they are exactly the
        // turns whose terminal this session now owes.
        const displacedTurnIds = [...this.activeTurnIds];
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
        // THE ACK IS BACKED, NOT MERELY ASSERTED. Arming happens on the same
        // path that returns INTERRUPTED, so the two cannot disagree: every
        // turn this ack calls displaced now has a deadline by which its
        // terminal exists.
        if (wasLive) this.armDisplacedTurnTerminals(displacedTurnIds);
        return wasLive ? InterruptOutcome.INTERRUPTED : InterruptOutcome.ALREADY_COMPLETE;
      },
      cancelDetachedAgents: async ({ requestId }): Promise<DetachedCancelOutcome> => {
        // THE SAME STRUCTURAL ARGUMENT interrupt() MAKES, applied to the task
        // set instead of the turn counter.
        //
        // The snapshot is taken SYNCHRONOUSLY, here, before the first await.
        // This event loop is single-threaded and this class owns
        // liveSdkTaskIds — a task enters it on TaskStarted and leaves it on
        // its terminal — so no task can start or end between this read and
        // the outcome it decides. The ambiguous case "the last agent finished
        // at about the moment the cancel landed, so did we stop it or was it
        // already done?" is unrepresentable rather than merely unlikely.
        const taskIds = [...this.liveSdkTaskIds].sort();
        const query = this.query;
        LOGGER.log({
          agent_repl_session_id: this.deps.sessionId,
          request_id: requestId,
          live_sdk_task_ids: taskIds,
          live_sdk_task_count: taskIds.length,
          has_query: query !== null,
        }, "processing daemon cancel-detached-agents request");
        if (query === null) {
          // Provably unattemptable, and distinct from an idle session: there
          // is nothing to ask, rather than nothing running. Reported on the
          // degraded channel as well as on the ack, because a session serving
          // controls with no query behind it is a bring-up fault.
          const detail = "cancel detached agents could not be attempted: no SDK query is constructed for this session";
          this.reportDegraded("claude-shim-cancel-detached", detail);
          return create(DetachedCancelOutcomeSchema, {
            outcome: { case: "unsupported", value: create(DetachedCancelUnsupportedSchema, { detail }) },
          });
        }
        if (taskIds.length === 0) {
          return create(DetachedCancelOutcomeSchema, {
            outcome: { case: "nothingRunning", value: create(NoDetachedAgentsRunningSchema, {}) },
          });
        }
        // THE SDK'S OWN STOP, one task at a time. `stopTask` is the native
        // mechanism (control request `stop_task`); the CLI answers it by
        // emitting that task's `task_notification` with status `stopped`,
        // which converts to the ordinary TaskEnded the whole stack already
        // settles on. Nothing here reaches for a process boundary, because
        // the shim owns none that a subagent runs behind.
        const stopped: string[] = [];
        const failures: string[] = [];
        for (const taskId of taskIds) {
          try {
            await query.query.stopTask(taskId);
            stopped.push(taskId);
          } catch (err) {
            failures.push(`${taskId}: ${errMsg(err)}`);
          }
        }
        if (failures.length > 0) {
          // A PARTIAL STOP IS NEVER SILENT. The ack names only what was
          // actually stopped, and the agents that refused the stop are still
          // running and still spending tokens — which is exactly the sort of
          // divergence the degraded channel exists to say out loud.
          this.reportDegraded("claude-shim-cancel-detached",
            `${failures.length} of ${taskIds.length} detached agent(s) could not be stopped and are STILL RUNNING: ${failures.join("; ")}`);
        }
        if (stopped.length === 0) {
          // Nothing was stopped at all. A throw, so control.ts answers with a
          // Nack rather than an Ack whose `cancelled` arm would claim an
          // empty stop as a success.
          throw new Error(`no detached agent could be stopped (${taskIds.length} were running): ${failures.join("; ")}`);
        }
        LOGGER.log({
          agent_repl_session_id: this.deps.sessionId,
          request_id: requestId,
          stopped_task_ids: stopped,
          stopped_task_count: stopped.length,
          failed_task_count: failures.length,
        }, "detached agents stopped via the SDK stop_task control");
        return create(DetachedCancelOutcomeSchema, {
          outcome: { case: "cancelled", value: create(DetachedAgentsCancelledSchema, { taskIds: stopped }) },
        });
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
      // The ONE model this shim has observed or confirmed, handed back
      // verbatim. It already follows both authorities — every SDK
      // `system:init` and every confirmed setModel write it — so this read
      // needs no SDK round-trip and cannot race one.
      selectedModel: (): string => this.effectiveModel,
    };
    this.control = new ControlDispatch(
      target,
      (req) => this.server.sendPermissionRequest(req),
      {
        ...(this.deps.newRequestId !== undefined ? { newRequestId: this.deps.newRequestId } : {}),
        // activeTurnIds, not handshakeTurnIds: a blocked canUseTool belongs to
        // a turn the SDK is still RUNNING. A turn whose terminal is merely
        // owed (pendingTurnEndIds) has no tool call left to unblock, so
        // counting it live would let a reattach re-ask a question nothing is
        // waiting on.
        isTurnLive: () => this.activeTurnIds.length > 0,
      },
    );

    this.store = new StoreClient({
      socketPath: this.deps.storeSocketPath,
      sessionId: this.deps.sessionId,
      producer: `claude-shim:${this.deps.sessionId}`,
      ...(this.deps.heartbeatIntervalMs !== undefined ? { heartbeatIntervalMs: this.deps.heartbeatIntervalMs } : {}),
      ...(this.deps.storeSessionId !== undefined ? { storeSessionId: this.deps.storeSessionId } : {}),
      ...(this.deps.storeRelinkReportAfterMs !== undefined ? { relinkReportAfterMs: this.deps.storeRelinkReportAfterMs } : {}),
    });

    const handlers: SessionServerHandlers = {
      onSubmitPrompt: (m) => this.control.handleSubmitPrompt(m),
      onInterrupt: (m) => this.control.handleInterrupt(m),
      onCancelDetachedAgents: (m: CancelDetachedAgents) => this.control.handleCancelDetachedAgents(m),
      onSetModel: (m) => this.control.handleSetModel(m),
      onQuerySelectedModel: (m) => this.control.handleQuerySelectedModel(m),
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
        // DESIGN DEPENDENCE — THE SHIM MUST KEEP WORKING WITH NO DAEMON
        // ATTACHED.
        //
        // A closed or lost UDS connection is a routine condition, not a fault:
        // it is what every daemon restart, deploy and reload looks like from
        // here. The constraints this session holds across it:
        //
        //  - The SDK query keeps running and its turn keeps producing. Nothing
        //    is torn down and no turn is cancelled by the loss.
        //  - An IN-FLIGHT INTERRUPT COMPLETES LOCALLY. The control dispatch is
        //    synchronous (uds/server.ts dispatch -> control.ts
        //    handleInterrupt), so a stop that reached this process has already
        //    run against the SDK. Losing the socket may lose the Ack; it may
        //    never abandon the abort, and the abort's own terminal is written to
        //    the store like any other.
        //  - Pending permission waits are NOT cancelled — a reattaching daemon
        //    can still answer them; cancelAll fires only on interrupt/shutdown.
        //  - Every persistent event continues to be written to the store,
        //    whether or not anything is listening on this socket.
        //
        // ALL OF IT IS LICENSED BY THE STORE BEING WHAT RESOLVES THE DIVERGENCE
        // ON REATTACH. The two sides diverge for exactly as long as the
        // connection is down, and the reconnect does not have to reconstruct
        // what happened from either side's memory: the daemon replays from its
        // own cursor and reads the durable record, which is what settles a turn
        // that finished in the gap and what answers a stop whose Ack was lost.
        // A shim that instead stopped, cancelled, or buffered on disconnect
        // would destroy the very evidence that reconciliation reads.
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
      //
      // DESIGN DEPENDENCE — ORDINARY TERMINAL TURN EVENTS ARE DELIVERED BY
      // DURABLE REPLAY, NOT BY THIS SEND.
      //
      // `sendEvent` is fire-and-forget and drops when no daemon is attached
      // (uds/server.ts). For a PERSISTENT event, including the `TurnEnded` that
      // closes a turn, that drop loses nothing, and this is the chain it depends
      // on. Each link is a structural property of code in this file or the
      // daemon's, not a timing assumption, and a change to any of them breaks
      // the guarantee:
      //
      //  1. The terminal batch is written to the store with an AWAITED receipt
      //     (`await this.store.write(persistentBatch)` in routeSdkMessage), and
      //     a write that does not receipt THROWS. A `TurnEnded` therefore never
      //     reaches this callback without already being durable.
      //  2. Its turn id stays in `pendingTurnEndIds` — and so in EVERY
      //     `ShimHello.active_turn_ids` (`handshakeTurnIds`) — until that
      //     receipt lands. No hello can report idle for a turn whose end is not
      //     yet in the store.
      //  3. What arrives here is the store's own merged echo of an
      //     already-persisted event, so dropping it discards a copy, never the
      //     record.
      //  4. The daemon advances `last_seen_seq` ONLY for an event it received
      //     AND accepted (daemon internal/shimclient/events.go). A dropped
      //     event's seq therefore stays above the daemon's cursor.
      //  5. The reattach's `DaemonHello.from_seq` IS that cursor, and
      //     `completeWiring` opens the standing subscription at it — which
      //     replays every event dropped here, the terminal one included.
      //
      // The flushed path below is NOT durability. It exists for the
      // unexpected-termination pair's ORDERING latch, which waits on the frame
      // reaching a handshaked daemon; a durable event needs no such wait.
      this.store.onMerged((evt) => {
        this.observeTaskNotificationQueue(evt);
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
      // The rewind is the reason this seq space exists, so it is the FIRST
      // persistent event in it — the explanation precedes what it explains.
      // A reader replaying the new vendor session must learn the conversation
      // was truncated before it sees the query that resumed it.
      await this.persistSessionRewound();
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
    // THE RE-ANNOUNCE PRECEDES THE ACK, and that order is the whole guarantee.
    // Both frames travel this one ordered connection, so a daemon that has read
    // ShimReady has necessarily already read every DegradedState re-announced
    // ahead of it — and ShimReady is what drives the session to OPERATIONAL.
    // "The session is up" therefore cannot be observed before "and here is what
    // is wrong with it". Sent after the ack instead, the two facts were merely
    // adjacent, and every reader in between saw a session that had just come
    // back perfectly healthy. Nothing here can abort the gate: a bring-up is
    // only failed by a claude-shim-sdk fault (bringupescape.go
    // noteBringUpFault), and the gate is explicitly open to a DegradedState
    // before readiness (uds/server.ts onMessage).
    this.reannounceDegradedState();
    this.server.sendReady(create(ShimReadySchema, {
      sessionId: this.deps.sessionId,
      fromSeq,
      vendorSessionId: this.store.storeSessionId(),
    }));
    // THE OPEN QUESTION IS RE-ASKED, LAST, ONCE THE GATE IS CLOSED. A pending
    // canUseTool is the one piece of session state no daemon can reconstruct:
    // the first daemon may never have received it, or may have died holding it,
    // and either way only this shim still has the blocked promise. Re-sending
    // it after ShimReady means the daemon it lands on is wired for the session
    // and can route the ask to the frontend.
    this.control.resendPending("daemon reattach");
    this.publishModelCatalog();
  }

  /**
   * Re-announce this session's CURRENT degraded state to a daemon that has
   * just (re)attached.
   *
   * EPHEMERAL VERSUS STATE — the whole reason this exists. Two kinds of thing
   * travel to the daemon while it is absent, and only one of them can be
   * recovered afterwards:
   *
   *   - PERSISTENT events are durable in the store. Whatever the daemon missed
   *     it replays from the from_seq its next DaemonHello carries, so nothing
   *     is owed here.
   *   - EPHEMERAL events are not written anywhere. A ContentDelta or a typing
   *     frame dropped into the gap is gone, and that is BY DESIGN: it described
   *     an instant that has passed, and re-sending it later would be a lie
   *     about when it happened.
   *
   * A DegradedState rides the ephemeral path but is not an instant — it is a
   * STATE that is either still true or no longer true right now. Losing it
   * meant a session that degraded while the daemon was down came back looking
   * healthy, with an open store outage nobody was ever told about. So rather
   * than making ephemerals replayable, the shim re-reports the state it is IN,
   * read fresh at the moment the link is proven usable. A healthy session
   * reports nothing, which is what keeps this from manufacturing faults.
   *
   * TWO LEDGERS, ONE ANSWER. The store client owns the current state of the
   * store link; this session owns the current state of everything it reported
   * itself (openDegradedWindows). Both are asked, because a daemon that is
   * re-told about only one of them is still being told a session is healthier
   * than it is.
   */
  private reannounceDegradedState(): void {
    const open: DegradedState[] = [...this.openDegradedWindows.values()];
    const storeOpen = this.store.openDegradedReport();
    if (storeOpen !== null) open.push(storeOpen);
    if (open.length === 0) return;
    for (const report of open) {
      LOGGER.log({
        level: "warn",
        agent_repl_session_id: this.deps.sessionId,
        component: report.component,
        reason: report.reason,
      }, "re-announcing this session's OPEN degraded window to the reattached daemon: a DegradedState is ephemeral, so one raised while no daemon was attached reaches this one only by being re-reported");
      this.server.sendEvent(this.degradedEvent(report));
    }
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
        this.emitDegraded(create(DegradedStateSchema, {
          component: "claude-shim-model-catalog",
          reason,
          droppedCount: 0n,
          recovered: false,
        }));
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
      queryInstanceId: this.queryInstanceId,
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
   * Live detached-task count: the set a CancelDetachedAgents stops
   * (test/diagnostics).
   *
   * It reads the SAME set the cancel snapshots, so a test can wait for a
   * launch to register before asking for the stop rather than sleeping and
   * hoping — the difference between a deterministic test and a flaky one.
   */
  detachedTaskCount(): number {
    return this.liveSdkTaskIds.size;
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
      // A dying session cannot write a synthesized end, and the process exit
      // itself closes every open claim (the daemon's own stop reconciliation).
      for (const timer of this.displacedTurnDeadlines.values()) clearTimeout(timer);
      this.displacedTurnDeadlines.clear();
      this.interruptedTurnIds.clear();
      this.disarmTurnQuietWatchdog("session_resources_closing");
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
      queryInstanceId: this.queryInstanceId,
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
          resetsAtMs: null,
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
      if (sample.resetsAtMs === null) {
        throw new Error("available five-hour usage observation omitted its parsed reset timestamp");
      }
      return BigInt(sample.resetsAtMs);
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
      queryInstanceId: this.queryInstanceId,
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

  /**
   * Persist the durable explanation of a rewound vendor-session identity.
   *
   * No-op without argv lineage, which is the overwhelmingly common case: an
   * ordinary resume rewinds nothing.
   *
   * The receipt admits TWO shapes, and both are success. A first emission is
   * accepted with a fresh seq; a shim RESTART on the same truncated transcript
   * re-emits the identical event and the store's (session_id, dedup_key) index
   * collapses it, returning deduped=1 and last_seq=0 because a duplicate
   * consumes no seq. Anything else — a partial batch, a receipt claiming both,
   * an acceptance with no assigned seq — is a store contract violation and
   * fails the bring-up loudly rather than proceeding on an unproven record.
   */
  private async persistSessionRewound(): Promise<void> {
    const lineage = this.deps.rewindLineage;
    if (lineage === undefined) return;
    const storeKey = this.store.storeSessionId();
    if (storeKey === lineage.previousVendorSessionId) {
      throw new Error(`SessionRewound would name one vendor session as both sides of the rewind (${storeKey})`);
    }
    const envelope = this.sessionRewoundEnvelope(lineage, storeKey);
    LOGGER.log({
      agent_repl_session_id: this.deps.sessionId,
      query_instance_id: this.queryInstanceId,
      store_key: storeKey,
      previous_vendor_session_id: lineage.previousVendorSessionId,
      retained_leaf_uuid: lineage.retainedLeafUuid,
      dropped_turn_count: lineage.droppedTurnIds.length,
      dedup_key: envelope.dedupKey,
    }, "persisting session rewind lineage into the resumed vendor seq space");
    const ack = await this.store.write([envelope]);
    const accepted = ack.accepted === 1n && ack.deduped === 0n && ack.lastSeq !== 0n;
    const deduped = ack.accepted === 0n && ack.deduped === 1n;
    if (!accepted && !deduped) {
      LOGGER.log({
        level: "error",
        operation: "shim.uds-session.session-rewound",
        agent_repl_session_id: this.deps.sessionId,
        query_instance_id: this.queryInstanceId,
        store_key: storeKey,
        previous_vendor_session_id: lineage.previousVendorSessionId,
        accepted: String(ack.accepted),
        deduped: String(ack.deduped),
        last_seq: String(ack.lastSeq),
        outcome: "fatal_invalid_session_rewound_receipt",
      }, "session rewind lineage receipt was neither a single acceptance nor a single dedup");
      throw new Error(`SessionRewound persistence returned an invalid receipt: accepted=${ack.accepted} deduped=${ack.deduped} last_seq=${ack.lastSeq}`);
    }
    LOGGER.log({
      agent_repl_session_id: this.deps.sessionId,
      query_instance_id: this.queryInstanceId,
      store_key: storeKey,
      outcome: deduped ? "deduped_existing_lineage" : "persisted_new_lineage",
      seq: String(ack.lastSeq),
    }, "session rewind lineage received its durable store receipt");
  }

  /** Build the PERSISTENT SessionRewound envelope for the new vendor seq space. */
  private sessionRewoundEnvelope(lineage: RewindLineage, storeKey: string): Event {
    return create(EventSchema, {
      sessionId: storeKey,
      seq: 0n,
      plane: Plane.STREAM,
      class: EventClass.PERSISTENT,
      queryInstanceId: this.queryInstanceId,
      producedAtMs: BigInt(this.now()),
      dedupKey: sessionRewoundDedupKey(lineage.previousVendorSessionId),
      payload: {
        case: "sessionRewound",
        value: create(SessionRewoundSchema, {
          previousVendorSessionId: lineage.previousVendorSessionId,
          newVendorSessionId: storeKey,
          retainedLeafUuid: lineage.retainedLeafUuid,
          reason: {
            case: "keepAliveDiscard",
            value: create(KeepAliveDiscardSchema, { droppedTurnIds: lineage.droppedTurnIds }),
          },
        }),
      },
    });
  }

  /** Persist effective query configuration after the SDK reports system initialization. */
  private async persistRuntimeObserved(
    message: SdkMessageLike,
    vendorSessionId: string,
    eventSessionId = vendorSessionId,
  ): Promise<void> {
    const identity = queryRuntimeIdentity(message as unknown as Record<string, unknown>, {
      vendorSessionId,
      effectiveModel: this.effectiveModel,
      sdkVersion: this.deps.sdkVersion ?? "",
      shimBuildSha: this.deps.shimBuildSha ?? "",
      effectiveQueryOptions: this.deps.effectiveQueryOptions,
      contextPrefix: this.deps.contextPrefix,
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
      queryInstanceId: this.queryInstanceId,
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
      five_hour_resets_at_ms: sample.resetsAtMs,
      five_hour_reset_contract_version: FIVE_HOUR_RESET_WINDOW_CONTRACT_VERSION,
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

    const comparison = startUsage?.resetsAtMs !== null
      && startUsage?.resetsAtMs !== undefined
      && sample.resetsAtMs !== null
      ? compareFiveHourResetWindows(startUsage.resetsAtMs, sample.resetsAtMs)
      : null;
    const sameWindow = comparison?.sameWindow === true;
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
      start_five_hour_resets_at_ms: startUsage?.resetsAtMs ?? null,
      end_five_hour_resets_at_ms: sample.resetsAtMs,
      five_hour_reset_raw_delta_ms: comparison?.rawDeltaMs ?? null,
      five_hour_reset_cycle_displacement: comparison?.canonicalCycleDisplacement ?? null,
      five_hour_reset_residual_jitter_ms: comparison?.residualJitterMs ?? null,
      five_hour_reset_comparison_outcome: comparison === null
        ? "unavailable"
        : comparison.sameWindow ? "same_window" : "different_window",
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
    // THE ONE FUNNEL, so the quiet watchdog's evidence cannot be stamped by
    // some message kinds and missed by others. Every SDK message — a stream
    // delta, an assistant message, a task lifecycle fact, a result — proves the
    // query is alive, and each one refreshes the window before it is routed.
    this.lastSdkActivityMs = this.now();
    if (msg.type === "stream_event") {
      // Observe BEFORE converting: a message_start must make its own id
      // current for the deltas that follow it.
      this.streamMessages.observe(msg, this.deps.sessionId);
      const opts = {
        ...this.convertOpts(),
        messageId: this.streamMessages.current(),
        agentReplSessionId: this.deps.sessionId,
      };
      const persistent = toPersistentEvent(msg, opts);
      if (persistent !== null) {
        await this.confirmVendorIdentity(msg, persistent.sessionId);
        this.store.adoptStoreKey(persistent.sessionId);
        setClaudeSessionId(persistent.sessionId);
        LOGGER.logVerbose({ agent_repl_session_id: this.deps.sessionId, sdk_type: msg.type, payload_case: persistent.payload.case, claude_session_id: persistent.sessionId }, "writing persistent SDK structural event to store");
        // The SDK pump owns this durability boundary. It may not consume the
        // next SDK message until this evidence has a store receipt: doing so
        // would allow the response and its usage to succeed without the timing
        // evidence needed to compute TTFT and generation throughput, or without
        // the final output_tokens the turn's ledger reconciles against.
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
            api_message_id: latency?.uuid ?? this.streamMessages.current(),
            evidence_kind: latency !== undefined ? "message_latency" : "response_usage",
            failed_operation: latency !== undefined ? "store.write.message_latency" : "store.write.response_usage",
            outcome: "fatal_missing_persistent_evidence_receipt",
            cause,
          }, "persistent SDK structural evidence did not receive a durable store receipt");
          throw cause;
        }
        return;
      }
    }
    if (isEphemeral(msg)) {
      const evt = toEphemeralEvent(msg, {
        ...this.convertOpts(),
        messageId: this.streamMessages.current(),
        toolUseId: this.streamMessages.toolUseIdFor(msg, this.deps.sessionId),
        agentReplSessionId: this.deps.sessionId,
      });
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
    let taskNotificationResult = false;
    if (msg.type === "result") {
      const claimedTurnId = this.activeTurnIds[0];
      if (claimedTurnId === undefined) {
        // A LATE TERMINAL IS NOT AN ANOMALY OF UNKNOWN ORIGIN. When this
        // session already synthesized the end for a turn it acked as
        // displaced, a result arriving afterwards is that turn's own overdue
        // terminal. It closes nothing (the turn is closed), it is still stored
        // as vendor evidence by the batch below, and it is named as what it is
        // instead of being reported as an uncorrelated result.
        const lateFor = this.synthesizedTurnEndIds.shift();
        if (lateFor !== undefined) {
          LOGGER.log({
            level: "warn",
            agent_repl_session_id: this.deps.sessionId,
            request_id: lateFor,
            turn_id: lateFor,
            decision: "retain_late_result_after_synthesized_turn_end",
          }, "SDK result arrived after the shim synthesized the interrupted turn's terminal; keeping the vendor evidence without reopening or re-closing the turn");
          this.reportDegraded(
            "claude-shim-turn-lifecycle",
            `SDK result for interrupted turn ${JSON.stringify(lateFor)} arrived after the shim had already synthesized its terminal; the result is kept as vendor evidence and closes no turn`,
            { recovered: true, level: "warn" },
          );
        } else {
          this.reportDegraded(
            "claude-shim-turn-lifecycle",
            "SDK result has no accepted prompt turn to close",
          );
        }
      } else {
        const origin = typeof msg.origin === "object" && msg.origin !== null
          ? msg.origin as Record<string, unknown>
          : undefined;
        taskNotificationResult = origin?.kind === "task_notification" || origin?.kind === "task-notification";
        const taskCount = this.turnSdkTaskIds.get(claimedTurnId)?.size ?? 0;
        // A TURN MAY ONLY BE RETAINED FOR A CYCLE THAT CAN STILL ARRIVE. That
        // is the same rule the pending queue is held to below, and having
        // LAUNCHED an SDK task is not evidence of it: once every task this turn
        // started has terminated and the durable queue holds no
        // `<task-notification>`, there is no internal agent cycle left to
        // produce the task-notification result this clause waits for. Retaining
        // on the bare launch count latched the turn open until the quiet
        // watchdog synthesized its end a full grace period later, which is how a
        // subagent turn whose Task result was consumed inline rendered as
        // `thinking` for ten minutes after the agent had already answered.
        //
        // It is the SAME predicate the quiet watch stands down on
        // (`awaitingOutOfBandTaskWork`), and sharing the one expression is what
        // keeps them from drifting: a state that retains the turn but not the
        // watch is a turn the watchdog closes while it is still working.
        const taskCycleOutstanding = this.awaitingOutOfBandTaskWork();
        const retainForLiveTasks = this.liveSdkTaskIds.size > 0
          || (taskCount > 0 && !taskNotificationResult && taskCycleOutstanding);
        // THE PENDING QUEUE MAY ONLY RETAIN A TURN THIS QUERY CAN STILL ADVANCE.
        // A queued `<task-notification>` drives another internal agent cycle,
        // and that cycle exists only because a background task of THIS query
        // instance produced the notification. So when this query has never seen
        // a single SDK task — none live, none launched by this turn, none
        // completed at all — a pending entry names work no cycle here can
        // consume, and retaining on it would hold the turn open for the whole
        // life of the process.
        //
        // The escape RELEASES, it does not discard: the entries stay queued and
        // are named in the record below, and the daemon is told through a
        // recovered DegradedState so a residue nobody can drain is visible
        // rather than silently absorbed.
        const notificationsCanAdvance = this.liveSdkTaskIds.size > 0
          || taskCount > 0
          || this.completedSdkTaskIds.size > 0;
        const abandonedNotifications = !retainForLiveTasks
          && this.pendingTaskNotificationQueue.size > 0
          && !notificationsCanAdvance;
        retainResultTurn = retainForLiveTasks
          || (this.pendingTaskNotificationQueue.size > 0 && notificationsCanAdvance);
        if (abandonedNotifications) {
          LOGGER.log({
            level: "warn",
            agent_repl_session_id: this.deps.sessionId,
            request_id: claimedTurnId,
            turn_id: claimedTurnId,
            live_sdk_task_count: this.liveSdkTaskIds.size,
            sdk_task_count: taskCount,
            completed_sdk_task_count: this.completedSdkTaskIds.size,
            task_notification_result: taskNotificationResult,
            pending_task_notification_count: this.pendingTaskNotificationQueue.size,
            turns_in_flight: this.activeTurnIds.length,
            decision: "release_turn_over_unresolvable_task_notifications",
          }, "pending task notifications are backed by no SDK task this query instance ever ran; releasing the turn rather than retaining it forever");
          this.reportDegraded(
            "claude-shim-turn-lifecycle",
            `${this.pendingTaskNotificationQueue.size} pending task notification(s) are backed by no SDK task this query instance ran; the turn is released and the notifications are abandoned rather than retaining the turn forever`,
            { recovered: true, level: "warn" },
          );
        }
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
          task_notification_result: taskNotificationResult,
          pending_task_notification_count: this.pendingTaskNotificationQueue.size,
          turns_in_flight: this.activeTurnIds.length,
          decision: "retain_turn_for_sdk_task_cycles",
        }, "SDK result retained while background-task result cycles remain outstanding");
      } else if (claimedTurnId !== undefined) {
        terminalTurnId = this.activeTurnIds.shift();
        if (terminalTurnId !== claimedTurnId) {
          throw new Error(`accepted turn FIFO changed while closing ${JSON.stringify(claimedTurnId)}`);
        }
        // The SDK supplied the terminal an interrupt ack was owed, so the
        // watchdog has nothing left to cover.
        this.disarmDisplacedTurn(terminalTurnId);
        // AND THE QUIET WATCH STANDS DOWN WITH THE LAST OPEN TURN. Leaving it
        // armed over an idle session would burn a wake every grace period to
        // rediscover there is nothing to judge, and a session between turns is
        // SUPPOSED to be silent — which is the one reading the watchdog must
        // never make.
        this.reconcileTurnQuietWatch();
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
    // SPEND the interrupt outcome on the terminal that closes the turn. Only a
    // CLOSING result can be the abort's own terminal: a result retained for
    // background-task cycles leaves the turn open, and the stop it was acked
    // for is still owed a terminal after it.
    const terminalIsInterrupted = terminalTurnId !== undefined
      && this.interruptedTurnIds.delete(terminalTurnId);
    if (terminalIsInterrupted) {
      LOGGER.log({
        agent_repl_session_id: this.deps.sessionId,
        query_instance_id: this.queryInstanceId,
        request_id: terminalTurnId,
        turn_id: terminalTurnId,
        decision: "name_terminal_result_aborted",
        sdk_subtype: typeof msg.subtype === "string" ? msg.subtype : "",
      }, "the SDK terminal closes a turn this session acked as interrupted; the result is named aborted rather than by the SDK's error flavor");
    }
    const { vendor, lifecycle, assistantApiUsage } = convert(msg, {
      sessionSource: this.deps.sessionSource,
      sessionGate: this.sessionGate,
      ...(rootTurnId !== undefined ? { rootTurnId } : {}),
      ...(terminalIsInterrupted ? { interrupted: true } : {}),
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
        queryInstanceId: this.queryInstanceId,
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
    // An end for a task this PROCESS never saw start is a contained accounting
    // gap, never a reason to kill the live query. The task tables are in-memory
    // and per query instance, so `--resume` (and any shim restart) attaches to a
    // vendor conversation whose background tasks were started under the previous
    // shim's lifetime; their terminal fact then arrives with no local start to
    // retire. Throwing here escaped into pump(), which read it as
    // `iterator_throw` and tore down the WHOLE conversation over one unmatched
    // task id.
    //
    // The durable TaskEnded is still FORWARDED, not dropped. The daemon already
    // owns this reconciliation: its SSM appends the entailed TaskStarted before
    // an orphan end (`daemon/internal/ssm/orphan.go`, cause kind
    // `task_reconciled`), and its frontend task catalog creates the entry on
    // demand. Swallowing the end here would instead strand the task as
    // forever-running for every reader.
    const unknownEndedTaskIds = endedTaskIds.filter((taskId) => !liveAfterThisEvent.has(taskId));
    for (const taskId of endedTaskIds) liveAfterThisEvent.delete(taskId);
    if (unknownEndedTaskIds.length > 0) {
      const unknown = [...new Set(unknownEndedTaskIds)].sort();
      LOGGER.log({
        level: "warn",
        agent_repl_session_id: this.deps.sessionId,
        request_id: rootTurnId,
        turn_id: rootTurnId,
        unknown_ended_sdk_task_ids: unknown,
        live_sdk_task_count: this.liveSdkTaskIds.size,
        live_sdk_task_ids: [...this.liveSdkTaskIds].sort(),
        completed_sdk_task_count: this.completedSdkTaskIds.size,
        completed_sdk_task_ids: [...this.completedSdkTaskIds].sort(),
        sdk_type: msg.type,
        sdk_subtype: typeof msg.subtype === "string" ? msg.subtype : "",
        decision: "contain_unknown_task_end",
      }, "SDK emitted TaskEnded for a task this query instance never saw start; containing the accounting gap");
      this.reportDegraded(
        "claude-shim-task-lifecycle",
        `SDK emitted TaskEnded for unknown task(s) ${JSON.stringify(unknown)}; live tasks ${JSON.stringify([...this.liveSdkTaskIds].sort())}`,
        { recovered: true, level: "warn" },
      );
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
      // The live task set just moved, so the window in which silence is legal
      // moved with it: the first task of a turn stands the watch down, and the
      // last one to end hands the turn back to it.
      this.reconcileTurnQuietWatch();
      if (terminalTurnId !== undefined) {
        this.turnSdkTaskIds.delete(terminalTurnId);
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

  /**
   * Give every turn an INTERRUPTED ack claimed as displaced a deadline by which
   * its terminal exists.
   *
   * The deadline is DISARMED, never merely ignored, when the SDK supplies the
   * terminal itself (`disarmDisplacedTurn`), so the ordinary result path stays
   * the one that closes an interrupted turn in every case where it can.
   */
  private armDisplacedTurnTerminals(turnIds: string[]): void {
    const armed: string[] = [];
    for (const turnId of turnIds) {
      // The OUTCOME is recorded for every turn the ack names, even one already
      // carrying a deadline: a second stop on the same turn is still a stop,
      // and the terminal must be named for it.
      this.interruptedTurnIds.add(turnId);
      if (this.displacedTurnDeadlines.has(turnId)) continue;
      const timer = setTimeout(() => {
        this.displacedTurnDeadlines.delete(turnId);
        void this.closeUnterminatedTurn(turnId, this.interruptedTurnEvidence());
      }, this.interruptTerminalGraceMs);
      // The deadline must never be the reason this process stays alive; it is
      // a watchdog over work the process is already doing.
      timer.unref?.();
      this.displacedTurnDeadlines.set(turnId, timer);
      armed.push(turnId);
    }
    if (armed.length === 0) return;
    LOGGER.log({
      agent_repl_session_id: this.deps.sessionId,
      query_instance_id: this.queryInstanceId,
      displaced_turn_ids: armed,
      grace_ms: this.interruptTerminalGraceMs,
      decision: "arm_displaced_turn_terminal_deadline",
    }, "acked an interrupt as INTERRUPTED; the displaced turn owes a terminal within the grace");
  }

  /** Retire a displaced turn's deadline because its terminal has arrived. */
  private disarmDisplacedTurn(turnId: string): void {
    const timer = this.displacedTurnDeadlines.get(turnId);
    if (timer === undefined) return;
    clearTimeout(timer);
    this.displacedTurnDeadlines.delete(turnId);
    LOGGER.log({
      agent_repl_session_id: this.deps.sessionId,
      request_id: turnId,
      turn_id: turnId,
      decision: "displaced_turn_terminal_observed",
    }, "the SDK closed the displaced turn within its grace; nothing is synthesized");
  }

  /**
   * Whether the open turns are waiting on work that reaches this session
   * through NO SDK message of its own.
   *
   * A background SDK task streams into its own agent transcript, not into this
   * session's iterator, and a queued `<task-notification>` is a durable
   * file-plane fact the sidecar owns. So while either is outstanding the parent
   * query is legitimately silent for as long as the task runs — minutes, and
   * routinely longer than any quiet grace worth setting.
   *
   * This is the SAME condition `retainForLiveTasks` keeps the turn open on, and
   * that is the point: the one state that makes a turn stay open is the one
   * state that makes silence legal, so the watchdog and the retention can never
   * disagree about a turn.
   */
  private awaitingOutOfBandTaskWork(): boolean {
    return this.liveSdkTaskIds.size > 0 || this.pendingTaskNotificationQueue.size > 0;
  }

  /**
   * PUT THE QUIET WATCH IN THE STATE THE SESSION IS ACTUALLY IN.
   *
   * The watch is armed only while a turn is open AND nothing out-of-band is
   * owed to it, so the watchdog's premise — silence proves the query is not
   * running — is true by construction whenever it can fire. Silence during
   * background-task work is not evidence of anything, and judging it closed
   * healthy turns a full grace after their last SDK message.
   *
   * It is a LATCH, not a longer grace: raising the grace only moves the same
   * wrong verdict later, whereas standing the watch down for exactly the window
   * in which silence is expected makes the wrong verdict unreachable. Every
   * mutation of the three inputs — the open-turn queue, the live task set, the
   * notification queue — calls this, so no state change can leave the watch
   * out of step with the session.
   */
  private reconcileTurnQuietWatch(): void {
    if (this.activeTurnIds.length === 0) {
      this.disarmTurnQuietWatchdog("no_turn_open");
      return;
    }
    if (this.awaitingOutOfBandTaskWork()) {
      this.disarmTurnQuietWatchdog("awaiting_out_of_band_task_work");
      return;
    }
    this.armTurnQuietWatchdog();
  }

  /**
   * ARM THE QUIET WATCHDOG over whatever turns are open.
   *
   * It re-arms itself rather than firing on a fixed schedule, so a turn that
   * keeps producing output keeps pushing its own deadline out: each wake
   * computes how long the stream has actually been silent and, when that is
   * short of the grace, sleeps only for the remainder. A session with nothing
   * open holds no timer at all.
   *
   * Arming is IDEMPOTENT. One timer covers every open turn because the evidence
   * is one stream, so a second prompt admitted mid-turn joins the watch rather
   * than starting a rival one.
   */
  private armTurnQuietWatchdog(): void {
    if (this.turnQuietTimer !== null) return;
    if (this.turnQuietGraceMs <= 0) return;
    this.scheduleTurnQuietCheck(this.turnQuietGraceMs);
    LOGGER.log({
      agent_repl_session_id: this.deps.sessionId,
      query_instance_id: this.queryInstanceId,
      active_turn_ids: [...this.activeTurnIds],
      quiet_grace_ms: this.turnQuietGraceMs,
      decision: "arm_turn_quiet_watchdog",
    }, "a turn is open; the SDK owes it activity within the quiet grace");
  }

  /** Sleep `delayMs` and then take the quiet verdict. */
  private scheduleTurnQuietCheck(delayMs: number): void {
    const timer = setTimeout(() => {
      this.turnQuietTimer = null;
      void this.checkTurnQuiet();
    }, delayMs);
    // The watch must never be the reason this process stays alive; it is a
    // watchdog over work the process is already doing.
    timer.unref?.();
    this.turnQuietTimer = timer;
  }

  /**
   * Stand the quiet watchdog down, naming what made silence legal.
   *
   * The reason goes to the canonical log on the TRANSITION only, so a reader
   * who finds a turn open for an hour with no synthesized end can see the one
   * fact that explains it rather than inferring the watch was never armed.
   */
  private disarmTurnQuietWatchdog(reason: string): void {
    if (this.turnQuietTimer === null) return;
    clearTimeout(this.turnQuietTimer);
    this.turnQuietTimer = null;
    LOGGER.log({
      agent_repl_session_id: this.deps.sessionId,
      query_instance_id: this.queryInstanceId,
      active_turn_ids: [...this.activeTurnIds],
      live_sdk_task_count: this.liveSdkTaskIds.size,
      live_sdk_task_ids: [...this.liveSdkTaskIds].sort(),
      pending_task_notification_count: this.pendingTaskNotificationQueue.size,
      disarm_reason: reason,
      decision: "disarm_turn_quiet_watchdog",
    }, "the quiet watch stands down; silence is no longer evidence that the query stopped");
  }

  /**
   * Decide whether the open turns' query is still alive, and close the oldest
   * turn when it provably is not.
   *
   * THE CLOSE IS ONE TURN PER WAKE, deliberately. Closing the whole queue at
   * once would spend the same single piece of evidence — one silent stream — on
   * several independent claims; taking the oldest and re-arming lets the very
   * next wake reconsider, and a genuinely revived stream stops the rest.
   */
  private async checkTurnQuiet(): Promise<void> {
    const turnId = this.activeTurnIds[0];
    if (turnId === undefined) return;
    const quietMs = this.now() - this.lastSdkActivityMs;
    if (quietMs < this.turnQuietGraceMs) {
      // The stream spoke since the timer was set, so the deadline it was armed
      // against has moved. Sleep out the remainder rather than judging on a
      // window the SDK already refreshed.
      this.scheduleTurnQuietCheck(this.turnQuietGraceMs - quietMs);
      return;
    }
    await this.closeUnterminatedTurn(turnId, {
      decision: "synthesize_quiet_turn_end",
      logMessage: "a turn has been open with NO SDK activity for the whole quiet grace while nothing out-of-band was owed to it; the query behind it is not running, so the shim writes the end itself rather than leaving the turn latched open",
      degradedReason: (id) => `turn ${JSON.stringify(id)} has been open with no SDK activity for ${quietMs}ms (grace ${this.turnQuietGraceMs}ms) while no background task was running and no task notification was queued for it; nothing could still produce its terminal, so the shim closed the turn itself`,
      graceMs: this.turnQuietGraceMs,
      quietMs,
    });
    // The close already reconciled the watch against the open-turn queue, so
    // the remaining turns are covered without re-arming over one that is
    // waiting on background work.
  }

  /** The evidence an interrupt-grace expiry closes a turn on. */
  private interruptedTurnEvidence(): UnterminatedTurnEvidence {
    return {
      decision: "synthesize_interrupted_turn_end",
      logMessage: "the SDK never produced a terminal for a turn acked as interrupted; the shim writes the end itself rather than leaving the turn latched open",
      degradedReason: (id) => `turn ${JSON.stringify(id)} was acknowledged as interrupted but the SDK produced no terminal result within ${this.interruptTerminalGraceMs}ms; the shim closed the turn itself so it cannot stay open forever`,
      graceMs: this.interruptTerminalGraceMs,
    };
  }

  /**
   * Write the terminal a turn never received from the SDK.
   *
   * It converges on the SAME shape the ordinary result path produces — the
   * ordered turn-end boundary (`AccountUsageObservation` then `TurnEnded`) in
   * the turn's own store-key space, tracked through `pendingTurnEndIds` so the
   * handshake keeps claiming the turn until the end is durably observable. What
   * it does NOT do is invent a vendor `ResultMessage`: the SDK produced no
   * result, and manufacturing one would put evidence on the wire that never
   * existed. The end is a lifecycle fact this session owns; the result is not.
   *
   * It is LOUD on every arm. A synthesized terminal means the SDK broke a
   * contract this session relied on, so it goes out as a warn line AND a
   * recovered DegradedState naming the turn, rather than quietly papering the
   * gap over. `evidence` is what distinguishes the two arms — an interrupt this
   * session acked, or a stream that went silent — so a reader never has to
   * guess which observation produced the close.
   */
  private async closeUnterminatedTurn(turnId: string, evidence: UnterminatedTurnEvidence): Promise<void> {
    const index = this.activeTurnIds.indexOf(turnId);
    if (index < 0) {
      // Belt to the disarm's braces: the turn already reached its terminal.
      LOGGER.log({
        agent_repl_session_id: this.deps.sessionId,
        request_id: turnId,
        turn_id: turnId,
        decision: "displaced_turn_terminal_observed",
      }, "an unterminated-turn deadline fired on a turn that is no longer in flight; nothing is synthesized");
      // Nothing changed, but the wake that brought us here consumed the timer,
      // so whatever is still open has to be handed back to the watch.
      this.reconcileTurnQuietWatch();
      return;
    }
    this.activeTurnIds.splice(index, 1);
    // This IS the turn's terminal, so the interrupt outcome is spent here too:
    // the synthesized TurnEnded already names `interrupted`, and a late SDK
    // result arriving afterwards closes nothing and must not be renamed.
    this.interruptedTurnIds.delete(turnId);
    // The open-turn queue moved, so the watch is reconciled against it here
    // rather than by each caller: an interrupt deadline and a quiet verdict
    // both close turns, and only one of them used to stand the watch down.
    this.reconcileTurnQuietWatch();
    this.synthesizedTurnEndIds.push(turnId);
    const boundaryAtMs = this.now();
    const turnStart = this.activeTurnStarts.get(turnId);
    // The turn's OWN store-key space: a rotation between the prompt and now
    // moved the retained carrier, and the end belongs where the start is.
    const storeKey = turnStart?.sessionId ?? this.store.storeSessionId();
    LOGGER.log({
      level: "warn",
      agent_repl_session_id: this.deps.sessionId,
      query_instance_id: this.queryInstanceId,
      request_id: turnId,
      turn_id: turnId,
      grace_ms: evidence.graceMs,
      quiet_ms: evidence.quietMs,
      live_sdk_task_count: this.liveSdkTaskIds.size,
      pending_task_notification_count: this.pendingTaskNotificationQueue.size,
      turns_in_flight: this.activeTurnIds.length,
      store_key: storeKey,
      decision: evidence.decision,
    }, evidence.logMessage);
    this.reportDegraded(
      "claude-shim-turn-lifecycle",
      evidence.degradedReason(turnId),
      { recovered: true, level: "warn" },
    );
    const usage = await this.captureFiveHourUsage("turn_end", turnId, this.turnStartUsage.get(turnId));
    this.turnStartUsage.delete(turnId);
    const usageEvent = this.accountUsageObservationEvent("turn_end", turnId, boundaryAtMs, usage, storeKey);
    const durationMs = turnStart === undefined
      ? 0n
      : BigInt(Math.max(0, boundaryAtMs - Number(turnStart.producedAtMs)));
    const turnEnded = create(EventSchema, {
      sessionId: storeKey,
      seq: 0n,
      plane: Plane.STREAM,
      class: EventClass.PERSISTENT,
      requestId: turnId,
      queryInstanceId: this.queryInstanceId,
      producedAtMs: BigInt(boundaryAtMs),
      payload: {
        case: "turnEnded",
        value: create(TurnEndedSchema, {
          stopReason: "interrupted",
          durationMs,
          isError: false,
          turnId,
        }),
      },
    });
    this.pendingTurnEndIds.push(turnId);
    this.activeTurnStarts.delete(turnId);
    this.turnSdkTaskIds.delete(turnId);
    try {
      const ack = await this.store.write([usageEvent, turnEnded]);
      const pendingIndex = this.pendingTurnEndIds.indexOf(turnId);
      if (pendingIndex >= 0) this.pendingTurnEndIds.splice(pendingIndex, 1);
      LOGGER.log({
        agent_repl_session_id: this.deps.sessionId,
        request_id: turnId,
        turn_id: turnId,
        store_last_seq: ack.lastSeq,
        pending_turn_end_ids: this.pendingTurnEndIds,
        decision: "retire_durable_turn_end_claim",
      }, "synthesized TurnEnded is durably observable; retired its handshake claim");
    } catch (cause) {
      // A throw out of a timer callback would become an unhandled rejection
      // and tell nobody, so the failure is surfaced on the channel a daemon
      // actually reads. The claim stays in pendingTurnEndIds: the end is NOT
      // durable, and the handshake must keep saying so.
      LOGGER.log({
        level: "error",
        operation: "shim.uds-session.persistent-evidence",
        agent_repl_session_id: this.deps.sessionId,
        claude_session_id: storeKey,
        query_instance_id: this.queryInstanceId,
        request_id: turnId,
        turn_id: turnId,
        evidence_kind: "synthesized_terminal_turn_batch",
        failed_operation: "store.write.synthesized_terminal_turn_batch",
        outcome: "fatal_missing_persistent_evidence_receipt",
        synthesis_decision: evidence.decision,
        cause,
      }, "synthesized unterminated-turn terminal did not receive a durable store receipt");
      this.reportDegraded(
        "claude-shim-turn-lifecycle",
        `the synthesized terminal for unterminated turn ${JSON.stringify(turnId)} was not durably stored: ${errMsg(cause)}`,
      );
    }
  }

  /** Build the lifecycle fact placed first in an acknowledged turn-start batch. */
  private turnStartedEvent(
    requestId: string,
    text: string,
    boundaryAtMs: number,
    promptOrigin: PromptOrigin,
  ): Event {
    return create(EventSchema, {
      sessionId: this.store.storeSessionId(),
      seq: 0n,
      plane: Plane.STREAM,
      class: EventClass.PERSISTENT,
      requestId,
      queryInstanceId: this.queryInstanceId,
      producedAtMs: BigInt(boundaryAtMs),
      payload: {
        case: "turnStarted",
        value: create(TurnStartedSchema, {
          promptPreview: promptPreview(text),
          turnId: requestId,
          promptOrigin,
        }),
      },
    });
  }

  /**
   * The options every converter call shares, including the query this session
   * is running.
   *
   * Threading it HERE is what makes the converters part of the PRODUCER rather
   * than relays: there is exactly one funnel, so no conversion path can build
   * an Event that forgot to say which query it belongs to, and nothing
   * downstream ever has to infer it from how the event was delivered. See the
   * `query_instance_id` contract in core.proto.
   */
  private convertOpts(): { nowMs?: number; queryInstanceId: string } {
    return {
      ...(this.deps.nowMs !== undefined ? { nowMs: this.deps.nowMs() } : {}),
      queryInstanceId: this.queryInstanceId,
    };
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
   *
   * `recovered: true` marks a CONTAINED degradation: the shim absorbed an
   * anomaly and the session keeps serving, so the daemon can surface the notice
   * without treating the session as ongoing downtime. Default is uncontained.
   */
  private reportDegraded(
    component: string,
    reason: string,
    opts: { recovered?: boolean; level?: "warn" | "error" } = {},
  ): void {
    LOGGER.log({ level: opts.level ?? "error", agent_repl_session_id: this.deps.sessionId }, reason);
    this.emitDegraded(create(DegradedStateSchema, {
      component,
      reason,
      droppedCount: 0n,
      recovered: opts.recovered ?? false,
    }));
  }

  /**
   * Record a SESSION-OWNED degraded report against its component and send it.
   *
   * Recording is what makes the report survivable across a daemon bounce: the
   * event itself is ephemeral, so the state it describes has to live here to be
   * re-announceable. An UNCONTAINED report (`recovered: false`) opens the
   * component's window; the `recovered: true` edge closes it, exactly as the
   * daemon closes its fault row, so a recovery is never re-announced as a
   * standing fault and a component that recovered stops being re-reported.
   *
   * The store client is deliberately NOT routed through here: it keeps its own
   * current window (openDegradedReport) and is the authority on it, and two
   * ledgers for one component could disagree.
   */
  private emitDegraded(report: DegradedState): void {
    if (report.recovered) {
      this.openDegradedWindows.delete(report.component);
    } else {
      this.openDegradedWindows.set(report.component, report);
    }
    this.server.sendEvent(this.degradedEvent(report));
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

  /**
   * Whether a merged store event belongs to THIS query invocation's own epoch,
   * rather than to the conversation history that preceded it.
   *
   * THE BOUNDARY IS ALREADY WRITTEN DOWN. `persistQueryCreated` records the
   * store coordinate of the one QueryCreated this shim owns, and the protocol
   * already treats it as the epoch line (`queryCreatedSeq` on ShimHello). This
   * reuses that same coordinate rather than inventing a second notion of "new".
   *
   *   - No position yet means the QueryCreated has not been acknowledged, so
   *     nothing observed can be after it. FAIL CLOSED — pre-epoch.
   *   - An event in the boundary's OWN seq space compares by seq.
   *   - An event in the CURRENT store key when that key is not the boundary's
   *     is in a space minted by a rotation that happened AFTER the boundary,
   *     so all of it is in-epoch.
   *   - Anything else is a retired seq space: history by construction.
   */
  private isWithinQueryEpoch(evt: Event): boolean {
    const position = this.queryCreatedPosition;
    if (position === null) return false;
    if (evt.sessionId === position.storeKey) return evt.seq > position.seq;
    return evt.sessionId === this.store.storeSessionId();
  }

  /**
   * Track the durable file-plane queue that feeds task-notification cycles.
   *
   * ONLY THIS EPOCH'S QUEUE OPERATIONS COUNT, and that restriction is the whole
   * point. The merged feed replays the vendor transcript's WHOLE history on
   * every bring-up, so a revived conversation re-delivers every
   * `<task-notification>` enqueue it ever made — including entries whose
   * matching dequeue was consumed by a process epoch that no longer exists.
   * Those entries are unresolvable by construction: no live SDK task backs
   * them and no cycle will ever drain them. Folded into the pending queue they
   * became a permanent retention against the NEXT result
   * (`retain_turn_for_sdk_task_cycles`), which is how a compact-first
   * revival's `/compact` turn ended with its result stored and its turn never
   * closed — the claim stayed open, the workspace stayed red, and the restart
   * guard refused the workspace for as long as the daemon ran.
   */
  private observeTaskNotificationQueue(evt: Event): void {
    if (evt.payload.case !== "vendor" || !envelopeIs(evt.payload.value, TranscriptLineSchema)) return;
    const line = unpackAs(evt.payload.value, TranscriptLineSchema);
    if (line?.line.case !== "queueOperation") return;
    const queue = line.line.value;
    if (!queue.content.startsWith("<task-notification>")) return;
    if (!this.isWithinQueryEpoch(evt)) {
      LOGGER.logVerbose({
        agent_repl_session_id: this.deps.sessionId,
        queue_operation: QueueOp[queue.operation],
        seq: evt.seq,
        store_key: evt.sessionId,
        query_created_seq: this.queryCreatedPosition?.seq ?? null,
        pending_task_notification_count: this.pendingTaskNotificationQueue.size,
        decision: "ignore_pre_epoch_task_notification_queue_transition",
      }, "task-notification queue transition predates this query invocation; it belongs to a retired process epoch and can drive no result cycle here");
      return;
    }
    if (queue.operation === QueueOp.ENQUEUE) {
      this.pendingTaskNotificationQueue.add(queue.content);
    } else if (queue.operation === QueueOp.DEQUEUE || queue.operation === QueueOp.REMOVE) {
      this.pendingTaskNotificationQueue.delete(queue.content);
    } else {
      return;
    }
    LOGGER.logVerbose({
      agent_repl_session_id: this.deps.sessionId,
      queue_operation: QueueOp[queue.operation],
      pending_task_notification_count: this.pendingTaskNotificationQueue.size,
      decision: "update_task_notification_queue",
    }, "observed durable task-notification queue transition");
    // A queued notification is an internal agent cycle this session has not
    // been given yet, so it buys the open turn the same silence a live task
    // does — and draining the queue takes that silence back.
    this.reconcileTurnQuietWatch();
  }

  /** Wrap a DegradedState as a SYNTHETIC/EPHEMERAL Event for the daemon. */
  private degradedEvent(report: DegradedState): Event {
    return create(EventSchema, {
      sessionId: this.deps.sessionId,
      seq: 0n,
      plane: Plane.SYNTHETIC,
      class: EventClass.EPHEMERAL,
      queryInstanceId: this.queryInstanceId,
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
