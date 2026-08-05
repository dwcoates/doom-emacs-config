/**
 * agentshim.frontend.v1 — hand-typed protojson frame types + a strict, loud
 * decoder for the daemon's resolved frontend surface (§5.4, §11).
 *
 * The daemon pushes canonical proto3-JSON (protojson) `FrontendFrame`s over
 * the WebSocket — the SAME message set Emacs consumes, so the two frontends
 * can never diverge.
 *
 * S9 RECOMPOSITION — the two breaking changes this decoder now speaks:
 * - `ConversationDelta.items` is a repeated typed `ConversationItem`: a thin
 *   envelope {uuid, tsMs, requestId} carrying EXACTLY ONE typed data.v1/core.v1
 *   payload arm (assistantMessage, userMessage, toolUse, toolResult,
 *   toolUseResult, result, contextCleared, contextCompacted, sessionCommand,
 *   permission, systemFailure). The webapp DECOMPOSES those typed payloads back into its
 *   render vocabulary in `state-adapter.ts`; the OLD `kind`-discriminated
 *   pre-rendered Struct vocabulary is gone.
 * - `TypingDelta` embeds a `core.v1.ContentDelta` under `delta`
 *   ({uuid, blockIndex, one of text/thinking/inputJson/signature,
 *   estimatedTokens}) rather than the old flat uuid/blockIndex/kind/delta.
 * Additive S9: the `sessionInit` frame + `StateSnapshot.inits` carry the
 * retained `data.v1.SystemInit`; `SessionView.configDir`.
 *
 * GENERATED CONTRACT + STRICT ADAPTER:
 * The committed `proto/gen/ts` protobuf-es stubs provide compile-time field
 * manifests. The webapp-local protobuf runtime dependency and scoped TypeScript
 * aliases make those stubs resolvable even though their source directory has no
 * adjacent `node_modules`. Response token accounting is parsed by the generated
 * protobuf decoder before its generated typed values are projected into render
 * DTOs. The projection is exhaustively typed against every generated field, so
 * scalar, presence, oneof, and additive-field changes fail at the shared schema
 * boundary. Other frontend messages use exact generated field manifests around
 * their stricter render-specific validation.
 *
 * VALIDATION CONTRACT (§5.1) — the decoder hard-errors (never returns a
 * degraded value) on:
 * - input that is not valid JSON / not a JSON object;
 * - an unrecognized field (top-level or on a `frontend.v1`-owned message) —
 *   the protojson analogue of a new/unknown field, surfaced loudly;
 * - an empty or unrecognized `FrontendFrame` oneof variant;
 * - an empty, multiple, or unrecognized `ConversationItem`/`ContentDelta`
 *   oneof arm;
 * - an unknown enum name/value; a scalar of the wrong JSON type; a recognized
 *   variant missing a load-bearing field.
 * BOUNDARY: the DEEP interior of a `ConversationItem`'s typed data.v1/core.v1
 * payload (and of `SessionInit`) is ADOPTED BY SHAPE — those messages grow
 * additively on the daemon side, so re-litigating every nested field would
 * break the webapp on every additive daemon change. The state-adapter reads
 * the load-bearing fields of each payload loudly; validating the rest is the
 * daemon-side converter's job (§5.1).
 * Field names are canonical protojson (lowerCamelCase); int64/uint64 scalars
 * arrive as JSON strings and are parsed to `number`.
 */

import {
  MissingUsageBoundarySchema,
  ModelTokenUtilizationSchema,
  RuntimeIdentityIncompleteSchema,
  QueryTerminationFailureSchema,
  SessionResumeFailureAutomaticRestoreSchema,
  SessionResumeFailureBringUpFailureSchema,
  SessionResumeFailureCreateSchema,
  SessionResumeFailureIdentityMismatchSchema,
  SessionResumeFailureSchema,
  SessionResumeFailureTranscriptUnavailableSchema,
  SystemFailureItemSchema,
  TelemetryRecordMissingPersistenceReceiptSchema,
  TelemetryRecordMissingQueryLifecycleSchema,
  TelemetryRecordMissingResponseUsageSchema,
  TelemetryRecordMissingSchema,
  TokenCacheCreationSchema,
  TokenCacheRatesSchema,
  TokenLedgerMismatchSchema,
  TokenOutputDetailsSchema,
  TokenServerToolUseSchema,
  TokenTimingTotalsSchema,
  TokenUsageReconciliationSchema,
  TokenUsageTotalsSchema,
  TokenUtilizationSchema,
  SessionTokenUtilizationSchema,
  TurnAccountingProblemSchema,
  TurnAccountingSchema,
  TurnAccountingTimingSchema,
  TurnAccountingInvalidSchema,
  UnmodeledUsageFieldsSchema,
  UsageWindowResetSchema,
  type TokenCacheCreation as GeneratedTokenCacheCreation,
  type TokenCacheDiagnostic as GeneratedTokenCacheDiagnostic,
  type TokenCacheRates as GeneratedTokenCacheRates,
  type TokenOutputDetails as GeneratedTokenOutputDetails,
  type TokenServerToolUse as GeneratedTokenServerToolUse,
  type TokenUsage as GeneratedTokenUsage,
  type TokenUsageIteration as GeneratedTokenUsageIteration,
  type TokenUtilization as GeneratedTokenUtilization,
  type TurnAccounting as GeneratedTurnAccounting,
  type TurnAccountingProblem as GeneratedTurnAccountingProblem,
  type TurnAccountingTiming as GeneratedTurnAccountingTiming,
  type TokenUsageReconciliation as GeneratedTokenUsageReconciliation,
  type TokenUsageTotals as GeneratedTokenUsageTotals,
  type ModelTokenUtilization as GeneratedModelTokenUtilization,
  type QueryTerminationFailure as GeneratedQueryTerminationFailure,
  type SessionTokenUtilization as GeneratedSessionTokenUtilization,
} from "../../proto/gen/ts/agentshim/frontend/v1/frontend_pb";
import { fromJson, toJson, type JsonValue } from "@bufbuild/protobuf";
import {
  HIBERNATION_CAUSE,
  KEEP_ALIVE_HOLD_TURN_ID,
  PROMPT_ORIGIN_CACHE_KEEP_ALIVE,
  PROMPT_ORIGIN_PREFIX,
  PROMPT_ORIGIN_UNSPECIFIED,
  QUEUE_ENTRY_KEEP_ALIVE_HOLD,
  QUEUE_ENTRY_REVIVAL_HOLD,
  REVIVAL_HOLD_SESSION_ID,
} from "./proto-names.js";
import { ApiUsageSchema } from "../../proto/gen/ts/agentshim/data/v1/tools_pb";
import {
  AccountUsageAvailableSchema,
  AccountUsageObservationSchema,
  AccountUsageUnavailableSchema,
  EvidenceFingerprintSchema,
  FingerprintUnavailableSchema,
  QueryRuntimeIdentitySchema,
  UsageSamplingFailureSchema,
  UsageWindowSchema,
  type AccountUsageObservation as GeneratedAccountUsageObservation,
  type EvidenceFingerprint as GeneratedEvidenceFingerprint,
  type QueryRuntimeIdentity as GeneratedQueryRuntimeIdentity,
} from "../../proto/gen/ts/agentshim/core/v1/core_pb";

/** Exact generated field lists: omission and invention are both type errors. */
function generatedFieldSet<Fields extends string>() {
  return <const Keys extends readonly Fields[]>(
    ...keys: Keys & (Exclude<Fields, Keys[number]> extends never ? unknown : ["missing generated field", Exclude<Fields, Keys[number]>])
  ): ReadonlySet<string> => new Set<string>(keys);
}

// --- enums ------------------------------------------------------------------

/** The closed render-state vocabulary (SSM-resolved). Mirrors frontend.proto. */
export enum RenderState {
  UNSPECIFIED = 0,
  INIT = 1,
  IDLE = 2,
  IDLE_ASYNC = 3,
  THINKING = 4,
  PERMISSION = 5,
  DONE = 6,
  STOP_FAILED = 7,
  MERGING = 8,
  MERGE_QUEUED = 9,
  MERGE_CONFLICT = 10,
  MERGE_FAILED = 11,
  MERGED = 12,
  DEAD = 13,
  DEGRADED = 14,
  READY = 15,
  VENDOR_BLOCKED = 16,
  INTERRUPTED = 17,
  CLEARING = 18,
  COMPACTING = 19,
  /* Field number 20, held while this was named DORMANT. The rename is
     wire-compatible on purpose: an append-only state log written by an older
     daemon still carries the literal text `dormant`, which the SSM resolves
     onto this state forever. */
  SEVERED = 20,
  HIBERNATED = 21,
  SUBMITTING = 22,
  /* The first instant of a merge attempt, before anything durable exists
     for it. Transient by construction: superseded by MERGE_QUEUED or MERGING
     within milliseconds, or by MERGE_FAILED when the enqueue is refused. */
  MERGE_ENQUEUING = 23,
}

/**
 * `frontend.v1.ConversationSource` — WHO drove the turn that produced an item.
 *
 * The merge coordinator borrows a workspace's own shim to resolve a conflict,
 * so a session can emit a full turn the user never prompted. The provenance is
 * a durable FACT on every item, not a rendering hint.
 *
 * UNSPECIFIED is never emitted by the daemon: proto3 reserves 0 for "not
 * populated", and every item the daemon builds sets one of the other arms. The
 * decoder therefore ADOPTS it rather than throwing — the loud rejection belongs
 * to the layer that owns conversation items (`state-adapter.ts`), which logs it
 * as an error and drops the item rather than defaulting it to USER.
 */
export enum ConversationSource {
  UNSPECIFIED = 0,
  USER = 1,
  MERGE = 2,
}

const CONVERSATION_SOURCE_BY_NAME: Readonly<Record<string, ConversationSource>> = {
  CONVERSATION_SOURCE_UNSPECIFIED: ConversationSource.UNSPECIFIED,
  CONVERSATION_SOURCE_USER: ConversationSource.USER,
  CONVERSATION_SOURCE_MERGE: ConversationSource.MERGE,
};

const RENDER_STATE_BY_NAME: Readonly<Record<string, RenderState>> = {
  RENDER_STATE_UNSPECIFIED: RenderState.UNSPECIFIED,
  RENDER_STATE_INIT: RenderState.INIT,
  RENDER_STATE_IDLE: RenderState.IDLE,
  RENDER_STATE_IDLE_ASYNC: RenderState.IDLE_ASYNC,
  RENDER_STATE_READY: RenderState.READY,
  RENDER_STATE_VENDOR_BLOCKED: RenderState.VENDOR_BLOCKED,
  RENDER_STATE_INTERRUPTED: RenderState.INTERRUPTED,
  RENDER_STATE_CLEARING: RenderState.CLEARING,
  RENDER_STATE_COMPACTING: RenderState.COMPACTING,
  RENDER_STATE_SEVERED: RenderState.SEVERED,
  RENDER_STATE_HIBERNATED: RenderState.HIBERNATED,
  RENDER_STATE_SUBMITTING: RenderState.SUBMITTING,
  RENDER_STATE_THINKING: RenderState.THINKING,
  RENDER_STATE_PERMISSION: RenderState.PERMISSION,
  RENDER_STATE_DONE: RenderState.DONE,
  RENDER_STATE_STOP_FAILED: RenderState.STOP_FAILED,
  RENDER_STATE_MERGE_ENQUEUING: RenderState.MERGE_ENQUEUING,
  RENDER_STATE_MERGING: RenderState.MERGING,
  RENDER_STATE_MERGE_QUEUED: RenderState.MERGE_QUEUED,
  RENDER_STATE_MERGE_CONFLICT: RenderState.MERGE_CONFLICT,
  RENDER_STATE_MERGE_FAILED: RenderState.MERGE_FAILED,
  RENDER_STATE_MERGED: RenderState.MERGED,
  RENDER_STATE_DEAD: RenderState.DEAD,
  RENDER_STATE_DEGRADED: RenderState.DEGRADED,
};

/**
 * `frontend.v1.RosterRow.status` — the closed lifecycle vocabulary a sidebar
 * roster row may carry, as the ARM NAMES of the row's `status` oneof.
 *
 * WHICH arm is set IS the status. There is no enum here on purpose: a proto3
 * enum has a zero value that means both "unset" and "a legal member", and the
 * roster has no defensible default lifecycle. With a oneof, an unset status is
 * simply the absence of an arm, which the decoder refuses outright.
 *
 * These are the protojson spellings (lowerCamelCase), which is what the daemon
 * puts on the wire — `idle_async` in the .proto arrives here as `idleAsync`.
 *
 * NOT `RenderState`: the roster carries statuses no render state produces
 * (`inactive`, `none`), and coarsens idle and ready onto one dot.
 */
export type RosterRowStatusCase =
  | "submitting"
  | "thinking"
  | "clearing"
  | "compacting"
  | "permission"
  | "done"
  | "interrupted"
  | "ready"
  | "idleAsync"
  | "vendorBlocked"
  | "init"
  | "severed"
  | "hibernated"
  | "startFailed"
  | "degraded"
  | "dead"
  | "mergeEnqueuing"
  | "merging"
  | "mergeQueued"
  | "mergeConflict"
  | "mergeFailed"
  | "merged"
  | "none"
  | "inactive";

/**
 * The status arm name → the sidebar's CSS/wire spelling (`WorkspaceStatus` in
 * `sidebar.ts`). The oneof arms are the wire vocabulary; this is the
 * presentation spelling of the SAME closed set, which is what makes the
 * cross-language completeness test able to compare them arm for arm.
 *
 * Being a total `Record` over `RosterRowStatusCase` is load-bearing: a new arm
 * added to the union without a keyword here is a compile error, not a status
 * that silently renders as `undefined`.
 */
export const ROSTER_ROW_STATUS_KEYWORD: Readonly<Record<RosterRowStatusCase, string>> = {
  submitting: "submitting",
  thinking: "thinking",
  clearing: "clearing",
  compacting: "compacting",
  permission: "permission",
  done: "done",
  interrupted: "interrupted",
  ready: "ready",
  idleAsync: "idle-async",
  vendorBlocked: "vendor-blocked",
  init: "init",
  severed: "severed",
  hibernated: "hibernated",
  startFailed: "start-failed",
  degraded: "degraded",
  dead: "dead",
  mergeEnqueuing: "merge-enqueuing",
  merging: "merging",
  mergeQueued: "merge-queued",
  mergeConflict: "merge-conflict",
  mergeFailed: "merge-failed",
  merged: "merged",
  none: "none",
  inactive: "inactive",
};

/**
 * Every status arm name, derived from the keyword table so the two can never
 * disagree about the vocabulary's size.
 */
export const ROSTER_ROW_STATUS_CASES: readonly RosterRowStatusCase[] = Object.keys(
  ROSTER_ROW_STATUS_KEYWORD,
) as RosterRowStatusCase[];

const ROSTER_ROW_STATUS_CASE_SET: ReadonlySet<string> = new Set(ROSTER_ROW_STATUS_CASES);

/** Daemon-resolved reliability of the current session-controller generation. */
export enum SessionConnectivity {
  UNSPECIFIED = 0,
  HIBERNATED = 1,
  CONNECTING = 2,
  OPERATIONAL = 3,
  DEGRADED = 4,
  UNAVAILABLE = 5,
}

const SESSION_CONNECTIVITY_BY_NAME: Readonly<Record<string, SessionConnectivity>> = {
  SESSION_CONNECTIVITY_UNSPECIFIED: SessionConnectivity.UNSPECIFIED,
  SESSION_CONNECTIVITY_HIBERNATED: SessionConnectivity.HIBERNATED,
  SESSION_CONNECTIVITY_CONNECTING: SessionConnectivity.CONNECTING,
  SESSION_CONNECTIVITY_OPERATIONAL: SessionConnectivity.OPERATIONAL,
  SESSION_CONNECTIVITY_DEGRADED: SessionConnectivity.DEGRADED,
  SESSION_CONNECTIVITY_UNAVAILABLE: SessionConnectivity.UNAVAILABLE,
};

/** Daemon-resolved activity of the session, independent of connectivity. */
export enum SessionStatus {
  UNSPECIFIED = 0,
  READY = 1,
  THINKING = 2,
  PERMISSION = 3,
  DONE = 4,
  INTERRUPTED = 5,
  VENDOR_BLOCKED = 6,
  MONITORING = 7,
  SUBMITTING = 8,
}

const SESSION_STATUS_BY_NAME: Readonly<Record<string, SessionStatus>> = {
  SESSION_STATUS_UNSPECIFIED: SessionStatus.UNSPECIFIED,
  SESSION_STATUS_READY: SessionStatus.READY,
  SESSION_STATUS_SUBMITTING: SessionStatus.SUBMITTING,
  SESSION_STATUS_THINKING: SessionStatus.THINKING,
  SESSION_STATUS_PERMISSION: SessionStatus.PERMISSION,
  SESSION_STATUS_DONE: SessionStatus.DONE,
  SESSION_STATUS_INTERRUPTED: SessionStatus.INTERRUPTED,
  SESSION_STATUS_VENDOR_BLOCKED: SessionStatus.VENDOR_BLOCKED,
  SESSION_STATUS_MONITORING: SessionStatus.MONITORING,
};

// --- message types ----------------------------------------------------------

/** A protojson google.protobuf.Struct value (a free-form JSON object). */
export type JsonObject = Record<string, unknown>;

export interface RuntimeFault {
  component: string;
  faultType: string;
  impact: string;
  causeKind: string;
  openedAtMs: number;
}

export interface WorkspaceState {
  workspace: string;
  sessionId: string;
  state: RenderState;
  turnActive: boolean;
  liveTaskCount: number;
  causeKind: string;
  causeSeq: number;
  atMs: number;
  connectivity: SessionConnectivity;
  status: SessionStatus;
  controllerGenerationId: string;
  activeFaults: RuntimeFault[];
  /**
   * Whether the merge coordinator holds the exclusivity lease on this
   * workspace's shim. While held the merge OWNS the session: the daemon refuses
   * user prompts with an explanatory error, and every conversation item the
   * session produces carries `CONVERSATION_SOURCE_MERGE`.
   */
  mergeLeaseHeld: boolean;
  /**
   * WHEN this workspace's merge landed, in unix millis; 0 means it never has.
   *
   * The decoder must know this field even where nothing renders it: unknown
   * fields are rejected outright, so omitting it took down every frame for a
   * workspace that had merged — the one state whose frames matter most.
   */
  mergedAtMs: number;
  /**
   * THE merge run's live progress, or `undefined` when this workspace has no
   * merge to report.
   *
   * It is the ONLY merge-run surface on this message. The flat
   * `mergePhase` / `mergeQueuePosition` / `mergeQueueDepth` trio it replaced is
   * reserved on the wire and gone from here: a frame carrying it is a frame the
   * decoder rejects as unknown, not one it silently prefers a field of.
   */
  mergeStatus?: MergeStatus;
}

/**
 * Live progress of a workspace's current (or most recent) merge run.
 *
 * WHICH member `phase` names IS the phase: there is no separate enum to keep in
 * sync and no field that is meaningless for the phase in flight. `undefined`
 * `phase` is a wire the decoder refuses — a status naming no phase says nothing
 * a renderer could paint.
 */
export interface MergeStatus {
  /** Daemon-minted, stable across the whole run. */
  runId: string;
  /** When the CURRENT phase was entered; for a terminal phase, when it ended. */
  phaseStartedAtMs: number;
  /** Monotonic within the run; a within-phase tick bumps this alone. */
  updatedAtMs: number;
  phase:
    | { case: "enqueued"; value: MergeStatusEnqueued }
    | { case: "beforeAction"; value: MergeStatusBeforeAction }
    | { case: "cherryPicking"; value: MergeStatusCherryPicking }
    | { case: "testing"; value: MergeStatusTesting }
    | { case: "conflict"; value: MergeStatusConflict }
    | { case: "afterAction"; value: MergeStatusAfterAction }
    | { case: "merged"; value: MergeStatusMerged }
    | { case: "failed"; value: MergeStatusFailed };
}

export interface MergeStatusEnqueued {
  /** 1-based. */
  position: number;
  depth: number;
}

export interface MergeStatusBeforeAction {
  prompt: string;
}

export interface MergeStatusCherryPicking {
  commitsTotal: number;
  commitsLanded: number;
  currentSha: string;
  currentSubject: string;
}

export interface MergeStatusTesting {
  commitsTotal: number;
  /** The commit under test is landed but ungated. */
  commitsLanded: number;
  currentSha: string;
  currentSubject: string;
}

export interface MergeStatusConflict {
  conflictedSha: string;
  conflictedSubject: string;
  commitsTotal: number;
  commitsLanded: number;
}

export interface MergeStatusAfterAction {
  prompt: string;
}

export interface MergeStatusMerged {
  commitsTotal: number;
  /** Empty when the after action succeeded, or when none ran. */
  afterActionError: string;
}

export interface MergeStatusFailed {
  cause: string;
  /** 0 when planning never completed. */
  commitsTotal: number;
  commitsLanded: number;
  /** Set only for commit-bound failures. */
  failingSha: string;
  failingSubject: string;
  /**
   * This arm, serialized as JSON by the daemon through proto3's own JSON
   * mapping, so a frontend can report the WHOLE failure record as one field
   * of the error it shows rather than the two fields its prose quotes.
   */
  failedJson: string;
}

export interface SessionView {
  workspace: string;
  sessionId: string;
  model: string;
  slug: string;
  title: string;
  totalTokens: number;
  totalCostUsd: number;
  contextWindow: number;
  permissionMode: string;
  shimAttached: boolean;
  /** Durable CLI conversation uuid — the resume/rebind key (protojson camelCase). */
  claudeSessionId: string;
  /** Working directory a rebind's CreateSessionCmd needs. */
  cwd: string;
  // S7 GET /sessions parity fields (Emacs reads these off the pushed
  // SessionView now that it dropped the HTTP poller). Optional: a SessionView
  // that predates them decodes to the field default.
  /** Whether the session's conversation has ended (delete / shim death). */
  terminal: boolean;
  /** Retained for wire parity; false post-cutover. */
  rehydratable: boolean;
  /** Retained for wire parity; false post-cutover. */
  hibernated: boolean;
  /** Count of unresolved permission requests on the live session. */
  pendingPermissions: number;
  /**
   * Additive (S8): the CLAUDE_CONFIG_DIR the session's shim runs against — the
   * account identity for a daemon-executed, webapp-initiated switch.
   */
  configDir: string;
  /** The live SDK's selectable-model menu; it does not select a model. */
  modelOptions: ModelOption[];
  /**
   * Whether this session's on-disk transcript has been read into the store
   * (F2, the never-blue completion signal). Daemon-resolved; the webapp
   * decodes it for wire parity and renders no visual for it — the consumer is
   * the Emacs workspace-switch ensure.
   */
  backfill: BackfillState;
  /**
   * The classified death of a terminal session (delete, supersede, shim
   * death), absent while the session lives. Emacs surfaces it as a failure
   * card (frontend-state.el); the webapp decodes it so a terminal push never
   * breaks the strict decoder, and card rendering rides the feed's own
   * SystemFailureItem.
   */
  death?: SystemFailure;
  tokenUtilization?: SessionTokenUtilization;
  /**
   * Present IFF the session is hibernated — the typed account behind the
   * `hibernated` bool above, which stays as its compatibility projection.
   * The revival gate is rendered from THIS, not from the bool: the bool can
   * say a session is asleep but not why, and the gate's whole job is to tell
   * the user what put it to sleep before asking them to pay for waking it.
   */
  hibernation?: HibernationDetail;
}

/**
 * Why and since when a session is hibernated.
 *
 * The cause is a ONEOF of typed arms rather than a string, so "asleep at the
 * idle cutoff", "the user asked for this", and "the cache went cold before a
 * ping could fire" stay three distinguishable facts in every snapshot instead
 * of three renderings of one sentence. Exactly one arm is always set.
 */
export interface HibernationDetail {
  /** When the session entered hibernation, unix millis. */
  sinceMs: number;
  // The arm keys are the spelling table's (proto-names.ts), which is checked
  // against the generated oneof — so this union cannot drift from the wire.
  cause:
    | { case: typeof HIBERNATION_CAUSE.idleCutoff; value: HibernationIdleCutoff }
    | { case: typeof HIBERNATION_CAUSE.forced; value: HibernationForced }
    | { case: typeof HIBERNATION_CAUSE.cacheExpired; value: HibernationCacheExpired };
}

/** Automatic hibernation at the idle cutoff. */
export interface HibernationIdleCutoff {
  /**
   * The configured cutoff that tripped, millis — carried so the gate can say
   * "asleep after 1h idle" without the frontend knowing daemon config.
   */
  cutoffMs: number;
}

/** User-forced hibernation. Deliberately empty: the cause IS the arm. */
export type HibernationForced = Record<string, never>;

/** Hibernation because the cache went cold before a ping could fire. */
export interface HibernationCacheExpired {
  /** How long the session had actually been idle when the check ran, millis. */
  elapsedMs: number;
  /** The expected cache TTL the elapsed time exceeded, millis. */
  ttlMs: number;
}

export interface ModelOption {
  value: string;
  displayName: string;
  description: string;
}

/**
 * The never-blue backfill vocabulary (F2). `unspecified` is a REAL answer —
 * "no transcript on disk, nothing to backfill" — not an unknown.
 */
export const BACKFILL_STATES = ["unspecified", "pending", "done", "failed"] as const;
export type BackfillState = (typeof BACKFILL_STATES)[number];

/** protojson enum name -> the webapp's keyword. */
const BACKFILL_STATE_BY_NAME: Readonly<Record<string, BackfillState>> = {
  BACKFILL_STATE_UNSPECIFIED: "unspecified",
  BACKFILL_STATE_PENDING: "pending",
  BACKFILL_STATE_DONE: "done",
  BACKFILL_STATE_FAILED: "failed",
};

/**
 * Daemon-level identity/liveness (S7). Emacs keys boot detection and
 * version-mismatch warnings on it; the webapp decodes it but renders no visual.
 */
export interface DaemonView {
  bootId: string;
  protocolVersion: string;
  daemonBinaryMtimeMs: number;
  daemonVersion: string;
}

/** The protojson arm keys a `ConversationItem` oneof may set (S9). */
export const CONVERSATION_ITEM_ARMS = [
  "assistantMessage",
  "userMessage",
  "toolUse",
  "toolResult",
  "toolUseResult",
  "result",
  "contextCleared",
  "contextCompacted",
  "permission",
  "systemFailure",
  "skillBody",
  "sessionCommand",
] as const;
export type ConversationItemArm = (typeof CONVERSATION_ITEM_ARMS)[number];

/**
 * `frontend.v1.SessionCommand` — the slash commands the CLI answers ITSELF,
 * as a closed set.
 *
 * The daemon recognizes one of these before it forwards the submit and pushes
 * a `SessionCommandItem` INSTEAD of a prompt bubble, so this list is also the
 * complete set of things that can suppress a bubble. Anything not named here
 * is a prompt and is always drawn.
 *
 * The names are the wire's, minus the `SESSION_COMMAND_` prefix. The slash
 * form a reader sees is derived from them at render time (`sessionCommandLabel`),
 * never carried on the wire: the item deliberately has no text field, which is
 * what makes it structurally incapable of putting the user's `/model opus`
 * back on screen.
 */
export const SESSION_COMMANDS = [
  "CLEAR",
  "COMPACT",
  "MODEL",
  "COST",
  "USAGE",
  "STATUS",
  "CONTEXT",
  "CONFIG",
  "HELP",
  "DOCTOR",
  "LOGIN",
  "LOGOUT",
  "MEMORY",
  "PERMISSIONS",
  "AGENTS",
  "MCP",
  "HOOKS",
  "OUTPUT_STYLE",
  "RELEASE_NOTES",
  "TODOS",
  "EXPORT",
  "ADD_DIR",
  "RESUME",
  "EXIT",
  "PRIVACY_SETTINGS",
  "STATUSLINE",
  "TERMINAL_SETUP",
  "VIM",
  "REWIND",
  "BUG",
] as const;
export type SessionCommand = (typeof SESSION_COMMANDS)[number];

/**
 * The `SessionCommand` a wire value names.
 *
 * An UNSPECIFIED or unrecognized command THROWS rather than defaulting. The
 * command IS the item's entire content — there is nothing else in the message
 * — so an item that cannot say which command it reports is empty, and drawing
 * a guessed one would tell the user they ran something they did not.
 */
export function sessionCommandOf(name: string, where: string): SessionCommand {
  const known = SESSION_COMMANDS.find((c) => name === c || name === `SESSION_COMMAND_${c}`);
  if (known === undefined) {
    throw new Error(`frontend-proto: ${where}.command has unrecognized value '${name}'`);
  }
  return known;
}

/**
 * `frontend.v1.ErrorClass` — the SEMANTIC class of a system failure.
 *
 * It says what KIND of thing failed, never what color to draw: INTERNAL is
 * agent-repl's own machinery (shim down, store outage, a refused command) and
 * API is the vendor or the account stopping the work. Each frontend maps class
 * to color from the shared table, so a wire that spoke in colors would make
 * every future frontend inherit this one's palette.
 */
export const ERROR_CLASSES = ["INTERNAL", "API"] as const;
export type ErrorClass = (typeof ERROR_CLASSES)[number];

/**
 * A daemon-classified failure (`frontend.v1.SystemFailureItem`).
 *
 * Every render-bound error in the tree arrives as one of these. Nothing here
 * is re-interpreted frontend-side: the class, the type and the prose are the
 * daemon's verdict, and this end renders them.
 */
export interface SystemFailure {
  errorClass: ErrorClass;
  /** The specific failure within the class ("shim.rejected", "api.rate_limit"). */
  errorType: string;
  /** The plain-language explanation the card renders. */
  message: string;
  /** The raw structured account, shown beside the prose rather than replacing it. */
  sourceDetail: string;
  /**
   * Wall-clock ms a WINDOW-shaped failure closed; 0 means still open. The card
   * is re-sent under the SAME uuid with this set, so the feed reconciles it in
   * place and shows a settled card rather than a standing alarm.
   */
  resolvedAtMs: number;
  /**
   * The conversation item this failure was filed under. It is how a failure
   * reached OUTSIDE the feed (a footer row, an ack) addresses its card.
   */
  itemUuid: string;
  detail: SystemFailureDetail;
}

/** The only structured evidence a system failure can carry. */
export type SystemFailureDetail =
  | { kind: "none" }
  | { kind: "sessionResume"; value: SessionResumeFailure }
  | { kind: "queryTermination"; value: QueryTerminationFailure };

/** Typed evidence for a failed authoritative-conversation resume. */
export interface SessionResumeFailure {
  agentReplSessionId: string;
  claudeSessionId: string;
  cwd: string;
  configDir: string;
  resolvedConfigDir: string;
  attempt: "create" | "automaticRestore";
  cause:
    | { case: "transcriptUnavailable"; searchedPaths: string[] }
    | { case: "identityMismatch"; replacementClaudeSessionId: string }
    | { case: "queryTermination"; value: QueryTerminationFailure }
    | { case: "bringUpFailure"; cause: string };
}

/** Exact generated evidence for an SDK query that ended unexpectedly. */
export type QueryTerminationFailure = GeneratedQueryTerminationFailure;

/** Exact generated cumulative usage for an agent-repl session. */
export type SessionTokenUtilization = GeneratedSessionTokenUtilization;

/**
 * One decoded conversation addition: the {uuid, tsMs, requestId} envelope plus
 * the single selected typed payload arm and its (shape-adopted) value. The
 * state-adapter decomposes `payload` per `arm` into the store's render items.
 */
export interface ConversationItemFrame {
  uuid: string;
  tsMs: number;
  requestId: string;
  /**
   * WHO drove the turn that produced this item. Decoded, never defaulted: an
   * `UNSPECIFIED` here is a malformed frame, and the conversation layer refuses
   * it loudly rather than assuming a user drove it.
   */
  source: ConversationSource;
  arm: ConversationItemArm;
  /** The typed data.v1/core.v1 payload, adopted by shape (see file-top §5.1). */
  payload: JsonObject;
  tokenUtilization: TokenUtilization[];
  /** Complete terminal-turn evidence, present only on result items. */
  turnAccounting?: TurnAccounting;
}

/** Durable evidence used to compare one completed turn with another client. */
interface TurnAccountingEvidence {
  turnId: string;
  queryInstanceId: string;
}
/** Evidence required before a turn can be compared definitively. */
export interface CompleteTurnAccounting extends TurnAccountingEvidence {
  verdict: { kind: "complete" };
  runtime?: QueryRuntimeIdentity;
  timing?: TurnAccountingTiming;
  usageAtStart?: AccountUsageObservation;
  usageAtEnd?: AccountUsageObservation;
  responses: TokenUtilization[];
  reconciliation?: TokenUsageReconciliation;
}
/** Invalid turns retain every available evidence fragment without inventing absent evidence. */
export interface InvalidTurnAccounting extends TurnAccountingEvidence {
  verdict: { kind: "invalid"; problems: TurnAccountingProblem[] };
  runtime?: QueryRuntimeIdentity;
  timing?: TurnAccountingTiming;
  usageAtStart?: AccountUsageObservation;
  usageAtEnd?: AccountUsageObservation;
  responses?: TokenUtilization[];
  reconciliation?: TokenUsageReconciliation;
}
export type TurnAccounting = CompleteTurnAccounting | InvalidTurnAccounting;
export interface TurnAccountingTiming { promptAdmittedAtMs: number; resultReceivedAtMs: number; accountingSettledAtMs: number; promptToResultMs: number; resultToSettlementMs: number; }
export interface QueryRuntimeIdentity { vendorSessionId: string; effectiveModel: string; sdkVersion: string; claudeCodeVersion: string; shimBuildSha: string; authSource: string; subscriptionType: string; fastModeState: string; fastModeReason: string; effectiveOptions?: EvidenceFingerprint; settings?: EvidenceFingerprint; tools?: EvidenceFingerprint; mcp?: EvidenceFingerprint; contextPrefix?: EvidenceFingerprint; }
export type EvidenceFingerprint = { kind: "sha256"; value: string } | { kind: "unavailable"; cause: string };
export interface AccountUsageObservation { queryInstanceId: string; turnId: string; boundaryAtMs: number; observedAtMs: number; sampleLatencyMs: number; subscriptionType: string; boundary?: "turnStart" | "turnEnd"; outcome?: { kind: "available"; utilizationPercent: number; resetsAtMs: number } | { kind: "unavailable"; reason: string }; }
export interface TokenUsageReconciliation { responseRecordCount: number; responseAllAgents?: UsageTotals; responseMainAgent?: UsageTotals; resultMainAgent?: UsageTotals; responseModels: ModelUsageTotals[]; resultModels: ModelUsageTotals[]; apiMessageIds: string[]; }
export interface UsageTotals { inputTokens: number; outputTokens: number; cacheReadInputTokens: number; cacheCreationInputTokens: number; cacheCreation5m?: number; cacheCreation1h?: number; webSearchRequests?: number; webFetchRequests?: number; thinkingTokens?: number; cacheRates?: { totalPromptInputTokens: number; cacheHitRate: number; cacheWriteRate: number; uncachedInputRate: number }; timing?: { outputTokensWithGenerationDuration: number; outputGenerationDurationMs: number; responsesWithGenerationDuration: number; responsesWithoutGenerationDuration: number; totalTimeToFirstTokenMs: number; responsesWithTimeToFirstToken: number; responsesWithoutTimeToFirstToken: number }; }
export interface ModelUsageTotals { model: string; canonicalModel?: string; provider?: string; totals?: UsageTotals; contextWindow?: number; maxOutputTokens?: number; costUsd?: number; }
export type TurnAccountingProblem =
  | { kind: "missingUsageBoundary"; boundary: "turnStart" | "turnEnd" }
  | { kind: "windowReset"; startResetsAtMs: number; endResetsAtMs: number }
  | { kind: "tokenLedgerMismatch"; differingFieldPaths: string[] }
  | { kind: "runtimeIdentityIncomplete"; missingFieldPaths: string[] }
  | { kind: "unmodeledUsageFields"; sourceFieldPaths: string[] }
  | { kind: "telemetryRecordMissing"; record: { kind: "queryLifecycle"; queryInstanceId: string } | { kind: "responseUsage"; apiMessageId: string } | { kind: "persistenceReceipt"; turnId: string } };

/** Response-level usage associated with one rendered assistant response. */
export interface TokenUtilization {
  agentReplSessionId: string;
  claudeSessionId: string;
  rootTurnId: string;
  apiRequestId?: string;
  apiMessageId: string;
  model: string;
  actor: "mainAgent" | "subagent";
  subagent?: { agentId: string; parentToolUseId: string; parentAgentId: string; subagentType: string; taskDescription: string };
  usage: ResponseTokenUsage;
  responseTiming?: { timeToFirstTokenMs?: number; outputGenerationDurationMs?: number };
}

export interface TokenCacheCreation { ephemeral5mInputTokens: number; ephemeral1hInputTokens: number; }
export interface TokenServerToolUse { webSearchRequests: number; webFetchRequests: number; }
export interface TokenOutputDetails { thinkingTokens: number; }
export interface TokenCacheRates { totalPromptInputTokens: number; cacheHitRate: number; cacheWriteRate: number; uncachedInputRate: number; }
export interface TokenUsageIterationCounters { inputTokens: number; outputTokens: number; cacheReadInputTokens: number; cacheCreationInputTokens: number; cacheCreation?: TokenCacheCreation; }
export type TokenUsageIteration =
  | ({ kind: "sampling"; model: string } & TokenUsageIterationCounters)
  | ({ kind: "compaction" } & TokenUsageIterationCounters)
  | ({ kind: "advisor"; model: string } & TokenUsageIterationCounters)
  | ({ kind: "fallback"; model: string } & TokenUsageIterationCounters);
export type TokenCacheDiagnostic =
  | { kind: "pending" }
  | { kind: "modelChanged"; cacheMissedInputTokens: number }
  | { kind: "systemChanged"; cacheMissedInputTokens: number }
  | { kind: "toolsChanged"; cacheMissedInputTokens: number }
  | { kind: "messagesChanged"; cacheMissedInputTokens: number }
  | { kind: "previousMessageUnavailable" }
  | { kind: "diagnosticsUnavailable" };
/** Every field modeled by `frontend.v1.TokenUsage`, preserving intentional absence. */
export interface ResponseTokenUsage {
  inputTokens: number;
  outputTokens: number;
  cacheReadInputTokens: number;
  cacheCreationInputTokens: number;
  cacheCreation?: TokenCacheCreation;
  serverToolUse?: TokenServerToolUse;
  serviceTier: string;
  speed: string;
  inferenceGeo: string;
  outputDetails?: TokenOutputDetails;
  iterations: TokenUsageIteration[];
  cacheDiagnostic?: TokenCacheDiagnostic;
  cacheRates?: TokenCacheRates;
  fallbackCredit?: JsonObject;
  unmodeledUsage?: JsonObject;
  rawUsage: JsonObject;
}

export interface ConversationDelta {
  workspace: string;
  sessionId: string;
  items: ConversationItemFrame[];
  throughSeq: number;
}

/** The content-delta kinds a `TypingDelta`'s embedded `ContentDelta` may set. */
export const CONTENT_DELTA_KINDS = ["text", "thinking", "input_json", "signature"] as const;
export type ContentDeltaKind = (typeof CONTENT_DELTA_KINDS)[number];

/**
 * Ephemeral live-typing relay — flattened from the embedded
 * `core.v1.ContentDelta` for the store's reveal feed. `kind` is normalized to
 * the store's snake vocabulary (the `inputJson` arm reads as `input_json`).
 */
export interface TypingDelta {
  workspace: string;
  sessionId: string;
  uuid: string;
  blockIndex: number;
  kind: ContentDeltaKind;
  delta: string;
  estimatedTokens: number;
}

/**
 * The ephemeral long-tool liveness relay (E4), flattened from
 * `HeartbeatView{workspace, session_id, progress}` the same way `TypingDelta`
 * flattens its embedded `ContentDelta`.
 *
 * Never persisted and never present in a `StateSnapshot`: a reconnecting
 * frontend simply waits for the next heartbeat rather than replaying old ones.
 */
export interface HeartbeatView {
  workspace: string;
  sessionId: string;
  toolUseId: string;
  toolName: string;
  parentToolUseId: string;
  /** The SDK's raw elapsed clock for the running tool, in seconds. */
  elapsedSeconds: number;
}

/**
 * The session's retained `data.v1.SystemInit` (slash commands, tools, skills,
 * model, auth source, …), pushed on attach + carried in `StateSnapshot.inits`
 * (S9). `init` is adopted by shape — a large, additively-growing message the
 * status panel reads leniently. Replaces the HTTP `/status` snapshot source.
 */
export interface SessionInitView {
  workspace: string;
  sessionId: string;
  init: JsonObject;
}

export interface TaskEntry {
  taskId: string;
  kind: string;
  description: string;
  status: string;
  outputPath: string;
  startedAtMs: number;
  endedAtMs: number;
}

export interface TaskCatalog {
  workspace: string;
  sessionId: string;
  tasks: TaskEntry[];
}

export interface CommandAck {
  requestId: string;
  ok: boolean;
  error: string;
  /** Shim-confirmed current model for a SetModel receipt, including rejection. */
  selectedModel: string;
  /**
   * The CLASSIFIED refusal (F4). `error` above is the raw handler text this
   * end never rendered at all; this is the same failure run through the
   * daemon's one classifier, which is what lets a rejected prompt become a
   * visible card instead of a console log. Absent when `ok`.
   */
  failure?: SystemFailure;
  /**
   * The interrupt confirmation CHALLENGE (I1). NOT a failure and NOT an error:
   * the command was understood and deliberately not performed, because no turn
   * was live and stopping live subagents deserves an explicit yes. Arrives
   * with `ok` false and `failure` absent; the answer is a resent
   * `InterruptCmd{confirmAgents: true}`.
   */
  interruptConfirmRequired?: InterruptConfirmRequired;
}

/**
 * What the interrupt would actually stop (I1), so a client can ask a concrete
 * question ("interrupt 3 running subagents?") rather than a bare are-you-sure.
 */
export interface InterruptConfirmRequired {
  liveTasks: number;
}

/**
 * One activity window on the progress footer (F1): open until cleared.
 * `active` false is the window closed; `sinceMs` counts from when it opened
 * and survives a detail refresh.
 */
export interface ProgressWindow {
  active: boolean;
  sinceMs: number;
  /** Window-specific detail (a hook name, an auth line, a retry summary). */
  detail: string;
}

/**
 * One ALLOWANCE's rate-limit window, which carries structured detail (F1).
 *
 * Present means the vendor has reported this allowance; `active` is the
 * narrower claim that the report was newsworthy (anything other than a plain
 * "allowed"). A quiet allowance still ships its figures, because they are what
 * the reader needs beside the allowance that is not quiet.
 */
export interface RateLimitWindow {
  active: boolean;
  /** Epoch SECONDS (the vendor event's own unit), not millis. */
  resetsAt: number;
  utilization: number;
  status: string;
}

/**
 * How an interrupt turned out (I1), from `agentshim.core.v1.InterruptOutcome`.
 *
 * The three real answers are decided ATOMICALLY on the shim's ack, and they
 * are deliberately distinct: `already_complete` is a SUCCESS (the user asked
 * for the turn to be over and it already was), and only `failed` reads as a
 * failure anywhere. Deriving this downstream is what once painted a turn that
 * had already ended as a stop that had failed.
 */
export const INTERRUPT_OUTCOMES = ["interrupted", "already_complete", "failed"] as const;
export type InterruptOutcome = (typeof INTERRUPT_OUTCOMES)[number];

/** protojson enum name -> the webapp's keyword. */
const INTERRUPT_OUTCOME_BY_NAME: Readonly<Record<string, InterruptOutcome>> = {
  INTERRUPT_OUTCOME_INTERRUPTED: "interrupted",
  INTERRUPT_OUTCOME_ALREADY_COMPLETE: "already_complete",
  INTERRUPT_OUTCOME_FAILED: "failed",
};

/**
 * The interrupt window (I1): opened by the shim's ack, cleared when the next
 * turn starts. `outcome` rides along so ALREADY_COMPLETE and FAILED render
 * distinctly even though neither moves the workspace's phase.
 */
export interface InterruptWindow {
  active: boolean;
  sinceMs: number;
  outcome: InterruptOutcome | null;
}

/**
 * The consolidated progress footer's ENTIRE input (F1), resolved daemon-side.
 *
 * Latest-wins per workspace: every push carries the complete view, so an absent
 * window means closed rather than "no news". Deliberately carries NO
 * output-token figure — the token cell is the CURRENT TURN's cumulative input
 * only, and session-wide token figures stay in `SessionView`.
 */
export interface ProgressView {
  workspace: string;
  sessionId: string;
  // NO PHASE (F5). The daemon used to mirror the SSM's verdict here so the
  // footer had one self-sufficient frame, and the copy went stale exactly as a
  // second copy of an authoritative fact always does — it refreshed on the
  // progress resolver's triggers rather than the SSM's, which is what put
  // "starting" in the footer of an already-green tab. The footer reads the
  // phase off `WorkspaceState` now, the same message the tab bar reads.
  //
  // The wire field is deprecated in place rather than removed, so it stays in
  // the accepted key set below (an older daemon still sends it) and is simply
  // not read.
  /** 0 = no turn in flight. */
  turnStartedAtMs: number;
  thinkingTokens: number;
  /** THIS turn's cumulative cached + uncached input tokens. */
  inputTokens: number;
  ttftMs: number;
  /** Absent = the window is closed. */
  compacting?: ProgressWindow;
  retrying?: ProgressWindow;
  authenticating?: ProgressWindow;
  hook?: ProgressWindow;
  /**
   * The vendor's TWO allowances, each with its own deadline and its own
   * severity: the rolling five-hour session window, and the seven-day weekly
   * one. They were a single field until a reader saw a weekly figure named as
   * the session's, with no way to tell which allowance the percentage meant.
   */
  rateLimited?: RateLimitWindow;
  rateLimitedWeekly?: RateLimitWindow;
  /**
   * The session reporting it is parked on the USER. NOT a phase — `state`
   * above remains the SSM's verdict — but a fact the daemon cannot otherwise
   * see, since a session can block on an interaction it holds no count for.
   */
  blocked?: ProgressWindow;
  /**
   * The interrupt window (I1). Ack-opened, next-turn-cleared, and carrying the
   * outcome the shim's ack decided. Absent = no interrupt to speak of.
   */
  interrupt?: InterruptWindow;
  /**
   * The CLASSIFIED error state (F4). Persists until the next turn starts; its
   * own `itemUuid` is the feed item the error row scrolls to. Carrying the
   * class is what lets the footer take its color from the shared table
   * instead of the hardcoded red no other surface consulted.
   */
  failure?: SystemFailure;
  /**
   * Set when a turn's UNCACHED input cost crossed the alert threshold — the
   * loud "this prompt re-ingested context" signal. Persists until the next
   * turn starts, exactly like `failure`. Absent means the last turn was
   * cache-efficient, which is the only reading of absence.
   */
  expensiveTurn?: ContextCostAlert;
  pendingPermissions: number;
  queueDepth: number;
  liveTaskCount: number;
}

/**
 * One turn's excessive uncached-input observation, resolved DAEMON-SIDE from
 * the turn's result usage. The webapp renders it; it computes nothing here.
 *
 * `promptOrigin` is carried verbatim because a CACHE_KEEP_ALIVE origin is not
 * a variant of "that turn was expensive" — it is the alarm that the ping meant
 * to keep the cache warm came back COLD, having paid full freight for nothing.
 */
export interface ContextCostAlert {
  /** The turn that crossed the threshold. */
  turnId: string;
  /** The observed uncached cost: input_tokens + cache_creation_input_tokens. */
  uncachedInputTokens: number;
  /** The configured threshold that tripped, so the row can say "N over M". */
  thresholdTokens: number;
  /** When the result carrying the usage arrived, unix millis. */
  atMs: number;
  /**
   * The turn's attribution, as the canonical `core.v1.PromptOrigin` NAME.
   *
   * Kept as the wire name rather than mapped through a webapp keyword table:
   * PromptOrigin is a large, still-growing enum and the webapp has exactly ONE
   * question to ask of it (is this the keep-alive ping?). A local mirror of
   * every arm would have to be extended on every daemon-side addition, and a
   * strict name table would throw the whole ProgressView away over an origin
   * this surface does not even distinguish.
   */
  promptOrigin: string;
}

/**
 * The one `PromptOrigin` this surface distinguishes: the daemon's cache
 * keep-alive ping. See `ContextCostAlert.promptOrigin` for why the rest of the
 * enum is carried as an opaque name.
 *
 * Re-exported from the build-checked spelling table rather than spelled again:
 * this name is the whole difference between "that turn was expensive" and "the
 * ping came back cold", and a stale copy of it would silently pick the wrong
 * alarm.
 */
export { PROMPT_ORIGIN_CACHE_KEEP_ALIVE };

export interface StateSnapshot {
  workspaces: WorkspaceState[];
  sessions: SessionView[];
  catalogs: TaskCatalog[];
  /** Daemon identity carried on every connect snapshot (S7); optional. */
  daemon?: DaemonView;
  /** Retained per-session SystemInits (S9); absent on a pre-S9 daemon. */
  inits: SessionInitView[];
  /** Each live session's held-prompt queue (E4); empty on a pre-E4 daemon. */
  queues: QueueView[];
  /** Each workspace's resolved progress view (F1); empty on a pre-F1 daemon. */
  progress: ProgressView[];
  /** Durable host-only workspace descriptors; stripped before a GUI receives a snapshot. */
  workspaceAvailable: WorkspaceAvailable[];
  /** Durable host-only UI actions; stripped before a GUI receives a snapshot. */
  hostActions: HostAction[];
  /**
   * The daemon-global drain lease as of connect, so a client that joins
   * mid-drain sees it without waiting for an edge. ABSENT means the daemon
   * does not carry the lease at all (pre-feature), which is the absence of
   * INFORMATION — never a claim that the lease is idle.
   */
  shutdownSchedule?: ShutdownScheduleView;
}

/** The daemon's authoritative, shim-ready workspace descriptor for the Emacs host. */
export interface WorkspaceAvailable {
  jobId: string;
  finalName: string;
  worktreePath: string;
  branch: string;
  gitRoot: string;
  baseCommit: string;
  sourceWorkspace: string;
  sourceDir: string;
  forkFrom: string;
  forkSessionId: string;
  sessionId: string;
  priority: string;
  model: string;
  initialPromptQueued: boolean;
}

/**
 * `frontend.v1.WorkspaceRoster` — one complete picture of the sidebar.
 *
 * Emacs authors it; the daemon retains and rebroadcasts it. Always whole,
 * never a delta, so a partially-applied roster is unrepresentable.
 */
export interface WorkspaceRoster {
  /** Monotonic WITHIN one bootId; a same-epoch frame older than the held one is dropped. */
  revision: number;
  /**
   * The publishing editor's per-boot identity, and the epoch key `revision` is
   * scoped by. Opaque and never empty: a frame whose bootId differs from the
   * held one opens a new epoch and supersedes it whatever its revision.
   */
  bootId: string;
  /** The active grouping — the set arm IS the grouping, with no separate mode flag. */
  view:
    | { case: "repository"; value: RosterRepositoryView }
    | { case: "task"; value: RosterTaskView };
  /** Settled merges, hoisted out of the grouping and rendered under both views. */
  recentlyMerged: RosterSection;
  /** Absolute worktree dir of the current workspace; empty when there is none. */
  currentDir: string;
  /** Absolute worktree dir the keyboard cursor rests on; empty when there is none. */
  navDir: string;
}

/** The repository grouping's sections, in author-supplied render order. */
export interface RosterRepositoryView {
  sections: RosterRepoSection[];
}

/** The task grouping's sections, in author-supplied render order. */
export interface RosterTaskView {
  sections: RosterTaskSection[];
}

/** One repository's workspaces. `folded` hides rows the model still carries. */
export interface RosterRepoSection {
  repoKey: string;
  folded: boolean;
  rows: RosterRow[];
  /** Human display label; `repoKey` stays the fold identity. Empty when none. */
  label: string;
}

/** One task's workspaces. `taskId` is the identity; `title` is display only. */
export interface RosterTaskSection {
  taskId: string;
  title: string;
  done: boolean;
  rows: RosterRow[];
}

/** A bare row list: the shell for a section with no key of its own. It still
 * folds and still carries a heading — the author resolves both, so no client
 * has to synthesize a fold key or hardcode the label. */
export interface RosterSection {
  rows: RosterRow[];
  folded: boolean;
  label: string;
}

/** One workspace row. `dir` is the identity and the join key to every command. */
export interface RosterRow {
  dir: string;
  name: string;
  /**
   * The set arm IS the status. Modeled as `{ case }` alone rather than the
   * `{ case, value }` shape the file's other oneofs use, because every status
   * arm message is EMPTY by contract — a `value` here could only ever hold
   * `{}`, and offering one would invite a payload the wire cannot carry.
   */
  status: { case: RosterRowStatusCase };
  current: boolean;
  children: RosterRow[];
  /** Epoch ms the user last viewed this workspace; 0 means never viewed. */
  lastViewedAtMs: number;
  /** Epoch ms the merge settled; 0 means not merged. Wins the when-column. */
  mergedAtMs: number;
  /** The workspace's branch; empty means unknown. */
  branch: string;
  /** The branch it was cut from and merges back into; empty means unknown. */
  parentBranch: string;
  /** Short prose describing the workspace's work; empty means none. */
  summary: string;
  /** Panes dismissed but still switchable — renders receded, like merged. */
  closed: boolean;
}

/** A typed UI-only action from the daemon-owned workspace-command inbox. */
export type HostAction = {
  actionId: string;
  action:
    | { case: "switchWorkspace"; dir: string }
    | { case: "setRepositoryFold"; repoKey: string; folded: boolean }
    | { case: "setSidebarView"; view: string }
    | { case: "taskCreate" }
    | { case: "taskToggleDone"; id: string }
    | { case: "taskOpen"; id: string }
    | { case: "taskAddWorkspace"; id: string };
};

/**
 * How the classifier judged a held prompt (E4). `pending` = still being
 * judged; `error` = NOTHING decided it (the classifier failed, or answered
 * unreadably) and is deliberately distinct from a real verdict.
 */
export const QUEUE_CLASSIFICATIONS = ["pending", "interject", "hold", "error"] as const;
export type QueueClassification = (typeof QUEUE_CLASSIFICATIONS)[number];

/** protojson enum name -> the webapp's keyword. */
const QUEUE_CLASSIFICATION_BY_NAME: Readonly<Record<string, QueueClassification>> = {
  QUEUE_CLASSIFICATION_PENDING: "pending",
  QUEUE_CLASSIFICATION_INTERJECT: "interject",
  QUEUE_CLASSIFICATION_HOLD: "hold",
  QUEUE_CLASSIFICATION_ERROR: "error",
};

/**
 * Present when this entry is held by a scheduled shutdown's DRAIN LEASE rather
 * than by a running turn. The classifier never ran on it, so its
 * `classification` says nothing about why it is waiting, and the webapp renders
 * a dedicated lease bubble instead of the classifier bubble.
 */
export interface QueueEntryShutdownHold {
  /** The schedule holding this entry, joining it to the live lease view. */
  scheduleId: string;
}

/**
 * Present when this entry is held because a CACHE KEEP-ALIVE turn is in
 * flight. Like the drain lease and unlike an ordinary held prompt, the
 * classifier never ran on it — but its exits are narrower still: there is NO
 * force-through, because the keep-alive must complete before the daemon can
 * rewind and submit this entry. The only ways out are delivery when the ping's
 * turn ends, and cancel.
 */
export interface QueueEntryKeepAliveHold {
  /** The in-flight keep-alive turn whose completion releases this entry. */
  turnId: string;
}

/**
 * Present when this entry is held because a COMPACT-FIRST REVIVAL's compaction
 * is still pending. Like the other two holds the classifier never ran on it,
 * and like the keep-alive hold there is NO force-through: a session still
 * asleep cannot take a prompt at all. The exits are delivery once the
 * compaction lands and the gate opens, a loud drop if the revival fails, and
 * cancel.
 */
export interface QueueEntryRevivalHold {
  /** The session being revived, whose completed compaction releases this. */
  sessionId: string;
}

/** One prompt the daemon is holding (E4). */
export interface QueueEntry {
  id: string;
  text: string;
  queuedAtMs: number;
  classification: QueueClassification;
  rationale: string;
  accepted: boolean;
  /**
   * Absent for an ordinary classifier-held entry. Present ONLY while a drain
   * lease holds the prompt — absence is therefore the real answer "no lease is
   * holding this", not a missing field to default away.
   */
  shutdownHold?: QueueEntryShutdownHold;
  /**
   * Set ONLY while an in-flight cache keep-alive turn holds this prompt.
   * Absence is the real answer "no keep-alive is holding this", not a missing
   * field: it selects the keep-alive bubble over the classifier bubble.
   */
  keepAliveHold?: QueueEntryKeepAliveHold;
  /**
   * Set ONLY while a pending compact-first revival holds this prompt. Absence
   * is the real answer "no revival is holding this", not a missing field: it
   * selects the revival bubble over the classifier bubble.
   */
  revivalHold?: QueueEntryRevivalHold;
}

/** The `idle` arm's payload: deliberately empty — the arm IS the state. */
export type ShutdownScheduleIdle = Record<string, never>;

/** An active turn blocking the drain, named by its turn id. */
export interface ShutdownHoldTurn {
  turnId: string;
}

/** Live background tasks blocking the drain; the count is display-grade. */
export interface ShutdownHoldTasks {
  count: number;
}

/**
 * One workspace the drain is waiting on, and why. `turn` and `tasks` are
 * CO-OCCURRING facts, not exclusive states: a session can hold a running turn
 * and live background tasks at once. At least one is always set.
 */
export interface ShutdownHold {
  /** Absolute workspace CWD — the join key every session-routed command uses. */
  workspace: string;
  sessionId: string;
  turn?: ShutdownHoldTurn;
  tasks?: ShutdownHoldTasks;
}

/** The held lease: what the scheduled bounce is waiting on, right now. */
export interface ShutdownScheduleDraining {
  scheduleId: string;
  /** Epoch ms the lease was taken, for elapsed-time display. */
  scheduledAtMs: number;
  /** Free display text ("merge of <ws> rebuilt the daemon"); never parsed. */
  cause: string;
  /** Whether the executed shutdown will also SIGTERM every session shim. */
  stopShims: boolean;
  /** Never empty: a drained lease is executed, not broadcast. */
  holds: ShutdownHold[];
}

/**
 * The daemon-global scheduled-shutdown lease. EXACTLY ONE arm is always set —
 * `idle` is a real broadcast value (a cancel, or a completed drain), so
 * clearing the lease is representable and "no lease" can never be confused
 * with "no information".
 */
export type ShutdownScheduleView = {
  state:
    | { case: "idle"; value: ShutdownScheduleIdle }
    | { case: "draining"; value: ShutdownScheduleDraining };
};

/**
 * The session's whole held-prompt queue (E4). It is a REPLACEMENT, not a
 * delta: every push carries the complete queue, so an empty entries list means
 * the queue is empty rather than "no news".
 */
export interface QueueView {
  workspace: string;
  sessionId: string;
  entries: QueueEntry[];
}

/** The push-channel oneof wrapper (FrontendFrame.frame). */
export type FrontendFrame = {
  frame:
    | { case: "snapshot"; value: StateSnapshot }
    | { case: "workspaceState"; value: WorkspaceState }
    | { case: "sessionView"; value: SessionView }
    | { case: "conversationDelta"; value: ConversationDelta }
    | { case: "typingDelta"; value: TypingDelta }
    | { case: "taskCatalog"; value: TaskCatalog }
    | { case: "commandAck"; value: CommandAck }
    | { case: "daemonView"; value: DaemonView }
    | { case: "sessionInit"; value: SessionInitView }
    | { case: "heartbeat"; value: HeartbeatView }
    | { case: "queue"; value: QueueView }
    | { case: "progress"; value: ProgressView }
    | { case: "workspaceAvailable"; value: WorkspaceAvailable }
    | { case: "hostAction"; value: HostAction }
    | { case: "workspaceRoster"; value: WorkspaceRoster }
    | { case: "shutdownSchedule"; value: ShutdownScheduleView };
};

/** The frame-variant discriminators FrontendFrame.frame.case may hold. */
export type FrameCase = FrontendFrame["frame"]["case"];

// --- vocabularies -----------------------------------------------------------

/** The kinds a `TaskEntry.kind` may name. */
export const TASK_KINDS = ["agent", "shell", "workflow"] as const;
export type TaskKind = (typeof TASK_KINDS)[number];

/** The statuses a `TaskEntry.status` may name. */
export const TASK_STATUSES = ["running", "done", "error", "killed", "stopped", "lost"] as const;
export type TaskStatus = (typeof TASK_STATUSES)[number];

/**
 * The EXPLICIT unsupported-shapes registry (§11 deliverable) — the
 * `FrontendFrame` variants the webapp does NOT map to a visual, each with the
 * reason. Everything else maps to a supported visual (`sessionInit` feeds the
 * /status panel + slash-menu source). The state adapter routes a listed
 * variant down its typed, counted, log-once ignore path. Unsupported
 * CONVERSATION-ITEM arms/blocks (a `toolUseResult` with no correlation key, a
 * `signature` content delta, an image content block) are ignored dynamically
 * by the adapter the same way, since that set is the daemon's to grow.
 */
export const UNSUPPORTED_SHAPES: ReadonlyMap<string, string> = new Map<string, string>([
  [
    "commandAck",
    "control-plane command receipt (agentshim.frontend.v1.CommandAck); the " +
      "webapp consumes it in the command dispatcher, not the render adapter",
  ],
  [
    "daemonView",
    "daemon-level identity/liveness (agentshim.frontend.v1.DaemonView); the " +
      "webapp decodes it but renders no visual — boot detection and " +
      "version-mismatch warnings are an Emacs-frontend concern",
  ],
  [
    "workspaceAvailable",
    "host-only workspace materialization directive; GUI transports never receive it",
  ],
  ["hostAction", "host-only UI inbox action; GUI transports never receive it"],
]);

/** Whether a frame variant is mapped to a webapp visual (not in the registry). */
export function isVisuallySupportedFrame(frameCase: FrameCase): boolean {
  return !UNSUPPORTED_SHAPES.has(frameCase);
}

// --- decoder ----------------------------------------------------------------

function errMsg(err: unknown): string {
  return err instanceof Error ? err.message : String(err);
}

type Obj = Record<string, unknown>;

/** Decode ONE raw protojson `FrontendFrame` string, validating loudly. */
export function decodeFrontendFrame(json: string): FrontendFrame {
  let raw: unknown;
  try {
    raw = JSON.parse(json);
  } catch (err) {
    throw new Error(`frontend-proto: frame is not valid JSON: ${errMsg(err)}`);
  }
  const o = ensureObject(raw, "FrontendFrame");

  const keys = Object.keys(o);
  const variantKeys = keys.filter((k) => FRAME_DECODERS.has(k));
  const unknownKeys = keys.filter((k) => !FRAME_DECODERS.has(k));
  if (unknownKeys.length > 0) {
    throw new Error(
      `frontend-proto: FrontendFrame has unrecognized field(s): ${unknownKeys.join(", ")}`,
    );
  }
  if (variantKeys.length === 0) {
    throw new Error(
      "frontend-proto: FrontendFrame carries no known frame variant " +
        "(empty or unrecognized oneof)",
    );
  }
  if (variantKeys.length > 1) {
    throw new Error(
      `frontend-proto: FrontendFrame sets multiple oneof variants: ${variantKeys.join(", ")}`,
    );
  }
  const key = variantKeys[0];
  return { frame: FRAME_DECODERS.get(key)!(o[key]) };
}

const FRAME_DECODERS: ReadonlyMap<string, (v: unknown) => FrontendFrame["frame"]> = new Map<
  string,
  (v: unknown) => FrontendFrame["frame"]
>([
  ["snapshot", (v: unknown) => ({ case: "snapshot" as const, value: decodeStateSnapshot(v) })],
  ["workspaceState", (v: unknown) => ({ case: "workspaceState" as const, value: decodeWorkspaceState(v) })],
  ["sessionView", (v: unknown) => ({ case: "sessionView" as const, value: decodeSessionView(v) })],
  ["conversationDelta", (v: unknown) => ({ case: "conversationDelta" as const, value: decodeConversationDelta(v) })],
  ["typingDelta", (v: unknown) => ({ case: "typingDelta" as const, value: decodeTypingDelta(v) })],
  ["taskCatalog", (v: unknown) => ({ case: "taskCatalog" as const, value: decodeTaskCatalog(v) })],
  ["commandAck", (v: unknown) => ({ case: "commandAck" as const, value: decodeCommandAck(v) })],
  ["daemonView", (v: unknown) => ({ case: "daemonView" as const, value: decodeDaemonView(v) })],
  ["sessionInit", (v: unknown) => ({ case: "sessionInit" as const, value: decodeSessionInitView(v) })],
  ["heartbeat", (v: unknown) => ({ case: "heartbeat" as const, value: decodeHeartbeatView(v) })],
  ["queue", (v: unknown) => ({ case: "queue" as const, value: decodeQueueView(v) })],
  ["progress", (v: unknown) => ({ case: "progress" as const, value: decodeProgressView(v) })],
  [
    "workspaceAvailable",
    (v: unknown) => ({ case: "workspaceAvailable" as const, value: decodeWorkspaceAvailable(v) }),
  ],
  ["hostAction", (v: unknown) => ({ case: "hostAction" as const, value: decodeHostAction(v) })],
  [
    "workspaceRoster",
    (v: unknown) => ({ case: "workspaceRoster" as const, value: decodeWorkspaceRoster(v) }),
  ],
  [
    "shutdownSchedule",
    (v: unknown) => ({
      case: "shutdownSchedule" as const,
      value: decodeShutdownScheduleView(v),
    }),
  ],
]);

// --- per-message decoders (strict: reject unknown fields, validate required) -

const WORKSPACE_STATE_KEYS = new Set([
  "workspace",
  "sessionId",
  "state",
  "turnActive",
  "liveTaskCount",
  "causeKind",
  "causeSeq",
  "atMs",
  "connectivity",
  "status",
  "controllerGenerationId",
  "activeFaults",
  "mergeLeaseHeld",
  "mergedAtMs",
  "mergeStatus",
]);
function decodeWorkspaceState(v: unknown): WorkspaceState {
  const o = ensureObject(v, "WorkspaceState");
  rejectUnknown(o, WORKSPACE_STATE_KEYS, "WorkspaceState");
  const ws: WorkspaceState = {
    workspace: str(o, "workspace", "WorkspaceState"),
    sessionId: str(o, "sessionId", "WorkspaceState"),
    state: enumRenderState(o, "state", "WorkspaceState"),
    turnActive: bool(o, "turnActive", "WorkspaceState"),
    liveTaskCount: num(o, "liveTaskCount", "WorkspaceState"),
    causeKind: str(o, "causeKind", "WorkspaceState"),
    causeSeq: num(o, "causeSeq", "WorkspaceState"),
    atMs: num(o, "atMs", "WorkspaceState"),
    connectivity: enumSessionConnectivity(o, "connectivity", "WorkspaceState"),
    status: enumSessionStatus(o, "status", "WorkspaceState"),
    controllerGenerationId: str(o, "controllerGenerationId", "WorkspaceState"),
    activeFaults: ensureArray(o.activeFaults ?? [], "WorkspaceState.activeFaults").map(
      decodeRuntimeFault,
    ),
    mergeLeaseHeld: bool(o, "mergeLeaseHeld", "WorkspaceState"),
    mergedAtMs: num(o, "mergedAtMs", "WorkspaceState"),
  };
  // ABSENCE IS THE ABSENCE OF A MERGE, and it is the only reading: the daemon
  // leaves the field unset for a workspace whose merge axis has never spoken,
  // so there is no zero-valued status standing in for "no merge".
  if (o.mergeStatus !== undefined && o.mergeStatus !== null) {
    ws.mergeStatus = decodeMergeStatus(o.mergeStatus);
  }
  if (ws.workspace === "") {
    throw new Error("frontend-proto: WorkspaceState missing required `workspace`");
  }
  if (ws.state === RenderState.UNSPECIFIED) {
    throw new Error(
      `frontend-proto: WorkspaceState for '${ws.workspace}' has UNSPECIFIED ` +
        "render state (SSM must resolve a concrete state)",
    );
  }
  if (ws.connectivity === SessionConnectivity.UNSPECIFIED) {
    throw new Error(
      `frontend-proto: WorkspaceState for '${ws.workspace}' has UNSPECIFIED session connectivity`,
    );
  }
  if (
    ws.connectivity !== SessionConnectivity.HIBERNATED &&
    (ws.sessionId === "" || ws.controllerGenerationId === "")
  ) {
    throw new Error(
      `frontend-proto: WorkspaceState for '${ws.workspace}' has ` +
        `${SessionConnectivity[ws.connectivity]} connectivity without complete session-controller identity`,
    );
  }
  return ws;
}

const MERGE_STATUS_KEYS = new Set([
  "runId",
  "phaseStartedAtMs",
  "updatedAtMs",
  "enqueued",
  "beforeAction",
  "cherryPicking",
  "testing",
  "conflict",
  "afterAction",
  "merged",
  "failed",
]);

/**
 * The `phase` oneof, decoded by NAME.
 *
 * The map is what makes the phase set closed: a member the webapp has never
 * heard of lands in `rejectUnknown` above, and a status that names no member at
 * all is refused below. Both are wires this build cannot paint, and painting a
 * merge as "nothing in particular" is exactly the silence the status exists to
 * end.
 */
const MERGE_PHASE_DECODERS: ReadonlyMap<string, (v: unknown) => MergeStatus["phase"]> = new Map<
  string,
  (v: unknown) => MergeStatus["phase"]
>([
  [
    "enqueued",
    (v: unknown) => {
      const o = phaseObject(v, "MergeStatusEnqueued", ["position", "depth"]);
      const position = num(o, "position", "MergeStatusEnqueued");
      const depth = num(o, "depth", "MergeStatusEnqueued");
      // MOVED HERE from the retired flat merge_queue_position /
      // merge_queue_depth pair, which used to carry these checks. The figures
      // now arrive only on this arm, so this is where a nonsensical pair has to
      // be refused -- dropping the checks with the fields would have retired
      // real coverage along with the wire surface.
      if (position < 0 || depth < 0) {
        throw new Error(
          `frontend-proto: MergeStatusEnqueued has a negative merge-queue figure ` +
            `(position=${position} depth=${depth})`,
        );
      }
      // A 1-based position can never exceed the depth it indexes into. A pair
      // that says otherwise is a daemon-side accounting bug, and rendering
      // "3/2" would hide it behind a plausible-looking chip.
      if (position > depth) {
        throw new Error(
          `frontend-proto: MergeStatusEnqueued has merge-queue position ` +
            `${position} beyond depth ${depth}`,
        );
      }
      return {
        case: "enqueued" as const,
        value: { position, depth },
      };
    },
  ],
  [
    "beforeAction",
    (v: unknown) => {
      const o = phaseObject(v, "MergeStatusBeforeAction", ["prompt"]);
      return {
        case: "beforeAction" as const,
        value: { prompt: str(o, "prompt", "MergeStatusBeforeAction") },
      };
    },
  ],
  [
    "cherryPicking",
    (v: unknown) => ({
      case: "cherryPicking" as const,
      value: decodeMergeCommitProgress(v, "MergeStatusCherryPicking"),
    }),
  ],
  [
    "testing",
    (v: unknown) => ({
      case: "testing" as const,
      value: decodeMergeCommitProgress(v, "MergeStatusTesting"),
    }),
  ],
  [
    "conflict",
    (v: unknown) => {
      const o = phaseObject(v, "MergeStatusConflict", [
        "conflictedSha",
        "conflictedSubject",
        "commitsTotal",
        "commitsLanded",
      ]);
      return {
        case: "conflict" as const,
        value: {
          conflictedSha: str(o, "conflictedSha", "MergeStatusConflict"),
          conflictedSubject: str(o, "conflictedSubject", "MergeStatusConflict"),
          commitsTotal: num(o, "commitsTotal", "MergeStatusConflict"),
          commitsLanded: num(o, "commitsLanded", "MergeStatusConflict"),
        },
      };
    },
  ],
  [
    "afterAction",
    (v: unknown) => {
      const o = phaseObject(v, "MergeStatusAfterAction", ["prompt"]);
      return {
        case: "afterAction" as const,
        value: { prompt: str(o, "prompt", "MergeStatusAfterAction") },
      };
    },
  ],
  [
    "merged",
    (v: unknown) => {
      const o = phaseObject(v, "MergeStatusMerged", ["commitsTotal", "afterActionError"]);
      return {
        case: "merged" as const,
        value: {
          commitsTotal: num(o, "commitsTotal", "MergeStatusMerged"),
          afterActionError: str(o, "afterActionError", "MergeStatusMerged"),
        },
      };
    },
  ],
  [
    "failed",
    (v: unknown) => {
      const o = phaseObject(v, "MergeStatusFailed", [
        "cause",
        "commitsTotal",
        "commitsLanded",
        "failingSha",
        "failingSubject",
        "failedJson",
      ]);
      return {
        case: "failed" as const,
        value: {
          cause: str(o, "cause", "MergeStatusFailed"),
          commitsTotal: num(o, "commitsTotal", "MergeStatusFailed"),
          commitsLanded: num(o, "commitsLanded", "MergeStatusFailed"),
          failingSha: str(o, "failingSha", "MergeStatusFailed"),
          failingSubject: str(o, "failingSubject", "MergeStatusFailed"),
          failedJson: str(o, "failedJson", "MergeStatusFailed"),
        },
      };
    },
  ],
]);

function phaseObject(v: unknown, ctx: string, allowed: readonly string[]): Obj {
  const o = ensureObject(v, ctx);
  rejectUnknown(o, new Set(allowed), ctx);
  return o;
}

/**
 * cherry_picking and testing carry the identical four fields, and deliberately
 * so: testing is the same commit walk with the suite gating each landing, so a
 * renderer draws one progress bar for both.
 */
function decodeMergeCommitProgress(
  v: unknown,
  ctx: string,
): MergeStatusCherryPicking & MergeStatusTesting {
  const o = phaseObject(v, ctx, [
    "commitsTotal",
    "commitsLanded",
    "currentSha",
    "currentSubject",
  ]);
  return {
    commitsTotal: num(o, "commitsTotal", ctx),
    commitsLanded: num(o, "commitsLanded", ctx),
    currentSha: str(o, "currentSha", ctx),
    currentSubject: str(o, "currentSubject", ctx),
  };
}

function decodeMergeStatus(v: unknown): MergeStatus {
  const o = ensureObject(v, "MergeStatus");
  rejectUnknown(o, MERGE_STATUS_KEYS, "MergeStatus");
  const phaseKeys = Object.keys(o).filter((k) => MERGE_PHASE_DECODERS.has(k));
  if (phaseKeys.length === 0) {
    throw new Error(
      "frontend-proto: MergeStatus sets no phase (WHICH member of the oneof is set IS the phase)",
    );
  }
  if (phaseKeys.length > 1) {
    throw new Error(`frontend-proto: MergeStatus sets multiple phases: ${phaseKeys.join(", ")}`);
  }
  const status: MergeStatus = {
    runId: str(o, "runId", "MergeStatus"),
    phaseStartedAtMs: num(o, "phaseStartedAtMs", "MergeStatus"),
    updatedAtMs: num(o, "updatedAtMs", "MergeStatus"),
    phase: MERGE_PHASE_DECODERS.get(phaseKeys[0])!(o[phaseKeys[0]]),
  };
  if (status.runId === "") {
    throw new Error("frontend-proto: MergeStatus missing required `runId`");
  }
  return status;
}

const RUNTIME_FAULT_KEYS = new Set([
  "component",
  "faultType",
  "impact",
  "causeKind",
  "openedAtMs",
]);
function decodeRuntimeFault(v: unknown): RuntimeFault {
  const o = ensureObject(v, "RuntimeFault");
  rejectUnknown(o, RUNTIME_FAULT_KEYS, "RuntimeFault");
  const fault: RuntimeFault = {
    component: str(o, "component", "RuntimeFault"),
    faultType: str(o, "faultType", "RuntimeFault"),
    impact: str(o, "impact", "RuntimeFault"),
    causeKind: str(o, "causeKind", "RuntimeFault"),
    openedAtMs: num(o, "openedAtMs", "RuntimeFault"),
  };
  if (fault.component === "" || fault.faultType === "" || fault.impact === "") {
    throw new Error("frontend-proto: RuntimeFault missing component, faultType, or impact");
  }
  return fault;
}

const SESSION_VIEW_KEYS = new Set([
  "workspace",
  "sessionId",
  "model",
  "slug",
  "title",
  "totalTokens",
  "totalCostUsd",
  "contextWindow",
  "permissionMode",
  "shimAttached",
  "claudeSessionId",
  "cwd",
  "terminal",
  "rehydratable",
  "hibernated",
  "pendingPermissions",
  "configDir",
  "backfill",
  "death",
  "modelOptions",
  "tokenUtilization",
  "hibernation",
]);
const HIBERNATION_DETAIL_ENVELOPE_KEYS = new Set(["sinceMs"]);
const HIBERNATION_IDLE_CUTOFF_KEYS = new Set(["cutoffMs"]);
const HIBERNATION_CACHE_EXPIRED_KEYS = new Set(["elapsedMs", "ttlMs"]);
// Arm keys come from the build-checked spelling table (proto-names.ts), not
// from literals typed out here: the decoder recognizes an arm by NAME, so a
// name that drifted from the proto would make it refuse a well-formed frame.
const HIBERNATION_CAUSE_ARM_DECODERS = new Map<
  string,
  (v: unknown) => HibernationDetail["cause"]
>([
  [
    HIBERNATION_CAUSE.idleCutoff,
    (v) => ({ case: HIBERNATION_CAUSE.idleCutoff, value: decodeHibernationIdleCutoff(v) }),
  ],
  [
    HIBERNATION_CAUSE.forced,
    (v) => ({ case: HIBERNATION_CAUSE.forced, value: decodeHibernationForced(v) }),
  ],
  [
    HIBERNATION_CAUSE.cacheExpired,
    (v) => ({ case: HIBERNATION_CAUSE.cacheExpired, value: decodeHibernationCacheExpired(v) }),
  ],
]);

/**
 * Decode `HibernationDetail`.
 *
 * A cause-less detail THROWS. The gate exists to say why the session is
 * asleep, and there is no honest default: picking `forced` would tell the user
 * they did this, and picking `idleCutoff` would tell them the daemon did.
 * Absence here is a malformed frame, not a fourth cause.
 */
function decodeHibernationDetail(v: unknown): HibernationDetail {
  const ctx = "SessionView.hibernation";
  const o = ensureObject(v, ctx);
  const arms = Object.keys(o).filter((k) => HIBERNATION_CAUSE_ARM_DECODERS.has(k));
  const unknown = Object.keys(o).filter(
    (k) => !HIBERNATION_DETAIL_ENVELOPE_KEYS.has(k) && !HIBERNATION_CAUSE_ARM_DECODERS.has(k),
  );
  if (unknown.length > 0) {
    throw new Error(`frontend-proto: ${ctx} has unrecognized field(s): ${unknown.join(", ")}`);
  }
  if (arms.length === 0) {
    throw new Error(
      `frontend-proto: ${ctx} sets no cause ` +
        "(WHICH member of the oneof is set IS the reason the gate explains)",
    );
  }
  if (arms.length > 1) {
    throw new Error(`frontend-proto: ${ctx} sets multiple causes: ${arms.join(", ")}`);
  }
  return {
    sinceMs: num(o, "sinceMs", ctx),
    cause: HIBERNATION_CAUSE_ARM_DECODERS.get(arms[0])!(o[arms[0]]),
  };
}

function decodeHibernationIdleCutoff(v: unknown): HibernationIdleCutoff {
  const ctx = "SessionView.hibernation.idleCutoff";
  const o = ensureObject(v, ctx);
  rejectUnknown(o, HIBERNATION_IDLE_CUTOFF_KEYS, ctx);
  return { cutoffMs: num(o, "cutoffMs", ctx) };
}

/** The empty arm still validates: a stray field here is a contract drift. */
function decodeHibernationForced(v: unknown): HibernationForced {
  const ctx = "SessionView.hibernation.forced";
  const o = ensureObject(v, ctx);
  rejectUnknown(o, new Set<string>(), ctx);
  return {};
}

function decodeHibernationCacheExpired(v: unknown): HibernationCacheExpired {
  const ctx = "SessionView.hibernation.cacheExpired";
  const o = ensureObject(v, ctx);
  rejectUnknown(o, HIBERNATION_CACHE_EXPIRED_KEYS, ctx);
  return { elapsedMs: num(o, "elapsedMs", ctx), ttlMs: num(o, "ttlMs", ctx) };
}
function decodeSessionView(v: unknown): SessionView {
  const o = ensureObject(v, "SessionView");
  rejectUnknown(o, SESSION_VIEW_KEYS, "SessionView");
  const sv: SessionView = {
    workspace: str(o, "workspace", "SessionView"),
    sessionId: str(o, "sessionId", "SessionView"),
    model: str(o, "model", "SessionView"),
    slug: str(o, "slug", "SessionView"),
    title: str(o, "title", "SessionView"),
    totalTokens: num(o, "totalTokens", "SessionView"),
    totalCostUsd: num(o, "totalCostUsd", "SessionView"),
    contextWindow: num(o, "contextWindow", "SessionView"),
    permissionMode: str(o, "permissionMode", "SessionView"),
    shimAttached: bool(o, "shimAttached", "SessionView"),
    // Optional resume keys: absent in a SessionView that predates them decodes
    // to "" (str default), so the rebind path simply has nothing to persist.
    claudeSessionId: str(o, "claudeSessionId", "SessionView"),
    cwd: str(o, "cwd", "SessionView"),
    // S7 parity fields: default to the zero value when absent (a pre-S7 daemon
    // does not send them), so the webapp is never fed a fabricated value.
    terminal: bool(o, "terminal", "SessionView"),
    rehydratable: bool(o, "rehydratable", "SessionView"),
    hibernated: bool(o, "hibernated", "SessionView"),
    pendingPermissions: num(o, "pendingPermissions", "SessionView"),
    // S8 account identity: "" when the daemon has not resolved a config dir.
    configDir: str(o, "configDir", "SessionView"),
    modelOptions: (() => {
      if (o.modelOptions === undefined || o.modelOptions === null) {
        throw new Error("frontend-proto: SessionView missing required `modelOptions`");
      }
      return ensureArray(o.modelOptions, "SessionView.modelOptions")
        .map((model, i) => decodeModelOption(model, i));
    })(),
    // F2 never-blue signal; absent on a pre-F2 daemon, which reads as
    // `unspecified` — the same "nothing to backfill" a fresh workspace has.
    backfill: decodeBackfillState(o.backfill),
  };
  // F4 terminal-session classification: optional, present only once the
  // session died. Decoded strictly (an unknown class still throws) rather
  // than skipped, so a malformed death is loud while an absent one is normal.
  if (o.death !== undefined) {
    sv.death = decodeSystemFailure(o.death, "SessionView.death");
  }
  if (o.tokenUtilization !== undefined) sv.tokenUtilization = decodeSessionTokenUtilization(o.tokenUtilization);
  // ABSENCE IS "THE SESSION IS AWAKE", and that is its only reading. Decoded
  // when present rather than synthesized from the `hibernated` bool: the bool
  // is the compatibility projection of this message, so deriving one from the
  // other would make the webapp a second authority on a fact the daemon
  // already resolved — and it could not invent the cause anyway.
  if (o.hibernation !== undefined && o.hibernation !== null) {
    sv.hibernation = decodeHibernationDetail(o.hibernation);
  }
  if (sv.sessionId === "") {
    throw new Error("frontend-proto: SessionView missing required `session_id`");
  }
  return sv;
}

const MODEL_OPTION_KEYS = new Set(["value", "displayName", "description"]);
function decodeModelOption(v: unknown, i: number): ModelOption {
  const o = ensureObject(v, `SessionView.modelOptions[${i}]`);
  rejectUnknown(o, MODEL_OPTION_KEYS, `SessionView.modelOptions[${i}]`);
  const model = {
    value: str(o, "value", `SessionView.modelOptions[${i}]`),
    displayName: str(o, "displayName", `SessionView.modelOptions[${i}]`),
    description: str(o, "description", `SessionView.modelOptions[${i}]`),
  };
  if (model.value === "" || model.value.trim() === "<synthetic>") {
    throw new Error(`frontend-proto: SessionView.modelOptions[${i}] has no selectable model value`);
  }
  return model;
}

const CONVERSATION_DELTA_KEYS = new Set(["workspace", "sessionId", "items", "throughSeq"]);
function decodeConversationDelta(v: unknown): ConversationDelta {
  const o = ensureObject(v, "ConversationDelta");
  rejectUnknown(o, CONVERSATION_DELTA_KEYS, "ConversationDelta");
  const cd: ConversationDelta = {
    workspace: str(o, "workspace", "ConversationDelta"),
    sessionId: str(o, "sessionId", "ConversationDelta"),
    items: (o.items === undefined || o.items === null
      ? []
      : ensureArray(o.items, "ConversationDelta.items")
    ).map((item, i) => decodeConversationItem(item, i)),
    throughSeq: num(o, "throughSeq", "ConversationDelta"),
  };
  if (cd.sessionId === "") {
    throw new Error("frontend-proto: ConversationDelta missing required `session_id`");
  }
  return cd;
}

const CONVERSATION_ITEM_ENVELOPE_KEYS = new Set(["uuid", "tsMs", "requestId", "source", "tokenUtilization", "turnAccounting"]);
const CONVERSATION_ITEM_ARM_SET: ReadonlySet<string> = new Set(CONVERSATION_ITEM_ARMS);
function decodeConversationItem(v: unknown, i: number): ConversationItemFrame {
  const ctx = `ConversationItem[${i}]`;
  const o = ensureObject(v, ctx);
  const keys = Object.keys(o);
  const armKeys = keys.filter((k) => CONVERSATION_ITEM_ARM_SET.has(k));
  const unknown = keys.filter(
    (k) => !CONVERSATION_ITEM_ENVELOPE_KEYS.has(k) && !CONVERSATION_ITEM_ARM_SET.has(k),
  );
  if (unknown.length > 0) {
    throw new Error(`frontend-proto: ${ctx} has unrecognized field(s): ${unknown.join(", ")}`);
  }
  if (armKeys.length === 0) {
    throw new Error(`frontend-proto: ${ctx} carries no item variant (empty or unrecognized oneof)`);
  }
  if (armKeys.length > 1) {
    throw new Error(`frontend-proto: ${ctx} sets multiple item variants: ${armKeys.join(", ")}`);
  }
  const arm = armKeys[0] as ConversationItemArm;
  const frame: ConversationItemFrame = {
    uuid: str(o, "uuid", ctx),
    tsMs: num(o, "tsMs", ctx),
    requestId: str(o, "requestId", ctx),
    source: enumConversationSource(o, "source", ctx),
    arm,
    // Adopt the typed payload by shape (see file-top §5.1 boundary note).
    payload: ensureObject(o[arm], `${ctx}.${arm}`),
    tokenUtilization: o.tokenUtilization === undefined ? [] : ensureArray(o.tokenUtilization, `${ctx}.tokenUtilization`).map((entry, index) => decodeTokenUtilization(entry, `${ctx}.tokenUtilization[${index}]`)),
  };
  if (o.turnAccounting !== undefined) {
    if (arm !== "result") throw new Error(`frontend-proto: ${ctx}.turnAccounting is valid only on result`);
    frame.turnAccounting = decodeTurnAccounting(o.turnAccounting, `${ctx}.turnAccounting`);
  }
  return frame;
}

function int64(o: JsonObject, key: string, where: string): number {
  const value = o[key];
  if (typeof value !== "string" && typeof value !== "number") throw new Error(`frontend-proto: ${where}.${key} must be an int64 string`);
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed)) throw new Error(`frontend-proto: ${where}.${key} must be a safe integer`);
  return parsed;
}

function oneof(o: JsonObject, keys: readonly string[], where: string): string {
  const found = keys.filter((key) => o[key] !== undefined);
  if (found.length !== 1) throw new Error(`frontend-proto: ${where} requires exactly one of ${keys.join(", ")}`);
  return found[0];
}

const FINGERPRINT_KEYS = generatedFieldSet<keyof typeof EvidenceFingerprintSchema.field>()("sha256", "unavailable");
const FINGERPRINT_UNAVAILABLE_KEYS = generatedFieldSet<keyof typeof FingerprintUnavailableSchema.field>()("cause");
const RUNTIME_IDENTITY_KEYS = generatedFieldSet<keyof typeof QueryRuntimeIdentitySchema.field>()("vendorSessionId", "effectiveModel", "sdkVersion", "claudeCodeVersion", "shimBuildSha", "authSource", "subscriptionType", "fastModeState", "fastModeReason", "effectiveOptions", "settings", "tools", "mcp", "contextPrefix");
const USAGE_OBSERVATION_KEYS = generatedFieldSet<keyof typeof AccountUsageObservationSchema.field>()("queryInstanceId", "turnId", "boundaryAtMs", "observedAtMs", "sampleLatencyMs", "subscriptionType", "turnStart", "turnEnd", "available", "unavailable");
const USAGE_AVAILABLE_KEYS = generatedFieldSet<keyof typeof AccountUsageAvailableSchema.field>()("fiveHour");
const USAGE_WINDOW_KEYS = generatedFieldSet<keyof typeof UsageWindowSchema.field>()("utilizationPercent", "resetsAtMs");
const USAGE_UNAVAILABLE_KEYS = generatedFieldSet<keyof typeof AccountUsageUnavailableSchema.field>()("serviceUnavailable", "windowUnavailable", "utilizationUnavailable", "samplingFailure");
const USAGE_SAMPLING_FAILURE_KEYS = generatedFieldSet<keyof typeof UsageSamplingFailureSchema.field>()("cause");

function fingerprint(v: unknown, where: string): EvidenceFingerprint {
  const o = ensureObject(v, where); rejectUnknown(o, FINGERPRINT_KEYS, where);
  const arm = oneof(o, [...FINGERPRINT_KEYS], where);
  return arm === "sha256" ? { kind: "sha256", value: str(o, "sha256", where) } : (() => { const unavailable = ensureObject(o.unavailable, `${where}.unavailable`); rejectUnknown(unavailable, FINGERPRINT_UNAVAILABLE_KEYS, `${where}.unavailable`); return { kind: "unavailable" as const, cause: str(unavailable, "cause", `${where}.unavailable`) }; })();
}

function decodeRuntime(v: unknown, where: string): QueryRuntimeIdentity {
  const o = ensureObject(v, where); rejectUnknown(o, RUNTIME_IDENTITY_KEYS, where);
  return { vendorSessionId: str(o, "vendorSessionId", where), effectiveModel: str(o, "effectiveModel", where), sdkVersion: str(o, "sdkVersion", where), claudeCodeVersion: str(o, "claudeCodeVersion", where), shimBuildSha: str(o, "shimBuildSha", where), authSource: str(o, "authSource", where), subscriptionType: str(o, "subscriptionType", where), fastModeState: str(o, "fastModeState", where), fastModeReason: str(o, "fastModeReason", where), effectiveOptions: fingerprint(o.effectiveOptions, `${where}.effectiveOptions`), settings: fingerprint(o.settings, `${where}.settings`), tools: fingerprint(o.tools, `${where}.tools`), mcp: fingerprint(o.mcp, `${where}.mcp`), contextPrefix: fingerprint(o.contextPrefix, `${where}.contextPrefix`) };
}

function decodeUsageObservation(v: unknown, where: string): AccountUsageObservation {
  const o = ensureObject(v, where); rejectUnknown(o, USAGE_OBSERVATION_KEYS, where);
  const boundary = oneof(o, ["turnStart", "turnEnd"], where); const outcome = oneof(o, ["available", "unavailable"], where);
  const common = { queryInstanceId: str(o, "queryInstanceId", where), turnId: str(o, "turnId", where), boundaryAtMs: int64(o, "boundaryAtMs", where), observedAtMs: int64(o, "observedAtMs", where), sampleLatencyMs: int64(o, "sampleLatencyMs", where), subscriptionType: str(o, "subscriptionType", where), boundary: boundary as "turnStart" | "turnEnd" };
  if (outcome === "available") { const available = ensureObject(o.available, `${where}.available`); rejectUnknown(available, USAGE_AVAILABLE_KEYS, `${where}.available`); const five = ensureObject(available.fiveHour, `${where}.available.fiveHour`); rejectUnknown(five, USAGE_WINDOW_KEYS, `${where}.available.fiveHour`); return { ...common, outcome: { kind: "available", utilizationPercent: num(five, "utilizationPercent", `${where}.available.fiveHour`), resetsAtMs: int64(five, "resetsAtMs", `${where}.available.fiveHour`) } }; }
  const unavailable = ensureObject(o.unavailable, `${where}.unavailable`); rejectUnknown(unavailable, USAGE_UNAVAILABLE_KEYS, `${where}.unavailable`); const reason = oneof(unavailable, [...USAGE_UNAVAILABLE_KEYS], `${where}.unavailable`); if (reason === "samplingFailure") { const failure = ensureObject(unavailable.samplingFailure, `${where}.unavailable.samplingFailure`); rejectUnknown(failure, USAGE_SAMPLING_FAILURE_KEYS, `${where}.unavailable.samplingFailure`); return { ...common, outcome: { kind: "unavailable", reason: `${reason}:${str(failure, "cause", `${where}.unavailable.samplingFailure`)}` } }; } return { ...common, outcome: { kind: "unavailable", reason } };
}

const USAGE_TOTALS_KEYS = generatedFieldSet<keyof typeof TokenUsageTotalsSchema.field>()("inputTokens", "outputTokens", "cacheReadInputTokens", "cacheCreationInputTokens", "cacheCreation", "serverToolUse", "outputDetails", "cacheRates", "timing");
const RECONCILIATION_KEYS = generatedFieldSet<keyof typeof TokenUsageReconciliationSchema.field>()("responseRecordCount", "responseAllAgents", "responseMainAgent", "resultMainAgent", "responseModels", "resultModels", "apiMessageIds");
const CACHE_CREATION_KEYS = generatedFieldSet<keyof typeof TokenCacheCreationSchema.field>()("ephemeral5mInputTokens", "ephemeral1hInputTokens");
const SERVER_TOOL_USE_KEYS = generatedFieldSet<keyof typeof TokenServerToolUseSchema.field>()("webSearchRequests", "webFetchRequests");
const OUTPUT_DETAILS_KEYS = generatedFieldSet<keyof typeof TokenOutputDetailsSchema.field>()("thinkingTokens");
const CACHE_RATES_KEYS = generatedFieldSet<keyof typeof TokenCacheRatesSchema.field>()("totalPromptInputTokens", "cacheHitRate", "cacheWriteRate", "uncachedInputRate");
const TOKEN_TIMING_TOTALS_KEYS = generatedFieldSet<keyof typeof TokenTimingTotalsSchema.field>()("outputTokensWithGenerationDuration", "outputGenerationDurationMs", "responsesWithGenerationDuration", "responsesWithoutGenerationDuration", "totalTimeToFirstTokenMs", "responsesWithTimeToFirstToken", "responsesWithoutTimeToFirstToken");
const MODEL_UTILIZATION_KEYS = generatedFieldSet<keyof typeof ModelTokenUtilizationSchema.field>()("model", "canonicalModel", "provider", "totals", "contextWindow", "maxOutputTokens", "costUsd");

function projectAccountingFingerprint(value: GeneratedEvidenceFingerprint, where: string): EvidenceFingerprint {
  switch (value.evidence.case) {
    case "sha256": return { kind: "sha256", value: value.evidence.value };
    case "unavailable": return { kind: "unavailable", cause: value.evidence.value.cause };
    case undefined: throw new Error(`frontend-proto: ${where} requires a generated evidence oneof`);
  }
  return unreachableGeneratedCase(value.evidence, where);
}

function projectAccountingRuntime(value: GeneratedQueryRuntimeIdentity, where: string): QueryRuntimeIdentity {
  return {
    vendorSessionId: value.vendorSessionId, effectiveModel: value.effectiveModel, sdkVersion: value.sdkVersion,
    claudeCodeVersion: value.claudeCodeVersion, shimBuildSha: value.shimBuildSha, authSource: value.authSource,
    subscriptionType: value.subscriptionType, fastModeState: value.fastModeState, fastModeReason: value.fastModeReason,
    ...(value.effectiveOptions === undefined ? {} : { effectiveOptions: projectAccountingFingerprint(value.effectiveOptions, `${where}.effectiveOptions`) }),
    ...(value.settings === undefined ? {} : { settings: projectAccountingFingerprint(value.settings, `${where}.settings`) }),
    ...(value.tools === undefined ? {} : { tools: projectAccountingFingerprint(value.tools, `${where}.tools`) }),
    ...(value.mcp === undefined ? {} : { mcp: projectAccountingFingerprint(value.mcp, `${where}.mcp`) }),
    ...(value.contextPrefix === undefined ? {} : { contextPrefix: projectAccountingFingerprint(value.contextPrefix, `${where}.contextPrefix`) }),
  };
}

function projectAccountingUsageObservation(value: GeneratedAccountUsageObservation, where: string): AccountUsageObservation {
  const boundary = value.boundary.case === undefined ? undefined : value.boundary.case;
  let outcome: AccountUsageObservation["outcome"];
  switch (value.outcome.case) {
    case "available":
      if (value.outcome.value.fiveHour === undefined) throw new Error(`frontend-proto: ${where}.outcome.available requires fiveHour`);
      outcome = { kind: "available", utilizationPercent: value.outcome.value.fiveHour.utilizationPercent, resetsAtMs: safeGeneratedInt64(value.outcome.value.fiveHour.resetsAtMs, `${where}.outcome.available.fiveHour.resetsAtMs`) };
      break;
    case "unavailable":
      outcome = value.outcome.value.reason.case === "samplingFailure"
        ? { kind: "unavailable", reason: `samplingFailure:${value.outcome.value.reason.value.cause}` }
        : value.outcome.value.reason.case === undefined
          ? (() => { throw new Error(`frontend-proto: ${where}.outcome.unavailable requires a reason`); })()
          : { kind: "unavailable", reason: value.outcome.value.reason.case };
      break;
    case undefined: outcome = undefined; break;
  }
  return {
    queryInstanceId: value.queryInstanceId, turnId: value.turnId,
    boundaryAtMs: safeGeneratedInt64(value.boundaryAtMs, `${where}.boundaryAtMs`),
    observedAtMs: safeGeneratedInt64(value.observedAtMs, `${where}.observedAtMs`),
    sampleLatencyMs: safeGeneratedInt64(value.sampleLatencyMs, `${where}.sampleLatencyMs`),
    subscriptionType: value.subscriptionType,
    ...(boundary === undefined ? {} : { boundary }), ...(outcome === undefined ? {} : { outcome }),
  };
}

function projectAccountingUsageTotals(value: GeneratedTokenUsageTotals, where: string): UsageTotals {
  return {
    inputTokens: safeGeneratedInt64(value.inputTokens, `${where}.inputTokens`), outputTokens: safeGeneratedInt64(value.outputTokens, `${where}.outputTokens`),
    cacheReadInputTokens: safeGeneratedInt64(value.cacheReadInputTokens, `${where}.cacheReadInputTokens`), cacheCreationInputTokens: safeGeneratedInt64(value.cacheCreationInputTokens, `${where}.cacheCreationInputTokens`),
    ...(value.cacheCreation === undefined ? {} : { cacheCreation5m: safeGeneratedInt64(value.cacheCreation.ephemeral5mInputTokens, `${where}.cacheCreation.ephemeral5mInputTokens`), cacheCreation1h: safeGeneratedInt64(value.cacheCreation.ephemeral1hInputTokens, `${where}.cacheCreation.ephemeral1hInputTokens`) }),
    ...(value.serverToolUse === undefined ? {} : { webSearchRequests: safeGeneratedInt64(value.serverToolUse.webSearchRequests, `${where}.serverToolUse.webSearchRequests`), webFetchRequests: safeGeneratedInt64(value.serverToolUse.webFetchRequests, `${where}.serverToolUse.webFetchRequests`) }),
    ...(value.outputDetails === undefined ? {} : { thinkingTokens: safeGeneratedInt64(value.outputDetails.thinkingTokens, `${where}.outputDetails.thinkingTokens`) }),
    ...(value.cacheRates === undefined ? {} : { cacheRates: { totalPromptInputTokens: safeGeneratedInt64(value.cacheRates.totalPromptInputTokens, `${where}.cacheRates.totalPromptInputTokens`), cacheHitRate: value.cacheRates.cacheHitRate, cacheWriteRate: value.cacheRates.cacheWriteRate, uncachedInputRate: value.cacheRates.uncachedInputRate } }),
    ...(value.timing === undefined ? {} : { timing: { outputTokensWithGenerationDuration: safeGeneratedInt64(value.timing.outputTokensWithGenerationDuration, `${where}.timing.outputTokensWithGenerationDuration`), outputGenerationDurationMs: safeGeneratedInt64(value.timing.outputGenerationDurationMs, `${where}.timing.outputGenerationDurationMs`), responsesWithGenerationDuration: safeGeneratedInt64(value.timing.responsesWithGenerationDuration, `${where}.timing.responsesWithGenerationDuration`), responsesWithoutGenerationDuration: safeGeneratedInt64(value.timing.responsesWithoutGenerationDuration, `${where}.timing.responsesWithoutGenerationDuration`), totalTimeToFirstTokenMs: safeGeneratedInt64(value.timing.totalTimeToFirstTokenMs, `${where}.timing.totalTimeToFirstTokenMs`), responsesWithTimeToFirstToken: safeGeneratedInt64(value.timing.responsesWithTimeToFirstToken, `${where}.timing.responsesWithTimeToFirstToken`), responsesWithoutTimeToFirstToken: safeGeneratedInt64(value.timing.responsesWithoutTimeToFirstToken, `${where}.timing.responsesWithoutTimeToFirstToken`) } }),
  };
}

function projectAccountingModel(value: GeneratedModelTokenUtilization, where: string): ModelUsageTotals {
  return { model: value.model, ...(value.canonicalModel === undefined ? {} : { canonicalModel: value.canonicalModel }), ...(value.provider === undefined ? {} : { provider: value.provider }), ...(value.totals === undefined ? {} : { totals: projectAccountingUsageTotals(value.totals, `${where}.totals`) }), ...(value.contextWindow === undefined ? {} : { contextWindow: safeGeneratedInt64(value.contextWindow, `${where}.contextWindow`) }), ...(value.maxOutputTokens === undefined ? {} : { maxOutputTokens: safeGeneratedInt64(value.maxOutputTokens, `${where}.maxOutputTokens`) }), ...(value.costUsd === undefined ? {} : { costUsd: value.costUsd }) };
}

function projectAccountingReconciliation(value: GeneratedTokenUsageReconciliation, where: string): TokenUsageReconciliation {
  return { responseRecordCount: safeGeneratedInt64(value.responseRecordCount, `${where}.responseRecordCount`), ...(value.responseAllAgents === undefined ? {} : { responseAllAgents: projectAccountingUsageTotals(value.responseAllAgents, `${where}.responseAllAgents`) }), ...(value.responseMainAgent === undefined ? {} : { responseMainAgent: projectAccountingUsageTotals(value.responseMainAgent, `${where}.responseMainAgent`) }), ...(value.resultMainAgent === undefined ? {} : { resultMainAgent: projectAccountingUsageTotals(value.resultMainAgent, `${where}.resultMainAgent`) }), responseModels: value.responseModels.map((model, index) => projectAccountingModel(model, `${where}.responseModels[${index}]`)), resultModels: value.resultModels.map((model, index) => projectAccountingModel(model, `${where}.resultModels[${index}]`)), apiMessageIds: [...value.apiMessageIds] };
}

function projectAccountingProblem(value: GeneratedTurnAccountingProblem, where: string): TurnAccountingProblem {
  switch (value.problem.case) {
    case "missingUsageBoundary": {
      const boundary = value.problem.value.boundary.case;
      if (boundary === undefined) throw new Error(`frontend-proto: ${where}.missingUsageBoundary requires a generated boundary oneof`);
      return { kind: "missingUsageBoundary", boundary };
    }
    case "windowReset": return { kind: "windowReset", startResetsAtMs: safeGeneratedInt64(value.problem.value.startResetsAtMs, `${where}.windowReset.startResetsAtMs`), endResetsAtMs: safeGeneratedInt64(value.problem.value.endResetsAtMs, `${where}.windowReset.endResetsAtMs`) };
    case "tokenLedgerMismatch": return { kind: "tokenLedgerMismatch", differingFieldPaths: [...value.problem.value.differingFieldPaths] };
    case "runtimeIdentityIncomplete": return { kind: "runtimeIdentityIncomplete", missingFieldPaths: [...value.problem.value.missingFieldPaths] };
    case "unmodeledUsageFields": return { kind: "unmodeledUsageFields", sourceFieldPaths: [...value.problem.value.sourceFieldPaths] };
    case "telemetryRecordMissing": {
      switch (value.problem.value.record.case) {
        case "queryLifecycle": return { kind: "telemetryRecordMissing", record: { kind: "queryLifecycle", queryInstanceId: value.problem.value.record.value.queryInstanceId } };
        case "responseUsage": return { kind: "telemetryRecordMissing", record: { kind: "responseUsage", apiMessageId: value.problem.value.record.value.apiMessageId } };
        case "persistenceReceipt": return { kind: "telemetryRecordMissing", record: { kind: "persistenceReceipt", turnId: value.problem.value.record.value.turnId } };
        case undefined: throw new Error(`frontend-proto: ${where}.telemetryRecordMissing requires a generated record oneof`);
      }
      return unreachableGeneratedCase(value.problem.value.record, `${where}.telemetryRecordMissing`);
    }
    case undefined: throw new Error(`frontend-proto: ${where} requires a generated accounting-problem oneof`);
  }
  return unreachableGeneratedCase(value.problem, where);
}

function projectAccountingTiming(value: GeneratedTurnAccountingTiming, where: string): TurnAccountingTiming {
  return { promptAdmittedAtMs: safeGeneratedInt64(value.promptAdmittedAtMs, `${where}.promptAdmittedAtMs`), resultReceivedAtMs: safeGeneratedInt64(value.resultReceivedAtMs, `${where}.resultReceivedAtMs`), accountingSettledAtMs: safeGeneratedInt64(value.accountingSettledAtMs, `${where}.accountingSettledAtMs`), promptToResultMs: safeGeneratedInt64(value.promptToResultMs, `${where}.promptToResultMs`), resultToSettlementMs: safeGeneratedInt64(value.resultToSettlementMs, `${where}.resultToSettlementMs`) };
}

function decodeTurnAccounting(v: unknown, where: string): TurnAccounting {
  let value: GeneratedTurnAccounting;
  try { value = fromJson(TurnAccountingSchema, ensureObject(v, where) as JsonValue); }
  catch (error) { throw new Error(`frontend-proto: ${where} violates the generated TurnAccounting contract: ${error instanceof Error ? error.message : String(error)}`); }
  const consumed = { turnId: value.turnId, queryInstanceId: value.queryInstanceId, runtime: value.runtime, timing: value.timing, usageAtStart: value.usageAtStart, usageAtEnd: value.usageAtEnd, responses: value.responses, reconciliation: value.reconciliation, verdict: value.verdict } satisfies { [K in Exclude<keyof GeneratedTurnAccounting, "$typeName" | "$unknown">]: GeneratedTurnAccounting[K] };
  const evidence = { ...(consumed.runtime === undefined ? {} : { runtime: projectAccountingRuntime(consumed.runtime, `${where}.runtime`) }), ...(consumed.timing === undefined ? {} : { timing: projectAccountingTiming(consumed.timing, `${where}.timing`) }), ...(consumed.usageAtStart === undefined ? {} : { usageAtStart: projectAccountingUsageObservation(consumed.usageAtStart, `${where}.usageAtStart`) }), ...(consumed.usageAtEnd === undefined ? {} : { usageAtEnd: projectAccountingUsageObservation(consumed.usageAtEnd, `${where}.usageAtEnd`) }), responses: consumed.responses.map((response, index) => decodeTokenUtilization(toJson(TokenUtilizationSchema, response), `${where}.responses[${index}]`)), ...(consumed.reconciliation === undefined ? {} : { reconciliation: projectAccountingReconciliation(consumed.reconciliation, `${where}.reconciliation`) }) };
  switch (consumed.verdict.case) {
    case "complete": return { turnId: consumed.turnId, queryInstanceId: consumed.queryInstanceId, ...evidence, verdict: { kind: "complete" } };
    case "invalid": return { turnId: consumed.turnId, queryInstanceId: consumed.queryInstanceId, ...evidence, verdict: { kind: "invalid", problems: consumed.verdict.value.problems.map((problem, index) => projectAccountingProblem(problem, `${where}.invalid.problems[${index}]`)) } };
    case undefined: throw new Error(`frontend-proto: ${where} requires a generated verdict oneof`);
  }
  return unreachableGeneratedCase(consumed.verdict, where);
}

function safeGeneratedInt64(value: bigint, where: string): number {
  const projected = Number(value);
  if (!Number.isSafeInteger(projected)) throw new Error(`frontend-proto: ${where} exceeds the webapp's safe integer range`);
  return projected;
}

function unreachableGeneratedCase(value: never, where: string): never {
  throw new Error(`frontend-proto: ${where} has an unsupported generated oneof case ${JSON.stringify(value)}`);
}

function projectCacheCreation(value: GeneratedTokenCacheCreation, where: string): TokenCacheCreation {
  return {
    ephemeral5mInputTokens: safeGeneratedInt64(value.ephemeral5mInputTokens, `${where}.ephemeral5mInputTokens`),
    ephemeral1hInputTokens: safeGeneratedInt64(value.ephemeral1hInputTokens, `${where}.ephemeral1hInputTokens`),
  };
}

function projectServerToolUse(value: GeneratedTokenServerToolUse, where: string): TokenServerToolUse {
  return {
    webSearchRequests: safeGeneratedInt64(value.webSearchRequests, `${where}.webSearchRequests`),
    webFetchRequests: safeGeneratedInt64(value.webFetchRequests, `${where}.webFetchRequests`),
  };
}

function projectOutputDetails(value: GeneratedTokenOutputDetails, where: string): TokenOutputDetails {
  return { thinkingTokens: safeGeneratedInt64(value.thinkingTokens, `${where}.thinkingTokens`) };
}

function projectCacheRates(value: GeneratedTokenCacheRates, where: string): TokenCacheRates {
  return {
    totalPromptInputTokens: safeGeneratedInt64(value.totalPromptInputTokens, `${where}.totalPromptInputTokens`),
    cacheHitRate: value.cacheHitRate,
    cacheWriteRate: value.cacheWriteRate,
    uncachedInputRate: value.uncachedInputRate,
  };
}

type GeneratedIterationValue = Exclude<GeneratedTokenUsageIteration["iteration"], { case: undefined }>["value"];

function projectIterationCounters(value: GeneratedIterationValue, where: string): TokenUsageIterationCounters {
  return {
    inputTokens: safeGeneratedInt64(value.inputTokens, `${where}.inputTokens`),
    outputTokens: safeGeneratedInt64(value.outputTokens, `${where}.outputTokens`),
    cacheReadInputTokens: safeGeneratedInt64(value.cacheReadInputTokens, `${where}.cacheReadInputTokens`),
    cacheCreationInputTokens: safeGeneratedInt64(value.cacheCreationInputTokens, `${where}.cacheCreationInputTokens`),
    ...(value.cacheCreation === undefined ? {} : { cacheCreation: projectCacheCreation(value.cacheCreation, `${where}.cacheCreation`) }),
  };
}

function projectUsageIteration(value: GeneratedTokenUsageIteration, where: string): TokenUsageIteration {
  const arm = value.iteration;
  switch (arm.case) {
    case "sampling":
      return { kind: "sampling", ...projectIterationCounters(arm.value, `${where}.sampling`), model: arm.value.model };
    case "compaction":
      return { kind: "compaction", ...projectIterationCounters(arm.value, `${where}.compaction`) };
    case "advisor":
      return { kind: "advisor", ...projectIterationCounters(arm.value, `${where}.advisor`), model: arm.value.model };
    case "fallback":
      return { kind: "fallback", ...projectIterationCounters(arm.value, `${where}.fallback`), model: arm.value.model };
    case undefined:
      throw new Error(`frontend-proto: ${where} requires a generated iteration oneof`);
  }
  return unreachableGeneratedCase(arm, where);
}

function projectCacheDiagnostic(value: GeneratedTokenCacheDiagnostic, where: string): TokenCacheDiagnostic {
  const arm = value.reason;
  switch (arm.case) {
    case "pending":
    case "previousMessageUnavailable":
    case "diagnosticsUnavailable":
      return { kind: arm.case };
    case "modelChanged":
    case "systemChanged":
    case "toolsChanged":
    case "messagesChanged":
      return { kind: arm.case, cacheMissedInputTokens: safeGeneratedInt64(arm.value.cacheMissedInputTokens, `${where}.${arm.case}.cacheMissedInputTokens`) };
    case undefined:
      throw new Error(`frontend-proto: ${where} requires a generated cache-diagnostic oneof`);
  }
  return unreachableGeneratedCase(arm, where);
}

function projectResponseTokenUsage(value: GeneratedTokenUsage, where: string): ResponseTokenUsage {
  if (value.rawUsage === undefined) throw new Error(`frontend-proto: ${where}.rawUsage is required`);
  const rawUsage = ensureObject(toJson(ApiUsageSchema, value.rawUsage), `${where}.rawUsage`);
  const projected = {
    inputTokens: safeGeneratedInt64(value.inputTokens, `${where}.inputTokens`),
    outputTokens: safeGeneratedInt64(value.outputTokens, `${where}.outputTokens`),
    cacheReadInputTokens: safeGeneratedInt64(value.cacheReadInputTokens, `${where}.cacheReadInputTokens`),
    cacheCreationInputTokens: safeGeneratedInt64(value.cacheCreationInputTokens, `${where}.cacheCreationInputTokens`),
    cacheCreation: value.cacheCreation === undefined ? undefined : projectCacheCreation(value.cacheCreation, `${where}.cacheCreation`),
    serverToolUse: value.serverToolUse === undefined ? undefined : projectServerToolUse(value.serverToolUse, `${where}.serverToolUse`),
    serviceTier: value.serviceTier,
    speed: value.speed,
    inferenceGeo: value.inferenceGeo,
    outputDetails: value.outputDetails === undefined ? undefined : projectOutputDetails(value.outputDetails, `${where}.outputDetails`),
    iterations: value.iterations.map((entry, index) => projectUsageIteration(entry, `${where}.iterations[${index}]`)),
    cacheDiagnostic: value.cacheDiagnostic === undefined ? undefined : projectCacheDiagnostic(value.cacheDiagnostic, `${where}.cacheDiagnostic`),
    cacheRates: value.cacheRates === undefined ? undefined : projectCacheRates(value.cacheRates, `${where}.cacheRates`),
    fallbackCredit: value.fallbackCredit,
    unmodeledUsage: value.unmodeledUsage,
    rawUsage,
  } satisfies { [K in Exclude<keyof GeneratedTokenUsage, "$typeName" | "$unknown">]-?: K extends keyof ResponseTokenUsage ? ResponseTokenUsage[K] : never };
  return {
    inputTokens: projected.inputTokens,
    outputTokens: projected.outputTokens,
    cacheReadInputTokens: projected.cacheReadInputTokens,
    cacheCreationInputTokens: projected.cacheCreationInputTokens,
    ...(projected.cacheCreation === undefined ? {} : { cacheCreation: projected.cacheCreation }),
    ...(projected.serverToolUse === undefined ? {} : { serverToolUse: projected.serverToolUse }),
    serviceTier: projected.serviceTier,
    speed: projected.speed,
    inferenceGeo: projected.inferenceGeo,
    ...(projected.outputDetails === undefined ? {} : { outputDetails: projected.outputDetails }),
    iterations: projected.iterations,
    ...(projected.cacheDiagnostic === undefined ? {} : { cacheDiagnostic: projected.cacheDiagnostic }),
    ...(projected.cacheRates === undefined ? {} : { cacheRates: projected.cacheRates }),
    ...(projected.fallbackCredit === undefined ? {} : { fallbackCredit: projected.fallbackCredit }),
    ...(projected.unmodeledUsage === undefined ? {} : { unmodeledUsage: projected.unmodeledUsage }),
    rawUsage: projected.rawUsage,
  };
}

/** Parse the schema-owned contract through generated protobuf types before projecting a web DTO. */
function decodeTokenUtilization(v: unknown, where: string): TokenUtilization {
  const o = ensureObject(v, where);
  let generated: GeneratedTokenUtilization;
  try {
    generated = fromJson(TokenUtilizationSchema, o as JsonValue);
  } catch (error) {
    throw new Error(`frontend-proto: ${where} violates the generated TokenUtilization contract: ${error instanceof Error ? error.message : String(error)}`);
  }
  const consumed = {
    agentReplSessionId: generated.agentReplSessionId,
    claudeSessionId: generated.claudeSessionId,
    rootTurnId: generated.rootTurnId,
    apiRequestId: generated.apiRequestId,
    apiMessageId: generated.apiMessageId,
    model: generated.model,
    actor: generated.actor,
    usage: generated.usage,
    responseTiming: generated.responseTiming,
  } satisfies { [K in Exclude<keyof GeneratedTokenUtilization, "$typeName" | "$unknown">]: GeneratedTokenUtilization[K] };
  if (consumed.usage === undefined) throw new Error(`frontend-proto: ${where}.usage is required`);
  const actor = consumed.actor;
  const result: TokenUtilization = {
    agentReplSessionId: consumed.agentReplSessionId,
    claudeSessionId: consumed.claudeSessionId,
    rootTurnId: consumed.rootTurnId,
    ...(consumed.apiRequestId === undefined ? {} : { apiRequestId: consumed.apiRequestId }),
    apiMessageId: consumed.apiMessageId,
    model: consumed.model,
    actor: actor.case === "subagent" ? "subagent" : "mainAgent",
    usage: projectResponseTokenUsage(consumed.usage, `${where}.usage`),
  };
  for (const [field, value] of [
    ["agentReplSessionId", result.agentReplSessionId],
    ["claudeSessionId", result.claudeSessionId],
    ["rootTurnId", result.rootTurnId],
    ["apiMessageId", result.apiMessageId],
  ] as const) {
    if (value === "") throw new Error(`frontend-proto: ${where}.${field} must be nonblank`);
  }
  if (result.apiRequestId === "") throw new Error(`frontend-proto: ${where}.apiRequestId must be absent or nonblank`);
  switch (actor.case) {
    case "mainAgent":
      break;
    case "subagent":
      result.subagent = { agentId: actor.value.agentId, parentToolUseId: actor.value.parentToolUseId, parentAgentId: actor.value.parentAgentId, subagentType: actor.value.subagentType, taskDescription: actor.value.taskDescription };
      break;
    case undefined:
      throw new Error(`frontend-proto: ${where} requires a generated actor oneof`);
    default:
      unreachableGeneratedCase(actor, where);
  }
  if (consumed.responseTiming !== undefined) {
    const timing = consumed.responseTiming;
    result.responseTiming = {
      ...(timing.timeToFirstTokenMs === undefined ? {} : { timeToFirstTokenMs: safeGeneratedInt64(timing.timeToFirstTokenMs, `${where}.responseTiming.timeToFirstTokenMs`) }),
      ...(timing.outputGenerationDurationMs === undefined ? {} : { outputGenerationDurationMs: safeGeneratedInt64(timing.outputGenerationDurationMs, `${where}.responseTiming.outputGenerationDurationMs`) }),
    };
  }
  return result;
}

const HEARTBEAT_VIEW_KEYS = new Set(["workspace", "sessionId", "progress"]);
const HEARTBEAT_PROGRESS_KEYS = new Set([
  "toolUseId",
  "toolName",
  "parentToolUseId",
  "elapsedSeconds",
]);

function decodeHeartbeatView(v: unknown): HeartbeatView {
  const o = ensureObject(v, "HeartbeatView");
  rejectUnknown(o, HEARTBEAT_VIEW_KEYS, "HeartbeatView");
  if (o.progress === undefined || o.progress === null) {
    throw new Error("frontend-proto: HeartbeatView missing required `progress`");
  }
  const p = ensureObject(o.progress, "HeartbeatView.progress");
  rejectUnknown(p, HEARTBEAT_PROGRESS_KEYS, "HeartbeatView.progress");
  const hv: HeartbeatView = {
    workspace: str(o, "workspace", "HeartbeatView"),
    sessionId: str(o, "sessionId", "HeartbeatView"),
    toolUseId: str(p, "toolUseId", "HeartbeatView.progress"),
    toolName: str(p, "toolName", "HeartbeatView.progress"),
    parentToolUseId: str(p, "parentToolUseId", "HeartbeatView.progress"),
    elapsedSeconds: num(p, "elapsedSeconds", "HeartbeatView.progress"),
  };
  // Without a tool_use_id there is no call to attribute the liveness to, so the
  // frame is unusable rather than merely empty.
  if (hv.toolUseId === "") {
    throw new Error("frontend-proto: HeartbeatView.progress missing required `toolUseId`");
  }
  return hv;
}

const QUEUE_VIEW_KEYS = new Set(["workspace", "sessionId", "entries"]);
const QUEUE_ENTRY_KEYS = new Set([
  "id",
  "text",
  "queuedAtMs",
  "classification",
  "rationale",
  "accepted",
  "shutdownHold",
  QUEUE_ENTRY_KEEP_ALIVE_HOLD,
  QUEUE_ENTRY_REVIVAL_HOLD,
]);
const QUEUE_ENTRY_SHUTDOWN_HOLD_KEYS = new Set(["scheduleId"]);
const QUEUE_ENTRY_KEEP_ALIVE_HOLD_KEYS = new Set([KEEP_ALIVE_HOLD_TURN_ID]);
const QUEUE_ENTRY_REVIVAL_HOLD_KEYS = new Set([REVIVAL_HOLD_SESSION_ID]);

function decodeQueueView(v: unknown): QueueView {
  const o = ensureObject(v, "QueueView");
  rejectUnknown(o, QUEUE_VIEW_KEYS, "QueueView");
  const qv: QueueView = {
    workspace: str(o, "workspace", "QueueView"),
    sessionId: str(o, "sessionId", "QueueView"),
    entries: (o.entries === undefined || o.entries === null
      ? []
      : ensureArray(o.entries, "QueueView.entries")
    ).map(decodeQueueEntry),
  };
  if (qv.sessionId === "") {
    throw new Error("frontend-proto: QueueView missing required `session_id`");
  }
  return qv;
}

function decodeQueueEntry(v: unknown): QueueEntry {
  const o = ensureObject(v, "QueueView.entries[]");
  rejectUnknown(o, QUEUE_ENTRY_KEYS, "QueueView.entries[]");
  const e: QueueEntry = {
    id: str(o, "id", "QueueView.entries[]"),
    text: str(o, "text", "QueueView.entries[]"),
    queuedAtMs: num(o, "queuedAtMs", "QueueView.entries[]"),
    classification: decodeQueueClassification(o.classification),
    rationale: str(o, "rationale", "QueueView.entries[]"),
    accepted: bool(o, "accepted", "QueueView.entries[]"),
  };
  // Without an id nothing can force, accept or cancel this entry, so the
  // controls it renders would all be dead.
  if (e.id === "") {
    throw new Error("frontend-proto: QueueView entry missing required `id`");
  }
  // ABSENCE IS THE ABSENCE OF A LEASE HOLD, and that is its only reading: an
  // ordinary classifier-held entry simply does not carry the field. Decoded
  // when present rather than defaulted away, so the lease bubble is drawn from
  // the daemon's own claim and never from an inference about the classifier.
  if (o.shutdownHold !== undefined && o.shutdownHold !== null) {
    e.shutdownHold = decodeQueueEntryShutdownHold(o.shutdownHold);
  }
  // Same reading as the lease hold above: absence is the absence of a
  // keep-alive hold. Decoded from the daemon's own claim, never inferred from
  // the classification the classifier deliberately never produced.
  if (
    o[QUEUE_ENTRY_KEEP_ALIVE_HOLD] !== undefined &&
    o[QUEUE_ENTRY_KEEP_ALIVE_HOLD] !== null
  ) {
    e.keepAliveHold = decodeQueueEntryKeepAliveHold(o[QUEUE_ENTRY_KEEP_ALIVE_HOLD]);
  }
  // Same reading again: absence is the absence of a revival hold. The daemon's
  // own claim is what puts the "waiting on the revival's compaction" bubble on
  // screen — the classifier never ran on this entry either.
  if (o[QUEUE_ENTRY_REVIVAL_HOLD] !== undefined && o[QUEUE_ENTRY_REVIVAL_HOLD] !== null) {
    e.revivalHold = decodeQueueEntryRevivalHold(o[QUEUE_ENTRY_REVIVAL_HOLD]);
  }
  return e;
}

function decodeQueueEntryRevivalHold(v: unknown): QueueEntryRevivalHold {
  const o = ensureObject(v, "QueueEntryRevivalHold");
  rejectUnknown(o, QUEUE_ENTRY_REVIVAL_HOLD_KEYS, "QueueEntryRevivalHold");
  const sessionId = str(o, REVIVAL_HOLD_SESSION_ID, "QueueEntryRevivalHold");
  // The session id is the whole content of the message: it names the revival
  // whose completed compaction releases this entry. A hold naming no session
  // would claim the prompt waits on something nothing else on screen can
  // corroborate.
  if (sessionId === "") {
    throw new Error("frontend-proto: QueueEntryRevivalHold missing required `sessionId`");
  }
  return { sessionId };
}

function decodeQueueEntryKeepAliveHold(v: unknown): QueueEntryKeepAliveHold {
  const o = ensureObject(v, "QueueEntryKeepAliveHold");
  rejectUnknown(o, QUEUE_ENTRY_KEEP_ALIVE_HOLD_KEYS, "QueueEntryKeepAliveHold");
  const turnId = str(o, KEEP_ALIVE_HOLD_TURN_ID, "QueueEntryKeepAliveHold");
  // The turn id is the whole content of the message: it names the ping whose
  // completion releases this entry. A hold naming no turn would claim the
  // prompt is waiting on something nothing else on screen can corroborate.
  if (turnId === "") {
    throw new Error("frontend-proto: QueueEntryKeepAliveHold missing required `turnId`");
  }
  return { turnId };
}

function decodeQueueEntryShutdownHold(v: unknown): QueueEntryShutdownHold {
  const o = ensureObject(v, "QueueEntryShutdownHold");
  rejectUnknown(o, QUEUE_ENTRY_SHUTDOWN_HOLD_KEYS, "QueueEntryShutdownHold");
  const scheduleId = str(o, "scheduleId", "QueueEntryShutdownHold");
  // The id is the whole content of the message: it joins this bubble to the
  // lease view that explains it. A hold that names no schedule would render a
  // bubble claiming a bounce nothing on screen can corroborate.
  if (scheduleId === "") {
    throw new Error("frontend-proto: QueueEntryShutdownHold missing required `scheduleId`");
  }
  return { scheduleId };
}

// --- the drain lease --------------------------------------------------------

const SHUTDOWN_SCHEDULE_VIEW_KEYS = new Set(["idle", "draining"]);
const SHUTDOWN_DRAINING_KEYS = new Set([
  "scheduleId",
  "scheduledAtMs",
  "cause",
  "stopShims",
  "holds",
]);
const SHUTDOWN_HOLD_KEYS = new Set(["workspace", "sessionId", "turn", "tasks"]);

/**
 * The `state` oneof, decoded by NAME — the same discipline `MergeStatus` uses.
 * The map is what makes the arm set closed, and a view naming no arm at all is
 * refused below: `idle` is a REAL value the daemon broadcasts, so a view with
 * nothing set is not "idle by omission", it is a frame the webapp cannot read.
 */
const SHUTDOWN_SCHEDULE_ARM_DECODERS: ReadonlyMap<
  string,
  (v: unknown) => ShutdownScheduleView["state"]
> = new Map<string, (v: unknown) => ShutdownScheduleView["state"]>([
  [
    "idle",
    (v: unknown) => {
      // Empty by design, and still validated: an `idle` carrying fields is a
      // daemon saying something this build cannot read.
      phaseObject(v, "ShutdownScheduleIdle", []);
      return { case: "idle" as const, value: {} as ShutdownScheduleIdle };
    },
  ],
  [
    "draining",
    (v: unknown) => ({
      case: "draining" as const,
      value: decodeShutdownScheduleDraining(v),
    }),
  ],
]);

function decodeShutdownScheduleView(v: unknown): ShutdownScheduleView {
  const o = ensureObject(v, "ShutdownScheduleView");
  rejectUnknown(o, SHUTDOWN_SCHEDULE_VIEW_KEYS, "ShutdownScheduleView");
  const arms = Object.keys(o).filter((k) => SHUTDOWN_SCHEDULE_ARM_DECODERS.has(k));
  if (arms.length === 0) {
    throw new Error(
      "frontend-proto: ShutdownScheduleView sets no state " +
        "(WHICH member of the oneof is set IS the lease state; `idle` is a real value)",
    );
  }
  if (arms.length > 1) {
    throw new Error(
      `frontend-proto: ShutdownScheduleView sets multiple states: ${arms.join(", ")}`,
    );
  }
  return { state: SHUTDOWN_SCHEDULE_ARM_DECODERS.get(arms[0])!(o[arms[0]]) };
}

function decodeShutdownScheduleDraining(v: unknown): ShutdownScheduleDraining {
  const o = ensureObject(v, "ShutdownScheduleDraining");
  rejectUnknown(o, SHUTDOWN_DRAINING_KEYS, "ShutdownScheduleDraining");
  const draining: ShutdownScheduleDraining = {
    scheduleId: str(o, "scheduleId", "ShutdownScheduleDraining"),
    scheduledAtMs: num(o, "scheduledAtMs", "ShutdownScheduleDraining"),
    cause: str(o, "cause", "ShutdownScheduleDraining"),
    stopShims: bool(o, "stopShims", "ShutdownScheduleDraining"),
    holds: (o.holds === undefined || o.holds === null
      ? []
      : ensureArray(o.holds, "ShutdownScheduleDraining.holds")
    ).map(decodeShutdownHold),
  };
  // Without the id no cancel can name this schedule and no held prompt can be
  // joined to it, so every control the banner and the lease bubble draw would
  // be aimed at nothing.
  if (draining.scheduleId === "") {
    throw new Error("frontend-proto: ShutdownScheduleDraining missing required `scheduleId`");
  }
  // The banner's elapsed clock counts from this stamp. A zero (proto3's
  // omitted value) would render the drain as decades old, which is a
  // fabricated reading, not a missing one.
  if (draining.scheduledAtMs <= 0) {
    throw new Error(
      `frontend-proto: ShutdownScheduleDraining has non-positive scheduledAtMs ` +
        `(${draining.scheduledAtMs}) for schedule ${draining.scheduleId}`,
    );
  }
  // NEVER EMPTY ON THE WIRE: the daemon executes the shutdown the moment the
  // last hold clears rather than broadcasting a drained lease. An empty list
  // would paint a banner saying the bounce is waiting on nothing at all.
  if (draining.holds.length === 0) {
    throw new Error(
      `frontend-proto: ShutdownScheduleDraining for schedule ${draining.scheduleId} ` +
        "carries an empty holds list (a drained lease is executed, never broadcast)",
    );
  }
  return draining;
}

function decodeShutdownHold(v: unknown): ShutdownHold {
  const o = ensureObject(v, "ShutdownHold");
  rejectUnknown(o, SHUTDOWN_HOLD_KEYS, "ShutdownHold");
  const hold: ShutdownHold = {
    workspace: str(o, "workspace", "ShutdownHold"),
    sessionId: str(o, "sessionId", "ShutdownHold"),
  };
  if (o.turn !== undefined && o.turn !== null) {
    const t = phaseObject(o.turn, "ShutdownHoldTurn", ["turnId"]);
    const turnId = str(t, "turnId", "ShutdownHoldTurn");
    // Naming the turn is the message's entire purpose (logs, the webapp and
    // the turn ledger have to name the same turn), so an unnamed one is
    // malformed rather than merely terse.
    if (turnId === "") {
      throw new Error("frontend-proto: ShutdownHoldTurn missing required `turnId`");
    }
    hold.turn = { turnId };
  }
  if (o.tasks !== undefined && o.tasks !== null) {
    const t = phaseObject(o.tasks, "ShutdownHoldTasks", ["count"]);
    const count = num(t, "count", "ShutdownHoldTasks");
    // The arm is set when live tasks are RUNNING. A non-positive count denies
    // the very fact its presence asserts.
    if (count <= 0) {
      throw new Error(
        `frontend-proto: ShutdownHoldTasks has non-positive count (${count}) ` +
          "on a hold that claims live background tasks",
      );
    }
    hold.tasks = { count };
  }
  // The workspace is the join key a client attributes the hold by, and the
  // session id is what keeps a hold from being pinned on a later independent
  // session of the same workspace.
  if (hold.workspace === "" || hold.sessionId === "") {
    throw new Error("frontend-proto: ShutdownHold missing `workspace` or `sessionId`");
  }
  // AT LEAST ONE reason is always set. A hold that names neither says the
  // drain is waiting on this workspace for no expressible reason, which the
  // banner could only render as an unexplained blocker.
  if (hold.turn === undefined && hold.tasks === undefined) {
    throw new Error(
      `frontend-proto: ShutdownHold for '${hold.workspace}' names neither a turn nor tasks`,
    );
  }
  return hold;
}

/**
 * Decode the classification enum. An UNRECOGNIZED name throws rather than
 * falling back: silently rendering an unknown verdict as `pending` would tell
 * the user their prompt is being judged when the daemon said something else
 * entirely.
 *
 * ABSENT / UNSPECIFIED also throws. `QUEUE_CLASSIFICATION_UNSPECIFIED` is the
 * proto3 zero, so protojson OMITS the field when it holds that value — absent
 * and UNSPECIFIED are the same wire fact, and both are handled here the same
 * way. The daemon sets one of the four real states on every entry it builds
 * (PENDING at enqueue, then the verdict), so neither can occur against a
 * correct daemon; seeing one means the frame is malformed. Defaulting it to
 * `pending` would invent the very claim the wire declined to make, which is
 * the same failure the unrecognized-name branch exists to prevent.
 */
function decodeQueueClassification(v: unknown): QueueClassification {
  if (v === undefined || v === null) {
    throw new Error(
      "frontend-proto: QueueView entry has no classification " +
        "(absent === QUEUE_CLASSIFICATION_UNSPECIFIED, which the daemon never sends)",
    );
  }
  if (typeof v !== "string") {
    throw new Error(`frontend-proto: QueueView entry classification must be a string (got ${typeof v})`);
  }
  if (v === "QUEUE_CLASSIFICATION_UNSPECIFIED") {
    throw new Error("frontend-proto: QueueView entry classification is UNSPECIFIED, which the daemon never sends");
  }
  const known = QUEUE_CLASSIFICATION_BY_NAME[v];
  if (known === undefined) {
    throw new Error(`frontend-proto: QueueView entry has unrecognized classification '${v}'`);
  }
  return known;
}

const TYPING_DELTA_KEYS = new Set(["workspace", "sessionId", "delta"]);
const CONTENT_DELTA_KEYS = new Set([
  "uuid",
  "blockIndex",
  "text",
  "thinking",
  "inputJson",
  "signature",
  "estimatedTokens",
]);
/** ContentDelta oneof arm key → the store's normalized kind. */
const CONTENT_DELTA_ARM_KIND: Readonly<Record<string, ContentDeltaKind>> = {
  text: "text",
  thinking: "thinking",
  inputJson: "input_json",
  signature: "signature",
};
function decodeTypingDelta(v: unknown): TypingDelta {
  const o = ensureObject(v, "TypingDelta");
  rejectUnknown(o, TYPING_DELTA_KEYS, "TypingDelta");
  if (o.delta === undefined || o.delta === null) {
    throw new Error("frontend-proto: TypingDelta missing required `delta`");
  }
  const d = ensureObject(o.delta, "TypingDelta.delta");
  rejectUnknown(d, CONTENT_DELTA_KEYS, "TypingDelta.delta");
  const armKeys = Object.keys(d).filter((k) => k in CONTENT_DELTA_ARM_KIND);
  if (armKeys.length === 0) {
    throw new Error("frontend-proto: TypingDelta.delta carries no content delta (empty oneof)");
  }
  if (armKeys.length > 1) {
    throw new Error(
      `frontend-proto: TypingDelta.delta sets multiple content deltas: ${armKeys.join(", ")}`,
    );
  }
  const armKey = armKeys[0];
  const td: TypingDelta = {
    workspace: str(o, "workspace", "TypingDelta"),
    sessionId: str(o, "sessionId", "TypingDelta"),
    uuid: str(d, "uuid", "TypingDelta.delta"),
    blockIndex: num(d, "blockIndex", "TypingDelta.delta"),
    kind: CONTENT_DELTA_ARM_KIND[armKey],
    delta: str(d, armKey, "TypingDelta.delta"),
    estimatedTokens: num(d, "estimatedTokens", "TypingDelta.delta"),
  };
  if (td.uuid === "") {
    throw new Error("frontend-proto: TypingDelta.delta missing required `uuid`");
  }
  return td;
}

const SESSION_INIT_VIEW_KEYS = new Set(["workspace", "sessionId", "init"]);
function decodeSessionInitView(v: unknown): SessionInitView {
  const o = ensureObject(v, "SessionInitView");
  rejectUnknown(o, SESSION_INIT_VIEW_KEYS, "SessionInitView");
  const siv: SessionInitView = {
    workspace: str(o, "workspace", "SessionInitView"),
    sessionId: str(o, "sessionId", "SessionInitView"),
    // The SystemInit is adopted by shape (large, additive); an absent init is {}.
    init: o.init === undefined || o.init === null ? {} : ensureObject(o.init, "SessionInitView.init"),
  };
  if (siv.sessionId === "") {
    throw new Error("frontend-proto: SessionInitView missing required `session_id`");
  }
  return siv;
}

const TASK_ENTRY_KEYS = new Set([
  "taskId",
  "kind",
  "description",
  "status",
  "outputPath",
  "startedAtMs",
  "endedAtMs",
]);
function decodeTaskEntry(v: unknown, i: number): TaskEntry {
  const o = ensureObject(v, `TaskEntry[${i}]`);
  rejectUnknown(o, TASK_ENTRY_KEYS, `TaskEntry[${i}]`);
  const t: TaskEntry = {
    taskId: str(o, "taskId", "TaskEntry"),
    kind: str(o, "kind", "TaskEntry"),
    description: str(o, "description", "TaskEntry"),
    status: str(o, "status", "TaskEntry"),
    outputPath: str(o, "outputPath", "TaskEntry"),
    startedAtMs: num(o, "startedAtMs", "TaskEntry"),
    endedAtMs: num(o, "endedAtMs", "TaskEntry"),
  };
  if (t.taskId === "") {
    throw new Error("frontend-proto: TaskEntry missing required `task_id`");
  }
  if (!(TASK_KINDS as readonly string[]).includes(t.kind)) {
    throw new Error(
      `frontend-proto: TaskEntry '${t.taskId}' has unknown kind '${t.kind}' ` +
        `(expected one of ${TASK_KINDS.join(", ")})`,
    );
  }
  if (!(TASK_STATUSES as readonly string[]).includes(t.status)) {
    throw new Error(
      `frontend-proto: TaskEntry '${t.taskId}' has unknown status '${t.status}' ` +
        `(expected one of ${TASK_STATUSES.join(", ")})`,
    );
  }
  return t;
}

const TASK_CATALOG_KEYS = new Set(["workspace", "sessionId", "tasks"]);
function decodeTaskCatalog(v: unknown): TaskCatalog {
  const o = ensureObject(v, "TaskCatalog");
  rejectUnknown(o, TASK_CATALOG_KEYS, "TaskCatalog");
  const tc: TaskCatalog = {
    workspace: str(o, "workspace", "TaskCatalog"),
    sessionId: str(o, "sessionId", "TaskCatalog"),
    tasks: (o.tasks === undefined || o.tasks === null ? [] : ensureArray(o.tasks, "TaskCatalog.tasks")).map(
      (t, i) => decodeTaskEntry(t, i),
    ),
  };
  if (tc.sessionId === "") {
    throw new Error("frontend-proto: TaskCatalog missing required `session_id`");
  }
  return tc;
}

const SYSTEM_FAILURE_KEYS = generatedFieldSet<keyof typeof SystemFailureItemSchema.field>()("errorClass", "errorType", "message", "sourceDetail", "resolvedAtMs", "itemUuid", "sessionResume", "queryTermination");
const SESSION_RESUME_KEYS = generatedFieldSet<keyof typeof SessionResumeFailureSchema.field>()("agentReplSessionId", "claudeSessionId", "cwd", "configDir", "resolvedConfigDir", "create", "automaticRestore", "transcriptUnavailable", "identityMismatch", "queryTermination", "bringUpFailure");
const SESSION_RESUME_CREATE_KEYS = generatedFieldSet<keyof typeof SessionResumeFailureCreateSchema.field>()();
const SESSION_RESUME_AUTOMATIC_KEYS = generatedFieldSet<keyof typeof SessionResumeFailureAutomaticRestoreSchema.field>()();
const SESSION_RESUME_TRANSCRIPT_KEYS = generatedFieldSet<keyof typeof SessionResumeFailureTranscriptUnavailableSchema.field>()("searchedPaths");
const SESSION_RESUME_IDENTITY_KEYS = generatedFieldSet<keyof typeof SessionResumeFailureIdentityMismatchSchema.field>()("replacementClaudeSessionId");
const SESSION_RESUME_BRING_UP_KEYS = generatedFieldSet<keyof typeof SessionResumeFailureBringUpFailureSchema.field>()("cause");

function decodeSessionTokenUtilization(v: unknown): SessionTokenUtilization {
  let generated: SessionTokenUtilization;
  try {
    generated = fromJson(SessionTokenUtilizationSchema, ensureObject(v, "SessionTokenUtilization") as JsonValue);
  } catch (error) {
    throw new Error(`frontend-proto: SessionTokenUtilization violates its generated contract: ${error instanceof Error ? error.message : String(error)}`);
  }
  if (generated.allAgents === undefined || generated.mainAgent === undefined) {
    throw new Error("frontend-proto: SessionTokenUtilization requires allAgents and mainAgent totals");
  }
  for (const [index, entry] of generated.subagents.entries()) {
    if (entry.agent === undefined || entry.totals === undefined) {
      throw new Error(`frontend-proto: SessionTokenUtilization.subagents[${index}] requires agent and totals`);
    }
    if (entry.agent.agentId === "" && entry.agent.parentToolUseId === "") {
      throw new Error(`frontend-proto: SessionTokenUtilization.subagents[${index}] lacks a stable invocation identity`);
    }
  }
  for (const [index, model] of generated.models.entries()) {
    if (model.model === "" || model.totals === undefined) {
      throw new Error(`frontend-proto: SessionTokenUtilization.models[${index}] requires model identity and totals`);
    }
  }
  const seenApiMessageIds = new Set<string>();
  for (const [index, response] of generated.ungroupedSubagentResponses.entries()) {
    if (response.actor.case !== "subagent") throw new Error(`frontend-proto: SessionTokenUtilization.ungroupedSubagentResponses[${index}] must be a subagent response`);
    if (response.actor.value.agentId !== "" || response.actor.value.parentToolUseId !== "") throw new Error(`frontend-proto: SessionTokenUtilization.ungroupedSubagentResponses[${index}] has a stable invocation identity`);
    if (response.apiMessageId === "" || seenApiMessageIds.has(response.apiMessageId)) throw new Error(`frontend-proto: SessionTokenUtilization.ungroupedSubagentResponses has missing or repeated apiMessageId ${response.apiMessageId}`);
    seenApiMessageIds.add(response.apiMessageId);
  }
  return generated;
}

function decodeSessionResumeFailure(v: unknown, where: string): SessionResumeFailure {
  const o = ensureObject(v, where);
  rejectUnknown(o, SESSION_RESUME_KEYS, where);
  const attempts = ["create", "automaticRestore"].filter((key) => o[key] !== undefined);
  const causes = ["transcriptUnavailable", "identityMismatch", "queryTermination", "bringUpFailure"].filter((key) => o[key] !== undefined);
  if (attempts.length !== 1 || causes.length !== 1) throw new Error(`frontend-proto: ${where} requires exactly one attempt and cause`);
  const attemptValue = ensureObject(o[attempts[0]], `${where}.${attempts[0]}`);
  rejectUnknown(attemptValue, attempts[0] === "create" ? SESSION_RESUME_CREATE_KEYS : SESSION_RESUME_AUTOMATIC_KEYS, `${where}.${attempts[0]}`);
  const base = { agentReplSessionId: str(o, "agentReplSessionId", where), claudeSessionId: str(o, "claudeSessionId", where), cwd: str(o, "cwd", where), configDir: str(o, "configDir", where), resolvedConfigDir: str(o, "resolvedConfigDir", where), attempt: attempts[0] as "create" | "automaticRestore" };
  if (causes[0] === "identityMismatch") {
    const cause = ensureObject(o.identityMismatch, `${where}.identityMismatch`);
    rejectUnknown(cause, SESSION_RESUME_IDENTITY_KEYS, `${where}.identityMismatch`);
    return { ...base, cause: { case: "identityMismatch", replacementClaudeSessionId: str(cause, "replacementClaudeSessionId", `${where}.identityMismatch`) } };
  }
  if (causes[0] === "queryTermination") {
    return { ...base, cause: { case: "queryTermination", value: decodeQueryTerminationFailure(o.queryTermination, `${where}.queryTermination`) } };
  }
  if (causes[0] === "bringUpFailure") {
    const cause = ensureObject(o.bringUpFailure, `${where}.bringUpFailure`);
    rejectUnknown(cause, SESSION_RESUME_BRING_UP_KEYS, `${where}.bringUpFailure`);
    const message = str(cause, "cause", `${where}.bringUpFailure`);
    if (message.trim() === "") throw new Error(`frontend-proto: ${where}.bringUpFailure.cause must be nonblank`);
    return { ...base, cause: { case: "bringUpFailure", cause: message } };
  }
  const cause = ensureObject(o.transcriptUnavailable, `${where}.transcriptUnavailable`);
  rejectUnknown(cause, SESSION_RESUME_TRANSCRIPT_KEYS, `${where}.transcriptUnavailable`);
  return { ...base, cause: { case: "transcriptUnavailable", searchedPaths: ensureArray(cause.searchedPaths, `${where}.transcriptUnavailable.searchedPaths`).map((path, i) => { if (typeof path !== "string") throw new Error(`frontend-proto: ${where}.transcriptUnavailable.searchedPaths[${i}] must be a string`); return path; }) } };
}

function decodeQueryTerminationFailure(v: unknown, where: string): QueryTerminationFailure {
  let generated: QueryTerminationFailure;
  try {
    generated = fromJson(QueryTerminationFailureSchema, ensureObject(v, where) as JsonValue);
  } catch (error) {
    throw new Error(`frontend-proto: ${where} violates the generated QueryTerminationFailure contract: ${error instanceof Error ? error.message : String(error)}`);
  }
  if (generated.agentReplSessionId === "" || generated.queryInstanceId === "" || generated.observedAtMs <= 0n) {
    throw new Error(`frontend-proto: ${where} requires session, query, and observed-time identity`);
  }
  if (generated.vendorIdentity.case === undefined) throw new Error(`frontend-proto: ${where} requires explicit vendor identity evidence`);
  if (generated.vendorIdentity.case === "vendorSessionId" && generated.vendorIdentity.value === "") {
    throw new Error(`frontend-proto: ${where}.vendorSessionId must be nonblank`);
  }
  if (generated.reason.case === undefined) {
    throw new Error(`frontend-proto: ${where} requires an unexpected termination reason`);
  }
  if ((generated.reason.case === "iteratorFailure" || generated.reason.case === "startupFailure") && generated.reason.value.cause.trim() === "") {
    throw new Error(`frontend-proto: ${where}.${generated.reason.case}.cause must be nonblank`);
  }
  return generated;
}

/**
 * Decode a `SystemFailureItem`.
 *
 * An UNSPECIFIED or unrecognized class THROWS rather than defaulting: the
 * class decides the card's color, so guessing one would paint a failure the
 * wrong color — quietly, and in a way that contradicts the workspace beside
 * it. A frame that cannot say what kind of failure it carries is malformed.
 */
export function decodeSystemFailure(v: unknown, where: string): SystemFailure {
  const o = ensureObject(v, where);
  rejectUnknown(o, SYSTEM_FAILURE_KEYS, where);
  const cls = str(o, "errorClass", where);
  const known = ERROR_CLASSES.find((c) => cls === c || cls === `ERROR_CLASS_${c}`);
  if (known === undefined) {
    throw new Error(`frontend-proto: ${where}.error_class has unrecognized value '${cls}'`);
  }
  const failure: SystemFailure = {
    errorClass: known,
    errorType: str(o, "errorType", where),
    message: str(o, "message", where),
    sourceDetail: str(o, "sourceDetail", where),
    resolvedAtMs: num(o, "resolvedAtMs", where),
    itemUuid: str(o, "itemUuid", where),
    detail: { kind: "none" },
  };
  const structured = ["sessionResume", "queryTermination"].filter((key) => o[key] !== undefined);
  if (structured.length > 1) throw new Error(`frontend-proto: ${where} requires at most one structured detail`);
  if (o.sessionResume !== undefined) failure.detail = { kind: "sessionResume", value: decodeSessionResumeFailure(o.sessionResume, `${where}.sessionResume`) };
  if (o.queryTermination !== undefined) failure.detail = { kind: "queryTermination", value: decodeQueryTerminationFailure(o.queryTermination, `${where}.queryTermination`) };
  return failure;
}

const COMMAND_ACK_KEYS = new Set([
  "requestId",
  "ok",
  "error",
  "failure",
  "interruptConfirmRequired",
  "selectedModel",
]);
const INTERRUPT_CONFIRM_KEYS = new Set(["liveTasks"]);
function decodeCommandAck(v: unknown): CommandAck {
  const o = ensureObject(v, "CommandAck");
  rejectUnknown(o, COMMAND_ACK_KEYS, "CommandAck");
  const ack: CommandAck = {
    requestId: str(o, "requestId", "CommandAck"),
    ok: bool(o, "ok", "CommandAck"),
    error: str(o, "error", "CommandAck"),
    selectedModel: str(o, "selectedModel", "CommandAck"),
  };
  if (o.failure !== undefined && o.failure !== null) {
    ack.failure = decodeSystemFailure(o.failure, "CommandAck.failure");
  }
  if (o.interruptConfirmRequired !== undefined && o.interruptConfirmRequired !== null) {
    const where = "CommandAck.interruptConfirmRequired";
    const c = ensureObject(o.interruptConfirmRequired, where);
    rejectUnknown(c, INTERRUPT_CONFIRM_KEYS, where);
    ack.interruptConfirmRequired = { liveTasks: num(c, "liveTasks", where) };
  }
  if (ack.requestId === "") {
    throw new Error("frontend-proto: CommandAck missing required `request_id`");
  }
  return ack;
}

const DAEMON_VIEW_KEYS = new Set([
  "bootId",
  "protocolVersion",
  "daemonBinaryMtimeMs",
  "daemonVersion",
]);
function decodeDaemonView(v: unknown): DaemonView {
  const o = ensureObject(v, "DaemonView");
  rejectUnknown(o, DAEMON_VIEW_KEYS, "DaemonView");
  return {
    bootId: str(o, "bootId", "DaemonView"),
    protocolVersion: str(o, "protocolVersion", "DaemonView"),
    daemonBinaryMtimeMs: num(o, "daemonBinaryMtimeMs", "DaemonView"),
    daemonVersion: str(o, "daemonVersion", "DaemonView"),
  };
}

/**
 * Decode the backfill enum. An UNRECOGNIZED name throws rather than falling
 * back: reading an unknown state as `done` would leave a workspace blue with
 * nothing retrying it, which is the exact failure this signal exists to catch.
 */
function decodeBackfillState(v: unknown): BackfillState {
  if (v === undefined || v === null) return "unspecified";
  if (typeof v !== "string") {
    throw new Error(`frontend-proto: SessionView.backfill must be a string (got ${typeof v})`);
  }
  const mapped = BACKFILL_STATE_BY_NAME[v];
  if (mapped === undefined) {
    throw new Error(`frontend-proto: SessionView.backfill has unrecognized value '${v}'`);
  }
  return mapped;
}

const PROGRESS_VIEW_KEYS = new Set([
  "workspace",
  "sessionId",
  "state",
  "turnStartedAtMs",
  "thinkingTokens",
  "inputTokens",
  "ttftMs",
  "compacting",
  "retrying",
  "authenticating",
  "hook",
  "rateLimited",
  "rateLimitedWeekly",
  "blocked",
  "interrupt",
  "failure",
  "expensiveTurn",
  "pendingPermissions",
  "queueDepth",
  "liveTaskCount",
]);
const CONTEXT_COST_ALERT_KEYS = new Set([
  "turnId",
  "uncachedInputTokens",
  "thresholdTokens",
  "atMs",
  "promptOrigin",
]);
const PROGRESS_WINDOW_KEYS = new Set(["active", "sinceMs", "detail"]);
const INTERRUPT_WINDOW_KEYS = new Set(["active", "sinceMs", "outcome"]);
const RATE_LIMIT_WINDOW_KEYS = new Set(["active", "resetsAt", "utilization", "status"]);

function decodeProgressView(v: unknown): ProgressView {
  const o = ensureObject(v, "ProgressView");
  rejectUnknown(o, PROGRESS_VIEW_KEYS, "ProgressView");
  const pv: ProgressView = {
    workspace: str(o, "workspace", "ProgressView"),
    sessionId: str(o, "sessionId", "ProgressView"),
    turnStartedAtMs: num(o, "turnStartedAtMs", "ProgressView"),
    thinkingTokens: num(o, "thinkingTokens", "ProgressView"),
    inputTokens: num(o, "inputTokens", "ProgressView"),
    ttftMs: num(o, "ttftMs", "ProgressView"),
    pendingPermissions: num(o, "pendingPermissions", "ProgressView"),
    queueDepth: num(o, "queueDepth", "ProgressView"),
    liveTaskCount: num(o, "liveTaskCount", "ProgressView"),
  };
  // A window is a message: absent means CLOSED, which is why each is decoded
  // only when present rather than materialized as an inactive placeholder.
  for (const key of ["compacting", "retrying", "authenticating", "hook", "blocked"] as const) {
    if (o[key] !== undefined && o[key] !== null) {
      pv[key] = decodeProgressWindow(o[key], `ProgressView.${key}`);
    }
  }
  if (o.rateLimited !== undefined && o.rateLimited !== null) {
    pv.rateLimited = decodeRateLimitWindow(o.rateLimited, "ProgressView.rateLimited");
  }
  if (o.rateLimitedWeekly !== undefined && o.rateLimitedWeekly !== null) {
    pv.rateLimitedWeekly = decodeRateLimitWindow(o.rateLimitedWeekly, "ProgressView.rateLimitedWeekly");
  }
  if (o.interrupt !== undefined && o.interrupt !== null) {
    pv.interrupt = decodeInterruptWindow(o.interrupt);
  }
  if (o.failure !== undefined && o.failure !== null) {
    pv.failure = decodeSystemFailure(o.failure, "ProgressView.failure");
  }
  // Absent = the last turn was cache-efficient. Decoded when present so the
  // alert is the daemon's own observation rather than anything re-derived from
  // the input-token counter beside it.
  if (o.expensiveTurn !== undefined && o.expensiveTurn !== null) {
    pv.expensiveTurn = decodeContextCostAlert(o.expensiveTurn);
  }
  // Without a workspace the view addresses nothing: the footer could not tell
  // which session it is describing.
  if (pv.workspace === "") {
    throw new Error("frontend-proto: ProgressView missing required `workspace`");
  }
  return pv;
}

/**
 * Decode `ContextCostAlert`.
 *
 * The origin is REQUIRED to be a recognizable `PROMPT_ORIGIN_*` name. The
 * daemon rejects UNSPECIFIED before a turn is accepted, so an alert without a
 * usable attribution cannot be told apart from a keep-alive that came back
 * cold — and the whole point of carrying the origin is that those two are
 * different alarms.
 */
function decodeContextCostAlert(v: unknown): ContextCostAlert {
  const ctx = "ProgressView.expensiveTurn";
  const o = ensureObject(v, ctx);
  rejectUnknown(o, CONTEXT_COST_ALERT_KEYS, ctx);
  const promptOrigin = str(o, "promptOrigin", ctx);
  if (!promptOrigin.startsWith(PROMPT_ORIGIN_PREFIX)) {
    throw new Error(
      `frontend-proto: ${ctx}.promptOrigin has unrecognized value '${promptOrigin}' ` +
        "(expected a canonical core.v1.PromptOrigin name)",
    );
  }
  if (promptOrigin === PROMPT_ORIGIN_UNSPECIFIED) {
    throw new Error(
      `frontend-proto: ${ctx}.promptOrigin is UNSPECIFIED — the daemon refuses a turn ` +
        "without an attribution, so an alert cannot carry one",
    );
  }
  const alert: ContextCostAlert = {
    turnId: str(o, "turnId", ctx),
    uncachedInputTokens: num(o, "uncachedInputTokens", ctx),
    thresholdTokens: num(o, "thresholdTokens", ctx),
    atMs: num(o, "atMs", ctx),
    promptOrigin,
  };
  // Without a turn id the alert names no turn, so it could not be joined to
  // the work that paid the cost it reports.
  if (alert.turnId === "") {
    throw new Error(`frontend-proto: ${ctx} missing required \`turnId\``);
  }
  return alert;
}

function decodeProgressWindow(v: unknown, ctx: string): ProgressWindow {
  const o = ensureObject(v, ctx);
  rejectUnknown(o, PROGRESS_WINDOW_KEYS, ctx);
  return {
    active: bool(o, "active", ctx),
    sinceMs: num(o, "sinceMs", ctx),
    detail: str(o, "detail", ctx),
  };
}

/**
 * Decode the interrupt window (I1).
 *
 * An OPEN window with no outcome THROWS. The outcome is decided atomically on
 * the shim's ack — the same ack that opens the window — so there is no
 * outcome-pending phase to represent, and `INTERRUPT_OUTCOME_UNSPECIFIED` is
 * the proto3 zero protojson omits: absent and UNSPECIFIED are the same wire
 * fact. Picking one of the three anyway would invent the very claim the frame
 * declined to make, and the three read very differently to a user.
 *
 * A CLOSED window carries no outcome by construction, so it decodes to null
 * rather than throwing.
 */
function decodeInterruptWindow(v: unknown): InterruptWindow {
  const ctx = "ProgressView.interrupt";
  const o = ensureObject(v, ctx);
  rejectUnknown(o, INTERRUPT_WINDOW_KEYS, ctx);
  const active = bool(o, "active", ctx);
  const raw = o.outcome;
  if (raw === undefined || raw === null || raw === "INTERRUPT_OUTCOME_UNSPECIFIED") {
    if (active) {
      throw new Error(
        `frontend-proto: ${ctx} is open with no outcome ` +
          "(absent === INTERRUPT_OUTCOME_UNSPECIFIED, which the daemon never sends on an open window)",
      );
    }
    return { active, sinceMs: num(o, "sinceMs", ctx), outcome: null };
  }
  if (typeof raw !== "string") {
    throw new Error(`frontend-proto: ${ctx}.outcome must be a string (got ${typeof raw})`);
  }
  const known = INTERRUPT_OUTCOME_BY_NAME[raw];
  if (known === undefined) {
    throw new Error(`frontend-proto: ${ctx}.outcome has unrecognized value '${raw}'`);
  }
  return { active, sinceMs: num(o, "sinceMs", ctx), outcome: known };
}

function decodeRateLimitWindow(v: unknown, ctx: string): RateLimitWindow {
  const o = ensureObject(v, ctx);
  rejectUnknown(o, RATE_LIMIT_WINDOW_KEYS, ctx);
  return {
    active: bool(o, "active", ctx),
    resetsAt: num(o, "resetsAt", ctx),
    utilization: num(o, "utilization", ctx),
    status: str(o, "status", ctx),
  };
}

const STATE_SNAPSHOT_KEYS = new Set([
  "workspaces",
  "sessions",
  "catalogs",
  "daemon",
  "inits",
  "queues",
  "progress",
  "workspaceAvailable",
  "hostActions",
  "shutdownSchedule",
]);
function decodeStateSnapshot(v: unknown): StateSnapshot {
  const o = ensureObject(v, "StateSnapshot");
  rejectUnknown(o, STATE_SNAPSHOT_KEYS, "StateSnapshot");
  const snap: StateSnapshot = {
    workspaces: (o.workspaces === undefined || o.workspaces === null
      ? []
      : ensureArray(o.workspaces, "StateSnapshot.workspaces")
    ).map(decodeWorkspaceState),
    sessions: (o.sessions === undefined || o.sessions === null
      ? []
      : ensureArray(o.sessions, "StateSnapshot.sessions")
    ).map(decodeSessionView),
    catalogs: (o.catalogs === undefined || o.catalogs === null
      ? []
      : ensureArray(o.catalogs, "StateSnapshot.catalogs")
    ).map(decodeTaskCatalog),
    inits: (o.inits === undefined || o.inits === null
      ? []
      : ensureArray(o.inits, "StateSnapshot.inits")
    ).map(decodeSessionInitView),
    queues: (o.queues === undefined || o.queues === null
      ? []
      : ensureArray(o.queues, "StateSnapshot.queues")
    ).map(decodeQueueView),
    progress: (o.progress === undefined || o.progress === null
      ? []
      : ensureArray(o.progress, "StateSnapshot.progress")
    ).map(decodeProgressView),
    workspaceAvailable: (o.workspaceAvailable === undefined || o.workspaceAvailable === null
      ? []
      : ensureArray(o.workspaceAvailable, "StateSnapshot.workspaceAvailable")
    ).map(decodeWorkspaceAvailable),
    hostActions: (o.hostActions === undefined || o.hostActions === null
      ? []
      : ensureArray(o.hostActions, "StateSnapshot.hostActions")
    ).map(decodeHostAction),
  };
  // The daemon block is optional (absent on a pre-S7 daemon). Decode it when
  // present rather than defaulting it away.
  if (o.daemon !== undefined && o.daemon !== null) {
    snap.daemon = decodeDaemonView(o.daemon);
  }
  // Same reading as the daemon block: absent is a daemon that does not carry
  // the lease, so the snapshot seeds nothing rather than asserting `idle` on
  // the daemon's behalf.
  if (o.shutdownSchedule !== undefined && o.shutdownSchedule !== null) {
    snap.shutdownSchedule = decodeShutdownScheduleView(o.shutdownSchedule);
  }
  return snap;
}

const WORKSPACE_AVAILABLE_KEYS = new Set([
  "jobId",
  "finalName",
  "worktreePath",
  "branch",
  "gitRoot",
  "baseCommit",
  "sourceWorkspace",
  "sourceDir",
  "forkFrom",
  "forkSessionId",
  "sessionId",
  "priority",
  "model",
  "initialPromptQueued",
]);

function decodeWorkspaceAvailable(v: unknown): WorkspaceAvailable {
  const o = ensureObject(v, "WorkspaceAvailable");
  rejectUnknown(o, WORKSPACE_AVAILABLE_KEYS, "WorkspaceAvailable");
  const available: WorkspaceAvailable = {
    jobId: str(o, "jobId", "WorkspaceAvailable"),
    finalName: str(o, "finalName", "WorkspaceAvailable"),
    worktreePath: str(o, "worktreePath", "WorkspaceAvailable"),
    branch: str(o, "branch", "WorkspaceAvailable"),
    gitRoot: str(o, "gitRoot", "WorkspaceAvailable"),
    baseCommit: str(o, "baseCommit", "WorkspaceAvailable"),
    sourceWorkspace: str(o, "sourceWorkspace", "WorkspaceAvailable"),
    sourceDir: str(o, "sourceDir", "WorkspaceAvailable"),
    forkFrom: str(o, "forkFrom", "WorkspaceAvailable"),
    forkSessionId: str(o, "forkSessionId", "WorkspaceAvailable"),
    sessionId: str(o, "sessionId", "WorkspaceAvailable"),
    priority: str(o, "priority", "WorkspaceAvailable"),
    model: str(o, "model", "WorkspaceAvailable"),
    initialPromptQueued: bool(o, "initialPromptQueued", "WorkspaceAvailable"),
  };
  if (available.jobId === "" || available.finalName === "" || available.worktreePath === "" || available.sessionId === "") {
    throw new Error("frontend-proto: WorkspaceAvailable missing jobId, finalName, worktreePath, or sessionId");
  }
  return available;
}

const HOST_ACTION_KEYS = new Set([
  "actionId",
  "switchWorkspace",
  "setRepositoryFold",
  "setSidebarView",
  "taskCreate",
  "taskToggleDone",
  "taskOpen",
  "taskAddWorkspace",
]);
const HOST_ACTION_ARMS = [
  "switchWorkspace",
  "setRepositoryFold",
  "setSidebarView",
  "taskCreate",
  "taskToggleDone",
  "taskOpen",
  "taskAddWorkspace",
] as const;

function decodeHostAction(v: unknown): HostAction {
  const o = ensureObject(v, "HostAction");
  rejectUnknown(o, HOST_ACTION_KEYS, "HostAction");
  const actionId = str(o, "actionId", "HostAction");
  if (actionId === "") throw new Error("frontend-proto: HostAction missing required `actionId`");
  const arms = HOST_ACTION_ARMS.filter((arm) => o[arm] !== undefined && o[arm] !== null);
  if (arms.length !== 1) {
    throw new Error(`frontend-proto: HostAction must set exactly one action arm (got ${arms.join(", ") || "none"})`);
  }
  const arm = arms[0];
  const payload = ensureObject(o[arm], `HostAction.${arm}`);
  switch (arm) {
    case "switchWorkspace":
      rejectUnknown(payload, new Set(["dir"]), `HostAction.${arm}`);
      return { actionId, action: { case: arm, dir: str(payload, "dir", `HostAction.${arm}`) } };
    case "setRepositoryFold":
      rejectUnknown(payload, new Set(["repoKey", "folded"]), `HostAction.${arm}`);
      return {
        actionId,
        action: { case: arm, repoKey: str(payload, "repoKey", `HostAction.${arm}`), folded: bool(payload, "folded", `HostAction.${arm}`) },
      };
    case "setSidebarView":
      rejectUnknown(payload, new Set(["view"]), `HostAction.${arm}`);
      return { actionId, action: { case: arm, view: str(payload, "view", `HostAction.${arm}`) } };
    case "taskCreate":
      rejectUnknown(payload, new Set(), `HostAction.${arm}`);
      return { actionId, action: { case: arm } };
    case "taskToggleDone":
    case "taskOpen":
    case "taskAddWorkspace":
      rejectUnknown(payload, new Set(["id"]), `HostAction.${arm}`);
      return { actionId, action: { case: arm, id: str(payload, "id", `HostAction.${arm}`) } };
  }
}

// --- workspace roster -------------------------------------------------------

const WORKSPACE_ROSTER_KEYS = new Set([
  "revision",
  "bootId",
  "repository",
  "task",
  "recentlyMerged",
  "currentDir",
  "navDir",
]);

/**
 * Decode a `WorkspaceRoster`, validating loudly.
 *
 * The grouping oneof is REQUIRED: a roster with no view arm names no grouping,
 * and there is no defensible default to pick between repository and task, so
 * it throws rather than guessing. Setting both is equally a breach.
 *
 * `bootId` is REQUIRED and non-empty: it is the epoch key `revision` is scoped
 * by, so a roster without one cannot be compared against a held roster at all.
 * Defaulting it to "" would silently merge two publishers' counters, so it
 * throws instead.
 */
function decodeWorkspaceRoster(v: unknown): WorkspaceRoster {
  const o = ensureObject(v, "WorkspaceRoster");
  rejectUnknown(o, WORKSPACE_ROSTER_KEYS, "WorkspaceRoster");

  const bootId = str(o, "bootId", "WorkspaceRoster");
  if (bootId === "") {
    throw new Error(
      "frontend-proto: WorkspaceRoster.bootId is empty; a publisher must identify its epoch",
    );
  }

  const hasRepository = o.repository !== undefined && o.repository !== null;
  const hasTask = o.task !== undefined && o.task !== null;
  if (hasRepository && hasTask) {
    throw new Error("frontend-proto: WorkspaceRoster sets both view arms (repository, task)");
  }
  if (!hasRepository && !hasTask) {
    throw new Error(
      "frontend-proto: WorkspaceRoster carries no view variant (empty or unrecognized oneof)",
    );
  }

  const view: WorkspaceRoster["view"] = hasRepository
    ? { case: "repository", value: decodeRosterRepositoryView(o.repository) }
    : { case: "task", value: decodeRosterTaskView(o.task) };

  return {
    revision: num(o, "revision", "WorkspaceRoster"),
    bootId,
    view,
    // Absent recently-merged is an EMPTY section, not a missing one: proto3
    // omits an unset message, and "nothing merged lately" is the normal case.
    recentlyMerged:
      o.recentlyMerged === undefined || o.recentlyMerged === null
        ? { rows: [], folded: false, label: "" }
        : decodeRosterSection(o.recentlyMerged, "WorkspaceRoster.recentlyMerged"),
    currentDir: str(o, "currentDir", "WorkspaceRoster"),
    navDir: str(o, "navDir", "WorkspaceRoster"),
  };
}

function decodeRosterRepositoryView(v: unknown): RosterRepositoryView {
  const o = ensureObject(v, "RosterRepositoryView");
  rejectUnknown(o, new Set(["sections"]), "RosterRepositoryView");
  return { sections: rosterArray(o.sections, "RosterRepositoryView.sections").map(decodeRosterRepoSection) };
}

function decodeRosterTaskView(v: unknown): RosterTaskView {
  const o = ensureObject(v, "RosterTaskView");
  rejectUnknown(o, new Set(["sections"]), "RosterTaskView");
  return { sections: rosterArray(o.sections, "RosterTaskView.sections").map(decodeRosterTaskSection) };
}

function decodeRosterRepoSection(v: unknown): RosterRepoSection {
  const o = ensureObject(v, "RosterRepoSection");
  rejectUnknown(o, new Set(["repoKey", "folded", "rows", "label"]), "RosterRepoSection");
  return {
    repoKey: str(o, "repoKey", "RosterRepoSection"),
    folded: bool(o, "folded", "RosterRepoSection"),
    rows: rosterArray(o.rows, "RosterRepoSection.rows").map((r) => decodeRosterRow(r, "RosterRepoSection.rows")),
    // Display only, and optional: absent is the proto3 empty string, meaning
    // the author offered no label. The fold identity is repoKey regardless.
    label: str(o, "label", "RosterRepoSection"),
  };
}

function decodeRosterTaskSection(v: unknown): RosterTaskSection {
  const o = ensureObject(v, "RosterTaskSection");
  rejectUnknown(o, new Set(["taskId", "title", "done", "rows"]), "RosterTaskSection");
  return {
    taskId: str(o, "taskId", "RosterTaskSection"),
    title: str(o, "title", "RosterTaskSection"),
    done: bool(o, "done", "RosterTaskSection"),
    rows: rosterArray(o.rows, "RosterTaskSection.rows").map((r) => decodeRosterRow(r, "RosterTaskSection.rows")),
  };
}

function decodeRosterSection(v: unknown, ctx: string): RosterSection {
  const o = ensureObject(v, ctx);
  rejectUnknown(o, new Set(["rows", "folded", "label"]), ctx);
  return {
    rows: rosterArray(o.rows, `${ctx}.rows`).map((r) => decodeRosterRow(r, `${ctx}.rows`)),
    // Both optional with proto3 zeros: an unfolded, unlabeled section is a
    // legal thing for the author to publish, so absence defaults rather than
    // throwing. A wrong TYPE still throws — that is a wire bug, not a default.
    folded: bool(o, "folded", ctx),
    label: str(o, "label", ctx),
  };
}

/**
 * A row's own fields plus every status arm, which sit directly on the row
 * because a oneof is flattened into its parent message on the wire.
 */
const ROSTER_ROW_KEYS: ReadonlySet<string> = new Set([
  "dir",
  "name",
  "current",
  "children",
  "lastViewedAtMs",
  "mergedAtMs",
  "branch",
  "parentBranch",
  "summary",
  "closed",
  ...ROSTER_ROW_STATUS_CASES,
]);

function decodeRosterRow(v: unknown, ctx: string): RosterRow {
  const o = ensureObject(v, ctx);
  rejectUnknown(o, ROSTER_ROW_KEYS, ctx);
  return {
    dir: str(o, "dir", ctx),
    name: str(o, "name", ctx),
    status: decodeRosterRowStatus(o, ctx),
    current: bool(o, "current", ctx),
    // Recursive by construction: a spawned family nests under its parent.
    children: rosterArray(o.children, `${ctx}.children`).map((c) =>
      decodeRosterRow(c, `${ctx}.children`),
    ),
    // The display fields, every one optional with a MEANINGFUL proto3 zero —
    // 0 is "never viewed" / "not merged" and "" is "unknown / none", each an
    // assertion the renderer can act on rather than a missing value. So an
    // absent field defaults; a field of the wrong TYPE still throws, because
    // that is a publisher speaking a wire this build cannot read.
    lastViewedAtMs: num(o, "lastViewedAtMs", ctx),
    mergedAtMs: num(o, "mergedAtMs", ctx),
    branch: str(o, "branch", ctx),
    parentBranch: str(o, "parentBranch", ctx),
    summary: str(o, "summary", ctx),
    closed: bool(o, "closed", ctx),
  };
}

/** A repeated field: absent is an empty list, present must be a JSON array. */
function rosterArray(v: unknown, ctx: string): unknown[] {
  if (v === undefined || v === null) return [];
  return ensureArray(v, ctx);
}

/**
 * The row's status oneof, REJECTED when unset rather than defaulted.
 *
 * Unlike `ConversationSource`, there is no layer downstream that owns this
 * error: the status IS the row's whole lifecycle assertion, and a row drawn
 * with no dot would silently misreport a workspace. So an unset oneof throws,
 * exactly as the retired `ROSTER_ROW_STATUS_UNSPECIFIED` did — the breach did
 * not go away with the enum, it just moved to the arm's absence.
 *
 * Setting more than one arm is refused for the same reason: two lifecycles is
 * no more a lifecycle than none, and picking one would be a silent guess.
 */
function decodeRosterRowStatus(o: Obj, ctx: string): RosterRow["status"] {
  const armKeys = Object.keys(o).filter((k) => ROSTER_ROW_STATUS_CASE_SET.has(k));
  if (armKeys.length === 0) {
    throw new Error(
      `frontend-proto: ${ctx} sets no status arm, which is not a lifecycle ` +
        `(WHICH member of the oneof is set IS the status)`,
    );
  }
  if (armKeys.length > 1) {
    throw new Error(`frontend-proto: ${ctx} sets multiple status arms: ${armKeys.join(", ")}`);
  }
  const arm = armKeys[0] as RosterRowStatusCase;
  // Every status message is empty by contract, so the arm's payload must be an
  // object with nothing in it. A field inside is a wire this build does not
  // understand, and accepting it would silently drop whatever it carried.
  rejectUnknown(ensureObject(o[arm], `${ctx}.${arm}`), EMPTY_KEY_SET, `${ctx}.${arm}`);
  return { case: arm };
}

/** The allowed-key set for a message with no fields at all. */
const EMPTY_KEY_SET: ReadonlySet<string> = new Set<string>();

// --- primitive readers (loud, protojson-aware) ------------------------------

function ensureObject(v: unknown, ctx: string): Obj {
  if (typeof v !== "object" || v === null || Array.isArray(v)) {
    throw new Error(`frontend-proto: ${ctx} must be a JSON object`);
  }
  return v as Obj;
}

function ensureArray(v: unknown, ctx: string): unknown[] {
  if (!Array.isArray(v)) {
    throw new Error(`frontend-proto: ${ctx} must be a JSON array`);
  }
  return v;
}

function rejectUnknown(o: Obj, allowed: ReadonlySet<string>, ctx: string): void {
  const bad = Object.keys(o).filter((k) => !allowed.has(k));
  if (bad.length > 0) {
    throw new Error(`frontend-proto: ${ctx} has unrecognized field(s): ${bad.join(", ")}`);
  }
}

function str(o: Obj, key: string, ctx: string): string {
  const v = o[key];
  if (v === undefined || v === null) return "";
  if (typeof v !== "string") {
    throw new Error(`frontend-proto: ${ctx}.${key} must be a string (got ${typeof v})`);
  }
  return v;
}

/** A proto3 numeric scalar: a JSON number, or (int64/uint64) a numeric string. */
function num(o: Obj, key: string, ctx: string): number {
  const v = o[key];
  if (v === undefined || v === null) return 0;
  if (typeof v === "number") return v;
  if (typeof v === "string") {
    const n = Number(v);
    if (!Number.isFinite(n)) {
      throw new Error(`frontend-proto: ${ctx}.${key} is not a numeric string ('${v}')`);
    }
    return n;
  }
  throw new Error(`frontend-proto: ${ctx}.${key} must be a number or numeric string (got ${typeof v})`);
}

function bool(o: Obj, key: string, ctx: string): boolean {
  const v = o[key];
  if (v === undefined || v === null) return false;
  if (typeof v !== "boolean") {
    throw new Error(`frontend-proto: ${ctx}.${key} must be a boolean (got ${typeof v})`);
  }
  return v;
}

/**
 * `ConversationSource`, adopted rather than rejected at UNSPECIFIED.
 *
 * An unknown NAME still throws (that is a wire the webapp cannot read), but the
 * proto3 zero value is a well-formed enum member here. Refusing it in the
 * decoder would take the whole frame down and lose the correlated context — the
 * item's uuid, arm and session — that makes the malformed item findable. The
 * conversation layer owns that error (see `state-adapter.ts`).
 */
function enumConversationSource(o: Obj, key: string, ctx: string): ConversationSource {
  return enumValue(
    o,
    key,
    ctx,
    ConversationSource,
    CONVERSATION_SOURCE_BY_NAME,
    ConversationSource.UNSPECIFIED,
  );
}

function enumRenderState(o: Obj, key: string, ctx: string): RenderState {
  return enumValue(o, key, ctx, RenderState, RENDER_STATE_BY_NAME, RenderState.UNSPECIFIED);
}

function enumSessionConnectivity(o: Obj, key: string, ctx: string): SessionConnectivity {
  return enumValue(
    o,
    key,
    ctx,
    SessionConnectivity,
    SESSION_CONNECTIVITY_BY_NAME,
    SessionConnectivity.UNSPECIFIED,
  );
}

function enumSessionStatus(o: Obj, key: string, ctx: string): SessionStatus {
  return enumValue(o, key, ctx, SessionStatus, SESSION_STATUS_BY_NAME, SessionStatus.UNSPECIFIED);
}

function enumValue<T extends number>(
  o: Obj,
  key: string,
  ctx: string,
  numericNames: Record<number, string>,
  names: Readonly<Record<string, T>>,
  unspecified: T,
): T {
  const v = o[key];
  if (v === undefined || v === null) return unspecified;
  if (typeof v === "number") {
    if (numericNames[v] === undefined) {
      throw new Error(`frontend-proto: ${ctx}.${key} has unknown enum value ${v}`);
    }
    return v as T;
  }
  if (typeof v === "string") {
    const mapped = names[v];
    if (mapped === undefined) {
      throw new Error(`frontend-proto: ${ctx}.${key} has unknown enum value '${v}'`);
    }
    return mapped;
  }
  throw new Error(`frontend-proto: ${ctx}.${key} must be an enum name or number`);
}
