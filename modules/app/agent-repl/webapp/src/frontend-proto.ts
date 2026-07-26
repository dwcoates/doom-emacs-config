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
 *   toolUseResult, result, compactBoundary, compactBoundaryLine, apiError,
 *   permission). The webapp DECOMPOSES those typed payloads back into its
 *   render vocabulary in `state-adapter.ts`; the OLD `kind`-discriminated
 *   pre-rendered Struct vocabulary is gone.
 * - `TypingDelta` embeds a `core.v1.ContentDelta` under `delta`
 *   ({uuid, blockIndex, one of text/thinking/inputJson/signature,
 *   estimatedTokens}) rather than the old flat uuid/blockIndex/kind/delta.
 * Additive S9: the `sessionInit` frame + `StateSnapshot.inits` carry the
 * retained `data.v1.SystemInit`; `SessionView.configDir`.
 *
 * STUB vs HAND-TYPED — the choice (per the G11 charter):
 * We HAND-TYPE the protojson shapes rather than importing the committed
 * `proto/gen/ts` protobuf-es stubs. The stubs work at runtime (Vite/Vitest
 * resolve them), but `tsc` cannot: the stub's own bare imports
 * (`@bufbuild/protobuf/codegenv2`, `/wkt`) are resolved relative to the stub's
 * directory (`proto/gen/ts/…`), which has no reachable `node_modules`, so
 * `npm run typecheck` fails with TS2307. `frontend.v1` is small, so hand-typing
 * is cheap, self-contained, and lets us implement the §5.1 loud validation
 * contract directly and visibly.
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
}

const RENDER_STATE_BY_NAME: Readonly<Record<string, RenderState>> = {
  RENDER_STATE_UNSPECIFIED: RenderState.UNSPECIFIED,
  RENDER_STATE_INIT: RenderState.INIT,
  RENDER_STATE_IDLE: RenderState.IDLE,
  RENDER_STATE_IDLE_ASYNC: RenderState.IDLE_ASYNC,
  RENDER_STATE_READY: RenderState.READY,
  RENDER_STATE_VENDOR_BLOCKED: RenderState.VENDOR_BLOCKED,
  RENDER_STATE_THINKING: RenderState.THINKING,
  RENDER_STATE_PERMISSION: RenderState.PERMISSION,
  RENDER_STATE_DONE: RenderState.DONE,
  RENDER_STATE_STOP_FAILED: RenderState.STOP_FAILED,
  RENDER_STATE_MERGING: RenderState.MERGING,
  RENDER_STATE_MERGE_QUEUED: RenderState.MERGE_QUEUED,
  RENDER_STATE_MERGE_CONFLICT: RenderState.MERGE_CONFLICT,
  RENDER_STATE_MERGE_FAILED: RenderState.MERGE_FAILED,
  RENDER_STATE_MERGED: RenderState.MERGED,
  RENDER_STATE_DEAD: RenderState.DEAD,
  RENDER_STATE_DEGRADED: RenderState.DEGRADED,
};

// --- message types ----------------------------------------------------------

/** A protojson google.protobuf.Struct value (a free-form JSON object). */
export type JsonObject = Record<string, unknown>;

export interface WorkspaceState {
  workspace: string;
  sessionId: string;
  state: RenderState;
  turnActive: boolean;
  liveTaskCount: number;
  mergePhase: string;
  causeKind: string;
  causeSeq: number;
  atMs: number;
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
  /** Why a terminal session ended; "" while alive. */
  deathReason: string;
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
  /**
   * Whether this session's on-disk transcript has been read into the store
   * (F2, the never-blue completion signal). Daemon-resolved; the webapp
   * decodes it for wire parity and renders no visual for it — the consumer is
   * the Emacs workspace-switch ensure.
   */
  backfill: BackfillState;
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
  "compactBoundary",
  "compactBoundaryLine",
  "apiError",
  "permission",
  "systemFailure",
] as const;
export type ConversationItemArm = (typeof CONVERSATION_ITEM_ARMS)[number];

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
}

/**
 * One decoded conversation addition: the {uuid, tsMs, requestId} envelope plus
 * the single selected typed payload arm and its (shape-adopted) value. The
 * state-adapter decomposes `payload` per `arm` into the store's render items.
 */
export interface ConversationItemFrame {
  uuid: string;
  tsMs: number;
  requestId: string;
  arm: ConversationItemArm;
  /** The typed data.v1/core.v1 payload, adopted by shape (see file-top §5.1). */
  payload: JsonObject;
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
  /**
   * The CLASSIFIED refusal (F4). `error` above is the raw handler text this
   * end never rendered at all; this is the same failure run through the
   * daemon's one classifier, which is what lets a rejected prompt become a
   * visible card instead of a console log. Absent when `ok`.
   */
  failure?: SystemFailure;
}

export interface DegradedNotice {
  component: string;
  reason: string;
  recovered: boolean;
  atMs: number;
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

/** The rate-limit window, which carries structured detail (F1). */
export interface RateLimitWindow {
  active: boolean;
  /** Epoch SECONDS (the vendor event's own unit), not millis. */
  resetsAt: number;
  utilization: number;
  status: string;
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
  /** The SSM's resolved phase, mirrored so the footer is self-sufficient. */
  state: RenderState;
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
  rateLimited?: RateLimitWindow;
  /**
   * The session reporting it is parked on the USER. NOT a phase — `state`
   * above remains the SSM's verdict — but a fact the daemon cannot otherwise
   * see, since a session can block on an interaction it holds no count for.
   */
  blocked?: ProgressWindow;
  /** Persists until the next turn starts; "" = no error standing. */
  errorSummary: string;
  /** The feed item the error row scrolls to; "" = not addressable. */
  errorItemUuid: string;
  /**
   * The CLASSIFIED error state (F4), superseding the two fields above — its
   * own `itemUuid` absorbs the addressing job. Carrying the class is what lets
   * the footer take its color from the shared table instead of the hardcoded
   * red no other surface consulted.
   */
  failure?: SystemFailure;
  pendingPermissions: number;
  queueDepth: number;
  liveTaskCount: number;
}

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
}

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

/** One prompt the daemon is holding (E4). */
export interface QueueEntry {
  id: string;
  text: string;
  queuedAtMs: number;
  classification: QueueClassification;
  rationale: string;
  accepted: boolean;
}

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
    | { case: "degradedNotice"; value: DegradedNotice }
    | { case: "daemonView"; value: DaemonView }
    | { case: "sessionInit"; value: SessionInitView }
    | { case: "heartbeat"; value: HeartbeatView }
    | { case: "queue"; value: QueueView }
    | { case: "progress"; value: ProgressView };
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
  ["degradedNotice", (v: unknown) => ({ case: "degradedNotice" as const, value: decodeDegradedNotice(v) })],
  ["daemonView", (v: unknown) => ({ case: "daemonView" as const, value: decodeDaemonView(v) })],
  ["sessionInit", (v: unknown) => ({ case: "sessionInit" as const, value: decodeSessionInitView(v) })],
  ["heartbeat", (v: unknown) => ({ case: "heartbeat" as const, value: decodeHeartbeatView(v) })],
  ["queue", (v: unknown) => ({ case: "queue" as const, value: decodeQueueView(v) })],
  ["progress", (v: unknown) => ({ case: "progress" as const, value: decodeProgressView(v) })],
]);

// --- per-message decoders (strict: reject unknown fields, validate required) -

const WORKSPACE_STATE_KEYS = new Set([
  "workspace",
  "sessionId",
  "state",
  "turnActive",
  "liveTaskCount",
  "mergePhase",
  "causeKind",
  "causeSeq",
  "atMs",
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
    mergePhase: str(o, "mergePhase", "WorkspaceState"),
    causeKind: str(o, "causeKind", "WorkspaceState"),
    causeSeq: num(o, "causeSeq", "WorkspaceState"),
    atMs: num(o, "atMs", "WorkspaceState"),
  };
  if (ws.workspace === "") {
    throw new Error("frontend-proto: WorkspaceState missing required `workspace`");
  }
  if (ws.state === RenderState.UNSPECIFIED) {
    throw new Error(
      `frontend-proto: WorkspaceState for '${ws.workspace}' has UNSPECIFIED ` +
        "render state (SSM must resolve a concrete state)",
    );
  }
  return ws;
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
  "deathReason",
  "rehydratable",
  "hibernated",
  "pendingPermissions",
  "configDir",
  "backfill",
]);
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
    deathReason: str(o, "deathReason", "SessionView"),
    rehydratable: bool(o, "rehydratable", "SessionView"),
    hibernated: bool(o, "hibernated", "SessionView"),
    pendingPermissions: num(o, "pendingPermissions", "SessionView"),
    // S8 account identity: "" when the daemon has not resolved a config dir.
    configDir: str(o, "configDir", "SessionView"),
    // F2 never-blue signal; absent on a pre-F2 daemon, which reads as
    // `unspecified` — the same "nothing to backfill" a fresh workspace has.
    backfill: decodeBackfillState(o.backfill),
  };
  if (sv.sessionId === "") {
    throw new Error("frontend-proto: SessionView missing required `session_id`");
  }
  return sv;
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

const CONVERSATION_ITEM_ENVELOPE_KEYS = new Set(["uuid", "tsMs", "requestId"]);
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
  return {
    uuid: str(o, "uuid", ctx),
    tsMs: num(o, "tsMs", ctx),
    requestId: str(o, "requestId", ctx),
    arm,
    // Adopt the typed payload by shape (see file-top §5.1 boundary note).
    payload: ensureObject(o[arm], `${ctx}.${arm}`),
  };
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
]);

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
  return e;
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

const SYSTEM_FAILURE_KEYS = new Set([
  "errorClass",
  "errorType",
  "message",
  "sourceDetail",
  "resolvedAtMs",
  "itemUuid",
]);

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
  return {
    errorClass: known,
    errorType: str(o, "errorType", where),
    message: str(o, "message", where),
    sourceDetail: str(o, "sourceDetail", where),
    resolvedAtMs: num(o, "resolvedAtMs", where),
    itemUuid: str(o, "itemUuid", where),
  };
}

const COMMAND_ACK_KEYS = new Set(["requestId", "ok", "error", "failure"]);
function decodeCommandAck(v: unknown): CommandAck {
  const o = ensureObject(v, "CommandAck");
  rejectUnknown(o, COMMAND_ACK_KEYS, "CommandAck");
  const ack: CommandAck = {
    requestId: str(o, "requestId", "CommandAck"),
    ok: bool(o, "ok", "CommandAck"),
    error: str(o, "error", "CommandAck"),
  };
  if (o.failure !== undefined && o.failure !== null) {
    ack.failure = decodeSystemFailure(o.failure, "CommandAck.failure");
  }
  if (ack.requestId === "") {
    throw new Error("frontend-proto: CommandAck missing required `request_id`");
  }
  return ack;
}

const DEGRADED_NOTICE_KEYS = new Set(["component", "reason", "recovered", "atMs"]);
function decodeDegradedNotice(v: unknown): DegradedNotice {
  const o = ensureObject(v, "DegradedNotice");
  rejectUnknown(o, DEGRADED_NOTICE_KEYS, "DegradedNotice");
  const dn: DegradedNotice = {
    component: str(o, "component", "DegradedNotice"),
    reason: str(o, "reason", "DegradedNotice"),
    recovered: bool(o, "recovered", "DegradedNotice"),
    atMs: num(o, "atMs", "DegradedNotice"),
  };
  if (dn.component === "") {
    throw new Error("frontend-proto: DegradedNotice missing required `component`");
  }
  return dn;
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
  "blocked",
  "errorSummary",
  "errorItemUuid",
  "failure",
  "pendingPermissions",
  "queueDepth",
  "liveTaskCount",
]);
const PROGRESS_WINDOW_KEYS = new Set(["active", "sinceMs", "detail"]);
const RATE_LIMIT_WINDOW_KEYS = new Set(["active", "resetsAt", "utilization", "status"]);

function decodeProgressView(v: unknown): ProgressView {
  const o = ensureObject(v, "ProgressView");
  rejectUnknown(o, PROGRESS_VIEW_KEYS, "ProgressView");
  const pv: ProgressView = {
    workspace: str(o, "workspace", "ProgressView"),
    sessionId: str(o, "sessionId", "ProgressView"),
    state: enumRenderState(o, "state", "ProgressView"),
    turnStartedAtMs: num(o, "turnStartedAtMs", "ProgressView"),
    thinkingTokens: num(o, "thinkingTokens", "ProgressView"),
    inputTokens: num(o, "inputTokens", "ProgressView"),
    ttftMs: num(o, "ttftMs", "ProgressView"),
    errorSummary: str(o, "errorSummary", "ProgressView"),
    errorItemUuid: str(o, "errorItemUuid", "ProgressView"),
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
    pv.rateLimited = decodeRateLimitWindow(o.rateLimited);
  }
  if (o.failure !== undefined && o.failure !== null) {
    pv.failure = decodeSystemFailure(o.failure, "ProgressView.failure");
  }
  // Without a workspace the view addresses nothing: the footer could not tell
  // which session it is describing.
  if (pv.workspace === "") {
    throw new Error("frontend-proto: ProgressView missing required `workspace`");
  }
  // The phase mirror must name a concrete state. A workspace with nothing
  // resolved yet is INIT, which the daemon seeds — so UNSPECIFIED here is a
  // malformed frame, not an early one.
  if (pv.state === RenderState.UNSPECIFIED) {
    throw new Error(
      `frontend-proto: ProgressView for '${pv.workspace}' has UNSPECIFIED ` +
        "render state (the resolver must mirror a concrete phase)",
    );
  }
  return pv;
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

function decodeRateLimitWindow(v: unknown): RateLimitWindow {
  const ctx = "ProgressView.rateLimited";
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
  };
  // The daemon block is optional (absent on a pre-S7 daemon). Decode it when
  // present rather than defaulting it away.
  if (o.daemon !== undefined && o.daemon !== null) {
    snap.daemon = decodeDaemonView(o.daemon);
  }
  return snap;
}

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

function enumRenderState(o: Obj, key: string, ctx: string): RenderState {
  const v = o[key];
  if (v === undefined || v === null) return RenderState.UNSPECIFIED;
  if (typeof v === "number") {
    if (RenderState[v] === undefined) {
      throw new Error(`frontend-proto: ${ctx}.${key} has unknown enum value ${v}`);
    }
    return v as RenderState;
  }
  if (typeof v === "string") {
    const mapped = RENDER_STATE_BY_NAME[v];
    if (mapped === undefined) {
      throw new Error(`frontend-proto: ${ctx}.${key} has unknown enum value '${v}'`);
    }
    return mapped;
  }
  throw new Error(`frontend-proto: ${ctx}.${key} must be an enum name or number`);
}
