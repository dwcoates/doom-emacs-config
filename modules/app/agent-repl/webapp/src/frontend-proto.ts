/**
 * agentshim.frontend.v1 — hand-typed protojson frame types + a strict, loud
 * decoder for the daemon's resolved frontend surface (§5.4, §11).
 *
 * The daemon pushes canonical proto3-JSON (protojson) `FrontendFrame`s over
 * the WebSocket — the SAME message set Emacs consumes, so the two frontends
 * can never diverge.
 *
 * STUB vs HAND-TYPED — the choice (per the G11 charter):
 * We HAND-TYPE the protojson shapes rather than importing the committed
 * `proto/gen/ts` protobuf-es stubs. The stubs work at runtime (Vite/Vitest
 * resolve them), but `tsc` cannot: the stub's own bare imports
 * (`@bufbuild/protobuf/codegenv2`, `/wkt`) are resolved relative to the stub's
 * directory (`proto/gen/ts/…`), which has no reachable `node_modules`, so
 * `npm run typecheck` fails with TS2307. Making tsc resolve it would require a
 * `tsconfig` `paths`/`baseUrl` hack reaching outside the webapp project — a
 * change to an existing webapp config file the G11 constraints tell us to
 * leave alone. `frontend.v1` is small (~10 messages), so hand-typing is cheap,
 * self-contained (no new dependency), and lets us implement the §5.1 loud
 * validation contract directly and visibly.
 *
 * VALIDATION CONTRACT (§5.1) — the decoder hard-errors (never returns a
 * degraded value) on:
 * - input that is not valid JSON / not a JSON object;
 * - an unrecognized field (top-level or nested) — the protojson analogue of a
 *   new/unknown field, surfaced loudly rather than silently kept;
 * - an empty or unrecognized `FrontendFrame` oneof variant;
 * - an unknown enum name/value;
 * - a scalar of the wrong JSON type;
 * - a recognized variant missing a load-bearing field.
 * Field names are the canonical protojson (lowerCamelCase) names; int64/uint64
 * scalars arrive as JSON strings and are parsed to `number`.
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
}

const RENDER_STATE_BY_NAME: Readonly<Record<string, RenderState>> = {
  RENDER_STATE_UNSPECIFIED: RenderState.UNSPECIFIED,
  RENDER_STATE_INIT: RenderState.INIT,
  RENDER_STATE_IDLE: RenderState.IDLE,
  RENDER_STATE_IDLE_ASYNC: RenderState.IDLE_ASYNC,
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
  /** Working directory a rebind's POST /sessions needs. */
  cwd: string;
  // S7 GET /sessions parity fields (Emacs reads these off the pushed
  // SessionView now that it dropped the HTTP poller). Optional: a SessionView
  // that predates them decodes to the field default. The webapp adapter does
  // not need them, but the strict decoder accepts them typed.
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
}

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

export interface ConversationDelta {
  workspace: string;
  sessionId: string;
  items: JsonObject[];
  throughSeq: number;
}

export interface TypingDelta {
  workspace: string;
  sessionId: string;
  uuid: string;
  blockIndex: number;
  kind: string;
  delta: string;
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
}

export interface DegradedNotice {
  component: string;
  reason: string;
  recovered: boolean;
  atMs: number;
}

export interface StateSnapshot {
  workspaces: WorkspaceState[];
  sessions: SessionView[];
  catalogs: TaskCatalog[];
  /** Daemon identity carried on every connect snapshot (S7); absent on a
   * pre-S7 daemon, so it is optional. */
  daemon?: DaemonView;
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
    | { case: "daemonView"; value: DaemonView };
};

/** The frame-variant discriminators FrontendFrame.frame.case may hold. */
export type FrameCase = FrontendFrame["frame"]["case"];

// --- vocabularies -----------------------------------------------------------

/** The kinds a `TypingDelta.kind` may name. */
export const TYPING_KINDS = ["text", "thinking", "input_json"] as const;
export type TypingKind = (typeof TYPING_KINDS)[number];

/** The kinds a `TaskEntry.kind` may name. */
export const TASK_KINDS = ["agent", "shell", "workflow"] as const;
export type TaskKind = (typeof TASK_KINDS)[number];

/** The statuses a `TaskEntry.status` may name. */
export const TASK_STATUSES = ["running", "done", "error", "killed", "stopped", "lost"] as const;
export type TaskStatus = (typeof TASK_STATUSES)[number];

/**
 * The EXPLICIT unsupported-shapes registry (§11 deliverable). Every
 * `FrontendFrame` variant the webapp does NOT map to a visual is listed here
 * with the reason, so unsupported coverage is a known, enumerated quantity
 * rather than an unknown. The state adapter consults this to route a listed
 * variant down its typed, counted, log-once ignore path instead of crashing or
 * silently dropping it.
 *
 * Within `agentshim.frontend.v1`, `commandAck` is the sole frame with no
 * webapp visual: it is a control-plane receipt for a `FrontendCommand`,
 * consumed by the command dispatcher (outside the render adapter's scope).
 * Every OTHER frame variant maps to an already-supported visual (see the
 * coverage table in the G11 report). Unsupported CONVERSATION-ITEM kinds
 * inside a `ConversationDelta` are handled dynamically by the adapter (an item
 * whose `kind` is outside the store's bubble/card vocabulary is ignored the
 * same way, logged once per distinct kind), since that set is the daemon's
 * (G9 translate.go) to grow, not a fixed frontend.v1 enum.
 */
export const UNSUPPORTED_SHAPES: ReadonlyMap<string, string> = new Map<string, string>([
  [
    "commandAck",
    "control-plane command receipt (agentshim.frontend.v1.CommandAck); the " +
      "webapp renders no visual for it — command dispatch/acking is out of " +
      "the render adapter's scope",
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
    items: (o.items === undefined || o.items === null ? [] : ensureArray(o.items, "ConversationDelta.items")).map(
      (item, i) => {
        const io = ensureObject(item, `ConversationDelta.items[${i}]`);
        if (typeof io.kind !== "string" || io.kind === "") {
          throw new Error(
            `frontend-proto: ConversationDelta item[${i}] missing string \`kind\` discriminator`,
          );
        }
        return io;
      },
    ),
    throughSeq: num(o, "throughSeq", "ConversationDelta"),
  };
  if (cd.sessionId === "") {
    throw new Error("frontend-proto: ConversationDelta missing required `session_id`");
  }
  return cd;
}

const TYPING_DELTA_KEYS = new Set(["workspace", "sessionId", "uuid", "blockIndex", "kind", "delta"]);
function decodeTypingDelta(v: unknown): TypingDelta {
  const o = ensureObject(v, "TypingDelta");
  rejectUnknown(o, TYPING_DELTA_KEYS, "TypingDelta");
  const td: TypingDelta = {
    workspace: str(o, "workspace", "TypingDelta"),
    sessionId: str(o, "sessionId", "TypingDelta"),
    uuid: str(o, "uuid", "TypingDelta"),
    blockIndex: num(o, "blockIndex", "TypingDelta"),
    kind: str(o, "kind", "TypingDelta"),
    delta: str(o, "delta", "TypingDelta"),
  };
  if (td.uuid === "") {
    throw new Error("frontend-proto: TypingDelta missing required `uuid`");
  }
  if (!(TYPING_KINDS as readonly string[]).includes(td.kind)) {
    throw new Error(
      `frontend-proto: TypingDelta has unknown kind '${td.kind}' ` +
        `(expected one of ${TYPING_KINDS.join(", ")})`,
    );
  }
  return td;
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

const COMMAND_ACK_KEYS = new Set(["requestId", "ok", "error"]);
function decodeCommandAck(v: unknown): CommandAck {
  const o = ensureObject(v, "CommandAck");
  rejectUnknown(o, COMMAND_ACK_KEYS, "CommandAck");
  const ack: CommandAck = {
    requestId: str(o, "requestId", "CommandAck"),
    ok: bool(o, "ok", "CommandAck"),
    error: str(o, "error", "CommandAck"),
  };
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

const STATE_SNAPSHOT_KEYS = new Set(["workspaces", "sessions", "catalogs", "daemon"]);
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
