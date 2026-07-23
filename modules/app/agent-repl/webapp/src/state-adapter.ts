/**
 * state-adapter — maps decoded `agentshim.frontend.v1` frames onto the
 * webapp's EXISTING store/render input shapes, for the ALREADY-supported
 * visuals only (§11 scope rule).
 *
 * This is the seam between the new protobuf frontend surface and the webapp's
 * current rendering vocabulary. It is deliberately a PURE producer: `apply()`
 * returns typed effects describing what the store/render layer should adopt,
 * and never touches the DOM or mutates a store itself — the stitch phase wires
 * each effect into its existing consumer (see the G11 report's stitch section).
 *
 * SUPPORTED MAPPINGS (the only visuals the webapp renders today):
 * - WorkspaceState  → status/tail-row inputs (`WorkspaceStatusInput`).
 * - SessionView     → topbar / session-info inputs (`SessionViewInput`).
 * - ConversationDelta items → the existing bubble/card vocabulary
 *   (`ConversationItem[]` from store.ts) — one per item whose `kind` the store
 *   knows.
 * - TypingDelta     → the smooth.ts reveal feed (`TypingReveal`).
 * - TaskCatalog     → the async/task roster inputs (`CounterEntry[]` from
 *   counter-menu.ts, the same shape tasks.ts feeds the topbar counter).
 * - DegradedNotice  → a visible banner input (`DegradedBanner`).
 * - StateSnapshot   → decomposed into the WorkspaceState / SessionView /
 *   TaskCatalog effects above.
 *
 * EXPLICIT IGNORE (no new visuals are added; §11): a frame variant in
 * `UNSUPPORTED_SHAPES` (today: `commandAck`) and a `ConversationDelta` item
 * whose `kind` is outside the store's vocabulary are IGNORED EXPLICITLY —
 * typed as an `{ kind: "ignored" }` effect, counted, and debug-logged once per
 * distinct shape name. They are never crashed on and never silently dropped
 * without the log.
 */

import type { CounterEntry, CounterStatus } from "./counter-menu.js";
import type {
  AssistantMessageError,
  AsyncSource,
  ContentBlock,
  PermissionPreview,
  ResultSubtype,
  Usage,
} from "./protocol.js";
import type {
  CompactBoundaryItem,
  ConversationItem,
  ErrorItem,
  PermissionItem,
  ResultContext,
  ResultItem,
  RetryItem,
  SystemItem,
  TextItem,
  ThinkingItem,
  ToolItem,
  UserTurnItem,
} from "./store.js";
import {
  RenderState,
  UNSUPPORTED_SHAPES,
  type ConversationDelta,
  type DegradedNotice,
  type FrontendFrame,
  type SessionView,
  type TaskCatalog,
  type TaskEntry,
  type TypingDelta,
  type WorkspaceState,
} from "./frontend-proto.js";

// --- adapter output shapes --------------------------------------------------

/** The closed render-state keyword the status/tail row maps its display to. */
export type WebRenderState =
  | "init"
  | "idle"
  | "idle_async"
  | "thinking"
  | "permission"
  | "done"
  | "stop_failed"
  | "merging"
  | "merge_queued"
  | "merge_conflict"
  | "merge_failed"
  | "merged"
  | "dead"
  | "degraded";

/** WorkspaceState → status/tail-row input. */
export interface WorkspaceStatusInput {
  workspace: string;
  sessionId: string;
  state: WebRenderState;
  /** SSM resolution input, surfaced for debuggability (the tail row reads it). */
  turnActive: boolean;
  liveTaskCount: number;
  mergePhase: string;
}

/** SessionView → topbar / session-info input. */
export interface SessionViewInput {
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
}

/** TypingDelta → the smooth.ts reveal feed's append. */
export interface TypingReveal {
  workspace: string;
  sessionId: string;
  uuid: string;
  blockIndex: number;
  kind: "text" | "thinking" | "input_json";
  delta: string;
  /**
   * The stable block id the store/smooth reveal keys the growing block on,
   * synthesized from the ephemeral relay's `uuid` + `block_index` (a message
   * can open several blocks). Matches how the existing store keys text/thinking
   * blocks by a single id.
   */
  blockId: string;
}

/** TaskCatalog → the async/task roster input (topbar counter vocabulary). */
export interface TaskCatalogInput {
  workspace: string;
  sessionId: string;
  entries: CounterEntry[];
}

/** DegradedNotice → the visible banner input. */
export interface DegradedBanner {
  component: string;
  reason: string;
  recovered: boolean;
  atMs: number;
}

/** One thing the store/render layer should adopt from a decoded frame. */
export type AdapterEffect =
  | { kind: "workspace-state"; value: WorkspaceStatusInput }
  | { kind: "session-view"; value: SessionViewInput }
  | {
      kind: "conversation-items";
      workspace: string;
      sessionId: string;
      throughSeq: number;
      items: ConversationItem[];
    }
  | { kind: "typing"; value: TypingReveal }
  | { kind: "task-catalog"; value: TaskCatalogInput }
  | { kind: "degraded"; value: DegradedBanner }
  | { kind: "ignored"; shape: string };

export type AdapterLogLevel = "debug" | "info" | "warn";
export type AdapterLogger = (level: AdapterLogLevel, message: string) => void;

// --- the store's bubble/card vocabulary (ConversationDelta items) -----------

/** The `kind` discriminators the store's ConversationItem union recognizes. */
export const KNOWN_ITEM_KINDS: ReadonlySet<string> = new Set([
  "user-turn",
  "text",
  "thinking",
  "tool",
  "permission",
  "result",
  "compact-boundary",
  "error",
  "retry",
  "system",
]);

// --- the adapter ------------------------------------------------------------

export class StateAdapter {
  private readonly ignoreCounts = new Map<string, number>();
  private readonly loggedShapes = new Set<string>();

  /**
   * DEBUG sink for the explicit-ignore path — injected so tests can assert the
   * once-per-name discipline and the stitch phase can route it into wslog. The
   * no-op default keeps the adapter usable standalone.
   */
  constructor(private readonly log: AdapterLogger = () => {}) {}

  /**
   * Map ONE decoded frame to the effects the store/render layer should adopt.
   * A `StateSnapshot` fans out into the per-workspace effects it carries; an
   * unsupported frame or conversation-item resolves to an `ignored` effect.
   */
  apply(frame: FrontendFrame): AdapterEffect[] {
    switch (frame.frame.case) {
      case "snapshot": {
        const s = frame.frame.value;
        return [
          ...s.workspaces.map((ws) => this.workspaceEffect(ws)),
          ...s.sessions.map((sv) => this.sessionEffect(sv)),
          ...s.catalogs.map((tc) => this.catalogEffect(tc)),
        ];
      }
      case "workspaceState":
        return [this.workspaceEffect(frame.frame.value)];
      case "sessionView":
        return [this.sessionEffect(frame.frame.value)];
      case "conversationDelta":
        return this.conversationEffects(frame.frame.value);
      case "typingDelta":
        return [this.typingEffect(frame.frame.value)];
      case "taskCatalog":
        return [this.catalogEffect(frame.frame.value)];
      case "degradedNotice":
        return [this.degradedEffect(frame.frame.value)];
      case "commandAck":
        // Registered unsupported shape: no webapp visual (§11).
        return [this.ignore("commandAck")];
      default: {
        // Exhaustiveness guard: a new frame variant is a compile error here,
        // never a silent skip.
        const never: never = frame.frame;
        throw new Error(`state-adapter: unhandled frame variant ${JSON.stringify(never)}`);
      }
    }
  }

  /** The per-shape ignore tallies (a copy), for the coverage/diagnostics view. */
  ignoredCounts(): ReadonlyMap<string, number> {
    return new Map(this.ignoreCounts);
  }

  // --- mappers --------------------------------------------------------------

  private workspaceEffect(ws: WorkspaceState): AdapterEffect {
    return {
      kind: "workspace-state",
      value: {
        workspace: ws.workspace,
        sessionId: ws.sessionId,
        state: renderStateKeyword(ws.state),
        turnActive: ws.turnActive,
        liveTaskCount: Number(ws.liveTaskCount),
        mergePhase: ws.mergePhase,
      },
    };
  }

  private sessionEffect(sv: SessionView): AdapterEffect {
    return {
      kind: "session-view",
      value: {
        workspace: sv.workspace,
        sessionId: sv.sessionId,
        model: sv.model,
        slug: sv.slug,
        title: sv.title,
        totalTokens: Number(sv.totalTokens),
        totalCostUsd: sv.totalCostUsd,
        contextWindow: Number(sv.contextWindow),
        permissionMode: sv.permissionMode,
        shimAttached: sv.shimAttached,
      },
    };
  }

  private typingEffect(td: TypingDelta): AdapterEffect {
    return {
      kind: "typing",
      value: {
        workspace: td.workspace,
        sessionId: td.sessionId,
        uuid: td.uuid,
        blockIndex: td.blockIndex,
        // Validated by decodeFrontendFrame; narrow for the output type.
        kind: td.kind as TypingReveal["kind"],
        delta: td.delta,
        blockId: `${td.uuid}:${td.blockIndex}`,
      },
    };
  }

  private catalogEffect(tc: TaskCatalog): AdapterEffect {
    return {
      kind: "task-catalog",
      value: {
        workspace: tc.workspace,
        sessionId: tc.sessionId,
        entries: tc.tasks.map(taskEntryToCounter),
      },
    };
  }

  private degradedEffect(dn: DegradedNotice): AdapterEffect {
    return {
      kind: "degraded",
      value: {
        component: dn.component,
        reason: dn.reason,
        recovered: dn.recovered,
        atMs: Number(dn.atMs),
      },
    };
  }

  private conversationEffects(cd: ConversationDelta): AdapterEffect[] {
    const items: ConversationItem[] = [];
    const ignored: AdapterEffect[] = [];
    for (const raw of cd.items) {
      const kind = String((raw as Record<string, unknown>).kind);
      if (!KNOWN_ITEM_KINDS.has(kind)) {
        ignored.push(this.ignore(`conversation-item:${kind}`));
        continue;
      }
      items.push(buildConversationItem(kind, raw as Record<string, unknown>));
    }
    return [
      {
        kind: "conversation-items",
        workspace: cd.workspace,
        sessionId: cd.sessionId,
        throughSeq: Number(cd.throughSeq),
        items,
      },
      ...ignored,
    ];
  }

  // --- explicit ignore path -------------------------------------------------

  private ignore(shape: string): AdapterEffect {
    this.ignoreCounts.set(shape, (this.ignoreCounts.get(shape) ?? 0) + 1);
    if (!this.loggedShapes.has(shape)) {
      this.loggedShapes.add(shape);
      const why = UNSUPPORTED_SHAPES.get(shape);
      this.log(
        "debug",
        `state-adapter: ignoring unsupported shape '${shape}' (no webapp ` +
          `visual)${why ? ` — ${why}` : ""}`,
      );
    }
    return { kind: "ignored", shape };
  }
}

// --- render-state + task-status mapping -------------------------------------

const RENDER_STATE_KEYWORD: Record<RenderState, WebRenderState | null> = {
  [RenderState.UNSPECIFIED]: null,
  [RenderState.INIT]: "init",
  [RenderState.IDLE]: "idle",
  [RenderState.IDLE_ASYNC]: "idle_async",
  [RenderState.THINKING]: "thinking",
  [RenderState.PERMISSION]: "permission",
  [RenderState.DONE]: "done",
  [RenderState.STOP_FAILED]: "stop_failed",
  [RenderState.MERGING]: "merging",
  [RenderState.MERGE_QUEUED]: "merge_queued",
  [RenderState.MERGE_CONFLICT]: "merge_conflict",
  [RenderState.MERGE_FAILED]: "merge_failed",
  [RenderState.MERGED]: "merged",
  [RenderState.DEAD]: "dead",
  [RenderState.DEGRADED]: "degraded",
};

function renderStateKeyword(state: RenderState): WebRenderState {
  const kw = RENDER_STATE_KEYWORD[state];
  if (kw === null || kw === undefined) {
    throw new Error(
      `state-adapter: WorkspaceState has unrenderable RenderState ${state}`,
    );
  }
  return kw;
}

/**
 * Map a TaskCatalog status onto the shared counter vocabulary
 * (counter-menu.ts): the roster only shows active work, so terminal statuses
 * collapse onto the two the counter has — `done` (a clean end) and `error` (a
 * bad one). `killed`/`lost` are failures, `stopped` a benign halt.
 */
function taskStatusToCounter(status: string): CounterStatus {
  switch (status) {
    case "running":
      return "running";
    case "done":
    case "stopped":
      return "done";
    case "error":
    case "killed":
    case "lost":
      return "error";
    default:
      throw new Error(`state-adapter: TaskEntry has unmappable status '${status}'`);
  }
}

function taskEntryToCounter(t: TaskEntry): CounterEntry {
  return {
    id: t.taskId,
    summary: t.description,
    detail: t.kind,
    status: taskStatusToCounter(t.status),
    nested: false,
  };
}

// --- ConversationDelta item → ConversationItem ------------------------------
//
// The daemon (G9 translate.go) pre-renders complete conversation additions
// INTO the store's bubble/card vocabulary and ships them as protojson Structs.
// This builder is the receiving contract: it validates each item's `kind`
// discriminator and its load-bearing fields loudly, then materializes the
// typed ConversationItem. Deeply-nested optional payloads (a tool result, an
// async source, a permission preview) are adopted by shape — validating their
// interior is the daemon-side converter's job (§5.1), not re-litigated here.

type Obj = Record<string, unknown>;

function buildConversationItem(kind: string, o: Obj): ConversationItem {
  switch (kind) {
    case "user-turn":
      return {
        kind: "user-turn",
        requestId: reqStr(o, "requestId", "user-turn"),
        content: reqArr(o, "content", "user-turn") as ContentBlock[],
        ts: reqStr(o, "ts", "user-turn"),
        ...(has(o, "origin") ? { origin: reqStr(o, "origin", "user-turn") } : {}),
      } satisfies UserTurnItem;
    case "text":
      return {
        kind: "text",
        blockId: reqStr(o, "blockId", "text"),
        messageId: reqStr(o, "messageId", "text"),
        text: reqStr(o, "text", "text"),
        done: reqBool(o, "done", "text"),
        ts: reqStr(o, "ts", "text"),
        ...(has(o, "parentToolUseId")
          ? { parentToolUseId: reqStr(o, "parentToolUseId", "text") }
          : {}),
        ...(has(o, "error")
          ? { error: reqStr(o, "error", "text") as AssistantMessageError }
          : {}),
      } satisfies TextItem;
    case "thinking":
      return {
        kind: "thinking",
        blockId: reqStr(o, "blockId", "thinking"),
        messageId: reqStr(o, "messageId", "thinking"),
        text: reqStr(o, "text", "thinking"),
        done: reqBool(o, "done", "thinking"),
        ...(has(o, "signature")
          ? { signature: reqStr(o, "signature", "thinking") }
          : {}),
        ...(has(o, "parentToolUseId")
          ? { parentToolUseId: reqStr(o, "parentToolUseId", "thinking") }
          : {}),
      } satisfies ThinkingItem;
    case "tool":
      return buildToolItem(o);
    case "permission":
      return {
        kind: "permission",
        requestId: reqStr(o, "requestId", "permission"),
        toolUseId: reqStr(o, "toolUseId", "permission"),
        toolName: reqStr(o, "toolName", "permission"),
        input: reqPresent(o, "input", "permission"),
        ...(has(o, "preview") ? { preview: o.preview as PermissionPreview } : {}),
        ...(has(o, "resolution")
          ? { resolution: o.resolution as PermissionItem["resolution"] }
          : {}),
      } satisfies PermissionItem;
    case "result":
      return {
        kind: "result",
        subtype: reqStr(o, "subtype", "result") as ResultSubtype,
        durationMs: reqNum(o, "durationMs", "result"),
        sincePrevFinalMs: reqNum(o, "sincePrevFinalMs", "result"),
        numTurns: reqNum(o, "numTurns", "result"),
        totalCostUsd: reqNum(o, "totalCostUsd", "result"),
        usage: reqObj(o, "usage", "result") as unknown as Usage,
        isError: reqBool(o, "isError", "result"),
        ...(has(o, "resultText")
          ? { resultText: reqStr(o, "resultText", "result") }
          : {}),
        context: (o.context ?? null) as ResultContext | null,
      } satisfies ResultItem;
    case "compact-boundary":
      return {
        kind: "compact-boundary",
        trigger: reqStr(o, "trigger", "compact-boundary") as "auto" | "manual",
        preTokens: reqNum(o, "preTokens", "compact-boundary"),
        postTokens: reqNum(o, "postTokens", "compact-boundary"),
      } satisfies CompactBoundaryItem;
    case "error":
      return {
        kind: "error",
        code: reqStr(o, "code", "error"),
        message: reqStr(o, "message", "error"),
        recoverable: reqBool(o, "recoverable", "error"),
      } satisfies ErrorItem;
    case "retry":
      return {
        kind: "retry",
        attempt: reqNum(o, "attempt", "retry"),
        reason: reqStr(o, "reason", "retry"),
        fatal: reqBool(o, "fatal", "retry"),
      } satisfies RetryItem;
    case "system":
      return {
        kind: "system",
        subtype: reqStr(o, "subtype", "system"),
      } satisfies SystemItem;
    default:
      // Unreachable: callers gate on KNOWN_ITEM_KINDS. Loud, never silent.
      throw new Error(`state-adapter: no builder for conversation item kind '${kind}'`);
  }
}

function buildToolItem(o: Obj): ToolItem {
  const item: ToolItem = {
    kind: "tool",
    toolUseId: reqStr(o, "toolUseId", "tool"),
    toolName: reqStr(o, "toolName", "tool"),
    messageId: reqStr(o, "messageId", "tool"),
    ts: reqStr(o, "ts", "tool"),
    inputJson: has(o, "inputJson") ? reqStr(o, "inputJson", "tool") : "",
    inputDone: has(o, "inputDone") ? reqBool(o, "inputDone", "tool") : false,
  };
  if (has(o, "parentToolUseId")) item.parentToolUseId = reqStr(o, "parentToolUseId", "tool");
  if (has(o, "contextTokens")) item.contextTokens = reqNum(o, "contextTokens", "tool");
  if (has(o, "input")) item.input = o.input as Record<string, unknown>;
  if (has(o, "progress")) item.progress = reqStr(o, "progress", "tool");
  if (has(o, "progressElapsedS")) item.progressElapsedS = reqNum(o, "progressElapsedS", "tool");
  if (has(o, "notification")) item.notification = o.notification as ToolItem["notification"];
  if (has(o, "resultTs")) item.resultTs = reqStr(o, "resultTs", "tool");
  if (has(o, "asyncSource")) item.asyncSource = o.asyncSource as AsyncSource;
  if (has(o, "taskOutput")) item.taskOutput = reqStr(o, "taskOutput", "tool");
  if (has(o, "result")) item.result = o.result as ToolItem["result"];
  return item;
}

// --- field readers (loud) ---------------------------------------------------

function has(o: Obj, key: string): boolean {
  return o[key] !== undefined && o[key] !== null;
}

function reqPresent(o: Obj, key: string, ctx: string): unknown {
  if (!(key in o)) {
    throw new Error(`state-adapter: ${ctx} item missing required \`${key}\``);
  }
  return o[key];
}

function reqStr(o: Obj, key: string, ctx: string): string {
  const v = o[key];
  if (typeof v !== "string") {
    throw new Error(
      `state-adapter: ${ctx} item field \`${key}\` must be a string (got ${typeof v})`,
    );
  }
  return v;
}

function reqNum(o: Obj, key: string, ctx: string): number {
  const v = o[key];
  if (typeof v !== "number") {
    throw new Error(
      `state-adapter: ${ctx} item field \`${key}\` must be a number (got ${typeof v})`,
    );
  }
  return v;
}

function reqBool(o: Obj, key: string, ctx: string): boolean {
  const v = o[key];
  if (typeof v !== "boolean") {
    throw new Error(
      `state-adapter: ${ctx} item field \`${key}\` must be a boolean (got ${typeof v})`,
    );
  }
  return v;
}

function reqArr(o: Obj, key: string, ctx: string): unknown[] {
  const v = o[key];
  if (!Array.isArray(v)) {
    throw new Error(`state-adapter: ${ctx} item field \`${key}\` must be an array`);
  }
  return v;
}

function reqObj(o: Obj, key: string, ctx: string): Obj {
  const v = o[key];
  if (typeof v !== "object" || v === null || Array.isArray(v)) {
    throw new Error(`state-adapter: ${ctx} item field \`${key}\` must be an object`);
  }
  return v as Obj;
}
