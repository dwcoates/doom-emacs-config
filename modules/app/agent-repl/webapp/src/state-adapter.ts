/**
 * state-adapter — maps decoded `agentshim.frontend.v1` frames onto the
 * webapp's EXISTING store/render input shapes, for the ALREADY-supported
 * visuals only (§11 scope rule).
 *
 * This is the seam between the new protobuf frontend surface and the webapp's
 * current rendering vocabulary. It is a PURE producer: `apply()` returns typed
 * effects describing what the store/render layer should adopt, and never
 * touches the DOM or mutates a store itself.
 *
 * S9 RECOMPOSITION — the daemon (translate.go) became a CURATOR that pushes the
 * TYPED data.v1/core.v1 payloads (frontend-proto.ts `ConversationItemFrame`),
 * and THIS adapter now does the DECOMPOSITION the daemon used to do: it fans a
 * payload back into the store's bubble/card vocabulary. The mapping mirrors the
 * old translate.go decomposition semantics:
 * - assistantMessage (ApiAssistantMessage) → one item per content block:
 *   text → TextItem, thinking → ThinkingItem, tool_use → ToolItem. A prose
 *   block carries TWO ids: `messageId` is the Anthropic message id (the
 *   identity it shares with its own live stream, so a finished block can be
 *   paired with the preview the deltas grew), and `uuid` is the record's own
 *   envelope-derived identity (what the store dedups a replayed record on).
 *   The index within `content` is NOT an identity — see assistantMessageItems.
 * - userMessage (ApiUserMessage) → each tool_result block → a ToolItem
 *   (result-only, empty toolName — it reconciles onto the tool_use item by
 *   toolUseId in the store); the remaining blocks → one UserTurnItem (none = no
 *   turn, the tool-feedback case).
 * - toolUse (ToolUseBlock) / toolResult (ToolResultBlock) arms → the same two
 *   ToolItems, standalone; the store field-merges the pair by toolUseId.
 * - result (ResultMessage) → ResultItem; contextCleared (core.v1.ContextCleared,
 *   an EMPTY message) → ContextClearedItem and contextCompacted
 *   (core.v1.ContextCompacted) → ContextCompactedItem — the CLEAR and the
 *   COMPACTION,
 *   coalesced and de-duplicated by the daemon so each arrives ONCE as one
 *   complete fact; permission (core.v1.PermissionItem) →
 *   PermissionItem; systemFailure (SystemFailureItem) → the classified
 *   failure card (F4) — the ApiErrorLine `apiError` arm it superseded is
 *   RETIRED (step 11).
 * - sessionInit (SessionInitView) → the /status panel's SystemInit source.
 *
 * EXPLICIT IGNORE (no new visuals; §11): a frame variant in `UNSUPPORTED_SHAPES`
 * (commandAck, daemonView) and a CONVERSATION-ITEM shape with no webapp visual
 * — a `toolUseResult` arm (no correlation key on the proto arm; the tool result
 * is carried by tool_result blocks), a `signature` content delta, an image /
 * tool_reference / fallback / unknown content block — are IGNORED EXPLICITLY:
 * typed as an `{ kind: "ignored" }` effect, counted, and debug-logged once per
 * distinct shape name. They are never crashed on and never silently dropped.
 */

import type { CounterEntry, CounterStatus } from "./counter-menu.js";
import type { ContentBlock, ModelInfo, ModelUsage, ResultSubtype, Usage } from "./protocol.js";
import { recordBlockIdentity } from "./streaming.js";
import type {
  ContextClearedItem,
  ContextCompactedItem,
  ConversationItem,
  PermissionItem,
  ResultItem,
  SystemFailureCard,
  TextItem,
  ThinkingItem,
  ToolItem,
  UserTurnItem,
} from "./store.js";
import {
  ERROR_CLASSES,
  RenderState,
  UNSUPPORTED_SHAPES,
  type ConversationDelta,
  type ConversationItemArm,
  type ConversationItemFrame,
  type ErrorClass,
  type SystemFailure,
  type FrontendFrame,
  type HeartbeatView,
  type InterruptOutcome,
  type InterruptWindow,
  type ProgressView,
  type ProgressWindow,
  type QueueEntry,
  type QueueView,
  type SessionInitView,
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
  | "ready"
  | "idle_async"
  | "thinking"
  | "clearing"
  | "compacting"
  | "permission"
  | "done"
  | "interrupted"
  | "vendor_blocked"
  | "merging"
  | "merge_queued"
  | "merge_conflict"
  | "merge_failed"
  | "merged"
  | "dead"
  | "degraded"
  | "dormant";

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
  /** Durable CLI conversation uuid — the resume/rebind key. */
  claudeSessionId: string;
  /** Working directory a rebind's CreateSessionCmd needs. */
  cwd: string;
  /** CLAUDE_CONFIG_DIR the session runs against (account identity, S8). */
  configDir: string;
  /** SDK-published menu; the browser renders it but never owns selection. */
  models: ModelInfo[];
}

/**
 * TypingDelta → the store's live-typing feed, structurally a
 * {@link StreamDelta} so `streaming.ts` can reconcile it directly.
 *
 * No block id is carried. The identity of a streamed block is derived in ONE
 * place — `streaming.ts` — precisely so a second derivation here cannot drift
 * from the one the finished record is matched against, which is the bug this
 * shape used to have.
 */
export interface TypingReveal {
  workspace: string;
  sessionId: string;
  /**
   * The ANTHROPIC message id every delta of one message shares, stamped at the
   * source (the shim's `message_start` tracker) and carried opaquely since.
   *
   * The proto field it arrives in is spelled `uuid`, which is a misnomer it has
   * outlived: it has never held an SDK envelope uuid, because the SDK mints a
   * fresh one per emitted event and a message's chunks would then share nothing.
   */
  messageId: string;
  /** The block's ordinal within that message, as the API numbered it. */
  blockIndex: number;
  kind: "text" | "thinking" | "input_json";
  delta: string;
}

/**
 * One long-running tool's liveness tick (E4), flattened from a `HeartbeatView`.
 * The store attributes it to the open `ToolItem` bearing `toolUseId`.
 */
export interface ToolProgressInput {
  workspace: string;
  sessionId: string;
  toolUseId: string;
  toolName: string;
  parentToolUseId: string;
  /** The SDK's raw elapsed clock for the running tool, in seconds. */
  elapsedSeconds: number;
}

/**
 * The session's held-prompt queue (E4). A REPLACEMENT, not a delta: the store
 * adopts `entries` wholesale, so an empty list empties the queue.
 */
export interface QueueInput {
  workspace: string;
  sessionId: string;
  entries: QueueEntry[];
}

/**
 * One activity window on the progress footer, flattened from a
 * `ProgressWindow`. `null` in a `ProgressInput` slot means the window is
 * closed — the adapter never materializes an inactive placeholder, so a
 * consumer's truthiness check IS the open/closed question.
 */
export interface ProgressWindowInput {
  sinceMs: number;
  detail: string;
}

/**
 * The interrupt window (I1), flattened from an `InterruptWindow` exactly as the
 * other windows are: `null` in the `ProgressInput` slot means no interrupt to
 * speak of.
 *
 * The daemon OPENS it on the shim's ack and CLEARS it when the next turn
 * starts. The webapp keeps no bookkeeping of its own — it renders the frame it
 * was sent, so a frame with the window closed leaves nothing behind.
 */
export interface InterruptInput {
  sinceMs: number;
  /** Which of the three answers the ack decided; never absent while open. */
  outcome: InterruptOutcome;
}

/** The rate-limit window, which carries structured detail rather than a line. */
export interface RateLimitInput {
  /** Epoch SECONDS (the vendor event's own unit). */
  resetsAt: number;
  utilization: number;
  status: string;
}

/**
 * `ProgressView` → the consolidated progress footer's whole input (F1).
 *
 * The daemon resolved ALL of this; the adapter only maps the phase enum into
 * the webapp's render-state keyword and flattens each window's open/closed
 * question into a nullable slot. Nothing here is re-derived, and there is
 * deliberately no output-token figure to carry.
 */
export interface ProgressInput {
  workspace: string;
  sessionId: string;
  // NO PHASE (F5): the footer reads the workspace's one authoritative render
  // state off `StoreState.renderState`, which is the same message the tab bar
  // and the sidebar dot read. The copy that used to live here refreshed on the
  // progress resolver's triggers and went stale.
  /** 0 = no turn in flight. */
  turnStartedAtMs: number;
  thinkingTokens: number;
  /** THIS turn's cumulative cached + uncached input tokens. */
  inputTokens: number;
  ttftMs: number;
  compacting: ProgressWindowInput | null;
  retrying: ProgressWindowInput | null;
  authenticating: ProgressWindowInput | null;
  hook: ProgressWindowInput | null;
  rateLimited: RateLimitInput | null;
  /** The session reporting it is parked on the user. NOT a phase. */
  blocked: ProgressWindowInput | null;
  /**
   * The interrupt window (I1), ack-opened and next-turn-cleared BY THE DAEMON.
   * `null` = closed, which is the whole of the webapp's bookkeeping.
   */
  interrupt: InterruptInput | null;
  /**
   * The CLASSIFIED error state (F4). The footer takes its color from the
   * failure's class rather than from a hardcoded red no other surface
   * consulted, and addresses the card through the failure's own uuid.
   * `null` = no error standing.
   */
  failure: SystemFailureCard | null;
  pendingPermissions: number;
  queueDepth: number;
  liveTaskCount: number;
}

/** TaskCatalog → the async/task roster input (topbar counter vocabulary). */
export interface TaskCatalogInput {
  workspace: string;
  sessionId: string;
  entries: CounterEntry[];
}

/** SessionInitView → the /status panel's SystemInit snapshot source. */
export interface SessionInitInput {
  workspace: string;
  sessionId: string;
  /** The data.v1.SystemInit payload, adopted by shape (read leniently). */
  init: Record<string, unknown>;
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
  | { kind: "tool-progress"; value: ToolProgressInput }
  | { kind: "queue"; value: QueueInput }
  | { kind: "task-catalog"; value: TaskCatalogInput }
  | { kind: "progress"; value: ProgressInput }
  | { kind: "session-init"; value: SessionInitInput }
  | { kind: "ignored"; shape: string };

export type AdapterLogLevel = "debug" | "info" | "warn";
export type AdapterLogger = (level: AdapterLogLevel, message: string) => void;

// --- ingest-time user-turn receipt -------------------------------------------

/** A user turn as it ARRIVED, before the store or the renderer saw it. */
export interface UserTurnReceipt {
  requestId: string;
  /** The delta's `throughSeq` — the daemon's sequence for this batch. */
  seq: number;
  /** Total length of the turn's text blocks. */
  len: number;
  /**
   * Whether the batch carries ground the store has not covered. A connect
   * resync replays history at or below `lastSeq`, so its user turns are
   * re-deliveries of prompts already ingested once, not arrivals.
   */
  live: boolean;
}

/**
 * The user turn EFFECTS carry, if any — the ingest-side counterpart to render's
 * `feed: user turn rendering` line. The two are separated by a coalesced
 * animation frame, so with only the render line a suspended rAF and a late
 * delta look identical in the log; this pins WHEN the delta reached the webapp.
 *
 * LASTSEQ must be read BEFORE the effects are ingested, since ingesting is what
 * advances it. Null when the batch carries no user turn, or when the turn has
 * no text (a pure tool-feedback message yields no turn at all, and a textless
 * one times nothing).
 */
export function userTurnReceipt(effects: AdapterEffect[], lastSeq: number): UserTurnReceipt | null {
  for (const effect of effects) {
    if (effect.kind !== "conversation-items") continue;
    for (const item of effect.items) {
      if (item.kind !== "user-turn") continue;
      let len = 0;
      for (const block of item.content) {
        // Only text blocks carry prompt length; the catch-all ContentBlock arm
        // makes `text` unknown, so read it defensively rather than casting.
        if (block.type === "text" && typeof block.text === "string") len += block.text.length;
      }
      if (len === 0) continue;
      return {
        requestId: item.requestId,
        seq: effect.throughSeq,
        len,
        live: effect.throughSeq > lastSeq,
      };
    }
  }
  return null;
}

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
          ...s.inits.map((si) => this.sessionInitEffect(si)),
          ...s.queues.map((q) => this.queueEffect(q)),
          ...s.progress.map((p) => this.progressEffect(p)),
        ];
      }
      case "workspaceState":
        return [this.workspaceEffect(frame.frame.value)];
      case "sessionView":
        return [this.sessionEffect(frame.frame.value)];
      case "conversationDelta":
        return this.conversationEffects(frame.frame.value);
      case "typingDelta":
        return this.typingEffects(frame.frame.value);
      case "heartbeat":
        return [this.heartbeatEffect(frame.frame.value)];
      case "queue":
        return [this.queueEffect(frame.frame.value)];
      case "progress":
        return [this.progressEffect(frame.frame.value)];
      case "taskCatalog":
        return [this.catalogEffect(frame.frame.value)];
      case "sessionInit":
        return [this.sessionInitEffect(frame.frame.value)];
      case "commandAck":
        // Registered unsupported shape: no webapp visual (§11).
        return [this.ignore("commandAck")];
      case "daemonView":
        // Registered unsupported shape (S7): decoded for wire parity, but boot
        // detection / version warnings are an Emacs-frontend concern.
        return [this.ignore("daemonView")];
      case "workspaceAvailable":
        // This frame is host-only by transport contract.  Keeping a typed
        // ignore here makes an accidental GUI delivery diagnosable rather than
        // letting a new frame variant turn into an adapter crash.
        return [this.ignore("workspaceAvailable")];
      case "hostAction":
        // Host actions are consumed by Emacs, never rendered by the GUI.
        return [this.ignore("hostAction")];
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
        claudeSessionId: sv.claudeSessionId,
        cwd: sv.cwd,
        configDir: sv.configDir,
        models: sv.modelOptions.map((model) => ({
          value: model.value,
          displayName: model.displayName,
          description: model.description,
        })),
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

  private sessionInitEffect(si: SessionInitView): AdapterEffect {
    return {
      kind: "session-init",
      value: { workspace: si.workspace, sessionId: si.sessionId, init: si.init },
    };
  }

  /**
   * A `text`/`thinking`/`input_json` content delta grows the reveal feed; a
   * `signature` delta streams the thinking block's signature, which the webapp
   * renders no live preview for — so it is ignored (counted, logged once).
   */
  /**
   * A long-tool liveness heartbeat (E4). Purely additive to an EXISTING visual:
   * the running tool chip already renders an elapsed clock off
   * `ToolItem.progressElapsedS` (stream-member.ts), it just had nothing feeding
   * it while HeartbeatProgress was being dropped daemon-side. So this is not a
   * new visual and stays inside the §11 scope rule.
   */
  private heartbeatEffect(hv: HeartbeatView): AdapterEffect {
    return {
      kind: "tool-progress",
      value: {
        workspace: hv.workspace,
        sessionId: hv.sessionId,
        toolUseId: hv.toolUseId,
        toolName: hv.toolName,
        parentToolUseId: hv.parentToolUseId,
        elapsedSeconds: hv.elapsedSeconds,
      },
    };
  }

  /**
   * The held-prompt queue (E4). Passed through structurally — the daemon owns
   * both the ordering and the classification, and re-deriving either here is
   * exactly the second source of truth the redesign exists to remove.
   */
  private queueEffect(qv: QueueView): AdapterEffect {
    return {
      kind: "queue",
      value: { workspace: qv.workspace, sessionId: qv.sessionId, entries: qv.entries },
    };
  }

  /**
   * The progress footer's input (F1). Everything here was resolved daemon-side,
   * so this maps rather than derives: the phase enum becomes the webapp's
   * render-state keyword and each window collapses to its open detail or null.
   */
  private progressEffect(pv: ProgressView): AdapterEffect {
    return {
      kind: "progress",
      value: {
        workspace: pv.workspace,
        sessionId: pv.sessionId,
        turnStartedAtMs: pv.turnStartedAtMs,
        thinkingTokens: pv.thinkingTokens,
        inputTokens: pv.inputTokens,
        ttftMs: pv.ttftMs,
        compacting: openWindow(pv.compacting),
        retrying: openWindow(pv.retrying),
        authenticating: openWindow(pv.authenticating),
        hook: openWindow(pv.hook),
        blocked: openWindow(pv.blocked),
        interrupt: openInterrupt(pv.interrupt),
        rateLimited:
          pv.rateLimited !== undefined && pv.rateLimited.active
            ? {
                resetsAt: pv.rateLimited.resetsAt,
                utilization: pv.rateLimited.utilization,
                status: pv.rateLimited.status,
              }
            : null,
        failure: pv.failure === undefined ? null : systemFailureFrom(pv.failure),
        pendingPermissions: pv.pendingPermissions,
        queueDepth: pv.queueDepth,
        liveTaskCount: pv.liveTaskCount,
      },
    };
  }

  private typingEffects(td: TypingDelta): AdapterEffect[] {
    if (td.kind === "signature") return [this.ignore("content-delta:signature")];
    return [
      {
        kind: "typing",
        value: {
          workspace: td.workspace,
          sessionId: td.sessionId,
          messageId: td.uuid,
          blockIndex: td.blockIndex,
          kind: td.kind,
          delta: td.delta,
        },
      },
    ];
  }

  private conversationEffects(cd: ConversationDelta): AdapterEffect[] {
    const items: ConversationItem[] = [];
    const ignored: AdapterEffect[] = [];
    for (const frame of cd.items) {
      const built = itemsFromFrame(frame);
      items.push(...built.items);
      for (const shape of built.ignores) ignored.push(this.ignore(shape));
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
  // The two CONTEXT CUTS. Red like thinking — the agent is busy and a prompt
  // cannot land yet — and only the phase word distinguishes them.
  [RenderState.CLEARING]: "clearing",
  [RenderState.COMPACTING]: "compacting",
  [RenderState.PERMISSION]: "permission",
  [RenderState.DONE]: "done",
  [RenderState.READY]: "ready",
  [RenderState.INTERRUPTED]: "interrupted",
  [RenderState.VENDOR_BLOCKED]: "vendor_blocked",
  // DEPRECATED upstream and no longer resolved by the SSM, but still mapped:
  // an old daemon binary can push it, and erroring on a state we know how to
  // render would be worse than rendering it. It resolves to the same purple
  // VENDOR_BLOCKED means, because that is what it always was — a turn that
  // ended on something only a human or the vendor can fix.
  [RenderState.STOP_FAILED]: "vendor_blocked",
  [RenderState.MERGING]: "merging",
  [RenderState.MERGE_QUEUED]: "merge_queued",
  [RenderState.MERGE_CONFLICT]: "merge_conflict",
  [RenderState.MERGE_FAILED]: "merge_failed",
  [RenderState.MERGED]: "merged",
  [RenderState.DEAD]: "dead",
  [RenderState.DEGRADED]: "degraded",
  [RenderState.DORMANT]: "dormant",
};

function renderStateKeyword(state: RenderState): WebRenderState {
  const kw = RENDER_STATE_KEYWORD[state];
  if (kw === null || kw === undefined) {
    throw new Error(`state-adapter: WorkspaceState has unrenderable RenderState ${state}`);
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

// --- ConversationItemFrame → ConversationItem[] -----------------------------
//
// The daemon (translate.go, S9) is a CURATOR: it pushes the TYPED data.v1 /
// core.v1 payload for each conversation addition and no longer pre-renders the
// store vocabulary. This decomposition is the receiving contract — it fans one
// typed payload into the store's items, reading the load-bearing fields loudly
// and adopting deep interiors by shape (§5.1).
//
// KNOWN WIRE GAPS (surfaced to the coordinator, not papered over):
// - `parentToolUseId` (subagent nesting) has no home in the ConversationItem
//   envelope or in ApiAssistantMessage/ToolUseBlock, so items decomposed here
//   carry none — subagent bubbles render on the main chain until the proto
//   threads the parent id.
// - `toolUseResult` (the rich structured result) has no correlation key on its
//   arm and no built curator counterpart; it is routed to the ignore path. The
//   basic tool result rides the `toolResult` block path instead.
// - permission `toolUseId`/`preview` and user-turn `origin` have no source in
//   the typed payloads and are left unset.

type Obj = Record<string, unknown>;

/** Decompose one decoded conversation item into store items + ignore shapes. */
function itemsFromFrame(frame: ConversationItemFrame): { items: ConversationItem[]; ignores: string[] } {
  const arm: ConversationItemArm = frame.arm;
  switch (arm) {
    case "assistantMessage":
      return assistantMessageItems(frame);
    case "userMessage":
      return userMessageItems(frame);
    case "toolUse":
      return { items: [toolItemFromUse(frame.payload, frame.uuid, tsFromMs(frame.tsMs))], ignores: [] };
    case "toolResult":
      return { items: [toolItemFromResult(frame.payload, frame.uuid, tsFromMs(frame.tsMs))], ignores: [] };
    case "toolUseResult":
      // No correlation key on the arm + unbuilt curator counterpart; ignored.
      return { items: [], ignores: ["conversation-item:toolUseResult"] };
    case "result":
      return { items: [resultItemFrom(frame.payload)], ignores: [] };
    case "contextCleared":
      // An EMPTY message: its existence and position are the whole fact, so
      // there is nothing to read off the payload but the envelope's uuid.
      return { items: [{ kind: "context-cleared", uuid: frame.uuid }], ignores: [] };
    case "contextCompacted":
      return { items: [contextCompactedItem(frame.payload, frame.uuid)], ignores: [] };
    case "permission":
      return { items: [permissionItemFrom(frame.payload, frame.uuid)], ignores: [] };
    case "systemFailure":
      return { items: [systemFailureCard(frame.payload, frame.uuid)], ignores: [] };
    case "skillBody":
      return { items: [skillBodyToolItem(frame.payload, frame.uuid, tsFromMs(frame.tsMs))], ignores: [] };
    default: {
      const never: never = arm;
      throw new Error(`state-adapter: unhandled conversation item arm ${JSON.stringify(never)}`);
    }
  }
}

/** The content-block oneof arm keys (data.v1.ContentBlock, protojson). */
const CONTENT_BLOCK_ARMS = [
  "text",
  "thinking",
  "toolUse",
  "toolResult",
  "image",
  "toolReference",
  "fallback",
] as const;

/** The single set content-block arm and its (shape-adopted) value. */
function contentBlockArm(block: Obj): { arm: string; value: Obj } {
  for (const arm of CONTENT_BLOCK_ARMS) {
    const v = block[arm];
    if (v !== undefined && v !== null) {
      return { arm, value: typeof v === "object" && !Array.isArray(v) ? (v as Obj) : {} };
    }
  }
  return { arm: "unknown", value: {} };
}

function assistantMessageItems(frame: ConversationItemFrame): {
  items: ConversationItem[];
  ignores: string[];
} {
  // The ANTHROPIC message id: the only identity a message shares with its own
  // live stream, and so the only thing that can pair a finished block with the
  // preview its deltas grew. Falls back to the envelope uuid when a payload
  // carries no id of its own.
  //
  // The block's own RECORD identity comes from `recordBlockIdentity` — the one
  // authority — rather than being spelled out a second time here.
  const messageId = pstr(frame.payload, "id") || frame.uuid;
  const ts = tsFromMs(frame.tsMs);
  const items: ConversationItem[] = [];
  const ignores: string[] = [];
  parr(frame.payload, "content").forEach((raw, index) => {
    const { arm, value } = contentBlockArm(ensureObj(raw));
    switch (arm) {
      case "text": {
        const item: TextItem = {
          kind: "text",
          ...recordBlockIdentity(frame.uuid, messageId, index),
          messageId,
          text: pstr(value, "text"),
          done: true,
          ts,
        };
        items.push(item);
        break;
      }
      case "thinking": {
        const item: ThinkingItem = {
          kind: "thinking",
          ...recordBlockIdentity(frame.uuid, messageId, index),
          messageId,
          text: pstr(value, "thinking"),
          done: true,
        };
        const sig = pstr(value, "signature");
        if (sig !== "") item.signature = sig;
        items.push(item);
        break;
      }
      case "toolUse":
        items.push(toolItemFromUse(value, messageId, ts));
        break;
      case "toolResult":
        items.push(toolItemFromResult(value, messageId, ts));
        break;
      default:
        // image | toolReference | fallback | unknown — no per-block visual.
        ignores.push(`content-block:${arm}`);
    }
  });
  return { items, ignores };
}

function userMessageItems(frame: ConversationItemFrame): {
  items: ConversationItem[];
  ignores: string[];
} {
  const msg = frame.payload;
  const ts = tsFromMs(frame.tsMs);
  const items: ConversationItem[] = [];
  const ignores: string[] = [];

  // ApiUserMessage.content oneof: content_string | content_blocks.
  if (typeof msg.contentString === "string") {
    items.push(userTurn(frame, [{ type: "text", text: msg.contentString }], ts));
    return { items, ignores };
  }

  const list = pobj(msg, "contentBlocks");
  const blocks = list ? parr(list, "blocks") : [];
  const turnContent: ContentBlock[] = [];
  for (const raw of blocks) {
    const { arm, value } = contentBlockArm(ensureObj(raw));
    if (arm === "toolResult") {
      // A tool-result block reconciles onto its tool_use item by toolUseId.
      items.push(toolItemFromResult(value, frame.uuid, ts));
    } else if (arm === "text") {
      turnContent.push({ type: "text", text: pstr(value, "text") });
    } else {
      // thinking/image/toolReference/fallback/unknown in a user message: keep
      // as a generic content block on the turn (render reads text primarily).
      turnContent.push({ type: arm, ...value });
    }
  }
  // A pure tool-feedback message (only tool_result blocks) yields no user-turn.
  if (turnContent.length > 0) items.push(userTurn(frame, turnContent, ts));
  return { items, ignores };
}

/**
 * One prompt bubble off a user message. The record's UUID rides along beside
 * the request id because the request id is EMPTY for every prompt the real
 * pipeline delivers (a transcript `UserLine`) and for all replayed history —
 * the uuid is what keeps two such prompts apart in the store (`userTurnKey`).
 */
function userTurn(
  frame: ConversationItemFrame,
  content: ContentBlock[],
  ts: string,
): UserTurnItem {
  const item: UserTurnItem = { kind: "user-turn", requestId: frame.requestId, content, ts };
  if (frame.uuid !== "") item.uuid = frame.uuid;
  return item;
}

/** ToolUseBlock {id, name, input, caller} → the tool CALL item (no result). */
function toolItemFromUse(use: Obj, messageUuid: string, ts: string): ToolItem {
  const item: ToolItem = {
    kind: "tool",
    toolUseId: pstr(use, "id"),
    toolName: pstr(use, "name"),
    messageId: messageUuid,
    ts,
    inputJson: "",
    inputDone: true,
  };
  const input = pobj(use, "input");
  if (input !== undefined) item.input = input;
  return item;
}

/**
 * ToolResultBlock {toolUseId, content, isError} → the tool RESULT item. Empty
 * toolName by contract (the name lives on the tool_use item this reconciles
 * onto by toolUseId; the store field-merges the pair).
 */
function toolItemFromResult(res: Obj, messageUuid: string, ts: string): ToolItem {
  return {
    kind: "tool",
    toolUseId: pstr(res, "toolUseId"),
    toolName: "",
    messageId: messageUuid,
    ts,
    inputJson: "",
    inputDone: true,
    resultTs: ts,
    result: {
      isError: pbool(res, "isError"),
      content: toolResultContent(res),
    },
  };
}

/**
 * SkillBodyItem {toolUseId, bodyMarkdown} → a PARTIAL tool item carrying only
 * the body.
 *
 * The daemon addresses a launched skill's SKILL.md to the tool_use_id of the
 * Skill call that launched it, which is the identity the store already files
 * that call's card under — so this merges INTO the existing card exactly the
 * way a tool_result does, and one invocation stays one card however its three
 * pieces interleave. Empty toolName by the same contract as
 * `toolItemFromResult`: the name lives on the call item, and the store
 * field-merges the pair rather than replacing it.
 */
function skillBodyToolItem(body: Obj, messageUuid: string, ts: string): ToolItem {
  return {
    kind: "tool",
    toolUseId: pstr(body, "toolUseId"),
    toolName: "",
    messageId: messageUuid,
    ts,
    inputJson: "",
    inputDone: true,
    skillBody: pstr(body, "bodyMarkdown"),
  };
}

/** ToolResultBlock content oneof (content_string | content_blocks) → store shape. */
function toolResultContent(res: Obj): string | Array<{ type: "text"; text: string }> {
  if (typeof res.contentString === "string") return res.contentString;
  const list = pobj(res, "contentBlocks");
  const blocks = list ? parr(list, "blocks") : [];
  return blocks.map((raw) => {
    const { arm, value } = contentBlockArm(ensureObj(raw));
    return { type: "text" as const, text: arm === "text" ? pstr(value, "text") : "" };
  });
}

/** RESULT_SUBTYPE_* (proto enum name) → the store's ResultSubtype vocabulary. */
function resultSubtype(name: string): ResultSubtype {
  switch (name) {
    case "RESULT_SUBTYPE_SUCCESS":
    case "success":
      return "success";
    case "RESULT_SUBTYPE_ERROR_MAX_TURNS":
    case "error_max_turns":
      return "error_max_turns";
    default:
      // ERROR_DURING_EXECUTION / ERROR_MAX_BUDGET_USD /
      // ERROR_MAX_STRUCTURED_OUTPUT_RETRIES / UNSPECIFIED all read as a
      // during-execution error (the store has no finer bucket for them).
      return "error_during_execution";
  }
}

/** data.v1.Usage (protojson camelCase) → the store's snake-cased Usage. */
function usageFrom(u: Obj | undefined): Usage {
  if (u === undefined) return { input_tokens: 0, output_tokens: 0 };
  return {
    input_tokens: pnum(u, "inputTokens"),
    output_tokens: pnum(u, "outputTokens"),
    cache_creation_input_tokens: pnum(u, "cacheCreationInputTokens"),
    cache_read_input_tokens: pnum(u, "cacheReadInputTokens"),
  };
}

function resultItemFrom(r: Obj): ResultItem {
  const item: ResultItem = {
    kind: "result",
    subtype: resultSubtype(pstr(r, "subtype")),
    durationMs: pnum(r, "durationMs"),
    // The cross-turn deltas are session state this per-item translator does not
    // hold; the store re-derives them (matching the old translate.go contract).
    sincePrevFinalMs: 0,
    numTurns: pnum(r, "numTurns"),
    totalCostUsd: pnum(r, "totalCostUsd"),
    usage: usageFrom(pobj(r, "usage")),
    isError: pbool(r, "isError"),
    context: null,
  };
  const models = modelUsageFrom(pobj(r, "modelUsage"));
  if (models !== undefined) item.modelUsage = models;
  const resultText = pstr(r, "result");
  if (resultText !== "") item.resultText = resultText;
  return item;
}

/**
 * A result's `model_usage` map: model id -> that model's whole-tree slice.
 *
 * Stays undefined for an absent map rather than becoming an empty object,
 * because the two mean different things to the tokens overlay — absent is "the
 * SDK did not itemize this result" (the standing map survives), empty is a
 * result that genuinely itemized nothing.
 */
function modelUsageFrom(m: Obj | undefined): Record<string, ModelUsage> | undefined {
  if (m === undefined) return undefined;
  const out: Record<string, ModelUsage> = {};
  for (const [model, raw] of Object.entries(m)) {
    const u = ensureObj(raw);
    out[model] = {
      input_tokens: pnum(u, "inputTokens"),
      output_tokens: pnum(u, "outputTokens"),
      cache_creation_input_tokens: pnum(u, "cacheCreationInputTokens"),
      cache_read_input_tokens: pnum(u, "cacheReadInputTokens"),
      web_search_requests: pnum(u, "webSearchRequests"),
      cost_usd: pnum(u, "costUsd"),
      context_window: pnum(u, "contextWindow"),
    };
  }
  return out;
}

/**
 * `core.v1.ContextCompactTrigger` by its protojson name. An ABSENT field is
 * the proto3 default (protojson omits it), which is UNSPECIFIED.
 *
 * An unrecognized name is an ERROR rather than a default: silently calling an
 * unknown trigger "auto" would invent a fact the daemon never sent.
 */
const COMPACT_TRIGGERS: Readonly<Record<string, "auto" | "manual" | "unspecified">> = {
  "": "unspecified",
  CONTEXT_COMPACT_TRIGGER_UNSPECIFIED: "unspecified",
  CONTEXT_COMPACT_TRIGGER_MANUAL: "manual",
  CONTEXT_COMPACT_TRIGGER_AUTO: "auto",
};

function compactTrigger(raw: string): "auto" | "manual" | "unspecified" {
  const trigger = COMPACT_TRIGGERS[raw];
  if (trigger === undefined) {
    throw new Error(`state-adapter: unknown ContextCompactTrigger '${raw}'`);
  }
  return trigger;
}

/**
 * The COALESCED compaction. The daemon merges the vendor's separate reports
 * (start status, token boundary, summary line) into this one message, so
 * every field is read from one payload rather than correlated across three —
 * which is exactly what made the summary text unreachable before.
 */
function contextCompactedItem(c: Obj, uuid: string): ContextCompactedItem {
  return {
    kind: "context-compacted",
    uuid,
    trigger: compactTrigger(pstr(c, "trigger")),
    preTokens: pnum(c, "preTokens"),
    postTokens: pnum(c, "postTokens"),
    durationMs: pnum(c, "durationMs"),
    summary: pstr(c, "summary"),
  };
}

/**
 * Adopt a daemon-classified failure as a conversation card.
 *
 * It is an ADOPTION, not a derivation. What it replaces re-decided, on this
 * side of the wire, whether an ApiErrorLine was retrying (by a different test
 * than the daemon's), whether it was fatal (by a third test nothing rendered),
 * and what to call it (a hardcoded "api_error" code and a hardcoded
 * `recoverable: false`, neither fed by anything). Every field below is the
 * daemon's, unexamined.
 *
 * UUID is the item envelope's, carried onto the card so the progress footer's
 * error row can scroll the feed to it.
 */
function systemFailureCard(e: Obj, uuid: string): SystemFailureCard {
  return {
    kind: "failure",
    errorClass: errorClassOf(pstr(e, "errorClass")),
    errorType: pstr(e, "errorType"),
    message: pstr(e, "message"),
    sourceDetail: pstr(e, "sourceDetail"),
    resolvedAtMs: pnum(e, "resolvedAtMs"),
    uuid,
  };
}

/**
 * Adopt an already-DECODED `SystemFailure` (from a `ProgressView` or a
 * `CommandAck`) as the store's card shape.
 *
 * The decoder validated the class, so unlike `systemFailureCard` — which
 * adopts a raw conversation-item payload — this one has nothing left to
 * check. Both exist because a failure reaches this end through two different
 * doors, and neither may re-interpret what the daemon decided.
 */
export function systemFailureFrom(f: SystemFailure): SystemFailureCard {
  return {
    kind: "failure",
    errorClass: f.errorClass,
    errorType: f.errorType,
    message: f.message,
    sourceDetail: f.sourceDetail,
    resolvedAtMs: f.resolvedAtMs,
    uuid: f.itemUuid,
  };
}

/**
 * `frontend.v1.ErrorClass` name → the store's class.
 *
 * An unrecognized class THROWS rather than defaulting. The class decides the
 * card's color, so guessing one would paint a failure the wrong color —
 * quietly, and in a way that contradicts the workspace colored beside it.
 */
function errorClassOf(name: string): ErrorClass {
  const known = ERROR_CLASSES.find((c) => name === c || name === `ERROR_CLASS_${c}`);
  if (known === undefined) {
    throw new Error(`state-adapter: SystemFailureItem has unrecognized error_class '${name}'`);
  }
  return known;
}

/** core.v1.PermissionItem.Resolution (proto enum name) → the store shape. */
function permissionResolution(
  name: string,
  denyMessage: string,
): PermissionItem["resolution"] {
  switch (name) {
    case "RESOLUTION_ALLOWED":
      return { decision: "allow" };
    case "RESOLUTION_DENIED":
      return { decision: "deny", ...(denyMessage !== "" ? { message: denyMessage } : {}) };
    case "RESOLUTION_ABANDONED":
      return { decision: "cancel", ...(denyMessage !== "" ? { message: denyMessage } : {}) };
    default:
      // PENDING / UNSPECIFIED / unknown → no resolution (a live prompt).
      return undefined;
  }
}

function permissionItemFrom(p: Obj, uuid: string): PermissionItem {
  const request = pobj(p, "request") ?? {};
  const item: PermissionItem = {
    kind: "permission",
    // uuid == the permission request_id (frontend.proto §ConversationItem).
    requestId: pstr(request, "requestId") || uuid,
    // GAP: core.v1.PermissionItem carries no tool_use_id (render keys on
    // requestId, so this is inert) and no preview.
    toolUseId: "",
    toolName: pstr(request, "toolName"),
    input: pobj(request, "input") ?? {},
  };
  const resolution = permissionResolution(pstr(p, "resolution"), pstr(p, "denyMessage"));
  if (resolution !== undefined) item.resolution = resolution;
  return item;
}

// --- field readers (loud on wrong type, defaulting on absent) ---------------

function ensureObj(v: unknown): Obj {
  if (typeof v !== "object" || v === null || Array.isArray(v)) {
    throw new Error("state-adapter: expected a JSON object");
  }
  return v as Obj;
}

function pstr(o: Obj, key: string): string {
  const v = o[key];
  if (v === undefined || v === null) return "";
  if (typeof v !== "string") {
    throw new Error(`state-adapter: field \`${key}\` must be a string (got ${typeof v})`);
  }
  return v;
}

function pnum(o: Obj, key: string): number {
  const v = o[key];
  if (v === undefined || v === null) return 0;
  if (typeof v === "number") return v;
  if (typeof v === "string") {
    const n = Number(v);
    if (!Number.isFinite(n)) {
      throw new Error(`state-adapter: field \`${key}\` is not numeric ('${v}')`);
    }
    return n;
  }
  throw new Error(`state-adapter: field \`${key}\` must be a number or numeric string (got ${typeof v})`);
}

function pbool(o: Obj, key: string): boolean {
  const v = o[key];
  if (v === undefined || v === null) return false;
  if (typeof v !== "boolean") {
    throw new Error(`state-adapter: field \`${key}\` must be a boolean (got ${typeof v})`);
  }
  return v;
}

function pobj(o: Obj, key: string): Obj | undefined {
  const v = o[key];
  if (v === undefined || v === null) return undefined;
  if (typeof v !== "object" || Array.isArray(v)) {
    throw new Error(`state-adapter: field \`${key}\` must be an object`);
  }
  return v as Obj;
}

function parr(o: Obj, key: string): unknown[] {
  const v = o[key];
  if (v === undefined || v === null) return [];
  if (!Array.isArray(v)) {
    throw new Error(`state-adapter: field \`${key}\` must be an array`);
  }
  return v;
}

function tsFromMs(ms: number): string {
  return ms > 0 ? new Date(ms).toISOString() : "";
}

/**
 * A `ProgressWindow` as its open detail, or null when the window is closed.
 * Absent and inactive collapse to the same answer on purpose: both mean the
 * footer has nothing to say about that window.
 */
function openWindow(w: ProgressWindow | undefined): ProgressWindowInput | null {
  if (w === undefined || !w.active) return null;
  return { sinceMs: w.sinceMs, detail: w.detail };
}

/**
 * The interrupt window as its open outcome, or null when it is closed (I1).
 *
 * Same discipline as `openWindow`: absent and inactive are one answer. The
 * outcome cannot be absent on an OPEN window — the decoder refuses such a
 * frame — so a live window always has one of the three answers to render.
 */
function openInterrupt(w: InterruptWindow | undefined): InterruptInput | null {
  if (w === undefined || !w.active || w.outcome === null) return null;
  return { sinceMs: w.sinceMs, outcome: w.outcome };
}
