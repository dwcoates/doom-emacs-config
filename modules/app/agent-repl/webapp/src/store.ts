/**
 * ConversationStore — applies Layer-2 frames (in seq order) to a
 * renderable conversation state, detecting seq gaps and driving the
 * §2.10 replay handshake.
 *
 * The store is pure with respect to I/O: `apply()` returns the command
 * the transport should send (if any) instead of sending it itself.
 */
import {
  ContentBlock,
  HelloFrame,
  L2Frame,
  ModelInfo,
  PermissionMode,
  PermissionPreview,
  RenderHint,
  ReplayRequestCmd,
  ResultSubtype,
  Usage,
  WsEnvelope,
  parseFrame,
} from "./protocol.js";

// --- conversation items -------------------------------------------------------

export interface UserTurnItem {
  kind: "user-turn";
  requestId: string;
  content: ContentBlock[];
  /** Envelope ts (§2.1): when the prompt was sent, rendered on the bubble. */
  ts: string;
}
export interface TextItem {
  kind: "text";
  blockId: string;
  messageId: string;
  text: string;
  done: boolean;
  /**
   * Envelope ts (§2.1) of the `text-start` frame: when the agent OPENED
   * the block, rendered on the bubble. Taken at the start rather than the
   * end so the stamp holds still while the block streams.
   */
  ts: string;
}
export interface ThinkingItem {
  kind: "thinking";
  blockId: string;
  messageId: string;
  text: string;
  done: boolean;
  signature?: string;
}
export interface ToolItem {
  kind: "tool";
  toolUseId: string;
  toolName: string;
  messageId: string;
  parentToolUseId?: string;
  inputJson: string;
  input?: Record<string, unknown>;
  inputDone: boolean;
  progress?: string;
  result?: {
    isError: boolean;
    content: string | Array<{ type: "text"; text: string }>;
    render?: RenderHint;
  };
}
export interface PermissionItem {
  kind: "permission";
  requestId: string;
  toolUseId: string;
  toolName: string;
  input: unknown;
  preview?: PermissionPreview;
  resolution?: { decision: "allow" | "deny" | "cancel"; message?: string };
}
/**
 * The session's input-token standing as of a result: how many tokens the
 * conversation carries into an API request, and how much that grew over
 * the previous result. The growth is negative when the turn shed context
 * rather than added it.
 */
export interface ResultContext {
  total: number;
  delta: number;
}
export interface ResultItem {
  kind: "result";
  subtype: ResultSubtype;
  durationMs: number;
  numTurns: number;
  totalCostUsd: number;
  usage: Usage;
  isError: boolean;
  resultText?: string;
  /**
   * `null` when the turn ended with the session's context size unknown: a
   * `/clear` re-inits the session and a compaction rewrites it, and
   * neither reports the size it left behind, so the figure is unknown
   * until the next API request declares it.
   */
  context: ResultContext | null;
}
export interface CompactBoundaryItem {
  kind: "compact-boundary";
  trigger: "auto" | "manual";
  preTokens: number;
  postTokens: number;
}
export interface ErrorItem {
  kind: "error";
  code: string;
  message: string;
  recoverable: boolean;
}
export interface RetryItem {
  kind: "retry";
  attempt: number;
  reason: string;
  fatal: boolean;
}
export interface SystemItem {
  kind: "system";
  subtype: string;
}

/**
 * The tokens an API request carries in its input: the fresh tokens, plus
 * the cached prefix it read, plus the prefix it wrote to cache. Their sum
 * IS the conversation's context size at that request, so the last one a
 * session reported is what the session currently costs to think with.
 */
export function contextTokens(usage: Usage | null): number {
  if (!usage) return 0;
  return (
    usage.input_tokens +
    (usage.cache_read_input_tokens ?? 0) +
    (usage.cache_creation_input_tokens ?? 0)
  );
}

export type ConversationItem =
  | UserTurnItem
  | TextItem
  | ThinkingItem
  | ToolItem
  | PermissionItem
  | ResultItem
  | CompactBoundaryItem
  | ErrorItem
  | RetryItem
  | SystemItem;

// --- store state -----------------------------------------------------------------

export interface StoreState {
  sessionId: string;
  daemonVersion: string;
  /**
   * The session's live model. Moved by the hello AND by every
   * `model-changed` frame — the CLI owns this value and can move it
   * without being asked, so the store follows rather than remembers.
   */
  model: string;
  /** The `set-model` menu; empty until the daemon reports it. */
  models: ModelInfo[];
  cwd: string;
  /**
   * Durable CLI session uuid from the hello frame (empty until the
   * SDK's system:init). This is the resume target the rebind path
   * persists client-side — the key that survives a lost daemon
   * session id.
   */
  claudeSessionId: string;
  permissionMode: PermissionMode;
  items: ConversationItem[];
  turnInFlight: boolean;
  /**
   * When the in-flight turn started, as the daemon stamped its `user-turn`
   * frame (§2.1 envelope ts), and `null` whenever no turn is running. Set
   * and cleared in lockstep with `turnInFlight`.
   *
   * The daemon's stamp rather than this tab's own clock: the stamp rides
   * the retained frame through a replay, so a tab that reconnects mid-turn
   * resumes the count where the turn actually is.
   */
  turnStartedAt: string | null;
  usage: Usage | null;
  /**
   * The session's context size as last reported by an API request, which
   * a `/clear` (`system: init`) and a compaction both invalidate: neither
   * reports the context it leaves behind, so the figure reverts to `null`
   * (unknown) until the next request declares the new one.
   */
  contextTokens: number | null;
  costUsd: number | null;
  lastSeq: number;
}

function initialState(): StoreState {
  return {
    sessionId: "",
    daemonVersion: "",
    model: "",
    models: [],
    cwd: "",
    claudeSessionId: "",
    permissionMode: "default",
    items: [],
    turnInFlight: false,
    turnStartedAt: null,
    usage: null,
    contextTokens: null,
    costUsd: null,
    lastSeq: 0,
  };
}

/** Result of applying one raw WebSocket message. */
export interface ApplyResult {
  /** Command the transport must send (replay-request), if any. */
  send?: ReplayRequestCmd;
  /** Whether visible state changed (render needed). */
  changed: boolean;
  /**
   * Set on the frame that completes a fresh-join history replay: the
   * feed should now render restored (tail-first backfill) instead of
   * incrementally.
   */
  restored?: boolean;
}

export class ConversationStore {
  state: StoreState = initialState();

  /**
   * Discard all state, as if freshly constructed. Used when the live
   * view is rebound onto a DIFFERENT daemon session (the "session
   * gone" rebind): the successor's hello must be treated as a fresh
   * join, not a reconnect — a stale lastSeq would otherwise make its
   * replay look like a small gap-fill and splice two conversations.
   */
  reset(): void {
    this.state = initialState();
    this.replayTarget = null;
  }

  /**
   * Fresh-join replay watermark: the hello's seq when a full history
   * replay was requested, null outside a replay. While set, frames are
   * applied silently (no per-frame feed render) and the feed renders
   * once, restored, when lastSeq catches up.
   */
  private replayTarget: number | null = null;

  /** Whether a fresh-join history replay is still streaming in. */
  get replaying(): boolean {
    return this.replayTarget !== null;
  }

  /** Apply one raw WebSocket text message. */
  applyRaw(data: string): ApplyResult {
    const { envelope, frame } = parseFrame(data);
    return this.apply(envelope, frame);
  }

  /**
   * Apply one parsed frame.
   *
   * Ordering rules:
   * - `hello` is connection-scoped and handled outside the seq stream.
   * - frames with seq <= lastSeq are duplicates (replay overlap): skip.
   * - a frame with seq > lastSeq+1 signals dropped frames: skip it and
   *   request a replay from the first missing seq — the daemon re-sends
   *   the skipped frame too.
   * - unknown frame types still advance the cursor (their seq is real).
   */
  apply(envelope: WsEnvelope, frame: L2Frame | null): ApplyResult {
    if (envelope.type === "hello") {
      return this.applyHello(frame as HelloFrame);
    }
    if (envelope.seq <= this.state.lastSeq) {
      return { changed: false };
    }
    if (envelope.seq > this.state.lastSeq + 1) {
      return {
        changed: false,
        send: { type: "replay-request", from_seq: this.state.lastSeq + 1 },
      };
    }
    this.state.lastSeq = envelope.seq;
    const restored =
      this.replayTarget !== null && envelope.seq >= this.replayTarget;
    if (restored) this.replayTarget = null;
    if (frame === null) {
      // unknown type: cursor advanced, nothing to render — but a replay
      // that ends on an unknown frame still has a backlog to show.
      return restored ? { changed: true, restored } : { changed: false };
    }
    this.applyKnown(frame);
    return restored ? { changed: true, restored } : { changed: true };
  }

  private applyHello(hello: HelloFrame): ApplyResult {
    const s = this.state;
    s.sessionId = hello.session_id;
    s.daemonVersion = hello.daemon_version;
    s.model = hello.model;
    // The hello republishes the menu, so a reconnect never empties a
    // populated picker.
    if (hello.models !== undefined) s.models = hello.models;
    s.cwd = hello.cwd;
    s.claudeSessionId = hello.claude_session_id ?? "";
    s.permissionMode = hello.permission_mode;

    const canFillFromHistory =
      s.lastSeq > 0 && hello.resume_from_seq <= s.lastSeq + 1;
    if (canFillFromHistory) {
      // Reconnect with our history still inside the retention window:
      // fetch only what we missed (if anything).
      if (hello.seq > s.lastSeq) {
        return {
          changed: true,
          send: { type: "replay-request", from_seq: s.lastSeq + 1 },
        };
      }
      return { changed: true };
    }
    // Fresh join, or §2.10 eviction rebuild: discard local state and
    // request everything the daemon still retains.
    s.items = [];
    s.turnInFlight = false;
    s.turnStartedAt = null;
    s.usage = null;
    s.contextTokens = null;
    s.costUsd = null;
    s.lastSeq = Math.max(0, hello.resume_from_seq - 1);
    if (hello.resume_from_seq > 0 && hello.seq >= hello.resume_from_seq) {
      // Full-history replay incoming: render nothing per-frame and
      // restore (tail-first) once lastSeq reaches the hello watermark.
      this.replayTarget = hello.seq;
      return {
        changed: true,
        send: { type: "replay-request", from_seq: hello.resume_from_seq },
      };
    }
    this.replayTarget = null;
    return { changed: true };
  }

  // --- per-frame application ---------------------------------------------------

  private applyKnown(frame: L2Frame): void {
    const s = this.state;
    switch (frame.type) {
      case "hello":
        break; // handled in apply()
      case "user-turn":
        s.items.push({
          kind: "user-turn",
          requestId: frame.request_id,
          content: frame.content,
          ts: frame.ts,
        });
        s.turnInFlight = true;
        s.turnStartedAt = frame.ts;
        break;
      case "text-start":
        s.items.push({
          kind: "text",
          blockId: frame.block_id,
          messageId: frame.message_id,
          text: "",
          done: false,
          ts: frame.ts,
        });
        break;
      case "text-delta": {
        const item = this.findText(frame.block_id);
        if (item) item.text += frame.text;
        break;
      }
      case "text-end": {
        const item = this.findText(frame.block_id);
        if (item) {
          item.text = frame.final_text;
          item.done = true;
        }
        break;
      }
      case "thinking-start":
        s.items.push({
          kind: "thinking",
          blockId: frame.block_id,
          messageId: frame.message_id,
          text: "",
          done: false,
        });
        break;
      case "thinking-delta": {
        const item = this.findThinking(frame.block_id);
        if (item) item.text += frame.text;
        break;
      }
      case "thinking-end": {
        const item = this.findThinking(frame.block_id);
        if (item) {
          item.text = frame.final_text;
          item.done = true;
          item.signature = frame.signature;
        }
        break;
      }
      case "tool-use-start":
        s.items.push({
          kind: "tool",
          toolUseId: frame.tool_use_id,
          toolName: frame.tool_name,
          messageId: frame.message_id,
          parentToolUseId: frame.parent_tool_use_id,
          inputJson: "",
          inputDone: false,
        });
        break;
      case "tool-use-input-delta": {
        const item = this.findTool(frame.tool_use_id);
        if (item) item.inputJson += frame.partial_json;
        break;
      }
      case "tool-use-input-end": {
        const item = this.findTool(frame.tool_use_id);
        if (item) {
          item.input = frame.input;
          item.inputJson = JSON.stringify(frame.input, null, 2);
          item.inputDone = true;
        }
        break;
      }
      case "tool-use-result": {
        const item = this.findTool(frame.tool_use_id);
        if (item) {
          item.result = {
            isError: frame.is_error,
            content: frame.content,
            render: frame.render,
          };
        }
        break;
      }
      case "tool-use-progress": {
        const item = this.findTool(frame.tool_use_id);
        if (item) item.progress = frame.text;
        break;
      }
      case "permission-request":
        s.items.push({
          kind: "permission",
          requestId: frame.request_id,
          toolUseId: frame.tool_use_id,
          toolName: frame.tool_name,
          input: frame.input,
          preview: frame.preview,
        });
        break;
      case "permission-resolved": {
        const item = this.findPermission(frame.request_id);
        if (item) {
          item.resolution = { decision: frame.decision, message: frame.message };
        }
        break;
      }
      case "result":
        s.items.push({
          kind: "result",
          subtype: frame.subtype,
          durationMs: frame.duration_ms,
          numTurns: frame.num_turns,
          totalCostUsd: frame.total_cost_usd,
          usage: frame.usage,
          isError: frame.is_error,
          resultText: frame.result_text,
          context: this.resultContext(),
        });
        s.turnInFlight = false;
        s.turnStartedAt = null;
        s.usage = frame.usage;
        s.costUsd = frame.total_cost_usd;
        break;
      case "compact-boundary":
        s.items.push({
          kind: "compact-boundary",
          trigger: frame.trigger,
          preTokens: frame.pre_tokens,
          postTokens: frame.post_tokens,
        });
        // A compaction rewrites the conversation and the SDK reports no
        // post-compaction size, so the standing figure is stale the moment
        // the boundary lands.
        s.contextTokens = null;
        break;
      case "usage":
        s.usage = frame.usage;
        s.contextTokens = contextTokens(frame.usage);
        if (frame.cost_usd !== undefined) s.costUsd = frame.cost_usd;
        break;
      case "permission-mode-changed":
        s.permissionMode = frame.mode;
        break;
      case "models":
        s.models = frame.models;
        break;
      case "model-changed":
        // Every origin lands here: a switch the user picked, one the agent
        // made itself, and one the daemon's transcript reconcile caught.
        s.model = frame.model;
        break;
      case "error":
        s.items.push({
          kind: "error",
          code: frame.code,
          message: frame.message,
          recoverable: frame.recoverable,
        });
        if (!frame.recoverable) {
          s.turnInFlight = false;
          s.turnStartedAt = null;
        }
        break;
      case "retry":
        s.items.push({
          kind: "retry",
          attempt: frame.attempt,
          reason: frame.reason,
          fatal: frame.fatal,
        });
        break;
      case "system":
        s.items.push({ kind: "system", subtype: frame.subtype });
        // A `/clear` re-inits the session, dropping the context it had
        // accumulated. The init announces no size, so the figure is
        // unknown until the next request reports the fresh one.
        if (frame.subtype === "init") s.contextTokens = null;
        break;
    }
  }

  /**
   * The context standing to stamp on the result now closing: the session's
   * last-reported size, and its growth over the previous result's. A result
   * whose own size is unknown carries no standing at all, and one following
   * such a result measures its growth from zero, since a `/clear` really does
   * zero the context and a compaction leaves only a fraction of it behind.
   */
  private resultContext(): ResultContext | null {
    const total = this.state.contextTokens;
    if (total === null) return null;
    const prior = this.findLast((i): i is ResultItem => i.kind === "result");
    return { total, delta: total - (prior?.context?.total ?? 0) };
  }

  // --- lookups (last matching item wins: block/tool ids are unique, but
  // scanning from the tail is O(active-turn) rather than O(history)) ------------

  private findText(blockId: string): TextItem | undefined {
    return this.findLast(
      (i): i is TextItem => i.kind === "text" && i.blockId === blockId,
    );
  }

  private findThinking(blockId: string): ThinkingItem | undefined {
    return this.findLast(
      (i): i is ThinkingItem => i.kind === "thinking" && i.blockId === blockId,
    );
  }

  private findTool(toolUseId: string): ToolItem | undefined {
    return this.findLast(
      (i): i is ToolItem => i.kind === "tool" && i.toolUseId === toolUseId,
    );
  }

  private findPermission(requestId: string): PermissionItem | undefined {
    return this.findLast(
      (i): i is PermissionItem => i.kind === "permission" && i.requestId === requestId,
    );
  }

  private findLast<T extends ConversationItem>(
    pred: (item: ConversationItem) => item is T,
  ): T | undefined {
    const items = this.state.items;
    for (let i = items.length - 1; i >= 0; i--) {
      const item = items[i];
      if (pred(item)) return item;
    }
    return undefined;
  }
}
