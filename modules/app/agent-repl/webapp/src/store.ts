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
  QueuedItem,
  RenderHint,
  ReplayRequestCmd,
  ResultSubtype,
  Usage,
  WsEnvelope,
  parseFrame,
} from "./protocol.js";
import { countedTurns } from "./turn-clock.js";

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
  /** Owning subagent call; undefined on main-chain blocks. */
  parentToolUseId?: string;
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
  /** Owning subagent call; undefined on main-chain blocks. */
  parentToolUseId?: string;
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
  /** The SDK's raw elapsed clock from the latest heartbeat, in seconds. */
  progressElapsedS?: number;
  /** Background-work completion that named this call as its spawner. */
  notification?: {
    taskId?: string;
    status?: string;
    summary?: string;
    outputFile?: string;
    text: string;
  };
  /** Envelope ts of the result frame: when the call settled. */
  resultTs?: string;
  /** Streamed output of the detached task this call spawned. */
  taskOutput?: string;
  result?: {
    isError: boolean;
    content: string | Array<{ type: "text"; text: string }>;
    render?: RenderHint;
  };
  /**
   * The counted-turn ordinal in effect when this tool's result arrived,
   * stamped once at result time. A subagent's result mutates its
   * spawn-positioned item in place, so the item's array position records
   * when it STARTED, not when it finished — this field is the only record
   * of when the call went terminal, which the topbar's recency window ages
   * against. Absent until the result lands.
   */
  deactivatedAtTurn?: number;
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
  /**
   * How long the turn ran measured from the PREVIOUS final response of the
   * session (a `success` result), or from this turn's own start for the
   * session's first one. This is what the final-response bubble's chip
   * shows, as against `durationMs` (the SDK's whole-task figure) which the
   * standalone chip still shows.
   *
   * Derived from the daemon stamps in the store (`ts` and the anchor), not
   * from any tab's own clock, so a tab that reconnects mid-session
   * re-derives the same figure from the replayed results.
   */
  sincePrevFinalMs: number;
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
  /**
   * The in-flight message queue (§2.13): messages submitted while a turn
   * was running, parked by the daemon rather than run immediately. Kept
   * SEPARATE from `items` — a queued message is not a conversation item;
   * it becomes a real `user-turn` only once the daemon drains it. Nothing
   * here ever touches `turnInFlight`, which only real `user-turn`/`result`
   * frames move.
   */
  queued: QueuedItem[];
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
  /**
   * The daemon stamp of the session's most recent FINAL RESPONSE (a
   * `success` result's envelope ts), or `null` before the first one. The
   * anchor the final-response chip measures its elapsed time from: a chip
   * reads "since the previous final response", and the session's first one
   * falls back to its own turn start (`turnStartedAt`).
   *
   * Advanced ONLY at a `success` result, so a queued follow-up prompt or an
   * aborted turn between two answers never moves it. The daemon's stamp
   * rather than this tab's clock, like `turnStartedAt`, so a reconnecting
   * tab re-derives the same anchor from the replayed results.
   */
  lastFinalResponseAt: string | null;
  /**
   * The session's context size as last reported by an API request, which
   * a `/clear` (`system: init`) and a compaction both invalidate: neither
   * reports the context it leaves behind, so the figure reverts to `null`
   * (unknown) until the next request declares the new one.
   *
   * This is the ONLY token figure the store keeps, and deliberately so.
   * A `result`'s usage is the turn's CUMULATIVE spend across every API
   * request it made, which re-counts the cached prefix once per request
   * and so runs past the context window without bound. It is not a
   * context size and must never be mistaken for one.
   */
  contextTokens: number | null;
  /**
   * Whether a context compaction is IN PROGRESS: opened by a `compact-status`
   * frame (the SDK's `status: "compacting"`) and closed by the `compact-boundary`
   * that ends it. A compaction always runs inside a turn, so this is also
   * cleared wherever `turnInFlight` clears (a terminating result or a
   * non-recoverable error), guaranteeing the indicator never sticks. The SDK
   * reports no progress percentage, so this is a plain boolean, not a fraction.
   */
  compacting: boolean;
  /**
   * Whether the running turn is being INTERRUPTED: opened by an `interrupt`
   * frame (only while a turn is in flight) and closed by the turn's
   * terminating `result`, a non-recoverable `error`, or the next `user-turn`.
   * While set, the feed shows the red "interrupting…" indicator and the store
   * drops content-start frames that would open a NEW bubble — the interrupt is
   * cooperative, so already-dispatched tools and subagents keep streaming, and
   * their tail must land WITHIN existing bubbles rather than sprouting fresh
   * ones. Updates to existing items and tools continuing the current run still
   * apply, so an in-flight bubble finishes streaming.
   */
  interrupting: boolean;
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
    queued: [],
    turnInFlight: false,
    turnStartedAt: null,
    lastFinalResponseAt: null,
    contextTokens: null,
    compacting: false,
    interrupting: false,
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
   * Set (true) on the frame that completes a FRESH-JOIN history replay:
   * the feed should now render restored (tail-first backfill) instead
   * of incrementally. The frame completing a reconnect's gap-fill
   * replay reports plain `changed` instead — the feed is already built,
   * and one reconcile render shows the whole backlog at once.
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
    this.helloTurnActive = false;
  }

  /**
   * Replay watermark: the hello's seq when a replay was requested, null
   * outside a replay. While set, frames are applied silently (no
   * per-frame feed render) and the feed renders once when lastSeq
   * catches up — rendering per replayed frame is what a switched-back-to
   * feed's catch-up jitter is made of.
   */
  private replayTarget: number | null = null;

  /**
   * What kind of replay the watermark guards: a fresh join's completion
   * renders restored (full tail-first rebuild), a reconnect gap-fill's
   * completion renders as one ordinary reconcile — the feed is already
   * built, and a rebuild would drop expanded sections and yank a
   * scrolled-up reader to the tail.
   */
  private replayKind: "fresh" | "gap" = "fresh";

  /**
   * The daemon's authoritative turn-active bit from the last hello. A
   * transcript-seeded fresh-join replay (§2.11) synthesizes a `result` for
   * answered turns, but a trailing prompt the agent never answered gets
   * none, so its dangling `user-turn` sets `turnInFlight`/`turnStartedAt`
   * with nothing to clear them; this is what the completed replay reconciles
   * against so a cold/rehydrated session does not paint a phantom running
   * turn. `false` until a hello reports otherwise.
   */
  private helloTurnActive = false;

  /** Whether a history replay (fresh-join or gap-fill) is still streaming in. */
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
    const caughtUp =
      this.replayTarget !== null && envelope.seq >= this.replayTarget;
    if (caughtUp) this.replayTarget = null;
    // Only a fresh join's completion renders restored; a gap-fill's is a
    // plain `changed` (see `replayKind`). An unknown last frame still leaves
    // a backlog to show, so it advances the cursor without applying.
    const restored = caughtUp && this.replayKind === "fresh";
    if (frame !== null) this.applyKnown(frame);
    // A fresh-join replay is seeded from the transcript (§2.11), which
    // synthesizes a `result` for answered turns but leaves a trailing
    // unanswered prompt's `user-turn` dangling. That dangling turn leaves
    // `turnInFlight`/`turnStartedAt` set with nothing to clear them, so a
    // cold/rehydrated session would paint the topbar timer counting from a
    // days-old prompt. The daemon's hello turn_active is authoritative: when
    // it says no turn runs, drop the phantom one. A genuinely mid-turn fresh
    // join (a live session's second tab) reports turn_active true and keeps
    // its real turn start.
    if (restored && !this.helloTurnActive) {
      this.state.turnInFlight = false;
      this.state.turnStartedAt = null;
    }
    if (!caughtUp) return { changed: frame !== null };
    return { changed: true, restored };
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
    // Remembered for the fresh-join replay's completion to reconcile against:
    // the replay's frames alone cannot say whether a trailing unanswered
    // prompt's turn is still running (that turn gets no synthetic `result`),
    // so the daemon's word is what settles it.
    this.helloTurnActive = hello.turn_active ?? false;

    const canFillFromHistory =
      s.lastSeq > 0 && hello.resume_from_seq <= s.lastSeq + 1;
    if (canFillFromHistory) {
      // Reconnect with our history still inside the retention window:
      // fetch only what we missed (if anything). The backlog applies
      // silently behind the gap-fill watermark and the feed renders once
      // when it lands — a webview whose socket died while its workspace
      // was hidden otherwise replays a long backlog as a burst of
      // per-frame renders, which is the switch-in jitter.
      if (hello.seq > s.lastSeq) {
        this.replayTarget = hello.seq;
        this.replayKind = "gap";
        return {
          changed: true,
          send: { type: "replay-request", from_seq: s.lastSeq + 1 },
        };
      }
      return { changed: true };
    }
    // Fresh join, or §2.10 eviction rebuild: discard local state and
    // request everything the daemon still retains. The queue is seeded
    // from the hello snapshot (§2.13) so a replay-evicted client whose
    // queue-added frame fell out of the retention window still shows its
    // parked messages; the queue-added reducer dedupes by queue_id, so a
    // still-retained queue-added re-sent by the replay does not double it.
    s.items = [];
    s.queued = (hello.queue ?? []).slice();
    s.turnInFlight = false;
    s.turnStartedAt = null;
    s.lastFinalResponseAt = null;
    s.contextTokens = null;
    s.interrupting = false;
    s.costUsd = null;
    s.lastSeq = Math.max(0, hello.resume_from_seq - 1);
    if (hello.resume_from_seq > 0 && hello.seq >= hello.resume_from_seq) {
      // Full-history replay incoming: render nothing per-frame and
      // restore (tail-first) once lastSeq reaches the hello watermark.
      this.replayTarget = hello.seq;
      this.replayKind = "fresh";
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
        // A fresh turn is starting: any interrupt of the PREVIOUS turn is over
        // (a queue-run-now preemption drains its promoted message here).
        s.interrupting = false;
        break;
      case "text-start":
        if (this.opensNewBubbleWhileInterrupting(frame.parent_tool_use_id, undefined)) break;
        s.items.push({
          kind: "text",
          blockId: frame.block_id,
          messageId: frame.message_id,
          parentToolUseId: frame.parent_tool_use_id,
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
        if (this.opensNewBubbleWhileInterrupting(frame.parent_tool_use_id, undefined)) break;
        s.items.push({
          kind: "thinking",
          blockId: frame.block_id,
          messageId: frame.message_id,
          parentToolUseId: frame.parent_tool_use_id,
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
        if (this.opensNewBubbleWhileInterrupting(frame.parent_tool_use_id, frame.tool_name)) break;
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
          item.resultTs = frame.ts;
          // Stamp when this call went terminal, in counted turns. The
          // item's array position dates its START; a result arriving turns
          // later is the only place that later moment is recorded.
          item.deactivatedAtTurn = countedTurns(s.items);
        }
        break;
      }
      case "tool-use-progress": {
        const item = this.findTool(frame.tool_use_id);
        if (item) {
          item.progress = frame.text;
          item.progressElapsedS = frame.elapsed_seconds;
        }
        break;
      }
      case "task-output-delta": {
        const item = frame.tool_use_id ? this.findTool(frame.tool_use_id) : undefined;
        if (item) item.taskOutput = (item.taskOutput ?? "") + frame.text;
        break;
      }
      case "task-notification": {
        // The completion lands on the SPAWNING card when the payload
        // names one the feed knows; anything else surfaces as a system
        // note rather than vanishing — a background completion is never
        // swallowed.
        const item = frame.tool_use_id ? this.findTool(frame.tool_use_id) : undefined;
        if (item) {
          item.notification = {
            taskId: frame.task_id,
            status: frame.status,
            summary: frame.summary,
            outputFile: frame.output_file,
            text: frame.text,
          };
        } else {
          s.items.push({ kind: "system", subtype: "task-notification" });
        }
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
      case "result": {
        // Measure from the previous final response, or from this turn's own
        // start for the session's first one. An unknown anchor (a result
        // whose turn's start never arrived, e.g. a truncated replay) falls
        // back to the SDK's own whole-task figure.
        const anchor = s.lastFinalResponseAt ?? s.turnStartedAt;
        const sincePrevFinalMs =
          anchor === null
            ? frame.duration_ms
            : Date.parse(frame.ts) - Date.parse(anchor);
        s.items.push({
          kind: "result",
          subtype: frame.subtype,
          durationMs: frame.duration_ms,
          sincePrevFinalMs,
          numTurns: frame.num_turns,
          totalCostUsd: frame.total_cost_usd,
          usage: frame.usage,
          isError: frame.is_error,
          resultText: frame.result_text,
          context: this.resultContext(),
        });
        // Only a successful turn is a final response, so only it becomes the
        // next chip's anchor — an aborted or errored turn produced no answer.
        if (frame.subtype === "success") s.lastFinalResponseAt = frame.ts;
        s.turnInFlight = false;
        s.turnStartedAt = null;
        // The turn has ended, so an interrupt of it is over: this `result` is
        // the `aborted` one the interrupt was waiting on (or the turn finished
        // before the interrupt landed). Either way the indicator drops here.
        s.interrupting = false;
        // A compaction always finishes within its turn; clear the indicator
        // here too so a compaction that ended without a boundary (a failure)
        // never leaves it stuck on.
        s.compacting = false;
        // `contextTokens` is deliberately NOT moved here: this frame's usage
        // is the turn's cumulative spend, not the context it left behind.
        // The standing figure is the one the turn's last request declared.
        s.costUsd = frame.total_cost_usd;
        break;
      }
      case "interrupt":
        // Enter the interrupting state only while a turn is actually running:
        // an idle interrupt aborts nothing, and no terminating result would
        // arrive to clear a stuck indicator. The daemon already gates the
        // frame on turn-active, but the store double-checks against its own
        // authoritative turnInFlight so a replayed interrupt never sticks.
        if (s.turnInFlight) s.interrupting = true;
        break;
      case "compact-status":
        // The compaction window: opened here, closed by the compact-boundary
        // (or, defensively, by the turn's terminating result/error below).
        s.compacting = frame.active;
        break;
      case "compact-boundary":
        s.items.push({
          kind: "compact-boundary",
          trigger: frame.trigger,
          preTokens: frame.pre_tokens,
          postTokens: frame.post_tokens,
        });
        // The compaction is done: close the in-progress window.
        s.compacting = false;
        // A compaction rewrites the conversation, so the standing figure is
        // stale the moment the boundary lands. The SDK now reports the
        // post-compaction size in `post_tokens`, so re-anchor on it when
        // present (> 0), and only revert to unknown (a dash) when the SDK
        // omits it — never a fabricated estimate.
        s.contextTokens = frame.post_tokens > 0 ? frame.post_tokens : null;
        break;
      case "usage":
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
          // A fatal error ends the turn, and with it any compaction the turn
          // was running: drop the in-progress indicator.
          s.compacting = false;
          // A fatal error also ends any interrupt in progress — no aborted
          // result will follow to clear it.
          s.interrupting = false;
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
      case "queue-added":
        // §2.13: a message parked because a turn was in flight. This never
        // touches turnInFlight — the item is NOT running. Deduped by
        // queue_id so a replay re-sending the frame (or a hello snapshot
        // seed racing the retained frame) never double-adds.
        if (!s.queued.some((q) => q.queue_id === frame.queue_id)) {
          s.queued.push({
            queue_id: frame.queue_id,
            request_id: frame.request_id,
            content: frame.content,
            status: frame.status,
          });
        }
        break;
      case "queue-classified": {
        // The classifier's verdict lands on the matching item: `wait` keeps
        // it parked ("waiting"), `interrupt` escalates it ("interrupt"). A
        // verdict whose item already drained/cancelled is discarded.
        const item = s.queued.find((q) => q.queue_id === frame.queue_id);
        if (item) {
          item.verdict = frame.verdict;
          item.reason = frame.reason;
          item.status = frame.verdict === "wait" ? "waiting" : "interrupt";
        }
        break;
      }
      case "queue-removed":
        // The item left the queue (drained into a user-turn, cancelled,
        // dropped by a user interrupt, or dropped on session end). The
        // user-turn it drained into arrives as its own frame; here we only
        // drop the parked entry.
        s.queued = s.queued.filter((q) => q.queue_id !== frame.queue_id);
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

  // --- interrupt gate ----------------------------------------------------------

  /**
   * While interrupting, whether a content-start frame would open a NEW
   * top-level bubble — the frames the store drops so the aborting turn's tail
   * lands within existing bubbles instead of sprouting fresh ones. Two shapes
   * still pass (return false):
   * - a block nested inside a card the feed already holds (it streams within
   *   that card's activity panel, i.e. within a bubble);
   * - a tool that continues the current top-level tool run (it joins the same
   *   tabbed group, i.e. the same bubble).
   * Returns false whenever not interrupting, so the normal path is untouched.
   * `toolName` is set only for a `tool-use-start`.
   */
  private opensNewBubbleWhileInterrupting(
    parentToolUseId: string | undefined,
    toolName: string | undefined,
  ): boolean {
    if (!this.state.interrupting) return false;
    if (parentToolUseId !== undefined && this.findTool(parentToolUseId)) return false;
    if (toolName !== undefined && this.extendsCurrentToolRun(toolName)) return false;
    return true;
  }

  /**
   * Whether a new MAIN-CHAIN tool of TOOLNAME would join the current top-level
   * tool run (nesting into the existing tabbed bubble) rather than opening a
   * new one — true when the last main-chain item is a tool of the same name.
   * Nested items (those bound to a parent card) are skipped, since grouping is
   * a top-level concern; this mirrors the render's consecutive-run grouping
   * closely enough for the brief interrupt window.
   */
  private extendsCurrentToolRun(toolName: string): boolean {
    const items = this.state.items;
    for (let i = items.length - 1; i >= 0; i--) {
      const item = items[i];
      if (
        (item.kind === "text" || item.kind === "thinking" || item.kind === "tool") &&
        item.parentToolUseId !== undefined
      ) {
        continue;
      }
      return item.kind === "tool" && item.toolName === toolName;
    }
    return false;
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
