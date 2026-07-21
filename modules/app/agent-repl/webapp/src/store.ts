/**
 * ConversationStore — applies Layer-2 frames (in seq order) to a
 * renderable conversation state, detecting seq gaps and driving the
 * §2.10 replay handshake.
 *
 * The store is pure with respect to I/O: `apply()` returns the command
 * the transport should send (if any) instead of sending it itself.
 */
import {
  AssistantMessageError,
  AsyncSource,
  ContentBlock,
  HelloFrame,
  L2Frame,
  ModelInfo,
  ModelUsage,
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
// A runtime cycle (partition imports store's types and helpers), safe in
// ESM because every cross-use is call-time, never module-init.
import { spawnedTaskIds } from "./partition.js";
import { isClearTurn } from "./turn.js";

// --- conversation items -------------------------------------------------------

export interface UserTurnItem {
  kind: "user-turn";
  requestId: string;
  content: ContentBlock[];
  /** Envelope ts (§2.1): when the prompt was sent, rendered on the bubble. */
  ts: string;
  /**
   * When set, the prompt was injected on the user's behalf and renders as a
   * status card rather than a user-prompt bubble (see the `user-turn` render
   * dispatch). "merge" is the merge-failure remediation turn. Absent for a
   * user's own prompt.
   */
  origin?: string;
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
   * Set when the owning assistant message was an API-level failure (a
   * session/usage limit, a billing or auth error): the bubble is a failure
   * notice, not an answer, so it renders red instead of the green
   * final-response border. Filled by the `assistant-error` frame, which
   * arrives after the block, keyed by message id. Undefined on a normal
   * block.
   */
  error?: AssistantMessageError;
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
  /**
   * Envelope ts of the `tool-use-start` frame: when the call opened. The
   * agent-scoped topbar's elapsed clock counts from here (to `resultTs`
   * once the call settles, to now while it runs).
   */
  ts: string;
  /**
   * When this call is a subagent's, the SUBAGENT conversation's own
   * context size as its last attributed `usage` frame declared it (the
   * same input+cache sum `contextTokens` computes for the session).
   * Absent until the agent's first request reports one.
   */
  contextTokens?: number;
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
  /**
   * The detached work this call spawned (§2.6), as the daemon classified
   * it from the SDK's structured tool result. Present only on a call that
   * actually owns a stream, which is what the async fold keys off — a card
   * without one renders no fold at all.
   */
  asyncSource?: AsyncSource;
  /** Streamed output of the detached task this call spawned. */
  taskOutput?: string;
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
  /**
   * How long the turn ran measured from the PREVIOUS turn boundary of the
   * session (a `success` result, or an interrupt's `aborted` one), or from
   * this turn's own start for the session's first one. This is what the
   * final-response bubble's chip shows, as against `durationMs` (the SDK's
   * whole-task figure) which the standalone chip still shows.
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
 * The tokens a request leaves occupying the model's context: the fresh
 * input tokens, plus the cached prefix it read, plus the prefix it wrote
 * to cache, PLUS the output it produced. Their sum IS the conversation's
 * full standing size at that request — everything the model had to think
 * with, plus the answer it just appended — so the last one a session
 * reported is precisely how much of the context window the session now
 * occupies, and the model max minus it is what remains.
 *
 * The output side is what makes this a LIVE occupancy figure rather than a
 * turn-lagged one: excluding it would undercount by the current response
 * until the next request folded that response into its input.
 */
export function contextTokens(usage: Usage | null): number {
  if (!usage) return 0;
  return (
    usage.input_tokens +
    (usage.cache_read_input_tokens ?? 0) +
    (usage.cache_creation_input_tokens ?? 0) +
    usage.output_tokens
  );
}

/**
 * Whether a usage payload carries no tokens at all. `contextTokens` now
 * sums every dimension, output included, so a zero sum already means all
 * four fields are zero. A real turn always spends tokens, so an all-zero
 * usage identifies the one producer that reports none: the synthetic
 * result a transcript-seeded replay closes each turn with (§2.11) — the
 * transcript records no usage, and adopting its zeros would wipe the
 * session tallies on every rehydrate.
 */
function isZeroUsage(usage: Usage): boolean {
  return contextTokens(usage) === 0;
}

/**
 * Field-wise max of two usage payloads for the SAME message: every
 * field is cumulative within a message (`message_start` carries the
 * input side, each `message_delta` a growing output count), so the max
 * is the message's most complete figure and never double-counts.
 */
function maxUsage(a: Usage, b: Usage): Usage {
  return {
    input_tokens: Math.max(a.input_tokens, b.input_tokens),
    output_tokens: Math.max(a.output_tokens, b.output_tokens),
    cache_creation_input_tokens: Math.max(
      a.cache_creation_input_tokens ?? 0,
      b.cache_creation_input_tokens ?? 0,
    ),
    cache_read_input_tokens: Math.max(
      a.cache_read_input_tokens ?? 0,
      b.cache_read_input_tokens ?? 0,
    ),
  };
}

/** Field-wise sum of two usage payloads from DIFFERENT messages. */
function addUsage(a: Usage, b: Usage): Usage {
  return {
    input_tokens: a.input_tokens + b.input_tokens,
    output_tokens: a.output_tokens + b.output_tokens,
    cache_creation_input_tokens:
      (a.cache_creation_input_tokens ?? 0) + (b.cache_creation_input_tokens ?? 0),
    cache_read_input_tokens:
      (a.cache_read_input_tokens ?? 0) + (b.cache_read_input_tokens ?? 0),
  };
}

/**
 * The top-level agent's cumulative session usage RIGHT NOW: the last
 * real result's authoritative figure plus the per-message spend of the
 * turn still running (`turnUsage` — cleared whenever a result lands, so
 * nothing is counted twice). Subagent spend is in neither input (§2.4,
 * §2.8), so it is not in the sum. `null` only before the session has
 * reported any usage at all, which renders as a dash rather than a
 * lying zero.
 */
export function topLevelUsage(state: StoreState): Usage | null {
  let tally = state.resultUsage;
  for (const usage of state.turnUsage.values()) {
    tally = tally === null ? usage : addUsage(tally, usage);
  }
  return tally;
}

/**
 * The running turn's context growth SO FAR: how far the session's CURRENT
 * context size (`contextTokens`, input + output) has moved past where the PREVIOUS result
 * left it. This is the live twin of the settled `delta` a result stamps
 * (see `resultContext`), measured from the SAME baseline — the last result's
 * `context.total` — so the figure a turn ends on equals the delta its
 * final-response bubble then shows. At a turn's start no new request has
 * landed yet, so the size still stands where the last result left it and the
 * delta reads 0.
 *
 * `null` when the size is unknown (a `/clear` or a compaction), the same
 * condition under which a result carries no standing.
 */
export function liveContextDelta(state: StoreState): number | null {
  const total = state.contextTokens;
  if (total === null) return null;
  for (let i = state.items.length - 1; i >= 0; i--) {
    const item = state.items[i];
    if (item.kind === "result") return total - (item.context?.total ?? 0);
  }
  return total;
}

/**
 * One string field of a tool call's input, or "" when absent or not a
 * string. A call's input is unparsed until `tool-use-input-end`, so every
 * reader needs this guard and several grew their own byte-identical copy.
 */
export function stringField(item: ToolItem, key: string): string {
  const value = item.input?.[key];
  return typeof value === "string" ? value : "";
}

/** The closed §2.6 status enum, for narrowing a newer daemon's value. */
const ASYNC_STATUSES: ReadonlySet<string> = new Set([
  "running",
  "done",
  "error",
  "killed",
]);

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
  /**
   * The session's `/status` snapshot: the SDK's `system:init` payload the
   * status panel renders. Pushed by a `status` frame — the daemon's answer
   * to a `refresh-status` re-probe — so it reflects a value changed
   * mid-session (a `/fast` toggle, a config edit) that the frozen init would
   * miss. `null` until the first refresh lands; the panel seeds an instant
   * value from GET /status meanwhile, and prefers this fresher one once it
   * arrives. Opaque exactly like the frame's `snapshot`.
   */
  statusSnapshot: unknown;
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
   * The daemon stamp of the session's most recent turn BOUNDARY (a
   * `success` result's envelope ts, or an interrupt's `aborted` one), or
   * `null` before the first one. The anchor the final-response chip
   * measures its elapsed time from: a chip reads "since the previous final
   * response", and the session's first one falls back to its own turn start
   * (`turnStartedAt`).
   *
   * Advanced at a `success` result AND at an interrupt (a non-retracted
   * `aborted` result): an interrupt is a user-made boundary the next
   * response's clock resets from, so the interrupted turn's runtime never
   * bleeds into the following answer's chip. A queued follow-up prompt, a
   * retracted turn, and an errored turn all leave it untouched. The daemon's
   * stamp rather than this tab's clock, like `turnStartedAt`, so a
   * reconnecting tab re-derives the same anchor from the replayed results.
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
   * The top-level agent's CUMULATIVE session usage as of the last real
   * result — the tokens dropdown's authoritative baseline. Results carry
   * session-cumulative snapshots, so each one supersedes the previous
   * rather than adding to it; the synthetic result a transcript-seeded
   * replay closes turns with carries all-zero usage and is skipped, so a
   * rehydrated session shows the last live figure instead of a lying 0.
   * `null` until a real result lands. Per §2.4, subagent spend is NEVER
   * in this figure — the SDK scopes a result's `usage` to the top-level
   * agent loop.
   */
  resultUsage: Usage | null;
  /**
   * Per-message usage observed SINCE `resultUsage` was adopted, keyed by
   * message id — the live increment that keeps the topbar tally moving
   * mid-turn between results. Fields within one message are cumulative
   * (`message_start` carries the input side, each `message_delta` a
   * growing output count), so frames for the same message field-wise max
   * rather than add. Cleared when a real result lands: that result's
   * cumulative figure already includes these messages.
   */
  turnUsage: Map<string, Usage>;
  /**
   * Per-model usage INCLUDING subagents, from the latest result carrying
   * a `model_usage` map — the only whole-tree token figure the SDK
   * reports (`usage` above excludes sidechains). Session-cumulative like
   * `resultUsage`; `null` until a result carries one (a synthetic replay
   * result and a pre-`model_usage` shim never do).
   */
  modelUsage: Record<string, ModelUsage> | null;
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
  /**
   * Whether the turn now ending was RETRACTED — its prompt withdrawn by a
   * `user-turn-retracted` frame rather than merely interrupted. Set by that
   * frame and consumed by the turn's terminating `result`, which is rendered
   * as nothing: a retracted turn never happened, so an "aborted" bubble
   * reporting that it did would be the one trace of the prompt left on a feed
   * that just dropped it. The result still does all its bookkeeping — it is
   * what clears `turnInFlight` and the interrupting indicator.
   */
  turnRetracted: boolean;
  costUsd: number | null;
  /**
   * The session's one-line "current objective" label (§2.14), or `null`
   * before the first completed turn produces one. Set by every
   * `task-summary` frame — the daemon emits one per completed turn only
   * when a Haiku summary came back usable, so the store simply adopts the
   * latest. Rendered centered in the topbar.
   */
  taskSummary: string | null;
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
    statusSnapshot: null,
    items: [],
    queued: [],
    turnInFlight: false,
    turnStartedAt: null,
    lastFinalResponseAt: null,
    contextTokens: null,
    resultUsage: null,
    turnUsage: new Map(),
    modelUsage: null,
    compacting: false,
    interrupting: false,
    turnRetracted: false,
    costUsd: null,
    taskSummary: null,
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
   * Diagnostic sink for the seq/replay decisions this store makes —
   * the machinery that, when it wedges, reads as "cards stopped
   * arriving". Injected (a client-log forwarder in the app, a spy in
   * tests) because the store itself is transport-agnostic; the no-op
   * default keeps every existing call site unchanged.
   */
  constructor(
    private readonly log: (level: "info" | "warn", message: string) => void = () => {},
  ) {}

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
      this.log(
        "warn",
        `seq gap: have ${this.state.lastSeq}, got ${envelope.seq} (${envelope.type}) — requesting replay from ${this.state.lastSeq + 1}`,
      );
      return {
        changed: false,
        send: { type: "replay-request", from_seq: this.state.lastSeq + 1 },
      };
    }
    this.state.lastSeq = envelope.seq;
    const caughtUp =
      this.replayTarget !== null && envelope.seq >= this.replayTarget;
    if (caughtUp) {
      this.replayTarget = null;
      this.log("info", `replay complete (${this.replayKind}) at seq ${envelope.seq}`);
    }
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
        this.log("info", `hello: gap-fill replay ${s.lastSeq + 1}..${hello.seq}`);
        return {
          changed: true,
          send: { type: "replay-request", from_seq: s.lastSeq + 1 },
        };
      }
      this.log("info", `hello: already current at seq ${s.lastSeq}`);
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
    s.resultUsage = null;
    s.turnUsage = new Map();
    s.modelUsage = null;
    s.interrupting = false;
    s.turnRetracted = false;
    s.costUsd = null;
    // Discarded like the other derived state: a still-retained task-summary
    // frame re-applies on replay, and an evicted one is simply absent until
    // the next completed turn produces a fresh label.
    s.taskSummary = null;
    s.lastSeq = Math.max(0, hello.resume_from_seq - 1);
    if (hello.resume_from_seq > 0 && hello.seq >= hello.resume_from_seq) {
      // Full-history replay incoming: render nothing per-frame and
      // restore (tail-first) once lastSeq reaches the hello watermark.
      this.replayTarget = hello.seq;
      this.replayKind = "fresh";
      this.log(
        "info",
        `hello: fresh-join replay ${hello.resume_from_seq}..${hello.seq}`,
      );
      return {
        changed: true,
        send: { type: "replay-request", from_seq: hello.resume_from_seq },
      };
    }
    this.replayTarget = null;
    this.log("info", `hello: fresh join, nothing to replay (seq ${hello.seq})`);
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
          origin: frame.origin,
        });
        s.turnInFlight = true;
        s.turnStartedAt = frame.ts;
        // A fresh turn is starting: any interrupt of the PREVIOUS turn is over
        // (a queue-run-now preemption drains its promoted message here).
        s.interrupting = false;
        // Likewise any retraction: this turn is its own, and answers to it are
        // its own to render.
        s.turnRetracted = false;
        break;
      case "user-turn-retracted": {
        // The prompt is withdrawn: drop its bubble so the feed reads as though
        // it was never sent. The daemon only retracts a turn the agent never
        // answered, so there is nothing below it to orphan.
        const at = s.items.findIndex(
          (i) => i.kind === "user-turn" && i.requestId === frame.request_id,
        );
        if (at !== -1) s.items.splice(at, 1);
        // `interrupting` deliberately stays: the turn is still aborting, and
        // its tail must keep being kept out of new bubbles until the result
        // lands. That result is also what clears the indicator.
        s.turnRetracted = true;
        break;
      }
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
      case "assistant-error": {
        // The error scopes the whole message, so every text block it opened
        // is a failure notice. The frame lands after those blocks (the SDK's
        // verdict arrives only with the completed message), so they already
        // exist and are colored retroactively.
        for (const item of s.items) {
          if (item.kind === "text" && item.messageId === frame.message_id) {
            item.error = frame.error;
          }
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
          ts: frame.ts,
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
      case "async-source": {
        // The descriptor lands on the SPAWNING card, which the frame's own
        // ordering guarantees already exists (§2.6 rides it after the
        // result). An orphan is dropped rather than pushed as a system
        // note: unlike a task-notification, a source with no card names no
        // completion the user needs told about, and the card it belongs to
        // is what the fold would have hung on.
        const item = this.findTool(frame.tool_use_id);
        if (item) {
          item.asyncSource = {
            ...frame.source,
            // A status outside the closed enum reads as running: a fold
            // wrongly saying "done" hides live output, while one wrongly
            // saying "running" only spins a beat too long. The daemon
            // already narrows this, so the guard is against a NEWER daemon
            // paired with this client.
            status: ASYNC_STATUSES.has(frame.source.status)
              ? frame.source.status
              : "running",
          };
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
        // names one the feed knows. A payload whose tool-use-id tag is
        // missing or names a call the feed never saw falls back to the
        // card that ANNOUNCED the task id — the id is the durable half of
        // the correlation, so a harness that drops or mangles the tag
        // still settles its member instead of stranding it amber. Only
        // when neither resolves does the completion surface as a system
        // note rather than vanishing — it is never swallowed.
        const item =
          (frame.tool_use_id ? this.findTool(frame.tool_use_id) : undefined) ??
          (frame.task_id ? this.findSpawner(frame.task_id) : undefined);
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
        // A retracted turn never happened, so it ends in nothing: rendering
        // its "aborted" bubble would leave a report of the prompt on a feed
        // that just dropped the prompt itself. The frame still runs the
        // bookkeeping below — it is what ends the turn.
        if (!s.turnRetracted)
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
        // A success is a final response and becomes the next chip's anchor.
        // An interrupt (an `aborted` result) produced no answer, but it IS a
        // user-made boundary the next response's clock must measure from:
        // leaving the anchor on the pre-interrupt answer bleeds the
        // interrupted turn's whole runtime into the FOLLOWING answer's chip,
        // so the clock never resets across an interrupt. A RETRACTED turn is
        // excluded — its prompt was withdrawn, so it never happened and leaves
        // no clock mark, exactly as it leaves no bubble. An errored turn
        // (`error_*`) still never anchors: it is a failure, not a boundary the
        // user chose.
        if (frame.subtype === "success" ||
            (frame.subtype === "aborted" && !s.turnRetracted))
          s.lastFinalResponseAt = frame.ts;
        s.turnRetracted = false;
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
        // is the session's cumulative spend, not the context it left behind.
        // The standing figure is the one the turn's last request declared.
        s.costUsd = frame.total_cost_usd;
        // The session tallies, by contrast, ARE this frame's business: its
        // usage is the authoritative cumulative figure, superseding the
        // baseline and the per-message increments it already includes. A
        // synthetic replay result carries all-zero usage (the transcript
        // records none) and must not wipe a live tally.
        if (!isZeroUsage(frame.usage)) {
          s.resultUsage = frame.usage;
          s.turnUsage = new Map();
        }
        if (frame.model_usage !== undefined) s.modelUsage = frame.model_usage;
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
      case "usage": {
        // An attributed frame is a SUBAGENT conversation's standing, so it
        // banks on that agent's tool item and never touches the session
        // figure — the agent-scoped topbar reads it from there. An
        // attribution naming a call the feed does not hold (evicted, or
        // discarded by the interrupt gate) has no bubble to feed and drops.
        if (frame.parent_tool_use_id !== undefined) {
          const item = this.findTool(frame.parent_tool_use_id);
          if (item) item.contextTokens = contextTokens(frame.usage);
          break;
        }
        s.contextTokens = contextTokens(frame.usage);
        if (frame.cost_usd !== undefined) s.costUsd = frame.cost_usd;
        // The live increment of the session tally: fold this request's
        // usage into its message's entry so the topbar figure moves
        // mid-turn instead of waiting for the result. Main-chain only —
        // an attributed (subagent) frame banked on its tool item above
        // and never reaches this tally.
        const prior = s.turnUsage.get(frame.message_id);
        s.turnUsage.set(
          frame.message_id,
          prior === undefined ? frame.usage : maxUsage(prior, frame.usage),
        );
        break;
      }
      case "permission-mode-changed":
        s.permissionMode = frame.mode;
        break;
      case "models":
        s.models = frame.models;
        break;
      case "status":
        // A `refresh-status` re-probe answered: adopt the fresh snapshot the
        // panel renders. REPLACES rather than merges — a value cleared since
        // the last probe (a `/fast` toggled back off) must disappear.
        s.statusSnapshot = frame.snapshot;
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
        if (frame.subtype === "init") {
          s.contextTokens = null;
          // The same drop must zero the SESSION token tallies the topbar
          // chip reads (`topLevelUsage`): a cleared context has spent
          // nothing to think with, so the chip dashes until the next turn
          // rather than keep the pre-clear figure. Gated on the init
          // actually following a `/clear` — a RESUMED session emits an
          // init AFTER its transcript replay too (`turn.ts`), and that
          // init must NOT wipe the tally the replay just restored. The
          // `/clear` turn is the most recent user-turn exactly at a
          // clear-init; a resume whose last turn was not a clear keeps
          // its restored tally.
          const lastTurn = this.findLast(
            (i): i is UserTurnItem => i.kind === "user-turn",
          );
          if (lastTurn && isClearTurn(lastTurn)) {
            s.resultUsage = null;
            s.turnUsage = new Map();
            s.modelUsage = null;
          }
        }
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
      case "task-summary":
        // The daemon emits one per completed turn only when a usable Haiku
        // summary came back, so adopting the latest is the whole reducer.
        s.taskSummary = frame.summary;
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

  /**
   * The call that spawned TASKID: classified source id first (structured,
   * so immune to prose drift), announced ids second — the notification
   * fallback when the payload's tool-use-id tag resolves to no card.
   */
  private findSpawner(taskId: string): ToolItem | undefined {
    return this.findLast(
      (i): i is ToolItem =>
        i.kind === "tool" &&
        (i.asyncSource?.source_id === taskId || spawnedTaskIds(i).includes(taskId)),
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
