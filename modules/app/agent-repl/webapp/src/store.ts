/**
 * ConversationStore — holds the renderable conversation + session state that
 * `render.ts` (and the topbar/tail chrome) draw from.
 *
 * After the agent-shim cutover (design §11, §14.2, §16) the store no longer
 * parses a bespoke daemon frame vocabulary. The daemon pushes
 * `agentshim.frontend.v1` protojson frames; `main.ts` decodes them
 * (`frontend-proto.ts`) and maps them to typed `AdapterEffect`s
 * (`state-adapter.ts`); the store's SOLE ingestion entry point is `ingest()`,
 * which folds those effects onto this state. There is one path, no dual
 * mechanism, and the old `applyRaw`/`parseFrame`/per-frame reducers are gone.
 *
 * The store is pure with respect to I/O: `ingest()` mutates state and reports
 * whether anything visible changed; the caller schedules the render.
 */
import type { CounterEntry } from "./counter-menu.js";
import type { ErrorClass } from "./frontend-proto.js";
import type {
  AdapterEffect,
  ProgressInput,
  QueueInput,
  SessionInitInput,
  SessionViewInput,
  ToolProgressInput,
  TypingReveal,
  WebRenderState,
  WorkspaceStatusInput,
} from "./state-adapter.js";
import { applyStreamDelta, blockKey, insertBySeq, settleStreamedBlock } from "./streaming.js";
import {
  AsyncSource,
  ContentBlock,
  ModelInfo,
  ModelUsage,
  PermissionMode,
  PermissionPreview,
  QueuedItem,
  RenderHint,
  ResultSubtype,
  Usage,
} from "./protocol.js";

// --- conversation items -------------------------------------------------------

/**
 * The feed-order rank every conversation item carries: the session-stream seq
 * of the `ConversationDelta` that delivered it (locally-minted items take the
 * store's high-water mark at creation instead).
 *
 * Feed order is NOT arrival order. The connect resync replays history (low
 * seqs) over the same socket that carries live pushes (high seqs), so a live
 * item — the user's own prompt echo, most visibly — can arrive mid-replay.
 * Appending in arrival order stranded that prompt wherever the replay
 * happened to be, permanently. Items are instead inserted at their seq rank
 * (`insertBySeq`), which is deterministic however the two streams interleave.
 *
 * Optional only for items minted before ranking existed (tests, catalogue
 * fixtures); every store ingestion path assigns it.
 */
interface FeedOrderedItem {
  seq?: number;
}

export interface UserTurnItem extends FeedOrderedItem {
  kind: "user-turn";
  requestId: string;
  content: ContentBlock[];
  /** When the prompt was sent, rendered on the bubble. */
  ts: string;
  /**
   * When set, the prompt was injected on the user's behalf and renders as a
   * status card rather than a user-prompt bubble (see the `user-turn` render
   * dispatch). "merge" is the merge-failure remediation turn. Absent for a
   * user's own prompt.
   */
  origin?: string;
}
export interface TextItem extends FeedOrderedItem {
  kind: "text";
  /**
   * The block's PLACE in the feed: the DOM key `render.ts` draws it under and
   * the id `smooth.ts` tracks its reveal by. Opened by whichever side saw the
   * block first (a preview's `${messageId}:${apiBlockIndex}`, or a backfilled
   * record's own identity) and never moved afterwards, so neither the reveal
   * cursor nor the rendered node is disturbed when the block settles.
   */
  blockId: string;
  messageId: string;
  /**
   * The block's RECORD identity, set once the finished message lands and
   * absent while it is only a streamed preview. This — not `blockId` — is what
   * the block is deduped on, so a replayed record replaces the item it already
   * produced instead of appearing twice.
   */
  uuid?: string;
  /** Owning subagent call; undefined on main-chain blocks. */
  parentToolUseId?: string;
  text: string;
  done: boolean;
  /**
   * When the agent OPENED the block, rendered on the bubble. Taken at the
   * start rather than the end so the stamp holds still while the block streams.
   */
  ts: string;
}
export interface ThinkingItem extends FeedOrderedItem {
  kind: "thinking";
  /** The block's place in the feed; see {@link TextItem.blockId}. */
  blockId: string;
  messageId: string;
  /** The block's record identity; see {@link TextItem.uuid}. */
  uuid?: string;
  /** Owning subagent call; undefined on main-chain blocks. */
  parentToolUseId?: string;
  text: string;
  done: boolean;
  signature?: string;
}
export interface ToolItem extends FeedOrderedItem {
  kind: "tool";
  toolUseId: string;
  toolName: string;
  messageId: string;
  parentToolUseId?: string;
  /**
   * When the call opened. The agent-scoped topbar's elapsed clock counts from
   * here (to `resultTs` once the call settles, to now while it runs).
   */
  ts: string;
  /**
   * When this call is a subagent's, the SUBAGENT conversation's own context
   * size as its last attributed usage declared it. Absent until the agent's
   * first request reports one.
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
  /** When the call settled. */
  resultTs?: string;
  /**
   * The detached work this call spawned, as the daemon classified it from the
   * structured tool result. Present only on a call that actually owns a
   * stream, which is what the async fold keys off.
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
export interface PermissionItem extends FeedOrderedItem {
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
 * conversation carries into an API request, and how much that grew over the
 * previous result.
 */
export interface ResultContext {
  total: number;
  delta: number;
}
export interface ResultItem extends FeedOrderedItem {
  kind: "result";
  subtype: ResultSubtype;
  durationMs: number;
  /**
   * How long the turn ran measured from the PREVIOUS turn boundary of the
   * session, or from this turn's own start for the session's first one. This
   * is what the final-response bubble's chip shows, as against `durationMs`
   * (the SDK's whole-task figure) which the standalone chip still shows.
   */
  sincePrevFinalMs: number;
  numTurns: number;
  totalCostUsd: number;
  usage: Usage;
  /**
   * The result's per-model usage map (§2.4 `model_usage`): session-wide and
   * WHOLE-TREE, so unlike `usage` it counts every subagent's requests too.
   * ABSENT when the result carried none, the same convention `resultText`
   * below follows.
   *
   * It rides here because the daemon passes the typed `ResultMessage` through
   * unchanged into the `result` conversation item, so the map has always been
   * on the wire — the webapp simply never read it.
   */
  modelUsage?: Record<string, ModelUsage>;
  isError: boolean;
  resultText?: string;
  /**
   * `null` when the turn ended with the session's context size unknown: a
   * `/clear` re-inits the session and a compaction rewrites it, and neither
   * reports the size it left behind, so the figure is unknown until the next
   * API request declares it.
   */
  context: ResultContext | null;
}
/**
 * The context was CLEARED (`core.v1.ContextCleared`): discarded outright.
 *
 * The wire message is EMPTY — its existence and its position in the feed are
 * the entire fact — so the item carries nothing but the envelope uuid that
 * keys its DOM node. It is a first-class event now rather than a `/clear`
 * prompt this end recognized by its text, which is why a replayed session
 * draws the same boundary a live one does.
 */
export interface ContextClearedItem extends FeedOrderedItem {
  kind: "context-cleared";
  uuid: string;
}
/**
 * The context was COMPACTED (`core.v1.ContextCompacted`): replaced by a
 * summary that stands in for it.
 *
 * The daemon coalesces the vendor's three partial reports (a start status, a
 * token-carrying boundary, and the summary text the vendor writes as an
 * ordinary user message) into this one fact, so every field below is read
 * from a single item rather than correlated across three.
 *
 * `summary` is load-bearing: it is the ONLY surviving account of everything
 * the compaction discarded, which is why the feed renders it as a bubble
 * rather than folding it away.
 *
 * `trigger` is carried for information only. A compaction is a compaction,
 * and NOTHING branches on whether the system or the user asked for one.
 */
export interface ContextCompactedItem extends FeedOrderedItem {
  kind: "context-compacted";
  uuid: string;
  trigger: "auto" | "manual" | "unspecified";
  preTokens: number;
  postTokens: number;
  durationMs: number;
  summary: string;
}
/**
 * A daemon-classified failure, as a conversation card.
 *
 * It replaces the ErrorItem/RetryItem pair, which this end derived from a raw
 * ApiErrorLine by rules the DAEMON did not share: a different retry test, a
 * third rule for "fatal" that nothing rendered, a hardcoded `code` of
 * "api_error" and a hardcoded `recoverable` of false — neither fed by
 * anything. Every field here is the daemon's verdict, adopted unchanged.
 */
export interface SystemFailureCard extends FeedOrderedItem {
  kind: "failure";
  /** SEMANTIC, never chromatic: the color comes from the shared table. */
  errorClass: ErrorClass;
  errorType: string;
  message: string;
  /** The raw account, shown beside the prose rather than replacing it. */
  sourceDetail: string;
  /**
   * Wall-clock ms a WINDOW-shaped failure closed; 0 while open. A resolved
   * card renders as settled rather than as a standing alarm, which is the
   * whole reason the two edges reconcile onto one uuid.
   */
  resolvedAtMs: number;
  /**
   * The daemon's uuid for this card. It is the ADDRESS the progress footer's
   * error row scrolls to, which is the only way to find a failure that has
   * already scrolled off.
   */
  uuid: string;
}
export interface SystemItem extends FeedOrderedItem {
  kind: "system";
  subtype: string;
}

/**
 * The tokens a request leaves occupying the model's context: the fresh input
 * tokens, plus the cached prefix it read, plus the prefix it wrote to cache,
 * PLUS the output it produced. Their sum IS the conversation's full standing
 * size at that request.
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
 * The top-level agent's cumulative session usage RIGHT NOW: the last real
 * result's authoritative figure plus the per-message spend of the turn still
 * running. `null` only before the session has reported any usage at all,
 * which renders as a dash rather than a lying zero.
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
 * context size has moved past where the PREVIOUS result left it. `null` when
 * the size is unknown (a `/clear` or a compaction).
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
 * One string field of a tool call's input, or "" when absent or not a string.
 * A call's input is unparsed until its input completes, so every reader needs
 * this guard.
 */
export function stringField(item: ToolItem, key: string): string {
  const value = item.input?.[key];
  return typeof value === "string" ? value : "";
}

export type ConversationItem =
  | UserTurnItem
  | TextItem
  | ThinkingItem
  | ToolItem
  | PermissionItem
  | ResultItem
  | ContextClearedItem
  | ContextCompactedItem
  | SystemFailureCard
  | SystemItem;

// --- store state -----------------------------------------------------------------

export interface StoreState {
  sessionId: string;
  daemonVersion: string;
  /** The session's live model (from `SessionView`). */
  model: string;
  /**
   * The `set-model` menu. GAP after the cutover: `agentshim.frontend.v1`'s
   * `SessionView` carries only the current model, not the selectable list, so
   * this stays empty until the frontend surface grows a model-catalog field.
   */
  models: ModelInfo[];
  /**
   * The session's working directory, from `SessionView.cwd` (the additive S6
   * field that gave the resume/rebind path its input back).
   */
  cwd: string;
  /**
   * Durable CLI session uuid, from `SessionView.claude_session_id` (additive
   * S6) — the client-side rebind key.
   */
  claudeSessionId: string;
  permissionMode: PermissionMode;
  /**
   * The session's retained `data.v1.SystemInit` (protojson, camelCase),
   * adopted from the pushed `sessionInit` frame — the /status panel's snapshot
   * source after the cutover (replacing the GET /status probe). `null` before
   * any init lands.
   */
  systemInit: Record<string, unknown> | null;
  items: ConversationItem[];
  /**
   * The prompts the DAEMON is holding for this session (E4), sourced wholesale
   * from the pushed `QueueView`. Empty whenever no turn is running, because
   * the queue only forms behind a running turn.
   */
  queued: QueuedItem[];
  turnInFlight: boolean;
  /**
   * When the in-flight turn started, and `null` whenever no turn is running.
   * Stamped when `WorkspaceState.turnActive` transitions false→true (the
   * effect carries no start timestamp, so the store uses its own clock — a
   * mid-turn reconnect therefore restarts the elapsed count).
   */
  turnStartedAt: string | null;
  /**
   * The stamp of the session's most recent turn BOUNDARY, or `null` before the
   * first one. The anchor the final-response chip measures its elapsed time
   * from. Carried on pre-rendered `result` items from the daemon; the store no
   * longer derives it.
   */
  lastFinalResponseAt: string | null;
  /**
   * The session's context size. Fed from `SessionView.totalTokens`. A
   * `/clear` or compaction that used to revert this to `null` is now the
   * daemon's to resolve.
   */
  contextTokens: number | null;
  /**
   * The top-level agent's CUMULATIVE session usage baseline, re-adopted from
   * every landed `result` item's `usage` (see `adoptResultUsage`). `null`
   * before the session's first result, which dashes the overlay's top-level
   * section rather than lying with zeros.
   */
  resultUsage: Usage | null;
  /**
   * Per-message usage observed SINCE `resultUsage` was adopted, summed onto
   * that baseline by `topLevelUsage`. Cleared whenever a fresh result
   * re-baselines, so a request folded into the new baseline is never counted
   * twice. Still empty in practice: the daemon pushes no per-message usage
   * frame, so the baseline alone carries the top-level figure between results.
   */
  turnUsage: Map<string, Usage>;
  /**
   * Per-model usage INCLUDING subagents (§2.4 `model_usage`) — the only figure
   * that counts subagent spend. Re-adopted from every landed `result` item
   * that carries a map; `null` until the first one does.
   */
  modelUsage: Record<string, ModelUsage> | null;
  /**
   * Whether the running turn is being INTERRUPTED. GAP after the cutover: no
   * interrupt frame in `frontend.v1`; stays false (the SSM-resolved
   * `WorkspaceState` is the daemon's place to express this now).
   */
  interrupting: boolean;
  /** Whether the turn now ending was RETRACTED. GAP after the cutover; stays false. */
  turnRetracted: boolean;
  costUsd: number | null;
  /**
   * The session's one-line "current objective" label, fed from
   * `SessionView.title`, or `null` before one is produced.
   */
  taskSummary: string | null;
  /**
   * The store's through-seq watermark: the highest `ConversationDelta`
   * `throughSeq` ingested. Diagnostics only; the daemon owns ordering now.
   */
  lastSeq: number;
  /**
   * THE workspace's resolved render state (F5) — the one authority for the
   * footer's phase, the same message the Emacs tab bar reads.
   *
   * `null` before the first `WorkspaceState` lands. The footer used to read a
   * copy of this carried on `ProgressView`; the copy refreshed on the progress
   * resolver's own triggers and went stale, which is what put "starting" in
   * the footer of an already-green tab.
   */
  renderState: WebRenderState | null;
  /**
   * The delivery generation of `renderState` — what this end acknowledges once
   * it has drawn that state. Emacs is not sent a state until this end answers
   * for its generation, so the two surfaces cannot show different states.
   */
  renderGeneration: number;
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
    systemInit: null,
    items: [],
    queued: [],
    turnInFlight: false,
    turnStartedAt: null,
    lastFinalResponseAt: null,
    contextTokens: null,
    resultUsage: null,
    turnUsage: new Map(),
    modelUsage: null,
    interrupting: false,
    turnRetracted: false,
    costUsd: null,
    taskSummary: null,
    lastSeq: 0,
    renderState: null,
    renderGeneration: 0,
  };
}

/** Result of ingesting a batch of adapter effects. */
export interface IngestResult {
  /** Whether visible state changed (render needed). */
  changed: boolean;
}

/**
 * Fold an incoming tool item onto the one it reconciles with (same
 * toolUseId). A tool call arrives as TWO items sharing that id — the
 * `tool_use` (name + input) and, later, the `tool_result` (the result,
 * carrying an EMPTY toolName and no input by the daemon's contract, since
 * those live on the call item). Each contributes only the fields it holds, so
 * an incoming empty/absent field never clobbers a value already held, and
 * cross-plane reordering (a result observed before its call) lands the same
 * merged card either way.
 */
function mergeToolItem(existing: ToolItem, incoming: ToolItem): ToolItem {
  const merged: ToolItem = { ...existing };
  if (incoming.toolName !== "") merged.toolName = incoming.toolName;
  if (incoming.messageId !== "") merged.messageId = incoming.messageId;
  // `ts` is when the call OPENED; a result item's later stamp must not move it.
  if (merged.ts === "" && incoming.ts !== "") merged.ts = incoming.ts;
  if (incoming.inputDone) merged.inputDone = true;
  if (incoming.inputJson !== "") merged.inputJson = incoming.inputJson;
  if (incoming.input !== undefined) merged.input = incoming.input;
  if (incoming.parentToolUseId !== undefined) merged.parentToolUseId = incoming.parentToolUseId;
  if (incoming.contextTokens !== undefined) merged.contextTokens = incoming.contextTokens;
  if (incoming.progress !== undefined) merged.progress = incoming.progress;
  if (incoming.progressElapsedS !== undefined) merged.progressElapsedS = incoming.progressElapsedS;
  if (incoming.notification !== undefined) merged.notification = incoming.notification;
  if (incoming.resultTs !== undefined) merged.resultTs = incoming.resultTs;
  if (incoming.asyncSource !== undefined) merged.asyncSource = incoming.asyncSource;
  if (incoming.taskOutput !== undefined) merged.taskOutput = incoming.taskOutput;
  if (incoming.result !== undefined) merged.result = incoming.result;
  return merged;
}

/** The stable identity a conversation item is reconciled on, or null if it has none. */
function itemKey(item: ConversationItem): string | null {
  switch (item.kind) {
    case "user-turn":
      return `user-turn:${item.requestId}`;
    // Streamed prose is keyed by the ONE authority on block identity, so the
    // rule cannot drift from the one the reconciler matches on.
    case "text":
    case "thinking":
      return blockKey(item);
    case "tool":
      return `tool:${item.toolUseId}`;
    case "permission":
      return `permission:${item.requestId}`;
    // A WINDOW-shaped failure is re-sent under its OPENING uuid with a
    // resolution stamp, so it must reconcile in place. Appending instead
    // would leave the alarm standing beside its own all-clear.
    case "failure":
      return `failure:${item.uuid}`;
    // Terminal / one-shot items carry no reconcilable id: they are appended.
    default:
      return null;
  }
}

export class ConversationStore {
  state: StoreState = initialState();

  /**
   * The session's live detached-task roster (agent/shell/workflow), fed
   * verbatim from `TaskCatalog` frames via the `task-catalog` effect. This is
   * the resolved list the topbar's tasks counter renders — the daemon (G9)
   * owns the task-lifecycle resolution now, so the webapp no longer derives it
   * from tool cards. Kept beside `state` (not in `StoreState`) so the render
   * input's shape is unchanged for its many consumers.
   */
  taskRoster: CounterEntry[] = [];

  /**
   * The consolidated progress footer's input (F1), adopted wholesale from the
   * daemon's `ProgressView`. `null` before the first one lands, which is the
   * footer's own "nothing resolved yet" state rather than a fabricated blank.
   *
   * Kept beside `state` for the same reason `taskRoster` is: the render input's
   * shape stays unchanged for its many consumers, and the footer is the only
   * surface that reads this.
   */
  progress: ProgressInput | null = null;

  /**
   * Diagnostic sink and clock. LOG surfaces ingestion anomalies (a typing
   * delta with nowhere to land); NOW stamps a turn's start (the effect carries
   * no timestamp). Both injected so the store stays transport- and clock-
   * agnostic; the no-op/`Date.now` defaults keep call sites unchanged.
   */
  constructor(
    private readonly log: (level: "info" | "warn", message: string) => void = () => {},
    private readonly now: () => number = () => Date.now(),
  ) {}

  /**
   * Discard all state, as if freshly constructed. Used when the live view is
   * rebound onto a DIFFERENT daemon session (the "session gone" rebind).
   */
  reset(): void {
    this.state = initialState();
    this.taskRoster = [];
    this.progress = null;
  }

  /**
   * THE ingestion entry point. Folds a batch of decoded-frame adapter effects
   * onto the store, returning whether anything visible changed. An `ignored`
   * effect is not store state — the adapter already counted/logged it — so
   * it is a no-op here.
   */
  ingest(effects: readonly AdapterEffect[]): IngestResult {
    let changed = false;
    for (const effect of effects) {
      switch (effect.kind) {
        case "workspace-state":
          changed = this.applyWorkspaceState(effect.value) || changed;
          break;
        case "session-view":
          changed = this.applySessionView(effect.value) || changed;
          break;
        case "conversation-items":
          changed = this.applyConversationItems(effect.items, effect.throughSeq) || changed;
          break;
        case "typing":
          changed = this.applyTyping(effect.value) || changed;
          break;
        case "tool-progress":
          changed = this.applyToolProgress(effect.value) || changed;
          break;
        case "queue":
          changed = this.applyQueue(effect.value) || changed;
          break;
        case "task-catalog":
          this.taskRoster = effect.value.entries;
          changed = true;
          break;
        case "progress":
          changed = this.applyProgress(effect.value) || changed;
          break;
        case "session-init":
          changed = this.applySessionInit(effect.value) || changed;
          break;
        case "ignored":
          break;
      }
    }
    return { changed };
  }

  // --- effect appliers ------------------------------------------------------

  private applyWorkspaceState(ws: WorkspaceStatusInput): boolean {
    const s = this.state;
    if (ws.sessionId !== "") s.sessionId = ws.sessionId;
    // THE workspace's phase, kept so the footer reads the same authority the
    // tab bar does. The generation rides with it because acknowledging the
    // state is how this end releases it to Emacs.
    s.renderState = ws.state;
    s.renderGeneration = ws.generation;
    const wasActive = s.turnInFlight;
    s.turnInFlight = ws.turnActive;
    if (ws.turnActive && !wasActive) {
      s.turnStartedAt = new Date(this.now()).toISOString();
    } else if (!ws.turnActive) {
      s.turnStartedAt = null;
    }
    return true;
  }

  /**
   * Adopt the daemon-resolved progress view (F1) wholesale, and take the one
   * fact it resolves BETTER than the store could: the turn's REAL start
   * stamp, so a tab that joins mid-turn picks the elapsed clock up where the
   * turn actually is rather than restarting it from the moment this tab
   * noticed.
   *
   * The view's in-progress windows (including `compacting`) are read straight
   * off the retained `ProgressView` by the footer — the store keeps no copy.
   */
  private applyProgress(p: ProgressInput): boolean {
    this.progress = p;
    const s = this.state;
    if (p.turnStartedAtMs > 0) {
      s.turnStartedAt = new Date(p.turnStartedAtMs).toISOString();
    }
    return true;
  }

  private applySessionView(sv: SessionViewInput): boolean {
    const s = this.state;
    if (sv.sessionId !== "") s.sessionId = sv.sessionId;
    if (sv.model !== "") s.model = sv.model;
    if (sv.permissionMode !== "") s.permissionMode = sv.permissionMode as PermissionMode;
    s.costUsd = sv.totalCostUsd;
    s.contextTokens = sv.totalTokens > 0 ? sv.totalTokens : null;
    if (sv.title !== "") s.taskSummary = sv.title;
    // The durable resume keys feed the client-side rebind (main.ts); an
    // empty value never clobbers a filled record.
    if (sv.claudeSessionId !== "") s.claudeSessionId = sv.claudeSessionId;
    if (sv.cwd !== "") s.cwd = sv.cwd;
    return true;
  }

  /**
   * Adopt the pushed `SystemInit` — the /status panel's snapshot source. The
   * daemon re-pushes the whole retained init, so the latest wins wholesale.
   */
  private applySessionInit(si: SessionInitInput): boolean {
    this.state.systemInit = si.init;
    return true;
  }

  /**
   * File a failure card that arrived OUTSIDE the conversation stream — a
   * refused command's `CommandAck.failure` (F4).
   *
   * It goes into the feed rather than into a toast, for the same reason the
   * degraded card does: the feed is where a user looks to find out what
   * happened, and a refusal that renders anywhere else is a refusal they will
   * miss. Keyed by uuid like every other item, so a re-delivery replaces
   * rather than duplicates.
   */
  addFailure(failure: SystemFailureCard): boolean {
    // A locally-minted card has no delta seq; the high-water mark ranks it at
    // the feed's live tail, where a fault report belongs.
    this.mergeItem(failure, this.state.lastSeq);
    return true;
  }

  private applyConversationItems(
    items: readonly ConversationItem[],
    throughSeq: number,
  ): boolean {
    for (const item of items) {
      this.mergeItem(item, throughSeq);
      if (item.kind === "result") this.adoptResultUsage(item);
    }
    if (throughSeq > this.state.lastSeq) this.state.lastSeq = throughSeq;
    return items.length > 0;
  }

  /**
   * Bank a landed result's usage figures — the tokens overlay's two cumulative
   * sources, which had no feed at all after the cutover and rendered as dashes.
   *
   * Both are SESSION-CUMULATIVE snapshots the SDK recomputes per result, not
   * deltas, so each result REPLACES rather than accumulates onto the last:
   * - `resultUsage` is the top-level agent loop's own spend, and `turnUsage`
   *   (the per-message increments banked on top of it) is cleared with it, so
   *   the overlay's top-level section never double-counts a request already
   *   folded into the new baseline;
   * - `modelUsage` is the whole-tree per-model map, the only figure that
   *   counts subagents.
   *
   * A result carrying no map leaves the standing one alone: an absent map is
   * the SDK declining to itemize this result, not an assertion that the
   * session has spent nothing.
   */
  private adoptResultUsage(item: ResultItem): void {
    this.state.resultUsage = item.usage;
    this.state.turnUsage.clear();
    if (item.modelUsage !== undefined) this.state.modelUsage = item.modelUsage;
  }

  /**
   * Reconcile ONE pre-rendered conversation item onto the feed: replace the
   * existing item with the same stable id (a tool card gaining its result, a
   * text block completing), else insert at SEQ's rank (`insertBySeq`) —
   * arrival order is replay-vs-live interleave, not conversation order.
   * A redelivery replaces content, never position: the standing item keeps
   * the rank the feed first placed it at.
   */
  private mergeItem(item: ConversationItem, seq: number): void {
    // Streamed prose has its own lifecycle — a finished block and the preview
    // its deltas grew share no key, so pairing them is a match rather than a
    // lookup — and `streaming.ts` owns all of it.
    if (item.kind === "text" || item.kind === "thinking") {
      settleStreamedBlock(this.state.items, item, seq);
      return;
    }
    const key = itemKey(item);
    if (key !== null) {
      const idx = this.state.items.findIndex((i) => itemKey(i) === key);
      if (idx !== -1) {
        const existing = this.state.items[idx];
        // A tool call's two items (use + result) field-merge so the result
        // never wipes the call's name/input; every other kind is a whole-item
        // replace.
        const merged =
          item.kind === "tool" && existing.kind === "tool"
            ? mergeToolItem(existing, item)
            : item;
        merged.seq = existing.seq;
        this.state.items[idx] = merged;
        return;
      }
    }
    insertBySeq(this.state.items, item, seq);
  }

  /**
   * Grow a still-streaming block from an EPHEMERAL typing relay, keyed by the
   * `${uuid}:${blockIndex}` block id so the `smooth.ts` reveal animates it. A
   * `text`/`thinking` delta finds-or-creates the block; the complete message
   * later arrives via `ConversationDelta` and REPLACES this preview
   * (reconciled by the same block id). An `input_json` delta grows the tool
   * call still awaiting its input; with none open it is loud-logged, never
   * silently dropped.
   */
  /**
   * Adopt a long-tool liveness heartbeat (E4) onto the open call it names, so
   * the running tool chip's elapsed clock keeps ticking instead of the tool
   * looking hung.
   *
   * A heartbeat naming a call the store does not hold is NOT an error: the
   * relay is ephemeral and deliberately absent from StateSnapshot, so a
   * frontend that reconnects mid-tool can legitimately receive heartbeats for
   * a call whose tool_use block it never saw. That case is reported on the
   * diagnostics channel and changes nothing, rather than being invented into
   * a phantom item.
   */
  private applyToolProgress(p: ToolProgressInput): boolean {
    const item = this.state.items.find(
      (i): i is ToolItem => i.kind === "tool" && i.toolUseId === p.toolUseId,
    );
    if (!item) {
      this.log(
        "info",
        `heartbeat for unknown tool call ${p.toolUseId} (${p.toolName}); ` +
          "expected after a reconnect, since heartbeats are never replayed",
      );
      return false;
    }
    // A settled call cannot get any livelier; a late heartbeat must not revive
    // its elapsed clock after the result already froze it.
    if (item.resultTs !== undefined) return false;
    if (item.progressElapsedS === p.elapsedSeconds) return false;
    item.progressElapsedS = p.elapsedSeconds;
    return true;
  }

  /**
   * Adopt the session's held-prompt queue (E4).
   *
   * WHOLESALE REPLACEMENT, not a merge: the daemon pushes the complete queue on
   * every change and owns both the ordering and the classifications. Merging
   * here would make the webapp a second, divergent source of truth for state
   * the daemon already resolved — the exact thing the redesign removes. An
   * empty entries list therefore empties the queue, which is how a drained
   * queue and a dead session both clear their chips.
   */
  private applyQueue(q: QueueInput): boolean {
    this.state.queued = q.entries.map((e) => ({
      id: e.id,
      text: e.text,
      queuedAtMs: e.queuedAtMs,
      classification: e.classification,
      rationale: e.rationale,
      accepted: e.accepted,
    }));
    return true;
  }

  private applyTyping(reveal: TypingReveal): boolean {
    const outcome = applyStreamDelta(
      this.state.items,
      reveal,
      new Date(this.now()).toISOString(),
      // The typing relay is ephemeral and carries no seq; the high-water mark
      // ranks the preview at the live tail while replayed history (lower
      // seqs) still slots above it.
      this.state.lastSeq,
    );
    if (outcome.changed) return true;
    // A dropped delta is prose the user never sees, so it is reported loudly
    // rather than being absorbed by the `false` return.
    this.log(
      "warn",
      `typing input_json delta with no open tool to grow (block ${outcome.blockId})`,
    );
    return false;
  }
}
