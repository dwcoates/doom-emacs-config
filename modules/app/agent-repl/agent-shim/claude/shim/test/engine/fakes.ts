/**
 * The doubles the engine suites drive.
 *
 * A SCRIPTED QUERY, NOT A MOCKED SDK. `ScriptedQuery` implements `QueryLike`
 * and records every control verb, so a suite asserts what the engine ASKED THE
 * VENDOR TO DO rather than what it did internally — the difference between
 * testing the contract and testing the implementation.
 */
import { create } from "@bufbuild/protobuf";
import { conversationv1, storev1 } from "../../src/proto.js";
import type {
  AccountInfoLike,
  AccountUsageLike,
  ContextUsageLike,
  InitializationResultLike,
  InterruptReceipt,
  McpServerStatusLike,
  ModelInfoLike,
  PermissionModeLike,
  SdkMessage,
  SlashCommandLike,
  AgentInfoLike,
  QueryLike,
} from "../../src/sdk/types.js";
import type {
  AgentPageSession,
  FlushOutcome,
  PersistEntry,
  Persistence,
} from "../../src/store/persistence.js";
import { PersistenceError } from "../../src/store/persistence.js";
import type { EngineFold, EngineFoldOutput, FoldContext } from "../../src/engine/fold-context.js";
import type { KeepaliveScheduler } from "../../src/engine/keepalive.js";
import type { BashRunStanding } from "../../src/store/reader.js";

/** A query whose message stream a suite pushes into, one message at a time. */
export class ScriptedQuery implements QueryLike {
  private readonly queued: SdkMessage[] = [];
  private waiting: ((value: IteratorResult<SdkMessage>) => void) | undefined;
  /** The parked consumer's rejection, so a failure THROWS rather than ending. */
  private waitingReject: ((reason: Error) => void) | undefined;
  private ended = false;
  private failure: Error | undefined;

  readonly calls: string[] = [];
  readonly stoppedTasks: string[] = [];
  models: ModelInfoLike[] = [];
  mcp: McpServerStatusLike[] = [];
  contextUsage: ContextUsageLike = emptyContextUsage();
  accountUsage: AccountUsageLike = emptyAccountUsage();
  backgroundTaskAnswer = false;
  setModelRejects: Error | undefined;
  setPermissionModeRejects: Error | undefined;
  /**
   * `close()` records the call but leaves the stream standing.
   *
   * The vendor's own end-of-stream is a courtesy, not a guarantee: a query
   * whose iterator never completes after a close is exactly what wedges a
   * teardown that waits on its message loop.
   */
  closeLeavesStreamOpen = false;

  emit(message: SdkMessage): void {
    if (this.waiting !== undefined) {
      const resolve = this.waiting;
      this.waiting = undefined;
      this.waitingReject = undefined;
      resolve({ value: message, done: false });
      return;
    }
    this.queued.push(message);
  }

  /** End the stream as an orderly EOF. */
  end(): void {
    this.ended = true;
    if (this.waiting !== undefined) {
      const resolve = this.waiting;
      this.waiting = undefined;
      this.waitingReject = undefined;
      resolve({ value: undefined, done: true });
    }
  }

  /**
   * End the stream by THROWING out of the iterator.
   *
   * It rejects a consumer that is already parked in `next`, which is the only
   * state a live session's message loop is ever in. Delegating to `end()` made
   * this fake report an orderly EOF for a death instead — so the shim's own
   * "the vendor query is gone" record named the stream ending, not the vendor's
   * error, for every test that used it.
   */
  fail(error: Error): void {
    this.failure = error;
    this.ended = true;
    const reject = this.waitingReject;
    this.waiting = undefined;
    this.waitingReject = undefined;
    if (reject !== undefined) reject(error);
  }

  [Symbol.asyncIterator](): AsyncIterator<SdkMessage> {
    return {
      next: (): Promise<IteratorResult<SdkMessage>> => {
        const next = this.queued.shift();
        if (next !== undefined) return Promise.resolve({ value: next, done: false });
        if (this.failure !== undefined) return Promise.reject(this.failure);
        if (this.ended) return Promise.resolve({ value: undefined, done: true });
        return new Promise((resolve, reject) => {
          this.waiting = resolve;
          this.waitingReject = reject;
        });
      },
    };
  }

  interrupt(): Promise<InterruptReceipt | undefined> {
    this.calls.push("interrupt");
    return Promise.resolve({ still_queued: [] });
  }
  setPermissionMode(mode: PermissionModeLike): Promise<void> {
    this.calls.push(`setPermissionMode:${mode}`);
    return this.setPermissionModeRejects === undefined
      ? Promise.resolve()
      : Promise.reject(this.setPermissionModeRejects);
  }
  setModel(model?: string): Promise<void> {
    this.calls.push(`setModel:${model ?? ""}`);
    return this.setModelRejects === undefined ? Promise.resolve() : Promise.reject(this.setModelRejects);
  }
  /**
   * PARK `supportedModels` UNTIL A SUITE RELEASES IT.
   *
   * That call is the start's PROVEN-LIVE SIGNAL, so a query that answers it
   * settles the start the moment the engine asks. Every suite whose subject is
   * something that settles a start BEFORE the live signal — a blocking hook, an
   * error result, a stream that ends, the silence bound — has to hold the
   * signal to reach its own subject at all, and this is how.
   */
  holdModels(): void {
    this.modelsGate = new Promise<void>((resolve) => {
      this.releaseModelsGate = resolve;
    });
  }

  /** Let a held {@link holdModels} answer. A no-op when nothing is held. */
  releaseModels(): void {
    const release = this.releaseModelsGate;
    this.releaseModelsGate = undefined;
    this.modelsGate = undefined;
    release?.();
  }

  private modelsGate: Promise<void> | undefined;
  private releaseModelsGate: (() => void) | undefined;

  async supportedModels(): Promise<ModelInfoLike[]> {
    this.calls.push("supportedModels");
    if (this.modelsGate !== undefined) await this.modelsGate;
    return this.models;
  }
  supportedCommands(): Promise<SlashCommandLike[]> {
    return Promise.resolve([]);
  }
  supportedAgents(): Promise<AgentInfoLike[]> {
    return Promise.resolve([]);
  }
  mcpServerStatus(): Promise<McpServerStatusLike[]> {
    this.calls.push("mcpServerStatus");
    return Promise.resolve(this.mcp);
  }
  getContextUsage(): Promise<ContextUsageLike> {
    this.calls.push("getContextUsage");
    return Promise.resolve(this.contextUsage);
  }
  usage_EXPERIMENTAL_MAY_CHANGE_DO_NOT_RELY_ON_THIS_API_YET(): Promise<AccountUsageLike> {
    this.calls.push("usage");
    return Promise.resolve(this.accountUsage);
  }
  accountInfo(): Promise<AccountInfoLike> {
    return Promise.resolve({} as AccountInfoLike);
  }
  initializationResult(): Promise<InitializationResultLike> {
    return Promise.resolve({} as InitializationResultLike);
  }
  stopTask(taskId: string): Promise<void> {
    this.calls.push(`stopTask:${taskId}`);
    this.stoppedTasks.push(taskId);
    return Promise.resolve();
  }
  backgroundTasks(toolUseId?: string): Promise<boolean> {
    this.calls.push(`backgroundTasks:${toolUseId ?? ""}`);
    return Promise.resolve(this.backgroundTaskAnswer);
  }
  streamInput(): Promise<void> {
    return Promise.resolve();
  }
  close(): void {
    this.calls.push("close");
    if (this.closeLeavesStreamOpen) return;
    this.end();
  }
}

/** The vendor's `system:init`, with only the fields the engine reads varied. */
export function initMessage(overrides: Partial<{ sessionId: string; model: string; version: string }> = {}): SdkMessage {
  return {
    type: "system",
    subtype: "init",
    apiKeySource: "user",
    claude_code_version: overrides.version ?? "2.1.999",
    cwd: "/workspace",
    tools: [],
    mcp_servers: [],
    model: overrides.model ?? "claude-opus-5",
    permissionMode: "default",
    slash_commands: [],
    output_style: "default",
    skills: [],
    plugins: [],
    uuid: "11111111-1111-4111-8111-111111111111",
    session_id: overrides.sessionId ?? "session-1",
  };
}

/**
 * A `hook_response`, as the vendor spells one.
 *
 * Defaults to the grounded shape: a `SessionStart:resume` firing on a resume,
 * which is the one that can land BEFORE the vendor's init.
 */
export function hookResponse(
  overrides: Partial<{
    hook_id: string;
    hook_name: string;
    hook_event: string;
    output: string;
    stdout: string;
    stderr: string;
    exit_code: number;
    outcome: "success" | "error" | "cancelled";
    uuid: string;
  }> = {},
): SdkMessage {
  return {
    type: "system",
    subtype: "hook_response",
    hook_id: overrides.hook_id ?? "hook-1",
    hook_name: overrides.hook_name ?? "SessionStart:resume",
    hook_event: overrides.hook_event ?? "SessionStart",
    output: overrides.output ?? "",
    stdout: overrides.stdout ?? "",
    stderr: overrides.stderr ?? "",
    exit_code: overrides.exit_code ?? 0,
    outcome: overrides.outcome ?? "success",
    uuid: overrides.uuid ?? "33333333-3333-4333-8333-333333333333",
    session_id: "session-1",
  } as SdkMessage;
}

/** A turn terminal. */
export function resultMessage(uuid = "22222222-2222-4222-8222-222222222222"): SdkMessage {
  return {
    type: "result",
    subtype: "success",
    duration_ms: 1,
    duration_api_ms: 1,
    is_error: false,
    num_turns: 1,
    result: "done",
    stop_reason: null,
    total_cost_usd: 0,
    usage: { input_tokens: 1, output_tokens: 2 },
    modelUsage: {},
    permission_denials: [],
    uuid,
    session_id: "session-1",
  } as unknown as SdkMessage;
}

/**
 * A `result` the vendor marks as an ERROR.
 *
 * The grounded shape of a refused opening: the CLI answers with an error result
 * naming what it refused, and then says nothing further.
 */
export function errorResultMessage(
  overrides: Partial<{ errors: string[]; uuid: string }> = {},
): SdkMessage {
  return {
    type: "result",
    subtype: "error_during_execution",
    duration_ms: 1,
    duration_api_ms: 1,
    is_error: true,
    num_turns: 0,
    errors: overrides.errors ?? ["No conversation found with session ID: session-1"],
    stop_reason: null,
    total_cost_usd: 0,
    usage: { input_tokens: 0, output_tokens: 0 },
    modelUsage: {},
    permission_denials: [],
    uuid: overrides.uuid ?? "44444444-4444-4444-8444-444444444444",
    session_id: "session-1",
  } as unknown as SdkMessage;
}

/** An in-memory record plane that remembers everything it was handed. */
export class RecordingPersistence implements Persistence {
  readonly durable: PersistEntry[] = [];
  readonly buffered: PersistEntry[] = [];
  flushes = 0;
  page: conversationv1.HistoryPage = create(conversationv1.HistoryPageSchema, {
    boundary: { case: "floor", value: create(conversationv1.HistoryFloorSchema, {}) },
  });
  tail: conversationv1.HistoryEntryAt[] = [];
  bashFrames: conversationv1.AgentBash[] = [];
  live: storev1.GetLiveWorkSuccess = create(storev1.GetLiveWorkSuccessSchema, {});
  openError: PersistenceError | undefined;
  /** `openAgentPage` never answers, the way an unreachable store's call does not. */
  openHangs = false;
  /** The opened page's tail stands rather than ending, the way a real tail does. */
  standingTail = false;
  readError: PersistenceError | undefined;
  liveWorkError: PersistenceError | undefined;
  closedPages = 0;

  /** The producer name StartSession handed it, if it did. */
  producer: string | undefined;

  /** Every agent id StartSession declared this shim had MINTED, in order. */
  readonly mintedAgents: string[] = [];

  noteAgentMinted(agentValue: string): void {
    this.mintedAgents.push(agentValue);
  }
  setProducer(originalVendorSessionId: string): void {
    this.producer = originalVendorSessionId;
  }
  clearProducer(): void {
    if (this.wroteUnderProducer) {
      // THE REAL WRITER THROWS HERE, so a caller that stopped asking first would
      // pass against this fake and fail in production.
      throw new Error(
        `the producer ${JSON.stringify(this.producer)} has already written rows and cannot be un-named`,
      );
    }
    this.producer = undefined;
  }
  /** Whether a row has been handed over since the writer was named. */
  wroteUnderProducer = false;
  producerHasWrittenRows(): boolean {
    return this.wroteUnderProducer;
  }
  writeDurable(entries: PersistEntry[]): Promise<void> {
    this.wroteUnderProducer = true;
    this.durable.push(...entries);
    return Promise.resolve();
  }
  /** What `write` throws, the way a row the writer cannot envelope does. */
  writeThrows: Error | undefined;
  write(entries: PersistEntry[]): void {
    if (this.writeThrows !== undefined) throw this.writeThrows;
    this.wroteUnderProducer = true;
    this.buffered.push(...entries);
  }
  /** Every pointer a reading session was asked to conclude through. */
  readonly concludedThrough: string[] = [];
  /** Rows this fake reports as lost, so a stand-down's exit code is testable. */
  lostRows = 0;
  flush(): Promise<FlushOutcome> {
    this.flushes++;
    return Promise.resolve({ lostRows: this.lostRows });
  }
  /** The `known` predicate the caller passed on its last openAgentPage, if any. */
  lastKnownAgent: (() => boolean) | undefined;
  /**
   * Reading sessions opened, and one-shot pages read, counted apart.
   *
   * THE TWO VERBS ARE NOT INTERCHANGEABLE at the store: an open stands a tail
   * and is answered with a watch token, while a one-shot read says `page_only`
   * and is answered with none. A caller that opens where it meant to read
   * leaves the store holding a token nothing will ever spend.
   */
  pagesOpened = 0;
  firstPageReads = 0;
  openAgentPage(
    _agent?: conversationv1.AgentId,
    _pageSize?: number,
    _knownThrough?: conversationv1.HistoryPointer,
    known?: () => boolean,
  ): Promise<AgentPageSession> {
    this.lastKnownAgent = known;
    this.pagesOpened++;
    if (this.openHangs) return new Promise<AgentPageSession>(() => undefined);
    if (this.openError !== undefined) return Promise.reject(this.openError);
    const entries = this.tail;
    const standing = this.standingTail;
    return Promise.resolve({
      page: this.page,
      tail: {
        async *[Symbol.asyncIterator](): AsyncIterator<conversationv1.HistoryEntryAt> {
          for (const entry of entries) yield entry;
          if (standing) await new Promise<void>(() => undefined);
        },
      },
      concludeThrough: (through) => {
        this.concludedThrough.push(through?.value ?? "");
      },
      close: () => {
        this.closedPages++;
      },
    });
  }
  /**
   * The one-shot read, at the seam the engine calls it through.
   *
   * The refusal-to-empty-page conversion this fake does NOT do lives in the
   * real reader, over the store client: at this seam a refusal is a refusal,
   * which is exactly what the engine's arm mapping is tested against.
   */
  readFirstPage(
    _agent?: conversationv1.AgentId,
    _pageSize?: number,
    _knownThrough?: conversationv1.HistoryPointer,
    known?: () => boolean,
  ): Promise<conversationv1.HistoryPage> {
    this.lastKnownAgent = known;
    this.firstPageReads++;
    if (this.openHangs) return new Promise<conversationv1.HistoryPage>(() => undefined);
    if (this.openError !== undefined) return Promise.reject(this.openError);
    this.closedPages++;
    return Promise.resolve(this.page);
  }
  /**
   * The older pages `readAgentPage` serves, oldest last, one per call; when
   * none are queued it serves `page`, as it always did.
   */
  olderPages: conversationv1.HistoryPage[] = [];
  /** Every pointer an older-page read walked down from, in order. */
  readonly olderPageAfter: string[] = [];
  readAgentPage(
    _agent?: conversationv1.AgentId,
    _pageSize?: number,
    after?: conversationv1.HistoryPointer,
  ): Promise<conversationv1.HistoryPage> {
    if (this.readError !== undefined) return Promise.reject(this.readError);
    this.olderPageAfter.push(after?.value ?? "");
    return Promise.resolve(this.olderPages.shift() ?? this.page);
  }
  /** The session every live-work read was scoped to, in order. */
  readonly liveWorkSessions: string[] = [];
  liveWork(session: conversationv1.AgentId): Promise<storev1.GetLiveWorkSuccess> {
    this.liveWorkSessions.push(session.value);
    if (this.liveWorkError !== undefined) return Promise.reject(this.liveWorkError);
    return Promise.resolve(this.live);
  }
  /** The standing predicate the caller passed on its last openBashRun, if any. */
  lastAnnouncement: (() => BashRunStanding) | undefined;
  openBashRun(
    _work?: conversationv1.DetachedWorkId,
    announcement?: () => BashRunStanding,
  ): Promise<AsyncIterable<conversationv1.AgentBash>> {
    this.lastAnnouncement = announcement;
    const frames = this.bashFrames;
    return Promise.resolve({
      async *[Symbol.asyncIterator](): AsyncIterator<conversationv1.AgentBash> {
        for (const frame of frames) yield frame;
      },
    });
  }
  private readonly faultListeners: ((fault: conversationv1.SessionFault) => void)[] = [];
  private readonly windowListeners: ((w: conversationv1.SessionDegradedWindow) => void)[] = [];
  onFault(listener: (fault: conversationv1.SessionFault) => void): () => void {
    this.faultListeners.push(listener);
    return () => undefined;
  }
  onDegradedWindow(listener: (window: conversationv1.SessionDegradedWindow) => void): () => void {
    this.windowListeners.push(listener);
    return () => undefined;
  }
  /** Raise a record-plane fault, the way the writer does on a store outage. */
  raiseFault(detail: string): void {
    const fault = create(conversationv1.SessionFaultSchema, {
      component: "store-writer",
      detail,
      kind: {
        case: "storeUnreachable",
        value: create(conversationv1.SessionFaultStoreUnreachableSchema, {}),
      },
    });
    for (const listener of this.faultListeners) listener(fault);
  }
  /** Open a record-plane degraded window, the way the writer does. */
  raiseDegradedWindow(reason: string): void {
    const window = create(conversationv1.SessionDegradedWindowSchema, {
      component: "store-writer",
      reason,
      beganAtMs: 1n,
      extent: { case: "open", value: create(conversationv1.SessionDegradedOpenSchema, {}) },
    });
    for (const listener of this.windowListeners) listener(window);
  }
  /**
   * Close that window, the way the writer does when the store answers again.
   *
   * The writer announces the SAME window twice — open, then closed — and the
   * closed announcement is the record plane's only statement that it recovered.
   */
  closeDegradedWindow(reason: string, droppedCount: number): void {
    const window = create(conversationv1.SessionDegradedWindowSchema, {
      component: "store-writer",
      reason,
      beganAtMs: 1n,
      extent: {
        case: "closed",
        value: create(conversationv1.SessionDegradedClosedSchema, {
          endedAtMs: 2n,
          droppedCount: BigInt(droppedCount),
        }),
      },
    });
    for (const listener of this.windowListeners) listener(window);
  }
}

/** A fold that converts nothing and ends a turn on `result`. */
export class RecordingFold implements EngineFold {
  readonly seen: SdkMessage[] = [];
  readonly contexts: FoldContext[] = [];
  entriesFor: (message: SdkMessage) => PersistEntry[] = () => [];
  /** When it answers a detail, the fold REFUSED this message and says so. */
  faultFor: (message: SdkMessage) => string | undefined = () => undefined;

  onSdkMessage(message: SdkMessage, context: FoldContext): EngineFoldOutput {
    this.seen.push(message);
    this.contexts.push(context);
    const detail = this.faultFor(message);
    if (detail !== undefined) {
      context.reportFault?.("converter_defect", detail);
      return { entries: [] };
    }
    const entries = this.entriesFor(message);
    return message.type === "result"
      ? {
          entries,
          turnEnded: {
            frame: create(conversationv1.AgentFrameSchema, { agentId: context.mainAgentId }),
          },
        }
      : { entries };
  }
}

/** A scheduler that never schedules; the suite fires the beat itself. */
export class ManualScheduler implements KeepaliveScheduler {
  readonly handlers: (() => void)[] = [];
  /** The interval each handler was scheduled at, in the same order. */
  readonly intervals: number[] = [];
  cleared = 0;
  setInterval(handler: () => void, intervalMs: number): unknown {
    this.handlers.push(handler);
    this.intervals.push(intervalMs);
    return this.handlers.length - 1;
  }
  clearInterval(): void {
    this.cleared++;
  }
  /** Fire the nth registered interval once. */
  fire(index = 0): void {
    this.handlers[index]?.();
  }
}

function emptyContextUsage(): ContextUsageLike {
  return {
    categories: [],
    totalTokens: 0,
    maxTokens: 0,
    rawMaxTokens: 0,
    percentage: 0,
    gridRows: [],
    model: "",
    memoryFiles: [],
    mcpTools: [],
    agents: [],
    isAutoCompactEnabled: false,
    apiUsage: null,
  };
}

function emptyAccountUsage(): AccountUsageLike {
  return {
    session: {
      total_cost_usd: 0,
      total_api_duration_ms: 0,
      total_duration_ms: 0,
      total_lines_added: 0,
      total_lines_removed: 0,
      model_usage: {},
    },
    subscription_type: null,
    rate_limits_available: false,
    rate_limits: null,
    behaviors: null,
  };
}
