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

/** A query whose message stream a suite pushes into, one message at a time. */
export class ScriptedQuery implements QueryLike {
  private readonly queued: SdkMessage[] = [];
  private waiting: ((value: IteratorResult<SdkMessage>) => void) | undefined;
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

  emit(message: SdkMessage): void {
    if (this.waiting !== undefined) {
      const resolve = this.waiting;
      this.waiting = undefined;
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
      resolve({ value: undefined, done: true });
    }
  }

  /** End the stream by throwing out of the iterator. */
  fail(error: Error): void {
    this.failure = error;
    this.end();
  }

  [Symbol.asyncIterator](): AsyncIterator<SdkMessage> {
    return {
      next: (): Promise<IteratorResult<SdkMessage>> => {
        const next = this.queued.shift();
        if (next !== undefined) return Promise.resolve({ value: next, done: false });
        if (this.failure !== undefined) return Promise.reject(this.failure);
        if (this.ended) return Promise.resolve({ value: undefined, done: true });
        return new Promise((resolve) => {
          this.waiting = resolve;
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
  supportedModels(): Promise<ModelInfoLike[]> {
    this.calls.push("supportedModels");
    return Promise.resolve(this.models);
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
  readError: PersistenceError | undefined;
  liveWorkError: PersistenceError | undefined;
  closedPages = 0;

  /** The producer name StartSession handed it, if it did. */
  producer: string | undefined;

  setProducer(originalVendorSessionId: string): void {
    this.producer = originalVendorSessionId;
  }
  clearProducer(): void {
    this.producer = undefined;
  }
  writeDurable(entries: PersistEntry[]): Promise<void> {
    this.durable.push(...entries);
    return Promise.resolve();
  }
  write(entries: PersistEntry[]): void {
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
  openAgentPage(
    _agent?: conversationv1.AgentId,
    _pageSize?: number,
    _knownThrough?: conversationv1.HistoryPointer,
    known?: () => boolean,
  ): Promise<AgentPageSession> {
    this.lastKnownAgent = known;
    if (this.openError !== undefined) return Promise.reject(this.openError);
    const entries = this.tail;
    const self = this;
    return Promise.resolve({
      page: this.page,
      tail: {
        async *[Symbol.asyncIterator](): AsyncIterator<conversationv1.HistoryEntryAt> {
          for (const entry of entries) yield entry;
        },
      },
      concludeThrough: (through) => {
        self.concludedThrough.push(through?.value ?? "");
      },
      close: () => {
        self.closedPages++;
      },
    });
  }
  readAgentPage(): Promise<conversationv1.HistoryPage> {
    if (this.readError !== undefined) return Promise.reject(this.readError);
    return Promise.resolve(this.page);
  }
  liveWork(): Promise<storev1.GetLiveWorkSuccess> {
    if (this.liveWorkError !== undefined) return Promise.reject(this.liveWorkError);
    return Promise.resolve(this.live);
  }
  /** The `stillLive` predicate the caller passed on its last openBashRun, if any. */
  lastStillLive: (() => boolean) | undefined;
  openBashRun(
    _work?: conversationv1.DetachedWorkId,
    stillLive?: () => boolean,
  ): Promise<AsyncIterable<conversationv1.AgentBash>> {
    this.lastStillLive = stillLive;
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
