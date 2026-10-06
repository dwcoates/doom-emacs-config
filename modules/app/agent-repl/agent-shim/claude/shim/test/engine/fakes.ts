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
  AppliedSettingsLike,
  ContextUsageLike,
  EffortLevelLike,
  InitializationResultLike,
  InterruptReceipt,
  McpServerStatusLike,
  ModelInfoLike,
  PermissionModeLike,
  SdkMessage,
  SlashCommandLike,
  AgentInfoLike,
  QueryLike,
  RewindFilesResultLike,
} from "../../src/sdk/types.js";
import type {
  AgentOpening,
  AgentPageSession,
  AgentTailFrame,
  FlushOutcome,
  PersistEntry,
  Persistence,
} from "../../src/store/persistence.js";
import { PersistenceError, REPAINT } from "../../src/store/persistence.js";
import type { DetachedWorkAnswer } from "../../src/store/detached-work.js";
import type { VendorTaskAnswer } from "../../src/store/locator.js";
import type { TaskAgentKnowledge, TaskAwaitingKind } from "../../src/convert/detached.js";
import type { EngineFold, EngineFoldOutput, FoldContext } from "../../src/engine/fold-context.js";
import type { KeepaliveScheduler } from "../../src/engine/keepalive.js";
import type { ReachabilityProbe } from "../../src/engine/network-resume.js";

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
  /** When set, `interrupt` rejects with it, as a vendor that refuses the interrupt does. */
  interruptRejects: Error | undefined;
  setModelRejects: Error | undefined;
  applyFlagSettingsRejects: Error | undefined;
  /** What `getSettings` answers for `applied.effort`; null is "no level". */
  appliedEffort: EffortLevelLike | null = null;
  /** When true, `applyFlagSettings` leaves `appliedEffort` as the test set it. */
  appliedEffortPinned = false;
  /** When set, `getSettings` rejects with it. */
  getSettingsRejects: Error | undefined;
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
    if (this.interruptRejects !== undefined) return Promise.reject(this.interruptRejects);
    return Promise.resolve({ still_queued: [] });
  }
  setPermissionMode(mode: PermissionModeLike): Promise<void> {
    this.calls.push(`setPermissionMode:${mode}`);
    return this.setPermissionModeRejects === undefined
      ? Promise.resolve()
      : Promise.reject(this.setPermissionModeRejects);
  }
  applyFlagSettings(settings: { effortLevel: EffortLevelLike }): Promise<void> {
    this.calls.push(`applyFlagSettings:effortLevel=${settings.effortLevel}`);
    if (this.applyFlagSettingsRejects !== undefined) return Promise.reject(this.applyFlagSettingsRejects);
    // The vendor then states the level it applied, unless a test pinned what
    // it states (an override, a downgrade).
    if (!this.appliedEffortPinned) this.appliedEffort = settings.effortLevel;
    return Promise.resolve();
  }
  getSettings(): Promise<AppliedSettingsLike> {
    this.calls.push("getSettings");
    return this.getSettingsRejects === undefined
      ? Promise.resolve({ applied: { effort: this.appliedEffort } })
      : Promise.reject(this.getSettingsRejects);
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
  /** What `rewindFiles` answers a dry run, and a real rewind. */
  rewindDryRun: RewindFilesResultLike = { canRewind: true, filesChanged: [] };
  rewindReal: RewindFilesResultLike = { canRewind: true, filesChanged: [] };
  rewindFiles(userMessageId: string, options?: { dryRun?: boolean }): Promise<RewindFilesResultLike> {
    const dryRun = options?.dryRun === true;
    this.calls.push(`rewindFiles:${userMessageId}:${dryRun ? "dry" : "real"}`);
    return Promise.resolve(dryRun ? this.rewindDryRun : this.rewindReal);
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
  tail: AgentTailFrame[] = [];
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
  /** What `writeDurable` rejects with, the way an unreachable store does. */
  writeDurableRejects: Error | undefined;
  writeDurable(entries: PersistEntry[]): Promise<void> {
    this.wroteUnderProducer = true;
    this.durable.push(...entries);
    this.bashRunCalls.push(...entries.map((entry) => `durable:${entry.upsertKey}`));
    if (this.writeDurableRejects !== undefined) return Promise.reject(this.writeDurableRejects);
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
  /** How many times the vendor loop waited on the writer's backpressure. */
  writableWaits = 0;
  /** Set to hold `whenWritable` until the test releases it, the way a backlog does. */
  backlog: Promise<void> | undefined;
  whenWritable(): Promise<void> {
    this.writableWaits++;
    return this.backlog ?? Promise.resolve();
  }
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
  /** Head-only reads, counted apart from page reads: they read no line. */
  bookHeadReads = 0;
  /** Every opening an open or a one-shot read was asked for, in call order. */
  readonly openings: AgentOpening[] = [];
  /**
   * What an open reports as `foundNothing`; unset, it is whether `page` is
   * empty — the answer for every opening but a tail-only one.
   */
  foundNothing: boolean | undefined;
  openAgentPage(
    _agent?: conversationv1.AgentId,
    opening?: AgentOpening,
    known?: () => boolean,
  ): Promise<AgentPageSession> {
    this.lastKnownAgent = known;
    this.pagesOpened++;
    this.openings.push(opening ?? REPAINT);
    if (this.openHangs) return new Promise<AgentPageSession>(() => undefined);
    if (this.openError !== undefined) return Promise.reject(this.openError);
    const entries = this.tail;
    const standing = this.standingTail;
    return Promise.resolve({
      page: this.page,
      foundNothing: this.foundNothing ?? this.page.entries.length === 0,
      tail: {
        async *[Symbol.asyncIterator](): AsyncIterator<AgentTailFrame> {
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
  /** The agent every one-shot first-page read named, in call order. */
  readonly firstPageAgents: string[] = [];
  readFirstPage(
    agent?: conversationv1.AgentId,
    opening?: AgentOpening,
    known?: () => boolean,
  ): Promise<conversationv1.HistoryPage> {
    this.lastKnownAgent = known;
    this.firstPageReads++;
    this.firstPageAgents.push(agent?.value ?? "");
    this.openings.push(opening ?? REPAINT);
    if (this.openHangs) return new Promise<conversationv1.HistoryPage>(() => undefined);
    if (this.openError !== undefined) return Promise.reject(this.openError);
    this.closedPages++;
    return Promise.resolve(this.page);
  }
  /**
   * The one-shot HEAD read: the newest pointer of `page`, exactly what the
   * store's place index answers for the book `page` stands for.
   */
  readBookHead(
    _agent?: conversationv1.AgentId,
    known?: () => boolean,
  ): Promise<conversationv1.HistoryPointer | undefined> {
    this.lastKnownAgent = known;
    this.bookHeadReads++;
    if (this.openHangs) return new Promise<conversationv1.HistoryPointer | undefined>(() => undefined);
    if (this.openError !== undefined) return Promise.reject(this.openError);
    return Promise.resolve(this.page.entries[0]?.at);
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
    after?: conversationv1.HistoryPointer,
  ): Promise<conversationv1.HistoryPage> {
    if (this.readError !== undefined) return Promise.reject(this.readError);
    this.olderPageAfter.push(after?.value ?? "");
    return Promise.resolve(this.olderPages.shift() ?? this.page);
  }
  /** Every bound a through read named, in order. */
  readonly throughBounds: bigint[] = [];
  readPageThrough(
    _agent?: conversationv1.AgentId,
    through?: conversationv1.ConversationThrough,
  ): Promise<conversationv1.HistoryPage> {
    if (this.readError !== undefined) return Promise.reject(this.readError);
    this.throughBounds.push(through?.atMs ?? 0n);
    return Promise.resolve(this.page);
  }
  /** The session every live-work read was scoped to, in order. */
  readonly liveWorkSessions: string[] = [];
  liveWork(session: conversationv1.AgentId): Promise<storev1.GetLiveWorkSuccess> {
    this.liveWorkSessions.push(session.value);
    if (this.liveWorkError !== undefined) return Promise.reject(this.liveWorkError);
    return Promise.resolve(this.live);
  }
  /**
   * The store's answer per vendor task locator; an unlisted locator answers
   * `not_found`, which is what a store that never heard of it says.
   */
  readonly vendorTasks = new Map<string, VendorTaskAnswer>();
  /** Every locator lookup, as `<session>/<locator>`, in order. */
  readonly vendorTaskLookups: string[] = [];
  agentByVendorTask(session: conversationv1.AgentId, vendorTaskId: string): Promise<VendorTaskAnswer> {
    this.vendorTaskLookups.push(`${session.value}/${vendorTaskId}`);
    return Promise.resolve(this.vendorTasks.get(vendorTaskId) ?? { kind: "not_found" });
  }
  /**
   * The store's answer per unit, by its activity id; an unlisted unit answers
   * `not_found`, which is what a store that never heard of it says.
   */
  readonly detachedWorks = new Map<string, DetachedWorkAnswer>();
  /** Every detached-work lookup, by unit, in order. */
  readonly detachedWorkLookups: string[] = [];
  detachedWork(unit: conversationv1.AgentActivityId): Promise<DetachedWorkAnswer> {
    this.detachedWorkLookups.push(unit.value);
    return Promise.resolve(this.detachedWorks.get(unit.value) ?? { kind: "not_found" });
  }
  /**
   * The durable writes and shell-run opens, in the order they were made — so a
   * suite can say the start was made durable BEFORE the run was opened.
   */
  readonly bashRunCalls: string[] = [];
  /** Whether each shell-run open asked the store to wait for a first row. */
  readonly bashRunAwaits: boolean[] = [];
  openBashRun(
    work?: conversationv1.DetachedWorkId,
    options?: { readonly awaitFirstRow: boolean },
  ): Promise<AsyncIterable<conversationv1.AgentBash>> {
    this.bashRunCalls.push(`open:${work?.value ?? ""}`);
    this.bashRunAwaits.push(options?.awaitFirstRow ?? false);
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
  /** Every `endQuery` the engine called, by its stated reason. */
  readonly queryEnds: string[] = [];
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

  endQuery(why: string): void {
    this.queryEnds.push(why);
  }

  /** Every absorbed turn the engine concluded, by the turn its context named and its coordinate. */
  readonly absorbed: { turn: string; coordinate: string }[] = [];

  concludeAbsorbedTurn(context: FoldContext, coordinate: string): EngineFoldOutput {
    this.absorbed.push({ turn: context.turnId?.value ?? "", coordinate });
    return { entries: [], turnEnded: { frame: create(conversationv1.AgentFrameSchema, { agentId: context.mainAgentId }) } };
  }

  /** The task a message awaits the store for; none unless a suite says so. */
  awaitingFor: (message: SdkMessage) => string | undefined = () => undefined;
  /** Every store answer the engine handed back, in order. */
  readonly learned: { taskId: string; answer: VendorTaskAnswer }[] = [];
  /** What the fold knows per task; a found answer is remembered here, as the real fold does. */
  readonly knowledge = new Map<string, TaskAgentKnowledge>();

  taskAwaitingAgent(message: SdkMessage): string | undefined {
    return this.awaitingFor(message);
  }

  /** The task whose kind a message awaits the store for; none unless a suite says so. */
  awaitingKindFor: (message: SdkMessage) => TaskAwaitingKind | undefined = () => undefined;
  /** Every kind answer the engine handed back, in order. */
  readonly learnedKinds: { taskId: string; answer: DetachedWorkAnswer }[] = [];

  taskAwaitingKind(message: SdkMessage): TaskAwaitingKind | undefined {
    return this.awaitingKindFor(message);
  }

  learnTaskKind(taskId: string, answer: DetachedWorkAnswer): void {
    this.learnedKinds.push({ taskId, answer });
  }

  /** Every unit the engine said it asked the vendor to move, in order. */
  readonly userDetaches: string[] = [];
  /** Every request the engine retired, with why, in order. */
  readonly retiredUserDetaches: { toolUseId: string; why: string }[] = [];

  noteUserDetach(toolUseId: string): void {
    this.userDetaches.push(toolUseId);
  }

  retireUserDetach(toolUseId: string, why: string): void {
    this.retiredUserDetaches.push({ toolUseId, why });
  }

  learnTaskAgent(taskId: string, answer: VendorTaskAnswer): void {
    this.learned.push({ taskId, answer });
    if (answer.kind === "found") this.knowledge.set(taskId, { kind: "named", agent: answer.agent });
  }

  taskAgent(taskId: string): TaskAgentKnowledge {
    return this.knowledge.get(taskId) ?? { kind: "unknown" };
  }
}

/**
 * A reachability probe answering whatever the suite last set. It touches no
 * network, and counts how often it was asked.
 */
export class ScriptedProbe {
  reachable = true;
  calls = 0;
  readonly probe: ReachabilityProbe = () => {
    this.calls++;
    return Promise.resolve({ reachable: this.reachable, detail: "scripted" });
  };
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
