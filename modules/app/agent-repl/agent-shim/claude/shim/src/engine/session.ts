/**
 * engine/session.ts — THE session engine.
 *
 * RESPONSIBILITY. The one implementation of {@link Engine}: it owns the single
 * vendor query, drives `StartSession` fresh and resume, applies
 * `SetSessionModel` at a turn boundary and `SetSessionPermissionMode`
 * immediately, performs `Hibernate` and `KillSession`, fans WatchSession out,
 * and carries the SIGTERM stand-down.
 *
 * THE SESSION LOCK IS TAKEN HERE, NOT IN main.ts. `main.ts` takes the WORKSPACE
 * lock at startup because a workspace is knowable from argv; the session lock
 * is keyed by the VENDOR SESSION ID, which does not exist until StartSession
 * pre-mints it (fresh) or is handed it (resume). It is taken BEFORE the SDK is
 * touched — a shim that started a query and then discovered another shim owned
 * the conversation would already have two writers on one transcript — and held
 * for the process lifetime.
 *
 * STATELESSNESS. The engine accumulates NOTHING of variable size. History is
 * served from the store, never from memory; the joins it keeps are constant
 * (the pending permission callbacks, the live task table, spawn provenance).
 *
 * TEARDOWN ORDER, WHICH IS NOT NEGOTIABLE: resolve every pending permission
 * callback as denied (an unresolved one wedges the vendor process), then stop
 * the live work, then end the query, then wait for every store write to be
 * acked, then exit. Ending the query first would abandon writes the record
 * needs.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog, clearRequestId, onLogSinkPoisoned, setClaudeSessionId, setRequestId } from "../log.js";
import { conversationv1, shimv1 } from "../proto.js";
import { acquireSessionLock, acquireWorkspaceLock, workspaceLockPath } from "../locks.js";
import type { LockRelease } from "../locks.js";
import { workspaceLockKey } from "../locks.js";
import { recordAgentBinaryVersion, requireSessionRuntime } from "../build-identity.js";
import { subagentId, toolCallActivityId } from "../convert/ids.js";
import { terminalUpsertKey } from "../store/keys.js";
import type { AgentPageSession, PersistEntry, Persistence } from "../store/persistence.js";
import {
  announceLiveWork,
  closingAgentTerminal,
  closingBashTerminal,
  closingSubagentTerminal,
  findBashStart,
  findUnit,
  stoppedBashTerminal,
} from "../store/reconcile.js";
import type {
  CanUseToolLike,
  PermissionModeLike,
  QueryLike,
  SdkMessage,
  SdkUserMessage,
  ContextUsageLike,
  AccountUsageLike,
  McpServerStatusLike,
  ModelInfoLike,
} from "../sdk/types.js";
import {
  hibernateAcked,
  hibernateRefused,
  killSessionClosed,
  killSessionRefused,
  sessionFault,
  setSessionModelRefused,
  setSessionPermissionModeRefused,
  startSessionRefused,
  startSessionStarted,
} from "../service/failures.js";
import type { Engine } from "./engine.js";
import type { EngineFold, FoldContext, LastChange } from "./fold-context.js";
import { SYNTHETIC_MODEL } from "../model.js";
import { fastModeUpdate } from "../convert/session-updates.js";
import { backupTranscript } from "./backup.js";
import {
  appendCompactionLines,
  compactionLines,
  compactionPrompt,
  contextCutCompacted,
  contextCutFailed,
  readAmbient,
} from "./compaction.js";
import { judgeCold, readTranscriptFacts, sessionCold, transcriptPath, type TranscriptFacts } from "./cold.js";
import { ForegroundUnitTable } from "./foreground.js";
import { LiveWorkTable, type LiveWorkEntry } from "./detached.js";
import {
  createAgentIdentityStore,
  mintVendorSessionId,
  SessionIdentity,
  type AgentIdentityStore,
} from "./identity.js";
import {
  KeepaliveCadence,
  KeepaliveRewind,
  keepalivePromptText,
  REAL_SCHEDULER,
  type KeepaliveScheduler,
} from "./keepalive.js";
import { fromVendorPermissionMode, PermissionGate, toVendorPermissionMode } from "./permission-gate.js";
import { SessionPushes } from "./pushes.js";
import { TurnEngine, promptEntry, buildPrompt, saidText, textSaid, type OpenTurn, type SessionContext } from "./turn.js";

const LOGGER = bindLog({ component: "shim-engine-session", operation: "shim.engine.session" });

/** How the engine asks for a query; the factory behind it is real or fake. */
export interface QuerySpec {
  readonly binding:
    | { readonly kind: "fresh"; readonly sessionId: string }
    | { readonly kind: "resume"; readonly resumeSessionId: string };
  readonly model?: string;
  readonly permissionMode: PermissionModeLike;
  readonly canUseTool: CanUseToolLike;
  readonly abortController: AbortController;
  /** The rewind: resume only THROUGH this record. */
  readonly resumeSessionAt?: string;
  readonly prompt: AsyncIterable<SdkUserMessage>;
}

/** Build one query. `--fake` swaps the implementation and nothing else. */
export type CreateQuery = (spec: QuerySpec) => Promise<QueryLike>;

/** Everything the engine needs that it does not own. */
interface EngineDeps {
  readonly persistence: Persistence;
  readonly fold: EngineFold;
  readonly createQuery: CreateQuery;
  readonly runtime: { shimBuildSha: string; sdkVersion: string };
  readonly env: {
    readonly stateDir: string;
    readonly configDir: string;
    readonly cwd: string;
  };
  readonly nowMs: () => number;
  /** Injected so a suite never waits on a clock. */
  readonly scheduler?: KeepaliveScheduler;
  /** Injected so a suite substitutes a temp directory without a state dir. */
  readonly identityStore?: AgentIdentityStore;
  /** Injected so a suite can take no kernel lock. */
  readonly acquireLock?: (sessionId: string) => LockRelease | Promise<LockRelease>;
  /**
   * Injected so a suite can take no kernel workspace lock.
   *
   * THE WORKSPACE LOCK IS A SESSION CLAIM, NOT A PROCESS ONE (rollout ruling,
   * 2026-09-02). A shim that has been spawned but has no session is INERT: it
   * serves shim.v1 and holds NEITHER lock, so a prelaunched inert shim can sit
   * beside the live one it is about to replace instead of blocking forever on
   * a lock the live shim holds for its lifetime. The claim is still made
   * before the SDK is ever touched, and still held for the process lifetime
   * once a session exists, so the daemon's probe semantics are unchanged: a
   * HELD workspace lock still means a live shim owns the conversation.
   */
  readonly acquireWorkspaceLock?: (cwd: string) => LockRelease | Promise<LockRelease>;
  /** How long StartSession waits for the vendor's own `system:init`. */
  readonly initTimeoutMs?: number;
  /**
   * The keep-alive cadence, when something overrode the module constant.
   *
   * `main.ts` fills this ONLY for a `--fake` process; a real session always
   * beats on {@link KEEPALIVE_INTERVAL_MS}.
   */
  readonly keepaliveIntervalMs?: number;
  /**
   * The teardown's per-tail conclusion budget, when something overrode the
   * module constant.
   *
   * `main.ts` fills this ONLY for a `--fake` process; a real session always
   * bounds on {@link WATCHER_CONCLUSION_BUDGET_MS}, whose own doc states what
   * its size is answerable to. It is a LAST RESORT bound either way — the
   * ordering it protects is unchanged by its size.
   */
  readonly watcherConclusionBudgetMs?: number;
  /**
   * End the process, once the session has been stood down.
   *
   * `KillSession` is a PROCESS-LEVEL verb: the session it ends is the only one
   * this shim will ever serve, so a shim that tore the session down and kept
   * serving would hold its socket, its workspace lock and its vendor account
   * against the next spawn. The engine cannot exit by itself, though — the
   * response has to reach the daemon first — so it hands the decision, and the
   * exit CODE, to `main.ts`, which ends the process once the wire is quiet.
   *
   * Unset in a suite that drives the engine directly, where the process is the
   * test runner and must survive.
   */
  readonly endProcess?: (exitCode: number) => void;
}

/**
 * One open `WatchAgent` tail, held so the teardown can conclude it.
 */
interface OpenWatcher {
  readonly agent: conversationv1.AgentId;
  readonly page: AgentPageSession;
  /** Resolves when the handler's stream has finished, however it finished. */
  readonly ended: Promise<void>;
}

/** One open `WatchBash` stream, held so the teardown can wait for its terminal. */
interface OpenBashWatcher {
  readonly work: conversationv1.DetachedWorkId;
  /** Resolves when the handler's stream has finished, however it finished. */
  readonly ended: Promise<void>;
}

/**
 * How long the teardown waits for ONE of its bounded stages to finish.
 *
 * A LAST RESORT: a tail ends on its own the moment it has served the book's
 * head, and the vendor's message loop ends the moment its query is closed.
 * This only bounds a party that stopped answering, so it cannot keep a killed
 * shim alive forever.
 *
 * IT IS SIZED AGAINST THE DAEMON'S STAND BOUND, NOT AGAINST ITSELF. The whole
 * teardown runs INSIDE the daemon's `KillSession` call, which
 * `drain.DefaultStandBound` (5s) gives up on; a per-stage budget large enough
 * that the daemon's bound fires first would mean the shim's own last resort
 * can never be reached, and the daemon would report a shim as leaked while it
 * was still legitimately working. The teardown spends at most FOUR of these
 * back to back -- the message loop's end, then, per agent, the book-head read
 * and the tail's own end, then the bash tails -- so the worst case must stay
 * strictly under the daemon's bound: 4 x 1s = 4s, one second inside it.
 *
 * MEASURED: a forced `KillSession` on a session parked at an OPEN permission
 * ask, with a `WatchAgent` tail standing, concluded in 9ms in the shim's own
 * integration harness, and the whole daemon-side stop it sits inside measured
 * 9ms p50 / 15ms max across 104 e2e runs at -parallel 8 and -parallel 32. One
 * second is a hundred times that, so nothing healthy can reach this bound.
 */
const WATCHER_CONCLUSION_BUDGET_MS = 1_000;

/** The component name the log sink's own fault and degraded window carry. */
const LOG_SINK_COMPONENT = "log-sink";

/** How often the account's rate-limit windows are sampled. */
const ACCOUNT_USAGE_INTERVAL_MS = 5 * 60 * 1000;

/**
 * A push-style bridge onto the SDK's pull-style streaming input.
 *
 * The SDK wants an `AsyncIterable` it pulls from; the engine has prompts that
 * arrive when a consumer sends them. This is the ONE submitter — everything
 * that delivers a prompt goes through it, which is what makes "one turn in
 * flight" enforceable rather than merely intended.
 */
class PromptQueue implements AsyncIterable<SdkUserMessage> {
  private readonly queued: SdkUserMessage[] = [];
  private waiting: ((value: IteratorResult<SdkUserMessage>) => void) | undefined;
  private done = false;

  push(message: SdkUserMessage): void {
    if (this.done) throw new Error("shim session: a prompt was submitted after the query was closed");
    if (this.waiting !== undefined) {
      const resolve = this.waiting;
      this.waiting = undefined;
      resolve({ value: message, done: false });
      return;
    }
    this.queued.push(message);
  }

  close(): void {
    this.done = true;
    if (this.waiting !== undefined) {
      const resolve = this.waiting;
      this.waiting = undefined;
      resolve({ value: undefined, done: true });
    }
  }

  [Symbol.asyncIterator](): AsyncIterator<SdkUserMessage> {
    return {
      next: (): Promise<IteratorResult<SdkUserMessage>> => {
        const next = this.queued.shift();
        if (next !== undefined) return Promise.resolve({ value: next, done: false });
        if (this.done) return Promise.resolve({ value: undefined, done: true });
        return new Promise((resolve) => {
          this.waiting = resolve;
        });
      },
    };
  }
}

/** The engine, plus the hooks tests and main.ts drive it with. */
export interface SessionEngine extends Engine {
  /** Feed one SDK message through the whole session path. Used by the loop and by tests. */
  onSdkMessage(message: SdkMessage): Promise<void>;
  /** The WatchSession fan-out, for a caller that needs to observe pushes. */
  readonly pushes: SessionPushes;
}

export function createEngine(deps: EngineDeps): SessionEngine {
  const workspaceKey = workspaceLockKey(deps.env.cwd);
  const identityStore =
    deps.identityStore ?? createAgentIdentityStore(deps.env.stateDir, workspaceKey, deps.nowMs);
  const acquireLock = deps.acquireLock ?? acquireSessionLock;
  const acquireWorkspace = deps.acquireWorkspaceLock ?? acquireWorkspaceLock;
  const pushes = new SessionPushes(deps.nowMs);
  const live = new LiveWorkTable();
  const foreground = new ForegroundUnitTable();
  const rewind = new KeepaliveRewind();

  let identity: SessionIdentity | undefined;
  let releaseLock: LockRelease | undefined;
  let releaseWorkspaceLock: LockRelease | undefined;
  let query: QueryLike | undefined;
  let abort: AbortController | undefined;
  let prompts: PromptQueue | undefined;
  let open: OpenTurn | undefined;
  let effectiveModel = "";
  let permissionMode: conversationv1.AgentPermissionMode = fromVendorPermissionMode("default");
  let modelCatalog: conversationv1.ModelOption[] = [];
  let started = false;
  /**
   * The `SessionStarted` this session announced, kept for re-announcement.
   *
   * UNSET until StartSession succeeds, which is exactly when a watch has
   * nothing to be told.
   */
  let announcedStart: conversationv1.SessionStarted | undefined;
  let standingDown = false;
  /** A model change accepted mid-turn, and the call still waiting on it. */
  let pendingModel:
    | {
        readonly model: conversationv1.AgentModel;
        readonly resolve: (response: shimv1.SetSessionModelResponse) => void;
      }
    | undefined;
  let accountUsageHandle: unknown;
  let cadence: KeepaliveCadence | undefined;
  let initResolve: ((message: SdkMessage) => void) | undefined;
  let loop: Promise<void> | undefined;
  /** Rows the store never acked by the time the teardown finished. */
  let lostRowsAtStandDown = 0;
  /** Every open `WatchAgent` tail, so the teardown can conclude each honestly. */
  const watchers = new Set<OpenWatcher>();
  /** Every open `WatchBash` stream, so the teardown can wait for its terminal. */
  const bashWatchers = new Set<OpenBashWatcher>();
  /**
   * Subagent ids the RECORD named at reconciliation.
   *
   * Two sources, both bounded by the conversation's own shape: the open
   * obligations the record named once at start, and every `created_agent_id`
   * this session has ANNOUNCED. The second is the load-bearing one — a
   * SYNCHRONOUS subagent runs inside the turn and is never a task, so the live
   * table never holds it, yet the daemon opens a WatchAgent on it the instant
   * it sees the spawn, before a single row of the child's exists.
   */
  const announcedAgents = new Set<string>();
  /**
   * The last write or edit this session folded, and which of the two it was.
   *
   * THE IDE-DIAGNOSTICS JOIN. The vendor's `diagnostics` attachment carries no
   * tool id at all, so `convert/attachments.ts` joins it to the change it
   * concerns by ADJACENCY -- and nothing ever assigned this, so every
   * diagnostics record fell to "IDE diagnostics arrived with no preceding write
   * or edit" and landed as residue instead of on the edit it belonged to.
   *
   * ONE remembered value, per the fold context's contract, and it is remembered
   * where every other cross-message observation is: from the entries the fold
   * produced. It carries the KIND as well as the id, because the report lands
   * on that unit's own arm -- a write's findings on `AgentWrite.diagnostics`,
   * an edit's on `AgentEdit.diagnostics` -- and nothing else in the diagnostics
   * record states which.
   */
  let lastChange: LastChange | undefined;
  /**
   * Cuts produced BEFORE the session had an identity to key them to.
   *
   * The cold gate's `compact` remediation runs inside `StartSession`, before
   * the identity is settled — a row cannot be written yet, and dropping it
   * would lose the one page line that makes the compaction visible in the feed
   * rather than only in the transcript. They are written the moment the
   * identity exists, in the order they happened.
   */
  const pendingContextCuts: conversationv1.ContextCut[] = [];

  // THE RECORD PLANE'S FAULTS ARE THE SESSION'S. `Persistence` raises a
  // store_unreachable fault and opens a degraded window when the store stops
  // answering, and nothing subscribing to them meant a store outage was visible
  // only in the shim's own log -- the daemon's diagnostics stayed healthy while
  // the conversation was being lost.
  deps.persistence.onFault((fault) => {
    pushes.fault(fault);
  });
  deps.persistence.onDegradedWindow((window) => {
    pushes.recordDegradedWindow(window);
  });
  // THE SHIM'S OWN LOG DYING IS A SESSION FACT. Once fd 3 is gone the shim has
  // no durable channel left to complain through, so WatchSession is the only
  // place the loss can still be stated -- and it is stated ONCE, as a fault and
  // a window that never closes, because nothing restores a lost record.
  onLogSinkPoisoned((cause) => {
    pushes.fault(sessionFault({ kind: "logSinkPoisoned" }, LOG_SINK_COMPONENT, cause.message));
    pushes.openDegradedWindow(LOG_SINK_COMPONENT, `the durable log sink is poisoned: ${cause.message}`);
  });

  const gate = new PermissionGate({
    mainAgentId: () => requireIdentity().agentId,
    // The SAME set the daemon's own WatchAgent is answered from: an agent this
    // session announced is addressable, and nothing else is.
    agentFor: (vendorAgentId) => {
      const main = requireIdentity().agentId;
      if (vendorAgentId === main.value) return main;
      if (announcedAgents.has(vendorAgentId)) return subagentId(vendorAgentId);
      // THE LIVE REGISTRY IS THE OTHER ANNOUNCEMENT, AND IT IS KEYED THE WAY
      // THE VENDOR SPELLS THE ASK. `canUseTool`'s `agentID` is the agent TASK
      // id -- `task_started.task_id` for the `local_agent` task, verbatim
      // (testdata/captures/ctrl-b-detach-of-foreground-subagent: `agentID`
      // "a2c1930ea977473d4" is that task's id, and the subagent's own
      // transcript is `subagents/agent-a2c1930ea977473d4.jsonl`). What this
      // session ANNOUNCED under, however, is the spawning call's
      // `tool_use_id`, because that is the only agent identity the stream
      // plane carries (convert/fold-context.ts subagentBook). The two are
      // different strings for the same subagent, so an ask raised inside a
      // subagent resolved to nothing and landed on the main agent's book.
      //
      // `task_started` states BOTH ids in one message, which is exactly the
      // join the live table already holds -- so the task id is translated to
      // the announced book through it, never guessed.
      const byTask = live.get(vendorAgentId);
      if (byTask?.toolUseId !== undefined && byTask.toolUseId !== "") {
        return subagentId(byTask.toolUseId);
      }
      // A vendor that names the spawning call directly is taken at its word.
      //
      // Only the LIVE set resolves: once the subagent has concluded there is no
      // book still taking questions, and the ask falls back to the main agent
      // with the log note, which is the contract for an unaddressable ask.
      if (live.byToolUseId(vendorAgentId) !== undefined) return subagentId(vendorAgentId);
      return undefined;
    },
    persist: (entries) => deps.persistence.write(entries),
    keepalive: () => open?.keepalive === true,
    nowMs: deps.nowMs,
    onPermissionModeSet: (mode) => {
      permissionMode = mode;
      pushPermissionMode();
    },
  });

  function requireIdentity(): SessionIdentity {
    if (identity === undefined) {
      throw new Error("shim session: the main agent identity is not established yet");
    }
    return identity;
  }

  function foldContext(): FoldContext {
    return {
      mainAgentId: requireIdentity().agentId,
      ...(open === undefined ? {} : { turnId: open.id }),
      keepalive: open?.keepalive === true,
      nowMs: deps.nowMs,
      ...(lastChange === undefined ? {} : { lastChange }),
      pendingAsk: (toolUseId) => gate.pendingAsk(toolUseId),
      deniedCall: (toolUseId) => gate.deniedCall(toolUseId),
      reportFault: (_kind, detail) => {
        noteConverterDefect(detail);
      },
      liveTask: (taskId) => {
        const entry = live.get(taskId);
        if (entry === undefined) return undefined;
        return entry.toolUseId === undefined ? { toolUseId: "" } : { toolUseId: entry.toolUseId };
      },
    };
  }

  // -- the converter's own health -------------------------------------------

  /** Which component a converter defect is reported against, on both sides. */
  const CONVERTER_COMPONENT = "converter";

  /** Open while the converter is refusing messages; counts what it refused. */
  let converterDegraded: { droppedCount: number } | undefined;
  /** Set by the fault channel during ONE fold call, cleared before the next. */
  let converterDefectThisMessage = false;
  /** Set by any refusal within the turn now open; cleared when that turn ends. */
  let converterDefectThisTurn = false;

  /**
   * The fold refused a vendor message.
   *
   * The fault is standing (the session is unhealthy) and the window is OPEN
   * until a message converts cleanly. Every refusal counts toward the window's
   * `dropped_count`, which is the only place the number of lost records is
   * ever stated.
   */
  function noteConverterDefect(detail: string): void {
    converterDefectThisMessage = true;
    converterDefectThisTurn = true;
    if (converterDegraded === undefined) {
      converterDegraded = { droppedCount: 0 };
      pushes.openDegradedWindow(
        CONVERTER_COMPONENT,
        "the fold refused a vendor message it should have modelled",
      );
    }
    converterDegraded.droppedCount += 1;
    LOGGER.log(
      { level: "error", component: CONVERTER_COMPONENT, detail, dropped_count: converterDegraded.droppedCount },
      "the converter refused a vendor message; the session is degraded until one converts",
    );
    pushes.fault(sessionFault({ kind: "converterDefect" }, CONVERTER_COMPONENT, detail));
  }

  /** A TURN converted with nothing refused: the converter is well. */
  function noteConverterHealthy(): void {
    if (converterDegraded === undefined) return;
    const droppedCount = converterDegraded.droppedCount;
    converterDegraded = undefined;
    pushes.resolveComponent(CONVERTER_COMPONENT, droppedCount);
  }

  // -- session-level pushes the shim itself produces ------------------------

  /** Arms this engine states itself; a fold-produced duplicate is dropped. */
  const OWNED_ARMS = new Set([
    "diagnostics",
    "contextUsage",
    "accountUsage",
    "mcpServer",
    "modelChanged",
    "permissionModeChanged",
    "identityRotated",
    "queryDied",
    "compacting",
    "fastMode",
  ]);

  function pushModel(): void {
    pushes.push(
      create(conversationv1.SessionUpdateSchema, {
        update: {
          case: "modelChanged",
          value: create(conversationv1.SessionModelChangedSchema, {
            effectiveModel: create(conversationv1.AgentModelSchema, { name: effectiveModel }),
          }),
        },
      }),
    );
  }

  /**
   * THE MODEL THE VENDOR SAYS IT ANSWERED ON.
   *
   * The vendor can serve a model nobody asked for — a refusal fallback retries
   * on another model and says so ONLY through the next assistant message's
   * `message.model`. The chip a surface draws is the model in effect, not the
   * last `SetSessionModel`, so the reported name is adopted as truth and pushed
   * whenever it differs.
   *
   * A `<synthetic>` MARKER IS NEVER A MODEL: it is the CLI's stand-in for "no
   * real nameable model", and adopting it would put an unspawnable id in the
   * picker and in every later authoritative field.
   */
  function noteReportedModel(reported: unknown): void {
    if (typeof reported !== "string") return;
    const name = reported.trim();
    if (name === "") return;
    if (name === SYNTHETIC_MODEL) {
      LOGGER.log(
        { level: "warn", reported: name },
        "the vendor reported the synthetic marker as its model; it is not a model and is not adopted",
      );
      return;
    }
    if (name === effectiveModel) return;
    LOGGER.log(
      { previous_model: effectiveModel, effective_model: name },
      "the vendor answered on a model the shim did not ask for; adopting it as the effective model",
    );
    effectiveModel = name;
    pushModel();
  }

  /**
   * FAST MODE, from the two places the vendor states it.
   *
   * `init` states it at the session's start (and again after every rotation),
   * and EVERY `result` restates it — which is what makes a mid-session change
   * observable at all, since the vendor announces nothing when it flips. ONE
   * PRODUCER: the fold's own `fast_mode` row still lands in the record, but the
   * push is the engine's, so no consumer ever sees the same flip twice.
   *
   * Unchanged values are dropped by the fan-out itself, and a joining consumer
   * is replayed the current one.
   */
  function noteFastMode(state: unknown, reason: unknown): void {
    if (typeof state !== "string" || state === "") return;
    LOGGER.logVerbose({ fast_mode_state: state }, "the vendor stated its fast-mode state");
    pushes.push(fastModeUpdate(state, typeof reason === "string" ? reason : undefined));
  }

  function pushPermissionMode(): void {
    pushes.push(
      create(conversationv1.SessionUpdateSchema, {
        update: {
          case: "permissionModeChanged",
          value: create(conversationv1.SessionPermissionModeChangedSchema, { permissionMode }),
        },
      }),
    );
  }

  /**
   * A vendor number as an int64 field.
   *
   * The vendor reports some of these as FRACTIONS (`percentage` arrives as
   * 0.85), and `BigInt()` throws outright on a non-integer — which would turn
   * one fractional field into a lost context-usage push and a spurious fault.
   * A fraction is rounded and RECORDED, so a token count that should never have
   * been fractional is visible rather than quietly absorbed.
   */
  function asInt64(value: number, field: string): bigint {
    if (Number.isInteger(value)) return BigInt(value);
    LOGGER.log(
      { level: "warn", field, value },
      "the vendor reported a fractional value for an integer field; rounding it",
    );
    return BigInt(Math.round(value));
  }

  /**
   * Context usage, mapped field by field from the vendor's own answer.
   *
   * NEVER DERIVED FROM USAGE FRAMES: the vendor knows what its context holds
   * and a sum of per-request usage does not. `gridRows` is dropped on purpose —
   * it is a rendering of the categories, and shipping a second representation
   * of the same numbers invites the two to disagree.
   */
  function contextUsageUpdate(usage: ContextUsageLike): conversationv1.SessionUpdate {
    return create(conversationv1.SessionUpdateSchema, {
      update: {
        case: "contextUsage",
        value: create(conversationv1.SessionContextUsageSchema, {
          totalTokens: asInt64(usage.totalTokens, "usage.totalTokens"),
          maxTokens: asInt64(usage.maxTokens, "usage.maxTokens"),
          rawMaxTokens: asInt64(usage.rawMaxTokens, "usage.rawMaxTokens"),
          percentage: asInt64(usage.percentage, "usage.percentage"),
          model: usage.model,
          isAutoCompactEnabled: usage.isAutoCompactEnabled,
          categories: usage.categories.map((category) =>
            create(conversationv1.SessionContextCategorySchema, {
              label: category.name,
              tokens: asInt64(category.tokens, "category.tokens"),
              color: category.color,
              ...(category.isDeferred === undefined ? {} : { isDeferred: category.isDeferred }),
            }),
          ),
          memoryFiles: usage.memoryFiles.map((file) =>
            create(conversationv1.SessionContextMemoryFileSchema, {
              path: file.path,
              type: file.type,
              tokens: asInt64(file.tokens, "file.tokens"),
            }),
          ),
          mcpTools: usage.mcpTools.map((tool) =>
            create(conversationv1.SessionContextMcpToolSchema, {
              name: tool.name,
              serverName: tool.serverName,
              tokens: asInt64(tool.tokens, "tool.tokens"),
              ...(tool.isLoaded === undefined ? {} : { isLoaded: tool.isLoaded }),
            }),
          ),
          deferredBuiltinTools: (usage.deferredBuiltinTools ?? []).map((tool) =>
            create(conversationv1.SessionContextDeferredBuiltinToolSchema, {
              name: tool.name,
              tokens: asInt64(tool.tokens, "tool.tokens"),
              isLoaded: tool.isLoaded,
            }),
          ),
          systemTools: (usage.systemTools ?? []).map((tool) =>
            create(conversationv1.SessionContextSystemToolSchema, {
              name: tool.name,
              tokens: asInt64(tool.tokens, "tool.tokens"),
            }),
          ),
          systemPromptSections: (usage.systemPromptSections ?? []).map((section) =>
            create(conversationv1.SessionContextSystemPromptSectionSchema, {
              name: section.name,
              tokens: asInt64(section.tokens, "section.tokens"),
            }),
          ),
          agents: usage.agents.map((agent) =>
            create(conversationv1.SessionContextAgentSchema, {
              agentType: agent.agentType,
              source: agent.source,
              tokens: asInt64(agent.tokens, "agent.tokens"),
            }),
          ),
          ...(usage.slashCommands === undefined
            ? {}
            : {
                slashCommands: create(conversationv1.SessionContextSlashCommandsSchema, {
                  totalCommands: asInt64(usage.slashCommands.totalCommands, "usage.slashCommands.totalCommands"),
                  includedCommands: asInt64(usage.slashCommands.includedCommands, "usage.slashCommands.includedCommands"),
                  tokens: asInt64(usage.slashCommands.tokens, "usage.slashCommands.tokens"),
                }),
              }),
          ...(usage.skills === undefined
            ? {}
            : {
                skills: create(conversationv1.SessionContextSkillsSchema, {
                  totalSkills: asInt64(usage.skills.totalSkills, "usage.skills.totalSkills"),
                  includedSkills: asInt64(usage.skills.includedSkills, "usage.skills.includedSkills"),
                  tokens: asInt64(usage.skills.tokens, "usage.skills.tokens"),
                  skillFrontmatter: usage.skills.skillFrontmatter.map((skill) =>
                    create(conversationv1.SessionContextSkillFrontmatterSchema, {
                      name: skill.name,
                      source: skill.source,
                      tokens: asInt64(skill.tokens, "skill.tokens"),
                    }),
                  ),
                }),
              }),
          ...(usage.autoCompactThreshold === undefined
            ? {}
            : { autoCompactThreshold: asInt64(usage.autoCompactThreshold, "usage.autoCompactThreshold") }),
          ...(usage.messageBreakdown === undefined
            ? {}
            : {
                messageBreakdown: create(conversationv1.SessionContextMessageBreakdownSchema, {
                  toolCallTokens: asInt64(usage.messageBreakdown.toolCallTokens, "usage.messageBreakdown.toolCallTokens"),
                  toolResultTokens: asInt64(usage.messageBreakdown.toolResultTokens, "usage.messageBreakdown.toolResultTokens"),
                  attachmentTokens: asInt64(usage.messageBreakdown.attachmentTokens, "usage.messageBreakdown.attachmentTokens"),
                  assistantMessageTokens: asInt64(usage.messageBreakdown.assistantMessageTokens, "usage.messageBreakdown.assistantMessageTokens"),
                  userMessageTokens: asInt64(usage.messageBreakdown.userMessageTokens, "usage.messageBreakdown.userMessageTokens"),
                  redirectedContextTokens: asInt64(usage.messageBreakdown.redirectedContextTokens, "usage.messageBreakdown.redirectedContextTokens"),
                  unattributedTokens: asInt64(usage.messageBreakdown.unattributedTokens, "usage.messageBreakdown.unattributedTokens"),
                  toolCallsByType: usage.messageBreakdown.toolCallsByType.map((entry) =>
                    create(conversationv1.SessionContextToolCallsByTypeSchema, {
                      name: entry.name,
                      callTokens: asInt64(entry.callTokens, "entry.callTokens"),
                      resultTokens: asInt64(entry.resultTokens, "entry.resultTokens"),
                    }),
                  ),
                  attachmentsByType: usage.messageBreakdown.attachmentsByType.map((entry) =>
                    create(conversationv1.SessionContextAttachmentsByTypeSchema, {
                      name: entry.name,
                      tokens: asInt64(entry.tokens, "entry.tokens"),
                    }),
                  ),
                }),
              }),
          ...(usage.apiUsage === null
            ? {}
            : {
                apiUsage: create(conversationv1.SessionContextApiUsageSchema, {
                  inputTokens: asInt64(usage.apiUsage.input_tokens, "usage.apiUsage.input_tokens"),
                  outputTokens: asInt64(usage.apiUsage.output_tokens, "usage.apiUsage.output_tokens"),
                  cacheCreationInputTokens: asInt64(usage.apiUsage.cache_creation_input_tokens, "usage.apiUsage.cache_creation_input_tokens"),
                  cacheReadInputTokens: asInt64(usage.apiUsage.cache_read_input_tokens, "usage.apiUsage.cache_read_input_tokens"),
                }),
              }),
        }),
      },
    });
  }

  async function pushContextUsage(): Promise<void> {
    const active = query;
    if (active === undefined) return;
    try {
      pushes.push(contextUsageUpdate(await active.getContextUsage()));
    } catch (err) {
      pushes.fault(
        sessionFault(
          { kind: "vendorQueryFailed" },
          "shim-engine-session",
          `getContextUsage failed: ${err instanceof Error ? err.message : String(err)}`,
        ),
      );
    }
  }

  /** One usage window, or absence when the vendor could not state it. */
  function usageWindow(
    window: { utilization: number | null; resets_at: string | null } | null | undefined,
  ): conversationv1.SessionUsageWindow | undefined {
    if (window === undefined || window === null) return undefined;
    if (window.utilization === null || window.resets_at === null) return undefined;
    const resetsAt = Date.parse(window.resets_at);
    if (Number.isNaN(resetsAt)) return undefined;
    return create(conversationv1.SessionUsageWindowSchema, {
      utilizationPercent: window.utilization,
      resetsAtMs: BigInt(resetsAt),
    });
  }

  /**
   * The account's windows.
   *
   * Every "the vendor could not say" case gets its OWN unavailable reason
   * rather than an absent field, because "we did not ask" and "the service is
   * down" are different facts to a consumer deciding whether to warn a user.
   */
  function accountUsageUpdate(usage: AccountUsageLike): conversationv1.SessionUpdate {
    const observedAtMs = BigInt(deps.nowMs());
    const subscriptionType = usage.subscription_type ?? "";
    const unavailable = (
      reason: conversationv1.SessionAccountUsageUnavailable["reason"],
    ): conversationv1.SessionUpdate =>
      create(conversationv1.SessionUpdateSchema, {
        update: {
          case: "accountUsage",
          value: create(conversationv1.SessionAccountUsageSchema, {
            observedAtMs,
            subscriptionType,
            outcome: {
              case: "unavailable",
              value: create(conversationv1.SessionAccountUsageUnavailableSchema, { reason }),
            },
          }),
        },
      });
    if (!usage.rate_limits_available) {
      return unavailable({
        case: "serviceUnavailable",
        value: create(conversationv1.SessionUsageServiceUnavailableSchema, {}),
      });
    }
    if (usage.rate_limits === null || usage.rate_limits === undefined) {
      // The service said it had limits and then produced none: still the
      // service failing to answer, not a shape with a window missing from it.
      return unavailable({
        case: "serviceUnavailable",
        value: create(conversationv1.SessionUsageServiceUnavailableSchema, {}),
      });
    }
    // THE TWO REASONS ARE DIFFERENT FACTS ABOUT THE FIVE-HOUR WINDOW, and the
    // consumer acts on them differently. `window_unavailable` means the service
    // answered WITHOUT a five-hour window at all; `utilization_unavailable`
    // means the window is there and its utilization is not. Collapsing the
    // first onto the second left `window_unavailable` unproducible while
    // looking as though it were covered.
    const rawFiveHour = usage.rate_limits.five_hour;
    if (rawFiveHour === null || rawFiveHour === undefined) {
      return unavailable({
        case: "windowUnavailable",
        value: create(conversationv1.SessionUsageWindowUnavailableSchema, {}),
      });
    }
    const fiveHour = usageWindow(rawFiveHour);
    if (fiveHour === undefined) {
      return unavailable({
        case: "utilizationUnavailable",
        value: create(conversationv1.SessionUsageUtilizationUnavailableSchema, {}),
      });
    }
    const optional = (
      window: { utilization: number | null; resets_at: string | null } | null | undefined,
      key: "sevenDay" | "sevenDayOauthApps" | "sevenDayOpus" | "sevenDaySonnet",
    ): Record<string, conversationv1.SessionUsageWindow> => {
      const built = usageWindow(window);
      return built === undefined ? {} : { [key]: built };
    };
    return create(conversationv1.SessionUpdateSchema, {
      update: {
        case: "accountUsage",
        value: create(conversationv1.SessionAccountUsageSchema, {
          observedAtMs,
          subscriptionType,
          outcome: {
            case: "available",
            value: create(conversationv1.SessionAccountUsageAvailableSchema, {
              fiveHour,
              ...optional(usage.rate_limits.seven_day, "sevenDay"),
              ...optional(usage.rate_limits.seven_day_oauth_apps, "sevenDayOauthApps"),
              ...optional(usage.rate_limits.seven_day_opus, "sevenDayOpus"),
              ...optional(usage.rate_limits.seven_day_sonnet, "sevenDaySonnet"),
              modelScoped: (usage.rate_limits.model_scoped ?? []).flatMap((scoped) => {
                const window = usageWindow(scoped);
                return window === undefined
                  ? []
                  : [
                      create(conversationv1.SessionModelUsageWindowSchema, {
                        model: create(conversationv1.AgentModelSchema, { name: scoped.display_name }),
                        window,
                      }),
                    ];
              }),
            }),
          },
        }),
      },
    });
  }

  /**
   * Re-read the pulled session facts, after something that could have changed
   * them.
   *
   * Failures here are the probes' own business: each already turns its
   * exception into a stated arm or a `SessionFault`, so nothing is swallowed
   * and a probe that cannot answer does not take the turn end down with it.
   */
  async function reprobeSessionFacts(): Promise<void> {
    if (standingDown) return;
    await pushAccountUsage();
    await pushMcpServerStatus();
  }

  /** Probe every declared mcp server's health and state each one. */
  async function pushMcpServerStatus(): Promise<void> {
    const active = query;
    if (active === undefined) return;
    try {
      for (const status of await active.mcpServerStatus()) pushes.push(mcpUpdate(status));
    } catch (err) {
      pushes.fault(
        sessionFault(
          { kind: "vendorQueryFailed" },
          "shim-engine-session",
          `mcpServerStatus failed: ${err instanceof Error ? err.message : String(err)}`,
        ),
      );
    }
  }

  async function pushAccountUsage(): Promise<void> {
    const active = query;
    if (active === undefined) return;
    try {
      pushes.push(
        accountUsageUpdate(await active.usage_EXPERIMENTAL_MAY_CHANGE_DO_NOT_RELY_ON_THIS_API_YET()),
      );
    } catch (err) {
      pushes.push(
        create(conversationv1.SessionUpdateSchema, {
          update: {
            case: "accountUsage",
            value: create(conversationv1.SessionAccountUsageSchema, {
              observedAtMs: BigInt(deps.nowMs()),
              subscriptionType: "",
              outcome: {
                case: "unavailable",
                value: create(conversationv1.SessionAccountUsageUnavailableSchema, {
                  reason: {
                    case: "samplingFailure",
                    value: create(conversationv1.SessionUsageSamplingFailureSchema, {
                      cause: err instanceof Error ? err.message : String(err),
                    }),
                  },
                }),
              },
            }),
          },
        }),
      );
    }
  }

  /** The five declared healths, each its own arm. */
  function mcpUpdate(status: McpServerStatusLike): conversationv1.SessionUpdate {
    const health = ((): conversationv1.SessionMcpServer["health"] => {
      switch (status.status) {
        case "connected":
          return { case: "connected", value: create(conversationv1.SessionMcpServerConnectedSchema, {}) };
        case "failed":
          return {
            case: "failed",
            value: create(conversationv1.SessionMcpServerFailedSchema, { error: status.error ?? "" }),
          };
        case "needs-auth":
          return { case: "needsAuth", value: create(conversationv1.SessionMcpServerNeedsAuthSchema, {}) };
        case "pending":
          return { case: "pending", value: create(conversationv1.SessionMcpServerPendingSchema, {}) };
        case "disabled":
          return { case: "disabled", value: create(conversationv1.SessionMcpServerDisabledSchema, {}) };
      }
    })();
    return create(conversationv1.SessionUpdateSchema, {
      update: {
        case: "mcpServer",
        value: create(conversationv1.SessionMcpServerSchema, { name: status.name, health }),
      },
    });
  }

  function modelOptions(models: readonly ModelInfoLike[]): conversationv1.ModelOption[] {
    const efforts: Record<string, conversationv1.AgentEffortLevel> = {
      low: conversationv1.AgentEffortLevel.LOW,
      medium: conversationv1.AgentEffortLevel.MEDIUM,
      high: conversationv1.AgentEffortLevel.HIGH,
      xhigh: conversationv1.AgentEffortLevel.XHIGH,
      max: conversationv1.AgentEffortLevel.MAX,
    };
    return models.map((model) =>
      create(conversationv1.ModelOptionSchema, {
        model: create(conversationv1.AgentModelSchema, { name: model.value }),
        displayName: model.displayName,
        description: model.description,
        // Capabilities are stated only where the vendor declared them: an
        // absent `supportsEffort` is "not stated", not "unsupported".
        ...(model.supportsEffort === undefined &&
        model.supportsAdaptiveThinking === undefined &&
        model.supportsFastMode === undefined &&
        model.supportsAutoMode === undefined &&
        model.resolvedModel === undefined
          ? {}
          : {
              capabilities: create(conversationv1.ModelCapabilitiesSchema, {
                supportsAdaptiveThinking: model.supportsAdaptiveThinking === true,
                supportsFastMode: model.supportsFastMode === true,
                supportsAutoMode: model.supportsAutoMode === true,
                ...(model.resolvedModel === undefined
                  ? {}
                  : {
                      resolvedModel: create(conversationv1.AgentModelSchema, {
                        name: model.resolvedModel,
                      }),
                    }),
                effortSupport:
                  model.supportsEffort === true
                    ? {
                        case: "effortSupported",
                        value: create(conversationv1.ModelEffortSupportedSchema, {
                          levels: (model.supportedEffortLevels ?? []).map(
                            (level) => efforts[level] ?? conversationv1.AgentEffortLevel.UNSPECIFIED,
                          ),
                        }),
                      }
                    : {
                        case: "effortUnsupported",
                        value: create(conversationv1.ModelEffortUnsupportedSchema, {}),
                      },
              }),
            }),
      }),
    );
  }

  // -- the message loop -----------------------------------------------------

  async function onSdkMessage(message: SdkMessage): Promise<void> {
    noteIdentityFacts(message);
    noteDetachedWork(message);
    converterDefectThisMessage = false;
    const output = deps.fold.onSdkMessage(message, foldContext());
    if (converterDefectThisMessage) {
      LOGGER.logVerbose({}, "this message was refused; the converter's window stays open");
    }
    const entries = [...output.entries];
    noteForegroundUnits(entries);
    if (entries.length > 0) deps.persistence.write(entries);
    for (const entry of entries) {
      if (entry.item.kind !== "session_update") continue;
      const arm = entry.item.update.update.case ?? "";
      if (OWNED_ARMS.has(arm)) continue;
      pushes.push(entry.item.update);
    }
    const uuid = (message as { uuid?: string }).uuid;
    if (typeof uuid === "string" && uuid !== "") rewind.noteRecord(uuid, open?.keepalive === true);
    if (output.turnEnded !== undefined) {
      // THE TURN IS THE UNIT OF RECOVERY. A defective turn is degraded for its
      // WHOLE length: the messages that follow the refused one are the same
      // turn's own remainder, and recovering on the next of them closed the
      // window a millisecond after opening it — before any consumer could
      // observe it, and while the turn that lost a record was still running.
      // So the window closes only at the end of a turn that refused nothing.
      if (!converterDefectThisTurn) noteConverterHealthy();
      converterDefectThisTurn = false;
      await closeTurn();
    }
  }

  function noteIdentityFacts(message: SdkMessage): void {
    if (message.type === "system" && message.subtype === "init") {
      setClaudeSessionId(message.session_id);
      recordAgentBinaryVersion(message.claude_code_version);
      if (message.model.trim() === SYNTHETIC_MODEL) {
        LOGGER.log(
          { level: "warn" },
          "the vendor's init reported the synthetic marker as its model; keeping the model already in effect",
        );
      } else {
        effectiveModel = message.model;
      }
      permissionMode = fromVendorPermissionMode(message.permissionMode);
      noteFastMode(
        (message as { fast_mode_state?: unknown }).fast_mode_state,
        (message as { fast_mode_disabled_reason?: unknown }).fast_mode_disabled_reason,
      );
      if (identity !== undefined && message.session_id !== identity.vendorSessionId) {
        void rotate(message.session_id);
      }
      initResolve?.(message);
      initResolve = undefined;
      return;
    }
    if (message.type === "result") {
      // EVERY RESULT RESTATES IT, which is the only way a flip mid-session is
      // ever seen: the vendor announces the change nowhere else.
      noteFastMode(
        (message as { fast_mode_state?: unknown }).fast_mode_state,
        (message as { fast_mode_disabled_reason?: unknown }).fast_mode_disabled_reason,
      );
      return;
    }
    if (message.type === "assistant") {
      // THE ASSISTANT MESSAGE IS THE ONLY PLACE THE VENDOR NAMES ITS REQUEST.
      // Stamping it here puts request_id on every record of the rest of this
      // turn, which is what joins a shim record to the vendor call it came
      // from. A message that carries none leaves the standing stamp alone: an
      // absent field is the vendor not saying, never a new request.
      const requestId = (message as { request_id?: unknown }).request_id;
      if (typeof requestId === "string" && requestId !== "") setRequestId(requestId);
      // UNSOLICITED CHANGES ARE STILL CHANGES: nothing called SetSessionModel,
      // so this message is the only evidence the swap happened.
      noteReportedModel((message.message as { model?: unknown } | undefined)?.model);
      return;
    }
    if (message.type === "conversation_reset") {
      // `conversation_reset` SIGNALS a rotation; it does not name the id the
      // session rotates to. Evidence, from the real /clear capture: this
      // message's own `session_id` is still the OLD id, and its
      // `new_conversation_id` is a third uuid NOTHING later uses -- no
      // transcript is written under it, no init announces it, and no resume
      // takes it. The id the session actually moves to is announced by the
      // SECOND `system:init` that follows, and repeated by the next turn's
      // init; the branch above performs the rotation when it arrives.
      //
      // Writing `new_conversation_id` as the new identity would publish a
      // SessionIdentityRotated naming an id that does not exist, link the
      // vendor id to nothing, and hand the daemon a resume handle the vendor
      // would refuse.
      LOGGER.log(
        { previous: identity?.vendorSessionId ?? "", signalled: message.new_conversation_id },
        "the vendor reset the conversation; awaiting the init that names the id it rotated to",
      );
      return;
    }
    if (message.type === "system" && message.subtype === "model_refusal_fallback") {
      // THE VENDOR'S OWN ANNOUNCEMENT OF THE SWAP. `SessionUpdate.model_changed`
      // is stated "by SetSessionModel, or by the vendor … so one place is
      // authoritative", so the fallback record folds into the SAME fact rather
      // than an arm of its own — the proto retired the model-fallback arm.
      //
      // Stated FROM THE RECORD and not only from the fallback leg's assistant
      // message: the record arrives first, and a fallback whose retry produces
      // no assistant message at all would otherwise never move the chip.
      // `noteReportedModel` is idempotent, so the assistant message that
      // follows restates nothing, and a fallback naming the model ALREADY in
      // effect states nothing — the fact is that the effective model CHANGED.
      noteReportedModel((message as { fallback_model?: unknown }).fallback_model);
      return;
    }
    if (message.type === "system" && message.subtype === "status") {
      if (message.status === "compacting") {
        // The vendor compacts on its own when the window fills. The status
        // message is the START signal so a surface can draw the in-progress
        // state; the ContextCut page line below is the end.
        pushes.push(
          create(conversationv1.SessionUpdateSchema, {
            update: { case: "compacting", value: create(conversationv1.SessionCompactingSchema, {}) },
          }),
        );
      }
      // `compact_result`/`compact_error` ride the STATUS message, not init.
      //
      // A SUCCESS is deliberately NOT reported from here: the figures a
      // `ContextCompacted` needs (the summary, the token delta, the duration)
      // are on the vendor's own `compact_boundary` record, which the fold maps,
      // and a page line built here would have to state them as zeroes — a
      // sentinel the contract forbids. A FAILURE has no boundary record at all,
      // so this is its only producer, and its one field is fully stated.
      if (message.compact_result === "failed") {
        writeContextCut(contextCutFailed(message.compact_error ?? "the vendor's compaction failed"));
      }
    }
  }

  async function rotate(newVendorSessionId: string): Promise<void> {
    const current = identity;
    if (current === undefined || current.vendorSessionId === newVendorSessionId) return;
    const previous = current.vendorSessionId;
    pushes.push(await current.rotate(newVendorSessionId));
    setClaudeSessionId(newVendorSessionId);
    backupTranscript({
      transcript: transcriptPath(deps.env.configDir, deps.env.cwd, previous),
      stateDir: deps.env.stateDir,
      workspaceKey,
      vendorSessionId: previous,
      atMs: deps.nowMs(),
    });
  }

  /**
   * Track the units in flight, so `DetachForeground` can tell its four refusals
   * apart.
   *
   * Read off the FOLD's own frames and not off the raw SDK blocks: a consumer
   * addresses a unit by the `AgentActivityId` it was shown, so the table has to
   * be keyed by exactly that, and the `item` arm carries the kind in the same
   * vocabulary the refusals speak.
   */
  function noteForegroundUnits(entries: readonly PersistEntry[]): void {
    for (const entry of entries) {
      if (entry.item.kind !== "frame") continue;
      const frame = entry.item.frame;
      if (frame.result.case !== "update") continue;
      const update = frame.result.value.update;
      // A DENIAL THE FOLD PRODUCED is still a denial this shim has to remember:
      // the policy and undecidable arms never reach the gate's ask, and the
      // `tool_result` that follows them must be recognised as the relayed deny
      // rather than as a call that ran. Relayed into the gate's one memory so
      // `deniedCall` does not depend on which half saw the denial.
      if (update.case === "permission") {
        const result = update.value.result;
        if (result.case === "success" && result.value.decision.case === "denied") {
          gate.noteVendorDenial(update.value.gatedCall?.value ?? "");
        }
        continue;
      }
      if (update.case !== "activity") continue;
      const activity = update.value;
      const item = activity.item;
      // AN ANNOUNCEMENT IS A PROMISE THE ID IS ADDRESSABLE. `created_agent_id`
      // is the key a consumer draws a container under and opens its own
      // WatchAgent on — the daemon does exactly that, the instant it sees the
      // spawn. Recording it here is what lets that watch be answered before the
      // subagent's first row exists, and it is the ONLY record for a
      // SYNCHRONOUS subagent, which runs inside the turn and is never a task.
      if (item.case === "subagent" && item.value.result.case === "start") {
        const created = item.value.result.value.createdAgentId?.value ?? "";
        if (created !== "") announcedAgents.add(created);
      }
      // NOT EVERY ITEM HAS A LIFECYCLE. `AgentTaskAct`, `AgentPlanMode`,
      // `AgentWorktree`, `AgentCron`, `AgentContextInjected`,
      // `AgentReportFindings` and `AgentPushNotification` carry no `result`
      // oneof at all: the act IS the whole unit, and there is no "start" of it
      // to be waiting on. Reading settledness off a `result` those items do not
      // have left every one of them in flight forever, so `DetachForeground`
      // answered `not_detachable` for a unit that had plainly concluded.
      // THE ADJACENCY the IDE-diagnostics join is made on. Remembered from the
      // fold's own frames, so the id is the one the diagnostics report has to
      // name -- the unit a consumer was shown.
      if (item.case === "write" || item.case === "edit") {
        const activityId = activity.activityId;
        // THE KIND IS REMEMBERED WITH THE ID. The report rides the remembered
        // unit's OWN arm, so a write's findings can only reach
        // `AgentWrite.diagnostics` if this says the change was a write.
        if (activityId !== undefined && activityId.value !== "") {
          lastChange = { unit: activityId, kind: item.case };
        }
      }
      const inner = item.value as { result?: { case?: string } } | undefined;
      const settled =
        inner === undefined || !("result" in inner)
          ? true
          : inner.result?.case !== undefined && inner.result.case !== "start";
      foreground.note(activity.activityId?.value ?? "", item.case ?? "", settled);
    }
  }

  function noteDetachedWork(message: SdkMessage): void {
    if (message.type !== "system") return;
    switch (message.subtype) {
      case "task_started":
        live.onTaskStarted(message, open?.id.value);
        break;
      case "task_updated":
        live.onTaskUpdated(message);
        break;
      case "task_notification":
        live.onTaskNotification(message);
        break;
      case "background_tasks_changed":
        live.onLevel(message);
        break;
      default:
        break;
    }
  }

  async function closeTurn(): Promise<void> {
    const ended = open;
    open = undefined;
    // THE REQUEST ID DIES WITH ITS TURN. It named the vendor call this turn ran
    // under; carrying it past the end would attribute idle records and the next
    // turn to a request that is already answered.
    clearRequestId();
    if (ended === undefined) return;
    if (ended.keepalive) rewind.noteKeepaliveTurn();
    cadence?.resume();
    if (pendingModel !== undefined) {
      const waiting = pendingModel;
      pendingModel = undefined;
      try {
        await applyModel(waiting.model);
        waiting.resolve(
          create(shimv1.SetSessionModelResponseSchema, {
            result: {
              case: "success",
              value: create(shimv1.SetSessionModelSuccessSchema, {
                modelChanged: create(conversationv1.SessionModelChangedSchema, {
                  effectiveModel: waiting.model,
                }),
              }),
            },
          }),
        );
      } catch (err) {
        // The caller has been waiting for this exact moment, so the vendor's
        // refusal is ITS answer; swallowing it would leave the daemon believing
        // a model change landed that never did.
        waiting.resolve(
          setSessionModelRefused(
            { kind: "vendorRefused" },
            err instanceof Error ? err.message : String(err),
          ),
        );
      }
    }
    await pushContextUsage();
    if (identity !== undefined) {
      backupTranscript({
        transcript: transcriptPath(deps.env.configDir, deps.env.cwd, identity.vendorSessionId),
        stateDir: deps.env.stateDir,
        workspaceKey,
        vendorSessionId: identity.vendorSessionId,
        atMs: deps.nowMs(),
      });
    }
    // A TURN CAN CHANGE WHAT THE PROBES ANSWER. Account usage and mcp health
    // are PULLED from the vendor rather than folded out of the stream, so a
    // session that probed once at StartSession would report the account's
    // windows and its servers' health as they stood before any work happened,
    // and would never notice a server going down or a limit being approached.
    // Awaited, not fired and forgotten: an unawaited probe would race the next
    // turn's own close.
    await reprobeSessionFacts();
    LOGGER.log({ turn_id: ended.id.value, keepalive: ended.keepalive }, "closed a turn");
  }

  async function applyModel(model: conversationv1.AgentModel): Promise<void> {
    const active = query;
    if (active === undefined) return;
    await active.setModel(model.name);
    effectiveModel = model.name;
    pushModel();
  }

  function runLoop(active: QueryLike): Promise<void> {
    return (async (): Promise<void> => {
      try {
        for await (const message of active) {
          await onSdkMessage(message);
        }
        if (!standingDown) {
          pushes.push(
            create(conversationv1.SessionUpdateSchema, {
              update: {
                case: "queryDied",
                value: create(conversationv1.SessionQueryDiedSchema, {
                  cause: {
                    case: "unexpectedEof",
                    value: create(conversationv1.SessionQueryUnexpectedEofSchema, {}),
                  },
                }),
              },
            }),
          );
          onQueryLost("the vendor query ended without being asked to");
        }
      } catch (err) {
        pushes.push(
          create(conversationv1.SessionUpdateSchema, {
            update: {
              case: "queryDied",
              value: create(conversationv1.SessionQueryDiedSchema, {
                cause: {
                  case: "iteratorFailure",
                  value: create(conversationv1.SessionQueryIteratorFailureSchema, {
                    cause: err instanceof Error ? err.message : String(err),
                  }),
                },
              }),
            },
          }),
        );
        onQueryLost(err instanceof Error ? err.message : String(err));
      }
    })();
  }

  /** The query is gone: unwedge the vendor's callbacks, then report the fault. */
  function onQueryLost(detail: string): void {
    gate.standDown(`the vendor query died: ${detail}`);
    query = undefined;
    // THE STREAM OWNER GETS ITS OWN TERMINAL. `query_died` is a SESSION fact,
    // and a consumer watching the agent -- which is the consumer actually
    // waiting on the turn -- would otherwise see the stream simply stop
    // producing, with no terminal frame and no way to tell a dead query from a
    // slow one. Duplicated on purpose: a consumer with no stream open still
    // needs the session-level fact, and a consumer with no WatchSession open
    // still needs its turn concluded.
    writeQueryDeathTerminal(detail);
    open = undefined;
    cadence?.stop();
    pushes.fault(sessionFault({ kind: "vendorQueryFailed" }, "shim-engine-session", detail));
  }

  // -- submission -----------------------------------------------------------

  async function submit(said: conversationv1.UserSaid, keepalive: boolean): Promise<void> {
    const text = saidText(said);
    if (!keepalive) await yieldObligation();
    const queue = prompts;
    if (queue === undefined) throw new Error("shim session: no query is accepting prompts");
    cadence?.pause();
    queue.push({ type: "user", message: { role: "user", content: text }, parent_tool_use_id: null });
    LOGGER.log({ keepalive, characters: text.length }, "submitted a prompt to the vendor");
  }

  /**
   * THE YIELD OBLIGATION.
   *
   * A real prompt must never build on keep-alive context, so when keep-alive
   * turns have run since the last real record the query is REPLACED by one that
   * resumes only THROUGH that record (`resumeSessionAt`). The old query is
   * closed first: two queries on one conversation are two writers on one
   * transcript.
   */
  async function yieldObligation(): Promise<void> {
    const owed = rewind.obligation();
    if (owed === undefined) return;
    const current = identity;
    if (current === undefined) return;
    LOGGER.log(
      { resume_session_at: owed.resumeSessionAt, discarded_keepalive_turns: owed.discardedKeepaliveTurns },
      "REWINDING the vendor context past the trailing keep-alive turns before delivering a real prompt",
    );
    await replaceQuery({
      binding: { kind: "resume", resumeSessionId: current.vendorSessionId },
      resumeSessionAt: owed.resumeSessionAt,
    });
    rewind.settled();
  }

  async function replaceQuery(options: {
    binding: QuerySpec["binding"];
    resumeSessionAt?: string;
  }): Promise<void> {
    const previous = query;
    const previousPrompts = prompts;
    query = undefined;
    prompts = undefined;
    abort?.abort();
    previousPrompts?.close();
    previous?.close();
    await startQuery(options.binding, options.resumeSessionAt);
  }

  async function startQuery(
    binding: QuerySpec["binding"],
    resumeSessionAt?: string,
  ): Promise<void> {
    const queue = new PromptQueue();
    const controller = new AbortController();
    const created = await deps.createQuery({
      binding,
      permissionMode: toVendorPermissionMode(permissionMode),
      canUseTool: gate.canUseTool,
      abortController: controller,
      prompt: queue,
      ...(effectiveModel === "" ? {} : { model: effectiveModel }),
      ...(resumeSessionAt === undefined ? {} : { resumeSessionAt }),
    });
    query = created;
    prompts = queue;
    abort = controller;
    loop = runLoop(created);
  }

  // -- StartSession ---------------------------------------------------------

  function awaitInit(): Promise<void> {
    return new Promise<void>((resolve, reject) => {
      initResolve = () => resolve();
      const timeout = deps.initTimeoutMs;
      if (timeout === undefined || timeout <= 0) return;
      const handle = setTimeout(() => {
        initResolve = undefined;
        reject(new Error(`the vendor did not send its init message within ${timeout}ms`));
      }, timeout);
      if (typeof (handle as { unref?: () => void }).unref === "function") {
        (handle as { unref: () => void }).unref();
      }
    });
  }

  async function startSession(
    request: shimv1.StartSessionRequest,
  ): Promise<shimv1.StartSessionResponse> {
    if (started) {
      return startSessionRefused(
        { kind: "alreadyStarted" },
        "this shim already started its session; one shim serves exactly one",
      );
    }
    const source = request.source;
    if (source.case === undefined) {
      throw new Error("shim session: StartSession reached the engine with no source");
    }
    let vendorSessionId: string;
    let clearedTo: string | undefined;
    let facts: TranscriptFacts | undefined;
    // No initializer: BOTH source arms below set it, and a third arm that
    // forgot to would be a compile error rather than a silent empty model.
    let requestedModel: string;
    if (source.case === "fresh") {
      vendorSessionId = mintVendorSessionId();
      requestedModel = source.value.model?.name ?? "";
      if (source.value.permissionMode !== undefined) permissionMode = source.value.permissionMode;
    } else {
      vendorSessionId = source.value.vendorSessionId;
      facts = readTranscriptFacts(
        transcriptPath(deps.env.configDir, deps.env.cwd, vendorSessionId),
      );
      if (facts === undefined) {
        return startSessionRefused(
          { kind: "unknownSession" },
          `no transcript exists for vendor session ${JSON.stringify(vendorSessionId)} in this workspace`,
        );
      }
      // A RESUME RESTORES THE CONVERSATION'S OWN POSTURE. The SDK records the
      // model and permission mode in the transcript but does not restore them,
      // so the shim reads the last of each back and passes them.
      requestedModel = facts.lastModel ?? "";
      if (facts.lastPermissionMode !== undefined) {
        permissionMode = fromVendorPermissionMode(
          facts.lastPermissionMode as PermissionModeLike,
        );
      }
      const remediation = source.value.coldRemediation;
      const cold = judgeCold(facts, deps.nowMs(), requestedModel);
      if (cold !== undefined && remediation === undefined) {
        LOGGER.log(
          { level: "warn", vendor_session_id: vendorSessionId, reason: cold, context_tokens: facts.contextTokens },
          "REFUSED a cold resume: the cost is stated and the caller must name a remediation",
        );
        return startSessionRefused(
          { kind: "cold", cold: sessionCold(facts, cold, requestedModel) },
          `resuming this conversation is a cold read of ${facts.contextTokens} tokens (${cold})`,
        );
      }
      const remedy = await applyColdRemediation(remediation, vendorSessionId, facts, requestedModel);
      if (remedy.kind === "failed") {
        return startSessionRefused({ kind: "vendorStartFailed" }, remedy.detail);
      }
      if (remedy.kind === "cleared") {
        clearedTo = remedy.vendorSessionId;
        // /clear KEEPS THE SESSION IDENTITY AND DISCARDS THE CONTEXT, and the
        // cheapest declared way to do that is to bind FRESH under a newly
        // minted vendor session id: no API call, no transcript rewrite, and the
        // AgentId is unaffected because R9 fixes it to the ORIGINAL id. The
        // rotation is announced on WatchSession like any other.
      }
    }
    const inForce = clearedTo ?? vendorSessionId;
    try {
      releaseLock = await acquireLock(inForce);
    } catch (err) {
      LOGGER.log(
        { level: "warn", vendor_session_id: inForce, cause: err instanceof Error ? err.message : String(err) },
        "REFUSED StartSession: another shim holds this conversation's session lock",
      );
      return startSessionRefused(
        { kind: "conversationOwned" },
        `another process already owns vendor session ${JSON.stringify(inForce)}`,
      );
    }
    // THE WORKSPACE CLAIM, taken with the session claim and in the same fixed
    // order the lock module documents (session first, then workspace), so two
    // shims racing for the pair cannot take them in opposite orders. It lands
    // HERE rather than at process start because an inert shim owns no
    // conversation and must not exclude the live one it will replace.
    try {
      releaseWorkspaceLock = await acquireWorkspace(deps.env.cwd);
    } catch (err) {
      await releaseLock?.();
      releaseLock = undefined;
      LOGGER.log(
        {
          level: "warn",
          workspace_dir: deps.env.cwd,
          lock_path: workspaceLockPath(deps.env.cwd),
          cause: err instanceof Error ? err.message : String(err),
        },
        "REFUSED StartSession: another shim holds this workspace's lock",
      );
      return startSessionRefused(
        { kind: "conversationOwned" },
        `another process already owns this workspace (${workspaceLockPath(deps.env.cwd)})`,
      );
    }
    const brandNew = source.case === "fresh" || facts === undefined;
    // WHETHER THIS WORKSPACE ALREADY HAD AN IDENTITY, read BEFORE one is
    // settled. It is the only thing that tells an identity this attempt minted
    // apart from one an earlier session established, and the abandonment path
    // below may only discard the former.
    const identityWasPersisted = (await identityStore.read()) !== undefined;
    identity = brandNew
      ? await SessionIdentity.fresh(identityStore, () => vendorSessionId)
      : await SessionIdentity.resume(identityStore, vendorSessionId);
    // NAME THE WRITER BEFORE ANYTHING IS WRITTEN. A row is keyed by the
    // conversation's ORIGINAL vendor session id, which is exactly what the
    // identity just settled — and a write attempted before this raises rather
    // than landing rows under a name no replay could absorb against.
    deps.persistence.setProducer(identity.originalVendorSessionId);
    // THE REGISTRATION ORDER, STATED AT THE ONE POINT THAT KNOWS IT. A book is
    // registered by the first write that names its agent, and a FRESH start's
    // AgentId is a uuid minted moments ago — so the store provably holds no row
    // for it, and will not until this session writes. The daemon opens the main
    // agent's watch before any turn, exactly as the endpoint contract tells it
    // to, so without this the record plane probed the store for an answer it
    // already had and collected `unknown_agent` refusals on every healthy
    // bring-up. Only a FRESH id qualifies: a resumed conversation's id was
    // minted by an earlier session that may well have written under it.
    if (source.case === "fresh") deps.persistence.noteAgentMinted(identity.agentId.value);
    if (clearedTo !== undefined) {
      // The AgentId does not move; only the resume handle does, and the
      // rotation is announced exactly like a vendor-initiated one.
      pushes.push(await identity.rotate(clearedTo));
    }
    effectiveModel = requestedModel;
    try {
      const initialized = awaitInit();
      await startQuery(
        brandNew || clearedTo !== undefined
          ? { kind: "fresh", sessionId: inForce }
          : { kind: "resume", resumeSessionId: vendorSessionId },
      );
      await initialized;
    } catch (err) {
      // A FAILED START LEAVES THE ENGINE AS IT FOUND IT. The next StartSession
      // is a fresh attempt that settles its OWN identity, so every trace of
      // this one goes: both kernel claims, the writer's name, and — when this
      // attempt is what minted it — the persisted identity. Leaving the writer
      // named is what made the retry hit the re-key guard and escape as an
      // unhandled `Internal` on a verb that has a typed refusal for every real
      // condition.
      //
      // NOTHING WAS WRITTEN UNDER THE NAME YET: the held cuts are written after
      // this block precisely so that stays true, and `clearProducer` refuses
      // outright if it ever stops being.
      await releaseLock?.();
      releaseLock = undefined;
      await releaseWorkspaceLock?.();
      releaseWorkspaceLock = undefined;
      deps.persistence.clearProducer();
      if (!identityWasPersisted) await identityStore.forget();
      identity = undefined;
      const detail = err instanceof Error ? err.message : String(err);
      LOGGER.log({ level: "error", cause: detail }, "the vendor query could not be started");
      return startSessionRefused({ kind: "vendorStartFailed" }, detail);
    }
    // THE HELD CUTS, NOW KEYABLE AND NOW SAFE TO KEY. Written AFTER the query
    // is up rather than before it: a row landed under a producer the failure
    // path then abandons would strand one conversation's rows under a name
    // nothing else ever uses again. They are still the FIRST rows this session
    // writes, in the order they happened, so the compaction the cold gate just
    // performed is on the first page a consumer opens.
    if (pendingContextCuts.length > 0) {
      const held = pendingContextCuts.splice(0, pendingContextCuts.length);
      LOGGER.log({ held: held.length }, "writing the context cuts held until the session had an identity");
      for (const cut of held) writeContextCut(cut);
    }
    started = true;
    const active = query;
    if (active !== undefined) {
      try {
        modelCatalog = modelOptions(await active.supportedModels());
      } catch (err) {
        pushes.fault(
          sessionFault(
            { kind: "vendorQueryFailed" },
            "shim-engine-session",
            `supportedModels failed: ${err instanceof Error ? err.message : String(err)}`,
          ),
        );
      }
      await pushMcpServerStatus();
    }
    const liveWork = await reconcile();
    // THE CADENCE BEGINS BEFORE SUCCESS RETURNS: a session that is never
    // prompted still has a cache worth keeping warm.
    cadence = new KeepaliveCadence(
      () => void keepaliveBeat(),
      deps.keepaliveIntervalMs,
      deps.scheduler ?? REAL_SCHEDULER,
    );
    cadence.start();
    accountUsageHandle = (deps.scheduler ?? REAL_SCHEDULER).setInterval(
      () => void pushAccountUsage(),
      ACCOUNT_USAGE_INTERVAL_MS,
    );
    pushModel();
    pushPermissionMode();
    await pushContextUsage();
    void pushAccountUsage();
    // The readiness signal, and the reason a consumer may open WatchSession
    // before anything else exists.
    pushes.push(pushes.diagnostics());
    // The build sha and SDK version are the PROCESS's, injected at construction;
    // only the agent binary version has to be waited for, and
    // requireSessionRuntime refuses to answer until `system:init` stated it —
    // which is the presence rule doing its job, not a failure.
    const runtime = requireSessionRuntime();
    const started_ = create(conversationv1.SessionStartedSchema, {
      vendorSessionId: identity.vendorSessionId,
      runtime: create(conversationv1.SessionRuntimeSchema, {
        shimBuildSha: deps.runtime.shimBuildSha,
        sdkVersion: deps.runtime.sdkVersion,
        agentBinaryVersion: runtime.agentBinaryVersion,
      }),
      effectiveModel: create(conversationv1.AgentModelSchema, { name: effectiveModel }),
      permissionMode,
      modelCatalog,
      liveWork,
    });
    // KEPT SO A LATER WATCH CAN BE TOLD. A daemon that adopts an already-started
    // shim (crash boot, handover) was not there for this announcement, and the
    // shim's own state is the only place it survives: WatchSession re-states it
    // once per watch, with the live membership refreshed to NOW (landing 7).
    announcedStart = started_;
    LOGGER.log(
      {
        vendor_session_id: identity.vendorSessionId,
        original_vendor_session_id: identity.originalVendorSessionId,
        model: effectiveModel,
        live_work: liveWork.length,
      },
      "session started",
    );
    return startSessionStarted(started_);
  }

  type Remedy =
    | { kind: "none" }
    | { kind: "cleared"; vendorSessionId: string }
    | { kind: "failed"; detail: string };

  async function applyColdRemediation(
    remediation: conversationv1.SessionColdRemediation | undefined,
    vendorSessionId: string,
    facts: TranscriptFacts,
    requestedModel: string,
  ): Promise<Remedy> {
    if (remediation === undefined) return { kind: "none" };
    switch (remediation.remediation.case) {
      case "pay":
        LOGGER.log({ vendor_session_id: vendorSessionId }, "cold remediation: paying for the read");
        return { kind: "none" };
      case "clear": {
        const minted = mintVendorSessionId();
        LOGGER.log(
          { previous_vendor_session_id: vendorSessionId, vendor_session_id: minted },
          "cold remediation: CLEAR — binding a newly minted vendor session id, which discards the context and costs no API call; the AgentId is unaffected (R9)",
        );
        return { kind: "cleared", vendorSessionId: minted };
      }
      case "compact": {
        const outcome = await compact(
          vendorSessionId,
          facts,
          remediation.remediation.value.model?.name ?? requestedModel,
          remediation.remediation.value.scope,
        );
        return outcome.ok ? { kind: "none" } : { kind: "failed", detail: outcome.error };
      }
      default:
        return { kind: "none" };
    }
  }

  /**
   * Compaction, on a THROWAWAY query.
   *
   * The live query owns the session's identity, its lock and its in-flight
   * turn; handing it a summarization prompt would put harness work inside the
   * user's conversation.
   */
  async function compact(
    vendorSessionId: string,
    facts: TranscriptFacts,
    model: string,
    scope: conversationv1.SessionCompactScope,
  ): Promise<{ ok: true; summary: string } | { ok: false; error: string }> {
    const transcript = transcriptPath(deps.env.configDir, deps.env.cwd, vendorSessionId);
    const startedAtMs = deps.nowMs();
    const queue = new PromptQueue();
    const controller = new AbortController();
    let throwaway: QueryLike | undefined;
    try {
      throwaway = await deps.createQuery({
        binding: { kind: "resume", resumeSessionId: vendorSessionId },
        permissionMode: "plan",
        canUseTool: (() => Promise.resolve({ behavior: "deny", message: "compaction takes no tools" })),
        abortController: controller,
        prompt: queue,
        ...(model === "" ? {} : { model }),
      });
      queue.push({
        type: "user",
        message: { role: "user", content: compactionPrompt(scope) },
        parent_tool_use_id: null,
      });
      let summary = "";
      let outputTokens = 0;
      for await (const message of throwaway) {
        if (message.type !== "result") continue;
        if (message.subtype !== "success") {
          return { ok: false, error: `the summarizing session ended as ${message.subtype}` };
        }
        summary = message.result;
        outputTokens = message.usage.output_tokens;
        break;
      }
      if (summary === "") return { ok: false, error: "the summarizing session produced no summary" };
      const durationMs = deps.nowMs() - startedAtMs;
      appendCompactionLines(
        transcript,
        compactionLines({
          ambient: readAmbient(transcript),
          summary,
          preTokens: facts.contextTokens,
          // What remains in context after compaction IS the summary, so the
          // summary's own output tokens are the honest "after" figure. Nothing
          // in the declared surface reports a post-compaction context size for
          // a compaction the shim performed.
          postTokens: outputTokens,
          durationMs,
          trigger: "manual",
          atMs: deps.nowMs(),
          // THE SESSION'S OWN MODE, NOT THE THROWAWAY'S. The summarizing query
          // above runs under `plan` and resumes the user's vendor session id,
          // so the last `permissionMode` the transcript states is `plan` — and
          // a resume restores the conversation's posture from that field. The
          // summary line is the LAST record this compaction appends, so
          // stating the session's own mode there is what keeps a revival from
          // coming back in a mode nobody chose.
          permissionMode: toVendorPermissionMode(permissionMode),
        }),
      );
      writeContextCut(
        contextCutCompacted({
          summary,
          tokensBefore: facts.contextTokens,
          tokensAfter: outputTokens,
          durationMs,
          requested: true,
        }),
      );
      return { ok: true, summary };
    } catch (err) {
      const error = err instanceof Error ? err.message : String(err);
      writeContextCut(contextCutFailed(error));
      return { ok: false, error };
    } finally {
      queue.close();
      controller.abort();
      throwaway?.close();
    }
  }

  /**
   * Conclude the open turn with a failure terminal, because the query died.
   *
   * `execution_error` and not an invented arm: the vendor stated no reason --
   * its process is simply gone -- and `errors` carries the account. Nothing is
   * written when no turn was open: a session that lost its query between turns
   * has no turn to conclude.
   */
  function writeQueryDeathTerminal(detail: string): void {
    const ended = open;
    if (ended === undefined || identity === undefined) return;
    const agentId = identity.agentId;
    const coordinate = `query-died-${ended.id.value}`;
    deps.persistence.write([
      {
        agentId,
        upsertKey: terminalUpsertKey(agentId, coordinate),
        source: {
          vendorUuid: `${coordinate}-${identity.vendorSessionId}`,
          discriminator: "agent_frame.failure.execution_error",
        },
        keepalive: ended.keepalive,
        item: {
          kind: "frame",
          frame: create(conversationv1.AgentFrameSchema, {
            agentId,
            result: {
              case: "failure",
              value: create(conversationv1.AgentFailureSchema, {
                errors: [detail],
                failure: {
                  case: "executionError",
                  value: create(conversationv1.AgentExecutionErrorSchema, {}),
                },
              }),
            },
          }),
        },
      },
    ]);
    LOGGER.log(
      { level: "warn", turn_id: ended.id.value, cause: detail },
      "concluded the open turn with a failure terminal: the vendor query died under it",
    );
  }

  /**
   * Conclude the open turn as INTERRUPTED BY HOST SHUTDOWN, because the session
   * is being torn down under it.
   *
   * EVERY STARTED THING EVENTUALLY GETS A TERMINAL ROW, and a turn is a started
   * thing. Without this a shim that was SIGTERMed or force-killed mid-turn left
   * the turn open in the record forever: nothing else ever writes it, because
   * the vendor's own interrupt terminal arrives after the stream this teardown
   * is closing.
   *
   * `host_shutdown` AND NOT `by_user`: nobody chose this. The distinction is
   * load-bearing for recovery — a user stop is a decision and the conversation
   * waits, while a host shutdown is an accident and the work is expected to be
   * driven again when the host returns — so reporting the accident as a
   * decision tells the user they stopped something they never touched.
   */
  function writeHostShutdownTerminal(reason: string): void {
    const ended = open;
    if (ended === undefined || identity === undefined) return;
    const agentId = identity.agentId;
    const coordinate = `host-shutdown-${ended.id.value}`;
    deps.persistence.write([
      {
        agentId,
        upsertKey: terminalUpsertKey(agentId, coordinate),
        source: {
          vendorUuid: `${coordinate}-${identity.vendorSessionId}`,
          discriminator: "agent_frame.success.interrupted.host_shutdown",
        },
        keepalive: ended.keepalive,
        item: {
          kind: "frame",
          frame: create(conversationv1.AgentFrameSchema, {
            agentId,
            result: {
              case: "success",
              value: create(conversationv1.AgentSuccessSchema, {
                outcome: {
                  case: "interrupted",
                  value: create(conversationv1.AgentInterruptedSchema, {
                    cause: {
                      case: "hostShutdown",
                      value: create(conversationv1.AgentInterruptedByHostShutdownSchema, {}),
                    },
                  }),
                },
              }),
            },
          }),
        },
      },
    ]);
    LOGGER.log(
      { level: "warn", turn_id: ended.id.value, reason },
      "concluded the open turn as interrupted by host shutdown: the session is being torn down under it",
    );
  }

  /** The `context_cut` page line, on the conversation's own book. */
  function writeContextCut(cut: conversationv1.ContextCut): void {
    if (identity === undefined) {
      // NEVER DROPPED, only DEFERRED. A row needs the agent id the identity
      // settles, and the cold gate's remediation runs before that — so the cut
      // waits rather than vanishing.
      LOGGER.log(
        { arm: cut.cut.case ?? "", pending: pendingContextCuts.length + 1 },
        "a context cut was produced before the session had an identity; held until one exists",
      );
      pendingContextCuts.push(cut);
      return;
    }
    const agentId = identity.agentId;
    const entry: PersistEntry = {
      agentId,
      upsertKey: terminalUpsertKey(agentId, `context-cut-${deps.nowMs()}`),
      source: {
        vendorUuid: `context-cut-${identity.vendorSessionId}-${deps.nowMs()}`,
        discriminator: `agent_frame.update.context_cut.${cut.cut.case ?? ""}`,
      },
      keepalive: false,
      item: {
        kind: "frame",
        frame: create(conversationv1.AgentFrameSchema, {
          agentId,
          result: {
            case: "update",
            value: create(conversationv1.AgentUpdateSchema, {
              update: { case: "contextCut", value: cut },
            }),
          },
        }),
      },
    };
    deps.persistence.write([entry]);
  }

  /**
   * How much of the book a reconciliation reads to describe live work.
   *
   * Generous rather than exact: the descriptions it needs are the STARTS of
   * units that are still open, which are near the end of the book by
   * definition, and a budget too small would silently leave live work
   * undescribed — which reads to a consumer as work that does not exist.
   */
  const RECONCILE_PAGE_SIZE = 512;

  /**
   * GetLiveWork reconciliation.
   *
   * RE-ADOPT what the revived vendor process actually has, and WRITE THE
   * CLOSING TERMINAL for what did not survive. The invariant it protects: every
   * started thing eventually gets a terminal row, by observation or by
   * reconciliation.
   */
  async function reconcile(): Promise<conversationv1.AgentDetachedWork[]> {
    let open;
    try {
      open = await deps.persistence.liveWork();
    } catch (err) {
      pushes.fault(
        sessionFault(
          { kind: "storeUnreachable" },
          "shim-engine-session",
          `GetLiveWork failed: ${err instanceof Error ? err.message : String(err)}`,
        ),
      );
      return [];
    }
    const closing: PersistEntry[] = [];
    const agentId = requireIdentity().agentId;

    // THE RECORD IS THE ONLY PLACE the work's own start survives a bounce, so
    // the book is read ONCE and every description below comes out of it. The
    // page session's tail is closed at once: this is a read, not a follow.
    let book: readonly conversationv1.HistoryEntryAt[] = [];
    if (open.liveDetached.length > 0 || open.liveAgents.length > 0) {
      let page;
      try {
        page = await deps.persistence.openAgentPage(agentId, RECONCILE_PAGE_SIZE);
        book = page.page.entries;
      } catch (err) {
        LOGGER.log(
          { level: "warn", cause: err instanceof Error ? err.message : String(err) },
          "the book could not be read for reconciliation; live work cannot be described",
        );
      } finally {
        page?.close();
      }
    }

    // RE-ADOPTED WORK IS ANNOUNCED `created`, NEVER `detached`: a daemon that
    // restarted was not there for the original announcement and has no element
    // to continue, so it must be told what the work IS.
    // A HANDLE NAMES THE SPAWNING CALL, so "does the revived vendor still have
    // it" is a lookup by tool_use_id and never by the vendor's own task id.
    //
    // ASK THE VENDOR, do not wait to be told. The live table is built from
    // messages the shim has ALREADY seen, and at StartSession it has seen
    // almost none -- a revived process announces its surviving tasks on its own
    // schedule, after the init this reconciliation follows. Judging survival
    // off that table alone therefore swept up work the vendor still had, and
    // wrote a lost.swept_up terminal over a run that was still producing.
    // `backgroundTasks(handle)` is the same declared observation DetachForeground
    // uses, and it answers now.
    const active = query;
    const surviving = new Set<string>();
    for (const work of open.liveDetached) {
      if (live.byToolUseId(work.value) !== undefined) {
        surviving.add(work.value);
        continue;
      }
      if (active === undefined) continue;
      try {
        if (await active.backgroundTasks(work.value)) surviving.add(work.value);
      } catch (err) {
        // A vendor that cannot answer is not a vendor that said "gone": leaving
        // the item to be swept would close a run that may still be producing.
        LOGGER.log(
          { level: "warn", work_id: work.value, cause: err instanceof Error ? err.message : String(err) },
          "the vendor could not be asked whether it still holds this work; treating it as surviving",
        );
        surviving.add(work.value);
      }
    }
    const survives = (work: conversationv1.DetachedWorkId): boolean =>
      surviving.has(work.value);
    const readopted = announceLiveWork(book, open.liveDetached.filter(survives));

    for (const work of open.liveDetached) {
      if (survives(work)) continue;
      // The vendor no longer has it and nobody stopped it: we simply stopped
      // being able to see it, which is what `lost.swept_up` says.
      //
      // THE KIND COMES FROM WHERE THE WORK WAS FOUND (ruling, landing 5).
      // `live_detached` is the store's NON-AGENT detached table — shell runs —
      // so a row there is a shell run whether or not the book still describes
      // its start, and a spawn unit found in the book is a spawn. NOTHING IS
      // EVER LEFT OPEN: an obligation the shim declines to close is one that
      // never gets a terminal at all, which breaks the whole invariant this
      // reconciliation exists to hold.
      const run = toolCallActivityId(work.value);
      const item = findUnit(book, run);
      if (item?.case === "subagent") {
        closing.push(closingSubagentTerminal(agentId, run));
        continue;
      }
      const recorded = findBashStart(book, run);
      if (recorded === undefined) {
        LOGGER.log(
          { level: "warn", work_id: work.value, kind: item?.case ?? "" },
          "the record holds no describable start for this live shell run; closing it as swept up with no command stated",
        );
      }
      closing.push(
        closingBashTerminal(
          agentId,
          run,
          recorded ??
            // AN EMPTY LINE IS THE RECORD SAYING IT NEVER SAW ONE, which is
            // exactly the situation; a terminal naming a command nobody
            // observed would be the invention. The WARN above names the run so
            // the gap is investigable rather than merely present.
            create(conversationv1.AgentBashStartSchema, {
              command: create(conversationv1.AgentBashCommandSchema, { line: "" }),
            }),
        ),
      );
    }
    for (const agent of open.liveAgents) {
      if (agent.value === agentId.value) continue;
      // NAMED BY THE RECORD, so a consumer may address it even though this
      // process never watched it start -- that is what makes an empty book
      // under this id "not written yet" rather than "no such agent".
      announcedAgents.add(agent.value);
      closing.push(closingAgentTerminal(subagentId(agent.value)));
    }
    if (open.liveWorkflows.length > 0) {
      LOGGER.log(
        { level: "warn", live_workflows: open.liveWorkflows.length },
        "GetLiveWork reports live workflow runs; WORKFLOW IS KICKED this wave, so they are neither re-adopted nor closed",
      );
    }
    if (closing.length > 0) deps.persistence.write(closing);
    LOGGER.log(
      { readopted: readopted.length, closed: closing.length },
      "reconciled the store's open obligations against what the vendor still has",
    );
    return readopted;
  }

  // -- keep-alive -----------------------------------------------------------

  async function keepaliveBeat(): Promise<void> {
    if (open !== undefined || query === undefined || identity === undefined) return;
    // A keep-alive turn's id NEVER reaches the wire: TurnIds are daemon-minted
    // and adopted, and this turn has no daemon behind it. The value exists only
    // so the prompt row has a key, and the row is flagged keep-alive so no page
    // ever serves it.
    const turn = create(conversationv1.TurnIdSchema, {
      value: `keepalive-${deps.nowMs()}-${process.pid}`,
    });
    const said = textSaid(keepalivePromptText());
    open = { id: turn, keepalive: true, startedAtMs: deps.nowMs() };
    try {
      const prompt = buildPrompt(
        turn,
        identity.agentId,
        said,
        conversationv1.PromptOrigin.UNSPECIFIED,
      );
      deps.persistence.write([promptEntry(prompt, identity.agentId, true)]);
      await submit(said, true);
      LOGGER.log(
        { turn: turn.value, outcome: "keepalive_submitted" },
        "submitted one of the shim's own keep-alive prompts; its rows are recorded and never served",
      );
    } catch (err) {
      open = undefined;
      pushes.fault(
        sessionFault(
          { kind: "keepaliveFailed" },
          "shim-engine-keepalive",
          err instanceof Error ? err.message : String(err),
        ),
      );
    }
  }

  // -- the SessionContext the turn verbs run against ------------------------

  const context: SessionContext = {
    persistence: deps.persistence,
    gate,
    live,
    foreground,
    identity: () => identity,
    query: () => query,
    nowMs: deps.nowMs,
    openTurn: () => open,
    submit,
    setOpenTurn: (turn) => {
      open = turn;
      if (turn === undefined) cadence?.resume();
      else cadence?.pause();
    },
    reportStoreUnreachable: (detail) => {
      pushes.fault(sessionFault({ kind: "storeUnreachable" }, "shim-store-reader", detail));
    },
    watcherOpened: (agent, page) => {
      let settle: () => void = () => undefined;
      const entry: OpenWatcher = {
        agent,
        page,
        ended: new Promise<void>((resolve) => {
          settle = resolve;
        }),
      };
      watchers.add(entry);
      return () => {
        watchers.delete(entry);
        settle();
      };
    },
    knowsAgent: (agent) => knowsAgent(agent),
    concludeStoppedRuns: (entries) => {
      concludeStoppedRuns(entries);
    },
    bashWatcherOpened: (work) => {
      let settle: () => void = () => undefined;
      const entry: OpenBashWatcher = {
        work,
        ended: new Promise<void>((resolve) => {
          settle = resolve;
        }),
      };
      bashWatchers.add(entry);
      return () => {
        bashWatchers.delete(entry);
        settle();
      };
    },
  };
  const turns = new TurnEngine(context);

  // -- the remaining session verbs ------------------------------------------

  async function setSessionModel(
    request: shimv1.SetSessionModelRequest,
  ): Promise<shimv1.SetSessionModelResponse> {
    if (!started || identity === undefined) {
      return setSessionModelRefused({ kind: "noSession" }, "no session has been started on this shim");
    }
    const model = request.model;
    if (model === undefined) throw new Error("shim session: SetSessionModel reached the engine with no model");
    if (modelCatalog.length > 0 && !modelCatalog.some((option) => option.model?.name === model.name)) {
      return setSessionModelRefused(
        { kind: "modelNotInCatalog" },
        `${JSON.stringify(model.name)} is not in this session's model catalog`,
      );
    }
    // A MODEL SWITCH IS A COLD CACHE — the cache is per model — so the refusal
    // is immediate and above the caller's own threshold, not a warning after.
    const facts = readTranscriptFacts(
      transcriptPath(deps.env.configDir, deps.env.cwd, identity.vendorSessionId),
    );
    if (
      facts !== undefined &&
      model.name !== effectiveModel &&
      BigInt(facts.contextTokens) > request.coldThresholdTokens &&
      request.coldRemediation === undefined
    ) {
      LOGGER.log(
        { level: "warn", model: model.name, context_tokens: facts.contextTokens },
        "REFUSED SetSessionModel: switching model is a cold cache above the caller's threshold",
      );
      return setSessionModelRefused(
        { kind: "cold", cold: sessionCold(facts, "model_switch", model.name) },
        `switching to ${model.name} discards a ${facts.contextTokens}-token warm cache`,
      );
    }
    if (open !== undefined) {
      // ONE MODEL PER TURN, a deliberate departure from the SDK's mid-turn
      // setModel: a turn that changed model halfway would have two models'
      // pricing and two models' behavior in one answer.
      //
      // AND THE CALL DOES NOT RESOLVE UNTIL IT HAS. Answering now would tell
      // the caller the model is in effect while the running turn is still
      // answering on the old one -- so the next turn-end's own
      // `context_usage.model` would contradict the ack it already had. The
      // response waits for the boundary that makes it true.
      LOGGER.log({ model: model.name, turn_id: open.id.value }, "model change accepted; it resolves at the turn boundary");
      return new Promise<shimv1.SetSessionModelResponse>((resolve) => {
        // A second SetSessionModel during one turn REPLACES the first, and the
        // first caller is told so rather than left holding a promise nothing
        // will ever settle.
        pendingModel?.resolve(
          setSessionModelRefused(
            { kind: "vendorRefused" },
            "a later SetSessionModel replaced this one before the turn ended",
          ),
        );
        pendingModel = { model, resolve };
      });
    }
    try {
      await applyModel(model);
    } catch (err) {
      return setSessionModelRefused(
        { kind: "vendorRefused" },
        err instanceof Error ? err.message : String(err),
      );
    }
    return create(shimv1.SetSessionModelResponseSchema, {
      result: {
        case: "success",
        value: create(shimv1.SetSessionModelSuccessSchema, {
          modelChanged: create(conversationv1.SessionModelChangedSchema, { effectiveModel: model }),
        }),
      },
    });
  }

  async function setSessionPermissionMode(
    request: shimv1.SetSessionPermissionModeRequest,
  ): Promise<shimv1.SetSessionPermissionModeResponse> {
    const active = query;
    if (!started || active === undefined) {
      return setSessionPermissionModeRefused(
        { kind: "noSession" },
        "no session has been started on this shim",
      );
    }
    const mode = request.permissionMode;
    if (mode === undefined) {
      throw new Error("shim session: SetSessionPermissionMode reached the engine with no mode");
    }
    try {
      await active.setPermissionMode(toVendorPermissionMode(mode));
    } catch (err) {
      return setSessionPermissionModeRefused(
        { kind: "vendorRefused" },
        err instanceof Error ? err.message : String(err),
      );
    }
    permissionMode = mode;
    pushPermissionMode();
    LOGGER.log({ permission_mode: mode.mode.case ?? "" }, "changed the session permission mode");
    return create(shimv1.SetSessionPermissionModeResponseSchema, {
      result: { case: "success", value: create(shimv1.SetSessionPermissionModeSuccessSchema, {}) },
    });
  }

  async function hibernate(): Promise<shimv1.HibernateResponse> {
    if (!started || identity === undefined) {
      return hibernateRefused({ kind: "noSession" });
    }
    if (open !== undefined) {
      return hibernateRefused({ kind: "turnInFlight" });
    }
    const facts = readTranscriptFacts(
      transcriptPath(deps.env.configDir, deps.env.cwd, identity.vendorSessionId),
    );
    if (facts === undefined) {
      return hibernateRefused({
        kind: "compactionFailed",
        error: "there is no transcript to compact",
      });
    }
    const outcome = await compact(
      identity.vendorSessionId,
      facts,
      effectiveModel,
      conversationv1.SessionCompactScope.ALL,
    );
    if (!outcome.ok) {
      return hibernateRefused({ kind: "compactionFailed", error: outcome.error });
    }
    LOGGER.log({ vendor_session_id: identity.vendorSessionId }, "hibernated: the session is compacted and the daemon may stand this shim down");
    return hibernateAcked();
  }

  /**
   * The session's opening, RE-STATED for a watch that just attached.
   *
   * Identity, runtime, model, mode and catalog are the ORIGINAL facts: they are
   * fixed for the session, which is why an opening states them at all. The live
   * membership is NOT — a daemon adopting a running shim needs what is live NOW,
   * not what was live when the session opened — so `turn_in_flight` and
   * `live_work` are recomputed here.
   *
   * `created`-origin, exactly as the reconciliation at StartSession announces
   * re-adopted work: a consumer that was never there for the original
   * announcement has no element to continue and must be told what the work IS.
   *
   * UNSET before StartSession, which is the one state with nothing to re-state.
   */
  async function reannounceStart(): Promise<conversationv1.SessionStarted | undefined> {
    if (announcedStart === undefined) return undefined;
    return create(conversationv1.SessionStartedSchema, {
      vendorSessionId: announcedStart.vendorSessionId,
      ...(announcedStart.runtime === undefined ? {} : { runtime: announcedStart.runtime }),
      ...(announcedStart.effectiveModel === undefined
        ? {}
        : { effectiveModel: announcedStart.effectiveModel }),
      ...(announcedStart.permissionMode === undefined
        ? {}
        : { permissionMode: announcedStart.permissionMode }),
      modelCatalog: announcedStart.modelCatalog,
      ...(open === undefined ? {} : { turnInFlight: open.id }),
      liveWork: await announceLiveWorkNow(),
    });
  }

  /**
   * Every detached item live RIGHT NOW, described from the record.
   *
   * A READ AND NOTHING ELSE. The StartSession reconciliation writes terminals
   * for work the vendor no longer holds; re-announcing must never do that — a
   * daemon attaching is not a reason to close anybody's run — so this shares
   * only the pure description step with it.
   *
   * THE STORE'S LIVE SET IS THE ANSWER, not the in-memory table: work this
   * process re-adopted at StartSession was never seen to START here, so the
   * table does not hold it, and a re-announcement built from the table alone
   * would tell an adopting daemon that a running shell does not exist.
   */
  async function announceLiveWorkNow(): Promise<conversationv1.AgentDetachedWork[]> {
    let page: AgentPageSession | undefined;
    try {
      const handles = (await deps.persistence.liveWork()).liveDetached;
      if (handles.length === 0) return [];
      page = await deps.persistence.openAgentPage(requireIdentity().agentId, RECONCILE_PAGE_SIZE);
      return announceLiveWork(page.page.entries, handles);
    } catch (err) {
      // LOUD, NEVER SILENT: the watch still opens — a consumer told nothing at
      // all is worse off than one told the opening with an empty membership —
      // but the record plane being unreachable is a session-level fact every
      // consumer is entitled to, so it goes out as the fault it is.
      const detail = err instanceof Error ? err.message : String(err);
      LOGGER.log(
        { level: "error", cause: detail },
        "the record plane could not be read to re-announce the live membership for a new watch",
      );
      pushes.fault(
        sessionFault(
          { kind: "storeUnreachable" },
          "shim-engine-session",
          `re-announcing live work for a new WatchSession failed: ${detail}`,
        ),
      );
      return [];
    } finally {
      page?.close();
    }
  }

  function sessionLive(): conversationv1.SessionLive {
    const announceable = live.announceable();
    return create(conversationv1.SessionLiveSchema, {
      ...(open === undefined ? {} : { turnInFlight: open.id }),
      liveWork: live.workIds(announceable),
    });
  }

  async function killSession(
    request: shimv1.KillSessionRequest,
  ): Promise<shimv1.KillSessionResponse> {
    if (!started) {
      return killSessionRefused({ kind: "noSession" }, "no session has been started on this shim");
    }
    const announceable = live.announceable();
    const busy = open !== undefined || announceable.length > 0;
    if (busy && !request.force) {
      LOGGER.log(
        { level: "warn", turn_in_flight: open?.id.value ?? "", live_work: announceable.length },
        "REFUSED KillSession: the session is live and force was not set",
      );
      return killSessionRefused(
        { kind: "live", live: sessionLive() },
        `the session has ${open === undefined ? "no turn" : `turn ${open.id.value}`} in flight and ${announceable.length} live item(s)`,
      );
    }
    const interruptedTurn = open?.id;
    const stopped = live.workIds(live.all());
    await teardown("KillSession");
    const killed = create(conversationv1.SessionKilledSchema, {
      how: busy
        ? {
            case: "forced",
            value: create(conversationv1.SessionKilledForcedSchema, {
              ...(interruptedTurn === undefined ? {} : { interruptedTurn }),
              stoppedWork: stopped,
            }),
          }
        : { case: "idle", value: create(conversationv1.SessionKilledIdleSchema, {}) },
    });
    LOGGER.log({ forced: busy, stopped: stopped.length }, "killed the session");
    endProcess();
    return killSessionClosed(killed);
  }

  /**
   * Ask `main.ts` to end the process, now that the session is torn down.
   *
   * Called AFTER the response message is built and BEFORE it is returned, so
   * the exit is already requested when the handler resolves; `main.ts` is the
   * half that waits for the wire to go quiet before actually exiting.
   */
  function endProcess(): void {
    const code = lostRowsAtStandDown > 0 ? 1 : 0;
    if (deps.endProcess === undefined) {
      LOGGER.log(
        { level: "warn", exit_code: code },
        "the session ended with no process to end; this build drives the engine in-process",
      );
      return;
    }
    deps.endProcess(code);
  }

  /**
   * The teardown, in the one order that works.
   *
   * Callbacks first (an unresolved `canUseTool` wedges the vendor), then the
   * live work, then the query, then the store. Ending the query first would
   * abandon writes the record needs.
   */
  async function teardown(reason: string): Promise<void> {
    if (standingDown) return;
    standingDown = true;
    gate.standDown(reason);
    // A CALL WAITING ON A TURN BOUNDARY THAT WILL NEVER COME still gets an
    // answer: leaving the daemon holding a promise nothing can settle is worse
    // than telling it the change did not land.
    if (pendingModel !== undefined) {
      const waiting = pendingModel;
      pendingModel = undefined;
      waiting.resolve(
        setSessionModelRefused(
          { kind: "vendorRefused" },
          `the session stood down before the turn ended (${reason}); the model change did not land`,
        ),
      );
    }
    cadence?.stop();
    if (accountUsageHandle !== undefined) {
      (deps.scheduler ?? REAL_SCHEDULER).clearInterval(accountUsageHandle);
      accountUsageHandle = undefined;
    }
    const active = query;
    if (active !== undefined) {
      try {
        await active.interrupt();
      } catch (err) {
        LOGGER.log(
          { level: "warn", cause: err instanceof Error ? err.message : String(err) },
          "the vendor refused the interrupt during teardown; continuing",
        );
      }
      // SNAPSHOT FIRST. Stopping a task provokes the vendor's own
      // `task_notification`, which retires the entry from the live table — so
      // reading the table again afterwards asks what is STILL live and gets
      // exactly the items this teardown did not have to close.
      const stopping = live.all();
      for (const entry of stopping) {
        try {
          await active.stopTask(entry.taskId);
        } catch (err) {
          LOGGER.log(
            { level: "warn", task_id: entry.taskId, cause: err instanceof Error ? err.message : String(err) },
            "could not stop a detached item during teardown; continuing",
          );
        }
      }
      concludeStoppedRuns(stopping);
    }
    // BEFORE `open` IS CLEARED: the terminal names the turn, and a teardown
    // that forgot the turn first would have nothing to write it for.
    writeHostShutdownTerminal(reason);
    open = undefined;
    prompts?.close();
    abort?.abort();
    active?.close();
    query = undefined;
    // BOUNDED, BECAUSE A CLOSED QUERY IS NOT A PROMISE THAT SETTLES. `close()`
    // is the vendor's own end-of-stream signal, but nothing in the SDK's
    // declared surface guarantees the iterator this loop is parked in ever
    // completes after one -- and a compaction that already closed a query on
    // this conversation is exactly the shape that leaves one parked forever.
    // The stand-down cannot be the thing that never finishes, so the wait is
    // given the same budget a tail's conclusion gets and the overrun is stated
    // at error level rather than swallowed.
    const ending = loop;
    loop = undefined;
    if (ending !== undefined) {
      await withBudget(
        ending.catch(() => undefined),
        watcherConclusionBudgetMs,
        "the vendor message loop did not end within its budget after the query was closed; standing down without it",
      );
    }
    // A23: the stand-down is only clean if the record actually landed. The
    // lost-row count decides the exit code, and a nonzero one is stated here
    // as well as in the writer's own drop records, so the reason a stand-down
    // exited nonzero is readable without correlating two logs.
    const flushed = await deps.persistence.flush();
    lostRowsAtStandDown = flushed.lostRows;
    if (flushed.lostRows > 0) {
      LOGGER.log(
        { level: "error", reason, lost_rows: flushed.lostRows },
        "stood the session down with rows the store never acked; the record is incomplete",
      );
    }
    await concludeWatchers();
    await concludeBashWatchers();
    pushes.standDown();
    await releaseLock?.();
    releaseLock = undefined;
    await releaseWorkspaceLock?.();
    releaseWorkspaceLock = undefined;
  }

  /**
   * Write the interrupted terminal for every shell run this teardown stopped.
   *
   * THE ACT WAS OURS, SO THE RECORD IS OURS. A detached shell's rows are
   * normally the sidecar's, read off the spool — but a run stopped as the
   * session dies may have no sidecar left to read the spool's `EXIT=` line, and
   * "every started thing eventually gets a terminal row" is unconditional.
   * Re-writing the same upsert key is absorbed, so a sidecar that does see the
   * line later cannot produce a second, conflicting terminal.
   *
   * A SUBAGENT IS NOT A SHELL: its terminal is the spawn unit's, written where
   * subagent terminals are written, so only shell work is closed here.
   */
  function concludeStoppedRuns(entries: readonly LiveWorkEntry[]): void {
    const agent = identity?.agentId;
    if (agent === undefined) return;
    const closing: PersistEntry[] = [];
    for (const entry of entries) {
      const handle = entry.toolUseId;
      if (handle === undefined || handle === "") {
        // Tracked for liveness, addressable by nobody: there is no unit to
        // settle, and inventing one would put work on a stream that never
        // announced it.
        LOGGER.log(
          { level: "warn", task_id: entry.taskId },
          "a stopped detached item names no originating call; it has no unit to settle",
        );
        continue;
      }
      // THE SAME RULE THE FOLD USES (`settlesAsSubagent`): `task_started`
      // states `local_agent` for a spawned agent and `local_bash` for a shell,
      // and an UNSTATED kind is an agent. Only a stated non-agent kind is a
      // shell run, and only a shell run's terminal is ours to write.
      if (entry.taskType === undefined || entry.taskType === "" || entry.taskType === "local_agent") {
        continue;
      }
      closing.push(
        stoppedBashTerminal(
          agent,
          toolCallActivityId(handle),
          create(conversationv1.AgentBashCommandSchema, { line: entry.description }),
        ),
      );
    }
    if (closing.length === 0) return;
    deps.persistence.write(closing);
    LOGGER.log({ closed: closing.length }, "closed the shell runs this teardown stopped, as interrupted by the user");
  }

  /**
   * Wait for every open `WatchBash` stream to end.
   *
   * A bash stream concludes ITSELF once the terminal row reaches it — the store
   * ends `WatchBashRun` after a terminal — so there is nothing to conclude
   * here, only something to WAIT FOR. Exiting the process first would cut the
   * stream exactly where its terminal was owed, which a consumer reads as a
   * transport failure rather than as the interrupted arm it was promised.
   */
  async function concludeBashWatchers(): Promise<void> {
    const open = [...bashWatchers];
    if (open.length === 0) return;
    await Promise.all(
      open.map((entry) =>
        withBudget(
          entry.ended,
          watcherConclusionBudgetMs,
          `the WatchBash stream on ${entry.work.value} did not end within its conclusion budget`,
        ),
      ),
    );
    bashWatchers.clear();
  }

  /**
   * End every open `WatchAgent` tail, after it has served what the teardown
   * wrote.
   *
   * The order is the whole point: the terminals are already DURABLE (the flush
   * above returned), so the book's head names the last row a consumer is owed.
   * Each tail is concluded through it and then awaited, so the interrupted
   * terminal reaches the consumer before the stream ends — and before the
   * process does. Cutting the tails instead would surface to the daemon as a
   * transport failure exactly where a terminal was expected.
   */
  async function concludeWatchers(): Promise<void> {
    const open = [...watchers];
    if (open.length === 0) return;
    await Promise.all(
      open.map(async (entry) => {
        try {
          entry.page.concludeThrough(await bookHead(entry.agent));
        } catch (err) {
          // The head could not be read, so there is no pointer to conclude
          // through; ending the tail now is still better than cutting it later.
          LOGGER.log(
            { level: "warn", agent: entry.agent.value, cause: err instanceof Error ? err.message : String(err) },
            "could not read a book's head while concluding its watcher; ending the tail unbounded",
          );
          entry.page.concludeThrough(undefined);
        }
        await withBudget(
          entry.ended,
          watcherConclusionBudgetMs,
          `the WatchAgent tail on ${entry.agent.value} did not end within its conclusion budget`,
        );
      }),
    );
    watchers.clear();
  }

  /**
   * Whether THIS session vouches for an agent id — the producer's own answer to
   * "does this name an agent at all".
   *
   * ONE PREDICATE, used by every caller that needs it. The record plane treats
   * it as the licence to serve a book the store holds no rows for, so a second
   * hand-rolled copy of these conditions would let two callers disagree about
   * whether the same id names an agent.
   */
  function knowsAgent(agent: conversationv1.AgentId): boolean {
    const value = agent.value;
    if (value === "") return false;
    if (identity !== undefined && value === identity.agentId.value) return true;
    if (live.byToolUseId(value) !== undefined) return true;
    if (live.retired(value)) return true;
    return announcedAgents.has(value);
  }

  /** The newest pointer in one agent's book, or absence when the book is empty. */
  async function bookHead(
    agent: conversationv1.AgentId,
  ): Promise<conversationv1.HistoryPointer | undefined> {
    // BOUNDED FOR THE SAME REASON THE CONCLUSION IS. This runs inside the
    // teardown, and the store call carries no deadline of its own: a store that
    // never answers would leave KillSession hanging with the tail unconcluded.
    // Rejecting hands the caller's own catch the honest outcome -- the head
    // could not be read -- instead of stalling the stand-down.
    const opened = await deadline(
      // THE PRODUCER VOUCHES HERE TOO. A session killed before its first turn
      // has an agent with no book, and asking the store for one earned an
      // `unknown_agent` refusal on every such teardown. The head of a book that
      // does not exist is absence, which is exactly what an empty page answers.
      deps.persistence.openAgentPage(agent, 1, undefined, () => knowsAgent(agent)),
      watcherConclusionBudgetMs,
      `the store did not answer for ${agent.value}'s book within ${watcherConclusionBudgetMs}ms`,
    );
    opened.close();
    return opened.page.entries[0]?.at;
  }

  /**
   * Await `work`, giving up loudly after `budgetMs`.
   *
   * A LAST RESORT and never the mechanism: the tail's own end settles this in
   * microseconds. It exists only so a consumer that stopped pulling its stream
   * cannot keep a killed shim alive forever.
   */
  /** The conclusion budget this session actually bounds its tails on. */
  const watcherConclusionBudgetMs =
    deps.watcherConclusionBudgetMs ?? WATCHER_CONCLUSION_BUDGET_MS;

  /**
   * Await `work`, REJECTING when `budgetMs` elapses first.
   *
   * The sibling of {@link withBudget}, for the awaits whose caller already has
   * an error path worth taking: this one surfaces the overrun as a failure the
   * caller handles, where `withBudget` merely gives up on work nobody is
   * waiting on an answer from.
   */
  async function deadline<T>(work: Promise<T>, budgetMs: number, complaint: string): Promise<T> {
    let timer: ReturnType<typeof setTimeout> | undefined;
    const expiry = new Promise<never>((_resolve, reject) => {
      timer = setTimeout(() => reject(new Error(complaint)), budgetMs);
      timer.unref?.();
    });
    try {
      return await Promise.race([work, expiry]);
    } finally {
      if (timer !== undefined) clearTimeout(timer);
    }
  }

  async function withBudget(work: Promise<void>, budgetMs: number, complaint: string): Promise<void> {
    let timer: ReturnType<typeof setTimeout> | undefined;
    const expiry = new Promise<"expired">((resolve) => {
      timer = setTimeout(() => resolve("expired"), budgetMs);
      timer.unref?.();
    });
    try {
      const outcome = await Promise.race([work.then(() => "ended" as const), expiry]);
      if (outcome === "expired") {
        LOGGER.log({ level: "error", budget_ms: budgetMs }, complaint);
      }
    } finally {
      if (timer !== undefined) clearTimeout(timer);
    }
  }

  const engine: SessionEngine = {
    pushes,
    onSdkMessage,
    startSession,
    // `subscribe()` runs EAGERLY here, at the open, not at the consumer's first
    // pull: the diagnostics frame is seeded into the subscriber's queue by that
    // call, and a generator that only subscribed on first `next()` would leave
    // the daemon unable to tell "not ready" from "refused".
    watchSession: () => watchSessionFrames(pushes.subscribe(), reannounceStart),
    setSessionModel,
    setSessionPermissionMode,
    hibernate,
    killSession,
    startTurn: (request) => turns.startTurn(request),
    watchAgent: (request) => turns.watchAgent(request),
    updateAgent: (request) => turns.updateAgent(request),
    killTurn: (request) => turns.killTurn(request),
    watchBash: (request) => turns.watchBash(request),
    stopBash: (request) => turns.stopBash(request),
    detachForeground: (request) => turns.detachForeground(request),
    readHistory: (request) => turns.readHistory(request),
    standDown: async (reason: string) => {
      await teardown(reason);
      return lostRowsAtStandDown > 0 ? 1 : 0;
    },
  };
  return engine;
}

/**
 * The watch's frames: the opening diagnostics, the re-announcement, then facts.
 *
 * THE DIAGNOSTICS GO FIRST AND ALONE. It is the readiness signal a consumer may
 * open this stream before anything else to receive, and connect surfaces a
 * server-stream refusal only at the first Receive — so nothing may be computed
 * ahead of it. The re-announcement follows it, on EVERY new watch, which is
 * what lets a daemon that adopts an already-started shim attach purely.
 *
 * A session that has not started yet re-announces nothing: there is no opening
 * to re-state, and the diagnostics already said so.
 */
async function* watchSessionFrames(
  updates: AsyncIterable<conversationv1.SessionUpdate>,
  reannounce: () => Promise<conversationv1.SessionStarted | undefined>,
): AsyncIterable<shimv1.WatchSessionResponse> {
  let opened = false;
  for await (const update of updates) {
    yield create(shimv1.WatchSessionResponseSchema, {
      frame: { case: "update", value: update },
    });
    if (opened) continue;
    opened = true;
    const started = await reannounce();
    if (started !== undefined) {
      yield create(shimv1.WatchSessionResponseSchema, {
        frame: { case: "sessionStarted", value: started },
      });
    }
  }
}
