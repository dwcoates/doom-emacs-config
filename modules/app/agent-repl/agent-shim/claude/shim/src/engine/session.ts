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
import { randomUUID } from "node:crypto";
import { create } from "@bufbuild/protobuf";
import { bindLog, clearRequestId, onLogSinkPoisoned, setClaudeSessionId, setRequestId } from "../log.js";
import { conversationv1, shimv1 } from "../proto.js";
import {
  acquireSessionLock,
  acquireWorkspaceLock,
  describeLockHolderHow,
  LockHeldError,
  LockHolderUnavailableError,
  workspaceLockPath,
} from "../locks.js";
import type { LockRelease } from "../locks.js";
import { workspaceLockKey } from "../locks.js";
import { recordAgentBinaryVersion, requireSessionRuntime } from "../build-identity.js";
import { subagentId, toolCallActivityId } from "../convert/ids.js";
import { hookBlockingText } from "../convert/hooks.js";
import { classifyVendorApiFailure, redactVendorMessage } from "../convert/terminals.js";
import { terminalUpsertKey } from "../store/keys.js";
import { PersistenceError } from "../store/persistence.js";
import type { AgentPageSession, PersistEntry, Persistence } from "../store/persistence.js";
import {
  announceLiveWork,
  closingAgentTerminal,
  closingBashTerminal,
  closingMonitorTerminal,
  findMonitorCall,
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
  titleDigestGathered,
  transcriptsRead,
  transcriptsRefused,
  titleDigestRefused,
} from "../service/failures.js";
import type { Engine } from "./engine.js";
import { readTitleDigest } from "./title-digest.js";
import { readTranscripts } from "./transcripts.js";
import type { EngineFold, FoldContext, LastChange } from "./fold-context.js";
import { normalizeModel, SYNTHETIC_MODEL } from "../model.js";
import { TRUST_KEY, VENDOR_CONFIG_FILE, trustRoot } from "../trust.js";
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
import { judgeCold, readTranscriptFacts, sessionCold, transcriptPath, underColdGateFloor, type TranscriptFacts } from "./cold.js";
import { TranscriptTitleTail } from "./title.js";
import { sessionTitleUpdate } from "../convert/session-title.js";
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
  KeepaliveScope,
  keepalivePromptText,
  REAL_SCHEDULER,
  type KeepaliveAttribution,
  type KeepaliveScheduler,
  type RewindObligation,
} from "./keepalive.js";
import {
  DEFAULT_PERMISSION_MODE,
  fromVendorPermissionMode,
  PermissionGate,
  toVendorPermissionMode,
} from "./permission-gate.js";
import { SessionPushes } from "./pushes.js";
import {
  KEEPALIVE_YIELD_BUDGET_MS,
  TurnEngine,
  promptEntry,
  buildPrompt,
  saidText,
  textSaid,
  type OpenTurn,
  type SessionContext,
} from "./turn.js";

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
  /**
   * Every chunk the vendor child writes to stderr.
   *
   * THE VENDOR'S OWN WORDS FOR ITS OWN FAILURE. A CLI that refuses a resume
   * says so on stderr and then simply never announces the session, so without
   * this the shim can only report the silence it observed, never the reason the
   * child printed. Optional: the mocked vendor has no child and no stderr.
   */
  readonly onStderr?: (chunk: string) => void;
  /**
   * How the vendor child ENDED: its exit code, or the signal that killed it.
   *
   * THE FACT A DEATH WAS OTHERWISE MISSING. When the query goes the shim has
   * the SDK's wording for the STREAM ending and nothing about the process, so
   * a vendor that died mid-morning on 2026-09-14 left three messages about
   * transports and nothing at all about why its process was gone. Optional:
   * the mocked vendor has no child to end.
   */
  readonly onChildExit?: (exit: { code: number | null; signal: string | null }) => void;
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
  /**
   * Mints the client uuid a keep-alive send carries, the one the vendor echoes
   * on every reply to it (engine/keepalive.ts, `KeepaliveScope`). Injected so a
   * suite can stamp its scripted replies; production mints a random one.
   */
  readonly newUuid?: () => string;
  /** Injected so a suite never waits on a clock. */
  readonly scheduler?: KeepaliveScheduler;
  /**
   * Announced when a hibernation compaction settles.
   *
   * Injected so a suite awaits the work the DAEMON does not await: the daemon
   * has a next sweep pass and a test has no clock to wait on. Production leaves
   * it absent; the compaction's own records are the operator's account.
   */
  readonly onHibernationCompactionSettled?: (outcome: { ok: boolean; error?: string }) => void;
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
  /** The LAST-RESORT bound on a child that answers nothing at all. */
  readonly initTimeoutMs?: number;
  /** How long StartSession waits for its one control round-trip to answer. */
  readonly liveSignalTimeoutMs?: number;
  /**
   * The keep-alive cadence, when something overrode the module constant.
   *
   * `main.ts` fills this ONLY for a `--fake` process; a real session always
   * beats on {@link KEEPALIVE_INTERVAL_MS}.
   */
  readonly keepaliveIntervalMs?: number;
  /**
   * How long a `StartTurn` waits behind the shim's own keep-alive, when
   * something overrode {@link KEEPALIVE_YIELD_BUDGET_MS}. A suite shortens it
   * to reach the bound's refusal; production always uses the constant.
   */
  readonly keepaliveYieldBudgetMs?: number;
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

/**
 * The LAST-RESORT bound on a child that answers NOTHING AT ALL.
 *
 * STARTSESSION ALWAYS ANSWERS, and six things settle it: the PROVEN-LIVE
 * SIGNAL (one control round-trip, {@link LIVE_SIGNAL_TIMEOUT_MS}); `init`, when
 * a vendor still announces one before that; a hook that BLOCKS before either
 * (which has blocked the session's own opening); a `result` carrying `is_error`
 * (the vendor refusing the opening in its own words); the query ENDING before
 * either (a child that exited, a stream that ended, an iterator that threw); or
 * this bound.
 *
 * SO THIS BOUND IS FOR SILENCE, AND ONLY SILENCE — and after the live signal
 * landed it is silence of a kind the round-trip's own 3s bound already catches,
 * which leaves this for the one case that has no bound of its own: a
 * `createQuery` that returned a child which then answers neither the control
 * request nor anything else, so nothing ever resolves or rejects. Every
 * conclusive answer is relayed the instant it lands, because waiting a bound
 * out on an answer already in hand is what let the DAEMON's 60s bound fire
 * first and name the shim instead of the vendor.
 *
 * Sized under that daemon bound on purpose: the shim knows WHY the start failed
 * and the daemon does not, so the shim must be the one that answers first.
 */
const INIT_TIMEOUT_MS = 45_000;

/**
 * How long one control round-trip may take before the start is refused.
 *
 * THE START SETTLES ON A PROVEN-LIVE SIGNAL, NOT ON `init`. Grounded 2026-09-13
 * against claude 2.1.220 AND 2.1.270, driven exactly as this shim drives them
 * (`--input-format stream-json`): the child emits NO `system:init` until a
 * first user turn reaches it, while answering control requests
 * (`supportedModels`, `supportedCommands`, `setPermissionMode`) in ~300ms
 * throughout. A start that waited for `init` before it would accept a prompt
 * therefore deadlocked on every real session and failed at the 45s bound.
 *
 * ONE ROUND-TRIP IS THE WHOLE PROOF: a child that answers a control request is
 * spawned, connected, and taking work — which is exactly what "a prompt can be
 * accepted" means, and what `endpoint_start_session.proto` says this verb
 * resolves on. `supportedModels()` is the one chosen because the opening has to
 * make it anyway: its answer IS `SessionStarted.model_catalog`, so the proof
 * costs no extra call.
 *
 * TEN TIMES THE OBSERVED HEALTHY MAX. ~300ms measured, 3s bounded: small enough
 * that a wedged child is reported in seconds rather than at the daemon's 60s,
 * wide enough that nothing healthy can reach it.
 */
const LIVE_SIGNAL_TIMEOUT_MS = 3_000;

/**
 * How many pre-`init` vendor messages a failed start names in its own record.
 *
 * A START THAT FAILS SAYS WHAT THE VENDOR DID SAY. The grounded case
 * (2026-09-13, workspace 2b81f45a724642ef) waited the whole bound out with two
 * messages already in hand — a `SessionStart:resume` hook that started and then
 * SUCCEEDED — and nothing in any log said so, so the only way to learn what the
 * vendor had emitted was to decode the store's frames by hand. The kinds are
 * bounded because the list is a diagnosis, not a transcript: everything the
 * vendor emitted is on the record plane already.
 */
const PRE_INIT_KINDS_KEPT = 24;

/**
 * How much of the vendor child's stderr a failed start may quote.
 *
 * The CLI names its own refusals there — "No conversation found with session ID
 * ..." is the shape this exists for — and that text is the ONE thing that turns
 * "the vendor did not send its init message" into an answer the reader can act
 * on. Bounded: a chatty child must not push a refusal detail past what any
 * consumer will render.
 */
const VENDOR_STDERR_KEPT = 2_000;

/**
 * The component name the log sink's own fault and degraded window carry.
 *
 * PERMANENT BY NATURE. Nothing restores a lost record, so this component has
 * no recovery path and is the one fault meant to stand for the shim's life.
 */
const LOG_SINK_COMPONENT = "log-sink";

/**
 * ONE COMPONENT PER OPERATION THAT CAN FAULT, and why that matters.
 *
 * A recovery is stated per COMPONENT — `SessionPushes.resolveComponent` clears
 * every standing fault a component holds — so a component shared by several
 * operations makes one of them succeeding clear another's fault. Every
 * fault-raising operation here therefore has its own name, and two names are
 * shared ONLY where the operation genuinely is the same one: both readers of
 * the record's open obligations answer to {@link LIVE_WORK_COMPONENT}, because
 * either of them answering proves the store is reading again.
 *
 * The one component with NO recovery is {@link VENDOR_QUERY_COMPONENT}: a query
 * this process lost is not restarted by this process, so its fault is
 * permanent for the session by design rather than by omission.
 */
const VENDOR_QUERY_COMPONENT = "vendor-query";
/** The context-usage probe: transient, recovered by the next sample. */
const CONTEXT_USAGE_COMPONENT = "vendor-context-usage";
/** The mcp health probe: transient, recovered by the next probe. */
const MCP_STATUS_COMPONENT = "vendor-mcp-status";
/** The model catalog read: transient, recovered by a later StartSession's read. */
const MODEL_CATALOG_COMPONENT = "vendor-model-catalog";
/** Reading the record's open obligations: transient, recovered by any later read. */
const LIVE_WORK_COMPONENT = "store-live-work";
/** Serving history out of the store: transient, recovered by the next served read. */
const HISTORY_READ_COMPONENT = "shim-store-reader";
/** The shim's own keep-alive prompt: transient, recovered by the next beat. */
const KEEPALIVE_COMPONENT = "shim-engine-keepalive";
/**
 * The keep-alive REWIND: its own name, recovered by the next rewind that lands.
 *
 * Not {@link KEEPALIVE_COMPONENT}: a beat succeeding says nothing about whether
 * the vendor will resume at the anchor, and sharing the name would let one
 * clear the other's fault.
 */
const KEEPALIVE_REWIND_COMPONENT = "shim-engine-keepalive-rewind";

/**
 * What the vendor says when it will not resume at the uuid it was handed.
 *
 * Grounded on the owner's two dead sessions of 2026-09-14: the child exits 1
 * having printed `No message found with message.uuid of: <uuid>`.
 */
const REWIND_REFUSAL_PHRASE = "No message found with message.uuid";

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
  /**
   * Collapse all outstanding keep-alive turns back to the last real record.
   *
   * The manual counterpart of the per-cycle rewind (see engine/keepalive.ts),
   * reached from `main.ts`'s SIGUSR2 handler. A safe no-op when nothing is
   * owed or no session is bound.
   */
  resetKeepalives(): Promise<void>;
}

/**
 * The refusal for a claim this shim's own lock holder FAILED: it could not be
 * spawned, died before holding the lock, answered wrongly, or never answered.
 * Nobody is known to own the conversation, so this is never
 * `conversation_owned`; locks.ts has already recorded the defect at ERROR.
 */
function lockHolderUnavailable(err: LockHolderUnavailableError): shimv1.StartSessionResponse {
  return startSessionRefused(
    { kind: "lockHolderUnavailable", binary: err.binary, how: err.how },
    `this shim's lock helper ${err.binary} ${describeLockHolderHow(err.how)}; ` +
      `no other process is known to own this conversation`,
  );
}

export function createEngine(deps: EngineDeps): SessionEngine {
  const workspaceKey = workspaceLockKey(deps.env.cwd);
  const identityStore =
    deps.identityStore ?? createAgentIdentityStore(deps.env.stateDir, workspaceKey, deps.nowMs);
  const acquireLock = deps.acquireLock ?? acquireSessionLock;
  const acquireWorkspace = deps.acquireWorkspaceLock ?? acquireWorkspaceLock;
  const pushes = new SessionPushes(deps.nowMs, deps.runtime.shimBuildSha);
  const live = new LiveWorkTable();
  const foreground = new ForegroundUnitTable();
  const rewind = new KeepaliveRewind();
  /**
   * WHICH VENDOR MESSAGES THE KEEP-ALIVE PRODUCED. Asked once per message in
   * {@link onSdkMessage}; its answer is the tag every row and push of that
   * message carries (engine/keepalive.ts).
   */
  const keepaliveScope = new KeepaliveScope();
  const newUuid = deps.newUuid ?? randomUUID;

  let identity: SessionIdentity | undefined;
  /**
   * The identity a FAILED start already recorded rows under, if there is one.
   *
   * A start that never reached a query leaves nothing behind and is abandoned
   * whole. A start the vendor DID open and then refused — a `SessionStart` hook
   * that blocks the opening is the live case — has already had its messages
   * converted and written, so the record plane carries this conversation under
   * this name. The name and the file that persists it therefore STAND, and the
   * retry must reuse them rather than mint a second identity for one book.
   */
  let recordedOriginalVendorSessionId: string | undefined;
  let releaseLock: LockRelease | undefined;
  let releaseWorkspaceLock: LockRelease | undefined;
  let query: QueryLike | undefined;
  let abort: AbortController | undefined;
  let prompts: PromptQueue | undefined;
  /** The one turn that may be open. Written ONLY through {@link setOpen}. */
  let open: OpenTurn | undefined;
  /**
   * HOW the person commanded the last stop issued on the main thread, held for
   * the stopped turn's terminal (`KillTurnRequest.commanded_by`; `command` is
   * undefined when the caller stated none).
   *
   * THE NEXT MAIN-THREAD RESULT CONSUMES IT, whatever its outcome: that result
   * is the stopped turn's terminal. It is not keyed by the fold's turn because
   * the kill clears the turn slot before the vendor's stop result arrives. A
   * real turn opening retires one never consumed, so a stop can never be
   * attributed to a later turn.
   */
  let stopCommand:
    | { readonly turn: conversationv1.TurnId; readonly command: conversationv1.AgentInterruptedByUser | undefined }
    | undefined;
  /**
   * Settled the moment the keep-alive now open leaves the turn slot.
   *
   * Minted lazily by the first `StartTurn` that has to wait behind it, and
   * one per keep-alive however many ask, so an expired wait leaves nothing
   * behind to grow.
   */
  let keepaliveEnd: { readonly promise: Promise<void>; readonly resolve: () => void } | undefined;
  let effectiveModel = "";
  /**
   * The mode in force. `auto` IS THE UNSTATED MODE (owner ruling 2026-09-14):
   * a `StartSession{fresh}` that names no mode, and a resume whose transcript
   * states none, both run under the classifier-gated `auto` rather than the
   * vendor's own `default`. The gate is kept either way — `auto` decides each
   * ask with a model instead of the user — so nothing is dropped by the
   * choice, and `createQuery` therefore always passes `permissionMode: "auto"`
   * when the daemon stated nothing.
   */
  let permissionMode: conversationv1.AgentPermissionMode =
    fromVendorPermissionMode(DEFAULT_PERMISSION_MODE);
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
  /**
   * How many keep-alive beats this session has sent, so each prompt is unique.
   *
   * Only ever increments — see {@link keepalivePromptText} for why two identical
   * keep-alive prompts are the pattern the numbering exists to break.
   */
  let keepaliveCount = 0;
  let startResolve: (() => void) | undefined;
  /** Settles the same pending start as {@link startResolve}, with a named reason. */
  let startReject: ((reason: Error) => void) | undefined;
  /** The pending start's own bound, cleared by whatever settles the start first. */
  let startTimer: ReturnType<typeof setTimeout> | undefined;
  /**
   * What the vendor DID emit before the pending start settled, kind by kind.
   *
   * Reset by every attempt, so a retry's record is its own and not the previous
   * attempt's. Bounded by {@link PRE_INIT_KINDS_KEPT}.
   */
  let preInitKinds: string[] = [];
  /** Whether {@link preInitKinds} dropped anything to its bound. */
  let preInitKindsDropped = 0;
  /**
   * The tail of the vendor child's stderr, for the child's WHOLE LIFE.
   *
   * Reset by each start ATTEMPT, so a retry's refusal quotes its own child and
   * not its predecessor's; never reset after that, so a death hours later
   * still quotes whatever the child last said. Bounded by
   * {@link VENDOR_STDERR_KEPT}.
   */
  let vendorStderrTail = "";
  /**
   * The rewind the last real prompt rode in on, until the vendor proves it took.
   *
   * A REWIND THAT IS REFUSED MUST NOT COST THE PROMPT. The refusal arrives
   * AFTER the prompt was queued -- as an error result, or as the child simply
   * exiting 1 -- so the prompt has to be held here to be delivered again on a
   * plain resume. Cleared the moment the rewound query answers.
   */
  let rewindWatch:
    | {
        readonly anchorUuid: string;
        readonly anchorTurnId: string;
        readonly said: conversationv1.UserSaid;
        /** True when the rewind was performed to precede a keep-alive, not a real prompt. */
        readonly keepalive: boolean;
        /** The keep-alive send's client uuid, which a re-delivery must carry too. */
        readonly clientUuid: string | undefined;
      }
    | undefined;
  /**
   * How the vendor child ended, once it has.
   *
   * Read by {@link onQueryLost}, which is the one record a reader has for a
   * death. Absent means the child has not ended — or, for the mocked vendor,
   * that there was never a child to end.
   */
  let vendorExit: { code: number | null; signal: string | null } | undefined;
  let loop: Promise<void> | undefined;
  /**
   * What the cold gate's own compaction measured, kept for the phases AFTER it.
   *
   * `resuming` and `started` are pushed from `StartSession`, which is not where
   * the figures are read — the compaction is. Carrying them here is what lets
   * those two frames restate the same before/after pair the `summarized` frame
   * stated, rather than a second reading that could disagree with it. Absent
   * means no cold-gate compaction has landed in this process.
   */
  let coldCompactionFigures: { tokensBefore: number; tokensAfter: number } | undefined;
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
  // A CLOSED WINDOW IS A RECOVERY, NOT A SECOND OUTAGE. The record plane
  // announces its window twice -- open when the store stops answering, closed
  // when it answers again -- and relaying both as fresh windows left the
  // session holding two windows for one outage AND a `store_unreachable` fault
  // that nothing ever lifted. The closed announcement resolves the component
  // instead: it closes the window already standing and clears the faults that
  // window explains.
  deps.persistence.onDegradedWindow((window) => {
    if (window.extent.case !== "closed") {
      pushes.recordDegradedWindow(window);
      return;
    }
    const dropped = Number(window.extent.value.droppedCount);
    if (!pushes.resolveComponent(window.component, dropped)) {
      pushes.recordDegradedWindow(window);
    }
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
      // FOREGROUND WORK INCLUDED: a synchronous subagent is a task the vendor
      // tracks too, and its asks name that task id exactly as a detached one's
      // do. `tracked` translates the id; it says nothing about liveness.
      const byTask = live.tracked(vendorAgentId);
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
    // THE RUNNING VENDOR TURN'S ATTRIBUTION, not the shim's open turn: a
    // question a task-notification turn asks while the keep-alive waits is a
    // real question, and hiding it would leave the vendor waiting forever. A
    // SUBAGENT's ask is the keep-alive's only when the keep-alive spawned it:
    // a backgrounded subagent keeps asking across the turns after its own.
    keepalive: (agentId) => askIsKeepalive(agentId),
    // THE SAME ATTRIBUTION NAMES THE ASK'S TURN: the open turn, unless the open
    // turn is the keep-alive and the keep-alive did not raise this ask.
    turn: (agentId) => turnFor(askIsKeepalive(agentId)),
    nowMs: deps.nowMs,
    onPermissionModeSet: (mode) => {
      permissionMode = mode;
      pushPermissionMode();
    },
  });

  /** Whether the ask raised on `agentId`'s book is the keep-alive's. */
  function askIsKeepalive(agentId: conversationv1.AgentId): boolean {
    return agentId.value === requireIdentity().agentId.value
      ? keepaliveScope.producing()
      : keepaliveScope.spawned(agentId.value);
  }

  /**
   * THE TURN A ROW PRODUCED NOW BELONGS TO, under its keep-alive attribution:
   * the open turn, except that while the keep-alive is the open turn only the
   * keep-alive's own rows carry its id. A row produced with no turn open, or
   * one the keep-alive did not produce while it is open, belongs to no turn.
   */
  function turnFor(keepalive: boolean): conversationv1.TurnId | undefined {
    return open !== undefined && (!open.keepalive || keepalive) ? open.id : undefined;
  }

  function requireIdentity(): SessionIdentity {
    if (identity === undefined) {
      throw new Error("shim session: the main agent identity is not established yet");
    }
    return identity;
  }

  /**
   * What the fold holds for one message, under that message's attribution.
   *
   * THE KEEP-ALIVE'S TURN ID NAMES ONLY ITS OWN ROWS. A message the keep-alive
   * did not produce is folded with no turn while the keep-alive is the open
   * turn — otherwise a vendor turn nobody asked for would write served rows
   * keyed to a turn id that must never reach the wire.
   */
  function foldContext(attribution: KeepaliveAttribution): FoldContext {
    const turn = turnFor(attribution.keepalive);
    return {
      mainAgentId: requireIdentity().agentId,
      ...(turn === undefined ? {} : { turnId: turn }),
      keepalive: attribution.keepalive,
      nowMs: deps.nowMs,
      // DIAGNOSTIC-ONLY session facts the terminal's api-failure record reads.
      // The config-dir is the ACCOUNT the failing token belonged to (a path,
      // never a credential), and the model is what the turn ran under; neither
      // crosses the wire.
      claudeConfigDir: deps.env.configDir,
      ...(effectiveModel === "" ? {} : { model: effectiveModel }),
      ...(lastChange === undefined ? {} : { lastChange }),
      // THE KEEP-ALIVE IS NOBODY'S STOP: only a main-thread message reads it.
      ...(attribution.keepalive || stopCommand?.command === undefined ? {} : { stopCommand: stopCommand.command }),
      pendingAsk: (toolUseId) => gate.pendingAsk(toolUseId),
      deniedCall: (toolUseId) => gate.deniedCall(toolUseId),
      reportFault: (_kind, detail) => {
        noteConverterDefect(detail);
      },
      liveTask: (taskId) => {
        // LIVE OR FOREGROUND: a task frame of work that has not moved yet — the
        // patch that moves it among them — still needs its spawning call.
        const entry = live.tracked(taskId);
        if (entry === undefined) return undefined;
        // THE KIND TRAVELS WITH IT: the vendor's live level states a task's
        // `task_type` even for a task whose start this process never saw.
        return {
          toolUseId: entry.toolUseId ?? "",
          ...(entry.taskType === undefined ? {} : { taskType: entry.taskType }),
        };
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
    LOGGER.error(
      { component: CONVERTER_COMPONENT, detail, dropped_count: converterDegraded.droppedCount },
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

  /** The transcript scan behind `pushSessionTitle`, minted on first use. */
  let titleTail: TranscriptTitleTail | undefined;

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
    "title",
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
      LOGGER.debug(
        { reported: name },
        "the vendor reported the synthetic marker as its model; it is not a model and is not adopted",
      );
      return;
    }
    if (name === effectiveModel) return;
    LOGGER.debug(
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

  /**
   * THE PERMISSION MODE THE VENDOR SAYS IS IN FORCE.
   *
   * `init` states it, and since the start no longer waits for `init` that
   * statement lands with the FIRST TURN — after the opening already announced
   * the mode the start asked for. So it is adopted and pushed exactly like a
   * `SetSessionPermissionMode` would be when the two disagree, and does nothing
   * at all when they agree, which is the ordinary case.
   */
  function notePermissionMode(reported: conversationv1.AgentPermissionMode): void {
    if (reported.mode.case === permissionMode.mode.case) return;
    LOGGER.debug(
      { previous_permission_mode: permissionMode.mode.case ?? "", permission_mode: reported.mode.case ?? "" },
      "the vendor reported a permission mode the shim did not ask for; adopting it",
    );
    permissionMode = reported;
    pushPermissionMode();
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
    LOGGER.debug(
      { field, value },
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

  /**
   * WHERE A COLD-GATE COMPACTION HAS GOT TO.
   *
   * The compaction the cold gate performs runs INSIDE `StartSession`, so it has
   * no turn to narrate through and no frame of its own: without this arm a user
   * who answered the gate with "compact and resume" watches a minute of nothing
   * anywhere. Each phase is one frame, and a consumer draws the last one stated.
   *
   * THE FIGURES ARE ONLY EVER STATED WHERE THE COMPACTION KNOWS THEM. `0` is
   * what the proto documents as "not known yet" and is never an estimate: the
   * before figure is the transcript's own, as the gate read it, and the after
   * figure is the summary's output tokens, which is what remains in context.
   */
  function compactionProgressUpdate(
    phase: conversationv1.SessionCompactionPhase,
    figures: { tokensBefore?: number; tokensAfter?: number; error?: string } = {},
  ): conversationv1.SessionUpdate {
    return create(conversationv1.SessionUpdateSchema, {
      update: {
        case: "compactionProgress",
        value: create(conversationv1.SessionCompactionProgressSchema, {
          phase,
          tokensBefore: asInt64(figures.tokensBefore ?? 0, "compaction.tokensBefore"),
          tokensAfter: asInt64(figures.tokensAfter ?? 0, "compaction.tokensAfter"),
          ...(figures.error === undefined ? {} : { error: figures.error }),
        }),
      },
    });
  }

  /**
   * One cold-gate compaction phase, said to the fan-out and to the log at once.
   *
   * The two phases `StartSession` itself owns — `resuming` and `started` —
   * restate the figures {@link coldCompactionFigures} kept from the compaction
   * rather than reading the transcript again, so the whole sequence carries one
   * pair of numbers.
   */
  function pushColdCompactionPhase(
    which: conversationv1.SessionCompactionPhase,
    said: string,
  ): void {
    const figures = coldCompactionFigures;
    pushes.push(compactionProgressUpdate(which, figures ?? {}));
    LOGGER.info(
      {
        phase: conversationv1.SessionCompactionPhase[which],
        tokens_before: figures?.tokensBefore ?? 0,
        tokens_after: figures?.tokensAfter ?? 0,
      },
      said,
    );
  }

  /**
   * THE VENDOR'S OWN SUMMARY OF THIS CONVERSATION, when it has stated one.
   *
   * It is written to the transcript and NOWHERE ELSE — no SDK message
   * announces it — so the shim reads it, incrementally, at the same two edges
   * it reports the context usage at: the session's start and the end of every
   * turn. Nothing is pushed until the vendor states a title, and nothing is
   * pushed again until it states a DIFFERENT one; the tail is what decides
   * both (engine/title.ts).
   *
   * THE TAIL IS PER TRANSCRIPT. A clear rotates the conversation to a new
   * vendor session id and therefore to a new file, so a tail whose file is no
   * longer the current one is replaced rather than read at a stale offset.
   */
  function pushSessionTitle(): void {
    const current = identity;
    if (current === undefined) return;
    const file = transcriptPath(deps.env.configDir, deps.env.cwd, current.vendorSessionId);
    if (titleTail === undefined || titleTail.file !== file) {
      titleTail = new TranscriptTitleTail(file);
    }
    const title = titleTail.read();
    if (title === undefined) return;
    pushes.push(sessionTitleUpdate(title));
  }

  /**
   * The title digest: the material the daemon summarizes into a workspace
   * title of its own when the vendor has stated no ai-title.
   *
   * It reads the CURRENT transcript — the same file `pushSessionTitle` tails —
   * and returns the prompts since the last boundary plus, after a /compact, the
   * compaction summary. A session with no identity yet has no transcript, which
   * is the no_transcript arm rather than an empty digest: there is a difference
   * between "asked before the session named itself" and "a real conversation
   * with no prompts".
   */
  function gatherTitleDigest(): shimv1.GatherTitleDigestResponse {
    const current = identity;
    if (current === undefined) {
      return titleDigestRefused({ kind: "noTranscript" }, "the session has not named a transcript yet");
    }
    const file = transcriptPath(deps.env.configDir, deps.env.cwd, current.vendorSessionId);
    const read = readTitleDigest(file);
    switch (read.kind) {
      case "ok":
        return titleDigestGathered(read.digest);
      case "no_transcript":
        return titleDigestRefused({ kind: "noTranscript" }, `no transcript exists at ${file}`);
      case "unreadable":
        return titleDigestRefused({ kind: "unreadable" }, read.detail);
    }
  }

  /**
   * Every conversation filed under this shim's working directory.
   *
   * IT ANSWERS THE DIRECTORY, NOT THE SESSION, so a shim with no identity yet
   * still answers: the list is a fact about the working directory, and a
   * session that has not named itself simply flags nothing as bound.
   */
  function readTranscriptsHere(): shimv1.ReadTranscriptsResponse {
    const read = readTranscripts(
      deps.env.configDir,
      deps.env.cwd,
      identity?.vendorSessionId,
      deps.nowMs(),
    );
    return read.kind === "ok" ? transcriptsRead(read.transcripts) : transcriptsRefused(read);
  }

  /**
   * THE CONTEXT READINGS ARE SERIAL. Each probe starts only after the one before
   * it has pushed, so the readings reach the daemon in the order they were
   * asked for: a turn-end probe can never be overtaken by a slower mid-turn one
   * and leave a stale figure standing on the topbar's chip and the footer's
   * cell.
   */
  let contextUsageChain: Promise<void> = Promise.resolve();
  /** Whether a mid-turn refresh is queued and not yet started. */
  let contextRefreshQueued = false;

  function pushContextUsage(): Promise<void> {
    return enqueueContextProbe(() => undefined);
  }

  /**
   * Queue one probe behind every probe already asked for. The caller holds the
   * returned promise and so hears the probe's own failure; the chain keeps only
   * its SETTLEMENT, so one failed probe cannot stop every later one from
   * running.
   */
  function enqueueContextProbe(onStart: () => void): Promise<void> {
    const next = contextUsageChain.then(async () => {
      onStart();
      await probeContextUsage();
    });
    contextUsageChain = next.then(
      () => undefined,
      () => undefined,
    );
    return next;
  }

  /**
   * REFRESH THE CONTEXT READING AFTER A MAIN-AGENT API RESPONSE. The topbar's
   * chip states the context held and the footer's cell how much the turn has
   * grown it, both from this one reading, so both move while the turn runs
   * rather than only at its end. Only the MAIN agent's responses count: a
   * subagent's never enters the main window. A keep-alive's does not either,
   * because its exchange is rewound away. Refreshes COALESCE: while one is
   * queued and not yet started, another response asks for nothing more, since
   * the queued probe will read the later state anyway.
   */
  function noteMainApiResponse(message: SdkMessage): void {
    if (message.type !== "assistant") return;
    if (message.parent_tool_use_id !== null) return;
    if ((message.message as { usage?: unknown } | undefined)?.usage === undefined) return;
    if (open === undefined || open.keepalive) return;
    if (contextRefreshQueued) {
      LOGGER.logVerbose({}, "a context refresh is already queued; this response rides it");
      return;
    }
    contextRefreshQueued = true;
    const turnId = open.id.value;
    LOGGER.debug({ turn_id: turnId }, "a main-agent API response landed; refreshing the context reading");
    void enqueueContextProbe(() => {
      contextRefreshQueued = false;
    }).catch((err: unknown) => {
      LOGGER.error(
        { turn_id: turnId, cause: err instanceof Error ? err.message : String(err) },
        "the mid-turn context refresh failed before it could push a reading",
      );
    });
  }

  async function probeContextUsage(): Promise<void> {
    const active = query;
    if (active === undefined) return;
    try {
      pushes.push(contextUsageUpdate(await active.getContextUsage()));
      // THE SAME OPERATION SUCCEEDING IS THE RECOVERY. A probe that answers
      // proves the last refusal was a moment, not a state.
      pushes.resolveComponent(CONTEXT_USAGE_COMPONENT, 0);
    } catch (err) {
      pushes.fault(
        sessionFault(
          { kind: "vendorQueryFailed" },
          CONTEXT_USAGE_COMPONENT,
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
    // THE CATALOG IS PULLED LIKE THE REST, so it is re-read like the rest. It
    // was read exactly once, at StartSession, which made a single refusal there
    // two permanent things at once: a session with no model list to offer, and
    // an unhealthy verdict no later success could lift. A re-read is also the
    // honest answer to a catalog that changed under the account.
    await pushModelCatalog();
  }

  /** Re-read the account's model list, and state the health of that read. */
  async function pushModelCatalog(): Promise<void> {
    const active = query;
    if (active === undefined) return;
    try {
      modelCatalog = modelOptions(await active.supportedModels());
      pushes.resolveComponent(MODEL_CATALOG_COMPONENT, 0);
    } catch (err) {
      pushes.fault(
        sessionFault(
          { kind: "vendorQueryFailed" },
          MODEL_CATALOG_COMPONENT,
          `supportedModels failed: ${err instanceof Error ? err.message : String(err)}`,
        ),
      );
    }
  }

  /** Probe every declared mcp server's health and state each one. */
  async function pushMcpServerStatus(): Promise<void> {
    const active = query;
    if (active === undefined) return;
    try {
      for (const status of await active.mcpServerStatus()) pushes.push(mcpUpdate(status));
      pushes.resolveComponent(MCP_STATUS_COMPONENT, 0);
    } catch (err) {
      pushes.fault(
        sessionFault(
          { kind: "vendorQueryFailed" },
          MCP_STATUS_COMPONENT,
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
    // A CATALOG ROW THAT NAMES NO MODEL IS NOT AN OPTION. The vendor's catalog
    // can carry a row whose `value` is the synthetic marker or empty — the
    // "let the CLI pick" pseudo-entry — and normalizeModel collapses both to
    // "no model override". Such a row is unselectable (SetModel would refuse it
    // as not naming a model), so it is dropped HERE, at the producer, and never
    // reaches SessionStarted.model_catalog. The webapp keeps its own guard as
    // defense in depth, but the daemon should never be served one to begin
    // with.
    const selectable = models.filter((model) => {
      if (normalizeModel(model.value) !== "") return true;
      LOGGER.debug(
        { value: model.value, display_name: model.displayName },
        "the vendor offered a catalog row that names no model; it is not selectable and is omitted",
      );
      return false;
    });
    return selectable.map((model) =>
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
    // BEFORE THE FOLD. A rewind the vendor refused reaches us as this message,
    // and recovering here is what keeps the refusal out of the feed and the
    // prompt alive. A recovered message is consumed: the query it arrived on is
    // already closed and the prompt already re-delivered on its replacement.
    if (await noteRewindOutcome(message)) return;
    // THE TAG, TAKEN ONCE. Everything below reads this one answer: the fold
    // writes it into every row (the writer stores nothing tagged), and the push
    // plane drops every tagged session fact.
    const attribution = keepaliveScope.attribute(message);
    noteRewindBoundary(message);
    notePreInitMessage(message);
    noteIdentityFacts(message, attribution);
    noteMainApiResponse(message);
    settleStartOnBlockingHook(message);
    settleStartOnErrorResult(message);
    noteDetachedWork(message);
    converterDefectThisMessage = false;
    const output = deps.fold.onSdkMessage(message, foldContext(attribution));
    if (message.type === "result" && !attribution.keepalive) retireStopCommand("the stopped turn's result was folded");
    if (converterDefectThisMessage) {
      LOGGER.logVerbose({}, "this message was refused; the converter's window stays open");
    }
    const entries = [...output.entries];
    noteForegroundUnits(entries);
    if (entries.length > 0) deps.persistence.write(entries);
    serveSessionUpdates(entries);
    // THE ANCHOR, AND ONLY FROM WHAT MAY BE ONE. The whole message goes in; the
    // rewind itself refuses everything that is not an assistant record of the
    // open real turn (see engine/keepalive.ts).
    rewind.noteRecord(
      message,
      open === undefined ? undefined : { turnId: open.id.value, keepalive: open.keepalive },
    );
    if (output.turnEnded === undefined) return;
    // THE TURN IS THE UNIT OF RECOVERY. A defective turn is degraded for its
    // WHOLE length: the messages that follow the refused one are the same
    // turn's own remainder, and recovering on the next of them closed the
    // window a millisecond after opening it — before any consumer could
    // observe it, and while the turn that lost a record was still running. So
    // the window closes only at the end of a turn that refused nothing.
    if (!converterDefectThisTurn) noteConverterHealthy();
    converterDefectThisTurn = false;
    // A VENDOR TURN ENDING IS NOT THE KEEP-ALIVE ENDING. Only the result that
    // answers the keep-alive's own send closes it; a turn the vendor ran on its
    // own while the keep-alive waited leaves it open.
    if (open?.keepalive === true && !attribution.endsKeepalive) return;
    await closeTurn();
  }

  /**
   * THE WATCHSESSION PLANE'S ONE FILTER over what the fold produced.
   *
   * A session fact the keep-alive turn produced — its rate-limit reading, its
   * usage — is neither stored nor pushed: the tag on the entry is the whole
   * decision, so no reader downstream has to ask.
   */
  function serveSessionUpdates(entries: readonly PersistEntry[]): void {
    for (const entry of entries) {
      if (entry.item.kind !== "session_update") continue;
      if (entry.keepalive) {
        LOGGER.logVerbose(
          { arm: entry.item.update.update.case },
          "a session fact the keep-alive turn produced is never pushed (nor stored)",
        );
        continue;
      }
      const arm = entry.item.update.update.case ?? "";
      if (OWNED_ARMS.has(arm)) continue;
      pushes.push(entry.item.update);
    }
  }

  /**
   * The vendor's own name for one message: `type`, or `type:subtype`.
   *
   * The one reading of "what kind of thing was that", shared by the pre-`init`
   * ledger and the failed start's record, so the two cannot describe the same
   * message differently.
   */
  function messageKind(message: SdkMessage): string {
    const subtype = (message as { subtype?: unknown }).subtype;
    return typeof subtype === "string" && subtype !== "" ? `${message.type}:${subtype}` : message.type;
  }

  /** Remember a message that arrived while a start was still pending. */
  function notePreInitMessage(message: SdkMessage): void {
    if (startReject === undefined) return;
    if (preInitKinds.length >= PRE_INIT_KINDS_KEPT) {
      preInitKindsDropped += 1;
      return;
    }
    preInitKinds.push(messageKind(message));
  }

  /** Keep the tail of the vendor child's stderr, bounded. */
  function noteVendorStderr(chunk: string): void {
    if (chunk === "") return;
    const joined = vendorStderrTail + chunk;
    vendorStderrTail =
      joined.length <= VENDOR_STDERR_KEPT ? joined : joined.slice(joined.length - VENDOR_STDERR_KEPT);
    LOGGER.debug({ characters: chunk.length }, "the vendor child wrote to stderr");
  }

  /** Remember how the vendor child ended, for the death record. */
  function noteVendorExit(exit: { code: number | null; signal: string | null }): void {
    vendorExit = exit;
    LOGGER.debug(
      { vendor_exit_code: exit.code ?? -1, vendor_exit_signal: exit.signal ?? "" },
      "the vendor child ended",
    );
  }

  /**
   * WHAT THE SHIM KNOWS ABOUT A DEAD VENDOR, ALL OF IT, IN ONE PLACE.
   *
   * The SDK's own wording describes the STREAM ending; the exit code, the
   * signal and the child's last words describe the PROCESS, and on 2026-09-14
   * none of the three was anywhere on the record — the shim reported
   * `getContextUsage failed`, then that the transport was not ready for
   * writing, then that the query was gone, and the immediate cause could not
   * be recovered afterwards from any log.
   */
  function vendorDeathFields(): Record<string, string | number> {
    return {
      // -1 AND "" FOR ABSENCE, NOT OMISSION. A field that disappears when
      // there is no answer makes "the child exited 0" and "no child ever
      // ended" the same record, and those are opposite diagnoses.
      vendor_exit_code: vendorExit?.code ?? -1,
      vendor_exit_signal: vendorExit?.signal ?? "",
      vendor_stderr: vendorStderrTail.trim(),
    };
  }

  /**
   * A START'S DETAIL CARRIES THE VENDOR'S OWN WORDS WHEN THERE ARE ANY.
   *
   * The shim's reason states what the shim observed; the child's stderr states
   * what the vendor decided. Both, in that order, because a reader who sees
   * only the first cannot tell a refused resume from a slow one.
   */
  function withVendorStderr(reason: string): string {
    const said = vendorStderrTail.trim();
    return said === "" ? reason : `${reason}; the vendor said: ${said}`;
  }

  /**
   * WHAT A START THAT WAITED THE WHOLE BOUND OUT IN SILENCE SHOULD BE TOLD.
   *
   * The bound is for SILENCE, and silence has one grounded explanation and one
   * grounded companion, both worth naming in the refusal because neither is
   * visible anywhere else:
   *
   *   - THE CHILD ANSWERED NO CONTROL REQUEST EITHER. The start no longer
   *     waits for `init` — it settles on one control round-trip, whose own
   *     3s bound is what a wedged child normally trips. Reaching THIS bound
   *     means even that round-trip neither answered nor failed, so the child
   *     is not merely quiet on its stdout: it is not serving at all.
   *   - AN UNTRUSTED WORKSPACE IS A DEGRADED ONE. It does not hang, but its
   *     permission allowlists are dropped, so a reader here should confirm the
   *     entry `trust.ts` writes is present.
   *
   * NAMED ONLY WHEN THE EVIDENCE FITS: a child that wrote to stderr, or that
   * emitted anything beyond its `SessionStart` hooks, has a more specific story
   * and this one would talk over it.
   */
  function silentStartReason(timeoutMs: number): string {
    const bound = `the vendor neither answered a control request nor said anything within ${timeoutMs}ms`;
    const onlyHooks = preInitKinds.every((kind) => kind.startsWith("system:hook_"));
    if (!onlyHooks || preInitKindsDropped > 0 || vendorStderrTail.trim() !== "") return bound;
    const configFile = `${deps.env.configDir}/${VENDOR_CONFIG_FILE}`;
    const seen = preInitKinds.length === 0 ? "nothing at all" : "only its SessionStart hook events";
    return (
      `${bound}: it emitted ${seen} and wrote nothing to stderr. The start settles on one ` +
      "control round-trip, so a child this quiet is not serving its control channel at all. " +
      `Also confirm projects[${JSON.stringify(trustRoot(deps.env.cwd))}].` +
      `${TRUST_KEY} is true in ${configFile}: an untrusted workspace still runs, but with its ` +
      "permission allowlists silently dropped."
    );
  }

  /**
   * A RESULT THAT IS AN ERROR, BEFORE THE SESSION EVER OPENED.
   *
   * The vendor answers an opening it cannot honour with a `result` carrying
   * `is_error` — a refused resume, an exhausted budget, an execution error in
   * its own bring-up — and then has nothing further to say. That result IS the
   * start's answer, so relaying it at once is what keeps {@link INIT_TIMEOUT_MS}
   * a bound on SILENCE rather than a bound on every kind of failure.
   */
  function settleStartOnErrorResult(message: SdkMessage): void {
    const reject = startReject;
    if (reject === undefined) return;
    if (message.type !== "result") return;
    if (!message.is_error) return;
    const said =
      message.subtype === "success" ? message.result : message.errors.join("; ");
    // AUTH FAILURES AT START LAND HERE, before the session ever opened. When the
    // opening refusal is a credential rejection or a model/resource-not-found,
    // record the one greppable diagnostic line with the ACCOUNT (config-dir) the
    // failing token belonged to and the model in effect — the only way to tell a
    // clobbered/expired token apart from an account/scope mismatch after the
    // fact. NO CREDENTIAL VALUE IS LOGGED: the config-dir is a path, the sentence
    // is redacted of any token shape. The error reject below is untouched.
    const startStatus = (message as unknown as { api_error_status?: number | null })
      .api_error_status;
    const startHttpStatus = typeof startStatus === "number" ? startStatus : undefined;
    const startText = said ?? "";
    const startKind = classifyVendorApiFailure(startHttpStatus, undefined, startText);
    if (startKind === "shim.vendor.auth_rejected" || startKind === "shim.vendor.model_missing") {
      LOGGER.info(
        {
          operation: startKind,
          subtype: message.subtype,
          http_status: startHttpStatus,
          vendor_message: redactVendorMessage(startText),
          claude_config_dir: deps.env.configDir,
          model: effectiveModel === "" ? undefined : effectiveModel,
        },
        "the shim observed a vendor API failure while opening the session; recording the status, message, account config-dir and model for diagnosis",
      );
    }
    LOGGER.error(
      { subtype: message.subtype, detail: said },
      "the vendor answered the session's opening with an error result; the start is refused with its own text",
    );
    reject(
      new Error(
        `the vendor ended the session's opening with an error result (${message.subtype}): ${said}`,
      ),
    );
  }

  /**
   * THE QUERY ENDED BEFORE `init`, SO THE START IS SETTLED NOW.
   *
   * A child that exits, a stream that ends, an iterator that throws: each is a
   * conclusive answer already in hand, and waiting {@link INIT_TIMEOUT_MS} out
   * on top of it buys nothing and costs the daemon its whole bring-up. The
   * bound survives for the one case it was written for — a vendor that is
   * genuinely still there and genuinely silent.
   */
  function settleStartOnQueryEnd(detail: string): void {
    const reject = startReject;
    if (reject === undefined) return;
    reject(new Error(`the vendor query ended before its init message: ${detail}`));
  }

  /**
   * The session facts a message states about the vendor itself.
   *
   * A KEEP-ALIVE'S FACTS ARE NOT SERVED. The model it was answered on and the
   * fast-mode state its result restates are pushed to WatchSession as changes
   * — the footer's chips — so under the keep-alive tag only the request id,
   * which joins the shim's own log records, is taken.
   */
  function noteIdentityFacts(message: SdkMessage, attribution: KeepaliveAttribution): void {
    if (message.type === "system" && message.subtype === "init") {
      setClaudeSessionId(message.session_id);
      recordAgentBinaryVersion(message.claude_code_version);
      // WHERE THE INIT FACTS LAND NOW. The start settles on a proven-live
      // control round-trip, so this message usually arrives WITH THE FIRST
      // TURN, long after `SessionStarted` was answered. Each fact is therefore
      // applied through the same notifier a mid-session change uses, so a
      // consumer that was told the start's values is told when init disagrees
      // with them; a fact init merely restates pushes nothing.
      LOGGER.info(
        {
          vendor_session_id: message.session_id,
          model: message.model,
          permission_mode: message.permissionMode,
          agent_binary_version: message.claude_code_version,
          after_start: started,
        },
        started
          ? "the vendor announced its init with the first turn; applying the facts it carries to the live session"
          : "the vendor announced its init before the start settled",
      );
      noteReportedModel(message.model);
      notePermissionMode(fromVendorPermissionMode(message.permissionMode));
      noteFastMode(
        (message as { fast_mode_state?: unknown }).fast_mode_state,
        (message as { fast_mode_disabled_reason?: unknown }).fast_mode_disabled_reason,
      );
      if (identity !== undefined && message.session_id !== identity.vendorSessionId) {
        void rotate(message.session_id);
      }
      startResolve?.();
      return;
    }
    if (message.type === "result") {
      if (attribution.keepalive) return;
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
      if (attribution.keepalive) return;
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
      LOGGER.debug(
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
    if (message.type === "user") {
      // A TOOL RESULT CAN STATE THE MOVE: a `Bash` result naming a
      // `backgroundTaskId` says its foreground task left the turn, which is
      // one of the three statements the shared rule reads.
      live.onToolResult(message.tool_use_result);
      return;
    }
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

  /**
   * THE ONE WRITER OF {@link open}.
   *
   * A keep-alive leaving the slot — by its own result, an abandoned beat, the
   * query's death, a teardown or a kill — is the moment a `StartTurn` waiting
   * behind it may go (engine/turn.ts), so every way out goes through here and
   * the release cannot be forgotten on any of them.
   */
  function setOpen(turn: OpenTurn | undefined): void {
    const left = open;
    open = turn;
    if (left?.keepalive !== true || left === turn) return;
    // THE KEEP-ALIVE'S REWIND WATCH ENDS WITH IT. The watch holds the send to
    // re-deliver if the vendor refuses the anchor; once the keep-alive has left
    // the slot a refusal can no longer be about it, and a watch left standing
    // would re-deliver the KEEP-ALIVE — client uuid and all — under the real
    // turn that opens next.
    if (rewindWatch?.keepalive === true) {
      LOGGER.debug(
        { keepalive_turn: left.id.value, resume_session_at: rewindWatch.anchorUuid },
        "the keep-alive left the turn slot; its rewind watch ends with it",
      );
      rewindWatch = undefined;
    }
    const end = keepaliveEnd;
    keepaliveEnd = undefined;
    end?.resolve();
  }

  /** Drop the held stop command, if one is held, saying why. */
  function retireStopCommand(why: string): void {
    const held = stopCommand;
    if (held === undefined) return;
    stopCommand = undefined;
    LOGGER.debug(
      { turn_id: held.turn.value, command: held.command?.command.case ?? "unstated", why },
      "the held stop command retired",
    );
  }

  /** The promise a waiting `StartTurn` holds; absence when no keep-alive holds the slot. */
  function keepaliveEnded(): Promise<void> | undefined {
    if (open?.keepalive !== true) return undefined;
    if (keepaliveEnd === undefined) {
      let resolve = (): void => {};
      const promise = new Promise<void>((settle) => {
        resolve = settle;
      });
      keepaliveEnd = { promise, resolve };
    }
    return keepaliveEnd.promise;
  }

  async function closeTurn(): Promise<void> {
    const ended = open;
    setOpen(undefined);
    // THE REQUEST ID DIES WITH ITS TURN. It named the vendor call this turn ran
    // under; carrying it past the end would attribute idle records and the next
    // turn to a request that is already answered.
    clearRequestId();
    if (ended === undefined) return;
    // THE KEEP-ALIVE'S DEBT IS COUNTED BEFORE ANYTHING AWAITS. A `StartTurn`
    // waiting behind it runs the moment this function first yields, and its
    // yield obligation must already see the keep-alive it has to rewind past;
    // counted after an await, that prompt would build on keep-alive context.
    if (ended.keepalive) rewind.noteKeepaliveTurn();
    cadence?.resume();
    await applyPendingModel();
    if (identity !== undefined) {
      backupTranscript({
        transcript: transcriptPath(deps.env.configDir, deps.env.cwd, identity.vendorSessionId),
        stateDir: deps.env.stateDir,
        workspaceKey,
        vendorSessionId: identity.vendorSessionId,
        atMs: deps.nowMs(),
      });
    }
    if (ended.keepalive) {
      // A KEEP-ALIVE'S END IS NOBODY'S TURN END. Nothing is pushed for it: the
      // context usage it moved, the title it cannot have changed and the
      // account usage it spent all belong to housekeeping no surface shows, and
      // the next real turn's own end restates every one of them.
      LOGGER.info({ turn_id: ended.id.value, keepalive: true }, "closed a turn");
      return;
    }
    await pushContextUsage();
    pushSessionTitle();
    // A TURN CAN CHANGE WHAT THE PROBES ANSWER. Account usage and mcp health
    // are PULLED from the vendor rather than folded out of the stream, so a
    // session that probed once at StartSession would report the account's
    // windows and its servers' health as they stood before any work happened,
    // and would never notice a server going down or a limit being approached.
    // Awaited, not fired and forgotten: an unawaited probe would race the next
    // turn's own close.
    await reprobeSessionFacts();
    LOGGER.info({ turn_id: ended.id.value, keepalive: false }, "closed a turn");
  }

  /** A model change that waited for the turn to end lands now, and its caller is answered. */
  async function applyPendingModel(): Promise<void> {
    if (pendingModel === undefined) return;
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

  async function applyModel(model: conversationv1.AgentModel): Promise<void> {
    const active = query;
    if (active === undefined) return;
    const name = normalizeModel(model.name);
    await active.setModel(name === "" ? undefined : name);
    effectiveModel = name;
    pushModel();
  }

  function runLoop(active: QueryLike): Promise<void> {
    return (async (): Promise<void> => {
      try {
        for await (const message of active) {
          await onSdkMessage(message);
          // THE BACKPRESSURE. The vendor stream is the one producer of rows
          // with no bound of its own, so while the store writer's backlog is
          // past its high-water mark the next message is not read. The writer
          // never evicts a row; this wait is what keeps its buffer bounded.
          await deps.persistence.whenWritable();
        }
        if (isStaleLoop(active)) return;
        if (!standingDown) {
          // A QUERY THAT DIED BECAUSE ITS REWIND ANCHOR WAS REFUSED IS NOT A
          // LOST SESSION. The child exits 1 having said so on stderr; the
          // recovery reopens plainly and re-delivers the prompt.
          if (await recoverFromRewindRefusal("the vendor query ended without being asked to")) return;
          onQueryLost(
            create(conversationv1.SessionQueryDiedSchema, {
              cause: {
                case: "unexpectedEof",
                value: create(conversationv1.SessionQueryUnexpectedEofSchema, {}),
              },
            }),
            "the vendor query ended without being asked to",
          );
        }
      } catch (err) {
        if (isStaleLoop(active)) return;
        const cause = err instanceof Error ? err.message : String(err);
        if (await recoverFromRewindRefusal(cause)) return;
        onQueryLost(
          create(conversationv1.SessionQueryDiedSchema, {
            cause: {
              case: "iteratorFailure",
              value: create(conversationv1.SessionQueryIteratorFailureSchema, { cause }),
            },
          }),
          cause,
        );
      }
    })();
  }

  /**
   * Whether this loop belongs to a query the session has already let go.
   *
   * A REPLACED QUERY'S DEATH IS NOT THE LIVE QUERY'S DEATH. The keep-alive
   * rewind and the failed-start teardown both close the query they are done
   * with, and its loop then ends exactly as a real death does — so without this
   * a healthy session reported `query_died` and raised a permanent fault for a
   * query nothing was using. The live query is the only one that can lose the
   * session.
   */
  function isStaleLoop(active: QueryLike): boolean {
    if (query === active) return false;
    LOGGER.debug({}, "a query the session had already released ended its message loop");
    return true;
  }

  /**
   * The query is gone: report it, settle any pending start, unwedge the
   * callbacks.
   *
   * ONE DEATH, TWO STATEMENTS OF IT, ONE MESSAGE. `died` is pushed on
   * WatchSession AND carried by the open turn's terminal, so the two can
   * never disagree about the cause — and since they travel by independent
   * channels with no ordering between them, the terminal must say the death
   * itself rather than lean on the push arriving first.
   */
  function onQueryLost(died: conversationv1.SessionQueryDied, detail: string): void {
    LOGGER.error({ cause: detail, ...vendorDeathFields() }, "the vendor query is gone");
    pushes.push(
      create(conversationv1.SessionUpdateSchema, {
        update: { case: "queryDied", value: died },
      }),
    );
    // BEFORE ANYTHING ELSE. A start still waiting on `init` has its answer the
    // moment the query it was waiting on ends, and the teardown below would
    // otherwise run while the verb sat out the rest of its bound.
    settleStartOnQueryEnd(detail);
    gate.standDown(`the vendor query died: ${detail}`);
    // NOTHING THE DEAD QUERY ANNOUNCED CAN SETTLE, so the fold lets go of it.
    deps.fold.endQuery(`the vendor query died: ${detail}`);
    query = undefined;
    // THE STREAM OWNER GETS ITS OWN TERMINAL. `query_died` is a SESSION fact,
    // and a consumer watching the agent -- which is the consumer actually
    // waiting on the turn -- would otherwise see the stream simply stop
    // producing, with no terminal frame and no way to tell a dead query from a
    // slow one. Duplicated on purpose: a consumer with no stream open still
    // needs the session-level fact, and a consumer with no WatchSession open
    // still needs its turn concluded.
    writeQueryDeathTerminal(died, detail);
    setOpen(undefined);
    keepaliveScope.abandon(`the vendor query died: ${detail}`);
    cadence?.stop();
    // NO RECOVERY PATH, DELIBERATELY. Nothing in this process restarts a query
    // it lost, so this fault is meant to stand for the session's life -- and it
    // holds its own component so a probe that still answers cannot clear it.
    pushes.fault(sessionFault({ kind: "vendorQueryFailed" }, VENDOR_QUERY_COMPONENT, detail));
  }

  // -- submission -----------------------------------------------------------

  async function submit(said: conversationv1.UserSaid, keepalive: boolean): Promise<void> {
    const text = saidText(said);
    // THE ROLLBACK PRECEDES A KEEP-ALIVE TOO, not only a real prompt. The anchor
    // never advances past the last real record, so the same obligation a real
    // prompt owes is owed by the next keep-alive beat, and discharging it here
    // keeps at most one keep-alive in the transcript (engine/keepalive.ts). The
    // real-prompt path is byte-for-byte the same rollback; nothing about it
    // changes.
    await yieldObligation(said, keepalive);
    const queue = prompts;
    if (queue === undefined) throw new Error("shim session: no query is accepting prompts");
    cadence?.pause();
    queue.push(userMessage(text, keepalive ? keepaliveScope.pendingUuid() : undefined));
    LOGGER.debug({ keepalive, characters: text.length }, "submitted a prompt to the vendor");
  }

  /**
   * One streaming-input send.
   *
   * A KEEP-ALIVE SEND CARRIES ITS CLIENT UUID, and it is the only send that
   * does: the vendor echoes it on every reply to that send, which is how the
   * keep-alive turn scope knows its own answer from a turn the vendor ran on
   * its own (engine/keepalive.ts).
   */
  function userMessage(text: string, clientUuid: string | undefined): SdkUserMessage {
    return {
      type: "user",
      message: { role: "user", content: text },
      parent_tool_use_id: null,
      ...(clientUuid === undefined ? {} : { uuid: clientUuid as SdkUserMessage["uuid"] }),
    };
  }

  /**
   * THE SHARED COLLAPSE (ruled 2026-09-17).
   *
   * Both {@link yieldObligation} (run before every keep-alive and before every
   * real prompt) and {@link resetKeepalives} (the manual SIGUSR2 backdoor) roll
   * the vendor context back to the identical place: the last REAL assistant
   * record, discarding every keep-alive turn accumulated since. This is the ONE
   * place that performs that roll — replace the query with one that resumes
   * only THROUGH the anchor (`resumeSessionAt`), then settle the debt. The old
   * query is closed first: two queries on one conversation are two writers on
   * one transcript. Neither caller hand-edits or truncates the vendor's
   * transcript file; this is byte-for-byte the same declared `resumeSessionAt`
   * surface either way.
   *
   * A caller with a prompt riding on the collapse (`yieldObligation`) arranges
   * its own delivery and watch afterward; a caller with nothing to deliver
   * (`resetKeepalives`) just wants the collapse itself.
   */
  async function collapseToAnchor(
    owed: RewindObligation,
    current: SessionIdentity,
  ): Promise<{ ok: true } | { ok: false; detail: string }> {
    try {
      await replaceQuery({
        binding: { kind: "resume", resumeSessionId: current.vendorSessionId },
        resumeSessionAt: owed.resumeSessionAt,
      });
    } catch (err) {
      return { ok: false, detail: err instanceof Error ? err.message : String(err) };
    }
    rewind.settled();
    return { ok: true };
  }

  /**
   * THE YIELD OBLIGATION.
   *
   * A real prompt must never build on keep-alive context, so when keep-alive
   * turns have run since the last real record the query is REPLACED by one that
   * resumes only THROUGH that record (`resumeSessionAt`), via the shared
   * {@link collapseToAnchor}.
   */
  async function yieldObligation(said: conversationv1.UserSaid, keepalive: boolean): Promise<void> {
    const owed = rewind.obligation();
    if (owed === undefined) return;
    const current = identity;
    if (current === undefined) return;
    // INFO, WITH THE ANCHOR'S PROVENANCE. This is the one step that can lose a
    // real prompt, and when it goes wrong the only evidence anyone has is this
    // line naming which turn's assistant record the shim chose to resume at. The
    // two messages stay whole string literals (not one templated line) so the
    // log-classification guard can pin each at INFO by its exact text.
    const rewindContext = {
      resume_session_at: owed.resumeSessionAt,
      anchor_turn_id: owed.anchorTurnId,
      discarded_keepalive_turns: owed.discardedKeepaliveTurns,
      before: keepalive ? "keepalive" : "real_prompt",
    };
    if (keepalive) {
      LOGGER.info(
        rewindContext,
        "REWINDING the vendor context past the outstanding keep-alive turn before submitting the next keep-alive; at most one keep-alive is ever in the transcript",
      );
    } else {
      LOGGER.info(
        rewindContext,
        "REWINDING the vendor context past the trailing keep-alive turns before delivering a real prompt; the anchor is the assistant record of that turn",
      );
    }
    const collapsed = await collapseToAnchor(owed, current);
    if (!collapsed.ok) {
      // THE REWIND FAILED TO START. The prompt has not been queued yet, so the
      // recovery is simply a plain resume; `submit` pushes onto whatever query
      // this leaves behind.
      await abandonRewind(owed, current, collapsed.detail);
      return;
    }
    // WATCH IT LAND. The vendor may still refuse the anchor asynchronously, and
    // the prompt this rewind was performed FOR has to survive that.
    rewindWatch = {
      anchorUuid: owed.resumeSessionAt,
      anchorTurnId: owed.anchorTurnId,
      said,
      keepalive,
      clientUuid: keepalive ? keepaliveScope.pendingUuid() : undefined,
    };
  }

  /**
   * THE MANUAL RESET (SIGUSR2 backdoor; ruled 2026-09-17).
   *
   * "Reset all the keep-alives to the last one": collapse every outstanding
   * keep-alive turn back to the last real record, through the exact same
   * {@link collapseToAnchor} the per-cycle rewind uses. Nothing is owed to a
   * prompt here — there is none in flight for this call — so on success there
   * is nothing further to watch. A no-op (no keep-alive debt, or no bound
   * session) is safe and logged as such.
   */
  async function resetKeepalives(): Promise<void> {
    const owed = rewind.obligation();
    if (owed === undefined) {
      LOGGER.info({}, "keep-alive reset requested: no keep-alive turns are outstanding; nothing to do");
      return;
    }
    const current = identity;
    if (current === undefined) {
      LOGGER.info({}, "keep-alive reset requested: no vendor session is bound; nothing to do");
      return;
    }
    LOGGER.info(
      {
        resume_session_at: owed.resumeSessionAt,
        anchor_turn_id: owed.anchorTurnId,
        discarded_keepalive_turns: owed.discardedKeepaliveTurns,
      },
      "RESETTING all outstanding keep-alive turns back to the last real record on request",
    );
    const collapsed = await collapseToAnchor(owed, current);
    if (!collapsed.ok) {
      await abandonRewind(owed, current, collapsed.detail);
      return;
    }
    LOGGER.info(
      { resume_session_at: owed.resumeSessionAt, anchor_turn_id: owed.anchorTurnId },
      "the keep-alive RESET landed: the vendor resumed at the last real record",
    );
  }

  /**
   * The rewind is off: forget the anchor, reopen plainly, and say so.
   *
   * Used by the synchronous failure (the replaced query never started) and by
   * the asynchronous one ({@link recoverFromRewindRefusal}). It never throws:
   * the caller is on a path whose whole purpose is that the session survives.
   */
  async function abandonRewind(
    owed: { resumeSessionAt: string; anchorTurnId: string },
    current: SessionIdentity,
    detail: string,
  ): Promise<boolean> {
    LOGGER.error(
      {
        resume_session_at: owed.resumeSessionAt,
        anchor_turn_id: owed.anchorTurnId,
        cause: detail,
        vendor_stderr: vendorStderrTail.trim(),
      },
      "the vendor REFUSED the keep-alive rewind's anchor; reopening the query WITHOUT a rewind so the prompt is delivered rather than lost",
    );
    rewind.clearAnchor("the vendor refused to resume at it");
    rewind.settled();
    try {
      await replaceQuery({ binding: { kind: "resume", resumeSessionId: current.vendorSessionId } });
    } catch (err) {
      // The plain resume failed too. Nothing here can save the session, and
      // pretending otherwise would hide a dead query -- the caller's own loss
      // path takes it from here.
      LOGGER.error(
        { cause: err instanceof Error ? err.message : String(err) },
        "the plain resume that was to replace the refused rewind ALSO failed; the session's own loss path owns this now",
      );
      return false;
    }
    pushes.fault(
      sessionFault(
        { kind: "keepaliveFailed" },
        KEEPALIVE_REWIND_COMPONENT,
        `the vendor refused to resume at ${owed.resumeSessionAt}: ${detail}`,
      ),
    );
    return true;
  }

  /**
   * What the vendor said, when it is about the anchor this rewind named.
   *
   * Either the uuid itself appears in the words, or the vendor's own sentence
   * for the condition does. Both are checked because the sentence reaches us on
   * stderr and the uuid reaches us in an error result's `errors`.
   */
  function refusalNamesAnchor(words: string, anchorUuid: string): boolean {
    const evidence = `${words}\n${vendorStderrTail}`;
    return evidence.includes(REWIND_REFUSAL_PHRASE) || evidence.includes(anchorUuid);
  }

  /** An error result's words, or absence when the message is not one. */
  function resultErrorWords(message: SdkMessage): string | undefined {
    if (message.type !== "result" || message.subtype === "success") return undefined;
    const errors = (message as { errors?: unknown }).errors;
    const listed = Array.isArray(errors)
      ? errors.map((entry) => (typeof entry === "string" ? entry : JSON.stringify(entry))).join("; ")
      : "";
    return listed === "" ? message.subtype : `${message.subtype}: ${listed}`;
  }

  /**
   * Did this message settle the rewind the last real prompt rode in on?
   *
   * Answers TRUE only when the message was CONSUMED by a recovery: the query it
   * arrived on is closed, the prompt is re-delivered on its replacement, and
   * nothing downstream should see the refusal.
   */
  async function noteRewindOutcome(message: SdkMessage): Promise<boolean> {
    const watch = rewindWatch;
    if (watch === undefined) return false;
    if (message.type === "assistant" || (message.type === "result" && message.subtype === "success")) {
      rewindWatch = undefined;
      // Whole string literals, one per branch, so the log-classification guard
      // can pin each LANDED message at INFO by its exact text.
      const landedContext = {
        resume_session_at: watch.anchorUuid,
        anchor_turn_id: watch.anchorTurnId,
        before: watch.keepalive ? "keepalive" : "real_prompt",
      };
      if (watch.keepalive) {
        LOGGER.info(
          landedContext,
          "the keep-alive rewind LANDED: the vendor resumed at the anchor and is answering the next keep-alive",
        );
      } else {
        LOGGER.info(
          landedContext,
          "the keep-alive rewind LANDED: the vendor resumed at the anchor and is answering the real prompt",
        );
      }
      pushes.resolveComponent(KEEPALIVE_REWIND_COMPONENT, 0);
      return false;
    }
    const words = resultErrorWords(message);
    if (words === undefined) return false;
    return recoverFromRewindRefusal(words);
  }

  /**
   * The vendor refused the anchor after the prompt was already queued.
   *
   * THE PROMPT IS NEVER LOST. The query is reopened on a plain resume and the
   * same prompt delivered onto it; the session keeps its open turn, and the
   * footer gets a fault saying what happened.
   */
  async function recoverFromRewindRefusal(words: string): Promise<boolean> {
    const watch = rewindWatch;
    if (watch === undefined) return false;
    if (!refusalNamesAnchor(words, watch.anchorUuid)) return false;
    const current = identity;
    if (current === undefined) return false;
    rewindWatch = undefined;
    if (!(await abandonRewind({ resumeSessionAt: watch.anchorUuid, anchorTurnId: watch.anchorTurnId }, current, words))) {
      return false;
    }
    const queue = prompts;
    if (queue === undefined) {
      LOGGER.error(
        { resume_session_at: watch.anchorUuid, detail: "the plain resume produced no prompt queue" },
        "the plain resume left no prompt queue, so the prompt the refused rewind was carrying could not be re-delivered",
      );
      return false;
    }
    // THE SAME SEND, uuid and all: a re-delivered keep-alive is still the
    // keep-alive, and its answer must still be recognized as such.
    queue.push(userMessage(saidText(watch.said), watch.clientUuid));
    LOGGER.info(
      { resume_session_at: watch.anchorUuid, anchor_turn_id: watch.anchorTurnId },
      "the prompt the refused rewind was carrying was RE-DELIVERED on a plain resume; it was not lost",
    );
    return true;
  }

  /**
   * A boundary the anchor cannot be trusted across.
   *
   * The vendor's own compaction and its conversation reset both move the file
   * the uuid names out from under it, and neither replaces the query -- so
   * neither is caught by {@link startQuery}'s clear.
   */
  function noteRewindBoundary(message: SdkMessage): void {
    if (message.type === "conversation_reset") {
      rewind.clearAnchor("the vendor reset the conversation");
      rewindWatch = undefined;
      return;
    }
    if (message.type !== "system" || message.subtype !== "compact_boundary") return;
    rewind.clearAnchor("the vendor compacted the conversation");
    rewindWatch = undefined;
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
    // THE REPLACED QUERY ANSWERS NOTHING MORE, so nothing it announced can
    // settle; the fold lets go of it before the new query's first message.
    deps.fold.endQuery("the query was replaced");
    await startQuery(options.binding, options.resumeSessionAt);
  }

  async function startQuery(
    binding: QuerySpec["binding"],
    resumeSessionAt?: string,
  ): Promise<QueryLike> {
    // EVERY QUERY BINDING THAT IS NOT ITSELF THE REWIND DROPS THE ANCHOR. A
    // session's opening, a cold-gate answer, a rotation, a restart and a plain
    // replacement all put the vendor somewhere the remembered uuid may not
    // exist; carrying it across one is how a dead uuid reaches `resumeSessionAt`.
    if (resumeSessionAt === undefined) {
      rewind.clearAnchor("the query was replaced without a rewind");
      rewindWatch = undefined;
    }
    const queue = new PromptQueue();
    const controller = new AbortController();
    const created = await deps.createQuery({
      binding,
      permissionMode: toVendorPermissionMode(permissionMode),
      canUseTool: gate.canUseTool,
      abortController: controller,
      prompt: queue,
      onStderr: noteVendorStderr,
      onChildExit: noteVendorExit,
      ...(effectiveModel === "" ? {} : { model: effectiveModel }),
      ...(resumeSessionAt === undefined ? {} : { resumeSessionAt }),
    });
    query = created;
    prompts = queue;
    abort = controller;
    // A NEW QUERY RUNS NO VENDOR TURN YET: whatever the last one was midway
    // through ended with it.
    keepaliveScope.queryBound();
    loop = runLoop(created);
    return created;
  }

  // -- StartSession ---------------------------------------------------------

  /**
   * The pending start, and everything that settles it.
   *
   * {@link proveLive} resolves it on one answered control round-trip — the
   * signal a start normally settles on; `init` resolves it too, for a vendor
   * that still announces one first; {@link settleStartOnBlockingHook} rejects
   * it with the hook's own refusal text; {@link settleStartOnErrorResult} and
   * {@link settleStartOnQueryEnd} reject it with the vendor's; and this bound
   * rejects it with the bound named. Whichever lands first clears BOTH slots
   * and the timer, so the losers are inert rather than settling a start that a
   * later attempt owns.
   */
  function awaitInit(): Promise<void> {
    // EVERY ATTEMPT REPORTS ITS OWN EVIDENCE. A retry that inherited the
    // previous attempt's ledger would name messages this query never emitted.
    preInitKinds = [];
    preInitKindsDropped = 0;
    vendorStderrTail = "";
    return new Promise<void>((resolve, reject) => {
      const timeout = deps.initTimeoutMs ?? INIT_TIMEOUT_MS;
      startResolve = () => {
        clearPendingStart();
        resolve();
      };
      startReject = (reason) => {
        clearPendingStart();
        reject(reason);
      };
      if (timeout <= 0) return;
      const handle = setTimeout(() => {
        startReject?.(new Error(silentStartReason(timeout)));
      }, timeout);
      if (typeof (handle as { unref?: () => void }).unref === "function") {
        (handle as { unref: () => void }).unref();
      }
      startTimer = handle;
    });
  }

  /**
   * THE PROVEN-LIVE SIGNAL: one control round-trip, answered.
   *
   * A child that answers `supportedModels()` is spawned, connected and taking
   * work, which is the whole of what `StartSession` promises — the verb
   * resolves when a prompt can be ACCEPTED, not when the vendor has announced
   * anything. `init` is no longer waited for because it does not come until a
   * first turn does ({@link LIVE_SIGNAL_TIMEOUT_MS} states the grounding).
   *
   * THE SAME CALL THE OPENING ALREADY OWED. `SessionStarted.model_catalog` is
   * this answer, so the proof costs nothing extra and the catalog is in hand
   * before the opening is returned.
   *
   * NEVER REJECTS. Its failure is the START's failure while the start is still
   * pending; once something else has settled the start, a catalog that will not
   * answer is what it always was — a `SessionFault` on the catalog's own
   * component, which a later re-read can lift.
   */
  async function proveLive(active: QueryLike, boundMs: number): Promise<void> {
    try {
      modelCatalog = modelOptions(await withinBound(active.supportedModels(), boundMs, "supportedModels"));
      pushes.resolveComponent(MODEL_CATALOG_COMPONENT, 0);
      if (startResolve !== undefined) {
        LOGGER.info(
          { bound_ms: boundMs, models: modelCatalog.length },
          "the vendor answered a control request, so the child is proven live and the start settles on it",
        );
      }
      startResolve?.();
    } catch (err) {
      const detail = err instanceof Error ? err.message : String(err);
      if (startReject !== undefined) {
        startReject(new Error(`the vendor did not prove itself live: ${detail}`));
        return;
      }
      if (query !== active) return;
      pushes.fault(
        sessionFault({ kind: "vendorQueryFailed" }, MODEL_CATALOG_COMPONENT, `supportedModels failed: ${detail}`),
      );
    }
  }

  /**
   * A control round-trip with a bound, and the BOUND NAMED when it is reached.
   *
   * A request that never answers is indistinguishable from a wedged child, and
   * a reader who is only told "the start failed" cannot tell either from a slow
   * one — so the refusal says which call and how long it was given.
   */
  function withinBound<T>(work: Promise<T>, boundMs: number, call: string): Promise<T> {
    if (boundMs <= 0) return work;
    return new Promise<T>((resolve, reject) => {
      const handle = setTimeout(() => {
        reject(new Error(`the vendor did not answer ${call} within ${boundMs}ms`));
      }, boundMs);
      if (typeof (handle as { unref?: () => void }).unref === "function") {
        (handle as { unref: () => void }).unref();
      }
      work.then(
        (value) => {
          clearTimeout(handle);
          resolve(value);
        },
        (err: unknown) => {
          clearTimeout(handle);
          reject(err instanceof Error ? err : new Error(String(err)));
        },
      );
    });
  }

  /** Disarm the pending start: whichever settler won, the losers go. */
  function clearPendingStart(): void {
    startResolve = undefined;
    startReject = undefined;
    if (startTimer !== undefined) {
      clearTimeout(startTimer);
      startTimer = undefined;
    }
  }

  /**
   * A HOOK THAT BLOCKS BEFORE `init` HAS BLOCKED THE SESSION'S OWN OPENING.
   *
   * The only hooks that can fire before the vendor announces the session are
   * its `SessionStart` ones, and the vendor gives a blocked one no further
   * answer — no `init`, no result, nothing. So the blocking text IS the start's
   * failure reason, and relaying it at once is what turns a sixty-second hang
   * into a prompt refusal that names the hook the user actually configured.
   *
   * A hook that merely FAILED, or that only printed a system message or extra
   * context, gates nothing: {@link hookBlockingText} is the single reading of
   * that difference, shared with the converter that draws the same hook.
   */
  function settleStartOnBlockingHook(message: SdkMessage): void {
    if (startReject === undefined) return;
    if (message.type !== "system" || message.subtype !== "hook_response") return;
    const blockingText = hookBlockingText(message);
    if (blockingText === undefined) return;
    LOGGER.error(
      {
        hook: message.hook_name,
        hook_event: message.hook_event,
        hook_id: message.hook_id,
        detail: blockingText,
      },
      "a hook blocked the session's opening; the start is refused with the hook's own reason",
    );
    startReject(
      new Error(
        `the vendor's ${message.hook_name} hook blocked the session from opening: ${blockingText}`,
      ),
    );
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
    /**
     * Did this start compact the conversation on its way in?
     *
     * Only a cold-gate compaction narrates, and only this attempt's own: a
     * `pay` or `clear` answer to the same gate cut nothing, and a resume with
     * no gate at all has no compaction to be the `started` phase of.
     */
    let coldCompacted = false;
    // No initializer: BOTH source arms below set it, and a third arm that
    // forgot to would be a compile error rather than a silent empty model.
    let requestedModel: string;
    if (source.case === "fresh") {
      // A retry of an attempt that already recorded rows resumes ITS id: minting
      // a second one would key the retry's rows to a book the first attempt's
      // rows are not on.
      vendorSessionId = recordedOriginalVendorSessionId ?? mintVendorSessionId();
      requestedModel = normalizeModel(source.value.model?.name ?? "");
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
      // NEVER THE `<synthetic>` MARKER: the facts reader already skips the
      // records the CLI wrote itself, and the normalize here is the same rule
      // at the one assignment every launch reads, so no other source of a
      // marker can reach the SDK option either.
      requestedModel = normalizeModel(facts.lastModel ?? "");
      if (facts.lastPermissionMode !== undefined) {
        permissionMode = fromVendorPermissionMode(
          facts.lastPermissionMode as PermissionModeLike,
        );
      }
      const remediation = source.value.coldRemediation;
      const cold = judgeCold(facts, deps.nowMs(), requestedModel);
      if (cold !== undefined && remediation === undefined) {
        LOGGER.debug(
          { vendor_session_id: vendorSessionId, reason: cold, context_tokens: facts.contextTokens },
          "refused a cold resume because the caller named no remediation for its stated cost",
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
      // THE HANDOVER FROM THE REMEDIATION TO THE SESSION ITSELF. Said here
      // rather than inside `compact`, because the compaction's own work is
      // over: what remains is the resume this whole gate answer was for, and
      // it is the second half of the wait the user is sitting through.
      if (remediation?.remediation.case === "compact") {
        coldCompacted = true;
        pushColdCompactionPhase(
          conversationv1.SessionCompactionPhase.RESUMING,
          "the cold gate's compaction is done; resuming the session from its summary",
        );
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
      if (err instanceof LockHolderUnavailableError) return lockHolderUnavailable(err);
      // ONLY A GENUINE OWNER IS conversation_owned. Anything else the claim
      // threw is not a refusal this contract names, and is raised loudly.
      if (!(err instanceof LockHeldError)) throw err;
      LOGGER.debug(
        { vendor_session_id: inForce, cause: err.message },
        "refused StartSession: another shim holds this conversation's session lock",
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
      if (err instanceof LockHolderUnavailableError) return lockHolderUnavailable(err);
      if (!(err instanceof LockHeldError)) throw err;
      LOGGER.debug(
        {
          workspace_dir: deps.env.cwd,
          lock_path: workspaceLockPath(deps.env.cwd),
          cause: err.message,
        },
        "refused StartSession: another shim holds this workspace's lock",
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
    // A REBIND IS A RESUME THAT MOVES THE BOOK. `rebind` is set only by a
    // BindWorkspaceSession, and it is the one resume whose identity is the
    // RESUMED conversation's rather than the workspace's persisted one — the
    // workspace is on a different conversation now. Every other resume leaves
    // it unset and keeps the persisted id.
    const rebinding = source.case === "resume" && source.value.rebind !== undefined;
    identity = brandNew
      ? await SessionIdentity.fresh(identityStore, () => vendorSessionId)
      : await SessionIdentity.resume(identityStore, vendorSessionId, { rebind: rebinding });
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
      // The start can be settled by a blocking hook BEFORE `startQuery` has
      // even returned, and a rejection nobody is awaiting yet is an unhandled
      // one. This attaches the handler now; `await initialized` below still
      // throws the same reason.
      initialized.catch(() => undefined);
      const child = await startQuery(
        brandNew || clearedTo !== undefined
          ? { kind: "fresh", sessionId: inForce }
          : { kind: "resume", resumeSessionId: vendorSessionId },
      );
      // THE PROOF THE START SETTLES ON, issued the moment the child exists.
      // It runs BESIDE the other settlers rather than instead of them: a
      // blocking hook, an error result or a query that ends can still land
      // first and refuse the start with its own reason, and each of those is
      // the more specific account of the same child.
      const live = proveLive(child, deps.liveSignalTimeoutMs ?? LIVE_SIGNAL_TIMEOUT_MS);
      await initialized;
      // AND THE CATALOG IS IN HAND BEFORE THE OPENING IS ANSWERED. When the
      // round-trip is what settled the start this has already resolved; when
      // `init` beat it, this is the wait that keeps `SessionStarted` whole.
      await live;
    } catch (err) {
      // A FAILED START LEAVES THE ENGINE AS IT FOUND IT. The next StartSession
      // is a fresh attempt that settles its OWN identity, so every trace of
      // this one goes: both kernel claims, the writer's name, and — when this
      // attempt is what minted it — the persisted identity. Leaving the writer
      // named is what made the retry hit the re-key guard and escape as an
      // unhandled `Internal` on a verb that has a typed refusal for every real
      // condition.
      //
      // THE HELD CUTS ARE NOT WRITTEN YET: they land after this block precisely
      // so this attempt's own compaction is not stranded by it. What the shim
      // does NOT control is the vendor, which writes through the converter the
      // moment its query is live — so whether the name is still free is a
      // question asked, not assumed.
      // WHAT THE VENDOR DID SAY, BEFORE ANY OF IT IS THROWN AWAY. Recorded at
      // info because a start that failed is exactly when the reader needs the
      // kinds: the grounded silent resume had a `SessionStart:resume` hook
      // start and succeed, and nothing said so anywhere.
      LOGGER.info(
        {
          vendor_session_id: inForce,
          binding: brandNew || clearedTo !== undefined ? "fresh" : "resume",
          pre_init_messages: preInitKinds.length + preInitKindsDropped,
          pre_init_kinds: preInitKinds.join(","),
          vendor_stderr: vendorStderrTail.trim(),
          // THE SPAWN'S OWN FACTS, on the one record a failed start always
          // writes: which directory the vendor was pointed at, and which
          // account root and trust key govern it. A reader chasing a silent
          // start otherwise has to reconstruct both from the daemon's side.
          config_dir: deps.env.configDir,
          trust_root: trustRoot(deps.env.cwd),
        },
        "what the vendor emitted before the start failed",
      );
      // NO ORPHANED VENDOR CHILD. `startQuery` returning means a child is
      // running, and the failure path used to walk away from it: the retry then
      // opened a SECOND child on the same conversation, two writers on one
      // transcript, and the first went on burning a session slot for the life
      // of the shim. Cleared BEFORE it is closed so its own loop reads as the
      // released query it now is rather than as the live session dying.
      const orphan = query;
      if (orphan !== undefined) {
        query = undefined;
        prompts?.close();
        prompts = undefined;
        abort?.abort();
        abort = undefined;
        orphan.close();
        LOGGER.info({ vendor_session_id: inForce }, "closed the vendor query the failed start had opened");
      }
      await releaseLock?.();
      releaseLock = undefined;
      await releaseWorkspaceLock?.();
      releaseWorkspaceLock = undefined;
      // ...UNLESS THE VENDOR ALREADY SPOKE. `startQuery` returning means the
      // query is live, and everything it emitted before the refusal went
      // through the converter's `persist` — a `SessionStart` hook that blocks
      // the opening emits exactly that. Those rows carry the producer name, so
      // the name is no longer free: un-naming it would throw out of a verb that
      // has a typed refusal for every real condition, and forgetting the file
      // behind it would strand the rows on a book nothing names again. The
      // identity stands instead, and the retry re-announces the same one —
      // which `setProducer` takes as the no-op it is.
      const recorded = deps.persistence.producerHasWrittenRows();
      if (recorded) {
        recordedOriginalVendorSessionId = identity.originalVendorSessionId;
        LOGGER.info(
          {
            original_vendor_session_id: identity.originalVendorSessionId,
            detail: "rows were written before the start was refused",
          },
          "keeping the identity a failed start recorded under: the record plane already carries this conversation",
        );
      } else {
        deps.persistence.clearProducer();
        if (!identityWasPersisted) await identityStore.forget();
      }
      identity = undefined;
      const detail = withVendorStderr(err instanceof Error ? err.message : String(err));
      LOGGER.error({ cause: detail }, "the vendor query could not be started");
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
      LOGGER.debug({ held: held.length }, "writing the context cuts held until the session had an identity");
      for (const cut of held) writeContextCut(cut);
    }
    started = true;
    // THE CATALOG IS ALREADY IN HAND: `proveLive` pulled it, because that pull
    // IS the round-trip the start settled on. Pulling it a second time here
    // would spend a control call to learn what the opening already knows.
    if (query !== undefined) await pushMcpServerStatus();
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
    pushSessionTitle();
    void pushAccountUsage();
    // The readiness signal, and the reason a consumer may open WatchSession
    // before anything else exists.
    pushes.push(pushes.diagnostics());
    // The build sha and SDK version are the PROCESS's, injected at
    // construction. The agent binary version comes from the SDK's own bundled
    // manifest — which is why the opening can state it without `init`, whose
    // `claude_code_version` arrives with the first turn and OVERWRITES the
    // manifest's answer when the two disagree (the running binary outranks the
    // packaged declaration). `requireSessionRuntime` still refuses to build the
    // message when NEITHER source has one: that is the presence rule doing its
    // job, and the start fails saying so rather than sending a sentinel.
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
    LOGGER.info(
      {
        vendor_session_id: identity.vendorSessionId,
        original_vendor_session_id: identity.originalVendorSessionId,
        model: effectiveModel,
        live_work: liveWork.length,
      },
      "session started",
    );
    // THE LAST PHASE, AND ONLY ON THE PATH THAT HAD A COMPACTION. It is pushed
    // after the start is whole so a consumer that draws `started` is drawing a
    // session it can immediately prompt.
    if (coldCompacted) {
      pushColdCompactionPhase(
        conversationv1.SessionCompactionPhase.STARTED,
        "the session resumed from the cold gate's compaction is up",
      );
    }
    return startSessionStarted(started_);
  }

  /**
   * WHICH COMPACTION THIS IS, and therefore whether it narrates.
   *
   * The cold gate's compaction is one the USER is waiting on, so it pushes its
   * phases; hibernation's runs behind a directive the daemon has already been
   * answered for, with nobody waiting on a progress frame it would never see
   * completed. One function serves both, and this is the only difference.
   */
  type CompactionOrigin = "cold_gate" | "hibernation";

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
        LOGGER.debug({ vendor_session_id: vendorSessionId }, "cold remediation: paying for the read");
        return { kind: "none" };
      case "clear": {
        const minted = mintVendorSessionId();
        LOGGER.debug(
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
          "cold_gate",
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
    origin: CompactionOrigin,
  ): Promise<{ ok: true; summary: string } | { ok: false; error: string }> {
    const transcript = transcriptPath(deps.env.configDir, deps.env.cwd, vendorSessionId);
    const startedAtMs = deps.nowMs();
    const queue = new PromptQueue();
    const controller = new AbortController();
    let throwaway: QueryLike | undefined;
    /** Narrate one phase, to the fan-out and to the log, for the cold gate only. */
    const phase = (
      which: conversationv1.SessionCompactionPhase,
      figures: { tokensBefore?: number; tokensAfter?: number; error?: string },
      said: string,
    ): void => {
      if (origin !== "cold_gate") return;
      pushes.push(compactionProgressUpdate(which, figures));
      LOGGER.info(
        {
          vendor_session_id: vendorSessionId,
          phase: conversationv1.SessionCompactionPhase[which],
          tokens_before: figures.tokensBefore ?? 0,
          tokens_after: figures.tokensAfter ?? 0,
          ...(figures.error === undefined ? {} : { cause: figures.error }),
        },
        said,
      );
    };
    /** A compaction that did not complete, said once to both surfaces. */
    const failed = (error: string): { ok: false; error: string } => {
      phase(
        conversationv1.SessionCompactionPhase.FAILED,
        { tokensBefore: facts.contextTokens, error },
        "the cold gate's compaction did not complete",
      );
      return { ok: false, error };
    };
    if (origin === "cold_gate") coldCompactionFigures = undefined;
    // BEFORE THE QUERY, NOT AFTER IT. Creating the throwaway query is itself
    // part of the minute the user is waiting through, so a frame that waited
    // for the query to exist would leave the opening of the wait unnarrated.
    phase(
      conversationv1.SessionCompactionPhase.SUMMARIZING,
      { tokensBefore: facts.contextTokens },
      "the cold gate is summarizing the conversation on a throwaway query",
    );
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
          return failed(`the summarizing session ended as ${message.subtype}`);
        }
        summary = message.result;
        outputTokens = message.usage.output_tokens;
        break;
      }
      if (summary === "") return failed("the summarizing session produced no summary");
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
      // THE COMPACTION IS A BOUNDARY. Whatever the anchor named is on the far
      // side of the summary now, so the next real prompt after keep-alives
      // carries them rather than resuming at a uuid this cut may have orphaned.
      rewind.clearAnchor("the shim compacted the conversation");
      rewindWatch = undefined;
      writeContextCut(
        contextCutCompacted({
          summary,
          tokensBefore: facts.contextTokens,
          tokensAfter: outputTokens,
          durationMs,
          requested: true,
        }),
      );
      if (origin === "cold_gate") {
        coldCompactionFigures = { tokensBefore: facts.contextTokens, tokensAfter: outputTokens };
      }
      phase(
        conversationv1.SessionCompactionPhase.SUMMARIZED,
        { tokensBefore: facts.contextTokens, tokensAfter: outputTokens },
        "the cold gate's summary came back and the compaction boundary was appended to the transcript",
      );
      return { ok: true, summary };
    } catch (err) {
      const error = err instanceof Error ? err.message : String(err);
      writeContextCut(contextCutFailed(error));
      return failed(error);
    } finally {
      queue.close();
      controller.abort();
      throwaway?.close();
    }
  }

  /**
   * Conclude the open turn with a failure terminal, because the query died.
   *
   * THE `query_died` ARM, carrying the very death the session pushed. The
   * terminal reaches the daemon through the store and the push through
   * WatchSession, with no ordering between them, and a replayed turn has only
   * the terminal: an `execution_error` stand-in here let whichever landed
   * first decide how the turn was drawn. `errors` carries the account. Nothing
   * is written when no turn was open: a session that lost its query between
   * turns has no turn to conclude.
   */
  function writeQueryDeathTerminal(died: conversationv1.SessionQueryDied, detail: string): void {
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
          discriminator: "agent_frame.failure.query_died",
        },
        keepalive: ended.keepalive,
        turn: ended.id,
        item: {
          kind: "frame",
          frame: create(conversationv1.AgentFrameSchema, {
            agentId,
            result: {
              case: "failure",
              value: create(conversationv1.AgentFailureSchema, {
                errors: [detail],
                failure: { case: "queryDied", value: died },
              }),
            },
          }),
        },
      },
    ]);
    LOGGER.info(
      { turn_id: ended.id.value, cause: detail },
      "concluded the open turn with a failure terminal after the vendor query died",
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
        turn: ended.id,
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
    LOGGER.info(
      { turn_id: ended.id.value, reason },
      "concluded the open turn as interrupted by host shutdown: the session is being torn down under it",
    );
  }

  /** The `context_cut` page line, on the conversation's own book. */
  function writeContextCut(cut: conversationv1.ContextCut): void {
    if (identity === undefined) {
      // NEVER DROPPED, only DEFERRED. A row needs the agent id the identity
      // settles, and the cold gate's remediation runs before that — so the cut
      // waits rather than vanishing.
      LOGGER.debug(
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
      turn: turnFor(false),
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
   * units that are still open, which are USUALLY near the end of the book, so
   * one page ordinarily finds them all. It is a page size, not a bound: work
   * that started earlier is found by walking back (readBookFor).
   */
  const RECONCILE_PAGE_SIZE = 512;

  /**
   * The book, read from its newest page back until every unit in `units` has
   * been found or the book's floor is reached.
   *
   * THE NEWEST PAGE IS NOT "WHERE LIVE WORK STARTED". A long session's detached
   * agents can have started thousands of entries ago, and a unit the read never
   * reached was closed as a SHELL run: on 2026-09-23 eight running subagents of
   * the owner's main session were closed with shell terminals and relabelled
   * `bash` in the store. The walk stops as soon as nothing is missing, so the
   * ordinary case still reads one page.
   */
  async function readBookFor(
    agentId: conversationv1.AgentId,
    units: readonly string[],
  ): Promise<conversationv1.HistoryEntryAt[]> {
    const first = await deps.persistence.readFirstPage(agentId, RECONCILE_PAGE_SIZE, undefined, () =>
      knowsAgent(agentId),
    );
    const book = [...first.entries];
    const missing = new Set(units);
    const mark = (entries: readonly conversationv1.HistoryEntryAt[]): void => {
      for (const unit of [...missing]) {
        const id = create(conversationv1.AgentActivityIdSchema, { value: unit });
        if (findUnit(entries, id) !== undefined) missing.delete(unit);
      }
    };
    mark(first.entries);
    let boundary = first.boundary;
    let pages = 1;
    while (missing.size > 0 && boundary.case === "more") {
      const oldest = book[book.length - 1]?.at;
      if (oldest === undefined) break;
      const older = await deps.persistence.readAgentPage(agentId, RECONCILE_PAGE_SIZE, oldest);
      if (older.entries.length === 0) break;
      book.push(...older.entries);
      mark(older.entries);
      boundary = older.boundary;
      pages++;
    }
    LOGGER.debug(
      { agent: agentId.value, pages, entries: book.length, unfound: missing.size },
      "read the book back for the live work it must describe",
    );
    return book;
  }

  /**
   * Whether the record plane ANSWERED "no rows under that agent yet".
   *
   * THE CLASS IS THE MEANING, never "any error". `unknown_agent` is the store
   * reading its registry and telling the truth about a book whose first write
   * has not landed -- an ordinary state for a workspace created seconds ago,
   * which the store itself records at info. A reader that folds it in with a
   * refused socket or a timeout diagnoses a healthy store as unreachable, opens
   * a session fault, and makes the daemon open a health fault over a store that
   * did exactly what it should. Every OTHER kind stays the failure it is.
   */
  function answeredNoBookYet(err: unknown): err is PersistenceError {
    return err instanceof PersistenceError && err.kind === "unknown_agent";
  }

  /**
   * GetLiveWork reconciliation.
   *
   * RE-ADOPT what the revived vendor process actually has, and WRITE THE
   * CLOSING TERMINAL for what did not survive. The invariant it protects: every
   * started thing eventually gets a terminal row, by observation or by
   * reconciliation.
   *
   * SCOPED TO THIS SESSION. The store is shared by every session on the host,
   * and everything answered here that this vendor does not hold is CLOSED, so
   * the read names this conversation's main agent and gets only its lineage.
   * Unscoped, a session start wrote closing terminals into other sessions'
   * books for work still running there.
   */
  async function reconcile(): Promise<conversationv1.AgentDetachedWork[]> {
    const agentId = requireIdentity().agentId;
    let open;
    try {
      open = await deps.persistence.liveWork(agentId);
      pushes.resolveComponent(LIVE_WORK_COMPONENT, 0);
    } catch (err) {
      pushes.fault(
        sessionFault(
          { kind: "storeUnreachable" },
          LIVE_WORK_COMPONENT,
          `GetLiveWork failed: ${err instanceof Error ? err.message : String(err)}`,
        ),
      );
      return [];
    }
    const closing: PersistEntry[] = [];

    // THE RECORD IS THE ONLY PLACE the work's own start survives a bounce, so
    // the book is read ONCE and every description below comes out of it. This
    // is a read and not a follow, so it goes through the ONE-SHOT verb: the
    // store mints no watch token for a tail that is never stood.
    let book: readonly conversationv1.HistoryEntryAt[] = [];
    if (open.liveDetached.length > 0 || open.liveAgents.length > 0) {
      try {
        book = await readBookFor(
          agentId,
          open.liveDetached.map((work) => work.value),
        );
      } catch (err) {
        if (answeredNoBookYet(err)) {
          LOGGER.debug(
            { agent: agentId.value },
            "this session's book holds no rows yet, so reconciliation describes live work from an empty book",
          );
        } else {
          // warn: a defect because reconciliation lost access to the durable book it must describe.
          LOGGER.warn(
            { cause: err instanceof Error ? err.message : String(err) },
            "the book could not be read for reconciliation; live work cannot be described",
          );
        }
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
        // warn: a defect because the vendor could not confirm ownership while reconciliation continued.
        LOGGER.warn(
          { work_id: work.value, cause: err instanceof Error ? err.message : String(err) },
          "the vendor could not be asked whether it still holds this work; treating it as surviving",
        );
        surviving.add(work.value);
      }
    }
    const survives = (work: conversationv1.DetachedWorkId): boolean =>
      surviving.has(work.value);
    // A SURVIVING HANDLE THE RECORD CANNOT DESCRIBE IS A DEFECT, and here it is
    // provably one: this vendor process was just ASKED and said it still holds
    // the work, so the missing start belongs to work that really is this
    // conversation's. Nothing is written for it -- an announcement with an
    // invented description is worse than a missing one -- but it is stated as
    // the record-plane loss it is.
    const readopted = announceLiveWork(book, open.liveDetached.filter(survives), agentId, (handle) => {
      reportUndescribableWork(handle);
    });

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
        closing.push(closingSubagentTerminal(agentId, run, item.value));
        continue;
      }
      if (item?.case === "monitor") {
        closing.push(closingMonitorTerminal(agentId, run, findMonitorCall(book, run)));
        continue;
      }
      const recorded = findBashStart(book, run);
      if (recorded === undefined) {
        LOGGER.debug(
          { work_id: work.value, kind: item?.case ?? "" },
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
      // warn: a decision because live workflow records cannot be adopted while workflow support is disabled.
      LOGGER.warn(
        { live_workflows: open.liveWorkflows.length },
        "GetLiveWork reports live workflow runs while workflow support is disabled; they are neither adopted nor closed",
      );
    }
    if (closing.length > 0) deps.persistence.write(closing);
    LOGGER.debug(
      { readopted: readopted.length, closed: closing.length },
      "reconciled the store's open obligations against what the vendor still has",
    );
    return readopted;
  }

  // -- keep-alive -----------------------------------------------------------

  async function keepaliveBeat(): Promise<void> {
    if (open !== undefined || query === undefined || identity === undefined) return;
    // A REAL START IN FLIGHT OWNS THE SLOT IT IS ABOUT TO OPEN, even before
    // `open` says so: it is still writing its prompt row or reading its page.
    if (turns.startInFlight()) {
      LOGGER.logVerbose({ outcome: "skipped_start_in_flight" }, "keep-alive beat skipped: a StartTurn is being processed");
      return;
    }
    // A keep-alive turn's id NEVER reaches the wire: TurnIds are daemon-minted
    // and adopted, and this turn has no daemon behind it. The value exists only
    // so the turn has an identity in this process; its prompt entry is tagged
    // keep-alive, so the writer stores nothing of it.
    const turn = create(conversationv1.TurnIdSchema, {
      value: `keepalive-${deps.nowMs()}-${process.pid}`,
    });
    keepaliveCount += 1;
    const said = textSaid(keepalivePromptText(keepaliveCount));
    setOpen({ id: turn, keepalive: true, startedAtMs: deps.nowMs() });
    // THE SCOPE OPENS BEFORE THE SEND, so the vendor cannot answer a send the
    // shim does not yet recognize as its own.
    keepaliveScope.begin(newUuid(), turn.value);
    try {
      const prompt = buildPrompt(
        turn,
        identity.agentId,
        said,
        conversationv1.PromptOrigin.UNSPECIFIED,
      );
      deps.persistence.write([promptEntry(prompt, identity.agentId, true)]);
      await submit(said, true);
      LOGGER.debug(
        { turn: turn.value, outcome: "keepalive_submitted" },
        "submitted one of the shim's own keep-alive prompts; nothing of its turn is stored or served",
      );
      pushes.resolveComponent(KEEPALIVE_COMPONENT, 0);
    } catch (err) {
      setOpen(undefined);
      keepaliveScope.abandon(`the keep-alive could not be submitted: ${err instanceof Error ? err.message : String(err)}`);
      pushes.fault(
        sessionFault(
          { kind: "keepaliveFailed" },
          KEEPALIVE_COMPONENT,
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
    keepaliveEnded,
    keepaliveYieldBudgetMs: deps.keepaliveYieldBudgetMs ?? KEEPALIVE_YIELD_BUDGET_MS,
    submit,
    setOpenTurn: (turn) => {
      if (turn !== undefined && !turn.keepalive) retireStopCommand("a real turn opened");
      setOpen(turn);
      if (turn === undefined) cadence?.resume();
      else cadence?.pause();
    },
    noteStopCommand: (turn, command) => {
      stopCommand = { turn, command };
      LOGGER.debug(
        { turn_id: turn.value, command: command?.command.case ?? "unstated" },
        "holding the stop's command for the stopped turn's terminal",
      );
    },
    reportStoreUnreachable: (detail) => {
      pushes.fault(sessionFault({ kind: "storeUnreachable" }, HISTORY_READ_COMPONENT, detail));
    },
    // THE SERVED READ IS THE RECOVERY. Without a counterpart to the refusal
    // above, one history read that met a busy database made the session
    // unhealthy for as long as the shim lived, however many reads it served
    // afterwards.
    reportStoreReadable: () => {
      pushes.resolveComponent(HISTORY_READ_COMPONENT, 0);
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
      request.coldRemediation === undefined &&
      // THE FLOOR APPLIES HERE TOO, under the caller's own threshold: the
      // owner ruled the gate off below it for a model change as much as for a
      // lapse, so a caller asking for a threshold of 0 still gets a small
      // switch through rather than a refusal.
      !underColdGateFloor(facts, deps.nowMs(), "model_switch")
    ) {
      LOGGER.debug(
        { model: model.name, context_tokens: facts.contextTokens },
        "refused SetSessionModel because switching models crosses the caller's cold-cache threshold",
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
      LOGGER.debug({ model: model.name, turn_id: open.id.value }, "model change accepted; it resolves at the turn boundary");
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
    LOGGER.debug({ permission_mode: mode.mode.case ?? "" }, "changed the session permission mode");
    return create(shimv1.SetSessionPermissionModeResponseSchema, {
      result: { case: "success", value: create(shimv1.SetSessionPermissionModeSuccessSchema, {}) },
    });
  }

  /**
   * The pre-hibernation directive.
   *
   * IT STOPS THE SHIM AND THAT IS ALL IT DOES (owner's ruling, 2026-09-20).
   * Hibernation is a MEMORY measure: the daemon stands an idle shim down to
   * free what it holds, and a session revived later pays for its own context
   * through the cold gate, which is the gate that exists to judge exactly
   * that. So this directive never compacts, and it is single-phase again —
   * it acks, or it names why it cannot.
   *
   * THE COMPACTION THIS USED TO RUN IS GONE. It was a real vendor summary
   * turn standing between the sweep and the stand-down, and it bought a
   * two-phase answer, a durable per-transcript mark and a retry loop to pay
   * for it. Nothing about freeing the shim's memory needed a rewritten
   * transcript.
   *
   * THE TWO GUARDS STAY, because both are still true of a stand-down: there
   * is nothing to stand down without a session, and standing one down under
   * a live turn would kill the turn.
   */
  async function hibernate(): Promise<shimv1.HibernateResponse> {
    if (!started || identity === undefined) {
      return hibernateRefused({ kind: "noSession" });
    }
    // A keep-alive is not a turn in flight to the daemon (see servedOpenTurn):
    // standing the shim down under one ends housekeeping, never anyone's work.
    if (servedOpenTurn() !== undefined) {
      return hibernateRefused({ kind: "turnInFlight" });
    }
    LOGGER.info(
      { vendor_session_id: identity.vendorSessionId },
      "hibernated: nothing is in flight, so the daemon may stand this shim down; the transcript is left exactly as it stands",
    );
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
      ...servedTurnInFlight(),
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
    try {
      // SCOPED, like every live-work read: this session's lineage only.
      const agentId = requireIdentity().agentId;
      const handles = (await deps.persistence.liveWork(agentId)).liveDetached;
      // THE READ ANSWERING IS THE RECOVERY, whatever it answered. An empty set
      // is as much proof the store is reading again as a full one, and
      // resolving only on the non-empty path is why the owner's fault outlived
      // a store that had been healthy for hours.
      pushes.resolveComponent(LIVE_WORK_COMPONENT, 0);
      if (handles.length === 0) return [];
      // READ, NOT WATCH: the one-shot verb, so no watch token is minted for a
      // tail this description never stands.
      const book = await readBookFor(
        agentId,
        handles.map((handle) => handle.value),
      );
      const undescribed: conversationv1.DetachedWorkId[] = [];
      const announcements = announceLiveWork(book, handles, agentId, (handle) => {
        undescribed.push(handle);
      });
      for (const handle of undescribed) await recordUndescribedHandle(handle);
      return announcements;
    } catch (err) {
      // NO BOOK YET IS NOT AN UNREACHABLE STORE. A workspace created moments
      // ago has written no row, so the store answers `unknown_agent` -- its
      // ordinary answer, which it logs at info. The live membership of a
      // conversation with no record is EMPTY, and that is the whole answer:
      // no fault, nothing for the daemon's health to open, and a line at debug.
      if (answeredNoBookYet(err)) {
        LOGGER.debug(
          { cause: err.message },
          "this session's book holds no rows yet; re-announcing an empty live membership for the new watch",
        );
        return [];
      }
      // LOUD, NEVER SILENT: the watch still opens — a consumer told nothing at
      // all is worse off than one told the opening with an empty membership —
      // but the record plane being unreachable is a session-level fact every
      // consumer is entitled to, so it goes out as the fault it is.
      const detail = err instanceof Error ? err.message : String(err);
      LOGGER.error(
        { cause: detail },
        "the record plane could not be read to re-announce the live membership for a new watch",
      );
      pushes.fault(
        sessionFault(
          { kind: "storeUnreachable" },
          LIVE_WORK_COMPONENT,
          `re-announcing live work for a new WatchSession failed: ${detail}`,
        ),
      );
      return [];
    }
  }

  /**
   * Work this conversation owns whose own start the record cannot describe.
   *
   * ONE SITE FOR ONE FACT, so the two callers that reach this conclusion by
   * different routes -- StartSession, which has already asked the vendor about
   * every handle, and a re-announcement, which asks about this one -- cannot
   * drift into saying it differently.
   */
  function reportUndescribableWork(handle: conversationv1.DetachedWorkId): void {
    // warn: a defect because the vendor still holds work whose start the record cannot describe.
    LOGGER.warn(
      { work_id: handle.value, detail: "the vendor confirmed it holds this work" },
      "the record holds no describable start for work the vendor still has; it cannot be announced",
    );
  }

  /**
   * Say WHICH STATE an undescribable live handle is in, and never guess.
   *
   * THE OBLIGATION SET IS THIS SESSION'S LINEAGE. `GetLiveWork` names this
   * conversation's main agent and answers every started thing the record holds
   * no terminal for among it and the agents it spawned -- so a handle with no
   * start in THIS main book is either work a subagent of this session
   * announced (its start lives in that subagent's book) or work whose start the
   * record lost.
   *
   * The vendor is the authority that separates the two, exactly as it is at
   * StartSession: if this vendor process still holds the handle then its
   * missing start is a real loss; if it does not, the work is not this
   * process's to announce, and the StartSession reconciliation owns its
   * terminal. A vendor that cannot answer is not a vendor that said "not
   * held", so an unanswerable probe is reported as the defect it might be.
   */
  async function recordUndescribedHandle(
    handle: conversationv1.DetachedWorkId,
  ): Promise<void> {
    const active = query;
    let held = true;
    if (active !== undefined) {
      try {
        held = await active.backgroundTasks(handle.value);
      } catch (err) {
        // warn: a defect because the vendor could not say whether it holds work the record cannot describe.
        LOGGER.warn(
          { work_id: handle.value, cause: err instanceof Error ? err.message : String(err) },
          "the vendor could not be asked whether it holds this undescribable live work; treating it as this conversation's",
        );
      }
    }
    if (!held) {
      LOGGER.debug(
        { work_id: handle.value },
        "this session's live work is not held by this vendor process and has no start in the main book; it is not announced to the new watch",
      );
      return;
    }
    reportUndescribableWork(handle);
  }

  /**
   * The open turn AS ANY CONSUMER MAY SEE IT: the daemon's turn, never the
   * shim's own keep-alive.
   *
   * A KEEP-ALIVE IS NEVER A TURN IN FLIGHT to anyone outside this process: its
   * id is the shim's own, minted for a row key, and a daemon told of it would
   * track, draw and wait on a turn it never started. Every verb that answers
   * the daemon about the open turn asks this, never `open` itself.
   */
  function servedOpenTurn(): OpenTurn | undefined {
    return open === undefined || open.keepalive ? undefined : open;
  }

  function servedTurnInFlight(): { turnInFlight?: conversationv1.TurnId } {
    const served = servedOpenTurn();
    return served === undefined ? {} : { turnInFlight: served.id };
  }

  function sessionLive(): conversationv1.SessionLive {
    const announceable = live.announceable();
    return create(conversationv1.SessionLiveSchema, {
      ...servedTurnInFlight(),
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
    // A KEEP-ALIVE HOLDS NOTHING A KILL MUST WAIT FOR: it is housekeeping no
    // consumer asked for, and ending it mid-flight costs nothing but its call.
    const served = servedOpenTurn();
    const busy = served !== undefined || announceable.length > 0;
    if (busy && !request.force) {
      LOGGER.debug(
        { turn_in_flight: served?.id.value ?? "", live_work: announceable.length },
        "refused KillSession because the session is live and force was not set",
      );
      return killSessionRefused(
        { kind: "live", live: sessionLive() },
        `the session has ${served === undefined ? "no turn" : `turn ${served.id.value}`} in flight and ${announceable.length} live item(s)`,
      );
    }
    const interruptedTurn = served?.id;
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
    LOGGER.info({ forced: busy, stopped: stopped.length }, "killed the session");
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
      LOGGER.info(
        { exit_code: code },
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
        // warn: a defect because teardown continued after the vendor refused its interrupt.
        LOGGER.warn(
          { cause: err instanceof Error ? err.message : String(err) },
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
          // warn: a defect because teardown continued after a detached item could not be stopped.
          LOGGER.warn(
            { task_id: entry.taskId, cause: err instanceof Error ? err.message : String(err) },
            "could not stop a detached item during teardown; continuing",
          );
        }
      }
      concludeStoppedRuns(stopping);
    }
    // BEFORE `open` IS CLEARED: the terminal names the turn, and a teardown
    // that forgot the turn first would have nothing to write it for.
    writeHostShutdownTerminal(reason);
    setOpen(undefined);
    keepaliveScope.abandon(`the session is being torn down: ${reason}`);
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
      LOGGER.error(
        { reason, lost_rows: flushed.lostRows, detail: "store writes remained unacknowledged at stand-down" },
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
        LOGGER.debug(
          { task_id: entry.taskId },
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
    LOGGER.info({ closed: closing.length }, "closed the shell runs this teardown stopped, as interrupted by the user");
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
          // warn: a defect because watcher conclusion continued without a durable book head.
          LOGGER.warn(
            { agent: entry.agent.value, cause: err instanceof Error ? err.message : String(err) },
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
    const page = await deadline(
      // A ONE-SHOT READ, AND IT SAYS SO. The head is a page and nothing more:
      // `readFirstPage` opens page-only, so the store mints no watch token for
      // a tail this teardown will never stand.
      //
      // THE PRODUCER VOUCHES HERE TOO. A session killed before its first turn
      // has an agent with no book, and asking the store for one earned an
      // `unknown_agent` refusal on every such teardown. The head of a book that
      // does not exist is absence, which is exactly what an empty page answers.
      deps.persistence.readFirstPage(agent, 1, undefined, () => knowsAgent(agent)),
      watcherConclusionBudgetMs,
      `the store did not answer for ${agent.value}'s book within ${watcherConclusionBudgetMs}ms`,
    );
    return page.entries[0]?.at;
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
        LOGGER.error({ budget_ms: budgetMs, detail: complaint }, complaint);
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
    startTurn: (request, signal) => turns.startTurn(request, signal),
    watchAgent: (request) => turns.watchAgent(request),
    updateAgent: (request) => turns.updateAgent(request),
    killTurn: (request) => turns.killTurn(request),
    watchBash: (request) => turns.watchBash(request),
    stopBash: (request) => turns.stopBash(request),
    detachForeground: (request) => turns.detachForeground(request),
    readHistory: (request) => turns.readHistory(request),
    readTranscripts: () => Promise.resolve(readTranscriptsHere()),
    gatherTitleDigest: () => Promise.resolve(gatherTitleDigest()),
    resetKeepalives,
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
