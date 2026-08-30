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
import { bindLog, setClaudeSessionId } from "../log.js";
import { conversationv1, shimv1 } from "../proto.js";
import { acquireSessionLock } from "../locks.js";
import { workspaceLockKey } from "../locks.js";
import { recordAgentBinaryVersion, requireSessionRuntime } from "../build-identity.js";
import { subagentId, toolCallActivityId } from "../convert/ids.js";
import { bashUpsertKey, terminalUpsertKey } from "../store/keys.js";
import type { PersistEntry, Persistence } from "../store/persistence.js";
import {
  announceLiveWork,
  closingAgentTerminal,
  closingBashTerminal,
  closingSubagentTerminal,
  findBashStart,
  findUnit,
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
import type { EngineFold, FoldContext } from "./fold-context.js";
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
import { LiveWorkTable } from "./detached.js";
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
export interface EngineDeps {
  readonly persistence: Persistence;
  readonly fold: EngineFold;
  readonly createQuery: CreateQuery;
  readonly runtime: { shimBuildSha: string; sdkVersion: string };
  readonly env: {
    readonly stateDir: string;
    readonly configDir: string;
    readonly cwd: string;
  };
  nowMs(): number;
  /** Injected so a suite never waits on a clock. */
  readonly scheduler?: KeepaliveScheduler;
  /** Injected so a suite substitutes a temp directory without a state dir. */
  readonly identityStore?: AgentIdentityStore;
  /** Injected so a suite can take no kernel lock. */
  readonly acquireLock?: (sessionId: string) => () => void;
  /** How long StartSession waits for the vendor's own `system:init`. */
  readonly initTimeoutMs?: number;
}

/** How often the account's rate-limit windows are sampled. */
export const ACCOUNT_USAGE_INTERVAL_MS = 5 * 60 * 1000;

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
  const pushes = new SessionPushes(deps.nowMs);
  const live = new LiveWorkTable();
  const rewind = new KeepaliveRewind();

  let identity: SessionIdentity | undefined;
  let releaseLock: (() => void) | undefined;
  let query: QueryLike | undefined;
  let abort: AbortController | undefined;
  let prompts: PromptQueue | undefined;
  let open: OpenTurn | undefined;
  let effectiveModel = "";
  let permissionMode: conversationv1.AgentPermissionMode = fromVendorPermissionMode("default");
  let modelCatalog: conversationv1.ModelOption[] = [];
  let started = false;
  let standingDown = false;
  let pendingModel: conversationv1.AgentModel | undefined;
  let accountUsageHandle: unknown;
  let cadence: KeepaliveCadence | undefined;
  let initResolve: ((message: SdkMessage) => void) | undefined;
  let loop: Promise<void> | undefined;

  const gate = new PermissionGate({
    mainAgentId: () => requireIdentity().agentId,
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
      pendingAsk: (toolUseId) => gate.pendingAsk(toolUseId),
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

  /** A message converted cleanly (or the turn ended): the converter is well. */
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
      return unavailable({
        case: "windowUnavailable",
        value: create(conversationv1.SessionUsageWindowUnavailableSchema, {}),
      });
    }
    const fiveHour = usageWindow(usage.rate_limits.five_hour);
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
    } else {
      noteConverterHealthy();
    }
    const entries = [...output.entries];
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
      // THE TURN'S END IS ALSO A RECOVERY POINT: a defect on the turn's last
      // convertible message would otherwise leave the window open until some
      // later turn happened to arrive.
      noteConverterHealthy();
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
      // UNSOLICITED CHANGES ARE STILL CHANGES: nothing called SetSessionModel,
      // so this message is the only evidence the swap happened.
      noteReportedModel((message.message as { model?: unknown } | undefined)?.model);
      return;
    }
    if (message.type === "conversation_reset") {
      void rotate(message.new_conversation_id);
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
    if (ended === undefined) return;
    if (ended.keepalive) rewind.noteKeepaliveTurn();
    cadence?.resume();
    if (pendingModel !== undefined) {
      const model = pendingModel;
      pendingModel = undefined;
      await applyModel(model);
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
      canUseTool: gate.canUseTool as CanUseToolLike,
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
    let requestedModel = "";
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
      releaseLock = acquireLock(inForce);
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
    const brandNew = source.case === "fresh" || facts === undefined;
    identity = brandNew
      ? await SessionIdentity.fresh(identityStore, () => vendorSessionId)
      : await SessionIdentity.resume(identityStore, vendorSessionId);
    // NAME THE WRITER BEFORE ANYTHING IS WRITTEN. A row is keyed by the
    // conversation's ORIGINAL vendor session id, which is exactly what the
    // identity just settled — and a write attempted before this raises rather
    // than landing rows under a name no replay could absorb against.
    deps.persistence.setProducer(identity.originalVendorSessionId);
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
      releaseLock?.();
      releaseLock = undefined;
      identity = undefined;
      const detail = err instanceof Error ? err.message : String(err);
      LOGGER.log({ level: "error", cause: detail }, "the vendor query could not be started");
      return startSessionRefused({ kind: "vendorStartFailed" }, detail);
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
    const liveWork = await reconcile();
    // THE CADENCE BEGINS BEFORE SUCCESS RETURNS: a session that is never
    // prompted still has a cache worth keeping warm.
    cadence = new KeepaliveCadence(
      () => void keepaliveBeat(),
      undefined,
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
        canUseTool: (() => Promise.resolve({ behavior: "deny", message: "compaction takes no tools" })) as CanUseToolLike,
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

  /** The `context_cut` page line, on the conversation's own book. */
  function writeContextCut(cut: conversationv1.ContextCut): void {
    if (identity === undefined) return;
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
    const survives = (work: conversationv1.DetachedWorkId): boolean =>
      live.byToolUseId(work.value) !== undefined;
    const readopted = announceLiveWork(book, open.liveDetached.filter(survives));

    for (const work of open.liveDetached) {
      if (survives(work)) continue;
      // The vendor no longer has it and nobody stopped it: we simply stopped
      // being able to see it, which is what `lost.swept_up` says.
      //
      // THE KIND COMES FROM THE RECORD, never from a guess: every terminal arm
      // is kind-specific, so closing an unknown unit as a shell would claim it
      // ran a command and closing it as a spawn would claim it made an agent.
      // A unit the record cannot describe is reported and left open — the
      // obligation is real, and inventing its kind would not discharge it.
      const run = toolCallActivityId(work.value);
      const item = findUnit(book, run);
      if (item?.case === "bash") {
        const recorded = findBashStart(book, run);
        if (recorded !== undefined) {
          closing.push(closingBashTerminal(agentId, run, recorded));
          continue;
        }
      }
      if (item?.case === "subagent") {
        closing.push(closingSubagentTerminal(agentId, run));
        continue;
      }
      LOGGER.log(
        { level: "warn", work_id: work.value, kind: item?.case },
        "the record cannot describe this live work; its terminal would have to invent the unit's kind, so it stays open",
      );
    }
    for (const agent of open.liveAgents) {
      if (agent.value === agentId.value) continue;
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
      pendingModel = model;
      LOGGER.log({ model: model.name, turn_id: open.id.value }, "model change accepted; it resolves at the turn boundary");
      return create(shimv1.SetSessionModelResponseSchema, {
        result: {
          case: "success",
          value: create(shimv1.SetSessionModelSuccessSchema, {
            modelChanged: create(conversationv1.SessionModelChangedSchema, {
              effectiveModel: model,
            }),
          }),
        },
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
    return killSessionClosed(killed);
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
      for (const entry of live.all()) {
        try {
          await active.stopTask(entry.taskId);
        } catch (err) {
          LOGGER.log(
            { level: "warn", task_id: entry.taskId, cause: err instanceof Error ? err.message : String(err) },
            "could not stop a detached item during teardown; continuing",
          );
        }
      }
    }
    open = undefined;
    prompts?.close();
    abort?.abort();
    active?.close();
    query = undefined;
    await loop?.catch(() => undefined);
    await deps.persistence.flush();
    pushes.standDown();
    releaseLock?.();
    releaseLock = undefined;
  }

  const engine: SessionEngine = {
    pushes,
    onSdkMessage,
    startSession,
    // `subscribe()` runs EAGERLY here, at the open, not at the consumer's first
    // pull: the diagnostics frame is seeded into the subscriber's queue by that
    // call, and a generator that only subscribed on first `next()` would leave
    // the daemon unable to tell "not ready" from "refused".
    watchSession: () => mapSessionUpdates(pushes.subscribe()),
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
    },
  };
  return engine;
}

/** Wrap each session fact in its response message. */
async function* mapSessionUpdates(
  updates: AsyncIterable<conversationv1.SessionUpdate>,
): AsyncIterable<shimv1.WatchSessionResponse> {
  for await (const update of updates) {
    yield create(shimv1.WatchSessionResponseSchema, { update });
  }
}
