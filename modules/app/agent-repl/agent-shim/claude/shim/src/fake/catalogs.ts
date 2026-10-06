/**
 * fake/catalogs.ts — the mocked vendor's answers to the shim's CONTROL verbs.
 *
 * These are not part of any turn: `supportedModels`, `supportedCommands`,
 * `supportedAgents`, `mcpServerStatus`, `getContextUsage`, the account-usage
 * probe, `accountInfo` and `initializationResult` are questions the shim asks
 * out of band, and the answers feed `SessionStarted` and half the
 * `SessionUpdate` arms. They live here rather than inside a scenario because
 * they are session facts, not turn events — a scenario can SWITCH between
 * prepared arms (see the account-usage family) but never authors one.
 *
 * FULLNESS IS THE POINT. A context-usage answer with three fields set would
 * let a converter that ignores twelve of them pass; every optional field
 * `sdk.d.ts` declares is populated here so the fold has to handle it.
 */
import type {
  AccountInfoLike,
  AccountUsageLike,
  AgentInfoLike,
  ContextUsageLike,
  EffortLevelLike,
  InitializationResultLike,
  McpServerStatusLike,
  ModelInfoLike,
  SlashCommandLike,
} from "../sdk/types.js";
import type { AccountUsageArm } from "./scenario.js";

/** The model the mock answers with until `setModel` moves it. */
export const FAKE_DEFAULT_MODEL = "fake-opus-4-8";

/**
 * The selectable model catalog.
 *
 * FOUR rows, not one: an alias row (`fake-default`, whose `resolvedModel`
 * points at the opus row) plus three concrete models whose capability flags
 * differ in every dimension the topbar renders — effort support and the exact
 * levels, adaptive thinking, fast mode, auto mode. A catalog whose rows agreed
 * about their capabilities could not exercise a renderer that branches on them.
 */
export const FAKE_MODELS: ModelInfoLike[] = [
  {
    value: "fake-default",
    resolvedModel: FAKE_DEFAULT_MODEL,
    displayName: "Fake Default",
    description: "the alias row; resolves to the opus-class model",
    supportsEffort: true,
    supportedEffortLevels: ["low", "medium", "high", "xhigh", "max"],
    supportsAdaptiveThinking: true,
    supportsFastMode: true,
    supportsAutoMode: true,
  },
  {
    value: FAKE_DEFAULT_MODEL,
    displayName: "Fake Opus",
    description: "the offline default; every effort level, fast mode, auto mode",
    supportsEffort: true,
    supportedEffortLevels: ["low", "medium", "high", "xhigh", "max"],
    supportsAdaptiveThinking: true,
    supportsFastMode: true,
    supportsAutoMode: true,
  },
  {
    value: "fake-sonnet-5",
    displayName: "Fake Sonnet",
    description: "three effort levels, fast mode, no auto mode",
    supportsEffort: true,
    supportedEffortLevels: ["low", "medium", "high"],
    supportsAdaptiveThinking: false,
    supportsFastMode: true,
    supportsAutoMode: false,
  },
  {
    value: "fake-haiku-4-5",
    displayName: "Fake Haiku",
    description: "no effort levels, no fast mode, no auto mode",
    supportsEffort: false,
    supportsAdaptiveThinking: false,
    supportsFastMode: false,
    supportsAutoMode: false,
  },
];

/**
 * The slash commands the VENDOR answers itself.
 *
 * `/agents` and `/help` are deliberately absent: the daemon recognizes those
 * and they never reach the shim (kickoff ruling Q1), so a mock that offered
 * them would advertise a path production does not have. `/cost` is present as
 * an ALIAS of `/usage`, which is the only way to exercise the alias column.
 */
export const FAKE_COMMANDS: SlashCommandLike[] = [
  { name: "clear", description: "Clear the conversation and start a new one", argumentHint: "" },
  { name: "compact", description: "Compact the conversation", argumentHint: "[instructions]" },
  { name: "context", description: "Show the context window's occupancy", argumentHint: "" },
  { name: "status", description: "Show the session's status", argumentHint: "" },
  { name: "model", description: "Choose the model", argumentHint: "[model]" },
  {
    name: "usage",
    description: "Show plan usage and cost",
    argumentHint: "",
    aliases: ["cost", "stats"],
  },
  { name: "fake-skill", description: "the offline skill that takes an argument", argumentHint: "<target>" },
];

/** The subagent definitions the mock can spawn. */
export const FAKE_AGENTS: AgentInfoLike[] = [
  { name: "general-purpose", description: "the offline general agent" },
  { name: "Explore", description: "the offline read-only search agent" },
  { name: "fake-pinned", description: "an agent pinned to its own model", model: "fake-haiku-4-5" },
];

/**
 * Every MCP health the SDK declares, one server each.
 *
 * `SessionMcpServer` has five arms and a mock that only ever reported
 * `connected` would leave four unproducible. `echo` is the one whose tools the
 * MCP tool scenario calls, so its `tools` list is populated and the
 * others' are not — which is itself faithful (`tools` is documented as
 * "available when connected").
 */
export const FAKE_MCP_SERVERS: McpServerStatusLike[] = [
  {
    name: "echo",
    status: "connected",
    serverInfo: { name: "echo-server", version: "1.2.3" },
    scope: "project",
    tools: [{ name: "echo", description: "echoes its argument", annotations: { readOnly: true } }],
  },
  { name: "broken", status: "failed", error: "spawn ENOENT", scope: "user" },
  { name: "needs-login", status: "needs-auth", scope: "claudeai" },
  { name: "slow", status: "pending", scope: "local" },
  { name: "switched-off", status: "disabled", scope: "managed" },
];

/** Only the healthy server, for the arm-narrowing scenario. */
export const FAKE_MCP_SERVERS_HEALTHY: McpServerStatusLike[] = [FAKE_MCP_SERVERS[0]];

/** Who the offline session is authenticated as. */
export const FAKE_ACCOUNT_INFO: AccountInfoLike = {
  email: "offline@example.invalid",
  organization: "Offline Org",
  subscriptionType: "max",
  tokenSource: "oauth",
  apiKeySource: "none",
  apiProvider: "firstParty",
};

/** The cached first-connect initialize answer. */
export const FAKE_INITIALIZATION_RESULT: InitializationResultLike = {
  commands: FAKE_COMMANDS,
  agents: FAKE_AGENTS,
  output_style: "default",
  available_output_styles: ["default", "explanatory"],
  models: FAKE_MODELS,
  account: FAKE_ACCOUNT_INFO,
  fast_mode_state: "off",
  fast_mode_disabled_reason: "preference",
};

/**
 * A FULL context-usage answer: every declared field, optional ones included.
 *
 * `isDeferred` on a category, `isLoaded` on an mcp tool, the deferred-builtin
 * and system tool lists, the system-prompt sections, the slash-command and
 * skill rollups, the auto-compact threshold and switch, the message breakdown
 * with both by-type tables, and a non-null `apiUsage` — each is a field some
 * consumer renders, and each is unpopulated by every simpler fake.
 *
 * `growth` MOVES THE OCCUPANCY-DERIVED FIGURES, and nothing else. A drifting
 * answer whose `totalTokens` grew while its `messageBreakdown` stood still
 * would let a consumer that renders the breakdown pass against a mock that
 * never changed it, so the breakdown and the message category are scaled by the
 * same factor the total moved by. The FIXED costs stay fixed on purpose: a
 * system prompt, a memory file and an MCP tool list do not grow because a
 * conversation did, and a mock that grew them would be stating a falsehood the
 * consumer could come to depend on.
 */
export function fakeContextUsage(
  model: string,
  totalTokens: number,
  growth = 0,
): ContextUsageLike {
  const maxTokens = 200_000;
  const scale = 1 + growth;
  const scaled = (tokens: number): number => Math.round(tokens * scale);
  const messageTokens = totalTokens - 9_400;
  return {
    categories: [
      { name: "System prompt", tokens: 3_200, color: "blue", kind: "used" },
      { name: "Messages", tokens: messageTokens, color: "green", kind: "used" },
      { name: "Memory files", tokens: 1_200, color: "yellow", kind: "used" },
      { name: "MCP tools", tokens: 5_000, color: "magenta", isDeferred: true, kind: "deferred" },
    ],
    totalTokens,
    maxTokens,
    rawMaxTokens: 220_000,
    percentage: Math.round((totalTokens / maxTokens) * 100),
    gridRows: [
      [
        { color: "blue", isFilled: true, categoryName: "System prompt", tokens: 3_200, percentage: 2, squareFullness: 1 },
        {
          color: "green",
          isFilled: true,
          categoryName: "Messages",
          tokens: messageTokens,
          percentage: Math.round((messageTokens / maxTokens) * 100),
          squareFullness: 0.5,
        },
      ],
      [
        { color: "yellow", isFilled: false, categoryName: "Memory files", tokens: 1_200, percentage: 1, squareFullness: 0 },
        { color: "magenta", isFilled: false, categoryName: "MCP tools", tokens: 5_000, percentage: 3, squareFullness: 0 },
      ],
    ],
    model,
    memoryFiles: [{ path: "/w/s/CLAUDE.md", type: "Project", tokens: 1_200 }],
    mcpTools: [{ name: "echo", serverName: "echo", tokens: 5_000, isLoaded: true }],
    deferredBuiltinTools: [{ name: "WebSearch", tokens: 400, isLoaded: false }],
    systemTools: [{ name: "Bash", tokens: 900 }],
    systemPromptSections: [{ name: "identity", tokens: 700 }],
    agents: [{ agentType: "general-purpose", source: "builtin", tokens: 300 }],
    slashCommands: { totalCommands: FAKE_COMMANDS.length, includedCommands: FAKE_COMMANDS.length, tokens: 250 },
    skills: {
      totalSkills: 2,
      includedSkills: 1,
      tokens: 180,
      skillFrontmatter: [{ name: "fake-skill", source: "userSettings", tokens: 180 }],
    },
    autoCompactThreshold: 0.85,
    isAutoCompactEnabled: true,
    messageBreakdown: {
      toolCallTokens: scaled(1_100),
      toolResultTokens: scaled(2_400),
      attachmentTokens: scaled(600),
      assistantMessageTokens: scaled(3_000),
      userMessageTokens: scaled(900),
      redirectedContextTokens: scaled(120),
      unattributedTokens: scaled(80),
      toolCallsByType: [{ name: "Bash", callTokens: scaled(400), resultTokens: scaled(1_800) }],
      attachmentsByType: [{ name: "nested_memory", tokens: scaled(600) }],
    },
    apiUsage: {
      input_tokens: 12,
      output_tokens: 340,
      cache_creation_input_tokens: 4_000,
      cache_read_input_tokens: 90_000,
    },
  };
}

const window = (utilization: number | null, resetsAt: string | null) => ({ utilization, resets_at: resetsAt });

/**
 * HOW FAR AFTER THE FAKE'S OWN NOW EACH SAMPLED WINDOW RESETS.
 *
 * These used to be ABSOLUTE instants (`2026-08-29T20:00:00.000Z` and
 * `2026-09-02T00:00:00.000Z`), which meant every countdown a consumer drew off
 * a SAMPLE read `resets in 0m` the moment those instants fell into the past —
 * while a countdown drawn off a rate-limit EVENT (minted at `now + 3600s`)
 * counted down properly. A footer showing `0m` for one source and a live
 * countdown for the other is not a shape the vendor has; it was the fixture
 * rotting.
 *
 * So a window is stated as an OFFSET from the fake's own clock — the same
 * `nowMs` seam the transcript stamps and the rate-limit event already use, and
 * the only notion of "now" the fake has. The goldens stay deterministic because
 * that clock is injected: a fixed clock in, fixed instants out.
 *
 * The offsets are the windows' own natural lengths, so a reader of the drawn
 * countdown sees a plausible session/weekly reset rather than an arbitrary one.
 */
export const FAKE_SESSION_WINDOW_RESETS_IN_MS = 5 * 60 * 60 * 1_000;
export const FAKE_WEEKLY_WINDOW_RESETS_IN_MS = 7 * 24 * 60 * 60 * 1_000;

/** That offset as the vendor spells an instant, off the fake's own clock. */
const resetsAfter = (nowMs: number, offsetMs: number): string =>
  new Date(nowMs + offsetMs).toISOString();

const FAKE_SESSION_COST = {
  total_cost_usd: 0.1234,
  total_api_duration_ms: 4_200,
  total_duration_ms: 9_100,
  total_lines_added: 42,
  total_lines_removed: 7,
  model_usage: {
    [FAKE_DEFAULT_MODEL]: {
      inputTokens: 12,
      outputTokens: 340,
      cacheReadInputTokens: 90_000,
      cacheCreationInputTokens: 4_000,
      webSearchRequests: 1,
      costUSD: 0.1234,
      contextWindow: 200_000,
      maxOutputTokens: 32_000,
      canonicalModel: FAKE_DEFAULT_MODEL,
      provider: "firstParty",
    },
  },
};

const FAKE_BEHAVIOR_WINDOW = {
  request_count: 120,
  session_count: 4,
  behaviors: [
    { key: "cache_miss" as const, pct: 12, count: 14 },
    { key: "subagent_heavy" as const, pct: 30, count: 36 },
  ],
  agents: [{ name: "general-purpose", pct: 30 }],
  skills: [{ name: "fake-skill", pct: 10 }],
  plugins: [{ name: "fake-plugin", pct: 5 }],
  mcp_servers: [{ name: "echo", pct: 2 }],
};

/**
 * The windows the account usage samples, by the name a `rate_limit_event`'s
 * `rateLimitType` gives them.
 */
const SAMPLED_WINDOWS = ["five_hour", "seven_day", "seven_day_opus", "seven_day_sonnet"] as const;

/**
 * The utilization PERCENT each sampled window was last announced at by a
 * `rate_limit_event` the fake emitted.
 */
export type AnnouncedWindows = Partial<Record<(typeof SAMPLED_WINDOWS)[number], number>>;

/**
 * File one emitted vendor message into `announced` when it is a
 * `rate_limit_event` naming a sampled window with a utilization.
 *
 * ONE ACCOUNT, ONE FIGURE PER WINDOW. The event states a window's utilization
 * as a 0..1 fraction and the usage endpoint as a percent, but both describe the
 * same account: a fake that announced the seven-day window at 91% and then
 * sampled it at 63% modelled no account there is, and made the footer's drawn
 * line depend on which of two streams reached the daemon first. Every sample
 * after an event therefore reports the event's figure for its window.
 */
export function noteAnnouncedWindow(announced: AnnouncedWindows, message: Record<string, unknown>): void {
  if (message.type !== "rate_limit_event") return;
  const info = message.rate_limit_info as { rateLimitType?: unknown; utilization?: unknown } | undefined;
  const name = info?.rateLimitType;
  const utilization = info?.utilization;
  if (typeof utilization !== "number") return;
  const sampled = SAMPLED_WINDOWS.find((w) => w === name);
  if (sampled === undefined) return;
  announced[sampled] = Math.round(utilization * 100);
}

/** A per-seat account's spend: $223.88 of a $12,000 monthly allotment. */
const FAKE_SEAT_SPEND = {
  is_enabled: true,
  monthly_limit: 1_200_000,
  used_credits: 22_388,
  utilization: 1.87,
  currency: "USD",
};

/**
 * The account-usage answer for one arm.
 *
 * `SessionAccountUsage` has an available arm and FOUR unavailable reasons, and
 * each reason is a DIFFERENT shape on the wire: the service answering nothing
 * (`rate_limits: null`), one window missing, one window's utilization null, and
 * the local-transcript scan failing (`behaviors: null`). A single canned answer
 * could produce only the first arm, so the mock keeps all five and a scenario
 * picks.
 */
export function fakeAccountUsage(
  arm: AccountUsageArm,
  nowMs: number,
  announced: AnnouncedWindows = {},
): AccountUsageLike {
  const base = {
    session: FAKE_SESSION_COST,
    subscription_type: "max",
    rate_limits_available: true,
    behaviors: { day: FAKE_BEHAVIOR_WINDOW, week: FAKE_BEHAVIOR_WINDOW },
  };
  const sessionResetsAt = resetsAfter(nowMs, FAKE_SESSION_WINDOW_RESETS_IN_MS);
  const weeklyResetsAt = resetsAfter(nowMs, FAKE_WEEKLY_WINDOW_RESETS_IN_MS);
  const allWindows = {
    five_hour: window(announced.five_hour ?? 41, sessionResetsAt),
    seven_day: window(announced.seven_day ?? 63, weeklyResetsAt),
    seven_day_oauth_apps: window(5, weeklyResetsAt),
    seven_day_opus: window(announced.seven_day_opus ?? 77, weeklyResetsAt),
    seven_day_sonnet: window(announced.seven_day_sonnet ?? 21, weeklyResetsAt),
    model_scoped: [{ display_name: "Fable", utilization: 12, resets_at: weeklyResetsAt }],
    extra_usage: { is_enabled: true, monthly_limit: 100, used_credits: 13, utilization: 13, currency: "USD" },
  };
  switch (arm) {
    case "available":
      return { ...base, rate_limits: allWindows };
    case "service_unavailable":
      // The endpoint answered nothing at all.
      return { ...base, rate_limits_available: false, rate_limits: null };
    case "opus_absent":
      // An ABSENT OPTIONAL WINDOW, which is NOT an unavailability: the service
      // answered in full and this account simply has no opus window, so the
      // contract's arm is still `available` with `seven_day_opus` unset.
      return { ...base, rate_limits: { ...allWindows, seven_day_opus: null } };
    case "window_unavailable":
      // THE FIVE-HOUR WINDOW, specifically. `SessionUsageWindowUnavailable`
      // means "the service answered without a five-hour window" and nothing
      // else; nulling any other window would leave this reason unproducible
      // while looking as though it had been covered.
      return { ...base, rate_limits: { ...allWindows, five_hour: null } };
    case "utilization_unavailable":
      // The window exists and its utilization does not.
      return { ...base, rate_limits: { ...allWindows, five_hour: window(null, sessionResetsAt) } };
    case "seat_spend":
      // A PER-SEAT ENTERPRISE ACCOUNT, in the shape the work account's usage
      // answer takes (the vendor CLI's cached answer, 2026-10-06): every
      // window null, the seat's monthly allotment and its month-to-date spend
      // in minor units under `extra_usage`.
      return {
        ...base,
        subscription_type: "enterprise",
        rate_limits: { five_hour: null, seven_day: null, extra_usage: FAKE_SEAT_SPEND },
      };
    case "seat_spend_unreported":
      // The same seat before the vendor reports any spend figure.
      return {
        ...base,
        subscription_type: "enterprise",
        rate_limits: { five_hour: null, seven_day: null, extra_usage: { ...FAKE_SEAT_SPEND, used_credits: null, utilization: null } },
      };
    case "sampling_failure":
      // THE SHIM'S OWN SAMPLING FAILING IS A THROW, NOT A SHAPE. This arm used
      // to answer `{ ...base, behaviors: null }`, which the converter never
      // reads: `accountUsageUpdate` (engine/session.ts) branches only on
      // `rate_limits_available`, `rate_limits` and the five-hour window, so a
      // null `behaviors` produced the AVAILABLE outcome and the scenario named
      // an arm it could not reach — a dead trigger that looked covered.
      //
      // `sampling_failure` has exactly one producer in the shim: the catch
      // around the usage probe, which states the thrown error's message as the
      // cause. So the mock of "the shim's own sampling failed" is the probe
      // raising, and this is where it raises.
      throw new Error("the local transcript scan that produces `behaviors` failed");
  }
}

/**
 * The level the mocked vendor sends when nothing asked for one: the CLI's own
 * per-model default is internal to it, so the mock names one fixed level for
 * every model that takes a level.
 */
export const FAKE_DEFAULT_EFFORT: EffortLevelLike = "high";

/**
 * The level the mocked vendor states its next request sends for MODEL, as the
 * CLI does: none for a model that takes none, the asked-for level where the
 * model accepts it, and the default otherwise (the CLI runs an unsupported
 * `max` "as `high`", sdk.d.ts applyFlagSettings).
 */
export function fakeAppliedEffort(model: string, asked: EffortLevelLike | undefined): EffortLevelLike | null {
  const row = FAKE_MODELS.find((candidate) => candidate.value === model);
  if (row?.supportsEffort !== true) return null;
  if (asked !== undefined && (row.supportedEffortLevels ?? []).includes(asked)) return asked;
  return FAKE_DEFAULT_EFFORT;
}
