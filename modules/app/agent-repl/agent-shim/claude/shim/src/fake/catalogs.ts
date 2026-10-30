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
  AgentInfoLike,
  ContextUsageLike,
  EffortLevelLike,
  InitializationResultLike,
  McpServerStatusLike,
  ModelInfoLike,
  SlashCommandLike,
} from "../sdk/types.js";

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
