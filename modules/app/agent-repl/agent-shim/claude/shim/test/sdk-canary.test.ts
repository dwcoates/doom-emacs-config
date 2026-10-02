/**
 * THE SDK UPGRADE CANARY.
 *
 * `docs/overhaul/shim.md` pins the Claude Agent SDK in the lockfile and
 * assigns this suite the job of failing LOUDLY the moment an upgrade removes
 * or reshapes anything the shim relies on. Until the project lead dispatches
 * the one-time real capture run (`scripts/capture/`), this suite and the
 * `testdata/corpus` fixtures ARE the repository's golden knowledge of the
 * vendor — so a silent reshape here is a silent reshape of the contract.
 *
 * HOW IT READS THE SDK: as TEXT. It opens `sdk.d.ts` and `sdk-tools.d.ts` and
 * matches declarations with regexes. It deliberately does NOT import the SDK:
 * `AGENT_REPL_FORBID_VENDOR_CALLS` is set for the whole suite (test/setup.ts)
 * and `src/vendor-guard.ts` is the one place allowed to load the vendor. A
 * canary that imported the package to reflect on it would be the second import
 * site, and the guard would stop being structural.
 *
 * WHY REGEXES AND NOT A TYPE-LEVEL TEST: a `satisfies`-style compile-time
 * check answers "does our usage still typecheck", which is a weaker question.
 * A field the vendor turned optional, a method whose return type changed name,
 * a message subtype that quietly vanished from a union — all of those can keep
 * compiling while breaking the fold. Matching the declarations catches the
 * reshape itself.
 *
 * ONE TEST PER ITEM, on purpose: an upgrade should produce a list of exactly
 * what moved, named individually, not one assertion that says "something".
 *
 * Every assertion is anchored on the shape of a declaration (line starts,
 * indentation depth, brace nesting) and never on a line number.
 */
import { existsSync, readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

/** The vendor package's directory name under a `node_modules`. */
const SDK_PACKAGE = path.join("@anthropic-ai", "claude-agent-sdk");

/**
 * Locate the INSTALLED SDK directory by walking up from this file.
 *
 * Node's own resolver is not used because the package's `exports` map does not
 * publish `./package.json`, and this suite reads the raw declaration files
 * rather than any published entrypoint. Walking the tree also states the right
 * thing: the canary grades whatever is actually installed here, hoisted or not.
 */
function findSdkDir(): string {
  let dir = path.dirname(fileURLToPath(import.meta.url));
  for (;;) {
    const candidate = path.join(dir, "node_modules", SDK_PACKAGE);
    if (existsSync(path.join(candidate, "sdk.d.ts"))) return candidate;
    const parent = path.dirname(dir);
    if (parent === dir) {
      throw new Error(
        `sdk canary: no installed ${SDK_PACKAGE} found above ${fileURLToPath(import.meta.url)}`,
      );
    }
    dir = parent;
  }
}

const SDK_DIR = findSdkDir();

/**
 * Read a declaration file with its line endings normalized to `\n`.
 *
 * The published `.d.ts` files use CRLF. Every anchor in this suite is a
 * line-start or line-end anchor, so a stray `\r` would silently defeat them
 * and report the whole SDK as missing — normalizing once here keeps each
 * assertion about the SDK rather than about whitespace.
 */
function readDeclarations(file: string): string {
  return readFileSync(path.join(SDK_DIR, file), "utf8").replace(/\r\n/g, "\n");
}

const SDK_DTS = readDeclarations("sdk.d.ts");
const SDK_TOOLS_DTS = readDeclarations("sdk-tools.d.ts");

const SDK_PACKAGE_JSON = JSON.parse(
  readFileSync(path.join(SDK_DIR, "package.json"), "utf8"),
) as { version: string };

const LOCKFILE = JSON.parse(
  readFileSync(new URL("../package-lock.json", import.meta.url), "utf8"),
) as { packages: Record<string, { version: string }> };

/**
 * The version this wave was built and vetted against.
 *
 * Bumping the SDK means bumping this constant DELIBERATELY, after reading what
 * this suite reports. It is spelled out rather than read from the lockfile so
 * that a lockfile bump alone cannot slip through green.
 */
const PINNED_SDK_VERSION = "0.3.280";

/** The lockfile key the SDK is pinned under. */
const LOCK_KEY = "node_modules/@anthropic-ai/claude-agent-sdk";

/**
 * Extract one top-level declaration's body text.
 *
 * The `.d.ts` files put every top-level declaration's closing brace at column
 * zero, so the block runs from the declaration keyword to the first `\n}` that
 * begins a line. That rule survives UNION declarations (`PermissionResult`,
 * `PermissionUpdate`) whose inner `} | {` braces are indented, which a naive
 * first-closing-brace scan would truncate.
 */
function declarationBody(source: string, name: string): string {
  const start = new RegExp(
    `^(?:export )?declare (?:type|interface) ${name}\\b`,
    "m",
  ).exec(source);
  if (start === null) {
    throw new Error(`sdk canary: no declaration of ${name} found`);
  }
  const rest = source.slice(start.index);
  const end = /\n\}[;]?(?:\n|$)/.exec(rest);
  if (end === null) {
    throw new Error(`sdk canary: declaration of ${name} is unterminated`);
  }
  return rest.slice(0, end.index + end[0].length);
}

/** Extract an `export type|interface NAME` body from sdk-tools.d.ts. */
function toolsDeclarationBody(name: string): string | null {
  const start = new RegExp(`^export (?:type|interface) ${name}\\b`, "m").exec(
    SDK_TOOLS_DTS,
  );
  if (start === null) return null;
  const rest = SDK_TOOLS_DTS.slice(start.index);
  const end = /\n\}[;]?(?:\n|$)|;\n/.exec(rest);
  return end === null ? rest : rest.slice(0, end.index + end[0].length);
}

/**
 * The signature text of one method of an interface body: from the method name
 * at the start of a line through the terminating semicolon.
 *
 * Anchoring on line-start indentation keeps a doc comment (whose lines begin
 * with `*`) from ever being mistaken for a declaration.
 */
function methodSignature(body: string, name: string): string | null {
  const at = new RegExp(`^[ \\t]+${name}\\s*\\(`, "m").exec(body);
  if (at === null) return null;
  const rest = body.slice(at.index);
  // THE PARAMETER LIST IS SKIPPED BY DEPTH, not by the first `;`: a method
  // whose parameter is an inline options object (`getContextUsage(opts?: {
  // detail?: ...; })`) carries semicolons INSIDE its parentheses, and cutting
  // there would lose the return type the signature is read for.
  let depth = 0;
  let close = -1;
  for (let i = rest.indexOf("("); i < rest.length; i++) {
    if (rest[i] === "(") depth++;
    else if (rest[i] === ")" && --depth === 0) {
      close = i;
      break;
    }
  }
  if (close === -1) return rest;
  const semicolon = rest.indexOf(";", close);
  return semicolon === -1 ? rest : rest.slice(0, semicolon + 1);
}

/** Whether an object-type body declares KEY at its own top level (4 spaces). */
function hasTopLevelKey(body: string, key: string): boolean {
  return new RegExp(`^ {4}${key}\\??:`, "m").test(body);
}

/** Whether an object-type body declares KEY at ANY nesting depth. */
function hasKeyAtAnyDepth(body: string, key: string): boolean {
  return new RegExp(`^\\s+${key}\\??:`, "m").test(body);
}

/**
 * Whether a literal appears as a discriminator value anywhere in sdk.d.ts.
 *
 * TWO SPELLINGS, both real at 0.3.220 and both handled here rather than in the
 * caller: most message kinds are declared as `subtype: 'x'`, but
 * `rate_limit_event` is a `type: 'rate_limit_event'` and the four result error
 * arms share ONE line as a union
 * (`subtype: 'error_during_execution' | 'error_max_turns' | …`). Matching any
 * discriminator LINE that contains the quoted literal covers all three shapes
 * without caring which one the vendor happens to use next release.
 */
function hasDiscriminatorLiteral(literal: string): boolean {
  const quoted = `'${literal}'`;
  return SDK_DTS.split("\n").some(
    (line) => /^\s*(?:subtype|type)\??:/.test(line) && line.includes(quoted),
  );
}

const QUERY = declarationBody(SDK_DTS, "Query");
const OPTIONS = declarationBody(SDK_DTS, "Options");
const PERMISSION_RESULT = declarationBody(SDK_DTS, "PermissionResult");
const PERMISSION_UPDATE = declarationBody(SDK_DTS, "PermissionUpdate");
const CAN_USE_TOOL = declarationBody(SDK_DTS, "CanUseTool");
const CONTEXT_USAGE = declarationBody(SDK_DTS, "SDKControlGetContextUsageResponse");
const GET_USAGE = declarationBody(SDK_DTS, "SDKControlGetUsageResponse");

// ---------------------------------------------------------------- the pin

describe("the pin", () => {
  it("has the installed package at the vetted version", () => {
    expect(SDK_PACKAGE_JSON.version).toBe(PINNED_SDK_VERSION);
  });

  it("has the lockfile pinning the same version the installed package reports", () => {
    expect(LOCKFILE.packages[LOCK_KEY]?.version).toBe(SDK_PACKAGE_JSON.version);
  });

  it("has the lockfile pinning the vetted version", () => {
    expect(LOCKFILE.packages[LOCK_KEY]?.version).toBe(PINNED_SDK_VERSION);
  });
});

// ---------------------------------------------------------------- Query

/**
 * Every `Query` method the shim drives, with the return type its call site
 * depends on. The engine awaits these answers and folds them onto the wire, so
 * a changed return type is a changed contract even when the name survives.
 */
const QUERY_METHODS: ReadonlyArray<readonly [string, string]> = [
  ["interrupt", "Promise<SDKControlInterruptResponse | undefined>"],
  ["setPermissionMode", "Promise<void>"],
  ["setModel", "Promise<void>"],
  ["applyFlagSettings", "Promise<void>"],
  ["supportedModels", "Promise<ModelInfo[]>"],
  ["supportedCommands", "Promise<SlashCommand[]>"],
  ["supportedAgents", "Promise<AgentInfo[]>"],
  ["mcpServerStatus", "Promise<McpServerStatus[]>"],
  ["getContextUsage", "Promise<SDKControlGetContextUsageResponse>"],
  [
    "usage_EXPERIMENTAL_MAY_CHANGE_DO_NOT_RELY_ON_THIS_API_YET",
    "Promise<SDKControlGetUsageResponse>",
  ],
  ["accountInfo", "Promise<AccountInfo>"],
  ["initializationResult", "Promise<SDKControlInitializeResponse>"],
  ["stopTask", "Promise<void>"],
  ["backgroundTasks", "Promise<boolean>"],
  ["streamInput", "Promise<void>"],
  ["close", "void"],
];

/**
 * THE ONE UNDECLARED MEMBER. `Query.getSettings()` exists only in the SDK's
 * runtime (`sdk.mjs`), so it is graded there, together with the declared
 * request it sends and the declared meaning of the one field the shim reads
 * (`applied.effort`, see src/sdk/types.ts AppliedSettingsLike). A vendor that
 * drops or renames any of the three fails here, not in a session.
 */
describe("the undeclared getSettings the shim relies on", () => {
  const SDK_MJS = readDeclarations("sdk.mjs");

  it("is provided by the runtime query as the get_settings control request", () => {
    expect(SDK_MJS).toMatch(/async getSettings\(\)\{return\(await this\.request\(\{subtype:"get_settings"\}\)\)\.response\}/);
  });

  it("is a request the declarations still name", () => {
    expect(SDK_DTS).toMatch(/subtype: 'get_settings';/);
  });

  it("answers applied.effort, the level the next request sends", () => {
    expect(SDK_DTS).toContain("the same value get_settings reports as applied.effort");
  });
});

describe("Query methods the shim relies on", () => {
  for (const [name, returnType] of QUERY_METHODS) {
    it(`declares ${name}`, () => {
      expect(methodSignature(QUERY, name)).not.toBeNull();
    });

    it(`declares ${name} returning ${returnType}`, () => {
      expect(methodSignature(QUERY, name)).toContain(`: ${returnType}`);
    });
  }

  it("declares Query as an AsyncGenerator over SDKMessage — the fold's whole input", () => {
    expect(QUERY).toMatch(/interface Query extends AsyncGenerator<SDKMessage, void>/);
  });
});

// ---------------------------------------------------------------- Options

/**
 * Every `Options` key the shim sets. Several are load-bearing in ways nothing
 * else recovers: without `settingSources` the vendor never emits the
 * permission-denied messages the gate relies on, and without
 * `forwardSubagentText` a subagent's prose never reaches the shim at all.
 */
const OPTIONS_KEYS = [
  "sessionId",
  "resume",
  "forkSession",
  "resumeSessionAt",
  "cwd",
  "model",
  "permissionMode",
  "canUseTool",
  "includePartialMessages",
  "forwardSubagentText",
  "settingSources",
  "systemPrompt",
  "abortController",
  "env",
  "hooks",
  "persistSession",
] as const;

describe("Options keys the shim sets", () => {
  for (const key of OPTIONS_KEYS) {
    it(`declares Options.${key}`, () => {
      expect(hasTopLevelKey(OPTIONS, key)).toBe(true);
    });
  }

  it("still accepts the claude_code preset as a systemPrompt", () => {
    expect(OPTIONS).toMatch(/preset/);
    expect(SDK_DTS).toMatch(/'claude_code'/);
  });

  it("still declares SettingSource as user | project | local", () => {
    expect(SDK_DTS).toMatch(
      /declare type SettingSource = 'user' \| 'project' \| 'local'/,
    );
  });
});

// ---------------------------------------------------------------- messages

/**
 * Every message kind the fold dispatches on.
 *
 * `rate_limit_event` is spelled `type:` rather than `subtype:` at 0.3.220, and
 * the four result error arms share one union line; `hasDiscriminatorLiteral`
 * absorbs both spellings so this list stays a plain list of what we consume.
 */
const MESSAGE_KINDS = [
  "init",
  "task_started",
  "task_updated",
  "task_notification",
  "task_progress",
  "background_tasks_changed",
  "status",
  "compact_boundary",
  "hook_started",
  "hook_response",
  "notification",
  "permission_denied",
  "api_retry",
  "session_state_changed",
  "thinking_tokens",
  "rate_limit_event",
  "informational",
  "model_refusal_fallback",
  "model_refusal_no_fallback",
  "local_command_output",
] as const;

describe("message kinds the fold dispatches on", () => {
  for (const kind of MESSAGE_KINDS) {
    it(`declares the ${kind} message kind`, () => {
      expect(hasDiscriminatorLiteral(kind)).toBe(true);
    });
  }
});

/** The result subtypes the 16-arm turn-terminal taxonomy maps from. */
const RESULT_SUBTYPES = [
  "success",
  "error_during_execution",
  "error_max_turns",
  "error_max_budget_usd",
  "error_max_structured_output_retries",
] as const;

describe("result subtypes the turn terminals map from", () => {
  for (const subtype of RESULT_SUBTYPES) {
    it(`declares the ${subtype} result subtype`, () => {
      expect(hasDiscriminatorLiteral(subtype)).toBe(true);
    });
  }

  it("keeps the four error subtypes on one union, the spelling this canary handles", () => {
    expect(SDK_DTS).toMatch(
      /subtype: 'error_during_execution' \| 'error_max_turns' \| 'error_max_budget_usd' \| 'error_max_structured_output_retries';/,
    );
  });

  it("keeps rate_limit_event spelled as a `type`, not a `subtype`", () => {
    expect(SDK_DTS).toMatch(/^\s*type: 'rate_limit_event';/m);
  });
});

// ---------------------------------------------------------------- the gate

describe("the permission gate's shapes", () => {
  it("declares CanUseTool taking the tool name, the input, and an options bag", () => {
    expect(CAN_USE_TOOL).toMatch(
      /CanUseTool = \(toolName: string, input: Record<string, unknown>, options: \{/,
    );
  });

  it("offers the vendor's own standing suggestions on the gate call", () => {
    expect(CAN_USE_TOOL).toMatch(/suggestions\?: PermissionUpdate\[\]/);
  });

  it("signals the gate call with an AbortSignal, which teardown depends on", () => {
    expect(CAN_USE_TOOL).toMatch(/signal: AbortSignal/);
  });

  it("declares the allow arm of PermissionResult", () => {
    expect(PERMISSION_RESULT).toMatch(/behavior: 'allow';/);
  });

  it("declares the deny arm of PermissionResult", () => {
    expect(PERMISSION_RESULT).toMatch(/behavior: 'deny';/);
  });

  it("declares PermissionResult.updatedInput", () => {
    expect(PERMISSION_RESULT).toMatch(/updatedInput\?: Record<string, unknown>;/);
  });

  it("declares PermissionResult.updatedPermissions, the standing grant's transport", () => {
    expect(PERMISSION_RESULT).toMatch(/updatedPermissions\?: PermissionUpdate\[\];/);
  });

  it("declares PermissionResult.message on the deny arm", () => {
    expect(PERMISSION_RESULT).toMatch(/message: string;/);
  });

  it("declares PermissionResult.interrupt on the deny arm", () => {
    expect(PERMISSION_RESULT).toMatch(/interrupt\?: boolean;/);
  });

  it("declares the setMode arm of PermissionUpdate, which a standing grant can carry", () => {
    expect(PERMISSION_UPDATE).toMatch(/type: 'setMode';/);
  });

  it("declares the addRules arm of PermissionUpdate", () => {
    expect(PERMISSION_UPDATE).toMatch(/type: 'addRules';/);
  });

  it("declares a destination on every PermissionUpdate arm", () => {
    const arms = PERMISSION_UPDATE.split(/\n\} \| \{\n/);
    for (const arm of arms) {
      expect(arm).toMatch(/destination: PermissionUpdateDestination;/);
    }
  });

  it("declares PermissionMode with every mode the shim can be set to", () => {
    for (const mode of ["default", "acceptEdits", "bypassPermissions", "plan"]) {
      expect(declarationBody(SDK_DTS, "PermissionMode")).toContain(`'${mode}'`);
    }
  });
});

// ---------------------------------------------------------------- context usage

/**
 * `conversation.v1.SessionContextUsage` is the vendor's `get_context_usage`
 * answer typed field for field. Each row is [our proto field, the vendor field
 * it reads] so a failure names BOTH halves — the reader should not have to go
 * find which of our fields just lost its source.
 *
 * Three spellings differ on purpose and are pinned here so a drift is caught:
 * `categories[].label` reads the vendor's `name`, `is_deferred` reads
 * `isDeferred`, and `mcp_tools[].is_loaded` reads `isLoaded`.
 */
const CONTEXT_USAGE_FIELDS: ReadonlyArray<readonly [string, string]> = [
  ["total_tokens", "totalTokens"],
  ["max_tokens", "maxTokens"],
  ["raw_max_tokens", "rawMaxTokens"],
  ["percentage", "percentage"],
  ["model", "model"],
  ["categories", "categories"],
  ["categories[].label", "name"],
  ["categories[].tokens", "tokens"],
  ["categories[].color", "color"],
  ["categories[].is_deferred", "isDeferred"],
  ["memory_files", "memoryFiles"],
  ["mcp_tools", "mcpTools"],
  ["mcp_tools[].server_name", "serverName"],
  ["mcp_tools[].is_loaded", "isLoaded"],
  ["deferred_builtin_tools", "deferredBuiltinTools"],
  ["system_tools", "systemTools"],
  ["system_prompt_sections", "systemPromptSections"],
  ["agents", "agents"],
  ["agents[].agent_type", "agentType"],
  ["agents[].source", "source"],
  ["slash_commands", "slashCommands"],
  ["slash_commands.total_commands", "totalCommands"],
  ["slash_commands.included_commands", "includedCommands"],
  ["skills", "skills"],
  ["skills.total_skills", "totalSkills"],
  ["skills.included_skills", "includedSkills"],
  ["skills.skill_frontmatter", "skillFrontmatter"],
  ["auto_compact_threshold", "autoCompactThreshold"],
  ["is_auto_compact_enabled", "isAutoCompactEnabled"],
  ["message_breakdown", "messageBreakdown"],
  ["message_breakdown.tool_call_tokens", "toolCallTokens"],
  ["message_breakdown.tool_result_tokens", "toolResultTokens"],
  ["message_breakdown.attachment_tokens", "attachmentTokens"],
  ["message_breakdown.assistant_message_tokens", "assistantMessageTokens"],
  ["message_breakdown.user_message_tokens", "userMessageTokens"],
  ["message_breakdown.redirected_context_tokens", "redirectedContextTokens"],
  ["message_breakdown.unattributed_tokens", "unattributedTokens"],
  ["message_breakdown.tool_calls_by_type", "toolCallsByType"],
  ["message_breakdown.tool_calls_by_type[].call_tokens", "callTokens"],
  ["message_breakdown.tool_calls_by_type[].result_tokens", "resultTokens"],
  ["message_breakdown.attachments_by_type", "attachmentsByType"],
  ["api_usage", "apiUsage"],
  ["api_usage.input_tokens", "input_tokens"],
  ["api_usage.output_tokens", "output_tokens"],
  ["api_usage.cache_creation_input_tokens", "cache_creation_input_tokens"],
  ["api_usage.cache_read_input_tokens", "cache_read_input_tokens"],
];

describe("SDKControlGetContextUsageResponse — every field SessionContextUsage carries", () => {
  for (const [protoField, vendorField] of CONTEXT_USAGE_FIELDS) {
    it(`sources conversation.v1.SessionContextUsage.${protoField} from the vendor's ${vendorField}`, () => {
      expect(hasKeyAtAnyDepth(CONTEXT_USAGE, vendorField)).toBe(true);
    });
  }

  it("still declares gridRows, the one field SessionContextUsage deliberately omits", () => {
    // Pure presentation data (pre-rendered grid squares) the resolvers compose
    // for themselves. Pinned so its disappearance is a noticed non-event rather
    // than an unnoticed one.
    expect(hasTopLevelKey(CONTEXT_USAGE, "gridRows")).toBe(true);
  });
});

// ---------------------------------------------------------------- usage windows

/** The plan rate-limit windows the account_usage arm reports. */
const USAGE_WINDOWS = [
  "five_hour",
  "seven_day",
  "seven_day_oauth_apps",
  "seven_day_opus",
  "seven_day_sonnet",
  "model_scoped",
  "extra_usage",
] as const;

describe("SDKControlGetUsageResponse", () => {
  for (const window of USAGE_WINDOWS) {
    it(`declares the ${window} rate-limit window`, () => {
      expect(hasKeyAtAnyDepth(GET_USAGE, window)).toBe(true);
    });
  }

  it("declares the session cost and usage totals", () => {
    expect(hasTopLevelKey(GET_USAGE, "session")).toBe(true);
  });

  it("declares the per-model usage breakdown on the session totals", () => {
    expect(GET_USAGE).toMatch(/model_usage: Record<string, coreTypes\.ModelUsage>;/);
  });

  it("declares subscription_type", () => {
    expect(hasTopLevelKey(GET_USAGE, "subscription_type")).toBe(true);
  });

  it("declares rate_limits_available, the discriminator for plan limits applying at all", () => {
    expect(hasTopLevelKey(GET_USAGE, "rate_limits_available")).toBe(true);
  });

  it("declares rate_limits", () => {
    expect(hasTopLevelKey(GET_USAGE, "rate_limits")).toBe(true);
  });

  it("declares utilization on a window", () => {
    expect(hasKeyAtAnyDepth(GET_USAGE, "utilization")).toBe(true);
  });

  it("declares resets_at on a window", () => {
    expect(hasKeyAtAnyDepth(GET_USAGE, "resets_at")).toBe(true);
  });
});

// ---------------------------------------------------------------- tool types

/**
 * Every modeled tool kind's declared input and output type, by the EXACT
 * exported name found in `sdk-tools.d.ts`.
 *
 * `output: null` records a kind whose input is declared but whose output is
 * not; those are asserted ABSENT below rather than guessed at here.
 */
const TOOL_TYPES: ReadonlyArray<{
  kind: string;
  input: string;
  output: string | null;
}> = [
  { kind: "FileRead", input: "FileReadInput", output: "FileReadOutput" },
  { kind: "FileWrite", input: "FileWriteInput", output: "FileWriteOutput" },
  { kind: "FileEdit", input: "FileEditInput", output: "FileEditOutput" },
  { kind: "Grep", input: "GrepInput", output: "GrepOutput" },
  { kind: "Glob", input: "GlobInput", output: "GlobOutput" },
  { kind: "Bash", input: "BashInput", output: "BashOutput" },
  { kind: "Agent", input: "AgentInput", output: "AgentOutput" },
  { kind: "TaskCreate", input: "TaskCreateInput", output: "TaskCreateOutput" },
  { kind: "TaskUpdate", input: "TaskUpdateInput", output: "TaskUpdateOutput" },
  { kind: "TaskGet", input: "TaskGetInput", output: "TaskGetOutput" },
  { kind: "TaskList", input: "TaskListInput", output: "TaskListOutput" },
  { kind: "TaskStop", input: "TaskStopInput", output: "TaskStopOutput" },
  { kind: "WebFetch", input: "WebFetchInput", output: "WebFetchOutput" },
  { kind: "WebSearch", input: "WebSearchInput", output: "WebSearchOutput" },
  { kind: "Monitor", input: "MonitorInput", output: "MonitorOutput" },
  {
    kind: "ScheduleWakeup",
    input: "ScheduleWakeupInput",
    output: "ScheduleWakeupOutput",
  },
  { kind: "Artifact", input: "ArtifactInput", output: "ArtifactOutput" },
  {
    kind: "EnterPlanMode",
    input: "EnterPlanModeInput",
    output: "EnterPlanModeOutput",
  },
  {
    kind: "ExitPlanMode",
    input: "ExitPlanModeInput",
    output: "ExitPlanModeOutput",
  },
  {
    kind: "ReportFindings",
    input: "ReportFindingsInput",
    output: "ReportFindingsOutput",
  },
  {
    kind: "EnterWorktree",
    input: "EnterWorktreeInput",
    output: "EnterWorktreeOutput",
  },
  {
    kind: "ExitWorktree",
    input: "ExitWorktreeInput",
    output: "ExitWorktreeOutput",
  },
  { kind: "CronCreate", input: "CronCreateInput", output: "CronCreateOutput" },
  { kind: "CronDelete", input: "CronDeleteInput", output: "CronDeleteOutput" },
  { kind: "CronList", input: "CronListInput", output: "CronListOutput" },
  {
    kind: "PushNotification",
    input: "PushNotificationInput",
    output: "PushNotificationOutput",
  },
  {
    kind: "AskUserQuestion",
    input: "AskUserQuestionInput",
    output: "AskUserQuestionOutput",
  },
  {
    kind: "NotebookEdit",
    input: "NotebookEditInput",
    output: "NotebookEditOutput",
  },
  {
    kind: "ListMcpResources",
    input: "ListMcpResourcesInput",
    output: "ListMcpResourcesOutput",
  },
  {
    kind: "ReadMcpResource",
    input: "ReadMcpResourceInput",
    output: "ReadMcpResourceOutput",
  },
];

const TOOL_INPUT_UNION = toolsDeclarationBody("ToolInputSchemas") ?? "";
const TOOL_OUTPUT_UNION = toolsDeclarationBody("ToolOutputSchemas") ?? "";

describe("tool input and output types, by their exact exported names", () => {
  for (const { kind, input, output } of TOOL_TYPES) {
    it(`declares ${input} for the ${kind} tool`, () => {
      expect(toolsDeclarationBody(input)).not.toBeNull();
    });

    it(`wires ${input} into the ToolInputSchemas union`, () => {
      expect(TOOL_INPUT_UNION).toMatch(new RegExp(`\\|\\s*${input}\\b`));
    });

    if (output !== null) {
      it(`declares ${output} for the ${kind} tool`, () => {
        expect(toolsDeclarationBody(output)).not.toBeNull();
      });

      it(`wires ${output} into the ToolOutputSchemas union`, () => {
        expect(TOOL_OUTPUT_UNION).toMatch(new RegExp(`\\|\\s*${output}\\b`));
      });
    }
  }

  it("declares the two schema unions the fold reads tool shapes out of", () => {
    expect(TOOL_INPUT_UNION).not.toBe("");
    expect(TOOL_OUTPUT_UNION).not.toBe("");
  });
});

/**
 * MODELED KINDS WITH NO DECLARED TYPE AT 0.3.220.
 *
 * The shim models all four, but `sdk-tools.d.ts` declares nothing for them —
 * their shapes are known only from `testdata/corpus` fixtures and, once the
 * lead dispatches it, the real capture run. Asserting the ABSENCE is the point:
 * an SDK that starts declaring one flips this test red, and whoever sees the
 * failure re-derives our converter from the vendor's own declaration instead of
 * leaving it on inferred shapes forever.
 *
 * A red test here is GOOD NEWS. It is not a regression to be silenced — it is
 * the vendor finally publishing a shape we had to reverse-engineer.
 */
const UNDECLARED_AT_PIN = [
  {
    kind: "Skill (skill_use)",
    names: ["SkillInput", "SkillOutput"],
    known_from: "testdata/corpus/tool-results/skill.jsonl",
  },
  {
    kind: "SendMessage",
    names: ["SendMessageInput", "SendMessageOutput"],
    known_from: "testdata/corpus/tool-results/send_message.jsonl",
  },
  {
    kind: "ToolSearch (an exempt kind, dropped by the fold)",
    names: ["ToolSearchInput", "ToolSearchOutput"],
    known_from: "testdata/corpus/tool-results/tool_search.jsonl",
  },
  {
    kind: "TaskOutput (removed from the CLI at 0.3.280; an exempt kind, still read from older transcripts)",
    names: ["TaskOutputInput", "TaskOutputOutput"],
    known_from: "testdata/corpus/tool-results/task_output.jsonl",
  },
] as const;

describe("modeled kinds the SDK declares NO type for at the pinned version", () => {
  for (const { kind, names, known_from } of UNDECLARED_AT_PIN) {
    for (const name of names) {
      it(`still declares no ${name} — ${kind}; our shape comes from ${known_from}`, () => {
        expect(toolsDeclarationBody(name)).toBeNull();
      });
    }
  }
});
