/**
 * Tokens dropdown — the topbar token figure and the breakdown overlay
 * behind it. A sibling of the counter menus (`agents.ts`, `tasks.ts`):
 * the same chip-plus-overlay shape, the same renderer-owned disclosure
 * state, but stat rows rather than a roster, so it renders directly
 * instead of specializing the counter-menu facade.
 *
 * The chip's figure is the session's CURRENT context occupancy — the
 * standing `contextTokens`: uncached input plus cache read plus cache
 * write plus the output of the last top-level request, with every
 * subagent's spend excluded (§2.4: a result's `usage` never contains
 * sidechain spend). Including output is what keeps the figure a LIVE
 * occupancy the model max can be read against, not a turn-lagged one.
 * The overlay is where the cumulative resolution lives: the top-level
 * dimensions split apart, the whole-tree totals summed from the per-model
 * map (the only figure that counts subagents), and each model's own slice.
 */
import { dropdownChipHtml } from "./counter-menu.js";
import { escapeHtml } from "./highlight.js";
import { ModelUsage, TokenTimingTotals, Usage } from "./protocol.js";
import type { ResponseTokenUsage, TokenUtilization } from "./frontend-proto.js";
import { toJson } from "@bufbuild/protobuf";
import {
  TokenUtilizationSchema,
  type SessionTokenUtilization,
  type TokenUsageTotals,
  type TokenUtilization as GeneratedTokenUtilization,
} from "../../proto/gen/ts/agentshim/frontend/v1/frontend_pb";

/** Everything the dropdown knows how to break down. */
export interface TokenMenuData {
  /**
   * The session's CURRENT context size — the standing `s.contextTokens`,
   * the same figure the response bubble shows. This is the chip's headline
   * figure; null before any request is known (or after a `/clear` or
   * compaction leaves it unknown), which prints a dash.
   */
  contextSize: number | null;
  /** The top-level agent's cumulative usage; null before any is known. */
  topLevel: Usage | null;
  /**
   * Per-model usage INCLUDING subagents (§2.4 `model_usage`); null until
   * a result carries one — the whole-tree rows dash until then.
   */
  models: Record<string, ModelUsage> | null;
  /** Authoritative aggregate timing from the daemon, when it has observations. */
  timing?: TokenTimingTotals;
  /** Exact subagent responses that lack a stable invocation aggregate. */
  ungroupedSubagentResponses?: readonly TokenUtilization[];
  /** Generated cumulative accounting, including grouped subagent ownership. */
  sessionUtilization?: SessionTokenUtilization;
}

/** Token counts as the topbar and the result chip both write them: `300,000`. */
export function formatTokens(n: number): string {
  return n.toLocaleString("en-US");
}

/**
 * Token counts as a pill wears them: `812`, `12.3k`, `1.2M`. A badge has
 * no room for locale commas, and at pill scale the magnitude is the
 * information — the trailing digits are noise.
 */
export function compactTokens(n: number): string {
  if (n < 1000) return String(n);
  if (n < 1_000_000) return `${(n / 1000).toFixed(n < 10_000 ? 1 : 0)}k`;
  return `${(n / 1_000_000).toFixed(1)}M`;
}

/** Model-generation throughput from timed output only, or unavailable. */
export function generationTokensPerSecond(timing: TokenTimingTotals | undefined): number | null {
  if (timing === undefined || timing.output_generation_duration_ms <= 0) return null;
  return (1000 * timing.output_tokens_with_generation_duration) / timing.output_generation_duration_ms;
}

/** Mean first-token latency from responses that supplied that measurement. */
export function averageTimeToFirstTokenMs(timing: TokenTimingTotals | undefined): number | null {
  if (timing === undefined || timing.responses_with_time_to_first_token <= 0) return null;
  return timing.total_time_to_first_token_ms / timing.responses_with_time_to_first_token;
}

/** Human-readable timing rows that never invent a rate for untimed responses. */
export function timingRows(timing: TokenTimingTotals | undefined): Array<[string, string]> {
  const tps = generationTokensPerSecond(timing);
  const ttft = averageTimeToFirstTokenMs(timing);
  return [
    ["generation", tps === null ? "unavailable" : `${tps.toFixed(1)} tok/s`],
    ["average TTFT", ttft === null ? "unavailable" : `${ttft.toFixed(0)} ms`],
  ];
}

/**
 * The NEW input tokens a turn fed the model: uncached input plus the prefix
 * it wrote to cache. This is the figure the progress footer tickers up during
 * a turn and the final-response bubble stamps when it lands, computed here
 * from a result's own turn-scoped `usage` so the two agree.
 *
 * THE CACHE READ IS NOT NEW INPUT and is deliberately excluded. It is the same
 * standing prefix presented to the model again on every request of the turn,
 * so counting it reports the conversation's size times the request count — a
 * 94-request turn against a 500k prefix reads as 47M "input tokens", which is
 * the inflation this exclusion removes. Output is excluded for the same reason
 * it is excluded from the footer: this is an INPUT figure.
 */
export function turnInputTokens(u: Usage): number {
  return u.input_tokens + (u.cache_creation_input_tokens ?? 0);
}

/**
 * A cost estimate row's text. Two decimals once the figure is readable
 * at that resolution, four below a dime so small spends do not all
 * collapse into `$0.00`.
 */
function formatCost(usd: number): string {
  return `$${usd.toFixed(usd < 0.1 ? 4 : 2)}`;
}

/** The four token dimensions every usage payload carries, defaulted. */
interface UsageDims {
  input: number;
  cacheCreation: number;
  cacheRead: number;
  output: number;
}

function dimsOfUsage(u: Usage): UsageDims {
  return {
    input: u.input_tokens,
    cacheCreation: u.cache_creation_input_tokens ?? 0,
    cacheRead: u.cache_read_input_tokens ?? 0,
    output: u.output_tokens,
  };
}

function dimsOfModelUsage(u: ModelUsage): UsageDims {
  return {
    input: u.input_tokens,
    cacheCreation: u.cache_creation_input_tokens,
    cacheRead: u.cache_read_input_tokens,
    output: u.output_tokens,
  };
}

function dimsOfResponseUsage(u: ResponseTokenUsage): UsageDims {
  return {
    input: u.inputTokens,
    cacheCreation: u.cacheCreationInputTokens,
    cacheRead: u.cacheReadInputTokens,
    output: u.outputTokens,
  };
}

/**
 * The whole-tree totals: the per-model map summed. Context windows are
 * deliberately not summed — a capacity is per-model, not additive — so
 * that dimension stays on the per-model rows only.
 */
function totalDims(models: Record<string, ModelUsage>): UsageDims {
  const total: UsageDims = { input: 0, cacheCreation: 0, cacheRead: 0, output: 0 };
  for (const u of Object.values(models)) {
    total.input += u.input_tokens;
    total.cacheCreation += u.cache_creation_input_tokens;
    total.cacheRead += u.cache_read_input_tokens;
    total.output += u.output_tokens;
  }
  return total;
}

function row(label: string, value: string, sub = false): string {
  return `<li class="tokens-row${sub ? " sub" : ""}"><span class="tokens-label">${escapeHtml(
    label,
  )}</span><span class="tokens-value">${escapeHtml(value)}</span></li>`;
}

function section(title: string, rows: string[]): string {
  return `<li class="tokens-section">${escapeHtml(title)}</li>${rows.join("")}`;
}

/**
 * The stat rows one usage payload expands into: the input-side total the
 * chip's convention headlines, its three constituents indented under it,
 * then the output side.
 */
function usageRows(d: UsageDims): string[] {
  return [
    row("input", formatTokens(d.input + d.cacheRead + d.cacheCreation)),
    row("uncached", formatTokens(d.input), true),
    row("cache read", formatTokens(d.cacheRead), true),
    row("cache write", formatTokens(d.cacheCreation), true),
    row("output", formatTokens(d.output)),
  ];
}

/** The dashes a section shows before its data source has reported. */
function unknownRows(): string[] {
  return [row("input", "—"), row("output", "—")];
}

function percentage(value: number | undefined): string {
  return value === undefined ? "unavailable" : `${(100 * value).toFixed(1)}%`;
}

/** One ungrouped response's identity, lineage, and unaggregated token dimensions. */
function ungroupedResponseRows(response: TokenUtilization): string[] {
  if (response.actor !== "subagent" || response.subagent === undefined) {
    throw new Error(`ungrouped response ${response.apiMessageId} lacks subagent lineage`);
  }
  const usage = response.usage;
  return [
    row("API message ID", response.apiMessageId),
    row("API request ID", response.apiRequestId ?? "unavailable"),
    row("root turn", response.rootTurnId),
    row("model", response.model),
    row("agent ID", response.subagent.agentId || "unavailable"),
    row("parent tool use ID", response.subagent.parentToolUseId || "unavailable"),
    row("parent agent", response.subagent.parentAgentId),
    row("subagent type", response.subagent.subagentType),
    row("task", response.subagent.taskDescription),
    ...usageRows(dimsOfResponseUsage(usage)),
    row("cache hit rate", percentage(usage.cacheRates?.cacheHitRate)),
    row("cache write rate", percentage(usage.cacheRates?.cacheWriteRate)),
    row("uncached rate", percentage(usage.cacheRates?.uncachedInputRate)),
    row("service tier", usage.serviceTier || "unavailable"),
    row("speed", usage.speed || "unavailable"),
    row("inference geo", usage.inferenceGeo || "unavailable"),
  ];
}

function generatedInt(value: bigint, where: string): number {
  const number = Number(value);
  if (!Number.isSafeInteger(number)) throw new Error(`${where} exceeds the webapp's safe integer range`);
  return number;
}

/** Every additive field carried by generated cumulative usage. */
function generatedUsageRows(totals: TokenUsageTotals, where: string): string[] {
  const input = generatedInt(totals.inputTokens, `${where}.inputTokens`);
  const output = generatedInt(totals.outputTokens, `${where}.outputTokens`);
  const cacheRead = generatedInt(totals.cacheReadInputTokens, `${where}.cacheReadInputTokens`);
  const cacheWrite = generatedInt(totals.cacheCreationInputTokens, `${where}.cacheCreationInputTokens`);
  const timing = totals.timing;
  const tps = timing !== undefined && timing.outputGenerationDurationMs > 0n
    ? 1000 * generatedInt(timing.outputTokensWithGenerationDuration, `${where}.timing.outputTokensWithGenerationDuration`) /
      generatedInt(timing.outputGenerationDurationMs, `${where}.timing.outputGenerationDurationMs`)
    : null;
  const ttft = timing !== undefined && timing.responsesWithTimeToFirstToken > 0n
    ? generatedInt(timing.totalTimeToFirstTokenMs, `${where}.timing.totalTimeToFirstTokenMs`) /
      generatedInt(timing.responsesWithTimeToFirstToken, `${where}.timing.responsesWithTimeToFirstToken`)
    : null;
  return [
    ...usageRows({ input, output, cacheRead, cacheCreation: cacheWrite }),
    row("cache write 5m", totals.cacheCreation === undefined ? "unavailable" : formatTokens(generatedInt(totals.cacheCreation.ephemeral5mInputTokens, `${where}.cacheCreation.ephemeral5mInputTokens`))),
    row("cache write 1h", totals.cacheCreation === undefined ? "unavailable" : formatTokens(generatedInt(totals.cacheCreation.ephemeral1hInputTokens, `${where}.cacheCreation.ephemeral1hInputTokens`))),
    row("total prompt input", totals.cacheRates === undefined ? "unavailable" : formatTokens(generatedInt(totals.cacheRates.totalPromptInputTokens, `${where}.cacheRates.totalPromptInputTokens`))),
    row("cache hit rate", totals.cacheRates === undefined ? "unavailable" : percentage(totals.cacheRates.cacheHitRate)),
    row("cache write rate", totals.cacheRates === undefined ? "unavailable" : percentage(totals.cacheRates.cacheWriteRate)),
    row("uncached rate", totals.cacheRates === undefined ? "unavailable" : percentage(totals.cacheRates.uncachedInputRate)),
    row("web searches", totals.serverToolUse === undefined ? "unavailable" : formatTokens(generatedInt(totals.serverToolUse.webSearchRequests, `${where}.serverToolUse.webSearchRequests`))),
    row("web fetches", totals.serverToolUse === undefined ? "unavailable" : formatTokens(generatedInt(totals.serverToolUse.webFetchRequests, `${where}.serverToolUse.webFetchRequests`))),
    row("thinking tokens", totals.outputDetails === undefined ? "unavailable" : formatTokens(generatedInt(totals.outputDetails.thinkingTokens, `${where}.outputDetails.thinkingTokens`))),
    row("generation", tps === null ? "unavailable" : `${tps.toFixed(1)} tok/s`),
    row("average TTFT", ttft === null ? "unavailable" : `${ttft.toFixed(0)} ms`),
    row("timed output tokens", timing === undefined ? "unavailable" : formatTokens(generatedInt(timing.outputTokensWithGenerationDuration, `${where}.timing.outputTokensWithGenerationDuration`))),
    row("output generation duration", timing === undefined ? "unavailable" : `${formatTokens(generatedInt(timing.outputGenerationDurationMs, `${where}.timing.outputGenerationDurationMs`))} ms`),
    row("responses with generation duration", timing === undefined ? "unavailable" : formatTokens(generatedInt(timing.responsesWithGenerationDuration, `${where}.timing.responsesWithGenerationDuration`))),
    row("responses without generation duration", timing === undefined ? "unavailable" : formatTokens(generatedInt(timing.responsesWithoutGenerationDuration, `${where}.timing.responsesWithoutGenerationDuration`))),
    row("total TTFT", timing === undefined ? "unavailable" : `${formatTokens(generatedInt(timing.totalTimeToFirstTokenMs, `${where}.timing.totalTimeToFirstTokenMs`))} ms`),
    row("responses with TTFT", timing === undefined ? "unavailable" : formatTokens(generatedInt(timing.responsesWithTimeToFirstToken, `${where}.timing.responsesWithTimeToFirstToken`))),
    row("responses without TTFT", timing === undefined ? "unavailable" : formatTokens(generatedInt(timing.responsesWithoutTimeToFirstToken, `${where}.timing.responsesWithoutTimeToFirstToken`))),
  ];
}

/** Generated ungrouped response rendered without losing any wire field. */
function generatedUngroupedRows(response: GeneratedTokenUtilization): string[] {
  const raw = toJson(TokenUtilizationSchema, response);
  return [
    row("API message ID", response.apiMessageId),
    row("API request ID", response.apiRequestId ?? "unavailable"),
    row("root turn", response.rootTurnId),
    row("model", response.model),
    row("complete response JSON", JSON.stringify(raw)),
  ];
}

/**
 * The dropped breakdown: the top-level agent's dimensions, the recursive
 * whole-tree totals, then one section per model (most expensive first).
 * Sections whose source has not reported yet dash rather than lie with
 * zeros; the per-model sections simply wait (nothing to itemize).
 */
export function tokensOverlayHtml(data: TokenMenuData): string {
  const sections: string[] = [];
  const generated = data.sessionUtilization;
  if (generated !== undefined) {
    if (generated.mainAgent === undefined || generated.allAgents === undefined) {
      throw new Error("session token utilization lacks mainAgent or allAgents totals");
    }
    sections.push(section("main agent", generatedUsageRows(generated.mainAgent, "session.mainAgent")));
    sections.push(section("all agents", generatedUsageRows(generated.allAgents, "session.allAgents")));
    for (const [index, subagent] of generated.subagents.entries()) {
      if (subagent.agent === undefined || subagent.totals === undefined) throw new Error(`session subagent ${index} lacks identity or totals`);
      const identity = subagent.agent;
      const title = `subagent ${identity.agentId || identity.parentToolUseId}`;
      sections.push(section(title, [
        row("agent ID", identity.agentId || "unavailable"),
        row("parent tool use ID", identity.parentToolUseId || "unavailable"),
        row("parent agent ID", identity.parentAgentId || "unavailable"),
        row("subagent type", identity.subagentType || "unavailable"),
        row("task", identity.taskDescription || "unavailable"),
        ...generatedUsageRows(subagent.totals, `session.subagents[${index}].totals`),
      ]));
      for (const [modelIndex, model] of subagent.models.entries()) {
        if (model.totals === undefined) throw new Error(`session subagent ${index} model ${modelIndex} lacks totals`);
        sections.push(section(`${title} model ${model.model}`, [
          row("canonical model", model.canonicalModel ?? "unavailable"),
          row("provider", model.provider ?? "unavailable"),
          row("cost", model.costUsd === undefined ? "unavailable" : formatCost(model.costUsd)),
          row("context window", model.contextWindow === undefined ? "unavailable" : formatTokens(generatedInt(model.contextWindow, `session.subagents[${index}].models[${modelIndex}].contextWindow`))),
          row("max output", model.maxOutputTokens === undefined ? "unavailable" : formatTokens(generatedInt(model.maxOutputTokens, `session.subagents[${index}].models[${modelIndex}].maxOutputTokens`))),
          ...generatedUsageRows(model.totals, `session.subagents[${index}].models[${modelIndex}].totals`),
        ]));
      }
    }
    for (const [index, model] of generated.models.entries()) {
      if (model.totals === undefined) throw new Error(`session model ${index} lacks totals`);
      sections.push(section(`all agents model ${model.model}`, [
        row("canonical model", model.canonicalModel ?? "unavailable"),
        row("provider", model.provider ?? "unavailable"),
        row("cost", model.costUsd === undefined ? "unavailable" : formatCost(model.costUsd)),
        row("context window", model.contextWindow === undefined ? "unavailable" : formatTokens(generatedInt(model.contextWindow, `session.models[${index}].contextWindow`))),
        row("max output", model.maxOutputTokens === undefined ? "unavailable" : formatTokens(generatedInt(model.maxOutputTokens, `session.models[${index}].maxOutputTokens`))),
        ...generatedUsageRows(model.totals, `session.models[${index}].totals`),
      ]));
    }
    for (const [index, response] of generated.ungroupedSubagentResponses.entries()) {
      sections.push(section(`ungrouped subagent response ${response.apiMessageId}`, generatedUngroupedRows(response)));
    }
    return `<ul class="tokens-overlay" role="menu">${sections.join("")}</ul>`;
  }
  sections.push(
    section(
      "top-level agent",
      data.topLevel === null ? unknownRows() : usageRows(dimsOfUsage(data.topLevel)),
    ),
  );
  const modelMap = data.models ?? {};
  const timing = timingRows(data.timing).map(([label, value]) => row(label, value));
  const models = Object.entries(modelMap);
  if (models.length === 0) {
    sections.push(section("all agents", [...unknownRows(), ...timing]));
  } else {
    const totals = totalDims(modelMap);
    const totalCost = models.reduce((sum, [, u]) => sum + u.cost_usd, 0);
    const totalSearches = models.reduce((sum, [, u]) => sum + u.web_search_requests, 0);
    sections.push(
      section("all agents", [
        ...usageRows(totals), ...timing,
        row("web searches", formatTokens(totalSearches)),
        row("cost", formatCost(totalCost)),
      ]),
    );
    models.sort(([na, a], [nb, b]) => b.cost_usd - a.cost_usd || na.localeCompare(nb));
    for (const [model, u] of models) {
      sections.push(
        section(model, [
          ...usageRows(dimsOfModelUsage(u)),
          row("web searches", formatTokens(u.web_search_requests)),
          row("cost", formatCost(u.cost_usd)),
          row("context window", formatTokens(u.context_window)),
        ]),
      );
    }
  }
  for (const response of data.ungroupedSubagentResponses ?? []) {
    sections.push(
      section(`ungrouped subagent response ${response.apiMessageId}`, ungroupedResponseRows(response)),
    );
  }
  return `<ul class="tokens-overlay" role="menu">${sections.join("")}</ul>`;
}

/**
 * The chip and (when open) its overlay. Unlike the counters — which hide
 * until the session has something to count — the chip always renders:
 * the token figure is a session-constant datapoint, and before any usage
 * is known it reads a dash rather than a lying zero.
 *
 * The chip's figure is the session's CURRENT context size (the standing
 * `s.contextTokens`, the same value the response bubble shows), NOT the
 * cumulative input-side spend. The cumulative spend still lives in the
 * overlay's "top-level agent" section for anyone who opens the breakdown.
 */
export function tokensMenuHtml(data: TokenMenuData, open: boolean): string {
  const figure = data.contextSize === null ? "—" : formatTokens(data.contextSize);
  return dropdownChipHtml(
    "tokens",
    `tokens: ${figure}`,
    "current context size (uncached + cache read + cache write + output of the last request) — click for the cumulative breakdown",
    open,
    () => tokensOverlayHtml(data),
  );
}
