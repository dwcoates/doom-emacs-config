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
import { create, toJson } from "@bufbuild/protobuf";
import {
  TokenUtilizationSchema,
  type SessionTokenUtilization,
  type TokenUsageTotals,
  type TokenUtilization as GeneratedTokenUtilization,
} from "../../proto/gen/ts/agentshim/frontend/v1/durable_pb";
import {
  TokenUsageSchema,
  type TokenUsage as CanonicalTokenUsage,
} from "../../proto/gen/ts/agentshim/frontend/v1/tokens_pb";

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
 * THE CANONICAL SHAPE IS WHAT THIS FILE RENDERS, and `canonicalTokens` is the
 * webapp's ONE place where a vendor bucket record becomes it.
 *
 * The daemon owns every token judgment and resolves the canonical
 * `agentshim.frontend.v1.TokenUsage` onto the wire wherever a message is free
 * to carry it — the session and per-subagent totals here, `ProgressView.input_tokens`
 * for the live footer. Two surfaces are not free: a per-response record and a
 * result conversation item are DURABLE evidence whose shape is frozen (a
 * persisted row must keep replaying byte-identically), so they arrive as vendor
 * buckets. They pass through here, once, into the same shape everything else is
 * displayed from — rather than each renderer restating an addition.
 *
 * THE MAPPING IS THE DAEMON'S, RESTATED NOWHERE ELSE:
 * `cache_read_input_tokens` is the cache HIT, the only cheap bucket;
 * `cache_creation_input_tokens` is the miss that was WRITTEN as it was
 * processed; `input_tokens` is the miss that was never written at all. Both
 * misses were paid fresh, which is why they share a field.
 */
export function canonicalTokens(d: UsageDims): CanonicalTokenUsage {
  return create(TokenUsageSchema, {
    inputHits: { read: BigInt(d.cacheRead) },
    inputMisses: { written: BigInt(d.cacheCreation), unwritten: BigInt(d.input) },
    outputTokens: BigInt(d.output),
  });
}

/**
 * What a request fed the model NEW: everything the prompt cache did not serve.
 *
 * IT IS BOTH MISSES, and the canonical shape is why that is a field read rather
 * than an arithmetic a caller has to know to perform. The CLI marks nearly all
 * input cacheable, so a COLD prompt — a full context re-ingest, the most
 * expensive thing that can happen — surfaces almost entirely as the WRITTEN
 * miss while the unwritten one stays near zero. Reading either alone reports
 * near nothing for exactly the case a cost figure exists to catch.
 *
 * THE CACHE HIT IS EXCLUDED, and that exclusion is the other half of the point.
 * It is the same standing prefix presented to the model again on every request
 * of the turn, so counting it reports the conversation's size times the request
 * count — a 94-request turn against a 500k prefix reads as 47M "input tokens".
 * Output is excluded for the same reason it is excluded from the footer: this
 * is an INPUT figure.
 *
 * This is what the response bubble stamps and what `tokenHeatHue` colors; the
 * progress footer's live ticker is the DAEMON's answer to the same question
 * (`ProgressView.input_tokens`, from `internal/tokenusage.ExpensiveInput`), so
 * the live cell converges on the stamp the turn lands with.
 */
export function expensiveInput(tokens: CanonicalTokenUsage): number {
  const misses = tokens.inputMisses;
  if (misses === undefined) return 0;
  return generatedInt(misses.written + misses.unwritten, "tokenUsage.inputMisses");
}

/**
 * ONE SUBAGENT'S uncached input, as the daemon attributed it, or null when it
 * attributed none to that invocation.
 *
 * The figure is the same `expensiveInput` measure every other surface reads,
 * taken off the daemon-resolved `AgentTokenUtilization.tokens` — never summed
 * here from the vendor buckets, which would put a second owner of the
 * session's economics in a renderer.
 *
 * PARENT-TOOL-USE-ID is the key because that is the identity the roster row
 * already carries (the `Agent` call's tool-use id). NULL IS ABSENCE: an agent
 * the daemon has not yet attributed usage to reports no figure rather than a
 * zero, which would read as a subagent that cost nothing.
 */
export function agentUncachedInput(
  utilization: SessionTokenUtilization | null | undefined,
  parentToolUseId: string,
): number | null {
  if (utilization === undefined || utilization === null) return null;
  const attributed = utilization.subagents.find(
    (subagent) => subagent.agent?.parentToolUseId === parentToolUseId,
  );
  if (attributed === undefined || attributed.tokens === undefined) return null;
  return expensiveInput(attributed.tokens);
}

/**
 * The heat ramp's anchors: `[tokens, hue]` pairs the ramp passes through
 * exactly, interpolated linearly between and clamped outside.
 *
 * The hues are the named colors of the bands — green, yellow, orange, red —
 * but the ramp is CONTINUOUS rather than four buckets, so a turn near a
 * boundary does not change color on a token. 20k is the top of "cheap" and so
 * is still exactly green; the climb to yellow happens across the band ABOVE
 * it, which is what makes 19k and 21k read as the same news.
 *
 * Red is reached at 200k rather than at 100k because 100k is the start of the
 * red band, not its floor: a turn has to keep climbing to keep reddening, and
 * the very worst turns must stay distinguishable from the merely bad ones.
 */
const TOKEN_HEAT_STOPS: readonly (readonly [number, number])[] = [
  [0, 120],
  [20_000, 120],
  [50_000, 60],
  [100_000, 30],
  [200_000, 0],
];

/**
 * The hue for a turn's EXPENSIVE input figure (`expensiveInput` — both cache
 * misses, never one alone), for the corner stamp and the footer's token cell.
 * Coloring a single bucket would leave a cold re-ingest reading green.
 *
 * ONLY THE HUE IS COMPUTED HERE. Saturation and lightness belong to the
 * stylesheet, which has to pick different ones for the light and the dark
 * theme; emitting a whole color here would hardcode one theme's readability
 * into the markup. The caller sets this as the `--token-heat-hue` custom
 * property and CSS builds the color around it.
 */
export function tokenHeatHue(tokens: number): number {
  const first = TOKEN_HEAT_STOPS[0];
  const last = TOKEN_HEAT_STOPS[TOKEN_HEAT_STOPS.length - 1];
  if (tokens <= first[0]) return first[1];
  if (tokens >= last[0]) return last[1];
  for (let i = 1; i < TOKEN_HEAT_STOPS.length; i += 1) {
    const [loTokens, loHue] = TOKEN_HEAT_STOPS[i - 1];
    const [hiTokens, hiHue] = TOKEN_HEAT_STOPS[i];
    if (tokens > hiTokens) continue;
    const span = hiTokens - loTokens;
    return Math.round(loHue + ((tokens - loTokens) * (hiHue - loHue)) / span);
  }
  return last[1];
}

/** The class and custom property that carry the heat ramp to the stylesheet. */
export const TOKEN_HEAT_CLASS = "token-heat";

/** The inline custom-property declaration a heated figure carries. */
export function tokenHeatStyle(tokens: number): string {
  return `--token-heat-hue:${tokenHeatHue(tokens)}`;
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
 * The stat rows one CANONICAL usage expands into: the input-side total the
 * chip's convention headlines, its three DISJOINT buckets indented under it,
 * the expensive sum of the two misses, then the output side.
 *
 * THE BUCKET ROW IS NAMED "fresh input", NOT "uncached". It is the unwritten
 * miss alone, and labeling that "uncached" told the reader the cheap-versus-
 * expensive split was one row above where it is: cache WRITES are uncached too.
 * The sum gets its own unindented row so the figure the footer headlines and
 * heats is present here as a figure rather than as an addition the reader is
 * left to perform.
 */
function usageRows(tokens: CanonicalTokenUsage): string[] {
  const read = generatedInt(tokens.inputHits?.read ?? 0n, "tokenUsage.inputHits.read");
  const written = generatedInt(tokens.inputMisses?.written ?? 0n, "tokenUsage.inputMisses.written");
  const unwritten = generatedInt(tokens.inputMisses?.unwritten ?? 0n, "tokenUsage.inputMisses.unwritten");
  return [
    row("input", formatTokens(read + written + unwritten)),
    row("fresh input", formatTokens(unwritten), true),
    row("cache read", formatTokens(read), true),
    row("cache write", formatTokens(written), true),
    row("uncached input", formatTokens(expensiveInput(tokens))),
    row("output", formatTokens(generatedInt(tokens.outputTokens, "tokenUsage.outputTokens"))),
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
    ...usageRows(canonicalTokens(dimsOfResponseUsage(usage))),
    row("cache hit rate", percentage(usage.cacheRates?.cacheHitRate)),
    row("cache write rate", percentage(usage.cacheRates?.cacheWriteRate)),
    // "fresh input rate", not "uncached rate": the three rates partition the
    // prompt input, so this one is the input_tokens BUCKET's share and the
    // expensive share is this plus the cache-write rate.
    row("fresh input rate", percentage(usage.cacheRates?.uncachedInputRate)),
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

/**
 * The canonical shape of one per-model total.
 *
 * PER-MODEL IS THE ONE AGGREGATE THE DAEMON CANNOT RESOLVE ONTO THE WIRE.
 * ModelTokenUtilization is embedded in the durable TurnAccounting
 * reconciliation, so growing it a field would break the replay of every row an
 * earlier build wrote. The conversion therefore happens here, through the same
 * one translation every other vendor-shaped record in this file passes through.
 */
function canonicalTokensOfTotals(totals: TokenUsageTotals, where: string): CanonicalTokenUsage {
  return canonicalTokens({
    input: generatedInt(totals.inputTokens, `${where}.inputTokens`),
    output: generatedInt(totals.outputTokens, `${where}.outputTokens`),
    cacheRead: generatedInt(totals.cacheReadInputTokens, `${where}.cacheReadInputTokens`),
    cacheCreation: generatedInt(totals.cacheCreationInputTokens, `${where}.cacheCreationInputTokens`),
  });
}

/**
 * Every additive field carried by generated cumulative usage.
 *
 * `tokens` is the DAEMON'S OWN canonical resolution of these same totals, and
 * it is passed wherever the wire carries one. The fallback conversion is for
 * the per-model totals alone: ModelTokenUtilization is embedded in the durable
 * TurnAccounting reconciliation, so it cannot grow a field without breaking the
 * replay of every row an earlier build wrote.
 */
function generatedUsageRows(totals: TokenUsageTotals, where: string, tokens: CanonicalTokenUsage): string[] {
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
    ...usageRows(tokens),
    row("cache write 5m", totals.cacheCreation === undefined ? "unavailable" : formatTokens(generatedInt(totals.cacheCreation.ephemeral5mInputTokens, `${where}.cacheCreation.ephemeral5mInputTokens`))),
    row("cache write 1h", totals.cacheCreation === undefined ? "unavailable" : formatTokens(generatedInt(totals.cacheCreation.ephemeral1hInputTokens, `${where}.cacheCreation.ephemeral1hInputTokens`))),
    row("total prompt input", totals.cacheRates === undefined ? "unavailable" : formatTokens(generatedInt(totals.cacheRates.totalPromptInputTokens, `${where}.cacheRates.totalPromptInputTokens`))),
    row("cache hit rate", totals.cacheRates === undefined ? "unavailable" : percentage(totals.cacheRates.cacheHitRate)),
    row("cache write rate", totals.cacheRates === undefined ? "unavailable" : percentage(totals.cacheRates.cacheWriteRate)),
    row("fresh input rate", totals.cacheRates === undefined ? "unavailable" : percentage(totals.cacheRates.uncachedInputRate)),
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
    // THE DAEMON'S RESOLUTION IS REQUIRED, NOT PREFERRED. Falling back to a
    // local partition of the vendor buckets here would put a second owner of
    // the session's economics in the renderer, silently, on exactly the frames
    // where the daemon failed to resolve one.
    if (generated.mainAgentTokens === undefined || generated.allAgentsTokens === undefined) {
      throw new Error("session token utilization lacks daemon-resolved canonical tokens");
    }
    sections.push(section("main agent", generatedUsageRows(generated.mainAgent, "session.mainAgent", generated.mainAgentTokens)));
    sections.push(section("all agents", generatedUsageRows(generated.allAgents, "session.allAgents", generated.allAgentsTokens)));
    for (const [index, subagent] of generated.subagents.entries()) {
      if (subagent.agent === undefined || subagent.totals === undefined) throw new Error(`session subagent ${index} lacks identity or totals`);
      if (subagent.tokens === undefined) throw new Error(`session subagent ${index} lacks daemon-resolved canonical tokens`);
      const identity = subagent.agent;
      const title = `subagent ${identity.agentId || identity.parentToolUseId}`;
      sections.push(section(title, [
        row("agent ID", identity.agentId || "unavailable"),
        row("parent tool use ID", identity.parentToolUseId || "unavailable"),
        row("parent agent ID", identity.parentAgentId || "unavailable"),
        row("subagent type", identity.subagentType || "unavailable"),
        row("task", identity.taskDescription || "unavailable"),
        ...generatedUsageRows(subagent.totals, `session.subagents[${index}].totals`, subagent.tokens),
      ]));
      for (const [modelIndex, model] of subagent.models.entries()) {
        if (model.totals === undefined) throw new Error(`session subagent ${index} model ${modelIndex} lacks totals`);
        sections.push(section(`${title} model ${model.model}`, [
          row("canonical model", model.canonicalModel ?? "unavailable"),
          row("provider", model.provider ?? "unavailable"),
          row("cost", model.costUsd === undefined ? "unavailable" : formatCost(model.costUsd)),
          row("context window", model.contextWindow === undefined ? "unavailable" : formatTokens(generatedInt(model.contextWindow, `session.subagents[${index}].models[${modelIndex}].contextWindow`))),
          row("max output", model.maxOutputTokens === undefined ? "unavailable" : formatTokens(generatedInt(model.maxOutputTokens, `session.subagents[${index}].models[${modelIndex}].maxOutputTokens`))),
          ...generatedUsageRows(model.totals, `session.subagents[${index}].models[${modelIndex}].totals`, canonicalTokensOfTotals(model.totals, `session.subagents[${index}].models[${modelIndex}].totals`)),
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
        ...generatedUsageRows(model.totals, `session.models[${index}].totals`, canonicalTokensOfTotals(model.totals, `session.models[${index}].totals`)),
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
      data.topLevel === null ? unknownRows() : usageRows(canonicalTokens(dimsOfUsage(data.topLevel))),
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
        ...usageRows(canonicalTokens(totals)), ...timing,
        row("web searches", formatTokens(totalSearches)),
        row("cost", formatCost(totalCost)),
      ]),
    );
    models.sort(([na, a], [nb, b]) => b.cost_usd - a.cost_usd || na.localeCompare(nb));
    for (const [model, u] of models) {
      sections.push(
        section(model, [
          ...usageRows(canonicalTokens(dimsOfModelUsage(u))),
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
