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
import { ModelUsage, Usage } from "./protocol.js";
import { create } from "@bufbuild/protobuf";
import {
  TokenUsageSchema,
  type TokenUsage as CanonicalTokenUsage,
} from "../../proto/gen/ts/conversation/v1/api_pb";

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
  // RETIRED: `timing`, `ungroupedSubagentResponses` and `sessionUtilization`
  // stood here — aggregate generation/TTFT timing, the per-response ungrouped
  // subagent records, and the daemon's whole session token utilization. Their
  // wire fields (`SessionView.token_utilization`, `Message.token_utilization`)
  // are RESERVED with no successor on those messages; `TokenBreakdownView`
  // carries the resolved session and per-model rows instead.
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

// RETIRED: `generationTokensPerSecond`, `averageTimeToFirstTokenMs` and
// `timingRows` stood here. They read `TokenUsageTotals.timing`, which reached
// this end only on `SessionView.token_utilization` / `Message.token_utilization`
// — both RESERVED with no successor. The overlay's "generation" and
// "average TTFT" rows are therefore gone rather than recomputed here.

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

// RETIRED: `agentUncachedInput` stood here — ONE subagent's uncached input,
// looked up by its `Agent` call's tool-use id off
// `SessionTokenUtilization.subagents[].tokens`. Its only source was
// `SessionView.token_utilization`, now RESERVED with no successor, so the
// per-subagent figure it fed (the expanded footer's agent row and the detached
// agent's badge) is gone rather than re-derived from anything else.

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
 * ONE uncached-input figure, as EVERY surface that reports one draws it: the
 * footer's live cell, the settled turn's corner stamp, the expanded footer's
 * per-subagent row, and a detached agent's catalog badge.
 *
 * THE INVARIANCE IS THE POINT. All four report the same measure
 * (`expensiveInput` — both cache misses), in the same compact form, with the
 * same `in` suffix, carrying the same heat ramp. Before this helper each site
 * restated that markup, and the badge restated a DIFFERENT measure entirely
 * (its transcript's summed output tokens, labelled `tok`), so a subagent's
 * spend read as a wholly different number from the one the footer reported for
 * the same work. A site that renders this figure now cannot spell it its own
 * way, because there is only one spelling.
 *
 * CLASSNAME is the caller's own hook for placement and size only — the COLOR
 * comes from `TOKEN_HEAT_CLASS`, which the stylesheet lets win over every
 * per-surface color (see the `:not(.token-heat)` fallbacks in styles.css).
 */
export function uncachedInputHtml(className: string, tokens: number): string {
  return `<span class="${className} ${TOKEN_HEAT_CLASS}" style="${tokenHeatStyle(
    tokens,
  )}">${escapeHtml(`${compactTokens(tokens)} in`)}</span>`;
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

function generatedInt(value: bigint, where: string): number {
  const number = Number(value);
  if (!Number.isSafeInteger(number)) throw new Error(`${where} exceeds the webapp's safe integer range`);
  return number;
}

/**
 * The dropped breakdown: the top-level agent's dimensions, the recursive
 * whole-tree totals, then one section per model (most expensive first).
 * Sections whose source has not reported yet dash rather than lie with
 * zeros; the per-model sections simply wait (nothing to itemize).
 */
export function tokensOverlayHtml(data: TokenMenuData): string {
  const sections: string[] = [];
  // RETIRED: the daemon-resolved `sessionUtilization` branch stood here. It
  // rendered the "main agent" / "all agents" / per-subagent / per-model /
  // ungrouped-subagent-response sections off `SessionView.token_utilization`,
  // which is RESERVED with no successor. `TokenBreakdownView` (token-breakdown-view.ts)
  // renders those rows resolved by the daemon instead.
  sections.push(
    section(
      "top-level agent",
      data.topLevel === null ? unknownRows() : usageRows(canonicalTokens(dimsOfUsage(data.topLevel))),
    ),
  );
  const modelMap = data.models ?? {};
  // RETIRED: the "generation" and "average TTFT" timing rows stood in each of
  // the two branches below, off `TokenUsageTotals.timing`. That field reached
  // this end only on the reserved `token_utilization` fields, so the rows are
  // gone with no successor rather than recomputed here.
  const models = Object.entries(modelMap);
  if (models.length === 0) {
    sections.push(section("all agents", unknownRows()));
  } else {
    const totals = totalDims(modelMap);
    const totalCost = models.reduce((sum, [, u]) => sum + u.cost_usd, 0);
    const totalSearches = models.reduce((sum, [, u]) => sum + u.web_search_requests, 0);
    sections.push(
      section("all agents", [
        ...usageRows(canonicalTokens(totals)),
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
