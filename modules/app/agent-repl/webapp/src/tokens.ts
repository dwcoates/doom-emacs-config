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

/**
 * The dropped breakdown: the top-level agent's dimensions, the recursive
 * whole-tree totals, then one section per model (most expensive first).
 * Sections whose source has not reported yet dash rather than lie with
 * zeros; the per-model sections simply wait (nothing to itemize).
 */
export function tokensOverlayHtml(data: TokenMenuData): string {
  const sections: string[] = [];
  sections.push(
    section(
      "top-level agent",
      data.topLevel === null ? unknownRows() : usageRows(dimsOfUsage(data.topLevel)),
    ),
  );
  const modelMap = data.models ?? {};
  const models = Object.entries(modelMap);
  if (models.length === 0) {
    sections.push(section("all agents", unknownRows()));
  } else {
    const totals = totalDims(modelMap);
    const totalCost = models.reduce((sum, [, u]) => sum + u.cost_usd, 0);
    const totalSearches = models.reduce((sum, [, u]) => sum + u.web_search_requests, 0);
    sections.push(
      section("all agents", [
        ...usageRows(totals),
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
