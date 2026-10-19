/**
 * tool-head — THE ONE TOOL CARD HEAD.
 *
 * Every purple-titled card in the feed (a tool call such as Bash or Write, a
 * question, a permission, a skill, a hook) heads the same way, as an
 * invariant (owner ruling, 2026-10-13): its TITLE, its status BADGE, then its
 * CLOCK at the head's right end (`HEAD_CLOCK_CLASS`), and nothing else. Only the words differ per card; the arrangement,
 * the title's weight and color and the clock's shape are this module's, so a
 * card hands `drawToolCardHead` its parts and never lays a head out itself.
 * Anything bespoke to one card (a shell's exit code, a hook's) lives in the
 * card's content, never in its head.
 *
 * THE CLOCK STARTS WITH THE UNIT AND FREEZES WHEN IT SETTLES (owner ruling,
 * 2026-10-13): `frontend.v1.FeedToolCardClock` carries the instants, the
 * running clock ticks on the shared ticker, and a stopped one is drawn once.
 * Both are held at a reserved width (`elapsed-clock.ts`), so a tick never
 * reflows the head and never changes the feed's height.
 *
 * Subagent, detached-work, shell and merge bubbles are their own category and
 * head with their own renderers; they only share the head ROW (`drawToolHead`).
 */
import type { FeedToolCardClock } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import type { Ticker } from "../../clock.js";
import { reservedLiveElapsedClock, reservedSettledElapsedClock } from "../../elapsed-clock.js";
import { MalformedView } from "../../rpc/malformed.js";
import { msOf, requireCase, unreachableArm } from "../../rpc/strict.js";

/** The class every tool card head wears. */
export const TOOL_HEAD_CLASS = "tool-head";

/** The class every tool card name wears. */
export const TOOL_NAME_CLASS = "tool-name";

/** The class a tool card head's clock wears. */
export const TOOL_CLOCK_CLASS = "tool-clock";

/**
 * The class EVERY head's clock wears, on a tool card and on every bubble head
 * alike: the stylesheet sends it to the head's right end (owner request,
 * 2026-10-18), so a reader finds how long anything ran in the same place on
 * every head. Worn through `asHeadClock`, the one way a head marks its clock.
 */
export const HEAD_CLOCK_CLASS = "head-clock";

/** Mark CLOCK as its head's clock (`HEAD_CLOCK_CLASS`), and answer it. */
export function asHeadClock(clock: HTMLElement): HTMLElement {
  clock.classList.add(HEAD_CLOCK_CLASS);
  return clock;
}

/** The attribute every head built by `drawToolCardHead` carries. */
export const TOOL_CARD_HEAD_ATTRIBUTE = "data-tool-card-head";

/** The parts of one tool card's head, in the order they are drawn. */
export interface ToolCardHead {
  /** The purple title: the card's own words, or an element it composed (a folded headline). */
  readonly title: string | HTMLElement;
  /** The status badge, in the card's own vocabulary. */
  readonly badge: HTMLElement;
  /** The unit's clock; absent when the producer stated no start for it. */
  readonly clock: FeedToolCardClock | undefined;
  /** Where the clock sits in the view, for a malformed one's report. */
  readonly clockPath: string;
  /** The shared ticker a running clock repaints on. */
  readonly ticker: Ticker;
  /** The card's own classes, beside the shared one. */
  readonly classes?: readonly string[];
}

/**
 * A tool card's head: TITLE, BADGE, CLOCK, in that order, and nothing else.
 * The one way any tool card heads.
 */
export function drawToolCardHead(parts: ToolCardHead): HTMLElement {
  const head = drawToolHead(...(parts.classes ?? []));
  head.setAttribute(TOOL_CARD_HEAD_ATTRIBUTE, "");
  const title = typeof parts.title === "string" ? drawToolName(parts.title) : parts.title;
  title.classList.add(TOOL_NAME_CLASS);
  head.append(title, parts.badge);
  const clock = drawStatedToolCardClock(parts.clock, parts.clockPath, parts.ticker);
  if (clock !== null) head.append(asHeadClock(clock));
  return head;
}

/**
 * A tool card's clock where the producer stated one, or null where it stated
 * none: the one way a card's head or its tabline (tabs.ts) gets its clock.
 */
export function drawStatedToolCardClock(
  clock: FeedToolCardClock | undefined,
  path: string,
  ticker: Ticker,
): HTMLElement | null {
  return clock === undefined ? null : drawToolCardClock(clock, path, ticker);
}

/**
 * The head's clock: counting up from its start while the unit runs, or the
 * span it took once it stopped.
 */
export function drawToolCardClock(clock: FeedToolCardClock, path: string, ticker: Ticker): HTMLElement {
  const startedMs = msOf(clock.startedAtMs, `${path}.started_at_ms`);
  const span = requireCase(clock.span, `${path}.span`);
  switch (span.case) {
    case "running":
      return reservedLiveElapsedClock(ticker, TOOL_CLOCK_CLASS, startedMs);
    case "stopped": {
      const endedMs = msOf(span.value.endedAtMs, `${path}.stopped.ended_at_ms`);
      // The contract states a stop is never before its start; a reversed
      // span is a view this renderer cannot honestly draw.
      if (endedMs < startedMs) {
        throw new MalformedView(`${path}.stopped.ended_at_ms`, `stops at ${String(endedMs)}, before its start at ${String(startedMs)}`);
      }
      return reservedSettledElapsedClock(TOOL_CLOCK_CLASS, endedMs - startedMs);
    }
    default: {
      const other: { case: string } = span;
      return unreachableArm(`${path}.span`, other.case);
    }
  }
}

/**
 * A tool card's status badge: WORD, in the card's own vocabulary, wearing the
 * shared badge class and TONES (`ok`, `err`, `muted`, `run`, …), and naming
 * the card's ARM when given. The one way any tool card's badge is built, so
 * every card's badge is the same pill.
 */
export function drawToolCardBadge(word: string, tones: readonly string[] = [], arm?: string): HTMLElement {
  const badge = document.createElement("span");
  badge.classList.add("badge", ...tones);
  if (arm !== undefined) badge.setAttribute("data-arm", arm);
  badge.textContent = word;
  return badge;
}

/**
 * A bare head row, wearing EXTRA classes beside the shared one. Exported for
 * the bubbles' head row, which lays out its own head inside it; a tool card
 * heads through `drawToolCardHead`.
 */
export function drawToolHead(...extra: string[]): HTMLElement {
  const head = document.createElement("div");
  head.classList.add(TOOL_HEAD_CLASS, ...extra);
  return head;
}

/** The head's title, verbatim, wearing the shared name class. */
function drawToolName(text: string): HTMLElement {
  const name = document.createElement("span");
  name.classList.add(TOOL_NAME_CLASS);
  name.textContent = text;
  return name;
}
