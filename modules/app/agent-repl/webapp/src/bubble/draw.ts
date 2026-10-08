/**
 * draw — THE ONE BUBBLE. Every blue (prompt) and purple (response) bubble the
 * webapp draws is built here, from a spec its kind composes, and nowhere else.
 *
 * WHAT A KIND DECIDES, AND NOTHING ELSE (owner rulings, 2026-09-23):
 *
 *   - its ROLE, which is its side and its background: a prompt hangs on the
 *     right rail in blue, a response on the left rail in purple;
 *   - its VARIANT and STATE, which select its BORDER and nothing else (the
 *     thinking/pear/green/blue ladder, the compaction divider's own color, a
 *     user prompt's permanent purple, an agent-to-agent prompt's amber, an
 *     ended turn's red, and a held prompt's NONE) — the exceptions are the
 *     backgrounds the owner names: the held prompt's, 5% of its grey-blue
 *     tint over the feed, and a thinking or interim response's, the page's
 *     own `--bg` (2026-10-08);
 *   - its HEADER STRIP: the elements above the scroll box (a prompt's address
 *     and delivery line, a peer's label, a notice heading, a held prompt's
 *     badges), plus the response's floated usage CORNER inside the box;
 *   - its CONTENT, drawn through the one body pipeline (src/bubble/body.ts);
 *   - its COLLAPSED LINE LIMIT, the lines shown before the has-more fade, or
 *     the UNCAPPED mode (`BUBBLE_UNCAPPED`): always at full height, never a
 *     fade, a scroll or a fold;
 *   - its MORE SIGNAL, how a collapsed bubble says there is more: the shared
 *     bottom fade, or the one-line ELLIPSIS (`BUBBLE_MORE_ELLIPSIS`);
 *   - its EXPAND-ONLY chrome: what the reader sees only once the bubble is
 *     opened (a held prompt's details and actions), hidden while collapsed;
 *   - the WORKING wave, which only a prompt can carry: the spec types make a
 *     waving response unrepresentable rather than merely unlikely.
 *
 * Everything else — the element, its classes, the strip's placement, the scroll
 * box, the body element, the has-more measurer, the click-to-expand toggle
 * (expand.ts, keyed on the scroll box this builds) — is this module's and is the
 * same for every kind. `test/bubble/consolidation.test.ts` fails any source
 * that builds a bubble, a scroll box or a body of its own.
 */
import { armPromptWave, setPromptWave } from "../breathing.js";
import { placeChildren } from "../dom.js";
import { BUBBLE_EXPAND_ONLY_CLASS, BUBBLE_STRIP_CLASS } from "../expand.js";
import { BUBBLE_MORE_ATTRIBUTE, BUBBLE_MORE_ELLIPSIS, BUBBLE_MORE_FADE } from "../feed/bubble-more.js";
import { BUBBLE_BOX_CLASS, bubbleBox, isCappedBox } from "../feed/bubble-scroll.js";
import { stopTicking } from "../feed/ticking.js";
import {
  BUBBLE_BODY_CLASS,
  createBubbleBody,
  isBubbleBody,
  paintBody,
  type BubbleBody,
} from "./body.js";

/** The side and the background a bubble takes. */
export type BubbleRole = "prompt" | "response";

/** The response kinds: purple, left rail. */
export type ResponseVariant = "response" | "thinking" | "agentic" | "compaction";

/** The prompt kinds: blue (a held prompt grey-blue), right rail. */
export type PromptVariant = "user" | "agent" | "peer" | "held";

/** Every bubble variant, for the suite to hold the stylesheet's rules to. */
export const BUBBLE_VARIANTS = {
  response: "response",
  thinking: "response",
  agentic: "response",
  compaction: "response",
  user: "prompt",
  agent: "prompt",
  peer: "prompt",
  held: "prompt",
} as const satisfies Record<ResponseVariant | PromptVariant, BubbleRole>;

/**
 * A CAPPED bubble's collapsed line limit: the shared feed cap, five lines (a
 * user or subagent prompt), one line (a held prompt),
 * or none past the header strip (a peer message). Each kind states its limit itself, never through another kind's
 * constant, so changing one kind's cap never moves another's. A closed set,
 * because the stylesheet maps each value to its line count
 * (`.bubble[data-cap-lines=…]`) and styles.test.ts holds the two together.
 */
export type CappedLines = "feed" | 5 | 1 | 0;

/**
 * THE UNCAPPED MODE (owner request, 2026-09-27): a bubble shown at its FULL
 * HEIGHT, always — an interim or final response, and the ended-turn notice.
 * It is not a line count: its box is built WITHOUT `.bubble-scroll`
 * (`bubbleBox`), so no cap, clip, gutter, zoom cursor, has-more fade or fold
 * rule can reach it, and it is not a capped section, so no click, auto-collapse
 * or carried fold can open it. It carries no expand-only chrome (the spec types
 * forbid it), because there is no open state to reveal it in.
 */
export const BUBBLE_UNCAPPED = "none";

/** The collapsed line limit: a capped count, or the uncapped mode. */
export type BubbleCapLines = CappedLines | typeof BUBBLE_UNCAPPED;

/** Every CAPPED value, each of which the stylesheet must map to a line count. */
export const BUBBLE_CAP_LINES: readonly CappedLines[] = ["feed", 5, 1, 0];

/** Whether CAP limits the bubble at all (anything but `BUBBLE_UNCAPPED`). */
export function isCapped(cap: BubbleCapLines): cap is CappedLines {
  return cap !== BUBBLE_UNCAPPED;
}

/** The attributes the stylesheet keys a bubble's look on. */
export const BUBBLE_ROLE_ATTRIBUTE = "data-role";
export const BUBBLE_VARIANT_ATTRIBUTE = "data-variant";
export const BUBBLE_CAP_ATTRIBUTE = "data-cap-lines";
export { BUBBLE_MORE_ATTRIBUTE, BUBBLE_MORE_ELLIPSIS, BUBBLE_MORE_FADE };

/**
 * A capped bubble's MORE SIGNAL (owner rulings, 2026-09-27): the shared
 * bottom fade, or the ELLIPSIS — the collapsed body clamped to its line by the
 * stylesheet's ellipsis rule, which ends that line in `…` exactly when
 * anything is left after it (a wrapped over-long line, a further line, a
 * further block) and in nothing when it is the whole content, with no fade.
 * `data-more` carries it.
 */
export type BubbleMore = typeof BUBBLE_MORE_FADE | typeof BUBBLE_MORE_ELLIPSIS;

/**
 * The only line limit the ellipsis is drawn at: ONE line. A clamp counts whole
 * text lines, and the shared feed cap (27.5 lines, or the 50vh ceiling) is no
 * whole count, so the spec types make an ellipsis at any other cap
 * unrepresentable rather than drawn inexactly.
 */
export const ELLIPSIS_CAP_LINES = 1 satisfies CappedLines;

/**
 * A user or subagent prompt's collapsed line limit (owner request, 2026-09-28):
 * FIVE lines, far below the shared feed cap a response keeps, so a long prompt
 * never pushes its answer off the screen. Both prompt rows state it through
 * this one constant, so the two cannot drift apart.
 */
export const PROMPT_CAP_LINES = 5 satisfies CappedLines;

/** The class every bubble wears, and the class every header strip element wears. */
export const BUBBLE_CLASS = "bubble";
export { BUBBLE_EXPAND_ONLY_CLASS, BUBBLE_STRIP_CLASS };

/** What every bubble spec states, whatever its role. */
interface BubbleSpecBase {
  /** The `data-state` value, when the kind's message has a state arm. */
  state?: string;
  /**
   * The classes the kind's hooks and the integration suite know it by
   * (`user`, `assistant`, `peer`, `held-right`, `final-response`, …). They
   * select a BORDER at most; side, background, font and cap are the role's.
   */
  hooks?: readonly string[];
  /** The header strip: drawn above the scroll box, full width, in this order. */
  strip?: readonly HTMLElement[];
  /** The response's usage corner, floated top-right inside the scroll box. */
  corner?: HTMLElement;
  /** The content: nodes, markdown slots among them (see body.ts). */
  content: readonly ChildNode[];
  /** Chrome after the scroll box, always shown (a cut-short marker). */
  footer?: readonly HTMLElement[];
}

/** A capped bubble's limit, and the expand-only chrome only it can carry. */
interface CappedSpecBase {
  /**
   * Chrome after the scroll box shown ONLY while the bubble is expanded (a held
   * prompt's details and actions): it wears `BUBBLE_EXPAND_ONLY_CLASS`, which
   * the stylesheet hides while the scroll box is not `.expanded`, so the one
   * toggle (expand.ts) is what reveals it. Drawn before the footer.
   */
  expandOnly?: readonly HTMLElement[];
}

/** A capped bubble that signals more with the shared fade (the default). */
interface FadeSpec extends CappedSpecBase {
  /** The collapsed line limit. */
  capLines: CappedLines;
  more?: typeof BUBBLE_MORE_FADE;
}

/** A capped bubble that signals more with the one-line ellipsis. */
interface EllipsisSpec extends CappedSpecBase {
  capLines: typeof ELLIPSIS_CAP_LINES;
  more: typeof BUBBLE_MORE_ELLIPSIS;
}

/** An uncapped bubble: always at full height, so it has nothing expand-only and no more to signal. */
interface UncappedSpec {
  capLines: typeof BUBBLE_UNCAPPED;
  more?: never;
  expandOnly?: never;
}

/** A bubble's cap: its line limit, and how it signals what the limit hides. */
export type BubbleCapSpec = FadeSpec | EllipsisSpec | UncappedSpec;

/** A prompt bubble: right rail, blue, and the only role that can wave. */
export type PromptBubbleSpec = BubbleSpecBase &
  BubbleCapSpec & {
    role: "prompt";
    variant: PromptVariant;
    /** The daemon's in-flight fact, drawn as the working wave. */
    working: boolean;
  };

/** A response bubble: left rail, purple, never waving. */
export type ResponseBubbleSpec = BubbleSpecBase &
  BubbleCapSpec & {
    role: "response";
    variant: ResponseVariant;
  };

export type BubbleSpec = PromptBubbleSpec | ResponseBubbleSpec;

/** A drawn bubble: the element, the body, and the content nodes the body now holds. */
export interface DrawnBubble {
  bubble: HTMLElement;
  body: BubbleBody;
  /** The live content, position for position with the spec's (see `paintBody`). */
  content: readonly ChildNode[];
}

/** The attribute a chrome element may state what it says on, for the in-place compare. */
export const SAYS_ATTRIBUTE = "data-says";

/** The hook classes each bubble was last drawn with, so a redraw can take them back. */
const drawnHooks = new WeakMap<Element, readonly string[]>();

/**
 * Draw one bubble from its spec.
 *
 * A REDRAW IS IN PLACE (owner rule, 2026-09-23: the user owns the scroll). When
 * PREVIOUS — the element the previous draw of the same row returned — is a
 * bubble of the same role, it is UPDATED and returned instead of replaced: the
 * element, its scroll box and its body keep their identity (so a reader
 * scrolled inside the box keeps their place), its classes and attributes are
 * brought to the spec (a class the bubble did not get from a spec — the
 * controller's `entry-selected` — is left alone), a header, corner or footer
 * element that states the same `data-says` is kept (a replaced one's clock
 * stops), the
 * working wave keeps its phase, and the content is repainted in place
 * (`paintBody`). Nothing already in its place is moved (`placeChildren`).
 */
export function drawBubble(spec: BubbleSpec, previous?: HTMLElement): DrawnBubble {
  const capped = isCapped(spec.capLines);
  const reused = reusableParts(previous, spec.role, capped);
  const bubble = reused?.bubble ?? document.createElement("div");
  const body = reused?.body ?? createBubbleBody();
  const scroll = reused?.scroll ?? bubbleBox(body, capped);

  const hooks = [BUBBLE_CLASS, "md", ...(spec.hooks ?? [])];
  for (const old of drawnHooks.get(bubble) ?? []) {
    if (!hooks.includes(old)) bubble.classList.remove(old);
  }
  bubble.classList.add(...hooks);
  drawnHooks.set(bubble, hooks);
  bubble.setAttribute(BUBBLE_ROLE_ATTRIBUTE, spec.role);
  bubble.setAttribute(BUBBLE_VARIANT_ATTRIBUTE, spec.variant);
  bubble.setAttribute(BUBBLE_CAP_ATTRIBUTE, String(spec.capLines));
  // The more signal is a CAPPED box's; an uncapped one hides nothing to signal.
  if (capped) bubble.setAttribute(BUBBLE_MORE_ATTRIBUTE, spec.more ?? BUBBLE_MORE_FADE);
  else bubble.removeAttribute(BUBBLE_MORE_ATTRIBUTE);
  if (spec.state === undefined) bubble.removeAttribute("data-state");
  else bubble.setAttribute("data-state", spec.state);
  if (spec.role === "prompt") {
    // A fresh bubble takes the wave's phase; a reused one keeps its own, so a
    // push that flips nothing but the flag never jumps the band.
    if (reused === null) armPromptWave(bubble, spec.working);
    else setPromptWave(bubble, spec.working);
  }

  const strip = keepSaid(liveChrome(bubble, "strip", scroll), spec.strip ?? []);
  for (const el of strip) el.classList.add(BUBBLE_STRIP_CLASS);
  const expandOnly = spec.expandOnly ?? [];
  const after = keepSaid(liveChrome(bubble, "footer", scroll), [...expandOnly, ...(spec.footer ?? [])]);
  // Position decides the region, so a kept element that moved between the two
  // takes the class of the one it now stands in.
  after.forEach((el, i) => el.classList.toggle(BUBBLE_EXPAND_ONLY_CLASS, i < expandOnly.length));
  placeChildren(bubble, [...strip, scroll, ...after]);

  const liveCorner = reused === null ? [] : [...scroll.children].filter((el) => el !== body);
  const corner = keepSaid(liveCorner as HTMLElement[], spec.corner === undefined ? [] : [spec.corner]);
  placeChildren(scroll, [...corner, body]);

  // Painted LAST, inside the finished bubble: a slot holding a tree measures
  // against the bubble around it, so the body must already hang in one.
  const content = paintBody(body, spec.content);
  return { bubble, body, content };
}

/** The parts of a bubble a redraw keeps. */
interface BubbleParts {
  bubble: HTMLElement;
  scroll: HTMLElement;
  body: BubbleBody;
}

/**
 * The parts of PREVIOUS when it is a bubble of ROLE this module drew, whose box
 * is CAPPED exactly when this draw's is, to be updated in place; null for
 * anything else (a first draw, a row whose kind changed, a row whose cap mode
 * changed), which then gets a fresh bubble. A box never changes mode in place,
 * so an uncapped box can never inherit a capped one's open fold or fade.
 */
function reusableParts(previous: HTMLElement | undefined, role: BubbleRole, capped: boolean): BubbleParts | null {
  if (previous === undefined || !previous.classList.contains(BUBBLE_CLASS)) return null;
  if (previous.getAttribute(BUBBLE_ROLE_ATTRIBUTE) !== role) return null;
  const scroll = previous.querySelector<HTMLElement>(`:scope > .${BUBBLE_BOX_CLASS}`);
  const body = scroll?.querySelector(`:scope > .${BUBBLE_BODY_CLASS}`);
  if (scroll === null || !isBubbleBody(body) || isCappedBox(scroll) !== capped) return null;
  return { bubble: previous, scroll, body };
}

/** The bubble's current header strip (before the box) or footer (after it). */
function liveChrome(bubble: HTMLElement, side: "strip" | "footer", scroll: HTMLElement): HTMLElement[] {
  const children = [...bubble.children] as HTMLElement[];
  const at = children.indexOf(scroll);
  if (at < 0) return [];
  return side === "strip" ? children.slice(0, at) : children.slice(at + 1);
}

/**
 * The chrome to draw: each of NEXT, except that the live element in the same
 * position is KEPT when both state, in `data-says`, that they say the same
 * thing — a replaced corner would restart its clock and drop a hover reveal for
 * nothing. Keeping is OPT-IN: an element that states nothing is always the new
 * draw's own, because chrome can carry listeners bound to the push that drew it
 * (a held prompt's actions), and a stale one must never answer a click. A
 * discarded element's clock stops, whichever side it was on.
 */
function keepSaid(live: readonly HTMLElement[], next: readonly HTMLElement[]): HTMLElement[] {
  const drawn = next.map((want, i) => {
    const have = live[i];
    if (have === undefined || !saysTheSame(have, want)) return want;
    stopTicking(want);
    return have;
  });
  for (const have of live) {
    if (!drawn.includes(have)) stopTicking(have);
  }
  return drawn;
}

/** Whether A and B both state, and state the same thing (see `keepSaid`). */
function saysTheSame(a: HTMLElement, b: HTMLElement): boolean {
  const said = a.getAttribute(SAYS_ATTRIBUTE);
  return said !== null && said === b.getAttribute(SAYS_ATTRIBUTE);
}
