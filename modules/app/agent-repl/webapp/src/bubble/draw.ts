/**
 * draw — THE ONE BUBBLE. Every blue (prompt) and purple (response) bubble the
 * webapp draws is built here, from a spec its kind composes, and nowhere else.
 *
 * WHAT A KIND DECIDES, AND NOTHING ELSE (owner rulings, 2026-09-23):
 *
 *   - its ROLE, which is its side and its background: a prompt hangs on the
 *     right rail in blue, a response on the left rail in purple;
 *   - its VARIANT and STATE, which select its BORDER and nothing else (the
 *     thinking/pear/green/blue ladder, the compaction divider's own color, the
 *     held prompt's parked frame) — the one exception is the held prompt's
 *     grey-blue background, which the rulings name;
 *   - its HEADER STRIP: the elements above the scroll box (a prompt's address
 *     and delivery line, a peer's label, a notice heading, a held prompt's
 *     badges), plus the response's floated usage CORNER inside the box;
 *   - its CONTENT, drawn through the one body pipeline (src/bubble/body.ts);
 *   - its COLLAPSED LINE LIMIT, the lines shown before the has-more fade;
 *   - the WORKING wave, which only a prompt can carry: the spec types make a
 *     waving response unrepresentable rather than merely unlikely.
 *
 * Everything else — the element, its classes, the strip's placement, the scroll
 * box, the body element, the has-more measurer, the click-to-expand toggle
 * (expand.ts, keyed on the scroll box this builds) — is this module's and is the
 * same for every kind. `test/bubble/consolidation.test.ts` fails any source
 * that builds a bubble, a scroll box or a body of its own.
 */
import { armPromptWave } from "../breathing.js";
import { bubbleScroll } from "../feed/bubble-scroll.js";
import { createBubbleBody, type BubbleBody } from "./body.js";

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
 * The collapsed line limit: the shared feed cap, two lines (a thinking bubble,
 * a held prompt), or none past the header strip (a peer message). A closed set,
 * because the stylesheet maps each value to its line count
 * (`.bubble[data-cap-lines=…]`) and styles.test.ts holds the two together.
 */
export type BubbleCapLines = "feed" | 2 | 0;

/** Every cap value the stylesheet must carry a rule for. */
export const BUBBLE_CAP_LINES: readonly BubbleCapLines[] = ["feed", 2, 0];

/** The attributes the stylesheet keys a bubble's look on. */
export const BUBBLE_ROLE_ATTRIBUTE = "data-role";
export const BUBBLE_VARIANT_ATTRIBUTE = "data-variant";
export const BUBBLE_CAP_ATTRIBUTE = "data-cap-lines";

/** The class every bubble wears, and the class every header strip element wears. */
export const BUBBLE_CLASS = "bubble";
export const BUBBLE_STRIP_CLASS = "bubble-strip";

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
  content: readonly Node[];
  /** Chrome after the scroll box (a cut-short marker, a held prompt's actions). */
  footer?: readonly HTMLElement[];
  /** The collapsed line limit. */
  capLines: BubbleCapLines;
}

/** A prompt bubble: right rail, blue, and the only role that can wave. */
export interface PromptBubbleSpec extends BubbleSpecBase {
  role: "prompt";
  variant: PromptVariant;
  /** The daemon's in-flight fact, drawn as the working wave. */
  working: boolean;
}

/** A response bubble: left rail, purple, never waving. */
export interface ResponseBubbleSpec extends BubbleSpecBase {
  role: "response";
  variant: ResponseVariant;
}

export type BubbleSpec = PromptBubbleSpec | ResponseBubbleSpec;

/** A drawn bubble: the element, and the body its content was painted into. */
export interface DrawnBubble {
  bubble: HTMLElement;
  body: BubbleBody;
}

/** Draw one bubble from its spec. */
export function drawBubble(spec: BubbleSpec): DrawnBubble {
  const bubble = document.createElement("div");
  bubble.className = [BUBBLE_CLASS, ...(spec.hooks ?? [])].join(" ");
  bubble.setAttribute(BUBBLE_ROLE_ATTRIBUTE, spec.role);
  bubble.setAttribute(BUBBLE_VARIANT_ATTRIBUTE, spec.variant);
  bubble.setAttribute(BUBBLE_CAP_ATTRIBUTE, String(spec.capLines));
  if (spec.state !== undefined) bubble.setAttribute("data-state", spec.state);
  if (spec.role === "prompt") armPromptWave(bubble, spec.working);

  for (const el of spec.strip ?? []) {
    el.classList.add(BUBBLE_STRIP_CLASS);
    bubble.append(el);
  }

  const body = createBubbleBody();
  body.append(...spec.content);
  const scroll = bubbleScroll(body);
  if (spec.corner !== undefined) scroll.insertBefore(spec.corner, body);
  bubble.append(scroll);

  for (const el of spec.footer ?? []) bubble.append(el);
  return { bubble, body };
}
