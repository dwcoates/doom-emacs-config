/**
 * controls — the four things every card in this wave draws the same way: the
 * call-site refusal, a folded section, a cluster of buttons that goes inert
 * while its rpc is in flight, and the purple agentic bubble.
 *
 * WHY THESE LIVE TOGETHER RATHER THAN IN EACH CARD. All three are contract
 * behavior, not decoration. A refusal must appear AT the control that made the
 * call (never as pushed state); a fold must start where the wire said on the
 * FIRST draw and where the READER left it on every draw after (R2); and a
 * cluster of ask buttons must not accept a second click while the first is
 * still unanswered, because the daemon's answer to the second would be
 * "already answered" — a refusal the user caused by our own missing latch.
 * Written once per idea, so no card can implement one of them differently from
 * its neighbor and no reviewer has to check nine copies for the same defect.
 *
 * NONE OF THEM KEEP STATE OUTSIDE THE DOM. The fold's state is an attribute the
 * next draw reads off `rc.previous`; the in-flight latch lives for exactly the
 * duration of one awaited call. A module-level map keyed by row id would
 * outlive the row it described.
 */
import { log } from "../../log.js";
import { drawBubble } from "../../bubble/draw.js";
import type { RowContext } from "../renderers.js";

/** The attribute a fold's toggle carries its state on. */
export const FOLD_STATE_ATTRIBUTE = "data-folded";

// THE REFUSAL PRIMITIVES ARE THE RPC LAYER'S, not the feed's: `refusal`,
// `clearRefusals` and `drawMalformedRefusal` are the SAME elements the topbar,
// the login overlay and the sidebar draw, so they live in `src/rpc/refuse.ts`
// with the rest of the one refusal hook. They are re-exported here because the
// cards have always reached them through `controls`.
export { refusal, clearRefusals, drawMalformedRefusal } from "../../rpc/refuse.js";

/**
 * A folded section: a caret toggle over a body that hides rather than unmounts.
 *
 * HIDDEN, NOT REMOVED, because the body may hold ticking elements and rendered
 * markdown whose cost was already paid; and because `hidden` is what the
 * integration suite can read a fold's state from without opening it.
 *
 * THE INITIAL FOLD IS THE WIRE'S ONLY ONCE (R2). On a redraw the reader's own
 * toggle is read back off the previous element and re-applied, so a push can
 * never un-toggle a section the reader opened.
 */
export function foldSection(spec: {
  /** The fold's name within its card — the `data-fold` value. */
  name: string;
  /** The toggle's word, drawn after the caret. */
  label: string;
  /** What the fold reveals. */
  body: HTMLElement;
  /** The fold this section takes on its FIRST draw. */
  folded: boolean;
  rc: RowContext;
}): HTMLElement {
  const wrap = document.createElement("div");
  wrap.className = "card-fold";

  const toggle = document.createElement("button");
  toggle.type = "button";
  toggle.className = "card-fold-toggle";
  toggle.setAttribute("data-fold", spec.name);

  const apply = (folded: boolean): void => {
    toggle.setAttribute(FOLD_STATE_ATTRIBUTE, folded ? "true" : "false");
    toggle.textContent = `${folded ? "▸" : "▾"} ${spec.label}`;
    spec.body.hidden = folded;
  };
  apply(initialFold(spec.name, spec.folded, spec.rc));

  toggle.addEventListener("click", () => {
    const next = toggle.getAttribute(FOLD_STATE_ATTRIBUTE) !== "true";
    log.debug(`the reader ${next ? "folded" : "unfolded"} a card section`, {
      operation: "feed.cards.fold-toggled",
      context: { fold: spec.name, folded: next },
    });
    apply(next);
  });

  wrap.append(toggle, spec.body);
  return wrap;
}

/**
 * The fold a fresh draw starts in: the reader's own state when this row has
 * been drawn before, the wire's value only on the very first draw.
 */
export function initialFold(name: string, wireFolded: boolean, rc: RowContext): boolean {
  const previous = rc.previous?.querySelector(`[data-fold="${name}"]`) ?? null;
  const held = previous?.getAttribute(FOLD_STATE_ATTRIBUTE) ?? null;
  if (held === null) return wireFolded;
  return held === "true";
}

/**
 * Run one rpc with a cluster of controls latched inert, and hand back whatever
 * it answered.
 *
 * THE LATCH IS NOT COSMETIC. An ask card answers exactly once; a second click
 * while the first is unanswered is a second verb the daemon will refuse as
 * already-answered, and the refusal would be OUR defect wearing the daemon's
 * words. The buttons come back only if the call answered with something the
 * card draws in place — a refusal — because a card whose answer LANDED is about
 * to be replaced by the row's own re-push, and re-enabling its buttons in the
 * meantime offers a click that can no longer be legitimate.
 */
export async function whileInFlight<T>(
  buttons: readonly HTMLButtonElement[],
  fn: () => Promise<T>,
): Promise<{ value: T } | { failed: unknown }> {
  for (const button of buttons) button.disabled = true;
  try {
    return { value: await fn() };
  } catch (err) {
    for (const button of buttons) button.disabled = false;
    return { failed: err };
  }
}

/** Give the controls back after an answer the card drew in place. */
export function release(buttons: readonly HTMLButtonElement[]): void {
  for (const button of buttons) button.disabled = false;
}

/** The class the three agentic bubbles wear on top of the response treatment. */
export const AGENTIC_CLASS = "agentic";

/**
 * THE PURPLE RESPONSE-STYLED BUBBLE, shared by the artifact, plan and findings
 * cards.
 *
 * It IS the response bubble — the same `.bubble.assistant.md` element, the same
 * body, the same 25-line cap — plus ONE accent class. Purple is the register
 * for the agentic and vendor-facing bubbles, and the point of building it here
 * is that the three cards cannot drift into three purples: a reader must not
 * have to learn that a published page and a plan are the same kind of thing
 * twice.
 *
 * The heading, when there is one, is drawn VERBATIM. An artifact's heading
 * carries the daemon's own favicon emoji; this end adds no glyph of its own and
 * strips none.
 */
export function agenticBubble(opts: {
  /** The `data-state` value: the message's own state or outcome arm. */
  state: string;
  /** The heading line, verbatim. Omitted where the message has none. */
  heading?: string;
  /** What the state draws under the heading. */
  content: readonly ChildNode[];
  /** The row's previous draw, updated in place (drawBubble). */
  previous?: HTMLElement;
}): HTMLElement {
  const content: ChildNode[] = [];
  if (opts.heading !== undefined) {
    const heading = document.createElement("div");
    heading.className = "agentic-heading";
    heading.textContent = opts.heading;
    content.push(heading);
  }
  content.push(...opts.content);
  return drawBubble(
    {
      role: "response",
      variant: "agentic",
      state: opts.state,
      hooks: ["assistant", "md", AGENTIC_CLASS],
      content,
      capLines: "feed",
    },
    opts.previous,
  ).bubble;
}
