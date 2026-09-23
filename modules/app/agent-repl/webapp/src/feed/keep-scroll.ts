/**
 * keep-scroll — a redraw never replaces a box the reader has scrolled.
 *
 * OWNER RULE, 2026-09-23: THE USER OWNS THE SCROLL. A bubble's own scroll box
 * (an expanded tool output, a detached shell's tail, a hook's box, a prompt
 * bubble) moves only on the reader's input. But the feed core redraws a card
 * WHOLE on every push of its row -- a tool card on each progress beat and on
 * its result, a shell's spool on every chunk of output -- and a fresh element
 * starts at the top. Swapping it in threw a reader scrolled inside that box
 * back to its top on every push.
 *
 * So when the element a redraw would discard holds a box the reader scrolled,
 * the redraw MORPHS it instead: every element on the path from the card's root
 * down to that box is KEPT (its attributes brought to the fresh draw's), and
 * everything else is taken from the fresh draw. The scrolled box never leaves
 * the document, so its position -- and the content under the reader -- stay
 * exactly where they were. A card with no scrolled box is replaced as before,
 * which is the cheap and ordinary case.
 *
 * The response bubble does not come here: it updates itself in place
 * (cards/response.ts), since it streams far more often than anything else.
 */
import { log } from "../log.js";
import { placeChildren } from "../dom.js";
import { HAS_MORE_CLASS } from "./bubble-more.js";
import { DISCARD_ATTRIBUTE, TICKING_ATTRIBUTE, stopTicking } from "./ticking.js";

/** The live registries' own markers: the kept element's stand, whatever the draw says. */
const KEPT_ATTRIBUTES: ReadonlySet<string> = new Set([TICKING_ATTRIBUTE, DISCARD_ATTRIBUTE]);

/**
 * Morph LIVE into FRESH when LIVE holds a box the reader has scrolled, keeping
 * that box and its ancestors in the document. Answers whether it did: false
 * leaves LIVE untouched for the caller to replace with FRESH.
 */
export function keepScrolled(live: HTMLElement, fresh: HTMLElement): boolean {
  const path = scrolledPath(live);
  if (path.size === 0) return false;
  if (live.tagName !== fresh.tagName) {
    log.debug("a redraw changed the shape of a card the reader scrolled; it is replaced", {
      operation: "feed.keep-scroll.shape-changed",
      context: { from: live.tagName, to: fresh.tagName },
    });
    return false;
  }
  morph(live, fresh, path);
  log.debug("a redraw kept the scroll box the reader is inside", {
    operation: "feed.keep-scroll.kept",
    context: { boxes: [...path].filter((el) => el.scrollTop > 0).length },
  });
  return true;
}

/**
 * Every element of ROOT (itself included) that is a scrolled box or an
 * ancestor of one, up to ROOT. Empty when the reader has scrolled nothing in it.
 */
function scrolledPath(root: HTMLElement): Set<Element> {
  const path = new Set<Element>();
  for (const el of [root, ...root.querySelectorAll("*")]) {
    if (el.scrollTop <= 0 && el.scrollLeft <= 0) continue;
    for (let node: Element | null = el; node !== null; node = node.parentElement) {
      path.add(node);
      if (node === root) break;
    }
  }
  return path;
}

/**
 * Bring LIVE to FRESH in place: its attributes are FRESH's, and each child is
 * FRESH's own, except a child on PATH, which is kept and morphed in turn. Only
 * nodes out of place move (`placeChildren`), so the kept ones never leave the
 * document. Whatever is dropped -- the live nodes replaced, the fresh
 * counterparts of the kept ones -- has its clocks and discard hooks released.
 */
function morph(live: Element, fresh: Element, path: ReadonlySet<Element>): void {
  syncAttributes(live, fresh);
  const liveKids = [...live.childNodes];
  const freshKids = [...fresh.childNodes];
  const next: ChildNode[] = freshKids.map((want, i) => {
    const have = liveKids[i];
    if (have instanceof Element && want instanceof Element && path.has(have) && have.tagName === want.tagName) {
      morph(have, want, path);
      return have;
    }
    return want;
  });
  const kept = new Set<ChildNode>(next);
  for (const have of liveKids) {
    if (!kept.has(have) && have instanceof Element) stopTicking(have);
  }
  placeChildren(live, next);
  // FRESH now holds only what was not moved into LIVE: its own registrations
  // and those of the counterparts of kept children, all of which are dropped.
  stopTicking(fresh);
}

/** Make LIVE's attributes FRESH's, keeping the live registries' markers and the measured `has-more`. */
function syncAttributes(live: Element, fresh: Element): void {
  const measured = live.classList.contains(HAS_MORE_CLASS);
  for (const attr of [...live.attributes]) {
    if (KEPT_ATTRIBUTES.has(attr.name)) continue;
    if (!fresh.hasAttribute(attr.name)) live.removeAttribute(attr.name);
  }
  for (const attr of [...fresh.attributes]) {
    if (KEPT_ATTRIBUTES.has(attr.name)) continue;
    if (live.getAttribute(attr.name) !== attr.value) live.setAttribute(attr.name, attr.value);
  }
  live.classList.toggle(HAS_MORE_CLASS, measured);
}
