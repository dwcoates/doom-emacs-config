/**
 * quiet-markup — the page's markup as a settle compares it: what the page
 * says, without what is only time passing.
 *
 * Two things move on their own and are not the page changing:
 *
 *   - a CLOCK (`[data-ticking]`) counts up every second;
 *   - a TYPE-OUT reveals text that has already arrived, a step per animation
 *     frame (src/type-out.ts). The harness fakes frames on a clock that runs
 *     with real time, so a settle whose rounds were slower than a frame, as on
 *     a loaded host, saw a new step on every round and never converged
 *     (2026-10-16: "the DOM never stopped changing after 60 settle rounds"
 *     under the merge gate's full run, on a markdown response still typing).
 *
 * So a clock reads empty, every text node reads its whole text
 * (`wholeTextOf`), and the reveal's own bookkeeping attributes are dropped. A
 * new arrival still changes the whole text, so it still counts. A case that
 * needs the typed text waits for it (drive.ts `awaitTypedOut`).
 */
import { UNREVEALED_ATTRIBUTE, wholeTextOf } from "../../src/bubble/text-reveal.js";
import { REVEALED_ATTRIBUTE } from "../../src/feed/cards/response.js";
import { TITLE_REVEALED_ATTRIBUTE } from "../../src/feed/title-reveal.js";

/** The attributes a type-out writes as it goes. */
export const REVEAL_BOOKKEEPING = [UNREVEALED_ATTRIBUTE, REVEALED_ATTRIBUTE, TITLE_REVEALED_ATTRIBUTE] as const;

/** ROOT's inner markup with its clocks emptied and its type-outs read whole. */
export function quietMarkupOf(root: Element): string {
  const clone = root.cloneNode(true) as Element;
  const doc = root.ownerDocument;
  const original = doc.createTreeWalker(root, NodeFilter.SHOW_TEXT);
  const copy = doc.createTreeWalker(clone, NodeFilter.SHOW_TEXT);
  for (let node = original.nextNode(); node !== null; node = original.nextNode()) {
    const mirror = copy.nextNode() as Text;
    mirror.data = wholeTextOf(node as Text);
  }
  for (const attribute of REVEAL_BOOKKEEPING) {
    for (const element of clone.querySelectorAll(`[${attribute}]`)) element.removeAttribute(attribute);
  }
  for (const clock of clone.querySelectorAll("[data-ticking]")) clock.textContent = "";
  return clone.innerHTML;
}
