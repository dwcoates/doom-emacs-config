/**
 * title-measure — the box a title fold is measured against, for jsdom.
 *
 * NOT A SUITE. jsdom lays nothing out, so a title's overflow is whatever a test
 * says it is: `measureTitle` gives the title a two-line client height and a
 * content height that does or does not exceed it, and `inBubbleFold` seats a
 * bubble head where bubble.ts seats it, so a head's title has the card fold
 * that owns it (title-fold.ts) and is never reported as orphaned.
 */

/** Give TITLE a two-line box whose content does, or does not, overflow it. */
export function measureTitle(title: HTMLElement, overflow: boolean): void {
  Object.defineProperty(title, "clientHeight", { configurable: true, value: 40 });
  Object.defineProperty(title, "scrollHeight", { configurable: true, value: overflow ? 120 : 40 });
}

/**
 * Seat HEAD in a connected, collapsed `.bubble-fold`, as bubble.ts does
 * (`.tool-card.bubble-fold > .tool-head.bubble-head > .bubble-head-slot`), as
 * the document body's only child, and answer the bubble.
 */
export function inBubbleFold(head: HTMLElement): HTMLElement {
  const bubble = document.createElement("div");
  bubble.className = "tool-card bubble-fold";
  bubble.setAttribute("data-expanded", "false");
  const headLine = document.createElement("div");
  headLine.className = "tool-head bubble-head";
  const slot = document.createElement("span");
  slot.className = "bubble-head-slot";
  slot.append(head);
  headLine.append(slot);
  bubble.append(headLine);
  document.body.replaceChildren(bubble);
  return bubble;
}
