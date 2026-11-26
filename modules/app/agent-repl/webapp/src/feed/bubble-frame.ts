/**
 * bubble-frame — the tone a bubble's FRAME wears, stated by its head.
 *
 * The bubble chrome (bubble.ts) draws every bubble's frame; a head that wants
 * the frame drawn in a status color (a child's bubble, in the child's
 * canonical status color while it is open) states that color's name on the
 * element it answers, and the chrome repeats it on the bubble on every draw,
 * as it repeats the head's `data-state`. A head that states none leaves the
 * frame in the card's own border.
 */

/** The attribute a head states its frame's color name in (a render-colors.json name). */
export const FRAME_TONE_ATTRIBUTE = "data-frame-tone";

/** Repeat HEAD's frame tone on the bubble element EL, or clear it when HEAD states none. */
export function applyFrameTone(el: HTMLElement, head: Element): void {
  const tone = head.getAttribute(FRAME_TONE_ATTRIBUTE);
  if (tone === null) el.removeAttribute(FRAME_TONE_ATTRIBUTE);
  else el.setAttribute(FRAME_TONE_ATTRIBUTE, tone);
}
