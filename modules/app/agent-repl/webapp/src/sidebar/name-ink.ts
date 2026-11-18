/**
 * name-ink — the copies of a roster row's name that its looping animations
 * move, so the breath and the reviving shimmer run on the compositor.
 *
 * WHY COPIES (measured 2026-11-16). The breath once animated the name's
 * `color` and the shimmer its `mask-position`. WebKit computes both on the
 * page's main thread every frame, so every main-thread block froze them: a
 * roster draw (`sidebar.roster-draw`) takes median 41ms and p90 98ms, and runs
 * up to 140 times a minute during a deploy. `opacity` and `transform` run on
 * the compositor, which keeps them moving however busy the page is. So the
 * name is drawn twice over, and only those two properties move:
 *
 * - THE BREATH is a crossfade. The name's own text rests in the breath's rest
 *   ink, and a copy in its peak ink fades in and out on top of it.
 * - THE SHIMMER is a mask carried by a `transform`. The ink box, as wide as
 *   the old mask, carries the band across the name, and the copies inside it
 *   move back by the same amount, so the text stands still while the band
 *   passes. The name's own text is hidden while the shimmer draws it.
 *
 * The copies are the ink box's `::before` (the resting ink, drawn only while
 * reviving) and `::after` (the peak ink, drawn only while breathing), whose
 * text is the box's `data-name`. Generated text is no part of the name's
 * `textContent`, so search (`search.ts`) still reads the name alone, and the
 * box is `aria-hidden`, so a reader of the accessibility tree reads the name
 * once. `styles.css` owns every look; this only draws the box.
 *
 * The wall-clock seek (`wall-phase.ts`) reads every element's `::before` and
 * `::after` animations too, which are these copies', so a redraw continues
 * each loop where the clock says it is.
 */
import { REVIVING_CLASS } from "./reviving.js";

/** The class of the box holding a name's animated copies. */
export const NAME_INK_CLASS = "name-ink";

/** The attribute the copies draw their text from. */
export const NAME_INK_TEXT_ATTRIBUTE = "data-name";

/** The classes of a name whose looping animation the ink box carries. */
export const NAME_INK_MOTIONS: readonly string[] = ["breathes", REVIVING_CLASS];

/**
 * Give LABEL, whose text is NAME, the ink box its looping animations move,
 * when it wears one of them (`NAME_INK_MOTIONS`). Call it once LABEL's
 * classes are final; a still name gets no box.
 */
export function drawNameInk(label: HTMLElement, name: string): void {
  if (!NAME_INK_MOTIONS.some((motion) => label.classList.contains(motion))) return;
  const ink = document.createElement("span");
  ink.className = NAME_INK_CLASS;
  ink.setAttribute("aria-hidden", "true");
  ink.setAttribute(NAME_INK_TEXT_ATTRIBUTE, name);
  label.appendChild(ink);
}
