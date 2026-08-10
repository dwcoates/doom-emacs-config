/**
 * html-slot — a chrome slot that is written ONLY when its markup changed.
 *
 * WHY IT EXISTS. The chrome renders on its own cadence and rewrites every
 * state-derived slot from a pure function of the store. For a slot carrying
 * only text that is harmless. For a slot carrying BUTTONS it is not: a browser
 * fires `click` only when the mousedown and the mouseup land on the same node,
 * so a repaint between the two — one frame, from a state change the user's
 * press had nothing to do with — destroys the pressed button and swallows the
 * click. The user presses, nothing happens, and the only recovery is pressing
 * again.
 *
 * The fix is to stop rewriting markup that did not change, which makes the
 * swallow IMPOSSIBLE for every frame that is not a real state change rather
 * than merely unlikely: the button's node now outlives every such frame.
 *
 * THE LAST PAINT IS REMEMBERED HERE, not read back off the element. Reading
 * `innerHTML` serializes the live tree, which the browser is free to normalize
 * (attribute order, entity spelling, whitespace) into something that never
 * compares equal to the string that produced it — a guard that never holds is
 * the bug it was written to fix.
 */

/** One chrome slot, plus the markup last written into it. */
export class HtmlSlot {
  private painted: string | null = null;

  constructor(private readonly el: HTMLElement) {}

  /**
   * Write `html` into the slot if it differs from what is already there.
   *
   * Returns whether the DOM was written, so a caller can log or count the
   * repaints it actually caused.
   */
  paint(html: string): boolean {
    if (this.painted === html) return false;
    this.painted = html;
    this.el.innerHTML = html;
    return true;
  }
}
