/**
 * A PSEUDO-ELEMENT'S COMPUTED STYLE FOR JSDOM — an environment substitution,
 * because jsdom lacks the capability rather than because the app has a seam.
 *
 * jsdom computes no pseudo-element styles: `getComputedStyle(el, "::before")`
 * answers the ELEMENT's own, so an element running one animation would read
 * as running it three times (src/animation-delays.ts reads every part). The
 * truthful answer in a page with no stylesheet pseudo-element rules is a
 * style declaring nothing, which runs no animation.
 */

/** The marker the window wears once the substitution is in place, so it is installed once. */
const PSEUDO_AWARE = Symbol.for("agent-repl.test.pseudo-aware-computed-style");

/**
 * Install the substitution on the window unless it is already in place.
 * Called before every file, as the other substitutions are; a
 * node-environment file has no window to install on.
 */
export function installPseudoElementStyle(): void {
  if (typeof window === "undefined") return;
  const marked = window as unknown as { [PSEUDO_AWARE]?: true };
  if (marked[PSEUDO_AWARE] === true) return;
  const elementOnly = window.getComputedStyle.bind(window);
  window.getComputedStyle = (element: Element, pseudo?: string | null): CSSStyleDeclaration =>
    pseudo === undefined || pseudo === null || pseudo === "" ? elementOnly(element) : document.createElement("div").style;
  marked[PSEUDO_AWARE] = true;
}

/**
 * `Element.animate` FOR JSDOM: jsdom runs no animations, so the element is
 * left as it is and a finished animation is answered. A suite that asserts on
 * an animation drives the app's own seam (`collapse-motion.ts`'s deps) or spies
 * on this method.
 */
export function installAnimate(): void {
  if (typeof Element === "undefined") return;
  if (typeof Element.prototype.animate === "function") return;
  Element.prototype.animate = function inert(): Animation {
    return { finished: Promise.resolve(), cancel: () => {} } as unknown as Animation;
  };
}
