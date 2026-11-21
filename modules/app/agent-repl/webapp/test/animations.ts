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

/**
 * The substitution built over jsdom's own `getComputedStyle`, kept on the
 * WINDOW (which outlives every file of an un-isolated run) so every file's
 * install finds the same one.
 */
const SUBSTITUTED = Symbol.for("agent-repl.test.pseudo-aware-computed-style");

/**
 * Install the substitution on the window, replacing whatever `getComputedStyle`
 * stands there now. Called before every file, as the other substitutions are;
 * a node-environment file has no window to install on.
 *
 * IT NEVER WRAPS WHAT STANDS. A suite's own `getComputedStyle` (tree-layout.ts,
 * a `vi.spyOn`) is that suite's to remove, and one a file left standing (a
 * spy restored after the wrapper it spied on was uninstalled reinstates that
 * wrapper) would be wrapped again by every later file's install, each layer
 * calling the one beneath it: at 146 files in one process the stack overflowed
 * (2026-11-20). Built once over jsdom's own function, the substitution is
 * installed whole each file, and what a file left behind goes with it.
 */
export function installPseudoElementStyle(): void {
  if (typeof window === "undefined") return;
  let substituted: unknown = Reflect.get(window, SUBSTITUTED);
  if (substituted === undefined) {
    const native: unknown = Reflect.get(window, "getComputedStyle");
    if (typeof native !== "function") throw new Error("jsdom: the window has no getComputedStyle to substitute");
    const elementOnly = (native as typeof window.getComputedStyle).bind(window);
    substituted = (element: Element, pseudo?: string | null): CSSStyleDeclaration =>
      pseudo === undefined || pseudo === null || pseudo === "" ? elementOnly(element) : document.createElement("div").style;
    Reflect.set(window, SUBSTITUTED, substituted);
  }
  window.getComputedStyle = substituted as typeof window.getComputedStyle;
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
