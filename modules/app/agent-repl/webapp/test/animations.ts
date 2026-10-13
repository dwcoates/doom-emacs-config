/**
 * `Element.getAnimations` FOR JSDOM — an environment substitution, because
 * jsdom lacks the capability rather than because the app has a seam here.
 *
 * jsdom runs no CSS animations at all, so the truthful answer to "what is
 * animating under this element" is always nothing. The rail reads it after
 * every draw (`src/sidebar/wall-phase.ts`); a suite that needs animations to
 * seek injects its own source through the mount's `wallPhase` deps.
 */

/**
 * Install the stub on `Element.prototype` unless the environment already has
 * one. Called before every file, because the worker's environment is shared
 * across files (`isolate: false`) but a node-environment file has no DOM at
 * all — there is nothing to install on, and a later jsdom file in the same
 * worker installs it then.
 */
export function installGetAnimations(): void {
  if (typeof Element === "undefined") return;
  if (typeof Element.prototype.getAnimations === "function") return;
  Element.prototype.getAnimations = function none(): Animation[] {
    return [];
  };
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
