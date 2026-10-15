/**
 * THE CAPABILITIES JSDOM LACKS, installed as one set by every jsdom project's
 * setup file (`test/setup.ts`, `test/webapp-layer/setup.ts`).
 *
 * Each is an environment substitution rather than a seam in the app: jsdom
 * performs no layout and runs no animations, so it ships no
 * `ResizeObserver`, no `IntersectionObserver`, no `Range.getClientRects`, no
 * `Element.getAnimations` and
 * no `Element.animate`, and the app reaches for all four. One installer keeps the projects from
 * drifting apart: a capability added to one setup file and not the other let
 * the webapp layer throw on every roster push while the unit run stayed green.
 */
import { installAnimate, installGetAnimations } from "./animations.js";
import { installIntersectionObserver } from "./intersection-observer.js";
import { installRangeClientRects } from "./range-rects.js";
import { installResizeObserver } from "./resize-observer.js";

/** Install every substitution. Idempotent, as each installer is. */
export function installJsdomCapabilities(): void {
  installResizeObserver();
  installIntersectionObserver();
  installRangeClientRects();
  installGetAnimations();
  installAnimate();
}
