/**
 * animation-delays — READ AND MOVE A PAGE'S CSS ANIMATIONS BY THEIR DELAYS,
 * NEVER BY ASKING WEBKIT FOR AN ANIMATION.
 *
 * WHY. On the system WebKit an animation script has asked for
 * (`getAnimations()`) is never freed, nor the element it animated (2026-11-18;
 * the webapp's AGENTS.md, Performance lessons). So every module that seeks a
 * running animation (the rail's wall-clock phase, a hidden page's resume)
 * reads it off computed style and moves it by its `animation-delay`: a delay
 * D ms more negative puts an animation D ms further on.
 *
 * PSEUDO-ELEMENTS TOO. A loop on an element's `::before` or `::after` has no
 * inline style to write, so its delays go into a custom property on the
 * element, which the stylesheet applies to the pseudo-element whenever the
 * element wears its marker attribute (styles.css, "animation-delays").
 * RESTARTING an animation (making it fresh) is one attribute the stylesheet
 * reads as `animation-name: none` on the element and both its pseudo-elements.
 */
import { log } from "./log.js";

/** Where on an element an animation runs. */
export type AnimationPart = "" | "::before" | "::after";

/** Every part an element's animations can run on. */
export const ANIMATION_PARTS: readonly AnimationPart[] = ["", "::before", "::after"];

/** The attribute the stylesheet reads as "every animation here stops, so the next style starts it fresh". */
export const ANIMATION_RESTARTING_ATTRIBUTE = "data-animation-restarting";

/** The marker attribute and custom property carrying a pseudo-element's delays. */
export const PSEUDO_DELAY: Readonly<Record<"::before" | "::after", { attribute: string; property: string }>> = {
  "::before": { attribute: "data-animation-delay-before", property: "--animation-delay-before" },
  "::after": { attribute: "data-animation-delay-after", property: "--animation-delay-after" },
};

/** One part's running animations, as its computed style lists them. */
export interface PartAnimations {
  readonly element: HTMLElement | SVGElement;
  readonly part: AnimationPart;
  readonly names: string[];
  readonly durations: number[];
  readonly iterations: number[];
  readonly directions: PlaybackDirection[];
  readonly delays: number[];
}

/**
 * The parts of ELEMENT running at least one animation, read off VIEW's
 * computed style; none for an element that is neither HTML nor SVG.
 */
export function animatedParts(element: Element, view: Window & typeof globalThis): PartAnimations[] {
  if (!(element instanceof view.HTMLElement) && !(element instanceof view.SVGElement)) return [];
  const found: PartAnimations[] = [];
  for (const part of ANIMATION_PARTS) {
    const animations = partAnimations(element, part, view.getComputedStyle(element, part === "" ? null : part));
    if (animations !== null) found.push(animations);
  }
  return found;
}

/** PART of ELEMENT's animations as COMPUTED lists them, or null when it runs none. */
export function partAnimations(
  element: HTMLElement | SVGElement,
  part: AnimationPart,
  computed: CSSStyleDeclaration,
): PartAnimations | null {
  const names = cssList(computed.animationName);
  if (names.every((name) => name === "none" || name === "")) return null;
  return {
    element,
    part,
    names,
    durations: cssList(computed.animationDuration).map(cssDurationMs),
    iterations: cssList(computed.animationIterationCount).map((v) => (v === "infinite" ? Infinity : Number(v))),
    directions: cssList(computed.animationDirection) as PlaybackDirection[],
    delays: cssList(computed.animationDelay).map(cssTimeMs),
  };
}

/** LIST's I'th entry, repeating a list shorter than the names as CSS does. */
export function nth<T>(list: readonly T[], i: number): T {
  return list[i % list.length];
}

/** Give PART of ELEMENT the animation delays DELAYS_MS, one per animation. */
export function writeDelays(element: HTMLElement | SVGElement, part: AnimationPart, delaysMs: readonly number[]): void {
  const value = delaysMs.map((ms) => `${ms}ms`).join(", ");
  if (part === "") {
    element.style.animationDelay = value;
    return;
  }
  const { attribute, property } = PSEUDO_DELAY[part];
  element.style.setProperty(property, value);
  element.setAttribute(attribute, "");
}

/**
 * Make every animation on ELEMENTS (and their pseudo-elements) fresh: each
 * stops for one style pass, and the next style starts it again.
 */
export function restartAnimations(elements: Iterable<Element>, flush: () => void): void {
  const marked = [...elements];
  if (marked.length === 0) return;
  for (const element of marked) element.setAttribute(ANIMATION_RESTARTING_ATTRIBUTE, "");
  flush();
  for (const element of marked) element.removeAttribute(ANIMATION_RESTARTING_ATTRIBUTE);
}

/** A computed comma list's parts. */
export function cssList(value: string): string[] {
  return value.split(",").map((part) => part.trim());
}

/**
 * A computed `animation-duration` in milliseconds. `auto` is the property's
 * initial value, which CSS Animations Level 2 defines as 0s for a CSS
 * animation.
 */
export function cssDurationMs(value: string): number {
  return value === "auto" ? 0 : cssTimeMs(value);
}

/** A computed CSS time (`1.3s`, `200ms`) in milliseconds; anything else is a broken style read. */
export function cssTimeMs(value: string): number {
  const ms = value.endsWith("ms")
    ? Number(value.slice(0, -2))
    : value.endsWith("s")
      ? Number(value.slice(0, -1)) * 1000
      : NaN;
  if (!Number.isFinite(ms)) {
    log.error(`a computed animation time is no CSS time (${value})`, {
      operation: "animation-delays.bad-time",
      context: { value },
    });
    throw new Error(`animation-delays: a computed animation time is no CSS time (${value})`);
  }
  return ms;
}
