/**
 * reduced-motion — whether the reader asked for reduced motion: the one read
 * of the `prefers-reduced-motion` media query every script-driven motion takes.
 * A host with no `matchMedia` (a test host) has asked for nothing.
 */

/** The media query, verbatim. */
export const REDUCED_MOTION_QUERY = "(prefers-reduced-motion: reduce)";

/** Whether DOC's reader asked for reduced motion. */
export function prefersReducedMotion(doc: Document): boolean {
  const view = doc.defaultView;
  if (view === null || typeof view.matchMedia !== "function") return false;
  return view.matchMedia(REDUCED_MOTION_QUERY).matches;
}
