/**
 * cache-lapse — a hidden workspace's countdown to its prompt cache's lapse
 * (frontend.v1.RosterRowWhenHiddenCache, owner request 2026-10-13).
 *
 * A hidden workspace's keep-alives are held, so its cache lapses on its own
 * clock; until then the user can show it again warm. The when-column counts
 * down to that moment ("12m 34s") and reads "cold cache" once it has come,
 * on a continuous green-to-red gradient: green an hour or more out, the
 * deepest red at the lapse.
 */
import { formatAge } from "../duration.js";

/** How far out a lapse still reads fully green: the 1-hour cache tier. */
export const CACHE_LAPSE_GREEN_MS = 60 * 60 * 1000;

/** What the column reads once the lapse has come. */
export const COLD_CACHE_TEXT = "cold cache";

/**
 * The countdown's text for REMAINING_MS to the lapse. Seconds round UP, so the
 * column never reads "0s" while the cache is still warm: the last second reads
 * "1s", and the lapse itself reads "cold cache".
 */
export function cacheLapseText(remainingMs: number): string {
  if (remainingMs <= 0) return COLD_CACHE_TEXT;
  return formatAge(Math.ceil(remainingMs / 1000) * 1000);
}

/**
 * Where on the heat gradient REMAINING_MS sits: 0 (green) an hour or more out,
 * 1 (deepest red) at the lapse and after it, linear between.
 */
export function cacheLapseHeat(remainingMs: number): number {
  const warmth = Math.min(Math.max(remainingMs / CACHE_LAPSE_GREEN_MS, 0), 1);
  return 1 - warmth;
}
