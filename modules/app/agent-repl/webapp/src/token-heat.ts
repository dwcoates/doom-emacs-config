/**
 * token-heat — the ONE color a token figure takes by how hot it is (frontend.v1
 * TokenHeat): the footer's tokens cell and a response bubble's corner figure
 * draw it alike, so a figure reads the same color wherever it is drawn.
 */
import { MalformedView } from "./rpc/malformed.js";

/** How many colors the heat gradient runs through (`--token-heat-0` … `-3`). */
const HEAT_COLORS = 4;

/**
 * The color at POSITION on the heat gradient: the two theme colors bracketing
 * it, mixed by how far between them it sits. The daemon owns where a figure
 * falls (TokenHeat); the stylesheet owns the four colors, so this only
 * interpolates. A position outside [0, 1] is a daemon contract breach and is
 * refused.
 */
export function tokenHeatColor(position: number, path: string): string {
  if (!Number.isFinite(position) || position < 0 || position > 1) {
    throw new MalformedView(path, `heat position ${String(position)} is outside [0, 1]`);
  }
  const segments = HEAT_COLORS - 1;
  const lower = Math.min(Math.floor(position * segments), segments - 1);
  const upperShare = Math.round((position * segments - lower) * 100);
  return `color-mix(in oklab, var(--token-heat-${String(lower)}), var(--token-heat-${String(lower + 1)}) ${String(upperShare)}%)`;
}
