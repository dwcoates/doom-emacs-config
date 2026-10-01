/**
 * work-dot — THE STATE DOT leading a subagent or detached shell bubble's head
 * (owner request, 2026-10-01).
 *
 * The color comes from the shared vocabulary (`render-colors.json`
 * `feed_subagent_dot` / `feed_shell_dot`, through src/vocab.ts), never from a
 * local table. The SHAPE follows the color: a dot that spends no color is
 * HOLLOW (a run that settled without anything wanting a look), every colored
 * dot FILLED. A live dot also pulses, as it always has.
 */
import { toneClass, type Color } from "../vocab.js";

/** The glyph a filled and a hollow dot draw. */
export const WORK_DOT_GLYPHS = { filled: "●", hollow: "○" } as const;

/** The dot for a head painted COLOR; LIVE adds the pulse. */
export function drawWorkDot(color: Color, live: boolean): HTMLElement {
  const shape = color === "none" ? "hollow" : "filled";
  const dot = document.createElement("span");
  dot.className = `agent-dot work-dot ${toneClass(color)}`;
  if (live) dot.classList.add("work-dot-live");
  dot.setAttribute("data-dot", shape);
  dot.setAttribute("aria-hidden", "true");
  dot.textContent = WORK_DOT_GLYPHS[shape];
  return dot;
}
