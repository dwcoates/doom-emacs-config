/**
 * earlier-steps — A TURN'S EARLIER STEPS ARE ON AN OLDER PAGE.
 *
 * The daemon draws this row right under a turn's prompt when the page that
 * carried the prompt did not carry the steps between it and the page's own
 * rows (docs/protobuf-design/complete-pages.md). It is a LOADING STATE: a
 * small spinner centered under the prompt, and nothing else: no band, no
 * border, no label (owner request, 2026-11-01; a grey "earlier steps" fold
 * the reader clicked stood here before).
 *
 * The spinner is a FACE OF THE FEED'S WALK (walk-control.ts `WalkFaces`): it
 * is the walk control's own look, it runs the same older-page walk "load
 * previous page" runs the moment it comes into view, and it turns whenever
 * that walk is in flight. The daemon removes the row once the walk has loaded
 * the steps. A refused walk rests the spinner and draws its refusal beside
 * it, as the top control does; scrolling it out of view and back in, or a
 * click, asks again.
 */
import type { Control } from "../../control.js";
import { onDiscard } from "../ticking.js";
import { drawWalkControl } from "../walk-control.js";

/** The attribute marking a turn's earlier-steps indicator. */
export const EARLIER_STEPS_ATTRIBUTE = "data-earlier-steps";

/** What the row asks of the feed whose walk it is a face of. */
export interface EarlierStepsWalk {
  /** Run the older-page walk, its refusal drawn beside INDICATOR. */
  run(indicator: Control): void;
  /**
   * Make INDICATOR a face of the feed's walk (`WalkFaces.add`); answers its
   * teardown, run when the row is discarded.
   */
  attach(indicator: Control): () => void;
}

/** Draw the indicator, centered; seeing or clicking it runs WALK. */
export function drawFeedEarlierSteps(walk: EarlierStepsWalk): HTMLElement {
  const row = document.createElement("div");
  row.className = "feed-earlier-steps";
  const indicator = drawWalkControl(EARLIER_STEPS_ATTRIBUTE);
  indicator.addEventListener("click", (event) => {
    event.preventDefault();
    walk.run(indicator);
  });
  row.append(indicator);
  onDiscard(row, walk.attach(indicator));
  return row;
}
