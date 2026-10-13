/**
 * earlier-steps — A TURN'S EARLIER STEPS ARE ON AN OLDER PAGE.
 *
 * The daemon draws this row right under a turn's prompt when the page that
 * carried the prompt did not carry the steps between it and the page's own
 * rows (docs/protobuf-design/complete-pages.md). It is a fold: activating it
 * runs the same older-page walk "load previous page" runs, and the daemon
 * removes the row once the walk has loaded those steps.
 */
import { createControl } from "../../control.js";

/** The fold's label. */
export const EARLIER_STEPS_LABEL = "earlier steps";

/** Draw the fold; ACTIVATE runs the older-page walk. */
export function drawFeedEarlierSteps(activate: () => void): HTMLElement {
  const control = createControl();
  control.className = "feed-earlier-steps";
  control.setAttribute("data-earlier-steps", "");
  control.textContent = EARLIER_STEPS_LABEL;
  control.addEventListener("click", (event) => {
    event.preventDefault();
    activate();
  });
  return control;
}
