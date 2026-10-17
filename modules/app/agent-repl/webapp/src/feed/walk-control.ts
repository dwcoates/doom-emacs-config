/**
 * walk-control — THE "LOAD PREVIOUS PAGE" CONTROL AT THE TOP OF A FEED.
 *
 * Two things about the control live here, so every feed's control behaves the
 * same way:
 *
 *  - ITS LOADING STATE. While a walk is in flight (an older page, a walk to a
 *    row, a re-read of the edge at a compaction) the pill's label gives way to
 *    a spinner (`setWalkLoading`), and the control is disabled, so the reader
 *    sees the page coming rather than an inert label.
 *  - ITS TRIGGER ON SIGHT. The control loads its page the moment it comes into
 *    view (`createWalkTrigger`): scrolling up to the top of what is loaded is
 *    the ask for more. The trigger CLICKS the control, so a sighting runs the
 *    one path a click runs, and a disabled control (a walk already out)
 *    refuses it exactly as it refuses a click.
 *
 * ONE SIGHTING, ONE PAGE. The observer reports transitions, so a control that
 * stays in view after its page lands (the page was short) reports nothing new.
 * The feed calls `rearm` after a page lands, and the observer's fresh first
 * report for the control's current place decides whether the next page is
 * taken. A walk that FAILED is not rearmed: its refusal stands at the control
 * until the reader clicks it or scrolls it out of view and back in, so a daemon
 * that refuses is never asked again in a loop.
 */
import { createControl, type Control } from "../control.js";
import { pageIntersectionObserver } from "../dom.js";
import { log } from "../log.js";

/**
 * What the walk control at the top of a feed reads: it loads the page before
 * the oldest one held (owner request, 2026-10-08; it read "older").
 */
export const FEED_LOAD_MORE_LABEL = "load previous page";

/** The attribute a loading control wears, for the stylesheet and a query. */
export const WALK_LOADING_ATTRIBUTE = "data-loading";

/** The class of the spinner a loading control shows in place of its label. */
export const WALK_SPINNER_CLASS = "feed-load-more-spinner";

/** Build the walk control, idle: the pill reading its label. */
export function drawWalkControl(): Control {
  const control = createControl();
  control.className = "feed-load-more";
  control.setAttribute("data-load-more", "");
  setWalkLoading(control, false);
  return control;
}

/**
 * Show CONTROL loading (a spinner, disabled) or idle (its label, enabled).
 * The one place the control's face and its disabled state change, so the two
 * never disagree.
 */
export function setWalkLoading(control: Control, loading: boolean): void {
  control.disabled = loading;
  if (!loading) {
    control.removeAttribute(WALK_LOADING_ATTRIBUTE);
    control.removeAttribute("aria-label");
    control.textContent = FEED_LOAD_MORE_LABEL;
    return;
  }
  const spinner = document.createElement("span");
  spinner.className = WALK_SPINNER_CLASS;
  control.setAttribute(WALK_LOADING_ATTRIBUTE, "");
  control.setAttribute("aria-label", "loading the previous page");
  control.replaceChildren(spinner);
}

/** The control's trigger on sight, and its teardown. */
export interface WalkTrigger {
  /**
   * Ask the observer afresh where the control is, so a control still in view
   * after its page landed takes the next one.
   */
  rearm(): void;
  /** Stop watching the control when the feed disposes. */
  dispose(): void;
}

/**
 * Watch CONTROL against the page's viewport, clicking it whenever it comes
 * into view. FEED names the feed in the record of each sighting. The root is
 * the viewport, which every ancestor scroll box clips, so the root feed's
 * control and a bubble's are watched alike.
 */
export function createWalkTrigger(
  control: Control,
  feed: string,
  Observer: typeof IntersectionObserver | null = pageIntersectionObserver(),
): WalkTrigger {
  if (Observer === null) {
    // Every page a feed is drawn in is a WebKit page, which has one.
    log.error("the feed cannot load its previous page on sight: the page has no IntersectionObserver", {
      operation: "feed.walk-trigger.unsupported",
      context: { feed },
    });
    throw new Error("walk trigger: the page has no IntersectionObserver");
  }
  const observer = new Observer(
    (entries) => {
      for (const entry of entries) {
        if (!entry.isIntersecting || !entry.target.isConnected) continue;
        log.debug("the load-previous-page control came into view", {
          operation: "feed.walk-trigger.sighted",
          context: { feed, outcome: control.disabled ? "walk already in flight" : "clicked" },
        });
        control.click();
      }
    },
    { threshold: 0 },
  );
  observer.observe(control);
  return {
    rearm: () => {
      observer.unobserve(control);
      observer.observe(control);
    },
    dispose: () => observer.disconnect(),
  };
}
