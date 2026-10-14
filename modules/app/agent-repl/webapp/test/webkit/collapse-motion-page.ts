/**
 * THE PAGE THE WEBKIT COLLAPSE-MOTION TEST DRIVES (collapse-motion.webkit.test.ts):
 * the real easing over a real laid-out bubble.
 */
import { easeIntoLook } from "../../src/bubble/collapse-motion.js";
import { ForwardingLogger, bindLogContext, setLogger } from "../../src/log.js";

setLogger(new ForwardingLogger(() => Promise.resolve("accepted"), () => undefined, {}, "debug"));
bindLogContext({ connection_id: "webkit-collapse-motion-page" });

/** What the test calls, on `window.collapseMotion`. */
export interface CollapseMotionPage {
  /** Ease a 30px bubble from a 180px one, and read its height at each of AT_MS into the easing. */
  heightsDuring(atMs: readonly number[]): Promise<number[]>;
}

declare global {
  interface Window {
    collapseMotion: CollapseMotionPage;
  }
}

// THE HEIGHTS ARE READ AT THE ANIMATION'S OWN TIMES, never after a real wait:
// a timer due at 60ms fired after the 220ms easing had ended on a loaded host,
// and read the landed height as the eased one. The easing is paused once it
// starts, and seeked to each time in turn.
window.collapseMotion = {
  async heightsDuring(atMs) {
    const el = document.createElement("div");
    el.style.cssText = "height: 30px; background: rgb(255, 255, 255)";
    document.body.replaceChildren(el);
    easeIntoLook(el, { height: 180, background: "rgb(240, 236, 255)", boxShadow: "none" });
    // The easing starts in the frame callback it queued first, so it stands
    // by the time this later one runs.
    await new Promise<void>((resolve) => requestAnimationFrame(() => resolve()));
    const easing = el.getAnimations()[0];
    if (easing === undefined) throw new Error("collapse-motion page: the easing never started");
    easing.pause();
    return atMs.map((at) => {
      easing.currentTime = at;
      return Math.round(el.getBoundingClientRect().height);
    });
  },
};
