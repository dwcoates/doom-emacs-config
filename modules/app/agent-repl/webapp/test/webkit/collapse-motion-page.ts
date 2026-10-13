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
  /** Ease a 30px bubble from a 180px one, and read its height at each of AT_MS. */
  heightsDuring(atMs: readonly number[]): Promise<number[]>;
}

declare global {
  interface Window {
    collapseMotion: CollapseMotionPage;
  }
}

window.collapseMotion = {
  async heightsDuring(atMs) {
    const el = document.createElement("div");
    el.style.cssText = "height: 30px; background: rgb(255, 255, 255)";
    document.body.replaceChildren(el);
    easeIntoLook(el, { height: 180, background: "rgb(240, 236, 255)", boxShadow: "none" });
    const start = performance.now();
    const heights: number[] = [];
    for (const at of atMs) {
      await new Promise((resolve) => setTimeout(resolve, Math.max(0, at - (performance.now() - start))));
      heights.push(Math.round(el.getBoundingClientRect().height));
    }
    return heights;
  },
};
