/**
 * page-presence — the page's own record of being hidden, shown, focused,
 * blurred and resized, and of how long the first paint took after each.
 *
 * WHY IT EXISTS. Switching workspaces in Emacs with the keyboard (`s-{`/`s-}`,
 * the numerals) makes SOME workspaces' webviews blank for a moment and then
 * paint everything at once, repeatably for the same workspace, while
 * switching by clicking the sidebar does not. Nothing on the page recorded
 * what it went through during a switch, so the blank could not be attributed:
 * a hide/show, a resize (and the reflow it forces), a focus change, or a
 * long first paint on a heavy page. Each of those is now one INFO record, and
 * every show or resize is followed by a `page.repaint` record that measures
 * the time to the second animation frame — the first frame the compositor
 * has actually drawn — together with how big the page is, so a slow repaint
 * can be tied to the page's size.
 *
 * Resizes are coalesced to one record per animation frame: a window being
 * laid out reports several sizes in one frame, and only the settled one is a
 * fact about the switch.
 */
import { log } from "./log.js";

/** What the presence log needs from its environment, injectable for tests. */
export interface PagePresenceEnv {
  readonly doc: Document;
  readonly win: Window;
  /** Monotonic milliseconds. */
  readonly now: () => number;
  /** Schedules a callback for the next animation frame. */
  readonly frame: (callback: () => void) => void;
}

/** The installed log; `stop` removes every listener. */
export interface PagePresence {
  stop(): void;
}

/** The default environment: the page itself. */
export function pagePresenceEnv(): PagePresenceEnv {
  return {
    doc: document,
    win: window,
    now: () => performance.now(),
    frame: (callback) => {
      requestAnimationFrame(() => callback());
    },
  };
}

/** Install the presence log on the page. */
export function installPagePresenceLog(env: PagePresenceEnv = pagePresenceEnv()): PagePresence {
  const { doc, win, now, frame } = env;
  let lastEventAt = now();
  let size = { width: win.innerWidth, height: win.innerHeight };
  let resizePending = false;

  const sinceLast = (): number => {
    const at = now();
    const since = Math.round(at - lastEventAt);
    lastEventAt = at;
    return since;
  };

  const pageSize = (): Record<string, number> => ({
    dom_nodes: doc.getElementsByTagName("*").length,
    feed_rows: doc.querySelectorAll("[data-feed-row]").length,
  });

  const measureRepaint = (cause: string): void => {
    const started = now();
    frame(() => {
      frame(() => {
        log.info(`the page painted ${Math.round(now() - started)}ms after ${cause}`, {
          operation: "page.repaint",
          context: {
            cause,
            paint_ms: Math.round(now() - started),
            width: win.innerWidth,
            height: win.innerHeight,
            ...pageSize(),
          },
        });
      });
    });
  };

  const onVisibility = (): void => {
    const state = doc.visibilityState;
    log.info(`the page became ${state}`, {
      operation: "page.visibility",
      context: { state, since_last_ms: sinceLast() },
    });
    if (state === "visible") measureRepaint("visible");
  };

  const onFocus = (): void => {
    log.info("the page's window gained focus", {
      operation: "page.focus",
      context: { since_last_ms: sinceLast(), visibility: doc.visibilityState },
    });
  };

  const onBlur = (): void => {
    log.info("the page's window lost focus", {
      operation: "page.blur",
      context: { since_last_ms: sinceLast(), visibility: doc.visibilityState },
    });
  };

  const onResize = (): void => {
    if (resizePending) return;
    resizePending = true;
    frame(() => {
      resizePending = false;
      const next = { width: win.innerWidth, height: win.innerHeight };
      if (next.width === size.width && next.height === size.height) return;
      const before = size;
      size = next;
      log.info(`the page resized from ${before.width}x${before.height} to ${next.width}x${next.height}`, {
        operation: "page.resize",
        context: {
          before_width: before.width,
          before_height: before.height,
          width: next.width,
          height: next.height,
          since_last_ms: sinceLast(),
          visibility: doc.visibilityState,
        },
      });
      measureRepaint("resize");
    });
  };

  doc.addEventListener("visibilitychange", onVisibility);
  win.addEventListener("focus", onFocus);
  win.addEventListener("blur", onBlur);
  win.addEventListener("resize", onResize);
  return {
    stop(): void {
      doc.removeEventListener("visibilitychange", onVisibility);
      win.removeEventListener("focus", onFocus);
      win.removeEventListener("blur", onBlur);
      win.removeEventListener("resize", onResize);
    },
  };
}
