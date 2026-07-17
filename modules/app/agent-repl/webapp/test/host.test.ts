import { describe, expect, it } from "vitest";
import {
  CLOSE_MENUS_HOOK,
  HostGlobal,
  TAIL_HOOK,
  installHostCloseMenusHook,
  installHostTailHook,
} from "../src/host.js";
import { ScrollTail } from "../src/scroll.js";

/** A feed scrolled to the middle of its history, as a user leaves one. */
const scrolledUpFeed = (): ScrollTail => ({ scrollTop: 120, scrollHeight: 1000 });

/** Invoke the planted hook the way an Emacs host script does. */
const fireHook = (target: HostGlobal): void => {
  (target[TAIL_HOOK] as () => void)();
};

describe("installHostTailHook", () => {
  it("plants the hook under the name frontend.el calls", () => {
    const target: HostGlobal = {};
    installHostTailHook(target, scrolledUpFeed());
    expect(typeof target[TAIL_HOOK]).toBe("function");
  });

  it("parks a scrolled-up feed at its newest message when fired", () => {
    const target: HostGlobal = {};
    const feed = scrolledUpFeed();
    installHostTailHook(target, feed);
    fireHook(target);
    expect(feed.scrollTop).toBe(feed.scrollHeight);
  });

  it("leaves a feed already at its tail exactly where it is", () => {
    const target: HostGlobal = {};
    const feed: ScrollTail = { scrollTop: 1000, scrollHeight: 1000 };
    installHostTailHook(target, feed);
    fireHook(target);
    expect(feed.scrollTop).toBe(1000);
  });

  it("reads the feed's height at fire time, not at install time", () => {
    const target: HostGlobal = {};
    const feed = scrolledUpFeed();
    installHostTailHook(target, feed);
    // A turn streamed in while the workspace was off screen.
    feed.scrollHeight = 2400;
    fireHook(target);
    expect(feed.scrollTop).toBe(2400);
  });
});

/** Invoke the planted close-menus hook the way an Emacs host script does. */
const fireCloseMenus = (target: HostGlobal): void => {
  (target[CLOSE_MENUS_HOOK] as () => void)();
};

describe("installHostCloseMenusHook", () => {
  it("plants the hook under the name frontend.el calls", () => {
    const target: HostGlobal = {};
    installHostCloseMenusHook(target, () => undefined);
    expect(typeof target[CLOSE_MENUS_HOOK]).toBe("function");
  });

  it("closes the open menus through the callback when fired", () => {
    const target: HostGlobal = {};
    let closed = 0;
    installHostCloseMenusHook(target, () => {
      closed += 1;
    });
    fireCloseMenus(target);
    expect(closed).toBe(1);
  });

  it("does not close anything until the host fires it", () => {
    const target: HostGlobal = {};
    let closed = 0;
    installHostCloseMenusHook(target, () => {
      closed += 1;
    });
    expect(closed).toBe(0);
  });
});
