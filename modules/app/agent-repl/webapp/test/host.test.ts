import { describe, expect, it } from "vitest";
import { HostGlobal, TAIL_HOOK, installHostTailHook } from "../src/host.js";
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
