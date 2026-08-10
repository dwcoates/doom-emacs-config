import { describe, expect, it } from "vitest";
import {
  CLOSE_MENUS_HOOK,
  HostGlobal,
  RECOVER_HOOK,
  TAIL_HOOK,
  TEXT_SCALE_BASE_PX,
  TEXT_SCALE_HOOK,
  TEXT_SCALE_MAX,
  TEXT_SCALE_MIN,
  TextScaleRoot,
  clampTextScale,
  installHostCloseMenusHook,
  installHostRecoverHook,
  installHostTailHook,
  installHostTextScaleHook,
  textScalePx,
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

/** A fresh root whose only observable is the font size the hook writes. */
const freshRoot = (): TextScaleRoot => ({ style: { fontSize: "" } });

/** Fire the planted text-scale hook the way an Emacs host script does. */
const fireTextScale = (target: HostGlobal, arg: unknown): number =>
  (target[TEXT_SCALE_HOOK] as (arg: unknown) => number)(arg);

describe("clampTextScale", () => {
  it("passes a mid-range scale through unchanged", () => {
    expect(clampTextScale(1.2)).toBe(1.2);
  });

  it("floors a scale below the minimum", () => {
    expect(clampTextScale(TEXT_SCALE_MIN - 1)).toBe(TEXT_SCALE_MIN);
  });

  it("caps a scale above the maximum", () => {
    expect(clampTextScale(TEXT_SCALE_MAX + 1)).toBe(TEXT_SCALE_MAX);
  });
});

describe("installHostTextScaleHook", () => {
  it("plants the hook under the name frontend.el calls", () => {
    const target: HostGlobal = {};
    installHostTextScaleHook(target, freshRoot());
    expect(typeof target[TEXT_SCALE_HOOK]).toBe("function");
  });

  it("leaves the root font untouched until the host fires it", () => {
    const root = freshRoot();
    installHostTextScaleHook({}, root);
    expect(root.style.fontSize).toBe("");
  });

  it("grows the root font by a positive delta off the base", () => {
    const target: HostGlobal = {};
    const root = freshRoot();
    installHostTextScaleHook(target, root);
    fireTextScale(target, 0.5);
    expect(root.style.fontSize).toBe(`${textScalePx(1.5)}px`);
  });

  it("shrinks the root font by a negative delta off the base", () => {
    const target: HostGlobal = {};
    const root = freshRoot();
    installHostTextScaleHook(target, root);
    fireTextScale(target, -0.25);
    expect(root.style.fontSize).toBe(`${textScalePx(0.75)}px`);
  });

  it("accumulates successive deltas rather than replacing them", () => {
    const target: HostGlobal = {};
    const root = freshRoot();
    installHostTextScaleHook(target, root);
    fireTextScale(target, 0.1);
    fireTextScale(target, 0.1);
    expect(root.style.fontSize).toBe(`${textScalePx(1.2)}px`);
  });

  it("rounds binary-float dust out of the emitted px string", () => {
    const target: HostGlobal = {};
    const root = freshRoot();
    installHostTextScaleHook(target, root);
    // 1.4 * 16 lands as 22.400000000000002 in raw float; the emitted
    // string must carry the rounded value, never that dust.
    fireTextScale(target, 0.4);
    expect(root.style.fontSize).toBe("22.4px");
  });

  it("returns the new scale so the host can read where it landed", () => {
    const target: HostGlobal = {};
    installHostTextScaleHook(target, freshRoot());
    expect(fireTextScale(target, 0.3)).toBeCloseTo(1.3);
  });

  it("floors the accumulated scale at the minimum", () => {
    const target: HostGlobal = {};
    const root = freshRoot();
    installHostTextScaleHook(target, root);
    fireTextScale(target, -100);
    expect(root.style.fontSize).toBe(`${textScalePx(TEXT_SCALE_MIN)}px`);
  });

  it("caps the accumulated scale at the maximum", () => {
    const target: HostGlobal = {};
    const root = freshRoot();
    installHostTextScaleHook(target, root);
    fireTextScale(target, 100);
    expect(root.style.fontSize).toBe(`${textScalePx(TEXT_SCALE_MAX)}px`);
  });

  it("restores the base size when fired with reset", () => {
    const target: HostGlobal = {};
    const root = freshRoot();
    installHostTextScaleHook(target, root);
    fireTextScale(target, 0.5);
    fireTextScale(target, "reset");
    expect(root.style.fontSize).toBe(`${TEXT_SCALE_BASE_PX}px`);
  });

  it("ignores a non-finite argument, leaving the scale where it was", () => {
    const target: HostGlobal = {};
    const root = freshRoot();
    installHostTextScaleHook(target, root);
    fireTextScale(target, 0.3);
    fireTextScale(target, "banana");
    expect(root.style.fontSize).toBe(`${textScalePx(1.3)}px`);
  });
});

describe("installHostRecoverHook", () => {
  it("plants the hook under the name webview-recovery.el calls", () => {
    const target: HostGlobal = {};
    installHostRecoverHook(target, () => {});
    expect(typeof target[RECOVER_HOOK]).toBe("function");
  });

  it("delegates to the page's repair path with the reason the host named", () => {
    const target: HostGlobal = {};
    const reasons: string[] = [];
    installHostRecoverHook(target, (reason) => reasons.push(reason));
    (target[RECOVER_HOOK] as (reason: unknown) => void)("host_link_up");
    expect(reasons).toEqual(["host_link_up"]);
  });

  it("names a default reason when the host fires it with no argument", () => {
    const target: HostGlobal = {};
    const reasons: string[] = [];
    installHostRecoverHook(target, (reason) => reasons.push(reason));
    (target[RECOVER_HOOK] as () => void)();
    expect(reasons).toEqual(["host_recover"]);
  });

  it("repairs once per host call rather than coalescing repeats", () => {
    const target: HostGlobal = {};
    const reasons: string[] = [];
    installHostRecoverHook(target, (reason) => reasons.push(reason));
    const hook = target[RECOVER_HOOK] as (reason: unknown) => void;
    hook("a");
    hook("b");
    expect(reasons).toEqual(["a", "b"]);
  });
});
