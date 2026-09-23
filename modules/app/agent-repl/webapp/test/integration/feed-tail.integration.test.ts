/**
 * THE FEED'S TAIL AGAINST THE DOCKED FOOTER — the occlusion, on the booted app.
 *
 * The progress footer is a flex sibling laid out BELOW `#feed-scroll`
 * (index.html), so the scroll box's height is the window's minus whatever the
 * footer currently occupies. The space is reserved by the layout and needs no
 * padding; what went stale was the POSITION. A footer that appeared or grew
 * AFTER the render that parked the tail shrank the box under a `scrollTop`
 * nobody moved, and the last bubble was left that many pixels below the fold,
 * clipped by the strip's top edge. It survived some runs and not others purely
 * on the order the footer's first push and the feed's tail render landed in.
 *
 * `mountFeed` now subscribes the scroll box's own size to the tail owner
 * (`observeScrollBox`), so these boot the WHOLE app and drive a real footer
 * push through the fake daemon.
 *
 * TWO ENVIRONMENT FACTS SHAPE HOW THAT IS DRIVEN, and neither is a seam in the
 * app: jsdom lays nothing out, so the scroll box's geometry is scripted here
 * (as a browser's would be, `scrollTop` clamping into range on write), and the
 * box-size notification is delivered by the harness's `ResizeObserver`
 * substitution. `fireResize` throws when nothing is watching the element, so
 * the fire is itself the check that the mount subscribed to `#feed-scroll`.
 */
import { afterEach, describe, expect, it } from "vitest";

import { bootColdOnce, startHarness, type Harness } from "./harness";
import { fireResize } from "../resize-observer";
import { WORKSPACE_ID, footerView } from "./fixtures";

let harness: Harness;

bootColdOnce();

afterEach(async () => {
  await harness?.stop();
});

/** A scroll box the reader is parked at the tail of: 700 + 300 = 1000. */
function scriptGeometry(box: HTMLElement) {
  let scrollHeight = 1000;
  let clientHeight = 300;
  let scrollTop = 700;
  Object.defineProperties(box, {
    scrollHeight: { get: () => scrollHeight },
    clientHeight: { get: () => clientHeight },
    scrollTop: {
      get: () => scrollTop,
      set: (next: number) => {
        scrollTop = Math.max(0, Math.min(next, scrollHeight - clientHeight));
      },
    },
  });
  return {
    /** The footer settling or growing by PX: the box loses exactly that. */
    footerTakes: (px: number) => {
      clientHeight -= px;
    },
    /**
     * The rows collapsing to PX -- a compaction replacing a history with a
     * summary, a card settling smaller. The box's position rides the clamp
     * down with the range, which is the movement nobody made.
     */
    contentShrinksTo: (px: number) => {
      scrollHeight = px;
      scrollTop = Math.max(0, Math.min(scrollTop, scrollHeight - clientHeight));
    },
    /**
     * The rows growing by PX with the viewport unchanged: a bubble the wire
     * pushed unfolded finishing its own fetch and painting a page into its
     * panel, a deferred card settling, a highlighted block relaying out.
     */
    contentGrows: (px: number) => {
      scrollHeight += px;
      scrollTop = Math.max(0, Math.min(scrollTop, scrollHeight - clientHeight));
    },
    top: () => scrollTop,
  };
}

describe("the docked footer and the feed's tail", () => {
  it("re-lands the tail when a footer push takes height from the scroll box", async () => {
    // Arrange — booted with a bare footer, the reader following the tail.
    harness = await startHarness({
      arrange: (fake) => fake.setFooter(WORKSPACE_ID, footerView({ status: "idle" })),
    });
    const geometry = scriptGeometry(harness.shell.feedScroll);
    // The scroll event the browser dispatches for the reader's own arrival at
    // the tail: the owner reconciles against the position it last knows about,
    // and under jsdom the box only acquires one when this test scripts it.
    harness.shell.feedScroll.dispatchEvent(new Event("scroll"));
    // Act — the footer changes state and grows; the box loses that height.
    harness.fake.setFooter(WORKSPACE_ID, footerView({ status: "thinking" }));
    await harness.settle();
    geometry.footerTakes(48);
    fireResize(harness.shell.feedScroll);
    // Assert — the tail is the bottom of the SHRUNKEN viewport, so the last
    // bubble sits above the strip instead of behind it.
    expect(geometry.top()).toBe(748);
  });

  it("leaves a reader who scrolled up where they are when the footer grows", async () => {
    // Arrange
    harness = await startHarness({
      arrange: (fake) => fake.setFooter(WORKSPACE_ID, footerView({ status: "idle" })),
    });
    const geometry = scriptGeometry(harness.shell.feedScroll);
    // The reader's own input reaches the box before the movement it causes,
    // which is what makes the movement theirs rather than the box's clamp.
    harness.shell.feedScroll.dispatchEvent(new Event("wheel"));
    harness.shell.feedScroll.scrollTop = 200;
    harness.shell.feedScroll.dispatchEvent(new Event("scroll"));
    // Act
    harness.fake.setFooter(WORKSPACE_ID, footerView({ status: "thinking" }));
    await harness.settle();
    geometry.footerTakes(48);
    fireResize(harness.shell.feedScroll);
    // Assert — the reader owns the position; only they may leave it.
    expect(geometry.top()).toBe(200);
  });

  /**
   * THE ORDER THAT SURVIVED SUBSCRIBING THE BOX. Watching only `#feed-scroll`
   * hears the footer and nothing about what is inside the box, so growth that
   * lands LAST — after the footer settled and after the render that parked —
   * leaves the tail exactly as far below the fold as the footer once did. It
   * is the last thing to land precisely under load, where the fetch behind an
   * unfolded bubble's own page is slowest, which is why the hibernated tab's
   * `awaitTailClearsFooter` failed there and nowhere else.
   */
  it("re-lands the tail when the content grows after the footer settled", async () => {
    // Arrange — booted, following the tail, footer already settled.
    harness = await startHarness({
      arrange: (fake) => fake.setFooter(WORKSPACE_ID, footerView({ status: "idle" })),
    });
    const geometry = scriptGeometry(harness.shell.feedScroll);
    harness.shell.feedScroll.dispatchEvent(new Event("scroll"));
    harness.fake.setFooter(WORKSPACE_ID, footerView({ status: "thinking" }));
    await harness.settle();
    geometry.footerTakes(48);
    fireResize(harness.shell.feedScroll);
    // Act — the revived turn's unfolded bubble paints its page, last.
    geometry.contentGrows(120);
    fireResize(harness.shell.feed);
    // Assert — 1120 of content under a 252 viewport: the tail is 868.
    expect(geometry.top()).toBe(868);
  });

  /**
   * THE OTHER ORDER, which the box half already covered and which must stay
   * covered: a footer that settles after the content grew is the case the
   * subscription was first written for, and the content half must not have
   * displaced it.
   */
  it("re-lands the tail when the footer settles after the content grew", async () => {
    // Arrange
    harness = await startHarness({
      arrange: (fake) => fake.setFooter(WORKSPACE_ID, footerView({ status: "idle" })),
    });
    const geometry = scriptGeometry(harness.shell.feedScroll);
    harness.shell.feedScroll.dispatchEvent(new Event("scroll"));
    // Act — the content grows first, then the footer takes its height.
    geometry.contentGrows(120);
    fireResize(harness.shell.feed);
    harness.fake.setFooter(WORKSPACE_ID, footerView({ status: "thinking" }));
    await harness.settle();
    geometry.footerTakes(48);
    fireResize(harness.shell.feedScroll);
    // Assert — the same 868, whichever order the two landed in.
    expect(geometry.top()).toBe(868);
  });

  it("leaves a reader who scrolled up where they are when the content grows", async () => {
    // Arrange
    harness = await startHarness({
      arrange: (fake) => fake.setFooter(WORKSPACE_ID, footerView({ status: "idle" })),
    });
    const geometry = scriptGeometry(harness.shell.feedScroll);
    harness.shell.feedScroll.dispatchEvent(new Event("wheel"));
    harness.shell.feedScroll.scrollTop = 200;
    harness.shell.feedScroll.dispatchEvent(new Event("scroll"));
    // Act
    geometry.contentGrows(120);
    fireResize(harness.shell.feed);
    // Assert — growth below the reader is not a reason to move them.
    expect(geometry.top()).toBe(200);
  });

  /**
   * THE CLAMP, ON THE BOOTED APP. The feed shrinking under a parked box drags
   * `scrollTop` down with it, and the drag reads exactly like a gesture upward.
   * Measured in the hibernated tab's playbook under load: the box sat at 52 --
   * the reachable extent one turn earlier -- while 400px of new rows arrived
   * beneath it, because the reconcile that saw the movement arrived only after
   * the content had regrown. No input ever reached the box, so nothing that
   * happened there was the reader.
   */
  it("re-lands the tail after a shrink's clamp that no reader caused", async () => {
    // Arrange — following the tail of a feed that then shrinks under the box.
    harness = await startHarness({
      arrange: (fake) => fake.setFooter(WORKSPACE_ID, footerView({ status: "idle" })),
    });
    const geometry = scriptGeometry(harness.shell.feedScroll);
    harness.shell.feedScroll.dispatchEvent(new Event("scroll"));
    // Act — the shrink clamps the box down, the content regrows, and only then
    // does anything reconcile.
    geometry.contentShrinksTo(352);
    harness.shell.feedScroll.dispatchEvent(new Event("scroll"));
    geometry.contentGrows(747);
    fireResize(harness.shell.feed);
    // Assert — 1099 of content under a 300 viewport: the tail is 799.
    expect(geometry.top()).toBe(799);
  });
});
