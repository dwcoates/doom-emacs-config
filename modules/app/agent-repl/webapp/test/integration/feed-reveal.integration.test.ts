/**
 * EXPANDING A BUBBLE REVEALS WHAT IT EXPANDS — on the booted app.
 *
 * The defect, measured at a caret click: `below=208 scrollTop=40`. A fold opens
 * BELOW the fold and growth moves nothing on its own, so the sub-feed the
 * reader had just asked for unrolled entirely off the bottom of the screen and
 * the click looked like it had done nothing but swap a glyph. `TailFollow`
 * already owned the feed's position and `release()` had been written for
 * exactly this reader — one who deliberately opened content to read — and
 * nothing in production had ever called it.
 *
 * WHAT IS ASSERTED HERE and not in the unit suite: that the caret of a bubble
 * built by the REAL mount, in the real shell, against a real sub-feed page,
 * moves the page's own `#feed-scroll`. The unit suite pins the arithmetic and
 * the caret's two cases in isolation; a mount that never handed the bubble the
 * scroll owner would pass every one of those and fail these.
 *
 * THE GEOMETRY IS SCRIPTED, exactly as test/integration/feed-tail.integration
 * .test.ts scripts it and for the same reason: jsdom lays nothing out, so a
 * `scrollTop` that clamps into range on write and a panel with a real box are
 * facts the environment cannot supply and a browser always would.
 */
import { afterEach, describe, expect, it } from "vitest";

import { startHarness, type Harness } from "./harness";
import { ROOT_FEED } from "./fake-daemon";
import {
  WORKSPACE_ID,
  activityRow,
  feedId,
  feedPageSuccess,
  responseRow,
  subagentUnit,
} from "./fixtures";

let harness: Harness;

afterEach(async () => {
  await harness?.stop();
});

/** The scroll box's visible height, the panel's, and the feed's when shut. */
const BOX_HEIGHT = 300;
const PANEL_HEIGHT = 200;
const CONTENT_HEIGHT = 1000;

/** A DOMRect, as far as the reveal reads one. */
function rect(top: number, height: number): DOMRect {
  return {
    top,
    height,
    bottom: top + height,
    left: 0,
    right: 0,
    width: 0,
    x: 0,
    y: top,
    toJSON: () => ({}),
  };
}

/**
 * Boot the app on one subagent bubble whose sub-feed has rows of its own, and
 * script both boxes the reveal compares.
 *
 * The panel is scripted BEFORE the click, which it can be because a collapsed
 * bubble already carries its panel — hidden, and so with the empty box a hidden
 * element really has.
 */
async function bootWithBubble(scrollTop: number, panelTop: number) {
  harness = await startHarness({
    arrange: (fake) => {
      fake.setPage(
        WORKSPACE_ID,
        ROOT_FEED,
        feedPageSuccess([activityRow(subagentUnit("live"), { id: feedId("bubble") })]),
      );
      fake.setPage(WORKSPACE_ID, "bubble", feedPageSuccess([responseRow("success", "the subagent's own answer")]));
    },
  });
  const box = harness.shell.feedScroll;
  const panel = harness.row("bubble")?.querySelector<HTMLElement>("[data-subfeed]");
  if (panel === undefined || panel === null) throw new Error("the bubble drew no sub-feed panel");
  let top = scrollTop;
  const shown = (): boolean => !panel.hidden;
  Object.defineProperties(box, {
    scrollHeight: { get: () => CONTENT_HEIGHT + (shown() ? PANEL_HEIGHT : 0) },
    clientHeight: { get: () => BOX_HEIGHT },
    scrollTop: {
      get: () => top,
      set: (next: number) => {
        top = Math.max(0, Math.min(next, box.scrollHeight - BOX_HEIGHT));
      },
    },
  });
  box.getBoundingClientRect = () => rect(0, BOX_HEIGHT);
  panel.getBoundingClientRect = () =>
    panel.hidden ? rect(panelTop, 0) : rect(panelTop, PANEL_HEIGHT);
  // The scroll event the browser dispatches for the reader's own arrival: the
  // tail owner reconciles against the position it last knows about, and under
  // jsdom the box only acquires one when a test scripts it.
  box.dispatchEvent(new Event("scroll"));
  return { top: () => top };
}

describe("the caret's reveal on the booted app", () => {
  it("scrolls the opened sub-feed into view for a reader holding a place", async () => {
    // Arrange — 40px down, and the panel will hang from 250 in a 300px box.
    const view = await bootWithBubble(40, 250);
    // Act
    await harness.click('[data-feed-row="bubble"] [data-expand]');
    // Assert — moved by exactly the 150px overhang, so the bubble's head stays
    // where it was and every pixel of the panel that fits is on screen.
    expect(view.top()).toBe(190);
  });

  it("re-lands the tail for a reader who was following it", async () => {
    // Arrange — parked at the tail: 1000 - 300 = 700.
    const view = await bootWithBubble(700, 250);
    // Act — the expansion grows the feed by the panel's 200px.
    await harness.click('[data-feed-row="bubble"] [data-expand]');
    // Assert — the tail of the GROWN feed.
    expect(view.top()).toBe(900);
  });
});
