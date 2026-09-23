/**
 * EXPANDING A BUBBLE NEVER MOVES THE FEED — on the booted app.
 *
 * Owner rule, 2026-09-23: THE USER OWNS THE SCROLL. The caret used to scroll
 * the opened sub-feed into view for a reader holding a place, and to re-land
 * the tail for a reader following it; both were implicit moves outside the
 * closed set of scroll causes (scroll.ts `SCROLL_CAUSES`), and both are gone.
 * What remains is the follow a named cause started: a standing follow keeps
 * the tail when the grown content is reported, exactly as for any growth.
 *
 * WHAT IS ASSERTED HERE and not in the unit suite: that the caret of a bubble
 * built by the REAL mount, in the real shell, against a real sub-feed page,
 * leaves the page's own `#feed-scroll` where it was.
 *
 * THE GEOMETRY IS SCRIPTED, exactly as test/integration/feed-tail.integration
 * .test.ts scripts it and for the same reason: jsdom lays nothing out, so a
 * `scrollTop` that clamps into range on write and a panel with a real box are
 * facts the environment cannot supply and a browser always would.
 */
import { afterEach, describe, expect, it } from "vitest";

import { startHarness, type Harness } from "./harness";
import { fireResize } from "../resize-observer";
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
  //
  // A box scripted SHORT of its tail was put there by the READER, so their
  // input is dispatched first. The owner reads intent off the box's position
  // only once a real user input has reached it (`TailFollow.onInput`), because
  // the box moves that position too — a shrink clamps it down — and a bare
  // number cannot tell the two apart.
  if (scrollTop < CONTENT_HEIGHT - BOX_HEIGHT) box.dispatchEvent(new Event("wheel"));
  box.dispatchEvent(new Event("scroll"));
  return { top: () => top };
}

describe("the caret on the booted app", () => {
  it("does not scroll the opened sub-feed into view for a reader holding a place", async () => {
    // Arrange — 40px down, and the panel will hang from 250 in a 300px box.
    const view = await bootWithBubble(40, 250);
    // Act
    await harness.click('[data-feed-row="bubble"] [data-expand]');
    // Assert — the reader scrolls to what they opened themselves.
    expect(view.top()).toBe(40);
  });

  it("does not re-land the tail itself for a reader who was following it", async () => {
    // Arrange — parked at the tail: 1000 - 300 = 700.
    const view = await bootWithBubble(700, 250);
    // Act — the expansion grows the feed by the panel's 200px.
    await harness.click('[data-feed-row="bubble"] [data-expand]');
    // Assert — the caret wrote nothing.
    expect(view.top()).toBe(700);
  });

  it("keeps a standing follow at the tail once the grown content is reported", async () => {
    // Arrange — the first placement's follow stands, parked at 700.
    const view = await bootWithBubble(700, 250);
    await harness.click('[data-feed-row="bubble"] [data-expand]');
    // Act — the content's size change reaches the tail owner.
    fireResize(harness.shell.feed);
    // Assert — the tail of the GROWN feed, under the first placement's follow.
    expect(view.top()).toBe(900);
  });
});
