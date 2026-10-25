/**
 * A BUBBLE'S COPY CONTROL SITS INSIDE ITS BUBBLE, THE SAME DISTANCE FROM THE
 * BUBBLE'S TOP AND RIGHT EDGES WHATEVER THE BUBBLE HOLDS, FOLDED OR OPENED, ON
 * THE FIRST LINE OF TEXT, IN WEBKIT.
 *
 * Owner requests: 2026-10-24, the control was often off the bubble's
 * right-hand side once a prompt was opened, and a folded prompt, which is
 * never the selected one, had none; 2026-10-25, the control sat on a fresh
 * line of its own at the top of a short prompt, where it should share the
 * first line of text, the bubble widening just enough for it, and a final
 * response's control should sit the same way. jsdom lays nothing out, so
 * this draws real prompt and final response bubbles over the real stylesheet
 * in headless WebKit and measures the control against the bubble's border box
 * and its text.
 */
import path from "node:path";
import { fileURLToPath } from "node:url";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import type { CopyGaps, PromptCase } from "./bubble-copy-page";
import { bundlePage } from "./bundle";
import { openFeedPage, type FeedPage } from "./feed-page";

const here = path.dirname(fileURLToPath(import.meta.url));

/** A paragraph long enough to wrap at the bubble's cap. */
const LONG = "the daemon agent is done with one contract question about a compaction concluded line ".repeat(12);

/** The prompts measured, by name. */
const PROMPTS: Record<string, PromptCase> = {
  "a short prompt, folded": { blocks: [{ text: "hi" }], expanded: false, selected: true },
  "a short prompt, opened": { blocks: [{ text: "hi" }], expanded: true, selected: true },
  "a long prompt, folded": { blocks: [{ text: LONG }], expanded: false, selected: true },
  "a long prompt, opened": { blocks: [{ text: LONG }], expanded: true, selected: true },
  "a reply, folded": { blocks: [{ quote: LONG }, { text: "and Rome?" }], expanded: false, selected: true },
  "a reply, opened": { blocks: [{ quote: LONG }, { text: "and Rome?" }], expanded: true, selected: true },
  "a short prompt, folded, not selected": { blocks: [{ text: "hi" }], expanded: false, selected: false },
  "a long prompt, folded, not selected": { blocks: [{ text: LONG }], expanded: false, selected: false },
  "a reply, folded, not selected": { blocks: [{ quote: LONG }, { text: "and Rome?" }], expanded: false, selected: false },
  "a one-sentence prompt": { blocks: [{ text: "please rename the footer's activity cell" }], expanded: false, selected: false },
};

/** The final responses measured, by name. */
const RESPONSES: Record<string, string> = {
  "a short final response": "Done.",
  "a one-sentence final response": "Renamed the footer's activity cell and its tests.",
  "a long final response": LONG,
};

/** The cases whose first line is short of the bubble's cap, so the bubble fits it and the control. */
const SHORT = [
  "a short prompt, folded",
  "a short prompt, opened",
  "a short prompt, folded, not selected",
  "a one-sentence prompt",
  "a short final response",
  "a one-sentence final response",
];

/** The cases whose first line reaches the bubble's cap, so it wraps short of the control. */
const AT_CAP = ["a long prompt, folded", "a long prompt, opened", "a long prompt, folded, not selected", "a long final response"];

describe("a bubble's copy control in WebKit", () => {
  let feed: FeedPage;
  const measured: Record<string, CopyGaps> = {};

  beforeAll(async () => {
    const script = await bundlePage(path.join(here, "bubble-copy-page.ts"), "bubbleCopyPage");
    feed = await openFeedPage({ width: 900, height: 700 });
    await feed.page.addScriptTag({ content: script });
    for (const [name, c] of Object.entries(PROMPTS)) {
      measured[name] = await feed.page.evaluate((arg) => window.bubbleCopy.measurePrompt(arg), c);
    }
    for (const [name, text] of Object.entries(RESPONSES)) {
      measured[name] = await feed.page.evaluate((arg) => window.bubbleCopy.measureResponse(arg), text);
    }
  });

  afterAll(async () => {
    await feed?.browser.close();
  });

  const promptReference = (): CopyGaps => measured["a short prompt, folded"];
  const responseReference = (): CopyGaps => measured["a short final response"];

  for (const name of [...Object.keys(PROMPTS), ...Object.keys(RESPONSES)]) {
    it(`is shown: ${name}`, () => {
      expect(measured[name].shown).toBe(true);
    });

    it(`is drawn whole inside the bubble: ${name}`, () => {
      expect(measured[name].inside).toBe(true);
    });

    it(`leaves the text short of it: ${name}`, () => {
      expect(measured[name].clearOfText).toBe(true);
    });
  }

  for (const name of Object.keys(PROMPTS)) {
    it(`keeps the one gap from the bubble's top and right edges: ${name}`, () => {
      const { right, top } = measured[name];
      expect([right, top]).toEqual([promptReference().right, promptReference().top]);
    });
  }

  for (const name of Object.keys(RESPONSES)) {
    it(`keeps the one gap from the bubble's top and right edges: ${name}`, () => {
      const { right, top } = measured[name];
      expect([right, top]).toEqual([responseReference().right, responseReference().top]);
    });
  }

  for (const name of SHORT) {
    it(`shares the first line of text, never a line of its own: ${name}`, () => {
      expect([measured[name].firstLineBeside, measured[name].besideLines]).toEqual([true, 1]);
    });

    it(`widens the bubble just enough for it: ${name}`, () => {
      // The text gap (0.6em) and the body's own inset are the whole slack.
      expect(measured[name].slack).toBeLessThan(24);
    });
  }

  for (const name of AT_CAP) {
    it(`wraps a first line at the bubble's cap short of it, beside it: ${name}`, () => {
      expect([measured[name].firstLineBeside, measured[name].besideLines]).toEqual([true, 1]);
    });
  }

  it("keeps the right gap equal to the top gap", () => {
    expect(promptReference().right).toBe(promptReference().top);
  });
});
