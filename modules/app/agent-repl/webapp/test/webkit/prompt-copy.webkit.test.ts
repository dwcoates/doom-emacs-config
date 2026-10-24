/**
 * A PROMPT'S COPY CONTROL SITS INSIDE ITS BUBBLE, THE SAME DISTANCE FROM THE
 * BUBBLE'S TOP AND RIGHT EDGES WHATEVER THE PROMPT HOLDS, FOLDED OR OPENED, IN
 * WEBKIT (owner request, 2026-10-24: the control was often off the bubble's
 * right-hand side once a prompt was opened). It is drawn on every prompt,
 * selected or not (owner request, 2026-10-24: a folded prompt, which is never
 * the selected one, had none). jsdom lays nothing out, so this
 * draws real prompt bubbles over the real stylesheet in headless WebKit and
 * measures the control against the bubble's border box.
 */
import path from "node:path";
import { fileURLToPath } from "node:url";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import type { CopyGaps, PromptCase } from "./prompt-copy-page";
import { bundlePage } from "./bundle";
import { openFeedPage, type FeedPage } from "./feed-page";

const here = path.dirname(fileURLToPath(import.meta.url));

/** A paragraph long enough to wrap at the bubble's cap. */
const LONG = "the daemon agent is done with one contract question about a compaction concluded line ".repeat(12);

/** The prompts measured, by name. */
const CASES: Record<string, PromptCase> = {
  "a short prompt, folded": { blocks: [{ text: "hi" }], expanded: false, selected: true },
  "a short prompt, opened": { blocks: [{ text: "hi" }], expanded: true, selected: true },
  "a long prompt, folded": { blocks: [{ text: LONG }], expanded: false, selected: true },
  "a long prompt, opened": { blocks: [{ text: LONG }], expanded: true, selected: true },
  "a reply, folded": { blocks: [{ quote: LONG }, { text: "and Rome?" }], expanded: false, selected: true },
  "a reply, opened": { blocks: [{ quote: LONG }, { text: "and Rome?" }], expanded: true, selected: true },
  "a short prompt, folded, not selected": { blocks: [{ text: "hi" }], expanded: false, selected: false },
  "a long prompt, folded, not selected": { blocks: [{ text: LONG }], expanded: false, selected: false },
  "a reply, folded, not selected": { blocks: [{ quote: LONG }, { text: "and Rome?" }], expanded: false, selected: false },
};

describe("a prompt's copy control in WebKit", () => {
  let feed: FeedPage;
  const measured: Record<string, CopyGaps> = {};

  beforeAll(async () => {
    const script = await bundlePage(path.join(here, "prompt-copy-page.ts"), "promptCopyPage");
    feed = await openFeedPage({ width: 900, height: 700 });
    await feed.page.addScriptTag({ content: script });
    for (const [name, c] of Object.entries(CASES)) {
      measured[name] = await feed.page.evaluate((arg) => window.promptCopy.measure(arg), c);
    }
  });

  afterAll(async () => {
    await feed?.browser.close();
  });

  const reference = (): CopyGaps => measured["a short prompt, folded"];

  for (const name of Object.keys(CASES)) {
    it(`is shown: ${name}`, () => {
      expect(measured[name].shown).toBe(true);
    });

    it(`is drawn whole inside the bubble: ${name}`, () => {
      expect(measured[name].inside).toBe(true);
    });

    it(`leaves the prompt's text short of it: ${name}`, () => {
      expect(measured[name].clearOfText).toBe(true);
    });

    it(`keeps the one gap from the bubble's top and right edges: ${name}`, () => {
      const { right, top } = measured[name];
      expect([right, top]).toEqual([reference().right, reference().top]);
    });
  }

  it("wraps a long prompt's first line beside it, so the clearance is measured", () => {
    expect(measured["a long prompt, folded"].besideLines).toBe(1);
  });

  it("keeps the right gap equal to the top gap", () => {
    expect(reference().right).toBe(reference().top);
  });
});
