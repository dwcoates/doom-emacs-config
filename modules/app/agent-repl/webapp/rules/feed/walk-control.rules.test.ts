/**
 * THE FEED'S WALK GOES THROUGH walk-control.ts: a rule over
 * src/feed/feed-view.ts, comments stripped.
 */
import { describe, expect, it } from "vitest";
import feedViewSource from "../../src/feed/feed-view.ts?raw";
import { codeOf } from "../../test/source-text.js";

describe("the feed's walk control goes through walk-control.ts", () => {
  it("builds its control with drawWalkControl", () => {
    expect(codeOf(feedViewSource)).toMatch(/drawWalkControl\(\)/);
  });

  it("never sets the control's disabled state by hand", () => {
    expect(codeOf(feedViewSource)).not.toMatch(/loadMore\.disabled\s*=/);
  });

  it("never hand-rolls the control's class", () => {
    expect(codeOf(feedViewSource)).not.toMatch(/"feed-load-more"/);
  });
});

describe("every face of the feed's walk goes through createWalkFaces", () => {
  it("never watches a face with a trigger of its own", () => {
    expect(codeOf(feedViewSource)).not.toMatch(/createWalkTrigger\(/);
  });

  it("never sets a face's loading state by hand", () => {
    expect(codeOf(feedViewSource)).not.toMatch(/setWalkLoading\(/);
  });
});
