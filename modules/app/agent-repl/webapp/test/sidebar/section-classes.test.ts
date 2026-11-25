import { describe, expect, it } from "vitest";
import {
  BACKLOG_SECTION_CLASS,
  MERGED_SECTION_CLASS,
  MERGE_QUEUE_SECTION_CLASS,
  OPEN_SECTION_CLASS,
} from "../../src/sidebar/section-classes.js";
import { FOCUS_FOLDED_SECTIONS } from "../../src/sidebar/section-fold.js";

describe("the sections' classes", () => {
  it("are the four the stylesheet draws the sections with", () => {
    expect([OPEN_SECTION_CLASS, BACKLOG_SECTION_CLASS, MERGED_SECTION_CLASS, MERGE_QUEUE_SECTION_CLASS]).toEqual([
      "open-section",
      "backlog-section",
      "merged-section",
      "merge-queue-section",
    ]);
  });

  it("are defined where a module reads them at its top level", () => {
    expect(FOCUS_FOLDED_SECTIONS.map((shape) => shape.className)).toEqual([MERGED_SECTION_CLASS, BACKLOG_SECTION_CLASS]);
  });
});
