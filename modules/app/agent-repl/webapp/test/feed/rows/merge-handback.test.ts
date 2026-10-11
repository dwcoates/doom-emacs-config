// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FeedMergeHandbackBadgeSchema } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { MERGE_HANDBACK_BADGE_CLASS, drawFeedMergeHandback } from "../../../src/feed/rows/merge-handback.js";
import { MalformedView } from "../../../src/rpc/malformed.js";

/** A merge hand-back badge carrying LABEL. */
function badge(label: string) {
  return create(FeedMergeHandbackBadgeSchema, { label: { text: label } });
}

describe("drawFeedMergeHandback", () => {
  it("draws the daemon's label verbatim in the shared pill under its own class", () => {
    const el = drawFeedMergeHandback(badge("merge of fix-flaky landed"));
    expect([el.textContent, el.classList.contains("badge"), el.classList.contains(MERGE_HANDBACK_BADGE_CLASS)]).toEqual([
      "merge of fix-flaky landed",
      true,
      true,
    ]);
  });

  it("refuses a badge with no label rather than drawing an empty pill", () => {
    expect(() => drawFeedMergeHandback(create(FeedMergeHandbackBadgeSchema, {}))).toThrow(MalformedView);
  });
});
