// @vitest-environment jsdom
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { FeedMergeQueueSchema } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { drawFeedMerge } from "../../../src/feed/merge/merge.js";
import { drawPausedMark, PAUSED_MARK_CLASS } from "../../../src/feed/merge/paused.js";
import { drawFeedMergeQueue } from "../../../src/feed/merge/queue.js";
import { harness, mergeRow, rowContext } from "../harness.js";
import { mergeHead, TAB_STARTED_AT_MS } from "./fixtures.js";

/** A mark's shape: its children's classes and hidden-ness, words aside. */
function shapeOf(el: Element | null): [string, string | null][] {
  return [...(el?.children ?? [])].map((c) => [c.className, c.getAttribute("aria-hidden")]);
}

describe("drawPausedMark: the one paused mark", () => {
  it("carries the shared class beside the place's own", () => {
    const el = drawPausedMark("merge-queue-paused", "queue paused");
    expect([el.classList.contains(PAUSED_MARK_CLASS), el.classList.contains("merge-queue-paused")]).toEqual([true, true]);
  });

  it("draws a hidden glyph, then the word", () => {
    const el = drawPausedMark("x", "paused");
    expect(
      [...el.children].map((c) => [c.className, c.getAttribute("aria-hidden"), c.textContent]),
    ).toEqual([
      ["merge-paused-glyph", "true", ""],
      ["merge-paused-word", null, "paused"],
    ]);
  });

  it("is the shape the head's pill and the queue tab's banner both draw", () => {
    const h = harness();
    const rc = rowContext(h.ctx, mergeRow("m1"));
    const head = drawFeedMerge(mergeHead({ case: "update", stage: "waiting", paused: true }), rc);
    const queue = drawFeedMergeQueue(
      create(FeedMergeQueueSchema, {
        paused: {},
        current: {
          workspace: { ref: { id: "mine", dir: "/w/mine" } },
          label: { text: "mine" },
          target: { text: "doom", dir: "/src/doom" },
          status: { case: "waiting", value: { stageEnteredAtMs: TAB_STARTED_AT_MS } },
        },
      }),
      rc,
    );
    const want = shapeOf(drawPausedMark("x", "paused"));
    expect([
      shapeOf(head.querySelector(`.${PAUSED_MARK_CLASS}`)),
      shapeOf(queue.querySelector(`.${PAUSED_MARK_CLASS}`)),
    ]).toEqual([want, want]);
  });
});
