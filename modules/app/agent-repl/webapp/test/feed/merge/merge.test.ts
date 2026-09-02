// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  FeedMergeSchema,
  FeedMergeErrorSchema,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { drawFeedMerge, drawFeedMergeGlyph } from "../../../src/feed/merge/merge.js";
import { harness, mergeRow, rowContext } from "../harness.js";
import { oneofArms } from "../../arms.js";
import { mergeHead } from "./fixtures.js";

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(1_000_000);
});
afterEach(() => {
  vi.useRealTimers();
});

/** Draw a head against a scripted context. */
function draw(msg: ReturnType<typeof mergeHead>): HTMLElement {
  const h = harness();
  return drawFeedMerge(msg, rowContext(h.ctx, mergeRow("m1")));
}

describe("drawFeedMerge: the head's constant props", () => {
  it("draws the branch line the daemon resolved, verbatim", () => {
    const el = draw(mergeHead({ case: "update" }, { label: "DWC/fix → master" }));
    expect(el.querySelector(".merge-label")?.textContent).toBe("DWC/fix → master");
  });

  it("draws the ⇄ glyph for the vocabulary's merge icon", () => {
    const el = draw(mergeHead({ case: "update" }));
    expect(el.querySelector(".merge-glyph")?.textContent).toBe("⇄");
  });
});

describe("drawFeedMerge: every result arm", () => {
  it("says 'merging' while the run is live", () => {
    const el = draw(mergeHead({ case: "update" }));
    expect(el.getAttribute("data-state")).toBe("update");
    expect(el.querySelector(".merge-badge")?.textContent).toBe("merging");
  });

  it("says 'merged' and the commit when it landed", () => {
    const el = draw(mergeHead({ case: "success", endedAtMs: 61_000n, commit: "a1b2c3d" }));
    expect(el.getAttribute("data-state")).toBe("success");
    expect(el.querySelector(".merge-badge")?.textContent).toBe("merged");
    expect(el.querySelector(".merge-commit")?.textContent).toBe("a1b2c3d");
  });

  it("draws the failure summary the daemon composed", () => {
    const el = draw(
      mergeHead({ case: "failed", endedAtMs: 61_000n, summary: "tests never passed" }),
    );
    expect(el.getAttribute("data-state")).toBe("failed");
    expect(el.querySelector(".merge-summary")?.textContent).toBe("tests never passed");
  });

  it("says 'abandoned' for a run taken off the queue", () => {
    const el = draw(mergeHead({ case: "abandoned", endedAtMs: 61_000n, summary: "dropped" }));
    expect(el.getAttribute("data-state")).toBe("abandoned");
    expect(el.querySelector(".merge-badge")?.textContent).toBe("abandoned");
  });

  it("draws the abandonment summary the daemon composed", () => {
    const el = draw(
      mergeHead({ case: "abandoned", endedAtMs: 61_000n, summary: "the user dropped it" }),
    );
    expect(el.querySelector(".merge-summary")?.textContent).toBe("the user dropped it");
  });

  it("holds to the schema: every result arm of FeedMerge is drawn", () => {
    expect([...oneofArms(FeedMergeSchema, "result")].sort()).toEqual(
      ["error", "success", "update"].sort(),
    );
  });

  it("holds to the schema: every reason arm of FeedMergeError is drawn", () => {
    expect([...oneofArms(FeedMergeErrorSchema, "reason")].sort()).toEqual(
      ["abandoned", "failed"].sort(),
    );
  });
});

describe("drawFeedMerge: the clock", () => {
  it("counts up from the enqueue instant while live", () => {
    const el = draw(mergeHead({ case: "update" }, { startedAtMs: 1_000_000n - 65_000n }));
    expect(el.querySelector(".merge-clock")?.textContent).toBe("1m 5s");
  });

  it("keeps ticking a live head as the shared clock advances", () => {
    const el = draw(mergeHead({ case: "update" }, { startedAtMs: 1_000_000n - 65_000n }));
    vi.advanceTimersByTime(2000);
    expect(el.querySelector(".merge-clock")?.textContent).toBe("1m 7s");
  });

  it("reads the nearest second when a tick samples just short of one", () => {
    // Arrange + Act: the live clock's start does not share the ticker's phase.
    const el = draw(mergeHead({ case: "update" }, { startedAtMs: 1_000_000n - 4920n }));
    // Assert: five real seconds waited reads 5s, not the lagging 4s.
    expect(el.querySelector(".merge-clock")?.textContent).toBe("5s");
  });

  it("shows the span that ran, stopped, once it settled", () => {
    const el = draw(mergeHead({ case: "success", endedAtMs: 65_000n, commit: "c" }));
    vi.advanceTimersByTime(5000);
    expect(el.querySelector(".merge-clock")?.textContent).toBe("1m 5s");
  });
});

describe("drawFeedMerge: malformed views", () => {
  it("refuses a head whose result oneof is unset", () => {
    const msg = create(FeedMergeSchema, {
      head: {
        glyph: { icon: "merge" },
        label: { text: "x" },
        runtime: { startedAtMs: 0n },
        fold: { folded: true },
      },
    });
    expect(() => draw(msg)).toThrow(MalformedView);
  });

  it("refuses a head with no head message at all", () => {
    const msg = create(FeedMergeSchema, { result: { case: "update", value: {} } });
    expect(() => draw(msg)).toThrow(MalformedView);
  });

  it("refuses an error whose reason oneof is unset", () => {
    const msg = create(FeedMergeSchema, {
      head: {
        glyph: { icon: "merge" },
        label: { text: "x" },
        runtime: { startedAtMs: 0n },
        fold: { folded: true },
      },
      result: { case: "error", value: { endedAtMs: 1n } },
    });
    expect(() => draw(msg)).toThrow(MalformedView);
  });
});

describe("drawFeedMergeGlyph: an unknown name is decoration, never an error", () => {
  it("draws the generic glyph rather than throwing", () => {
    const el = drawFeedMergeGlyph({ $typeName: "frontend.v1.FeedMergeGlyph", icon: "rebase" });
    expect(el.textContent).toBe("⇄");
    expect(el.getAttribute("data-glyph")).toBe("rebase");
  });
});
