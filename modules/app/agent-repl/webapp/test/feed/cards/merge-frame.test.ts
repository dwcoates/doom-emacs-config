// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FeedFixGateSchema } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { drawMergeFrame, type MergeFrameHead, type MergeFrameState } from "../../../src/feed/cards/merge-frame.js";
import { FIX_GATE_WORDS, attemptText, drawFeedFixGate } from "../../../src/feed/cards/fix-gate.js";
import { mergeBadge } from "../../../src/feed/merge/merge.js";
import { HEAD_CLOCK_CLASS } from "../../../src/feed/cards/tool-head.js";
import { harness, mergeRow, rowContext } from "../harness.js";

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(1_000_000);
});
afterEach(() => {
  vi.useRealTimers();
});

const WORDS = { live: "working", succeeded: "done", failed: "broke" } as const;

/** A head of kind "probe" whose own prop is one span. */
function head(): MergeFrameHead {
  const prop = document.createElement("span");
  prop.className = "probe-prop";
  prop.textContent = "extra";
  return { kind: "probe", label: "probing", props: [prop], words: WORDS };
}

/** Draw a frame in STATE. */
function draw(state: MergeFrameState): HTMLElement {
  return drawMergeFrame(head(), state, rowContext(harness().ctx, mergeRow("p1")), "Probe.state");
}

describe("drawMergeFrame: the head line", () => {
  it("draws the merge glyph, the label, then the card's own props", () => {
    const el = draw({ case: "live", value: { startedAtMs: 940_000n } as never });
    const props = el.querySelector(".merge-head-props");
    expect([...(props?.children ?? [])].map((c) => c.className)).toEqual(["merge-glyph", "merge-label", "probe-prop"]);
    expect(el.querySelector(".merge-label")?.textContent).toBe("probing");
  });

  it("classes the card and its head by the card's kind", () => {
    const el = draw({ case: "live", value: { startedAtMs: 940_000n } as never });
    expect([el.className, el.firstElementChild?.className]).toEqual(["tool-card probe", "merge-head probe-head"]);
  });
});

describe("drawMergeFrame: every state", () => {
  it("says the live word and counts up from the start", () => {
    const el = draw({ case: "live", value: { startedAtMs: 940_000n } as never });
    expect([el.getAttribute("data-state"), el.querySelector(".merge-badge")?.outerHTML]).toEqual([
      "live",
      mergeBadge("working", "is-live").outerHTML,
    ]);
    expect(el.querySelector(`.${HEAD_CLOCK_CLASS}`)?.textContent).toBe("1m");
  });

  it("says the succeeded word and holds the span that ran", () => {
    const el = draw({
      case: "settled",
      value: { startedAtMs: 0n, endedAtMs: 125_000n, outcome: { case: "succeeded", value: {} } } as never,
    });
    expect([el.getAttribute("data-state"), el.querySelector(".merge-badge")?.textContent]).toEqual(["succeeded", "done"]);
    expect(el.querySelector(`.${HEAD_CLOCK_CLASS}`)?.textContent).toBe("2m 5s");
  });

  it("says the failed word with the daemon's account", () => {
    const el = draw({
      case: "settled",
      value: { startedAtMs: 0n, endedAtMs: 5_000n, outcome: { case: "failed", value: { summary: "it broke" } } } as never,
    });
    expect([el.getAttribute("data-state"), el.querySelector(".probe-summary")?.textContent]).toEqual(["failed", "it broke"]);
  });

  it("draws no account for a failure that carries none", () => {
    const el = draw({
      case: "settled",
      value: { startedAtMs: 0n, endedAtMs: 5_000n, outcome: { case: "failed", value: { summary: "" } } } as never,
    });
    expect(el.querySelector(".probe-summary")).toBeNull();
  });

  it("refuses an unset state", () => {
    expect(() => draw({ case: undefined })).toThrow(MalformedView);
  });

  it("refuses a settled state with no outcome", () => {
    expect(() =>
      draw({ case: "settled", value: { startedAtMs: 0n, endedAtMs: 1n, outcome: { case: undefined } } as never }),
    ).toThrow(MalformedView);
  });
});

describe("drawMergeFrame: the framed cards share it", () => {
  it("draws the fix gate's head, so the two never drift", () => {
    const msg = create(FeedFixGateSchema, {
      head: { label: { text: "testing the fix" }, attempt: 2, maxAttempts: 3 },
      state: { case: "live", value: { startedAtMs: 940_000n } },
      output: { suites: [] },
    });
    const rc = rowContext(harness().ctx, mergeRow("g1"));
    const attempt = document.createElement("span");
    attempt.className = "fix-gate-attempt";
    attempt.textContent = attemptText(2, 3);
    const framed = drawMergeFrame(
      { kind: "fix-gate", label: "testing the fix", props: [attempt], words: FIX_GATE_WORDS },
      msg.state,
      rc,
      "FeedFixGate.state",
    );
    expect(drawFeedFixGate(msg, rc).firstElementChild?.outerHTML).toBe(framed.firstElementChild?.outerHTML);
  });
});
