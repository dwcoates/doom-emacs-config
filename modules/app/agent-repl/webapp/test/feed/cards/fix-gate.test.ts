// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { FeedFixGateSchema } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { FIX_GATE_WORDS, attemptText, drawFeedFixGate } from "../../../src/feed/cards/fix-gate.js";
import { mergeBadge } from "../../../src/feed/merge/merge.js";
import { HEAD_CLOCK_CLASS } from "../../../src/feed/cards/tool-head.js";
import { harness, mergeRow, rowContext } from "../harness.js";
import { oneofArms } from "../../arms.js";

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(1_000_000);
});
afterEach(() => {
  vi.useRealTimers();
});

type State = MessageInitShape<typeof FeedFixGateSchema>["state"];

/** A gate attempt in STATE, with one running suite. */
function gate(state: State): ReturnType<typeof create<typeof FeedFixGateSchema>> {
  return create(FeedFixGateSchema, {
    head: { label: { text: "testing the fix" }, attempt: 2, maxAttempts: 3 },
    state,
    output: { suites: [{ name: "go", state: { case: "running", value: { soFar: { case: "unreported", value: {} } } }, output: [] }] },
  });
}

/** Draw MSG against a scripted context. */
function draw(msg: ReturnType<typeof gate>): HTMLElement {
  const h = harness();
  return drawFeedFixGate(msg, rowContext(h.ctx, mergeRow("g1")));
}

describe("drawFeedFixGate: the merge-styled head", () => {
  it("draws the daemon's label verbatim, then the attempt of its maximum", () => {
    const el = draw(gate({ case: "live", value: { startedAtMs: 940_000n } }));
    expect(el.querySelector(".merge-label")?.textContent).toBe("testing the fix");
    expect(el.querySelector(".fix-gate-attempt")?.textContent).toBe("attempt 2 of 3");
  });

  it("spells an attempt as 'attempt N of M'", () => {
    expect(attemptText(1, 3)).toBe("attempt 1 of 3");
  });

  it("draws its badge as the merge head's badge, so the two never drift", () => {
    const el = draw(gate({ case: "live", value: { startedAtMs: 940_000n } }));
    expect(el.querySelector(".merge-badge")?.outerHTML).toBe(mergeBadge(FIX_GATE_WORDS.live, "is-live").outerHTML);
  });
});

describe("drawFeedFixGate: every state arm", () => {
  it("says 'testing' and counts up while the suites run", () => {
    const el = draw(gate({ case: "live", value: { startedAtMs: 940_000n } }));
    expect([el.getAttribute("data-state"), el.querySelector(".merge-badge")?.textContent]).toEqual(["live", "testing"]);
    expect(el.querySelector(`.${HEAD_CLOCK_CLASS}`)?.textContent).toBe("1m");
  });

  it("says 'passed' and holds the span that ran once every suite passed", () => {
    const el = draw(
      gate({ case: "settled", value: { startedAtMs: 0n, endedAtMs: 125_000n, outcome: { case: "succeeded", value: {} } } }),
    );
    expect([el.getAttribute("data-state"), el.querySelector(".merge-badge")?.textContent]).toEqual(["succeeded", "passed"]);
    expect(el.querySelector(`.${HEAD_CLOCK_CLASS}`)?.textContent).toBe("2m 5s");
  });

  it("says 'failed' with the daemon's one-line account", () => {
    const el = draw(
      gate({
        case: "settled",
        value: { startedAtMs: 0n, endedAtMs: 5_000n, outcome: { case: "failed", value: { summary: "2 suites failed" } } },
      }),
    );
    expect([el.getAttribute("data-state"), el.querySelector(".fix-gate-summary")?.textContent]).toEqual([
      "failed",
      "2 suites failed",
    ]);
  });

  it("refuses an attempt whose state is unset", () => {
    expect(() => draw(gate(undefined))).toThrow(MalformedView);
  });

  it("holds to the schema: every state arm of FeedFixGate is drawn", () => {
    expect([...oneofArms(FeedFixGateSchema, "state")].sort()).toEqual(["live", "settled"]);
  });
});

describe("drawFeedFixGate: the body is the tests tab's run output alone", () => {
  it("draws the run's suites and no tab strip", () => {
    const el = draw(gate({ case: "live", value: { startedAtMs: 940_000n } }));
    expect(el.querySelector(".fix-gate-run .merge-suites")).not.toBeNull();
    expect(el.querySelector(".merge-tab-strip, .merge-tabs")).toBeNull();
  });
});
