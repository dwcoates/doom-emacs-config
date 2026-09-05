// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  FeedMergeTestSuiteSchema,
  type FeedMergeTestSuite,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { PAINT_CLASS_NAMES } from "../../../src/vocab.js";
import {
  drawFeedMergeTestSpan,
  drawFeedMergeTestSuite,
  drawTestSuites,
} from "../../../src/feed/merge/tests-tab.js";
import { oneofArms } from "../../arms.js";

/** A suite in STATE, carrying OUTPUT. */
function suite(
  name: string,
  state: "running" | "passed" | "failed",
  output: readonly { text: string; paintClass: string }[] = [],
): FeedMergeTestSuite {
  return create(FeedMergeTestSuiteSchema, {
    name,
    state: { case: state, value: {} },
    output: output.map((s) => ({ text: s.text, paintClass: s.paintClass })),
  });
}

describe("drawTestSuites: one block per suite, in served order", () => {
  it("draws every suite the tab carried", () => {
    const el = drawTestSuites([suite("unit", "passed"), suite("e2e", "running")]);
    expect([...el.querySelectorAll(".merge-suite-name")].map((e) => e.textContent)).toEqual([
      "unit",
      "e2e",
    ]);
  });

  it("delimits the blocks through the ONE shared list rule", () => {
    expect(drawTestSuites([]).classList.contains("list-rows")).toBe(true);
  });
});

describe("drawFeedMergeTestSuite: every state arm", () => {
  it.each([
    ["running", "●"],
    ["passed", "✓"],
    ["failed", "✗"],
  ] as const)("draws %s with its own glyph", (state, glyph) => {
    const el = drawFeedMergeTestSuite(suite("unit", state));
    expect(el.getAttribute("data-suite-state")).toBe(state);
    expect(el.querySelector(".merge-suite-glyph")?.textContent).toBe(glyph);
  });

  it("holds to the schema: every state arm of FeedMergeTestSuite is drawn", () => {
    expect([...oneofArms(FeedMergeTestSuiteSchema, "state")].sort()).toEqual([
      "failed",
      "passed",
      "running",
    ]);
  });

  it("draws the suite's name verbatim", () => {
    expect(
      drawFeedMergeTestSuite(suite("go test ./daemon/...", "passed")).querySelector(
        ".merge-suite-name",
      )?.textContent,
    ).toBe("go test ./daemon/...");
  });

  it("refuses a suite whose state oneof is unset", () => {
    const bare = create(FeedMergeTestSuiteSchema, { name: "unit" });
    expect(() => drawFeedMergeTestSuite(bare)).toThrow(MalformedView);
  });

  it("draws no output block at all for a suite that carried none", () => {
    expect(drawFeedMergeTestSuite(suite("unit", "running")).querySelector("pre")).toBeNull();
  });

  it("draws the output as spans inside one block", () => {
    const el = drawFeedMergeTestSuite(
      suite("unit", "failed", [
        { text: "FAIL ", paintClass: "removed" },
        { text: "TestReconnect", paintClass: "" },
      ]),
    );
    expect(el.querySelector("pre")?.textContent).toBe("FAIL TestReconnect");
  });
});

describe("drawFeedMergeTestSpan: the daemon paints, the client styles", () => {
  it("maps a vocabulary name to its paint class", () => {
    const name = PAINT_CLASS_NAMES[0];
    const el = drawFeedMergeTestSpan({
      $typeName: "frontend.v1.FeedMergeTestSpan",
      text: "x",
      paintClass: name,
    });
    expect(el.className).toBe(`paint-${name}`);
  });

  it("draws an empty class as plain text — the one spelling of unstyled", () => {
    const el = drawFeedMergeTestSpan({
      $typeName: "frontend.v1.FeedMergeTestSpan",
      text: "x",
      paintClass: "",
    });
    expect(el.className).toBe("");
  });

  it("draws a class this build never heard of unstyled rather than throwing", () => {
    const el = drawFeedMergeTestSpan({
      $typeName: "frontend.v1.FeedMergeTestSpan",
      text: "still readable",
      paintClass: "brand-new",
    });
    expect(el.className).toBe("");
    expect(el.textContent).toBe("still readable");
  });
});

describe("a suite state this build cannot draw is a refusal, never a default", () => {
  it("refuses a state arm a newer daemon set, quoting its name", () => {
    const newer = suite("unit", "running");
    (newer as { state: unknown }).state = { case: "skipped", value: {} };

    let thrown: unknown;
    try {
      drawFeedMergeTestSuite(newer);
    } catch (err) {
      thrown = err;
    }

    expect(thrown).toBeInstanceOf(MalformedView);
    expect((thrown as MalformedView).path).toBe("FeedMergeTestSuite.state");
    expect((thrown as MalformedView).detail).toBe(
      "arm 'skipped' is not one this build can draw",
    );
  });
});
