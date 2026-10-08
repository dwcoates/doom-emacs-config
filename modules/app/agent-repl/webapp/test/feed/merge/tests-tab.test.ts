// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  FeedMergeTabTestsSchema,
  FeedMergeTestLogSchema,
  FeedMergeTestSuiteRunningSchema,
  FeedMergeTestSuiteSchema,
  type FeedMergeTestSuite,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { harness } from "../harness.js";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { PAINT_CLASS_NAMES } from "../../../src/vocab.js";
import {
  drawFeedMergeTabTests,
  drawFeedMergeTestLog,
  drawFeedMergeTestSpan,
  drawFeedMergeTestSuite,
  drawTestSuites,
} from "../../../src/feed/merge/tests-tab.js";
import { oneofArms } from "../../arms.js";
import STYLESHEET from "../../../src/styles.css?raw";
import { rulesOf } from "../../stylesheet.js";

/** A suite in STATE, carrying OUTPUT; a running one has reported nothing yet. */
function suite(
  name: string,
  state: "running" | "passed" | "failed",
  output: readonly { text: string; paintClass: string }[] = [],
): FeedMergeTestSuite {
  return create(FeedMergeTestSuiteSchema, {
    name,
    state:
      state === "running"
        ? { case: "running", value: { soFar: { case: "unreported", value: {} } } }
        : { case: state, value: {} },
    output: output.map((s) => ({ text: s.text, paintClass: s.paintClass })),
  });
}

/** A running suite whose tests have said SO_FAR. */
function runningSuite(soFar: "unreported" | "passing" | "failing"): FeedMergeTestSuite {
  return create(FeedMergeTestSuiteSchema, {
    name: "unit",
    state: { case: "running", value: { soFar: { case: soFar, value: {} } } },
  });
}

describe("drawFeedMergeTestSuite: the running dot says what the tests said so far", () => {
  it.each([
    ["unreported", "is-unreported"],
    ["passing", "is-passing"],
    ["failing", "is-failing"],
  ] as const)("draws a %s suite's dot as %s", (soFar, tone) => {
    const glyph = drawFeedMergeTestSuite(runningSuite(soFar)).querySelector(".merge-suite-glyph");
    expect(glyph?.classList.contains(tone)).toBe(true);
  });

  it("stamps the running arm on the suite", () => {
    expect(drawFeedMergeTestSuite(runningSuite("failing")).getAttribute("data-so-far")).toBe("failing");
  });

  it("holds to the schema: every so-far arm is drawn", () => {
    expect([...oneofArms(FeedMergeTestSuiteRunningSchema, "so_far")].sort()).toEqual([
      "failing",
      "passing",
      "unreported",
    ]);
  });

  it("refuses a running suite whose so-far oneof is unset", () => {
    const bare = create(FeedMergeTestSuiteSchema, { name: "unit", state: { case: "running", value: {} } });
    expect(() => drawFeedMergeTestSuite(bare)).toThrow(MalformedView);
  });
});

describe("drawFeedMergeTestSuite: the counts column", () => {
  /** A passed suite carrying COUNTS. */
  function counted(counts?: { passed: number; failed: number; total: number }): HTMLElement {
    return drawFeedMergeTestSuite(
      create(FeedMergeTestSuiteSchema, { name: "unit", state: { case: "passed", value: {} }, counts }),
    );
  }

  it("draws passed/failed/total", () => {
    expect(counted({ passed: 3, failed: 1, total: 12 }).querySelector(".merge-suite-counts")?.textContent).toBe(
      "3/1/12",
    );
  });

  it("paints the passed figure green", () => {
    const el = counted({ passed: 3, failed: 1, total: 12 }).querySelector(".merge-suite-counts .is-passed");
    expect(el?.textContent).toBe("3");
  });

  it("paints the failed figure red", () => {
    const el = counted({ passed: 3, failed: 1, total: 12 }).querySelector(".merge-suite-counts .is-failed");
    expect(el?.textContent).toBe("1");
  });

  it("draws no counts while the daemon knows none", () => {
    expect(counted().querySelector(".merge-suite-counts")).toBeNull();
  });

  it("sits in the suite's head line, after the name", () => {
    const head = counted({ passed: 0, failed: 0, total: 2 }).querySelector(".merge-suite-head");
    expect(head?.lastElementChild?.classList.contains("merge-suite-counts")).toBe(true);
  });
});

describe("the stylesheet's suite colors", () => {
  /** The declarations of the rule whose selector is exactly SELECTOR. */
  function declarationsOf(selector: string): string {
    return rulesOf(STYLESHEET)
      .filter((rule) => rule.selectors.includes(selector))
      .map((rule) => rule.declarations)
      .join(";");
  }

  it.each([
    [".merge-suite-glyph.is-passing", "var(--ok)"],
    [".merge-suite-glyph.is-failing", "var(--err)"],
    [".merge-suite-glyph.is-unreported", "var(--muted)"],
    [".merge-suite-counts .is-passed", "var(--ok)"],
    [".merge-suite-counts .is-failed", "var(--err)"],
  ])("colors %s with %s", (selector, color) => {
    expect(declarationsOf(selector)).toContain(`color: ${color}`);
  });

  it("paints no suite dot purple", () => {
    const purple = rulesOf(STYLESHEET).filter(
      (rule) => rule.selectors.some((one) => one.startsWith(".merge-suite-glyph")) && rule.declarations.includes("--blocked"),
    );
    expect(purple).toEqual([]);
  });
});

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

describe("drawFeedMergeTabTests: the suites, then the round's log", () => {
  /** A tests tab carrying SUITES and, when given, LOG. */
  function testsTab(log?: { token: string; label: string }) {
    return create(FeedMergeTabTestsSchema, {
      state: { case: "live", value: {} },
      suites: [{ name: "unit", state: { case: "running", value: { soFar: { case: "unreported", value: {} } } }, output: [] }],
      ...(log === undefined ? {} : { log: { token: { value: log.token }, label: { text: log.label } } }),
    });
  }

  it("draws the suites first", () => {
    const parts = drawFeedMergeTabTests(testsTab(), harness().ctx, "FeedMergeTabTests");
    expect(parts[0]?.classList.contains("merge-suites")).toBe(true);
  });

  it("draws no log line before the daemon has written the log", () => {
    expect(drawFeedMergeTabTests(testsTab(), harness().ctx, "FeedMergeTabTests")).toHaveLength(1);
  });

  it("draws the log line after the suites once the log is written", () => {
    const parts = drawFeedMergeTabTests(testsTab({ token: "t", label: "~/x.log" }), harness().ctx, "FeedMergeTabTests");
    expect(parts[1]?.classList.contains("merge-test-log")).toBe(true);
  });

  it("draws the log's link with the daemon's label", () => {
    const parts = drawFeedMergeTabTests(testsTab({ token: "t", label: "~/x.log" }), harness().ctx, "FeedMergeTabTests");
    expect(parts[1]?.querySelector("[data-merge-test-log]")?.textContent).toBe("~/x.log");
  });

  it("introduces the link as the log", () => {
    const parts = drawFeedMergeTabTests(testsTab({ token: "t", label: "~/x.log" }), harness().ctx, "FeedMergeTabTests");
    expect(parts[1]?.textContent).toBe("log: ~/x.log");
  });
});

describe("drawFeedMergeTestLog", () => {
  it("refuses a log with no token", () => {
    const u = create(FeedMergeTestLogSchema, { label: { text: "~/x.log" } });
    expect(() => drawFeedMergeTestLog(u, harness().ctx, "log")).toThrow(MalformedView);
  });
});
