// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  FeedMergeTestCountsSchema,
  FeedMergeTestsRunOutputSchema,
  FeedMergeTestLogSchema,
  FeedMergeTestSuiteRunningSchema,
  FeedMergeTestSuiteSchema,
  type FeedMergeTestSuite,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { harness } from "../harness.js";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { PAINT_CLASS_NAMES } from "../../../src/vocab.js";
import {
  drawFeedMergeTestsRunOutput,
  drawFeedMergeTestLog,
  drawFeedMergeTestSpan,
  drawFeedMergeTestSuite,
  drawTestSuites,
  placeFeedMergeTestCounts,
  suitesInListOrder,
  updateFeedMergeTestsRunOutput,
  updateTestSuites,
} from "../../../src/feed/merge/tests-tab.js";
import TESTS_TAB from "../../../src/feed/merge/tests-tab.ts?raw";
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

  it("draws passed/total while no test has failed", () => {
    expect(counted({ passed: 3, failed: 0, total: 12 }).querySelector(".merge-suite-counts")?.textContent).toBe("3/12");
  });

  it("draws no red failed figure while no test has failed", () => {
    expect(counted({ passed: 3, failed: 0, total: 12 }).querySelector(".merge-suite-counts .is-failed")).toBeNull();
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
    [".merge-suite-glyph.is-passing", "var(--suite-running)"],
    [".merge-suite-glyph.is-failing", "var(--err)"],
    [".merge-suite-glyph.is-unreported", "var(--suite-running)"],
    [".merge-suite-glyph.is-succeeded", "var(--ok)"],
    [".merge-suite-glyph.is-failed", "var(--err)"],
    [".merge-suite-counts .is-passed", "var(--ok)"],
    [".merge-suite-counts .is-failed", "var(--err)"],
  ])("colors %s with %s", (selector, color) => {
    expect(declarationsOf(selector)).toContain(`color: ${color}`);
  });

  it("paints no running suite dot green", () => {
    const green = rulesOf(STYLESHEET).filter(
      (rule) =>
        rule.selectors.some((one) => one === ".merge-suite-glyph.is-unreported" || one === ".merge-suite-glyph.is-passing") &&
        rule.declarations.includes("--ok"),
    );
    expect(green).toEqual([]);
  });

  it("paints no suite dot purple", () => {
    const purple = rulesOf(STYLESHEET).filter(
      (rule) => rule.selectors.some((one) => one.startsWith(".merge-suite-glyph")) && rule.declarations.includes("--blocked"),
    );
    expect(purple).toEqual([]);
  });
});

describe("drawTestSuites: one block per suite, in list order", () => {
  it("draws every suite the tab carried, the running before the passed", () => {
    const el = drawTestSuites([suite("unit", "passed"), suite("e2e", "running")]);
    expect([...el.querySelectorAll(".merge-suite-name")].map((e) => e.textContent)).toEqual([
      "e2e",
      "unit",
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

describe("drawFeedMergeTestsRunOutput: the suites, then the run's log", () => {
  /** A run's output carrying SUITES and, when given, LOG. */
  function testsTab(log?: { token: string; label: string }) {
    return create(FeedMergeTestsRunOutputSchema, {
      suites: [{ name: "unit", state: { case: "running", value: { soFar: { case: "unreported", value: {} } } }, output: [] }],
      ...(log === undefined ? {} : { log: { token: { value: log.token }, label: { text: log.label } } }),
    });
  }

  it("draws the suites first", () => {
    const parts = drawFeedMergeTestsRunOutput(testsTab(), harness().ctx, "FeedMergeTestsRunOutput");
    expect(parts[0]?.classList.contains("merge-suites")).toBe(true);
  });

  it("draws no log line before the daemon has written the log", () => {
    expect(drawFeedMergeTestsRunOutput(testsTab(), harness().ctx, "FeedMergeTestsRunOutput")).toHaveLength(1);
  });

  it("draws the log line after the suites once the log is written", () => {
    const parts = drawFeedMergeTestsRunOutput(testsTab({ token: "t", label: "~/x.log" }), harness().ctx, "FeedMergeTestsRunOutput");
    expect(parts[1]?.classList.contains("merge-test-log")).toBe(true);
  });

  it("draws the log's link with the daemon's label", () => {
    const parts = drawFeedMergeTestsRunOutput(testsTab({ token: "t", label: "~/x.log" }), harness().ctx, "FeedMergeTestsRunOutput");
    expect(parts[1]?.querySelector("[data-merge-test-log]")?.textContent).toBe("~/x.log");
  });

  it("introduces the link as the log", () => {
    const parts = drawFeedMergeTestsRunOutput(testsTab({ token: "t", label: "~/x.log" }), harness().ctx, "FeedMergeTestsRunOutput");
    expect(parts[1]?.textContent).toBe("log: ~/x.log");
  });
});

describe("drawFeedMergeTestLog", () => {
  it("refuses a log with no token", () => {
    const u = create(FeedMergeTestLogSchema, { label: { text: "~/x.log" } });
    expect(() => drawFeedMergeTestLog(u, harness().ctx, "log")).toThrow(MalformedView);
  });
});

/** A running suite NAME whose tests have so far passed PASSED of TOTAL. */
function countedSuite(name: string, passed: number, total: number): FeedMergeTestSuite {
  return create(FeedMergeTestSuiteSchema, {
    name,
    state: { case: "running", value: { soFar: { case: "passing", value: {} } } },
    counts: { passed, failed: 0, total },
  });
}

/**
 * THE SUITES ARE DRAWN IN PLACE (owner request, 2026-10-10): a push of new
 * counts moves only the figures, and a running suite's breathing dot is never
 * a fresh element.
 */
describe("updateTestSuites", () => {
  it("keeps a suite's element and its live dot across a push of new counts", () => {
    // Arrange
    const el = drawTestSuites([countedSuite("unit", 1, 10)]);
    const suiteEl = el.querySelector(".merge-suite");
    const glyph = el.querySelector(".merge-suite-glyph");
    // Act
    updateTestSuites(el, [countedSuite("unit", 2, 10)]);
    // Assert
    expect([el.querySelector(".merge-suite"), el.querySelector(".merge-suite-glyph")]).toEqual([suiteEl, glyph]);
  });

  it("moves the counts a push carries", () => {
    const el = drawTestSuites([countedSuite("unit", 1, 10)]);
    updateTestSuites(el, [countedSuite("unit", 2, 10)]);
    expect(el.querySelector(".merge-suite-counts")?.textContent).toBe("2/10");
  });

  it("repaints the dot in place when the suite settles", () => {
    // Arrange
    const el = drawTestSuites([countedSuite("unit", 10, 10)]);
    const glyph = el.querySelector(".merge-suite-glyph");
    // Act
    updateTestSuites(el, [suite("unit", "passed")]);
    // Assert
    expect([el.querySelector(".merge-suite-glyph"), glyph?.className]).toEqual([glyph, "merge-suite-glyph is-succeeded"]);
  });

  it("draws a suite served for the first time in its place", () => {
    const el = drawTestSuites([countedSuite("unit", 1, 10)]);
    updateTestSuites(el, [countedSuite("unit", 1, 10), countedSuite("integration", 0, 5)]);
    expect([...el.querySelectorAll(".merge-suite")].map((s) => s.getAttribute("data-suite"))).toEqual(["unit", "integration"]);
  });

  it("drops a suite no longer served", () => {
    const el = drawTestSuites([countedSuite("unit", 1, 10), countedSuite("integration", 0, 5)]);
    updateTestSuites(el, [countedSuite("unit", 1, 10)]);
    expect(el.querySelectorAll(".merge-suite")).toHaveLength(1);
  });

  it("keeps the output box the reader may have scrolled, replacing only its spans", () => {
    // Arrange
    const el = drawTestSuites([suite("unit", "failed", [{ text: "FAIL a", paintClass: "" }])]);
    const pre = el.querySelector(".merge-suite-output");
    // Act
    updateTestSuites(el, [suite("unit", "failed", [{ text: "FAIL b", paintClass: "" }])]);
    // Assert
    expect([el.querySelector(".merge-suite-output"), pre?.textContent]).toEqual([pre, "FAIL b"]);
  });

  it("leaves unchanged output's spans as they stand", () => {
    // Arrange
    const el = drawTestSuites([suite("unit", "failed", [{ text: "FAIL a", paintClass: "" }])]);
    const span = el.querySelector(".merge-suite-output > *");
    // Act
    updateTestSuites(el, [suite("unit", "failed", [{ text: "FAIL a", paintClass: "" }])]);
    // Assert
    expect(el.querySelector(".merge-suite-output > *")).toBe(span);
  });
});

describe("updateFeedMergeTestsRunOutput", () => {
  it("keeps the log line while its link is unchanged", () => {
    // Arrange
    const ctx = harness().ctx;
    const tab = create(FeedMergeTestsRunOutputSchema, {
      suites: [countedSuite("unit", 1, 10)],
      log: create(FeedMergeTestLogSchema, { token: { value: "tok" }, label: { text: "the log" } }),
    });
    const host = document.createElement("div");
    host.append(...drawFeedMergeTestsRunOutput(tab, ctx, "p"));
    const logLine = host.querySelector(".merge-test-log");
    const next = create(FeedMergeTestsRunOutputSchema, { suites: [countedSuite("unit", 2, 10)], log: tab.log });
    // Act
    updateFeedMergeTestsRunOutput(host, next, tab, ctx, "p");
    // Assert
    expect(host.querySelector(".merge-test-log")).toBe(logLine);
  });

  it("draws the log line once the daemon has written it", () => {
    // Arrange
    const ctx = harness().ctx;
    const tab = create(FeedMergeTestsRunOutputSchema, { suites: [countedSuite("unit", 1, 10)] });
    const host = document.createElement("div");
    host.append(...drawFeedMergeTestsRunOutput(tab, ctx, "p"));
    const next = create(FeedMergeTestsRunOutputSchema, {
      suites: [countedSuite("unit", 1, 10)],
      log: create(FeedMergeTestLogSchema, { token: { value: "tok" }, label: { text: "the log" } }),
    });
    // Act
    updateFeedMergeTestsRunOutput(host, next, tab, ctx, "p");
    // Assert
    expect(host.querySelector(".merge-test-log")).not.toBeNull();
  });
});

/** The names of SUITES, in order. */
const namesOf = (suites: readonly FeedMergeTestSuite[]): string[] => suites.map((one) => one.name);

describe("suitesInListOrder: the failed lead, the passed trail", () => {
  it("lists the failed, then the running, then the passed", () => {
    const listed = suitesInListOrder([suite("p", "passed"), suite("r", "running"), suite("f", "failed")]);
    expect(namesOf(listed)).toEqual(["f", "r", "p"]);
  });

  it("keeps served order within a group", () => {
    const listed = suitesInListOrder([suite("f1", "failed"), suite("p", "passed"), suite("f2", "failed")]);
    expect(namesOf(listed)).toEqual(["f1", "f2", "p"]);
  });

  it("sorts a running suite by its state, whatever its tests said so far", () => {
    const failing = create(FeedMergeTestSuiteSchema, {
      name: "r",
      state: { case: "running", value: { soFar: { case: "failing", value: {} } } },
    });
    expect(namesOf(suitesInListOrder([failing, suite("f", "failed")]))).toEqual(["f", "r"]);
  });

  it("leaves the served list as it was", () => {
    const served = [suite("p", "passed"), suite("f", "failed")];
    suitesInListOrder(served);
    expect(namesOf(served)).toEqual(["p", "f"]);
  });

  it("refuses a suite whose state oneof is unset", () => {
    expect(() => suitesInListOrder([create(FeedMergeTestSuiteSchema, { name: "unit" })])).toThrow(MalformedView);
  });
});

describe("updateTestSuites: a suite whose state moved moves to its group", () => {
  it("moves a suite that failed to the top, keeping its element", () => {
    // Arrange
    const el = drawTestSuites([suite("a", "running"), suite("b", "running")]);
    const b = el.querySelector('[data-suite="b"]');
    // Act
    updateTestSuites(el, [suite("a", "running"), suite("b", "failed")]);
    // Assert
    expect([el.firstElementChild, b?.getAttribute("data-suite-state")]).toEqual([b, "failed"]);
  });

  it("moves a suite that passed below the still running", () => {
    const el = drawTestSuites([suite("a", "running"), suite("b", "running")]);
    updateTestSuites(el, [suite("a", "passed"), suite("b", "running")]);
    expect([...el.children].map((child) => child.getAttribute("data-suite"))).toEqual(["b", "a"]);
  });
});

/** Counts of PASSED, FAILED and TOTAL. */
function counts(passed: number, failed: number, total: number) {
  return create(FeedMergeTestCountsSchema, { passed, failed, total });
}

describe("placeFeedMergeTestCounts", () => {
  it("draws the counts once known", () => {
    const host = document.createElement("span");
    placeFeedMergeTestCounts(host, counts(1, 0, 2));
    expect(host.querySelector(".merge-suite-counts")?.textContent).toBe("1/2");
  });

  it("keeps the drawn counts while no figure moved", () => {
    // Arrange
    const host = document.createElement("span");
    placeFeedMergeTestCounts(host, counts(1, 0, 2));
    const drawn = host.querySelector(".merge-suite-counts");
    // Act
    placeFeedMergeTestCounts(host, counts(1, 0, 2));
    // Assert
    expect(host.querySelector(".merge-suite-counts")).toBe(drawn);
  });

  it("redraws the counts in their place when a figure moved", () => {
    const host = document.createElement("span");
    placeFeedMergeTestCounts(host, counts(1, 0, 2));
    placeFeedMergeTestCounts(host, counts(1, 1, 2));
    expect([host.children.length, host.textContent]).toEqual([1, "1/1/2"]);
  });

  it("drops the counts once none are known", () => {
    const host = document.createElement("span");
    placeFeedMergeTestCounts(host, counts(1, 0, 2));
    placeFeedMergeTestCounts(host, undefined);
    expect(host.querySelector(".merge-suite-counts")).toBeNull();
  });

  it("is the one placement of counts in tests-tab", () => {
    expect(TESTS_TAB.match(/:scope > \.merge-suite-counts/g)).toHaveLength(1);
  });
});
