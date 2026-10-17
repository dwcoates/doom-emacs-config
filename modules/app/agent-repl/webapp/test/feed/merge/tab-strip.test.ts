// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  FeedMergeTabLabelSchema,
  FeedMergeTabSchema,
  FeedMergeTabSettledSchema,
  FeedRowSchema,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { createTicker } from "../../../src/clock.js";
import { MalformedView } from "../../../src/rpc/malformed.js";
import {
  AGENTIC_KINDS,
  PARENT_KINDS,
  RESOLVED_KINDS,
  SECTIONED_KINDS,
  VIEWED_KINDS,
  autoSelectedTab,
  childTabsOf,
  drawFeedMergeTab,
  drawFeedMergeTabLabel,
  drawTabDuration,
  mergeTabIdsOf,
  readMergeTab,
  updateFeedMergeTab,
} from "../../../src/feed/merge/tab-strip.js";
import { oneofArms } from "../../arms.js";
import { TAB_STARTED_AT_MS, id, tabRow } from "./fixtures.js";
import { orderFor } from "../../feed-order.js";
import { cascadedValue, installStylesheet } from "../../stylesheet.js";

/** The shared clock the badges tick on, stepped by each test's fake timers. */
let TICKER = createTicker(1000);

beforeEach(() => {
  vi.useFakeTimers();
  TICKER = createTicker(1000);
  // Four seconds after every fixture tab's work began.
  vi.setSystemTime(Number(TAB_STARTED_AT_MS) + 4_000);
});
afterEach(() => {
  vi.useRealTimers();
});

describe("childTabsOf: a strip is the sub-feed's tab rows it holds, in served order", () => {
  it("takes only the tabs parented to the strip's tab", () => {
    const tabs = childTabsOf(
      [
        tabRow("t-rebase", { kind: "rebasing", state: "live" }),
        tabRow("c1", { kind: "rebaseCommit", state: "live", parent: "t-rebase" }),
        tabRow("t-run", { kind: "testsRun", state: "live", parent: "t-tests" }),
      ],
      "t-rebase",
    );
    expect(tabs.map((t) => t.id)).toEqual(["c1"]);
  });

  it("takes only the unparented tabs for the bubble's own strip", () => {
    const tabs = childTabsOf(
      [
        tabRow("t-rebase", { kind: "rebasing", state: "live" }),
        tabRow("c1", { kind: "rebaseCommit", state: "live", parent: "t-rebase" }),
      ],
      undefined,
    );
    expect(tabs.map((t) => t.id)).toEqual(["t-rebase"]);
  });

  it("names every tab row at every level in mergeTabIdsOf", () => {
    const ids = mergeTabIdsOf([
      tabRow("t-rebase", { kind: "rebasing", state: "live" }),
      tabRow("c1", { kind: "rebaseCommit", state: "live", parent: "t-rebase" }),
      create(FeedRowSchema, { id: id("r1"), order: orderFor("r1"), row: { case: "userPrompt", value: {} } }),
    ]);
    expect([...ids].sort()).toEqual(["c1", "t-rebase"]);
  });

  it("keeps the served order rather than sorting by kind", () => {
    const tabs = childTabsOf([
      tabRow("t1", { kind: "tests", state: "settled", outcome: "failed" }),
      tabRow("t2", { kind: "rebasing", state: "live" }),
    ], undefined);
    expect(tabs.map((t) => t.kind)).toEqual(["tests", "rebasing"]);
  });

  it("ignores every row that is not a tab", () => {
    const tabs = childTabsOf([
      create(FeedRowSchema, { id: id("r1"), order: orderFor("r1"), row: { case: "userPrompt", value: {} } }),
      tabRow("t1", { kind: "tests", state: "live" }),
    ], undefined);
    expect(tabs.map((t) => t.id)).toEqual(["t1"]);
  });
});

describe("readMergeTab: the arms are read from the contract", () => {
  it("holds to the schema: every kind is resolved, agentic, a parent, sectioned, or viewed", () => {
    expect([...RESOLVED_KINDS, ...AGENTIC_KINDS, ...PARENT_KINDS, ...SECTIONED_KINDS, ...VIEWED_KINDS].sort()).toEqual(
      [...oneofArms(FeedMergeTabSchema, "kind")].sort(),
    );
  });

  it.each(FeedMergeTabSchema.oneofs.find((o) => o.name === "kind")?.fields.map((f) => [f.localName, f]) ?? [])(
    "holds to the schema: the %s tab is live or settled, since nothing parks, a tab an asking agent works in can wait on the user, and a commit can be pending",
    (kind, field) => {
      const message = field.fieldKind === "message" ? field.message : undefined;
      const waits = ["resolving", "rebasing", "tests", "testsRunFixing"].includes(kind);
      const want =
        kind === "rebaseCommit"
          ? ["live", "pending", "settled", "waitingOnUser"]
          : kind === "testsRun"
            ? ["live", "settled", "waitingForTurn"]
            : waits
              ? ["live", "settled", "waitingOnUser"]
              : ["live", "settled"];
      expect(message === undefined ? [] : [...oneofArms(message, "state")].sort()).toEqual(want);
    },
  );

  it("holds to the schema: a settled tab's outcome arms are succeeded and failed", () => {
    expect([...oneofArms(FeedMergeTabSettledSchema, "outcome")].sort()).toEqual([
      "failed",
      "succeeded",
    ]);
  });

  it("refuses a tab whose kind oneof is unset", () => {
    const row = create(FeedRowSchema, {
      id: id("t1"),
      order: orderFor("t1"),
      row: { case: "mergeTab", value: create(FeedMergeTabSchema, { label: { words: { case: "text", value: "x" } } }) },
    });
    expect(() => childTabsOf([row], undefined)).toThrow(MalformedView);
  });

  it("refuses a tab whose state oneof is unset", () => {
    const row = create(FeedRowSchema, {
      id: id("t1"),
      order: orderFor("t1"),
      row: {
        case: "mergeTab",
        value: create(FeedMergeTabSchema, {
          label: { words: { case: "text", value: "x" } },
          kind: { case: "tests", value: {} },
        }),
      },
    });
    expect(() => childTabsOf([row], undefined)).toThrow(MalformedView);
  });

  it("refuses a settled tab whose outcome oneof is unset", () => {
    const row = create(FeedRowSchema, {
      id: id("t1"),
      order: orderFor("t1"),
      row: {
        case: "mergeTab",
        value: create(FeedMergeTabSchema, {
          label: { words: { case: "text", value: "x" } },
          kind: { case: "tests", value: { state: { case: "settled", value: {} } } },
        }),
      },
    });
    expect(() => childTabsOf([row], undefined)).toThrow(MalformedView);
  });
});

describe("drawFeedMergeTabLabel: rounds beyond the first", () => {
  it("decorates a second round with its number", () => {
    const el = drawFeedMergeTabLabel(create(FeedMergeTabLabelSchema, { words: { case: "text", value: "tests" }, round: 2 }));
    expect(el.textContent).toBe("tests (2)");
  });

  it("draws round 1 undecorated, so an ordinary run never reads as a retry", () => {
    const el = drawFeedMergeTabLabel(create(FeedMergeTabLabelSchema, { words: { case: "text", value: "tests" }, round: 1 }));
    expect(el.textContent).toBe("tests");
  });
});

describe("drawFeedMergeTabLabel: a replayed commit", () => {
  const commitLabel = (round: number) =>
    drawFeedMergeTabLabel(
      create(FeedMergeTabLabelSchema, {
        words: { case: "commit", value: { shortSha: "a1b2c3d4e5f6", subject: "fix the loop" } },
        round,
      }),
    );

  it("draws the short hash and the subject as two parts", () => {
    const el = commitLabel(1);
    expect([el.querySelector(".merge-tab-sha")?.textContent, el.querySelector(".merge-tab-subject")?.textContent]).toEqual([
      "a1b2c3d4e5f6",
      "fix the loop",
    ]);
  });

  it("reads as the hash, the subject and a later replay's round", () => {
    expect(commitLabel(2).textContent).toBe("a1b2c3d4e5f6 fix the loop (2)");
  });

  it("refuses a label with no words", () => {
    expect(() => drawFeedMergeTabLabel(create(FeedMergeTabLabelSchema, { round: 1 }))).toThrow(MalformedView);
  });
});

describe("the badge's paint", () => {
  /** A drawn tab of SPEC, in the page with the stylesheet. */
  function painted(spec: Parameters<typeof tabRow>[1]) {
    const teardownCss = installStylesheet();
    const [tab] = childTabsOf([tabRow("t1", spec)], undefined);
    const el = drawFeedMergeTab(tab, { active: false, ticker: createTicker(1000) });
    document.body.append(el);
    return {
      el,
      teardown: () => {
        el.remove();
        teardownCss();
      },
    };
  }

  it("draws a commit's short hash at the duration's size, in a grey darker than the muted text", () => {
    const { el, teardown } = painted({ kind: "rebaseCommit", state: "live" });
    try {
      const sha = el.querySelector<HTMLElement>(".merge-tab-sha")!;
      expect([cascadedValue(sha, "font-size"), cascadedValue(sha, "color")]).toEqual([
        cascadedValue(el.querySelector<HTMLElement>(".merge-tab-duration")!, "font-size"),
        "color-mix(in srgb, var(--muted) 80%, black)",
      ]);
    } finally {
      teardown();
    }
  });

  it("leaves a commit's subject in the badge's own color", () => {
    const { el, teardown } = painted({ kind: "rebaseCommit", state: "live" });
    try {
      expect(cascadedValue(el.querySelector<HTMLElement>(".merge-tab-subject")!, "color")).toBe(cascadedValue(el, "color"));
    } finally {
      teardown();
    }
  });

  it.each([
    ["live", { kind: "tests", state: "live" }],
    ["in line for its turn", { kind: "testsRun", state: "waitingForTurn" }],
  ] as const)("draws a %s tab's dot at 70%% of the badge's size", (_name, spec) => {
    const { el, teardown } = painted(spec);
    try {
      expect(cascadedValue(el.querySelector<HTMLElement>(".merge-tab-glyph")!, "font-size")).toBe("0.7em");
    } finally {
      teardown();
    }
  });

  it.each([
    ["succeeded", { kind: "tests", state: "settled", outcome: "succeeded" }],
    ["failed", { kind: "tests", state: "settled", outcome: "failed" }],
  ] as const)("keeps a %s tab's mark at the badge's size", (_name, spec) => {
    const { el, teardown } = painted(spec);
    try {
      expect(cascadedValue(el.querySelector<HTMLElement>(".merge-tab-glyph")!, "font-size")).toBe(cascadedValue(el, "font-size"));
    } finally {
      teardown();
    }
  });
});

describe("drawFeedMergeTab: the badge says label, round and state — no counts but a run's tally", () => {
  it("marks the kind as the hook the suite targets", () => {
    const row = tabRow("t1", { kind: "updatingMain", state: "live" });
    const tab = readMergeTab(row, row.row.value as never);
    expect(drawFeedMergeTab(tab, { active: true, ticker: TICKER }).getAttribute("data-merge-tab")).toBe("updatingMain");
  });

  it("marks the state as the hook the suite targets", () => {
    const row = tabRow("t1", { kind: "committing", state: "live" });
    const tab = readMergeTab(row, row.row.value as never);
    expect(drawFeedMergeTab(tab, { active: true, ticker: TICKER }).getAttribute("data-tab-state")).toBe("live");
  });

  it("carries no count of any kind in its text", () => {
    const [tab] = childTabsOf([tabRow("t1", { kind: "tests", state: "live", label: "tests" })], undefined);
    const el = drawFeedMergeTab(tab, { active: false, ticker: TICKER });
    expect(el.textContent).toBe("tests4s●");
  });

  it("marks a settled tab with its outcome", () => {
    const [tab] = childTabsOf([
      tabRow("t1", { kind: "tests", state: "settled", outcome: "failed" }),
    ], undefined);
    const el = drawFeedMergeTab(tab, { active: false, ticker: TICKER });
    expect(el.getAttribute("data-tab-outcome")).toBe("failed");
    expect(el.querySelector(".merge-tab-glyph")?.textContent).toBe("✗");
  });

  it("marks the active tab so the strip shows where the reader is", () => {
    const [tab] = childTabsOf([tabRow("t1", { kind: "queue", state: "live" })], undefined);
    expect(drawFeedMergeTab(tab, { active: true, ticker: TICKER }).getAttribute("aria-selected")).toBe("true");
  });
});

describe("a run tab's tally", () => {
  const SUITES = (passed: number) => [
    { name: "unit", state: { case: "failed", value: {} }, counts: { passed, failed: 1, total: 7 } },
  ];
  const run = (passed: number) => {
    const [tab] = childTabsOf(
      [tabRow("t-run", { kind: "testsRun", state: "live", payload: { output: { suites: SUITES(passed) } } })],
      undefined,
    );
    return tab;
  };

  it("sits between the run's duration and its state glyph", () => {
    const el = drawFeedMergeTab(run(3), { active: false, ticker: TICKER });
    expect([...el.children].map((c) => c.className.split(" ")[0])).toEqual([
      "merge-tab-label",
      "merge-tab-duration",
      "merge-run-tally",
      "merge-tab-glyph",
    ]);
  });

  it("sums the run's suite counts", () => {
    expect(drawFeedMergeTab(run(3), { active: false, ticker: TICKER }).querySelector(".merge-run-tally")?.textContent).toBe(
      "3/1/7",
    );
  });

  it("moves in place on a push of new counts, keeping its place before the glyph", () => {
    const first = run(3);
    const el = drawFeedMergeTab(first, { active: false, ticker: TICKER });
    const tally = el.querySelector(".merge-run-tally");
    updateFeedMergeTab(el, run(5), first, { active: false, ticker: TICKER });
    expect([el.querySelector(".merge-run-tally") === tally, tally?.textContent, tally?.nextElementSibling?.className.split(" ")[0]]).toEqual([
      true,
      "5/1/7",
      "merge-tab-glyph",
    ]);
  });

  it("is drawn on no other kind of tab", () => {
    const [tab] = childTabsOf([tabRow("t1", { kind: "tests", state: "live" })], undefined);
    expect(drawFeedMergeTab(tab, { active: false, ticker: TICKER }).querySelector(".merge-run-tally")).toBeNull();
  });
});

describe("readMergeTab: the instants the tab's duration reads", () => {
  it("reads a live tab's start", () => {
    const [tab] = childTabsOf([tabRow("t1", { kind: "tests", state: "live", startedAtMs: 2_000n })], undefined);
    expect([tab.startedMs, tab.endedMs]).toEqual([2_000, undefined]);
  });

  it("reads a settled tab's start and end", () => {
    const [tab] = childTabsOf([
      tabRow("t1", { kind: "tests", state: "settled", startedAtMs: 2_000n, endedAtMs: 9_000n }),
    ], undefined);
    expect([tab.startedMs, tab.endedMs]).toEqual([2_000, 9_000]);
  });
});

describe("drawTabDuration: how long the tab has been in its state", () => {
  it("ticks a live tab from when its work began", () => {
    const [tab] = childTabsOf([tabRow("t1", { kind: "tests", state: "live" })], undefined);
    const el = drawTabDuration(tab, TICKER);
    vi.advanceTimersByTime(2_000);
    expect(el.textContent).toBe("6s");
  });

  it("shows a settled tab's run time, ended less started, and never ticks it", () => {
    const [tab] = childTabsOf([
      tabRow("t1", { kind: "tests", state: "settled", startedAtMs: 1_000n, endedAtMs: 91_000n }),
    ], undefined);
    const el = drawTabDuration(tab, TICKER);
    vi.advanceTimersByTime(5_000);
    expect(el.textContent).toBe("1m 30s");
  });

  it("sits beside the tab's label, ahead of its state glyph", () => {
    const [tab] = childTabsOf([tabRow("t1", { kind: "tests", state: "live" })], undefined);
    const el = drawFeedMergeTab(tab, { active: false, ticker: TICKER });
    expect([...el.children].map((c) => c.className)).toEqual([
      "merge-tab-label",
      "merge-tab-duration reserved-clock",
      "merge-tab-glyph is-live",
    ]);
  });
});

// A MERGE WAITING ON THE USER (owner request, 2026-10-08): the agentic tab
// whose agent has an ask open wears a question mark emoji while it stands.
describe("a tab waiting on the user", () => {
  it.each(["rebasing", "rebaseCommit", "tests", "testsRunFixing"] as const)("draws the %s tab's glyph as ❓", (kind) => {
    const [tab] = childTabsOf([tabRow("t1", { kind, state: "waitingOnUser" })], undefined);
    const glyph = drawFeedMergeTab(tab, { active: false, ticker: TICKER }).querySelector(".merge-tab-glyph");
    expect(glyph?.textContent).toBe("❓");
  });

  it("marks the state as the hook the suite targets", () => {
    const [tab] = childTabsOf([tabRow("t1", { kind: "rebaseCommit", state: "waitingOnUser" })], undefined);
    expect(drawFeedMergeTab(tab, { active: false, ticker: TICKER }).getAttribute("data-tab-state")).toBe(
      "waitingOnUser",
    );
  });

  it("keeps ticking from when the tab's work began", () => {
    const [tab] = childTabsOf([tabRow("t1", { kind: "rebaseCommit", state: "waitingOnUser" })], undefined);
    expect(drawTabDuration(tab, TICKER).textContent).toBe("4s");
  });

  it("is still where the merge is when the reader has chosen nothing", () => {
    const tabs = childTabsOf([
      tabRow("t1", { kind: "rebaseCommit", state: "waitingOnUser" }),
      tabRow("t2", { kind: "queue", state: "settled", outcome: "succeeded" }),
    ], undefined);
    expect(autoSelectedTab(tabs)?.id).toBe("t1");
  });
});

describe("a test run waiting for its repository's test turn", () => {
  it("draws a hollow dot", () => {
    const [tab] = childTabsOf([tabRow("r1", { kind: "testsRun", state: "waitingForTurn" })], undefined);
    const glyph = drawFeedMergeTab(tab, { active: false, ticker: TICKER }).querySelector(".merge-tab-glyph");
    expect([glyph?.textContent, glyph?.classList.contains("is-waiting-for-turn")]).toEqual(["○", true]);
  });

  it("ticks from when its round began", () => {
    const [tab] = childTabsOf([tabRow("r1", { kind: "testsRun", state: "waitingForTurn" })], undefined);
    expect(drawTabDuration(tab, TICKER).textContent).toBe("4s");
  });

  it("is where the merge is when the reader has chosen nothing", () => {
    const tabs = childTabsOf([
      tabRow("r1", { kind: "testsRun", state: "waitingForTurn" }),
      tabRow("r0", { kind: "testsRun", state: "settled", outcome: "failed" }),
    ], undefined);
    expect(autoSelectedTab(tabs)?.id).toBe("r1");
  });
});

describe("a commit the replay has not reached", () => {
  it("draws no clock", () => {
    const [tab] = childTabsOf([tabRow("c1", { kind: "rebaseCommit", state: "pending" })], undefined);
    expect([tab?.startedMs, drawTabDuration(tab, TICKER).textContent]).toEqual([undefined, ""]);
  });

  it("draws no glyph", () => {
    const [tab] = childTabsOf([tabRow("c1", { kind: "rebaseCommit", state: "pending" })], undefined);
    const glyph = drawFeedMergeTab(tab, { active: false, ticker: TICKER }).querySelector(".merge-tab-glyph");
    expect([glyph?.textContent, glyph?.classList.contains("is-pending")]).toEqual(["", true]);
  });

  it("is never where the merge is while a commit is live", () => {
    const tabs = childTabsOf(
      [
        tabRow("c1", { kind: "rebaseCommit", state: "live" }),
        tabRow("c2", { kind: "rebaseCommit", state: "pending" }),
      ],
      undefined,
    );
    expect(autoSelectedTab(tabs)?.id).toBe("c1");
  });
});

describe("autoSelectedTab: the tab whose state changed last", () => {
  it("picks the live tab even when a settled one follows nothing", () => {
    const tabs = childTabsOf([
      tabRow("t1", { kind: "queue", state: "settled", outcome: "succeeded" }),
      tabRow("t2", { kind: "rebasing", state: "live" }),
    ], undefined);
    expect(autoSelectedTab(tabs)?.id).toBe("t2");
  });

  it("picks the LAST live tab when several are live", () => {
    const tabs = childTabsOf([
      tabRow("t1", { kind: "queue", state: "live" }),
      tabRow("t2", { kind: "tests", state: "live" }),
    ], undefined);
    expect(autoSelectedTab(tabs)?.id).toBe("t2");
  });

  it("falls back to the last tab on a wholly settled run", () => {
    const tabs = childTabsOf([
      tabRow("t1", { kind: "queue", state: "settled", outcome: "succeeded" }),
      tabRow("t2", { kind: "committing", state: "settled", outcome: "succeeded" }),
    ], undefined);
    expect(autoSelectedTab(tabs)?.id).toBe("t2");
  });

  it("selects nothing at all when the sub-feed has no tabs yet", () => {
    expect(autoSelectedTab([])).toBeUndefined();
  });

  it("follows a tab that started after the live queue tab", () => {
    const tabs = childTabsOf([
      tabRow("q", { kind: "queue", state: "live", startedAtMs: 1_000n }),
      tabRow("r", { kind: "rebasing", state: "live", startedAtMs: 2_000n }),
    ], undefined);
    expect(autoSelectedTab(tabs)?.id).toBe("r");
  });

  it("goes back to the live queue tab once a tab concludes", () => {
    const tabs = childTabsOf([
      tabRow("q", { kind: "queue", state: "live", startedAtMs: 1_000n }),
      tabRow("r", { kind: "rebasing", state: "settled", outcome: "succeeded", startedAtMs: 2_000n, endedAtMs: 3_000n }),
    ], undefined);
    expect(autoSelectedTab(tabs)?.id).toBe("q");
  });

  it("follows a tab that started after an earlier one concluded", () => {
    const tabs = childTabsOf([
      tabRow("q", { kind: "queue", state: "live", startedAtMs: 1_000n }),
      tabRow("r", { kind: "rebasing", state: "settled", outcome: "succeeded", startedAtMs: 2_000n, endedAtMs: 3_000n }),
      tabRow("t", { kind: "tests", state: "live", startedAtMs: 4_000n }),
    ], undefined);
    expect(autoSelectedTab(tabs)?.id).toBe("t");
  });

  it("hands a step that settles as the next begins to the next", () => {
    const tabs = childTabsOf([
      tabRow("r", { kind: "rebasing", state: "settled", outcome: "succeeded", startedAtMs: 1_000n, endedAtMs: 3_000n }),
      tabRow("t", { kind: "tests", state: "live", startedAtMs: 3_000n }),
    ], undefined);
    expect(autoSelectedTab(tabs)?.id).toBe("t");
  });

  it("hands a conclusion to the most recently started of several running tabs", () => {
    const tabs = childTabsOf([
      tabRow("t", { kind: "tests", state: "live", startedAtMs: 2_000n }),
      tabRow("q", { kind: "queue", state: "live", startedAtMs: 1_000n }),
      tabRow("r", { kind: "rebasing", state: "settled", outcome: "succeeded", startedAtMs: 2_500n, endedAtMs: 3_000n }),
    ], undefined);
    expect(autoSelectedTab(tabs)?.id).toBe("t");
  });

  it("hands a conclusion to a run waiting for its test turn", () => {
    const tabs = childTabsOf([
      tabRow("r1", { kind: "testsRun", state: "waitingForTurn", startedAtMs: 1_000n }),
      tabRow("r0", { kind: "testsRun", state: "settled", outcome: "failed", startedAtMs: 500n, endedAtMs: 3_000n }),
    ], undefined);
    expect(autoSelectedTab(tabs)?.id).toBe("r1");
  });

  it("keeps the concluded tab shown when nothing is running", () => {
    const tabs = childTabsOf([
      tabRow("q", { kind: "queue", state: "settled", outcome: "succeeded", startedAtMs: 1_000n, endedAtMs: 2_000n }),
      tabRow("c", { kind: "committing", state: "settled", outcome: "failed", startedAtMs: 2_000n, endedAtMs: 4_000n }),
      tabRow("u", { kind: "updatingMain", state: "settled", outcome: "succeeded", startedAtMs: 2_000n, endedAtMs: 3_000n }),
    ], undefined);
    expect(autoSelectedTab(tabs)?.id).toBe("c");
  });

  it("shows the last tab when no tab has begun", () => {
    const tabs = childTabsOf([
      tabRow("c1", { kind: "rebaseCommit", state: "pending" }),
      tabRow("c2", { kind: "rebaseCommit", state: "pending" }),
    ], undefined);
    expect(autoSelectedTab(tabs)?.id).toBe("c2");
  });
});

describe("drawTabStateGlyph: a state this build has no glyph for", () => {
  it("draws the badge with an empty glyph rather than inventing one", () => {
    // `readMergeTab` validates the arm, so this shape only arrives from a
    // daemon newer than this bundle; the tab is built directly to reach it.
    const row = tabRow("t1", { kind: "tests", state: "live" });
    const tab = { ...readMergeTab(row, row.row.value as never), state: "rewinding" };

    const el = drawFeedMergeTab(tab, { active: false, ticker: TICKER });

    const glyph = el.querySelector(".merge-tab-glyph");
    expect(el.getAttribute("data-tab-state")).toBe("rewinding");
    expect(glyph?.textContent).toBe("");
    expect(glyph?.className).toBe("merge-tab-glyph");
  });
});

/**
 * A TAB IS DRAWN IN PLACE (owner request, 2026-10-10): a push repaints only
 * what moved, so a live tab's breathing dot is never a fresh element.
 */
describe("updateFeedMergeTab", () => {
  /** TAB drawn, then updated to NEXT; answers the element and its parts before and after. */
  function updated(tab: Parameters<typeof tabRow>[1], next: Parameters<typeof tabRow>[1], active = false) {
    const before = readMergeTab(tabRow("t1", tab), tabRow("t1", tab).row.value as never);
    const after = readMergeTab(tabRow("t1", next), tabRow("t1", next).row.value as never);
    const el = drawFeedMergeTab(before, { active: false, ticker: TICKER });
    const glyph = el.querySelector(".merge-tab-glyph");
    const duration = el.querySelector(".merge-tab-duration");
    updateFeedMergeTab(el, after, before, { active, ticker: TICKER });
    return { el, glyph, duration };
  }

  it("keeps the live glyph's element when its state did not move", () => {
    const u = updated({ kind: "tests", state: "live" }, { kind: "tests", state: "live", round: 2 });
    expect(u.el.querySelector(".merge-tab-glyph")).toBe(u.glyph);
  });

  it("repaints the glyph in place when the tab settles", () => {
    const u = updated({ kind: "tests", state: "live" }, { kind: "tests", state: "settled", outcome: "failed" });
    const glyph = u.el.querySelector(".merge-tab-glyph");
    expect([glyph, glyph?.className, u.el.getAttribute("data-tab-outcome")]).toEqual([
      u.glyph,
      "merge-tab-glyph is-failed",
      "failed",
    ]);
  });

  it("keeps the duration's element while its instants did not move", () => {
    const u = updated({ kind: "tests", state: "live" }, { kind: "tests", state: "live", round: 2 });
    expect(u.el.querySelector(".merge-tab-duration")).toBe(u.duration);
  });

  it("redraws the duration when the tab's instants moved", () => {
    const u = updated({ kind: "tests", state: "live" }, { kind: "tests", state: "settled" });
    expect(u.el.querySelector(".merge-tab-duration")).not.toBe(u.duration);
  });

  it("redraws the label when the round moved", () => {
    const u = updated({ kind: "tests", state: "live" }, { kind: "tests", state: "live", round: 2 });
    expect(u.el.querySelector(".merge-tab-label")?.textContent).toBe("tests (2)");
  });

  it("marks the tab active when it became the shown one", () => {
    const u = updated({ kind: "tests", state: "live" }, { kind: "tests", state: "live" }, true);
    expect([u.el.classList.contains("is-active"), u.el.getAttribute("aria-selected")]).toEqual([true, "true"]);
  });
});
