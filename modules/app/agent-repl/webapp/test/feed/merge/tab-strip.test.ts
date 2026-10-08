// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  FeedMergeTabSchema,
  FeedMergeTabSettledSchema,
  FeedRowSchema,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { createTicker } from "../../../src/clock.js";
import { MalformedView } from "../../../src/rpc/malformed.js";
import {
  AGENTIC_KINDS,
  RESOLVED_KINDS,
  autoSelectedTab,
  drawFeedMergeTab,
  drawFeedMergeTabLabel,
  drawTabDuration,
  mergeTabsOf,
  readMergeTab,
} from "../../../src/feed/merge/tab-strip.js";
import { oneofArms } from "../../arms.js";
import { TAB_STARTED_AT_MS, id, tabRow } from "./fixtures.js";
import { orderFor } from "../../feed-order.js";

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

describe("mergeTabsOf: the strip is the sub-feed's tab rows, in served order", () => {
  it("keeps the served order rather than sorting by kind", () => {
    const tabs = mergeTabsOf([
      tabRow("t1", { kind: "tests", state: "settled", outcome: "failed" }),
      tabRow("t2", { kind: "rebasing", state: "live" }),
    ]);
    expect(tabs.map((t) => t.kind)).toEqual(["tests", "rebasing"]);
  });

  it("ignores every row that is not a tab", () => {
    const tabs = mergeTabsOf([
      create(FeedRowSchema, { id: id("r1"), order: orderFor("r1"), row: { case: "userPrompt", value: {} } }),
      tabRow("t1", { kind: "tests", state: "live" }),
    ]);
    expect(tabs.map((t) => t.id)).toEqual(["t1"]);
  });
});

describe("readMergeTab: the arms are read from the contract", () => {
  it("holds to the schema: every kind is either resolved or agentic", () => {
    expect([...RESOLVED_KINDS, ...AGENTIC_KINDS].sort()).toEqual(
      [...oneofArms(FeedMergeTabSchema, "kind")].sort(),
    );
  });

  it.each(FeedMergeTabSchema.oneofs.find((o) => o.name === "kind")?.fields.map((f) => [f.localName, f]) ?? [])(
    "holds to the schema: the %s tab is live or settled, since nothing parks, and an agentic step that asks can wait on the user",
    (kind, field) => {
      const message = field.fieldKind === "message" ? field.message : undefined;
      const want = kind === "conflicts" || kind === "fixes" ? ["live", "settled", "waitingOnUser"] : ["live", "settled"];
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
      row: { case: "mergeTab", value: create(FeedMergeTabSchema, { label: { text: "x" } }) },
    });
    expect(() => mergeTabsOf([row])).toThrow(MalformedView);
  });

  it("refuses a tab whose state oneof is unset", () => {
    const row = create(FeedRowSchema, {
      id: id("t1"),
      order: orderFor("t1"),
      row: {
        case: "mergeTab",
        value: create(FeedMergeTabSchema, {
          label: { text: "x" },
          kind: { case: "tests", value: {} },
        }),
      },
    });
    expect(() => mergeTabsOf([row])).toThrow(MalformedView);
  });

  it("refuses a settled tab whose outcome oneof is unset", () => {
    const row = create(FeedRowSchema, {
      id: id("t1"),
      order: orderFor("t1"),
      row: {
        case: "mergeTab",
        value: create(FeedMergeTabSchema, {
          label: { text: "x" },
          kind: { case: "tests", value: { state: { case: "settled", value: {} } } },
        }),
      },
    });
    expect(() => mergeTabsOf([row])).toThrow(MalformedView);
  });
});

describe("drawFeedMergeTabLabel: rounds beyond the first", () => {
  it("decorates a second round with its number", () => {
    const el = drawFeedMergeTabLabel({
      $typeName: "frontend.v1.FeedMergeTabLabel",
      text: "tests",
      round: 2,
    });
    expect(el.textContent).toBe("tests (2)");
  });

  it("draws round 1 undecorated, so an ordinary run never reads as a retry", () => {
    const el = drawFeedMergeTabLabel({
      $typeName: "frontend.v1.FeedMergeTabLabel",
      text: "tests",
      round: 1,
    });
    expect(el.textContent).toBe("tests");
  });
});

describe("drawFeedMergeTab: the badge says label, round and state — no counts (R5)", () => {
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
    const [tab] = mergeTabsOf([tabRow("t1", { kind: "tests", state: "live", label: "tests" })]);
    const el = drawFeedMergeTab(tab, { active: false, ticker: TICKER });
    expect(el.textContent).toBe("tests4s●");
  });

  it("marks a settled tab with its outcome", () => {
    const [tab] = mergeTabsOf([
      tabRow("t1", { kind: "tests", state: "settled", outcome: "failed" }),
    ]);
    const el = drawFeedMergeTab(tab, { active: false, ticker: TICKER });
    expect(el.getAttribute("data-tab-outcome")).toBe("failed");
    expect(el.querySelector(".merge-tab-glyph")?.textContent).toBe("✗");
  });

  it("marks the active tab so the strip shows where the reader is", () => {
    const [tab] = mergeTabsOf([tabRow("t1", { kind: "queue", state: "live" })]);
    expect(drawFeedMergeTab(tab, { active: true, ticker: TICKER }).getAttribute("aria-selected")).toBe("true");
  });
});

describe("readMergeTab: the instants the tab's duration reads", () => {
  it("reads a live tab's start", () => {
    const [tab] = mergeTabsOf([tabRow("t1", { kind: "tests", state: "live", startedAtMs: 2_000n })]);
    expect([tab.startedMs, tab.endedMs]).toEqual([2_000, undefined]);
  });

  it("reads a settled tab's start and end", () => {
    const [tab] = mergeTabsOf([
      tabRow("t1", { kind: "tests", state: "settled", startedAtMs: 2_000n, endedAtMs: 9_000n }),
    ]);
    expect([tab.startedMs, tab.endedMs]).toEqual([2_000, 9_000]);
  });
});

describe("drawTabDuration: how long the tab has been in its state", () => {
  it("ticks a live tab from when its work began", () => {
    const [tab] = mergeTabsOf([tabRow("t1", { kind: "tests", state: "live" })]);
    const el = drawTabDuration(tab, TICKER);
    vi.advanceTimersByTime(2_000);
    expect(el.textContent).toBe("6s");
  });

  it("shows a settled tab's run time, ended less started, and never ticks it", () => {
    const [tab] = mergeTabsOf([
      tabRow("t1", { kind: "tests", state: "settled", startedAtMs: 1_000n, endedAtMs: 91_000n }),
    ]);
    const el = drawTabDuration(tab, TICKER);
    vi.advanceTimersByTime(5_000);
    expect(el.textContent).toBe("1m 30s");
  });

  it("sits beside the tab's label, ahead of its state glyph", () => {
    const [tab] = mergeTabsOf([tabRow("t1", { kind: "tests", state: "live" })]);
    const el = drawFeedMergeTab(tab, { active: false, ticker: TICKER });
    expect([...el.children].map((c) => c.className)).toEqual([
      "merge-tab-label",
      "merge-tab-duration",
      "merge-tab-glyph is-live",
    ]);
  });
});

// A MERGE WAITING ON THE USER (owner request, 2026-10-08): the agentic tab
// whose agent has an ask open wears a question mark emoji while it stands.
describe("a tab waiting on the user", () => {
  it.each(["conflicts", "fixes"] as const)("draws the %s tab's glyph as ❓", (kind) => {
    const [tab] = mergeTabsOf([tabRow("t1", { kind, state: "waitingOnUser", payload: kind === "fixes" ? { attempt: { attempt: 1, maxAttempts: 3 } } : {} })]);
    const glyph = drawFeedMergeTab(tab, { active: false, ticker: TICKER }).querySelector(".merge-tab-glyph");
    expect(glyph?.textContent).toBe("❓");
  });

  it("marks the state as the hook the suite targets", () => {
    const [tab] = mergeTabsOf([tabRow("t1", { kind: "conflicts", state: "waitingOnUser" })]);
    expect(drawFeedMergeTab(tab, { active: false, ticker: TICKER }).getAttribute("data-tab-state")).toBe(
      "waitingOnUser",
    );
  });

  it("keeps ticking from when the tab's work began", () => {
    const [tab] = mergeTabsOf([tabRow("t1", { kind: "conflicts", state: "waitingOnUser" })]);
    expect(drawTabDuration(tab, TICKER).textContent).toBe("4s");
  });

  it("is still where the merge is when the reader has chosen nothing", () => {
    const tabs = mergeTabsOf([
      tabRow("t1", { kind: "conflicts", state: "waitingOnUser" }),
      tabRow("t2", { kind: "queue", state: "settled", outcome: "succeeded" }),
    ]);
    expect(autoSelectedTab(tabs)?.id).toBe("t1");
  });
});

describe("autoSelectedTab: the last live tab, else the last tab", () => {
  it("picks the live tab even when a settled one follows nothing", () => {
    const tabs = mergeTabsOf([
      tabRow("t1", { kind: "queue", state: "settled", outcome: "succeeded" }),
      tabRow("t2", { kind: "rebasing", state: "live" }),
    ]);
    expect(autoSelectedTab(tabs)?.id).toBe("t2");
  });

  it("picks the LAST live tab when several are live", () => {
    const tabs = mergeTabsOf([
      tabRow("t1", { kind: "queue", state: "live" }),
      tabRow("t2", { kind: "tests", state: "live" }),
    ]);
    expect(autoSelectedTab(tabs)?.id).toBe("t2");
  });

  it("falls back to the last tab on a wholly settled run", () => {
    const tabs = mergeTabsOf([
      tabRow("t1", { kind: "queue", state: "settled", outcome: "succeeded" }),
      tabRow("t2", { kind: "committing", state: "settled", outcome: "succeeded" }),
    ]);
    expect(autoSelectedTab(tabs)?.id).toBe("t2");
  });

  it("selects nothing at all when the sub-feed has no tabs yet", () => {
    expect(autoSelectedTab([])).toBeUndefined();
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
