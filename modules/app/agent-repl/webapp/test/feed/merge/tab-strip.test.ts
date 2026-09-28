// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  FeedMergeTabSchema,
  FeedMergeTabConflictsSchema,
  FeedMergeTabQueueSchema,
  FeedMergeTabSettledSchema,
  FeedRowSchema,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../../src/rpc/malformed.js";
import {
  AGENTIC_KINDS,
  RESOLVED_KINDS,
  autoSelectedTab,
  drawFeedMergeTab,
  drawFeedMergeTabLabel,
  mergeTabsOf,
  readMergeTab,
} from "../../../src/feed/merge/tab-strip.js";
import { oneofArms } from "../../arms.js";
import { id, tabRow } from "./fixtures.js";
import { orderFor } from "../../feed-order.js";

describe("mergeTabsOf: the strip is the sub-feed's tab rows, in served order", () => {
  it("keeps the served order rather than sorting by kind", () => {
    const tabs = mergeTabsOf([
      tabRow("t1", { kind: "queue", state: "settled", outcome: "succeeded" }),
      tabRow("t2", { kind: "merge", state: "live" }),
    ]);
    expect(tabs.map((t) => t.kind)).toEqual(["queue", "merge"]);
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

  it("holds to the schema: parked is legal only on conflicts and fixes", () => {
    expect([...oneofArms(FeedMergeTabConflictsSchema, "state")]).toContain("parked");
    expect([...oneofArms(FeedMergeTabQueueSchema, "state")]).not.toContain("parked");
  });

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
  it("marks the kind and the state as the hooks the suite targets", () => {
    const row = tabRow("t1", { kind: "fixes", state: "parked" });
    const tab = readMergeTab(row, row.row.value as never);
    const el = drawFeedMergeTab(tab, { active: true });
    expect(el.getAttribute("data-merge-tab")).toBe("fixes");
    expect(el.getAttribute("data-tab-state")).toBe("parked");
  });

  it("carries no count of any kind in its text", () => {
    const [tab] = mergeTabsOf([tabRow("t1", { kind: "tests", state: "live", label: "tests" })]);
    const el = drawFeedMergeTab(tab, { active: false });
    expect(el.textContent).toBe("tests●");
  });

  it("marks a settled tab with its outcome", () => {
    const [tab] = mergeTabsOf([
      tabRow("t1", { kind: "tests", state: "settled", outcome: "failed" }),
    ]);
    const el = drawFeedMergeTab(tab, { active: false });
    expect(el.getAttribute("data-tab-outcome")).toBe("failed");
    expect(el.querySelector(".merge-tab-glyph")?.textContent).toBe("✗");
  });

  it("marks the active tab so the strip shows where the reader is", () => {
    const [tab] = mergeTabsOf([tabRow("t1", { kind: "queue", state: "live" })]);
    expect(drawFeedMergeTab(tab, { active: true }).getAttribute("aria-selected")).toBe("true");
  });
});

describe("autoSelectedTab: the last live-or-parked tab, else the last tab", () => {
  it("picks the live tab even when a settled one follows nothing", () => {
    const tabs = mergeTabsOf([
      tabRow("t1", { kind: "queue", state: "settled", outcome: "succeeded" }),
      tabRow("t2", { kind: "merge", state: "live" }),
    ]);
    expect(autoSelectedTab(tabs)?.id).toBe("t2");
  });

  it("picks a parked tab, because it is waiting on the user", () => {
    const tabs = mergeTabsOf([
      tabRow("t1", { kind: "merge", state: "settled", outcome: "succeeded" }),
      tabRow("t2", { kind: "conflicts", state: "parked" }),
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
      tabRow("t2", { kind: "merge", state: "settled", outcome: "succeeded" }),
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

    const el = drawFeedMergeTab(tab, { active: false });

    const glyph = el.querySelector(".merge-tab-glyph");
    expect(el.getAttribute("data-tab-state")).toBe("rewinding");
    expect(glyph?.textContent).toBe("");
    expect(glyph?.className).toBe("merge-tab-glyph");
  });
});
