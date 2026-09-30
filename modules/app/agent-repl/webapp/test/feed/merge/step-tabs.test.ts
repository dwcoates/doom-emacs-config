// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  FeedMergeTabCommittingSchema,
  FeedMergeTabFixesSchema,
  FeedMergeTabRebasingSchema,
  FeedMergeTabUpdatingMainSchema,
  FeedMergeUpdatingMainStepSchema,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../../src/rpc/malformed.js";
import {
  drawFeedMergeTabCommitting,
  drawFeedMergeTabFixesAttempt,
  drawFeedMergeTabRebasing,
  drawFeedMergeTabUpdatingMain,
  drawFeedMergeUpdatingMainStep,
} from "../../../src/feed/merge/step-tabs.js";
import { oneofArms } from "../../arms.js";

const LIVE = { case: "live" as const, value: {} };

describe("drawFeedMergeTabRebasing", () => {
  it("draws the progress as the daemon's two figures", () => {
    const u = create(FeedMergeTabRebasingSchema, { state: LIVE, progress: { replayed: 3, total: 7 } });
    expect(drawFeedMergeTabRebasing(u, "rebasing").querySelector("[data-merge-progress]")?.textContent).toBe("3/7");
  });

  it("draws each narration line verbatim, oldest first", () => {
    const u = create(FeedMergeTabRebasingSchema, {
      state: LIVE,
      progress: { replayed: 2, total: 2 },
      lines: [{ text: "replaying 1/2 · tidy" }, { text: "replaying 2/2 · fix" }],
    });
    const el = drawFeedMergeTabRebasing(u, "rebasing");
    expect([...el.querySelectorAll(".merge-line")].map((line) => line.textContent)).toEqual([
      "replaying 1/2 · tidy",
      "replaying 2/2 · fix",
    ]);
  });

  it("delimits the narration through the ONE shared list rule", () => {
    const u = create(FeedMergeTabRebasingSchema, { state: LIVE, progress: { replayed: 0, total: 1 } });
    expect(drawFeedMergeTabRebasing(u, "rebasing").querySelector(".merge-lines")?.classList.contains("list-rows")).toBe(
      true,
    );
  });

  it("does not count the narration lines for its figure", () => {
    const u = create(FeedMergeTabRebasingSchema, {
      state: LIVE,
      progress: { replayed: 1, total: 5 },
      lines: [{ text: "a" }, { text: "b" }, { text: "c" }],
    });
    expect(drawFeedMergeTabRebasing(u, "rebasing").querySelector("[data-merge-progress]")?.textContent).toBe("1/5");
  });

  it("refuses a rebasing tab with no progress", () => {
    const u = create(FeedMergeTabRebasingSchema, { state: LIVE });
    expect(() => drawFeedMergeTabRebasing(u, "rebasing")).toThrow(MalformedView);
  });
});

describe("drawFeedMergeTabCommitting", () => {
  it("draws the merge commit's subject verbatim", () => {
    const u = create(FeedMergeTabCommittingSchema, { state: LIVE, subject: { text: "Merge branch 'fix'" } });
    expect(drawFeedMergeTabCommitting(u, "committing").textContent).toBe("Merge branch 'fix'");
  });

  it("refuses a committing tab with no subject", () => {
    const u = create(FeedMergeTabCommittingSchema, { state: LIVE });
    expect(() => drawFeedMergeTabCommitting(u, "committing")).toThrow(MalformedView);
  });
});

describe("drawFeedMergeTabUpdatingMain", () => {
  it("draws every step the contract declares", () => {
    expect([...oneofArms(FeedMergeUpdatingMainStepSchema, "step")].sort()).toEqual(["fastForwarding", "fetching"]);
  });

  it("says it is fetching", () => {
    const u = create(FeedMergeTabUpdatingMainSchema, { state: LIVE, step: { step: { case: "fetching", value: {} } } });
    expect(drawFeedMergeTabUpdatingMain(u, "updating_main").textContent).toBe("fetching");
  });

  it("says which commit it is fast-forwarding to", () => {
    const u = create(FeedMergeTabUpdatingMainSchema, {
      state: LIVE,
      step: { step: { case: "fastForwarding", value: { commit: "4f2a1c" } } },
    });
    expect(drawFeedMergeTabUpdatingMain(u, "updating_main").textContent).toBe("fast-forwarding to 4f2a1c");
  });

  it("stamps the step's arm", () => {
    const u = create(FeedMergeTabUpdatingMainSchema, {
      state: LIVE,
      step: { step: { case: "fastForwarding", value: { commit: "4f2a1c" } } },
    });
    expect(drawFeedMergeTabUpdatingMain(u, "updating_main").getAttribute("data-update-step")).toBe("fastForwarding");
  });

  it("refuses an updating main tab with no step", () => {
    const u = create(FeedMergeTabUpdatingMainSchema, { state: LIVE });
    expect(() => drawFeedMergeTabUpdatingMain(u, "updating_main")).toThrow(MalformedView);
  });

  it("refuses a step that names no arm", () => {
    expect(() => drawFeedMergeUpdatingMainStep(create(FeedMergeUpdatingMainStepSchema, {}), "step")).toThrow(
      MalformedView,
    );
  });

  it("refuses a step arm a newer daemon set, rather than drawing a default", () => {
    const u = create(FeedMergeUpdatingMainStepSchema, { step: { case: "fetching", value: {} } });
    (u as unknown as { step: { case: string; value: unknown } }).step = { case: "rewinding", value: {} };
    expect(() => drawFeedMergeUpdatingMainStep(u, "step")).toThrow(MalformedView);
  });
});

describe("drawFeedMergeTabFixesAttempt", () => {
  it("draws the attempt as 'attempt 2/3'", () => {
    const u = create(FeedMergeTabFixesSchema, { state: LIVE, attempt: { attempt: 2, maxAttempts: 3 } });
    expect(drawFeedMergeTabFixesAttempt(u, "fixes").textContent).toBe("attempt 2/3");
  });

  it("refuses a fixes tab with no attempt", () => {
    const u = create(FeedMergeTabFixesSchema, { state: LIVE });
    expect(() => drawFeedMergeTabFixesAttempt(u, "fixes")).toThrow(MalformedView);
  });
});
