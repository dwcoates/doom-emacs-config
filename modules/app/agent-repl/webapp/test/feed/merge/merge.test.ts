// @vitest-environment jsdom
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  type FeedMerge,
  FeedMergeSchema,
  FeedMergeErrorSchema,
  FeedMergeModeSchema,
  FeedMergeWorkspaceSwitchSchema,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { drawFeedMerge, drawFeedMergeGlyph, drawMergeCommitText } from "../../../src/feed/merge/merge.js";
import { countingTicker, drawnBefore, harness, mergeRow, rowContext } from "../harness.js";
import { oneofArms } from "../../arms.js";
import { mergeHead } from "./fixtures.js";
import { HEAD_CLOCK_CLASS } from "../../../src/feed/cards/tool-head.js";
import { cascadedValue, installStylesheet } from "../../stylesheet.js";

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
    const el = draw(mergeHead({ case: "update" }, { label: "ABC/fix → master" }));
    expect(el.querySelector(".merge-label")?.textContent).toBe("ABC/fix → master");
  });

  it("draws the ⇄ glyph for the vocabulary's merge icon", () => {
    const el = draw(mergeHead({ case: "update" }));
    expect(el.querySelector(".merge-glyph")?.textContent).toBe("⇄");
  });
});

describe("drawFeedMerge: every result arm", () => {
  it("says 'merging' while the run is live", () => {
    const el = draw(mergeHead({ case: "update", stage: "merging" }));
    expect(el.getAttribute("data-state")).toBe("update");
    expect(el.querySelector(".merge-badge")?.textContent).toBe("merging");
  });

  it("says 'waiting' while the merge waits in its queue", () => {
    const el = draw(mergeHead({ case: "update", stage: "waiting" }));
    expect([el.getAttribute("data-state"), el.querySelector(".merge-badge")?.textContent]).toEqual(["update", "waiting"]);
  });

  it("breathes the waiting word in the live badge's tone, as it does the merging one", () => {
    const el = draw(mergeHead({ case: "update", stage: "waiting" }));
    expect(el.querySelector(".merge-badge")?.classList.contains("is-live")).toBe(true);
  });

  it("refuses a live head whose stage oneof is unset", () => {
    const msg = create(FeedMergeSchema, {
      head: { glyph: { icon: "merge" }, label: { text: "x" }, runtime: { startedAtMs: 0n } },
      result: { case: "update", value: {} },
    });
    expect(() => draw(msg)).toThrow(MalformedView);
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
        fold: { folded: true, decidedBy: { case: "daemon", value: {} } },
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
        fold: { folded: true, decidedBy: { case: "daemon", value: {} } },
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

describe("an arm this build cannot draw is a refusal, never a default", () => {
  it("refuses a merge whose result is an arm a newer daemon set", () => {
    const newer = mergeHead({ case: "update" });
    (newer as { result: unknown }).result = { case: "rewound", value: {} };

    let thrown: unknown;
    try {
      draw(newer);
    } catch (err) {
      thrown = err;
    }

    expect(thrown).toBeInstanceOf(MalformedView);
    expect((thrown as MalformedView).path).toBe("FeedMerge.result");
    expect((thrown as MalformedView).detail).toBe(
      "arm 'rewound' is not one this build can draw",
    );
  });

  it("refuses a merge error whose reason is an arm a newer daemon set", () => {
    const newer = mergeHead({ case: "failed", endedAtMs: 9_000n, summary: "nope" });
    const error = (newer as { result: { value: { reason: unknown } } }).result.value;
    error.reason = { case: "superseded", value: {} };

    let thrown: unknown;
    try {
      draw(newer);
    } catch (err) {
      thrown = err;
    }

    expect(thrown).toBeInstanceOf(MalformedView);
    expect((thrown as MalformedView).path).toBe("FeedMerge.error.reason");
    expect((thrown as MalformedView).detail).toBe(
      "arm 'superseded' is not one this build can draw",
    );
  });
});

describe("drawFeedMerge: a settled head's clocks", () => {
  it("subscribes to nothing once the merge has landed", () => {
    // Arrange.
    const ticker = countingTicker();
    const h = harness({ ticker });
    // Act.
    drawFeedMerge(mergeHead({ case: "success", endedAtMs: 12_000n, commit: "abc1234" }), rowContext(h.ctx, mergeRow("m1")));
    // Assert.
    expect(ticker.live()).toBe(0);
  });

  it("subscribes to nothing once the merge has failed", () => {
    const ticker = countingTicker();
    const h = harness({ ticker });
    drawFeedMerge(mergeHead({ case: "failed", endedAtMs: 12_000n, summary: "tests never passed" }), rowContext(h.ctx, mergeRow("m1")));
    expect(ticker.live()).toBe(0);
  });
});

describe("drawFeedMerge: a detached merge's switch", () => {
  /** Make MSG an async merge whose head carries an available switch. */
  function withSwitch(msg: FeedMerge): void {
    msg.head!.mode = create(FeedMergeModeSchema, {
      mode: {
        case: "async",
        value: {
          workspaceSwitch: create(FeedMergeWorkspaceSwitchSchema, {
            label: { text: "open merge workspace" },
            availability: { case: "available", value: { target: { id: "ws-merge", dir: "/w/merge" } } },
          }),
        },
      },
    });
  }

  it("draws the switch to the merge workspace when the head carries one", () => {
    const msg = mergeHead({ case: "update" });
    withSwitch(msg);
    const el = draw(msg);
    expect(el.querySelector(".merge-workspace-switch-head .merge-workspace-switch-button")?.textContent).toBe(
      "open merge workspace",
    );
  });

  it("keeps the switch on a settled merge", () => {
    const msg = mergeHead({ case: "success", endedAtMs: 61_000n, commit: "a1b2c3d" });
    withSwitch(msg);
    expect(draw(msg).querySelector(".merge-workspace-switch-head")).not.toBeNull();
  });

  it("draws no switch for an in-place merge", () => {
    const msg = mergeHead({ case: "update" });
    msg.head!.mode = create(FeedMergeModeSchema, { mode: { case: "sync", value: {} } });
    expect(draw(msg).querySelector(".merge-workspace-switch")).toBeNull();
  });

  it("draws no switch for an async merge whose workspace is hidden", () => {
    const msg = mergeHead({ case: "update" });
    msg.head!.mode = create(FeedMergeModeSchema, { mode: { case: "async", value: {} } });
    expect(draw(msg).querySelector(".merge-workspace-switch")).toBeNull();
  });

  it("draws no switch for a head recorded before heads carried their mode", () => {
    expect(draw(mergeHead({ case: "update" })).querySelector(".merge-workspace-switch")).toBeNull();
  });
});

// THE BADGE COMES BEFORE THE CLOCK (owner ruling, 2026-10-11), and THE CLOCK
// CLOSES THE HEAD'S RIGHT END (owner request, 2026-10-18).
describe("drawFeedMerge: the badge before the clock", () => {
  it.each([
    ["a running merge", { case: "update" } as const],
    ["a landed merge", { case: "success", endedAtMs: 12_000n, commit: "abc1234" } as const],
    ["a failed merge", { case: "failed", endedAtMs: 12_000n, summary: "tests never passed" } as const],
  ])("draws %s's badge before its clock", (_name, result) => {
    // Arrange
    const h = harness();
    // Act
    const el = drawFeedMerge(mergeHead(result), rowContext(h.ctx, mergeRow("m1")));
    // Assert
    expect(drawnBefore(el.querySelector(".merge-badge"), el.querySelector(".merge-clock"))).toBe(true);
  });

  it.each([
    ["a running merge", { case: "update" } as const],
    ["a landed merge", { case: "success", endedAtMs: 12_000n, commit: "abc1234" } as const],
    ["a failed merge", { case: "failed", endedAtMs: 12_000n, summary: "tests never passed" } as const],
    ["an abandoned merge", { case: "abandoned", endedAtMs: 12_000n, summary: "dropped" } as const],
  ])("closes %s's head with its clock, in the right end, as the head's clock", (_name, result) => {
    // Arrange
    const h = harness();
    // Act
    const el = drawFeedMerge(mergeHead(result), rowContext(h.ctx, mergeRow("m1")));
    // Assert
    const clock = el.querySelector(".merge-clock");
    expect([
      el.lastElementChild?.classList.contains("merge-head-end"),
      el.querySelector(".merge-head-end")?.lastElementChild === clock,
      clock?.classList.contains(HEAD_CLOCK_CLASS),
    ]).toEqual([true, true, true]);
  });
});

describe("drawFeedMerge: the landed commit", () => {
  const landed = () =>
    mergeHead({
      case: "success",
      endedAtMs: 61_000n,
      commit: "a1b2c3d",
      landedOn: { repositoryDir: "/home/u/repo", branch: "master" },
    });

  it("draws the commit as a link to its branch's log when the head says where it landed", () => {
    const link = draw(landed()).querySelector(".merge-commit-link");
    expect([link?.tagName, link?.textContent, link?.hasAttribute("data-merge-commit")]).toEqual(["A", "a1b2c3d", true]);
  });

  it("draws the commit as plain text on a head recorded before heads said where it landed", () => {
    const el = draw(mergeHead({ case: "success", endedAtMs: 61_000n, commit: "a1b2c3d" }));
    expect([el.querySelector(".merge-commit")?.tagName, el.querySelector(".merge-commit-link")]).toEqual(["SPAN", null]);
  });

  it("sits at the head's right end, before the workspace switch and the clock", () => {
    const msg = landed();
    msg.head!.mode = create(FeedMergeModeSchema, {
      mode: {
        case: "async",
        value: {
          workspaceSwitch: create(FeedMergeWorkspaceSwitchSchema, {
            label: { text: "open merge workspace" },
            availability: { case: "available", value: { target: { id: "ws-merge", dir: "/w/merge" } } },
          }),
        },
      },
    });
    const end = draw(msg).querySelector(".merge-head-end");
    expect(Array.from(end?.children ?? [], (c) => c.classList.contains("merge-commit"))).toEqual([true, false, false]);
  });
});

describe("the landed commit's paint", () => {
  /** The commit link inside a merge bubble's head, folded or not. */
  function commitIn(expanded: boolean): { link: HTMLElement; teardown: () => void } {
    const teardownCss = installStylesheet();
    const fold = document.createElement("div");
    fold.className = "tool-card bubble-fold";
    fold.setAttribute("data-expanded", expanded ? "true" : "false");
    const headLine = document.createElement("div");
    headLine.className = "bubble-head";
    headLine.append(
      draw(
        mergeHead({
          case: "success",
          endedAtMs: 61_000n,
          commit: "a1b2c3d",
          landedOn: { repositoryDir: "/r", branch: "master" },
        }),
      ),
    );
    fold.append(headLine);
    document.body.append(fold);
    return {
      link: fold.querySelector<HTMLElement>(".merge-commit-link")!,
      teardown: () => {
        fold.remove();
        teardownCss();
      },
    };
  }

  it("hides the commit on a folded bubble's head", () => {
    const { link, teardown } = commitIn(false);
    try {
      expect(cascadedValue(link, "display")).toBe("none");
    } finally {
      teardown();
    }
  });

  it("shows the commit on an expanded bubble's head", () => {
    const { link, teardown } = commitIn(true);
    try {
      expect(cascadedValue(link, "display")).not.toBe("none");
    } finally {
      teardown();
    }
  });

  it("underlines the commit in a grey darker than the muted text", () => {
    const { link, teardown } = commitIn(true);
    try {
      expect([cascadedValue(link, "text-decoration"), cascadedValue(link, "color")]).toEqual([
        "underline",
        "color-mix(in srgb, var(--muted) 80%, black)",
      ]);
    } finally {
      teardown();
    }
  });

  it("draws the commit smaller than the head's text", () => {
    const { link, teardown } = commitIn(true);
    try {
      expect(cascadedValue(link, "font-size")).toBe("0.8em");
    } finally {
      teardown();
    }
  });

  it("pushes the head's right end to the right", () => {
    const { link, teardown } = commitIn(true);
    try {
      expect(cascadedValue(link.closest(".merge-head-end")!, "margin-left")).toBe("auto");
    } finally {
      teardown();
    }
  });
});

describe("drawMergeCommitText", () => {
  it("draws the commit as plain text in the merge commit face", () => {
    const el = drawMergeCommitText("a1b2c3d");
    expect([el.tagName, el.className, el.textContent]).toEqual(["SPAN", "merge-commit", "a1b2c3d"]);
  });

  it("is the one place a plain merge commit is drawn: no merge source hand-rolls its own", () => {
    const dir = path.join(path.dirname(fileURLToPath(import.meta.url)), "..", "..", "..", "src", "feed", "merge");
    const count = (file: string) =>
      (readFileSync(path.join(dir, file), "utf8").match(/className = "merge-commit"/g) ?? []).length;
    expect([count("merge.ts"), count("step-tabs.ts")]).toEqual([1, 0]);
  });
});
