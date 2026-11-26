// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  SelectInEditorResponseSchema,
  type SelectInEditorRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_in_editor_pb";
import { FeedChildSchema, type FeedChild } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import { CHILD_CLASS, CHILD_LONG_SUMMARY_CLASS, CHILD_NAME_CLASS, CHILD_SHORT_SUMMARY_CLASS, drawFeedChild } from "../../src/feed/child.js";
import { FRAME_TONE_ATTRIBUTE } from "../../src/feed/bubble-frame.js";
import { BRANCH_LINE_TARGET_LINK_CLASS } from "../../src/feed/merge/merge.js";
import { cascadedValue, inDocument } from "../stylesheet.js";
import { scriptedAppContext } from "../rpc/app-context.js";
import { childHeadInit } from "./child-init.js";
import { RecordingSink, WORKSPACE, mergeRow, rowContext } from "./harness.js";

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(1_000_000);
});
afterEach(() => {
  vi.useRealTimers();
});


/** An open child, its head as the daemon draws one, edited by INIT. */
function child(init: { summary?: undefined; border?: { tone: string } | undefined } = {}): FeedChild {
  return create(FeedChildSchema, {
    head: childHeadInit(940_000n),
    border: "border" in init ? init.border : { tone: "red" },
    summary: "summary" in init ? undefined : { shortSummary: { text: "Fix the flaky test" }, longSummary: { markdown: "- It fixes the test.\n- It pins the backoff." } },
  });
}

/** Draw U against a context recording every SelectInEditor. */
function draw(u: FeedChild) {
  const calls: SelectInEditorRequest[] = [];
  const ctx = scriptedAppContext(
    {
      selectInEditor: (req) => {
        calls.push(req);
        return create(SelectInEditorResponseSchema, { result: { case: "success", value: { route: { case: "relayed", value: {} } } } });
      },
    },
    { workspace: WORKSPACE, failures: new RecordingSink() },
  );
  return { el: drawFeedChild(u, rowContext(ctx, mergeRow("m1"))), calls };
}

describe("drawFeedChild: the head", () => {
  it("draws the glyph, the name, the target and no badge while open", () => {
    const { el } = draw(child());
    expect([
      el.querySelector(".merge-glyph")?.textContent,
      el.querySelector(`.${CHILD_NAME_CLASS}`)?.textContent,
      el.querySelector(".feed-child-target")?.textContent,
      el.querySelector(".merge-badge"),
    ]).toEqual(["⇄", "fix-flaky", "→ ~/.config/doom", null]);
  });

  it.each([
    ["waiting in its queue", { case: "merging" as const, value: { stage: { case: "waiting" as const, value: {} } } }, "waiting"],
    ["being merged", { case: "merging" as const, value: { stage: { case: "merging" as const, value: {} } } }, "merging"],
    ["landed", { case: "merged" as const, value: {} }, "merged"],
    ["closed", { case: "closed" as const, value: {} }, "closed"],
    ["removed", { case: "removed" as const, value: { removedAtMs: 990_000n } }, "removed"],
  ])("badges a child %s", (_name, standing, word) => {
    const u = child();
    u.head!.standing = create(FeedChildSchema, { head: { standing: { standing } } }).head!.standing;
    expect(draw(u).el.querySelector(".merge-badge")?.textContent).toBe(word);
  });

  it("draws a landing's version, its incremented number apart", () => {
    const u = child();
    u.head!.standing = create(FeedChildSchema, {
      head: { standing: { standing: { case: "merged", value: { version: { leading: { text: "0.591." }, incremented: { text: "1" } } } } } },
    }).head!.standing;
    const badge = draw(u).el.querySelector(".merge-badge");
    expect([badge?.textContent, badge?.querySelector(".feed-child-version-incremented")?.textContent]).toEqual(["merged 0.591.1", "1"]);
  });

  it("selects the child on a click of its name", async () => {
    const { el, calls } = draw(child());
    el.querySelector<HTMLElement>(`.${CHILD_NAME_CLASS}`)?.click();
    await vi.advanceTimersByTimeAsync(1);
    expect(calls.map((c) => c.workspace?.id)).toEqual(["ws-child"]);
  });

  it("draws a removed child's name as no door", async () => {
    const u = child();
    u.head!.name!.select = undefined;
    const { el, calls } = draw(u);
    const name = el.querySelector<HTMLElement>(`.${CHILD_NAME_CLASS}`);
    name?.click();
    await vi.advanceTimersByTimeAsync(1);
    expect([name?.classList.contains("is-door"), calls.length]).toEqual([false, 0]);
  });

  it("counts a settled child's clock up to when it stopped being open", () => {
    const u = child();
    u.head!.runtime!.endedAtMs = 1_000_000n;
    expect(draw(u).el.querySelector(".merge-head-end")?.textContent).toBe("1m");
  });

  it("refuses a head with no standing", () => {
    const u = child();
    u.head!.standing = undefined;
    expect(() => draw(u)).toThrow(MalformedView);
  });
});

describe("drawFeedChild: the frame's tone", () => {
  it("states the child's canonical status color for the frame while open", () => {
    expect(draw(child()).el.getAttribute(FRAME_TONE_ATTRIBUTE)).toBe("red");
  });

  it("states no frame tone once the child is not open", () => {
    expect(draw(child({ border: undefined })).el.hasAttribute(FRAME_TONE_ATTRIBUTE)).toBe(false);
  });

  it("refuses a color the vocabulary does not declare", () => {
    expect(() => draw(child({ border: { tone: "chartreuse" } }))).toThrow(MalformedView);
  });
});

describe("drawFeedChild: the target", () => {
  it("draws a target that deploys through a pull request as a link to it", () => {
    const u = child();
    u.head!.target!.pullRequest = create(FeedChildSchema, {
      head: { target: { text: "x", pullRequest: { url: "https://github.com/o/r/pull/7" } } },
    }).head!.target!.pullRequest;
    const link = draw(u).el.querySelector<HTMLAnchorElement>(`.feed-child-target a.${BRANCH_LINE_TARGET_LINK_CLASS}`);
    expect([link?.textContent, link?.getAttribute("href")]).toEqual(["~/.config/doom", "https://github.com/o/r/pull/7"]);
  });
});

describe("drawFeedChild: the summaries", () => {
  it("draws the short summary and the long one, rendered", () => {
    const { el } = draw(child());
    expect([
      el.querySelector(`.${CHILD_SHORT_SUMMARY_CLASS}`)?.textContent,
      el.querySelector(`.${CHILD_LONG_SUMMARY_CLASS}`)?.querySelectorAll("li").length,
    ]).toEqual(["Fix the flaky test", 2]);
  });

  it("draws the short summary alone when the child has no long one", () => {
    const u = child();
    u.summary!.longSummary = undefined;
    const { el } = draw(u);
    expect([el.querySelector(`.${CHILD_LONG_SUMMARY_CLASS}`), el.hasAttribute("data-has-long")]).toEqual([null, false]);
  });

  it("draws the head alone for a child with no summary", () => {
    const { el } = draw(child({ summary: undefined }));
    expect([el.classList.contains(CHILD_CLASS), el.querySelector(".feed-child-summary")]).toEqual([true, null]);
  });
});

describe("drawFeedChild: which summary the fold shows", () => {
  /** Which summaries show on a child's head inside a bubble folded or open. */
  function shown(expanded: boolean, u: FeedChild = child()): string[] {
    const fold = document.createElement("div");
    fold.className = "tool-card bubble-fold";
    fold.setAttribute("data-expanded", expanded ? "true" : "false");
    const head = document.createElement("div");
    head.className = "bubble-head";
    head.append(draw(u).el);
    fold.append(head);
    return inDocument(fold, () =>
      [...fold.querySelectorAll<HTMLElement>(".feed-child-summary")]
        .filter((summary) => cascadedValue(summary, "display") !== "none")
        .map((summary) => (summary.classList.contains(CHILD_SHORT_SUMMARY_CLASS) ? "short" : "long")),
    );
  }

  it("shows the short summary alone while folded", () => {
    expect(shown(false)).toEqual(["short"]);
  });

  it("shows the long summary alone while open", () => {
    expect(shown(true)).toEqual(["long"]);
  });

  it("shows the short summary while open when the child has no long one", () => {
    const u = child();
    u.summary!.longSummary = undefined;
    expect(shown(true, u)).toEqual(["short"]);
  });
});
