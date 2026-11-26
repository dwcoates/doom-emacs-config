// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  SelectInEditorResponseSchema,
  type SelectInEditorRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_in_editor_pb";
import { FeedChildSchema, type FeedChild } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import { CHILD_CLASS, CHILD_NAME_CLASS, CHILD_OPEN_ATTR, drawFeedChild } from "../../src/feed/child.js";
import { scriptedAppContext } from "../rpc/app-context.js";
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
    head: {
      glyph: { icon: "⇄" },
      name: { text: "fix-flaky", select: { target: { id: "ws-child", dir: "/w/child" } } },
      target: { text: "~/.config/doom" },
      standing: { standing: { case: "open", value: {} } },
      runtime: { startedAtMs: 940_000n },
    },
    border: "border" in init ? init.border : { tone: "red" },
    summary: "summary" in init ? undefined : { shortSummary: { text: "Fix the flaky test" }, longSummary: { markdown: "- It fixes the test.\n- It pins the backoff." } },
  });
}

/** Draw U as row ROWID against a context recording every SelectInEditor. */
function draw(u: FeedChild, rowId = "r|child|ws-child") {
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
  return { el: drawFeedChild(u, rowId, rowContext(ctx, mergeRow("m1"))), calls };
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

describe("drawFeedChild: the border", () => {
  it("draws the border in the child's canonical status color while open", () => {
    const el = draw(child()).el;
    expect([el.classList.contains("has-border"), el.classList.contains("tone-red")]).toEqual([true, true]);
  });

  it("draws no border once the child is not open", () => {
    const el = draw(child({ border: undefined })).el;
    expect(el.classList.contains("has-border")).toBe(false);
  });

  it("refuses a color the vocabulary does not declare", () => {
    expect(() => draw(child({ border: { tone: "chartreuse" } }))).toThrow(MalformedView);
  });
});

describe("drawFeedChild: the reader's fold", () => {
  it("draws the short summary folded, and the long one once opened", () => {
    const { el } = draw(child(), "r|child|fold-1");
    const summary = el.querySelector<HTMLElement>(".feed-child-summary");
    const folded = summary?.textContent;
    summary?.click();
    expect([folded, el.hasAttribute(CHILD_OPEN_ATTR), summary?.querySelectorAll("li").length]).toEqual(["Fix the flaky test", true, 2]);
  });

  it("keeps the reader's fold across a redraw of the same row", () => {
    draw(child(), "r|child|fold-2").el.querySelector<HTMLElement>(".feed-child-summary")?.click();
    const redrawn = draw(child(), "r|child|fold-2").el;
    expect(redrawn.hasAttribute(CHILD_OPEN_ATTR)).toBe(true);
  });

  it("draws the short summary open when the child has no long one", () => {
    const u = child();
    u.summary!.longSummary = undefined;
    const { el } = draw(u, "r|child|fold-3");
    el.querySelector<HTMLElement>(".feed-child-summary")?.click();
    expect(el.querySelector(".feed-child-summary")?.textContent).toBe("Fix the flaky test");
  });

  it("draws the head alone for a child with no summary", () => {
    const { el } = draw(child({ summary: undefined }));
    expect([el.classList.contains(CHILD_CLASS), el.querySelector(".feed-child-summary")]).toEqual([true, null]);
  });
});
