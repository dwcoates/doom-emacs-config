// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  ArrangeSidebarResponseSchema,
  type ArrangeSidebarRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_arrange_sidebar_pb";
import { disposeWorkspaceRoster, drawWorkspaceRoster } from "../../src/sidebar/roster.js";
import {
  bucketOf,
  familyTop,
  sameBucket,
  setWorkspaceOneShotRefusal,
  stampBucket,
} from "../../src/sidebar/subsections.js";
import {
  SetWorkspaceOneShotResponseSchema,
  type SetWorkspaceOneShotRequest,
  type SetWorkspaceOneShotResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_set_workspace_one_shot_pb";
import {
  RosterSubsectionCollapsedSchema,
  RosterSubsectionExpandedSchema,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { DRAG_GHOST_CLASS, DRAG_SLOT_CLASS, DROP_TARGET_CLASS, REARRANGED_ATTRIBUTE } from "../../src/sidebar/drag.js";
import type { RailContext, SidebarContext } from "../../src/sidebar/context.js";
import { frameQueue } from "../frame-queue.js";
import { queryAll } from "../../src/dom.js";
import { log } from "../../src/log.js";
import {
  UpdateSidebarViewResponseSchema,
  type UpdateSidebarViewRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_sidebar_view_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import { cascadedValue, inRail } from "../stylesheet.js";
import {
  appContext,
  drawnEntries,
  hiddenSection,
  primaryPointer,
  repoSection,
  roster,
  row,
  settleOwedDragClick,
  sidebarContext,
} from "./harness.js";
import { boxRect } from "../dom-rect.js";

/** Every ArrangeSidebar request the daemon received, and a context over it. */
function arranged(): { sc: RailContext & SidebarContext; sent: ArrangeSidebarRequest[] } {
  const sent: ArrangeSidebarRequest[] = [];
  const sc = sidebarContext(
    appContext({
      arrangeSidebar: (request) => {
        sent.push(request);
        return create(ArrangeSidebarResponseSchema, { result: { case: "success", value: { rosterRevision: 1n } } });
      },
    }),
  );
  return { sc, sent };
}

/** Draw ROSTER into the document, the rail's layer beside it; answers the open repository grouping. */
function mount(made: ReturnType<typeof roster>, sc: RailContext & SidebarContext): HTMLElement {
  const drawn = drawWorkspaceRoster(made, sc);
  document.body.replaceChildren(sc.arrange.status, drawn, sc.arrange.layer);
  return drawn.querySelector(".sb-sections") as HTMLElement;
}

const ROW_HEIGHT = 20;

/**
 * Lay LIST out at TOP, as a browser would: each row a ROW_HEIGHT band, a
 * subsection its header band over its rows, every item stacked in its
 * CURRENT place. Each box is measured when asked, so a slot that travels
 * (into a subsection, or out of one) moves and resizes the boxes around it.
 */
function layOut(list: HTMLElement, top: number): void {
  const isItem = (el: Element): boolean =>
    el.hasAttribute("data-roster-row") || el.classList.contains("sb-subsection") || el.hasAttribute("data-repository");
  const height = (el: Element): number => {
    if (el.classList.contains("sb-subsection")) {
      const rows = el.querySelector(":scope > .rows");
      return ROW_HEIGHT + (rows === null || el.classList.contains("folded") ? 0 : height(rows));
    }
    if (el.classList.contains("rows") || el === list) return [...el.children].filter(isItem).reduce((sum, item) => sum + height(item), 0);
    return ROW_HEIGHT;
  };
  const topOf = (el: Element): number => {
    if (el === list) return top;
    const parent = el.parentElement;
    if (parent === null) return top;
    // A subsection's rows sit under its header; its header at its top.
    if (parent.classList.contains("sb-subsection")) return topOf(parent) + (el.classList.contains("rows") ? ROW_HEIGHT : 0);
    let y = topOf(parent);
    for (const sibling of parent.children) {
      if (sibling === el) break;
      if (isItem(sibling)) y += height(sibling);
    }
    return y;
  };
  const box = (y: number, h: number): DOMRect => boxRect({ top: y, height: h, width: 200 });
  for (const el of [list, ...queryAll<HTMLElement>(list, ".rows, [data-roster-row], [data-repository], .sb-subsection, .sb-subsection-head")]) {
    Object.defineProperty(el, "getBoundingClientRect", {
      configurable: true,
      value: () => box(topOf(el), el === list ? Math.max(ROW_HEIGHT, height(el)) : el.classList.contains("sb-subsection-head") ? ROW_HEIGHT : height(el)),
    });
  }
}

/** The repository section's list: its rows and its subsections, in drawn order. */
function sectionList(pane: HTMLElement, repo = "repo-1"): HTMLElement {
  return pane.querySelector(`[data-repository='${repo}'] > .rows`) as HTMLElement;
}

const pointer = primaryPointer;

/** Press HANDLE at FROM, drag it to TO, and release. */
async function drag(handle: Element, from: number, to: number): Promise<void> {
  pointer("pointerdown", handle, from);
  pointer("pointermove", window, from + 1);
  pointer("pointermove", window, to);
  pointer("pointerup", window, to);
  await new Promise((resolve) => globalThis.setTimeout(resolve, 0));
}

function rowLine(pane: HTMLElement, id: string): HTMLElement {
  return pane.querySelector(`[data-roster-row='${id}'] > .row`) as HTMLElement;
}

beforeEach(() => {
  vi.stubGlobal("requestAnimationFrame", () => 0);
});
afterEach(() => {
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
  settleOwedDragClick();
  document.body.replaceChildren();
});

describe("a repository's subsections", () => {
  it("draw none for a repository with none", () => {
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" })] })] }), sidebarContext());
    expect(drawnEntries(sectionList(pane))).toEqual(["a"]);
  });

  it("draw in the section's list, each with its name, count and rows", () => {
    const pane = mount(
      roster({ repos: [repoSection({ id: "repo-1", subsections: [{ id: "sub-1", label: "later", rows: [row({ id: "ws-1" })] }] })] }),
      sidebarContext(),
    );
    const sub = pane.querySelector(".repo-section > .rows > .sb-subsection") as HTMLElement;
    expect([
      sub.getAttribute("data-subsection"),
      sub.querySelector(".sb-subsection-label")?.textContent,
      sub.querySelector(".sb-count")?.textContent,
      sub.querySelector(":scope > .rows > [data-roster-row='ws-1']") !== null,
    ]).toEqual(["sub-1", "later", "(1)", true]);
  });

  it("draw where the wire stands them, between two rows", () => {
    const pane = mount(
      roster({
        repos: [
          repoSection({
            id: "repo-1",
            rows: [row({ id: "a" }), row({ id: "b" }), row({ id: "c" })],
            subsections: [{ id: "foo", rows: [row({ id: "d" }), row({ id: "e" })], before: "b" }],
          }),
        ],
      }),
      sidebarContext(),
    );
    expect([
      drawnEntries(sectionList(pane)),
      drawnEntries(pane.querySelector("[data-subsection='foo'] > .rows") as HTMLElement),
    ]).toEqual([["a", "[foo]", "b", "c"], ["d", "e"]]);
  });

  it("draw an empty one as its header alone, marked empty, with no drop hint", () => {
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", subsections: [{ id: "sub-1" }] })] }), sidebarContext());
    const sub = pane.querySelector(".sb-subsection") as HTMLElement;
    expect([sub.hasAttribute("data-empty"), sub.querySelector(":scope > .rows")?.children.length, sub.textContent?.includes("drag")]).toEqual([
      true,
      0,
      false,
    ]);
  });

  it("do not mark a subsection with rows empty", () => {
    const pane = mount(
      roster({ repos: [repoSection({ id: "repo-1", subsections: [{ id: "sub-1", rows: [row({ id: "ws-1" })] }] })] }),
      sidebarContext(),
    );
    expect(pane.querySelector(".sb-subsection")?.hasAttribute("data-empty")).toBe(false);
  });

  it("delete through the header's remove control", async () => {
    const { sc, sent } = arranged();
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", subsections: [{ id: "sub-1" }] })] }), sc);
    (pane.querySelector("[data-subsection-delete='sub-1']") as HTMLElement).click();
    await new Promise((resolve) => globalThis.setTimeout(resolve, 0));
    expect(sent.map((r) => r.change.case)).toEqual(["deleteSubsection"]);
  });

  it("rename through a popup opened by a double-click on the name", async () => {
    const { sc, sent } = arranged();
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", subsections: [{ id: "sub-1", label: "old" }] })] }), sc);
    pane.querySelector(".sb-subsection-label")?.dispatchEvent(new MouseEvent("dblclick", { bubbles: true }));
    const input = sc.arrange.layer.querySelector("[data-subsection-name]") as HTMLInputElement;
    expect(input.value).toBe("old");
    input.value = "new";
    input.dispatchEvent(new KeyboardEvent("keydown", { key: "Enter" }));
    await new Promise((resolve) => globalThis.setTimeout(resolve, 0));
    const change = sent[0]?.change;
    expect(change?.case === "renameSubsection" ? [change.value.subsection?.id, change.value.name] : null).toEqual(["sub-1", "new"]);
  });
});

describe("the new-subsection popup", () => {
  function opened(): { sc: RailContext & SidebarContext; sent: ArrangeSidebarRequest[]; input: HTMLInputElement } {
    const { sc, sent } = arranged();
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1" })] }), sc);
    (pane.querySelector("[data-subsection-create='repo-1']") as HTMLElement).click();
    return { sc, sent, input: sc.arrange.layer.querySelector("[data-subsection-name]") as HTMLInputElement };
  }

  it("creates the subsection in its repository with the name typed, and closes", async () => {
    const { sc, sent, input } = opened();
    input.value = "  later ";
    (sc.arrange.layer.querySelector("[data-subsection-submit]") as HTMLElement).click();
    await new Promise((resolve) => globalThis.setTimeout(resolve, 0));
    const change = sent[0]?.change;
    expect(change?.case === "createSubsection" ? [change.value.repository?.id, change.value.name] : null).toEqual(["repo-1", "later"]);
    expect(sc.arrange.layer.querySelector("[data-subsection-popup]")).toBeNull();
  });

  it("sends nothing for a blank name", async () => {
    const { sc, sent, input } = opened();
    input.value = "   ";
    input.dispatchEvent(new KeyboardEvent("keydown", { key: "Enter" }));
    await new Promise((resolve) => globalThis.setTimeout(resolve, 0));
    expect([sent.length, sc.arrange.layer.querySelector("[data-subsection-popup]") !== null]).toEqual([0, true]);
  });

  it("closes on Escape", () => {
    const { sc, input } = opened();
    input.dispatchEvent(new KeyboardEvent("keydown", { key: "Escape" }));
    expect(sc.arrange.layer.querySelector("[data-subsection-popup]")).toBeNull();
  });

  it("closes on a press outside it", () => {
    const { sc } = opened();
    document.body.dispatchEvent(new PointerEvent("pointerdown", { bubbles: true }));
    expect(sc.arrange.layer.querySelector("[data-subsection-popup]")).toBeNull();
  });

  it("closes when the rail dismisses its dropdowns", () => {
    const { sc } = opened();
    sc.dropdowns.dismissAll("the page lost focus");
    expect(sc.arrange.layer.querySelector("[data-subsection-popup]")).toBeNull();
  });

  it("logs the rail's why when the rail dismisses it", () => {
    const { sc } = opened();
    const debug = vi.spyOn(log, "debug");
    sc.dropdowns.dismissAll("the selection moved");
    expect(debug).toHaveBeenCalledWith("closing the subsection name popup", {
      operation: "sidebar.subsection.popup-close",
      context: { why: "the selection moved" },
    });
  });

  it("closes when another dropdown opens", () => {
    const { sc } = opened();
    sc.dropdowns.opened({ kind: "test", element: document.createElement("div"), openers: [], close: () => undefined });
    expect(sc.arrange.layer.querySelector("[data-subsection-popup]")).toBeNull();
  });

  it("is let go of by the rail once it closed on its own", () => {
    const { sc, input } = opened();
    input.dispatchEvent(new KeyboardEvent("keydown", { key: "Escape" }));
    const debug = vi.spyOn(log, "debug");
    sc.dropdowns.dismissAll("the page lost focus");
    expect(debug).not.toHaveBeenCalled();
  });

  it("replaces an open one, which stops listening", () => {
    const { sc } = opened();
    (document.querySelector("[data-subsection-create='repo-1']") as HTMLElement).click();
    const debug = vi.spyOn(log, "debug");
    document.body.dispatchEvent(new PointerEvent("pointerdown", { bubbles: true }));
    expect([
      sc.arrange.layer.querySelectorAll("[data-subsection-popup]").length,
      debug.mock.calls.filter(([message]) => message === "closing the subsection name popup").length,
    ]).toEqual([0, 1]);
  });

  it("logs why it closed", () => {
    const { input } = opened();
    const debug = vi.spyOn(log, "debug");
    input.dispatchEvent(new KeyboardEvent("keydown", { key: "Escape" }));
    expect(debug).toHaveBeenCalledWith("closing the subsection name popup", {
      operation: "sidebar.subsection.popup-close",
      context: { why: "Escape" },
    });
  });

  it("stops listening once closed", () => {
    const { input } = opened();
    input.dispatchEvent(new KeyboardEvent("keydown", { key: "Escape" }));
    const debug = vi.spyOn(log, "debug");
    document.body.dispatchEvent(new PointerEvent("pointerdown", { bubbles: true }));
    expect(debug.mock.calls.filter(([message]) => message === "closing the subsection name popup")).toEqual([]);
  });

  it("keeps the popup open when the subsection is refused", async () => {
    const sc = sidebarContext(
      appContext({
        arrangeSidebar: () =>
          create(ArrangeSidebarResponseSchema, { result: { case: "error", value: { cause: { case: "unknownRepository", value: {} } } } }),
      }),
    );
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1" })] }), sc);
    (pane.querySelector("[data-subsection-create='repo-1']") as HTMLElement).click();
    (sc.arrange.layer.querySelector("[data-subsection-name]") as HTMLInputElement).value = "later";
    (sc.arrange.layer.querySelector("[data-subsection-submit]") as HTMLElement).click();
    await new Promise((resolve) => globalThis.setTimeout(resolve, 0));
    expect(sc.arrange.layer.querySelector("[data-subsection-popup] .refusal[data-arm='unknownRepository']")).not.toBeNull();
  });
});

describe("dragging a workspace row", () => {
  it("places it before the row it was dropped above", async () => {
    const { sc, sent } = arranged();
    const pane = mount(
      roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" }), row({ id: "b" }), row({ id: "c" })] })] }),
      sc,
    );
    layOut(pane.querySelector(".repo-section > .rows") as HTMLElement, 0);

    await drag(rowLine(pane, "c"), 50, 5);

    const change = sent[0]?.change;
    expect(
      change?.case === "placeWorkspace"
        ? [change.value.workspace?.id, change.value.into.case, change.value.at.case, change.value.at.case === "before" ? change.value.at.value.id : null]
        : null,
    ).toEqual(["c", "noSubsection", "before", "a"]);
  });

  it("places it last in the empty subsection it was dropped into", async () => {
    const { sc, sent } = arranged();
    const pane = mount(
      roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" }), row({ id: "b" })], subsections: [{ id: "sub-1" }] })] }),
      sc,
    );
    layOut(sectionList(pane), 0);

    await drag(rowLine(pane, "a"), 10, 50);

    const change = sent[0]?.change;
    expect(
      change?.case === "placeWorkspace" ? [change.value.workspace?.id, change.value.into.case === "subsection" ? change.value.into.value.id : null, change.value.at.case] : null,
    ).toEqual(["a", "sub-1", "end"]);
  });

  it("places it just above a subsection when dropped between the rows and the subsection", async () => {
    // Arrange -- a, b, [foo: x]: foo's header at 40, x at 60.
    const { sc, sent } = arranged();
    const pane = mount(
      roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" }), row({ id: "b" })], subsections: [{ id: "foo", rows: [row({ id: "x" })] }] })] }),
      sc,
    );
    layOut(sectionList(pane), 0);

    // Act
    await drag(rowLine(pane, "a"), 10, 35);

    // Assert
    const change = sent[0]?.change;
    expect(
      change?.case === "placeWorkspace"
        ? [change.value.into.case, change.value.at.case, change.value.at.case === "beforeSubsection" ? change.value.at.value.id : null]
        : null,
    ).toEqual(["noSubsection", "beforeSubsection", "foo"]);
  });

  it("places it into a subsection over its rows, before the row it was dropped above", async () => {
    // Arrange -- a, [foo: x y]: x at 40, y at 60.
    const { sc, sent } = arranged();
    const pane = mount(
      roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" })], subsections: [{ id: "foo", rows: [row({ id: "x" }), row({ id: "y" })] }] })] }),
      sc,
    );
    layOut(sectionList(pane), 0);

    // Act
    await drag(rowLine(pane, "a"), 10, 55);

    // Assert
    const change = sent[0]?.change;
    expect(
      change?.case === "placeWorkspace"
        ? [change.value.into.case === "subsection" ? change.value.into.value.id : null, change.value.at.case === "before" ? change.value.at.value.id : null]
        : null,
    ).toEqual(["foo", "y"]);
  });

  it("places a subsection's row out of it, at the bottom of the repository, when dropped below everything", async () => {
    // Arrange -- a, [foo: x].
    const { sc, sent } = arranged();
    const pane = mount(
      roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" })], subsections: [{ id: "foo", rows: [row({ id: "x" })] }] })] }),
      sc,
    );
    layOut(sectionList(pane), 0);

    // Act -- far below the section, where only its own list is nearest.
    await drag(rowLine(pane, "x"), 45, 200);

    // Assert
    const change = sent[0]?.change;
    expect(change?.case === "placeWorkspace" ? [change.value.workspace?.id, change.value.into.case, change.value.at.case] : null).toEqual([
      "x",
      "noSubsection",
      "end",
    ]);
  });

  it("lights the subsection it is over, and only while it is", () => {
    // Arrange -- a, b, [foo: x]: foo spans 40 to 80.
    const { sc } = arranged();
    const pane = mount(
      roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" }), row({ id: "b" })], subsections: [{ id: "foo", rows: [row({ id: "x" })] }] })] }),
      sc,
    );
    layOut(sectionList(pane), 0);
    const foo = pane.querySelector("[data-subsection='foo']") as HTMLElement;
    const lit = (): boolean => foo.classList.contains(DROP_TARGET_CLASS);

    // Act
    pointer("pointerdown", rowLine(pane, "a"), 10);
    pointer("pointermove", window, 15);
    const overTheRows = lit();
    pointer("pointermove", window, 45);
    const overTheHeader = lit();
    pointer("pointermove", window, 5);
    const backOverTheRows = lit();
    pointer("pointermove", window, 70);
    const overItsRows = lit();
    pointer("pointerup", window, 70);

    // Assert
    expect([overTheRows, overTheHeader, backOverTheRows, overItsRows, lit()]).toEqual([false, true, false, true, false]);
  });

  it("indents the slot under the subsection it would join", () => {
    // Arrange
    const { sc } = arranged();
    const pane = mount(
      roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" })], subsections: [{ id: "foo", rows: [row({ id: "x" })] }] })] }),
      sc,
    );
    layOut(sectionList(pane), 0);

    // Act
    pointer("pointerdown", rowLine(pane, "a"), 10);
    pointer("pointermove", window, 50);
    const parent = pane.querySelector("[data-roster-row='a']")?.parentElement;
    pointer("pointerup", window, 50);

    // Assert -- the slot stands in foo's rows, which the stylesheet indents.
    expect(parent?.matches("[data-subsection='foo'] > .rows")).toBe(true);
  });

  it("sends nothing when dropped where it started, and lets the hold go", async () => {
    const { sc, sent } = arranged();
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" }), row({ id: "b" })] })] }), sc);
    layOut(pane.querySelector(".repo-section > .rows") as HTMLElement, 0);

    await drag(rowLine(pane, "a"), 10, 8);

    expect([sent.length, sc.arrange.hold.holding()]).toEqual([0, false]);
  });

  it("does not lift under the threshold, so a click still selects", async () => {
    const { sc } = arranged();
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" })] })] }), sc);
    pointer("pointerdown", rowLine(pane, "a"), 10);
    pointer("pointermove", window, 12);
    expect([sc.arrange.layer.querySelector(`.${DRAG_GHOST_CLASS}`), sc.arrange.hold.holding()]).toEqual([null, false]);
    pointer("pointerup", window, 12);
  });

  it("lifts past the threshold: a copy follows the pointer and the row becomes its slot", () => {
    const { sc } = arranged();
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" }), row({ id: "b" })] })] }), sc);
    layOut(pane.querySelector(".repo-section > .rows") as HTMLElement, 0);
    pointer("pointerdown", rowLine(pane, "a"), 10);
    pointer("pointermove", window, 20);
    const ghost = sc.arrange.layer.querySelector(`.${DRAG_GHOST_CLASS}`);
    expect([
      ghost !== null,
      ghost?.hasAttribute("data-roster-row"),
      pane.querySelector("[data-roster-row='a']")?.classList.contains(DRAG_SLOT_CLASS),
      sc.arrange.hold.holding(),
    ]).toEqual([true, false, true, true]);
    pointer("pointerup", window, 20);
  });

  it("puts the row back on Escape and sends nothing", async () => {
    const { sc, sent } = arranged();
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" }), row({ id: "b" })] })] }), sc);
    const list = pane.querySelector(".repo-section > .rows") as HTMLElement;
    layOut(list, 0);
    pointer("pointerdown", rowLine(pane, "a"), 5);
    pointer("pointermove", window, 35);
    window.dispatchEvent(new KeyboardEvent("keydown", { key: "Escape" }));
    await new Promise((resolve) => globalThis.setTimeout(resolve, 0));
    expect([[...list.children].map((el) => el.getAttribute("data-roster-row")), sent.length, sc.arrange.hold.holding()]).toEqual([
      ["a", "b"],
      0,
      false,
    ]);
  });

  it("swallows the click the release leaves behind", async () => {
    const { sc } = arranged();
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" }), row({ id: "b" })] })] }), sc);
    layOut(pane.querySelector(".repo-section > .rows") as HTMLElement, 0);
    await drag(rowLine(pane, "b"), 30, 2);
    const clicked = vi.fn();
    document.body.addEventListener("click", clicked);
    rowLine(pane, "b").dispatchEvent(new MouseEvent("click", { bubbles: true }));
    expect(clicked).not.toHaveBeenCalled();
    document.body.removeEventListener("click", clicked);
  });

  it("leaves a press on the row's menu control to the control", () => {
    const { sc } = arranged();
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" })] })] }), sc);
    const menu = pane.querySelector("[data-roster-row='a'] > .row .sb-more") as HTMLElement;
    pointer("pointerdown", menu, 10);
    pointer("pointermove", window, 40);
    expect(sc.arrange.hold.holding()).toBe(false);
    pointer("pointerup", window, 40);
  });
});

describe("dragging near the Open region's edge", () => {
  it("scrolls the Open region toward the pointer, one step a frame, and stops when the drag ends", () => {
    // Arrange: the Open section's rows region is the box that scrolls.
    const frames = frameQueue();
    vi.stubGlobal("requestAnimationFrame", (fn: () => void) => frames.frame(fn));
    const { sc } = arranged();
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" }), row({ id: "b" })] })] }), sc);
    const scroller = pane;
    Object.defineProperty(scroller, "getBoundingClientRect", {
      value: () => boxRect({ height: 200, width: 200 }),
    });
    scroller.scrollTop = 0;
    layOut(pane.querySelector(".repo-section > .rows") as HTMLElement, 100);

    // Act: hold the drag in the bottom edge band, and run one frame.
    pointer("pointerdown", rowLine(pane, "a"), 105);
    pointer("pointermove", window, 195);
    const scrolledBy = (): number => scroller.scrollTop;
    frames.run();
    const afterOne = scrolledBy();
    pointer("pointerup", window, 195);
    frames.drain();

    // Assert: jsdom keeps scrollTop as written; one frame moved one step, and
    // the frame after the release moved nothing.
    expect([afterOne, scrolledBy()]).toEqual([8, 8]);
  });
});

describe("dragging a subsection or a repository", () => {
  it("moves a subsection before the one it was dropped above", async () => {
    const { sc, sent } = arranged();
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", subsections: [{ id: "sub-a" }, { id: "sub-b" }] })] }), sc);
    layOut(sectionList(pane), 0);

    await drag(pane.querySelector("[data-subsection='sub-b'] > .sb-subsection-head") as Element, 30, 2);

    const change = sent[0]?.change;
    expect(
      change?.case === "moveSubsection" ? [change.value.subsection?.id, change.value.to.case === "before" ? change.value.to.value.id : null] : null,
    ).toEqual(["sub-b", "sub-a"]);
  });

  it("stands a subsection before the row whose line it was dropped on", async () => {
    // Arrange -- the owner's example: a, b, c, then [foo: d e].
    const { sc, sent } = arranged();
    const pane = mount(
      roster({
        repos: [
          repoSection({
            id: "repo-1",
            rows: [row({ id: "a" }), row({ id: "b" }), row({ id: "c" })],
            subsections: [{ id: "foo", rows: [row({ id: "d" }), row({ id: "e" })] }],
          }),
        ],
      }),
      sc,
    );
    layOut(sectionList(pane), 0);

    // Act -- foo's header (at 60) onto b's line (20 to 40), above its middle.
    await drag(pane.querySelector("[data-subsection='foo'] > .sb-subsection-head") as Element, 65, 25);

    // Assert
    const change = sent[0]?.change;
    expect([
      change?.case === "moveSubsection"
        ? [change.value.subsection?.id, change.value.to.case, change.value.to.case === "beforeWorkspace" ? change.value.to.value.id : null]
        : null,
      drawnEntries(sectionList(pane)),
    ]).toEqual([["foo", "beforeWorkspace", "b"], ["a", "[foo]", "b", "c"]]);
  });

  it("moves a subsection to the bottom when dropped below every row", async () => {
    // Arrange -- a, [foo: d], b.
    const { sc, sent } = arranged();
    const pane = mount(
      roster({
        repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" }), row({ id: "b" })], subsections: [{ id: "foo", rows: [row({ id: "d" })], before: "b" }] })],
      }),
      sc,
    );
    layOut(sectionList(pane), 0);

    // Act
    await drag(pane.querySelector("[data-subsection='foo'] > .sb-subsection-head") as Element, 25, 79);

    // Assert
    const change = sent[0]?.change;
    expect(change?.case === "moveSubsection" ? [change.value.subsection?.id, change.value.to.case] : null).toEqual(["foo", "end"]);
  });

  it("moves a repository after every other when dropped below them", async () => {
    const { sc, sent } = arranged();
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1" }), repoSection({ id: "repo-2" })] }), sc);
    layOut(pane, 0);

    await drag(pane.querySelector(".repo-section > .repo-head") as Element, 5, 39);

    const change = sent[0]?.change;
    expect(change?.case === "moveRepository" ? [change.value.repository?.id, change.value.to.case] : null).toEqual(["repo-1", "end"]);
  });
});

describe("a nested row", () => {
  it("lifts its family's top row, so a child takes its ancestors", () => {
    // Arrange
    const { sc } = arranged();
    const pane = mount(
      roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a", children: [row({ id: "kid" })] }), row({ id: "b" })] })] }),
      sc,
    );
    layOut(sectionList(pane), 0);

    // Act
    pointer("pointerdown", rowLine(pane, "kid"), 25);
    pointer("pointermove", window, 60);
    const slot = pane.querySelector(`.${DRAG_SLOT_CLASS}`)?.getAttribute("data-roster-row");
    pointer("pointerup", window, 60);

    // Assert
    expect(slot).toBe("a");
  });

  it("places its family's top workspace into the subsection it is dropped in", async () => {
    // Arrange -- a (with kid), then [foo: x]: foo spans 20 to 60.
    const { sc, sent } = arranged();
    const pane = mount(
      roster({
        repos: [repoSection({ id: "repo-1", rows: [row({ id: "a", children: [row({ id: "kid" })] })], subsections: [{ id: "foo", rows: [row({ id: "x" })] }] })],
      }),
      sc,
    );
    layOut(sectionList(pane), 0);

    // Act
    await drag(rowLine(pane, "kid"), 10, 50);

    // Assert
    const change = sent[0]?.change;
    expect(
      change?.case === "placeWorkspace"
        ? [change.value.workspace?.id, change.value.into.case === "subsection" ? change.value.into.value.id : null]
        : null,
    ).toEqual(["a", "foo"]);
  });
});

describe("familyTop", () => {
  it.each([
    ["a top-level row is its own", "a", "a"],
    ["a child climbs to its parent", "kid", "a"],
    ["a grandchild climbs to the top", "grand", "a"],
  ])("%s", (_name, pressed, want) => {
    // Arrange
    const pane = mount(
      roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a", children: [row({ id: "kid", children: [row({ id: "grand" })] })] })] })] }),
      sidebarContext(),
    );

    // Act
    const got = familyTop(pane.querySelector(`[data-roster-row='${pressed}']`) as HTMLElement);

    // Assert
    expect(got?.getAttribute("data-roster-row")).toBe(want);
  });

  it("finds none for a row in no arranged list", () => {
    // Arrange
    const loose = document.createElement("div");
    loose.setAttribute("data-roster-row", "z");
    document.createElement("div").appendChild(loose);

    // Act / Assert
    expect(familyTop(loose)).toBeNull();
  });
});

describe("what is not a grip", () => {
  it("leaves a hidden workspace's row alone", () => {
    // Arrange
    const { sc } = arranged();
    const drawn = drawWorkspaceRoster(roster({ repos: [repoSection({ id: "repo-1" })], hidden: hiddenSection([row({ id: "h" })]) }), sc);
    document.body.replaceChildren(drawn, sc.arrange.layer);

    // Act
    pointer("pointerdown", drawn.querySelector("[data-roster-row='h'] > .row") as Element, 10);
    pointer("pointermove", window, 40);

    // Assert
    expect(sc.arrange.hold.holding()).toBe(false);
    pointer("pointerup", window, 40);
  });
});

describe("a drag across pushes", () => {
  it("drags a row a later push redrew in place", async () => {
    // Arrange: the second push changes row b alone, which is patched in place.
    const { sc, sent } = arranged();
    mount(roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" }), row({ id: "b" })] })] }), sc);
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" }), row({ id: "b", current: true })] })] }),
      sc,
    );
    const pane = drawn.querySelector(".sb-sections") as HTMLElement;
    layOut(pane.querySelector(".repo-section > .rows") as HTMLElement, 0);

    // Act
    await drag(rowLine(pane, "b"), 30, 2);

    // Assert
    const change = sent[0]?.change;
    expect(change?.case === "placeWorkspace" ? change.value.workspace?.id : null).toBe("b");
  });

  it("redraws a section a drag rearranged, though the next push leaves it as it was", () => {
    // Arrange: a drag that lifts the row and is cancelled marks its section.
    const { sc } = arranged();
    const made = roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" }), row({ id: "b" })] })] });
    const pane = mount(made, sc);
    const section = pane.querySelector(".repo-section") as HTMLElement;
    layOut(section.querySelector(":scope > .rows") as HTMLElement, 0);
    pointer("pointerdown", rowLine(pane, "a"), 5);
    pointer("pointermove", window, 35);
    window.dispatchEvent(new KeyboardEvent("keydown", { key: "Escape" }));
    const marked = section.hasAttribute(REARRANGED_ATTRIBUTE);

    // Act
    const drawn = drawWorkspaceRoster(made, sc);

    // Assert
    const redrawn = drawn.querySelector(".repo-section") as HTMLElement;
    expect([marked, redrawn === section, redrawn.hasAttribute(REARRANGED_ATTRIBUTE)]).toEqual([true, false, false]);
  });

  it("redraws a subsection's changed row alone, keeping its section", () => {
    // Arrange
    const sc = sidebarContext();
    const first = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" })], subsections: [{ id: "sub-1", rows: [row({ id: "b" })] }] })] }),
      sc,
    );
    const section = first.querySelector(".repo-section");
    const a = first.querySelector("[data-roster-row='a']");

    // Act
    const drawn = drawWorkspaceRoster(
      roster({
        repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" })], subsections: [{ id: "sub-1", rows: [row({ id: "b", current: true })] }] })],
      }),
      sc,
    );

    // Assert
    expect([
      drawn.querySelector(".repo-section") === section,
      drawn.querySelector("[data-roster-row='a']") === a,
      drawn.querySelector(".sb-subsection [data-roster-row='b']")?.hasAttribute("data-current"),
    ]).toEqual([true, true, true]);
    disposeWorkspaceRoster(sc);
  });

  it("redraws a section whose subsections changed", () => {
    // Arrange
    const sc = sidebarContext();
    const first = drawWorkspaceRoster(roster({ repos: [repoSection({ id: "repo-1", subsections: [{ id: "sub-1" }] })] }), sc);
    const section = first.querySelector(".repo-section");

    // Act
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1", subsections: [{ id: "sub-1" }, { id: "sub-2" }] })] }),
      sc,
    );

    // Assert
    expect([drawn.querySelector(".repo-section") === section, drawn.querySelectorAll(".sb-subsection").length]).toEqual([false, 2]);
    disposeWorkspaceRoster(sc);
  });
});

describe("a drag stays inside its repository", () => {
  it("files a row dropped over another repository's rows into its own repository", async () => {
    // Arrange: repo-2's rows sit right below repo-1's, under the drop point.
    const { sc, sent } = arranged();
    const pane = mount(
      roster({
        repos: [
          repoSection({ id: "repo-1", rows: [row({ id: "a" }), row({ id: "b" })] }),
          repoSection({ id: "repo-2", rows: [row({ id: "x" }), row({ id: "y" })] }),
        ],
      }),
      sc,
    );
    const [own, other] = queryAll<HTMLElement>(pane, ".repo-section > .rows");
    layOut(own, 0);
    layOut(other, 100);

    // Act: drag a far below its own list, over repo-2's rows.
    await drag(rowLine(pane, "a"), 5, 130);

    // Assert: the row never entered repo-2's list, and the placement names
    // a's own repository's list.
    const change = sent[0]?.change;
    expect([
      [...other.children].map((el) => el.getAttribute("data-roster-row")),
      change?.case === "placeWorkspace" ? [change.value.into.case, change.value.at.case] : null,
    ]).toEqual([["x", "y"], ["noSubsection", "end"]]);
  });

  it("keeps a subsection among its own repository's subsections", async () => {
    // Arrange
    const { sc, sent } = arranged();
    const pane = mount(
      roster({
        repos: [
          repoSection({ id: "repo-1", subsections: [{ id: "sub-a" }, { id: "sub-b" }] }),
          repoSection({ id: "repo-2", subsections: [{ id: "sub-x" }] }),
        ],
      }),
      sc,
    );
    const own = sectionList(pane, "repo-1");
    const other = sectionList(pane, "repo-2");
    layOut(own, 0);
    layOut(other, 100);

    // Act: drag sub-a over repo-2's subsections.
    await drag(pane.querySelector("[data-subsection='sub-a'] > .sb-subsection-head") as Element, 5, 110);

    // Assert
    const change = sent[0]?.change;
    expect([
      queryAll(other, ".sb-subsection").map((el) => el.getAttribute("data-subsection")),
      change?.case === "moveSubsection" ? [change.value.subsection?.id, change.value.to.case] : null,
    ]).toEqual([["sub-x"], ["sub-a", "end"]]);
  });
});

describe("a subsection's fold", () => {
  /** A context whose daemon records every UpdateSidebarView and answers success. */
  function viewing(): { sc: RailContext & SidebarContext; asked: UpdateSidebarViewRequest[] } {
    const asked: UpdateSidebarViewRequest[] = [];
    const sc = sidebarContext(
      appContext({
        updateSidebarView: (request) => {
          asked.push(request);
          return create(UpdateSidebarViewResponseSchema, { result: { case: "success", value: {} } });
        },
      }),
    );
    return { sc, asked };
  }

  /** Click EL as a pointer does: pressed and released first. */
  const click = (el: HTMLElement): void => {
    pointer("pointerdown", el, 0);
    pointer("pointerup", window, 0);
    el.click();
  };

  const subsectionOf = (collapsed: boolean) =>
    roster({ repos: [repoSection({ id: "repo-1", subsections: [{ id: "sub-1", rows: [row({ id: "ws-1" })], collapsed }] })] });

  it.each([true, false])("draws the subsection folded as the daemon says (%s)", (collapsed) => {
    // Act
    const pane = mount(subsectionOf(collapsed), sidebarContext());

    // Assert
    const sub = pane.querySelector(".sb-subsection") as HTMLElement;
    expect([sub.classList.contains("folded"), sub.querySelector("[data-section-fold]")?.getAttribute("data-folded")]).toEqual([
      collapsed,
      String(collapsed),
    ]);
  });

  it.each([
    { collapsed: false, fold: "collapse" },
    { collapsed: true, fold: "expand" },
  ])("asks for the other fold from its triangle ($fold)", async ({ collapsed, fold }) => {
    // Arrange
    const { sc, asked } = viewing();
    const pane = mount(subsectionOf(collapsed), sc);

    // Act
    click(pane.querySelector(".sb-subsection [data-section-fold]") as HTMLElement);
    await new Promise((resolve) => globalThis.setTimeout(resolve, 0));

    // Assert
    const change = asked[0]?.change;
    expect(
      change?.case === "foldSection" && change.value.section.case === "subsection"
        ? [change.value.section.value.id, change.value.fold.case]
        : change,
    ).toEqual(["sub-1", fold]);
  });

  it("is not folded by a click on its name, which renames on a double-click", async () => {
    // Arrange
    const { sc, asked } = viewing();
    const pane = mount(subsectionOf(false), sc);

    // Act
    click(pane.querySelector(".sb-subsection-label") as HTMLElement);
    await new Promise((resolve) => globalThis.setTimeout(resolve, 0));

    // Assert
    expect(asked).toEqual([]);
  });

  it("refuses a subsection with no fold as a malformed view", () => {
    // Arrange
    const made = subsectionOf(false);
    const entry = made.repository!.sections[0].entries.find((e) => e.entry.case === "subsection")!.entry;
    if (entry.case !== "subsection") throw new Error("the fixture has its subsection");
    entry.value.fold = { case: undefined };

    // Act / Assert
    expect(() => drawWorkspaceRoster(made, sidebarContext())).toThrow(MalformedView);
  });

  it.each([
    { collapsed: true, rows: "none", color: "var(--repo-head-collapsed)" },
    { collapsed: false, rows: "block", color: "var(--muted)" },
  ])("styles a subsection collapsed=$collapsed: rows $rows, header $color", ({ collapsed, rows, color }) => {
    // Arrange
    const drawn = drawWorkspaceRoster(subsectionOf(collapsed), sidebarContext());

    // Act / Assert
    inRail(drawn, (rail) => {
      const sub = rail.querySelector(".sb-subsection") as HTMLElement;
      expect([
        cascadedValue(sub.querySelector(":scope > .rows") as HTMLElement, "display"),
        cascadedValue(sub.querySelector(":scope > .sb-subsection-head") as HTMLElement, "color"),
      ]).toEqual([rows, color]);
    });
  });

  it.each([true, false])("draws a subsection's name in the canonical orange, collapsed=%s", (collapsed) => {
    // Arrange
    const drawn = drawWorkspaceRoster(subsectionOf(collapsed), sidebarContext());

    // Act / Assert
    inRail(drawn, (rail) => {
      expect(cascadedValue(rail.querySelector(".sb-subsection-label") as HTMLElement, "color")).toBe("var(--canonical-orange)");
    });
  });
});

describe("the one-shot subsection", () => {
  /** A context recording every ArrangeSidebar and SetWorkspaceOneShot ask, the mark answered by ANSWER. */
  function marking(answer: () => SetWorkspaceOneShotResponse = () =>
    create(SetWorkspaceOneShotResponseSchema, { result: { case: "success", value: { rosterRevision: 2n } } })) {
    const arranged: ArrangeSidebarRequest[] = [];
    const marked: SetWorkspaceOneShotRequest[] = [];
    const asked: UpdateSidebarViewRequest[] = [];
    const sc = sidebarContext(
      appContext({
        arrangeSidebar: (request) => {
          arranged.push(request);
          return create(ArrangeSidebarResponseSchema, { result: { case: "success", value: { rosterRevision: 1n } } });
        },
        setWorkspaceOneShot: (request) => {
          marked.push(request);
          return answer();
        },
        updateSidebarView: (request) => {
          asked.push(request);
          return create(UpdateSidebarViewResponseSchema, { result: { case: "success", value: {} } });
        },
      }),
    );
    return { sc, arranged, marked, asked };
  }

  /** a, b, [later: l], <one-shot: o p>. */
  const example = () =>
    roster({
      repos: [
        repoSection({
          id: "repo-1",
          rows: [row({ id: "a" }), row({ id: "b" })],
          subsections: [{ id: "later", rows: [row({ id: "l" })] }],
          oneShot: [row({ id: "o" }), row({ id: "p" })],
        }),
      ],
    });

  const oneShotOf = (pane: HTMLElement): HTMLElement => pane.querySelector(".sb-subsection[data-one-shot]") as HTMLElement;

  it("draws last in its repository's list, after every named subsection", () => {
    const pane = mount(example(), sidebarContext());
    expect(drawnEntries(sectionList(pane))).toEqual(["a", "b", "[later]", "<one-shot>"]);
  });

  it("draws its name, count and rows as a subsection does", () => {
    const sub = oneShotOf(mount(example(), sidebarContext()));
    expect([
      sub.querySelector(".sb-subsection-label")?.textContent,
      sub.querySelector(".sb-count")?.textContent,
      drawnEntries(sub.querySelector(":scope > .rows") as HTMLElement),
    ]).toEqual(["one-shot", "(2)", ["o", "p"]]);
  });

  it("states no subsection id, on itself or its rows", () => {
    const sub = oneShotOf(mount(example(), sidebarContext()));
    expect([sub.hasAttribute("data-subsection"), sub.querySelector(":scope > .rows")?.hasAttribute("data-subsection")]).toEqual([false, false]);
  });

  it("has no delete control", () => {
    const sub = oneShotOf(mount(example(), sidebarContext()));
    expect(sub.querySelector("[data-subsection-delete]")).toBeNull();
  });

  it("opens no rename popup on a double-click of its name", () => {
    const { sc } = marking();
    const sub = oneShotOf(mount(example(), sc));
    sub.querySelector(".sb-subsection-label")?.dispatchEvent(new MouseEvent("dblclick", { bubbles: true }));
    expect(sc.arrange.layer.querySelector("[data-subsection-popup]")).toBeNull();
  });

  it("asks nothing from its triangle, and logs that it never collapses", async () => {
    // Arrange
    const { sc, asked } = marking();
    const debug = vi.spyOn(log, "debug");
    const sub = oneShotOf(mount(example(), sc));

    // Act
    (sub.querySelector("[data-section-fold]") as HTMLElement).click();
    await new Promise((resolve) => globalThis.setTimeout(resolve, 0));

    // Assert
    expect([asked, sub.classList.contains("folded"), debug.mock.calls.some(([, f]) => f?.operation === "sidebar.fold.never-collapses")]).toEqual([
      [],
      false,
      true,
    ]);
  });

  it.each([
    { name: "a one-shot subsection that may collapse", oneShot: true, collapse: "collapsible" as const },
    { name: "a collapsed one-shot subsection", oneShot: true, collapse: null },
    { name: "a named subsection that never collapses", oneShot: false, collapse: "neverCollapses" as const },
  ])("refuses $name as a malformed view", ({ oneShot, collapse }) => {
    // Arrange
    const made = example();
    const entry = made.repository!.sections[0].entries.filter((e) => e.entry.case === "subsection")[oneShot ? 1 : 0].entry;
    if (entry.case !== "subsection") throw new Error("the fixture has its subsections");
    entry.value.fold =
      collapse === null
        ? { case: "collapsed", value: create(RosterSubsectionCollapsedSchema) }
        : { case: "expanded", value: create(RosterSubsectionExpandedSchema, { collapse: { case: collapse, value: {} } }) };

    // Act / Assert
    expect(() => drawWorkspaceRoster(made, sidebarContext())).toThrow(MalformedView);
  });

  it("does not drag by its header", async () => {
    // Arrange -- a 0, b 20, [later: l] 40, <one-shot: o p> 80.
    const { sc, arranged: sent, marked } = marking();
    const pane = mount(example(), sc);
    layOut(sectionList(pane), 0);

    // Act
    await drag(oneShotOf(pane).querySelector(".sb-subsection-head") as HTMLElement, 85, 5);

    // Assert
    expect([sent, marked, oneShotOf(pane).classList.contains(DRAG_SLOT_CLASS)]).toEqual([[], [], false]);
  });

  it("marks a row dropped into it one-shot, as the user, and places nothing", async () => {
    // Arrange -- o at 100, p at 120.
    const { sc, arranged: sent, marked } = marking();
    const pane = mount(example(), sc);
    layOut(sectionList(pane), 0);

    // Act
    await drag(rowLine(pane, "a"), 5, 115);

    // Assert
    expect([sent, marked.map((r) => [r.workspace?.id, r.oneShot, r.requester?.asker.case])]).toEqual([[], [["a", true, "user"]]]);
  });

  it("holds the drawing after a drop into it until the push at the answered revision", async () => {
    // Arrange
    const { sc } = marking();
    const pane = mount(example(), sc);
    layOut(sectionList(pane), 0);

    // Act
    await drag(rowLine(pane, "a"), 5, 115);

    // Assert
    expect(sc.arrange.hold.holding()).toBe(true);
  });

  it("draws a refused mark beside the arrangement status and lets the hold go", async () => {
    // Arrange
    const { sc } = marking(() =>
      create(SetWorkspaceOneShotResponseSchema, {
        result: { case: "error", value: { cause: { case: "ownerLands", value: {} } } },
      }),
    );
    const pane = mount(example(), sc);
    layOut(sectionList(pane), 0);

    // Act
    await drag(rowLine(pane, "a"), 5, 115);

    // Assert
    expect([document.querySelector('.refusal[data-arm="ownerLands"]') !== null, sc.arrange.hold.holding()]).toEqual([true, false]);
  });

  it("places a row dragged out of it where it was dropped, which clears the mark", async () => {
    // Arrange
    const { sc, arranged: sent, marked } = marking();
    const pane = mount(example(), sc);
    layOut(sectionList(pane), 0);

    // Act -- o, from 100 to above a.
    await drag(rowLine(pane, "o"), 105, 2);

    // Assert
    const change = sent[0]?.change;
    expect([
      marked,
      change?.case === "placeWorkspace"
        ? [change.value.workspace?.id, change.value.into.case, change.value.at.case === "before" ? change.value.at.value.id : null]
        : change,
    ]).toEqual([[], ["o", "noSubsection", "a"]]);
  });

  it("sends nothing for a row reordered inside it, whose order is the roster's", async () => {
    // Arrange
    const { sc, arranged: sent, marked } = marking();
    const pane = mount(example(), sc);
    layOut(sectionList(pane), 0);

    // Act -- p, from 120 to above o.
    await drag(rowLine(pane, "p"), 125, 101);

    // Assert
    expect([sent, marked, sc.arrange.hold.holding()]).toEqual([[], [], false]);
  });

  it("lands a row dropped below every entry at the end of the repository's own rows, never after it", async () => {
    // Arrange -- the section's own list; the one-shot subsection is no anchor.
    const { sc, arranged: sent } = marking();
    const pane = mount(
      roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" }), row({ id: "b" })], oneShot: [row({ id: "o" })] })] }),
      sc,
    );
    layOut(sectionList(pane), 0);

    // Act -- a, dropped just above the one-shot header.
    await drag(rowLine(pane, "a"), 5, 39);

    // Assert
    const change = sent[0]?.change;
    expect(change?.case === "placeWorkspace" ? [change.value.into.case, change.value.at.case] : change).toEqual(["noSubsection", "end"]);
  });
});

describe("setWorkspaceOneShotRefusal", () => {
  it.each([
    ["closed", "the workspace is closed"],
    ["noCreationJob", "a registered workspace cannot be a one-shot"],
    ["ownerLands", "its owner lands this workspace, so it is never a one-shot"],
    ["requesterMayNotSet", "only the workspace, its owner or the user may mark it"],
  ])("words %s", (arm, text) => {
    expect(setWorkspaceOneShotRefusal({ case: arm, value: {} } as never)).toBe(text);
  });
});

describe("a list's bucket", () => {
  const list = (): HTMLElement => document.createElement("div");

  it.each([
    { case: "none" as const },
    { case: "named" as const, id: "sub-1" },
    { case: "oneShot" as const },
  ])("reads back what was stamped ($case)", (bucket) => {
    const el = list();
    stampBucket(el, bucket);
    expect(bucketOf(el)).toEqual(bucket);
  });

  it("refuses a list stating both a subsection id and the one-shot subsection", () => {
    const el = list();
    stampBucket(el, { case: "named", id: "sub-1" });
    stampBucket(el, { case: "oneShot" });
    expect(() => bucketOf(el)).toThrow(/invariant/);
  });

  it("refuses a list stating no bucket", () => {
    expect(() => bucketOf(list())).toThrow(/invariant/);
  });

  it.each([
    [{ case: "none" as const }, { case: "none" as const }, true],
    [{ case: "named" as const, id: "a" }, { case: "named" as const, id: "a" }, true],
    [{ case: "named" as const, id: "a" }, { case: "named" as const, id: "b" }, false],
    [{ case: "oneShot" as const }, { case: "oneShot" as const }, true],
    [{ case: "oneShot" as const }, { case: "none" as const }, false],
  ])("compares %j and %j: %s", (a, b, same) => {
    expect(sameBucket(a, b)).toBe(same);
  });
});
