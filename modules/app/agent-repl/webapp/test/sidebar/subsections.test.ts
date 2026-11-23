// @vitest-environment jsdom
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  ArrangeSidebarResponseSchema,
  type ArrangeSidebarRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_arrange_sidebar_pb";
import { disposeWorkspaceRoster, drawWorkspaceRoster } from "../../src/sidebar/roster.js";
import {
  bucketOf,
  paintSubsectionWork,
  sameBucket,
  showsNoRows,
  stampBucket,
  workingTone,
  WORKING_TONE_ATTRIBUTE,
} from "../../src/sidebar/subsections.js";
import { createRosterDrawer } from "../../src/sidebar/roster.js";
import { NAME_INK_CLASS, NAME_INK_TEXT_ATTRIBUTE } from "../../src/sidebar/name-ink.js";
import { openRow } from "../roster-families.js";
import { RosterOpenRowsSchema, type RosterRow, type RosterSubsection } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import {
  SetWorkspaceOneShotResponseSchema,
  type SetWorkspaceOneShotRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_set_workspace_one_shot_pb";
import {
  SetWorkspaceParentResponseSchema,
  type SetWorkspaceParentRequest,
  type SetWorkspaceParentResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_set_workspace_parent_pb";
import {
  DRAG_GHOST_CLASS,
  DRAG_GHOST_INTO_CLASS,
  DRAG_SLOT_CLASS,
  DROP_INTO_CLASS,
  DROP_TARGET_CLASS,
  REARRANGED_ATTRIBUTE,
} from "../../src/drag.js";
import type { RailContext, SidebarContext } from "../../src/sidebar/context.js";
import { frameQueue } from "../frame-queue.js";
import { queryAll } from "../../src/dom.js";
import { spyLog } from "../log-spy.js";
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
import { boxAt, boxRect } from "../dom-rect.js";
import { withRepositoryBlocks } from "../roster-families.js";
import type { RosterOpenRow } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { REPOSITORY_BLOCK_CLASS } from "../../src/sidebar/row.js";
import { codeOf } from "../source-text.js";

/**
 * Every ArrangeSidebar and SetWorkspaceParent request the daemon received,
 * and a context over it, each answered with SET_PARENT (success when unsaid).
 */
function arranged(setParent?: () => SetWorkspaceParentResponse): {
  sc: RailContext & SidebarContext;
  sent: ArrangeSidebarRequest[];
  nested: SetWorkspaceParentRequest[];
} {
  const sent: ArrangeSidebarRequest[] = [];
  const nested: SetWorkspaceParentRequest[] = [];
  const sc = sidebarContext(
    appContext({
      arrangeSidebar: (request) => {
        sent.push(request);
        return create(ArrangeSidebarResponseSchema, { result: { case: "success", value: { rosterRevision: 1n } } });
      },
      setWorkspaceParent: (request) => {
        nested.push(request);
        return setParent?.() ?? create(SetWorkspaceParentResponseSchema, { result: { case: "success", value: { rosterRevision: 1n } } });
      },
    }),
  );
  return { sc, sent, nested };
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
function layOut(list: HTMLElement, top: number, lines = false): void {
  const isItem = (el: Element): boolean =>
    el.hasAttribute("data-roster-row") || el.classList.contains("sb-subsection") || el.hasAttribute("data-repository");
  /** A row's box of nested rows, stacked under its line when LINES lays lines out. */
  const kidsOf = (el: Element): Element | null => (lines ? el.querySelector(":scope > .kids") : null);
  const height = (el: Element): number => {
    if (el.hasAttribute("data-roster-row")) {
      const kids = kidsOf(el);
      return ROW_HEIGHT + (kids === null ? 0 : height(kids));
    }
    if (el.classList.contains("kids")) return [...el.children].filter(isItem).reduce((sum, item) => sum + height(item), 0);
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
    // A row's line at its top, its nested rows under the line.
    if (lines && parent.hasAttribute("data-roster-row")) return topOf(parent) + (el.classList.contains("kids") ? ROW_HEIGHT : 0);
    let y = topOf(parent);
    for (const sibling of parent.children) {
      if (sibling === el) break;
      if (isItem(sibling)) y += height(sibling);
    }
    return y;
  };
  const box = (y: number, h: number): DOMRect => boxRect({ top: y, height: h, width: 200 });
  for (const el of [list, ...queryAll<HTMLElement>(list, `.rows, [data-roster-row], [data-repository], .sb-subsection, .sb-subsection-head${lines ? ", .kids" : ""}`)]) {
    Object.defineProperty(el, "getBoundingClientRect", {
      configurable: true,
      value: () => box(topOf(el), el === list ? Math.max(ROW_HEIGHT, height(el)) : el.classList.contains("sb-subsection-head") ? ROW_HEIGHT : height(el)),
    });
  }
  if (!lines) return;
  for (const line of queryAll<HTMLElement>(list, "[data-roster-row] > .row")) {
    Object.defineProperty(line, "getBoundingClientRect", {
      configurable: true,
      value: () => box(topOf(line), ROW_HEIGHT),
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

  it("draw their count as the synchronously merging and every row, folded or not", () => {
    // Arrange
    const rows = [
      row({ id: "ws-1", status: { case: "thinking", value: {} } }),
      row({ id: "ws-2", status: { case: "merging", value: {} } }),
      row({ id: "ws-3" }),
    ];
    const sub = { id: "sub-1", rows, collapsed: true, unfolded: 0 };

    // Act
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", subsections: [sub] })] }), sidebarContext());

    // Assert
    const count = pane.querySelector(".sb-subsection .sb-count") as HTMLElement;
    expect([
      count.textContent,
      count.querySelector(".sb-count-merging")?.className,
    ]).toEqual(["(1/3)", "sb-count-merging tone-purple"]);
  });

  it("count a nested working child as no merge and draw no working number", () => {
    // Arrange
    const sub = { id: "sub-1", rows: [row({ id: "p", children: [row({ id: "c", status: { case: "thinking", value: {} } })] })] };

    // Act
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", subsections: [sub] })] }), sidebarContext());

    // Assert
    expect(pane.querySelector(".sb-subsection .sb-count")?.textContent).toBe("(1)");
  });

  it("refuse a count with more unfolded rows than rows", () => {
    // Arrange
    const sub = { id: "sub-1", rows: [row({ id: "ws-1" })], unfolded: 2 };

    // Act / Assert
    expect(() => mount(roster({ repos: [repoSection({ id: "repo-1", subsections: [sub] })] }), sidebarContext())).toThrow(MalformedView);
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

  it("draw no count for a subsection with no workspaces", () => {
    // Arrange / Act
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", subsections: [{ id: "sub-1" }] })] }), sidebarContext());

    // Assert
    expect(pane.querySelector(".sb-subsection .sb-count")).toBeNull();
  });

  it("draw the count of a subsection with workspaces, though none work", () => {
    // Arrange / Act
    const pane = mount(
      roster({ repos: [repoSection({ id: "repo-1", subsections: [{ id: "sub-1", rows: [row({ id: "ws-1" })] }] })] }),
      sidebarContext(),
    );

    // Assert
    expect(pane.querySelector(".sb-subsection .sb-count")?.textContent).toBe("(1)");
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
    const debug = spyLog("debug");
    sc.dropdowns.dismissAll("the selection moved");
    expect(debug).toHaveBeenCalledWith("closing the subsection name popup", {
      operation: "sidebar.subsection.popup-close",
      context: { why: "the selection moved", subject: "subsection" },
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
    const debug = spyLog("debug");
    sc.dropdowns.dismissAll("the page lost focus");
    expect(debug).not.toHaveBeenCalled();
  });

  it("replaces an open one, which stops listening", () => {
    const { sc } = opened();
    (document.querySelector("[data-subsection-create='repo-1']") as HTMLElement).click();
    const debug = spyLog("debug");
    document.body.dispatchEvent(new PointerEvent("pointerdown", { bubbles: true }));
    expect([
      sc.arrange.layer.querySelectorAll("[data-subsection-popup]").length,
      debug.mock.calls.filter(([message]) => message === "closing the subsection name popup").length,
    ]).toEqual([0, 1]);
  });

  it("logs why it closed", () => {
    const { input } = opened();
    const debug = spyLog("debug");
    input.dispatchEvent(new KeyboardEvent("keydown", { key: "Escape" }));
    expect(debug).toHaveBeenCalledWith("closing the subsection name popup", {
      operation: "sidebar.subsection.popup-close",
      context: { why: "Escape", subject: "subsection" },
    });
  });

  it("stops listening once closed", () => {
    const { input } = opened();
    input.dispatchEvent(new KeyboardEvent("keydown", { key: "Escape" }));
    const debug = spyLog("debug");
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

  /**
   * Drag row a over subsection foo, drawn with ROWS and COLLAPSED, to Y
   * (foo's header at 20), and read whether foo and the copy say the row goes
   * inside it; the row is still held.
   */
  function overFoo(rows: ReturnType<typeof row>[], collapsed: boolean, y = 25): { into: boolean; tucked: boolean; foo: HTMLElement } {
    const { sc } = arranged();
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" })], subsections: [{ id: "foo", rows, collapsed }] })] }), sc);
    layOut(sectionList(pane), 0);
    const foo = pane.querySelector("[data-subsection='foo']") as HTMLElement;
    pointer("pointerdown", rowLine(pane, "a"), 10);
    pointer("pointermove", window, y);
    const ghost = sc.arrange.layer.querySelector(`.${DRAG_GHOST_CLASS}`);
    return { into: foo.classList.contains(DROP_INTO_CLASS), tucked: ghost?.classList.contains(DRAG_GHOST_INTO_CLASS) ?? false, foo };
  }

  it("draws the row going inside a folded subsection it is over", () => {
    // Arrange / Act
    const { into, tucked } = overFoo([row({ id: "x" })], true);
    pointer("pointerup", window, 25);

    // Assert
    expect([into, tucked]).toEqual([true, true]);
  });

  it("draws the row going inside an empty subsection it is over", () => {
    // Arrange / Act
    const { into, tucked } = overFoo([], false);
    pointer("pointerup", window, 25);

    // Assert
    expect([into, tucked]).toEqual([true, true]);
  });

  it("does not draw the row going inside an open subsection that shows its rows", () => {
    // Arrange / Act
    const { into, tucked } = overFoo([row({ id: "x" })], false);
    pointer("pointerup", window, 25);

    // Assert
    expect([into, tucked]).toEqual([false, false]);
  });

  it("stops drawing the row going inside once it leaves the subsection", () => {
    // Arrange
    const { foo } = overFoo([], true);

    // Act -- back over row a's own place, above foo.
    pointer("pointermove", window, 5);
    const ghost = document.querySelector(`.${DRAG_GHOST_CLASS}`);
    const after = [foo.classList.contains(DROP_INTO_CLASS), ghost?.classList.contains(DRAG_GHOST_INTO_CLASS)];
    pointer("pointerup", window, 5);

    // Assert
    expect(after).toEqual([false, false]);
  });

  it("leaves no going-inside mark on the subsection once the row is dropped", () => {
    // Arrange
    const { foo } = overFoo([], true);

    // Act
    pointer("pointerup", window, 25);

    // Assert
    expect([foo.classList.contains(DROP_INTO_CLASS), foo.classList.contains(DROP_TARGET_CLASS)]).toEqual([false, false]);
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

describe("dragging a repository block", () => {
  /**
   * Repository repo-1's parents P (own child o, then a repo-2 block of b1 and
   * a repo-3 block of g1) and, when TWO, Q (a repo-2 block of q1); repo-2 and
   * repo-3 have sections of their own.
   */
  function blockRoster(two = false): ReturnType<typeof roster> {
    const section = repoSection({
      id: "repo-1",
      rows: [row({ id: "p", children: [row({ id: "o" })] }), ...(two ? [row({ id: "q" })] : [])],
    });
    const top = (index: number): RosterOpenRow => {
      const entry = section.entries[index]?.entry;
      if (entry?.case !== "row") throw new Error("test: a top-level row entry");
      return entry.value;
    };
    withRepositoryBlocks(top(0), [
      { repo: "repo-2", rows: [row({ id: "b1" })] },
      { repo: "repo-3", rows: [row({ id: "g1" })] },
    ]);
    if (two) withRepositoryBlocks(top(1), [{ repo: "repo-2", rows: [row({ id: "q1" })] }]);
    return roster({ repos: [section, repoSection({ id: "repo-2" }), repoSection({ id: "repo-3" })] });
  }

  /** P's family box and its two blocks, laid out: o at 20, the repo-2 block at 40, the repo-3 block at 70. */
  function layOutP(pane: HTMLElement): { kids: HTMLElement; blocks: HTMLElement[] } {
    const kids = pane.querySelector("[data-roster-row='p'] > .kids") as HTMLElement;
    const blocks = [...kids.querySelectorAll<HTMLElement>(`:scope > .${REPOSITORY_BLOCK_CLASS}`)];
    boxAt(kids, 20, 80);
    boxAt(blocks[0], 40, 30);
    boxAt(blocks[1], 70, 30);
    return { kids, blocks };
  }

  /** KIDS' drawn items, in order: a row by its id, a block by its repository. */
  function drawnKin(kids: HTMLElement): string[] {
    return [...kids.children].map((el) =>
      el.classList.contains(REPOSITORY_BLOCK_CLASS) ? `block:${el.getAttribute("data-repository-block")}` : (el.getAttribute("data-roster-row") ?? "?"),
    );
  }

  /** The block move SENT asked for: parent, repository, and where. */
  function blockMove(sent: ArrangeSidebarRequest[]): [string | undefined, string | undefined, string] | null {
    const change = sent[0]?.change;
    if (change?.case !== "moveRepositoryBlock") return null;
    const to = change.value.to;
    return [change.value.parent?.id, change.value.repository?.id, to.case === "before" ? `before:${to.value.id}` : String(to.case)];
  }

  it("moves a block before the block it was dropped above", async () => {
    // Arrange
    const { sc, sent } = arranged();
    const pane = mount(blockRoster(), sc);
    const { blocks } = layOutP(pane);

    // Act: the repo-3 block's divider (at 75) above the repo-2 block's middle.
    await drag(blocks[1].querySelector(".sb-repo-divider")!, 75, 45);

    // Assert
    expect(blockMove(sent)).toEqual(["p", "repo-3", "before:repo-2"]);
  });

  it("moves a block after every other when dropped below them", async () => {
    // Arrange
    const { sc, sent } = arranged();
    const pane = mount(blockRoster(), sc);
    const { blocks } = layOutP(pane);

    // Act
    await drag(blocks[0].querySelector(".sb-repo-divider")!, 45, 99);

    // Assert
    expect(blockMove(sent)).toEqual(["p", "repo-2", "end"]);
  });

  it("never stands a block among the parent's own-repository children", async () => {
    // Arrange
    const { sc, sent } = arranged();
    const pane = mount(blockRoster(), sc);
    const { kids, blocks } = layOutP(pane);

    // Act: the repo-3 block dragged over o's line (20 to 40).
    pointer("pointerdown", blocks[1].querySelector(".sb-repo-divider")!, 75);
    pointer("pointermove", window, 74);
    pointer("pointermove", window, 25);
    const during = drawnKin(kids);
    pointer("pointerup", window, 25);
    await new Promise((resolve) => globalThis.setTimeout(resolve, 0));

    // Assert: o stays first, and the block stands first among the blocks.
    expect([during, blockMove(sent)]).toEqual([["o", "block:repo-3", "block:repo-2"], ["p", "repo-3", "before:repo-2"]]);
  });

  it("keeps a block under its own parent, whatever parent it is dragged over", async () => {
    // Arrange: Q's family box sits below P's.
    const { sc, sent } = arranged();
    const pane = mount(blockRoster(true), sc);
    layOutP(pane);
    const other = pane.querySelector("[data-roster-row='q'] > .kids") as HTMLElement;
    boxAt(other, 120, 30);
    boxAt(other.querySelector(`.${REPOSITORY_BLOCK_CLASS}`)!, 120, 30);

    // Act: P's repo-2 block dragged over Q's block.
    await drag(pane.querySelector("[data-roster-row='p'] > .kids > .sb-repo-block .sb-repo-divider")!, 45, 125);

    // Assert
    expect([drawnKin(other), blockMove(sent)]).toEqual([["block:repo-2"], ["p", "repo-2", "end"]]);
  });

  it("drags a block row among its repository block's rows only, as any nested row", async () => {
    // Arrange: P's repo-2 block holds b1 and b2; the block stands at 40..70.
    const { sc, sent } = arranged();
    const made = blockRoster();
    const pane = mount(made, sc);
    const { blocks } = layOutP(pane);
    const second = document.createElement("div");
    second.setAttribute("data-roster-row", "b2");
    second.appendChild(Object.assign(document.createElement("div"), { className: "row" }));
    blocks[0].appendChild(second);
    boxAt(blocks[0], 40, 40);
    boxAt(pane.querySelector("[data-roster-row='b1']")!, 55, 15);
    boxAt(second, 70, 15);
    boxAt(rowLine(pane, "b1"), 55, 15);

    // Act: b1 dragged below b2.
    await drag(rowLine(pane, "b1"), 60, 80);

    // Assert
    const change = sent[0]?.change;
    expect(change?.case === "moveChild" ? [change.value.workspace?.id, change.value.to.case] : null).toEqual(["b1", "end"]);
  });
});

describe("a nested row", () => {
  /** Repository repo-1's family a with children k1, k2, k3 (k2 with a child g), then top-level b. */
  function familyRoster(): ReturnType<typeof roster> {
    return roster({
      repos: [
        repoSection({
          id: "repo-1",
          rows: [row({ id: "a", children: [row({ id: "k1" }), row({ id: "k2", children: [row({ id: "g" })] }), row({ id: "k3" })] }), row({ id: "b" })],
        }),
      ],
    });
  }

  /** Lay a's box of nested rows out under a's line at 20: k1 at 20, k2 at 40 (g at 60), k3 at 80. */
  function layOutKids(pane: HTMLElement): HTMLElement {
    const kids = pane.querySelector("[data-roster-row='a'] > .kids") as HTMLElement;
    boxAt(kids, 20, 80);
    boxAt(pane.querySelector("[data-roster-row='k1']")!, 20, 20);
    boxAt(pane.querySelector("[data-roster-row='k2']")!, 40, 40);
    boxAt(pane.querySelector("[data-roster-row='g']")!, 60, 20);
    boxAt(pane.querySelector("[data-roster-row='k3']")!, 80, 20);
    for (const id of ["k1", "k2", "g", "k3"]) boxAt(rowLine(pane, id), Number(pane.querySelector(`[data-roster-row='${id}']`)!.getBoundingClientRect().top), 20);
    return kids;
  }

  /** The child move SENT asked for: the child and where. */
  function childMove(sent: ArrangeSidebarRequest[]): [string | undefined, string] | null {
    const change = sent[0]?.change;
    if (change?.case !== "moveChild") return null;
    const to = change.value.to;
    return [change.value.workspace?.id, to.case === "before" ? `before:${to.value.id}` : String(to.case)];
  }

  it("lifts only itself, so its parent's family stays where it is", () => {
    // Arrange
    const { sc } = arranged();
    const pane = mount(familyRoster(), sc);
    layOutKids(pane);

    // Act
    pointer("pointerdown", rowLine(pane, "k1"), 25);
    pointer("pointermove", window, 26);
    pointer("pointermove", window, 50);
    const slot = pane.querySelector(`.${DRAG_SLOT_CLASS}`)?.getAttribute("data-roster-row");
    pointer("pointerup", window, 50);

    // Assert
    expect(slot).toBe("k1");
  });

  it.each([
    { name: "before an earlier sibling", id: "k3", from: 85, to: 22, want: ["k3", "before:k1"] },
    { name: "before a later sibling", id: "k1", from: 25, to: 82, want: ["k1", "before:k3"] },
    { name: "after every sibling", id: "k1", from: 25, to: 99, want: ["k1", "end"] },
  ])("reorders $id among its parent's children: $name", async ({ id, from, to, want }) => {
    // Arrange
    const { sc, sent } = arranged();
    const pane = mount(familyRoster(), sc);
    layOutKids(pane);

    // Act
    await drag(rowLine(pane, id), from, to);

    // Assert
    expect(childMove(sent)).toEqual(want);
  });

  it("takes its own descendants along", async () => {
    // Arrange
    const { sc, sent } = arranged();
    const pane = mount(familyRoster(), sc);
    layOutKids(pane);

    // Act
    pointer("pointerdown", rowLine(pane, "k2"), 45);
    pointer("pointermove", window, 46);
    pointer("pointermove", window, 22);
    const slot = pane.querySelector(`.${DRAG_SLOT_CLASS}`);
    const lifted = slot?.querySelector("[data-roster-row='g']") !== null;
    pointer("pointerup", window, 22);
    await new Promise((resolve) => globalThis.setTimeout(resolve, 0));

    // Assert
    expect([lifted, childMove(sent)]).toEqual([true, ["k2", "before:k1"]]);
  });

  /** The top-level retargets NESTED asked for, by workspace. */
  function freed(nested: SetWorkspaceParentRequest[]): (string | undefined)[] {
    return nested.filter((request) => request.parent.case === "topLevel").map((request) => request.workspace?.id);
  }

  /** The placement SENT asked for: the workspace, where, and before what. */
  function placed(sent: ArrangeSidebarRequest[]): [string | undefined, string, string] | null {
    const change = sent[0]?.change;
    if (change?.case !== "placeWorkspace") return null;
    const { into, at } = change.value;
    return [
      change.value.workspace?.id,
      into.case === "subsection" ? `subsection:${into.value.id}` : String(into.case),
      at.case === "before" || at.case === "beforeSubsection" ? `${at.case}:${at.value.id}` : String(at.case),
    ];
  }

  /** familyRoster laid out by its lines: a 0 to 20, k1 20, k2 40 (g 60), k3 80, b 100 to 120. */
  function laidOutFamily(setParent?: () => SetWorkspaceParentResponse): ReturnType<typeof arranged> & { pane: HTMLElement } {
    const made = arranged(setParent);
    const pane = mount(familyRoster(), made.sc);
    layOut(sectionList(pane), 0, true);
    return { ...made, pane };
  }

  it("leaves its parent for the top level, standing where it was dropped among the top-level rows", async () => {
    // Arrange
    const { pane, sent, nested } = laidOutFamily();

    // Act: k1 dropped on b's line's top quarter.
    await drag(rowLine(pane, "k1"), 25, 103);

    // Assert
    expect([freed(nested), placed(sent), sent.length]).toEqual([["k1"], ["k1", "noSubsection", "before:b"], 1]);
  });

  it("asks the placement only after the retarget, so it places the freed workspace and not its old family", async () => {
    // Arrange
    const order: string[] = [];
    const sc = sidebarContext(
      appContext({
        arrangeSidebar: () => {
          order.push("arrangeSidebar");
          return create(ArrangeSidebarResponseSchema, { result: { case: "success", value: { rosterRevision: 2n } } });
        },
        setWorkspaceParent: () => {
          order.push("setWorkspaceParent");
          return create(SetWorkspaceParentResponseSchema, { result: { case: "success", value: { rosterRevision: 1n } } });
        },
      }),
    );
    const pane = mount(familyRoster(), sc);
    layOut(sectionList(pane), 0, true);

    // Act
    await drag(rowLine(pane, "k1"), 25, 103);

    // Assert
    expect(order).toEqual(["setWorkspaceParent", "arrangeSidebar"]);
  });

  it("never nests under its own parent: over the parent's line it stands top-level just before the parent", async () => {
    // Arrange
    const { pane, sent, nested } = laidOutFamily();

    // Act: k1 dropped on the middle of a's line.
    await drag(rowLine(pane, "k1"), 25, 10);

    // Assert
    expect([freed(nested), placed(sent)]).toEqual([["k1"], ["k1", "noSubsection", "before:a"]]);
  });

  it.each([
    { name: "another top-level row", to: 110, want: ["k1", "b"] },
    { name: "a sibling", to: 90, want: ["k1", "k3"] },
  ])("becomes the child of $name whose line's middle it was dropped on", async ({ to, want }) => {
    // Arrange
    const { pane, sent, nested } = laidOutFamily();

    // Act
    await drag(rowLine(pane, "k1"), 25, to);

    // Assert
    const request = nested[0];
    expect([
      request?.parent.case === "childOf" ? [request.workspace?.id, request.parent.value.parent?.id] : null,
      nested.length,
      sent.length,
    ]).toEqual([want, 1, 0]);
  });

  it("never nests under its own descendants", async () => {
    // Arrange
    const { pane, nested } = laidOutFamily();
    const g = pane.querySelector("[data-roster-row='g']") as HTMLElement;

    // Act: k2 held over the middle of its own child g's line.
    pointer("pointerdown", rowLine(pane, "k2"), 45);
    pointer("pointermove", window, 46);
    pointer("pointermove", window, 70);
    const gLit = g.classList.contains(DROP_TARGET_CLASS);
    pointer("pointerup", window, 70);
    await new Promise((resolve) => globalThis.setTimeout(resolve, 0));

    // Assert
    expect([gLit, nested.filter((request) => request.parent.case === "childOf")]).toEqual([false, []]);
  });

  it("leaves its parent for a subsection, standing there top-level", async () => {
    // Arrange -- a 0 to 40 (k1 20 to 40), b 40 to 60, [foo: x]: foo's header 60, x 80 to 100.
    const { sc, sent, nested } = arranged();
    const pane = mount(
      roster({
        repos: [
          repoSection({
            id: "repo-1",
            rows: [row({ id: "a", children: [row({ id: "k1" })] }), row({ id: "b" })],
            subsections: [{ id: "foo", rows: [row({ id: "x" })] }],
          }),
        ],
      }),
      sc,
    );
    layOut(sectionList(pane), 0, true);
    const foo = pane.querySelector("[data-subsection='foo']") as HTMLElement;

    // Act: k1 held on x's line's top quarter, then dropped there.
    pointer("pointerdown", rowLine(pane, "k1"), 25);
    pointer("pointermove", window, 26);
    pointer("pointermove", window, 82);
    const fooLit = foo.classList.contains(DROP_TARGET_CLASS);
    pointer("pointerup", window, 82);
    await new Promise((resolve) => globalThis.setTimeout(resolve, 0));

    // Assert
    expect([fooLit, freed(nested), placed(sent)]).toEqual([true, ["k1"], ["k1", "subsection:foo", "before:x"]]);
  });

  it("asks no placement when the retarget to the top level is refused, and draws the refusal", async () => {
    // Arrange
    const { pane, sc, sent, nested } = laidOutFamily(() =>
      create(SetWorkspaceParentResponseSchema, { result: { case: "error", value: { cause: { case: "coldGate", value: {} } } } }),
    );

    // Act
    await drag(rowLine(pane, "k1"), 25, 103);

    // Assert
    expect([
      freed(nested),
      sent.length,
      sc.arrange.status.parentElement?.querySelector(".refusal")?.getAttribute("data-arm"),
      sc.arrange.hold.holding(),
    ]).toEqual([["k1"], 0, "coldGate", false]);
  });

  it("goes top-level in its own repository's section when it is a child of another repository, marking both sections", async () => {
    // Arrange -- repo-1's p (0 to 60) holds a repo-2 block (20 to 60) of b1 (40 to 60); repo-2's section holds z at 200.
    const { sc, sent, nested } = arranged();
    const section = repoSection({ id: "repo-1", rows: [row({ id: "p" })] });
    const top = section.entries[0]?.entry;
    if (top?.case !== "row") throw new Error("test: a top-level row entry");
    withRepositoryBlocks(top.value, [{ repo: "repo-2", rows: [row({ id: "b1" })] }]);
    const pane = mount(roster({ repos: [section, repoSection({ id: "repo-2", rows: [row({ id: "z" })] })] }), sc);
    boxAt(sectionList(pane), 0, 60);
    boxAt(pane.querySelector("[data-roster-row='p']")!, 0, 60);
    boxAt(rowLine(pane, "p"), 0, 20);
    boxAt(pane.querySelector(`.${REPOSITORY_BLOCK_CLASS}`)!, 20, 40);
    boxAt(pane.querySelector("[data-roster-row='b1']")!, 40, 20);
    boxAt(rowLine(pane, "b1"), 40, 20);
    boxAt(sectionList(pane, "repo-2"), 200, 20);
    boxAt(pane.querySelector("[data-roster-row='z']")!, 200, 20);
    boxAt(rowLine(pane, "z"), 200, 20);
    const sections = ["repo-1", "repo-2"].map((id) => pane.querySelector(`[data-repository='${id}']`) as HTMLElement);

    // Act: b1 dropped on z's line's top quarter.
    pointer("pointerdown", rowLine(pane, "b1"), 45);
    pointer("pointermove", window, 46);
    pointer("pointermove", window, 202);
    const marked = sections.map((el) => el.hasAttribute(REARRANGED_ATTRIBUTE));
    pointer("pointerup", window, 202);
    await new Promise((resolve) => globalThis.setTimeout(resolve, 0));

    // Assert
    expect([marked, freed(nested), placed(sent)]).toEqual([[true, true], ["b1"], ["b1", "noSubsection", "before:z"]]);
  });

  it("leaves a press on a top-level row lifting its own whole family", () => {
    // Arrange
    const { sc } = arranged();
    const pane = mount(familyRoster(), sc);
    layOut(sectionList(pane), 0);

    // Act
    pointer("pointerdown", rowLine(pane, "a"), 5);
    pointer("pointermove", window, 6);
    pointer("pointermove", window, 15);
    const slot = pane.querySelector(`.${DRAG_SLOT_CLASS}`)?.getAttribute("data-roster-row");
    pointer("pointerup", window, 15);

    // Assert
    expect(slot).toBe("a");
  });
});

describe("the placement a dropped row asks for", () => {
  it("is built in one place, so the top-level row's drop and the freed child's drop ask the same shape", () => {
    // Arrange
    const source = codeOf(readFileSync(path.join(path.dirname(fileURLToPath(import.meta.url)), "../../src/sidebar/subsections.ts"), "utf8"));

    // Act
    const built = source.match(/case: "placeWorkspace"/g) ?? [];
    const asked = source.match(/placement\(workspace, l, before, refs\)/g) ?? [];

    // Assert
    expect([built.length, asked.length]).toEqual([1, 2]);
  });
});

describe("showsNoRows", () => {
  /** A subsection drawn with ROWS and COLLAPSED. */
  function drawnFoo(rows: ReturnType<typeof row>[], collapsed: boolean): HTMLElement {
    const { sc } = arranged();
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", subsections: [{ id: "foo", rows, collapsed }] })] }), sc);
    return pane.querySelector("[data-subsection='foo']") as HTMLElement;
  }

  it.each([
    { name: "a folded subsection with rows", rows: [row({ id: "x" })], collapsed: true, want: true },
    { name: "an open empty subsection", rows: [], collapsed: false, want: true },
    { name: "a folded empty subsection", rows: [], collapsed: true, want: true },
    { name: "an open subsection with rows", rows: [row({ id: "x" })], collapsed: false, want: false },
  ])("answers $want for $name", ({ rows, collapsed, want }) => {
    // Arrange
    const foo = drawnFoo(rows, collapsed);

    // Act / Assert
    expect(showsNoRows(foo)).toBe(want);
  });

  it("throws for an element that is no subsection", () => {
    // Arrange / Act / Assert
    expect(() => showsNoRows(document.createElement("div"))).toThrow("invariant: a row drag's target is a subsection");
  });
});

describe("dropping a row onto another row", () => {
  /** Rows a, b and c, each line laid out: a 0 to 20, b 20 to 40, c 40 to 60. */
  function threeRows(setParent?: () => SetWorkspaceParentResponse): ReturnType<typeof arranged> & { pane: HTMLElement } {
    const made = arranged(setParent);
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" }), row({ id: "b" }), row({ id: "c" })] })] }), made.sc);
    layOut(sectionList(pane), 0, true);
    return { ...made, pane };
  }

  /** The parent and the workspace a SetWorkspaceParent request names. */
  function nesting(request: SetWorkspaceParentRequest | undefined): [string | undefined, string | undefined] | null {
    return request?.parent.case === "childOf" ? [request.workspace?.id, request.parent.value.parent?.id] : null;
  }

  it("makes it a child of the row whose line's middle it was dropped on", async () => {
    // Arrange
    const { pane, sent, nested } = threeRows();

    // Act
    await drag(rowLine(pane, "c"), 50, 10);

    // Assert
    expect([nesting(nested[0]), nested.length, sent.length]).toEqual([["c", "a"], 1, 0]);
  });

  it("keeps where the dropped workspace lands", async () => {
    // Arrange
    const { pane, nested } = threeRows();

    // Act
    await drag(rowLine(pane, "c"), 50, 10);

    // Assert
    expect(nested[0]?.landingTarget).toBeUndefined();
  });

  it("makes it a child of a row below where it started", async () => {
    // Arrange
    const { pane, nested } = threeRows();

    // Act
    await drag(rowLine(pane, "a"), 10, 50);

    // Assert
    expect(nesting(nested[0])).toEqual(["a", "c"]);
  });

  it("places it before the row whose line's top quarter it was dropped on", async () => {
    // Arrange
    const { pane, sent, nested } = threeRows();

    // Act
    await drag(rowLine(pane, "c"), 50, 22);

    // Assert
    const change = sent[0]?.change;
    expect([
      change?.case === "placeWorkspace" && change.value.at.case === "before" ? change.value.at.value.id : null,
      nested.length,
    ]).toEqual(["b", 0]);
  });

  it("lights the row it would become a child of, and only while it would", () => {
    // Arrange
    const { pane } = threeRows();
    const a = pane.querySelector("[data-roster-row='a']") as HTMLElement;
    const lit = (): boolean => a.classList.contains(DROP_TARGET_CLASS);

    // Act
    pointer("pointerdown", rowLine(pane, "c"), 50);
    pointer("pointermove", window, 10);
    const overTheMiddle = lit();
    pointer("pointermove", window, 2);
    const overTheTopQuarter = lit();
    pointer("pointerup", window, 2);

    // Assert
    expect([overTheMiddle, overTheTopQuarter, lit()]).toEqual([true, false, false]);
  });

  it("never draws the row it would become a child of as taking the row inside unseen", () => {
    // Arrange
    const { pane } = threeRows();
    const a = pane.querySelector("[data-roster-row='a']") as HTMLElement;

    // Act
    pointer("pointerdown", rowLine(pane, "c"), 50);
    pointer("pointermove", window, 10);
    const ghost = document.querySelector(`.${DRAG_GHOST_CLASS}`);
    const drawn = [a.classList.contains(DROP_TARGET_CLASS), a.classList.contains(DROP_INTO_CLASS), ghost?.classList.contains(DRAG_GHOST_INTO_CLASS)];
    pointer("pointerup", window, 10);

    // Assert
    expect(drawn).toEqual([true, false, false]);
  });

  it("stands the slot among the children of the row it would become a child of", () => {
    // Arrange
    const { pane } = threeRows();

    // Act
    pointer("pointerdown", rowLine(pane, "c"), 50);
    pointer("pointermove", window, 10);
    const parent = pane.querySelector("[data-roster-row='c']")?.parentElement;
    pointer("pointerup", window, 10);

    // Assert
    expect(parent?.matches("[data-roster-row='a'] > .kids")).toBe(true);
  });

  it("stands the slot before the children a row already has", () => {
    // Arrange
    const { sc } = arranged();
    const pane = mount(
      roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a", children: [row({ id: "kid" })] }), row({ id: "b" })] })] }),
      sc,
    );
    layOut(sectionList(pane), 0, true);

    // Act -- a's line 0 to 20, kid 20 to 40, b 40 to 60.
    pointer("pointerdown", rowLine(pane, "b"), 50);
    pointer("pointermove", window, 10);
    const kids = [...(pane.querySelector("[data-roster-row='a'] > .kids")?.children ?? [])].map((el) => el.getAttribute("data-roster-row"));
    pointer("pointerup", window, 10);

    // Assert
    expect(kids).toEqual(["b", "kid"]);
  });

  it("keeps nesting while the pointer is over the slot the rows above moved onto", () => {
    // Arrange -- dragging a, from above, onto c's line moves b and c up a row.
    const { pane } = threeRows();
    const c = pane.querySelector("[data-roster-row='c']") as HTMLElement;

    // Act
    pointer("pointerdown", rowLine(pane, "a"), 10);
    pointer("pointermove", window, 50);
    pointer("pointermove", window, 51);
    const stillLit = c.classList.contains(DROP_TARGET_CLASS);
    const parent = pane.querySelector("[data-roster-row='a']")?.parentElement;
    pointer("pointerup", window, 51);

    // Assert
    expect([stillLit, parent?.matches("[data-roster-row='c'] > .kids")]).toEqual([true, true]);
  });

  it("never nests a row among its own family", () => {
    // Arrange
    const { sc } = arranged();
    const pane = mount(
      roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a", children: [row({ id: "kid" })] }), row({ id: "b" })] })] }),
      sc,
    );
    layOut(sectionList(pane), 0, true);
    const kid = pane.querySelector("[data-roster-row='kid']") as HTMLElement;

    // Act -- the press on kid lifts a; the pointer stays over kid's line.
    pointer("pointerdown", rowLine(pane, "kid"), 30);
    pointer("pointermove", window, 35);
    const kidLit = kid.classList.contains(DROP_TARGET_CLASS);
    pointer("pointerup", window, 35);

    // Assert
    expect([kidLit, kid.querySelector(":scope > .kids")]).toEqual([false, null]);
  });

  it("holds the drawing until the revision the nesting answered", async () => {
    // Arrange
    const { pane, sc } = threeRows(() =>
      create(SetWorkspaceParentResponseSchema, { result: { case: "success", value: { rosterRevision: 9n } } }),
    );

    // Act
    await drag(rowLine(pane, "c"), 50, 10);

    // Assert
    expect(sc.arrange.hold.holding()).toBe(true);
  });

  it("draws a refused nesting beside the rail's arrangement status and lets the hold go", async () => {
    // Arrange
    const { pane, sc } = threeRows(() => create(SetWorkspaceParentResponseSchema, { result: { case: "error", value: { cause: { case: "cycle", value: {} } } } }));

    // Act
    await drag(rowLine(pane, "c"), 50, 10);

    // Assert
    expect([sc.arrange.status.parentElement?.querySelector(".refusal")?.getAttribute("data-arm"), sc.arrange.hold.holding()]).toEqual([
      "cycle",
      false,
    ]);
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

  it("is folded by a click on its name, the whole header folding it", async () => {
    // Arrange
    const { sc, asked } = viewing();
    const pane = mount(subsectionOf(false), sc);

    // Act
    click(pane.querySelector(".sb-subsection-label") as HTMLElement);
    await new Promise((resolve) => globalThis.setTimeout(resolve, 0));

    // Assert
    const change = asked[0]?.change;
    expect(
      change?.case === "foldSection" && change.value.section.case === "subsection"
        ? [change.value.section.value.id, change.value.fold.case]
        : change,
    ).toEqual(["sub-1", "collapse"]);
  });

  it("refuses a subsection with no fold as a malformed view", () => {
    // Arrange
    const made = subsectionOf(false);
    const entry = made.workspaces.repository!.sections[0].entries.find((e) => e.entry.case === "subsection")!.entry;
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

  it.each([
    [false, "var(--repo-head-expanded)"],
    [true, "var(--repo-head-collapsed)"],
  ])("draws a subsection's name in the repository names' ink, collapsed=%s", (collapsed, want) => {
    // Arrange
    const drawn = drawWorkspaceRoster(subsectionOf(collapsed), sidebarContext());

    // Act / Assert
    inRail(drawn, (rail) => {
      expect(cascadedValue(rail.querySelector(".sb-subsection-label") as HTMLElement, "color")).toBe(want);
    });
  });
});

describe("a one-shot row", () => {
  /** A context recording every ArrangeSidebar and SetWorkspaceOneShot ask. */
  function recording() {
    const arranged: ArrangeSidebarRequest[] = [];
    const marked: SetWorkspaceOneShotRequest[] = [];
    const sc = sidebarContext(
      appContext({
        arrangeSidebar: (request) => {
          arranged.push(request);
          return create(ArrangeSidebarResponseSchema, { result: { case: "success", value: { rosterRevision: 1n } } });
        },
        setWorkspaceOneShot: (request) => {
          marked.push(request);
          return create(SetWorkspaceOneShotResponseSchema, { result: { case: "success", value: { rosterRevision: 2n } } });
        },
      }),
    );
    return { sc, arranged, marked };
  }

  /** a, o (a one-shot), [later: l]. */
  const example = () =>
    roster({
      repos: [
        repoSection({
          id: "repo-1",
          rows: [row({ id: "a" }), row({ id: "o", oneShot: true })],
          subsections: [{ id: "later", rows: [row({ id: "l" })] }],
        }),
      ],
    });

  it("draws among its repository's own rows, as any row does", () => {
    const pane = mount(example(), sidebarContext());
    expect(drawnEntries(sectionList(pane))).toEqual(["a", "o", "[later]"]);
  });

  it("is placed where it is dropped, and its mark is left alone", async () => {
    // Arrange -- a 0, o 20, [later: l] 40.
    const { sc, arranged: sent, marked } = recording();
    const pane = mount(example(), sc);
    layOut(sectionList(pane), 0);

    // Act -- o, dropped into later.
    await drag(rowLine(pane, "o"), 25, 65);

    // Assert
    const change = sent[0]?.change;
    expect([
      marked,
      change?.case === "placeWorkspace"
        ? [change.value.workspace?.id, change.value.into.case === "subsection" ? change.value.into.value.id : change.value.into.case]
        : change,
    ]).toEqual([[], ["o", "later"]]);
  });
});

describe("a list's bucket", () => {
  const list = (): HTMLElement => document.createElement("div");

  it.each([
    { case: "none" as const },
    { case: "named" as const, id: "sub-1" },
  ])("reads back what was stamped ($case)", (bucket) => {
    const el = list();
    stampBucket(el, bucket);
    expect(bucketOf(el)).toEqual(bucket);
  });

  it("refuses a list stating no bucket", () => {
    expect(() => bucketOf(list())).toThrow(/invariant/);
  });

  it.each([
    [{ case: "none" as const }, { case: "none" as const }, true],
    [{ case: "named" as const, id: "a" }, { case: "named" as const, id: "a" }, true],
    [{ case: "named" as const, id: "a" }, { case: "named" as const, id: "b" }, false],
    [{ case: "named" as const, id: "a" }, { case: "none" as const }, false],
  ])("compares %j and %j: %s", (a, b, same) => {
    expect(sameBucket(a, b)).toBe(same);
  });
});

describe("a folded subsection whose workspaces work", () => {
  /** A row standing on ARM. */
  const on = (id: string, arm: string, children: RosterRow[] = []) =>
    row({ id, status: { case: arm, value: {} } as Parameters<typeof row>[0]["status"], children });
  /** One repository holding subsection sub-1, folded when COLLAPSED, over ROWS. */
  const sectionOver = (rows: RosterRow[], collapsed = true) =>
    roster({ repos: [repoSection({ id: "repo-1", subsections: [{ id: "sub-1", label: "later", rows, collapsed }] })] });
  /** The subsection sub-1 as drawn in ROOT. */
  const subOf = (root: ParentNode) => root.querySelector("[data-subsection='sub-1'].sb-subsection") as HTMLElement;
  const labelOf = (root: ParentNode) => subOf(root).querySelector(".sb-subsection-label") as HTMLElement;
  /** The wire subsection sub-1 over ROWS. */
  const wireOver = (rows: RosterRow[]): RosterSubsection => {
    const entry = repoSection({ id: "repo-1", subsections: [{ id: "sub-1", label: "later", rows, collapsed: true }] }).entries[0].entry;
    if (entry.case !== "subsection") throw new Error("harness: the entry is the subsection");
    return entry.value;
  };

  describe("workingTone", () => {
    it.each(["submitting", "thinking", "vendorSilent", "clearing", "compacting"])("answers the red tone for a row %s", (arm) => {
      // Arrange
      const rows = create(RosterOpenRowsSchema, { rows: [openRow(on("a", "ready")), openRow(on("b", arm))] });

      // Act
      const tone = workingTone(rows, "rows");

      // Assert
      expect(tone).toBe("tone-red");
    });

    it.each(["ready", "idleAsync", "merging", "permission", "init"])("answers none when the only row is %s", (arm) => {
      // Arrange
      const rows = create(RosterOpenRowsSchema, { rows: [openRow(on("a", arm))] });

      // Act
      const tone = workingTone(rows, "rows");

      // Assert
      expect(tone).toBeNull();
    });

    it("answers none for a subsection with no rows", () => {
      // Arrange
      const rows = create(RosterOpenRowsSchema, { rows: [] });

      // Act
      const tone = workingTone(rows, "rows");

      // Assert
      expect(tone).toBeNull();
    });

    it("finds a working child nested under a still parent", () => {
      // Arrange
      const rows = create(RosterOpenRowsSchema, { rows: [openRow(on("p", "ready", [on("c", "thinking")]))] });

      // Act
      const tone = workingTone(rows, "rows");

      // Assert
      expect(tone).toBe("tone-red");
    });

    it("finds a working row in a parent's repository block", () => {
      // Arrange
      const parent = withRepositoryBlocks(openRow(on("p", "ready")), [{ repo: "other", rows: [on("c", "compacting")] }]);
      const rows = create(RosterOpenRowsSchema, { rows: [parent] });

      // Act
      const tone = workingTone(rows, "rows");

      // Assert
      expect(tone).toBe("tone-red");
    });

    it("refuses a row with no status", () => {
      // Arrange
      const bad = openRow(on("a", "ready"));
      bad.row!.status = { case: undefined };
      const rows = create(RosterOpenRowsSchema, { rows: [bad] });

      // Act / Assert
      expect(() => workingTone(rows, "rows")).toThrow(MalformedView);
    });
  });

  describe("the drawn name", () => {
    it("holds the name at the working tone and breathes it, its count untouched", () => {
      // Arrange / Act
      const drawn = drawWorkspaceRoster(sectionOver([on("a", "thinking")]), sidebarContext());

      // Assert
      const label = labelOf(drawn);
      const count = subOf(drawn).querySelector(".sb-count") as HTMLElement;
      expect([
        ["tinted", "tone-red", "breathes"].every((c) => label.classList.contains(c)),
        label.querySelector(`:scope > .${NAME_INK_CLASS}`)?.getAttribute(NAME_INK_TEXT_ATTRIBUTE),
        label.textContent,
        count.className,
      ]).toEqual([true, "later", "later", "sb-count"]);
    });

    it("draws a still name when no workspace works", () => {
      // Arrange / Act
      const drawn = drawWorkspaceRoster(sectionOver([on("a", "ready")]), sidebarContext());

      // Assert
      const label = labelOf(drawn);
      expect([label.className, label.children.length, label.hasAttribute(WORKING_TONE_ATTRIBUTE)]).toEqual(["sb-subsection-label", 0, false]);
    });

    // jsdom resolves no `-webkit-text-fill-color`, so the hidden own text is
    // pinned by the stylesheet's shared rules (`styles.test.ts`).
    it.each([
      { collapsed: true, color: "var(--name-dark)" },
      { collapsed: false, color: "var(--repo-head-expanded)" },
    ])("styles a working subsection's name collapsed=$collapsed in $color", ({ collapsed, color }) => {
      // Arrange
      const drawn = drawWorkspaceRoster(sectionOver([on("a", "thinking")], collapsed), sidebarContext());

      // Act / Assert
      inRail(drawn, (rail) => {
        const label = labelOf(rail);
        expect([cascadedValue(label, "color"), cascadedValue(label, "--name-tone")]).toEqual([color, "var(--err)"]);
      });
    });
  });

  describe("paintSubsectionWork", () => {
    it("stills a breathing name once no workspace works", () => {
      // Arrange
      const drawn = drawWorkspaceRoster(sectionOver([on("a", "thinking")]), sidebarContext());
      const label = labelOf(drawn);

      // Act
      paintSubsectionWork(label, wireOver([on("a", "ready")]), "sub");

      // Assert
      expect([label.className, label.children.length, label.hasAttribute(WORKING_TONE_ATTRIBUTE)]).toEqual(["sb-subsection-label", 0, false]);
    });

    it("keeps the ink box of a name still breathing in the same tone", () => {
      // Arrange
      const drawn = drawWorkspaceRoster(sectionOver([on("a", "thinking")]), sidebarContext());
      const label = labelOf(drawn);
      const ink = label.querySelector(`.${NAME_INK_CLASS}`);

      // Act
      paintSubsectionWork(label, wireOver([on("a", "compacting")]), "sub");

      // Assert
      expect([label.querySelector(`.${NAME_INK_CLASS}`) === ink, label.children.length]).toEqual([true, 1]);
    });

    it("breathes a still name once a workspace works", () => {
      // Arrange
      const drawn = drawWorkspaceRoster(sectionOver([on("a", "ready")]), sidebarContext());
      const label = labelOf(drawn);

      // Act
      paintSubsectionWork(label, wireOver([on("a", "submitting")]), "sub");

      // Assert
      expect([label.getAttribute(WORKING_TONE_ATTRIBUTE), label.children.length]).toEqual(["tone-red", 1]);
    });
  });

  // A push that changed only rows keeps the section (`roster.ts`'s row
  // patch), and the patch repaints the header's breath.
  it.each([
    { name: "breathes a kept header once its row starts working", from: "ready", to: "thinking", want: true },
    { name: "stills a kept header once its row stops working", from: "thinking", to: "ready", want: false },
  ])("$name", ({ from, to, want }) => {
    // Arrange
    const drawer = createRosterDrawer(sidebarContext());
    const section = drawer.draw(sectionOver([on("a", from)])).querySelector(".repo-section");

    // Act
    const after = drawer.draw(sectionOver([on("a", to)]));

    // Assert
    expect([after.querySelector(".repo-section") === section, labelOf(after).classList.contains("breathes")]).toEqual([true, want]);
  });
});
