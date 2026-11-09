// @vitest-environment jsdom
// A workspace row's children fold, asked from the row's status mark. See src/sidebar/children-fold.ts.
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { afterEach, describe, expect, it } from "vitest";
import {
  UpdateSidebarViewResponseSchema,
  type UpdateSidebarViewRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_sidebar_view_pb";
import {
  RosterOpenRowChildrenFoldSchema,
  type RosterOpenRow,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import { CHILDREN_FOLD_CONTROL_ATTRIBUTE, drawRosterChildrenFold } from "../../src/sidebar/children-fold.js";
import { workspaceChildrenFoldKey } from "../../src/sidebar/context.js";
import { CURRENT_FLASH_CLASS, HOLDS_CURRENT_ATTRIBUTE } from "../../src/sidebar/section-fold.js";
import { drawRosterFamily } from "../../src/sidebar/row.js";
import { appContext, openRow, row, sidebarContext } from "./harness.js";

type FoldInit = MessageInitShape<typeof RosterOpenRowChildrenFoldSchema>;

const COLLAPSED: FoldInit = { fold: { case: "collapsed", value: {} } };
const COLLAPSIBLE: FoldInit = { fold: { case: "expanded", value: { collapse: { case: "collapsible", value: {} } } } };
const HOLDS_CURRENT: FoldInit = { fold: { case: "expanded", value: { collapse: { case: "holdsCurrent", value: {} } } } };

/** A parent ws-1 with one child ws-2 whose children fold is FOLD. */
function parentWith(fold: FoldInit, currentChild = false): RosterOpenRow {
  const parent = openRow(row({ id: "ws-1", children: [row({ id: "ws-2", current: currentChild })] }));
  parent.childrenFold = create(RosterOpenRowChildrenFoldSchema, fold);
  return parent;
}

/** A sidebar whose daemon records every UpdateSidebarView and answers success. */
function recording(): { asked: UpdateSidebarViewRequest[]; sc: ReturnType<typeof sidebarContext> } {
  const asked: UpdateSidebarViewRequest[] = [];
  const sc = sidebarContext(
    appContext({
      updateSidebarView: (request) => {
        asked.push(request);
        return create(UpdateSidebarViewResponseSchema, { result: { case: "success", value: {} } });
      },
    }),
  );
  return { asked, sc };
}

const markOf = (drawn: HTMLElement): HTMLElement => drawn.querySelector(":scope > .row > .st") as HTMLElement;

describe("drawRosterChildrenFold", () => {
  it.each([
    { name: "collapsed", fold: COLLAPSED, want: { collapsed: true, holdsCurrent: false } },
    { name: "collapsible", fold: COLLAPSIBLE, want: { collapsed: false, holdsCurrent: false } },
    { name: "holding the current workspace", fold: HOLDS_CURRENT, want: { collapsed: false, holdsCurrent: true } },
  ])("reads $name", ({ fold, want }) => {
    // Act
    const drawn = drawRosterChildrenFold(create(RosterOpenRowChildrenFoldSchema, fold), "R.childrenFold");

    // Assert
    expect(drawn).toEqual(want);
  });

  it.each([
    { name: "an unset fold", fold: {} },
    { name: "an expanded fold with no collapse arm", fold: { fold: { case: "expanded", value: {} } } },
  ] as Array<{ name: string; fold: FoldInit }>)("refuses $name as a malformed view", ({ fold }) => {
    // Act and Assert
    expect(() => drawRosterChildrenFold(create(RosterOpenRowChildrenFoldSchema, fold), "R.childrenFold")).toThrow(MalformedView);
  });
});

describe("a row's children fold", () => {
  afterEach(() => document.body.replaceChildren());

  it.each([
    { name: "collapsed", fold: COLLAPSED, folded: true },
    { name: "collapsible", fold: COLLAPSIBLE, folded: false },
  ])("draws the row $name", ({ fold, folded }) => {
    // Act
    const drawn = drawRosterFamily(parentWith(fold), sidebarContext(), "R");

    // Assert
    expect(drawn.classList.contains("folded")).toBe(folded);
    expect(drawn.querySelector(":scope > .kids [data-roster-row='ws-2']")).not.toBeNull();
  });

  it("makes the status mark the control of a row with children", () => {
    // Act
    const drawn = drawRosterFamily(parentWith(COLLAPSIBLE), sidebarContext(), "R");

    // Assert
    expect(markOf(drawn).hasAttribute(CHILDREN_FOLD_CONTROL_ATTRIBUTE)).toBe(true);
  });

  it("makes no control of the status mark of a row with no children", () => {
    // Act
    const drawn = drawRosterFamily(openRow(row({ id: "ws-1" })), sidebarContext(), "R");

    // Assert
    expect([markOf(drawn).hasAttribute(CHILDREN_FOLD_CONTROL_ATTRIBUTE), drawn.classList.contains("folded")]).toEqual([false, false]);
  });

  it.each([
    { name: "collapsible", fold: COLLAPSIBLE, ask: "collapse" },
    { name: "collapsed", fold: COLLAPSED, ask: "expand" },
  ])("asks the daemon for the other fold of the $name children from a click on the mark", async ({ fold, ask }) => {
    // Arrange
    const { asked, sc } = recording();
    const drawn = drawRosterFamily(parentWith(fold), sc, "R");
    document.body.appendChild(drawn);

    // Act
    markOf(drawn).click();
    await new Promise((resolve) => setTimeout(resolve, 0));

    // Assert
    const change = asked[0]?.change;
    expect(
      change?.case === "foldSection" ? [change.value.section.case, change.value.section.value?.id, change.value.fold.case] : change?.case,
    ).toEqual(["workspace", "ws-1", ask]);
  });

  it("paints the collapse at once, before the daemon answers", () => {
    // Arrange
    const { sc } = recording();
    const drawn = drawRosterFamily(parentWith(COLLAPSIBLE), sc, "R");
    document.body.appendChild(drawn);

    // Act
    markOf(drawn).click();

    // Assert
    expect(sc.view.track(`fold:${workspaceChildrenFoldKey("ws-1")}`, false, () => undefined)).toBe(true);
    expect(drawn.classList.contains("folded")).toBe(true);
  });

  it("selects nothing on a click of the mark", () => {
    // Arrange
    const selected: string[] = [];
    const sc = sidebarContext(
      appContext({
        updateSidebarView: () => create(UpdateSidebarViewResponseSchema, { result: { case: "success", value: {} } }),
        selectWorkspace: (request) => {
          selected.push(request.workspace?.id ?? "");
          throw new Error("a fold click selected a workspace");
        },
      }),
    );
    const drawn = drawRosterFamily(parentWith(COLLAPSIBLE), sc, "R");
    document.body.appendChild(drawn);

    // Act
    markOf(drawn).click();

    // Assert
    expect(selected).toEqual([]);
  });

  it("asks no collapse while a child is the current workspace, and flashes it", async () => {
    // Arrange
    const { asked, sc } = recording();
    const drawn = drawRosterFamily(parentWith(HOLDS_CURRENT, true), sc, "R");
    document.body.appendChild(drawn);

    // Act
    markOf(drawn).click();
    await new Promise((resolve) => setTimeout(resolve, 0));

    // Assert
    expect(drawn.hasAttribute(HOLDS_CURRENT_ATTRIBUTE)).toBe(true);
    expect(asked).toEqual([]);
    expect(drawn.classList.contains("folded")).toBe(false);
    expect(drawn.querySelector(".ws.current")?.classList.contains(CURRENT_FLASH_CLASS)).toBe(true);
  });
});
