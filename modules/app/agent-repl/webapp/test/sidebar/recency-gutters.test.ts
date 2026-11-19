// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  RosterOpenRowSchema,
  RosterRepositoryViewSchema,
  type RosterOpenRow,
  type RosterRepositoryView,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  RECENCY_ATTRIBUTE,
  RECENCY_GUTTER_CLASS,
  RECENCY_PRESSES_ATTRIBUTE,
  drawRecencyGutterSlot,
  paintRecencyGutters,
  recencyGutters,
} from "../../src/sidebar/recency-gutters.js";
import { captureLogRecords } from "../log-capture.js";

type Reach = { direction: "older" | "newer"; presses: number } | null;

/** An open row for ID reached as REACH (none when null), with CHILDREN. */
function open(id: string, reach: Reach, ...children: RosterOpenRow[]): RosterOpenRow {
  return create(RosterOpenRowSchema, {
    row: { workspace: { workspace: { id, dir: `/w/${id}` } } },
    tab: { tab: { case: "numbered", value: { number: 1 } } },
    ...(reach === null ? {} : { recencyGutter: { reach: { case: reach.direction, value: { presses: reach.presses } } } }),
    children,
  });
}

/** An Open section of one repository: OWN rows, then one subsection per SUBS. */
function view(own: RosterOpenRow[], ...subs: RosterOpenRow[][]): RosterRepositoryView {
  return create(RosterRepositoryViewSchema, {
    sections: [
      {
        entries: [
          ...own.map((row) => ({ entry: { case: "row" as const, value: row } })),
          ...subs.map((rows) => ({ entry: { case: "subsection" as const, value: { openRows: { rows } } } })),
        ],
      },
    ],
  });
}

/** A roster row box for ID holding one gutter slot, under ROOT. */
function slotFor(root: HTMLElement, id: string): HTMLElement {
  const ws = document.createElement("div");
  ws.setAttribute("data-roster-row", id);
  const line = document.createElement("div");
  const slot = drawRecencyGutterSlot();
  line.appendChild(slot);
  ws.appendChild(line);
  root.appendChild(ws);
  return slot;
}

describe("recencyGutters", () => {
  it.each<{ name: string; view: RosterRepositoryView; want: Array<[string, Reach]> }>([
    {
      name: "reads each reached row's direction and presses",
      view: view([open("a", { direction: "older", presses: 1 }), open("b", { direction: "newer", presses: 3 })]),
      want: [["a", { direction: "older", presses: 1 }], ["b", { direction: "newer", presses: 3 }]],
    },
    { name: "leaves out a row with no gutter", view: view([open("a", null), open("b", { direction: "older", presses: 2 })]), want: [["b", { direction: "older", presses: 2 }]] },
    { name: "reads a nested row's gutter", view: view([open("p", null, open("k", { direction: "newer", presses: 1 }))]), want: [["k", { direction: "newer", presses: 1 }]] },
    { name: "reads a subsection's rows", view: view([], [open("s", { direction: "older", presses: 4 })]), want: [["s", { direction: "older", presses: 4 }]] },
    { name: "reads nothing in an empty section", view: view([]), want: [] },
  ])("$name", ({ view: v, want }) => {
    // Act
    const gutters = recencyGutters(v, "WorkspaceRoster.repository");

    // Assert
    expect([...gutters]).toEqual(want);
  });

  it("refuses a gutter on no arm as a malformed view", () => {
    // Arrange
    const row = open("a", null);
    row.recencyGutter = create(RosterOpenRowSchema, { recencyGutter: {} }).recencyGutter;

    // Act
    const read = (): unknown => recencyGutters(view([row]), "WorkspaceRoster.repository");

    // Assert
    expect(read).toThrow(MalformedView);
    expect(read).toThrow(/entries\[0\]\.row\.recency_gutter\.reach/);
  });
});

describe("drawRecencyGutterSlot", () => {
  it("draws a bare slot of the gutter class, hidden from assistive tech", () => {
    // Act
    const slot = drawRecencyGutterSlot();

    // Assert
    expect([slot.className, slot.hasAttribute(RECENCY_ATTRIBUTE), slot.getAttribute("aria-hidden")]).toEqual([
      RECENCY_GUTTER_CLASS,
      false,
      "true",
    ]);
  });
});

describe("paintRecencyGutters", () => {
  it("marks each reached row's slot with its direction and presses", () => {
    // Arrange
    const root = document.createElement("div");
    const a = slotFor(root, "a");
    const b = slotFor(root, "b");

    // Act
    paintRecencyGutters(root, new Map([["a", { direction: "older", presses: 1 }], ["b", { direction: "newer", presses: 5 }]]));

    // Assert
    expect([
      a.getAttribute(RECENCY_ATTRIBUTE),
      a.getAttribute(RECENCY_PRESSES_ATTRIBUTE),
      b.getAttribute(RECENCY_ATTRIBUTE),
      b.getAttribute(RECENCY_PRESSES_ATTRIBUTE),
    ]).toEqual(["older", "1", "newer", "5"]);
  });

  it("marks a nested row by its own workspace, not its parent's", () => {
    // Arrange
    const root = document.createElement("div");
    const parentSlot = slotFor(root, "parent");
    const child = slotFor(root.querySelector<HTMLElement>("[data-roster-row='parent']")!, "child");

    // Act
    paintRecencyGutters(root, new Map([["child", { direction: "newer", presses: 2 }]]));

    // Assert
    expect([parentSlot.hasAttribute(RECENCY_ATTRIBUTE), child.getAttribute(RECENCY_ATTRIBUTE)]).toEqual([false, "newer"]);
  });

  it("bares the slot of a row no longer reached", () => {
    // Arrange
    const root = document.createElement("div");
    const a = slotFor(root, "a");
    paintRecencyGutters(root, new Map([["a", { direction: "older", presses: 3 }]]));

    // Act
    paintRecencyGutters(root, new Map());

    // Assert
    expect([a.hasAttribute(RECENCY_ATTRIBUTE), a.hasAttribute(RECENCY_PRESSES_ATTRIBUTE)]).toEqual([false, false]);
  });

  it("refuses a slot outside any roster row, logging why", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const root = document.createElement("div");
    root.appendChild(drawRecencyGutterSlot());

    // Act
    const paint = (): void => paintRecencyGutters(root, new Map());

    // Assert
    expect(paint).toThrow(/outside any roster row/);
    capture.logger.flush();
    await Promise.resolve();
    expect(capture.sent.find((r) => r.operation === "sidebar.recency-gutters.orphan-slot")?.level.case).toBe("error");
  });
});
