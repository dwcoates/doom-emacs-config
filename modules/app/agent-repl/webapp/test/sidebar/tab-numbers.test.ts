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
  TAB_NUMBER_ATTRIBUTE,
  TAB_NUMBER_CLASS,
  drawTabNumberSlot,
  paintTabNumbers,
  tabNumbers,
} from "../../src/sidebar/tab-numbers.js";
import { captureLogRecords } from "../log-capture.js";

/** An open row for ID: numbered N, or folded when N is null; with CHILDREN. */
function open(id: string, n: number | null, ...children: RosterOpenRow[]): RosterOpenRow {
  return create(RosterOpenRowSchema, {
    row: { workspace: { workspace: { id, dir: `/w/${id}` } } },
    tab: { tab: n === null ? { case: "folded", value: {} } : { case: "numbered", value: { number: n } } },
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

/** A roster row box for ID holding one number slot, under ROOT. */
function slotFor(root: HTMLElement, id: string): HTMLElement {
  const ws = document.createElement("div");
  ws.setAttribute("data-roster-row", id);
  const line = document.createElement("div");
  const slot = drawTabNumberSlot();
  line.appendChild(slot);
  ws.appendChild(line);
  root.appendChild(ws);
  return slot;
}

describe("tabNumbers", () => {
  it.each<{ name: string; view: RosterRepositoryView; want: Array<[string, number]> }>([
    { name: "reads each open row's own number", view: view([open("b", 1), open("a", 2)]), want: [["b", 1], ["a", 2]] },
    { name: "reads a nested row's number", view: view([open("p", 1, open("k", 2))]), want: [["p", 1], ["k", 2]] },
    { name: "reads a subsection's rows", view: view([open("a", 1)], [open("s", 2)]), want: [["a", 1], ["s", 2]] },
    { name: "gives a folded tab no number", view: view([open("a", 1), open("f", null), open("c", 2)]), want: [["a", 1], ["c", 2]] },
    { name: "numbers nothing in an empty section", view: view([]), want: [] },
  ])("$name", ({ view: v, want }) => {
    // Act
    const numbers = tabNumbers(v, "WorkspaceRoster.repository");

    // Assert
    expect([...numbers]).toEqual(want);
  });

  it("reads a subsection's rows where it stands between two rows", () => {
    // Arrange
    const v = create(RosterRepositoryViewSchema, {
      sections: [
        {
          entries: [
            { entry: { case: "row", value: open("a", 1) } },
            { entry: { case: "subsection", value: { openRows: { rows: [open("x", 2)] } } } },
            { entry: { case: "row", value: open("b", 3) } },
          ],
        },
      ],
    });

    // Act
    const numbers = tabNumbers(v, "WorkspaceRoster.repository");

    // Assert
    expect([...numbers.entries()]).toEqual([
      ["a", 1],
      ["x", 2],
      ["b", 3],
    ]);
  });

  it("refuses a section entry with no arm as a malformed view", () => {
    // Arrange
    const v = create(RosterRepositoryViewSchema, { sections: [{ entries: [{}] }] });

    // Act
    const read = (): unknown => tabNumbers(v, "WorkspaceRoster.repository");

    // Assert
    expect(read).toThrow(/entries\[0\]\.entry/);
  });

  it("refuses an open row with no tab as a malformed view", () => {
    // Arrange
    const v = view([create(RosterOpenRowSchema, { row: { workspace: { workspace: { id: "a", dir: "/w/a" } } } })]);

    // Act
    const read = (): unknown => tabNumbers(v, "WorkspaceRoster.repository");

    // Assert
    expect(read).toThrow(MalformedView);
    expect(read).toThrow(/entries\[0\]\.row\.tab/);
  });

  it("refuses an open row with no tab arm as a malformed view", () => {
    // Arrange
    const v = view([create(RosterOpenRowSchema, { row: { workspace: { workspace: { id: "a", dir: "/w/a" } } }, tab: {} })]);

    // Act
    const read = (): unknown => tabNumbers(v, "WorkspaceRoster.repository");

    // Assert
    expect(read).toThrow(/entries\[0\]\.row\.tab\.tab/);
  });

  it("refuses a workspace with two open rows as a malformed view", () => {
    // Arrange
    const v = view([open("a", 1), open("a", 2)]);

    // Act
    const read = (): unknown => tabNumbers(v, "WorkspaceRoster.repository");

    // Assert
    expect(read).toThrow(/workspace a has two open rows/);
  });
});

describe("drawTabNumberSlot", () => {
  it("draws an empty slot of the number class, hidden from assistive tech", () => {
    // Act
    const slot = drawTabNumberSlot();

    // Assert
    expect([slot.className, slot.textContent, slot.getAttribute("aria-hidden")]).toEqual([TAB_NUMBER_CLASS, "", "true"]);
  });
});

describe("paintTabNumbers", () => {
  it("writes each row's number into its slot", () => {
    // Arrange
    const root = document.createElement("div");
    const a = slotFor(root, "a");
    const b = slotFor(root, "b");

    // Act
    paintTabNumbers(root, new Map([["a", 2], ["b", 1]]));

    // Assert
    expect([a.textContent, a.getAttribute(TAB_NUMBER_ATTRIBUTE), b.textContent]).toEqual(["2", "2", "1"]);
  });

  it("numbers a nested row by its own workspace, not its parent's", () => {
    // Arrange
    const root = document.createElement("div");
    slotFor(root, "parent");
    const parent = root.querySelector<HTMLElement>("[data-roster-row='parent']")!;
    const child = slotFor(parent, "child");

    // Act
    paintTabNumbers(root, new Map([["parent", 1], ["child", 2]]));

    // Assert
    expect(child.textContent).toBe("2");
  });

  it("empties the slot of a row whose tab is no longer drawn", () => {
    // Arrange
    const root = document.createElement("div");
    const a = slotFor(root, "a");
    paintTabNumbers(root, new Map([["a", 3]]));

    // Act
    paintTabNumbers(root, new Map());

    // Assert
    expect([a.textContent, a.hasAttribute(TAB_NUMBER_ATTRIBUTE)]).toEqual(["", false]);
  });

  it("refuses a slot outside any roster row, logging why", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const root = document.createElement("div");
    root.appendChild(drawTabNumberSlot());

    // Act
    const paint = (): void => paintTabNumbers(root, new Map());

    // Assert
    expect(paint).toThrow(/outside any roster row/);
    capture.logger.flush();
    await Promise.resolve();
    expect(capture.sent.find((r) => r.operation === "sidebar.tab-numbers.orphan-slot")?.level.case).toBe("error");
  });
});
