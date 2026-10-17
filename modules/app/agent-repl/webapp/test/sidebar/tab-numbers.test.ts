// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { RosterTabBarSchema, type RosterTabBar } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  TAB_NUMBER_ATTRIBUTE,
  TAB_NUMBER_CLASS,
  drawTabNumberSlot,
  paintTabNumbers,
  tabNumbers,
} from "../../src/sidebar/tab-numbers.js";
import { captureLogRecords } from "../log-capture.js";

type Visibility = "drawn" | "withheldCollapsed" | "withheldHidden";

function tabBar(...tabs: Array<[string, Visibility]>): RosterTabBar {
  return create(RosterTabBarSchema, {
    tabs: tabs.map(([id, visibility]) => ({
      workspace: { id, dir: `/w/${id}` },
      visibility: { case: visibility, value: {} },
    })),
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
  it.each<{ name: string; bar: RosterTabBar; want: Array<[string, number]> }>([
    { name: "numbers drawn tabs from 1 in tab-bar order", bar: tabBar(["b", "drawn"], ["a", "drawn"]), want: [["b", 1], ["a", 2]] },
    {
      name: "skips a collapsed repository's tab without counting it",
      bar: tabBar(["a", "drawn"], ["b", "withheldCollapsed"], ["c", "drawn"]),
      want: [["a", 1], ["c", 2]],
    },
    {
      name: "skips a hidden workspace's tab without counting it",
      bar: tabBar(["a", "withheldHidden"], ["b", "drawn"]),
      want: [["b", 1]],
    },
    { name: "numbers nothing on an empty bar", bar: tabBar(), want: [] },
  ])("$name", ({ bar, want }) => {
    // Act
    const numbers = tabNumbers(bar, "WorkspaceRoster.tab_bar");

    // Assert
    expect([...numbers]).toEqual(want);
  });

  it("refuses a tab with no workspace as a malformed view", () => {
    // Arrange
    const bar = create(RosterTabBarSchema, { tabs: [{ visibility: { case: "drawn", value: {} } }] });

    // Act
    const read = (): unknown => tabNumbers(bar, "WorkspaceRoster.tab_bar");

    // Assert
    expect(read).toThrow(MalformedView);
    expect(read).toThrow(/tab_bar\.tabs\[0\]\.workspace/);
  });

  it("refuses a tab with no visibility arm as a malformed view", () => {
    // Arrange
    const bar = create(RosterTabBarSchema, { tabs: [{ workspace: { id: "a", dir: "/w/a" } }] });

    // Act
    const read = (): unknown => tabNumbers(bar, "WorkspaceRoster.tab_bar");

    // Assert
    expect(read).toThrow(/tab_bar\.tabs\[0\]\.visibility/);
  });

  it("refuses a workspace with two tabs as a malformed view", () => {
    // Arrange
    const bar = tabBar(["a", "drawn"], ["a", "drawn"]);

    // Act
    const read = (): unknown => tabNumbers(bar, "WorkspaceRoster.tab_bar");

    // Assert
    expect(read).toThrow(/workspace a has two tabs/);
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
