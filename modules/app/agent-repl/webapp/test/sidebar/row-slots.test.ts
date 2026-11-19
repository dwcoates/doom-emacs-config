// @vitest-environment jsdom
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { rowSlots } from "../../src/sidebar/row-slots.js";
import { captureLogRecords } from "../log-capture.js";

/** A roster row box for ID under ROOT, holding one slot of CLASS_NAME. */
function rowWithSlot(root: HTMLElement, id: string, className: string): HTMLElement {
  const ws = document.createElement("div");
  ws.setAttribute("data-roster-row", id);
  const slot = document.createElement("span");
  slot.className = className;
  ws.appendChild(slot);
  root.appendChild(ws);
  return slot;
}

describe("rowSlots", () => {
  it("answers each slot of the class with its row's workspace, in document order", () => {
    // Arrange
    const root = document.createElement("div");
    const a = rowWithSlot(root, "a", "s");
    const b = rowWithSlot(root, "b", "s");
    rowWithSlot(root, "c", "other");

    // Act
    const got = rowSlots(root, "s", "op", {});

    // Assert
    expect(got).toEqual([{ slot: a, id: "a" }, { slot: b, id: "b" }]);
  });

  it("names a nested row's slot by its own row, not its parent's", () => {
    // Arrange
    const root = document.createElement("div");
    rowWithSlot(root, "parent", "other");
    const child = rowWithSlot(root.querySelector<HTMLElement>("[data-roster-row='parent']")!, "child", "s");

    // Act
    const got = rowSlots(root, "s", "op", {});

    // Assert
    expect(got).toEqual([{ slot: child, id: "child" }]);
  });

  it("refuses a slot outside any roster row, recording the operation at error", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const root = document.createElement("div");
    const slot = document.createElement("span");
    slot.className = "s";
    root.appendChild(slot);

    // Act
    const read = (): unknown => rowSlots(root, "s", "sidebar.test.orphan-slot", { slots: 2 });

    // Assert
    expect(read).toThrow(/outside any roster row/);
    capture.logger.flush();
    await Promise.resolve();
    expect(capture.sent.find((r) => r.operation === "sidebar.test.orphan-slot")?.level.case).toBe("error");
  });

  it("is the read every slot painter finds its slots by", () => {
    // Arrange
    const source = (file: string): string => readFileSync(join(__dirname, "..", "..", "src", "sidebar", file), "utf8");

    // Act / Assert
    for (const file of ["tab-numbers.ts", "recency-gutters.ts"]) {
      expect(source(file)).toContain("rowSlots(");
      expect(source(file)).not.toContain('closest("[data-roster-row]")');
    }
  });
});
