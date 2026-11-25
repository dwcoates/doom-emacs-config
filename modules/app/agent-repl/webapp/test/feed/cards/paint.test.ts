import { describe, expect, it } from "vitest";
import { paintSpanClass } from "../../../src/feed/cards/paint.js";
import { PAINT_INVENTORY as INVENTORY } from "./paint-inventory.js";

describe("paintSpanClass", () => {
  it("maps a name in the inventory to its paint- class", () => {
    // Arrange / Act / Assert: one name stands for the mapping rule itself.
    expect(paintSpanClass("keyword")).toBe("paint-keyword");
  });

  it("maps the empty string — the one spelling of plain — to no class", () => {
    expect(paintSpanClass("")).toBe("");
  });

  it("maps a name outside the inventory to no class rather than throwing", () => {
    expect(paintSpanClass("kwyjibo")).toBe("");
  });

  it.each(INVENTORY.map((name) => [name] as const))(
    "maps the inventory name %s",
    (name) => {
      expect(paintSpanClass(name)).toBe(`paint-${name}`);
    },
  );
});
