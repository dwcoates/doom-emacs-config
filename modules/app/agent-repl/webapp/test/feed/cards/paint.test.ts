import { describe, expect, it } from "vitest";
import paintClasses from "../../../../proto/vocab/paint-classes.json";
import stylesheet from "../../../src/styles.css?raw";
import { paintSpanClass } from "../../../src/feed/cards/paint.js";

const SYNTAX: readonly string[] = paintClasses.syntax;
const ANSI: readonly string[] = paintClasses.ansi;
const INVENTORY: readonly string[] = [...SYNTAX, ...ANSI];

/** Every `.paint-<name>` selector the stylesheet declares a rule for. */
function styledNames(): Set<string> {
  const found = new Set<string>();
  for (const match of stylesheet.matchAll(/\.paint-([a-z0-9-]+)/g)) found.add(match[1]);
  return found;
}

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

describe("the paint stylesheet section", () => {
  it.each(INVENTORY.map((name) => [name] as const))(
    "declares a rule for the inventory name %s",
    (name) => {
      expect(styledNames().has(name)).toBe(true);
    },
  );

  it("declares no rule for a name the inventory does not carry", () => {
    // The mirror of the row-for-row check above: a stale rule is drift too.
    expect([...styledNames()].filter((name) => !INVENTORY.includes(name))).toEqual([]);
  });
});
