/**
 * The stylesheet's paint section (src/styles.css) declares a rule for every
 * paint name of the vocabulary, and none for a name it does not carry.
 */
import { describe, expect, it } from "vitest";
import stylesheet from "../../../src/styles.css?raw";
import { PAINT_INVENTORY as INVENTORY } from "../../../test/feed/cards/paint-inventory.js";

/** Every `.paint-<name>` selector the stylesheet declares a rule for. */
function styledNames(): Set<string> {
  const found = new Set<string>();
  for (const match of stylesheet.matchAll(/\.paint-([a-z0-9-]+)/g)) found.add(match[1]);
  return found;
}

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
