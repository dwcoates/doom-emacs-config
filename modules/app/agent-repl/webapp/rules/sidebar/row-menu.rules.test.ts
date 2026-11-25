/**
 * THE ROW MENU'S PLACE IS ITS ROW'S PANEL'S: its source places it through the
 * one `placeBeside`, and the stylesheet draws both from one rule.
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

const SRC = path.join(path.dirname(fileURLToPath(import.meta.url)), "../../src");

describe("the menu's place is the row's panel's", () => {
  it("is placed through the one placeBeside the panel is placed through", () => {
    const source = readFileSync(path.join(SRC, "sidebar", "row-menu.ts"), "utf8");
    expect(/placeBeside\(/.test(source)).toBe(true);
  });

  it("wears the panel's card: the stylesheet draws both from one rule", () => {
    const css = readFileSync(path.join(SRC, "styles.css"), "utf8");
    const card = /#ws-sidebar \.ws\.open > \.detail,\s*#ws-sidebar \.detail\.sb-memory\.open,\s*#ws-sidebar \.ws > \.sb-menu \{[^}]*position: fixed;[^}]*background: var\(--card\);/;
    expect(card.test(css)).toBe(true);
  });
});
