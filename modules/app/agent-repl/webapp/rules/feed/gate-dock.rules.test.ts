// @vitest-environment jsdom
/**
 * THE GATE DOCK'S RULES OVER THE STYLESHEET'S TEXT: the rule that hides a
 * docked row matches a row inside the feed's body, the dock's own rule places
 * and sizes it, and the gate's background is the feed's own ground.
 */
import { afterEach, describe, expect, it } from "vitest";
import { DOCKED_ROW_CLASS, installGateDock, WEBHOST_GATE_CLASS, type GateDock } from "../../src/feed/gate-dock.js";
import { bodiedFeed, gateRow, page, settle } from "../../test/feed/gate-dock-page.js";
import { rulesOf } from "../stylesheet-text.js";

// Imported at collection, not inside a test, so the transform never counts
// against a test's timeout.
import css from "../../src/styles.css?raw";

let installed: GateDock | null = null;
afterEach(() => {
  installed?.dispose();
  installed = null;
  document.documentElement.classList.remove(WEBHOST_GATE_CLASS);
});

describe("installGateDock on the feed's real shape", () => {
  it("hides the docked row by a rule that matches a row inside the body", async () => {
    // Arrange
    const { feed, dock } = page();
    const { row } = gateRow("standing");
    bodiedFeed(feed, row);
    installed = installGateDock(feed, dock);
    await settle();
    const hiding = rulesOf(css)
      .filter((rule) => /^\s*display:\s*none;\s*$/.test(rule.declarations))
      .flatMap((rule) => rule.selectors)
      .filter((selector) => selector.includes(DOCKED_ROW_CLASS));

    // Act
    const matched = hiding.filter((selector) => row.matches(selector));

    // Assert
    expect(matched.length).toBeGreaterThan(0);
  });
});

describe("installGateDock", () => {
  it("spans every column of the page's second row", () => {
    // Arrange

    // Act
    const rule = /#gate-dock\s*\{[^}]*\}/.exec(css)?.[0] ?? "";

    // Assert
    expect([rule.includes("grid-column: 1 / -1"), rule.includes("grid-row: 2")]).toEqual([true, true]);
  });

  it("keeps the rail in the first row, so it never reaches down beside the dock", () => {
    // Arrange

    // Act
    const rail = /#ws-sidebar\s*\{[^}]*\}/.exec(css)?.[0] ?? "";
    const main = /#main-col\s*\{[^}]*\}/.exec(css)?.[0] ?? "";

    // Assert
    expect([rail.includes("grid-row: 1"), main.includes("grid-row: 1")]).toEqual([true, true]);
  });

  it("takes no composer's slot, since a page on its own has none", () => {
    // Arrange

    // Act
    const rule = /#gate-dock\s*\{[^}]*\}/.exec(css)?.[0] ?? "";

    // Assert
    expect(rule).not.toMatch(/min-height|--gate-dock-height/);
  });

  it("sets no fixed or maximum height, so a taller gate grows the dock (owner request, 2026-10-29)", () => {
    // Arrange

    // Act
    const rule = /#gate-dock\s*\{[^}]*\}/.exec(css)?.[0] ?? "";

    // Assert
    expect(rule).not.toMatch(/(?<![-\w])(?:max-)?height:/);
  });

  it("never scrolls the dock", () => {
    // Arrange

    // Act
    const rule = /#gate-dock\s*\{[^}]*\}/.exec(css)?.[0] ?? "";

    // Assert
    expect(rule).not.toMatch(/overflow/);
  });
});

describe("the gate's background (owner request, 2026-10-28)", () => {
  it.each([
    ["the docked card", /#gate-dock > \.cold-gate\.cold-gate-docked[^{]*\{[^}]*\}/],
    ["the webhost's card", /#webhost-gate > \.cold-gate\.cold-gate-docked[^{]*\{[^}]*\}/],
    ["the undocked card", /\n\.cold-gate\s*\{[^}]*\}/],
  ])("paints %s the feed's own ground, never a told color or the revival yellow", (_name, selector) => {
    // Arrange
    // Act
    const rule = selector.exec(css)?.[0] ?? "";
    // Assert
    expect([rule.includes("background: var(--bg);"), rule.includes("--input-bg"), rule.includes("revival-gate-bg")]).toEqual([true, false, false]);
  });

  it.each([
    ["the page's dock", /#gate-dock > \.cold-gate\.cold-gate-docked[^{]*\{[^}]*\}/],
    ["the webhost's gate", /#webhost-gate > \.cold-gate\.cold-gate-docked[^{]*\{[^}]*\}/],
  ])("frames the docked gate on every side, not only above it, in %s", (_name, selector) => {
    // Arrange
    // Act
    const rule = selector.exec(css)?.[0] ?? "";
    // Assert
    expect([rule.includes("border-width: 1px;"), rule.includes("border-width: 1px 0 0")]).toEqual([true, false]);
  });

  it("lifts the docked gate off the page's last row so its bottom edge is whole", () => {
    // Arrange
    // Act
    const rule = /#gate-dock\s*\{[^}]*\}/.exec(css)?.[0] ?? "";
    // Assert
    expect(rule.includes("padding-bottom: 1px;")).toBe(true);
  });

  it("is told by no property any more", () => {
    // Arrange / Act / Assert
    expect(css.includes("--input-bg")).toBe(false);
  });
});
