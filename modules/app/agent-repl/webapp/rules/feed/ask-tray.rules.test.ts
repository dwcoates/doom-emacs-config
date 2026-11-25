/**
 * THE ASK TRAY'S STYLES, read off src/styles.css as text.
 */
import { describe, expect, it } from "vitest";
// Imported at collection, not inside a test, so the transform never counts
// against a test's timeout.
import css from "../../src/styles.css?raw";

describe("the ask tray's styles", () => {
  /** The declarations of the first rule whose selector list is exactly SELECTORS. */
  const ruleOf = (selectors: string): string =>
    new RegExp(`(^|\\n)${selectors.replace(/[.*+?^${}()|[\]\\#]/g, "\\$&")}\\s*\\{[^}]*\\}`).exec(css)?.[0] ?? "";

  it("lays the tray out in the held section's own padding and text scale", () => {
    // Arrange / Act
    const rule = ruleOf("#hold-tray,\n#ask-tray");

    // Assert
    expect([rule.includes("padding: 0.5rem 1rem 0;"), rule.includes("font-size: calc(1em * var(--feed-text-scale));")]).toEqual([
      true,
      true,
    ]);
  });

  it("spaces the docked asks by the feed's own row gap", () => {
    // Arrange / Act
    const rule = ruleOf(".feed-body,\n.feed-rows,\n#ask-tray");

    // Assert
    expect(rule.includes("gap: 0.5rem;")).toBe(true);
  });

  it("takes no slot while hidden", () => {
    // Arrange / Act / Assert
    expect(ruleOf("#ask-tray[hidden]")).toContain("display: none;");
  });

  it("gives the docked ask's home row no slot in the feed", () => {
    // Arrange / Act / Assert
    expect(ruleOf(".feed-item.ask-docked-row")).toContain("display: none;");
  });
});
