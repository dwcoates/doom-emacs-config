// The one class a folded sidebar box wears. See src/sidebar/folded-class.ts.
import { describe, expect, it } from "vitest";
import { FOLDED_CLASS } from "../../src/sidebar/folded-class.js";
import stylesheet from "../../src/styles.css?raw";
import { rulesOf } from "../stylesheet.js";

/** Every sidebar source module, by path, as text. */
function sidebarSources(): Record<string, string> {
  // eslint-disable-next-line @typescript-eslint/no-unnecessary-type-assertion -- eslint resolves import.meta.glob through vite/client and reads the assertion as a no-op; tsc, whose program has no vite/client, does not, and rejects the raw result as `unknown` without it.
  return import.meta.glob("../../src/sidebar/*.ts", { query: "?raw", import: "default", eager: true }) as Record<string, string>;
}

describe("FOLDED_CLASS", () => {
  it("is the class the stylesheet hides a folded subsection's rows under", () => {
    // Arrange / Act
    const hiding = rulesOf(stylesheet).filter((rule) => rule.selectors.includes(`#ws-sidebar .sb-subsection.${FOLDED_CLASS} > .rows:not(:has(.surfaced))`));

    // Assert
    expect(hiding.map((rule) => /display:\s*none/.test(rule.declarations))).toEqual([true]);
  });

  it("is the one spelling of the fold class: no sidebar module names it as a literal", () => {
    // Arrange
    const sources = Object.entries(sidebarSources()).filter(([path]) => !path.endsWith("/folded-class.ts"));

    // Act
    const spelled = sources.filter(([, src]) => /classList\.\w+\(\s*["']folded["']/.test(src)).map(([path]) => path);

    // Assert
    expect([sources.length > 0, spelled]).toEqual([true, []]);
  });
});
