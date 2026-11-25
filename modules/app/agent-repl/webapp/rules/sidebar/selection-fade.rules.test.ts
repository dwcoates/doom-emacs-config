/**
 * THE SELECTION FADE'S STYLING, read off the stylesheet: the highlighted
 * number's color and rise, its keyframes, and its stop under reduced motion.
 */
import { describe, expect, it } from "vitest";
import { SELECTION_NUMBER_RISE } from "../../src/sidebar/selection-fade.js";
import stylesheet from "../../src/styles.css?raw";
import { keyframesOf, mediaBlocksOf, rulesOf } from "../stylesheet-text.js";

describe("the selection fade's styling", () => {
  /** The declarations of every rule whose selector names SELECTOR. */
  const declarationsOf = (selector: string): string[] =>
    rulesOf(stylesheet)
      .filter((rule) => rule.selectors.includes(selector))
      .map((rule) => rule.declarations);

  it("leaves the selected row's line still: the stylesheet carries no selection wash", () => {
    // Arrange / Act / Assert
    expect(stylesheet).not.toContain("selection-blink");
  });

  it("draws the highlighted row's number white for as long as it is highlighted", () => {
    // Arrange / Act
    const declarations = declarationsOf("#ws-sidebar .ws.current > .row .sb-num");

    // Assert
    expect(declarations.map((d) => d.trim())).toContain("color: white;");
  });

  it.each([
    { animation: SELECTION_NUMBER_RISE, keyframes: "ws-selection-number-rise", easing: "ease-out" },
  ])("animates the $keyframes number on its length and delay, once", ({ animation, keyframes, easing }) => {
    // Arrange / Act
    const declarations = declarationsOf(`#ws-sidebar .ws.${animation.className} > .row .sb-num`);

    // Assert
    expect(declarations.map((d) => d.trim())).toContain(
      `animation: ${keyframes} var(${animation.durationProperty}) ${easing} var(${animation.delayProperty}) 1;`,
    );
  });

  it("rises from the number's own grey, ending on the highlighted white", () => {
    // Arrange / Act
    const stops = rulesOf(keyframesOf("ws-selection-number-rise")).map((rule) => [rule.selectors, rule.declarations.trim()]);

    // Assert
    expect(stops).toEqual([[["from"], "color: var(--tab-number-color);"]]);
  });

  it("carries no fall: the row the highlight left has no animation to run", () => {
    // Arrange / Act / Assert
    expect(stylesheet).not.toContain("selection-number-fall");
  });

  it("draws the number in the grey the rise starts from", () => {
    // Arrange / Act
    const declarations = declarationsOf("#ws-sidebar .sb-num");

    // Assert
    expect(declarations.some((d) => d.includes("color: var(--tab-number-color);"))).toBe(true);
  });

  it.each(["selection-number-rise"])("stops the %s under reduced motion", (className) => {
    // Arrange
    const reduced = mediaBlocksOf("prefers-reduced-motion: reduce").some(({ body }) => {
      const rule = body.match(/([^{}]*)\{\s*animation: none;/g) ?? [];
      return rule.some((r) => r.includes(`#ws-sidebar .ws.${className} > .row .sb-num`));
    });

    // Assert
    expect(reduced).toBe(true);
  });
});
