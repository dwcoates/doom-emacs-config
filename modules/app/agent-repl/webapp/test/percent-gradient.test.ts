import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { percentGradientColor, type PercentStop } from "../src/percent-gradient.js";

const here = path.dirname(fileURLToPath(import.meta.url));

// The hue is the first field of the `hsl(H S% L%)` string; saturation and
// lightness are fixed.
function hueOf(color: string): number {
  const match = /^hsl\((\d+)\s/.exec(color);
  if (match === null) throw new Error(`not an hsl color: ${color}`);
  return Number(match[1]);
}

const STOPS: readonly PercentStop[] = [
  { at: 20, hue: 100 },
  { at: 60, hue: 20 },
  { at: 60, hue: 0 },
];

describe("percentGradientColor", () => {
  it.each([
    ["below the first stop takes the first stop's hue", 5, 100],
    ["at the first stop takes its hue", 20, 100],
    ["between two stops interpolates linearly", 40, 60],
    ["just below a hard step still interpolates", 59, 22],
    ["at a hard step takes the later stop's hue", 60, 0],
    ["past the last stop takes the last stop's hue", 150, 0],
  ])("%s", (_name, percent, hue) => {
    // Act
    const color = percentGradientColor(percent, STOPS);
    // Assert
    expect(hueOf(color)).toBe(hue);
  });

  it("draws at the shared saturation and lightness", () => {
    // Act / Assert
    expect(percentGradientColor(40, STOPS)).toBe("hsl(60 80% 45%)");
  });

  it("refuses an empty stop table", () => {
    // Act / Assert
    expect(() => percentGradientColor(50, [])).toThrow("at least one stop");
  });

  it("refuses stops out of order", () => {
    // Arrange
    const disordered = [
      { at: 60, hue: 0 },
      { at: 20, hue: 100 },
    ];
    // Act / Assert
    expect(() => percentGradientColor(50, disordered)).toThrow("stop 1 sits below");
  });

  it.each(["src/panels/context-colors.ts"])(
    "%s colors its percent through the shared helper, not its own interpolation",
    (file) => {
      // Arrange
      const source = readFileSync(path.join(here, "..", file), "utf8");
      // Assert
      expect(source).toContain("percentGradientColor(");
      expect(source).not.toMatch(/function lerp\b/);
    },
  );
});
