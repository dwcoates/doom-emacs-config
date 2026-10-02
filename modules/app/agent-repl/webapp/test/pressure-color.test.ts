import { describe, expect, it } from "vitest";
import { pressurePercentColor } from "../src/pressure-color.js";

describe("pressurePercentColor", () => {
  // The hue is the first field of the `hsl(H S% L%)` string.
  const hueOf = (color: string): number => Number(/^hsl\((\d+)\s/.exec(color)?.[1]);

  it.each([
    ["flat green well below 40", 10, 140],
    ["still green at 40", 40, 140],
    ["halfway between green and yellow at 55", 55, 100],
    ["yellow at 70", 70, 60],
    ["halfway between yellow and orange at 80", 80, 45],
    ["nearly orange at 89", 89, 32],
    ["red at 90", 90, 0],
    ["red at 100", 100, 0],
  ])("is %s", (_name, percent, hue) => {
    expect(hueOf(pressurePercentColor(percent))).toBe(hue);
  });
});
