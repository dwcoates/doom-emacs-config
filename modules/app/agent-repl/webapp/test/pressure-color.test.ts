import { describe, expect, it } from "vitest";
import { MalformedView } from "../src/rpc/malformed.js";
import { coldGateFigureColor, coldGatePercentColor, pressurePercentColor, windowFillPercent } from "../src/pressure-color.js";

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

describe("coldGatePercentColor", () => {
  const hueOf = (color: string): number => Number(/^hsl\((\d+)\s/.exec(color)?.[1]);

  it.each([
    ["green at 19", 19, 140],
    ["still green at 20", 20, 140],
    ["blended between green and yellow at 27", 27, 103],
    ["yellow at 35", 35, 60],
    ["blended between yellow and orange at 40", 40, 50],
    ["orange at 50", 50, 30],
    ["blended between orange and red at 60", 60, 15],
    ["red at 70", 70, 0],
    ["red past 70", 95, 0],
  ])("is %s", (_name, percent, hue) => {
    // Act / Assert
    expect(hueOf(coldGatePercentColor(percent))).toBe(hue);
  });
});

describe("windowFillPercent", () => {
  it("is the fill as a whole percent", () => {
    // Act / Assert
    expect(windowFillPercent(0.409, "X.window_fill")).toBe(41);
  });

  it("refuses a fill above one at its path", () => {
    // Act / Assert
    expect(() => windowFillPercent(1.5, "X.window_fill")).toThrow(MalformedView);
  });

  it("refuses a fill below zero", () => {
    // Act / Assert
    expect(() => windowFillPercent(-0.1, "X.window_fill")).toThrow(MalformedView);
  });
});

describe("coldGateFigureColor", () => {
  it("colors a fill through the cold-gate stops", () => {
    // Act / Assert
    expect(coldGateFigureColor(0.35, "X.window_fill")).toBe(coldGatePercentColor(35));
  });
});
