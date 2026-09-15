import { describe, expect, it } from "vitest";
import { contextPercentColor, contextSectionColor } from "../../src/panels/context-colors.js";

// The hue is the first field of the `hsl(H S% L%)` string, which is all these
// tests care about — saturation and lightness are fixed.
function hueOf(color: string): number {
  const match = /^hsl\((\d+)\s/.exec(color);
  if (match === null) throw new Error(`not an hsl color: ${color}`);
  return Number(match[1]);
}

describe("the context percent gradient", () => {
  it("is flat green at rest, well below the fill line", () => {
    // Arrange
    const low = 10;
    // Act
    const hue = hueOf(contextPercentColor(low));
    // Assert
    expect(hue).toBe(140);
  });

  it("is still flat green at the 50 boundary", () => {
    // Arrange
    const boundary = 50;
    // Act / Assert
    expect(hueOf(contextPercentColor(boundary))).toBe(140);
  });

  it("has warmed to yellow by 70", () => {
    // Arrange
    const yellow = 70;
    // Act / Assert
    expect(hueOf(contextPercentColor(yellow))).toBe(60);
  });

  it("sits at orange by 90", () => {
    // Arrange
    const orange = 90;
    // Act / Assert
    expect(hueOf(contextPercentColor(orange))).toBe(30);
  });

  it("is red at a full window", () => {
    // Arrange
    const full = 100;
    // Act / Assert
    expect(hueOf(contextPercentColor(full))).toBe(0);
  });

  it("interpolates between anchors rather than stepping", () => {
    // Arrange. 60 sits halfway between the green (50) and yellow (70) anchors.
    const midpoint = 60;
    // Act
    const hue = hueOf(contextPercentColor(midpoint));
    // Assert. Halfway between hue 140 and hue 60 is 100.
    expect(hue).toBe(100);
  });

  it("clamps a percent past 100 to red", () => {
    // Arrange
    const over = 130;
    // Act / Assert
    expect(hueOf(contextPercentColor(over))).toBe(0);
  });

  it("clamps a negative percent to green", () => {
    // Arrange
    const under = -5;
    // Act / Assert
    expect(hueOf(contextPercentColor(under))).toBe(140);
  });
});

describe("the context section palette", () => {
  it("assigns adjacent sections distinct colors", () => {
    // Arrange / Act
    const first = contextSectionColor(0);
    const second = contextSectionColor(1);
    // Assert
    expect(first).not.toBe(second);
  });

  it("cycles the palette when sections outrun the colors", () => {
    // Arrange. The palette has eight entries, so index 8 wraps to index 0.
    // Act / Assert
    expect(contextSectionColor(8)).toBe(contextSectionColor(0));
  });
});
