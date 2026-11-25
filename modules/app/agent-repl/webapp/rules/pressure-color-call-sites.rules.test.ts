/** The pressure color's call sites color through the shared helpers and interpolate nothing themselves. */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

const here = path.dirname(fileURLToPath(import.meta.url));

describe("the pressure color's call sites", () => {
  it.each(["src/footer/activity.ts", "src/topbar/context-chip.ts"])(
    "%s colors through pressurePercentColor and interpolates nothing itself",
    (file) => {
      // Arrange
      const source = readFileSync(path.join(here, "..", file), "utf8");
      // Act / Assert
      expect([source.includes("pressurePercentColor("), source.includes("percentGradientColor(")]).toEqual([
        true,
        false,
      ]);
    },
  );

  it.each(["src/footer/parts.ts", "src/feed/asks/cold-gate.ts"])(
    "%s colors the cold gate's figure through coldGateFigureColor and interpolates nothing itself",
    (file) => {
      // Arrange
      const source = readFileSync(path.join(here, "..", file), "utf8");
      // Act / Assert
      expect([source.includes("coldGateFigureColor("), source.includes("percentGradientColor(")]).toEqual([
        true,
        false,
      ]);
    },
  );

  it("src/footer/activity.ts draws every cold-gate figure through the shared figured line", () => {
    // Arrange
    const source = readFileSync(path.join(here, "..", "src/footer/activity.ts"), "utf8");
    // Act / Assert
    expect([source.includes("coldGateFiguredLine("), source.includes("coldGateFigureColor(")]).toEqual([true, false]);
  });
});
